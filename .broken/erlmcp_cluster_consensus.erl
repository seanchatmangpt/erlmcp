%% @doc Cluster Consensus Module for erlmcp v3
%%
%% This module implements distributed consensus mechanisms:
%%   - Raft-inspired leader election
%%   - Voting and proposal system
%%   - Quorum-based decision making
%%   - Conflict resolution
%%   - Log replication
%%   - Configuration changes
-module(erlmcp_cluster_consensus).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([propose/2, vote/3]).
-export([get_leader/0, get_status/0]).
-export([force_election/0, step_down/0]).
-export([get_current_term/0]).
-export([subscribe/1, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(consensus_state,
        {
         node_id :: node(),
         current_term :: non_neg_integer(),
         voted_for :: node() | undefined,
         leader :: node() | undefined,
         state :: follower | candidate | leader,
         votes_received :: sets:set(node()),
         log :: [log_entry()],
         commit_index :: non_neg_integer(),
         last_applied :: non_neg_integer(),
         next_index :: #{node() => non_neg_integer()},
         match_index :: #{node() => non_neg_integer()},
         election_timeout :: pos_integer(),
         heartbeat_interval :: pos_integer(),
         election_timer :: reference() | undefined,
         heartbeat_timer :: reference() | undefined,
         subscribers :: sets:set(pid()),
         quorum_size :: pos_integer(),
         proposals :: #{reference() => proposal()}
        }).

-type log_entry() :: #{
        index => non_neg_integer(),
        term => non_neg_integer(),
        command => term(),
        timestamp => integer()
       }.

-type proposal() :: #{
        id => reference(),
        proposer => node(),
        command => term(),
        proposed_at => integer(),
        votes => #{node() => boolean()},
        status => pending | approved | rejected | committed
       }.

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Propose a value for consensus
-spec propose(term(), pos_integer()) -> {ok, reference()} | {error, term()}.
propose(Command, Timeout) ->
    gen_server:call(?MODULE, {propose, Command}, Timeout).

%% @doc Cast a vote for a proposal
-spec vote(reference(), boolean(), node()) -> ok.
vote(ProposalId, Vote, Voter) ->
    gen_server:cast(?MODULE, {vote, ProposalId, Vote, Voter}).

%% @doc Get current leader
-spec get_leader() -> {ok, node() | undefined}.
get_leader() ->
    gen_server:call(?MODULE, get_leader).

%% @doc Get consensus status
-spec get_status() -> {ok, map()}.
get_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc Force a new election
-spec force_election() -> ok.
force_election() ->
    gen_server:cast(?MODULE, force_election).

%% @doc Step down from leadership
-spec step_down() -> ok | {error, term()}.
step_down() ->
    gen_server:call(?MODULE, step_down).

%% @doc Get current term
-spec get_current_term() -> {ok, non_neg_integer()}.
get_current_term() ->
    gen_server:call(?MODULE, get_current_term).

%% @doc Subscribe to consensus events
-spec subscribe(pid()) -> ok.
subscribe(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe, Subscriber}).

%% @doc Unsubscribe from consensus events
-spec unsubscribe(pid()) -> ok.
unsubscribe(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe, Subscriber}).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, #consensus_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    QuorumSize = maps:get(quorum_size, Options, 2),
    ElectionTimeout = maps:get(election_timeout, Options, 15000),
    HeartbeatInterval = maps:get(heartbeat_interval, Options, 5000),

    State = #consensus_state{
        node_id = NodeId,
        current_term = 0,
        voted_for = undefined,
        leader = undefined,
        state = follower,
        votes_received = sets:new(),
        log = [],
        commit_index = 0,
        last_applied = 0,
        next_index => #{},
        match_index => #{},
        election_timeout = ElectionTimeout,
        heartbeat_interval = HeartbeatInterval,
        election_timer = undefined,
        heartbeat_timer = undefined,
        subscribers = sets:new(),
        quorum_size = QuorumSize,
        proposals => #{}
    },

    %% Start election timer
    NewState = start_election_timer(State),

    logger:info("Consensus module started (quorum=~p)", [QuorumSize]),

    {ok, NewState}.

-spec handle_call(term(), {pid(), term()}, #consensus_state{}) ->
    {reply, term(), #consensus_state{}}.
handle_call({propose, Command}, From, #consensus_state{state = State} = S) ->
    case State of
        leader ->
            handle_propose(Command, From, S);
        _ ->
            Leader = S#consensus_state.leader,
            {reply, {error, {not_leader, Leader}}, S}
    end;

handle_call(get_leader, _From, State) ->
    {reply, {ok, State#consensus_state.leader}, State};

handle_call(get_status, _From, State) ->
    Status = #{
        current_term => State#consensus_state.current_term,
        leader => State#consensus_state.leader,
        state => State#consensus_state.state,
        commit_index => State#consensus_state.commit_index,
        log_length => length(State#consensus_state.log),
        quorum_size => State#consensus_state.quorum_size
    },
    {reply, {ok, Status}, State};

handle_call(step_down, _From, #consensus_state{state = leader} = State) ->
    NewState = become_follower(State),
    {reply, ok, NewState};

handle_call(step_down, _From, State) ->
    {reply, {error, not_leader}, State};

handle_call(get_current_term, _From, State) ->
    {reply, {ok, State#consensus_state.current_term}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #consensus_state{}) -> {noreply, #consensus_state{}}.
handle_cast({vote, ProposalId, Vote, Voter},
            #consensus_state{proposals = Proposals} = State) ->

    case maps:get(ProposalId, Proposals, undefined) of
        undefined ->
            {noreply, State};
        Proposal ->
           NewProposal = record_vote(Proposal, Voter, Vote),
            NewProposals = maps:put(ProposalId, NewProposal, Proposals),
            NewState = State#consensus_state{proposals = NewProposals},

            case check_proposal_approved(NewProposal, State) of
                true ->
                    FinalState = commit_proposal(ProposalId, NewState),
                    {noreply, FinalState};
                false ->
                    {noreply, NewState}
            end
    end;

handle_cast(force_election, State) ->
    NewState = start_election(State),
    {noreply, NewState};

handle_cast({subscribe, Subscriber}, State) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#consensus_state.subscribers),
    {noreply, State#consensus_state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Subscriber}, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#consensus_state.subscribers),
    {noreply, State#consensus_state{subscribers = NewSubscribers}};

handle_cast({request_vote, Candidate, Term, _LastLogIndex, _LastLogTerm}, State) ->
    NewState = handle_vote_request(Candidate, Term, State),
    {noreply, NewState};

handle_cast({vote_response, Voter, Term, VoteGranted}, State) ->
    NewState = handle_vote_response(Voter, Term, VoteGranted, State),
    {noreply, NewState};

handle_cast({append_entries, Leader, Term, Entries, CommitIndex}, State) ->
    NewState = handle_append_entries(Leader, Term, Entries, CommitIndex, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #consensus_state{}) -> {noreply, #consensus_state{}}.
handle_info(election_timeout, #consensus_state{state = follower} = State) ->
    NewState = start_election(State),
    {noreply, NewState};

handle_info(election_timeout, #consensus_state{state = candidate} = State) ->
    %% Election timeout while candidate, restart election
    NewState = start_election(State),
    {noreply, NewState};

handle_info(heartbeat_timeout, #consensus_state{state = leader} = State) ->
    NewState = send_heartbeats(State),
    {noreply, NewState};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = sets:filter(fun(S) -> S =/= Pid end, State#consensus_state.subscribers),
    {noreply, State#consensus_state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #consensus_state{}) -> ok.
terminate(_Reason, #consensus_state{election_timer = ET, heartbeat_timer = HT}) ->
    case ET of undefined -> ok; _ -> erlang:cancel_timer(ET) end,
    case HT of undefined -> ok; _ -> erlang:cancel_timer(HT) end,
    logger:info("Consensus module terminating"),
    ok.

-spec code_change(term(), #consensus_state{}, term()) -> {ok, #consensus_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Start election
-spec start_election(#consensus_state{}) -> #consensus_state{}.
start_election(#consensus_state{node_id = NodeId} = State) ->
    %% Increment term
    NewTerm = State#consensus_state.current_term + 1,

    %% Vote for self
    Votes = sets:from_list([NodeId]),

    %% Transition to candidate
    NewState = State#consensus_state{
        current_term = NewTerm,
        voted_for = NodeId,
        leader = undefined,
        state = candidate,
        votes_received = Votes
    },

    %% Request votes from all members
    request_votes(NewState),

    %% Reset election timer
    ElectionState = start_election_timer(NewState),

    logger:info("Started election for term ~p", [NewTerm]),

    notify_subscribers(ElectionState, {election_started, NewTerm}),

    ElectionState.

%% @doc Become follower
-spec become_follower(#consensus_state{}) -> #consensus_state{}.
become_follower(State) ->
    State#consensus_state{state = follower}.

%% @doc Become leader
-spec become_leader(#consensus_state{}) -> #consensus_state{}.
become_leader(#consensus_state{node_id = NodeId, current_term = Term} = State) ->

    logger:info("Node ~p became leader for term ~p", [NodeId, Term]),

    NewState = State#consensus_state{
        leader = NodeId,
        state = leader
    },

    %% Start heartbeat timer
    HeartbeatState = send_heartbeats(NewState),

    notify_subscribers(HeartbeatState, {leader_elected, NodeId, Term}),

    HeartbeatState.

%% @doc Request votes from cluster members
-spec request_votes(#consensus_state{}) -> ok.
request_votes(#consensus_state{
        node_id = NodeId,
        current_term = Term,
        log = Log,
        quorum_size = Quorum
    }) ->

    %% Get cluster members
    {ok, Members} = erlmcp_cluster_membership:get_members(),

    LastLogIndex = length(Log),
    LastLogTerm = case Log of
        [] -> 0;
        [#{} = LastEntry | _] -> maps:get(term, LastEntry, 0)
    end,

    %% Request votes
    lists:foreach(fun(Member) ->
        case Member of
            NodeId -> skip;
            _ ->
                gen_server:cast({?MODULE, Member},
                    {request_vote, NodeId, Term, LastLogIndex, LastLogTerm})
        end
    end, Members),

    ok.

%% @doc Handle vote request
-spec handle_vote_request(node(), non_neg_integer(), #consensus_state{}) ->
    #consensus_state{}.
handle_vote_request(Candidate, Term, #consensus_state{
        current_term = CurrentTerm,
        voted_for = VotedFor,
        node_id = NodeId
    } = State) ->

    {GrantVote, NewTerm} = if
        Term > CurrentTerm ->
            %% Higher term, grant vote
            {true, Term};
        Term == CurrentTerm ->
            %% Same term, check if already voted
            case VotedFor of
                undefined -> {true, Term};
                Candidate -> {true, Term};
                _ -> {false, Term}
            end;
        true ->
            %% Lower term, deny
            {false, CurrentTerm}
    end,

    %% Send response
    gen_server:cast({?MODULE, Candidate}, {vote_response, NodeId, NewTerm, GrantVote}),

    %% Update state if term changed
    case NewTerm > CurrentTerm of
        true ->
            State#consensus_state{
                current_term = NewTerm,
                voted_for = case GrantVote of true -> Candidate; false -> VotedFor end
            };
        false ->
            State
    end.

%% @doc Handle vote response
-spec handle_vote_response(node(), non_neg_integer(), boolean(), #consensus_state{}) ->
    #consensus_state{}.
handle_vote_response(Voter, Term, VoteGranted,
                      #consensus_state{
                          state = candidate,
                          current_term = CurrentTerm,
                          votes_received = Votes,
                          quorum_size = Quorum,
                          node_id = NodeId
                      } = State) ->

    case Term =/= CurrentTerm of
        true ->
            %% Old term, ignore
            State;
        false ->
            NewVotes = case VoteGranted of
                true -> sets:add_element(Voter, Votes);
                false -> Votes
            end,

            VoteCount = sets:size(NewVotes),

            case VoteCount >= Quorum of
                true ->
                    %% Won election
                    become_leader(State#consensus_state{votes_received = NewVotes});
                false ->
                    State#consensus_state{votes_received = NewVotes}
            end
    end;

handle_vote_response(_Voter, _Term, _VoteGranted, State) ->
    %% Not candidate, ignore
    State.

%% @doc Send heartbeats (empty append_entries)
-spec send_heartbeats(#consensus_state{}) -> #consensus_state{}.
send_heartbeats(#consensus_state{
        node_id = NodeId,
        current_term = Term,
        commit_index = CommitIndex,
        heartbeat_interval = Interval
    } = State) ->

    %% Get cluster members
    {ok, Members} = erlmcp_cluster_membership:get_members(),

    %% Send heartbeat to all members
    lists:foreach(fun(Member) ->
        case Member of
            NodeId -> skip;
            _ ->
                gen_server:cast({?MODULE, Member},
                    {append_entries, NodeId, Term, [], CommitIndex})
        end
    end, Members),

    %% Schedule next heartbeat
    Ref = erlang:send_after(Interval, self(), heartbeat_timeout),

    State#consensus_state{heartbeat_timer = Ref}.

%% @doc Handle append_entries (heartbeat or log replication)
-spec handle_append_entries(node(), non_neg_integer(), [log_entry()],
                            non_neg_integer(), #consensus_state{}) ->
    #consensus_state{}.
handle_append_entries(Leader, Term, Entries, CommitIndex,
                      #consensus_state{
                          current_term = CurrentTerm,
                          state = StateName
                      } = State) ->

    case Term >= CurrentTerm of
        true ->
            %% Recognize leader
            NewState = case StateName of
                candidate ->
                    State#consensus_state{
                        leader = Leader,
                        state = follower,
                        current_term = Term
                    };
                _ ->
                    State#consensus_state{
                        leader = Leader,
                        current_term = Term
                    }
            end,

            %% Append entries
            NewLog = append_log_entries(NewState#consensus_state.log, Entries),

            %% Update commit index
            NewCommitIndex = max(NewState#consensus_state.commit_index, CommitIndex),

            NewState#consensus_state{
                log = NewLog,
                commit_index = NewCommitIndex
            };
        false ->
            %% Stale term, reject
            State
    end.

%% @doc Append log entries
-spec append_log_entries([log_entry()], [log_entry()]) -> [log_entry()].
append_log_entries(Log, Entries) ->
    Log ++ Entries.

%% @doc Start election timer
-spec start_election_timer(#consensus_state{}) -> #consensus_state{}.
start_election_timer(#consensus_state{election_timeout = Timeout} = State) ->
    %% Add random jitter to prevent split votes
    Jitter = rand:uniform(Timeout div 2),
    TimeoutWithJitter = Timeout + Jitter,

    Ref = erlang:send_after(TimeoutWithJitter, self(), election_timeout),

    State#consensus_state{election_timer = Ref}.

%% @doc Handle proposal
-spec handle_propose(term(), {pid(), term()}, #consensus_state{}) ->
    {reply, term(), #consensus_state{}}.
handle_propose(Command, From, #consensus_state{
        current_term = Term,
        log = Log,
        proposals = Proposals
    } = State) ->

    %% Create proposal
    ProposalId = make_ref(),
    Index = length(Log) + 1,

    Proposal = #{
        id => ProposalId,
        proposer => node(),
        command => Command,
        proposed_at => erlang:system_time(millisecond),
        votes => #{},
        status => pending
    },

    %% Create log entry
    LogEntry = #{
        index => Index,
        term => Term,
        command => Command,
        timestamp => erlang:system_time(millisecond)
    },

    NewLog = Log ++ [LogEntry],
    NewProposals = maps:put(ProposalId, Proposal, Proposals),

    NewState = State#consensus_state{
        log = NewLog,
        proposals = NewProposals
    },

    {reply, {ok, ProposalId}, NewState}.

%% @doc Record vote
-spec record_vote(proposal(), node(), boolean()) -> proposal().
record_vote(Proposal, Voter, Vote) ->
    Votes = maps:get(votes, Proposal),
    Proposal#{votes => maps:put(Voter, Vote, Votes)}.

%% @doc Check if proposal is approved
-spec check_proposal_approved(proposal(), #consensus_state{}) -> boolean().
check_proposal_approved(Proposal, #consensus_state{quorum_size = Quorum}) ->
    Votes = maps:get(votes, Proposal),
    Approvals = maps:fold(fun(_Voter, Vote, Acc) ->
        case Vote of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Votes),

    Approvals >= Quorum.

%% @doc Commit proposal
-spec commit_proposal(reference(), #consensus_state{}) -> #consensus_state{}.
commit_proposal(ProposalId, #consensus_state{proposals = Proposals} = State) ->
    case maps:get(ProposalId, Proposals, undefined) of
        undefined ->
            State;
        Proposal ->
            NewProposal = Proposal#{status => committed},
           NewProposals = maps:put(ProposalId, NewProposal, Proposals),

            notify_subscribers(State, {proposal_committed, ProposalId}),

            State#consensus_state{proposals = NewProposals}
    end.

%% @doc Notify subscribers
-spec notify_subscribers(#consensus_state{}, term()) -> ok.
notify_subscribers(#consensus_state{subscribers = Subscribers}, Event) ->
    sets:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {consensus_event, Event};
            false ->
                ok
        end
    end, Subscribers),
    ok.
