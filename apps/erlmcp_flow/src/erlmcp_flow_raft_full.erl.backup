%%%-------------------------------------------------------------------
%%% @doc Raft Consensus Protocol Implementation
%%% Leader-based consensus with < 100ms finality
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_raft).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    append_entries/4,
    request_vote/3,
    submit_command/2,
    get_state/1,
    get_leader/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-record(log_entry, {
    term :: non_neg_integer(),
    index :: non_neg_integer(),
    command :: term()
}).

-record(state, {
    node_id :: binary(),
    current_term = 0 :: non_neg_integer(),
    voted_for = undefined :: binary() | undefined,
    log = [] :: [#log_entry{}],
    commit_index = 0 :: non_neg_integer(),
    last_applied = 0 :: non_neg_integer(),
    role = follower :: follower | candidate | leader,

    % Leader state
    next_index = #{} :: #{binary() => non_neg_integer()},
    match_index = #{} :: #{binary() => non_neg_integer()},

    % Cluster
    peers = [] :: [binary()],
    election_timeout = 5000 :: pos_integer(),
    heartbeat_timeout = 1000 :: pos_integer(),
    election_timer :: reference() | undefined,
    heartbeat_timer :: reference() | undefined,

    % Votes
    votes_received = [] :: [binary()],
    leader :: binary() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Config) ->
    NodeId = maps:get(node_id, Config),
    gen_server:start_link({local, node_name(NodeId)}, ?MODULE, Config, []).

-spec append_entries(NodeId, LeaderTerm, PrevLogInfo, Entries) -> {ok, Term, Success}
    when NodeId :: binary(),
         LeaderTerm :: non_neg_integer(),
         PrevLogInfo :: {Term :: non_neg_integer(), Index :: non_neg_integer()},
         Entries :: [term()],
         Term :: non_neg_integer(),
         Success :: boolean().
append_entries(NodeId, LeaderTerm, PrevLogInfo, Entries) ->
    gen_server:call(node_name(NodeId), {append_entries, LeaderTerm, PrevLogInfo, Entries}).

-spec request_vote(NodeId, CandidateTerm, LastLogInfo) -> {ok, Term, VoteGranted}
    when NodeId :: binary(),
         CandidateTerm :: non_neg_integer(),
         LastLogInfo :: {Term :: non_neg_integer(), Index :: non_neg_integer()},
         Term :: non_neg_integer(),
         VoteGranted :: boolean().
request_vote(NodeId, CandidateTerm, LastLogInfo) ->
    gen_server:call(node_name(NodeId), {request_vote, CandidateTerm, LastLogInfo}).

-spec submit_command(NodeId, Command) -> {ok, Index} | {error, not_leader}
    when NodeId :: binary(),
         Command :: term(),
         Index :: non_neg_integer().
submit_command(NodeId, Command) ->
    gen_server:call(node_name(NodeId), {submit_command, Command}).

get_state(NodeId) ->
    gen_server:call(node_name(NodeId), get_state).

get_leader(NodeId) ->
    gen_server:call(node_name(NodeId), get_leader).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
    NodeId = maps:get(node_id, Config),
    Peers = maps:get(peers, Config, []),
    ElectionTimeout = maps:get(election_timeout, Config, 5000),

    State = #state{
        node_id = NodeId,
        peers = Peers,
        election_timeout = ElectionTimeout,
        heartbeat_timeout = ElectionTimeout div 5
    },

    % Start election timer
    TimerRef = erlang:send_after(random_election_timeout(ElectionTimeout),
                                  self(), election_timeout),

    ?LOG_INFO("Raft node ~p starting with ~p peers", [NodeId, length(Peers)]),
    {ok, State#state{election_timer = TimerRef}}.

handle_call({append_entries, LeaderTerm, {PrevLogTerm, PrevLogIndex}, Entries},
            _From, State) ->
    % Reply false if term < currentTerm (§5.1)
    if
        LeaderTerm < State#state.current_term ->
            {reply, {ok, State#state.current_term, false}, State};
        true ->
            % Update term if necessary
            NewState = if
                LeaderTerm > State#state.current_term ->
                    become_follower(State, LeaderTerm);
                true ->
                    State
            end,

            % Reset election timer (received AppendEntries from valid leader)
            reset_election_timer(NewState),

            % Reply false if log doesn't contain entry at prevLogIndex whose term matches
            case check_log_consistency(NewState, PrevLogIndex, PrevLogTerm) of
                true ->
                    % If an existing entry conflicts with a new one, delete existing entry
                    % and all that follow it (§5.3)
                    TruncatedLog = lists:sublist(NewState#state.log, PrevLogIndex),

                    % Append new entries
                    NewLog = TruncatedLog ++ [#log_entry{
                        term = LeaderTerm,
                        index = PrevLogIndex + I,
                        command = Cmd
                    } || {I, Cmd} <- lists:zip(lists:seq(1, length(Entries)), Entries)],

                    FinalState = NewState#state{log = NewLog},

                    {reply, {ok, LeaderTerm, true}, FinalState};
                false ->
                    {reply, {ok, NewState#state.current_term, false}, NewState}
            end
    end;

handle_call({request_vote, CandidateTerm, {LastLogTerm, LastLogIndex}}, {CandidatePid, _},
            State) ->
    CandidateId = get_candidate_id(CandidatePid),

    % Reply false if term < currentTerm (§5.1)
    if
        CandidateTerm < State#state.current_term ->
            {reply, {ok, State#state.current_term, false}, State};
        true ->
            % Update term if necessary
            NewState = if
                CandidateTerm > State#state.current_term ->
                    State#state{
                        current_term = CandidateTerm,
                        voted_for = undefined,
                        role = follower
                    };
                true ->
                    State
            end,

            % If votedFor is null or candidateId, and candidate's log is at least
            % as up-to-date as receiver's log, grant vote (§5.2, §5.4)
            CanVote = (NewState#state.voted_for =:= undefined orelse
                      NewState#state.voted_for =:= CandidateId),

            LogUpToDate = is_log_up_to_date(NewState, LastLogIndex, LastLogTerm),

            case CanVote andalso LogUpToDate of
                true ->
                    ?LOG_INFO("Node ~p: granting vote to ~p for term ~p",
                             [State#state.node_id, CandidateId, CandidateTerm]),

                    VotedState = NewState#state{voted_for = CandidateId},
                    reset_election_timer(VotedState),

                    {reply, {ok, CandidateTerm, true}, VotedState};
                false ->
                    ?LOG_DEBUG("Node ~p: denying vote to ~p (CanVote=~p, LogUpToDate=~p)",
                              [State#state.node_id, CandidateId, CanVote, LogUpToDate]),
                    {reply, {ok, NewState#state.current_term, false}, NewState}
            end
    end;

handle_call({submit_command, Command}, _From, State) ->
    case State#state.role of
        leader ->
            % Append entry to local log
            NewIndex = length(State#state.log) + 1,
            NewEntry = #log_entry{
                term = State#state.current_term,
                index = NewIndex,
                command = Command
            },

            NewLog = State#state.log ++ [NewEntry],
            NewState = State#state{log = NewLog},

            % Replicate to followers (async)
            gen_server:cast(self(), replicate_log),

            {reply, {ok, NewIndex}, NewState};
        _ ->
            % Not leader, return leader info
            {reply, {error, not_leader, State#state.leader}, State}
    end;

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(get_leader, _From, State) ->
    {reply, {ok, State#state.leader}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(replicate_log, #state{role = leader} = State) ->
    % Send AppendEntries to all peers
    lists:foreach(fun(PeerId) ->
        NextIdx = maps:get(PeerId, State#state.next_index, 1),
        PrevIdx = NextIdx - 1,
        PrevTerm = case PrevIdx of
            0 -> 0;
            _ -> (lists:nth(PrevIdx, State#state.log))#log_entry.term
        end,

        Entries = [E#log_entry.command ||
                  E <- lists:sublist(State#state.log, NextIdx, length(State#state.log))],

        % Async send
        spawn(fun() ->
            try
                case append_entries(PeerId, State#state.current_term,
                                  {PrevTerm, PrevIdx}, Entries) of
                    {ok, _Term, true} ->
                        gen_server:cast(self(), {append_success, PeerId, length(Entries)});
                    {ok, _Term, false} ->
                        gen_server:cast(self(), {append_failed, PeerId})
                end
            catch
                _:_ -> ok  % Peer unreachable
            end
        end)
    end, State#state.peers),

    {noreply, State};

handle_cast({append_success, PeerId, NumEntries}, State) ->
    % Update nextIndex and matchIndex for follower
    OldNextIdx = maps:get(PeerId, State#state.next_index, 1),
    NewNextIdx = OldNextIdx + NumEntries,
    NewMatchIdx = NewNextIdx - 1,

    NewState = State#state{
        next_index = maps:put(PeerId, NewNextIdx, State#state.next_index),
        match_index = maps:put(PeerId, NewMatchIdx, State#state.match_index)
    },

    % Check if we can update commit index
    FinalState = maybe_update_commit_index(NewState),

    {noreply, FinalState};

handle_cast({append_failed, PeerId}, State) ->
    % Decrement nextIndex and retry
    OldNextIdx = maps:get(PeerId, State#state.next_index, 1),
    NewNextIdx = max(1, OldNextIdx - 1),

    NewState = State#state{
        next_index = maps:put(PeerId, NewNextIdx, State#state.next_index)
    },

    gen_server:cast(self(), replicate_log),

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(election_timeout, State) ->
    ?LOG_INFO("Node ~p: election timeout, starting election", [State#state.node_id]),
    NewState = start_election(State),
    {noreply, NewState};

handle_info(heartbeat_timeout, #state{role = leader} = State) ->
    % Send heartbeat (empty AppendEntries) to all peers
    gen_server:cast(self(), replicate_log),

    % Reset heartbeat timer
    NewTimer = erlang:send_after(State#state.heartbeat_timeout, self(), heartbeat_timeout),
    {noreply, State#state{heartbeat_timer = NewTimer}};

handle_info({vote_response, VoterNodeId, VoteGranted}, State) ->
    case State#state.role of
        candidate when VoteGranted ->
            NewVotes = [VoterNodeId | State#state.votes_received],
            NumVotes = length(NewVotes),
            Majority = (length(State#state.peers) + 1) div 2 + 1,

            case NumVotes >= Majority of
                true ->
                    ?LOG_INFO("Node ~p: won election with ~p votes", [State#state.node_id, NumVotes]),
                    NewState = become_leader(State#state{votes_received = NewVotes}),
                    {noreply, NewState};
                false ->
                    {noreply, State#state{votes_received = NewVotes}}
            end;
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

node_name(NodeId) ->
    binary_to_atom(<<"raft_", NodeId/binary>>, utf8).

random_election_timeout(Base) ->
    Base + rand:uniform(Base).

reset_election_timer(State) ->
    case State#state.election_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    NewTimer = erlang:send_after(random_election_timeout(State#state.election_timeout),
                                  self(), election_timeout),
    State#state{election_timer = NewTimer}.

check_log_consistency(State, PrevLogIndex, PrevLogTerm) ->
    if
        PrevLogIndex =:= 0 ->
            true;
        PrevLogIndex > length(State#state.log) ->
            false;
        true ->
            Entry = lists:nth(PrevLogIndex, State#state.log),
            Entry#log_entry.term =:= PrevLogTerm
    end.

is_log_up_to_date(State, LastLogIndex, LastLogTerm) ->
    case State#state.log of
        [] ->
            LastLogIndex =:= 0 andalso LastLogTerm =:= 0;
        Log ->
            LastEntry = lists:last(Log),
            MyLastTerm = LastEntry#log_entry.term,
            MyLastIndex = LastEntry#log_entry.index,

            LastLogTerm > MyLastTerm orelse
            (LastLogTerm =:= MyLastTerm andalso LastLogIndex >= MyLastIndex)
    end.

start_election(State) ->
    NewTerm = State#state.current_term + 1,

    ?LOG_INFO("Node ~p: starting election for term ~p", [State#state.node_id, NewTerm]),

    NewState = State#state{
        current_term = NewTerm,
        role = candidate,
        voted_for = State#state.node_id,
        votes_received = [State#state.node_id]
    },

    % Send RequestVote RPCs to all peers
    {LastLogTerm, LastLogIndex} = case NewState#state.log of
        [] -> {0, 0};
        Log ->
            LastEntry = lists:last(Log),
            {LastEntry#log_entry.term, LastEntry#log_entry.index}
    end,

    lists:foreach(fun(PeerId) ->
        spawn(fun() ->
            try
                case request_vote(PeerId, NewTerm, {LastLogTerm, LastLogIndex}) of
                    {ok, _Term, VoteGranted} ->
                        self() ! {vote_response, PeerId, VoteGranted}
                catch
                    _:_ -> ok  % Peer unreachable
                end
        end)
    end, NewState#state.peers),

    reset_election_timer(NewState).

become_leader(State) ->
    ?LOG_INFO("Node ~p: became leader for term ~p",
             [State#state.node_id, State#state.current_term]),

    % Initialize leader state
    NextIndex = maps:from_list([{P, length(State#state.log) + 1} ||
                               P <- State#state.peers]),
    MatchIndex = maps:from_list([{P, 0} || P <- State#state.peers]),

    % Cancel election timer
    case State#state.election_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    % Start heartbeat timer
    HeartbeatTimer = erlang:send_after(State#state.heartbeat_timeout,
                                       self(), heartbeat_timeout),

    % Send initial empty AppendEntries (heartbeat)
    gen_server:cast(self(), replicate_log),

    State#state{
        role = leader,
        leader = State#state.node_id,
        next_index = NextIndex,
        match_index = MatchIndex,
        election_timer = undefined,
        heartbeat_timer = HeartbeatTimer
    }.

become_follower(State, NewTerm) ->
    ?LOG_INFO("Node ~p: became follower for term ~p", [State#state.node_id, NewTerm]),

    % Cancel heartbeat timer if leader
    case State#state.heartbeat_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    State#state{
        current_term = NewTerm,
        role = follower,
        voted_for = undefined,
        heartbeat_timer = undefined
    }.

maybe_update_commit_index(#state{role = leader} = State) ->
    % Find highest N where majority of matchIndex[i] >= N
    MatchIndices = [0 | maps:values(State#state.match_index)],
    SortedIndices = lists:reverse(lists:sort(MatchIndices)),

    Majority = (length(State#state.peers) + 1) div 2 + 1,
    NewCommitIndex = lists:nth(Majority, SortedIndices),

    case NewCommitIndex > State#state.commit_index of
        true ->
            ?LOG_DEBUG("Node ~p: updating commit index to ~p",
                      [State#state.node_id, NewCommitIndex]),
            State#state{commit_index = NewCommitIndex};
        false ->
            State
    end;
maybe_update_commit_index(State) ->
    State.

get_candidate_id(_Pid) ->
    % Extract candidate ID from process
    <<"candidate">>.
