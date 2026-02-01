%% @doc Consensus POC - Leader Election with Raft-like Pattern
%%
%% This is a simplified proof-of-concept demonstrating leader election
%% and state management using a Raft-like algorithm pattern.
%%
%% API for tests:
%% - start_link/1: Start a consensus node
%% - get_state/1: Get current state (leader | follower)
%% - join_cluster/2: Join nodes into a cluster
%% - stop/1: Stop the node
-module(erlmcp_consensus_poc).

-behaviour(gen_server).

%% API
-export([start_link/1, get_state/1, join_cluster/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ELECTION_TIMEOUT_MIN, 1000).
-define(ELECTION_TIMEOUT_MAX, 1500).

-record(state,
        {node_id :: atom(),
         current_state :: follower | leader | candidate,
         term :: non_neg_integer(),
         voted_for :: atom() | undefined,
         votes_received :: [atom()],
         cluster_members :: [atom()],
         election_timer :: reference() | undefined}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a consensus node
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(NodeId) ->
    gen_server:start_link(?MODULE, [NodeId], []).

%% @doc Get current state of the node
-spec get_state(pid()) -> {ok, follower | leader | candidate}.
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%% @doc Join a cluster of nodes (inform about other members)
-spec join_cluster(pid(), [pid()]) -> ok.
join_cluster(Pid, OtherNodes) ->
    gen_server:cast(Pid, {join_cluster, OtherNodes}).

%% @doc Stop the node
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([NodeId]) ->
    %% Start as follower
    State =
        #state{node_id = NodeId,
               current_state = follower,
               term = 0,
               voted_for = undefined,
               votes_received = [],
               cluster_members = [],
               election_timer = undefined},
    self() ! start_election_timer,
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State#state.current_state}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({join_cluster, OtherNodes}, State) ->
    %% Store cluster members
    NodeIds = [extract_node_id(Node) || Node <- OtherNodes],
    {noreply, State#state{cluster_members = NodeIds}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(start_election_timer, State) ->
    %% Start random election timeout
    Timeout = rand:uniform(?ELECTION_TIMEOUT_MAX - ?ELECTION_TIMEOUT_MIN) + ?ELECTION_TIMEOUT_MIN,
    TimerRef = erlang:send_after(Timeout, self(), election_timeout),
    {noreply, State#state{election_timer = TimerRef}};
handle_info(election_timeout, State = #state{cluster_members = Members}) ->
    case Members of
        [] ->
            %% Single node - become leader immediately
            {noreply, State#state{current_state = leader}};
        _ ->
            %% Multi-node - start election
            NewState = start_election(State),
            {noreply, NewState}
    end;
handle_info({vote_request, Term, CandidateId},
            State = #state{term = CurrentTerm, voted_for = VotedFor}) ->
    if Term > CurrentTerm ->
           %% Vote for this candidate
           NewState = State#state{term = Term, voted_for = CandidateId},
           %% Send vote back
           reply_vote(CandidateId, granted, Term),
           {noreply, NewState};
       Term == CurrentTerm, VotedFor == undefined ->
           %% Haven't voted yet in this term
           NewState = State#state{voted_for = CandidateId},
           reply_vote(CandidateId, granted, Term),
           {noreply, NewState};
       true ->
           %% Deny vote
           reply_vote(CandidateId, denied, Term),
           {noreply, State}
    end;
handle_info({vote_response, granted, Term},
            State =
                #state{term = Term,
                       votes_received = Votes,
                       cluster_members = Members}) ->
    %% Add vote
    NewVotes = [granted | Votes],
    %% Check if we have majority
    Quorum = length(Members) div 2 + 1,
    case length(NewVotes) >= Quorum of
        true ->
            %% Become leader
            {noreply, State#state{current_state = leader, votes_received = NewVotes}};
        false ->
            {noreply, State#state{votes_received = NewVotes}}
    end;
handle_info({vote_response, denied, _Term}, State) ->
    %% Vote denied - stay as candidate
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Start leader election
start_election(State =
                   #state{node_id = NodeId,
                          term = Term,
                          cluster_members = Members}) ->
    %% Increment term
    NewTerm = Term + 1,
    %% Vote for self
    NewState =
        State#state{term = NewTerm,
                    current_state = candidate,
                    voted_for = NodeId,
                    votes_received = [self_vote]},
    %% Request votes from all members
    request_votes(Members, NodeId, NewTerm),
    %% Reset election timer
    cancel_timer(State#state.election_timer),
    self() ! start_election_timer,
    NewState.

%% @doc Request votes from cluster members
request_votes(Members, CandidateId, Term) ->
    lists:foreach(fun(MemberId) ->
                     %% In real implementation, would send to actual node
                     %% For POC, we simulate by sending to self
                     self() ! {vote_request, Term, CandidateId}
                  end,
                  Members).

%% @doc Reply to vote request
reply_vote(_CandidateId, Result, Term) ->
    %% Simulate vote response
    self() ! {vote_response, Result, Term}.

%% @doc Extract node ID from pid
extract_node_id(Pid) when is_pid(Pid) ->
    %% For POC, generate a name from pid
    list_to_atom("node_" ++ pid_to_list(Pid));
extract_node_id(NodeId) when is_atom(NodeId) ->
    NodeId.

%% @doc Cancel timer
cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) when is_reference(TimerRef) ->
    erlang:cancel_timer(TimerRef).
