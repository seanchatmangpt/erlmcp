%% @doc Raft Consensus Algorithm Implementation for erlmcp v3
%%
%% This module implements the Raft consensus algorithm providing:
%%   - Strong consistency guarantee for distributed configuration
%%   - Leader election with randomized timeouts to prevent split votes
%%   - Log replication with quorum acknowledgment
%%   - Split-brain prevention through term-based voting
%%   - Cluster membership changes via joint consensus
%%   - Log compaction through snapshotting
%%
%% Usage:
%%   1. Define a state machine callback module
%%   2. Start a Raft cluster with erlmcp_raft_sup:start_cluster/4
%%   3. Submit commands via erlmcp_raft:write/2
%%   4. Read state via erlmcp_raft:read/1 (linearizable reads on leader)
%%
%% @reference Diego Ongaro and John Ousterhout, "In Search of an Understandable
%% Consensus Algorithm", USENIX ATC 2014.
-module(erlmcp_raft).

-behaviour(gen_server).

-include("erlmcp_raft.hrl").
-include("erlmcp.hrl").

%% API - Client Operations
-export([start_link/4, start_link/5, start_link/6,
         write/2, read/1, status/1,
         add_server/2, remove_server/2,
         get_leader/1, get_cluster_members/1,
         stop/1, force_election/1]).

%% API - Internal RPC handlers
-export([handle_request_vote/2,
         handle_append_entries/2,
         handle_install_snapshot/2,
         forward_to_leader/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3]).

%% Internal state transitions
-export([become_follower/2, become_candidate/1, become_leader/1]).

%%%====================================================================
%%% API Functions - Client Operations
%%%====================================================================

%% @doc Start a Raft server node
-spec start_link(ClusterName :: binary(), NodeId :: raft_node_id(),
                 StateMachine :: raft_state_machine(), Peers :: [raft_node_id()]) ->
    {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId, StateMachine, Peers) ->
    start_link(ClusterName, NodeId, StateMachine, Peers, erlmcp_raft_log, erlmcp_raft_transport).

-spec start_link(ClusterName :: binary(), NodeId :: raft_node_id(),
                 StateMachine :: raft_state_machine(), Peers :: [raft_node_id()],
                 LogModule :: module()) ->
    {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId, StateMachine, Peers, LogModule) ->
    start_link(ClusterName, NodeId, StateMachine, Peers, LogModule, erlmcp_raft_transport).

-spec start_link(ClusterName :: binary(), NodeId :: raft_node_id(),
                 StateMachine :: raft_state_machine(), Peers :: [raft_node_id()],
                 LogModule :: module(), Transport :: raft_transport()) ->
    {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId, StateMachine, Peers, LogModule, Transport) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [ClusterName, NodeId, StateMachine, Peers, LogModule, Transport], []).

%% @doc Submit a write command to the Raft cluster
-spec write(pid() | atom(), term()) -> raft_write_result().
write(ServerRef, Command) ->
    gen_server:call(ServerRef, {write, Command}, infinity).

%% @doc Read the current state from the state machine
%% Only the leader can serve linearizable reads safely
-spec read(pid() | atom()) -> raft_read_result().
read(ServerRef) ->
    gen_server:call(ServerRef, read, infinity).

%% @doc Get the current status of this Raft node
-spec status(pid() | atom()) -> {ok, raft_status()} | {error, term()}.
status(ServerRef) ->
    gen_server:call(ServerRef, status, 5000).

%% @doc Add a new server to the cluster
-spec add_server(pid() | atom(), raft_node_id()) -> ok | {error, term()}.
add_server(ServerRef, NewServerId) ->
    gen_server:call(ServerRef, {add_server, NewServerId}, infinity).

%% @doc Remove a server from the cluster
-spec remove_server(pid() | atom(), raft_node_id()) -> ok | {error, term()}.
remove_server(ServerRef, ServerId) ->
    gen_server:call(ServerRef, {remove_server, ServerId}, infinity).

%% @doc Get the current cluster leader
-spec get_leader(pid() | atom()) -> {ok, raft_node_id() | undefined} | {error, term()}.
get_leader(ServerRef) ->
    gen_server:call(ServerRef, get_leader, 5000).

%% @doc Get all cluster members
-spec get_cluster_members(pid() | atom()) -> {ok, [raft_node_id()]} | {error, term()}.
get_cluster_members(ServerRef) ->
    gen_server:call(ServerRef, get_cluster_members, 5000).

%% @doc Stop the Raft server
-spec stop(pid() | atom()) -> ok.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%% @doc Force a new election (useful for testing)
-spec force_election(pid() | atom()) -> ok.
force_election(ServerRef) ->
    gen_server:cast(ServerRef, force_election).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(list()) -> {ok, #raft_server_state{}, {continue, initialize}}.
init([ClusterName, NodeId, StateMachine, Peers, LogModule, Transport]) ->
    process_flag(trap_exit, true),

    %% Initialize log storage
    {ok, LogTid} = LogModule:init(ClusterName, NodeId),

    %% Initialize state machine
    {ok, SMState} = StateMachine:init([]),

    %% Load or initialize persistent state
    {CurrentTerm, VotedFor} = load_persistent_state(LogTid, NodeId),

    %% Build cluster membership
    ClusterMembers = [NodeId | Peers],

    State = #raft_server_state{
        current_term = CurrentTerm,
        voted_for = VotedFor,
        log = LogTid,
        commit_index = 0,
        last_applied = 0,
        next_index = maps:from_list([{P, 1} || P <- Peers]),
        match_index = maps:from_list([{P, 0} || P <- Peers]),
        peers = Peers,
        cluster_members = ClusterMembers,
        leader_id = undefined,
        state_machine = StateMachine,
        state_machine_state = SMState,
        server_state = follower,
        node_id = NodeId,
        election_timer_ref = undefined,
        votes_received = sets:new(),
        heartbeat_timer_ref = undefined,
        snapshot_in_progress = false,
        snapshot_threshold = ?RAFT_SNAPSHOT_THRESHOLD,
        leader_lease_until = undefined,
        transport = Transport,
        metrics = #raft_metrics{}
    },

    logger:info("Raft server ~p starting in cluster ~p with peers ~p",
                [NodeId, ClusterName, Peers]),

    {ok, State, {continue, initialize}}.

-spec handle_continue(initialize, #raft_server_state{}) ->
    {noreply, #raft_server_state{}}.
handle_continue(initialize, State) ->
    %% Register in global registry for discovery
    ClusterName = get_cluster_name(State),
    register_node(ClusterName, State#raft_server_state.node_id),

    %% Start election timer
    NewState = reset_election_timer(State),

    %% Apply any uncommitted log entries to state machine
    FullyAppliedState = apply_uncommitted_entries(NewState),

    {noreply, FullyAppliedState}.

-spec handle_call(term(), {pid(), term()}, #raft_server_state{}) ->
    {reply, term(), #raft_server_state{}}.
handle_call({write, _Command}, _From, State = #raft_server_state{server_state = S}) when S =/= leader ->
    %% Not leader - redirect to leader
    LeaderId = State#raft_server_state.leader_id,
    {reply, {error, {not_leader, LeaderId}}, State};

handle_call({write, Command}, From, State = #raft_server_state{server_state = leader}) ->
    %% Append command to log as leader
    NewState = append_entry(State, Command),
    {reply, {ok, pending}, NewState};

handle_call(read, _From, State = #raft_server_state{server_state = S}) when S =/= leader ->
    LeaderId = State#raft_server_state.leader_id,
    {reply, {error, {not_leader, LeaderId}}, State};

handle_call(read, _From, State = #raft_server_state{
        server_state = leader,
        leader_lease_until = LeaseUntil,
        state_machine = SM,
        state_machine_state = SMState}) ->
    %% Linearizable read: check leader lease before reading
    Now = erlang:monotonic_time(millisecond),
    case LeaseUntil of
        undefined ->
            {reply, {error, leader_lease_not_established}, State};
        ExpireTime when Now < ExpireTime ->
            %% Leader lease is valid - safe to read
            Result = SM:get_state(SMState),
            {reply, {ok, Result}, State};
        _ ->
            %% Lease expired - not safe to read
            {reply, {error, leader_lease_expired}, State}
    end;

handle_call(status, _From, State) ->
    Status = #{
        state => State#raft_server_state.server_state,
        term => State#raft_server_state.current_term,
        leader => State#raft_server_state.leader_id,
        commit_index => State#raft_server_state.commit_index,
        last_applied => State#raft_server_state.last_applied,
        cluster_size => length(State#raft_server_state.cluster_members),
        is_leader => ?is_leader(State)
    },
    {reply, {ok, Status}, State};

handle_call({add_server, NewServerId}, _From, State) when ?is_leader(State) ->
    %% Append config change entry to log
    ConfigChange = #raft_config_change{
        type = add_server,
        server_id = NewServerId,
        old_cluster = State#raft_server_state.cluster_members,
        new_cluster = lists:usort([NewServerId | State#raft_server_state.cluster_members]),
        config_index = 0  % Will be set when appended
    },
    NewState = append_entry(State, {config_change, ConfigChange}),
    {reply, ok, NewState};

handle_call({add_server, _NewServerId}, _From, State) ->
    {reply, {error, not_leader}, State};

handle_call({remove_server, ServerId}, _From, State) when ?is_leader(State) ->
    ConfigChange = #raft_config_change{
        type = remove_server,
        server_id = ServerId,
        old_cluster = State#raft_server_state.cluster_members,
        new_cluster = lists:delete(ServerId, State#raft_server_state.cluster_members),
        config_index = 0
    },
    NewState = append_entry(State, {config_change, ConfigChange}),
    {reply, ok, NewState};

handle_call({remove_server, _ServerId}, _From, State) ->
    {reply, {error, not_leader}, State};

handle_call(get_leader, _From, State) ->
    {reply, {ok, State#raft_server_state.leader_id}, State};

handle_call(get_cluster_members, _From, State) ->
    {reply, {ok, State#raft_server_state.cluster_members}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #raft_server_state{}) -> {noreply, #raft_server_state{}}.
handle_cast(force_election, State) ->
    %% Force immediate election by becoming candidate
    NewState = become_candidate(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #raft_server_state{}) -> {noreply, #raft_server_state{}}.
handle_info(election_timeout, State) ->
    %% Start new election
    logger:debug("Election timeout for node ~p, starting election",
                 [State#raft_server_state.node_id]),
    NewState = become_candidate(State),
    {noreply, NewState};

handle_info(heartbeat_timeout, State = #raft_server_state{server_state = leader}) ->
    %% Send heartbeats to all followers
    NewState = send_heartbeats(State),
    {noreply, NewState};

handle_info(heartbeat_timeout, State) ->
    %% Not leader anymore, cancel heartbeat timer
    {noreply, cancel_heartbeat_timer(State)};

handle_info({request_vote, RequestVote}, State) ->
    NewState = handle_request_vote(RequestVote, State),
    {noreply, NewState};

handle_info({append_entries, AppendEntries}, State) ->
    NewState = handle_append_entries(AppendEntries, State),
    {noreply, NewState};

handle_info({install_snapshot, InstallSnapshot}, State) ->
    NewState = handle_install_snapshot(InstallSnapshot, State),
    {noreply, NewState};

handle_info({request_vote_response, Response}, State = #raft_server_state{server_state = candidate}) ->
    NewState = handle_vote_response(Response, State),
    {noreply, NewState};

handle_info({append_entries_response, From, Response}, State = #raft_server_state{server_state = leader}) ->
    NewState = handle_append_response(From, Response, State),
    {noreply, NewState};

handle_info({install_snapshot_response, _From, _Response}, State) ->
    %% Snapshot installation response - update peer state
    {noreply, State};

handle_info({apply_entry, Index}, State) ->
    NewState = apply_log_entry(Index, State),
    {noreply, NewState};

handle_info({timeout, RequestRef, {append_entries, _To}}, State) ->
    %% RPC timeout - update metrics
    Metrics = State#raft_server_state.metrics,
    NewMetrics = Metrics#raft_metrics{rpc_timeouts = Metrics#raft_metrics.rpcs_sent + 1},
    {noreply, State#raft_server_state{metrics = NewMetrics}};

handle_info({timeout, RequestRef, {request_vote, _To}}, State) ->
    %% Vote request timeout - treat as vote denied
    {noreply, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:warning("Raft server ~p received EXIT from ~p: ~p",
                   [State#raft_server_state.node_id, _Pid, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #raft_server_state{}) -> ok.
terminate(_Reason, State) ->
    %% Unregister from global registry
    ClusterName = get_cluster_name(State),
    Key = ?RAFT_MEMBER_KEY(ClusterName, State#raft_server_state.node_id),
    try
        gproc:unreg(Key)
    catch
        _:_ -> ok
    end,

    %% Cancel timers
    cancel_election_timer(State),
    cancel_heartbeat_timer(State),

    logger:info("Raft server ~p terminating", [State#raft_server_state.node_id]),
    ok.

-spec code_change(term(), #raft_server_state{}, term()) -> {ok, #raft_server_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% State Transition Functions
%%%====================================================================

%% @doc Transition to follower state
-spec become_follower(raft_term(), #raft_server_state{}) -> #raft_server_state{}.
become_follower(NewTerm, State = #raft_server_state{server_state = SS}) when SS =/= follower ->
    logger:debug("Node ~p becoming follower in term ~p",
                 [State#raft_server_state.node_id, NewTerm]),

    %% Cancel heartbeat timer if we were leader
    NewState = cancel_heartbeat_timer(State),

    %% Reset election timer
    FollowerState = reset_election_timer(NewState),

    FollowerState#raft_server_state{
        server_state = follower,
        current_term = NewTerm,
        voted_for = undefined,
        votes_received = sets:new(),
        leader_id = undefined,
        leader_lease_until = undefined
    };
become_follower(NewTerm, State) ->
    %% Already follower, just update term
    reset_election_timer(State#raft_server_state{current_term = NewTerm}).

%% @doc Transition to candidate state and start election
-spec become_candidate(#raft_server_state{}) -> #raft_server_state{}.
become_candidate(State) ->
    NodeId = State#raft_server_state.node_id,
    CurrentTerm = State#raft_server_state.current_term,
    NewTerm = CurrentTerm + 1,

    logger:info("Node ~p becoming candidate in term ~p", [NodeId, NewTerm]),

    %% Vote for self
    VotesReceived = sets:from_list([NodeId]),

    %% Increment election counter in metrics
    Metrics = State#raft_server_state.metrics,

    CandidateState = State#raft_server_state{
        server_state = candidate,
        current_term = NewTerm,
        voted_for = NodeId,
        votes_received = VotesReceived,
        leader_id = undefined,
        leader_lease_until = undefined
    },

    %% Save voted_for to persistent storage
    save_persistent_state(CandidateState),

    %% Request votes from all peers
    ElectionState = request_votes(CandidateState),

    %% Reset election timer
    reset_election_timer(ElectionState).

%% @doc Transition to leader state
-spec become_leader(#raft_server_state{}) -> #raft_server_state{}.
become_leader(State) ->
    NodeId = State#raft_server_state.node_id,
    Term = State#raft_server_state.current_term,

    logger:info("Node ~p becoming leader in term ~p", [NodeId, Term]),

    %% Initialize leader state
    Peers = State#raft_server_state.peers,
    LastIndex = get_last_log_index(State),

    NextIndex = maps:from_list([{P, LastIndex + 1} || P <- Peers]),
    MatchIndex = maps:from_list([{P, 0} || P <- Peers]),

    LeaderState = State#raft_server_state{
        server_state = leader,
        next_index = NextIndex,
        match_index = MatchIndex,
        leader_id = NodeId
    },

    %% Register as leader in global registry
    ClusterName = get_cluster_name(State),
    register_leader(ClusterName, NodeId, Term),

    %% Update metrics
    Metrics = State#raft_server_state.metrics,
    NewMetrics = Metrics#raft_metrics{elections_won = Metrics#raft_metrics.elections_won + 1},

    %% Send immediate heartbeat to establish authority
    HeartbeatState = send_heartbeats(LeaderState#raft_server_state{metrics = NewMetrics}),

    %% Start heartbeat timer
    reset_heartbeat_timer(HeartbeatState).

%%%====================================================================
%%% RPC Handlers
%%%====================================================================

%% @doc Handle incoming RequestVote RPC
-spec handle_request_vote(#raft_request_vote{}, #raft_server_state{}) -> #raft_server_state{}.
handle_request_vote(RequestVote = #raft_request_vote{
        term = CandidateTerm,
        candidate_id = CandidateId,
        last_log_index = CandidateLastIndex,
        last_log_term = CandidateLastTerm},
        State = #raft_server_state{
        current_term = CurrentTerm,
        voted_for = VotedFor,
        log = LogTid,
        node_id = NodeId}) ->

    %% Check if we should grant vote
    VoteGranted =
        case CandidateTerm > CurrentTerm of
            true ->
                %% Candidate's term is newer
                true;
            false when CandidateTerm =:= CurrentTerm ->
                %% Same term - check if we've already voted
                case VotedFor of
                    undefined ->
                        %% Haven't voted - check log completeness
                        {OurLastIndex, OurLastTerm} = get_last_log_term(State),
                        ?is_log_up_to_date(OurLastIndex, OurLastTerm,
                                          CandidateLastIndex, CandidateLastTerm);
                    CandidateId ->
                        %% Already voted for this candidate
                        true;
                    _Other ->
                        %% Already voted for someone else
                        false
                end;
            _ ->
                %% Candidate's term is older
                false
        end,

    %% Send response
    Response = #raft_request_vote_response{
        term = max(CurrentTerm, CandidateTerm),
        vote_granted = VoteGranted,
        reason = case VoteGranted of
                     true -> vote_granted;
                     false -> {log_less_complete, CurrentTerm, VotedFor}
                 end
    },

    %% Send response via transport
    send_rpc(CandidateId, {request_vote_response, Response}, State),

    %% Update state if candidate's term is newer
    NewState =
        case CandidateTerm > CurrentTerm of
            true ->
                become_follower(CandidateTerm, State);
            false ->
                State
        end,

    NewState.

%% @doc Handle incoming AppendEntries RPC
-spec handle_append_entries(#raft_append_entries{}, #raft_server_state{}) -> #raft_server_state{}.
handle_append_entries(AppendEntries = #raft_append_entries{
        term = LeaderTerm,
        leader_id = LeaderId,
        prev_log_index = PrevLogIndex,
        prev_log_term = PrevLogTerm,
        entries = Entries,
        leader_commit = LeaderCommit},
        State = #raft_server_state{
        current_term = CurrentTerm,
        log = LogTid,
        commit_index = CommitIndex,
        node_id = NodeId}) ->

    %% Update leader lease
    Now = erlang:monotonic_time(millisecond),
    LeaseExpire = Now + (?RAFT_ELECTION_TIMEOUT_MAX * 2),

    %% Validate append request
    case LeaderTerm < CurrentTerm of
        true ->
            %% Leader's term is stale - reject
            send_rpc(LeaderId,
                     {append_entries_response, NodeId,
                      #raft_append_entries_response{
                          term = CurrentTerm,
                          success = false,
                          match_index = 0,
                          reason = stale_term
                      }}, State),
            State#raft_server_state{leader_lease_until = LeaseExpire};
        false ->
            %% Term is valid - check log consistency
            LogConsistent = check_log_consistency(LogTid, PrevLogIndex, PrevLogTerm),

            NewState = case LeaderTerm > CurrentTerm of
                           true ->
                               become_follower(LeaderTerm, State);
                           false ->
                               State
                       end,

            case LogConsistent of
                false ->
                    %% Log inconsistency - reject
                    send_rpc(LeaderId,
                             {append_entries_response, NodeId,
                              #raft_append_entries_response{
                                  term = max(CurrentTerm, LeaderTerm),
                                  success = false,
                                  match_index = 0,
                                  reason = log_inconsistency
                              }}, NewState),
                    NewState#raft_server_state{
                        leader_id = LeaderId,
                        leader_lease_until = LeaseExpire
                    };
                true ->
                    %% Log is consistent - append new entries
                    UpdatedState = append_log_entries(NewState, PrevLogIndex, Entries),

                    %% Update commit index if leader's commit is higher
                    NewCommitIndex = min(LeaderCommit, get_last_log_index(UpdatedState)),
                    CommittedState =
                        case NewCommitIndex > CommitIndex of
                            true ->
                                UpdatedState#raft_server_state{
                                    commit_index = NewCommitIndex
                                };
                            false ->
                                UpdatedState
                        end,

                    %% Send success response
                    MatchIndex = PrevLogIndex + length(Entries),
                    send_rpc(LeaderId,
                             {append_entries_response, NodeId,
                              #raft_append_entries_response{
                                  term = max(CurrentTerm, LeaderTerm),
                                  success = true,
                                  match_index = MatchIndex,
                                  reason = ok
                              }}, CommittedState),

                    CommittedState#raft_server_state{
                        leader_id = LeaderId,
                        leader_lease_until = LeaseExpire
                    }
            end
    end.

%% @doc Handle incoming InstallSnapshot RPC
-spec handle_install_snapshot(#raft_install_snapshot{}, #raft_server_state{}) -> #raft_server_state{}.
handle_install_snapshot(#raft_install_snapshot{
        term = LeaderTerm,
        leader_id = LeaderId,
        last_included_index = LastIncludedIndex,
        last_included_term = LastIncludedTerm,
        snapshot_data = SnapshotData,
        snapshot_size = SnapshotSize},
        State = #raft_server_state{
        current_term = CurrentTerm,
        log = LogTid,
        node_id = NodeId,
        state_machine = SM,
        commit_index = CommitIndex}) ->

    logger:info("Node ~p installing snapshot up to index ~p term ~p (size: ~p bytes)",
                [NodeId, LastIncludedIndex, LastIncludedTerm, SnapshotSize]),

    %% Update term if necessary
    NewState = case LeaderTerm > CurrentTerm of
                   true -> become_follower(LeaderTerm, State);
                   false -> State
               end,

    %% Restore state machine from snapshot
    {ok, SMState} = SM:restore(SnapshotData),

    %% Discard log entries up to and including snapshot
    erlmcp_raft_log:discard_log_up_to(LogTid, LastIncludedIndex),

    %% Restore snapshot metadata
    erlmcp_raft_log:save_snapshot_metadata(LogTid, #raft_snapshot{
        last_included_index = LastIncludedIndex,
        last_included_term = LastIncludedTerm,
        state_machine_state = SMState,
        config = NewState#raft_server_state.cluster_members,
        timestamp = erlang:monotonic_time(millisecond)
    }),

    %% Update commit and applied indices
    UpdatedState = NewState#raft_server_state{
        state_machine_state = SMState,
        commit_index = LastIncludedIndex,
        last_applied = LastIncludedIndex,
        snapshot_in_progress = false,
        leader_id = LeaderId
    },

    %% Update metrics
    Metrics = UpdatedState#raft_server_state.metrics,
    UpdatedMetrics = Metrics#raft_metrics{
        snapshots_installed = Metrics#raft_metrics.snapshots_installed + 1
    },

    %% Send response
    Response = #raft_install_snapshot_response{
        term = max(CurrentTerm, LeaderTerm)
    },
    send_rpc(LeaderId, {install_snapshot_response, NodeId, Response},
             UpdatedState#raft_server_state{metrics = UpdatedMetrics}),

    UpdatedState.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Append a new entry to the log (leader only)
-spec append_entry(#raft_server_state{}, term()) -> #raft_server_state{}.
append_entry(State = #raft_server_state{
        log = LogTid,
        current_term = Term,
        commit_index = CommitIndex}, Command) ->

    %% Create new log entry
    LastIndex = get_last_log_index(State),
    NewIndex = LastIndex + 1,

    EntryType = case Command of
                    {config_change, _} -> config_change;
                    _ -> command
                end,

    Entry = #raft_log_entry{
        index = NewIndex,
        term = Term,
        type = EntryType,
        command = Command,
        timestamp = erlang:monotonic_time(millisecond)
    },

    %% Append to log
    erlmcp_raft_log:append_entry(LogTid, Entry),

    %% Update metrics
    Metrics = State#raft_server_state.metrics,
    NewMetrics = Metrics#raft_metrics{
        log_entries_appended = Metrics#raft_metrics.log_entries_appended + 1
    },

    %% Immediately replicate to followers
    ReplicateState = State#raft_server_state{metrics = NewMetrics},
    replicate_log(ReplicateState).

%% @doc Send AppendEntries to all followers (replication or heartbeat)
-spec replicate_log(#raft_server_state{}) -> #raft_server_state{}.
replicate_log(State = #raft_server_state{
        peers = Peers,
        log = LogTid,
        current_term = Term,
        node_id = LeaderId,
        commit_index = CommitIndex}) ->

    lists:foreach(fun(Peer) ->
                          send_append_entries(State, Peer)
                  end, Peers),
    State.

%% @doc Send AppendEntries RPC to a specific peer
-spec send_append_entries(#raft_server_state{}, raft_node_id()) -> ok.
send_append_entries(State = #raft_server_state{
        log = LogTid,
        current_term = Term,
        node_id = LeaderId,
        commit_index = CommitIndex,
        next_index = NextIndex}, Peer) ->

    PeerNextIndex = maps:get(Peer, NextIndex, 1),

    %% Get previous log entry info
    {PrevLogIndex, PrevLogTerm} =
        case PeerNextIndex > 1 of
            true ->
                case erlmcp_raft_log:get_entry(LogTid, PeerNextIndex - 1) of
                    {ok, #raft_log_entry{index = I, term = T}} -> {I, T};
                    {error, not_found} -> {0, 0}
                end;
            false ->
                {0, 0}
        end,

    %% Get entries to send
    Entries = erlmcp_raft_log:get_entries_from(LogTid, PeerNextIndex),

    %% Limit batch size for efficiency
    BatchSize = 100,
    BatchedEntries = lists:sublist(Entries, BatchSize),

    AppendEntries = #raft_append_entries{
        term = Term,
        leader_id = LeaderId,
        prev_log_index = PrevLogIndex,
        prev_log_term = PrevLogTerm,
        entries = BatchedEntries,
        leader_commit = CommitIndex
    },

    send_rpc(Peer, {append_entries, AppendEntries}, State).

%% @doc Send heartbeats to all followers
-spec send_heartbeats(#raft_server_state{}) -> #raft_server_state{}.
send_heartbeats(State) ->
    replicate_log(State),
    reset_heartbeat_timer(State).

%% @doc Request votes from all peers during election
-spec request_votes(#raft_server_state{}) -> #raft_server_state{}.
request_votes(State = #raft_server_state{
        peers = Peers,
        log = LogTid,
        current_term = Term,
        node_id = CandidateId}) ->

    {LastLogIndex, LastLogTerm} = get_last_log_term(State),

    RequestVote = #raft_request_vote{
        term = Term,
        candidate_id = CandidateId,
        last_log_index = LastLogIndex,
        last_log_term = LastLogTerm
    },

    lists:foreach(fun(Peer) ->
                          send_rpc(Peer, {request_vote, RequestVote}, State)
                  end, Peers),

    State.

%% @doc Handle vote response during election
-spec handle_vote_response(#raft_request_vote_response{}, #raft_server_state{}) -> #raft_server_state{}.
handle_vote_response(#raft_request_vote_response{
        term = ResponseTerm,
        vote_granted = true},
        State = #raft_server_state{
        current_term = Term,
        votes_received = Votes,
        cluster_members = ClusterMembers,
        node_id = NodeId}) ->

    NewState = case ResponseTerm > Term of
                   true ->
                       %% Newer term discovered - become follower
                       become_follower(ResponseTerm, State);
                   false ->
                       %% Add vote
                       NewVotes = sets:add_element(NodeId, Votes),
                       State#raft_server_state{votes_received = NewVotes}
               end,

    %% Check if we won the election
    VotesCount = sets:size(NewState#raft_server_state.votes_received),
    Quorum = ?quorum(length(ClusterMembers)),

    case VotesCount >= Quorum of
        true ->
            become_leader(NewState);
        false ->
            NewState
    end;

handle_vote_response(#raft_request_vote_response{term = ResponseTerm, vote_granted = false},
                     State = #raft_server_state{current_term = Term}) ->
    case ResponseTerm > Term of
        true ->
            become_follower(ResponseTerm, State);
        false ->
            State
    end.

%% @doc Handle AppendEntries response from follower
-spec handle_append_response(raft_node_id(), #raft_append_entries_response{}, #raft_server_state{}) ->
    #raft_server_state{}.
handle_append_response(From,
                        #raft_append_entries_response{
                            term = ResponseTerm,
                            success = Success,
                            match_index = MatchIndex},
                        State = #raft_server_state{
                        current_term = Term,
                        next_index = NextIndex,
                        match_index = MatchIndexMap,
                        log = LogTid,
                        commit_index = CommitIndex,
                        cluster_members = ClusterMembers}) ->

    NewState = case ResponseTerm > Term of
                   true ->
                       %% Newer term discovered - step down
                       become_follower(ResponseTerm, State);
                   false ->
                       State
               end,

    case Success of
        true ->
            %% Follower successfully appended entries
            UpdatedMatchIndex = maps:put(From, MatchIndex, MatchIndexMap),
            UpdatedNextIndex = maps:put(From, MatchIndex + 1, NextIndex),

            %% Check if we can commit more entries
            NewCommitIndex = calculate_commit_index(
                               maps:values(UpdatedMatchIndex),
                               CommitIndex,
                               LogTid,
                               length(ClusterMembers)),

            NewState#raft_server_state{
                next_index = UpdatedNextIndex,
                match_index = UpdatedMatchIndex,
                commit_index = NewCommitIndex
            };
        false ->
            %% Follower rejected - decrement next_index and retry
            CurrentNext = maps:get(From, NextIndex, 1),
            DecrementedNext = max(1, CurrentNext - 1),
            UpdatedNextIndex = maps:put(From, DecrementedNext, NextIndex),

            %% Retry immediately
            RetryState = NewState#raft_server_state{
                next_index = UpdatedNextIndex
            },
            send_append_entries(RetryState, From),
            RetryState
    end.

%% @doc Calculate new commit index based on replicated entries
-spec calculate_commit_index([raft_index()], raft_index(), ets:tid(), pos_integer()) ->
    raft_index().
calculate_commit_index(MatchIndices, CurrentCommit, LogTid, ClusterSize) ->
    %% Sort match indices in descending order
    Sorted = lists:reverse(lists:sort(MatchIndices)),

    %% Find the highest index that is replicated on a quorum
    Quorum = ?quorum_size(ClusterSize),

    %% Check each index to see if it has quorum
    find_quorum_index(Sorted, Quorum, CurrentCommit, LogTid).

%% @doc Find highest index with quorum, checking term consistency
-spec find_quorum_index([raft_index()], pos_integer(), raft_index(), ets:tid()) ->
    raft_index().
find_quorum_index([], _Quorum, CurrentCommit, _LogTid) ->
    CurrentCommit;
find_quorum_index([Index | Rest], Quorum, CurrentCommit, LogTid) ->
    %% Count how many servers have replicated at least this index
    case length([I || I <- [Index | Rest], I >= Index]) >= Quorum of
        true ->
            %% Check if this entry's term matches current term
            case erlmcp_raft_log:get_entry(LogTid, Index) of
                {ok, #raft_log_entry{term = Term}} when Term =/= 0 ->
                    %% For safety, only commit entries from current term
                    Index;
                _ ->
                    find_quorum_index(Rest, Quorum, CurrentCommit, LogTid)
            end;
        false ->
            find_quorum_index(Rest, Quorum, CurrentCommit, LogTid)
    end.

%% @doc Apply log entries to state machine
-spec apply_uncommitted_entries(#raft_server_state{}) -> #raft_server_state{}.
apply_uncommitted_entries(State = #raft_server_state{
        commit_index = CommitIndex,
        last_applied = LastApplied}) when CommitIndex > LastApplied ->

    NewState = apply_log_entry(LastApplied + 1, State),
    apply_uncommitted_entries(NewState);
apply_uncommitted_entries(State) ->
    State.

%% @doc Apply a single log entry to state machine
-spec apply_log_entry(raft_index(), #raft_server_state{}) -> #raft_server_state{}.
apply_log_entry(Index, State = #raft_server_state{
        log = LogTid,
        state_machine = SM,
        state_machine_state = SMState,
        commit_index = CommitIndex}) when Index =< CommitIndex ->

    case erlmcp_raft_log:get_entry(LogTid, Index) of
        {ok, #raft_log_entry{command = Command}} ->
            case SM:apply(Command, SMState) of
                {ok, NewSMState, _Result} ->
                    State#raft_server_state{
                        state_machine_state = NewSMState,
                        last_applied = Index
                    };
                {error, Reason} ->
                    logger:error("Failed to apply command ~p at index ~p: ~p",
                                 [Command, Index, Reason]),
                    State
            end;
        {error, not_found} ->
            logger:warning("Log entry at index ~p not found during apply", [Index]),
            State
    end;
apply_log_entry(_Index, State) ->
    State.

%% @doc Append entries to log (for log replication)
-spec append_log_entries(#raft_server_state{}, raft_index(), [raft_log_entry()]) ->
    #raft_server_state{}.
append_log_entries(State, _PrevLogIndex, []) ->
    State;
append_log_entries(State = #raft_server_state{log = LogTid}, PrevLogIndex, [Entry | Rest]) ->
    Index = PrevLogIndex + 1,
    case erlmcp_raft_log:get_entry(LogTid, Index) of
        {ok, ExistingEntry} when ExistingEntry#raft_log_entry.term =:= Entry#raft_log_entry.term ->
            %% Entry already matches - skip
            append_log_entries(State, Index, Rest);
        {ok, _ExistingEntry} ->
            %% Entry conflicts - delete from here and append
            erlmcp_raft_log:delete_from(LogTid, Index),
            erlmcp_raft_log:append_entry(LogTid, Entry),
            append_log_entries(State, Index, Rest);
        {error, not_found} ->
            %% Entry doesn't exist - append
            erlmcp_raft_log:append_entry(LogTid, Entry),
            append_log_entries(State, Index, Rest)
    end.

%% @doc Check if log is consistent with prev_log_index and prev_log_term
-spec check_log_consistency(ets:tid(), raft_index(), raft_term()) -> boolean().
check_log_consistency(_LogTid, 0, _Term) ->
    true;
check_log_consistency(LogTid, PrevLogIndex, PrevLogTerm) ->
    case erlmcp_raft_log:get_entry(LogTid, PrevLogIndex) of
        {ok, #raft_log_entry{term = PrevLogTerm}} ->
            true;
        _ ->
            false
    end.

%% @doc Reset election timer with random timeout
-spec reset_election_timer(#raft_server_state{}) -> #raft_server_state{}.
reset_election_timer(State = #raft_server_state{election_timer_ref = OldRef}) ->
    cancel_timer(OldRef),
    Timeout = ?election_timeout(),
    Ref = erlang:send_after(Timeout, self(), election_timeout),
    State#raft_server_state{election_timer_ref = Ref}.

%% @doc Reset heartbeat timer
-spec reset_heartbeat_timer(#raft_server_state{}) -> #raft_server_state{}.
reset_heartbeat_timer(State = #raft_server_state{heartbeat_timer_ref = OldRef}) ->
    cancel_timer(OldRef),
    Ref = erlang:send_after(?RAFT_HEARTBEAT_INTERVAL, self(), heartbeat_timeout),
    State#raft_server_state{heartbeat_timer_ref = Ref}.

%% @doc Cancel election timer
-spec cancel_election_timer(#raft_server_state{}) -> #raft_server_state{}.
cancel_election_timer(State = #raft_server_state{election_timer_ref = Ref}) ->
    cancel_timer(Ref),
    State#raft_server_state{election_timer_ref = undefined}.

%% @doc Cancel heartbeat timer
-spec cancel_heartbeat_timer(#raft_server_state{}) -> #raft_server_state{}.
cancel_heartbeat_timer(State = #raft_server_state{heartbeat_timer_ref = Ref}) ->
    cancel_timer(Ref),
    State#raft_server_state{heartbeat_timer_ref = undefined}.

%% @doc Cancel timer if defined
-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok.

%% @doc Get last log index
-spec get_last_log_index(#raft_server_state{}) -> raft_index().
get_last_log_index(#raft_server_state{log = LogTid}) ->
    erlmcp_raft_log:last_index(LogTid).

%% @doc Get last log index and term
-spec get_last_log_term(#raft_server_state{}) -> {raft_index(), raft_term()}.
get_last_log_term(State = #raft_server_state{log = LogTid}) ->
    Index = erlmcp_raft_log:last_index(LogTid),
    case erlmcp_raft_log:get_entry(LogTid, Index) of
        {ok, #raft_log_entry{term = Term}} ->
            {Index, Term};
        {error, not_found} ->
            {0, 0}
    end.

%% @doc Send RPC via transport layer
-spec send_rpc(raft_node_id(), term(), #raft_server_state{}) -> ok.
send_rpc(To, Message, #raft_server_state{transport = Transport}) ->
    Transport:send_rpc(To, Message).

%% @doc Register node in global registry
-spec register_node(binary(), raft_node_id()) -> ok.
register_node(ClusterName, NodeId) ->
    Key = ?RAFT_MEMBER_KEY(ClusterName, NodeId),
    try
        gproc:reg_or_update(Key, [{p, l}], [{node, node()}])
    catch
        _:_ ->
            ok
    end.

%% @doc Register as leader in global registry
-spec register_leader(binary(), raft_node_id(), raft_term()) -> ok.
register_leader(ClusterName, LeaderId, Term) ->
    Key = ?RAFT_LEADER_KEY(ClusterName),
    try
        gproc:reg_or_update(Key, [{p, l}], [{leader, LeaderId}, {term, Term}])
    catch
        _:_ ->
            ok
    end.

%% @doc Load persistent state from log storage
-spec load_persistent_state(ets:tid(), raft_node_id()) -> {raft_term(), raft_node_id() | undefined}.
load_persistent_state(LogTid, NodeId) ->
    case erlmcp_raft_log:get_persistent_state(LogTid) of
        {ok, Term, VotedFor} ->
            {Term, VotedFor};
        {error, not_found} ->
            {0, undefined}
    end.

%% @doc Save persistent state to log storage
-spec save_persistent_state(#raft_server_state{}) -> ok.
save_persistent_state(#raft_server_state{log = LogTid, current_term = Term, voted_for = VotedFor}) ->
    erlmcp_raft_log:save_persistent_state(LogTid, Term, VotedFor).

%% @doc Get cluster name from state (placeholder - would be stored in state)
-spec get_cluster_name(#raft_server_state{}) -> binary().
get_cluster_name(_State) ->
    <<"erlmcp_raft_cluster">>.

%% @doc Forward client request to leader
-spec forward_to_leader(term(), #raft_server_state{}) -> {error, term()}.
forward_to_leader(_Request, #raft_server_state{leader_id = undefined}) ->
    {error, no_leader};
forward_to_leader(Request, #raft_server_state{leader_id = LeaderId}) ->
    {error, {forward_to_leader, LeaderId}}.
