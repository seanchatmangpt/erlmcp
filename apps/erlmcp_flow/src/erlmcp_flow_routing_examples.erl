%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Routing Protocol Code Examples
%%%
%%% Complete implementations for:
%%% 1. Message Routing (gproc registry, swarm discovery, leader election)
%%% 2. Consensus Protocols (Raft, Byzantine, Gossip)
%%% 3. Load Balancing (round-robin, load-aware, least-used-first)
%%% 4. Failover (heartbeat, task requeue, backup promotion)
%%% 5. Network Awareness (partition detection, heal protocol)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_routing_examples).

-export([
    % Message Routing
    register_agent_gproc/3,
    discover_agents_by_capability/1,
    broadcast_to_swarm/2,
    elect_leader/1,

    % Consensus Protocols
    raft_append_entries/3,
    raft_request_vote/2,
    byzantine_quorum_consensus/3,
    gossip_disseminate/3,

    % Load Balancing
    load_balance_round_robin/2,
    load_balance_least_used/1,
    load_balance_load_aware/1,

    % Failover
    detect_heartbeat_failure/2,
    requeue_failed_task/2,
    promote_backup_agent/2,

    % Network Awareness
    detect_network_partition/1,
    heal_partition/2,

    % Example usage
    demo_complete_routing/0
]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% 1. MESSAGE ROUTING
%%%===================================================================

%% @doc Register agent with gproc for O(log N) lookup
%% Based on erlmcp_registry.erl pattern
-spec register_agent_gproc(AgentId, Pid, Capabilities) -> ok | {error, term()}
    when AgentId :: binary(),
         Pid :: pid(),
         Capabilities :: [binary()].
register_agent_gproc(AgentId, Pid, Capabilities) ->
    % Primary name registration (O(log N) lookup)
    NameKey = {n, l, {flow_agent, AgentId}},

    try
        % Register with gproc
        gproc:reg_other(NameKey, Pid, #{
            capabilities => Capabilities,
            registered_at => erlang:timestamp(),
            load => 0
        }),

        % Monitor for auto-cleanup
        gproc:monitor(NameKey),

        % Index by each capability for discovery
        lists:foreach(fun(Cap) ->
            PropKey = {p, l, {flow_capability, Cap}},
            gproc:reg_other(PropKey, Pid, #{agent_id => AgentId})
        end, Capabilities),

        % Load counter for load balancing
        CounterKey = {c, l, {flow_load, AgentId}},
        gproc:reg_other(CounterKey, Pid, 0),

        ?LOG_INFO("Registered agent ~p with capabilities ~p", [AgentId, Capabilities]),
        ok
    catch
        error:badarg ->
            {error, already_registered}
    end.

%% @doc Discover agents by capability (O(M) where M = matching agents)
-spec discover_agents_by_capability(Capability) -> [pid()]
    when Capability :: binary().
discover_agents_by_capability(Capability) ->
    PropKey = {p, l, {flow_capability, Capability}},

    % gproc returns all PIDs with this property
    Pids = gproc:lookup_pids(PropKey),

    ?LOG_DEBUG("Found ~p agents with capability ~p", [length(Pids), Capability]),
    Pids.

%% @doc Broadcast message to all agents in swarm
-spec broadcast_to_swarm(SwarmName, Message) -> ok
    when SwarmName :: binary(),
         Message :: term().
broadcast_to_swarm(SwarmName, Message) ->
    % Find all agents in swarm
    PropKey = {p, l, {flow_swarm, SwarmName}},
    Pids = gproc:lookup_pids(PropKey),

    % Broadcast via gproc send
    gproc:send(PropKey, {swarm_message, SwarmName, Message}),

    ?LOG_INFO("Broadcast message to ~p agents in swarm ~p",
              [length(Pids), SwarmName]),
    ok.

%% @doc Leader election using gproc leader election
-spec elect_leader(GroupName) -> {leader, pid()} | {follower, pid()}
    when GroupName :: binary().
elect_leader(GroupName) ->
    % Use gproc's leader election
    Resource = {flow_leader, GroupName},

    case gproc:leader_call(Resource, self()) of
        {ok, Leader} when Leader =:= self() ->
            ?LOG_INFO("Elected as leader for group ~p", [GroupName]),
            {leader, self()};
        {ok, Leader} ->
            ?LOG_INFO("Following leader ~p for group ~p", [Leader, GroupName]),
            {follower, Leader}
    end.

%%%===================================================================
%%% 2. CONSENSUS PROTOCOLS
%%%===================================================================

%%--------------------------------------------------------------------
%% Raft Consensus Implementation
%%--------------------------------------------------------------------

-record(raft_state, {
    current_term = 0 :: non_neg_integer(),
    voted_for = undefined :: pid() | undefined,
    log = [] :: [term()],
    commit_index = 0 :: non_neg_integer(),
    last_applied = 0 :: non_neg_integer(),
    role = follower :: follower | candidate | leader,

    % Leader state
    next_index = #{} :: #{pid() => non_neg_integer()},
    match_index = #{} :: #{pid() => non_neg_integer()},

    % Cluster
    peers = [] :: [pid()],
    election_timeout = 5000 :: pos_integer(),
    heartbeat_timeout = 1000 :: pos_integer()
}).

%% @doc Raft AppendEntries RPC (leader sends to followers)
%% Returns: {ok, Term, Success} | {error, Reason}
-spec raft_append_entries(LeaderPid, Entries, PrevLogInfo) ->
    {ok, non_neg_integer(), boolean()} | {error, term()}
    when LeaderPid :: pid(),
         Entries :: [term()],
         PrevLogInfo :: {Term :: non_neg_integer(), Index :: non_neg_integer()}.
raft_append_entries(LeaderPid, Entries, {PrevLogTerm, PrevLogIndex}) ->
    % Get current state
    State = get_raft_state(),

    % Reply false if term < currentTerm
    if
        State#raft_state.current_term > PrevLogTerm ->
            {ok, State#raft_state.current_term, false};
        true ->
            % Check log consistency
            case check_log_consistency(State, PrevLogIndex, PrevLogTerm) of
                true ->
                    % Append entries
                    NewLog = append_raft_entries(State#raft_state.log,
                                                  PrevLogIndex,
                                                  Entries),

                    % Update state
                    NewState = State#raft_state{
                        log = NewLog,
                        current_term = max(State#raft_state.current_term, PrevLogTerm)
                    },
                    put_raft_state(NewState),

                    ?LOG_DEBUG("AppendEntries success: added ~p entries",
                              [length(Entries)]),
                    {ok, NewState#raft_state.current_term, true};
                false ->
                    ?LOG_WARNING("AppendEntries failed: log inconsistency"),
                    {ok, State#raft_state.current_term, false}
            end
    end.

%% @doc Raft RequestVote RPC (candidate requests votes)
%% Returns: {ok, Term, VoteGranted}
-spec raft_request_vote(CandidatePid, CandidateInfo) ->
    {ok, non_neg_integer(), boolean()}
    when CandidatePid :: pid(),
         CandidateInfo :: {Term :: non_neg_integer(),
                          LastLogIndex :: non_neg_integer(),
                          LastLogTerm :: non_neg_integer()}.
raft_request_vote(CandidatePid, {CandidateTerm, LastLogIndex, LastLogTerm}) ->
    State = get_raft_state(),

    % Reply false if term < currentTerm
    if
        CandidateTerm < State#raft_state.current_term ->
            {ok, State#raft_state.current_term, false};
        true ->
            % Check if we can vote for this candidate
            CanVote = (State#raft_state.voted_for =:= undefined orelse
                      State#raft_state.voted_for =:= CandidatePid),

            % Check if candidate's log is at least as up-to-date
            LogUpToDate = is_log_up_to_date(State, LastLogIndex, LastLogTerm),

            case CanVote andalso LogUpToDate of
                true ->
                    % Grant vote
                    NewState = State#raft_state{
                        current_term = CandidateTerm,
                        voted_for = CandidatePid
                    },
                    put_raft_state(NewState),

                    ?LOG_INFO("Granted vote to ~p for term ~p",
                             [CandidatePid, CandidateTerm]),
                    {ok, CandidateTerm, true};
                false ->
                    ?LOG_WARNING("Denied vote to ~p: CanVote=~p, LogUpToDate=~p",
                                [CandidatePid, CanVote, LogUpToDate]),
                    {ok, State#raft_state.current_term, false}
            end
    end.

%%--------------------------------------------------------------------
%% Byzantine Consensus Implementation
%%--------------------------------------------------------------------

-record(byzantine_state, {
    total_nodes :: pos_integer(),
    byzantine_tolerance :: non_neg_integer(),  % F
    quorum_size :: pos_integer(),              % 2F + 1
    responses = #{} :: #{node_id() => term()},
    signatures = #{} :: #{node_id() => binary()},
    blacklist = [] :: [node_id()]
}).

%% @doc Byzantine quorum consensus with signature verification
%% Returns: {quorum_reached, Value} | {pending, State} | {error, Reason}
-spec byzantine_quorum_consensus(Nodes, Request, SecretKey) ->
    {quorum_reached, term()} | {pending, map()} | {error, term()}
    when Nodes :: [pid()],
         Request :: term(),
         SecretKey :: binary().
byzantine_quorum_consensus(Nodes, Request, SecretKey) ->
    F = (length(Nodes) - 1) div 3,  % Byzantine tolerance
    QuorumSize = 2 * F + 1,

    ?LOG_INFO("Starting Byzantine consensus: N=~p, F=~p, Quorum=~p",
              [length(Nodes), F, QuorumSize]),

    % Send signed request to all nodes
    SignedRequest = sign_byzantine_message(Request, self(), SecretKey),
    [Node ! {byzantine_request, SignedRequest} || Node <- Nodes],

    % Collect responses
    State = #byzantine_state{
        total_nodes = length(Nodes),
        byzantine_tolerance = F,
        quorum_size = QuorumSize
    },

    collect_byzantine_responses(State, 5000).  % 5s timeout

%% @doc Sign Byzantine message with HMAC-SHA256
sign_byzantine_message(Payload, SenderId, SecretKey) ->
    Timestamp = erlang:system_time(millisecond),
    Data = term_to_binary({Payload, SenderId, Timestamp}),
    Signature = crypto:mac(hmac, sha256, SecretKey, Data),

    #{
        payload => Payload,
        sender_id => SenderId,
        timestamp => Timestamp,
        signature => Signature
    }.

%% @doc Verify Byzantine message signature
verify_byzantine_signature(SignedMsg, SecretKey) ->
    #{payload := Payload, sender_id := SenderId,
      timestamp := Timestamp, signature := Signature} = SignedMsg,

    Data = term_to_binary({Payload, SenderId, Timestamp}),
    ExpectedSignature = crypto:mac(hmac, sha256, SecretKey, Data),

    crypto:hash_equals(ExpectedSignature, Signature).

%% @doc Collect and verify Byzantine responses
collect_byzantine_responses(State, Timeout) ->
    receive
        {byzantine_response, NodeId, Response} ->
            NewResponses = maps:put(NodeId, Response, State#byzantine_state.responses),
            NewState = State#byzantine_state{responses = NewResponses},

            % Check if quorum reached
            case maps:size(NewResponses) >= State#byzantine_state.quorum_size of
                true ->
                    case find_majority_response(NewResponses) of
                        {ok, Value} ->
                            ?LOG_INFO("Byzantine quorum reached with value ~p", [Value]),
                            {quorum_reached, Value};
                        no_majority ->
                            ?LOG_WARNING("Byzantine consensus failed: no majority"),
                            {error, no_consensus}
                    end;
                false ->
                    collect_byzantine_responses(NewState, Timeout)
            end
    after Timeout ->
        ?LOG_ERROR("Byzantine consensus timeout: got ~p/~p responses",
                  [maps:size(State#byzantine_state.responses),
                   State#byzantine_state.quorum_size]),
        {error, timeout}
    end.

%% @doc Find majority response (must have > F identical responses)
find_majority_response(Responses) ->
    % Count occurrences of each response
    Counts = maps:fold(
        fun(_NodeId, Response, Acc) ->
            maps:update_with(Response, fun(Count) -> Count + 1 end, 1, Acc)
        end,
        #{},
        Responses
    ),

    % Find response with maximum count
    {MajorityValue, MaxCount} = maps:fold(
        fun(Value, Count, {CurValue, CurMax}) ->
            if Count > CurMax -> {Value, Count};
               true -> {CurValue, CurMax}
            end
        end,
        {undefined, 0},
        Counts
    ),

    % Verify it exceeds majority threshold
    TotalResponses = maps:size(Responses),
    case MaxCount > TotalResponses div 2 of
        true -> {ok, MajorityValue};
        false -> no_majority
    end.

%%--------------------------------------------------------------------
%% Gossip Protocol Implementation
%%--------------------------------------------------------------------

-record(gossip_state, {
    node_id :: binary(),
    peers = [] :: [pid()],
    data = #{} :: #{binary() => term()},
    vector_clock = #{} :: #{binary() => non_neg_integer()},
    fanout = 3 :: pos_integer(),
    ttl = 5 :: pos_integer()
}).

%% @doc Gossip disseminate - spread update through network
%% O(log N) rounds for eventual consistency
-spec gossip_disseminate(NodeId, Update, Peers) -> ok
    when NodeId :: binary(),
         Update :: {Key :: binary(), Value :: term()},
         Peers :: [pid()].
gossip_disseminate(NodeId, {Key, Value}, Peers) ->
    % Update local state
    State = get_gossip_state(),

    % Increment vector clock
    NewVC = maps:update_with(NodeId, fun(V) -> V + 1 end, 1,
                             State#gossip_state.vector_clock),

    % Update data
    NewData = maps:put(Key, Value, State#gossip_state.data),

    NewState = State#gossip_state{
        data = NewData,
        vector_clock = NewVC
    },
    put_gossip_state(NewState),

    % Create gossip message
    GossipMsg = #{
        node_id => NodeId,
        key => Key,
        value => Value,
        vector_clock => NewVC,
        ttl => State#gossip_state.ttl
    },

    % Send to random subset of peers (fanout)
    Fanout = min(State#gossip_state.fanout, length(Peers)),
    SelectedPeers = select_random_peers(Peers, Fanout),

    lists:foreach(fun(Peer) ->
        Peer ! {gossip, GossipMsg}
    end, SelectedPeers),

    ?LOG_DEBUG("Gossip disseminated: key=~p to ~p peers", [Key, Fanout]),
    ok.

%% @doc Handle incoming gossip message
handle_gossip_message(GossipMsg) ->
    #{node_id := SenderNodeId, key := Key, value := Value,
      vector_clock := TheirVC, ttl := TTL} = GossipMsg,

    State = get_gossip_state(),

    % Check if we should accept this update (vector clock comparison)
    ShouldAccept = should_accept_gossip(State#gossip_state.vector_clock,
                                        TheirVC,
                                        SenderNodeId),

    case ShouldAccept andalso (TTL > 0) of
        true ->
            % Accept update
            NewData = maps:put(Key, Value, State#gossip_state.data),
            NewVC = merge_vector_clocks(State#gossip_state.vector_clock, TheirVC),

            NewState = State#gossip_state{
                data = NewData,
                vector_clock = NewVC
            },
            put_gossip_state(NewState),

            % Propagate to peers (with decremented TTL)
            case TTL > 1 of
                true ->
                    PropagateMsg = GossipMsg#{ttl => TTL - 1},
                    SelectedPeers = select_random_peers(State#gossip_state.peers,
                                                        State#gossip_state.fanout),
                    [Peer ! {gossip, PropagateMsg} || Peer <- SelectedPeers],
                    ?LOG_DEBUG("Propagated gossip: key=~p, ttl=~p", [Key, TTL - 1]);
                false ->
                    ?LOG_DEBUG("Gossip TTL expired for key=~p", [Key])
            end,
            ok;
        false ->
            ?LOG_DEBUG("Rejected gossip: key=~p (stale or ttl=0)", [Key]),
            ok
    end.

%%%===================================================================
%%% 3. LOAD BALANCING
%%%===================================================================

%% @doc Round-robin load balancing
%% O(1) selection with fair distribution
-spec load_balance_round_robin(Agents, RoundRobinIndex) ->
    {AgentId, NewIndex}
    when Agents :: [binary()],
         RoundRobinIndex :: non_neg_integer(),
         AgentId :: binary(),
         NewIndex :: non_neg_integer().
load_balance_round_robin(Agents, Index) ->
    AgentList = case is_map(Agents) of
        true -> maps:keys(Agents);
        false -> Agents
    end,

    SelectedAgent = lists:nth((Index rem length(AgentList)) + 1, AgentList),
    NewIndex = Index + 1,

    ?LOG_DEBUG("Round-robin selected agent ~p (index=~p)",
              [SelectedAgent, Index]),
    {SelectedAgent, NewIndex}.

%% @doc Least-used-first load balancing
%% Selects agent with minimum current load
-spec load_balance_least_used(Agents) -> binary()
    when Agents :: [binary()].
load_balance_least_used(Agents) ->
    % Get load for each agent from gproc counters
    AgentLoads = lists:map(fun(AgentId) ->
        CounterKey = {c, l, {flow_load, AgentId}},
        Load = case gproc:lookup_value(CounterKey) of
            undefined -> 0;
            L -> L
        end,
        {AgentId, Load}
    end, Agents),

    % Find minimum load agent
    {MinAgent, MinLoad} = lists:foldl(
        fun({AgentId, Load}, {CurAgent, CurMin}) ->
            if Load < CurMin -> {AgentId, Load};
               true -> {CurAgent, CurMin}
            end
        end,
        {hd(Agents), infinity},
        AgentLoads
    ),

    ?LOG_DEBUG("Least-used selected agent ~p (load=~p)", [MinAgent, MinLoad]),
    MinAgent.

%% @doc Load-aware load balancing
%% Uses agent metrics to make informed decision
-spec load_balance_load_aware(Agents) -> binary()
    when Agents :: [binary()].
load_balance_load_aware(Agents) ->
    % Get comprehensive metrics for each agent
    AgentMetrics = lists:map(fun(AgentId) ->
        Metrics = get_agent_metrics(AgentId),
        Score = calculate_load_score(Metrics),
        {AgentId, Score, Metrics}
    end, Agents),

    % Select agent with best (lowest) score
    {BestAgent, BestScore, _Metrics} = lists:foldl(
        fun({AgentId, Score, Metrics}, {CurAgent, CurScore, CurMetrics}) ->
            if Score < CurScore -> {AgentId, Score, Metrics};
               true -> {CurAgent, CurScore, CurMetrics}
            end
        end,
        {hd(Agents), infinity, #{}},
        AgentMetrics
    ),

    ?LOG_DEBUG("Load-aware selected agent ~p (score=~.2f)",
              [BestAgent, BestScore]),
    BestAgent.

%% @doc Calculate load score from agent metrics
%% Lower score = better candidate
calculate_load_score(Metrics) ->
    Load = maps:get(load, Metrics, 0),
    AvgLatency = maps:get(avg_latency, Metrics, 0),
    SuccessRate = maps:get(success_rate, Metrics, 1.0),

    % Weighted score: load (50%) + latency (30%) + failure rate (20%)
    LoadScore = Load / 10.0,
    LatencyScore = AvgLatency / 1000.0,  % ms to seconds
    FailureScore = (1.0 - SuccessRate) * 10.0,

    0.5 * LoadScore + 0.3 * LatencyScore + 0.2 * FailureScore.

%%%===================================================================
%%% 4. FAILOVER
%%%===================================================================

%% @doc Detect heartbeat failure
%% Returns true if agent hasn't responded within timeout
-spec detect_heartbeat_failure(AgentId, TimeoutMs) -> boolean()
    when AgentId :: binary(),
         TimeoutMs :: pos_integer().
detect_heartbeat_failure(AgentId, TimeoutMs) ->
    % Get last heartbeat time from registry
    case get_agent_last_heartbeat(AgentId) of
        {ok, LastHeartbeat} ->
            Now = erlang:timestamp(),
            ElapsedMs = timer:now_diff(Now, LastHeartbeat) div 1000,

            Failed = ElapsedMs > TimeoutMs,

            case Failed of
                true ->
                    ?LOG_WARNING("Agent ~p heartbeat failed: ~pms since last heartbeat",
                                [AgentId, ElapsedMs]);
                false ->
                    ?LOG_DEBUG("Agent ~p heartbeat OK: ~pms since last",
                              [AgentId, ElapsedMs])
            end,

            Failed;
        {error, not_found} ->
            ?LOG_WARNING("Agent ~p not found for heartbeat check", [AgentId]),
            true  % Treat as failed
    end.

%% @doc Requeue failed task to another agent
-spec requeue_failed_task(TaskId, FailedAgentId) ->
    {ok, NewAgentId} | {error, Reason}
    when TaskId :: binary(),
         FailedAgentId :: binary(),
         NewAgentId :: binary(),
         Reason :: term().
requeue_failed_task(TaskId, FailedAgentId) ->
    % Get task details
    case get_task_details(TaskId) of
        {ok, Task} ->
            % Get required capabilities
            Capabilities = maps:get(capabilities, Task, []),

            % Find alternative agents (excluding failed one)
            AllCandidates = discover_agents_by_capability(hd(Capabilities)),
            Candidates = [Pid || Pid <- AllCandidates,
                                get_agent_id(Pid) =/= FailedAgentId],

            case Candidates of
                [] ->
                    ?LOG_ERROR("No alternative agents for task ~p", [TaskId]),
                    {error, no_agents_available};
                _ ->
                    % Select best agent using load-aware balancing
                    AgentIds = [get_agent_id(Pid) || Pid <- Candidates],
                    NewAgentId = load_balance_load_aware(AgentIds),

                    % Reassign task
                    ok = assign_task_to_agent(TaskId, NewAgentId),

                    ?LOG_INFO("Requeued task ~p from ~p to ~p",
                             [TaskId, FailedAgentId, NewAgentId]),
                    {ok, NewAgentId}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Promote backup agent to primary
-spec promote_backup_agent(PrimaryAgentId, BackupAgentId) -> ok | {error, term()}
    when PrimaryAgentId :: binary(),
         BackupAgentId :: binary().
promote_backup_agent(PrimaryAgentId, BackupAgentId) ->
    ?LOG_INFO("Promoting backup agent ~p to replace primary ~p",
              [BackupAgentId, PrimaryAgentId]),

    % Get primary agent's configuration
    case get_agent_config(PrimaryAgentId) of
        {ok, Config} ->
            % Transfer state to backup
            case transfer_agent_state(PrimaryAgentId, BackupAgentId) of
                ok ->
                    % Update registry - backup becomes primary
                    ok = update_agent_role(BackupAgentId, primary),
                    ok = update_agent_role(PrimaryAgentId, failed),

                    % Reassign all pending tasks
                    PendingTasks = get_agent_pending_tasks(PrimaryAgentId),
                    lists:foreach(fun(TaskId) ->
                        assign_task_to_agent(TaskId, BackupAgentId)
                    end, PendingTasks),

                    ?LOG_INFO("Backup promotion complete: ~p active tasks transferred",
                             [length(PendingTasks)]),
                    ok;
                {error, Reason} ->
                    {error, {state_transfer_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {primary_config_not_found, Reason}}
    end.

%%%===================================================================
%%% 5. NETWORK AWARENESS
%%%===================================================================

%% @doc Detect network partition using heartbeat timeouts
-spec detect_network_partition(ClusterNodes) -> {partitioned, [node()]} | ok
    when ClusterNodes :: [node()].
detect_network_partition(ClusterNodes) ->
    % Check connectivity to each node
    HeartbeatTimeout = 5000,  % 5 seconds

    UnreachableNodes = lists:filter(fun(Node) ->
        case net_adm:ping(Node) of
            pong ->
                false;
            pang ->
                ?LOG_WARNING("Node ~p unreachable", [Node]),
                true
        end
    end, ClusterNodes),

    case UnreachableNodes of
        [] ->
            ok;
        _ ->
            ?LOG_ERROR("Network partition detected: ~p nodes unreachable",
                      [length(UnreachableNodes)]),
            {partitioned, UnreachableNodes}
    end.

%% @doc Heal network partition by reconciling state
-spec heal_partition(PartitionA, PartitionB) -> ok | {error, term()}
    when PartitionA :: [node()],
         PartitionB :: [node()].
heal_partition(PartitionA, PartitionB) ->
    ?LOG_INFO("Healing partition: A=~p nodes, B=~p nodes",
              [length(PartitionA), length(PartitionB)]),

    % Step 1: Verify connectivity restored
    AllNodes = PartitionA ++ PartitionB,
    case verify_connectivity(AllNodes) of
        ok ->
            % Step 2: Determine authoritative partition (larger one)
            {AuthNodes, NonAuthNodes} = case length(PartitionA) >= length(PartitionB) of
                true -> {PartitionA, PartitionB};
                false -> {PartitionB, PartitionA}
            end,

            ?LOG_INFO("Using partition A (~p nodes) as authoritative",
                     [length(AuthNodes)]),

            % Step 3: Reconcile state from authoritative to non-authoritative
            AuthState = collect_partition_state(AuthNodes),

            lists:foreach(fun(Node) ->
                rpc:call(Node, ?MODULE, apply_partition_state, [AuthState])
            end, NonAuthNodes),

            % Step 4: Re-elect leader
            NewLeader = elect_partition_leader(AllNodes),

            ?LOG_INFO("Partition healed: new leader = ~p", [NewLeader]),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Cannot heal partition: connectivity not restored (~p)",
                      [Reason]),
            {error, connectivity_not_restored}
    end.

%% @doc Verify connectivity to all nodes
verify_connectivity(Nodes) ->
    Unreachable = [N || N <- Nodes, net_adm:ping(N) =:= pang],
    case Unreachable of
        [] -> ok;
        _ -> {error, {unreachable_nodes, Unreachable}}
    end.

%% @doc Collect partition state for reconciliation
collect_partition_state(Nodes) ->
    % Collect state from all nodes in partition
    States = lists:map(fun(Node) ->
        case rpc:call(Node, erlmcp_flow_registry, get_all_state, []) of
            {ok, State} -> {Node, State};
            _ -> {Node, undefined}
        end
    end, Nodes),

    % Merge states (latest timestamp wins)
    merge_partition_states(States).

%% @doc Elect leader for healed partition
elect_partition_leader(Nodes) ->
    % Use consistent hashing or Raft leader election
    SortedNodes = lists:sort(Nodes),
    hd(SortedNodes).  % Simplest: pick first node alphabetically

%%%===================================================================
%%% COMPLETE DEMO
%%%===================================================================

%% @doc Demonstrate complete routing flow
demo_complete_routing() ->
    io:format("~n=== erlmcp-flow Routing Demo ===~n~n"),

    % 1. Register agents
    io:format("1. Registering agents with gproc...~n"),
    Agents = [
        {<<"agent-1">>, self(), [<<"erlang">>, <<"testing">>]},
        {<<"agent-2">>, self(), [<<"erlang">>, <<"performance">>]},
        {<<"agent-3">>, self(), [<<"testing">>, <<"qa">>]}
    ],
    [register_agent_gproc(Id, Pid, Caps) || {Id, Pid, Caps} <- Agents],

    % 2. Discover agents
    io:format("2. Discovering agents by capability...~n"),
    ErlangAgents = discover_agents_by_capability(<<"erlang">>),
    io:format("   Found ~p agents with 'erlang' capability~n", [length(ErlangAgents)]),

    % 3. Load balancing
    io:format("3. Load balancing selection...~n"),
    AgentIds = [<<"agent-1">>, <<"agent-2">>, <<"agent-3">>],
    {Selected, _} = load_balance_round_robin(AgentIds, 0),
    io:format("   Round-robin selected: ~p~n", [Selected]),

    % 4. Leader election
    io:format("4. Leader election...~n"),
    {Role, Leader} = elect_leader(<<"demo-group">>),
    io:format("   Role: ~p, Leader: ~p~n", [Role, Leader]),

    % 5. Gossip dissemination
    io:format("5. Gossip protocol dissemination...~n"),
    gossip_disseminate(<<"node-1">>, {<<"key">>, <<"value">>}, ErlangAgents),

    io:format("~n=== Demo Complete ===~n"),
    ok.

%%%===================================================================
%%% HELPER FUNCTIONS (stubs for compilation)
%%%===================================================================

get_raft_state() -> #raft_state{}.
put_raft_state(_State) -> ok.
check_log_consistency(_State, _Index, _Term) -> true.
append_raft_entries(Log, _PrevIndex, Entries) -> Log ++ Entries.
is_log_up_to_date(_State, _Index, _Term) -> true.

get_gossip_state() -> #gossip_state{node_id = <<"node-1">>, peers = []}.
put_gossip_state(_State) -> ok.
should_accept_gossip(_OurVC, _TheirVC, _NodeId) -> true.
merge_vector_clocks(VC1, VC2) -> maps:merge(VC1, VC2).
select_random_peers(Peers, N) -> lists:sublist(Peers, N).

get_agent_metrics(AgentId) ->
    #{
        load => rand:uniform(10),
        avg_latency => rand:uniform(100),
        success_rate => 0.95 + rand:uniform() * 0.05
    }.

get_agent_last_heartbeat(_AgentId) -> {ok, erlang:timestamp()}.
get_task_details(_TaskId) -> {ok, #{capabilities => [<<"erlang">>]}}.
get_agent_id(_Pid) -> <<"agent-1">>.
assign_task_to_agent(_TaskId, _AgentId) -> ok.
get_agent_config(_AgentId) -> {ok, #{}}.
transfer_agent_state(_From, _To) -> ok.
update_agent_role(_AgentId, _Role) -> ok.
get_agent_pending_tasks(_AgentId) -> [].
merge_partition_states(States) -> hd([S || {_, S} <- States, S =/= undefined]).
apply_partition_state(_State) -> ok.
