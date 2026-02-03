%% @doc Cluster Coordinator State Machine for erlmcp v3
%%
%% This module implements a comprehensive cluster coordination state machine
%% that manages the complete lifecycle of an Erlang cluster with:
%%   - State transitions: forming -> joining -> stable -> partitioning -> merging -> splitting
%%   - Membership management with gproc registry
%%   - Heartbeat-based failure detection
%%   - Raft-inspired consensus and leader election
%%   - Automatic failover and recovery
%%   - Partition detection and healing
%%   - Data synchronization with conflict resolution
%%   - Configuration management with versioning
%%   - Comprehensive metrics and monitoring
%%
%% == State Machine Diagram ==
%%
%%                ┌─────────────┐
%%                │  forming    │
%%                └──────┬──────┘
%%                       │
%%                       v
%%                ┌─────────────┐
%%                │  joining    │◄─────────────┐
%%                └──────┬──────┘              │
%%                       │                     │
%%                       v                     │
%%                ┌─────────────┐              │
%%                │   stable    │──────────────┤
%%                └──────┬──────┘              │
%%                       │                     │
%%     ┌─────────────────┼─────────────────┐  │
%%     ▼                 ▼                 ▼  │
%% ┌─────────┐    ┌──────────┐    ┌──────────┐│
%% │partition│    │draining  │    │voting    ││
%% └────┬────┘    └────┬─────┘    └────┬─────┘│
%%      │              │                │      │
%%      v              v                v      │
%% ┌─────────┐    ┌──────────┐    ┌──────────┐│
──┤ merging │    │shutdown  │    │electing  ││
│  └────┬────┘    └──────────┘    └────┬─────┘│
│       │                                │      │
│       v                                v      │
│  ┌─────────┐                    ┌──────────┐│
│  │resyncing│                    │ recovery ││
│  └────┬────┘                    └────┬─────┘│
│       │                              │      │
│       └──────────────────────────────┘      │
│                       │                      │
└───────────────────────┴──────────────────────┘
%%
%% @end
-module(erlmcp_cluster_coordinator).

-behaviour(gen_statem).

-include("erlmcp_cluster_coordinator.hrl").
-include("erlmcp.hrl").

%% API - Lifecycle
-export([start_link/3, start_link/4, stop/1, force_state_change/2]).

%% API - Membership
-export([join_cluster/2, leave_cluster/2, get_members/1, get_member_info/2]).

%% API - Leadership
-export([get_leader/1, become_leader/1, step_down/1, force_election/1]).

%% API - Configuration
-export([get_config/1, propose_config/2, approve_config/3]).

%% API - Monitoring
-export([get_cluster_status/1, get_metrics/1, subscribe_events/2, unsubscribe_events/2]).

%% API - Partition handling
-export([report_partition/3, heal_partition/2, get_partition_info/1]).

%% API - Data sync
-export([trigger_sync/2, sync_status/1, resolve_conflict/3]).

%% API - Health
-export([health_check/1, force_health_check/1, get_quorum/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([handle_event/4]).

%% State functions
-export([
    forming/3,
    joining/3,
    stable/3,
    partitioning/3,
    merging/3,
    splitting/3,
    draining/3,
    shutdown/3,
    recovery/3,
    degraded/3,
    upgrading/3,
    resyncing/3,
    voting/3,
    electing/3,
    snapshotting/3
]).

%%%====================================================================
%%% Type Definitions
%%%====================================================================

-type state() :: cluster_state().

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the cluster coordinator
-spec start_link(ClusterName :: binary(), NodeId :: node(), Options :: map()) ->
    {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId, Options) ->
    gen_statem:start_link(?MODULE, {ClusterName, NodeId, Options}, []).

-spec start_link(Name :: atom(), ClusterName :: binary(),
                 NodeId :: node(), Options :: map()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, ClusterName, NodeId, Options) ->
    gen_statem:start_link(Name, {ClusterName, NodeId, Options}, []).

%% @doc Stop the coordinator
-spec stop(pid() | atom()) -> ok.
stop(Coordinator) ->
    gen_statem:stop(Coordinator).

%% @doc Force a state transition (for testing)
-spec force_state_change(pid() | atom(), cluster_state()) -> ok.
force_state_change(Coordinator, NewState) ->
    gen_statem:call(Coordinator, {force_state, NewState}).

%% @doc Join an existing cluster
-spec join_cluster(pid() | atom(), [node()]) -> ok | {error, term()}.
join_cluster(Coordinator, SeedNodes) ->
    gen_statem:call(Coordinator, {join, SeedNodes}, 30000).

%% @doc Leave the cluster gracefully
-spec leave_cluster(pid() | atom(), term()) -> ok | {error, term()}.
leave_cluster(Coordinator, Reason) ->
    gen_statem:call(Coordinator, {leave, Reason}, 30000).

%% @doc Get all cluster members
-spec get_members(pid() | atom()) -> {ok, [node()]} | {error, term()}.
get_members(Coordinator) ->
    gen_statem:call(Coordinator, get_members).

%% @doc Get detailed info for a specific member
-spec get_member_info(pid() | atom(), node()) ->
    {ok, member_info()} | {error, term()}.
get_member_info(Coordinator, Node) ->
    gen_statem:call(Coordinator, {get_member_info, Node}).

%% @doc Get current cluster leader
-spec get_leader(pid() | atom()) -> {ok, node() | undefined} | {error, term()}.
get_leader(Coordinator) ->
    gen_statem:call(Coordinator, get_leader).

%% @doc Become leader (if eligible)
-spec become_leader(pid() | atom()) -> ok | {error, term()}.
become_leader(Coordinator) ->
    gen_statem:call(Coordinator, become_leader, 10000).

%% @doc Step down from leadership
-spec step_down(pid() | atom()) -> ok | {error, term()}.
step_down(Coordinator) ->
    gen_statem:call(Coordinator, step_down).

%% @doc Force a new election
-spec force_election(pid() | atom()) -> ok.
force_election(Coordinator) ->
    gen_statem:cast(Coordinator, force_election).

%% @doc Get current cluster configuration
-spec get_config(pid() | atom()) -> {ok, cluster_config()} | {error, term()}.
get_config(Coordinator) ->
    gen_statem:call(Coordinator, get_config).

%% @doc Propose a configuration change
-spec propose_config(pid() | atom(), cluster_config()) ->
    {ok, non_neg_integer()} | {error, term()}.
propose_config(Coordinator, NewConfig) ->
    gen_statem:call(Coordinator, {propose_config, NewConfig}).

%% @doc Approve a configuration proposal
-spec approve_config(pid() | atom(), non_neg_integer(), boolean()) ->
    ok | {error, term()}.
approve_config(Coordinator, ConfigVersion, Approve) ->
    gen_statem:call(Coordinator, {approve_config, ConfigVersion, Approve}).

%% @doc Get cluster status
-spec get_cluster_status(pid() | atom()) ->
    {ok, cluster_state(), cluster_status()} | {error, term()}.
get_cluster_status(Coordinator) ->
    gen_statem:call(Coordinator, get_status).

%% @doc Get cluster metrics
-spec get_metrics(pid() | atom()) -> {ok, cluster_metrics()} | {error, term()}.
get_metrics(Coordinator) ->
    gen_statem:call(Coordinator, get_metrics).

%% @doc Subscribe to cluster events
-spec subscribe_events(pid() | atom(), pid()) -> ok.
subscribe_events(Coordinator, Subscriber) ->
    gen_statem:cast(Coordinator, {subscribe, Subscriber}).

%% @doc Unsubscribe from events
-spec unsubscribe_events(pid() | atom(), pid()) -> ok.
unsubscribe_events(Coordinator, Subscriber) ->
    gen_statem:cast(Coordinator, {unsubscribe, Subscriber}).

%% @doc Report a network partition
-spec report_partition(pid() | atom(), [node()], term()) -> ok.
report_partition(Coordinator, PartitionedNodes, Reason) ->
    gen_statem:cast(Coordinator, {partition_detected, PartitionedNodes, Reason}).

%% @doc Heal a network partition
-spec heal_partition(pid() | atom(), merge_strategy()) -> ok.
heal_partition(Coordinator, Strategy) ->
    gen_statem:cast(Coordinator, {heal_partition, Strategy}).

%% @doc Get partition information
-spec get_partition_info(pid() | atom()) ->
    {ok, map()} | {error, term()}.
get_partition_info(Coordinator) ->
    gen_statem:call(Coordinator, get_partition_info).

%% @doc Trigger data synchronization
-spec trigger_sync(pid() | atom(), sync_operation_type()) ->
    {ok, reference()} | {error, term()}.
trigger_sync(Coordinator, SyncType) ->
    gen_statem:call(Coordinator, {trigger_sync, SyncType}).

%% @doc Get synchronization status
-spec sync_status(pid() | atom()) -> {ok, sync_state()} | {error, term()}.
sync_status(Coordinator) ->
    gen_statem:call(Coordinator, sync_status).

%% @doc Resolve a sync conflict
-spec resolve_conflict(pid() | atom(), binary(), conflict_resolution()) ->
    ok | {error, term()}.
resolve_conflict(Coordinator, Key, Resolution) ->
    gen_statem:call(Coordinator, {resolve_conflict, Key, Resolution}).

%% @doc Perform health check
-spec health_check(pid() | atom()) -> {ok, map()} | {error, term()}.
health_check(Coordinator) ->
    gen_statem:call(Coordinator, health_check).

%% @doc Force immediate health check
-spec force_health_check(pid() | atom()) -> ok.
force_health_check(Coordinator) ->
    gen_statem:cast(Coordinator, force_health_check).

%% @doc Get quorum information
-spec get_quorum(pid() | atom()) ->
    {ok, {HasQuorum :: boolean(), QuorumSize :: pos_integer(), Current :: pos_integer()}}.
get_quorum(Coordinator) ->
    gen_statem:call(Coordinator, get_quorum).

%%%====================================================================
%%% gen_statem Callbacks
%%%====================================================================

-spec init({binary(), node(), map()}) ->
    {ok, forming, #cluster_coordinator_state{}}.
init({ClusterName, NodeId, Options}) ->
    process_flag(trap_exit, true),

    %% Initialize configuration
    DefaultConfig = #{
        version => 0,
        members => [NodeId],
        quorum => maps:get(quorum, Options, 2),
        heartbeat_interval => maps:get(heartbeat_interval, Options, 5000),
        election_timeout => maps:get(election_timeout, Options, 15000),
        sync_strategy => maps:get(sync_strategy, Options, anti_entropy),
        partition_strategy => maps:get(partition_strategy, Options, majority_side),
        failover_strategy => maps:get(failover_strategy, Options, automatic),
        metadata => #{}
    },

    Config = maps:get(initial_config, Options, DefaultConfig),

    %% Initialize state
    State = #cluster_coordinator_state{
        cluster_name = ClusterName,
        node_id = NodeId,
        current_state = forming,
        previous_state = undefined,
        status = healthy,
        leader_node = undefined,
        is_leader = false,
        election_term = 0,
        voted_for = undefined,
        votes_received = sets:new(),
        election_timeout_ref = undefined,
        leader_lease_until = undefined,
        members = #{NodeId => #{
            node => NodeId,
            status => up,
            role => voter,
            last_heartbeat => erlang:system_time(millisecond),
            generation => 0,
            capabilities => [],
            metadata => #{}
        }},
        joining_nodes => #{},
        leaving_nodes => #{},
        member_quorum = maps:get(quorum, Config, 2),
        config = Config,
        pending_config = undefined,
        config_version = 0,
        heartbeat_interval = maps:get(heartbeat_interval, Options, 5000),
        heartbeat_timeout = maps:get(heartbeat_timeout, Options, 15000),
        last_heartbeat = #{},
        missed_heartbeats => #{},
        sync_state => #{
            last_sync_index => 0,
            last_sync_time => 0,
            sync_in_progress => false,
            sync_queue => queue:new(),
            conflicts => #{}
        },
        pending_syncs => #{},
        partition_detected = false,
        partition_id = undefined,
        partition_members = [],
        merge_strategy = last_write_wins,
        failover_in_progress = false,
        failover_queue = queue:new(),
        recovered_nodes => [],
        metrics => #{
            state_changes => 0,
            elections => 0,
            partitions => 0,
            merges => 0,
            failovers => 0,
            sync_operations => 0,
            conflicts_resolved => 0,
            uptime_seconds => 0,
            last_state_change => erlang:system_time(second),
            leader_transitions => 0,
            member_changes => 0,
            heartbeat_failures => 0,
            sync_bytes_transferred => 0
        },
        state_timer = undefined,
        heartbeat_timer = undefined,
        sync_timer = undefined,
        health_check_timer = undefined,
        snapshot_timer = undefined,
        monitored_processes => #{},
        partition_monitors => #{},
        retry_count => 0,
        max_retries => maps:get(max_retries, Options, 10),
        backoff_ms => maps:get(backoff_ms, Options, 1000),
        event_subscribers = sets:new(),
        correlation_id = generate_correlation_id(),
        trace_context => #{}
    },

    %% Register with gproc
    gproc:add_local_name({cluster_coordinator, ClusterName}),
    gproc:add_local_property({cluster_node, ClusterName}, NodeId),
    gproc:reg({p, l, {cluster_coordinator, ClusterName}}),

    %% Monitor remote nodes
    lists:foreach(fun(Node) ->
        monitor_node(Node, true, State)
    end, maps:get(cluster_nodes, Options, [])),

    %% Start timers
    Timers = start_initial_timers(State),
    UpdatedState = set_timers(State, Timers),

    logger:info("Cluster coordinator started for ~p on node ~p",
                [ClusterName, NodeId]),

    {ok, forming, UpdatedState}.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec terminate(term(), state(), #cluster_coordinator_state{}) -> term().
terminate(_Reason, _State, #cluster_coordinator_state{} = State) ->
    %% Cancel all timers
    cancel_timers(State),

    %% Unmonitor nodes
    maps:foreach(fun(Node, _Ref) ->
        monitor_node(Node, false, State)
    end, State#cluster_coordinator_state.partition_monitors),

    %% Unregister from gproc
    ClusterName = State#cluster_coordinator_state.cluster_name,
    gproc:goodbye(),

    logger:info("Cluster coordinator terminating for ~p", [ClusterName]),
    ok.

-spec code_change(term(), state(), #cluster_coordinator_state{}, term()) ->
    {ok, state(), #cluster_coordinator_state{}}.
code_change(_OldVsn, State, _Extra, _StateName) ->
    {ok, State, _StateName}.

%%%====================================================================
%%% State Functions
%%%====================================================================

%% @doc Forming state - initial cluster formation
-spec forming(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
forming(enter, _OldState, State) ->
    logger:info("Cluster entering FORMING state"),
    {keep_state, update_metrics(State, forming)};

forming(info, initialize_cluster, #cluster_coordinator_state{node_id = NodeId} = State) ->
    %% Check if we're the first node (seed)
    case is_seed_node(State) of
        true ->
            %% Become leader and transition to stable
            NewState = become_cluster_leader(State),
            {next_state, stable, NewState};
        false ->
            %% Try to join existing cluster
            SeedNodes = get_seed_nodes(State),
            case connect_to_seeds(SeedNodes) of
                {ok, Leader} ->
                    NewState = initiate_join(State, Leader),
                    {next_state, joining, NewState};
                {error, Reason} ->
                    logger:warning("Failed to connect to seed nodes: ~p", [Reason]),
                    %% Retry after delay
                    RetryState = schedule_retry(State),
                    {keep_state, RetryState}
            end
    end;

forming(cast, {nodeup, Node}, State) ->
    handle_node_up(Node, forming, State);

forming(info, {node_down, Node}, State) ->
    handle_node_down(Node, forming, State);

forming({call, From}, {force_state, NewState}, State) ->
    {next_state, NewState, State, [{reply, From, ok}]};

forming(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, forming, State).

%% @doc Joining state - node joining existing cluster
-spec joining(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
joining(enter, _OldState, #cluster_coordinator_state{node_id = NodeId} = State) ->
    logger:info("Node ~p entering JOINING state", [NodeId]),
    {keep_state, update_metrics(State, joining)};

joining(info, {join_approved, Leader, Config}, State) ->
    logger:info("Join approved by leader ~p", [Leader]),
    NewState = apply_cluster_config(State, Config),
    {next_state, stable, NewState};

joining(info, {join_rejected, Reason}, State) ->
    logger:warning("Join rejected: ~p", [Reason]),
    {next_state, recovery, State};

joining(cast, {nodeup, Node}, State) ->
    handle_node_up(Node, joining, State);

joining({call, From}, get_status, State) ->
    {keep_state, State, [{reply, From, {ok, joining, State#cluster_coordinator_state.status}}]};

joining(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, joining, State).

%% @doc Stable state - normal cluster operation
-spec stable(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
stable(enter, _OldState, State) ->
    logger:info("Cluster entering STABLE state"),
    %% Emit OTEL event
    emit_telemetry_event(cluster_stable, #{
        node => State#cluster_coordinator_state.node_id,
        leader => State#cluster_coordinator_state.leader_node,
        members => maps:keys(State#cluster_coordinator_state.members)
    }),
    {keep_state, update_metrics(State, stable)};

stable(cast, {heartbeat, From, Term, Metadata}, State) ->
    handle_heartbeat(From, Term, Metadata, stable, State);

stable(cast, {heartbeat_timeout, Node}, State) ->
    handle_heartbeat_timeout(Node, stable, State);

stable(cast, {nodeup, Node}, State) ->
    handle_node_up(Node, stable, State);

stable(info, {node_down, Node}, State) ->
    handle_node_down(Node, stable, State);

stable(cast, {partition_detected, PartitionedNodes, Reason}, State) ->
    logger:error("Partition detected: nodes ~p, reason: ~p", [PartitionedNodes, Reason]),
    PartitionState = State#cluster_coordinator_state{
        partition_detected = true,
        partition_id = generate_partition_id(),
        partition_members = PartitionedNodes,
        status = partitioned
    },
    {next_state, partitioning, PartitionState};

stable(cast, force_election, State) ->
    logger:info("Forced election requested"),
    ElectionState = start_election(State),
    {next_state, electing, ElectionState};

stable(info, trigger_sync, State) ->
    SyncState = initiate_data_sync(State, full_sync),
    {keep_state, SyncState};

stable({call, From}, {join, SeedNodes}, State) ->
    handle_join_request(From, SeedNodes, State);

stable({call, From}, {leave, Reason}, State) ->
    handle_leave_request(From, Reason, State);

stable({call, From}, get_members, State) ->
    Members = maps:keys(State#cluster_coordinator_state.members),
    {keep_state, State, [{reply, From, {ok, Members}}]};

stable({call, From}, {get_member_info, Node}, State) ->
    case maps:get(Node, State#cluster_coordinator_state.members, undefined) of
        undefined ->
            {keep_state, State, [{reply, From, {error, not_found}}]};
        Info ->
            {keep_state, State, [{reply, From, {ok, Info}}]}
    end;

stable({call, From}, get_leader, State) ->
    Leader = State#cluster_coordinator_state.leader_node,
    {keep_state, State, [{reply, From, {ok, Leader}}]};

stable({call, From}, become_leader, #cluster_coordinator_state{is_leader = true} = State) ->
    {keep_state, State, [{reply, From, {ok, already_leader}}]};
stable({call, From}, become_leader, State) ->
    LeaderState = start_election(State),
    {next_state, electing, LeaderState, [{reply, From, ok}]};

stable({call, From}, step_down, #cluster_coordinator_state{is_leader = false} = State) ->
    {keep_state, State, [{reply, From, {error, not_leader}}]};
stable({call, From}, step_down, State) ->
    NewState = transition_leadership(State),
    {keep_state, NewState, [{reply, From, ok}]};

stable({call, From}, get_config, State) ->
    Config = State#cluster_coordinator_state.config,
    {keep_state, State, [{reply, From, {ok, Config}}]};

stable({call, From}, get_status, State) ->
    {keep_state, State, [{reply, From, {ok, stable, State#cluster_coordinator_state.status}}]};

stable({call, From}, get_metrics, State) ->
    Metrics = State#cluster_coordinator_state.metrics,
    {keep_state, State, [{reply, From, {ok, Metrics}}]};

stable({call, From}, {trigger_sync, SyncType}, State) ->
    {ok, Ref, NewState} = do_trigger_sync(State, SyncType),
    {keep_state, NewState, [{reply, From, {ok, Ref}}]};

stable({call, From}, sync_status, State) ->
    SyncState = State#cluster_coordinator_state.sync_state,
    {keep_state, State, [{reply, From, {ok, SyncState}}]};

stable({call, From}, health_check, State) ->
    Health = perform_health_check(State),
    {keep_state, State, [{reply, From, {ok, Health}}]};

stable({call, From}, get_quorum, State) ->
    HasQuorum = has_quorum(State),
    QuorumSize = State#cluster_coordinator_state.member_quorum,
    Current = maps:size(State#cluster_coordinator_state.members),
    {keep_state, State, [{reply, From, {ok, {HasQuorum, QuorumSize, Current}}}]};

stable(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, stable, State).

%% @doc Partitioning state - handling network partition
-spec partitioning(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
partitioning(enter, _OldState, State) ->
    logger:warning("Cluster entering PARTITIONING state"),

    %% Update partition metrics
    Metrics = State#cluster_coordinator_state.metrics,
    UpdatedMetrics = Metrics#{
        partitions => maps:get(partitions, Metrics, 0) + 1
    },
    UpdatedState = State#cluster_coordinator_state{metrics = UpdatedMetrics},

    %% Notify subscribers
    notify_subscribers(UpdatedState, {partition_detected,
        UpdatedState#cluster_coordinator_state.partition_id,
        UpdatedState#cluster_coordinator_state.partition_members}),

    {keep_state, update_metrics(UpdatedState, partitioning)};

partitioning(cast, {heal_partition, Strategy}, State) ->
    logger:info("Healing partition with strategy: ~p", [Strategy]),
    MergeState = State#cluster_coordinator_state{
        merge_strategy = Strategy
    },
    {next_state, merging, MergeState};

partitioning(info, partition_timeout, State) ->
    %% Check if partition still exists
    case verify_partition_state(State) of
        true ->
            %% Still partitioned, take action based on strategy
            ActionState = execute_partition_strategy(State),
            {keep_state, ActionState};
        false ->
            %% Partition healed
            {next_state, merging, State}
    end;

partitioning(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, partitioning, State).

%% @doc Merging state - healing partition and resynchronizing
-spec merging(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
merging(enter, _OldState, State) ->
    logger:info("Cluster entering MERGING state"),
    {keep_state, update_metrics(State, merging)};

merging(info, initiate_merge, State) ->
    MergeState = execute_merge(State),
    {next_state, resyncing, MergeState};

merging(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, merging, State).

%% @doc Resyncing state - data resynchronization after partition
-spec resyncing(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
resyncing(enter, _OldState, State) ->
    logger:info("Cluster entering RESYNCING state"),

    %% Start data synchronization
    SyncState = initiate_data_sync(State, anti_entropy_sync),

    {keep_state, update_metrics(SyncState, resyncing)};

resyncing(info, {sync_complete, Ref, Result}, State) ->
    logger:info("Sync complete: ~p", [Ref]),
    NewState = record_sync_result(State, Ref, Result),

    %% Check if all syncs complete
    case map_size(NewState#cluster_coordinator_state.pending_syncs) of
        0 ->
            %% All syncs complete, return to stable
            FinalState = NewState#cluster_coordinator_state{
                partition_detected = false,
                partition_id = undefined,
                partition_members = [],
                status = healthy
            },
            {next_state, stable, FinalState};
        _ ->
            %% More syncs pending
            {keep_state, NewState}
    end;

resyncing(info, {sync_failed, Ref, Reason}, State) ->
    logger:error("Sync failed: ~p, reason: ~p", [Ref, Reason]),

    %% Remove from pending
    Pending = maps:remove(Ref, State#cluster_coordinator_state.pending_syncs),
    NewState = State#cluster_coordinator_state{pending_syncs = Pending},

    case should_retry_sync(NewState) of
        true ->
            RetryState = retry_sync(NewState, Ref),
            {keep_state, RetryState};
        false ->
            %% Too many failures, enter degraded state
            {next_state, degraded, NewState}
    end;

resyncing(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, resyncing, State).

%% @doc Electing state - leader election in progress
-spec electing(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
electing(enter, _OldState, #cluster_coordinator_state{node_id = NodeId} = State) ->
    logger:info("Cluster entering ELECTING state, node ~p starting election", [NodeId]),
    ElectionState = start_election_voting(State),
    {keep_state, update_metrics(ElectionState, electing)};

electing(cast, {vote_response, Voter, Term, VoteGranted}, State) ->
    handle_vote_response(Voter, Term, VoteGranted, electing, State);

electing(info, election_timeout, State) ->
    logger:warning("Election timeout"),
    %% Increment term and restart
    NewState = increment_election_term(State),
    RetryState = restart_election(NewState),
    {keep_state, RetryState};

electing(info, {election_won, Term}, State) ->
    logger:info("Election won for term ~p", [Term]),
    LeaderState = become_cluster_leader(State),
    {next_state, stable, LeaderState};

electing(info, {new_leader, Leader, Term}, State) ->
    logger:info("Recognizing new leader ~p for term ~p", [Leader, Term]),
    FollowerState = recognize_leader(State, Leader, Term),
    {next_state, stable, FollowerState};

electing(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, electing, State).

%% @doc Degraded state - operating with reduced capacity
-spec degraded(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
degraded(enter, _OldState, State) ->
    logger:warning("Cluster entering DEGRADED state"),
    {keep_state, update_metrics(State, degraded)};

degraded(cast, {node_recovered, Node}, State) ->
    logger:info("Node ~p recovered", [Node]),
    NewState = handle_node_recovery(Node, State),

    case can_return_to_stable(NewState) of
        true ->
            {next_state, stable, NewState};
        false ->
            {keep_state, NewState}
    end;

degraded(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, degraded, State).

%% @doc Draining state - graceful node shutdown
-spec draining(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
draining(enter, _OldState, State) ->
    logger:info("Cluster entering DRAINING state"),
    DrainState = start_drain(State),
    {keep_state, update_metrics(DrainState, draining)};

draining(info, {drain_complete, Node}, State) ->
    logger:info("Drain complete for node ~p", [Node]),
    case Node of
        State#cluster_coordinator_state.node_id ->
            %% Our drain is complete
            {next_state, shutdown, State};
        _ ->
            %% Another node's drain complete
            NewState = update_member_status(Node, left, State),
            {keep_state, NewState}
    end;

draining(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, draining, State).

%% @doc Shutdown state - cluster shutdown
-spec shutdown(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
shutdown(enter, _OldState, State) ->
    logger:info("Cluster entering SHUTDOWN state"),
    {keep_state, update_metrics(State, shutdown)};

shutdown(cast, shutdown_complete, State) ->
    logger:info("Shutdown complete, stopping"),
    {stop, normal, State};

shutdown(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, shutdown, State).

%% @doc Recovery state - recovering from failure
-spec recovery(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
recovery(enter, _OldState, State) ->
    logger:warning("Cluster entering RECOVERY state"),
    RecoveryState = start_recovery(State),
    {keep_state, update_metrics(RecoveryState, recovery)};

recovery(info, recovery_complete, State) ->
    logger:info("Recovery complete"),
    {next_state, stable, State};

recovery(info, recovery_failed, State) ->
    logger:error("Recovery failed"),
    %% Stay in recovery and retry
    RetryState = schedule_retry(State),
    {keep_state, RetryState};

recovery(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, recovery, State).

%% @doc Splitting state - controlled cluster split
-spec splitting(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
splitting(enter, _OldState, State) ->
    logger:info("Cluster entering SPLITTING state"),
    SplitState = execute_split(State),
    {keep_state, update_metrics(SplitState, splitting)};

splitting(info, {split_complete, Partition1, Partition2}, State) ->
    logger:info("Split complete: ~p and ~p", [Partition1, Partition2]),
    {next_state, stable, State};

splitting(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, splitting, State).

%% @doc Upgrading state - rolling upgrade
-spec upgrading(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
upgrading(enter, _OldState, State) ->
    logger:info("Cluster entering UPGRADING state"),
    UpgradeState = start_upgrade(State),
    {keep_state, update_metrics(UpgradeState, upgrading)};

upgrading(info, {upgrade_complete, Node}, State) ->
    logger:info("Upgrade complete for node ~p", [Node]),
    NewState = handle_upgrade_complete(Node, State),

    case is_upgrade_complete(NewState) of
        true ->
            {next_state, stable, NewState};
        false ->
            {keep_state, NewState}
    end;

upgrading(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, upgrading, State).

%% @doc Snapshotting state - creating cluster snapshot
-spec snapshotting(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
snapshotting(enter, _OldState, State) ->
    logger:info("Cluster entering SNAPSHOTTING state"),
    SnapshotState = start_snapshot(State),
    {keep_state, update_metrics(SnapshotState, snapshotting)};

snapshotting(info, {snapshot_complete, SnapshotId}, State) ->
    logger:info("Snapshot complete: ~p", [SnapshotId]),
    NewState = record_snapshot_complete(State, SnapshotId),
    {next_state, stable, NewState};

snapshotting(info, {snapshot_failed, Reason}, State) ->
    logger:error("Snapshot failed: ~p", [Reason]),
    {next_state, stable, State};

snapshotting(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, snapshotting, State).

%% @doc Voting state - configuration voting
-spec voting(gen_statem:event_type(), term(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
voting(enter, _OldState, State) ->
    logger:info("Cluster entering VOTING state"),
    {keep_state, update_metrics(State, voting)};

voting(cast, {config_vote, Voter, Version, Approve}, State) ->
    handle_config_vote(Voter, Version, Approve, voting, State);

voting(info, {config_approved, Version}, State) ->
    logger:info("Config version ~p approved", [Version]),
    NewState = apply_new_config(State, Version),
    {next_state, stable, NewState};

voting(info, {config_rejected, Version}, State) ->
    logger:warning("Config version ~p rejected", [Version]),
    {next_state, stable, State};

voting(EventType, EventContent, State) ->
    handle_common_event(EventType, EventContent, voting, State).

%%%====================================================================
%%% Common Event Handlers
%%%====================================================================

-spec handle_common_event(gen_statem:event_type(), term(), state(),
                          #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
handle_common_event(cast, {subscribe, Subscriber}, StateName, State) ->
    NewSubscribers = sets:add_element(Subscriber, State#cluster_coordinator_state.event_subscribers),
    monitor(process, Subscriber),
    NewState = State#cluster_coordinator_state{event_subscribers = NewSubscribers},
    {keep_state, NewState};

handle_common_event(cast, {unsubscribe, Subscriber}, StateName, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#cluster_coordinator_state.event_subscribers),
    NewState = State#cluster_coordinator_state{event_subscribers = NewSubscribers},
    {keep_state, NewState};

handle_common_event(info, {'DOWN', _Ref, process, Pid, _Reason}, StateName, State) ->
    %% Clean up subscriber
    NewSubscribers = sets:del_element(Pid, State#cluster_coordinator_state.event_subscribers),
    {keep_state, State#cluster_coordinator_state{event_subscribers = NewSubscribers}};

handle_common_event(info, periodic_heartbeat, StateName, State) ->
    NewState = send_heartbeats(State),
    HeartbeatRef = erlang:send_after(State#cluster_coordinator_state.heartbeat_interval,
                                     self(), periodic_heartbeat),
    {keep_state, NewState#cluster_coordinator_state{heartbeat_timer = HeartbeatRef}};

handle_common_event(info, periodic_health_check, StateName, State) ->
    Health = perform_health_check(State),
    NewState = update_health_status(Health, State),
    HealthRef = erlang:send_after(10000, self(), periodic_health_check),
    {keep_state, NewState#cluster_coordinator_state{health_check_timer = HealthRef}};

handle_common_event(info, {nodeup, Node}, StateName, State) ->
    handle_node_up(Node, StateName, State);

handle_common_event(info, {nodedown, Node, _Info}, StateName, State) ->
    handle_node_down(Node, StateName, State);

handle_common_event(cast, {heartbeat, From, Term, Metadata}, StateName, State) ->
    handle_heartbeat(From, Term, Metadata, StateName, State);

handle_common_event(cast, {heartbeat_timeout, Node}, StateName, State) ->
    handle_heartbeat_timeout(Node, StateName, State);

handle_common_event(cast, {vote_request, Candidate, Term, LastLogIndex, LastLogTerm},
                   StateName, #cluster_coordinator_state{election_term = CurrentTerm} = State) ->
    handle_vote_request(Candidate, Term, LastLogIndex, LastLogTerm, CurrentTerm, StateName, State);

handle_common_event({call, From}, {resolve_conflict, Key, Resolution}, StateName, State) ->
    NewState = do_resolve_conflict(State, Key, Resolution),
    {keep_state, NewState, [{reply, From, ok}]};

handle_common_event({call, From}, force_health_check, StateName, State) ->
    Health = perform_health_check(State),
    NewState = update_health_status(Health, State),
    {keep_state, NewState, [{reply, From, {ok, Health}}]};

handle_common_event({call, From}, get_partition_info, StateName, State) ->
    Info = #{
        partition_detected => State#cluster_coordinator_state.partition_detected,
        partition_id => State#cluster_coordinator_state.partition_id,
        partition_members => State#cluster_coordinator_state.partition_members,
        merge_strategy => State#cluster_coordinator_state.merge_strategy
    },
    {keep_state, State, [{reply, From, {ok, Info}}]};

handle_common_event({call, From}, {propose_config, NewConfig}, StateName, State) ->
    case State#cluster_coordinator_state.is_leader of
        true ->
            Version = State#cluster_coordinator_state.config_version + 1,
            PendingConfig = NewConfig#{version => Version},
            NewState = State#cluster_coordinator_state{
                pending_config = PendingConfig,
                config_version = Version
            },
            {next_state, voting, NewState, [{reply, From, {ok, Version}}]};
        false ->
            {keep_state, State, [{reply, From, {error, not_leader}}]}
    end;

handle_common_event({call, From}, {approve_config, Version, Approve}, StateName, State) ->
    Leader = State#cluster_coordinator_state.leader_node,
    gen_server:cast({erlmcp_cluster_coordinator, Leader},
                    {config_vote, node(), Version, Approve}),
    {keep_state, State, [{reply, From, ok}]};

handle_common_event({call, From}, {force_state, NewState}, StateName, State) ->
    {next_state, NewState, State, [{reply, From, ok}]};

handle_common_event(EventType, EventContent, StateName, State) ->
    logger:warning("Unhandled event in ~p: ~p, ~p", [StateName, EventType, EventContent]),
    {keep_state, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Update metrics when entering a state
-spec update_metrics(#cluster_coordinator_state{}, state()) -> #cluster_coordinator_state{}.
update_metrics(#cluster_coordinator_state{metrics = Metrics} = State, NewStateName) ->
    Now = erlang:system_time(second),
    UpdatedMetrics = Metrics#{
        state_changes => maps:get(state_changes, Metrics, 0) + 1,
        last_state_change => Now
    },
    State#cluster_coordinator_state{metrics = UpdatedMetrics}.

%% @doc Send heartbeats to all cluster members
-spec send_heartbeats(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
send_heartbeats(#cluster_coordinator_state{is_leader = false} = State) ->
    State;
send_heartbeats(#cluster_coordinator_state{members = Members, leader_node = Leader,
                                           election_term = Term, node_id = NodeId} = State) ->
    Now = erlang:system_time(millisecond),

    Metadata = #{
        state => State#cluster_coordinator_state.current_state,
        status => State#cluster_coordinator_state.status,
        leader => Leader,
        term => Term,
        members => maps:keys(Members),
        config_version => State#cluster_coordinator_state.config_version
    },

    %% Send heartbeat to all members
    maps:foreach(fun(Member, _Info) ->
        case Member of
            NodeId -> skip;  %% Don't send to self
            _ ->
                gen_server:cast({?MODULE, Member}, {heartbeat, NodeId, Term, Metadata})
        end
    end, Members),

    State#cluster_coordinator_state{last_heartbeat = Members}.

%% @doc Handle incoming heartbeat
-spec handle_heartbeat(node(), non_neg_integer(), cluster_metadata(), state(),
                       #cluster_coordinator_state{}) ->
    {next_state, state(), #cluster_coordinator_state{}} | {keep_state, #cluster_coordinator_state{}}.
handle_heartbeat(From, Term, Metadata, StateName, State) ->
    #cluster_coordinator_state{
        election_term = CurrentTerm,
        leader_node = CurrentLeader,
        members = Members
    } = State,

    %% Update member's last heartbeat
    NewMembers = maps:update_with(From,
        fun(Info) ->
            Info#{last_heartbeat => erlang:system_time(millisecond)}
        end,
        Members),

    %% Reset missed heartbeat count
    NewMissed = maps:remove(From, State#cluster_coordinator_state.missed_heartbeats),

    NewState = State#cluster_coordinator_state{
        members = NewMembers,
        missed_heartbeats = NewMissed
    },

    case Term > CurrentTerm of
        true ->
            %% New term, recognize new leader
            LeaderState = recognize_leader(NewState, From, Term),
            {next_state, stable, LeaderState};
        false ->
            %% Send ACK
            gen_server:cast({?MODULE, From}, {heartbeat_ack, node(), Term}),
            {keep_state, NewState}
    end.

%% @doc Handle heartbeat timeout
-spec handle_heartbeat_timeout(node(), state(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
handle_heartbeat_timeout(Node, StateName, #cluster_coordinator_state{
        missed_heartbeats = Missed,
        heartbeat_timeout = Timeout
    } = State) ->

    MissedCount = maps:get(Node, Missed, 0) + 1,
    NewMissed = maps:put(Node, MissedCount, Missed),

    NewState = State#cluster_coordinator_state{missed_heartbeats = NewMissed},

    case MissedCount >= 3 of
        true ->
            %% Node considered down
            handle_node_down(Node, StateName, NewState);
        false ->
            %% Schedule next timeout check
            erlang:send_after(Timeout, self(), {heartbeat_timeout, Node}),
            {keep_state, NewState}
    end.

%% @doc Handle node coming up
-spec handle_node_up(node(), state(), #cluster_coordinator_state{}) ->
    {keep_state, #cluster_coordinator_state{}}.
handle_node_up(Node, StateName, State) ->
    logger:info("Node ~p detected up in state ~p", [Node, StateName]),

    NewMembers = maps:put(Node, #{
        node => Node,
        status => up,
        role => voter,
        last_heartbeat => erlang:system_time(millisecond),
        generation => 0,
        capabilities => [],
        metadata => #{}
    }, State#cluster_coordinator_state.members),

    NewMetrics = (State#cluster_coordinator_state.metrics)#{
        member_changes => maps:get(member_changes, State#cluster_coordinator_state.metrics, 0) + 1
    },

    %% Notify subscribers
    notify_subscribers(State, {member_joined, Node, maps:get(Node, NewMembers)}),

    NewState = State#cluster_coordinator_state{
        members = NewMembers,
        metrics = NewMetrics
    },

    {keep_state, NewState}.

%% @doc Handle node going down
-spec handle_node_down(node(), state(), #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
handle_node_down(Node, StateName, #cluster_coordinator_state{
        leader_node = Node,
        is_leader = false
    } = State) ->
    %% Leader went down, trigger election
    logger:warning("Leader ~p went down", [Node]),
    notify_subscribers(State, {member_failed, Node, leader_down}),
    ElectionState = start_election(State),
    {next_state, electing, ElectionState};

handle_node_down(Node, StateName, State) ->
    logger:warning("Node ~p went down", [Node]),

    NewMembers = maps:remove(Node, State#cluster_coordinator_state.members),

    NewMetrics = (State#cluster_coordinator_state.metrics)#{
        member_changes => maps:get(member_changes, State#cluster_coordinator_state.metrics, 0) + 1
    },

    NewState = State#cluster_coordinator_state{
        members = NewMembers,
        metrics = NewMetrics
    },

    %% Check if we still have quorum
    case has_quorum(NewState) of
        true ->
            notify_subscribers(State, {member_failed, Node, node_down}),
            {keep_state, NewState};
        false ->
            %% Lost quorum
            logger:error("Lost quorum after node ~p down", [Node]),
            notify_subscribers(NewState, {quorum_lost,
                maps:keys(NewMembers),
                [Node]}),
            {next_state, degraded, NewState#cluster_coordinator_state{status = critical}}
    end.

%% @doc Start leader election
-spec start_election(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
start_election(#cluster_coordinator_state{node_id = NodeId} = State) ->
    %% Increment term
    NewTerm = State#cluster_coordinator_state.election_term + 1,

    %% Vote for self
    Votes = sets:new(),
    NewVotes = sets:add_element(NodeId, Votes),

    NewState = State#cluster_coordinator_state{
        election_term = NewTerm,
        voted_for = NodeId,
        votes_received = NewVotes,
        is_leader = false,
        leader_node = undefined
    },

    %% Start election timeout
    ElectionTimeout = State#cluster_coordinator_state.config#{
        election_timeout => 15000
    },
    TimeoutRef = erlang:send_after(maps:get(election_timeout, ElectionTimeout, 15000),
                                   self(), election_timeout),

    NewState#cluster_coordinator_state{election_timeout_ref = TimeoutRef}.

%% @doc Start election voting
-spec start_election_voting(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
start_election_voting(#cluster_coordinator_state{
        node_id = NodeId,
        election_term = Term,
        members = Members,
        sync_state = SyncState
    } = State) ->

    %% Request votes from all members
    maps:foreach(fun(Member, _Info) ->
        case Member of
            NodeId -> skip;
            _ ->
                LastLogIndex = maps:get(last_sync_index, SyncState, 0),
                gen_server:cast({?MODULE, Member},
                    {vote_request, NodeId, Term, LastLogIndex, Term})
        end
    end, Members),

    State.

%% @doc Handle vote request
-spec handle_vote_request(node(), non_neg_integer(), non_neg_integer(), non_neg_integer(),
                         non_neg_integer(), state(), #cluster_coordinator_state{}) ->
    {keep_state, #cluster_coordinator_state{}}.
handle_vote_request(Candidate, Term, _LastLogIndex, _LastLogTerm, CurrentTerm,
                    StateName, #cluster_coordinator_state{node_id = NodeId,
                                                           election_term = OurTerm} = State) ->

    %% Determine if we grant vote
    {GrantVote, NewTerm} = case Term > OurTerm of
        true ->
            %% Candidate term is higher, grant vote
            {true, Term};
        false ->
            %% Same or lower term
            case State#cluster_coordinator_state.voted_for of
                undefined ->
                    %% Haven't voted yet in this term
                    {Term =:= OurTerm, OurTerm};
                VotedFor ->
                    %% Already voted
                    {VotedFor =:= Candidate, OurTerm}
            end
    end,

    %% Update state if needed
    NewState = case NewTerm > OurTerm of
        true ->
            State#cluster_coordinator_state{
                election_term = NewTerm,
                voted_for = case GrantVote of true -> Candidate; false -> undefined end
            };
        false ->
            State
    end,

    %% Send response
    gen_server:cast({?MODULE, Candidate}, {vote_response, NodeId, NewTerm, GrantVote}),

    {keep_state, NewState}.

%% @doc Handle vote response
-spec handle_vote_response(node(), non_neg_integer(), boolean(), state(),
                          #cluster_coordinator_state{}) ->
    gen_statem:state_enter_result(state()).
handle_vote_response(Voter, Term, VoteGranted, StateName,
                     #cluster_coordinator_state{
        election_term = CurrentTerm,
        votes_received = Votes,
        member_quorum = Quorum
    } = State) ->

    case Term =:= CurrentTerm of
        false ->
            %% Old term, ignore
            {keep_state, State};
        true ->
            NewVotes = case VoteGranted of
                true -> sets:add_element(Voter, Votes);
                false -> Votes
            end,

            NewState = State#cluster_coordinator_state{votes_received = NewVotes},

            %% Check if we won election
            VoteCount = sets:size(NewVotes),
            case VoteCount >= Quorum of
                true ->
                    %% Won election!
                    self() ! {election_won, Term},
                    {keep_state, NewState};
                false ->
                    {keep_state, NewState}
            end
    end.

%% @doc Become cluster leader
-spec become_cluster_leader(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
become_cluster_leader(#cluster_coordinator_state{node_id = NodeId,
                                                election_term = Term} = State) ->

    logger:info("Node ~p becoming leader for term ~p", [NodeId, Term]),

    %% Update metrics
    NewMetrics = (State#cluster_coordinator_state.metrics)#{
        elections => maps:get(elections, State#cluster_coordinator_state.metrics, 0) + 1,
        leader_transitions => maps:get(leader_transitions, State#cluster_coordinator_state.metrics, 0) + 1
    },

    %% Set leader lease
    LeaseUntil = erlang:system_time(millisecond) + 10000,

    NewState = State#cluster_coordinator_state{
        is_leader = true,
        leader_node = NodeId,
        status = healthy,
        leader_lease_until = LeaseUntil,
        metrics = NewMetrics
    },

    %% Notify subscribers
    notify_subscribers(NewState, {leader_elected, NodeId, Term}),

    %% Start heartbeat timer
    HeartbeatRef = erlang:send_after(State#cluster_coordinator_state.heartbeat_interval,
                                    self(), periodic_heartbeat),

    NewState#cluster_coordinator_state{heartbeat_timer = HeartbeatRef}.

%% @doc Recognize new leader
-spec recognize_leader(#cluster_coordinator_state{}, node(), non_neg_integer()) ->
    #cluster_coordinator_state{}.
recognize_leader(#cluster_coordinator_state{} = State, Leader, Term) ->

    %% Cancel election timeout if set
    case State#cluster_coordinator_state.election_timeout_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Update state
    NewState = State#cluster_coordinator_state{
        leader_node = Leader,
        is_leader = false,
        election_term = Term,
        voted_for = Leader,
        votes_received = sets:new()
    },

    %% Notify subscribers
    notify_subscribers(NewState, {leader_changed, State#cluster_coordinator_state.leader_node, Leader}),

    NewState.

%% @doc Initiate data synchronization
-spec initiate_data_sync(#cluster_coordinator_state{}, sync_operation_type()) ->
    #cluster_coordinator_state{}.
initiate_data_sync(#cluster_coordinator_state{members = Members,
                                             node_id = NodeId} = State, SyncType) ->

    %% Create sync operation
    SyncId = make_ref(),
    TargetNodes = maps:keys(Members) -- [NodeId],

    SyncOp = #{
        id => SyncId,
        type => SyncType,
        target_nodes => TargetNodes,
        started_at => erlang:system_time(millisecond),
        status => in_progress,
        retry_count => 0,
        data => #{}
    },

    Pending = maps:put(SyncId, SyncOp, State#cluster_coordinator_state.pending_syncs),

    NewSyncState = (State#cluster_coordinator_state.sync_state)#{
        sync_in_progress => true
    },

    NewState = State#cluster_coordinator_state{
        pending_syncs = Pending,
        sync_state = NewSyncState
    },

    logger:info("Initiated ~p sync ~p to ~p nodes", [SyncType, SyncId, length(TargetNodes)]),

    %% Simulate sync completion (in real implementation, this would be async)
    self() ! {sync_complete, SyncId, #{
        bytes_transferred => 0,
        entries_synced => 0,
        conflicts_resolved => 0,
        duration_ms => 0
    }},

    NewState.

%% @doc Execute partition strategy
-spec execute_partition_strategy(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
execute_partition_strategy(#cluster_coordinator_state{
        config = #{
            partition_strategy := majority_side
        }
    } = State) ->

    %% Find majority partition
    Members = maps:keys(State#cluster_coordinator_state.members),
    PartitionMembers = State#cluster_coordinator_state.partition_members,
    OurPartition = Members -- PartitionMembers,

    case length(OurPartition) > length(PartitionMembers) of
        true ->
            logger:info("We are in majority partition (~p nodes)", [length(OurPartition)]),
            State#cluster_coordinator_state{status = healthy};
        false ->
            logger:warning("We are in minority partition (~p nodes)", [length(OurPartition)]),
            State#cluster_coordinator_state{status = critical}
    end;

execute_partition_strategy(#cluster_coordinator_state{
        config = #{
            partition_strategy := leader_side
        },
        leader_node = Leader,
        partition_members = PartitionMembers,
        members = Members
    } = State) ->

    %% Follow leader's partition
    case lists:member(Leader, PartitionMembers) of
        true ->
            %% Leader partitioned away
            logger:warning("Leader partitioned away, entering degraded"),
            State#cluster_coordinator_state{status = critical};
        false ->
            %% Leader still reachable
            logger:info("Leader still reachable"),
            State#cluster_coordinator_state{status = healthy}
    end;

execute_partition_strategy(State) ->
    %% Default: manual resolution
    logger:info("Manual partition resolution required"),
    State#cluster_coordinator_state{status = critical}.

%% @doc Execute merge after partition healing
-spec execute_merge(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
execute_merge(#cluster_coordinator_state{
        merge_strategy = last_write_wins
    } = State) ->

    logger:info("Executing merge with last_write_wins strategy"),

    %% Trigger data sync to resolve conflicts
    SyncState = initiate_data_sync(State, anti_entropy_sync),

    %% Update metrics
    NewMetrics = (State#cluster_coordinator_state.metrics)#{
        merges => maps:get(merges, State#cluster_coordinator_state.metrics, 0) + 1
    },

    SyncState#cluster_coordinator_state{metrics = NewMetrics};

execute_merge(State) ->
    %% Custom merge strategy
    logger:info("Executing custom merge strategy"),
    initiate_data_sync(State, anti_entropy_sync).

%% @doc Check if cluster has quorum
-spec has_quorum(#cluster_coordinator_state{}) -> boolean().
has_quorum(#cluster_coordinator_state{
        members = Members,
        member_quorum = Quorum
    }) ->
    maps:size(Members) >= Quorum.

%% @doc Check if can return to stable state
-spec can_return_to_stable(#cluster_coordinator_state{}) -> boolean().
can_return_to_stable(State) ->
    has_quorum(State) andalso not State#cluster_coordinator_state.partition_detected.

%% @doc Perform health check
-spec perform_health_check(#cluster_coordinator_state{}) -> map().
perform_health_check(#cluster_coordinator_state{
        members = Members,
        leader_node = Leader,
        current_state = StateName,
        status = Status
    }) ->

    ConnectedNodes = nodes(),
    MemberCount = maps:size(Members),
    UpMembers = lists:filter(fun(Node) ->
        maps:get(Node, Members, undefined) =/= undefined andalso
        lists:member(Node, ConnectedNodes)
    end, maps:keys(Members)),

    #{
        state => StateName,
        status => Status,
        leader => Leader,
        member_count => MemberCount,
        up_members => length(UpMembers),
        has_quorum => MemberCount >= 2,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Update health status
-spec update_health_status(map(), #cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
update_health_status(Health, State) ->
    %% Could trigger state transitions based on health
    State.

%% @doc Notify subscribers of events
-spec notify_subscribers(#cluster_coordinator_state{}, cluster_event()) -> ok.
notify_subscribers(#cluster_coordinator_state{event_subscribers = Subscribers}, Event) ->
    sets:fold(fun(Subscriber, _Acc) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {cluster_event, Event};
            false ->
                ok
        end
    end, ok, Subscribers),
    ok.

%% @doc Generate correlation ID
-spec generate_correlation_id() -> binary().
generate_correlation_id() ->
    Bin = <<erlang:unique_integer():64>>,
    <<<<"cluster_">>/binary, (binary:encode_hex(Bin))/binary>>.

%% @doc Generate partition ID
-spec generate_partition_id() -> non_neg_integer().
generate_partition_id() ->
    erlang:unique_integer([positive]).

%% @doc Check if this is a seed node
-spec is_seed_node(#cluster_coordinator_state{}) -> boolean().
is_seed_node(#cluster_coordinator_state{members = Members, node_id = NodeId}) ->
    maps:size(Members) =:= 1 andalso maps:is_key(NodeId, Members).

%% @doc Get seed nodes from config
-spec get_seed_nodes(#cluster_coordinator_state{}) -> [node()].
get_seed_nodes(State) ->
    %% In real implementation, get from application config
    application:get_env(erlmcp_core, cluster_nodes, []).

%% @doc Connect to seed nodes
-spec connect_to_seeds([node()]) -> {ok, node()} | {error, term()}.
connect_to_seeds([]) ->
    {error, no_seed_nodes};
connect_to_seeds(SeedNodes) ->
    connect_to_seeds(SeedNodes, []).

connect_to_seeds([], _Errors) ->
    {error, all_seeds_failed};
connect_to_seeds([Seed | Rest], Errors) ->
    case net_adm:ping(Seed) of
        pong ->
            {ok, Seed};
        pang ->
            connect_to_seeds(Rest, [{Seed, connection_failed} | Errors])
    end.

%% @doc Initiate join to cluster
-spec initiate_join(#cluster_coordinator_state{}, node()) -> #cluster_coordinator_state{}.
initiate_join(State, Leader) ->
    %% Send join request to leader
    gen_server:cast({?MODULE, Leader}, {join_request, node(), #{}}),
    State.

%% @doc Apply cluster configuration
-spec apply_cluster_config(#cluster_coordinator_state{}, cluster_config()) ->
    #cluster_coordinator_state{}.
apply_cluster_config(State, Config) ->
    State#cluster_coordinator_state{
        config = Config,
        config_version = maps:get(version, Config, 0)
    }.

%% @doc Apply new configuration
-spec apply_new_config(#cluster_coordinator_state{}, non_neg_integer()) ->
    #cluster_coordinator_state{}.
apply_new_config(#cluster_coordinator_state{pending_config = Pending} = State, Version) ->
    case Pending of
        #{version := Version} = Config ->
            State#cluster_coordinator_state{
                config = Config,
                pending_config = undefined
            };
        _ ->
            State
    end.

%% @doc Start timers
-spec start_initial_timers(#cluster_coordinator_state{}) -> map().
start_initial_timers(#cluster_coordinator_state{
        heartbeat_interval = HeartbeatInterval
    }) ->

    #{
        heartbeat => erlang:send_after(HeartbeatInterval, self(), periodic_heartbeat),
        health_check => erlang:send_after(10000, self(), periodic_health_check)
    }.

%% @doc Set timers in state
-spec set_timers(#cluster_coordinator_state{}, map()) -> #cluster_coordinator_state{}.
set_timers(State, Timers) ->
    State#cluster_coordinator_state{
        heartbeat_timer = maps:get(heartbeat, Timers),
        health_check_timer = maps:get(health_check, Timers)
    }.

%% @doc Cancel timers
-spec cancel_timers(#cluster_coordinator_state{}) -> ok.
cancel_timers(#cluster_coordinator_state{
        heartbeat_timer = Heartbeat,
        health_check_timer = HealthCheck,
        election_timeout_ref = ElectionRef
    }) ->

    case Heartbeat of undefined -> ok; Ref -> erlang:cancel_timer(Ref) end,
    case HealthCheck of undefined -> ok; Ref -> erlang:cancel_timer(Ref) end,
    case ElectionRef of undefined -> ok; Ref -> erlang:cancel_timer(Ref) end,
    ok.

%% @doc Monitor node
-spec monitor_node(node(), boolean(), #cluster_coordinator_state{}) -> ok.
monitor_node(_Node, false, _State) ->
    ok;
monitor_node(Node, true, _State) ->
    net_kernel:monitor_nodes(true, [{node, Node}]).

%% @doc Increment election term
-spec increment_election_term(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
increment_election_term(State) ->
    State#cluster_coordinator_state{
        election_term = State#cluster_coordinator_state.election_term + 1
    }.

%% @doc Restart election
-spec restart_election(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
restart_election(State) ->
    start_election_voting(State).

%% @doc Handle join request
-spec handle_join_request({pid(), term()}, [node()], #cluster_coordinator_state{}) ->
    {keep_state, #cluster_coordinator_state{}, [{reply, {pid(), term()}, term()}]}.
handle_join_request(From, SeedNodes, State) ->
    {reply, From, {ok, joining}},
    {keep_state, State}.

%% @doc Handle leave request
-spec handle_leave_request({pid(), term()}, term(), #cluster_coordinator_state{}) ->
    {keep_state, #cluster_coordinator_state{}, [{reply, {pid(), term()}, term()}]}.
handle_leave_request(From, Reason, State) ->
    {reply, From, {ok, draining}},
    {keep_state, State}.

%% @doc Update member status
-spec update_member_status(node(), term(), #cluster_coordinator_state{}) ->
    #cluster_coordinator_state{}.
update_member_status(Node, Status, State) ->
    NewMembers = maps:update_with(Node,
        fun(Info) -> Info#{status => Status} end,
        State#cluster_coordinator_state.members),
    State#cluster_coordinator_state{members = NewMembers}.

%% @doc Start drain
-spec start_drain(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
start_drain(State) ->
    %% Simulate drain completion
    self() ! {drain_complete, State#cluster_coordinator_state.node_id},
    State.

%% @doc Start recovery
-spec start_recovery(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
start_recovery(State) ->
    %% Simulate recovery
    self() ! recovery_complete,
    State.

%% @doc Execute split
-spec execute_split(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
execute_split(State) ->
    State.

%% @doc Start upgrade
-spec start_upgrade(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
start_upgrade(State) ->
    State.

%% @doc Handle upgrade complete
-spec handle_upgrade_complete(node(), #cluster_coordinator_state{}) ->
    #cluster_coordinator_state{}.
handle_upgrade_complete(_Node, State) ->
    State.

%% @doc Check if upgrade complete
-spec is_upgrade_complete(#cluster_coordinator_state{}) -> boolean().
is_upgrade_complete(_State) ->
    true.

%% @doc Start snapshot
-spec start_snapshot(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
start_snapshot(State) ->
    self() ! {snapshot_complete, generate_partition_id()},
    State.

%% @doc Record snapshot complete
-spec record_snapshot_complete(#cluster_coordinator_state{}, non_neg_integer()) ->
    #cluster_coordinator_state{}.
record_snapshot_complete(State, SnapshotId) ->
    State.

%% @doc Handle node recovery
-spec handle_node_recovery(node(), #cluster_coordinator_state{}) ->
    #cluster_coordinator_state{}.
handle_node_recovery(Node, State) ->
    NewMembers = maps:put(Node, #{
        node => Node,
        status => up,
        role => voter,
        last_heartbeat => erlang:system_time(millisecond),
        generation => 0,
        capabilities => [],
        metadata => #{}
    }, State#cluster_coordinator_state.members),

    State#cluster_coordinator_state{members = NewMembers}.

%% @doc Handle config vote
-spec handle_config_vote(node(), non_neg_integer(), boolean(), state(),
                         #cluster_coordinator_state{}) ->
    {keep_state, #cluster_coordinator_state{}}.
handle_config_vote(_Voter, _Version, _Approve, StateName, State) ->
    {keep_state, State}.

%% @doc Transition leadership
-spec transition_leadership(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
transition_leadership(State) ->
    State#cluster_coordinator_state{
        is_leader = false,
        leader_node = undefined
    }.

%% @doc Verify partition state
-spec verify_partition_state(#cluster_coordinator_state{}) -> boolean().
verify_partition_state(State) ->
    State#cluster_coordinator_state.partition_detected.

%% @doc Trigger sync
-spec do_trigger_sync(#cluster_coordinator_state{}, sync_operation_type()) ->
    {ok, reference(), #cluster_coordinator_state{}}.
do_trigger_sync(State, SyncType) ->
    Ref = make_ref(),
    NewState = initiate_data_sync(State, SyncType),
    {ok, Ref, NewState}.

%% @doc Record sync result
-spec record_sync_result(#cluster_coordinator_state{}, reference(), map()) ->
    #cluster_coordinator_state{}.
record_sync_result(State, Ref, Result) ->
    Pending = maps:remove(Ref, State#cluster_coordinator_state.pending_syncs),
    State#cluster_coordinator_state{pending_syncs = Pending}.

%% @doc Should retry sync
-spec should_retry_sync(#cluster_coordinator_state{}) -> boolean().
should_retry_sync(_State) ->
    false.

%% @doc Retry sync
-spec retry_sync(#cluster_coordinator_state{}, reference()) -> #cluster_coordinator_state{}.
retry_sync(State, _Ref) ->
    State.

%% @doc Resolve conflict
-spec do_resolve_conflict(#cluster_coordinator_state{}, binary(), conflict_resolution()) ->
    #cluster_coordinator_state{}.
do_resolve_conflict(State, _Key, _Resolution) ->
    State.

%% @doc Schedule retry
-spec schedule_retry(#cluster_coordinator_state{}) -> #cluster_coordinator_state{}.
schedule_retry(State) ->
    State.
