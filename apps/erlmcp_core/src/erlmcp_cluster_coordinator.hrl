%% @doc Cluster Coordinator Records for erlmcp v3
%% Defines all state records for cluster coordination state machine

%%%====================================================================
%%% Cluster State Records
%%%====================================================================

%% @doc Cluster lifecycle states
-type cluster_state() ::
    forming      | %% Initial cluster formation
    joining      | %% Node joining existing cluster
    stable       | %% Normal operation
    partitioning | %% Network partition detected
    merging      | %% Partition healing in progress
    splitting    | %% Controlled cluster split
    draining     | %% Node leaving cluster gracefully
    shutdown     | %% Cluster shutdown in progress
    recovery     | %% Recovery from failure
    degraded     | %% Operating with reduced capacity
    upgrading    | %% Rolling upgrade in progress
    resyncing    | %% Data resynchronization after partition
    voting       | %% Consensus voting in progress
    electing     | %% Leader election in progress
    snapshotting. %% Snapshot creation in progress

-type cluster_status() ::
    healthy      | %% All nodes operational
    degraded     | %% Some nodes down but quorum exists
    critical     | %% Quorum lost, read-only mode
    recovering   | %% Recovery in progress
    failed       | %% Cluster failed
    maintenance  | %% Scheduled maintenance
    upgrading    | %% Rolling upgrade in progress
    partitioned  | %% Network partition detected
    split_brain  | %% Multiple leaders detected (split-brain)
    resyncing    | %% Data synchronization in progress
    snapshotting. %% Snapshot in progress

%% @doc Cluster coordinator state
-record(cluster_coordinator_state,
        {
         %% Cluster identification
         cluster_name :: binary(),
         node_id :: node(),

         %% Current state
         current_state :: cluster_state(),
         previous_state :: cluster_state() | undefined,
         status :: cluster_status(),

         %% Leadership
         leader_node :: node() | undefined,
         is_leader :: boolean(),
         election_term :: non_neg_integer(),
         voted_for :: node() | undefined,
         votes_received :: sets:set(node()),
         election_timeout :: reference() | undefined,
         leader_lease_until :: integer() | undefined, %% Timestamp

         %% Membership
         members :: #{node() => member_info()},
         joining_nodes :: #{node() => join_request()},
         leaving_nodes :: #{node() => leave_request()},
         member_quorum :: pos_integer(),

         %% Configuration
         config :: cluster_config(),
         pending_config :: cluster_config() | undefined,
         config_version :: non_neg_integer(),

         %% Heartbeats
         heartbeat_interval :: pos_integer(),
         heartbeat_timeout :: pos_integer(),
         last_heartbeat :: #{node() => integer()}, %% Node -> timestamp
         missed_heartbeats :: #{node() => pos_integer()},

         %% Data synchronization
         sync_state :: sync_state(),
         pending_syncs :: #{reference() => sync_operation()},

         %% Partition handling
         partition_detected :: boolean(),
         partition_id :: non_neg_integer() | undefined,
         partition_members :: [node()],
         merge_strategy :: merge_strategy(),

         %% Failover
         failover_in_progress :: boolean(),
         failover_queue :: queue:queue(),
         recovered_nodes :: [node()],

         %% Metrics
         metrics :: cluster_metrics(),

         %% Timers
         state_timer :: reference() | undefined,
         heartbeat_timer :: reference() | undefined,
         sync_timer :: reference() | undefined,
         health_check_timer :: reference() | undefined,
         snapshot_timer :: reference() | undefined,

         %% Monitoring
         monitored_processes :: #{pid() => monitor_ref()},
         partition_monitors :: #{node() => reference()},

         %% Retry state
         retry_count :: non_neg_integer(),
         max_retries :: pos_integer(),
         backoff_ms :: non_neg_integer(),

         %% Event handlers
         event_subscribers :: sets:set(pid()),

         %% OTEL/correlation
         correlation_id :: binary() | undefined,
         trace_context :: map() | undefined
        }).

-type member_info() :: #{
        node => node(),
        status => up | down | joining | leaving,
        role => leader | follower | voter | observer,
        last_heartbeat => integer(),
        generation => non_neg_integer(),
        capabilities => [binary()],
        metadata => map()
       }.

-type join_request() :: #{
        node => node(),
        requested_at => integer(),
        status => pending | approved | rejected,
        voter_approval => #{node() => boolean()},
        metadata => map()
       }.

-type leave_request() :: #{
        node => node(),
        requested_at => integer(),
        status => pending | approved | completing,
        drain_complete => boolean(),
        metadata => map()
       }.

-type cluster_config() :: #{
        version => non_neg_integer(),
        members => [node()],
        quorum => pos_integer(),
        heartbeat_interval => pos_integer(),
        election_timeout => pos_integer(),
        sync_strategy => sync_strategy(),
        partition_strategy => partition_strategy(),
        failover_strategy => failover_strategy(),
        metadata => map()
       }.

-type sync_strategy() ::
    anti_entropy      | %% Full state comparison and sync
    merkle_tree       | %% Merkle tree based diff sync
    vector_clock      | %% Vector clock based conflict resolution
    gossip            | %% Gossip protocol propagation
    last_write_wins   | %% Timestamp-based resolution
    causal            | %% Causal consistency
    custom.           %% Custom sync strategy

-type partition_strategy() ::
    majority_side     | %% Follow majority partition
    leader_side       | %% Follow leader's partition
    quorum_side       | %% Follow partition with quorum
    manual            | %% Manual resolution required
    auto_merge        | %% Automatic merge when healed
    custom.           %% Custom strategy

-type failover_strategy() ::
    automatic         | %% Automatic failover to next candidate
    manual            | %% Manual failover required
    weighted          | %% Weighted node selection
    load_aware        | %% Consider current load
    geo_preferred     | %% Prefer same geographic region
    custom.           %% Custom strategy

-type merge_strategy() ::
    last_write_wins   | %% Last write wins by timestamp
        vector_clock    | %% Use vector clocks for causality
        operational      | %% CRDT operational transforms
        manual           | %% Manual conflict resolution
        leader_decides   | %% Leader resolves conflicts
        custom.           %% Custom merge strategy

-type sync_state() :: #{
        last_sync_index => non_neg_integer(),
        last_sync_time => integer(),
        sync_in_progress => boolean(),
        sync_queue => queue:queue(),
        conflicts => #{binary() => conflict_info()}
       }.

-type conflict_info() :: #{
        key => binary(),
        versions => [sync_version()],
        detected_at => integer(),
        resolution => conflict_resolution() | undefined
       }.

-type sync_version() :: #{
        node => node(),
        value => term(),
        timestamp => integer(),
        vector_clock => term()
       }.

-type conflict_resolution() ::
    local_wins       |
    remote_wins      |
    merge            |
    manual           |
    retry.

-type sync_operation() :: #{
        id => reference(),
        type => sync_operation_type(),
        target_nodes => [node()],
        started_at => integer(),
        status => pending | in_progress | completed | failed,
        retry_count => non_neg_integer(),
        data => map()
       }.

-type sync_operation_type() ::
    full_sync         |
    incremental_sync  |
    config_sync       |
    membership_sync   |
    snapshot_transfer |
    anti_entropy_sync.

-type cluster_metrics() :: #{
        state_changes => non_neg_integer(),
        elections => non_neg_integer(),
        partitions => non_neg_integer(),
        merges => non_neg_integer(),
        failovers => non_neg_integer(),
        sync_operations => non_neg_integer(),
        conflicts_resolved => non_neg_integer(),
        uptime_seconds => non_neg_integer(),
        last_state_change => integer(),
        leader_transitions => non_neg_integer(),
        member_changes => non_neg_integer(),
        heartbeat_failures => non_neg_integer(),
        sync_bytes_transferred => non_neg_integer()
       }.

%%%====================================================================
%%% Event Records
%%%====================================================================

%% @doc Cluster lifecycle events
-type cluster_event() ::
    {state_changed, cluster_state(), cluster_state()} |
    {member_joined, node(), member_info()} |
    {member_left, node(), member_info()} |
    {member_failed, node(), term()} |
    {member_recovered, node()} |
    {leader_elected, node(), non_neg_integer()} |
    {leader_changed, node(), node()} |
    {partition_detected, non_neg_integer(), [node()]} |
    {partition_healed, non_neg_integer()} |
    {merge_complete, [node()]} |
    {split_complete, [node()], [node()]} |
    {sync_complete, reference(), sync_result()} |
    {sync_failed, reference(), term()} |
    {conflict_detected, binary(), conflict_info()} |
    {conflict_resolved, binary(), conflict_resolution()} |
    {failover_started, node(), node()} |
    {failover_complete, node()} |
    {config_changed, cluster_config(), cluster_config()} |
    {quorum_lost, [node()], [node()]} |
    {quorum_regained, [node()]} |
    {snapshot_complete, non_neg_integer()} |
    {election_timeout, non_neg_integer()} |
    {heartbeat_timeout, node()} |
    {shutdown_initiated, term()} |
    {recovery_started, term()} |
    {recovery_complete}.

-type sync_result() :: #{
        bytes_transferred => non_neg_integer(),
        entries_synced => non_neg_integer(),
        conflicts_resolved => non_neg_integer(),
        duration_ms => non_neg_integer()
       }.

%%%====================================================================
%%% Message Records
%%%====================================================================

%% @doc Coordination protocol messages
-type coord_message() ::
    {heartbeat, node(), integer(), cluster_metadata()} |
    {heartbeat_ack, node(), integer()} |
    {vote_request, node(), non_neg_integer(), integer(), term()} |
    {vote_response, node(), non_neg_integer(), boolean()} |
    {sync_request, node(), sync_operation(), term()} |
    {sync_response, node(), reference(), sync_result()} |
    {config_proposal, node(), cluster_config()} |
    {config_vote, node(), non_neg_integer(), boolean()} |
    {partition_detected, node(), [node()]} |
    {merge_request, node(), merge_strategy()} |
    {merge_response, node(), boolean(), map()} |
    {failover_request, node(), failover_strategy()} |
    {failover_ack, node(), boolean()} |
    {snapshot_request, node()} |
    {snapshot_data, node(), binary()} |
    {leave_request, node(), term()} |
    {leave_ack, node(), boolean()} |
    {join_request, node(), map()} |
    {join_response, node(), boolean(), map()}.

-type cluster_metadata() :: #{
        state => cluster_state(),
        status => cluster_status(),
        leader => node() | undefined,
        term => non_neg_integer(),
        members => [node()],
        config_version => non_neg_integer()
       }.

-type monitor_ref() :: reference().
-type join_request() :: #{}.
-type leave_request() :: #{}.
