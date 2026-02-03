%% @private
%% @doc Data Replication System for erlmcp v3
%% Implements multi-site data synchronization with RPO compliance
-module(erlmcp_data_replication).

-behaviour(gen_server).

%% API
-export([start_link/0, replicate_data/2, get_replication_status/1,
         force_sync/1, validate_consistency/1, create_snapshot/1]).
-export([register_replication/2, get_replication_config/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_types.hrl").

-record(replication_config, {
    id :: replication_id(),
    source :: node(),
    targets :: [node()],
    data_type :: session | registry | config | log | analytics,
    mode :: sync | async | hybrid,
    interval_ms :: non_neg_integer(),
    max_lag_ms :: non_neg_integer(),
    compression :: boolean(),
    encryption :: boolean(),
    retention :: pos_integer(),  % seconds
    bandwidth_limit :: pos_integer()  % bytes per second
}).

-record(replication_state, {
    configs :: map(),  % replication_id() => replication_config()
    queues :: map(),  % node() => queue()
    metrics :: map(),  % replication_id() => metrics()
    last_sync :: integer(),  % timestamp
    active_connections :: [pid()],
    consensus :: undefined | pid(),  % Raft/Paxos process
    monitor :: pid()  % Consistency monitor
}).

-record(data_chunk, {
    id :: binary(),
    data :: binary(),
    timestamp :: integer(),
    checksum :: binary(),
    compressed :: boolean(),
    encrypted :: boolean(),
    sequence :: non_neg_integer(),
    source :: node()
}).

-record(replication_metrics, {
    bytes_sent :: non_neg_integer(),
    bytes_received :: non_neg_integer(),
    operations :: non_neg_integer(),
    failed_operations :: non_neg_integer(),
    average_latency :: float(),
    last_timestamp :: integer(),
    status :: healthy | degraded | failed
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec replicate_data(replication_id(), data()) -> ok | {error, term()}.
replicate_data(ReplicationId, Data) ->
    gen_server:call(?MODULE, {replicate_data, ReplicationId, Data}, 5000).

-spec get_replication_status(replication_id()) -> {ok, replication_metrics()} | {error, not_found}.
get_replication_status(ReplicationId) ->
    gen_server:call(?MODULE, {get_replication_status, ReplicationId}).

-spec force_sync(replication_id()) -> ok | {error, term()}.
force_sync(ReplicationId) ->
    gen_server:call(?MODULE, {force_sync, ReplicationId}, 30000).

-spec validate_consistency(replication_id()) -> consistency_result().
validate_consistency(ReplicationId) ->
    gen_server:call(?MODULE, {validate_consistency, ReplicationId}, 60000).

-spec create_snapshot(replication_id()) -> snapshot_id() | {error, term()}.
create_snapshot(ReplicationId) ->
    gen_server:call(?MODULE, {create_snapshot, ReplicationId}, 60000).

-spec register_replication(replication_id(), replication_config()) -> ok.
register_replication(ReplicationId, Config) ->
    gen_server:call(?MODULE, {register_replication, ReplicationId, Config}).

-spec get_replication_config(replication_id()) -> {ok, replication_config()} | {error, not_found}.
get_replication_config(ReplicationId) ->
    gen_server:call(?MODULE, {get_replication_config, ReplicationId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Initialize replication state
    State = #replication_state{
        configs = initialize_default_configs(),
        queues = initialize_queues(),
        metrics = initialize_metrics(),
        last_sync = erlang:system_time(millisecond),
        active_connections = [],
        consensus = start_consensus(),
        monitor = start_consistency_monitor()
    },

    % Start replication processes for each configuration
    lists:foreach(fun({ReplicationId, Config}) ->
        start_replication_process(ReplicationId, Config, State)
    end, maps:to_list(State#replication_state.configs)),

    % Start periodic sync
    erlang:send_after(1000, self(), periodic_sync),

    % Initialize metrics
    erlmcp_metrics:register(replication_metrics),

    {ok, State}.

handle_call({replicate_data, ReplicationId, Data}, _From, State) ->
    case maps:get(ReplicationId, State#replication_state.configs, undefined) of
        undefined ->
            {reply, {error, replication_not_found}, State};
        Config ->
            Result = replicate_data_to_targets(ReplicationId, Data, Config, State),
            {reply, Result, State}
    end;

handle_call({get_replication_status, ReplicationId}, _From, State) ->
    case maps:get(ReplicationId, State#replication_state.metrics, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Metrics ->
            {reply, {ok, Metrics}, State}
    end;

handle_call({force_sync, ReplicationId}, _From, State) ->
    case maps:get(ReplicationId, State#replication_state.configs, undefined) of
        undefined ->
            {reply, {error, replication_not_found}, State};
        Config ->
            Result = force_sync_to_targets(ReplicationId, Config, State),
            {reply, Result, State}
    end;

handle_call({validate_consistency, ReplicationId}, _From, State) ->
    case maps:get(ReplicationId, State#replication_state.configs, undefined) of
        undefined ->
            {reply, {error, replication_not_found}, State};
        Config ->
            Result = validate_data_consistency(ReplicationId, Config, State),
            {reply, Result, State}
    end;

handle_call({create_snapshot, ReplicationId}, _From, State) ->
    case maps:get(ReplicationId, State#replication_state.configs, undefined) of
        undefined ->
            {reply, {error, replication_not_found}, State};
        Config ->
            SnapshotId = create_data_snapshot(ReplicationId, Config, State),
            {reply, SnapshotId, State}
    end;

handle_call({register_replication, ReplicationId, Config}, _From, State) ->
    % Validate configuration
    case validate_replication_config(Config) of
        ok ->
            % Register new replication
            NewConfigs = maps:put(ReplicationId, Config, State#replication_state.configs),
            NewMetrics = maps:put(ReplicationId, initialize_metrics(), State#replication_state.metrics),

            % Start replication process
            start_replication_process(ReplicationId, Config, State),

            UpdatedState = State#replication_state{
                configs = NewConfigs,
                metrics = NewMetrics
            },
            {reply, ok, UpdatedState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_replication_config, ReplicationId}, _From, State) ->
    case maps:get(ReplicationId, State#replication_state.configs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Config ->
            {reply, {ok, Config}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(periodic_sync, State) ->
    % Perform periodic sync for all replication configs
    SyncResults = lists:foldl(fun(ReplicationId, Acc) ->
        Config = maps:get(ReplicationId, State#replication_state.configs),
        case Config#replication_config.mode of
            async ->
                sync_data(ReplicationId, Config, State);
            hybrid ->
                % For hybrid mode, check if sync is needed
                case need_sync(ReplicationId, Config, State) of
                    true -> sync_data(ReplicationId, Config, State);
                    false -> ok
                end;
            sync ->
                sync_data(ReplicationId, Config, State)
        end
    end, [], maps:keys(State#replication_state.configs)),

    % Update state
    UpdatedState = update_sync_metrics(SyncResults, State),

    % Schedule next sync
    erlang:send_after(calculate_next_sync_interval(UpdatedState), self(), periodic_sync),
    {noreply, UpdatedState};

handle_info({replication_complete, ReplicationId, Result}, State) ->
    % Handle replication completion
    UpdatedState = handle_replication_completion(ReplicationId, Result, State),
    {noreply, UpdatedState};

handle_info({replication_failed, ReplicationId, Error}, State) ->
    % Handle replication failure
    UpdatedState = handle_replication_failure(ReplicationId, Error, State),
    {noreply, UpdatedState};

handle_info(consensus_update, State) ->
    % Handle consensus updates
    UpdatedState = handle_consensus_update(State),
    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    % Cleanup all connections and processes
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Data Replication
-spec replicate_data_to_targets(replication_id(), data(), replication_config(), state()) -> ok | {error, term()}.
replicate_data_to_targets(ReplicationId, Data, Config, State) ->
    % Create data chunk
    Chunk = create_data_chunk(ReplicationId, Data, Config),

    % Queue data for replication
    case queue_data_for_replication(ReplicationId, Chunk, State) of
        ok ->
            % Send to targets based on mode
            case Config#replication_config.mode of
                sync ->
                    send_sync_replication(ReplicationId, Chunk, Config, State);
                async ->
                    send_async_replication(ReplicationId, Chunk, Config, State);
                hybrid ->
                    send_hybrid_replication(ReplicationId, Chunk, Config, State)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_data_chunk(replication_id(), data(), replication_config()) -> data_chunk().
create_data_chunk(ReplicationId, Data, Config) ->
    Now = erlang:system_time(millisecond),
    Sequence = get_next_sequence(ReplicationId),

    % Compress data if configured
    CompressedData = case Config#replication_config.compression of
        true -> compress_data(Data);
        false -> Data
    end,

    #data_chunk{
        id = generate_chunk_id(ReplicationId, Sequence),
        data = CompressedData,
        timestamp = Now,
        checksum = calculate_checksum(CompressedData),
        compressed = Config#replication_config.compression,
        encrypted = Config#replication_config.encryption,
        sequence = Sequence,
        source = node()
    }.

-spec send_sync_replication(replication_id(), data_chunk(), replication_config(), state()) -> ok | {error, term()}.
send_sync_replication(ReplicationId, Chunk, Config, State) ->
    Targets = Config#replication_config.targets,

    % Send to all targets synchronously
    Results = lists:foldl(fun(Target, Acc) ->
        case send_to_target(Target, Chunk, Config) of
            ok -> Acc;
            {error, Error} -> [Error | Acc]
        end
    end, [], Targets),

    case Results of
        [] ->
            % Update metrics
            update_replication_metrics(ReplicationId, ok, Chunk#data_chunk.data, State),
            ok;
        Errors ->
            % Handle partial failures
            handle_sync_failure(ReplicationId, Chunk, Errors, State)
    end.

-spec send_async_replication(replication_id(), data_chunk(), replication_config(), state()) -> ok.
send_async_replication(ReplicationId, Chunk, Config, State) ->
    Targets = Config#replication_config.targets,

    % Spawn async processes for each target
    lists:foreach(fun(Target) ->
        spawn_link(fun() ->
            case send_to_target(Target, Chunk, Config) of
                ok ->
                    self() ! {async_replication_complete, ReplicationId, Target};
                {error, Error} ->
                    self() ! {async_replication_failed, ReplicationId, Target, Error}
            end
        end)
    end, Targets),

    % Update metrics
    update_replication_metrics(ReplicationId, ok, Chunk#data_chunk.data, State),
    ok.

%% Consensus Management
-spec start_consensus() -> pid().
start_consensus() ->
    % Start Raft consensus for strong consistency
    erlmcp_raft:start([
        {nodes, get_all_nodes()},
        {data_type, replication},
        {election_timeout, 5000},
        {heartbeat_interval, 1000},
        {snapshot_threshold, 10000}
    ]).

-spec handle_consensus_update(state()) -> state().
handle_consensus_update(State) ->
    % Handle Raft consensus updates
    case erlmcp_raft:get_consensus_state() of
        {leader, Node} ->
            % This node is leader, can proceed with replication
            State;
        {follower, Leader} ->
            % Follow leader's decisions
            State;
        {candidate, _} ->
            % In election state, pause replication
            State
    end.

%% Consistency Validation
-spec validate_data_consistency(replication_id(), replication_config(), state()) -> consistency_result().
validate_data_consistency(ReplicationId, Config, State) ->
    Source = Config#replication_config.source,
    Targets = Config#replication_config.targets,

    % Get data from source and all targets
    SourceData = get_data_from_source(ReplicationId, Source),
    TargetDatas = lists:foldl(fun(Target, Acc) ->
        case get_data_from_target(ReplicationId, Target) of
            {ok, Data} -> [Data | Acc];
            {error, _} -> Acc
        end
    end, [], Targets),

    % Compare data
    {Consistent, InconsistentTargets} = compare_data(SourceData, TargetDatas),

    % Calculate consistency percentage
    ConsistencyPercentage = calculate_consistency_percentage(TargetDatas, Consistent, InconsistentTargets),

    % Generate report
    #{
        replication_id => ReplicationId,
        consistency_percentage => ConsistencyPercentage,
        consistent => Consistent,
        total_targets => length(Targets),
        inconsistent_targets => InconsistentTargets,
        timestamp => erlang:system_time(millisecond),
        rpo_compliance => ConsistencyPercentage >= 99.9  % Require 99.9% consistency for RPO
    }.

-spec compare_data(binary(), [binary()]) -> {boolean(), [node()]}.
compare_data(SourceData, TargetDatas) ->
    lists:foldl(fun({Target, TargetData}, {AccConsistent, AccInconsistent}) ->
        case SourceData =:= TargetData of
            true -> {true, AccInconsistent};
            false -> {AccConsistent, [Target | AccInconsistent]}
        end
    end, {true, []}, TargetDatas).

%% Snapshot Management
-spec create_data_snapshot(replication_id(), replication_config(), state()) -> snapshot_id().
create_data_snapshot(ReplicationId, Config, State) ->
    SnapshotId = generate_snapshot_id(ReplicationId),

    % Create snapshot from source
    SourceData = get_full_data(ReplicationId, Config#replication_config.source),

    % Compress and encrypt snapshot
    CompressedData = compress_data(SourceData),
    EncryptedData = case Config#replication_config.encryption of
        true -> encrypt_data(CompressedData);
        false -> CompressedData
    end,

    % Store snapshot
    Snapshot = #{
        id => SnapshotId,
        replication_id => ReplicationId,
        timestamp => erlang:system_time(millisecond),
        data => EncryptedData,
        checksum => calculate_checksum(EncryptedData),
        size => byte_size(EncryptedData)
    },

    erlmcp_storage:store(snapshot, SnapshotId, Snapshot),

    % Copy snapshots to all targets
    lists:foreach(fun(Target) ->
        copy_snapshot_to_target(SnapshotId, Target, Config)
    end, Config#replication_config.targets),

    SnapshotId.

%% State Management
-spec initialize_default_configs() -> map().
initialize_default_configs() ->
    #{
        session_replication => #replication_config{
            id = session_replication,
            source = node_ny,
            targets = [node_london, node_sgp],
            data_type = session,
            mode = sync,
            interval_ms = 5000,
            max_lag_ms = 1000,
            compression = true,
            encryption = true,
            retention = 2592000,  % 30 days
            bandwidth_limit = 1048576  % 1MB/s
        },
        registry_replication => #replication_config{
            id = registry_replication,
            source = node_ny,
            targets = [node_london, node_sgp],
            data_type = registry,
            mode = hybrid,
            interval_ms = 10000,
            max_lag_ms = 5000,
            compression = true,
            encryption = true,
            retention = 2592000,  % 30 days
            bandwidth_limit = 2097152  % 2MB/s
        }
    }.

-spec initialize_queues() -> map().
initialize_queues() ->
    % Initialize replication queues for each target
    lists:foldl(fun(Node, Acc) ->
        Acc#{Node => queue:new()}
    end, #{}, [node_ny, node_london, node_sgp]).

-spec initialize_metrics() -> map().
initialize_metrics() ->
    #replication_metrics{
        bytes_sent = 0,
        bytes_received = 0,
        operations = 0,
        failed_operations = 0,
        average_latency = 0.0,
        last_timestamp = erlang:system_time(millisecond),
        status = healthy
    }.

%% Helper Functions
-spec compress_data(binary()) -> binary().
compress_data(Data) ->
    % Implement compression (zlib)
    Compressed = zlib:compress(Data),
    Compressed.

-spec encrypt_data(binary()) -> binary().
encrypt_data(Data) ->
    % Implement AES-256 encryption
    Key = erlmcp_crypto:get_replication_key(),
    erlmcp_crypto:encrypt(Data, Key).

-spec calculate_checksum(binary()) -> binary().
calculate_checksum(Data) ->
    % Implement SHA-256 checksum
    crypto:hash(sha256, Data).

-spec generate_chunk_id(replication_id(), non_neg_integer()) -> binary().
generate_chunk_id(ReplicationId, Sequence) ->
    list_to_binary(io_lib:format("~s-~10.0B", [ReplicationId, Sequence])).

-spec generate_snapshot_id(replication_id()) -> binary().
generate_snapshot_id(ReplicationId) ->
    Timestamp = erlang:system_time(millisecond),
    list_to_binary(io_lib:format("~s-snapshot-~B", [ReplicationId, Timestamp])).

-spec get_next_sequence(replication_id()) -> non_neg_integer().
get_next_sequence(ReplicationId) ->
    % Implement sequence number generation
    case erlmcp_storage:get(sequence, ReplicationId) of
        {ok, Seq} ->
            NewSeq = Seq + 1,
            erlmcp_storage:store(sequence, ReplicationId, NewSeq),
            NewSeq;
        {error, not_found} ->
            erlmcp_storage:store(sequence, ReplicationId, 1),
            1
    end.

%% Error Handling
-spec handle_sync_failure(replication_id(), data_chunk(), [term()], state()) -> ok | {error, term()}.
handle_sync_failure(ReplicationId, Chunk, Errors, State) ->
    % Check if we can recover from partial failure
    case length(Errors) < length(State#replication_state.active_connections) of
        true ->
            % Partial success - update metrics
            update_replication_metrics(ReplicationId, partial_failure, Chunk#data_chunk.data, State),
            ok;
        false ->
            % Complete failure - trigger alert
            erlmcp_alert:trigger(replication_failed, #{
                replication_id => ReplicationId,
                error => complete_failure,
                targets => Errors
            }),
            {error, complete_failure}
    end.

-spec handle_replication_failure(replication_id(), term(), state()) -> state().
handle_replication_failure(ReplicationId, Error, State) ->
    % Update metrics
    Metrics = maps:get(ReplicationId, State#replication_state.metrics),
    UpdatedMetrics = Metrics#replication_metrics{
        failed_operations = Metrics#replication_metrics.failed_operations + 1,
        status = degraded
    },

    % Check if we need to fail over
    case Error of
        source_unavailable ->
            % Try to promote a target as new source
            handle_source_failure(ReplicationId, State);
        target_unavailable ->
            % Remove target from list temporarily
            handle_target_failure(ReplicationId, Error, State);
        network_error ->
            % Retry after backoff
            schedule_retry(ReplicationId, State)
    end,

    State#replication_state{
        metrics = maps:put(ReplicationId, UpdatedMetrics, State#replication_state.metrics)
    }.

%%====================================================================
%% Test Functions
%%====================================================================

-spec test_replication() -> ok.
test_replication() ->
    % Test basic replication functionality
    TestConfig = #replication_config{
        id = test_replication,
        source = node_ny,
        targets = [node_london],
        data_type = session,
        mode = sync,
        interval_ms = 1000,
        max_lag_ms = 1000,
        compression = false,
        encryption = false,
        retention = 3600,
        bandwidth_limit = 1048576
    },

    % Register test replication
    ok = erlmcp_data_replication:register_replication(test_replication, TestConfig),

    % Test data replication
    TestData = <<"test data">>,
    ok = erlmcp_data_replication:replicate_data(test_replication, TestData),

    % Verify replication
    case erlmcp_data_replication:get_replication_status(test_replication) of
        {ok, Metrics} ->
            case Metrics#replication_metrics.status of
                healthy -> ok;
                _ -> {error, replication_failed}
            end;
        {error, _} ->
            {error, replication_not_found}
    end.

-spec test_consistency_check() -> consistency_result().
test_consistency_check() ->
    % Test consistency validation
    TestConfig = #replication_config{
        id = test_consistency,
        source = node_ny,
        targets = [node_london],
        data_type = session,
        mode = sync,
        interval_ms = 1000,
        max_lag_ms = 1000,
        compression = false,
        encryption = false,
        retention = 3600,
        bandwidth_limit = 1048576
    },

    % Register test replication
    ok = erlmcp_data_replication:register_replication(test_consistency, TestConfig),

    % Test data replication
    TestData = <<"consistency test data">>,
    ok = erlmcp_data_replication:replicate_data(test_consistency, TestData),

    % Validate consistency
    erlmcp_data_replication:validate_consistency(test_consistency).

-spec test_snapshot_creation() -> snapshot_id().
test_snapshot_creation() ->
    % Test snapshot creation
    TestConfig = #replication_config{
        id = test_snapshot,
        source = node_ny,
        targets = [node_london],
        data_type = session,
        mode = sync,
        interval_ms = 1000,
        max_lag_ms = 1000,
        compression = true,
        encryption = false,
        retention = 3600,
        bandwidth_limit = 1048576
    },

    % Register test replication
    ok = erlmcp_data_replication:register_replication(test_snapshot, TestConfig),

    % Create snapshot
    erlmcp_data_replication:create_snapshot(test_snapshot).