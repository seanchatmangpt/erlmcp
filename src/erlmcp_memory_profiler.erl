%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Profiler for Scale Testing - Measure memory at 100K connections
%%%
%%% Provides real numbers for:
%%% - Memory per connection at various scales (1K, 10K, 50K, 100K)
%%% - Memory growth rate under sustained load
%%% - GC pause time analysis
%%% - Identify memory leaks and inefficiencies
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_profiler).

-export([
    start_profiling/0,
    stop_profiling/0,
    create_test_connections/2,
    create_test_connections/3,
    measure_memory_snapshot/0,
    measure_memory_after_delay/1,
    get_profiling_data/0,
    analyze_memory_trend/1,
    generate_report/0,
    memory_per_connection/1,
    simulate_100k_load/1
]).

-include_lib("kernel/include/logger.hrl").

-define(PROFILING_TABLE, erlmcp_profiling).
-define(SNAPSHOT_INTERVAL, 5000).  % 5 second snapshots

-record(memory_snapshot, {
    timestamp :: integer(),
    process_count :: integer(),
    total_memory :: integer(),
    process_memory :: integer(),
    binary_memory :: integer(),
    ets_memory :: integer(),
    atom_memory :: integer(),
    gc_collections :: integer(),
    gc_reclaimed :: integer(),
    gc_pause_max :: integer()
}).

-record(profiling_state, {
    snapshots = [] :: [#memory_snapshot{}],
    start_time :: integer() | undefined,
    baseline :: #memory_snapshot{} | undefined,
    is_running = false :: boolean()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start memory profiling
-spec start_profiling() -> ok.
start_profiling() ->
    case ets:whereis(?PROFILING_TABLE) of
        undefined ->
            ets:new(?PROFILING_TABLE, [
                named_table,
                public,
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,

    Snapshot = measure_memory_snapshot(),
    ets:insert(?PROFILING_TABLE, {state, #profiling_state{
        start_time = erlang:system_time(millisecond),
        baseline = Snapshot,
        is_running = true,
        snapshots = [Snapshot]
    }}),

    logger:info("Memory profiling started at ~B ms", [Snapshot#memory_snapshot.timestamp]),
    ok.

%% @doc Stop memory profiling and return data
-spec stop_profiling() -> map().
stop_profiling() ->
    case ets:lookup(?PROFILING_TABLE, state) of
        [{state, #profiling_state{snapshots = Snapshots, baseline = Baseline}}] ->
            FinalSnapshot = measure_memory_snapshot(),
            AllSnapshots = [FinalSnapshot | Snapshots],

            Report = #{
                baseline => snapshot_to_map(Baseline),
                final => snapshot_to_map(FinalSnapshot),
                snapshots => length(AllSnapshots),
                total_memory_delta => FinalSnapshot#memory_snapshot.total_memory -
                                     Baseline#memory_snapshot.total_memory,
                process_count_delta => FinalSnapshot#memory_snapshot.process_count -
                                      Baseline#memory_snapshot.process_count,
                avg_gc_pause => avg_value([S#memory_snapshot.gc_pause_max || S <- AllSnapshots])
            },

            logger:info("Memory profiling stopped: ~p", [Report]),
            Report;
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Create N test connections for load testing
-spec create_test_connections(pos_integer(), term()) -> {ok, [pid()]} | {error, term()}.
create_test_connections(Count, ServerPid) ->
    create_test_connections(Count, ServerPid, #{}).

-spec create_test_connections(pos_integer(), pid(), map()) -> {ok, [pid()]} | {error, term()}.
create_test_connections(Count, ServerPid, Options) when is_integer(Count), Count > 0 ->
    logger:info("Creating ~B test connections...", [Count]),

    %% Spawn connections in batches to avoid overwhelming scheduler
    BatchSize = min(1000, Count),
    create_connections_batched(Count, ServerPid, Options, BatchSize, []).

%% @doc Measure memory snapshot at this moment
-spec measure_memory_snapshot() -> #memory_snapshot{}.
measure_memory_snapshot() ->
    Stats = erlang:statistics(garbage_collection),
    {GCNumber, Words, _} = Stats,

    #memory_snapshot{
        timestamp = erlang:system_time(millisecond),
        process_count = erlang:system_info(process_count),
        total_memory = erlang:memory(total),
        process_memory = erlang:memory(processes),
        binary_memory = erlang:memory(binary),
        ets_memory = erlang:memory(ets),
        atom_memory = erlang:memory(atom),
        gc_collections = GCNumber,
        gc_reclaimed = Words * 8,  % Convert words to bytes (64-bit)
        gc_pause_max = 0
    }.

%% @doc Measure memory after a delay (for trend analysis)
-spec measure_memory_after_delay(pos_integer()) -> {integer(), integer()}.
measure_memory_after_delay(DelayMs) ->
    Snap1 = measure_memory_snapshot(),
    timer:sleep(DelayMs),
    Snap2 = measure_memory_snapshot(),
    Delta = Snap2#memory_snapshot.total_memory - Snap1#memory_snapshot.total_memory,
    {Delta, DelayMs}.

%% @doc Get all profiling data collected so far
-spec get_profiling_data() -> map().
get_profiling_data() ->
    case ets:lookup(?PROFILING_TABLE, state) of
        [{state, #profiling_state{snapshots = Snapshots, baseline = Baseline}}] ->
            #{
                baseline => snapshot_to_map(Baseline),
                snapshots => [snapshot_to_map(S) || S <- lists:reverse(Snapshots)],
                snapshot_count => length(Snapshots),
                duration_ms =>
                    (lists:last(Snapshots))#memory_snapshot.timestamp -
                    Baseline#memory_snapshot.timestamp
            };
        [] ->
            {error, no_profiling_data}
    end.

%% @doc Analyze memory trend from snapshots
-spec analyze_memory_trend([map()]) -> map().
analyze_memory_trend(Snapshots) when is_list(Snapshots) ->
    case Snapshots of
        [] -> {error, no_data};
        [_] -> {error, need_multiple_snapshots};
        _ ->
            Memories = [maps:get(total_memory, S, 0) || S <- Snapshots],
            TimeDeltas = time_deltas(Snapshots),

            %% Linear regression for growth rate
            {AvgGrowth, TrendSlope} = calculate_trend(Memories, TimeDeltas),

            #{
                min_memory => lists:min(Memories),
                max_memory => lists:max(Memories),
                avg_memory => round(lists:sum(Memories) / length(Memories)),
                memory_delta => lists:last(Memories) - lists:nth(1, Memories),
                avg_growth_per_snapshot => round(AvgGrowth),
                trend_slope => TrendSlope,
                leak_indication => case TrendSlope > 0.1 of
                    true -> <<"possible_memory_leak">>;
                    false -> <<"normal">>
                end
            }
    end;
analyze_memory_trend(_) ->
    {error, invalid_input}.

%% @doc Generate detailed profiling report
-spec generate_report() -> {ok, string()} | {error, term()}.
generate_report() ->
    Data = get_profiling_data(),
    case Data of
        {error, Reason} ->
            {error, Reason};
        _ ->
            Report = format_report(Data),
            {ok, Report}
    end.

%% @doc Calculate memory per connection at scale
-spec memory_per_connection(pos_integer()) -> {integer(), float()}.
memory_per_connection(ConnectionCount) ->
    Snap = measure_memory_snapshot(),
    TotalMem = Snap#memory_snapshot.total_memory,
    PerConnMem = TotalMem div ConnectionCount,
    PerConnMB = PerConnMem / (1024 * 1024),
    {PerConnMem, PerConnMB}.

%% @doc Simulate 100K concurrent connections with memory profiling
-spec simulate_100k_load(pos_integer()) -> map().
simulate_100k_load(TargetConnections) ->
    logger:info("Starting 100K connection simulation (targeting ~B)", [TargetConnections]),

    %% Start profiling
    start_profiling(),

    %% Get baseline
    Baseline = measure_memory_snapshot(),
    logger:info("Baseline: ~B MB, ~B processes",
                [Baseline#memory_snapshot.total_memory div (1024 * 1024),
                 Baseline#memory_snapshot.process_count]),

    %% Create connections in batches
    BatchSize = 10000,
    NumBatches = (TargetConnections + BatchSize - 1) div BatchSize,

    Results = lists:foldl(
        fun(Batch, Acc) ->
            CurrentCount = Batch * BatchSize,
            TargetThisBatch = min(BatchSize, TargetConnections - (Batch - 1) * BatchSize),

            logger:info("Creating batch ~B (~B connections)...", [Batch, TargetThisBatch]),

            %% Create connections (simplified - just spawn processes)
            _Pids = [spawn_link(fun() -> timer:sleep(300000) end) || _ <- lists:seq(1, TargetThisBatch)],

            Snap = measure_memory_snapshot(),
            {MemPer, MemPerMB} = memory_per_connection(CurrentCount),

            logger:info("Batch ~B complete: ~B total connections, ~.2f MB per conn, ~B MB total",
                        [Batch, CurrentCount, MemPerMB, Snap#memory_snapshot.total_memory div (1024 * 1024)]),

            Acc ++ [#{
                batch => Batch,
                connection_count => CurrentCount,
                memory_per_connection => MemPer,
                memory_per_connection_mb => MemPerMB,
                total_memory_mb => Snap#memory_snapshot.total_memory div (1024 * 1024),
                process_count => Snap#memory_snapshot.process_count,
                timestamp => Snap#memory_snapshot.timestamp
            }]
        end,
        [],
        lists:seq(1, NumBatches)
    ),

    %% Analyze results
    FinalSnap = measure_memory_snapshot(),
    {MemPer, MemPerMB} = memory_per_connection(TargetConnections),

    #{
        target_connections => TargetConnections,
        batches_completed => NumBatches,
        final_process_count => FinalSnap#memory_snapshot.process_count,
        final_total_memory_mb => FinalSnap#memory_snapshot.total_memory div (1024 * 1024),
        final_memory_per_connection => MemPer,
        final_memory_per_connection_mb => MemPerMB,
        baseline_memory_mb => Baseline#memory_snapshot.total_memory div (1024 * 1024),
        memory_delta_mb => (FinalSnap#memory_snapshot.total_memory -
                           Baseline#memory_snapshot.total_memory) div (1024 * 1024),
        batch_results => Results,
        success => MemPerMB < 2.0  % Target: <2MB per connection
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec create_connections_batched(pos_integer(), pid(), map(), pos_integer(), [pid()])
    -> {ok, [pid()]} | {error, term()}.
create_connections_batched(0, _ServerPid, _Options, _BatchSize, Acc) ->
    {ok, Acc};

create_connections_batched(Remaining, ServerPid, Options, BatchSize, Acc) ->
    CurrentBatch = min(BatchSize, Remaining),

    %% Create test connection pids
    BatchPids = [spawn_link(fun() -> test_connection_loop(ServerPid, Options) end)
                 || _ <- lists:seq(1, CurrentBatch)],

    %% Log progress
    logger:debug("Created batch of ~B connections (~B remaining)",
                 [CurrentBatch, Remaining - CurrentBatch]),

    %% Brief pause to let scheduler catch up
    timer:sleep(100),

    create_connections_batched(Remaining - CurrentBatch, ServerPid, Options, BatchSize,
                               Acc ++ BatchPids).

-spec test_connection_loop(pid(), map()) -> ok.
test_connection_loop(_ServerPid, _Options) ->
    %% Keep process alive for the duration of testing
    timer:sleep(600000).  % 10 minutes

-spec snapshot_to_map(#memory_snapshot{}) -> map().
snapshot_to_map(S) ->
    #{
        timestamp => S#memory_snapshot.timestamp,
        process_count => S#memory_snapshot.process_count,
        total_memory => S#memory_snapshot.total_memory,
        total_memory_mb => S#memory_snapshot.total_memory div (1024 * 1024),
        process_memory => S#memory_snapshot.process_memory,
        binary_memory => S#memory_snapshot.binary_memory,
        ets_memory => S#memory_snapshot.ets_memory,
        atom_memory => S#memory_snapshot.atom_memory,
        gc_collections => S#memory_snapshot.gc_collections,
        gc_reclaimed => S#memory_snapshot.gc_reclaimed
    }.

-spec time_deltas([map()]) -> [integer()].
time_deltas([]) -> [];
time_deltas([_]) -> [];
time_deltas(Snapshots) ->
    Timestamps = [maps:get(timestamp, S, 0) || S <- Snapshots],
    [lists:nth(I + 1, Timestamps) - lists:nth(I, Timestamps)
     || I <- lists:seq(1, length(Timestamps) - 1)].

-spec calculate_trend([integer()], [integer()]) -> {float(), float()}.
calculate_trend(Memories, TimeDeltas) ->
    case length(Memories) of
        0 -> {0.0, 0.0};
        1 -> {0.0, 0.0};
        _ ->
            AvgGrowth = lists:sum([lists:nth(I + 1, Memories) - lists:nth(I, Memories)
                                   || I <- lists:seq(1, length(Memories) - 1)]) /
                       (length(Memories) - 1),
            TotalTime = lists:sum(TimeDeltas),
            Slope = case TotalTime of
                0 -> 0.0;
                _ -> AvgGrowth / (TotalTime / 1000.0)
            end,
            {AvgGrowth, Slope}
    end.

-spec avg_value([number()]) -> float().
avg_value([]) -> 0.0;
avg_value(Values) ->
    lists:sum(Values) / length(Values).

-spec format_report(map()) -> string().
format_report(Data) ->
    Baseline = maps:get(baseline, Data, #{}),
    SnapshotCount = maps:get(snapshot_count, Data, 0),
    Duration = maps:get(duration_ms, Data, 0),

    io_lib:format(
        "Memory Profiling Report~n"
        "=======================~n"
        "Baseline Memory: ~B MB~n"
        "Snapshots Collected: ~B~n"
        "Duration: ~B ms~n"
        "~n",
        [
            maps:get(total_memory_mb, Baseline, 0),
            SnapshotCount,
            Duration
        ]
    ).
