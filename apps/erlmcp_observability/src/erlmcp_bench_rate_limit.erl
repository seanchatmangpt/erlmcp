%%%-------------------------------------------------------------------
%% @doc Rate Limiter Benchmark Module
%%
%% Comprehensive benchmarks for rate limiting algorithms:
%% - Token bucket algorithm overhead
%% - Sliding window algorithm overhead
%% - Leaky bucket algorithm overhead
%% - Per-client isolation overhead
%% - Global rate limit overhead
%% - Distributed limits with gproc
%% - Priority system overhead
%% - Middleware integration overhead
%%
%% Target: <1% overhead on request processing
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_rate_limit).

-include_lib("kernel/include/logger.hrl").

%% API
-export([run/1, run_all/0, benchmark_token_bucket/1, benchmark_sliding_window/1,
         benchmark_leaky_bucket/1, benchmark_per_client_isolation/1, benchmark_global_limit/1,
         benchmark_distributed_limits/1, benchmark_priority_system/1,
         benchmark_middleware_overhead/1]).

%% Benchmark workload IDs
-define(WORKLOAD_TOKEN_BUCKET_1K, <<"rate_limit_token_bucket_1k">>).
-define(WORKLOAD_TOKEN_BUCKET_10K, <<"rate_limit_token_bucket_10k">>).
-define(WORKLOAD_TOKEN_BUCKET_100K, <<"rate_limit_token_bucket_100k">>).
-define(WORKLOAD_SLIDING_WINDOW_1K, <<"rate_limit_sliding_window_1k">>).
-define(WORKLOAD_LEAKY_BUCKET_1K, <<"rate_limit_leaky_bucket_1k">>).
-define(WORKLOAD_PER_CLIENT_100, <<"rate_limit_per_client_100">>).
-define(WORKLOAD_GLOBAL_10K, <<"rate_limit_global_10k">>).
-define(WORKLOAD_DISTRIBUTED_1K, <<"rate_limit_distributed_1k">>).
-define(WORKLOAD_PRIORITY_1K, <<"rate_limit_priority_1k">>).
-define(WORKLOAD_MIDDLEWARE_1K, <<"rate_limit_middleware_1k">>).

%%====================================================================
%% API Functions
%%====================================================================

-spec run(binary()) -> ok.
run(WorkloadId) ->
    case WorkloadId of
        ?WORKLOAD_TOKEN_BUCKET_1K ->
            benchmark_token_bucket(1000);
        ?WORKLOAD_TOKEN_BUCKET_10K ->
            benchmark_token_bucket(10000);
        ?WORKLOAD_TOKEN_BUCKET_100K ->
            benchmark_token_bucket(100000);
        ?WORKLOAD_SLIDING_WINDOW_1K ->
            benchmark_sliding_window(1000);
        ?WORKLOAD_LEAKY_BUCKET_1K ->
            benchmark_leaky_bucket(1000);
        ?WORKLOAD_PER_CLIENT_100 ->
            benchmark_per_client_isolation(100);
        ?WORKLOAD_GLOBAL_10K ->
            benchmark_global_limit(10000);
        ?WORKLOAD_DISTRIBUTED_1K ->
            benchmark_distributed_limits(1000);
        ?WORKLOAD_PRIORITY_1K ->
            benchmark_priority_system(1000);
        ?WORKLOAD_MIDDLEWARE_1K ->
            benchmark_middleware_overhead(1000);
        _ ->
            logger:error("Unknown workload: ~s", [WorkloadId]),
            {error, unknown_workload}
    end.

-spec run_all() -> ok.
run_all() ->
    Workloads =
        [?WORKLOAD_TOKEN_BUCKET_1K,
         ?WORKLOAD_TOKEN_BUCKET_10K,
         ?WORKLOAD_TOKEN_BUCKET_100K,
         ?WORKLOAD_SLIDING_WINDOW_1K,
         ?WORKLOAD_LEAKY_BUCKET_1K,
         ?WORKLOAD_PER_CLIENT_100,
         ?WORKLOAD_GLOBAL_10K,
         ?WORKLOAD_DISTRIBUTED_1K,
         ?WORKLOAD_PRIORITY_1K,
         ?WORKLOAD_MIDDLEWARE_1K],

    logger:info("Running all rate limit benchmarks (~p workloads)...", [length(Workloads)]),

    lists:foreach(fun(WorkloadId) ->
                     logger:info("Running workload: ~s", [WorkloadId]),
                     run(WorkloadId),
                     timer:sleep(1000)  % Brief pause between workloads
                  end,
                  Workloads),

    ok.

%%====================================================================
%% Benchmark Implementations
%%====================================================================

%% @doc Benchmark token bucket algorithm
benchmark_token_bucket(NumOperations) ->
    logger:info("Benchmarking token bucket algorithm (~p operations)...", [NumOperations]),

    % Setup
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    ClientId = <<"bench_token_bucket">>,

    % Warm-up
    warmup(fun() ->
              TimeMs = erlang:system_time(millisecond),
              erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs)
           end,
           100),

    % Benchmark
    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
                     TimeMs = erlang:system_time(millisecond),
                     erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs)
                  end,
                  lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,

    logger:info("Token bucket benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    % Cleanup
    erlmcp_rate_limiter:stop(),

    ok.

%% @doc Benchmark sliding window algorithm
benchmark_sliding_window(NumOperations) ->
    logger:info("Benchmarking sliding window algorithm (~p operations)...", [NumOperations]),

    % Create window
    Window = erlmcp_rate_limiter:create_sliding_window(1000),

    StartTime = erlang:monotonic_time(microsecond),

    % Run operations
    lists:foldl(fun(_, Acc) ->
                   TimeMs = erlang:system_time(millisecond),
                   case erlmcp_rate_limiter:check_sliding_window(Acc, 100, TimeMs) of
                       {ok, NewWindow, _} ->
                           NewWindow;
                       {error, exceeded} ->
                           Acc
                   end
                end,
                Window,
                lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,

    logger:info("Sliding window benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    ok.

%% @doc Benchmark leaky bucket algorithm
benchmark_leaky_bucket(NumOperations) ->
    logger:info("Benchmarking leaky bucket algorithm (~p operations)...", [NumOperations]),

    % Create bucket
    Bucket = erlmcp_rate_limiter:create_leaky_bucket(100),

    StartTime = erlang:monotonic_time(microsecond),

    % Run operations
    lists:foldl(fun(_, Acc) ->
                   case erlmcp_rate_limiter:check_leaky_bucket(Acc, 100) of
                       {ok, NewBucket, _} ->
                           NewBucket;
                       {error, exceeded} ->
                           Acc
                   end
                end,
                Bucket,
                lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,

    logger:info("Leaky bucket benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    ok.

%% @doc Benchmark per-client isolation
benchmark_per_client_isolation(NumClients) ->
    logger:info("Benchmarking per-client isolation (~p clients)...", [NumClients]),

    % Setup
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    % Generate client IDs
    ClientIds = [list_to_binary("client_" ++ integer_to_list(N)) || N <- lists:seq(1, NumClients)],

    StartTime = erlang:monotonic_time(microsecond),

    % Each client makes 100 requests
    lists:foreach(fun(ClientId) ->
                     lists:foreach(fun(_) ->
                                      TimeMs = erlang:system_time(millisecond),
                                      erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs)
                                   end,
                                   lists:seq(1, 100))
                  end,
                  ClientIds),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    TotalOps = NumClients * 100,
    ThroughputPerS = TotalOps / DurationS,
    LatencyP50Us = DurationUs / TotalOps,

    logger:info("Per-client isolation benchmark complete:"),
    logger:info("  Clients: ~p", [NumClients]),
    logger:info("  Total operations: ~p", [TotalOps]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    % Cleanup
    erlmcp_rate_limiter:stop(),

    ok.

%% @doc Benchmark global rate limit
benchmark_global_limit(NumOperations) ->
    logger:info("Benchmarking global rate limit (~p operations)...", [NumOperations]),

    % Setup
    application:ensure_all_started(erlmcp),
    application:set_env(erlmcp,
                        rate_limiting,
                        #{global_max_messages_per_sec => 1000000, enabled => true}),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
                     TimeMs = erlang:system_time(millisecond),
                     erlmcp_rate_limiter:check_global_rate(TimeMs)
                  end,
                  lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,

    logger:info("Global rate limit benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    % Cleanup
    erlmcp_rate_limiter:stop(),

    ok.

%% @doc Benchmark distributed limits with gproc
benchmark_distributed_limits(NumOperations) ->
    logger:info("Benchmarking distributed limits (~p operations)...", [NumOperations]),

    % Setup
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    Scope = <<"bench_distributed">>,

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) -> erlmcp_rate_limiter:increment_distributed_limit(Scope, 1) end,
                  lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,

    % Get final count
    {ok, FinalCount} = erlmcp_rate_limiter:get_distributed_limit(Scope, node()),

    logger:info("Distributed limits benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Final count: ~p", [FinalCount]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    % Cleanup
    erlmcp_rate_limiter:stop(),

    ok.

%% @doc Benchmark priority system overhead
benchmark_priority_system(NumOperations) ->
    logger:info("Benchmarking priority system (~p operations)...", [NumOperations]),

    % Setup
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    ClientId = <<"bench_priority">>,

    % Set high priority
    erlmcp_rate_limiter:set_client_priority(ClientId, high),

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
                     TimeMs = erlang:system_time(millisecond),
                     erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs, high)
                  end,
                  lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,

    logger:info("Priority system benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),

    % Cleanup
    erlmcp_rate_limiter:stop(),

    ok.

%% @doc Benchmark middleware overhead
benchmark_middleware_overhead(NumOperations) ->
    logger:info("Benchmarking middleware overhead (~p operations)...", [NumOperations]),

    % Setup
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_rate_limiter:start_link(),
    {ok, _} = erlmcp_rate_limit_middleware:start_link(),

    ClientId = <<"bench_middleware">>,
    Method = <<"tools/call">>,

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
                     TimeMs = erlang:system_time(millisecond),
                     erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs)
                  end,
                  lists:seq(1, NumOperations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,

    ThroughputPerS = NumOperations / DurationS,
    LatencyP50Us = DurationUs / NumOperations,
    OverheadPct = LatencyP50Us / 10.0 * 100,  % Assuming 10us base request time

    logger:info("Middleware overhead benchmark complete:"),
    logger:info("  Operations: ~p", [NumOperations]),
    logger:info("  Duration: ~.2f s", [DurationS]),
    logger:info("  Throughput: ~.0f ops/sec", [ThroughputPerS]),
    logger:info("  Latency (p50): ~.2f us", [LatencyP50Us]),
    logger:info("  Overhead: ~.2f%", [OverheadPct]),

    % Cleanup
    erlmcp_rate_limit_middleware:stop(),
    erlmcp_rate_limiter:stop(),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Warm up the benchmark
warmup(Fun, NumIterations) ->
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, NumIterations)).
