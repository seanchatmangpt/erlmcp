%%%====================================================================
%%% erlmcp_bench_pool.erl - Connection Pool Benchmark
%%%====================================================================
%%%
%%% Benchmark connection pooling performance for high-throughput scenarios.
%%%
%%% Workloads:
%%% - pool_10_to_1000: Scale from 10 to 1000 connections
%%% - pool_strategies: Compare round_robin, least_loaded, random
%%% - pool_utilization: Measure throughput at various utilization levels
%%% - pool_vs_no_pool: Compare pooled vs direct connections
%%%
%%% Target: 2x improvement for 100+ concurrent clients
%%%
%%% Usage:
%%%   rebar3 shell
%%%   erlmcp_bench_pool:run_all().
%%%   erlmcp_bench_pool:run(<<"pool_10_to_1000">>).
%%%
%%%====================================================================

-module(erlmcp_bench_pool).

-export([
    run_all/0,
    run/1,
    list_workloads/0
]).

-export([dummy_worker_start_link/1]).

-record(workload, {
    id :: binary(),
    pool_sizes :: [pos_integer()],
    strategies :: [atom()],
    duration_s :: pos_integer(),
    concurrent_clients :: pos_integer(),
    operations_per_client :: pos_integer()
}).

-record(metrics, {
    workload_id :: binary(),
    benchmark = <<"pool">> :: binary(),
    pool_size :: pos_integer(),
    strategy :: atom(),
    duration_s :: pos_integer(),
    total_operations :: non_neg_integer(),
    throughput_ops_per_s :: float(),
    latency_p50_us :: float(),
    latency_p95_us :: float(),
    latency_p99_us :: float(),
    avg_checkout_time_us :: float(),
    pool_utilization_percent :: float(),
    failed_checkouts :: non_neg_integer(),
    concurrent_clients :: pos_integer(),
    timestamp :: integer(),
    environment :: binary()
}).

%%====================================================================
%% Dummy Worker for Benchmarking
%%====================================================================

dummy_worker_start_link(_Opts) ->
    Pid = spawn_link(fun() -> dummy_worker_loop() end),
    {ok, Pid}.

dummy_worker_loop() ->
    receive
        {work, From} ->
            %% Simulate work
            timer:sleep(rand:uniform(5)),
            From ! {done, self()},
            dummy_worker_loop();
        stop ->
            ok
    after 60000 ->
        ok
    end.

%%====================================================================
%% Workload Definitions
%%====================================================================

workloads() ->
    #{
        pool_10_to_1000 => #workload{
            id = <<"pool_10_to_1000">>,
            pool_sizes = [10, 50, 100, 250, 500, 1000],
            strategies = [round_robin],
            duration_s = 30,
            concurrent_clients = 100,
            operations_per_client = 1000
        },

        pool_strategies => #workload{
            id = <<"pool_strategies">>,
            pool_sizes = [50],
            strategies = [round_robin, least_loaded, random],
            duration_s = 30,
            concurrent_clients = 100,
            operations_per_client = 1000
        },

        pool_utilization => #workload{
            id = <<"pool_utilization">>,
            pool_sizes = [100],
            strategies = [round_robin],
            duration_s = 30,
            concurrent_clients = [10, 25, 50, 75, 100, 150],
            operations_per_client = 1000
        },

        pool_vs_no_pool => #workload{
            id = <<"pool_vs_no_pool">>,
            pool_sizes = [0, 100],  % 0 = no pooling
            strategies = [round_robin],
            duration_s = 30,
            concurrent_clients = 100,
            operations_per_client = 1000
        }
    }.

%%====================================================================
%% Public API
%%====================================================================

run_all() ->
    WorkloadIds = [<<"pool_10_to_1000">>, <<"pool_strategies">>,
                   <<"pool_utilization">>, <<"pool_vs_no_pool">>],
    lists:foreach(fun(Id) -> run(Id) end, WorkloadIds).

run(WorkloadId) ->
    case maps:get(WorkloadId, workloads(), undefined) of
        undefined ->
            io:format("Unknown workload: ~p~n", [WorkloadId]),
            {error, unknown_workload};
        Workload ->
            io:format("~n=== Running Pool Benchmark: ~s ===~n", [WorkloadId]),
            run_workload(Workload)
    end.

list_workloads() ->
    maps:keys(workloads()).

%%====================================================================
%% Benchmark Execution
%%====================================================================

run_workload(#workload{id = <<"pool_utilization">>} = Workload) ->
    %% Special case: vary concurrent clients
    Results = lists:map(fun(ClientCount) ->
        run_pool_benchmark(
            100,  % Fixed pool size
            round_robin,
            Workload#workload.duration_s,
            ClientCount,
            Workload#workload.operations_per_client
        )
    end, Workload#workload.concurrent_clients),

    print_results(Workload#workload.id, Results);

run_workload(#workload{id = <<"pool_vs_no_pool">>} = Workload) ->
    %% Compare pooled vs non-pooled
    [NoPoolSize, PoolSize] = Workload#workload.pool_sizes,

    %% No pool benchmark
    NoPoolResult = run_no_pool_benchmark(
        Workload#workload.duration_s,
        Workload#workload.concurrent_clients,
        Workload#workload.operations_per_client
    ),

    %% With pool benchmark
    PoolResult = run_pool_benchmark(
        PoolSize,
        round_robin,
        Workload#workload.duration_s,
        Workload#workload.concurrent_clients,
        Workload#workload.operations_per_client
    ),

    io:format("~n--- NO POOL ---~n"),
    print_result(NoPoolResult),

    io:format("~n--- WITH POOL (size=~p) ---~n", [PoolSize]),
    print_result(PoolResult),

    %% Calculate improvement
    NoPoolThroughput = NoPoolResult#metrics.throughput_ops_per_s,
    PoolThroughput = PoolResult#metrics.throughput_ops_per_s,
    Improvement = (PoolThroughput / NoPoolThroughput) * 100,

    io:format("~nIMPROVEMENT: ~.2f% (~.2fx faster)~n",
              [Improvement - 100, PoolThroughput / NoPoolThroughput]);

run_workload(Workload) ->
    %% Standard workload with multiple pool sizes and strategies
    Results = lists:flatten([
        [run_pool_benchmark(
            PoolSize,
            Strategy,
            Workload#workload.duration_s,
            Workload#workload.concurrent_clients,
            Workload#workload.operations_per_client
        ) || Strategy <- Workload#workload.strategies]
        || PoolSize <- Workload#workload.pool_sizes
    ]),

    print_results(Workload#workload.id, Results).

%%====================================================================
%% Pool Benchmark
%%====================================================================

run_pool_benchmark(PoolSize, Strategy, DurationS, ConcurrentClients, OpsPerClient) ->
    %% Start pool
    PoolOpts = #{
        min_size => PoolSize,
        max_size => PoolSize,
        strategy => Strategy,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(PoolOpts),

    %% Run benchmark
    StartTime = erlang:monotonic_time(microsecond),
    Latencies = run_clients_with_pool(Pool, ConcurrentClients, OpsPerClient),
    EndTime = erlang:monotonic_time(microsecond),

    %% Collect metrics
    PoolMetrics = erlmcp_pool_manager:get_metrics(Pool),

    %% Stop pool
    erlmcp_pool_manager:stop(Pool),

    %% Calculate results
    Duration = (EndTime - StartTime) / 1_000_000,  % Convert to seconds
    TotalOps = length(Latencies),
    Throughput = TotalOps / Duration,

    {P50, P95, P99} = calculate_percentiles(Latencies),

    #metrics{
        workload_id = list_to_binary(io_lib:format("pool_~p_~p", [PoolSize, Strategy])),
        pool_size = PoolSize,
        strategy = Strategy,
        duration_s = round(Duration),
        total_operations = TotalOps,
        throughput_ops_per_s = Throughput,
        latency_p50_us = P50,
        latency_p95_us = P95,
        latency_p99_us = P99,
        avg_checkout_time_us = maps:get(avg_checkout_time_us, PoolMetrics),
        pool_utilization_percent = maps:get(pool_utilization_percent, PoolMetrics),
        failed_checkouts = maps:get(failed_checkouts, PoolMetrics),
        concurrent_clients = ConcurrentClients,
        timestamp = erlang:system_time(second),
        environment = get_environment()
    }.

run_clients_with_pool(Pool, ClientCount, OpsPerClient) ->
    Parent = self(),

    %% Spawn clients
    Clients = [spawn(fun() ->
        Latencies = lists:map(fun(_) ->
            StartTime = erlang:monotonic_time(microsecond),

            %% Checkout from pool
            case erlmcp_pool_manager:checkout(Pool, 5000) of
                {ok, Worker} ->
                    %% Simulate work
                    Worker ! {work, self()},
                    receive
                        {done, Worker} ->
                            ok
                    after 1000 ->
                        ok
                    end,

                    %% Checkin to pool
                    erlmcp_pool_manager:checkin(Pool, Worker),

                    EndTime = erlang:monotonic_time(microsecond),
                    EndTime - StartTime;
                {error, _} ->
                    0  % Failed checkout
            end
        end, lists:seq(1, OpsPerClient)),

        Parent ! {client_done, self(), Latencies}
    end) || _ <- lists:seq(1, ClientCount)],

    %% Collect results
    AllLatencies = lists:flatten([
        receive
            {client_done, Client, Latencies} ->
                Latencies
        after 60000 ->
            []
        end || Client <- Clients
    ]),

    AllLatencies.

%%====================================================================
%% No-Pool Benchmark (Direct Connection)
%%====================================================================

run_no_pool_benchmark(DurationS, ConcurrentClients, OpsPerClient) ->
    StartTime = erlang:monotonic_time(microsecond),
    Latencies = run_clients_no_pool(ConcurrentClients, OpsPerClient),
    EndTime = erlang:monotonic_time(microsecond),

    Duration = (EndTime - StartTime) / 1_000_000,
    TotalOps = length(Latencies),
    Throughput = TotalOps / Duration,

    {P50, P95, P99} = calculate_percentiles(Latencies),

    #metrics{
        workload_id = <<"no_pool">>,
        pool_size = 0,
        strategy = none,
        duration_s = round(Duration),
        total_operations = TotalOps,
        throughput_ops_per_s = Throughput,
        latency_p50_us = P50,
        latency_p95_us = P95,
        latency_p99_us = P99,
        avg_checkout_time_us = 0.0,
        pool_utilization_percent = 0.0,
        failed_checkouts = 0,
        concurrent_clients = ConcurrentClients,
        timestamp = erlang:system_time(second),
        environment = get_environment()
    }.

run_clients_no_pool(ClientCount, OpsPerClient) ->
    Parent = self(),

    %% Spawn clients (each creates own workers)
    Clients = [spawn(fun() ->
        Latencies = lists:map(fun(_) ->
            StartTime = erlang:monotonic_time(microsecond),

            %% Create worker directly (no pool)
            {ok, Worker} = dummy_worker_start_link(#{}),

            %% Simulate work
            Worker ! {work, self()},
            receive
                {done, Worker} ->
                    ok
            after 1000 ->
                ok
            end,

            %% Stop worker
            exit(Worker, normal),

            EndTime = erlang:monotonic_time(microsecond),
            EndTime - StartTime
        end, lists:seq(1, OpsPerClient)),

        Parent ! {client_done, self(), Latencies}
    end) || _ <- lists:seq(1, ClientCount)],

    %% Collect results
    AllLatencies = lists:flatten([
        receive
            {client_done, Client, Latencies} ->
                Latencies
        after 60000 ->
            []
        end || Client <- Clients
    ]),

    AllLatencies.

%%====================================================================
%% Statistics
%%====================================================================

calculate_percentiles(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),

    P50 = percentile(Sorted, Len, 50),
    P95 = percentile(Sorted, Len, 95),
    P99 = percentile(Sorted, Len, 99),

    {P50, P95, P99}.

percentile(Sorted, Len, Percentile) ->
    Index = max(1, round(Len * Percentile / 100)),
    lists:nth(Index, Sorted).

%%====================================================================
%% Output
%%====================================================================

print_results(WorkloadId, Results) ->
    io:format("~n=== Results for ~s ===~n", [WorkloadId]),
    lists:foreach(fun print_result/1, Results).

print_result(#metrics{} = M) ->
    io:format("~n"),
    io:format("Pool Size: ~p~n", [M#metrics.pool_size]),
    io:format("Strategy: ~p~n", [M#metrics.strategy]),
    io:format("Concurrent Clients: ~p~n", [M#metrics.concurrent_clients]),
    io:format("Total Operations: ~p~n", [M#metrics.total_operations]),
    io:format("Throughput: ~.2f ops/sec~n", [M#metrics.throughput_ops_per_s]),
    io:format("Latency (p50): ~.2f μs~n", [M#metrics.latency_p50_us]),
    io:format("Latency (p95): ~.2f μs~n", [M#metrics.latency_p95_us]),
    io:format("Latency (p99): ~.2f μs~n", [M#metrics.latency_p99_us]),
    io:format("Avg Checkout Time: ~.2f μs~n", [M#metrics.avg_checkout_time_us]),
    io:format("Pool Utilization: ~.2f%~n", [M#metrics.pool_utilization_percent]),
    io:format("Failed Checkouts: ~p~n", [M#metrics.failed_checkouts]).

get_environment() ->
    {ok, Hostname} = inet:gethostname(),
    OTPRelease = erlang:system_info(otp_release),
    list_to_binary(io_lib:format("~s_OTP~s", [Hostname, OTPRelease])).
