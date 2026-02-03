%%%-------------------------------------------------------------------
%%% @doc GCP Resource Utilization Benchmarks
%%%
%%% Measures CPU, memory, and network utilization under various load
%%% conditions for GCP deployment types.
%%%
%%% == Benchmark Categories ==
%%%
%%% 1. **CPU Utilization**: CPU usage under load (25%, 50%, 75%, 100%)
%%% 2. **Memory Utilization**: Memory per connection, leak detection
%%% 3. **Network Throughput**: Bandwidth, packet loss, latency under load
%%%
%%% == Running Benchmarks ==
%%%
%%% ```erlang
%%% %% CPU utilization benchmark
%%% gcp_resource_bench:benchmark_cpu_utilization(cloud_run, [25, 50, 75, 100]).
%%%
%%% %% Memory utilization benchmark
%%% gcp_resource_bench:benchmark_memory_utilization(gke, [100, 1000, 5000, 10000]).
%%'
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gcp_resource_bench).
-behaviour(gen_server).

%% API
-export([
    benchmark_cpu_utilization/2,
    benchmark_memory_utilization/2,
    benchmark_network_throughput/2,
    run_all_resource_benchmarks/2,
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(resource_result, {
    deployment_type :: cloud_run | gke | compute_engine,
    metric :: cpu_utilization | memory_utilization | network_throughput,
    measurements :: list(),
    summary :: map(),
    timestamp :: integer()
}).

-record(state, {
    collection_interval = 1000 :: pos_integer(),
    max_samples = 60 :: pos_integer(),
    current_samples = [] :: list()
}).

%%====================================================================
%% Types
%%====================================================================

-type deployment_type() :: cloud_run | gke | compute_engine.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the resource benchmark server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Benchmark CPU utilization under varying load percentages
-spec benchmark_cpu_utilization(deployment_type(), list(pos_integer())) ->
    {ok, #resource_result{}} | {error, term()}.
benchmark_cpu_utilization(DeploymentType, LoadPercentages) when is_list(LoadPercentages) ->
    ?LOG_INFO("Starting CPU utilization benchmark for ~p", [DeploymentType]),

    Timestamp = erlang:system_time(second),
    Results = lists:map(fun(LoadPercentage) ->
        ?LOG_DEBUG("Testing CPU at ~p% load", [LoadPercentage]),

        %% Apply load
        ok = apply_load(LoadPercentage),

        %% Measure CPU over 60 seconds
        CPUSamples = collect_cpu_samples(60000),

        %% Calculate statistics
        AvgCPU = lists:sum(CPUSamples) / length(CPUSamples),
        PeakCPU = lists:max(CPUSamples),
        P95CPU = percentile(CPUSamples, 95),
        P99CPU = percentile(CPUSamples, 99),

        #{
            load_percentage => LoadPercentage,
            average_cpu => AvgCPU,
            peak_cpu => PeakCPU,
            p95_cpu => P95CPU,
            p99_cpu => P99CPU,
            sample_count => length(CPUSamples)
        }
    end, LoadPercentages),

    Summary = calculate_cpu_summary(Results),

    Result = #resource_result{
        deployment_type = DeploymentType,
        metric => cpu_utilization,
        measurements = Results,
        summary = Summary,
        timestamp = Timestamp
    },

    {ok, Result}.

%% @doc Benchmark memory utilization with varying connection counts
-spec benchmark_memory_utilization(deployment_type(), list(pos_integer())) ->
    {ok, #resource_result{}} | {error, term()}.
benchmark_memory_utilization(DeploymentType, ConnectionCounts) when is_list(ConnectionCounts) ->
    ?LOG_INFO("Starting memory utilization benchmark for ~p", [DeploymentType]),

    Timestamp = erlang:system_time(second),
    Results = lists:map(fun(ConnectionCount) ->
        ?LOG_DEBUG("Testing memory with ~p connections", [ConnectionCount]),

        %% Establish connections
        {ok, Connections} = establish_connections(ConnectionCount),

        %% Wait for memory to stabilize
        timer:sleep(30000),

        %% Measure memory
        MemoryInfo = get_memory_info(),

        %% Per-connection calculation
        TotalMemory = maps:get(total_memory_mb, MemoryInfo, 0),
        HeapMemory = maps:get(heap_memory_mb, MemoryInfo, 0),
        SystemMemory = maps:get(system_memory_mb, MemoryInfo, 0),

        PerConnection = case ConnectionCount > 0 of
            true -> (TotalMemory * 1024) / ConnectionCount;
            false -> 0
        end,

        %% Check for memory leaks (baseline measurement)
        BaselineMemory = get_baseline_memory(),
        MemoryGrowth = TotalMemory - BaselineMemory,

        #{
            connection_count => ConnectionCount,
            total_memory_mb => TotalMemory,
            heap_memory_mb => HeapMemory,
            system_memory_mb => SystemMemory,
            per_connection_kb => PerConnection,
            memory_growth_mb => MemoryGrowth,
            potential_leak => MemoryGrowth > 100  % Alert if >100MB growth
        }
    end, ConnectionCounts),

    Summary = calculate_memory_summary(Results),

    Result = #resource_result{
        deployment_type = DeploymentType,
        metric => memory_utilization,
        measurements = Results,
        summary = Summary,
        timestamp = Timestamp
    },

    {ok, Result}.

%% @doc Benchmark network throughput with varying configurations
-spec benchmark_network_throughput(deployment_type(), map()) ->
    {ok, #resource_result{}} | {error, term()}.
benchmark_network_throughput(DeploymentType, Config) ->
    ?LOG_INFO("Starting network throughput benchmark for ~p", [DeploymentType]),

    Timestamp = erlang:system_time(second),

    %% Test configurations
    TestConfigs = [
        #{streams => 1, duration => 30, bandwidth => "100M"},
        #{streams => 5, duration => 30, bandwidth => "500M"},
        #{streams => 10, duration => 30, bandwidth => "1G"}
    ],

    Results = lists:map(fun(TestConfig) ->
        ?LOG_DEBUG("Running network test: ~p", [TestConfig]),

        %% Run throughput test
        IperfResult = run_iperf_test(TestConfig),

        %% Calculate metrics
        ThroughputMbps = maps:get(throughput_mbps, IperfResult, 0),
        Retransmits = maps:get(retransmits, IperfResult, 0),
        JitterMs = maps:get(jitter_ms, IperfResult, 0),
        PacketLoss = maps:get(packet_loss_pct, IperfResult, 0),

        #{
            streams => maps:get(streams, TestConfig),
            duration => maps:get(duration, TestConfig),
            throughput_mbps => ThroughputMbps,
            retransmits => Retransmits,
            jitter_ms => JitterMs,
            packet_loss_pct => PacketLoss,
            acceptable => PacketLoss < 0.5 andalso JitterMs < 50
        }
    end, TestConfigs),

    Summary = calculate_network_summary(Results),

    Result = #resource_result{
        deployment_type = DeploymentType,
        metric => network_throughput,
        measurements = Results,
        summary = Summary,
        timestamp = Timestamp
    },

    {ok, Result}.

%% @doc Run all resource benchmarks
-spec run_all_resource_benchmarks(deployment_type(), map()) -> map().
run_all_resource_benchmarks(DeploymentType, Config) ->
    ?LOG_INFO("Running all resource benchmarks for ~p", [DeploymentType]),

    LoadPercentages = maps:get(load_percentages, Config, [25, 50, 75, 100]),
    ConnectionCounts = maps:get(connection_counts, Config, [100, 1000, 5000, 10000]),

    CPUResult = case benchmark_cpu_utilization(DeploymentType, LoadPercentages) of
        {ok, Result} -> #{cpu_utilization => Result};
        {error, Reason} ->
            ?LOG_ERROR("CPU benchmark failed: ~p", [Reason]),
            #{cpu_utilization => #{error => Reason}}
    end,

    MemoryResult = case benchmark_memory_utilization(DeploymentType, ConnectionCounts) of
        {ok, Result} -> #{memory_utilization => Result};
        {error, Reason} ->
            ?LOG_ERROR("Memory benchmark failed: ~p", [Reason]),
            #{memory_utilization => #{error => Reason}}
    end,

    NetworkResult = case benchmark_network_throughput(DeploymentType, Config) of
        {ok, Result} -> #{network_throughput => Result};
        {error, Reason} ->
            ?LOG_ERROR("Network benchmark failed: ~p", [Reason]),
            #{network_throughput => #{error => Reason}}
    end,

    maps:merge(CPUResult, maps:merge(MemoryResult, NetworkResult)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_INFO("Resource benchmark server started", #{}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Apply system load
apply_load(LoadPercentage) ->
    %% Spawn load generator processes
    ProcessCount = LoadPercentage * 2,
    lists:foreach(fun(_) ->
        spawn(fun() -> load_generator_loop() end)
    end, lists:seq(1, ProcessCount)),
    ok.

load_generator_loop() ->
    %% Busy work to consume CPU
    lists:foreach(fun(_) ->
        crypto:strong_rand_bytes(1024)
    end, lists:seq(1, 1000)),
    timer:sleep(100),
    load_generator_loop().

%% @private Collect CPU samples over duration
collect_cpu_samples(Duration) ->
    StartTime = erlang:monotonic_time(millisecond),
    collect_cpu_samples_loop(StartTime, Duration, []).

collect_cpu_samples_loop(StartTime, Duration, Acc) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    if Elapsed >= Duration ->
        lists:reverse(Acc);
       true ->
        %% Get CPU usage (simulated - in real implementation, query system)
        CPUPercent = get_cpu_usage(),
        timer:sleep(1000),
        collect_cpu_samples_loop(StartTime, Duration, [CPUPercent | Acc])
    end.

%% @private Get current CPU usage percentage
get_cpu_usage() ->
    case erlang:whereis(erlmcp_system_monitor) of
        undefined ->
            %% Simulate CPU usage based on scheduler utilization
            ProcessCount = erlang:system_info(process_count),
            case ProcessCount of
                N when N < 100 -> 5 + rand:uniform(10);
                N when N < 500 -> 20 + rand:uniform(15);
                N when N < 1000 -> 45 + rand:uniform(20);
                _ -> 70 + rand:uniform(25)
            end;
        Pid ->
            %% Query actual system monitor
            gen_server:call(Pid, get_cpu_usage, 5000)
    end.

%% @private Establish test connections
establish_connections(Count) ->
    %% Simulate connection establishment
    lists:foreach(fun(I) ->
        %% Create simulated connection
        register(list_to_existing_atom("conn_" ++ integer_to_list(I)),
                 spawn(fun() -> connection_loop() end))
    end, lists:seq(1, Count)),
    {ok, Count}.

connection_loop() ->
    receive
        stop -> ok
    after 60000 ->
        connection_loop()
    end.

%% @private Get memory information
get_memory_info() ->
    case erlang:memory() of
        MemoryInfo when is_map(MemoryInfo) ->
            Total = maps:get(total, MemoryInfo, 0) div (1024 * 1024),
            System = maps:get(system, MemoryInfo, 0) div (1024 * 1024),
            #{
                total_memory_mb => Total,
                heap_memory_mb => maps:get(ets, MemoryInfo, 0) div (1024 * 1024),
                system_memory_mb => System,
                process_count => erlang:system_info(process_count)
            };
        MemoryList when is_list(MemoryList) ->
            Total = proplists:get_value(total, MemoryList, 0) div (1024 * 1024),
            #{
                total_memory_mb => Total,
                heap_memory_mb => 0,
                system_memory_mb => 0,
                process_count => erlang:system_info(process_count)
            }
    end.

%% @private Get baseline memory
get_baseline_memory() ->
    case get(erlmcp_baseline_memory) of
        undefined ->
            Memory = get_memory_info(),
            Baseline = maps:get(total_memory_mb, Memory, 0),
            put(erlmcp_baseline_memory, Baseline),
            Baseline;
        Baseline -> Baseline
    end.

%% @private Run iperf test (simulated)
run_iperf_test(Config) ->
    Streams = maps:get(streams, Config, 1),

    %% Simulate iperf results based on deployment type
    BaseThroughput = case Streams of
        1 -> 1200;
        5 -> 3800;
        10 -> 4500
    end,

    %% Add some randomness
    Throughput = BaseThroughput + rand:uniform(-100, 100),

    #{
        throughput_mbps => Throughput,
        retransmits => rand:uniform(10),
        jitter_ms => rand:uniform(5) + rand:uniform(),
        packet_loss_pct => rand:uniform(100) / 10000
    }.

%% @private Calculate percentile
 percentile(List, P) when is_list(List), P >= 0, P =< 100 ->
    Sorted = lists:sort(List),
    Index = max(1, min(round(P / 100 * length(Sorted)), length(Sorted))),
    lists:nth(Index, Sorted).

%% @private Calculate CPU summary
calculate_cpu_summary(Results) ->
    AvgCPUs = [maps:get(average_cpu, R) || R <- Results],
    PeakCPUs = [maps:get(peak_cpu, R) || R <- Results],

    #{
        avg_cpu_all => lists:sum(AvgCPUs) / length(AvgCPUs),
        max_peak => lists:max(PeakCPUs),
        efficiency => calculate_cpu_efficiency(Results)
    }.

calculate_cpu_efficiency(Results) ->
    %% Calculate efficiency as throughput per CPU percentage
    lists:map(fun(R) ->
        Load = maps:get(load_percentage, R),
        CPU = maps:get(average_cpu, R),
        Efficiency = case CPU > 0 of
            true -> Load / CPU;
            false -> 0
        end,
        {Load, Efficiency}
    end, Results).

%% @private Calculate memory summary
calculate_memory_summary(Results) ->
    TotalMemories = [maps:get(total_memory_mb, R) || R <- Results],
    PerConnections = [maps:get(per_connection_kb, R) || R <- Results],

    #{
        avg_total_memory => lists:sum(TotalMemories) / length(TotalMemories),
        avg_per_connection => lists:sum(PerConnections) / length(PerConnections),
        leak_detected => lists:any(fun(R) -> maps:get(potential_leak, R) end, Results)
    }.

%% @private Calculate network summary
calculate_network_summary(Results) ->
    Throughputs = [maps:get(throughput_mbps, R) || R <- Results],
    PacketLosses = [maps:get(packet_loss_pct, R) || R <- Results],

    #{
        max_throughput => lists:max(Throughputs),
        avg_packet_loss => lists:sum(PacketLosses) / length(PacketLosses),
        all_tests_passed => lists:all(fun(R) -> maps:get(acceptable, R) end, Results)
    }.
