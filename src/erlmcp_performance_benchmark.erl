%%%-------------------------------------------------------------------
%%% @doc
%%% Performance Benchmarking Framework for Erlang MCP Transports
%%%
%%% This module implements comprehensive performance benchmarking and
%%% monitoring for transport layer implementations. It provides:
%%%
%%% - Baseline performance metrics establishment
%%% - Throughput and latency measurements
%%% - Memory usage profiling
%%% - CPU performance monitoring
%%% - Regression detection
%%% - Performance optimization recommendations
%%%
%%% Key Features:
%%% - Standardized benchmark suites for all transport types
%%% - Real-time performance monitoring
%%% - Historical performance data storage
%%% - Automated regression detection
%%% - Performance bottleneck identification
%%% - Resource utilization tracking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_benchmark).

-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start_link/0, start_link/1]).
-export([run_benchmark/2, run_baseline/1, monitor_performance/2]).
-export([get_baseline_metrics/1, get_benchmark_results/1]).
-export([start_monitoring/1, stop_monitoring/1]).
-export([analyze_performance/2, detect_regressions/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Performance measurement record
-record(perf_measurement, {
    transport_type :: atom(),
    transport_id :: atom(),
    test_type :: atom(),
    timestamp :: integer(),
    metrics :: map(),
    configuration :: map(),
    environment :: map()
}).

%% Benchmark state
-record(state, {
    benchmarks = #{} :: map(),
    baselines = #{} :: map(),
    monitoring = #{} :: map(),
    results_storage :: ets:tid(),
    config :: map()
}).

%% Default configuration
-define(DEFAULT_CONFIG, #{
    sample_size => 1000,
    warmup_samples => 100,
    timeout => 30000,
    measurement_interval => 1000,
    storage_retention => 86400000, % 24 hours
    regression_threshold => 0.15   % 15% performance degradation
}).

%% Benchmark test types
-define(BENCHMARK_TYPES, [
    throughput,
    latency,
    memory_usage,
    cpu_utilization,
    connection_overhead,
    message_framing,
    error_recovery
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the performance benchmark server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(?DEFAULT_CONFIG).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Run a specific benchmark on a transport
-spec run_benchmark(atom(), map()) -> {ok, map()} | {error, term()}.
run_benchmark(TransportType, Options) ->
    gen_server:call(?MODULE, {run_benchmark, TransportType, Options}, 60000).

%% @doc Run baseline performance measurement for a transport
-spec run_baseline(atom()) -> {ok, map()} | {error, term()}.
run_baseline(TransportType) ->
    gen_server:call(?MODULE, {run_baseline, TransportType}, 60000).

%% @doc Monitor transport performance in real-time
-spec monitor_performance(atom(), map()) -> ok | {error, term()}.
monitor_performance(TransportId, Options) ->
    gen_server:call(?MODULE, {monitor_performance, TransportId, Options}).

%% @doc Get baseline metrics for a transport type
-spec get_baseline_metrics(atom()) -> {ok, map()} | {error, not_found}.
get_baseline_metrics(TransportType) ->
    gen_server:call(?MODULE, {get_baseline_metrics, TransportType}).

%% @doc Get benchmark results for a specific test
-spec get_benchmark_results(term()) -> {ok, [map()]} | {error, term()}.
get_benchmark_results(TestId) ->
    gen_server:call(?MODULE, {get_benchmark_results, TestId}).

%% @doc Start performance monitoring for a transport
-spec start_monitoring(atom()) -> ok | {error, term()}.
start_monitoring(TransportId) ->
    gen_server:call(?MODULE, {start_monitoring, TransportId}).

%% @doc Stop performance monitoring for a transport
-spec stop_monitoring(atom()) -> ok.
stop_monitoring(TransportId) ->
    gen_server:call(?MODULE, {stop_monitoring, TransportId}).

%% @doc Analyze performance data and generate insights
-spec analyze_performance(atom(), map()) -> {ok, map()} | {error, term()}.
analyze_performance(TransportType, Options) ->
    gen_server:call(?MODULE, {analyze_performance, TransportType, Options}).

%% @doc Detect performance regressions
-spec detect_regressions(atom(), map()) -> {ok, [map()]} | {error, term()}.
detect_regressions(TransportType, CurrentMetrics) ->
    gen_server:call(?MODULE, {detect_regressions, TransportType, CurrentMetrics}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Config]) ->
    ?LOG_INFO("Starting performance benchmark server with config: ~p", 
              [maps:without([password, secret, token], Config)]),
    
    % Initialize ETS table for results storage
    ResultsTable = ets:new(benchmark_results, 
                          [ordered_set, public, {keypos, #perf_measurement.timestamp}]),
    
    State = #state{
        benchmarks = #{},
        baselines = #{},
        monitoring = #{},
        results_storage = ResultsTable,
        config = maps:merge(?DEFAULT_CONFIG, Config)
    },
    
    % Schedule cleanup of old results
    erlang:send_after(3600000, self(), cleanup_old_results), % 1 hour
    
    {ok, State}.

handle_call({run_benchmark, TransportType, Options}, _From, State) ->
    ?LOG_INFO("Running benchmark for transport type: ~p", [TransportType]),
    
    try
        Result = execute_benchmark_suite(TransportType, Options, State),
        store_benchmark_results(Result, State),
        {reply, {ok, Result}, State}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Benchmark failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {reply, {error, {benchmark_failed, Class, Reason}}, State}
    end;

handle_call({run_baseline, TransportType}, _From, State) ->
    ?LOG_INFO("Running baseline measurements for transport type: ~p", [TransportType]),
    
    try
        BaselineOptions = #{
            test_types => [throughput, latency, memory_usage],
            sample_size => maps:get(sample_size, State#state.config),
            warmup_samples => maps:get(warmup_samples, State#state.config)
        },
        
        Result = execute_benchmark_suite(TransportType, BaselineOptions, State),
        
        % Store as baseline
        NewBaselines = maps:put(TransportType, Result, State#state.baselines),
        NewState = State#state{baselines = NewBaselines},
        
        {reply, {ok, Result}, NewState}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Baseline measurement failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {reply, {error, {baseline_failed, Class, Reason}}, State}
    end;

handle_call({monitor_performance, TransportId, Options}, _From, State) ->
    case start_performance_monitoring(TransportId, Options, State) of
        {ok, MonitorPid} ->
            NewMonitoring = maps:put(TransportId, MonitorPid, State#state.monitoring),
            NewState = State#state{monitoring = NewMonitoring},
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_baseline_metrics, TransportType}, _From, State) ->
    case maps:get(TransportType, State#state.baselines, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Baseline ->
            {reply, {ok, Baseline}, State}
    end;

handle_call({get_benchmark_results, TestId}, _From, State) ->
    Results = ets:match_object(State#state.results_storage, 
                              #perf_measurement{_ = '_'}),
    FilteredResults = lists:filter(
        fun(#perf_measurement{metrics = Metrics}) ->
            maps:get(test_id, Metrics, undefined) =:= TestId
        end, Results),
    {reply, {ok, FilteredResults}, State};

handle_call({start_monitoring, TransportId}, _From, State) ->
    DefaultOptions = #{
        interval => maps:get(measurement_interval, State#state.config),
        metrics => [throughput, latency, memory_usage, cpu_utilization]
    },
    case start_performance_monitoring(TransportId, DefaultOptions, State) of
        {ok, MonitorPid} ->
            NewMonitoring = maps:put(TransportId, MonitorPid, State#state.monitoring),
            NewState = State#state{monitoring = NewMonitoring},
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop_monitoring, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.monitoring, undefined) of
        undefined ->
            {reply, ok, State};
        MonitorPid ->
            stop_monitor_process(MonitorPid),
            NewMonitoring = maps:remove(TransportId, State#state.monitoring),
            NewState = State#state{monitoring = NewMonitoring},
            {reply, ok, NewState}
    end;

handle_call({analyze_performance, TransportType, Options}, _From, State) ->
    try
        Analysis = perform_performance_analysis(TransportType, Options, State),
        {reply, {ok, Analysis}, State}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Performance analysis failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {reply, {error, {analysis_failed, Class, Reason}}, State}
    end;

handle_call({detect_regressions, TransportType, CurrentMetrics}, _From, State) ->
    case maps:get(TransportType, State#state.baselines, undefined) of
        undefined ->
            {reply, {error, no_baseline}, State};
        BaselineMetrics ->
            Regressions = identify_performance_regressions(
                BaselineMetrics, CurrentMetrics, State#state.config),
            {reply, {ok, Regressions}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_old_results, State) ->
    cleanup_old_benchmark_results(State),
    % Schedule next cleanup
    erlang:send_after(3600000, self(), cleanup_old_results),
    {noreply, State};

handle_info({monitor_result, TransportId, Metrics}, State) ->
    % Store monitoring results
    Measurement = #perf_measurement{
        transport_type = unknown,
        transport_id = TransportId,
        test_type = monitoring,
        timestamp = erlang:system_time(millisecond),
        metrics = Metrics,
        configuration = #{},
        environment = get_environment_info()
    },
    ets:insert(State#state.results_storage, Measurement),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Stop all monitoring processes
    maps:fold(fun(_TransportId, MonitorPid, _Acc) ->
                 stop_monitor_process(MonitorPid)
              end, ok, State#state.monitoring),
    
    % Clean up ETS table
    ets:delete(State#state.results_storage),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Execute comprehensive benchmark suite
execute_benchmark_suite(TransportType, Options, State) ->
    Config = State#state.config,
    TestTypes = maps:get(test_types, Options, ?BENCHMARK_TYPES),
    SampleSize = maps:get(sample_size, Options, maps:get(sample_size, Config)),
    
    ?LOG_INFO("Executing benchmark suite for ~p with tests: ~p", 
              [TransportType, TestTypes]),
    
    Results = lists:foldl(
        fun(TestType, Acc) ->
            ?LOG_DEBUG("Running ~p test for ~p", [TestType, TransportType]),
            TestResult = run_performance_test(TransportType, TestType, SampleSize, Config),
            maps:put(TestType, TestResult, Acc)
        end,
        #{}, TestTypes),
    
    % Add metadata
    Results#{
        transport_type => TransportType,
        timestamp => erlang:system_time(millisecond),
        configuration => Options,
        environment => get_environment_info()
    }.

%% @private Run individual performance test
run_performance_test(TransportType, throughput, SampleSize, Config) ->
    ?LOG_DEBUG("Running throughput test for ~p", [TransportType]),
    
    % Create test transport instance
    case create_test_transport(TransportType, Config) of
        {ok, TransportPid} ->
            try
                % Warmup phase
                WarmupSamples = maps:get(warmup_samples, Config),
                run_warmup_phase(TransportPid, WarmupSamples),
                
                % Measurement phase
                StartTime = erlang:system_time(millisecond),
                MessagesSent = send_test_messages(TransportPid, SampleSize),
                EndTime = erlang:system_time(millisecond),
                
                Duration = EndTime - StartTime,
                Throughput = (MessagesSent * 1000) / Duration, % messages per second
                
                #{
                    messages_sent => MessagesSent,
                    duration_ms => Duration,
                    throughput_mps => Throughput,
                    average_latency_ms => Duration / MessagesSent
                }
            after
                cleanup_test_transport(TransportPid)
            end;
        {error, Reason} ->
            #{error => Reason, test_type => throughput}
    end;

run_performance_test(TransportType, latency, SampleSize, Config) ->
    ?LOG_DEBUG("Running latency test for ~p", [TransportType]),
    
    case create_test_transport(TransportType, Config) of
        {ok, TransportPid} ->
            try
                Latencies = measure_message_latencies(TransportPid, SampleSize),
                
                % Calculate statistics
                SortedLatencies = lists:sort(Latencies),
                Mean = lists:sum(Latencies) / length(Latencies),
                Median = calculate_percentile(SortedLatencies, 50),
                P95 = calculate_percentile(SortedLatencies, 95),
                P99 = calculate_percentile(SortedLatencies, 99),
                
                #{
                    sample_size => length(Latencies),
                    mean_latency_ms => Mean,
                    median_latency_ms => Median,
                    p95_latency_ms => P95,
                    p99_latency_ms => P99,
                    min_latency_ms => lists:min(Latencies),
                    max_latency_ms => lists:max(Latencies)
                }
            after
                cleanup_test_transport(TransportPid)
            end;
        {error, Reason} ->
            #{error => Reason, test_type => latency}
    end;

run_performance_test(TransportType, memory_usage, SampleSize, Config) ->
    ?LOG_DEBUG("Running memory usage test for ~p", [TransportType]),
    
    case create_test_transport(TransportType, Config) of
        {ok, TransportPid} ->
            try
                % Measure baseline memory
                BaselineMemory = get_process_memory(TransportPid),
                
                % Send messages and measure memory growth
                MemoryMeasurements = measure_memory_during_load(TransportPid, SampleSize),
                
                PeakMemory = lists:max(MemoryMeasurements),
                AverageMemory = lists:sum(MemoryMeasurements) / length(MemoryMeasurements),
                
                #{
                    baseline_memory_kb => BaselineMemory / 1024,
                    peak_memory_kb => PeakMemory / 1024,
                    average_memory_kb => AverageMemory / 1024,
                    memory_growth_kb => (PeakMemory - BaselineMemory) / 1024,
                    measurements => length(MemoryMeasurements)
                }
            after
                cleanup_test_transport(TransportPid)
            end;
        {error, Reason} ->
            #{error => Reason, test_type => memory_usage}
    end;

run_performance_test(TransportType, cpu_utilization, SampleSize, Config) ->
    ?LOG_DEBUG("Running CPU utilization test for ~p", [TransportType]),
    
    case create_test_transport(TransportType, Config) of
        {ok, TransportPid} ->
            try
                % Measure CPU usage during load
                CPUMeasurements = measure_cpu_during_load(TransportPid, SampleSize),
                
                case CPUMeasurements of
                    [] ->
                        #{error => no_cpu_measurements, test_type => cpu_utilization};
                    _ ->
                        AverageCPU = lists:sum(CPUMeasurements) / length(CPUMeasurements),
                        PeakCPU = lists:max(CPUMeasurements),
                        
                        #{
                            average_cpu_percent => AverageCPU,
                            peak_cpu_percent => PeakCPU,
                            measurements => length(CPUMeasurements),
                            cpu_efficiency => AverageCPU / (SampleSize / 1000) % CPU per 1000 messages
                        }
                end
            after
                cleanup_test_transport(TransportPid)
            end;
        {error, Reason} ->
            #{error => Reason, test_type => cpu_utilization}
    end;

run_performance_test(_TransportType, TestType, _SampleSize, _Config) ->
    ?LOG_WARNING("Unknown test type: ~p", [TestType]),
    #{error => unknown_test_type, test_type => TestType}.

%% @private Create test transport instance
create_test_transport(http, Config) ->
    TestConfig = maps:merge(Config, #{
        url => "http://localhost:8080/test",
        test_mode => true,
        timeout => 5000
    }),
    erlmcp_transport_http:start_link(test_transport, TestConfig);

create_test_transport(tcp, Config) ->
    TestConfig = maps:merge(Config, #{
        host => "localhost",
        port => 8081,
        test_mode => true,
        connect_timeout => 5000
    }),
    erlmcp_transport_tcp:start_link(test_transport, TestConfig);

create_test_transport(stdio, Config) ->
    TestConfig = maps:merge(Config, #{
        test_mode => true,
        server_id => test_server
    }),
    erlmcp_transport_stdio_new:start_link(test_transport, TestConfig);

create_test_transport(TransportType, _Config) ->
    {error, {unsupported_transport_type, TransportType}}.

%% @private Cleanup test transport
cleanup_test_transport(TransportPid) when is_pid(TransportPid) ->
    try
        gen_server:stop(TransportPid, normal, 5000)
    catch
        _:_ -> ok
    end;
cleanup_test_transport(_) ->
    ok.

%% @private Run warmup phase
run_warmup_phase(TransportPid, WarmupSamples) ->
    ?LOG_DEBUG("Running warmup phase with ~p samples", [WarmupSamples]),
    send_test_messages(TransportPid, WarmupSamples).

%% @private Send test messages and count successful sends
send_test_messages(TransportPid, Count) ->
    TestMessage = <<"test message for performance benchmarking">>,
    
    lists:foldl(
        fun(_N, SuccessCount) ->
            case gen_server:call(TransportPid, {send, TestMessage}, 5000) of
                ok -> SuccessCount + 1;
                {error, _} -> SuccessCount
            end
        end, 0, lists:seq(1, Count)).

%% @private Measure message latencies
measure_message_latencies(TransportPid, Count) ->
    TestMessage = <<"latency test message">>,
    
    lists:foldl(
        fun(_N, Latencies) ->
            StartTime = erlang:system_time(microsecond),
            case gen_server:call(TransportPid, {send, TestMessage}, 5000) of
                ok ->
                    EndTime = erlang:system_time(microsecond),
                    Latency = (EndTime - StartTime) / 1000, % Convert to milliseconds
                    [Latency | Latencies];
                {error, _} ->
                    Latencies
            end
        end, [], lists:seq(1, Count)).

%% @private Get process memory usage
get_process_memory(Pid) ->
    case process_info(Pid, memory) of
        {memory, Memory} -> Memory;
        undefined -> 0
    end.

%% @private Measure memory usage during load
measure_memory_during_load(TransportPid, MessageCount) ->
    TestMessage = <<"memory test message">>,
    MeasurementInterval = max(1, MessageCount div 20), % 20 measurements
    
    {_, Measurements} = lists:foldl(
        fun(N, {LastMeasurement, MemList}) ->
            % Send message
            gen_server:call(TransportPid, {send, TestMessage}, 5000),
            
            % Measure memory periodically
            case N rem MeasurementInterval of
                0 ->
                    Memory = get_process_memory(TransportPid),
                    {N, [Memory | MemList]};
                _ ->
                    {N, MemList}
            end
        end, {0, []}, lists:seq(1, MessageCount)),
    
    lists:reverse(Measurements).

%% @private Measure CPU usage during load
measure_cpu_during_load(TransportPid, MessageCount) ->
    % This is a simplified CPU measurement - in production you might use
    % system monitoring tools or more sophisticated CPU tracking
    TestMessage = <<"cpu test message">>,
    MeasurementInterval = max(1, MessageCount div 10), % 10 measurements
    
    {_, Measurements} = lists:foldl(
        fun(N, {LastMeasurement, CPUList}) ->
            StartTime = erlang:system_time(microsecond),
            gen_server:call(TransportPid, {send, TestMessage}, 5000),
            EndTime = erlang:system_time(microsecond),
            
            case N rem MeasurementInterval of
                0 ->
                    % Simplified CPU usage estimation based on processing time
                    ProcessingTime = EndTime - StartTime,
                    CPUUsage = min(100, ProcessingTime / 1000), % Rough estimate
                    {N, [CPUUsage | CPUList]};
                _ ->
                    {N, CPUList}
            end
        end, {0, []}, lists:seq(1, MessageCount)),
    
    lists:reverse(Measurements).

%% @private Calculate percentile
calculate_percentile(SortedList, Percentile) ->
    Length = length(SortedList),
    case Length of
        0 -> 0;
        _ ->
            Index = round((Percentile / 100) * Length),
            ClampedIndex = max(1, min(Index, Length)),
            lists:nth(ClampedIndex, SortedList)
    end.

%% @private Store benchmark results
store_benchmark_results(Results, State) ->
    Measurement = #perf_measurement{
        transport_type = maps:get(transport_type, Results),
        transport_id = test_transport,
        test_type = benchmark,
        timestamp = maps:get(timestamp, Results),
        metrics = Results,
        configuration = maps:get(configuration, Results, #{}),
        environment = maps:get(environment, Results, #{})
    },
    ets:insert(State#state.results_storage, Measurement).

%% @private Start performance monitoring process
start_performance_monitoring(TransportId, Options, _State) ->
    MonitorPid = spawn_link(fun() ->
        performance_monitor_loop(TransportId, Options)
    end),
    {ok, MonitorPid}.

%% @private Performance monitoring loop
performance_monitor_loop(TransportId, Options) ->
    Interval = maps:get(interval, Options, 1000),
    Metrics = maps:get(metrics, Options, [throughput, latency, memory_usage]),
    
    % Collect metrics
    CollectedMetrics = collect_transport_metrics(TransportId, Metrics),
    
    % Send results back to benchmark server
    ?MODULE ! {monitor_result, TransportId, CollectedMetrics},
    
    % Wait for next interval
    timer:sleep(Interval),
    
    performance_monitor_loop(TransportId, Options).

%% @private Collect transport metrics
collect_transport_metrics(TransportId, Metrics) ->
    lists:foldl(
        fun(Metric, Acc) ->
            Value = collect_metric(TransportId, Metric),
            maps:put(Metric, Value, Acc)
        end, #{timestamp => erlang:system_time(millisecond)}, Metrics).

%% @private Collect individual metric
collect_metric(TransportId, memory_usage) ->
    case whereis(TransportId) of
        undefined -> 0;
        Pid -> get_process_memory(Pid) / 1024 % KB
    end;
collect_metric(_TransportId, throughput) ->
    % This would need to be implemented with proper message counting
    0;
collect_metric(_TransportId, latency) ->
    % This would need to be implemented with proper latency tracking
    0;
collect_metric(_TransportId, cpu_utilization) ->
    % This would need system-level CPU monitoring
    0.

%% @private Stop monitoring process
stop_monitor_process(MonitorPid) ->
    try
        exit(MonitorPid, normal)
    catch
        _:_ -> ok
    end.

%% @private Perform performance analysis
perform_performance_analysis(TransportType, Options, State) ->
    % Retrieve historical data for analysis
    Pattern = #perf_measurement{transport_type = TransportType, _ = '_'},
    Measurements = ets:match_object(State#state.results_storage, Pattern),
    
    % Analyze trends
    TrendAnalysis = analyze_performance_trends(Measurements),
    
    % Identify bottlenecks
    Bottlenecks = identify_performance_bottlenecks(Measurements),
    
    % Generate recommendations
    Recommendations = generate_optimization_recommendations(TrendAnalysis, Bottlenecks),
    
    #{
        transport_type => TransportType,
        analysis_timestamp => erlang:system_time(millisecond),
        trends => TrendAnalysis,
        bottlenecks => Bottlenecks,
        recommendations => Recommendations,
        measurement_count => length(Measurements)
    }.

%% @private Analyze performance trends
analyze_performance_trends(Measurements) ->
    % Group measurements by test type
    GroupedMeasurements = group_measurements_by_test_type(Measurements),
    
    maps:map(fun(_TestType, TestMeasurements) ->
                analyze_test_type_trend(TestMeasurements)
             end, GroupedMeasurements).

%% @private Group measurements by test type
group_measurements_by_test_type(Measurements) ->
    lists:foldl(
        fun(#perf_measurement{test_type = TestType} = Measurement, Acc) ->
            Current = maps:get(TestType, Acc, []),
            maps:put(TestType, [Measurement | Current], Acc)
        end, #{}, Measurements).

%% @private Analyze trend for specific test type
analyze_test_type_trend(Measurements) ->
    case length(Measurements) of
        0 -> #{trend => no_data};
        1 -> #{trend => insufficient_data};
        _ ->
            % Sort by timestamp
            Sorted = lists:sort(
                fun(#perf_measurement{timestamp = T1}, 
                    #perf_measurement{timestamp = T2}) ->
                    T1 =< T2
                end, Measurements),
            
            % Calculate trend (simplified linear regression)
            calculate_performance_trend(Sorted)
    end.

%% @private Calculate performance trend
calculate_performance_trend(Measurements) ->
    % This is a simplified trend calculation
    % In production, you'd use proper statistical methods
    case {hd(Measurements), lists:last(Measurements)} of
        {#perf_measurement{metrics = FirstMetrics},
         #perf_measurement{metrics = LastMetrics}} ->
            
            % Compare key metrics
            FirstThroughput = extract_metric_value(FirstMetrics, throughput),
            LastThroughput = extract_metric_value(LastMetrics, throughput),
            
            ThroughputTrend = calculate_trend_direction(FirstThroughput, LastThroughput),
            
            #{
                trend => ThroughputTrend,
                first_measurement => FirstMetrics,
                last_measurement => LastMetrics,
                measurement_count => length(Measurements)
            }
    end.

%% @private Extract metric value from metrics map
extract_metric_value(Metrics, MetricType) ->
    case maps:get(MetricType, Metrics, undefined) of
        undefined -> 0;
        #{throughput_mps := Value} -> Value;
        Value when is_number(Value) -> Value;
        _ -> 0
    end.

%% @private Calculate trend direction
calculate_trend_direction(First, Last) when First == 0, Last == 0 -> stable;
calculate_trend_direction(First, Last) when First == 0 -> improving;
calculate_trend_direction(First, Last) ->
    Change = (Last - First) / First,
    case Change of
        C when C > 0.05 -> improving;
        C when C < -0.05 -> degrading;
        _ -> stable
    end.

%% @private Identify performance bottlenecks
identify_performance_bottlenecks(Measurements) ->
    % Analyze measurements for common bottleneck patterns
    lists:foldl(
        fun(#perf_measurement{metrics = Metrics}, Bottlenecks) ->
            identify_metric_bottlenecks(Metrics) ++ Bottlenecks
        end, [], Measurements).

%% @private Identify bottlenecks in metrics
identify_metric_bottlenecks(Metrics) ->
    Bottlenecks = [],
    
    % Check for high memory usage
    MemoryBottlenecks = case extract_metric_value(Metrics, memory_usage) of
        Memory when Memory > 50000 -> % 50MB threshold
            [#{type => high_memory_usage, value => Memory, threshold => 50000}];
        _ -> []
    end,
    
    % Check for low throughput
    ThroughputBottlenecks = case extract_metric_value(Metrics, throughput) of
        Throughput when Throughput < 10 -> % 10 messages/sec threshold
            [#{type => low_throughput, value => Throughput, threshold => 10}];
        _ -> []
    end,
    
    Bottlenecks ++ MemoryBottlenecks ++ ThroughputBottlenecks.

%% @private Generate optimization recommendations
generate_optimization_recommendations(TrendAnalysis, Bottlenecks) ->
    TrendRecommendations = generate_trend_recommendations(TrendAnalysis),
    BottleneckRecommendations = generate_bottleneck_recommendations(Bottlenecks),
    
    TrendRecommendations ++ BottleneckRecommendations.

%% @private Generate recommendations based on trends
generate_trend_recommendations(TrendAnalysis) ->
    maps:fold(
        fun(TestType, TrendData, Recommendations) ->
            case maps:get(trend, TrendData, stable) of
                degrading ->
                    Rec = #{
                        type => performance_degradation,
                        test_type => TestType,
                        recommendation => "Performance is degrading. Consider profiling and optimization.",
                        priority => high
                    },
                    [Rec | Recommendations];
                _ ->
                    Recommendations
            end
        end, [], TrendAnalysis).

%% @private Generate recommendations based on bottlenecks
generate_bottleneck_recommendations(Bottlenecks) ->
    lists:map(
        fun(#{type := Type} = Bottleneck) ->
            case Type of
                high_memory_usage ->
                    Bottleneck#{
                        recommendation => "High memory usage detected. Consider memory optimization.",
                        priority => medium
                    };
                low_throughput ->
                    Bottleneck#{
                        recommendation => "Low throughput detected. Consider connection pooling or async processing.",
                        priority => high
                    }
            end
        end, Bottlenecks).

%% @private Identify performance regressions
identify_performance_regressions(BaselineMetrics, CurrentMetrics, Config) ->
    Threshold = maps:get(regression_threshold, Config, 0.15),
    
    CompareMetrics = [throughput, latency, memory_usage],
    
    lists:foldl(
        fun(MetricType, Regressions) ->
            case compare_metric_performance(BaselineMetrics, CurrentMetrics, MetricType, Threshold) of
                {regression, Details} ->
                    [Details | Regressions];
                no_regression ->
                    Regressions
            end
        end, [], CompareMetrics).

%% @private Compare metric performance
compare_metric_performance(BaselineMetrics, CurrentMetrics, MetricType, Threshold) ->
    BaselineValue = extract_metric_value(BaselineMetrics, MetricType),
    CurrentValue = extract_metric_value(CurrentMetrics, MetricType),
    
    case {BaselineValue, CurrentValue} of
        {0, _} -> no_regression;
        {Baseline, Current} ->
            PerformanceChange = (Current - Baseline) / Baseline,
            case PerformanceChange of
                Change when Change > Threshold ->
                    {regression, #{
                        metric => MetricType,
                        baseline_value => Baseline,
                        current_value => Current,
                        performance_change => PerformanceChange,
                        threshold => Threshold
                    }};
                _ ->
                    no_regression
            end
    end.

%% @private Cleanup old benchmark results
cleanup_old_benchmark_results(State) ->
    RetentionPeriod = maps:get(storage_retention, State#state.config),
    CutoffTime = erlang:system_time(millisecond) - RetentionPeriod,
    
    % Delete old measurements
    ets:select_delete(State#state.results_storage,
                     [{{perf_measurement, '_', '_', '_', '$1', '_', '_', '_'},
                       [{'<', '$1', CutoffTime}], 
                       [true]}]).

%% @private Get environment information
get_environment_info() ->
    #{
        otp_version => erlang:system_info(otp_release),
        system_version => erlang:system_info(version),
        system_architecture => erlang:system_info(system_architecture),
        schedulers => erlang:system_info(schedulers),
        memory => erlang:memory(),
        timestamp => erlang:system_time(millisecond)
    }.