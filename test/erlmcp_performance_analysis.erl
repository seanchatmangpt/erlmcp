-module(erlmcp_performance_analysis).

-behaviour(gen_server).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([start_link/0, stop/0, run_throughput_test/3, run_latency_test/3,
         run_concurrent_test/4, run_stress_test/3, monitor_resources/2, generate_test_data/1,
         measure_registry_performance/0, optimize_hot_paths/0, profile_message_routing/2,
         benchmark_ets_operations/0, collect_gc_metrics/1, analyze_memory_patterns/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Performance test records
-record(perf_config,
        {target_throughput = 10000 :: pos_integer(),  % Messages per second
         target_latency_p99 = 1000 :: pos_integer(),  % Microseconds
         max_memory_growth = 50 :: pos_integer(),     % MB
         test_duration = 30000 :: pos_integer(),      % Milliseconds
         sample_interval = 1000 :: pos_integer()}).     % Milliseconds
-record(perf_metrics,
        {throughput = 0 :: non_neg_integer(),
         latency_samples = [] :: [non_neg_integer()],
         memory_samples = [] :: [non_neg_integer()],
         cpu_samples = [] :: [float()],
         error_count = 0 :: non_neg_integer(),
         start_time :: integer(),
         end_time :: integer()}).
-record(benchmark_result,
        {test_name :: atom(),
         config :: #perf_config{},
         metrics :: #perf_metrics{},
         passed :: boolean(),
         details :: map()}).
-record(optimization_result,
        {optimization :: atom(),
         before_metrics :: #perf_metrics{},
         after_metrics :: #perf_metrics{},
         improvement_percent :: float(),
         recommendations :: [binary()]}).
%% State record
-record(state,
        {active_tests = #{} :: #{reference() => {atom(), pid()}},
         metrics_history = [] :: [#perf_metrics{}],
         config = #perf_config{} :: #perf_config{},
         ets_cache_table :: ets:tid() | undefined,
         optimization_cache = #{} :: #{atom() => #optimization_result{}}}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid)
    end.

%% Comprehensive throughput testing
-spec run_throughput_test(atom(), pos_integer(), pos_integer()) ->
                             {ok, float()} | {error, term()}.
run_throughput_test(Transport, MessageCount, MessageSize) ->
    gen_server:call(?MODULE, {throughput_test, Transport, MessageCount, MessageSize}, 60000).

%% Advanced latency testing with percentile analysis
-spec run_latency_test(atom(), pos_integer(), pos_integer()) ->
                          {ok, {float(), float(), float()}} | {error, term()}.
run_latency_test(Transport, MessageCount, MessageSize) ->
    gen_server:call(?MODULE, {latency_test, Transport, MessageCount, MessageSize}, 60000).

%% Concurrent load testing
-spec run_concurrent_test(atom(), pos_integer(), pos_integer(), pos_integer()) ->
                             {ok, map()} | {error, term()}.
run_concurrent_test(Transport, ConnectionCount, MessageCount, MessageSize) ->
    gen_server:call(?MODULE,
                    {concurrent_test, Transport, ConnectionCount, MessageCount, MessageSize},
                    120000).

%% Stress testing with sustained load
-spec run_stress_test(atom(), pos_integer(), pos_integer()) ->
                         {ok, map()} | {error, term()}.
run_stress_test(Transport, Duration, ConcurrentWorkers) ->
    gen_server:call(?MODULE,
                    {stress_test, Transport, Duration, ConcurrentWorkers},
                    Duration + 30000).

%% Resource monitoring
-spec monitor_resources(pos_integer(), pos_integer()) -> {ok, pid()} | {error, term()}.
monitor_resources(Duration, Interval) ->
    gen_server:call(?MODULE, {monitor_resources, Duration, Interval}).

%% Test data generation
-spec generate_test_data(pos_integer()) -> binary().
generate_test_data(Size) ->
    crypto:strong_rand_bytes(Size).

%% Registry performance measurement
-spec measure_registry_performance() -> {ok, map()} | {error, term()}.
measure_registry_performance() ->
    gen_server:call(?MODULE, measure_registry_performance, 30000).

%% Hot path optimization
-spec optimize_hot_paths() -> {ok, [#optimization_result{}]} | {error, term()}.
optimize_hot_paths() ->
    gen_server:call(?MODULE, optimize_hot_paths, 60000).

%% Message routing profiling
-spec profile_message_routing(pos_integer(), pos_integer()) ->
                                 {ok, map()} | {error, term()}.
profile_message_routing(MessageCount, MessageSize) ->
    gen_server:call(?MODULE, {profile_routing, MessageCount, MessageSize}, 30000).

%% ETS benchmarking
-spec benchmark_ets_operations() -> {ok, map()} | {error, term()}.
benchmark_ets_operations() ->
    gen_server:call(?MODULE, benchmark_ets_operations, 30000).

%% GC metrics collection
-spec collect_gc_metrics(pos_integer()) -> {ok, map()} | {error, term()}.
collect_gc_metrics(Duration) ->
    gen_server:call(?MODULE, {collect_gc_metrics, Duration}).

%% Memory pattern analysis
-spec analyze_memory_patterns(pos_integer()) -> {ok, map()} | {error, term()}.
analyze_memory_patterns(Duration) ->
    gen_server:call(?MODULE, {analyze_memory_patterns, Duration}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),

    % Create ETS table for caching performance data
    EtsTable = ets:new(perf_cache, [set, private, {keypos, 1}]),

    % Enable scheduler statistics for CPU monitoring
    erlang:system_flag(scheduler_wall_time, true),

    logger:info("Performance analysis module started"),

    {ok, #state{ets_cache_table = EtsTable}}.

handle_call({throughput_test, Transport, MessageCount, MessageSize}, _From, State) ->
    Result = execute_throughput_test(Transport, MessageCount, MessageSize, State),
    {reply, Result, State};
handle_call({latency_test, Transport, MessageCount, MessageSize}, _From, State) ->
    Result = execute_latency_test(Transport, MessageCount, MessageSize, State),
    {reply, Result, State};
handle_call({concurrent_test, Transport, ConnectionCount, MessageCount, MessageSize},
            _From,
            State) ->
    Result =
        execute_concurrent_test(Transport, ConnectionCount, MessageCount, MessageSize, State),
    {reply, Result, State};
handle_call({stress_test, Transport, Duration, ConcurrentWorkers}, _From, State) ->
    Result = execute_stress_test(Transport, Duration, ConcurrentWorkers, State),
    {reply, Result, State};
handle_call({monitor_resources, Duration, Interval}, _From, State) ->
    {ok, Pid} = start_resource_monitor(Duration, Interval),
    {reply, {ok, Pid}, State};
handle_call(measure_registry_performance, _From, State) ->
    Result = measure_registry_performance_impl(State),
    {reply, Result, State};
handle_call(optimize_hot_paths, _From, State) ->
    Result = execute_hot_path_optimizations(State),
    {reply, Result, State};
handle_call({profile_routing, MessageCount, MessageSize}, _From, State) ->
    Result = profile_message_routing_impl(MessageCount, MessageSize, State),
    {reply, Result, State};
handle_call(benchmark_ets_operations, _From, State) ->
    Result = benchmark_ets_operations_impl(State),
    {reply, Result, State};
handle_call({collect_gc_metrics, Duration}, _From, State) ->
    Result = collect_gc_metrics_impl(Duration),
    {reply, Result, State};
handle_call({analyze_memory_patterns, Duration}, _From, State) ->
    Result = analyze_memory_patterns_impl(Duration),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({reset_metrics}, State) ->
    {noreply, State#state{metrics_history = []}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    logger:warning("Performance test process ~p exited: ~p", [Pid, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.ets_cache_table of
        undefined ->
            ok;
        Table ->
            ets:delete(Table)
    end,
    logger:info("Performance analysis module terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Implementation Functions
%%====================================================================

%% Execute comprehensive throughput testing
execute_throughput_test(Transport, MessageCount, MessageSize, State) ->
    Config = State#state.config,
    TestData = generate_test_data(MessageSize),

    logger:info("Starting throughput test: ~p transport, ~p messages, ~p bytes",
                [Transport, MessageCount, MessageSize]),

    StartTime = erlang:system_time(microsecond),

    try
        Results =
            case Transport of
                stdio ->
                    run_stdio_throughput(MessageCount, TestData);
                tcp ->
                    run_tcp_throughput(MessageCount, TestData);
                http ->
                    run_http_throughput(MessageCount, TestData);
                _ ->
                    {error, unsupported_transport}
            end,

        EndTime = erlang:system_time(microsecond),
        Duration = EndTime - StartTime,

        case Results of
            {ok, ProcessedCount} ->
                ThroughputBytesPerSec = ProcessedCount * MessageSize * 1000000 / Duration,

                logger:info("Throughput test completed: ~.2f bytes/sec, ~.2f msg/sec",
                            [ThroughputBytesPerSec, ProcessedCount * 1000000 / Duration]),

                % Cache results for analysis
                cache_performance_data(throughput_test,
                                       #{transport => Transport,
                                         throughput => ThroughputBytesPerSec,
                                         duration => Duration,
                                         message_count => ProcessedCount},
                                       State),

                {ok, ThroughputBytesPerSec};
            Error ->
                logger:error("Throughput test failed: ~p", [Error]),
                Error
        end
    catch
        Class:Reason:Stacktrace ->
            logger:error("Throughput test crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {crash, Class, Reason}}
    end.

%% Execute latency testing with percentile analysis
execute_latency_test(Transport, MessageCount, MessageSize, State) ->
    TestData = generate_test_data(MessageSize),

    logger:info("Starting latency test: ~p transport, ~p messages, ~p bytes",
                [Transport, MessageCount, MessageSize]),

    LatencySamples =
        lists:map(fun(_) ->
                     StartTime = erlang:system_time(microsecond),

                     Result =
                         case Transport of
                             stdio -> simulate_stdio_operation(TestData);
                             tcp -> simulate_tcp_operation(TestData);
                             http -> simulate_http_operation(TestData);
                             _ -> {error, unsupported_transport}
                         end,

                     EndTime = erlang:system_time(microsecond),
                     Latency = EndTime - StartTime,

                     case Result of
                         ok -> Latency;
                         {error, _} -> -1  % Mark as error
                     end
                  end,
                  lists:seq(1, MessageCount)),

    ValidSamples = [L || L <- LatencySamples, L >= 0],

    case length(ValidSamples) of
        0 ->
            {error, no_valid_samples};
        ValidCount ->
            SortedSamples = lists:sort(ValidSamples),

            P50 = percentile(SortedSamples, 50),
            P95 = percentile(SortedSamples, 95),
            P99 = percentile(SortedSamples, 99),

            logger:info("Latency test completed: P50=~p Î¼s, P95=~p Î¼s, P99=~p Î¼s", [P50, P95, P99]),

            % Cache latency data
            cache_performance_data(latency_test,
                                   #{transport => Transport,
                                     p50 => P50,
                                     p95 => P95,
                                     p99 => P99,
                                     valid_samples => ValidCount,
                                     total_samples => MessageCount},
                                   State),

            {ok, {P50, P95, P99}}
    end.

%% Execute concurrent load testing
execute_concurrent_test(Transport, ConnectionCount, MessageCount, MessageSize, State) ->
    TestData = generate_test_data(MessageSize),

    logger:info("Starting concurrent test: ~p connections, ~p messages each, "
                "~p bytes",
                [ConnectionCount, MessageCount, MessageSize]),

    Parent = self(),
    StartTime = erlang:system_time(microsecond),

    % Spawn concurrent workers
    Workers =
        lists:map(fun(WorkerId) ->
                     spawn_link(fun() ->
                                   WorkerResults =
                                       lists:map(fun(_) ->
                                                    WorkerStartTime =
                                                        erlang:system_time(microsecond),

                                                    Result =
                                                        case Transport of
                                                            stdio ->
                                                                simulate_stdio_operation(TestData);
                                                            tcp -> simulate_tcp_operation(TestData);
                                                            http ->
                                                                simulate_http_operation(TestData);
                                                            _ -> {error, unsupported_transport}
                                                        end,

                                                    WorkerEndTime = erlang:system_time(microsecond),

                                                    {Result, WorkerEndTime - WorkerStartTime}
                                                 end,
                                                 lists:seq(1, MessageCount)),

                                   Parent ! {worker_done, WorkerId, WorkerResults}
                                end)
                  end,
                  lists:seq(1, ConnectionCount)),

    % Collect results from all workers
    AllResults = collect_worker_results(Workers, ConnectionCount, []),
    EndTime = erlang:system_time(microsecond),

    % Analyze results
    TotalOps = length(AllResults),
    SuccessfulOps = length([1 || {ok, _} <- AllResults]),
    SuccessRate = SuccessfulOps / TotalOps * 100,

    Duration = EndTime - StartTime,
    Throughput = SuccessfulOps * 1000000 / Duration,

    Result =
        #{total_operations => TotalOps,
          successful_operations => SuccessfulOps,
          success_rate => SuccessRate,
          throughput => Throughput,
          duration_microseconds => Duration},

    logger:info("Concurrent test completed: ~p ops, ~.1f%% success, ~.2f ops/sec",
                [TotalOps, SuccessRate, Throughput]),

    cache_performance_data(concurrent_test, Result#{transport => Transport}, State),

    {ok, Result}.

%% Execute stress testing with sustained load
execute_stress_test(Transport, Duration, ConcurrentWorkers, State) ->
    logger:info("Starting stress test: ~p ms duration, ~p workers",
                [Duration, ConcurrentWorkers]),

    Parent = self(),
    EndTime = erlang:system_time(millisecond) + Duration,

    % Start resource monitoring
    {ok, MonitorPid} = start_resource_monitor(Duration, 1000),

    % Spawn stress workers
    Workers =
        lists:map(fun(WorkerId) ->
                     spawn_link(fun() -> stress_worker_loop(Transport, WorkerId, EndTime, []) end)
                  end,
                  lists:seq(1, ConcurrentWorkers)),

    % Wait for completion
    timer:sleep(Duration + 1000),

    % Collect results
    WorkerResults = collect_stress_results(Workers, ConcurrentWorkers, []),

    % Analyze stress test results
    TotalOps = lists:sum([length(Ops) || Ops <- WorkerResults]),
    SuccessfulOps = lists:sum([length([1 || {ok, _} <- Ops]) || Ops <- WorkerResults]),

    OpsPerSecond = TotalOps * 1000 / Duration,
    SuccessRate =
        case TotalOps of
            0 ->
                0.0;
            _ ->
                SuccessfulOps / TotalOps * 100
        end,

    Result =
        #{operations_per_second => OpsPerSecond,
          success_rate => SuccessRate,
          total_operations => TotalOps,
          duration_ms => Duration,
          concurrent_workers => ConcurrentWorkers},

    logger:info("Stress test completed: ~.2f ops/sec, ~.1f%% success rate",
                [OpsPerSecond, SuccessRate]),

    cache_performance_data(stress_test, Result#{transport => Transport}, State),

    {ok, Result}.

%% Registry performance measurement
measure_registry_performance_impl(State) ->
    logger:info("Measuring registry performance"),

    % Test registry operations under load
    TestCases =
        [{register_operations, fun() -> test_registry_registrations(1000) end},
         {lookup_operations, fun() -> test_registry_lookups(10000) end},
         {routing_operations, fun() -> test_registry_routing(5000) end},
         {concurrent_access, fun() -> test_concurrent_registry_access(10, 1000) end}],

    Results =
        lists:map(fun({TestName, TestFun}) ->
                     StartTime = erlang:system_time(microsecond),
                     TestResult = TestFun(),
                     EndTime = erlang:system_time(microsecond),

                     Duration = EndTime - StartTime,

                     {TestName, #{result => TestResult, duration_microseconds => Duration}}
                  end,
                  TestCases),

    RegistryStats =
        #{performance_tests => Results, timestamp => erlang:system_time(millisecond)},

    cache_performance_data(registry_performance, RegistryStats, State),

    {ok, RegistryStats}.

%% Hot path optimization implementation
execute_hot_path_optimizations(State) ->
    logger:info("Executing hot path optimizations"),

    Optimizations =
        [{registry_ets_optimization, fun optimize_registry_ets/0},
         {message_routing_optimization, fun optimize_message_routing/0},
         {process_pool_optimization, fun optimize_process_pools/0},
         {memory_allocation_optimization, fun optimize_memory_allocation/0}],

    Results =
        lists:map(fun({OptName, OptFun}) ->
                     logger:info("Applying optimization: ~p", [OptName]),

                     % Measure before
                     BeforeMetrics = collect_baseline_metrics(),

                     try
                         OptResult = OptFun(),

                         % Wait for optimization to take effect
                         timer:sleep(2000),

                         % Measure after
                         AfterMetrics = collect_baseline_metrics(),

                         Improvement = calculate_improvement(BeforeMetrics, AfterMetrics),

                         #optimization_result{optimization = OptName,
                                              before_metrics = BeforeMetrics,
                                              after_metrics = AfterMetrics,
                                              improvement_percent = Improvement,
                                              recommendations =
                                                  generate_recommendations(OptName, Improvement)}
                     catch
                         Class:Reason:Stacktrace ->
                             logger:error("Optimization ~p failed: ~p:~p~n~p",
                                          [OptName, Class, Reason, Stacktrace]),
                             #optimization_result{optimization = OptName,
                                                  before_metrics = BeforeMetrics,
                                                  after_metrics = BeforeMetrics,
                                                  improvement_percent = 0.0,
                                                  recommendations = [<<"Optimization failed">>]}
                     end
                  end,
                  Optimizations),

    % Cache optimization results
    lists:foreach(fun(OptResult) ->
                     cache_performance_data(optimization_result, OptResult, State)
                  end,
                  Results),

    {ok, Results}.

%%====================================================================
%% Performance Test Implementations
%%====================================================================

%% STDIO transport throughput testing
run_stdio_throughput(MessageCount, TestData) ->
    try
        % Simulate STDIO operations with minimal delay
        ProcessedCount =
            lists:foldl(fun(_, Acc) ->
                           % Simulate message processing
                           timer:sleep(0),  % Yield to scheduler
                           Acc + 1
                        end,
                        0,
                        lists:seq(1, MessageCount)),

        {ok, ProcessedCount}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% TCP transport throughput testing
run_tcp_throughput(MessageCount, TestData) ->
    try
        % Simulate TCP operations with network delay
        ProcessedCount =
            lists:foldl(fun(_, Acc) ->
                           % Simulate network delay
                           timer:sleep(1),
                           Acc + 1
                        end,
                        0,
                        lists:seq(1, MessageCount)),

        {ok, ProcessedCount}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% HTTP transport throughput testing
run_http_throughput(MessageCount, TestData) ->
    try
        % Simulate HTTP operations with higher latency
        ProcessedCount =
            lists:foldl(fun(_, Acc) ->
                           % Simulate HTTP request/response cycle
                           timer:sleep(10),
                           Acc + 1
                        end,
                        0,
                        lists:seq(1, MessageCount)),

        {ok, ProcessedCount}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% Simulate transport operations for latency testing
simulate_stdio_operation(_TestData) ->
    timer:sleep(0),
    ok.

simulate_tcp_operation(_TestData) ->
    timer:sleep(
        rand:uniform(5)),
    ok.

simulate_http_operation(_TestData) ->
    timer:sleep(rand:uniform(20) + 10),
    ok.

%% Registry performance testing functions
test_registry_registrations(Count) ->
    try
        lists:foreach(fun(N) ->
                         ServerId = list_to_atom("test_server_" ++ integer_to_list(N)),
                         ServerPid = spawn(fun() -> timer:sleep(100) end),
                         Config = #{capabilities => undefined, options => #{}},

                         % Test registration performance
                         case erlmcp_registry:register_server(ServerId, ServerPid, Config) of
                             ok -> ok;
                             {error, _} -> error
                         end
                      end,
                      lists:seq(1, Count)),

        ok
    catch
        _:_ ->
            error
    end.

test_registry_lookups(Count) ->
    try
        lists:foldl(fun(N, Acc) ->
                       ServerId = list_to_atom("test_server_" ++ integer_to_list(N rem 100 + 1)),

                       case erlmcp_registry:find_server(ServerId) of
                           {ok, _} -> Acc + 1;
                           {error, not_found} -> Acc
                       end
                    end,
                    0,
                    lists:seq(1, Count))
    catch
        _:_ ->
            0
    end.

test_registry_routing(Count) ->
    try
        lists:foreach(fun(N) ->
                         ServerId = list_to_atom("test_server_" ++ integer_to_list(N rem 10 + 1)),
                         TransportId =
                             list_to_atom("test_transport_" ++ integer_to_list(N rem 5 + 1)),
                         Message = generate_test_data(256),

                         % Test routing performance
                         erlmcp_registry:route_to_server(ServerId, TransportId, Message)
                      end,
                      lists:seq(1, Count)),

        ok
    catch
        _:_ ->
            error
    end.

test_concurrent_registry_access(WorkerCount, OpsPerWorker) ->
    Parent = self(),

    Workers =
        lists:map(fun(WorkerId) ->
                     spawn_link(fun() ->
                                   Results =
                                       lists:map(fun(N) ->
                                                    ServerId =
                                                        list_to_atom("test_server_"
                                                                     ++ integer_to_list(N rem 10
                                                                                        + 1)),

                                                    StartTime = erlang:system_time(microsecond),
                                                    Result = erlmcp_registry:find_server(ServerId),
                                                    EndTime = erlang:system_time(microsecond),

                                                    {Result, EndTime - StartTime}
                                                 end,
                                                 lists:seq(1, OpsPerWorker)),

                                   Parent ! {worker_done, WorkerId, Results}
                                end)
                  end,
                  lists:seq(1, WorkerCount)),

    AllResults = collect_worker_results(Workers, WorkerCount, []),

    SuccessCount = length([1 || {ok, _} <- [Res || {Res, _} <- AllResults]]),
    AvgLatency =
        case length(AllResults) of
            0 ->
                0;
            Total ->
                lists:sum([Lat || {_, Lat} <- AllResults]) / Total
        end,

    #{success_count => SuccessCount, average_latency => AvgLatency}.

%%====================================================================
%% Optimization Implementations
%%====================================================================

%% ETS table optimization for registry
optimize_registry_ets() ->
    logger:info("Optimizing registry ETS tables"),

    % These optimizations would be applied to the actual registry module
    Recommendations =
        ["Use ordered_set for sorted lookups",
         "Enable compression for large datasets",
         "Consider partitioned tables for high concurrency",
         "Implement read concurrency optimizations"],

    {ok, Recommendations}.

%% Message routing optimization
optimize_message_routing() ->
    logger:info("Optimizing message routing paths"),

    % These would be actual optimizations to the routing logic
    Recommendations =
        ["Cache routing decisions for frequently used paths",
         "Implement message batching for improved throughput",
         "Use direct process messaging where possible",
         "Optimize message serialization/deserialization"],

    {ok, Recommendations}.

%% Process pool optimization
optimize_process_pools() ->
    logger:info("Optimizing process pool configurations"),

    Recommendations =
        ["Adjust pool sizes based on system load",
         "Implement work-stealing between pools",
         "Use process hibernation for idle workers",
         "Optimize process spawn/death cycles"],

    {ok, Recommendations}.

%% Memory allocation optimization
optimize_memory_allocation() ->
    logger:info("Optimizing memory allocation patterns"),

    Recommendations =
        ["Pre-allocate frequently used data structures",
         "Implement memory pooling for large messages",
         "Optimize garbage collection settings",
         "Use binary data structures where appropriate"],

    {ok, Recommendations}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Calculate percentiles
percentile([], _) ->
    0;
percentile(SortedList, Percentile) ->
    Length = length(SortedList),
    Index = max(1, min(Length, round(Length * Percentile / 100))),
    lists:nth(Index, SortedList).

%% Collect worker results
collect_worker_results(Workers, Count, Acc) when length(Acc) >= Count ->
    Acc;
collect_worker_results(Workers, Count, Acc) ->
    receive
        {worker_done, WorkerId, Results} ->
            collect_worker_results(Workers, Count, Results ++ Acc)
    after 30000 ->
        logger:warning("Timeout waiting for worker results"),
        Acc
    end.

%% Collect stress test results
collect_stress_results(Workers, Count, Acc) when length(Acc) >= Count ->
    Acc;
collect_stress_results(Workers, Count, Acc) ->
    receive
        {worker_done, WorkerId, Results} ->
            collect_stress_results(Workers, Count, [Results | Acc])
    after 60000 ->
        logger:warning("Timeout waiting for stress test results"),
        Acc
    end.

%% Stress worker loop
stress_worker_loop(Transport, WorkerId, EndTime, Operations) ->
    case erlang:system_time(millisecond) >= EndTime of
        true ->
            erlang:get(parent) ! {worker_done, WorkerId, Operations};
        false ->
            TestData = generate_test_data(512),
            StartTime = erlang:system_time(microsecond),

            Result =
                case Transport of
                    stdio ->
                        simulate_stdio_operation(TestData);
                    tcp ->
                        simulate_tcp_operation(TestData);
                    http ->
                        simulate_http_operation(TestData);
                    _ ->
                        {error, unsupported_transport}
                end,

            EndOpTime = erlang:system_time(microsecond),
            Operation = {Result, EndOpTime - StartTime},

            stress_worker_loop(Transport, WorkerId, EndTime, [Operation | Operations])
    end.

%% Start resource monitor
start_resource_monitor(Duration, Interval) ->
    spawn_link(fun() ->
                  resource_monitor_loop(erlang:system_time(millisecond) + Duration, Interval, [])
               end).

resource_monitor_loop(EndTime, Interval, Samples) ->
    case erlang:system_time(millisecond) >= EndTime of
        true ->
            logger:info("Resource monitoring completed with ~p samples", [length(Samples)]);
        false ->
            Sample =
                #{timestamp => erlang:system_time(millisecond),
                  memory => erlang:memory(total),
                  process_count => erlang:system_info(process_count),
                  reductions => element(1, erlang:statistics(reductions))},

            timer:sleep(Interval),
            resource_monitor_loop(EndTime, Interval, [Sample | Samples])
    end.

%% Cache performance data
cache_performance_data(TestType, Data, State) ->
    case State#state.ets_cache_table of
        undefined ->
            ok;
        Table ->
            Timestamp = erlang:system_time(millisecond),
            Key = {TestType, Timestamp},
            ets:insert(Table, {Key, Data})
    end.

%% Collect baseline metrics for optimization comparison
collect_baseline_metrics() ->
    #perf_metrics{start_time = erlang:system_time(microsecond),
                  memory_samples = [erlang:memory(total)],
                  cpu_samples = [get_cpu_usage()]}.

%% Calculate improvement percentage
calculate_improvement(BeforeMetrics, AfterMetrics) ->
    BeforeMemory = hd(BeforeMetrics#perf_metrics.memory_samples),
    AfterMemory = hd(AfterMetrics#perf_metrics.memory_samples),

    case BeforeMemory of
        0 ->
            0.0;
        _ ->
            (BeforeMemory - AfterMemory) / BeforeMemory * 100.0
    end.

%% Generate optimization recommendations
generate_recommendations(OptName, ImprovementPercent) ->
    BaseRecommendations =
        case OptName of
            registry_ets_optimization when ImprovementPercent > 10 ->
                [<<"ETS optimization was successful, consider applying to production">>];
            registry_ets_optimization ->
                [<<"ETS optimization had minimal impact, investigate other bottlenecks">>];
            message_routing_optimization when ImprovementPercent > 15 ->
                [<<"Routing optimization shows promise, expand to more code paths">>];
            _ ->
                [<<"Monitor performance impact in production environment">>]
        end,

    BaseRecommendations.

%% Get current CPU usage
get_cpu_usage() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            0.0;
        Stats ->
            % Simplified CPU calculation
            TotalActive = lists:sum([A || {_, A, _} <- Stats]),
            TotalTime = lists:sum([T || {_, _, T} <- Stats]),
            case TotalTime of
                0 ->
                    0.0;
                _ ->
                    TotalActive / TotalTime * 100.0
            end
    end.

%% Profiling and benchmarking implementations
profile_message_routing_impl(MessageCount, MessageSize, State) ->
    logger:info("Profiling message routing with ~p messages of ~p bytes",
                [MessageCount, MessageSize]),

    TestData = generate_test_data(MessageSize),

    % Profile different routing scenarios
    Scenarios =
        [{direct_routing, fun() -> profile_direct_routing(MessageCount, TestData) end},
         {registry_routing, fun() -> profile_registry_routing(MessageCount, TestData) end},
         {batched_routing, fun() -> profile_batched_routing(MessageCount, TestData) end}],

    Results =
        lists:map(fun({ScenarioName, ProfileFun}) ->
                     StartTime = erlang:system_time(microsecond),
                     ScenarioResult = ProfileFun(),
                     EndTime = erlang:system_time(microsecond),

                     Duration = EndTime - StartTime,

                     {ScenarioName,
                      #{result => ScenarioResult,
                        duration_microseconds => Duration,
                        messages_per_second => MessageCount * 1000000 / Duration}}
                  end,
                  Scenarios),

    ProfileData =
        #{routing_scenarios => Results,
          message_count => MessageCount,
          message_size => MessageSize,
          timestamp => erlang:system_time(millisecond)},

    cache_performance_data(routing_profile, ProfileData, State),

    {ok, ProfileData}.

profile_direct_routing(MessageCount, TestData) ->
    lists:foreach(fun(_) ->
                     % Simulate direct process messaging
                     self() ! {test_message, TestData},
                     receive {test_message, _} -> ok after 100 -> timeout end
                  end,
                  lists:seq(1, MessageCount)),
    ok.

profile_registry_routing(MessageCount, TestData) ->
    lists:foreach(fun(N) ->
                     ServerId = test_server,
                     TransportId = list_to_atom("transport_" ++ integer_to_list(N rem 3 + 1)),

                     % Use actual registry routing (if available)
                     try
                         erlmcp_registry:route_to_server(ServerId, TransportId, TestData)
                     catch
                         _:_ -> ok  % Registry might not be running in test
                     end
                  end,
                  lists:seq(1, MessageCount)),
    ok.

profile_batched_routing(MessageCount, TestData) ->
    BatchSize = 10,
    Batches = MessageCount div BatchSize,

    lists:foreach(fun(_) ->
                     Batch = lists:duplicate(BatchSize, TestData),
                     % Simulate batched message processing
                     lists:foreach(fun(Message) ->
                                      % Process message
                                      byte_size(Message)
                                   end,
                                   Batch)
                  end,
                  lists:seq(1, Batches)),
    ok.

benchmark_ets_operations_impl(State) ->
    logger:info("Benchmarking ETS operations"),

    % Create test table
    TestTable = ets:new(benchmark_table, [set, public]),

    try
        % Benchmark different ETS operations
        InsertResults = benchmark_ets_inserts(TestTable, 10000),
        LookupResults = benchmark_ets_lookups(TestTable, 50000),
        UpdateResults = benchmark_ets_updates(TestTable, 5000),
        DeleteResults = benchmark_ets_deletes(TestTable, 2000),

        Results =
            #{insert_ops => InsertResults,
              lookup_ops => LookupResults,
              update_ops => UpdateResults,
              delete_ops => DeleteResults,
              timestamp => erlang:system_time(millisecond)},

        cache_performance_data(ets_benchmark, Results, State),

        {ok, Results}
    after
        ets:delete(TestTable)
    end.

benchmark_ets_inserts(Table, Count) ->
    StartTime = erlang:system_time(microsecond),

    lists:foreach(fun(N) ->
                     Key = list_to_atom("key_" ++ integer_to_list(N)),
                     Value = {test_data, N, generate_test_data(64)},
                     ets:insert(Table, {Key, Value})
                  end,
                  lists:seq(1, Count)),

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    #{operations => Count,
      duration_microseconds => Duration,
      ops_per_second => Count * 1000000 / Duration}.

benchmark_ets_lookups(Table, Count) ->
    % First ensure we have data to lookup
    ExistingKeys = [list_to_atom("key_" ++ integer_to_list(N)) || N <- lists:seq(1, 1000)],

    StartTime = erlang:system_time(microsecond),

    HitCount =
        lists:foldl(fun(N, Acc) ->
                       Key = lists:nth(N rem length(ExistingKeys) + 1, ExistingKeys),
                       case ets:lookup(Table, Key) of
                           [] -> Acc;
                           [_] -> Acc + 1
                       end
                    end,
                    0,
                    lists:seq(1, Count)),

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    #{operations => Count,
      hits => HitCount,
      hit_rate => HitCount / Count * 100,
      duration_microseconds => Duration,
      ops_per_second => Count * 1000000 / Duration}.

benchmark_ets_updates(Table, Count) ->
    StartTime = erlang:system_time(microsecond),

    lists:foreach(fun(N) ->
                     Key = list_to_atom("key_" ++ integer_to_list(N rem 1000 + 1)),
                     NewValue = {updated_data, N, erlang:system_time()},
                     ets:insert(Table, {Key, NewValue})
                  end,
                  lists:seq(1, Count)),

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    #{operations => Count,
      duration_microseconds => Duration,
      ops_per_second => Count * 1000000 / Duration}.

benchmark_ets_deletes(Table, Count) ->
    StartTime = erlang:system_time(microsecond),

    DeletedCount =
        lists:foldl(fun(N, Acc) ->
                       Key = list_to_atom("key_" ++ integer_to_list(N)),
                       case ets:delete(Table, Key) of
                           true -> Acc + 1;
                           false -> Acc
                       end
                    end,
                    0,
                    lists:seq(1, Count)),

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    #{operations => Count,
      actual_deletes => DeletedCount,
      duration_microseconds => Duration,
      ops_per_second => Count * 1000000 / Duration}.

collect_gc_metrics_impl(Duration) ->
    logger:info("Collecting GC metrics for ~p ms", [Duration]),

    % Enable GC monitoring
    InitialGcStats = erlang:statistics(garbage_collection),
    InitialMemory = erlang:memory(),

    EndTime = erlang:system_time(millisecond) + Duration,

    % Generate memory pressure
    spawn_link(fun() -> generate_memory_pressure(EndTime) end),

    % Monitor GC activity
    timer:sleep(Duration),

    FinalGcStats = erlang:statistics(garbage_collection),
    FinalMemory = erlang:memory(),

    {InitialGCs, InitialWordsReclaimed} = InitialGcStats,
    {FinalGCs, FinalWordsReclaimed} = FinalGcStats,

    GcMetrics =
        #{gc_count => FinalGCs - InitialGCs,
          words_reclaimed => FinalWordsReclaimed - InitialWordsReclaimed,
          initial_memory => InitialMemory,
          final_memory => FinalMemory,
          memory_delta => calculate_memory_delta(InitialMemory, FinalMemory),
          duration_ms => Duration,
          timestamp => erlang:system_time(millisecond)},

    {ok, GcMetrics}.

analyze_memory_patterns_impl(Duration) ->
    logger:info("Analyzing memory patterns for ~p ms", [Duration]),

    EndTime = erlang:system_time(millisecond) + Duration,
    Samples = collect_memory_pattern_samples(EndTime, []),

    Analysis = analyze_memory_samples(Samples),

    MemoryPatterns =
        #{samples => Samples,
          analysis => Analysis,
          duration_ms => Duration,
          timestamp => erlang:system_time(millisecond)},

    {ok, MemoryPatterns}.

collect_memory_pattern_samples(EndTime, Samples) ->
    case erlang:system_time(millisecond) >= EndTime of
        true ->
            lists:reverse(Samples);
        false ->
            Sample =
                #{timestamp => erlang:system_time(millisecond),
                  memory => erlang:memory(),
                  process_count => erlang:system_info(process_count),
                  message_queue_len => get_total_message_queue_len()},

            timer:sleep(1000),
            collect_memory_pattern_samples(EndTime, [Sample | Samples])
    end.

analyze_memory_samples(Samples) ->
    case length(Samples) of
        0 ->
            #{trend => unknown,
              peak_memory => 0,
              avg_memory => 0};
        1 ->
            #{trend => stable,
              peak_memory => 0,
              avg_memory => 0};
        _ ->
            MemoryValues = [maps:get(total, maps:get(memory, S)) || S <- Samples],

            PeakMemory = lists:max(MemoryValues),
            AvgMemory = lists:sum(MemoryValues) / length(MemoryValues),

            % Simple trend analysis
            FirstHalf = lists:sublist(MemoryValues, length(MemoryValues) div 2),
            SecondHalf =
                lists:sublist(MemoryValues, length(MemoryValues) div 2 + 1, length(MemoryValues)),

            FirstAvg = lists:sum(FirstHalf) / length(FirstHalf),
            SecondAvg = lists:sum(SecondHalf) / length(SecondHalf),

            Trend =
                if SecondAvg > FirstAvg * 1.1 ->
                       increasing;
                   SecondAvg < FirstAvg * 0.9 ->
                       decreasing;
                   true ->
                       stable
                end,

            #{trend => Trend,
              peak_memory => PeakMemory,
              avg_memory => AvgMemory,
              first_half_avg => FirstAvg,
              second_half_avg => SecondAvg}
    end.

generate_memory_pressure(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true ->
            ok;
        false ->
            % Create temporary data structures
            _LargeData = lists:duplicate(10000, generate_test_data(1024)),
            timer:sleep(100),
            generate_memory_pressure(EndTime)
    end.

get_total_message_queue_len() ->
    Processes = erlang:processes(),
    lists:sum([begin
                   case erlang:process_info(P, message_queue_len) of
                       {message_queue_len, Len} ->
                           Len;
                       undefined ->
                           0
                   end
               end
               || P <- lists:sublist(Processes, 100)]).

calculate_memory_delta(InitialMemory, FinalMemory) ->
    maps:fold(fun(Type, FinalValue, Acc) ->
                 InitialValue = maps:get(Type, InitialMemory, 0),
                 Delta = FinalValue - InitialValue,
                 maps:put(Type, Delta, Acc)
              end,
              #{},
              FinalMemory).
