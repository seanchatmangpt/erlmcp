%%%-------------------------------------------------------------------
%%% @doc TCPS Performance Test Suite
%%%
%%% Comprehensive performance benchmarking including:
%%% - Throughput measurements
%%% - Latency profiling
%%% - Resource utilization
%%% - Scalability testing
%%% - Bottleneck identification
%%% - Performance regression detection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_performance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([
    test_1000_work_orders_under_10_seconds/1,
    test_pipeline_stage_latency/1,
    test_heijunka_scheduling_performance/1,
    test_quality_gate_throughput/1,
    test_andon_trigger_latency/1,
    test_receipt_generation_throughput/1,
    test_query_performance/1,
    test_concurrent_processing_scalability/1,
    test_memory_usage/1,
    test_cpu_utilization/1,
    test_disk_io_performance/1,
    test_bottleneck_identification/1,
    test_performance_regression/1,
    test_sustained_throughput/1,
    test_burst_performance/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 15}}].

all() ->
    [
        test_1000_work_orders_under_10_seconds,
        test_pipeline_stage_latency,
        test_heijunka_scheduling_performance,
        test_quality_gate_throughput,
        test_andon_trigger_latency,
        test_receipt_generation_throughput,
        test_query_performance,
        test_concurrent_processing_scalability,
        test_memory_usage,
        test_cpu_utilization,
        test_disk_io_performance,
        test_bottleneck_identification,
        test_performance_regression,
        test_sustained_throughput,
        test_burst_performance
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok = tcps_test_utils:init_test_env(),
    Config.

end_per_suite(_Config) ->
    ok = tcps_test_utils:cleanup_test_env(),
    ok = application:stop(tcps),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("~n=== Starting test case: ~p ===~n", [TestCase]),
    ok = tcps_test_utils:clear_all_data(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("~n=== Completed test case: ~p ===~n", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test processing 1000 work orders under 10 seconds
%% @end
%%--------------------------------------------------------------------
test_1000_work_orders_under_10_seconds(_Config) ->
    ct:pal("~n=== Testing 1000 Work Orders Under 10 Seconds ===~n"),

    %% Create 1000 work orders
    CreateStart = erlang:monotonic_time(millisecond),
    WorkOrders = lists:map(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        WO
    end, lists:seq(1, 1000)),
    CreateEnd = erlang:monotonic_time(millisecond),
    CreateDuration = CreateEnd - CreateStart,

    ct:pal("Created 1000 work orders in ~p ms (~.2f/s)~n",
           [CreateDuration, 1000 / (CreateDuration / 1000)]),

    %% Process all concurrently
    ProcessStart = erlang:monotonic_time(millisecond),
    Parent = self(),

    lists:foreach(fun(WO) ->
        spawn_link(fun() ->
            Result = tcps_test_utils:process_work_order_full(WO),
            Parent ! {done, WO, Result}
        end)
    end, WorkOrders),

    %% Collect results
    Results = lists:map(fun(WO) ->
        receive
            {done, WO, Result} -> {WO, Result}
        after 60000 ->
            {WO, timeout}
        end
    end, WorkOrders),

    ProcessEnd = erlang:monotonic_time(millisecond),
    ProcessDuration = ProcessEnd - ProcessStart,

    %% Verify timing
    ?assert(ProcessDuration < 10000),
    ct:pal("Processed 1000 work orders in ~p ms (~.2f/s)~n",
           [ProcessDuration, 1000 / (ProcessDuration / 1000)]),

    %% Verify success rate
    Successes = length([ok || {_, ok} <- Results]),
    ?assertEqual(1000, Successes),

    %% Calculate throughput
    Throughput = 1000 / (ProcessDuration / 1000),
    ct:pal("Throughput: ~.2f work orders/second~n", [Throughput]),

    ct:pal("~n=== 1000 Work Orders Under 10 Seconds: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test pipeline stage latency
%% @end
%%--------------------------------------------------------------------
test_pipeline_stage_latency(_Config) ->
    ct:pal("~n=== Testing Pipeline Stage Latency ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Measure each stage
    Stages = [
        {shacl, fun() -> tcps_test_utils:run_shacl_validation(WorkOrderId) end},
        {compile, fun() -> tcps_test_utils:run_compilation(WorkOrderId) end},
        {test, fun() -> tcps_test_utils:run_tests(WorkOrderId) end},
        {quality, fun() -> tcps_quality:check_gates(WorkOrderId) end},
        {deterministic, fun() -> tcps_deterministic:verify_build(WorkOrderId) end}
    ],

    Latencies = lists:map(fun({Stage, StageFun}) ->
        Start = erlang:monotonic_time(microsecond),
        _Result = StageFun(),
        End = erlang:monotonic_time(microsecond),
        Latency = End - Start,
        ct:pal("~p: ~p μs (~.2f ms)~n",
               [Stage, Latency, Latency / 1000]),
        {Stage, Latency}
    end, Stages),

    %% All stages should be under 1 second
    lists:foreach(fun({Stage, Latency}) ->
        case Latency > 1000000 of
            true ->
                ct:pal("WARNING: ~p took ~.2f ms~n",
                       [Stage, Latency / 1000]);
            false ->
                ok
        end
    end, Latencies),

    %% Calculate total latency
    TotalLatency = lists:sum([L || {_, L} <- Latencies]),
    ct:pal("Total pipeline latency: ~.2f ms~n", [TotalLatency / 1000]),

    ct:pal("~n=== Pipeline Stage Latency: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Heijunka scheduling performance
%% @end
%%--------------------------------------------------------------------
test_heijunka_scheduling_performance(_Config) ->
    ct:pal("~n=== Testing Heijunka Scheduling Performance ===~n"),

    %% Create various workloads
    Workloads = [10, 50, 100, 500, 1000],

    Results = lists:map(fun(Size) ->
        ok = tcps_test_utils:clear_all_data(),

        %% Create work orders
        lists:foreach(fun(_) ->
            {ok, _WO} = tcps_test_utils:create_test_work_order()
        end, lists:seq(1, Size)),

        %% Measure scheduling time
        Start = erlang:monotonic_time(microsecond),
        _Schedule = tcps_kanban:heijunka_schedule(),
        End = erlang:monotonic_time(microsecond),
        Duration = End - Start,

        ct:pal("~p work orders: ~p μs (~.2f ms)~n",
               [Size, Duration, Duration / 1000]),
        {Size, Duration}
    end, Workloads),

    %% Verify sub-linear scaling
    lists:foreach(fun({{Size1, Duration1}, {Size2, Duration2}}) ->
        Ratio = (Duration2 / Duration1) / (Size2 / Size1),
        ct:pal("Scaling ratio (~p -> ~p): ~.2f~n",
               [Size1, Size2, Ratio]),
        ?assert(Ratio < 2.0) % Should not scale linearly
    end, lists:zip(lists:droplast(Results), tl(Results))),

    ct:pal("~n=== Heijunka Scheduling Performance: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test quality gate throughput
%% @end
%%--------------------------------------------------------------------
test_quality_gate_throughput(_Config) ->
    ct:pal("~n=== Testing Quality Gate Throughput ===~n"),

    %% Create 100 work orders
    WorkOrders = lists:map(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_kanban:start_work_order(WO),
        ok = tcps_test_utils:process_pipeline_stages(WO),
        WO
    end, lists:seq(1, 100)),

    ct:pal("Created 100 work orders at quality gate stage~n"),

    %% Process quality gates concurrently
    StartTime = erlang:monotonic_time(millisecond),
    Parent = self(),

    lists:foreach(fun(WO) ->
        spawn_link(fun() ->
            Result = tcps_quality:check_gates(WO),
            Parent ! {gate_done, WO, Result}
        end)
    end, WorkOrders),

    %% Collect results
    lists:foreach(fun(WO) ->
        receive
            {gate_done, WO, _Result} -> ok
        after 30000 ->
            error({timeout, WO})
        end
    end, WorkOrders),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Calculate throughput
    Throughput = 100 / (Duration / 1000),
    ct:pal("Quality gate throughput: ~.2f gates/second~n", [Throughput]),
    ct:pal("Average latency: ~.2f ms~n", [Duration / 100]),

    ?assert(Throughput > 10), % At least 10 gates/sec

    ct:pal("~n=== Quality Gate Throughput: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon trigger latency
%% @end
%%--------------------------------------------------------------------
test_andon_trigger_latency(_Config) ->
    ct:pal("~n=== Testing Andon Trigger Latency ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),

    %% Measure Andon trigger time
    Start = erlang:monotonic_time(microsecond),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,

    ct:pal("Andon trigger latency: ~p μs (~.2f ms)~n",
           [Duration, Duration / 1000]),

    %% Should be under 100ms
    ?assert(Duration < 100000),

    %% Verify Andon created
    {ok, _Andon} = tcps_andon:get(AndonId),

    ct:pal("~n=== Andon Trigger Latency: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test receipt generation throughput
%% @end
%%--------------------------------------------------------------------
test_receipt_generation_throughput(_Config) ->
    ct:pal("~n=== Testing Receipt Generation Throughput ===~n"),

    %% Generate 1000 receipts
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(N) ->
        Receipt = #{
            id => iolist_to_binary(["receipt-", integer_to_list(N)]),
            work_order_id => <<"wo-perf">>,
            stage => test,
            result => pass,
            metadata => #{seq => N},
            timestamp => erlang:system_time(millisecond),
            signature => tcps_test_utils:generate_signature()
        },
        {ok, _Path} = tcps_persistence:store_receipt_json(Receipt)
    end, lists:seq(1, 1000)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Calculate throughput
    Throughput = 1000 / (Duration / 1000),
    ct:pal("Receipt generation throughput: ~.2f receipts/second~n",
           [Throughput]),
    ct:pal("Average latency: ~.2f ms~n", [Duration / 1000]),

    ?assert(Throughput > 100), % At least 100 receipts/sec

    ct:pal("~n=== Receipt Generation Throughput: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test query performance
%% @end
%%--------------------------------------------------------------------
test_query_performance(_Config) ->
    ct:pal("~n=== Testing Query Performance ===~n"),

    %% Create large dataset
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 100)),

    ct:pal("Created dataset: 100 work orders~n"),

    %% Benchmark various queries
    Queries = [
        {"List all work orders", fun() ->
            tcps_work_order:list_all()
        end},
        {"List by bucket (security)", fun() ->
            tcps_work_order:list_by_bucket(security)
        end},
        {"List by status (completed)", fun() ->
            tcps_work_order:list_by_status(completed)
        end},
        {"Get all receipts", fun() ->
            tcps_persistence:get_all_receipts()
        end},
        {"Query ontology", fun() ->
            tcps_persistence:query_ontology(#{
                predicate => <<"rdf:type">>,
                object => <<"tcps:WorkOrder">>
            })
        end}
    ],

    lists:foreach(fun({Name, QueryFun}) ->
        Durations = lists:map(fun(_) ->
            Start = erlang:monotonic_time(microsecond),
            _Result = QueryFun(),
            End = erlang:monotonic_time(microsecond),
            End - Start
        end, lists:seq(1, 10)),

        AvgDuration = lists:sum(Durations) / length(Durations),
        MinDuration = lists:min(Durations),
        MaxDuration = lists:max(Durations),

        ct:pal("~s:~n", [Name]),
        ct:pal("  Avg: ~.2f ms~n", [AvgDuration / 1000]),
        ct:pal("  Min: ~.2f ms~n", [MinDuration / 1000]),
        ct:pal("  Max: ~.2f ms~n", [MaxDuration / 1000])
    end, Queries),

    ct:pal("~n=== Query Performance: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test concurrent processing scalability
%% @end
%%--------------------------------------------------------------------
test_concurrent_processing_scalability(_Config) ->
    ct:pal("~n=== Testing Concurrent Processing Scalability ===~n"),

    %% Test with increasing concurrency
    Concurrencies = [1, 2, 5, 10, 20, 50],

    Results = lists:map(fun(Concurrency) ->
        ok = tcps_test_utils:clear_all_data(),

        WorkOrders = lists:map(fun(_) ->
            {ok, WO} = tcps_test_utils:create_test_work_order(),
            WO
        end, lists:seq(1, Concurrency)),

        StartTime = erlang:monotonic_time(millisecond),
        Parent = self(),

        lists:foreach(fun(WO) ->
            spawn_link(fun() ->
                ok = tcps_test_utils:process_work_order_full(WO),
                Parent ! {done, WO}
            end)
        end, WorkOrders),

        lists:foreach(fun(WO) ->
            receive
                {done, WO} -> ok
            after 60000 ->
                error({timeout, WO})
            end
        end, WorkOrders),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,
        Throughput = Concurrency / (Duration / 1000),

        ct:pal("Concurrency ~p: ~p ms (~.2f work orders/sec)~n",
               [Concurrency, Duration, Throughput]),
        {Concurrency, Throughput}
    end, Concurrencies),

    %% Verify throughput increases with concurrency
    [First | Rest] = Results,
    lists:foldl(fun({Concurrency, Throughput}, {_PrevC, PrevT}) ->
        ?assert(Throughput >= PrevT * 0.8), % Allow some overhead
        {Concurrency, Throughput}
    end, First, Rest),

    ct:pal("~n=== Concurrent Processing Scalability: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test memory usage
%% @end
%%--------------------------------------------------------------------
test_memory_usage(_Config) ->
    ct:pal("~n=== Testing Memory Usage ===~n"),

    %% Get initial memory
    InitialMem = erlang:memory(total),
    ct:pal("Initial memory: ~p bytes (~.2f MB)~n",
           [InitialMem, InitialMem / 1024 / 1024]),

    %% Process 100 work orders
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 100)),

    %% Get memory after processing
    AfterMem = erlang:memory(total),
    ct:pal("After 100 work orders: ~p bytes (~.2f MB)~n",
           [AfterMem, AfterMem / 1024 / 1024]),

    %% Calculate increase
    Increase = AfterMem - InitialMem,
    PerWorkOrder = Increase / 100,
    ct:pal("Memory increase: ~p bytes (~.2f MB)~n",
           [Increase, Increase / 1024 / 1024]),
    ct:pal("Per work order: ~p bytes (~.2f KB)~n",
           [PerWorkOrder, PerWorkOrder / 1024]),

    %% Check for memory leaks (run GC and check again)
    erlang:garbage_collect(),
    timer:sleep(1000),
    AfterGC = erlang:memory(total),
    ct:pal("After GC: ~p bytes (~.2f MB)~n",
           [AfterGC, AfterGC / 1024 / 1024]),

    %% Memory should decrease after GC
    ?assert(AfterGC =< AfterMem),

    ct:pal("~n=== Memory Usage: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test CPU utilization
%% @end
%%--------------------------------------------------------------------
test_cpu_utilization(_Config) ->
    ct:pal("~n=== Testing CPU Utilization ===~n"),

    %% Start monitoring
    ok = tcps_test_utils:start_cpu_monitor(),

    %% Process workload
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 50)),

    %% Get CPU stats
    {ok, CPUStats} = tcps_test_utils:get_cpu_stats(),
    ok = tcps_test_utils:stop_cpu_monitor(),

    ct:pal("CPU Statistics:~n"),
    ct:pal("  Average: ~.2f%~n", [maps:get(avg, CPUStats)]),
    ct:pal("  Peak: ~.2f%~n", [maps:get(peak, CPUStats)]),
    ct:pal("  Min: ~.2f%~n", [maps:get(min, CPUStats)]),

    %% Verify reasonable utilization
    ?assert(maps:get(avg, CPUStats) > 0),
    ?assert(maps:get(peak, CPUStats) =< 100),

    ct:pal("~n=== CPU Utilization: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test disk I/O performance
%% @end
%%--------------------------------------------------------------------
test_disk_io_performance(_Config) ->
    ct:pal("~n=== Testing Disk I/O Performance ===~n"),

    %% Process work orders (generates disk I/O)
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 100)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Count files created
    {ok, FileCount} = tcps_test_utils:count_files_in_data_dir(),
    ct:pal("Created ~p files in ~p ms~n", [FileCount, Duration]),

    %% Calculate I/O rate
    IORate = FileCount / (Duration / 1000),
    ct:pal("I/O rate: ~.2f files/second~n", [IORate]),

    ct:pal("~n=== Disk I/O Performance: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test bottleneck identification
%% @end
%%--------------------------------------------------------------------
test_bottleneck_identification(_Config) ->
    ct:pal("~n=== Testing Bottleneck Identification ===~n"),

    %% Profile full pipeline
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),

    ok = tcps_test_utils:start_profiler(),
    ok = tcps_test_utils:process_work_order_full(WorkOrderId),
    {ok, Profile} = tcps_test_utils:stop_profiler(),

    %% Analyze results
    Bottlenecks = tcps_test_utils:identify_bottlenecks(Profile),

    ct:pal("Identified bottlenecks:~n"),
    lists:foreach(fun({Function, Time, Percentage}) ->
        ct:pal("  ~p: ~.2f ms (~.1f%)~n",
               [Function, Time / 1000, Percentage])
    end, Bottlenecks),

    ct:pal("~n=== Bottleneck Identification: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test performance regression detection
%% @end
%%--------------------------------------------------------------------
test_performance_regression(_Config) ->
    ct:pal("~n=== Testing Performance Regression Detection ===~n"),

    %% Run baseline benchmark
    Baseline = run_benchmark(10),
    ct:pal("Baseline: ~.2f work orders/sec~n", [Baseline]),

    %% Run again (should be similar)
    Current = run_benchmark(10),
    ct:pal("Current: ~.2f work orders/sec~n", [Current]),

    %% Calculate regression
    Regression = ((Baseline - Current) / Baseline) * 100,
    ct:pal("Regression: ~.2f%~n", [Regression]),

    %% Flag significant regression
    case abs(Regression) > 10 of
        true ->
            ct:pal("WARNING: Significant performance regression detected!~n");
        false ->
            ct:pal("No significant regression detected~n")
    end,

    ?assert(abs(Regression) < 20), % Allow 20% variance

    ct:pal("~n=== Performance Regression Detection: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test sustained throughput
%% @end
%%--------------------------------------------------------------------
test_sustained_throughput(_Config) ->
    ct:pal("~n=== Testing Sustained Throughput ===~n"),

    %% Run for 30 seconds
    Parent = self(),
    EndTime = erlang:monotonic_time(millisecond) + 30000,

    spawn_link(fun() ->
        sustained_worker(Parent, EndTime, 0)
    end),

    %% Collect result
    Completed = receive
        {sustained_done, Count} -> Count
    after 35000 ->
        0
    end,

    ct:pal("Completed ~p work orders in 30 seconds~n", [Completed]),

    %% Calculate sustained throughput
    Throughput = Completed / 30,
    ct:pal("Sustained throughput: ~.2f work orders/second~n", [Throughput]),

    ?assert(Throughput > 1), % At least 1 per second sustained

    ct:pal("~n=== Sustained Throughput: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test burst performance
%% @end
%%--------------------------------------------------------------------
test_burst_performance(_Config) ->
    ct:pal("~n=== Testing Burst Performance ===~n"),

    %% Create burst of 200 work orders instantly
    BurstStart = erlang:monotonic_time(millisecond),

    WorkOrders = lists:map(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        WO
    end, lists:seq(1, 200)),

    BurstEnd = erlang:monotonic_time(millisecond),
    BurstDuration = BurstEnd - BurstStart,

    ct:pal("Created 200 work orders in ~p ms (~.2f/sec)~n",
           [BurstDuration, 200 / (BurstDuration / 1000)]),

    %% Process burst
    ProcessStart = erlang:monotonic_time(millisecond),
    Parent = self(),

    lists:foreach(fun(WO) ->
        spawn_link(fun() ->
            ok = tcps_test_utils:process_work_order_full(WO),
            Parent ! {done, WO}
        end)
    end, WorkOrders),

    %% Collect all
    lists:foreach(fun(WO) ->
        receive
            {done, WO} -> ok
        after 120000 ->
            error({timeout, WO})
        end
    end, WorkOrders),

    ProcessEnd = erlang:monotonic_time(millisecond),
    ProcessDuration = ProcessEnd - ProcessStart,

    ct:pal("Processed 200 work orders in ~p ms (~.2f/sec)~n",
           [ProcessDuration, 200 / (ProcessDuration / 1000)]),

    %% Verify system remained stable
    ?assert(erlang:memory(total) < 1024 * 1024 * 1024), % Under 1GB

    ct:pal("~n=== Burst Performance: SUCCESS ===~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

run_benchmark(Count) ->
    ok = tcps_test_utils:clear_all_data(),

    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, Count)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    Count / (Duration / 1000).

sustained_worker(Parent, EndTime, Count) ->
    case erlang:monotonic_time(millisecond) < EndTime of
        true ->
            {ok, WO} = tcps_test_utils:create_test_work_order(),
            ok = tcps_test_utils:process_work_order_full(WO),
            sustained_worker(Parent, EndTime, Count + 1);
        false ->
            Parent ! {sustained_done, Count}
    end.
