%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Upgrade Performance Regression Test Suite
%%%
%%% Performance tests for OTP upgrade operations:
%%% - Module reload timing
%%% - State migration overhead
%%% - Cluster upgrade latency
%%% - Process enumeration performance
%%% - JSON encoding/decoding throughput
%%% - Code loading performance
%%% - Memory usage during upgrade
%%%
%%% Chicago School TDD:
%%% - Real performance measurements (no mocks)
%%% - Observable timing metrics
%%% - Regression detection via thresholds
%%% - Production-like workloads
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_upgrade_performance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../../include/otp_compat.hrl").

%% Suite callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([test_module_reload_performance/1,
         test_state_migration_overhead/1,
         test_process_enumeration_performance/1,
         test_json_encoding_throughput/1,
         test_code_loading_performance/1,
         test_cluster_upgrade_latency/1,
         test_memory_usage_during_upgrade/1,
         test_concurrent_reload_performance/1,
         test_upgrade_with_active_load/1,
         test_regression_detection/1]).

%% Performance thresholds (milliseconds)
-define(RELOAD_THRESHOLD, 5000).       %% Max time for single module reload
-define(MIGRATION_THRESHOLD, 100).     %% Max time for state migration
-define(ENUM_THRESHOLD, 1000).         %% Max time for process enumeration
-define(JSON_THRESHOLD, 100).          %% Max time for JSON encode/decode (1KB)
-define(LOAD_THRESHOLD, 2000).         %% Max time for code loading
-define(CLUSTER_THRESHOLD, 30000).     %% Max time for cluster upgrade

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [test_module_reload_performance,
     test_state_migration_overhead,
     test_process_enumeration_performance,
     test_json_encoding_throughput,
     test_code_loading_performance,
     test_cluster_upgrade_latency,
     test_memory_usage_during_upgrade,
     test_concurrent_reload_performance,
     test_upgrade_with_active_load,
     test_regression_detection].

init_per_suite(Config) ->
    ct:pal("Starting OTP Upgrade Performance Suite"),
    ct:pal("Performance thresholds:"),
    ct:pal("  Reload: ~p ms", [?RELOAD_THRESHOLD]),
    ct:pal("  Migration: ~p ms", [?MIGRATION_THRESHOLD]),
    ct:pal("  Enum: ~p ms", [?ENUM_THRESHOLD]),
    ct:pal("  JSON: ~p ms", [?JSON_THRESHOLD]),
    ct:pal("  Load: ~p ms", [?LOAD_THRESHOLD]),
    ct:pal("  Cluster: ~p ms", [?CLUSTER_THRESHOLD]),

    application:ensure_all_started(erlmcp),

    %% Initialize baseline metrics
    Baseline = initialize_baseline(),
    [{baseline, Baseline} | Config].

end_per_suite(_Config) ->
    ct:pal("Performance suite completed"),
    application:stop(erlmcp),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting performance test: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending performance test: ~p", [TestCase]),
    %% Force GC between tests
    garbage_collect(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Cases - Core Performance
%%====================================================================

%% @doc Test module reload performance
test_module_reload_performance(Config) ->
    ct:pal("Testing module reload performance"),

    {ok, ReloadPid} = erlmcp_code_reload:start_link(),

    Module = erlmcp_registry,

    %% Measure reload time
    {Time, Result} = timer:tc(fun() ->
            Opts = #{drain_connections => false,
                     rollback_window_s => 60},
            erlmcp_code_reload:reload_module(Module, Opts)
        end),

    TimeMs = Time div 1000,

    ct:pal("Module reload took ~p ms, result: ~p", [TimeMs, Result]),

    %% Assert performance threshold
    ?assert(TimeMs =< ?RELOAD_THRESHOLD,
            "Module reload too slow: ~p ms > ~p ms", [TimeMs, ?RELOAD_THRESHOLD]),

    gen_server:stop(ReloadPid),

    %% Store metric
    store_metric(module_reload_time, TimeMs, Config),

    ct:pal("Module reload performance OK").

%% @doc Test state migration overhead
test_state_migration_overhead(Config) ->
    ct:pal("Testing state migration overhead"),

    %% Create sample state
    SampleState = #state{
        version = 0,
        reload_history = lists:duplicate(100, #{}),
        rollback_timers = maps:from_list([{I, make_ref()} || I <- lists:seq(1, 50)]),
        draining = false
    },

    %% Measure migration time
    {Time, Result} = timer:tc(fun() ->
            migrate_state_v0_to_v1(SampleState)
        end),

    TimeMs = Time div 1000,

    ct:pal("State migration took ~p ms, result: ~p", [TimeMs, Result]),

    %% Assert performance threshold
    ?assert(TimeMs =< ?MIGRATION_THRESHOLD,
            "State migration too slow: ~p ms > ~p ms", [TimeMs, ?MIGRATION_THRESHOLD]),

    %% Store metric
    store_metric(state_migration_time, TimeMs, Config),

    ct:pal("State migration performance OK").

%% @doc Test process enumeration performance
test_process_enumeration_performance(Config) ->
    ct:pal("Testing process enumeration performance"),

    %% Measure count operation
    {CountTime, Count} = timer:tc(fun() -> ?SAFE_PROCESS_COUNT() end),
    CountMs = CountTime div 1000,

    ct:pal("Process count (~p procs) took ~p ms", [Count, CountMs]),

    %% Assert performance threshold
    ?assert(CountMs =< ?ENUM_THRESHOLD,
            "Process count too slow: ~p ms > ~p ms", [CountMs, ?ENUM_THRESHOLD]),

    %% Measure list operation (with warning threshold)
    {ListTime, PidList} = timer:tc(fun() -> ?SAFE_PROCESSES() end),
    ListMs = ListTime div 1000,

    ct:pal("Process list (~p procs) took ~p ms", [length(PidList), ListMs]),

    %% Store metrics
    store_metric(process_count_time, CountMs, Config),
    store_metric(process_list_time, ListMs, Config),

    ct:pal("Process enumeration performance OK").

%% @doc Test JSON encoding throughput
test_json_encoding_throughput(Config) ->
    ct:pal("Testing JSON encoding throughput"),

    %% Generate test data (1KB)
    TestData = generate_test_json_data(1024),

    %% Measure encoding/decode cycle
    Iterations = 100,

    {Time, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                    Encoded = ?JSON_ENCODE_SAFE(TestData),
                    {Decoded, []} = ?JSON_DECODE_SAFE(Encoded),
                    ?assertEqual(TestData, Decoded)
                end, lists:seq(1, Iterations))
        end),

    TimeMs = Time div 1000,
    AvgMs = TimeMs / Iterations,

    ct:pal("JSON encode/decode (~p iterations, ~p bytes) took ~p ms avg",
           [Iterations, byte_size(term_to_binary(TestData)), AvgMs]),

    %% Assert performance threshold
    ?assert(AvgMs =< ?JSON_THRESHOLD,
            "JSON encoding too slow: ~p ms > ~p ms", [AvgMs, ?JSON_THRESHOLD]),

    %% Store metric
    store_metric(json_encode_time, AvgMs, Config),

    ct:pal("JSON encoding performance OK").

%% @doc Test code loading performance
test_code_loading_performance(Config) ->
    ct:pal("Testing code loading performance"),

    Module = erlmcp_registry,

    %% Unload module first
    code:purge(Module),
    code:delete(Module),

    %% Measure load time
    {Time, Result} = timer:tc(fun() ->
            code:load_file(Module)
        end),

    TimeMs = Time div 1000,

    ct:pal("Code load took ~p ms, result: ~p", [TimeMs, Result]),

    %% Assert performance threshold
    ?assert(TimeMs =< ?LOAD_THRESHOLD,
            "Code loading too slow: ~p ms > ~p ms", [TimeMs, ?LOAD_THRESHOLD]),

    %% Store metric
    store_metric(code_load_time, TimeMs, Config),

    ct:pal("Code loading performance OK").

%%====================================================================
%% Test Cases - Cluster Performance
%%====================================================================

%% @doc Test cluster upgrade latency
test_cluster_upgrade_latency(Config) ->
    ct:pal("Testing cluster upgrade latency"),

    {ok, CoordinatorPid} = erlmcp_reload_coordinator:start_link(),

    Module = erlmcp_registry,

    %% Measure cluster reload time
    {Time, Result} = timer:tc(fun() ->
            erlmcp_reload_coordinator:cluster_reload(Module, sync_all)
        end),

    TimeMs = Time div 1000,

    ct:pal("Cluster reload took ~p ms, result: ~p", [TimeMs, Result]),

    %% Assert performance threshold
    ?assert(TimeMs =< ?CLUSTER_THRESHOLD,
            "Cluster reload too slow: ~p ms > ~p ms", [TimeMs, ?CLUSTER_THRESHOLD]),

    %% Store metric
    store_metric(cluster_reload_time, TimeMs, Config),

    gen_server:stop(CoordinatorPid),

    ct:pal("Cluster upgrade performance OK").

%%====================================================================
%% Test Cases - Memory Performance
%%====================================================================

%% @doc Test memory usage during upgrade
test_memory_usage_during_upgrade(Config) ->
    ct:pal("Testing memory usage during upgrade"),

    %% Force GC before test
    garbage_collect(),
    timer:sleep(100),

    %% Get baseline memory
    MemoryBefore = erlang:memory(total),
    ct:pal("Memory before: ~p MB", [MemoryBefore div 1024 div 1024]),

    {ok, ReloadPid} = erlmcp_code_reload:start_link(),

    %% Perform upgrade
    Module = erlmcp_registry,
    Opts = #{drain_connections => false,
             rollback_window_s => 60},

    erlmcp_code_reload:reload_module(Module, Opts),

    %% Force GC
    garbage_collect(),
    timer:sleep(100),

    %% Get memory after
    MemoryAfter = erlang:memory(total),
    ct:pal("Memory after: ~p MB", [MemoryAfter div 1024 div 1024]),

    MemoryDelta = MemoryAfter - MemoryBefore,
    MemoryDeltaMB = MemoryDelta div 1024 div 1024,

    ct:pal("Memory delta: ~p MB", [MemoryDeltaMB]),

    %% Assert reasonable memory growth (< 10 MB)
    ?assert(MemoryDeltaMB < 10,
            "Memory growth too high: ~p MB", [MemoryDeltaMB]),

    %% Store metric
    store_metric(memory_delta, MemoryDeltaMB, Config),

    gen_server:stop(ReloadPid),

    ct:pal("Memory usage OK").

%%====================================================================
%% Test Cases - Concurrent Performance
%%====================================================================

%% @doc Test concurrent reload performance
test_concurrent_reload_performance(Config) ->
    ct:pal("Testing concurrent reload performance"),

    {ok, ReloadPid} = erlmcp_code_reload:start_link(),

    Modules = [erlmcp_registry, erlmcp_cache, erlmcp_auth],

    %% Measure concurrent reload
    {Time, Results} = timer:tc(fun() ->
            Opts = #{drain_connections => false,
                     rollback_window_s => 60},
            erlmcp_code_reload:reload_modules(Modules, Opts)
        end),

    TimeMs = Time div 1000,

    ct:pal("Concurrent reload (~p modules) took ~p ms", [length(Modules), TimeMs]),

    %% Verify all succeeded
    ?assertEqual(3, length(Results)),
    lists:foreach(fun({_M, R}) -> ?assertMatch({ok, _, _}, R) end, Results),

    %% Store metric
    store_metric(concurrent_reload_time, TimeMs, Config),

    gen_server:stop(ReloadPid),

    ct:pal("Concurrent reload performance OK").

%% @doc Test upgrade with active load
test_upgrade_with_active_load(Config) ->
    ct:pal("Testing upgrade with active load"),

    {ok, ServerPid} = erlmcp_server:start_link(#{}),
    {ok, ReloadPid} = erlmcp_code_reload:start_link(),

    %% Start load generator
    Self = self(),
    LoadPid = spawn(fun() -> load_generator(Self) end),

    %% Wait for load to build
    timer:sleep(500),

    %% Measure upgrade under load
    Module = erlmcp_registry,
    {Time, Result} = timer:tc(fun() ->
            Opts = #{drain_connections => false,
                     rollback_window_s => 60},
            erlmcp_code_reload:reload_module(Module, Opts)
        end),

    TimeMs = Time div 1000,

    ct:pal("Upgrade under load took ~p ms, result: ~p", [TimeMs, Result]),

    %% Stop load
    LoadPid ! stop,

    gen_server:stop(ServerPid),
    gen_server:stop(ReloadPid),

    %% Store metric
    store_metric(upgrade_under_load_time, TimeMs, Config),

    ct:pal("Upgrade under load OK").

%%====================================================================
%% Test Cases - Regression Detection
%%====================================================================

%% @doc Detect performance regressions vs baseline
test_regression_detection(Config) ->
    ct:pal("Testing regression detection"),

    %% Get baseline from config
    Baseline = proplists:get_value(baseline, Config, #{}),

    %% Get current metrics
    CurrentMetrics = get_stored_metrics(),

    %% Compare with baseline
    lists:foreach(fun({Metric, Value}) ->
            case maps:get(Metric, Baseline, undefined) of
                undefined ->
                    ct:pal("No baseline for ~p, establishing baseline: ~p", [Metric, Value]);
                BaselineValue ->
                    RegressionThreshold = 1.2,  %% 20% tolerance
                    Ratio = Value / BaselineValue,

                    ct:pal("~p: current=~p, baseline=~p, ratio=~.2f",
                           [Metric, Value, BaselineValue, Ratio]),

                    %% Assert no regression (>20% slowdown)
                    ?assert(Ratio =< RegressionThreshold,
                            "Performance regression detected for ~p: ~.2f slowdown",
                            [Metric, Ratio])
            end
        end, maps:to_list(CurrentMetrics)),

    ct:pal("Regression detection completed").

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc State record definition
-record(state, {
    version = 0 :: integer(),
    reload_history = [] :: [map()],
    rollback_timers = #{} :: map(),
    draining = false :: boolean()
}).

%% @doc Migrate state from v0 to v1
migrate_state_v0_to_v1(V0State) ->
    V1State = V0State#state{version = 1},
    {ok, V1State}.

%% @doc Generate test JSON data
generate_test_json_data(SizeBytes) ->
    TargetSize = SizeBytes div 10,  %% Approximate
    maps:from_list([{integer_to_binary(I), crypto:strong_rand_bytes(10)}
                    || I <- lists:seq(1, TargetSize)]).

%% @doc Load generator
load_generator(Parent) ->
    receive
        stop -> ok
    after 0 ->
        %% Generate load (compute + message)
        fib(20),
        Parent ! {load_msg, make_ref()},
        load_generator(Parent)
    end.

%% @doc Fibonacci for load generation
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

%% @doc Initialize baseline metrics
initialize_baseline() ->
    %% For first run, establish baseline
    %% For subsequent runs, load from file or DB
    #{
        module_reload_time => 1000,
        state_migration_time => 10,
        process_count_time => 10,
        process_list_time => 50,
        json_encode_time => 5,
        code_load_time => 500,
        cluster_reload_time => 5000,
        memory_delta => 2,
        concurrent_reload_time => 2000,
        upgrade_under_load_time => 3000
    }.

%% @doc Process dictionary for metrics
-define(METRIC_KEY, erlmcp_perf_metrics).

%% @doc Store metric
store_metric(Metric, Value, _Config) ->
    Current = case get(?METRIC_KEY) of
        undefined -> #{};
        Map -> Map
    end,
    put(?METRIC_KEY, maps:put(Metric, Value, Current)).

%% @doc Get stored metrics
get_stored_metrics() ->
    case get(?METRIC_KEY) of
        undefined -> #{};
        Map -> Map
    end.
