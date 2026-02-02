%%%-------------------------------------------------------------------
%%% @doc erlmcp_regression_SUITE - OTP 28 Performance Regression Tests
%%%
%%% Common Test suite for automated performance regression detection
%%% specifically for OTP 28 optimizations.
%%%
%%% == Features ==
%%% - Baseline tracking: Load/store baseline metrics from JSON files
%%% - Regression detection: Alert on >10% performance degradation
%%% - Historical comparison: Compare to previous N runs with trend analysis
%%% - Performance gates: Pass/fail based on configurable thresholds
%%% - CI/CD integration: Report generation suitable for automated pipelines
%%%
%%% == Usage ==
%%% ```bash
%%% rebar3 ct --suite=erlmcp_regression_SUITE
%%% rebar3 ct --suite=erlmcp_regression_SUITE --group=otp28_features
%%% rebar3 ct --suite=erlmcp_regression_SUITE --var=baseline_path=/path/to/baseline.json
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_regression_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT exports
-export([
    all/0,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_baseline_establishment/1,
    test_json_performance_regression/1,
    test_priority_messages_regression/1,
    test_hibernate_regression/1,
    test_process_iterator_regression/1,
    test_pcre2_regression/1,
    test_tagged_monitors_regression/1,
    test_full_regression_suite/1,
    test_performance_gates/1,
    test_baseline_comparison/1
]).

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

suite() ->
    [
        {timetrap, {seconds, 600}},  % 10 minute timeout
        {require, baseline_path},
        {default_config, #{
            baseline_path => "./bench/baseline/otp28_baseline.json",
            regression_threshold => 10.0
        }}
    ].

all() ->
    [
        test_baseline_establishment,
        test_json_performance_regression,
        test_priority_messages_regression,
        test_hibernate_regression,
        test_process_iterator_regression,
        test_pcre2_regression,
        test_tagged_monitors_regression,
        test_full_regression_suite,
        test_performance_gates,
        test_baseline_comparison
    ].

groups() ->
    [
        {otp28_features, [], [
            test_json_performance_regression,
            test_priority_messages_regression,
            test_hibernate_regression,
            test_process_iterator_regression,
            test_pcre2_regression,
            test_tagged_monitors_regression
        ]},
        {baseline, [], [
            test_baseline_establishment,
            test_baseline_comparison
        ]},
        {regression, [], [
            test_full_regression_suite,
            test_performance_gates
        ]}
    ].

%%%====================================================================
%%% Setup/Teardown
%%%====================================================================

init_per_suite(Config) ->
    ct:log("Starting OTP 28 Performance Regression Suite"),
    ct:log("OTP Release: ~s", [erlang:system_info(otp_release)]),
    ct:log("ERTS Version: ~s", [erlang:system_info(version)]),

    %% Ensure required applications are started
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(gproc),

    %% Initialize baseline storage directory
    BaselineDir = "./bench/baseline",
    ok = filelib:ensure_dir(BaselineDir ++ "/"),

    %% Store in config
    [{baseline_dir, BaselineDir} | Config].

end_per_suite(_Config) ->
    ct:log("Ending OTP 28 Performance Regression Suite"),
    ok.

init_per_group(otp28_features, Config) ->
    ct:log("Initializing OTP 28 Features regression tests"),
    Config;
init_per_group(baseline, Config) ->
    ct:log("Initializing Baseline tests"),
    Config;
init_per_group(regression, Config) ->
    ct:log("Initializing Regression tests"),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:log("Ending test case: ~p", [TestCase]),
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test baseline establishment
test_baseline_establishment(Config) ->
    ct:log("Testing baseline establishment"),

    %% Establish baseline
    {ok, Baseline} = erlmcp_bench_regression:establish_baseline(),

    %% Verify baseline structure
    ?assert(maps:is_key(<<"otp_release">>, Baseline)),
    ?assert(maps:is_key(<<"erts_version">>, Baseline)),
    ?assert(maps:is_key(<<"timestamp">>, Baseline)),
    ?assert(maps:is_key(<<"benchmarks">>, Baseline)),
    ?assert(maps:is_key(<<"environment">>, Baseline)),

    %% Verify benchmarks exist
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    ?assert(maps:is_key(<<"native_json">>, Benchmarks)),
    ?assert(maps:is_key(<<"jsx_json">>, Benchmarks)),
    ?assert(maps:is_key(<<"priority_messages">>, Benchmarks)),
    ?assert(maps:is_key(<<"hibernate">>, Benchmarks)),
    ?assert(maps:is_key(<<"process_iterator">>, Benchmarks)),
    ?assert(maps:is_key(<<"pcre2_regex">>, Benchmarks)),
    ?assert(maps:is_key(<<"tagged_monitors">>, Benchmarks)),

    %% Verify summary
    ?assert(maps:is_key(<<"summary">>, Baseline)),

    ct:log("Baseline established successfully"),
    ok.

%% @doc Test JSON performance regression detection
test_json_performance_regression(Config) ->
    ct:log("Testing JSON performance regression"),

    %% Load or create baseline
    BaselineFile = proplists:get_value(baseline_path, Config, 
                                       "./bench/baseline/otp28_baseline.json"),

    Baseline = case file:read_file(BaselineFile) of
        {ok, Content} ->
            jsx:decode(Content, [return_maps]);
        {error, _} ->
            %% Create baseline
            {ok, NewBaseline} = erlmcp_bench_regression:establish_baseline(),
            NewBaseline
    end,

    %% Run current benchmarks
    Current = erlmcp_bench_regression:bench_native_json(),
    JsxCurrent = erlmcp_bench_regression:bench_jsx_json(),

    %% Compare
    BaselineBenchmarks = maps:get(<<"benchmarks">>, Baseline),
    BaselineNative = maps:get(<<"native_json">>, BaselineBenchmarks),

    %% Decode throughput comparison
    CurrentDecodeThroughput = maps:get(decode_throughput, Current),
    BaselineDecodeThroughput = maps:get(<<"decode_throughput">>, BaselineNative),

    DecodeRegression = 
        ((CurrentDecodeThroughput - BaselineDecodeThroughput) / BaselineDecodeThroughput) * 100,

    ct:log("JSON decode throughput regression: ~.2f%", [DecodeRegression]),

    %% Check threshold (10%)
    Threshold = proplists:get_value(regression_threshold, Config, 10.0),
    
    case DecodeRegression < -Threshold of
        true ->
            ct:log("FAIL: JSON decode regression > ~p%", [Threshold]),
            ?assert(false, {json_decode_regression, DecodeRegression});
        false ->
            ct:log("PASS: JSON decode within ~p% threshold", [Threshold]),
            ok
    end.

%% @doc Test priority messages regression
test_priority_messages_regression(Config) ->
    ct:log("Testing priority messages regression"),

    %% Run benchmark
    Result = erlmcp_bench_regression:bench_priority_messages(),

    %% Check availability
    Available = maps:get(available, Result),
    
    case Available of
        true ->
            PriorityLatency = maps:get(priority_latency_us, Result),
            NormalLatency = maps:get(normal_latency_us, Result),

            ct:log("Normal message latency: ~.2f us", [NormalLatency]),
            ct:log("Priority message latency: ~.2f us", [PriorityLatency]),

            %% Priority should be faster or equal
            ?assert(PriorityLatency =< NormalLatency * 1.1),

            %% Check for reasonable latency (< 100us)
            ?assert(PriorityLatency < 100.0),
            
            ct:log("PASS: Priority messages working correctly");
        false ->
            ct:log("SKIP: Priority messages not available (OTP < 28)"),
            ok
    end.

%% @doc Test hibernate regression
test_hibernate_regression(Config) ->
    ct:log("Testing hibernate regression"),

    %% Run benchmark
    Result = erlmcp_bench_regression:bench_hibernate(),

    MemoryReduction = maps:get(memory_reduction_percent, Result),
    NormalMemory = maps:get(normal_bytes_per_process, Result),
    HibernateMemory = maps:get(hibernate_bytes_per_process, Result),

    ct:log("Normal memory/process: ~.2f KB", [NormalMemory / 1024]),
    ct:log("Hibernate memory/process: ~.2f KB", [HibernateMemory / 1024]),
    ct:log("Memory reduction: ~.1f%", [MemoryReduction]),

    %% Expect at least 50% reduction
    ?assert(MemoryReduction >= 50.0),

    %% Load baseline if available
    BaselineFile = proplists:get_value(baseline_path, Config,
                                       "./bench/baseline/otp28_baseline.json"),

    case file:read_file(BaselineFile) of
        {ok, Content} ->
            Baseline = jsx:decode(Content, [return_maps]),
            BaselineBenchmarks = maps:get(<<"benchmarks">>, Baseline),
            BaselineHibernate = maps:get(<<"hibernate">>, BaselineBenchmarks),

            BaselineReduction = maps:get(<<"memory_reduction_percent">>, BaselineHibernate),

            Regression = BaselineReduction - MemoryReduction,

            ct:log("Hibernate regression: ~.2f%", [Regression]),

            Threshold = proplists:get_value(regression_threshold, Config, 10.0),

            case Regression > Threshold of
                true ->
                    ct:log("FAIL: Hibernate regression > ~p%", [Threshold]),
                    ?assert(false, {hibernate_regression, Regression});
                false ->
                    ct:log("PASS: Hibernate within ~p% threshold", [Threshold]),
                    ok
            end;
        {error, _} ->
            ct:log("SKIP: No baseline available for comparison"),
            ok
    end.

%% @doc Test process iterator regression
test_process_iterator_regression(Config) ->
    ct:log("Testing process iterator regression"),

    %% Run benchmark
    Result = erlmcp_bench_regression:bench_process_iterator(),

    %% Check availability
    Available = maps:get(available, Result),

    case Available of
        true ->
            OldTime = maps:get(old_iterator_time_us, Result),
            NewTime = maps:get(new_iterator_time_us, Result),
            Improvement = maps:get(memory_improvement, Result),

            ct:log("erlang:processes/0 time: ~.2f us", [OldTime]),
            ct:log("Process iterator time: ~.2f us", [NewTime]),
            ct:log("Memory improvement: ~.2fx", [Improvement]),

            %% New iterator should use less memory (time as proxy)
            ?assert(Improvement >= 1.0),
            
            ct:log("PASS: Process iterator working correctly");
        false ->
            ct:log("SKIP: Process iterator not available"),
            ok
    end.

%% @doc Test PCRE2 regex regression
test_pcre2_regression(Config) ->
    ct:log("Testing PCRE2 regex regression"),

    %% Run benchmark
    Result = erlmcp_bench_regression:bench_pcre2_regex(),

    SimpleLatency = maps:get(simple_latency_us, Result),
    ComplexLatency = maps:get(complex_latency_us, Result),

    ct:log("Simple regex latency: ~.2f us", [SimpleLatency]),
    ct:log("Complex regex latency: ~.2f us", [ComplexLatency]),

    %% Complex patterns should take longer but still reasonable
    ?assert(SimpleLatency > 0),
    ?assert(ComplexLatency > 0),

    %% Complex should not be more than 10x slower
    ?assert(ComplexLatency < SimpleLatency * 10),

    ct:log("PASS: PCRE2 regex performance acceptable"),
    ok.

%% @doc Test tagged monitors regression
test_tagged_monitors_regression(Config) ->
    ct:log("Testing tagged monitors regression"),

    %% Run benchmark
    Result = erlmcp_bench_regression:bench_tagged_monitors(),

    %% Check availability
    Available = maps:get(available, Result),

    case Available of
        true ->
            NormalLatency = maps:get(normal_latency_us, Result),
            TaggedLatency = maps:get(tagged_latency_us, Result),
            Overhead = maps:get(overhead_percent, Result),

            ct:log("Normal monitor latency: ~.2f us", [NormalLatency]),
            ct:log("Tagged monitor latency: ~.2f us", [TaggedLatency]),
            ct:log("Overhead: ~.1f%", [Overhead]),

            %% Tagged monitors should have reasonable overhead (< 50%)
            ?assert(Overhead < 50.0),

            ct:log("PASS: Tagged monitors overhead acceptable");
        false ->
            ct:log("SKIP: Tagged monitors not available (OTP < 28)"),
            ok
    end.

%% @doc Test full regression suite
test_full_regression_suite(Config) ->
    ct:log("Running full regression suite"),

    %% Establish baseline
    {ok, Current} = erlmcp_bench_regression:establish_baseline(),

    %% Compare with baseline
    Comparison = erlmcp_bench_regression:compare_with_baseline(Current),

    ct:log("Comparison result: ~p", [Comparison]),

    %% Check for critical regressions
    case maps:get(overall, Comparison) of
        regression ->
            ct:log("FAIL: Overall regression detected"),
            ?assert(false, {overall_regression, Comparison});
        _ ->
            ct:log("PASS: No critical regressions"),
            ok
    end.

%% @doc Test performance gates
test_performance_gates(Config) ->
    ct:log("Testing performance gates"),

    %% Establish baseline
    {ok, Baseline} = erlmcp_bench_regression:establish_baseline(),

    %% Get benchmarks
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),

    %% Check JSON performance gate
    NativeJson = maps:get(<<"native_json">>, Benchmarks),
    JsxJson = maps:get(<<"jsx_json">>, Benchmarks),

    JsonDecodeSpeedup = 
        maps:get(<<"decode_throughput">>, NativeJson) / 
        maps:get(<<"decode_throughput">>, JsxJson),

    ct:log("JSON decode speedup: ~.2fx", [JsonDecodeSpeedup]),

    %% Native should be at least 1.5x faster than jsx
    case JsonDecodeSpeedup of
        S when S >= 1.5 ->
            ct:log("PASS: JSON performance gate (>= 1.5x)");
        S ->
            ct:log("FAIL: JSON performance gate (< 1.5x: ~.2fx)", [S]),
            ?assert(false, {json_performance_gate_failed, S})
    end,

    %% Check hibernate gate
    Hibernate = maps:get(<<"hibernate">>, Benchmarks),
    MemoryReduction = maps:get(<<"memory_reduction_percent">>, Hibernate),

    ct:log("Hibernate memory reduction: ~.1f%", [MemoryReduction]),

    %% Should be at least 50% reduction
    case MemoryReduction of
        R when R >= 50.0 ->
            ct:log("PASS: Hibernate gate (>= 50%)");
        R ->
            ct:log("FAIL: Hibernate gate (< 50%: ~.1f%)", [R]),
            ?assert(false, {hibernate_gate_failed, R})
    end.

%% @doc Test baseline comparison
test_baseline_comparison(Config) ->
    ct:log("Testing baseline comparison"),

    %% Create current results
    {ok, Current} = erlmcp_bench_regression:establish_baseline(),

    %% Save current as baseline
    BaselineDir = proplists:get_value(baseline_dir, Config),
    TempBaseline = filename:join([BaselineDir, "temp_baseline.json"]),

    ok = erlmcp_bench_regression:save_baseline(Current, TempBaseline),

    %% Load and compare
    {ok, Content} = file:read_file(TempBaseline),
    Baseline = jsx:decode(Content, [return_maps]),

    %% Verify structure
    ?assertEqual(maps:get(<<"otp_release">>, Current),
                 maps:get(<<"otp_release">>, Baseline)),
    ?assertEqual(maps:get(<<"benchmarks">>, Current),
                 maps:get(<<"benchmarks">>, Baseline)),

    %% Cleanup
    ok = file:delete(TempBaseline),

    ct:log("PASS: Baseline comparison successful"),
    ok.
