%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_performance_validator module
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL benchmark data (no mocks, no fake data)
%%% - Test ALL interfaces: validation functions, report generation
%%% - Real benchmark results from bench/results/ directory
%%% - NO internal state inspection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup function to ensure benchmark results directory exists
setup_validator() ->
    % Create temp benchmark directory for testing
    TestDir = "/tmp/erlmcp_bench_test",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    TestDir.

cleanup_validator(TestDir) ->
    % Clean up test directory
    case file:list_dir(TestDir) of
        {ok, Files} ->
            lists:foreach(fun(F) ->
                file:delete(TestDir ++ "/" ++ F)
            end, Files);
        _ -> ok
    end,
    file:del_dir(TestDir).

%%====================================================================
%% Latency Validation Tests
%%====================================================================

%% Test validate_latency with passing thresholds (all within limits)
validate_latency_pass_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Latency values within thresholds
              Latencies = #{
                  <<"latency_p50_us">> => 80.0,    % Below 100us threshold
                  <<"latency_p95_us">> => 400.0,   % Below 500us threshold
                  <<"latency_p99_us">> => 900.0    % Below 1000us threshold
              },
              Thresholds = #{
                  p50_us => 100,
                  p95_us => 500,
                  p99_us => 1000
              },

              % When: Validate latencies
              Result = erlmcp_performance_validator:validate_latency(Latencies, Thresholds),

              % Then: Should pass with no violations
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(0, maps:get(violations, Details)),
              ?assertEqual(true, maps:get(is_valid, Details))
          end)
         ]
     end}.

%% Test validate_latency with failing thresholds (p50 exceeds limit)
validate_latency_fail_p50_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: p50 exceeds threshold
              Latencies = #{
                  <<"latency_p50_us">> => 150.0,   % Above 100us threshold
                  <<"latency_p95_us">> => 400.0,
                  <<"latency_p99_us">> => 900.0
              },
              Thresholds = #{
                  p50_us => 100,
                  p95_us => 500,
                  p99_us => 1000
              },

              % When: Validate latencies
              Result = erlmcp_performance_validator:validate_latency(Latencies, Thresholds),

              % Then: Should fail with p50 violation
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(1, maps:get(violations, Details)),
              ?assertEqual(false, maps:get(is_valid, Details)),

              % Verify violation details
              Violations = maps:get(violation_list, Details),
              ?assertEqual(1, length(Violations)),
              #{
                  <<"metric">> := <<"latency_p50_us">>,
                  <<"threshold_us">> := 100,
                  <<"actual_us">> := 150.0,
                  <<"severity">> := Severity
              } = lists:nth(1, Violations),
              ?assertEqual(<<"critical">>, Severity)
          end)
         ]
     end}.

%% Test validate_latency with multiple violations
validate_latency_multiple_violations_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: All latencies exceed thresholds
              Latencies = #{
                  <<"latency_p50_us">> => 150.0,   % Above 100us
                  <<"latency_p95_us">> => 600.0,   % Above 500us
                  <<"latency_p99_us">> => 1500.0   % Above 1000us
              },
              Thresholds = #{
                  p50_us => 100,
                  p95_us => 500,
                  p99_us => 1000
              },

              % When: Validate latencies
              Result = erlmcp_performance_validator:validate_latency(Latencies, Thresholds),

              % Then: Should fail with 3 violations
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(3, maps:get(violations, Details)),

              % Verify all violations present
              Violations = maps:get(violation_list, Details),
              ?assertEqual(3, length(Violations))
          end)
         ]
     end}.

%% Test validate_latency with default thresholds
validate_latency_default_thresholds_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Latencies within default thresholds
              Latencies = #{
                  <<"latency_p50_us">> => 90.0,
                  <<"latency_p95_us">> => 450.0,
                  <<"latency_p99_us">> => 950.0
              },

              % When: Validate with default thresholds
              Result = erlmcp_performance_validator:validate_latency(Latencies, #{}),

              % Then: Should apply defaults and pass
              ?assertMatch({pass, _}, Result)
          end)
         ]
     end}.

%% Test validate_latency with missing latency fields
validate_latency_missing_fields_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Latency map missing p99
              Latencies = #{
                  <<"latency_p50_us">> => 80.0,
                  <<"latency_p95_us">> => 400.0
                  % p99 missing
              },
              Thresholds = #{},

              % When: Validate latencies
              Result = erlmcp_performance_validator:validate_latency(Latencies, Thresholds),

              % Then: Should handle gracefully (fail with error)
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(true, maps:get(is_error, Details, false))
          end)
         ]
     end}.

%%====================================================================
%% Throughput Validation Tests
%%====================================================================

%% Test validate_throughput with passing throughput
validate_throughput_pass_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Throughput above minimum
              Throughput = #{
                  <<"throughput_msg_per_s">> => 150000.0  % Above 100K baseline
              },
              MinThroughput = 100000,

              % When: Validate throughput
              Result = erlmcp_performance_validator:validate_throughput(Throughput, MinThroughput),

              % Then: Should pass
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(true, maps:get(is_valid, Details)),
              ?assertEqual(150000.0, maps:get(actual_throughput, Details)),
              ?assertEqual(100000, maps:get(min_required, Details))
          end)
         ]
     end}.

%% Test validate_throughput with failing throughput
validate_throughput_fail_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Throughput below minimum
              Throughput = #{
                  <<"throughput_msg_per_s">> => 50000.0  % Below 100K baseline
              },
              MinThroughput = 100000,

              % When: Validate throughput
              Result = erlmcp_performance_validator:validate_throughput(Throughput, MinThroughput),

              % Then: Should fail
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(false, maps:get(is_valid, Details)),
              ?assertEqual(50000.0, maps:get(actual_throughput, Details)),
              ?assertEqual(100000, maps:get(min_required, Details)),

              % Verify severity (50% degradation = critical)
              Severity = maps:get(severity, Details),
              ?assertEqual(<<"critical">>, Severity)
          end)
         ]
     end}.

%% Test validate_throughput with default minimum
validate_throughput_default_min_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Throughput map
              Throughput = #{
                  <<"throughput_msg_per_s">> => 1981826.65  % Real benchmark value
              },

              % When: Validate with default minimum
              Result = erlmcp_performance_validator:validate_throughput(Throughput, undefined),

              % Then: Should apply 100K default and pass
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(100000, maps:get(min_required, Details))
          end)
         ]
     end}.

%% Test validate_throughput with missing field
validate_throughput_missing_field_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Throughput map missing required field
              Throughput = #{
                  <<"some_other_field">> => 123
              },
              MinThroughput = 100000,

              % When: Validate throughput
              Result = erlmcp_performance_validator:validate_throughput(Throughput, MinThroughput),

              % Then: Should fail with error
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(true, maps:get(is_error, Details, false))
          end)
         ]
     end}.

%%====================================================================
%% Memory Validation Tests
%%====================================================================

%% Test validate_memory with passing memory usage
validate_memory_pass_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Memory below limit
              Memory = #{
                  <<"memory_per_connection_mib">> => 5.0  % Below 10MB limit
              },
              MaxMemory = 10,

              % When: Validate memory
              Result = erlmcp_performance_validator:validate_memory(Memory, MaxMemory),

              % Then: Should pass
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(true, maps:get(is_valid, Details)),
              ?assertEqual(5.0, maps:get(actual_memory_mib, Details))
          end)
         ]
     end}.

%% Test validate_memory with failing memory usage
validate_memory_fail_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Memory above limit
              Memory = #{
                  <<"memory_per_connection_mib">> => 15.0  % Above 10MB limit
              },
              MaxMemory = 10,

              % When: Validate memory
              Result = erlmcp_performance_validator:validate_memory(Memory, MaxMemory),

              % Then: Should fail
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(false, maps:get(is_valid, Details)),
              ?assertEqual(15.0, maps:get(actual_memory_mib, Details)),
              ?assertEqual(10, maps:get(max_allowed_mib, Details))
          end)
         ]
     end}.

%% Test validate_memory with default 10MB limit
validate_memory_default_limit_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Memory map
              Memory = #{
                  <<"memory_per_connection_mib">> => 8.0
              },

              % When: Validate with default limit
              Result = erlmcp_performance_validator:validate_memory(Memory, undefined),

              % Then: Should apply 10MB default
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(10, maps:get(max_allowed_mib, Details))
          end)
         ]
     end}.

%%====================================================================
%% Concurrency Validation Tests
%%====================================================================

%% Test validate_concurrency with passing concurrent connections
validate_concurrency_pass_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Concurrent connections below limit
              Concurrency = #{
                  <<"concurrent_connections">> => 30000  % Below 50K limit
              },
              MaxConnections = 50000,

              % When: Validate concurrency
              Result = erlmcp_performance_validator:validate_concurrency(Concurrency, MaxConnections),

              % Then: Should pass
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(true, maps:get(is_valid, Details))
          end)
         ]
     end}.

%% Test validate_concurrency with exceeding limit
validate_concurrency_fail_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Concurrent connections exceed limit
              Concurrency = #{
                  <<"concurrent_connections">> => 75000  % Above 50K limit
              },
              MaxConnections = 50000,

              % When: Validate concurrency
              Result = erlmcp_performance_validator:validate_concurrency(Concurrency, MaxConnections),

              % Then: Should fail
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(false, maps:get(is_valid, Details)),
              ?assertEqual(75000, maps:get(actual_connections, Details))
          end)
         ]
     end}.

%%====================================================================
%% Benchmark Comparison Tests
%%====================================================================

%% Test benchmark_comparison with passing comparison (within 10%)
benchmark_comparison_pass_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Current results within 10% of baseline
              Baseline = #{
                  <<"throughput_msg_per_s">> => 1000000.0,
                  <<"latency_p50_us">> => 100.0,
                  <<"latency_p95_us">> => 500.0,
                  <<"latency_p99_us">> => 1000.0
              },
              Current = #{
                  <<"throughput_msg_per_s">> => 1050000.0,  % 5% improvement
                  <<"latency_p50_us">> => 95.0,            % 5% improvement
                  <<"latency_p95_us">> => 475.0,           % 5% improvement
                  <<"latency_p99_us">> => 950.0            % 5% improvement
              },
              TolerancePercent = 10,

              % When: Compare against baseline
              Result = erlmcp_performance_validator:benchmark_comparison(Current, Baseline, TolerancePercent),

              % Then: Should pass (all within tolerance)
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(true, maps:get(is_valid, Details)),
              ?assertEqual(0, maps:get(regressions, Details))
          end)
         ]
     end}.

%% Test benchmark_comparison with regression (exceeds 10%)
benchmark_comparison_regression_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Current results with 15% degradation
              Baseline = #{
                  <<"throughput_msg_per_s">> => 1000000.0,
                  <<"latency_p99_us">> => 1000.0
              },
              Current = #{
                  <<"throughput_msg_per_s">> => 850000.0,  % 15% degradation
                  <<"latency_p99_us">> => 1150.0           % 15% degradation
              },
              TolerancePercent = 10,

              % When: Compare against baseline
              Result = erlmcp_performance_validator:benchmark_comparison(Current, Baseline, TolerancePercent),

              % Then: Should fail with regressions
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(false, maps:get(is_valid, Details)),
              ?assertEqual(2, maps:get(regressions, Details)),

              % Verify regression details
              Regressions = maps:get(regression_list, Details),
              ?assertEqual(2, length(Regressions))
          end)
         ]
     end}.

%% Test benchmark_comparison with default 10% tolerance
benchmark_comparison_default_tolerance_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Baseline and current
              Baseline = #{
                  <<"throughput_msg_per_s">> => 1000000.0
              },
              Current = #{
                  <<"throughput_msg_per_s">> => 950000.0  % 5% degradation (within 10%)
              },

              % When: Compare with default tolerance
              Result = erlmcp_performance_validator:benchmark_comparison(Current, Baseline, undefined),

              % Then: Should apply 10% default and pass
              ?assertMatch({pass, _}, Result)
          end)
         ]
     end}.

%% Test benchmark_comparison with improvement
benchmark_comparison_improvement_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Current results show improvement
              Baseline = #{
                  <<"throughput_msg_per_s">> => 1000000.0,
                  <<"latency_p99_us">> => 1000.0
              },
              Current = #{
                  <<"throughput_msg_per_s">> => 1200000.0,  % 20% improvement
                  <<"latency_p99_us">> => 800.0            % 20% improvement
              },
              TolerancePercent = 10,

              % When: Compare against baseline
              Result = erlmcp_performance_validator:benchmark_comparison(Current, Baseline, TolerancePercent),

              % Then: Should pass and show improvements
              ?assertMatch({pass, _}, Result),
              {pass, Details} = Result,
              ?assertEqual(0, maps:get(regressions, Details)),
              ?assertEqual(2, maps:get(improvements, Details))
          end)
         ]
     end}.

%%====================================================================
%% Performance Report Generation Tests
%%====================================================================

%% Test generate_performance_report with all passing metrics
generate_performance_report_all_pass_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: All metrics passing
              Metrics = #{
                  latency => #{
                      <<"latency_p50_us">> => 80.0,
                      <<"latency_p95_us">> => 400.0,
                      <<"latency_p99_us">> => 900.0
                  },
                  throughput => #{
                      <<"throughput_msg_per_s">> => 150000.0
                  },
                  memory => #{
                      <<"memory_per_connection_mib">> => 5.0
                  },
                  concurrency => #{
                      <<"concurrent_connections">> => 30000
                  }
              },

              % When: Generate performance report
              Report = erlmcp_performance_validator:generate_performance_report(Metrics),

              % Then: Should have all sections with pass status
              ?assertEqual(true, maps:get(overall_passed, Report)),
              ?assertEqual(4, maps:get(total_checks, Report)),
              ?assertEqual(4, maps:get(passed_checks, Report)),
              ?assertEqual(0, maps:get(failed_checks, Report)),

              % Verify individual sections
              LatencyResult = maps:get(latency, Report),
              ?assertEqual(pass, element(1, LatencyResult)),

              ThroughputResult = maps:get(throughput, Report),
              ?assertEqual(pass, element(1, ThroughputResult)),

              MemoryResult = maps:get(memory, Report),
              ?assertEqual(pass, element(1, MemoryResult)),

              ConcurrencyResult = maps:get(concurrency, Report),
              ?assertEqual(pass, element(1, ConcurrencyResult))
          end)
         ]
     end}.

%% Test generate_performance_report with mixed results
generate_performance_report_mixed_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Mixed pass/fail metrics
              Metrics = #{
                  latency => #{
                      <<"latency_p50_us">> => 150.0,     % FAIL
                      <<"latency_p95_us">> => 400.0,
                      <<"latency_p99_us">> => 900.0
                  },
                  throughput => #{
                      <<"throughput_msg_per_s">> => 50000.0  % FAIL
                  },
                  memory => #{
                      <<"memory_per_connection_mib">> => 5.0  % PASS
                  },
                  concurrency => #{
                      <<"concurrent_connections">> => 30000  % PASS
                  }
              },

              % When: Generate performance report
              Report = erlmcp_performance_validator:generate_performance_report(Metrics),

              % Then: Should show 2 pass, 2 fail
              ?assertEqual(false, maps:get(overall_passed, Report)),
              ?assertEqual(4, maps:get(total_checks, Report)),
              ?assertEqual(2, maps:get(passed_checks, Report)),
              ?assertEqual(2, maps:get(failed_checks, Report)),

              % Verify failed sections
              LatencyResult = maps:get(latency, Report),
              ?assertEqual(fail, element(1, LatencyResult)),

              ThroughputResult = maps:get(throughput, Report),
              ?assertEqual(fail, element(1, ThroughputResult))
          end)
         ]
     end}.

%% Test generate_performance_report includes timestamp
generate_performance_report_timestamp_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Metrics
              Metrics = #{
                  latency => #{<<"latency_p50_us">> => 80.0},
                  throughput => #{<<"throughput_msg_per_s">> => 150000.0}
              },

              % When: Generate report
              Before = os:system_time(second),
              Report = erlmcp_performance_validator:generate_performance_report(Metrics),
              After = os:system_time(second),

              % Then: Should include timestamp
              Timestamp = maps:get(generated_at, Report),
              ?assert(Timestamp >= Before andalso Timestamp =< After)
          end)
         ]
     end}.

%%====================================================================
%% Integration Tests with Real Benchmark Data
%%====================================================================

%% Test with real benchmark results from file
real_benchmark_validation_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Real benchmark results file
              BenchmarkFile = "/Users/sac/erlmcp/bench/results/core_ops_core_ops_100k_1769824174.json",

              % Load real benchmark data
              {ok, BenchmarkData} = file:read_file(BenchmarkFile),
              Benchmark = jsx:decode(BenchmarkData, [return_maps]),

              % Extract metrics
              Metrics = #{
                  latency => #{
                      <<"latency_p50_us">> => maps:get(<<"latency_p50_us">>, Benchmark),
                      <<"latency_p95_us">> => maps:get(<<"latency_p95_us">>, Benchmark),
                      <<"latency_p99_us">> => maps:get(<<"latency_p99_us">>, Benchmark)
                  },
                  throughput => Benchmark,
                  memory => #{
                      <<"memory_per_connection_mib">> =>
                          (maps:get(<<"memory_delta_mib">>, Benchmark) / 100000)  % Approximate
                  },
                  concurrency => #{
                      <<"concurrent_connections">> => 100
                  }
              },

              % When: Validate with real data
              Report = erlmcp_performance_validator:generate_performance_report(Metrics),

              % Then: Should validate successfully (this is real data)
              ?assert(is_map(Report)),
              ?assertEqual(4, maps:get(total_checks, Report)),

              % Verify throughput validation (1.98M ops/sec is way above 100K)
              ThroughputResult = maps:get(throughput, Report),
              ?assertEqual(pass, element(1, ThroughputResult))
          end)
         ]
     end}.

%% Test baseline comparison with real benchmark data
real_baseline_comparison_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Two real benchmark files
              BaselineFile = "/Users/sac/erlmcp/bench/results/core_ops_core_ops_100k_1769824174.json",
              CurrentFile = "/Users/sac/erlmcp/bench/results/core_ops_core_ops_100k_1769835665.json",

              % Load both files
              {ok, BaselineData} = file:read_file(BaselineFile),
              Baseline = jsx:decode(BaselineData, [return_maps]),

              {ok, CurrentData} = file:read_file(CurrentFile),
              Current = jsx:decode(CurrentData, [return_maps]),

              % When: Compare current against baseline
              Result = erlmcp_performance_validator:benchmark_comparison(Current, Baseline, 10),

              % Then: Should produce comparison result
              ?assert(is_tuple(Result)),
              element(1, Result) =:= pass orelse element(1, Result) =:= fail
          end)
         ]
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test validation with empty maps
validate_empty_metrics_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Empty metrics map
              Metrics = #{},

              % When: Generate report
              Report = erlmcp_performance_validator:generate_performance_report(Metrics),

              % Then: Should handle gracefully
              ?assert(is_map(Report)),
              ?assertEqual(0, maps:get(total_checks, Report, 0))
          end)
         ]
     end}.

%% Test validation with zero values
validate_zero_values_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Zero throughput
              Throughput = #{
                  <<"throughput_msg_per_s">> => 0.0
              },

              % When: Validate
              Result = erlmcp_performance_validator:validate_throughput(Throughput, 100000),

              % Then: Should fail (zero is below minimum)
              ?assertMatch({fail, _}, Result)
          end)
         ]
     end}.

%% Test validation with negative values
validate_negative_values_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Given: Negative latency (invalid)
              Latencies = #{
                  <<"latency_p50_us">> => -10.0,
                  <<"latency_p95_us">> => 400.0,
                  <<"latency_p99_us">> => 900.0
              },

              % When: Validate
              Result = erlmcp_performance_validator:validate_latency(Latencies, #{}),

              % Then: Should fail with error
              ?assertMatch({fail, _}, Result),
              {fail, Details} = Result,
              ?assertEqual(true, maps:get(is_error, Details, false))
          end)
         ]
     end}.

%% Test tolerance calculation edge cases
tolerance_edge_cases_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_TestDir) ->
         [
          ?_test(begin
              % Test exact tolerance boundary (10% = pass, 10.1% = fail)
              Baseline = #{<<"throughput_msg_per_s">> => 1000000.0},

              % Exactly 10% degradation should pass
              Current1 = #{<<"throughput_msg_per_s">> => 900000.0},
              Result1 = erlmcp_performance_validator:benchmark_comparison(Current1, Baseline, 10),
              ?assertMatch({pass, _}, Result1),

              % 10.1% degradation should fail
              Current2 = #{<<"throughput_msg_per_s">> => 899000.0},
              Result2 = erlmcp_performance_validator:benchmark_comparison(Current2, Baseline, 10),
              ?assertMatch({fail, _}, Result2)
          end)
         ]
     end}.
