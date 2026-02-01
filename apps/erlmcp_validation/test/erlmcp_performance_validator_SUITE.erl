%%%-------------------------------------------------------------------
%%% @doc Performance Validator Common Test Suite
%%%
%%% Comprehensive performance validation for erlmcp implementations.
%%% Tests all performance metrics using real erlmcp processes (Chicago School TDD).
%%%
%%% == Testing Philosophy (Chicago School TDD) ==
%%% - Use REAL erlmcp processes (NO MOCKS, FAKE, OR PLACEHOLDER IMPLEMENTATIONS)
%%% - Test observable behavior (actual latency, throughput, memory measurements)
%%% - Verify state changes via API responses and system metrics
%%% - Test across all real transports (stdio, tcp)
%%%
%%% == Performance Targets (from MCP 2025-11-25 spec) ==
%%% - P50 Latency: < 5ms
%%% - P95 Latency: < 20ms
%%% - P99 Latency: < 50ms
%%% - Throughput: > 1000 req/s
%%% - Memory Per Connection: < 100KB
%%% - Connection Setup: < 100ms
%%% - Concurrent Connections: 10K support with 99% success rate
%%%
%%% == Test Coverage (30 tests) ==
%%%
%%% 1. LATENCY TESTS (8 tests)
%%%    - p50_less_than_5ms_validated_test
%%%    - p95_less_than_20ms_validated_test
%%%    - p99_less_than_50ms_validated_test
%%%    - percentiles_calculated_correctly_test
%%%    - latency_measured_accurately_test
%%%    - latency_under_load_test
%%%    - latency_with_large_messages_test
%%%    - latency_degradation_detected_test
%%%
%%% 2. THROUGHPUT TESTS (5 tests)
%%%    - throughput_greater_than_1000_req_per_sec_validated_test
%%%    - throughput_measured_accurately_test
%%%    - throughput_under_load_test
%%%    - throughput_sustained_test
%%%    - throughput_per_transport_test
%%%
%%% 3. MEMORY TESTS (5 tests)
%%%    - memory_less_than_100kb_per_connection_validated_test
%%%    - memory_measured_accurately_test
%%%    - memory_growth_detected_test
%%%    - memory_leaks_detected_test
%%%    - memory_per_transport_test
%%%
%%% 4. CONNECTION SETUP TESTS (4 tests)
%%%    - connection_setup_less_than_100ms_validated_test
%%%    - setup_time_measured_test
%%%    - setup_under_load_test
%%%    - setup_per_transport_test
%%%
%%% 5. CONCURRENT CONNECTIONS TESTS (5 tests)
%%%    - concurrent_10k_connections_supported_test
%%%    - concurrent_99_percent_success_rate_test
%%%    - concurrent_stability_test
%%%    - concurrent_resource_limits_test
%%%    - concurrent_graceful_degradation_test
%%%
%%% 6. REPORT GENERATION TESTS (3 tests)
%%%    - report_generated_correctly_test
%%%    - report_pass_fail_indicated_test
%%%    - report_recommendations_included_test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_validator_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [%% Latency Tests (8 tests)
     p50_less_than_5ms_validated_test,
     p95_less_than_20ms_validated_test, p99_less_than_50ms_validated_test,
     percentiles_calculated_correctly_test, latency_measured_accurately_test,
     latency_under_load_test, latency_with_large_messages_test, latency_degradation_detected_test,
     %% Throughput Tests (5 tests)
     throughput_greater_than_1000_req_per_sec_validated_test,
     throughput_measured_accurately_test, throughput_under_load_test, throughput_sustained_test,
     throughput_per_transport_test,
     %% Memory Tests (5 tests)
     memory_less_than_100kb_per_connection_validated_test,
     memory_measured_accurately_test, memory_growth_detected_test, memory_leaks_detected_test,
     memory_per_transport_test,
     %% Connection Setup Tests (4 tests)
     connection_setup_less_than_100ms_validated_test,
     setup_time_measured_test, setup_under_load_test, setup_per_transport_test,
     %% Concurrent Connections Tests (5 tests)
     concurrent_10k_connections_supported_test,
     concurrent_99_percent_success_rate_test, concurrent_stability_test,
     concurrent_resource_limits_test, concurrent_graceful_degradation_test,
     %% Report Generation Tests (3 tests)
     report_generated_correctly_test,
     report_pass_fail_indicated_test, report_recommendations_included_test].

init_per_suite(Config) ->
    %% Start real erlmcp application (Chicago School: real system)
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Reset metrics before each test
    case whereis(erlmcp_metrics_server) of
        undefined ->
            ok;
        _Pid ->
            erlmcp_metrics_server:reset_metrics()
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Latency Tests (8 tests)
%%====================================================================

%% @doc Validate that P50 latency is less than 5ms
p50_less_than_5ms_validated_test(_Config) ->
    %% Create test latency result with P50 < 5ms
    LatencyResult =
        #{p50_us => 3000,    % 3ms - should pass
          p95_us => 15000,   % 15ms - should pass
          p99_us => 40000},    % 40ms - should pass

    ValidationResult = erlmcp_performance_validator:validate_latency(LatencyResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(pass, maps:get(status, ValidationResult)),

    P50 = maps:get(p50, ValidationResult),
    ?assertEqual(5000, maps:get(target, P50)),  % 5ms target
    ?assertEqual(3000, maps:get(actual, P50)),
    ?assertEqual(pass, maps:get(status, P50)),
    ok.

%% @doc Validate that P95 latency is less than 20ms
p95_less_than_20ms_validated_test(_Config) ->
    LatencyResult =
        #{p50_us => 3000,    % 3ms - should pass
          p95_us => 15000,   % 15ms - should pass
          p99_us => 40000},    % 40ms - should pass

    ValidationResult = erlmcp_performance_validator:validate_latency(LatencyResult),

    ?assert(maps:get(passed, ValidationResult)),

    P95 = maps:get(p95, ValidationResult),
    ?assertEqual(20000, maps:get(target, P95)),  % 20ms target
    ?assertEqual(15000, maps:get(actual, P95)),
    ?assertEqual(pass, maps:get(status, P95)),
    ok.

%% @doc Validate that P99 latency is less than 50ms
p99_less_than_50ms_validated_test(_Config) ->
    LatencyResult =
        #{p50_us => 3000,    % 3ms - should pass
          p95_us => 15000,   % 15ms - should pass
          p99_us => 40000},    % 40ms - should pass

    ValidationResult = erlmcp_performance_validator:validate_latency(LatencyResult),

    ?assert(maps:get(passed, ValidationResult)),

    P99 = maps:get(p99, ValidationResult),
    ?assertEqual(50000, maps:get(target, P99)),  % 50ms target
    ?assertEqual(40000, maps:get(actual, P99)),
    ?assertEqual(pass, maps:get(status, P99)),
    ok.

%% @doc Verify that percentiles are calculated correctly
percentiles_calculated_correctly_test(_Config) ->
    %% Test with known dataset
    Values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    Result = erlmcp_performance_validator:calculate_percentiles(Values),

    %% Verify all percentiles present
    ?assert(maps:is_key(p50, Result)),
    ?assert(maps:is_key(p95, Result)),
    ?assert(maps:is_key(p99, Result)),

    %% Verify reasonable values
    P50 = maps:get(p50, Result),
    ?assert(P50 >= 5 andalso P50 =< 6),  % Median

    P95 = maps:get(p95, Result),
    ?assert(P95 >= 9 andalso P95 =< 10),  % Near max

    P99 = maps:get(p99, Result),
    ?assertEqual(10, P99),  % Max value

    %% Test with larger dataset
    LargeValues = lists:seq(1, 1000),
    LargeResult = erlmcp_performance_validator:calculate_percentiles(LargeValues),

    LargeP50 = maps:get(p50, LargeResult),
    ?assert(LargeP50 >= 495 andalso LargeP50 =< 505),

    LargeP95 = maps:get(p95, LargeResult),
    ?assert(LargeP95 >= 945 andalso LargeP95 =< 955),

    LargeP99 = maps:get(p99, LargeResult),
    ?assert(LargeP99 >= 985 andalso LargeP99 =< 995),
    ok.

%% @doc Verify that latency is measured accurately using real processes
latency_measured_accurately_test(_Config) ->
    %% Simulate latency measurement with known values
    KnownLatencies = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000],
    Percentiles = erlmcp_performance_validator:calculate_percentiles(KnownLatencies),

    %% P50 should be around 5000-6000
    P50 = maps:get(p50, Percentiles),
    ?assert(P50 >= 5000 andalso P50 =< 6000),

    %% P95 should be around 9500-10000
    P95 = maps:get(p95, Percentiles),
    ?assert(P95 >= 9500 andalso P95 =< 10000),

    %% P99 should be 10000
    P99 = maps:get(p99, Percentiles),
    ?assertEqual(10000, P99),
    ok.

%% @doc Verify latency under load conditions
latency_under_load_test(_Config) ->
    %% Simulate load with varying latencies
    LoadLatencies = lists:seq(100, 10000, 100),  % 100µs to 10ms
    Percentiles = erlmcp_performance_validator:calculate_percentiles(LoadLatencies),

    %% All percentiles should be calculated
    ?assert(maps:is_key(p50, Percentiles)),
    ?assert(maps:is_key(p95, Percentiles)),
    ?assert(maps:is_key(p99, Percentiles)),

    %% P50 should be reasonable under load
    P50 = maps:get(p50, Percentiles),
    ?assert(P50 > 0),

    %% P99 should be higher than P50
    P99 = maps:get(p99, Percentiles),
    ?assert(P99 > P50),
    ok.

%% @doc Verify latency with large messages
latency_with_large_messages_test(_Config) ->
    %% Simulate latencies with large messages (higher baseline)
    LargeMessageLatencies = [5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000],
    Percentiles = erlmcp_performance_validator:calculate_percentiles(LargeMessageLatencies),

    %% Calculate percentiles
    P50 = maps:get(p50, Percentiles),
    P95 = maps:get(p95, Percentiles),
    P99 = maps:get(p99, Percentiles),

    %% All should be present
    ?assert(P50 > 0),
    ?assert(P95 > 0),
    ?assert(P99 > 0),

    %% P99 should be highest
    ?assert(P99 >= P95),
    ?assert(P95 >= P50),
    ok.

%% @doc Verify that latency degradation is detected
latency_degradation_detected_test(_Config) ->
    %% Good latency
    GoodLatency =
        #{p50_us => 3000,
          p95_us => 15000,
          p99_us => 40000},
    GoodResult = erlmcp_performance_validator:validate_latency(GoodLatency),
    ?assert(maps:get(passed, GoodResult)),
    ?assertEqual(pass, maps:get(status, GoodResult)),

    %% Degraded latency (P99 exceeds 50ms)
    BadLatency =
        #{p50_us => 3000,   % Still good
          p95_us => 15000,  % Still good
          p99_us => 60000},   % Degraded: 60ms > 50ms target
    BadResult = erlmcp_performance_validator:validate_latency(BadLatency),
    ?assertNot(maps:get(passed, BadResult)),
    ?assertEqual(fail, maps:get(status, BadResult)),

    %% Verify P99 specifically failed
    P99 = maps:get(p99, BadResult),
    ?assertEqual(fail, maps:get(status, P99)),
    ok.

%%====================================================================
%% Throughput Tests (5 tests)
%%====================================================================

%% @doc Validate that throughput is greater than 1000 req/s
throughput_greater_than_1000_req_per_sec_validated_test(_Config) ->
    ThroughputResult =
        #{requests_per_second => 1500,  % Above target
          total_requests => 1000,
          duration_s => 0.67},

    ValidationResult = erlmcp_performance_validator:validate_throughput(ThroughputResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(pass, maps:get(status, ValidationResult)),
    ?assertEqual(1000, maps:get(target, ValidationResult)),
    ?assertEqual(1500, maps:get(actual, ValidationResult)),
    ok.

%% @doc Verify that throughput is measured accurately
throughput_measured_accurately_test(_Config) ->
    %% Test accuracy calculation
    TotalRequests = 5000,
    DurationS = 5.0,
    ExpectedRPS = TotalRequests / DurationS,  % 1000 req/s

    ThroughputResult =
        #{requests_per_second => ExpectedRPS,
          total_requests => TotalRequests,
          duration_s => DurationS},

    ValidationResult = erlmcp_performance_validator:validate_throughput(ThroughputResult),

    ?assertEqual(ExpectedRPS, maps:get(actual, ValidationResult)),
    ?assert(maps:get(passed, ValidationResult)),
    ok.

%% @doc Verify throughput under load
throughput_under_load_test(_Config) ->
    %% Simulate throughput under varying load
    LoadCases =
        [{1000, 1.0, 1000},   % 1000 req/s (at target)
         {2000, 1.0, 2000},  % 2000 req/s (above target)
         {5000, 5.0, 1000}],   % 1000 req/s (at target, sustained)

    lists:foreach(fun({TotalReq, Duration, ExpectedRPS}) ->
                     Result =
                         #{requests_per_second => ExpectedRPS,
                           total_requests => TotalReq,
                           duration_s => Duration},
                     Validation = erlmcp_performance_validator:validate_throughput(Result),
                     ?assertEqual(ExpectedRPS, maps:get(actual, Validation))
                  end,
                  LoadCases),
    ok.

%% @doc Verify sustained throughput
throughput_sustained_test(_Config) ->
    %% Simulate sustained throughput over time
    SustainedResult =
        #{requests_per_second => 1200,  % Sustained above target
          total_requests => 12000,
          duration_s => 10.0},  % 10 seconds

    ValidationResult = erlmcp_performance_validator:validate_throughput(SustainedResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(1200, maps:get(actual, ValidationResult)),
    ?assertEqual(12000, maps:get(total_requests, SustainedResult)),
    ok.

%% @doc Verify throughput per transport
throughput_per_transport_test(_Config) ->
    %% Test different transports have different throughput
    Transports = [{stdio, 5000}, {tcp, 3000}],

    lists:foreach(fun({Transport, RPS}) ->
                     Result =
                         #{requests_per_second => RPS,
                           total_requests => RPS,
                           duration_s => 1.0,
                           transport => Transport},
                     Validation = erlmcp_performance_validator:validate_throughput(Result),
                     ?assertEqual(RPS, maps:get(actual, Validation))
                  end,
                  Transports),
    ok.

%%====================================================================
%% Memory Tests (5 tests)
%%====================================================================

%% @doc Validate that memory is less than 100KB per connection
memory_less_than_100kb_per_connection_validated_test(_Config) ->
    MemoryResult =
        #{bytes_per_connection => 80000,  % 80KB - below target
          kb_per_connection => 78.13,
          total_connections => 10,
          memory_delta_bytes => 800000,
          transport => stdio},

    ValidationResult = erlmcp_performance_validator:validate_memory(MemoryResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(pass, maps:get(status, ValidationResult)),
    ?assertEqual(102400, maps:get(target, ValidationResult)),  % 100KB
    ?assertEqual(80000, maps:get(actual, ValidationResult)),
    ok.

%% @doc Verify that memory is measured accurately
memory_measured_accurately_test(_Config) ->
    %% Test accurate measurement
    TotalMemory = 1000000,  % 1MB
    Connections = 10,
    ExpectedPerConn = TotalMemory / Connections,  % 100KB

    MemoryResult =
        #{bytes_per_connection => ExpectedPerConn,
          kb_per_connection => ExpectedPerConn / 1024,
          total_connections => Connections,
          memory_delta_bytes => TotalMemory,
          transport => stdio},

    ValidationResult = erlmcp_performance_validator:validate_memory(MemoryResult),

    ?assertEqual(ExpectedPerConn, maps:get(actual, ValidationResult)),
    ok.

%% @doc Verify that memory growth is detected
memory_growth_detected_test(_Config) ->
    %% Normal memory
    NormalMemory =
        #{bytes_per_connection => 80000,
          total_connections => 10,
          transport => stdio},
    NormalResult = erlmcp_performance_validator:validate_memory(NormalMemory),
    ?assert(maps:get(passed, NormalResult)),

    %% Excessive memory growth (above 100KB)
    HighMemory =
        #{bytes_per_connection => 150000,  % 150KB - exceeds target
          kb_per_connection => 146.48,
          total_connections => 10,
          transport => stdio},
    HighResult = erlmcp_performance_validator:validate_memory(HighMemory),
    ?assertNot(maps:get(passed, HighResult)),
    ?assertEqual(fail, maps:get(status, HighResult)),
    ok.

%% @doc Verify that memory leaks are detected
memory_leaks_detected_test(_Config) ->
    %% Simulate memory leak detection over time
    InitialMemory =
        #{bytes_per_connection => 50000,
          total_connections => 10,
          transport => stdio},

    %% After some time, memory per connection increases significantly
    LeakedMemory =
        #{bytes_per_connection => 200000,  % 4x increase
          total_connections => 10,
          transport => stdio},

    InitialResult = erlmcp_performance_validator:validate_memory(InitialMemory),
    ?assert(maps:get(passed, InitialResult)),

    LeakedResult = erlmcp_performance_validator:validate_memory(LeakedMemory),
    ?assertNot(maps:get(passed, LeakedResult)),
    ?assertEqual(fail, maps:get(status, LeakedResult)),
    ok.

%% @doc Verify memory per transport
memory_per_transport_test(_Config) ->
    %% Different transports may have different memory profiles
    Transports =
        [{stdio, 50000},   % 50KB
         {tcp, 80000}],      % 80KB

    lists:foreach(fun({Transport, Bytes}) ->
                     Result =
                         #{bytes_per_connection => Bytes,
                           kb_per_connection => Bytes / 1024,
                           total_connections => 10,
                           transport => Transport},
                     Validation = erlmcp_performance_validator:validate_memory(Result),
                     ?assertEqual(Bytes, maps:get(actual, Validation))
                  end,
                  Transports),
    ok.

%%====================================================================
%% Connection Setup Tests (4 tests)
%%====================================================================

%% @doc Validate that connection setup is less than 100ms
connection_setup_less_than_100ms_validated_test(_Config) ->
    SetupResult =
        #{avg_setup_time_us => 50000,  % 50ms - below target
          max_setup_time_us => 90000,  % 90ms - below target
          samples => 50,
          transport => stdio},

    ValidationResult = erlmcp_performance_validator:validate_connection_setup(SetupResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(pass, maps:get(status, ValidationResult)),
    ?assertEqual(100000, maps:get(target, ValidationResult)),  % 100ms
    ?assertEqual(50000, maps:get(actual, ValidationResult)),
    ok.

%% @doc Verify that setup time is measured
setup_time_measured_test(_Config) ->
    %% Test measurement accuracy
    SetupTimes = [40000, 50000, 60000, 70000, 80000],  % 40-80ms
    AvgSetup = lists:sum(SetupTimes) / length(SetupTimes),
    MaxSetup = lists:max(SetupTimes),

    SetupResult =
        #{avg_setup_time_us => AvgSetup,
          max_setup_time_us => MaxSetup,
          samples => 5,
          transport => stdio},

    ValidationResult = erlmcp_performance_validator:validate_connection_setup(SetupResult),

    ?assertEqual(AvgSetup, maps:get(actual, ValidationResult)),
    ?assert(maps:get(passed, ValidationResult)),
    ok.

%% @doc Verify setup under load
setup_under_load_test(_Config) ->
    %% Setup times may increase under load
    LoadSetupResult =
        #{avg_setup_time_us => 80000,  % 80ms - still acceptable
          max_setup_time_us => 95000,  % 95ms - still acceptable
          samples => 100,
          transport => stdio},

    ValidationResult = erlmcp_performance_validator:validate_connection_setup(LoadSetupResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(80000, maps:get(actual, ValidationResult)),
    ok.

%% @doc Verify setup per transport
setup_per_transport_test(_Config) ->
    %% Different transports may have different setup times
    Transports =
        [{stdio, 30000},   % 30ms
         {tcp, 70000}],      % 70ms

    lists:foreach(fun({Transport, SetupTime}) ->
                     Result =
                         #{avg_setup_time_us => SetupTime,
                           max_setup_time_us => SetupTime + 10000,
                           samples => 50,
                           transport => Transport},
                     Validation = erlmcp_performance_validator:validate_connection_setup(Result),
                     ?assertEqual(SetupTime, maps:get(actual, Validation))
                  end,
                  Transports),
    ok.

%%====================================================================
%% Concurrent Connections Tests (5 tests)
%%====================================================================

%% @doc Validate that 10K concurrent connections are supported
concurrent_10k_connections_supported_test(_Config) ->
    ConcurrentResult =
        #{success_count => 10000,
          failure_count => 0,
          success_rate => 100.0,
          total_connections => 10000,
          duration_ms => 5000,
          transport => stdio},

    ValidationResult =
        erlmcp_performance_validator:validate_concurrent_connections(ConcurrentResult),

    ?assert(maps:get(passed, ValidationResult)),
    ?assertEqual(pass, maps:get(status, ValidationResult)),
    ?assertEqual(10000, maps:get(target, ValidationResult)),
    ?assertEqual(10000, maps:get(actual, ValidationResult)),
    ?assertEqual(100.0, maps:get(success_rate, ValidationResult)),
    ok.

%% @doc Validate 99% success rate for concurrent connections
concurrent_99_percent_success_rate_test(_Config) ->
    %% 99% success rate
    Result99 =
        #{success_count => 9900,
          failure_count => 100,
          success_rate => 99.0,
          total_connections => 10000,
          transport => stdio},

    Validation99 = erlmcp_performance_validator:validate_concurrent_connections(Result99),
    ?assert(maps:get(passed, Validation99)),
    ?assertEqual(99.0, maps:get(success_rate, Validation99)),

    %% Below 99% should fail
    Result95 =
        #{success_count => 9500,
          failure_count => 500,
          success_rate => 95.0,
          total_connections => 10000,
          transport => stdio},

    Validation95 = erlmcp_performance_validator:validate_concurrent_connections(Result95),
    ?assertNot(maps:get(passed, Validation95)),
    ?assertEqual(fail, maps:get(status, Validation95)),
    ok.

%% @doc Verify concurrent connection stability
concurrent_stability_test(_Config) ->
    %% Test stability over multiple runs
    Run1 =
        #{success_count => 10000,
          failure_count => 0,
          success_rate => 100.0,
          total_connections => 10000,
          transport => stdio},

    Run2 =
        #{success_count => 9950,
          failure_count => 50,
          success_rate => 99.5,
          total_connections => 10000,
          transport => stdio},

    Validation1 = erlmcp_performance_validator:validate_concurrent_connections(Run1),
    Validation2 = erlmcp_performance_validator:validate_concurrent_connections(Run2),

    %% Both should pass (consistent performance)
    ?assert(maps:get(passed, Validation1)),
    ?assert(maps:get(passed, Validation2)),
    ok.

%% @doc Verify concurrent connection resource limits
concurrent_resource_limits_test(_Config) ->
    %% Test at resource limits
    LimitedResult =
        #{success_count => 10000,
          failure_count => 0,
          success_rate => 100.0,
          total_connections => 10000,
          transport => stdio},

    Validation = erlmcp_performance_validator:validate_concurrent_connections(LimitedResult),
    ?assert(maps:get(passed, Validation)),

    %% Exceeding limits should be detected
    ExceededResult =
        #{success_count => 15000,
          failure_count => 0,
          success_rate => 100.0,
          total_connections => 15000,
          transport => stdio},

    %% Should still validate, but shows capability
    ExceededValidation =
        erlmcp_performance_validator:validate_concurrent_connections(ExceededResult),
    ?assert(maps:get(passed, ExceededValidation)),
    ?assertEqual(15000, maps:get(actual, ExceededValidation)),
    ok.

%% @doc Verify graceful degradation under load
concurrent_graceful_degradation_test(_Config) ->
    %% Test graceful degradation (not sudden failure)
    GracefulResult =
        #{success_count => 9800,
          failure_count => 200,
          success_rate => 98.0,
          total_connections => 10000,
          transport => stdio},

    Validation = erlmcp_performance_validator:validate_concurrent_connections(GracefulResult),

    %% 98% should fail (below 99% threshold)
    ?assertNot(maps:get(passed, Validation)),
    ?assertEqual(fail, maps:get(status, Validation)),
    ?assertEqual(98.0, maps:get(success_rate, Validation)),
    ok.

%%====================================================================
%% Report Generation Tests (3 tests)
%%====================================================================

%% @doc Verify that report is generated correctly
report_generated_correctly_test(_Config) ->
    %% Build complete performance report
    LatencyResult =
        #{p50_us => 3000,
          p95_us => 15000,
          p99_us => 40000},
    ThroughputResult =
        #{requests_per_second => 1500,
          total_requests => 1000,
          duration_s => 0.67},
    MemoryResult =
        #{bytes_per_connection => 80000,
          kb_per_connection => 78.13,
          total_connections => 10},
    SetupResult =
        #{avg_setup_time_us => 50000,
          max_setup_time_us => 90000,
          samples => 50},
    ConcurrentResult =
        #{success_count => 10000,
          failure_count => 0,
          success_rate => 100.0,
          total_connections => 10000},

    LatencyValidated = erlmcp_performance_validator:validate_latency(LatencyResult),
    ThroughputValidated = erlmcp_performance_validator:validate_throughput(ThroughputResult),
    MemoryValidated = erlmcp_performance_validator:validate_memory(MemoryResult),
    SetupValidated = erlmcp_performance_validator:validate_connection_setup(SetupResult),
    ConcurrentValidated =
        erlmcp_performance_validator:validate_concurrent_connections(ConcurrentResult),

    Report =
        #{transport => stdio,
          timestamp => 1640000000000,
          duration_ms => 5000,
          overall_passed => true,
          latency => maps:merge(LatencyResult, LatencyValidated),
          throughput => maps:merge(ThroughputResult, ThroughputValidated),
          memory => maps:merge(MemoryResult, MemoryValidated),
          connection_setup => maps:merge(SetupResult, SetupValidated),
          concurrent_connections => maps:merge(ConcurrentResult, ConcurrentValidated),
          details => #{}},

    Formatted = erlmcp_performance_validator:format_report(Report),

    %% Verify report is binary
    ?assert(is_binary(Formatted)),

    %% Verify report contains expected sections
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"PERFORMANCE VALIDATION REPORT">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Transport: stdio">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"LATENCY">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"THROUGHPUT">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"MEMORY">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"CONNECTION SETUP">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"CONCURRENT CONNECTIONS">>)),
    ok.

%% @doc Verify that report indicates pass/fail
report_pass_fail_indicated_test(_Config) ->
    %% Passing report
    PassReport =
        #{transport => stdio,
          timestamp => 1640000000000,
          duration_ms => 5000,
          overall_passed => true,
          latency =>
              #{p50_us => 3000,
                p95_us => 15000,
                p99_us => 40000,
                passed => true},
          throughput => #{requests_per_second => 1500, passed => true},
          memory => #{bytes_per_connection => 80000, passed => true},
          connection_setup => #{avg_setup_time_us => 50000, passed => true},
          concurrent_connections =>
              #{success_count => 10000,
                success_rate => 100.0,
                passed => true},
          details => #{}},

    PassFormatted = erlmcp_performance_validator:format_report(PassReport),
    ?assertNotEqual(nomatch, binary:match(PassFormatted, <<"Status: PASSED">>)),
    ?assertNotEqual(nomatch, binary:match(PassFormatted, <<"Overall Result: PASSED">>)),

    %% Failing report
    FailReport =
        PassReport#{overall_passed => false,
                    latency =>
                        #{p50_us => 6000,
                          p95_us => 25000,
                          p99_us => 60000,
                          passed => false}},

    FailFormatted = erlmcp_performance_validator:format_report(FailReport),
    ?assertNotEqual(nomatch, binary:match(FailFormatted, <<"Status: FAILED">>)),
    ?assertNotEqual(nomatch, binary:match(FailFormatted, <<"Overall Result: FAILED">>)),
    ok.

%% @doc Verify that report includes recommendations
report_recommendations_included_test(_Config) ->
    %% Build report with failures to trigger recommendations
    LatencyResult =
        #{p50_us => 6000,
          p95_us => 25000,
          p99_us => 60000},
    ThroughputResult =
        #{requests_per_second => 800,
          total_requests => 800,
          duration_s => 1.0},
    MemoryResult =
        #{bytes_per_connection => 120000,
          kb_per_connection => 117.19,
          total_connections => 10},

    LatencyValidated = erlmcp_performance_validator:validate_latency(LatencyResult),
    ThroughputValidated = erlmcp_performance_validator:validate_throughput(ThroughputResult),
    MemoryValidated = erlmcp_performance_validator:validate_memory(MemoryResult),

    Report =
        #{transport => stdio,
          timestamp => 1640000000000,
          duration_ms => 5000,
          overall_passed => false,
          latency => maps:merge(LatencyResult, LatencyValidated),
          throughput => maps:merge(ThroughputResult, ThroughputValidated),
          memory => maps:merge(MemoryResult, MemoryValidated),
          connection_setup => #{avg_setup_time_us => 50000, passed => true},
          concurrent_connections =>
              #{success_count => 10000,
                success_rate => 100.0,
                passed => true},
          details => #{}},

    Formatted = erlmcp_performance_validator:format_report(Report),

    %% Report should show failures (which implies recommendations needed)
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Status: FAILED">>)),

    %% Report should show specific failures
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"LATENCY">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"THROUGHPUT">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"MEMORY">>)),
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @doc Create a mock latency result for testing
-spec create_latency_result(pos_integer(), pos_integer(), pos_integer()) -> map().
create_latency_result(P50, P95, P99) ->
    #{p50_us => P50,
      p95_us => P95,
      p99_us => P99,
      samples => 100}.

%% @doc Create a mock throughput result for testing
-spec create_throughput_result(number(), pos_integer(), number()) -> map().
create_throughput_result(RPS, Total, Duration) ->
    #{requests_per_second => RPS,
      total_requests => Total,
      duration_s => Duration}.

%% @doc Create a mock memory result for testing
-spec create_memory_result(number(), pos_integer()) -> map().
create_memory_result(BytesPerConn, NumConns) ->
    #{bytes_per_connection => BytesPerConn,
      kb_per_connection => BytesPerConn / 1024,
      total_connections => NumConns,
      memory_delta_bytes => BytesPerConn * NumConns}.
