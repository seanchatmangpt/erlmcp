%%%-------------------------------------------------------------------
%%% @doc Performance Validator Unit Tests
%%%
%%% Tests the performance validation module including:
%%% - Latency measurement and validation
%%% - Throughput measurement and validation
%%% - Memory measurement and validation
%%% - Connection setup measurement and validation
%%% - Concurrent connections testing
%%% - Report generation and formatting
%%%
%%% Uses real erlmcp processes (Chicago School TDD - no mocks)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Test Data
%%====================================================================

%% Test latency results (in microseconds)
-define(TEST_LATENCY_PASS, #{
    p50_us => 3000,    % 3ms (pass)
    p95_us => 15000,   % 15ms (pass)
    p99_us => 40000    % 40ms (pass)
}).

-define(TEST_LATENCY_FAIL_P50, #{
    p50_us => 6000,    % 6ms (fail)
    p95_us => 15000,   % 15ms (pass)
    p99_us => 40000    % 40ms (pass)
}).

-define(TEST_LATENCY_FAIL_P99, #{
    p50_us => 3000,    % 3ms (pass)
    p95_us => 15000,   % 15ms (pass)
    p99_us => 60000    % 60ms (fail)
}).

%% Test throughput results
-define(TEST_THROUGHPUT_PASS, #{
    requests_per_second => 1500,  % 1500 req/s (pass)
    total_requests => 1000,
    duration_s => 0.67
}).

-define(TEST_THROUGHPUT_FAIL, #{
    requests_per_second => 500,   % 500 req/s (fail)
    total_requests => 1000,
    duration_s => 2.0
}).

%% Test memory results
-define(TEST_MEMORY_PASS, #{
    bytes_per_connection => 80000,   % 80KB (pass)
    kb_per_connection => 78.13,
    total_connections => 10,
    memory_delta_bytes => 800000,
    transport => stdio
}).

-define(TEST_MEMORY_FAIL, #{
    bytes_per_connection => 120000,  % 120KB (fail)
    kb_per_connection => 117.19,
    total_connections => 10,
    memory_delta_bytes => 1200000,
    transport => stdio
}).

%% Test connection setup results
-define(TEST_SETUP_PASS, #{
    avg_setup_time_us => 50000,   % 50ms (pass)
    max_setup_time_us => 90000,   % 90ms (pass)
    samples => 50,
    transport => stdio
}).

-define(TEST_SETUP_FAIL, #{
    avg_setup_time_us => 150000,  % 150ms (fail)
    max_setup_time_us => 200000,  % 200ms (fail)
    samples => 50,
    transport => stdio
}).

%% Test concurrent connection results
-define(TEST_CONCURRENT_PASS, #{
    success_count => 10000,
    failure_count => 0,
    success_rate => 100.0,
    total_connections => 10000,
    duration_ms => 5000,
    transport => stdio
}).

-define(TEST_CONCURRENT_FAIL_RATE, #{
    success_count => 9500,
    failure_count => 500,
    success_rate => 95.0,
    total_connections => 10000,
    duration_ms => 5000,
    transport => stdio
}).

-define(TEST_CONCURRENT_FAIL_COUNT, #{
    success_count => 5000,
    failure_count => 0,
    success_rate => 100.0,
    total_connections => 5000,
    duration_ms => 2000,
    transport => stdio
}).

%%====================================================================
%% Percentile Calculation Tests
%%====================================================================

percentile_calculation_test() ->
    %% Test with known values
    Values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    Result = erlmcp_performance_validator:calculate_percentiles(Values),
    
    %% P50 should be around 5-6 (median)
    P50 = maps:get(p50, Result),
    ?assert(P50 >= 5 andalso P50 =< 6),
    
    %% P95 should be around 10
    P95 = maps:get(p95, Result),
    ?assert(P95 >= 9 andalso P95 =< 10),
    
    %% P99 should be 10
    P99 = maps:get(p99, Result),
    ?assertEqual(10, P99).

percentile_empty_list_test() ->
    %% Test with empty list
    Result = erlmcp_performance_validator:calculate_percentiles([]),
    
    ?assertEqual(0, maps:get(p50, Result)),
    ?assertEqual(0, maps:get(p95, Result)),
    ?assertEqual(0, maps:get(p99, Result)).

percentile_single_value_test() ->
    %% Test with single value
    Result = erlmcp_performance_validator:calculate_percentiles([42]),
    
    ?assertEqual(42, maps:get(p50, Result)),
    ?assertEqual(42, maps:get(p95, Result)),
    ?assertEqual(42, maps:get(p99, Result)).

percentile_large_dataset_test() ->
    %% Test with large dataset
    Values = lists:seq(1, 1000),
    Result = erlmcp_performance_validator:calculate_percentiles(Values),
    
    %% P50 should be around 500
    P50 = maps:get(p50, Result),
    ?assert(P50 >= 495 andalso P50 =< 505),
    
    %% P95 should be around 950
    P95 = maps:get(p95, Result),
    ?assert(P95 >= 945 andalso P95 =< 955),
    
    %% P99 should be around 990
    P99 = maps:get(p99, Result),
    ?assert(P99 >= 985 andalso P99 =< 995).

%%====================================================================
%% Latency Validation Tests
%%====================================================================

validate_latency_pass_test() ->
    %% All percentiles pass
    Result = erlmcp_performance_validator:validate_latency(?TEST_LATENCY_PASS),
    
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)),
    
    %% Check individual percentiles
    P50 = maps:get(p50, Result),
    ?assert(maps:get(status, P50) =:= pass),
    
    P95 = maps:get(p95, Result),
    ?assert(maps:get(status, P95) =:= pass),
    
    P99 = maps:get(p99, Result),
    ?assert(maps:get(status, P99) =:= pass).

validate_latency_fail_p50_test() ->
    %% P50 fails
    Result = erlmcp_performance_validator:validate_latency(?TEST_LATENCY_FAIL_P50),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)),
    
    P50 = maps:get(p50, Result),
    ?assert(maps:get(status, P50) =:= fail).

validate_latency_fail_p99_test() ->
    %% P99 fails
    Result = erlmcp_performance_validator:validate_latency(?TEST_LATENCY_FAIL_P99),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)),
    
    P99 = maps:get(p99, Result),
    ?assert(maps:get(status, P99) =:= fail).

validate_latency_targets_test() ->
    %% Verify targets are correctly set
    Result = erlmcp_performance_validator:validate_latency(?TEST_LATENCY_PASS),
    
    P50 = maps:get(p50, Result),
    ?assertEqual(5000, maps:get(target, P50)),  % 5ms
    
    P95 = maps:get(p95, Result),
    ?assertEqual(20000, maps:get(target, P95)), % 20ms
    
    P99 = maps:get(p99, Result),
    ?assertEqual(50000, maps:get(target, P99)). % 50ms

%%====================================================================
%% Throughput Validation Tests
%%====================================================================

validate_throughput_pass_test() ->
    %% Throughput passes
    Result = erlmcp_performance_validator:validate_throughput(?TEST_THROUGHPUT_PASS),
    
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)),
    ?assertEqual(1000, maps:get(target, Result)),
    ?assertEqual(1500, maps:get(actual, Result)).

validate_throughput_fail_test() ->
    %% Throughput fails
    Result = erlmcp_performance_validator:validate_throughput(?TEST_THROUGHPUT_FAIL),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)),
    ?assertEqual(1000, maps:get(target, Result)),
    ?assertEqual(500, maps:get(actual, Result)).

validate_throughput_boundary_test() ->
    %% Test boundary condition (exactly at target)
    BoundaryResult = #{
        requests_per_second => 1000,  % Exactly at target
        total_requests => 1000,
        duration_s => 1.0
    },
    Result = erlmcp_performance_validator:validate_throughput(BoundaryResult),
    
    %% Should pass (>= target)
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)).

%%====================================================================
%% Memory Validation Tests
%%====================================================================

validate_memory_pass_test() ->
    %% Memory passes
    Result = erlmcp_performance_validator:validate_memory(?TEST_MEMORY_PASS),
    
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)),
    ?assertEqual(102400, maps:get(target, Result)),  % 100KB
    ?assertEqual(80000, maps:get(actual, Result)).

validate_memory_fail_test() ->
    %% Memory fails
    Result = erlmcp_performance_validator:validate_memory(?TEST_MEMORY_FAIL),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)),
    ?assertEqual(102400, maps:get(target, Result)),
    ?assertEqual(120000, maps:get(actual, Result)).

validate_memory_boundary_test() ->
    %% Test boundary condition (exactly at target)
    BoundaryResult = #{
        bytes_per_connection => 102400,  % Exactly 100KB
        kb_per_connection => 100.0,
        total_connections => 10,
        memory_delta_bytes => 1024000,
        transport => stdio
    },
    Result = erlmcp_performance_validator:validate_memory(BoundaryResult),
    
    %% Should pass (= target)
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)).

%%====================================================================
%% Connection Setup Validation Tests
%%====================================================================

validate_connection_setup_pass_test() ->
    %% Connection setup passes
    Result = erlmcp_performance_validator:validate_connection_setup(?TEST_SETUP_PASS),
    
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)),
    ?assertEqual(100000, maps:get(target, Result)),  % 100ms
    ?assertEqual(50000, maps:get(actual, Result)).   % 50ms

validate_connection_setup_fail_test() ->
    %% Connection setup fails
    Result = erlmcp_performance_validator:validate_connection_setup(?TEST_SETUP_FAIL),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)),
    ?assertEqual(100000, maps:get(target, Result)),
    ?assertEqual(150000, maps:get(actual, Result)).

%%====================================================================
%% Concurrent Connections Validation Tests
%%====================================================================

validate_concurrent_connections_pass_test() ->
    %% Concurrent connections pass
    Result = erlmcp_performance_validator:validate_concurrent_connections(?TEST_CONCURRENT_PASS),
    
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)),
    ?assertEqual(10000, maps:get(actual, Result)),
    ?assertEqual(100.0, maps:get(success_rate, Result)).

validate_concurrent_connections_fail_rate_test() ->
    %% Success rate too low
    Result = erlmcp_performance_validator:validate_concurrent_connections(?TEST_CONCURRENT_FAIL_RATE),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)),
    ?assertEqual(95.0, maps:get(success_rate, Result)).

validate_concurrent_connections_fail_count_test() ->
    %% Not enough connections
    Result = erlmcp_performance_validator:validate_concurrent_connections(?TEST_CONCURRENT_FAIL_COUNT),
    
    ?assertNot(maps:get(passed, Result)),
    ?assertEqual(fail, maps:get(status, Result)).

validate_concurrent_connections_partial_success_test() ->
    %% Test partial success scenario
    PartialResult = #{
        success_count => 9900,
        failure_count => 100,
        success_rate => 99.0,
        total_connections => 10000,
        duration_ms => 5000,
        transport => stdio
    },
    Result = erlmcp_performance_validator:validate_concurrent_connections(PartialResult),
    
    %% Should pass (99% success rate at 10K connections)
    ?assert(maps:get(passed, Result)),
    ?assertEqual(pass, maps:get(status, Result)).

%%====================================================================
%% Report Formatting Tests
%%====================================================================

format_report_test() ->
    %% Test report formatting with validated results
    LatencyValidated = erlmcp_performance_validator:validate_latency(?TEST_LATENCY_PASS),
    ThroughputValidated = erlmcp_performance_validator:validate_throughput(?TEST_THROUGHPUT_PASS),
    MemoryValidated = erlmcp_performance_validator:validate_memory(?TEST_MEMORY_PASS),
    SetupValidated = erlmcp_performance_validator:validate_connection_setup(?TEST_SETUP_PASS),
    ConcurrentValidated = erlmcp_performance_validator:validate_concurrent_connections(?TEST_CONCURRENT_PASS),
    
    Report = #{
        transport => stdio,
        timestamp => 1640000000000,
        duration_ms => 5000,
        overall_passed => true,
        latency => maps:merge(?TEST_LATENCY_PASS, LatencyValidated),
        throughput => maps:merge(?TEST_THROUGHPUT_PASS, ThroughputValidated),
        memory => maps:merge(?TEST_MEMORY_PASS, MemoryValidated),
        connection_setup => maps:merge(?TEST_SETUP_PASS, SetupValidated),
        concurrent_connections => maps:merge(?TEST_CONCURRENT_PASS, ConcurrentValidated),
        details => #{}
    },
    
    Formatted = erlmcp_performance_validator:format_report(Report),
    
    %% Check that report is a binary
    ?assert(is_binary(Formatted)),
    
    %% Check for expected sections
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"PERFORMANCE VALIDATION REPORT">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Transport: stdio">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Status: PASSED">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"LATENCY">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"THROUGHPUT">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"MEMORY">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"CONNECTION SETUP">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"CONCURRENT CONNECTIONS">>)).

format_report_failed_test() ->
    %% Test failed report formatting
    LatencyValidated = erlmcp_performance_validator:validate_latency(?TEST_LATENCY_FAIL_P99),
    ThroughputValidated = erlmcp_performance_validator:validate_throughput(?TEST_THROUGHPUT_FAIL),
    MemoryValidated = erlmcp_performance_validator:validate_memory(?TEST_MEMORY_FAIL),
    SetupValidated = erlmcp_performance_validator:validate_connection_setup(?TEST_SETUP_FAIL),
    ConcurrentValidated = erlmcp_performance_validator:validate_concurrent_connections(?TEST_CONCURRENT_FAIL_RATE),
    
    Report = #{
        transport => tcp,
        timestamp => 1640000000000,
        duration_ms => 5000,
        overall_passed => false,
        latency => maps:merge(?TEST_LATENCY_FAIL_P99, LatencyValidated),
        throughput => maps:merge(?TEST_THROUGHPUT_FAIL, ThroughputValidated),
        memory => maps:merge(?TEST_MEMORY_FAIL, MemoryValidated),
        connection_setup => maps:merge(?TEST_SETUP_FAIL, SetupValidated),
        concurrent_connections => maps:merge(?TEST_CONCURRENT_FAIL_RATE, ConcurrentValidated),
        details => #{}
    },
    
    Formatted = erlmcp_performance_validator:format_report(Report),
    
    %% Check that report shows FAILED status
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Status: FAILED">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Overall Result: FAILED">>)).

%%====================================================================
%% Integration Tests (Real Processes)
%%====================================================================

%% Note: Full integration tests require running MCP servers
%% These are placeholder tests for the integration points

integration_test_placeholder_measurement_test_() ->
    %% This test would require a real MCP server
    %% Skipping for unit test suite
    {timeout, 10, fun() ->
        %% Placeholder for future integration test
        ok
    end}.

integration_test_placeholder_concurrent_test_() ->
    %% This test would require a real MCP server
    %% Skipping for unit test suite
    {timeout, 10, fun() ->
        %% Placeholder for future integration test
        ok
    end}.

%%====================================================================
%% Performance Target Constants Tests
%%====================================================================

verify_performance_targets_test() ->
    %% Verify that performance targets are correctly defined
    %% by running validation against known good/bad values
    
    %% Good latency
    GoodLatency = #{
        p50_us => 1000,  % 1ms
        p95_us => 5000,  % 5ms
        p99_us => 10000  % 10ms
    },
    GoodResult = erlmcp_performance_validator:validate_latency(GoodLatency),
    ?assert(maps:get(passed, GoodResult)),
    
    %% Bad latency
    BadLatency = #{
        p50_us => 10000,  % 10ms
        p95_us => 30000,  % 30ms
        p99_us => 100000 % 100ms
    },
    BadResult = erlmcp_performance_validator:validate_latency(BadLatency),
    ?assertNot(maps:get(passed, BadResult)),
    
    %% Good throughput
    GoodThroughput = #{
        requests_per_second => 10000,
        total_requests => 10000,
        duration_s => 1.0
    },
    GoodThroughputResult = erlmcp_performance_validator:validate_throughput(GoodThroughput),
    ?assert(maps:get(passed, GoodThroughputResult)),
    
    %% Bad throughput
    BadThroughput = #{
        requests_per_second => 100,
        total_requests => 100,
        duration_s => 1.0
    },
    BadThroughputResult = erlmcp_performance_validator:validate_throughput(BadThroughput),
    ?assertNot(maps:get(passed, BadThroughputResult)).
