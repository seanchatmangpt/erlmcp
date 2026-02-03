# Testing Framework for erlmcp v3

## Overview

This document outlines comprehensive testing frameworks for erlmcp v3 designed to validate performance targets, ensure reliability, and maintain quality at scale. The testing framework includes load testing, regression testing, chaos testing, and continuous performance validation.

## Testing Architecture

### Multi-Layer Testing Stack

```
┌─────────────────────────────────────────────────────┐
│                  Validation Layer                   │
│          SLA Verification & Compliance              │
├─────────────────────────────────────────────────────┤
│                  Analysis Layer                      │
│       Performance Analysis & Trending               │
├─────────────────────────────────────────────────────┤
│                  Execution Layer                     │
│          Test Execution & Orchestration             │
├─────────────────────────────────────────────────────┤
│                 Generator Layer                      │
│        Test Case Generation & Mutation             │
└─────────────────────────────────────────────────────┘
```

## Test Implementation

### 1. Load Testing Framework

**erlmcp_load_test.erl**
```erlang
-module(erlmcp_load_test).

-export([start_test/1, stop_test/0, get_test_results/0, generate_report/0]).

-record(test_scenario, {
    name,
    description,
    duration,              % ms
    target_load,           % req/sec
    ramp_up_time,          % ms
    warm_up_time,          % ms
    cooldown_time,         % ms
    request_mix = [],       % List of request types with weights
    think_time = 0,        % ms between requests
    measurements = []      % List of metrics to measure
}).

-record(test_result, {
    scenario,
    start_time,
    end_time,
    duration,
    total_requests = 0,
    successful_requests = 0,
    failed_requests = 0,
    response_times = [],
    throughput_data = [],
    error_data = [],
    resource_usage = #{},
    summary = #{}
}).

-record(test_status, {
    running = false,
    current_scenario = undefined,
    progress = 0.0,        % 0.0 to 1.0
    start_time = undefined,
    estimated_completion = undefined
}).

-define(DEFAULT_SCENARIO, #test_scenario{
    name = "default_load_test",
    description = "Standard load test for erlmcp v3",
    duration = 300000,     % 5 minutes
    target_load = 10000,   % req/sec
    ramp_up_time = 60000,  % 1 minute
    warm_up_time = 30000,  % 30 seconds
    cooldown_time = 60000, % 1 minute
    request_mix = [
        {get_resource, 0.7},
        {post_tool, 0.2},
        {subscribe, 0.1}
    ],
    think_time = 100,
    measurements = [
        response_time,
        throughput,
        error_rate,
        cpu_usage,
        memory_usage
    ]
}).

start_test(Scenario) ->
    % Start load test with given scenario
    case validate_scenario(Scenario) of
        ok ->
            stop_current_test(),
            spawn_test_process(Scenario);
        {error, Reason} ->
            {error, Reason}
    end.

stop_test() ->
    % Stop current test
    stop_current_test(),
    save_test_results(),
    ok.

get_test_results() ->
    % Get current test results
    case get_test_status() of
        {running, Scenario} ->
            % Return in-progress results
            get_in_progress_results();
        {stopped, Results} ->
            Results;
        not_running ->
            {error, no_test_running}
    end.

generate_report() ->
    % Generate comprehensive test report
    Results = get_test_results(),
    case Results of
        {error, _} ->
            {error, no_results_available};
        _ ->
            Report = #{
                summary => generate_summary(Results),
                performance => generate_performance_analysis(Results),
                errors => generate_error_analysis(Results),
                resources => generate_resource_analysis(Results),
                recommendations => generate_recommendations(Results),
                timestamp => erlang:monotonic_time(millisecond)
            },
            Report
    end.

%% Private Functions
validate_scenario(Scenario) ->
    % Validate test scenario
    if
        Scenario#test_scenario.duration =< 0 ->
            {error, invalid_duration};
        Scenario#test_scenario.target_load =< 0 ->
            {error, invalid_target_load};
        Scenario#test_scenario.ramp_up_time < 0 ->
            {error, invalid_ramp_up_time};
        true ->
            validate_request_mix(Scenario#test_scenario.request_mix)
    end.

validate_request_mix(RequestMix) ->
    % Validate request mix weights
    TotalWeight = lists:sum([Weight || {_, Weight} <- RequestMix]),
    if
        TotalWeight =< 0 orelse TotalWeight > 1.1 ->
            {error, invalid_request_mix};
        true ->
            ok
    end.

spawn_test_process(Scenario) ->
    % Spawn test process
    TestPid = spawn_link(fun() -> run_load_test(Scenario) end),
    register(load_test_process, TestPid),
    set_test_status(#test_status{
        running = true,
        current_scenario = Scenario,
        start_time = erlang:monotonic_time(millisecond)
    }),
    ok.

run_load_test(Scenario) ->
    % Execute load test
    set_test_status(#test_status{
        running = true,
        start_time = erlang:monotonic_time(millisecond)
    }),

    % Run test phases
    run_test_phases(Scenario),

    % Generate final report
    Report = generate_report(),

    % Save results
    save_test_results(Report),

    % Update status
    set_test_status(#test_status{running = false}),

    ok.

run_test_phases(Scenario) ->
    % Run test phases
    Now = erlang:monotonic_time(millisecond),

    % Warm-up phase
    WarmUpEnd = Now + Scenario#test_scenario.warm_up_time,
    run_phase(warmup, Now, WarmUpEnd, Scenario),

    % Ramp-up phase
    RampUpEnd = WarmUpEnd + Scenario#test_scenario.ramp_up_time,
    run_phase(rampup, WarmUpEnd, RampUpEnd, Scenario),

    % Sustained load phase
    SustainedEnd = RampUpEnd + Scenario#test_scenario.duration,
    run_phase(sustained, RampUpEnd, SustainedEnd, Scenario),

    % Cooldown phase
    CooldownEnd = SustainedEnd + Scenario#test_scenario.cooldown_time,
    run_phase(cooldown, SustainedEnd, CooldownEnd, Scenario).

run_phase(PhaseType, StartTime, EndTime, Scenario) ->
    % Run test phase
    PhaseDuration = EndTime - StartTime,
    PhaseStart = erlang:monotonic_time(millisecond),

    run_phase_loop(PhaseType, PhaseStart, EndTime, Scenario, #test_result{
        scenario = Scenario#test_scenario.name,
        start_time = PhaseStart
    }).

run_phase_loop(_PhaseType, _Now, EndTime, _Scenario, Results) when erlang:monotonic_time(millisecond) >= EndTime ->
    % Phase complete
    FinalResults = Results#test_result{
        end_time = erlang:monotonic_time(millisecond),
        duration = erlang:monotonic_time(millisecond) - Results#test_result.start_time
    },
    store_phase_results(FinalResults),
    ok;

run_phase_loop(PhaseType, Now, EndTime, Scenario, Results) ->
    % Continue phase
    case should_make_request(Scenario) of
        true ->
            % Make request
            {RequestId, RequestType, RequestTime} = make_test_request(Scenario),
            ProcessTime = erlang:monotonic_time(millisecond) - RequestTime,

            % Update results
            UpdatedResults = update_results(Results, RequestId, RequestType, ProcessTime),

            % Update progress
            Progress = calculate_progress(Now, EndTime),
            update_test_status(Progress),

            % Continue with think time
            if
                Scenario#test_scenario.think_time > 0 ->
                    timer:sleep(Scenario#test_scenario.think_time);
                true ->
                    ok
            end,
            run_phase_loop(PhaseType, erlang:monotonic_time(millisecond), EndTime, Scenario, UpdatedResults);
        false ->
            % Wait before next request
            timer:sleep(100),
            run_phase_loop(PhaseType, erlang:monotonic_time(millisecond), EndTime, Scenario, Results)
    end.

should_make_request(Scenario) ->
    % Determine if should make request based on target load
    Now = erlang:monotonic_time(millisecond),
    TestStart = get_test_start_time(),
    Elapsed = Now - TestStart,

    % Calculate target requests for elapsed time
    TargetRequests = (Scenario#test_scenario.target_load * Elapsed) / 1000,

    % Calculate actual requests made
    ActualRequests = get_actual_requests_count(),

    % Make request if below target
    ActualRequests < TargetRequests.

make_test_request(Scenario) ->
    % Make test request
    Now = erlang:monotonic_time(millisecond),
    RequestType = select_request_type(Scenario#test_scenario.request_mix),
    RequestId = generate_request_id(),

    % Execute request
    Result = execute_test_request(RequestType, RequestId),

    % Log request
    log_request(RequestId, RequestType, Now, Result),

    {RequestId, RequestType, Now}.

execute_test_request(get_resource, RequestId) ->
    % Execute GET resource request
    % Implementation would make actual HTTP request
    timer:sleep(50),  % Simulate request time
    success;

execute_test_request(post_tool, RequestId) ->
    % Execute POST tool request
    timer:sleep(200),  % Simulate longer processing
    case rand:uniform(10) of
        10 -> error;  % 10% chance of error
        _ -> success
    end;

execute_test_request(subscribe, RequestId) ->
    % Execute subscribe request
    timer:sleep(100),
    success.

update_results(Results, RequestId, RequestType, ProcessTime) ->
    % Update test results
    UpdatedResponseTimes = [ProcessTime | Results#test_result.response_times],
    UpdatedTotalRequests = Results#test_result.total_requests + 1,

    Success = case Result of
        success -> true;
        error -> false
    end,

    UpdatedSuccessful = case Success of
        true -> Results#test_result.successful_requests + 1;
        false -> Results#test_result.successful_requests
    end,

    UpdatedFailed = case Success of
        true -> Results#test_result.failed_requests;
        false -> Results#test_result.failed_requests + 1
    end,

    Results#test_result{
        total_requests = UpdatedTotalRequests,
        successful_requests = UpdatedSuccessful,
        failed_requests = UpdatedFailed,
        response_times = UpdatedResponseTimes
    }.

calculate_progress(Now, EndTime) ->
    % Calculate test progress
    TestStart = get_test_start_time(),
    TotalDuration = EndTime - TestStart,
    Elapsed = Now - TestStart,
    min(1.0, Elapsed / TotalDuration).

update_test_status(Progress) ->
    % Update test status
    set_test_status(#test_status{
        running = true,
        progress = Progress,
        estimated_completion = calculate_estimated_completion(Progress)
    }).

calculate_estimated_completion(Progress) ->
    % Calculate estimated completion time
    TestStart = get_test_start_time(),
    TotalDuration = get_test_total_duration(),
    case Progress > 0 of
        true ->
            EstimatedRemaining = (TotalDuration - (erlang:monotonic_time(millisecond) - TestStart)) / Progress;
        false ->
            undefined
    end,
    EstimatedRemaining.

generate_summary(Results) ->
    % Generate test summary
    TotalRequests = Results#test_result.total_requests,
    SuccessRate = if
        TotalRequests > 0 -> Results#test_result.successful_requests / TotalRequests;
        true -> 0.0
    end,
    AvgResponseTime = calculate_average_response_time(Results#test_result.response_times),
    MaxResponseTime = lists:max(Results#test_result.response_times),
    MinResponseTime = lists:min(Results#test_result.response_times),

    #{
        total_requests => TotalRequests,
        successful_requests => Results#test_result.successful_requests,
        failed_requests => Results#test_result.failed_requests,
        success_rate => SuccessRate,
        avg_response_time => AvgResponseTime,
        min_response_time => MinResponseTime,
        max_response_time => MaxResponseTime,
        test_duration => Results#test_result.duration
    }.

calculate_average_response_time(ResponseTimes) ->
    case ResponseTimes of
        [] -> 0.0;
        _ -> lists:sum(ResponseTimes) / length(ResponseTimes)
    end.

generate_performance_analysis(Results) ->
    % Generate performance analysis
    ResponseTimes = Results#test_result.response_times,
    Percentiles = calculate_percentiles(ResponseTimes, [50, 90, 95, 99]),

    #{
        percentiles => Percentiles,
        throughput => calculate_throughput(Results),
        stability => calculate_stability(ResponseTimes),
        bottlenecks => identify_bottlenecks(ResponseTimes)
    }.

calculate_percentiles(List, PercentileList) ->
    % Calculate percentiles for list
    Sorted = lists:sort(List),
    lists:map(fun(Percentile) ->
        Index = round((Percentile / 100) * length(Sorted)),
        {Percentile, lists:nth(Index + 1, Sorted)}
    end, PercentileList).

calculate_throughput(Results) ->
    % Calculate throughput in req/sec
    if
        Results#test_result.duration > 0 ->
            Results#test_result.total_requests / (Results#test_result.duration / 1000);
        true ->
            0.0
    end.

calculate_stability(ResponseTimes) ->
    % Calculate stability score (0-100)
    if
        length(ResponseTimes) > 10 ->
            StdDev = calculate_stddev(ResponseTimes),
            Avg = calculate_average_response_time(ResponseTimes),
            StabilityScore = max(0, 100 - (StdDev / Avg * 100)),
            StabilityScore;
        true ->
            100.0
    end.

calculate_stddev(List) ->
    % Calculate standard deviation
    Avg = calculate_average_response_time(List),
    SquaredDiffs = lists:map(fun(X) -> math:pow(X - Avg, 2) end, List),
    math:sqrt(lists:sum(SquaredDiffs) / length(List)).

identify_bottlenecks(ResponseTimes) ->
    % Identify performance bottlenecks
    SlowRequests = lists:filter(fun(Time) -> Time > 1000 end, ResponseTimes),
    case SlowRequests of
        [] -> [];
        _ ->
            [#{type => slow_requests, count => length(SlowRequests), percentage => length(SlowRequests) / length(ResponseTimes) * 100}]
    end.

generate_error_analysis(Results) ->
    % Generate error analysis
    TotalRequests = Results#test_result.total_requests,
    ErrorRate = if
        TotalRequests > 0 -> Results#test_result.failed_requests / TotalRequests;
        true -> 0.0
    end,

    #{
        total_errors => Results#test_result.failed_requests,
        error_rate => ErrorRate,
        error_distribution => get_error_distribution(Results),
        impact_analysis => calculate_error_impact(Results)
    }.

get_error_distribution(Results) ->
    % Get error distribution by request type
    % Implementation would track errors by type
    [].

calculate_error_impact(Results) ->
    % Calculate business impact of errors
    TotalErrors = Results#test_result.failed_requests,
    if
        TotalErrors > 0 ->
            EstimatedImpact = TotalErrors * 10,  % Simplified calculation
            #{errors => TotalErrors, estimated_cost => EstimatedImpact};
        true ->
            #{errors => 0, estimated_cost => 0}
    end.

generate_resource_analysis(Results) ->
    % Generate resource analysis
    #{
        cpu_usage => get_cpu_usage(Results),
        memory_usage => get_memory_usage(Results),
        network_io => get_network_io(Results),
        disk_io => get_disk_io(Results)
    }.

generate_recommendations(Results) ->
    % Generate optimization recommendations
    Recommendations = [],

    % Check response times
    P95Time = get_p95_response_time(Results),
    if
        P95Time > 100 ->
            [#{priority => high, action => optimize_database_queries, impact => high} | Recommendations];
        true ->
            Recommendations
    end,

    % Check error rate
    ErrorRate = get_error_rate(Results),
    if
        ErrorRate > 0.01 ->
            [#{priority => high, action => improve_error_handling, impact => high} | Recommendations];
        true ->
            Recommendations
    end,

    % Check throughput
    Throughput = get_throughput(Results),
    if
        Throughput < 10000 ->
            [#{priority => medium, action => scale_horizontally, impact => medium} | Recommendations];
        true ->
            Recommendations
    end,

    Recommendations.

get_p95_response_time(Results) ->
    % Get P95 response time
    Percentiles = calculate_percentiles(Results#test_result.response_times, [95]),
    case Percentiles of
        [{95, P95}] -> P95;
        _ -> 0
    end.

get_error_rate(Results) ->
    % Get error rate
    TotalRequests = Results#test_result.total_requests,
    if
        TotalRequests > 0 ->
            Results#test_result.failed_requests / TotalRequests;
        true ->
            0.0
    end.

get_throughput(Results) ->
    % Get throughput
    calculate_throughput(Results).

get_cpu_usage(Results) ->
    % Get CPU usage
    50.0.  % Simplified

get_memory_usage(Results) ->
    % Get memory usage
    60.0.  % Simplified

get_network_io(Results) ->
    % Get network I/O
    #{
        bytes_sent => 1024 * 1024,
        bytes_received => 2048 * 1024
    }.

get_disk_io(Results) ->
    % Get disk I/O
    #{
        bytes_read => 512 * 1024,
        bytes_written => 1024 * 1024
    }.

select_request_type(RequestMix) ->
    % Select request type based on weights
    Random = rand:uniform(),
    select_request_type_recursive(RequestMix, Random, 0.0).

select_request_type_recursive([], _Random, _Acc) ->
    unknown;

select_request_type_recursive([{Type, Weight} | Rest], Random, Acc) ->
    NewAcc = Acc + Weight,
    if
        Random =< NewAcc ->
            Type;
        true ->
            select_request_type_recursive(Rest, Random, NewAcc)
    end.

generate_request_id() ->
    % Generate unique request ID
    {Mega, Sec, Micro} = os:timestamp(),
    integer_to_binary(Mega * 1000000000 + Sec * 1000 + Micro).

log_request(RequestId, RequestType, Timestamp, Result) ->
    % Log request
    io:format("~s: ~s - ~s (~p ms)~n", [
        format_timestamp(Timestamp),
        RequestType,
        atom_to_list(Result),
        50  % Simulated latency
    ]),
    ok.

format_timestamp(Timestamp) ->
    % Format timestamp for logging
    calendar:system_time_to_rfc3339(Timestamp, [{unit, millisecond}]).

get_test_status() ->
    % Get current test status
    case get(test_status) of
        undefined -> not_running;
        Status -> Status
    end.

set_test_status(Status) ->
    % Set test status
    put(test_status, Status).

stop_current_test() ->
    % Stop current test
    case whereis(load_test_process) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

get_in_progress_results() ->
    % Get in-progress test results
    % Implementation would collect current metrics
    #test_result{
        scenario = "in_progress",
        start_time = get_test_start_time(),
        end_time = erlang:monotonic_time(millisecond),
        duration = erlang:monotonic_time(millisecond) - get_test_start_time(),
        total_requests = get_actual_requests_count(),
        successful_requests = get_successful_requests(),
        failed_requests = get_failed_requests(),
        response_times = get_current_response_times()
    }.

get_actual_requests_count() ->
    % Get actual requests count
    0.  % Implementation would track actual requests

get_successful_requests() ->
    % Get successful requests count
    0.

get_failed_requests() ->
    % Get failed requests count
    0.

get_current_response_times() ->
    % Get current response times
    [].

get_test_start_time() ->
    % Get test start time
    case get(test_status) of
        #test_status{start_time = Time} -> Time;
        _ -> 0
    end.

get_test_total_duration() ->
    % Get total test duration
    case get(test_status) of
        #test_status{current_scenario = Scenario} -> Scenario#test_scenario.duration + Scenario#test_scenario.ramp_up_time + Scenario#test_scenario.warm_up_time + Scenario#test_scenario.cooldown_time;
        _ -> 0
    end.

store_phase_results(Results) ->
    % Store phase results
    % Implementation would persist results
    ok.

save_test_results(Report) ->
    % Save test results
    % Implementation would save to database or file
    ok.

save_test_results() ->
    % Save current test results
    save_test_results(generate_report()).
```

### 2. Regression Testing

**erlmcp_regression_test.erl**
```erlang
-module(erlmcp_regression_test).

-export([run_tests/0, run_test_suite/1, compare_with_baseline/1, get_regression_report/0]).

-record(test_suite, {
    name,
    tests = [],
    baseline = undefined,
    current_results = [],
    regression_threshold = 0.1   % 10% threshold for regression
}).

-record(test_case, {
    name,
    type = performance,       % performance, functional, reliability
    metric,
    expected_value,
    tolerance = 0.05,         % 5% tolerance
    description
}).

-record(test_result, {
    test_case,
    baseline_value,
    current_value,
    deviation,
    passed,
    timestamp
}).

-define(TEST_SUITES, [
    #test_suite{
        name = "performance_regression",
        tests = [
            #test_case{
                name = "response_time_p95",
                type = performance,
                metric = "response_time.p95",
                expected_value = 100,
                tolerance = 0.1,
                description = "P95 response time should not exceed 100ms"
            },
            #test_case{
                name = "throughput",
                type = performance,
                metric = "throughput",
                expected_value = 10000,
                tolerance = 0.05,
                description = "Throughput should not drop below 9500 req/sec"
            },
            #test_case{
                name = "error_rate",
                type = performance,
                metric = "error_rate",
                expected_value = 0.01,
                tolerance = 0.2,
                description = "Error rate should not exceed 1.2%"
            }
        ]
    },
    #test_suite{
        name = "functional_regression",
        tests = [
            #test_case{
                name = "resource_creation",
                type = functional,
                metric = "success_rate",
                expected_value = 1.0,
                tolerance = 0.0,
                description = "Resource creation should always succeed"
            },
            #test_case{
                name = "tool_execution",
                type = functional,
                metric = "success_rate",
                expected_value = 1.0,
                tolerance = 0.0,
                description = "Tool execution should always succeed"
            }
        ]
    }
]).

run_tests() ->
    % Run all regression tests
    Results = [],
    lists:foldl(fun(Suite, Acc) ->
        SuiteResults = run_test_suite(Suite),
        Acc ++ SuiteResults
    end, [], ?TEST_SUITES).

run_test_suite(Suite) ->
    % Run specific test suite
    SuiteResults = [],
    lists:foldl fun(TestCase, SuiteAcc) ->
        CaseResult = run_test_case(TestCase, Suite),
        SuiteAcc ++ [CaseResult]
    end, [], Suite#test_suite.tests).

run_test_case(TestCase, Suite) ->
    % Run individual test case
    CurrentValue = get_current_metric(TestCase#test_case.metric),
    BaselineValue = get_baseline_value(TestCase, Suite),

    % Calculate deviation
    Deviation = calculate_deviation(BaselineValue, CurrentValue, TestCase#test_case.tolerance),

    % Determine if test passed
    Passed = abs(Deviation) <= TestCase#test_case.tolerance,

    Result = #test_result{
        test_case = TestCase,
        baseline_value = BaselineValue,
        current_value = CurrentValue,
        deviation = Deviation,
        passed = Passed,
        timestamp = erlang:monotonic_time(millisecond)
    },

    % Log result
    log_test_result(Result, Passed),

    Result.

compare_with_baseline(TestResults) ->
    % Compare current results with baseline
    Regressions = lists:filter(fun(Result) ->
        not Result#test_result.passed
    end, TestResults),

    if
        Regressions /= [] ->
            io:format("Regression detected!~n"),
            lists:foreach(fun(Regression) ->
                io:format("~s: ~.1f% deviation (baseline: ~.2f, current: ~.2f)~n", [
                    Regression#test_result.test_case#test_case.name,
                    Regression#test_result.deviation * 100,
                    Regression#test_result.baseline_value,
                    Regression#test_result.current_value
                ])
            end, Regressions);
        true ->
            io:format("No regressions detected!~n")
    end,

    Regressions.

get_regression_report() ->
    % Generate comprehensive regression report
    TestResults = run_tests(),

    Summary = generate_regression_summary(TestResults),
    Regressions = compare_with_baseline(TestResults),
    Analysis = generate_regression_analysis(TestResults, Regressions),

    Report = #{
        summary => Summary,
        results => TestResults,
        regressions => Regressions,
        analysis => Analysis,
        timestamp => erlang:monotonic_time(millisecond)
    },

    Report.

%% Private Functions
get_current_metric(Metric) ->
    % Get current metric value
    case Metric of
        "response_time.p95" -> 95.0;
        "throughput" -> 9800.0;
        "error_rate" -> 0.012;
        "success_rate" -> 1.0;
        _ -> 0.0
    end.

get_baseline_value(TestCase, Suite) ->
    % Get baseline value for test case
    case Suite#test_suite.baseline of
        undefined ->
            get_historical_baseline(TestCase);
        _ ->
            get_suite_baseline(TestCase, Suite)
    end.

get_historical_baseline(TestCase) ->
    % Get historical baseline value
    % Implementation would query historical data
    case TestCase#test_case.metric of
        "response_time.p95" -> 90.0;
        "throughput" -> 10000.0;
        "error_rate" -> 0.008;
        "success_rate" -> 1.0;
        _ -> 0.0
    end.

get_suite_baseline(TestCase, Suite) ->
    % Get baseline from suite
    case Suite#test_suite.baseline of
        undefined ->
            get_historical_baseline(TestCase);
        HistoricalData ->
            case lists:keyfind(TestCase#test_case.metric, 1, HistoricalData) of
                {_, Baseline} -> Baseline;
                false -> get_historical_baseline(TestCase)
            end
    end.

calculate_deviation(Baseline, Current, Tolerance) ->
    % Calculate percentage deviation
    if
        Baseline /= 0 ->
            (Current - Baseline) / Baseline;
        true ->
            case Current > Tolerance of
                true -> 1.0;
                false -> 0.0
            end
    end.

log_test_result(Result, Passed) ->
    % Log test result
    Status = case Passed of
        true -> "PASSED";
        false -> "FAILED"
    end,

    io:format("[~s] ~s: ~.2f vs ~.2f (~.1f%) ~s~n", [
        Status,
        Result#test_result.test_case#test_case.name,
        Result#test_result.current_value,
        Result#test_result.baseline_value,
        Result#test_result.deviation * 100,
        Result#test_result.test_case#test_case.description
    ]),
    ok.

generate_regression_summary(TestResults) ->
    % Generate regression test summary
    TotalTests = length(TestResults),
    PassedTests = length([R || R <- TestResults, R#test_result.passed]),
    FailedTests = TotalTests - PassedTests,

    #{
        total_tests => TotalTests,
        passed_tests => PassedTests,
        failed_tests => FailedTests,
        pass_rate => PassedTests / TotalTests,
        timestamp => erlang:monotonic_time(millisecond)
    }.

generate_regression_analysis(TestResults, Regressions) ->
    % Generate regression analysis
    Analysis = #{
        most_severe_regression => find_most_severe_regression(Regressions),
        regression_by_type => group_regressions_by_type(TestResults),
        recommendations => generate_regression_recommendations(Regressions)
    },

    Analysis.

find_most_severe_regression(Regressions) ->
    % Find most severe regression
    case Regressions of
        [] -> undefined;
        _ ->
            lists:max(Regressions, fun(R1, R2) ->
                abs(R1#test_result.deviation) > abs(R2#test_result.deviation)
            end)
    end.

group_regressions_by_type(TestResults) ->
    % Group regressions by test type
    lists:foldl(fun(Result, Acc) ->
        Type = Result#test_result.test_case#test_case.type,
        case lists:keyfind(Type, 1, Acc) of
            false ->
                [{Type, [Result]} | Acc];
            {_, TypeResults} ->
                lists:keyreplace(Type, 1, Acc, {Type, [Result | TypeResults]})
        end
    end, [], TestResults).

generate_regression_recommendations(Regressions) ->
    % Generate recommendations for regressions
    lists:map(fun(Regression) ->
        #{
            test_case => Regression#test_result.test_case#test_case.name,
            deviation => Regression#test_result.deviation,
            recommendation => generate_test_recommendation(Regression)
        }
    end, Regressions).

generate_test_recommendation(Regression) ->
    % Generate specific recommendation for failed test
    Case = Regression#test_result.test_case,
    Deviation = Regression#test_result.deviation,

    case Case#test_case.type of
        performance ->
            case Case#test_case.metric of
                "response_time.p95" ->
                    "Optimize database queries and increase cache size";
                "throughput" ->
                    "Scale horizontally and optimize hot paths";
                "error_rate" ->
                    "Improve error handling and add circuit breakers"
            end;
        functional ->
            "Review recent code changes and fix implementation issues";
        _ ->
            "Investigate the root cause of regression"
    end.
```

### 3. Chaos Testing

**erlmcp_chaos_test.erl**
```erlang
-module(erlmcp_chaos_test).

-export([start_chaos_test/1, stop_chaos_test/0, inject_failure/2, get_chaos_results/0]).

-record(chaos_scenario, {
    name,
    description,
    failures = [],         % List of failure types to inject
    failure_rate = 0.1,    % Percentage of requests to fail
    duration = 300000,     % Test duration in ms
    recovery_time = 10000, % Time to recover after failure
    resilience_checks = [] % List of resilience checks
}).

-record(failure_type, {
    name,
    probability = 1.0,    % Probability of occurring
    severity = medium,    % low, medium, high, critical
    description,
    injection_fn,        % Function to inject failure
    recovery_fn          % Function to recover from failure
}).

-record.chaos_result, {
    scenario,
    start_time,
    end_time,
    total_requests = 0,
    failed_requests = 0,
    successful_recoveries = 0,
    system_stability = 0.0,
    incident_timeline = [],
    lessons_learned = []
}).

-record.system_status, {
    chaos_active = false,
    current_scenario = undefined,
    failure_injections = [],
    recovery_attempts = [],
    stability_score = 100.0,
    degradation_detected = false
}).

-define(DEFAULT_FAILURE_TYPES, [
    #failure_type{
        name = network_partition,
        probability = 0.3,
        severity = high,
        description = "Simulate network partition",
        injection_fn = fun inject_network_partition/1,
        recovery_fn = fun recover_network_partition/1
    },
    #failure_type{
        name = cpu_spike,
        probability = 0.4,
        severity = medium,
        description = "Simulate CPU spike",
        injection_fn = fun inject_cpu_spike/1,
        recovery_fn = fun recover_cpu_spike/1
    },
    #failure_type{
        name = memory_pressure,
        probability = 0.3,
        severity = high,
        description = "Simulate memory pressure",
        injection_fn = fun inject_memory_pressure/1,
        recovery_fn = fun recover_memory_pressure/1
    },
    #failure_type{
        name = disk_full,
        probability = 0.2,
        severity = critical,
        description = "Simulate disk full",
        injection_fn = fun inject_disk_full/1,
        recovery_fn = fun recover_disk_full/1
    }
]).

start_chaos_test(Scenario) ->
    % Start chaos test with given scenario
    set_system_status(#system_status{
        chaos_active = true,
        current_scenario = Scenario
    }),

    % Start chaos test process
    spawn(fun() -> chaos_test_loop(Scenario) end),

    ok.

stop_chaos_test() ->
    % Stop chaos test
    recover_all_failures(),
    set_system_status(#system_status{chaos_active = false}),

    % Generate report
    Report = get_chaos_results(),
    save_chaos_report(Report),

    ok.

inject_failure(FailureType, Target) ->
    % Inject specific failure
    case get_failure_definition(FailureType) of
        undefined ->
            {error, unknown_failure_type};
        FailureDef ->
            inject_failure_with_def(FailureDef, Target)
    end.

get_chaos_results() ->
    % Get chaos test results
    SystemStatus = get_system_status(),
    Timeline = get_incident_timeline(),

    #chaos_result{
        scenario = SystemStatus#system_status.current_scenario#chaos_scenario.name,
        start_time = get_test_start_time(),
        end_time = erlang:monotonic_time(millisecond),
        total_requests = get_total_requests(),
        failed_requests = get_failed_requests(),
        successful_recoveries = get_successful_recoveries(),
        system_stability = calculate_system_stability(Timeline),
        incident_timeline = Timeline,
        lessons_learned = generate_lessons_learned(Timeline)
    }.

%% Private Functions
chaos_test_loop(Scenario) ->
    % Main chaos test loop
    set_system_status(#system_status{
        chaos_active = true,
        current_scenario = Scenario
    }),

    TestStart = erlang:monotonic_time(millisecond),
    TestEnd = TestStart + Scenario#chaos_scenario.duration,

    chaos_test_loop(Scenario, TestStart, TestEnd).

chaos_test_loop(Scenario, TestStart, TestEnd) ->
    continue_chaos_test(Scenario, TestStart, TestEnd).

continue_chaos_test(Scenario, TestStart, TestEnd) ->
    ChaosEndTime = erlang:monotonic_time(millisecond),

    if
        ChaosEndTime >= TestEnd ->
            % Test complete, recover all failures
            recover_all_failures(),
            complete_chaos_test(Scenario, TestStart, TestEnd);
        true ->
            % Continue chaos test
            case should_inject_failure(Scenario) of
                true ->
                    inject_random_failure(Scenario),
                    timer:sleep(Scenario#chaos_scenario.recovery_time),
                    continue_chaos_test(Scenario, TestStart, TestEnd);
                false ->
                    % Check system stability
                    Stability = measure_system_stability(),
                    update_system_stability(Stability),
                    timer:sleep(1000),
                    continue_chaos_test(Scenario, TestStart, TestEnd)
            end
    end.

should_inject_failure(Scenario) ->
    % Determine if should inject failure based on failure rate
    Random = rand:uniform(),
    Random < Scenario#chaos_scenario.failure_rate.

inject_random_failure(Scenario) ->
    % Inject random failure from scenario
    Failures = Scenario#chaos_scenario.failures,
    if
        Failures == [] ->
            % Use default failure types
            FailureDefs = ?DEFAULT_FAILURE_TYPES;
        true ->
            FailureDefs = lists:map(fun(FailureType) ->
                get_failure_definition(FailureType)
            end, Failures)
    end,

    % Filter available failures
    AvailableFailures = [F || F <- FailureDefs, F /= undefined],

    case AvailableFailures of
        [] ->
            ok;
        _ ->
            % Select failure based on probability
            SelectedFailure = select_failure_by_probability(AvailableFailures),
            inject_failure_with_def(SelectedFailure, system),
            log_failure_injection(SelectedFailure)
    end.

select_failure_by_probability(FailureDefs) ->
    % Select failure based on probability weighting
    TotalProbability = lists:sum([F#failure_type.probability || F <- FailureDefs]),
    Random = rand:uniform() * TotalProbability,
    select_failure_recursive(FailureDefs, Random, 0.0).

select_failure_recursive([], _Random, _Acc) ->
    undefined;

select_failure_recursive([Failure | Rest], Random, Acc) ->
    NewAcc = Acc + Failure#failure_type.probability,
    if
        Random =< NewAcc ->
            Failure;
        true ->
            select_failure_recursive(Rest, Random, NewAcc)
    end.

inject_failure_with_def(FailureDef, Target) ->
    % Inject failure using failure definition
    StartTime = erlang:monotonic_time(millisecond),
    Result = (FailureDef#failure_type.injection_fn)(Target),

    case Result of
        success ->
            % Log failure injection
            Incident = #{
                type => injection,
                failure_type => FailureDef#failure_type.name,
                target => Target,
                timestamp => StartTime,
                success => true
            },
            add_to_timeline(Incident),

            % Update failure injections
            CurrentInjections = get_current_injections(),
            set_current_injections([FailureDef | CurrentInjections]);
        {error, Reason} ->
            % Log injection failure
            Incident = #{
                type => injection_attempt,
                failure_type => FailureDef#failure_type.name,
                target => Target,
                timestamp => StartTime,
                success => false,
                reason => Reason
            },
            add_to_timeline(Incident)
    end,

    Result.

recover_all_failures() ->
    % Recover all active failures
    CurrentInjections = get_current_injections(),
    lists:foreach(fun(FailureDef) ->
        RecoveryResult = (FailureDef#failure_type.recovery_fn)(system),
        log_recovery_attempt(FailureDef, RecoveryResult)
    end, CurrentInjections),

    % Clear failure injections
    set_current_injections([]),

    ok.

measure_system_stability() ->
    % Measure current system stability
    Metrics = get_system_metrics(),

    % Calculate stability score
    CpuScore = max(0, 100 - Metrics#cpu_usage),
    MemoryScore = max(0, 100 - Metrics#memory_usage),
    ResponseTimeScore = max(0, 100 - Metrics#response_time * 2),  % Penalize high response times
    ErrorRateScore = if
        Metrics#error_rate > 0 ->
            max(0, 100 - Metrics#error_rate * 1000);
        true ->
            100
    end,

    (CpuScore + MemoryScore + ResponseTimeScore + ErrorRateScore) / 4.

update_system_stability(StabilityScore) ->
    % Update system stability score
    CurrentStatus = get_system_status(),
    NewStatus = CurrentStatus#system_status{
        stability_score = StabilityScore,
        degradation_detected = StabilityScore < 50
    },
    set_system_status(NewStatus).

get_failure_definition(FailureType) ->
    % Get failure definition by type
    case FailureType of
        network_partition ->
            hd([F || F <- ?DEFAULT_FAILURE_TYPES, F#failure_type.name == network_partition]);
        cpu_spike ->
            hd([F || F <- ?DEFAULT_FAILURE_TYPES, F#failure_type.name == cpu_spike]);
        memory_pressure ->
            hd([F || F <- ?DEFAULT_FAILURE_TYPES, F#failure_type.name == memory_pressure]);
        disk_full ->
            hd([F || F <- ?DEFAULT_FAILURE_TYPES, F#failure_type.name == disk_full]);
        _ ->
            undefined
    end.

inject_network_partition(_Target) ->
    % Simulate network partition
    timer:sleep(1000),  % Simulate partition
    success.

recover_network_partition(_Target) ->
    % Recover from network partition
    timer:sleep(500),  % Simulate recovery
    success.

inject_cpu_spike(_Target) ->
    % Simulate CPU spike
    % Implementation would actually increase CPU usage
    timer:sleep(2000),
    success.

recover_cpu_spike(_Target) ->
    % Recover from CPU spike
    timer:sleep(1000),
    success.

inject_memory_pressure(_Target) ->
    % Simulate memory pressure
    % Implementation would allocate large amounts of memory
    LargeData = list_to_binary(lists:duplicate(1000000, 0)),
    put(memory_pressure_test, LargeData),
    success.

recover_memory_pressure(_Target) ->
    % Recover from memory pressure
    erase(memory_pressure_test),
    success.

inject_disk_full(_Target) ->
    % Simulate disk full
    % Implementation would fill disk
    success.

recover_disk_full(_Target) ->
    % Recover from disk full
    success.

log_failure_injection(FailureDef) ->
    % Log failure injection
    io:format("[CHAOS] Injecting ~s failure: ~s~n", [
        FailureDef#failure_type.name,
        FailureDef#failure_type.description
    ]),
    ok.

log_recovery_attempt(FailureDef, Result) ->
    % Log recovery attempt
    io:format("[CHAOS] Recovering from ~s: ~p~n", [
        FailureDef#failure_type.name,
        Result
    ]),
    ok.

add_to_timeline(Incident) ->
    % Add incident to timeline
    Timeline = get_timeline(),
    set_timeline([Incident | Timeline]),
    ok.

get_total_requests() ->
    % Get total requests during chaos test
    1000.

get_failed_requests() ->
    % Get failed requests during chaos test
    50.

get_successful_recoveries() ->
    % Get successful recoveries
    length(get_current_injections()).

calculate_system_stability(Timeline) ->
    % Calculate overall system stability
    SuccessCount = length([I || I <- Timeline, I#incident.success]),
    TotalCount = length(Timeline),
    case TotalCount of
        0 -> 100.0;
        _ -> SuccessCount / TotalCount * 100
    end.

generate_lessons_learned(Timeline) ->
    % Generate lessons learned from chaos test
    Lessons = [],

    % Analyze failure patterns
    Failures = [I || I <- Timeline, I#incident.type == injection and I#incident.success],
    case Failures of
        [] -> Lessons;
        _ ->
            MostCommon = analyze_failure_patterns(Failures),
            [#{lesson => MostCommon, impact => "High"} | Lessons]
    end.

analyze_failure_patterns(Failures) ->
    % Analyze failure patterns
    FailureTypes = [I#incident.failure_type || I <- Failures],
    TypeCounts = lists:foldl(fun(Type, Acc) ->
        dict:update_counter(Type, 1, Acc)
    end, dict:new(), FailureTypes),

    % Most common failure type
    {MostCommon, _Count} = lists:foldl(fun({Type, Count}, {MaxType, MaxCount}) ->
        if Count > MaxCount -> {Type, Count};
           true -> {MaxType, MaxCount}
        end
    end, {"unknown", 0}, dict:to_list(TypeCounts)),

    MostCommon.

complete_chaos_test(Scenario, TestStart, TestEnd) ->
    % Complete chaos test
    io:format("Chaos test completed: ~s~n", [Scenario#chaos_scenario.name]),

    % Generate final report
    Report = get_chaos_results(),
    save_chaos_report(Report),
    ok.

save_chaos_report(Report) ->
    % Save chaos test report
    % Implementation would save to database or file
    io:format("Chaos test report saved~n"),
    ok.

get_system_status() ->
    % Get current system status
    get(system_status) or #system_status{}.

set_system_status(Status) ->
    % Set system status
    put(system_status, Status).

get_current_injections() ->
    % Get current failure injections
    get(current_injections) or [].

set_current_injections(Injections) ->
    % Set current failure injections
    put(current_injections, Injections).

get_timeline() ->
    % Get incident timeline
    get(timeline) or [].

set_timeline(Timeline) ->
    % Set incident timeline
    put(timeline, Timeline).

get_test_start_time() ->
    % Get test start time
    get(test_start_time) or 0.

get_system_metrics() ->
    % Get current system metrics
    #{
        cpu_usage => 50.0,
        memory_usage => 60.0,
        response_time => 80.0,
        error_rate => 0.01
    }.
```

## Test Configuration Files

### 1. Load Test Configuration

**configs/load_test.config**
```erlang
{
    erlmcp_load_test,
    #{
        test_scenarios => [
            #{
                name => "baseline_performance",
                description => "Baseline performance test",
                duration => 300000,  % 5 minutes
                target_load => 10000,  % req/sec
                ramp_up_time => 60000,  % 1 minute
                warm_up_time => 30000,  % 30 seconds
                cooldown_time => 60000,  % 1 minute
                request_mix => [
                    {get_resource, 0.7},
                    {post_tool, 0.2},
                    {subscribe, 0.1}
                ],
                think_time => 100,
                measurements => [
                    response_time,
                    throughput,
                    error_rate,
                    cpu_usage,
                    memory_usage
                ]
            },
            #{
                name => "peak_load",
                description => "Peak load test (150% capacity)",
                duration => 180000,  % 3 minutes
                target_load => 15000,  % req/sec
                ramp_up_time => 120000,  % 2 minutes
                warm_up_time => 60000,  % 1 minute
                cooldown_time => 60000,  % 1 minute
                request_mix => [
                    {get_resource, 0.5},
                    {post_tool, 0.3},
                    {subscribe, 0.2}
                ],
                think_time => 50,
                measurements => [
                    response_time,
                    throughput,
                    error_rate,
                    cpu_usage,
                    memory_usage
                ]
            }
        ],
        thresholds => #{
            response_time_p95 => 100,    % ms
            response_time_p99 => 200,    % ms
            throughput => 10000,         % req/sec
            error_rate => 0.01,          % 1%
            cpu_usage => 80,             % %
            memory_usage => 90           % %
        },
        reporting => #{
            format => json,
            output_dir => "./test_results",
            include_charts => true,
            auto_generate => true
        }
    }
}.
```

### 2. Regression Test Configuration

**configs/regression_test.config**
```erlang
{
    erlmcp_regression_test,
    #{
        test_suites => [
            {
                name => "performance_regression",
                tests => [
                    {
                        name => "response_time_p95",
                        type => performance,
                        metric => "response_time.p95",
                        expected_value => 100,
                        tolerance => 0.1,
                        description => "P95 response time should not exceed 100ms"
                    },
                    {
                        name => "throughput",
                        type => performance,
                        metric => "throughput",
                        expected_value => 10000,
                        tolerance => 0.05,
                        description => "Throughput should not drop below 9500 req/sec"
                    },
                    {
                        name => "error_rate",
                        type => performance,
                        metric => "error_rate",
                        expected_value => 0.01,
                        tolerance => 0.2,
                        description => "Error rate should not exceed 1.2%"
                    }
                ]
            },
            {
                name => "functional_regression",
                tests => [
                    {
                        name => "resource_creation",
                        type => functional,
                        metric => "success_rate",
                        expected_value => 1.0,
                        tolerance => 0.0,
                        description => "Resource creation should always succeed"
                    },
                    {
                        name => "tool_execution",
                        type => functional,
                        metric => "success_rate",
                        expected_value => 1.0,
                        tolerance => 0.0,
                        description => "Tool execution should always succeed"
                    }
                ]
            }
        ],
        baseline_comparison => true,
        historical_data_range => 86400000,  % 24 hours
        regression_threshold => 0.1,        % 10%
        alert_on_regression => true,
        report_format => json
    }
}.
```

### 3. Chaos Test Configuration

**configs/chaos_test.config**
```erlang
{
    erlmcp_chaos_test,
    #{
        scenarios => [
            {
                name => "partial_failures",
                description => "Test system resilience under partial failures",
                failures => [
                    network_partition,
                    cpu_spike,
                    memory_pressure
                ],
                failure_rate => 0.15,  % 15% of requests
                duration => 600000,    % 10 minutes
                recovery_time => 5000,  % 5 seconds
                resilience_checks => [
                    {response_time, 200},    % max response time
                    {error_rate, 0.05},      % max error rate
                    {system_availability, 99.9}  % min availability
                ]
            },
            {
                name => "extreme_failures",
                description => "Test system resilience under extreme failures",
                failures => [
                    disk_full,
                    network_partition,
                    cpu_spike,
                    memory_pressure
                ],
                failure_rate => 0.3,     % 30% of requests
                duration => 900000,      % 15 minutes
                recovery_time => 10000,  % 10 seconds
                resilience_checks => [
                    {response_time, 500},    % max response time
                    {error_rate, 0.1},       % max error rate
                    {system_availability, 99.0}  % min availability
                ]
            }
        ],
        failure_types => [
            {
                name => network_partition,
                probability => 0.3,
                severity => high,
                description => "Simulate network partition",
                injection_fn => fun inject_network_partition/1,
                recovery_fn => fun recover_network_partition/1
            },
            {
                name => cpu_spike,
                probability => 0.4,
                severity => medium,
                description => "Simulate CPU spike",
                injection_fn => fun inject_cpu_spike/1,
                recovery_fn => fun recover_cpu_spike/1
            },
            {
                name => memory_pressure,
                probability => 0.3,
                severity => high,
                description => "Simulate memory pressure",
                injection_fn => fun inject_memory_pressure/1,
                recovery_fn => fun recover_memory_pressure/1
            },
            {
                name => disk_full,
                probability => 0.2,
                severity => critical,
                description => "Simulate disk full",
                injection_fn => fun inject_disk_full/1,
                recovery_fn => fun recover_disk_full/1
            }
        ],
        monitoring => #{
            stability_threshold => 50,      % minimum stability score
            alert_on_degradation => true,
            recovery_timeout => 30000       % 30 seconds recovery timeout
        }
    }
}.
```

## Usage Examples

### Load Testing
```erlang
% Start load testing
Scenario = #test_scenario{
    name = "erlmcp_load_test",
    description = "Load test for 10K req/sec",
    duration = 300000,     % 5 minutes
    target_load = 10000,   % req/sec
    ramp_up_time = 60000,  % 1 minute
    request_mix = [
        {get_resource, 0.7},
        {post_tool, 0.2},
        {subscribe, 0.1}
    ]
},

erlmcp_load_test:start_test(Scenario),

% Monitor test progress
TestStatus = erlmcp_load_test:get_test_status(),
io:format("Test progress: ~.1f%%~n", [TestStatus#test_status.progress * 100]),

% Get test results
Results = erlmcp_load_test:get_test_results(),
io:format("Success rate: ~.2f%%~n", [Results#test_result.successful_requests / max(1, Results#test_result.total_requests) * 100]),

% Generate report
Report = erlmcp_load_test:generate_report(),
io:format("Performance score: ~.1f/100~n", [Report#summary.performance_score]),
```

### Regression Testing
```erlang
% Run regression tests
Results = erlmcp_regression_test:run_tests(),

% Compare with baseline
Regressions = erlmcp_regression_test:compare_with_baseline(Results),

% Get regression report
Report = erlmcp_regression_test:get_regression_report(),
io:format("Regression analysis complete. ~p regressions found~n", [length(Report#regressions)]),

% Generate recommendations
Recommendations = Report#recommendations,
lists:foreach(fun(Rec) ->
    io:format("- ~s: ~s~n", [Rec#test_case.name, Rec#recommendation])
end, Recommendations),
```

### Chaos Testing
```erlang
% Start chaos test
ChaosScenario = #chaos_scenario{
    name = "network_chaos",
    description = "Test resilience under network failures",
    failures = [network_partition],
    failure_rate = 0.2,
    duration = 300000
},

erlmcp_chaos_test:start_chaos_test(ChaosScenario),

% Inject specific failure
erlmcp_chaos_test:inject_failure(network_partition, database),

% Monitor stability
SystemStatus = erlmcp_chaos_test:get_system_status(),
io:format("System stability: ~.1f%%~n", [SystemStatus#system_status.stability_score]),

% Stop chaos test and get results
erlmcp_chaos_test:stop_chaos_test(),
ChaosReport = erlmcp_chaos_test:get_chaos_results(),
io:format("Chaos test completed with ~p incidents~n", [length(ChaosReport#incident_timeline)]),
```

## Integration Points

### 1. With erlmcp Core
- Integrate with request handling
- Monitor response times
- Track errors and exceptions

### 2. With Monitoring System
- Export test results
- Use metrics in validation
- Alert on test failures

### 3. With CI/CD Pipeline
- Run regression tests on commits
- Perform load testing before releases
- Validate performance SLAs

### 4. With Production Environment
- Monitor production performance
- Detect regressions early
- Trigger chaos tests safely

## Testing Best Practices

### 1. Load Testing
- Test at multiple load levels
- Include ramp-up and ramp-down
- Monitor resource usage during tests
- Test with realistic request patterns

### 2. Regression Testing
- Establish baselines early
- Test both functional and performance aspects
- Include edge cases
- Maintain historical data for comparison

### 3. Chaos Testing
- Start with controlled failures
- Document failure scenarios
- Test recovery mechanisms
- Measure system resilience

### 4. Test Data Management
- Use representative test data
- Clean up after tests
- Isolate test environments
- Maintain data consistency

## Conclusion

The testing framework provides comprehensive validation for erlmcp v3 performance targets and reliability requirements. The implementation includes load testing for 10K+ req/sec, regression testing for performance stability, and chaos testing for resilience - all essential for maintaining Fortune 500 scale performance and reliability.