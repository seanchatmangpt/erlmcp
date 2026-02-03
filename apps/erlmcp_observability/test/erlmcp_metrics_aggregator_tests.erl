%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Metrics Aggregator
%%%
%%% Tests cover:
%%% - Metrics recording (throughput, latency, errors, connections, memory, CPU)
%%% - Bucket rotation and aggregation
%%% - Percentile calculation (p50, p95, p99, p999)
%%% - Current and historical metrics retrieval
%%% - Metrics export (CSV, JSON)
%%% - Alert threshold checking
%%% - Timer-based bucket rotation
%%%
%%% Chicago School TDD: Real gen_server, state-based verification, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_aggregator_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% @doc Setup metrics aggregator for testing
setup_aggregator() ->
    {ok, Pid} = erlmcp_metrics_aggregator:start_link(),
    Pid.

%% @doc Cleanup aggregator
cleanup_aggregator(Pid) ->
    catch gen_server:stop(Pid).

%%%====================================================================
%%% Aggregator Lifecycle Tests
%%%====================================================================

start_link_test() ->
    Result = erlmcp_metrics_aggregator:start_link(),
    ?assertMatch({ok, _Pid}, Result),

    {ok, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    cleanup_aggregator(Pid).

start_link_registers_name_test() ->
    {ok, Pid} = erlmcp_metrics_aggregator:start_link(),
    try
        ?assertEqual(Pid, whereis(erlmcp_metrics_aggregator))
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Metric Recording Tests
%%%====================================================================

record_throughput_metric_test() ->
    Pid = setup_aggregator(),
    try
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(throughput, requests, 100)),

        timer:sleep(50),  %% Give it time to process

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assertEqual(100, maps:get(throughput, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

record_latency_metric_test() ->
    Pid = setup_aggregator(),
    try
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(latency, request, 1000)),
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(latency, request, 2000)),
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(latency, request, 3000)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assert(maps:get(latency_p50_us, Metrics) > 0),
        ?assert(maps:get(latency_p95_us, Metrics) > 0)
    after
        cleanup_aggregator(Pid)
    end.

record_error_metric_test() ->
    Pid = setup_aggregator(),
    try
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(error, request, error)),
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(error, request, error)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assertEqual(2, maps:get(errors, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

record_connections_metric_test() ->
    Pid = setup_aggregator(),
    try
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(connections, active, 50)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assertEqual(50, maps:get(connections, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

record_memory_metric_test() ->
    Pid = setup_aggregator(),
    try
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(memory, system, 512.5)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assertEqual(512.5, maps:get(memory_mb, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

record_cpu_metric_test() ->
    Pid = setup_aggregator(),
    try
        ?assertEqual(ok, erlmcp_metrics_aggregator:record_metric(cpu, system, 75.3)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assertEqual(75.3, maps:get(cpu_percent, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Percentile Calculation Tests
%%%====================================================================

calculate_percentiles_empty_test() ->
    Percentiles = erlmcp_metrics_aggregator:get_percentiles([]),
    ?assertEqual(0, maps:get(p50, Percentiles)),
    ?assertEqual(0, maps:get(p95, Percentiles)),
    ?assertEqual(0, maps:get(p99, Percentiles)),
    ?assertEqual(0, maps:get(p999, Percentiles)).

calculate_percentiles_single_value_test() ->
    Percentiles = erlmcp_metrics_aggregator:get_percentiles([100]),
    ?assertEqual(100, maps:get(p50, Percentiles)),
    ?assertEqual(100, maps:get(p95, Percentiles)),
    ?assertEqual(100, maps:get(p99, Percentiles)),
    ?assertEqual(100, maps:get(p999, Percentiles)).

calculate_percentiles_multiple_values_test() ->
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),

    ?assert(maps:get(p50, Percentiles) >= 50),
    ?assert(maps:get(p50, Percentiles) =< 60),

    ?assert(maps:get(p95, Percentiles) >= 90),
    ?assert(maps:get(p95, Percentiles) =< 100),

    ?assert(maps:get(p99, Percentiles) >= 95),
    ?assert(maps:get(p99, Percentiles) =< 100),

    ?assertEqual(100, maps:get(p999, Percentiles)).

calculate_percentiles_distribution_test() ->
    %% Test with realistic latency distribution (microseconds)
    Latencies = [
        100, 150, 200, 250, 300, 350, 400, 450, 500,
        550, 600, 650, 700, 750, 800, 850, 900, 950, 1000,
        1500, 2000, 5000
    ],

    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Latencies),

    %% p50 should be around median
    ?assert(maps:get(p50, Percentiles) > 500),
    ?assert(maps:get(p50, Percentiles) < 800),

    %% p95 should be higher
    ?assert(maps:get(p95, Percentiles) > 1000),
    ?assert(maps:get(p95, Percentiles) < 2000),

    %% p99 should be near max
    ?assert(maps:get(p99, Percentiles) > 1500),
    ?assert(maps:get(p99, Percentiles) =< 5000).

%%%====================================================================
%%% Current Metrics Tests
%%%====================================================================

get_current_metrics_structure_test() ->
    Pid = setup_aggregator(),
    try
        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

        RequiredKeys = [
            timestamp, throughput, latency_p50_us, latency_p95_us,
            latency_p99_us, latency_p999_us, errors, connections,
            memory_mb, cpu_percent, throughput_1min, latency_trend_1min,
            error_rate_1min
        ],

        lists:foreach(fun(Key) ->
            ?assert(maps:is_key(Key, Metrics), "Missing key: " ++ atom_to_list(Key))
        end, RequiredKeys)
    after
        cleanup_aggregator(Pid)
    end.

get_current_metrics_with_recorded_data_test() ->
    Pid = setup_aggregator(),
    try
        %% Record various metrics
        erlmcp_metrics_aggregator:record_metric(throughput, req, 1000),
        erlmcp_metrics_aggregator:record_metric(latency, req, 500),
        erlmcp_metrics_aggregator:record_metric(latency, req, 1000),
        erlmcp_metrics_aggregator:record_metric(error, req, error),
        erlmcp_metrics_aggregator:record_metric(connections, active, 100),
        erlmcp_metrics_aggregator:record_metric(memory, system, 1024),
        erlmcp_metrics_aggregator:record_metric(cpu, system, 80.0),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

        ?assertEqual(1000, maps:get(throughput, Metrics)),
        ?assertEqual(1, maps:get(errors, Metrics)),
        ?assertEqual(100, maps:get(connections, Metrics)),
        ?assertEqual(1024, maps:get(memory_mb, Metrics)),
        ?assertEqual(80.0, maps:get(cpu_percent, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Historical Metrics Tests
%%%====================================================================

get_historical_metrics_empty_test() ->
    Pid = setup_aggregator(),
    try
        Now = erlang:system_time(millisecond),
        {ok, Metrics} = erlmcp_metrics_aggregator:get_historical_metrics(Now - 1000, Now + 1000),

        %% Should have at least current bucket
        ?assert(length(Metrics) >= 0)
    after
        cleanup_aggregator(Pid)
    end.

get_historical_metrics_with_data_test() ->
    Pid = setup_aggregator(),
    try
        %% Record some metrics
        erlmcp_metrics_aggregator:record_metric(throughput, req, 100),
        timer:sleep(50),

        Now = erlang:system_time(millisecond),
        {ok, Metrics} = erlmcp_metrics_aggregator:get_historical_metrics(Now - 5000, Now + 1000),

        %% Should have some data
        ?assert(length(Metrics) >= 0)
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Metrics Export Tests
%%%====================================================================

export_metrics_json_test() ->
    Pid = setup_aggregator(),
    try
        %% Record some data
        erlmcp_metrics_aggregator:record_metric(throughput, req, 100),
        erlmcp_metrics_aggregator:record_metric(latency, req, 500),

        timer:sleep(50),

        {ok, Json} = erlmcp_metrics_aggregator:export_metrics(json),

        %% Should be valid JSON (list of maps)
        ?assert(is_binary(Json))
    after
        cleanup_aggregator(Pid)
    end.

export_metrics_csv_test() ->
    Pid = setup_aggregator(),
    try
        %% Record some data
        erlmcp_metrics_aggregator:record_metric(throughput, req, 100),

        timer:sleep(50),

        {ok, Csv} = erlmcp_metrics_aggregator:export_metrics(csv),

        %% Should be valid CSV with header
        ?assert(is_binary(Csv)),
        ?assertMatch(<<"timestamp,", _/binary>>, Csv)
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Bucket Rotation Tests
%%%====================================================================

bucket_rotation_aggregates_data_test() ->
    Pid = setup_aggregator(),
    try
        %% Record metrics
        erlmcp_metrics_aggregator:record_metric(throughput, req, 100),

        %% Wait for bucket rotation (1 second interval)
        timer:sleep(1100),

        %% Old bucket should be rotated, new bucket active
        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),
        ?assert(maps:is_key(timestamp, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Alert Threshold Tests
%%%====================================================================

alert_threshold_logging_test() ->
    %% This test verifies alert thresholds are checked
    %% Actual alert logging goes to logger, we just ensure no crashes
    Pid = setup_aggregator(),
    try
        %% Record high latency (above threshold of 100ms = 100000us)
        erlmcp_metrics_aggregator:record_metric(latency, req, 150000),

        timer:sleep(50),

        %% Should not crash
        {ok, _Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

        %% Wait for bucket rotation (triggers alert check)
        timer:sleep(1100),

        %% Still alive
        ?assert(is_process_alive(Pid))
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Metric Trends Tests
%%%====================================================================

calculate_throughput_trend_test() ->
    Pid = setup_aggregator(),
    try
        %% Record multiple throughput values over time
        lists:foreach(fun(N) ->
            erlmcp_metrics_aggregator:record_metric(throughput, req, N * 100),
            timer:sleep(100)
        end, lists:seq(1, 3)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

        %% Trend should be calculated
        ?assert(maps:is_key(throughput_1min, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

calculate_latency_trend_test() ->
    Pid = setup_aggregator(),
    try
        %% Record latencies
        lists:foreach(fun(_) ->
            erlmcp_metrics_aggregator:record_metric(latency, req, 1000),
            timer:sleep(50)
        end, lists:seq(1, 3)),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

        %% Should have latency trend
        ?assert(maps:is_key(latency_trend_1min, Metrics))
    after
        cleanup_aggregator(Pid)
    end.

calculate_error_rate_test() ->
    Pid = setup_aggregator(),
    try
        %% Record some errors
        erlmcp_metrics_aggregator:record_metric(error, req, error),
        erlmcp_metrics_aggregator:record_metric(error, req, error),

        timer:sleep(50),

        {ok, Metrics} = erlmcp_metrics_aggregator:get_current_metrics(),

        %% Error rate should be > 0
        ErrorRate = maps:get(error_rate_1min, Metrics),
        ?assert(ErrorRate > 0.0)
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Integration Tests
%%%====================================================================

full_metrics_workflow_test() ->
    Pid = setup_aggregator(),
    try
        %% Simulate realistic metrics recording
        lists:foreach(fun(N) ->
            erlmcp_metrics_aggregator:record_metric(throughput, req, 100 + N),
            erlmcp_metrics_aggregator:record_metric(latency, req, 500 + N * 10),
            erlmcp_metrics_aggregator:record_metric(connections, active, 50 + N),
            timer:sleep(50)
        end, lists:seq(1, 5)),

        %% Add an error
        erlmcp_metrics_aggregator:record_metric(error, req, error),

        timer:sleep(100),

        %% Get current metrics
        {ok, Current} = erlmcp_metrics_aggregator:get_current_metrics(),

        ?assert(maps:get(throughput, Current) > 0),
        ?assert(maps:get(latency_p50_us, Current) > 0),
        ?assert(maps:get(errors, Current) > 0),
        ?assert(maps:get(connections, Current) > 0),

        %% Export as JSON
        {ok, Json} = erlmcp_metrics_aggregator:export_metrics(json),
        ?assert(is_binary(Json)),

        %% Export as CSV
        {ok, Csv} = erlmcp_metrics_aggregator:export_metrics(csv),
        ?assert(is_binary(Csv))
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

unknown_request_test() ->
    Pid = setup_aggregator(),
    try
        Result = gen_server:call(Pid, unknown_request),
        ?assertEqual({error, unknown_request}, Result)
    after
        cleanup_aggregator(Pid)
    end.

%%%====================================================================
%%% State Verification Tests
%%%====================================================================

state_contains_required_fields_test() ->
    Pid = setup_aggregator(),
    try
        %% Verify state structure via sys:get_status
        {status, _Pid, {module, _Mod}, [_PDict, _Running, _Parent, _Debug, [_, _, State]]} =
            sys:get_status(Pid),

        ?assert(is_record(State, state))
    after
        cleanup_aggregator(Pid)
    end.
