-module(erlmcp_dashboard_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    erlmcp_metrics_server:start(),
    ok.

cleanup(_) ->
    erlmcp_metrics_server:stop(),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

metrics_server_basic_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Initialize metrics", fun test_init/0},
        {"Record messages", fun test_record_messages/0},
        {"Record errors", fun test_record_errors/0},
        {"Record latency", fun test_record_latency/0},
        {"Concurrent connections", fun test_concurrent_connections/0},
        {"Get metrics", fun test_get_metrics/0}
    ]}.

test_init() ->
    % Metrics server should already be started
    Metrics = erlmcp_metrics_server:get_metrics(),
    ?assertEqual(0, maps:get(concurrent_connections, Metrics)),
    ?assertEqual(0, maps:get(total_messages, Metrics)),
    ?assertEqual(0, maps:get(total_errors, Metrics)).

test_record_messages() ->
    erlmcp_metrics_server:record_message(1),
    erlmcp_metrics_server:record_message(5),
    Metrics = erlmcp_metrics_server:get_metrics(),
    TotalMessages = maps:get(total_messages, Metrics),
    ?assertEqual(true, TotalMessages >= 6).

test_record_errors() ->
    erlmcp_metrics_server:record_error(),
    erlmcp_metrics_server:record_error(),
    erlmcp_metrics_server:record_error(),
    Metrics = erlmcp_metrics_server:get_metrics(),
    TotalErrors = maps:get(total_errors, Metrics),
    ?assertEqual(true, TotalErrors >= 3).

test_record_latency() ->
    erlmcp_metrics_server:record_latency(10.5),
    erlmcp_metrics_server:record_latency(20.3),
    erlmcp_metrics_server:record_latency(15.7),
    Metrics = erlmcp_metrics_server:get_metrics(),
    LatencyStats = maps:get(latency_stats, Metrics),
    ?assertEqual(true, maps:is_key(p50, LatencyStats)),
    ?assertEqual(true, maps:is_key(p99, LatencyStats)).

test_concurrent_connections() ->
    erlmcp_metrics_server:increment_connections(100),
    Connections1 = erlmcp_metrics_server:get_concurrent_connections(),
    ?assertEqual(100, Connections1),

    erlmcp_metrics_server:increment_connections(50),
    Connections2 = erlmcp_metrics_server:get_concurrent_connections(),
    ?assertEqual(150, Connections2),

    erlmcp_metrics_server:decrement_connections(75),
    Connections3 = erlmcp_metrics_server:get_concurrent_connections(),
    ?assertEqual(75, Connections3).

test_get_metrics() ->
    erlmcp_metrics_server:reset_metrics(),
    erlmcp_metrics_server:increment_connections(1000),
    erlmcp_metrics_server:record_message(5000),
    erlmcp_metrics_server:record_error(),
    erlmcp_metrics_server:record_latency(25.5),

    Metrics = erlmcp_metrics_server:get_metrics(),

    % Verify all expected keys are present
    ?assertEqual(true, maps:is_key(timestamp, Metrics)),
    ?assertEqual(true, maps:is_key(uptime_ms, Metrics)),
    ?assertEqual(true, maps:is_key(concurrent_connections, Metrics)),
    ?assertEqual(true, maps:is_key(total_messages, Metrics)),
    ?assertEqual(true, maps:is_key(total_errors, Metrics)),
    ?assertEqual(true, maps:is_key(message_rate_per_sec, Metrics)),
    ?assertEqual(true, maps:is_key(latency_stats, Metrics)),
    ?assertEqual(true, maps:is_key(system_metrics, Metrics)),

    % Verify values
    ?assertEqual(1000, maps:get(concurrent_connections, Metrics)),
    ?assertEqual(true, maps:get(total_messages, Metrics) >= 5000),
    ?assertEqual(true, maps:get(total_errors, Metrics) >= 1).

%%====================================================================
%% Concurrency Tests
%%====================================================================

concurrent_message_recording_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Concurrent message recording", fun test_concurrent_recording/0}
    ]}.

test_concurrent_recording() ->
    % Spawn 100 processes, each recording 10 messages
    Workers = [spawn_link(fun() ->
        lists:foreach(
            fun(_) -> erlmcp_metrics_server:record_message(1) end,
            lists:seq(1, 10)
        )
    end) || _ <- lists:seq(1, 100)],

    % Wait for all workers
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        end
    end, Workers),

    % Give a moment for all messages to be recorded
    timer:sleep(100),

    Metrics = erlmcp_metrics_server:get_metrics(),
    TotalMessages = maps:get(total_messages, Metrics),
    % Should have ~1000 messages (100 workers * 10 messages)
    ?assertEqual(true, TotalMessages >= 900).

%%====================================================================
%% Stress Test
%%====================================================================

stress_test_test_() ->
    {timeout, 60, {setup, fun setup/0, fun cleanup/1, [
        {"100K concurrent connections", fun test_100k_concurrent/0}
    ]}}.

test_100k_concurrent() ->
    % Increment to 100K concurrent connections
    erlmcp_metrics_server:increment_connections(100000),
    Connections = erlmcp_metrics_server:get_concurrent_connections(),
    ?assertEqual(100000, Connections),

    % Start traffic for these connections
    spawn(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_metrics_server:record_message(1),
            case rand:uniform(100) > 95 of
                true -> erlmcp_metrics_server:record_error();
                false -> ok
            end,
            Latency = rand:uniform(100) + 5,
            erlmcp_metrics_server:record_latency(Latency)
        end, lists:seq(1, 1000))
    end),

    % Give time for metrics to accumulate
    timer:sleep(1000),

    Metrics = erlmcp_metrics_server:get_metrics(),

    % Verify high connection count
    ?assertEqual(100000, maps:get(concurrent_connections, Metrics)),

    % Verify messages were recorded
    TotalMessages = maps:get(total_messages, Metrics),
    ?assertEqual(true, TotalMessages >= 1000),

    % Verify latency stats exist
    LatencyStats = maps:get(latency_stats, Metrics),
    ?assertEqual(true, maps:get(p50, LatencyStats) > 0).

%%====================================================================
%% Latency Distribution Tests
%%====================================================================

latency_percentile_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Latency percentiles", fun test_latency_percentiles/0}
    ]}.

test_latency_percentiles() ->
    erlmcp_metrics_server:reset_metrics(),

    % Record 100 known latency values
    lists:foreach(fun(Value) ->
        erlmcp_metrics_server:record_latency(Value)
    end, lists:seq(1, 100)),

    timer:sleep(100),

    Metrics = erlmcp_metrics_server:get_metrics(),
    LatencyStats = maps:get(latency_stats, Metrics),

    P50 = maps:get(p50, LatencyStats),
    P95 = maps:get(p95, LatencyStats),
    P99 = maps:get(p99, LatencyStats),

    % P50 should be around 50
    ?assertEqual(true, P50 >= 40 andalso P50 =< 60),

    % P95 should be around 95
    ?assertEqual(true, P95 >= 85 andalso P95 =< 100),

    % P99 should be around 99
    ?assertEqual(true, P99 >= 90 andalso P99 =< 100),

    % Min and Max
    Min = maps:get(min, LatencyStats),
    Max = maps:get(max, LatencyStats),
    ?assertEqual(1, Min),
    ?assertEqual(100, Max).

%%====================================================================
%% HTTP Handler Tests
%%====================================================================

http_metrics_endpoint_test_() ->
    {setup,
        fun() ->
            erlmcp_metrics_server:start(),
            erlmcp_metrics_server:increment_connections(1000),
            erlmcp_metrics_server:record_message(5000),
            erlmcp_metrics_server:record_latency(25.5),
            ok
        end,
        fun(_) -> erlmcp_metrics_server:stop() end,
        [
            {"JSON encoding", fun test_json_encoding/0}
        ]
    }.

test_json_encoding() ->
    Metrics = erlmcp_metrics_server:get_metrics(),

    % Test that metrics can be encoded to JSON
    Json = jsx:encode(erlmcp_metrics_http:metrics_to_json(Metrics)),
    ?assertEqual(true, is_binary(Json)),
    ?assertEqual(true, byte_size(Json) > 0),

    % Decode back to verify structure
    Decoded = jsx:decode(Json),
    ?assertEqual(true, is_map(Decoded)),

    % Verify key fields are present
    ?assertEqual(true, maps:is_key(<<"concurrent_connections">>, Decoded)),
    ?assertEqual(true, maps:is_key(<<"total_messages">>, Decoded)).

%%====================================================================
%% System Metrics Tests
%%====================================================================

system_metrics_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"System metrics collection", fun test_system_metrics/0}
    ]}.

test_system_metrics() ->
    Metrics = erlmcp_metrics_server:get_metrics(),
    SystemMetrics = maps:get(system_metrics, Metrics),

    % Verify system metrics keys
    ?assertEqual(true, maps:is_key(memory, SystemMetrics)),
    ?assertEqual(true, maps:is_key(process_count, SystemMetrics)),
    ?assertEqual(true, maps:is_key(port_count, SystemMetrics)),
    ?assertEqual(true, maps:is_key(schedulers, SystemMetrics)),

    % Verify values are reasonable
    ProcessCount = maps:get(process_count, SystemMetrics),
    ?assertEqual(true, ProcessCount > 0),

    Schedulers = maps:get(schedulers, SystemMetrics),
    ?assertEqual(true, Schedulers > 0).

%%====================================================================
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Reset metrics", fun test_reset/0},
        {"Zero latency samples", fun test_zero_latency_samples/0},
        {"Decrement below zero", fun test_decrement_below_zero/0}
    ]}.

test_reset() ->
    erlmcp_metrics_server:increment_connections(500),
    erlmcp_metrics_server:record_message(1000),
    erlmcp_metrics_server:reset_metrics(),

    Metrics = erlmcp_metrics_server:get_metrics(),
    ?assertEqual(0, maps:get(concurrent_connections, Metrics)),
    ?assertEqual(0, maps:get(total_messages, Metrics)).

test_zero_latency_samples() ->
    erlmcp_metrics_server:reset_metrics(),
    Metrics = erlmcp_metrics_server:get_metrics(),
    LatencyStats = maps:get(latency_stats, Metrics),

    % When no samples, latency stats should be zeros
    ?assertEqual(0, maps:get(p50, LatencyStats)),
    ?assertEqual(0, maps:get(p99, LatencyStats)).

test_decrement_below_zero() ->
    erlmcp_metrics_server:reset_metrics(),
    erlmcp_metrics_server:decrement_connections(1000),

    Connections = erlmcp_metrics_server:get_concurrent_connections(),
    % Should be 0, not negative
    ?assertEqual(0, Connections).
