%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_counters_tests - Lock-Free Counters Tests
%%%
%%% Chicago School TDD:
%%% - NO MOCKS: Test real counter operations
%%% - Test ALL observable behavior
%%% - Test concurrency and race conditions
%%% - Test Prometheus export format
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_counters_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

counters_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_init/0,
         fun test_inc_requests/0,
         fun test_inc_success/0,
         fun test_inc_error/0,
         fun test_connections/0,
         fun test_tools_executed/0,
         fun test_resources_read/0,
         fun test_prompts_used/0,
         fun test_bytes_sent/0,
         fun test_bytes_received/0,
         fun test_get_all/0,
         fun test_reset/0,
         fun test_prometheus_format/0,
         fun test_concurrent_increments/0,
         fun test_high_frequency_updates/0
     ]}.

setup() ->
    erlmcp_counters:init(),
    erlmcp_counters:reset(),
    ok.

cleanup(_) ->
    erlmcp_counters:reset(),
    ok.

%%====================================================================
%% Initialization Tests
%%====================================================================

test_init() ->
    ?_test(begin
        % Test that all counters start at zero
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(0, maps:get(requests_total, Metrics)),
        ?assertEqual(0, maps:get(requests_success, Metrics)),
        ?assertEqual(0, maps:get(requests_error, Metrics)),
        ?assertEqual(0, maps:get(connections_active, Metrics)),
        ?assertEqual(0, maps:get(connections_total, Metrics)),
        ?assertEqual(0, maps:get(tools_executed, Metrics)),
        ?assertEqual(0, maps:get(resources_read, Metrics)),
        ?assertEqual(0, maps:get(prompts_used, Metrics)),
        ?assertEqual(0, maps:get(bytes_sent, Metrics)),
        ?assertEqual(0, maps:get(bytes_received, Metrics))
    end).

%%====================================================================
%% Increment Tests
%%====================================================================

test_inc_requests() ->
    ?_test(begin
        % Increment requests counter
        erlmcp_counters:inc_requests(),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(requests_total, Metrics)),

        % Increment again
        erlmcp_counters:inc_requests(),
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(2, maps:get(requests_total, Metrics2))
    end).

test_inc_success() ->
    ?_test(begin
        % Increment success counter
        erlmcp_counters:inc_success(),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(requests_success, Metrics)),

        % Increment multiple times
        erlmcp_counters:inc_success(),
        erlmcp_counters:inc_success(),
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(3, maps:get(requests_success, Metrics2))
    end).

test_inc_error() ->
    ?_test(begin
        % Increment error counter
        erlmcp_counters:inc_error(),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(requests_error, Metrics)),

        % Increment multiple times
        erlmcp_counters:inc_error(),
        erlmcp_counters:inc_error(),
        erlmcp_counters:inc_error(),
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(4, maps:get(requests_error, Metrics2))
    end).

test_connections() ->
    ?_test(begin
        % Increment connections
        erlmcp_counters:inc_connections(),
        Metrics1 = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(connections_active, Metrics1)),
        ?assertEqual(1, maps:get(connections_total, Metrics1)),

        % Increment again
        erlmcp_counters:inc_connections(),
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(2, maps:get(connections_active, Metrics2)),
        ?assertEqual(2, maps:get(connections_total, Metrics2)),

        % Decrement active connections
        erlmcp_counters:dec_connections(),
        Metrics3 = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(connections_active, Metrics3)),
        ?assertEqual(2, maps:get(connections_total, Metrics3)),

        % Decrement again
        erlmcp_counters:dec_connections(),
        Metrics4 = erlmcp_counters:get_all(),
        ?assertEqual(0, maps:get(connections_active, Metrics4)),
        ?assertEqual(2, maps:get(connections_total, Metrics4))
    end).

test_tools_executed() ->
    ?_test(begin
        % Increment tools executed
        erlmcp_counters:inc_tools_executed(),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(tools_executed, Metrics)),

        % Increment multiple times
        [erlmcp_counters:inc_tools_executed() || _ <- lists:seq(1, 10)],
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(11, maps:get(tools_executed, Metrics2))
    end).

test_resources_read() ->
    ?_test(begin
        % Increment resources read
        erlmcp_counters:inc_resources_read(),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(resources_read, Metrics)),

        % Increment multiple times
        [erlmcp_counters:inc_resources_read() || _ <- lists:seq(1, 5)],
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(6, maps:get(resources_read, Metrics2))
    end).

test_prompts_used() ->
    ?_test(begin
        % Increment prompts used
        erlmcp_counters:inc_prompts_used(),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(prompts_used, Metrics)),

        % Increment multiple times
        [erlmcp_counters:inc_prompts_used() || _ <- lists:seq(1, 3)],
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(4, maps:get(prompts_used, Metrics2))
    end).

test_bytes_sent() ->
    ?_test(begin
        % Add bytes sent
        erlmcp_counters:add_bytes_sent(1024),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(1024, maps:get(bytes_sent, Metrics)),

        % Add more bytes
        erlmcp_counters:add_bytes_sent(2048),
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(3072, maps:get(bytes_sent, Metrics2))
    end).

test_bytes_received() ->
    ?_test(begin
        % Add bytes received
        erlmcp_counters:add_bytes_received(512),
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(512, maps:get(bytes_received, Metrics)),

        % Add more bytes
        erlmcp_counters:add_bytes_received(1024),
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(1536, maps:get(bytes_received, Metrics2))
    end).

%%====================================================================
%% Get All Tests
%%====================================================================

test_get_all() ->
    ?_test(begin
        % Increment various counters
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_success(),
        erlmcp_counters:inc_connections(),
        erlmcp_counters:inc_tools_executed(),
        erlmcp_counters:add_bytes_sent(1024),

        % Get all metrics
        Metrics = erlmcp_counters:get_all(),

        % Verify all counters are present
        ?assert(is_map(Metrics)),
        ?assertEqual(1, maps:get(requests_total, Metrics)),
        ?assertEqual(1, maps:get(requests_success, Metrics)),
        ?assertEqual(0, maps:get(requests_error, Metrics)),
        ?assertEqual(1, maps:get(connections_active, Metrics)),
        ?assertEqual(1, maps:get(connections_total, Metrics)),
        ?assertEqual(1, maps:get(tools_executed, Metrics)),
        ?assertEqual(0, maps:get(resources_read, Metrics)),
        ?assertEqual(0, maps:get(prompts_used, Metrics)),
        ?assertEqual(1024, maps:get(bytes_sent, Metrics)),
        ?assertEqual(0, maps:get(bytes_received, Metrics))
    end).

%%====================================================================
%% Reset Tests
%%====================================================================

test_reset() ->
    ?_test(begin
        % Increment various counters
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_success(),
        erlmcp_counters:inc_error(),
        erlmcp_counters:inc_connections(),

        % Verify counters are non-zero
        Metrics1 = erlmcp_counters:get_all(),
        ?assertEqual(1, maps:get(requests_total, Metrics1)),

        % Reset all counters
        erlmcp_counters:reset(),

        % Verify all counters are zero
        Metrics2 = erlmcp_counters:get_all(),
        ?assertEqual(0, maps:get(requests_total, Metrics2)),
        ?assertEqual(0, maps:get(requests_success, Metrics2)),
        ?assertEqual(0, maps:get(requests_error, Metrics2)),
        ?assertEqual(0, maps:get(connections_active, Metrics2))
    end).

%%====================================================================
%% Prometheus Export Tests
%%====================================================================

test_prometheus_format() ->
    ?_test(begin
        % Set up some metrics
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_success(),
        erlmcp_counters:inc_connections(),
        erlmcp_counters:add_bytes_sent(1024),

        % Get Prometheus export
        PrometheusText = iolist_to_binary(erlmcp_counters:get_prometheus()),

        % Verify Prometheus format
        ?assert(is_binary(PrometheusText)),

        % Verify all metrics are present
        ?assertMatch({match, _}, re:run(PrometheusText, <<"erlmcp_requests_total 2">>)),
        ?assertMatch({match, _}, re:run(PrometheusText, <<"erlmcp_requests_success_total 1">>)),
        ?assertMatch({match, _}, re:run(PrometheusText, <<"erlmcp_connections_active 1">>)),
        ?assertMatch({match, _}, re:run(PrometheusText, <<"erlmcp_bytes_sent_total 1024">>)),

        % Verify HELP and TYPE annotations
        ?assertMatch({match, _}, re:run(PrometheusText, <<"# HELP erlmcp_requests_total">>)),
        ?assertMatch({match, _}, re:run(PrometheusText, <<"# TYPE erlmcp_requests_total counter">>)),
        ?assertMatch({match, _}, re:run(PrometheusText, <<"# TYPE erlmcp_connections_active gauge">>))
    end).

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_increments() ->
    ?_test(begin
        % Spawn multiple processes incrementing counters concurrently
        NumProcesses = 100,
        IncrementsPerProcess = 100,

        Pids = [
            spawn_link(fun() ->
                [erlmcp_counters:inc_requests() || _ <- lists:seq(1, IncrementsPerProcess)]
            end)
            || _ <- lists:seq(1, NumProcesses)
        ],

        % Wait for all processes to complete
        [begin
            Ref = monitor(process, Pid),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 ->
                ?assert(false)  % Timeout
            end
        end || Pid <- Pids],

        % Verify total count is correct (no lost updates)
        Metrics = erlmcp_counters:get_all(),
        Expected = NumProcesses * IncrementsPerProcess,
        ?assertEqual(Expected, maps:get(requests_total, Metrics))
    end).

test_high_frequency_updates() ->
    ?_test(begin
        % Simulate high-frequency updates (100K operations)
        NumOps = 100000,
        StartTime = erlang:system_time(microsecond),

        [erlmcp_counters:inc_requests() || _ <- lists:seq(1, NumOps)],

        EndTime = erlang:system_time(microsecond),
        Duration = EndTime - StartTime,

        % Verify all operations completed
        Metrics = erlmcp_counters:get_all(),
        ?assertEqual(NumOps, maps:get(requests_total, Metrics)),

        % Log performance (should be ~10ns per operation = 100M ops/sec)
        OpsPerSecond = (NumOps * 1000000) / Duration,
        io:format("~nHigh-frequency update performance: ~p ops/sec (~p us total)~n",
                  [OpsPerSecond, Duration]),

        % Should complete in reasonable time (<1 second for 100K ops)
        ?assert(Duration < 1000000)  % Less than 1 second
    end).

%%====================================================================
%% Property-Based Tests (Manual Properties)
%%====================================================================

% Property: Increments are never lost
% Property: Counters are monotonically increasing (except dec_connections)
% Property: Reset always zeros all counters
% Property: Concurrent operations are linearizable
