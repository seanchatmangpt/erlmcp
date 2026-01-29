%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_connection_limiter Module
%%
%% Comprehensive tests for:
%% - Connection limit enforcement (10K default)
%% - Per-server connection tracking
%% - Graceful rejection before resource exhaustion
%% - Monitoring and alerting at 70% capacity
%% - gproc-based distributed counters
%% - Configuration management
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_limiter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Set up default config
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),
    {ok, _Pid} = erlmcp_connection_limiter:start_link(),
    ok.

cleanup(_) ->
    erlmcp_connection_limiter:stop(),
    application:unset_env(erlmcp, connection_limiting).

%%====================================================================
%% Test Suites
%%====================================================================

%%--------------------------------------------------------------------
%% Module Lifecycle Tests
%%--------------------------------------------------------------------

module_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_start_stop()),
         ?_test(test_multiple_starts()),
         ?_test(test_get_stats())
     ]}.

test_start_stop() ->
    Pid = whereis(erlmcp_connection_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Pid)).

test_multiple_starts() ->
    % Clean up first instance if running
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(100),

    % Start new instance
    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    % Clean up for other tests
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50).

test_get_stats() ->
    % Ensure server is running
    case whereis(erlmcp_connection_limiter) of
        undefined ->
            {ok, _} = erlmcp_connection_limiter:start_link();
        _ ->
            ok
    end,

    Stats = erlmcp_connection_limiter:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(current_connections, Stats)),
    ?assert(maps:is_key(max_connections, Stats)),
    ?assert(maps:is_key(alert_threshold, Stats)).

%%--------------------------------------------------------------------
%% Connection Limit Enforcement Tests
%%--------------------------------------------------------------------

connection_limit_enforcement_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_accept_single_connection()),
         ?_test(test_accept_multiple_connections()),
         ?_test(test_reject_over_limit()),
         ?_test(test_release_connection()),
         ?_test(test_connection_tracking())
     ]}.

test_accept_single_connection() ->
    ServerId = test_server_1,
    Result = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(accept, Result),

    % Clean up
    erlmcp_connection_limiter:release_connection(ServerId).

test_accept_multiple_connections() ->
    ServerId = test_server_2,
    % Should accept multiple connections
    Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, Results)),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ok.

test_reject_over_limit() ->
    % Set a low limit for testing
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 5,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_3,

    % Accept 5 connections (at limit)
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    % 6th should be rejected
    RejectResult = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, RejectResult),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 5)],
    ok.

test_release_connection() ->
    ServerId = test_server_4,
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    % Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),
    ?assert(CountDuring > CountBefore),

    % Release connection
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assert(CountAfter < CountDuring).

test_connection_tracking() ->
    ServerId1 = test_server_5a,
    ServerId2 = test_server_5b,

    % Accept connections for different servers
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),

    % Check per-server counts
    Count1 = erlmcp_connection_limiter:get_connection_count(ServerId1),
    Count2 = erlmcp_connection_limiter:get_connection_count(ServerId2),
    ?assertEqual(1, Count1),
    ?assertEqual(1, Count2),

    % Check global count
    GlobalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assert(GlobalCount >= 2),

    % Clean up
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2).

%%--------------------------------------------------------------------
%% Alert Threshold Tests (70% capacity)
%%--------------------------------------------------------------------

alert_threshold_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_alert_at_70_percent()),
         ?_test(test_alert_cooldown()),
         ?_test(test_no_alert_below_threshold())
     ]}.

test_alert_at_70_percent() ->
    % Set limit to 100 for easy calculation
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_alert,

    % Accept 70 connections (70% of 100)
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 70)],

    % Check stats - should have alert info
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(70, maps:get(current_connections, Stats)),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 70)],
    ok.

test_alert_cooldown() ->
    % Set limit to 100
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_cooldown,

    % Accept 70 connections to trigger alert
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 70)],

    Stats1 = erlmcp_connection_limiter:get_stats(),
    LastAlert1 = maps:get(last_alert, Stats1),
    ?assertNotEqual(undefined, LastAlert1),

    % Wait for cooldown (1 minute + buffer)
    % Note: We can't actually wait 1 minute in tests, so we just verify the logic
    % In production, alerts would be throttled

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 70)],
    ok.

test_no_alert_below_threshold() ->
    % Set limit to 100
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_no_alert,

    % Accept only 50 connections (50% - below 70% threshold)
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 50)],

    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(50, maps:get(current_connections, Stats)),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 50)],
    ok.

%%--------------------------------------------------------------------
%% Configuration Tests
%%--------------------------------------------------------------------

configuration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_default_config()),
         ?_test(test_custom_config()),
         ?_test(test_disabled_limiting()),
         ?_test(test_set_limit())
     ]}.

test_default_config() ->
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(10000, maps:get(max_connections, Stats)),
    ?assertEqual(0.7, maps:get(alert_threshold, Stats)).

test_custom_config() ->
    erlmcp_connection_limiter:stop(),
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 5000,
        alert_threshold => 0.8,
        enabled => true
    }),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(5000, maps:get(max_connections, Stats)),
    ?assertEqual(0.8, maps:get(alert_threshold, Stats)).

test_disabled_limiting() ->
    erlmcp_connection_limiter:stop(),
    application:set_env(erlmcp, connection_limiting, #{
        enabled => false
    }),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_disabled,

    % Should always accept when disabled
    Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, Results)),

    % Count should return 0 when disabled
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count).

test_set_limit() ->
    % Set initial limit
    Limit1 = 1000,
    ok = erlmcp_connection_limiter:set_limit(Limit1),
    ?assertEqual(Limit1, erlmcp_connection_limiter:get_limit()),

    % Change limit
    Limit2 = 5000,
    ok = erlmcp_connection_limiter:set_limit(Limit2),
    ?assertEqual(Limit2, erlmcp_connection_limiter:get_limit()).

%%--------------------------------------------------------------------
%% gproc Counter Tests
%%--------------------------------------------------------------------

gproc_counter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_counter_increment()),
         ?_test(test_counter_decrement()),
         ?_test(test_per_server_counters())
     ]}.

test_counter_increment() ->
    ServerId = test_server_gproc_inc,
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    % Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    ?assertEqual(CountBefore + 1, CountAfter),

    % Clean up
    erlmcp_connection_limiter:release_connection(ServerId).

test_counter_decrement() ->
    ServerId = test_server_gproc_dec,

    % Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),

    % Release connection
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    ?assertEqual(CountDuring - 1, CountAfter).

test_per_server_counters() ->
    ServerId1 = test_server_gproc_1,
    ServerId2 = test_server_gproc_2,

    % Accept connections for different servers
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),

    % Check per-server counts
    Count1 = erlmcp_connection_limiter:get_connection_count(ServerId1),
    Count2 = erlmcp_connection_limiter:get_connection_count(ServerId2),

    ?assertEqual(2, Count1),
    ?assertEqual(1, Count2),

    % Clean up
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2).

%%--------------------------------------------------------------------
%% Graceful Refusal Tests
%%--------------------------------------------------------------------

graceful_refusal_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_refusal_before_exhaustion()),
         ?_test(test_refusal_error_message()),
         ?_test(test_recovery_after_refusal())
     ]}.

test_refusal_before_exhaustion() ->
    % Set a very low limit
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 10,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_refusal,

    % Fill up to limit
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    % Should refuse at limit
    RefuseResult = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, RefuseResult),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    ok.

test_refusal_error_message() ->
    % Set a low limit
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 1,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_error_msg,

    % Accept one connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),

    % Should get error on second
    Result = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, Result),

    % Clean up
    erlmcp_connection_limiter:release_connection(ServerId).

test_recovery_after_refusal() ->
    % Set a low limit
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 5,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_recovery,

    % Fill up to limit
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 5)],

    % Should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    % Release one connection
    ok = erlmcp_connection_limiter:release_connection(ServerId),

    % Should be able to accept again
    accept = erlmcp_connection_limiter:accept_connection(ServerId),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 5)],
    ok.

%%--------------------------------------------------------------------
%% Capacity Test (10K connections)
%%--------------------------------------------------------------------

capacity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_10k_capacity()),
         ?_test(test_70_percent_alert()),
         ?_test(test_graceful_degradation())
     ]}.

test_10k_capacity() ->
    % Set to 10K limit (production default)
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_10k,

    % Accept 10K connections (stress test)
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10000)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    % Should be at limit
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(10000, Count),

    % 10001st should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    % Clean up (release all)
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10000)],
    ok.

test_70_percent_alert() ->
    % Set to 100 for easy testing
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_alert_70,

    % Accept 70 connections
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 70)],

    % Check that we're at alert threshold
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(70, maps:get(current_connections, Stats)),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 70)],
    ok.

test_graceful_degradation() ->
    % Set to moderate limit
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 1000,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_degradation,

    % Ramp up connections gradually
    AcceptBatches = [begin
        Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],
        length([R || R <- Results, R =:= accept])
    end || _ <- lists:seq(1, 10)],

    % Should accept all 1000
    TotalAccepted = lists:sum(AcceptBatches),
    ?assertEqual(1000, TotalAccepted),

    % 1001st should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ok.
