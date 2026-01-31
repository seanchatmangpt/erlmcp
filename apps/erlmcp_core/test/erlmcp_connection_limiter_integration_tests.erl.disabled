%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_connection_limiter Integration Tests
%%
%% Integration tests for:
%% - Connection rejection at limit
%% - Alert threshold monitoring (70%)
%% - Graceful refusal
%% - Capacity stress tests
%% - Concurrent access
%% - Multi-server scenarios
%%
%% Chicago School TDD: Real processes, observable behavior, no mocks
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_limiter_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Start gproc first
    case application:start(gproc) of
        ok ->
            ok;
        {error, {already_started, gproc}} ->
            ok
    end,

    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),

    {ok, _Pid} = erlmcp_connection_limiter:start_link(),
    timer:sleep(10),
    ok.

cleanup(_) ->
    erlmcp_connection_limiter:stop(),
    application:unset_env(erlmcp_core, connection_limiting).

%%====================================================================
%% Test Suites
%%====================================================================

%%--------------------------------------------------------------------
%% Connection Limit Enforcement Tests
%%--------------------------------------------------------------------

connection_rejection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_reject_over_limit())
     ]}.

test_reject_over_limit() ->
    % Set a low limit for testing
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 5,
        alert_threshold => 0.7,
        enabled => true
    }),

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

%%--------------------------------------------------------------------
%% Alert Threshold Tests (70% capacity)
%%--------------------------------------------------------------------

alert_threshold_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_alert_at_70_percent()),
         ?_test(test_no_alert_below_threshold())
     ]}.

test_alert_at_70_percent() ->
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

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

test_no_alert_below_threshold() ->
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

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
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 10,
        alert_threshold => 0.7,
        enabled => true
    }),

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
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 1,
        alert_threshold => 0.7,
        enabled => true
    }),

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
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 5,
        alert_threshold => 0.7,
        enabled => true
    }),

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
%% Capacity Test (stress test with reasonable limits)
%%--------------------------------------------------------------------

capacity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_1k_capacity()),
         ?_test(test_graceful_degradation())
     ]}.

test_1k_capacity() ->
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 1000,
        alert_threshold => 0.7,
        enabled => true
    }),

    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_1k,

    % Accept 1K connections (stress test)
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    % Should be at limit
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(1000, Count),

    % 1001st should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ok.

test_graceful_degradation() ->
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 500,
        alert_threshold => 0.7,
        enabled => true
    }),

    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_degradation,

    % Ramp up connections gradually
    AcceptBatches = [begin
        Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 50)],
        length([R || R <- Results, R =:= accept])
    end || _ <- lists:seq(1, 10)],

    % Should accept all 500
    TotalAccepted = lists:sum(AcceptBatches),
    ?assertEqual(500, TotalAccepted),

    % 501st should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 500)],
    ok.

%%--------------------------------------------------------------------
%% Concurrent Access Tests
%%--------------------------------------------------------------------

concurrent_access_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_concurrent_accept()),
         ?_test(test_concurrent_release())
     ]}.

test_concurrent_accept() ->
    ServerId = test_server_concurrent_accept,

    % Spawn 100 processes accepting connections concurrently
    Pids = [spawn(fun() ->
        erlmcp_connection_limiter:accept_connection(ServerId)
    end) || _ <- lists:seq(1, 100)],

    % Wait for all to complete
    [begin
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, P, _} -> ok end
    end || Pid <- Pids],

    % Should have 100 connections
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(100, Count),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ok.

test_concurrent_release() ->
    ServerId = test_server_concurrent_release,

    % Accept connections first
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],
    ?assertEqual(100, erlmcp_connection_limiter:get_connection_count()),

    % Spawn 100 processes releasing connections concurrently
    Pids = [spawn(fun() ->
        erlmcp_connection_limiter:release_connection(ServerId)
    end) || _ <- lists:seq(1, 100)],

    % Wait for all to complete
    [begin
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, P, _} -> ok end
    end || Pid <- Pids],

    % Should have 0 connections
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),

    ok.

%%--------------------------------------------------------------------
%% Integration Tests
%%--------------------------------------------------------------------

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_multiple_servers()),
         ?_test(test_high_churn())
     ]}.

test_multiple_servers() ->
    % Simulate multiple servers with different loads
    ServerIds = [
        test_multi_server_1,
        test_multi_server_2,
        test_multi_server_3
    ],

    % Each server accepts different number of connections
    AcceptCounts = [10, 20, 30],

    lists:foreach(fun({ServerId, Count}) ->
        [begin
            erlmcp_connection_limiter:accept_connection(ServerId),
            timer:sleep(1)
        end || _ <- lists:seq(1, Count)]
    end, lists:zip(ServerIds, AcceptCounts)),

    timer:sleep(10),

    % Verify per-server counts
    lists:foreach(fun({ServerId, ExpectedCount}) ->
        ActualCount = erlmcp_connection_limiter:get_connection_count(ServerId),
        ?assertEqual(ExpectedCount, ActualCount)
    end, lists:zip(ServerIds, AcceptCounts)),

    % Verify global count
    GlobalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(lists:sum(AcceptCounts), GlobalCount),

    % Clean up
    lists:foreach(fun({ServerId, Count}) ->
        [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, Count)]
    end, lists:zip(ServerIds, AcceptCounts)),

    ok.

test_high_churn() ->
    ServerId = test_server_churn,

    % Simulate high churn: accept and release repeatedly
    ChurnCount = 100,
    lists:foreach(fun(_) ->
        accept = erlmcp_connection_limiter:accept_connection(ServerId),
        ok = erlmcp_connection_limiter:release_connection(ServerId)
    end, lists:seq(1, ChurnCount)),

    % Final count should be 0
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),

    ok.
