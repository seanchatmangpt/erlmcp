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
%% Chicago School TDD: Real processes, state-based verification, no mocks
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_limiter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Start gproc first (minimal dependency)
    case application:start(gproc) of
        ok ->
            ok;
        {error, {already_started, gproc}} ->
            ok
    end,

    % Set up default config - use erlmcp_core application name
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 10000,
                          alert_threshold => 0.7,
                          enabled => true}),

    % Start the connection limiter
    {ok, _Pid} = erlmcp_connection_limiter:start_link(),

    % Wait for gproc counter to be fully registered
    % Use a small delay to ensure counter is ready
    timer:sleep(10),

    % Verify counter is accessible
    case gproc:where({c, l, erlmcp_connection_count}) of
        undefined ->
            error({failed_to_register_counter, timeout});
        _ ->
            ok
    end,

    ok.

cleanup(_) ->
    erlmcp_connection_limiter:stop(),
    application:unset_env(erlmcp_core, connection_limiting).

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
     [?_test(test_start_stop()), ?_test(test_multiple_starts()), ?_test(test_get_stats())]}.

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
     [?_test(test_accept_single_connection()),
      ?_test(test_accept_multiple_connections()),
      ?_test(test_reject_over_limit()),
      ?_test(test_release_connection()),
      ?_test(test_connection_tracking())]}.

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
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ok.

test_reject_over_limit() ->
    % Set a low limit for testing
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 5,
                          alert_threshold => 0.7,
                          enabled => true}),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_3,

    % Accept 5 connections (at limit)
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      AcceptResults)),

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
    timer:sleep(1),  % Small delay for counter update
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    timer:sleep(1),  % Small delay for counter update

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
     [?_test(test_alert_at_70_percent()), ?_test(test_no_alert_below_threshold())]}.

test_alert_at_70_percent() ->
    % Set limit to 100 for easy calculation
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 100,
                          alert_threshold => 0.7,
                          enabled => true}),

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

test_no_alert_below_threshold() ->
    % Set limit to 100
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 100,
                          alert_threshold => 0.7,
                          enabled => true}),

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
     [?_test(test_default_config()),
      ?_test(test_custom_config()),
      ?_test(test_disabled_limiting()),
      ?_test(test_set_limit())]}.

test_default_config() ->
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(10000, maps:get(max_connections, Stats)),
    ?assertEqual(0.7, maps:get(alert_threshold, Stats)).

test_custom_config() ->
    erlmcp_connection_limiter:stop(),
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 5000,
                          alert_threshold => 0.8,
                          enabled => true}),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(5000, maps:get(max_connections, Stats)),
    ?assertEqual(0.8, maps:get(alert_threshold, Stats)).

test_disabled_limiting() ->
    erlmcp_connection_limiter:stop(),
    application:set_env(erlmcp_core, connection_limiting, #{enabled => false}),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_disabled,

    % Should always accept when disabled
    Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)),

    % Count should return actual count even when disabled
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(1000, Count),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ok.

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
     [?_test(test_counter_increment()),
      ?_test(test_counter_decrement()),
      ?_test(test_per_server_counters())]}.

test_counter_increment() ->
    ServerId = test_server_gproc_inc,
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    % Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    timer:sleep(1),  % Small delay for counter update
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    ?assertEqual(CountBefore + 1, CountAfter),

    % Clean up
    erlmcp_connection_limiter:release_connection(ServerId).

test_counter_decrement() ->
    ServerId = test_server_gproc_dec,

    % Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    timer:sleep(1),  % Small delay for counter update
    CountDuring = erlmcp_connection_limiter:get_connection_count(),

    % Release connection
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    timer:sleep(1),  % Small delay for counter update
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    ?assertEqual(CountDuring - 1, CountAfter).

test_per_server_counters() ->
    ServerId1 = test_server_gproc_1,
    ServerId2 = test_server_gproc_2,

    % Accept connections for different servers
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(1),  % Small delay for counter update
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(1),  % Small delay for counter update
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    timer:sleep(1),  % Small delay for counter update

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
     [?_test(test_refusal_before_exhaustion()),
      ?_test(test_refusal_error_message()),
      ?_test(test_recovery_after_refusal())]}.

test_refusal_before_exhaustion() ->
    % Set a very low limit
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 10,
                          alert_threshold => 0.7,
                          enabled => true}),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_refusal,

    % Fill up to limit
    AcceptResults =
        [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      AcceptResults)),

    % Should refuse at limit
    RefuseResult = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, RefuseResult),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    ok.

test_refusal_error_message() ->
    % Set a low limit
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 1,
                          alert_threshold => 0.7,
                          enabled => true}),

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
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 5,
                          alert_threshold => 0.7,
                          enabled => true}),

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
%% Capacity Test (stress test with reasonable limits)
%%--------------------------------------------------------------------

capacity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_1k_capacity()),
      ?_test(test_70_percent_alert()),
      ?_test(test_graceful_degradation())]}.

test_1k_capacity() ->
    % Set to 1K limit for testing
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 1000,
                          alert_threshold => 0.7,
                          enabled => true}),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_1k,

    % Accept 1K connections (stress test)
    AcceptResults =
        [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      AcceptResults)),

    % Should be at limit
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(1000, Count),

    % 1001st should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    % Clean up (release all)
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ok.

test_70_percent_alert() ->
    % Set to 100 for easy testing
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 100,
                          alert_threshold => 0.7,
                          enabled => true}),

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
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 500,
                          alert_threshold => 0.7,
                          enabled => true}),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    {ok, _} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_degradation,

    % Ramp up connections gradually
    AcceptBatches =
        [begin
             Results =
                 [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 50)],
             length([R || R <- Results, R =:= accept])
         end
         || _ <- lists:seq(1, 10)],

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
     [?_test(test_concurrent_accept()), ?_test(test_concurrent_release())]}.

test_concurrent_accept() ->
    ServerId = test_server_concurrent_accept,

    % Spawn 100 processes accepting connections concurrently
    Pids =
        [spawn(fun() -> erlmcp_connection_limiter:accept_connection(ServerId) end)
         || _ <- lists:seq(1, 100)],

    % Wait for all to complete
    [begin
         Ref = monitor(process, Pid),
         receive
             {'DOWN', Ref, process, P, _} ->
                 ok
         end
     end
     || Pid <- Pids],

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
    Pids =
        [spawn(fun() -> erlmcp_connection_limiter:release_connection(ServerId) end)
         || _ <- lists:seq(1, 100)],

    % Wait for all to complete
    [begin
         Ref = monitor(process, Pid),
         receive
             {'DOWN', Ref, process, P, _} ->
                 ok
         end
     end
     || Pid <- Pids],

    % Should have 0 connections
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),

    ok.

%%--------------------------------------------------------------------
%% Edge Cases Tests
%%--------------------------------------------------------------------

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_zero_connections()),
      ?_test(test_server_id_types()),
      ?_test(test_release_without_accept())]}.

test_zero_connections() ->
    % Initially should be 0
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),

    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(0, maps:get(current_connections, Stats)).

test_server_id_types() ->
    % Test with atom server ID
    AtomServerId = atom_server_test,
    accept = erlmcp_connection_limiter:accept_connection(AtomServerId),
    Count1 = erlmcp_connection_limiter:get_connection_count(AtomServerId),
    ?assertEqual(1, Count1),
    erlmcp_connection_limiter:release_connection(AtomServerId),

    % Test with binary server ID
    BinaryServerId = <<"binary_server_test">>,
    accept = erlmcp_connection_limiter:accept_connection(BinaryServerId),
    Count2 = erlmcp_connection_limiter:get_connection_count(BinaryServerId),
    ?assertEqual(1, Count2),
    erlmcp_connection_limiter:release_connection(BinaryServerId),

    ok.

test_release_without_accept() ->
    ServerId = test_server_no_accept,

    % Release without accept should not crash
    ok = erlmcp_connection_limiter:release_connection(ServerId),

    % Count should still be 0
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
     [?_test(test_multiple_servers()), ?_test(test_high_churn())]}.

test_multiple_servers() ->
    % Simulate multiple servers with different loads
    ServerIds = [test_multi_server_1, test_multi_server_2, test_multi_server_3],

    % Each server accepts different number of connections
    AcceptCounts = [10, 20, 30],

    lists:foreach(fun({ServerId, Count}) ->
                     [begin
                          erlmcp_connection_limiter:accept_connection(ServerId),
                          timer:sleep(1)  % Small delay for counter update
                      end
                      || _ <- lists:seq(1, Count)]
                  end,
                  lists:zip(ServerIds, AcceptCounts)),

    % Small delay to ensure all counters are updated
    timer:sleep(10),

    % Verify per-server counts
    lists:foreach(fun({ServerId, ExpectedCount}) ->
                     ActualCount = erlmcp_connection_limiter:get_connection_count(ServerId),
                     ?assertEqual(ExpectedCount, ActualCount)
                  end,
                  lists:zip(ServerIds, AcceptCounts)),

    % Verify global count
    GlobalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(lists:sum(AcceptCounts), GlobalCount),

    % Clean up
    lists:foreach(fun({ServerId, Count}) ->
                     [erlmcp_connection_limiter:release_connection(ServerId)
                      || _ <- lists:seq(1, Count)]
                  end,
                  lists:zip(ServerIds, AcceptCounts)),

    ok.

test_high_churn() ->
    ServerId = test_server_churn,

    % Simulate high churn: accept and release repeatedly
    ChurnCount = 100,
    lists:foreach(fun(_) ->
                     accept = erlmcp_connection_limiter:accept_connection(ServerId),
                     ok = erlmcp_connection_limiter:release_connection(ServerId)
                  end,
                  lists:seq(1, ChurnCount)),

    % Final count should be 0
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),

    ok.
