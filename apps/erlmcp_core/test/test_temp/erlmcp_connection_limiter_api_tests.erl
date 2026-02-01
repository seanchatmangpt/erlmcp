%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_connection_limiter API Boundary Tests
%%
%% API boundary tests for:
%% - Connection limit enforcement
%% - Per-server connection tracking
%% - gproc-based distributed counters
%% - Configuration management
%%
%% Chicago School TDD: Real processes, observable behavior, no mocks
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_limiter_api_tests).

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

    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 10000,
                          alert_threshold => 0.7,
                          enabled => true}),

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
%% Module Lifecycle Tests
%%--------------------------------------------------------------------

module_lifecycle_test_() ->
    {setup, fun setup/0, fun cleanup/1, [?_test(test_start_stop()), ?_test(test_get_stats())]}.

test_start_stop() ->
    Pid = whereis(erlmcp_connection_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Pid)).

test_get_stats() ->
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(current_connections, Stats)),
    ?assert(maps:is_key(max_connections, Stats)).

%%--------------------------------------------------------------------
%% Connection Limit Enforcement Tests
%%--------------------------------------------------------------------

connection_limit_enforcement_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_accept_single_connection()),
      ?_test(test_accept_multiple_connections()),
      ?_test(test_release_connection()),
      ?_test(test_connection_tracking())]}.

test_accept_single_connection() ->
    ServerId = test_server_1,
    Result = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(accept, Result),
    erlmcp_connection_limiter:release_connection(ServerId).

test_accept_multiple_connections() ->
    ServerId = test_server_2,
    Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)),
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ok.

test_release_connection() ->
    ServerId = test_server_4,
    CountBefore = erlmcp_connection_limiter:get_connection_count(),
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),
    ?assert(CountDuring > CountBefore),
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assert(CountAfter < CountDuring).

test_connection_tracking() ->
    ServerId1 = test_server_5a,
    ServerId2 = test_server_5b,
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    timer:sleep(1),
    Count1 = erlmcp_connection_limiter:get_connection_count(ServerId1),
    Count2 = erlmcp_connection_limiter:get_connection_count(ServerId2),
    ?assertEqual(1, Count1),
    ?assertEqual(1, Count2),
    GlobalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assert(GlobalCount >= 2),
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2).

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
    Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun (accept) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)),
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(1000, Count),
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ok.

test_set_limit() ->
    Limit1 = 1000,
    ok = erlmcp_connection_limiter:set_limit(Limit1),
    ?assertEqual(Limit1, erlmcp_connection_limiter:get_limit()),
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
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    timer:sleep(1),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountBefore + 1, CountAfter),
    erlmcp_connection_limiter:release_connection(ServerId).

test_counter_decrement() ->
    ServerId = test_server_gproc_dec,
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    timer:sleep(1),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    timer:sleep(1),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountDuring - 1, CountAfter).

test_per_server_counters() ->
    ServerId1 = test_server_gproc_1,
    ServerId2 = test_server_gproc_2,
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    timer:sleep(1),
    Count1 = erlmcp_connection_limiter:get_connection_count(ServerId1),
    Count2 = erlmcp_connection_limiter:get_connection_count(ServerId2),
    ?assertEqual(2, Count1),
    ?assertEqual(1, Count2),
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2).

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
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(0, maps:get(current_connections, Stats)).

test_server_id_types() ->
    AtomServerId = atom_server_test,
    accept = erlmcp_connection_limiter:accept_connection(AtomServerId),
    Count1 = erlmcp_connection_limiter:get_connection_count(AtomServerId),
    ?assertEqual(1, Count1),
    erlmcp_connection_limiter:release_connection(AtomServerId),
    BinaryServerId = <<"binary_server_test">>,
    accept = erlmcp_connection_limiter:accept_connection(BinaryServerId),
    Count2 = erlmcp_connection_limiter:get_connection_count(BinaryServerId),
    ?assertEqual(1, Count2),
    erlmcp_connection_limiter:release_connection(BinaryServerId),
    ok.

test_release_without_accept() ->
    ServerId = test_server_no_accept,
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count),
    ok.
