%%%-------------------------------------------------------------------
%%% @doc Connection Limiter Tests (Chicago School TDD)
%%%
%%% Tests for erlmcp_connection_limiter to ensure:
%%% - Connection limit enforcement (10K default, configurable)
%%% - Per-server connection tracking via gproc
%%% - Graceful rejection before resource exhaustion
%%% - Monitoring and alerting at 70% capacity
%%% - gproc-based distributed counters
%%% - Configuration management (enabled/disabled)
%%% - Concurrent connection handling
%%% - Resource cleanup and recovery
%%%
%%% Testing Methodology:
%%% - Chicago School TDD: Real gen_server, no mocks
%%% - State-based verification: Assert on observable state
%%% - Real processes: Actual gproc counters, not stubbed
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_limiter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup function - initializes connection limiter with test config
setup() ->
    %% Set test configuration
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),

    %% Start the connection limiter (real gen_server, no mocks)
    {ok, Pid} = erlmcp_connection_limiter:start_link(),

    %% Verify it's running (Chicago School: observable state)
    ?assertEqual(true, is_process_alive(Pid)),

    Pid.

%% @doc Cleanup function - stops connection limiter and clears config
cleanup(_Pid) ->
    %% Stop the connection limiter
    erlmcp_connection_limiter:stop(),

    %% Wait for cleanup to complete
    timer:sleep(50),

    %% Clear application environment
    application:unset_env(erlmcp, connection_limiting).

%%====================================================================
%% Module Lifecycle Tests
%%====================================================================

connection_limiter_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Start and stop connection limiter", fun test_start_stop/0},
         {"Get statistics returns valid map", fun test_get_stats/0},
         {"Set and get limit", fun test_set_get_limit/0},
         {"Check if limiting is enabled", fun test_is_limit_enabled/0}
     ]}.

%% @doc Test that connection limiter starts and stops correctly
test_start_stop() ->
    %% Verify gen_server is running (observable state)
    Pid = whereis(erlmcp_connection_limiter),
    ?assert(is_pid(Pid)),
    ?assertEqual(true, is_process_alive(Pid)),

    %% Stop and verify cleanup
    ok = erlmcp_connection_limiter:stop(),
    timer:sleep(50),

    %% Verify process is no longer alive
    ?assertNot(is_process_alive(Pid)),

    %% Restart for other tests
    {ok, _NewPid} = erlmcp_connection_limiter:start_link().

%% @doc Test that get_stats returns valid statistics map
test_get_stats() ->
    %% Get stats (real API call)
    Stats = erlmcp_connection_limiter:get_stats(),

    %% Verify map structure (Chicago School: state verification)
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(current_connections, Stats)),
    ?assert(maps:is_key(max_connections, Stats)),
    ?assert(maps:is_key(alert_threshold, Stats)),
    ?assert(maps:is_key(last_alert, Stats)),

    %% Verify types
    CurrentConn = maps:get(current_connections, Stats),
    ?assert(is_integer(CurrentConn)),
    ?assert(CurrentConn >= 0),

    MaxConn = maps:get(max_connections, Stats),
    ?assert(is_integer(MaxConn)),
    ?assert(MaxConn > 0).

%% @doc Test set_limit and get_limit
test_set_get_limit() ->
    %% Set a custom limit (real API call)
    ok = erlmcp_connection_limiter:set_limit(5000),

    %% Verify limit was set (state verification)
    ?assertEqual(5000, erlmcp_connection_limiter:get_limit()).

%% @doc Test is_limit_enabled
test_is_limit_enabled() ->
    %% With current config (enabled => true)
    ?assertEqual(true, erlmcp_connection_limiter:is_limit_enabled()).

%%====================================================================
%% Connection Acceptance Tests
%%====================================================================

connection_acceptance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Accept single connection", fun test_accept_single/0},
         {"Accept multiple connections sequentially", fun test_accept_multiple_sequential/0},
         {"Release connection decrements counter", fun test_release_connection/0},
         {"Get connection count is accurate", fun test_connection_count/0}
     ]}.

%% @doc Test accepting a single connection
test_accept_single() ->
    ServerId = test_server_1,

    %% Accept connection (real API call)
    Result = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(accept, Result),

    %% Verify counter incremented (state-based verification)
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assert(Count > 0),

    %% Cleanup
    erlmcp_connection_limiter:release_connection(ServerId).

%% @doc Test accepting multiple connections sequentially
test_accept_multiple_sequential() ->
    ServerId = test_server_2,

    %% Accept 100 connections
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],

    %% All should be accepted
    ?assertEqual(100, length([R || R <- AcceptResults, R =:= accept])),

    %% Verify count
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(100, Count),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ok.

%% @doc Test that releasing a connection decrements counter
test_release_connection() ->
    ServerId = test_server_3,

    %% Get initial count
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    %% Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountBefore + 1, CountDuring),

    %% Release connection
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountBefore, CountAfter).

%% @doc Test that get_connection_count is accurate
test_connection_count() ->
    ServerId = test_server_4,

    %% Baseline
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    %% Add 10 connections
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],
    CountDuring = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountBefore + 10, CountDuring),

    %% Remove 10 connections
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountBefore, CountAfter).

%%====================================================================
%% Per-Server Connection Tracking Tests
%%====================================================================

per_server_tracking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Track connections for multiple servers", fun test_multiple_servers/0},
         {"Get per-server connection count", fun test_per_server_count/0},
         {"Independent server counters", fun test_independent_counters/0}
     ]}.

%% @doc Test tracking connections for multiple servers
test_multiple_servers() ->
    ServerId1 = server_a,
    ServerId2 = server_b,
    ServerId3 = server_c,

    %% Accept connections for different servers
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    accept = erlmcp_connection_limiter:accept_connection(ServerId3),

    %% Check per-server counts (state verification)
    ?assertEqual(2, erlmcp_connection_limiter:get_connection_count(ServerId1)),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerId2)),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerId3)),

    %% Check global count
    GlobalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(4, GlobalCount),

    %% Cleanup
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2),
    erlmcp_connection_limiter:release_connection(ServerId3).

%% @doc Test getting per-server connection count
test_per_server_count() ->
    ServerId = test_server_count,

    %% Baseline
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Add connections
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerId)),

    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(2, erlmcp_connection_limiter:get_connection_count(ServerId)),

    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(3, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Release
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    ?assertEqual(2, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 2)],
    ok.

%% @doc Test that server counters are independent
test_independent_counters() ->
    ServerA = server_independent_a,
    ServerB = server_independent_b,

    %% Add to ServerA
    accept = erlmcp_connection_limiter:accept_connection(ServerA),
    accept = erlmcp_connection_limiter:accept_connection(ServerA),
    accept = erlmcp_connection_limiter:accept_connection(ServerA),

    %% Add to ServerB
    accept = erlmcp_connection_limiter:accept_connection(ServerB),

    %% Verify independence
    ?assertEqual(3, erlmcp_connection_limiter:get_connection_count(ServerA)),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerB)),
    ?assertEqual(4, erlmcp_connection_limiter:get_connection_count()),

    %% Release from ServerA - shouldn't affect ServerB
    ok = erlmcp_connection_limiter:release_connection(ServerA),
    ?assertEqual(2, erlmcp_connection_limiter:get_connection_count(ServerA)),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerB)),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerA) || _ <- lists:seq(1, 2)],
    erlmcp_connection_limiter:release_connection(ServerB).

%%====================================================================
%% Connection Limit Enforcement Tests
%%====================================================================

limit_enforcement_test_() ->
    {setup,
     fun setup_low_limit/0,
     fun cleanup_low_limit/1,
     [
         {"Reject connections over limit", fun test_reject_over_limit/0},
         {"Accept connections up to limit", fun test_accept_up_to_limit/0},
         {"Graceful refusal error message", fun test_refusal_message/0},
         {"Recovery after refusal", fun test_recovery_after_refusal/0}
     ]}.

%% @doc Setup with low limit for limit testing
setup_low_limit() ->
    %% Set low limit for testing
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 10,
        alert_threshold => 0.7,
        enabled => true
    }),

    %% Start limiter
    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    Pid.

%% @doc Cleanup for low limit tests
cleanup_low_limit(_Pid) ->
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    application:unset_env(erlmcp, connection_limiting).

%% @doc Test that connections are rejected over limit
test_reject_over_limit() ->
    ServerId = test_server_limit,

    %% Accept up to limit (10)
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],
    ?assertEqual(10, length([R || R <- AcceptResults, R =:= accept])),

    %% 11th should be rejected
    RejectResult = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, RejectResult),

    %% Verify count is at limit
    ?assertEqual(10, erlmcp_connection_limiter:get_connection_count()),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    ok.

%% @doc Test that connections are accepted up to limit
test_accept_up_to_limit() ->
    ServerId = test_server_upto_limit,

    %% Accept all 10
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    %% Verify we're exactly at limit
    ?assertEqual(10, erlmcp_connection_limiter:get_connection_count()),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    ok.

%% @doc Test graceful refusal error message
test_refusal_message() ->
    ServerId = test_server_message,

    %% Fill to limit
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],

    %% Verify error message
    Result = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, Result),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    ok.

%% @doc Test recovery after refusal
test_recovery_after_refusal() ->
    ServerId = test_server_recovery,

    %% Fill to limit
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10)],

    %% Should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    %% Release one
    ok = erlmcp_connection_limiter:release_connection(ServerId),

    %% Should be able to accept again
    accept = erlmcp_connection_limiter:accept_connection(ServerId),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 10)],
    ok.

%%====================================================================
%% Concurrent Connection Tests
%%====================================================================

concurrent_connections_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Concurrent accept from multiple processes", fun test_concurrent_accept/0},
         {"Concurrent release from multiple processes", fun test_concurrent_release/0},
         {"Race condition prevention", fun test_race_conditions/0}
     ]}.

%% @doc Test concurrent accepts from multiple processes (Chicago School: real processes)
test_concurrent_accept() ->
    ServerId = test_server_concurrent_accept,

    %% Spawn 100 processes that accept concurrently (real processes, no mocks)
    Pids = [spawn(fun() ->
        erlmcp_connection_limiter:accept_connection(ServerId)
    end) || _ <- lists:seq(1, 100)],

    %% Wait for all to complete
    timer:sleep(200),

    %% Verify all were accepted
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(100, Count),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ok.

%% @doc Test concurrent releases from multiple processes
test_concurrent_release() ->
    ServerId = test_server_concurrent_release,

    %% Accept 100 connections
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],
    ?assertEqual(100, erlmcp_connection_limiter:get_connection_count()),

    %% Spawn 100 processes that release concurrently
    Pids = [spawn(fun() ->
        erlmcp_connection_limiter:release_connection(ServerId)
    end) || _ <- lists:seq(1, 100)],

    %% Wait for all to complete
    timer:sleep(200),

    %% Verify all were released
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count).

%% @doc Test race condition prevention
test_race_conditions() ->
    ServerId = test_server_race,

    %% Spawn processes that accept and release concurrently
    AcceptPids = [spawn(fun() ->
        erlmcp_connection_limiter:accept_connection(ServerId),
        timer:sleep(10),
        erlmcp_connection_limiter:release_connection(ServerId)
    end) || _ <- lists:seq(1, 50)],

    %% Wait for completion
    timer:sleep(300),

    %% Verify counter is consistent (should be 0 after all releases)
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(0, Count).

%%====================================================================
%% Configuration Tests
%%====================================================================

configuration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Default configuration values", fun test_default_config/0},
         {"Custom configuration values", fun test_custom_config/0},
         {"Disabled limiting accepts all", fun test_disabled_limiting/0},
         {"Dynamic limit changes", fun test_dynamic_limit/0}
     ]}.

%% @doc Test default configuration
test_default_config() ->
    Stats = erlmcp_connection_limiter:get_stats(),

    %% Verify defaults from setup
    ?assertEqual(10000, maps:get(max_connections, Stats)),
    ?assertEqual(0.7, maps:get(alert_threshold, Stats)),

    %% Verify enabled
    ?assertEqual(true, erlmcp_connection_limiter:is_limit_enabled()).

%% @doc Test custom configuration
test_custom_config() ->
    %% Stop current instance
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),

    %% Set custom config
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 5000,
        alert_threshold => 0.8,
        enabled => true
    }),

    %% Start with new config
    {ok, _Pid} = erlmcp_connection_limiter:start_link(),

    %% Verify custom values
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(5000, maps:get(max_connections, Stats)),
    ?assertEqual(0.8, maps:get(alert_threshold, Stats)).

%% @doc Test disabled limiting accepts all connections
test_disabled_limiting() ->
    %% Stop current instance
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),

    %% Disable limiting
    application:set_env(erlmcp, connection_limiting, #{
        enabled => false
    }),

    %% Start with disabled config
    {ok, _Pid} = erlmcp_connection_limiter:start_link(),

    ServerId = test_server_disabled,

    %% Should accept many connections even without limit
    Results = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, Results)),

    %% Count returns 0 when disabled
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count()).

%% @doc Test dynamic limit changes
test_dynamic_limit() ->
    %% Set initial limit
    ok = erlmcp_connection_limiter:set_limit(1000),
    ?assertEqual(1000, erlmcp_connection_limiter:get_limit()),

    %% Change limit dynamically
    ok = erlmcp_connection_limiter:set_limit(5000),
    ?assertEqual(5000, erlmcp_connection_limiter:get_limit()).

%%====================================================================
%% Alert Threshold Tests
%%====================================================================

alert_threshold_test_() ->
    {setup,
     fun setup_alert_threshold/0,
     fun cleanup_alert_threshold/1,
     [
         {"Alert triggered at 70% threshold", fun test_alert_at_threshold/0},
         {"No alert below threshold", fun test_no_alert_below/0},
         {"Alert cooldown prevents spam", fun test_alert_cooldown/0}
     ]}.

%% @doc Setup for alert tests (100 max, 70% threshold = 70)
setup_alert_threshold() ->
    application:set_env(erlmcp, connection_limiting, #{
        max_connections => 100,
        alert_threshold => 0.7,
        enabled => true
    }),

    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    Pid.

%% @doc Cleanup for alert tests
cleanup_alert_threshold(_Pid) ->
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),
    application:unset_env(erlmcp, connection_limiting).

%% @doc Test that alert is triggered at threshold
test_alert_at_threshold() ->
    ServerId = test_server_alert,

    %% Accept exactly 70 connections (70% threshold)
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 70)],

    %% Check stats - should have alert info
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(70, maps:get(current_connections, Stats)),

    %% Last alert should be set (we can't test the exact log, but we verify state)
    LastAlert = maps:get(last_alert, Stats),
    ?assertNotEqual(undefined, LastAlert),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 70)],
    ok.

%% @doc Test no alert below threshold
test_no_alert_below() ->
    ServerId = test_server_no_alert,

    %% Accept only 50 connections (50% - below 70% threshold)
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 50)],

    %% Check stats
    Stats = erlmcp_connection_limiter:get_stats(),
    ?assertEqual(50, maps:get(current_connections, Stats)),

    %% Last alert should still be undefined (no alert triggered)
    LastAlert = maps:get(last_alert, Stats),
    ?assertEqual(undefined, LastAlert),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 50)],
    ok.

%% @doc Test alert cooldown (can't wait 1 minute, so verify logic exists)
test_alert_cooldown() ->
    ServerId = test_server_cooldown,

    %% Accept 70 to trigger alert
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 70)],

    Stats1 = erlmcp_connection_limiter:get_stats(),
    LastAlert1 = maps:get(last_alert, Stats1),

    %% Verify alert was set
    ?assertNotEqual(undefined, LastAlert1),

    %% Note: We can't actually test the 1-minute cooldown in unit tests
    %% but we verify the state is tracked
    ?assert(is_integer(LastAlert1)),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 70)],
    ok.

%%====================================================================
%% gproc Counter Tests
%%====================================================================

gproc_counter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Global counter increment", fun test_global_increment/0},
         {"Global counter decrement", fun test_global_decrement/0},
         {"Per-server counter increment", fun test_server_increment/0},
         {"Per-server counter decrement", fun test_server_decrement/0}
     ]}.

%% @doc Test global counter increment (via gproc)
test_global_increment() ->
    ServerId = test_server_gproc_global,

    %% Get baseline
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    %% Accept connection (increments gproc counter)
    accept = erlmcp_connection_limiter:accept_connection(ServerId),

    %% Verify increment
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(CountBefore + 1, CountAfter),

    %% Cleanup
    erlmcp_connection_limiter:release_connection(ServerId).

%% @doc Test global counter decrement
test_global_decrement() ->
    ServerId = test_server_gproc_dec,

    %% Accept connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),

    %% Release connection (decrements gproc counter)
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    %% Verify decrement
    ?assertEqual(CountDuring - 1, CountAfter).

%% @doc Test per-server counter increment
test_server_increment() ->
    ServerId = test_server_gproc_server,

    %% Accept multiple connections
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    accept = erlmcp_connection_limiter:accept_connection(ServerId),

    %% Verify per-server counter (via gproc)
    Count = erlmcp_connection_limiter:get_connection_count(ServerId),
    ?assertEqual(3, Count),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 3)],
    ok.

%% @doc Test per-server counter decrement
test_server_decrement() ->
    ServerId = test_server_gproc_server_dec,

    %% Accept 5 connections
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 5)],
    ?assertEqual(5, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Release 2
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    ok = erlmcp_connection_limiter:release_connection(ServerId),

    %% Verify decrement
    ?assertEqual(3, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 3)],
    ok.

%%====================================================================
%% Resource Management Tests
%%====================================================================

resource_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Cleanup on server stop", fun test_cleanup_on_stop/0},
         {"Recovery from resource exhaustion", fun test_recovery_from_exhaustion/0},
         {"Proper counter cleanup", fun test_counter_cleanup/0}
     ]}.

%% @doc Test cleanup on server stop
test_cleanup_on_stop() ->
    ServerId = test_server_cleanup,

    %% Accept connections
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 50)],
    CountBefore = erlmcp_connection_limiter:get_connection_count(),
    ?assert(CountBefore >= 50),

    %% Stop server
    ok = erlmcp_connection_limiter:stop(),
    timer:sleep(50),

    %% Restart
    {ok, _NewPid} = erlmcp_connection_limiter:start_link(),

    %% Count should be reset to 0
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count()).

%% @doc Test recovery from resource exhaustion
test_recovery_from_exhaustion() ->
    ServerId = test_server_exhaustion,

    %% Fill to limit
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 10000)],
    CountFull = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(10000, CountFull),

    %% Should be refused
    {error, too_many_connections} = erlmcp_connection_limiter:accept_connection(ServerId),

    %% Release half
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 5000)],
    CountHalf = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(5000, CountHalf),

    %% Should be able to accept again
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(5001, CountAfter),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 5001)],
    ok.

%% @doc Test proper counter cleanup
test_counter_cleanup() ->
    ServerId = test_server_counter_cleanup,

    %% Add and remove connections
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 100)],
    ?assertEqual(100, erlmcp_connection_limiter:get_connection_count(ServerId)),

    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 100)],
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Global counter should also be 0
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count()).

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Release without accept (no-op)", fun test_release_without_accept/0},
         {"Multiple releases of same connection", fun test_multiple_releases/0},
         {"Binary server ID", fun test_binary_server_id/0},
         {"Atom server ID", fun test_atom_server_id/0},
         {"Zero connections initially", fun test_zero_initially/0}
     ]}.

%% @doc Test releasing without accept (should be safe no-op)
test_release_without_accept() ->
    ServerId = test_server_no_accept,

    %% Release without accept should not crash
    ok = erlmcp_connection_limiter:release_connection(ServerId),

    %% Count should still be valid
    Count = erlmcp_connection_limiter:get_connection_count(),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0).

%% @doc Test multiple releases of same connection
test_multiple_releases() ->
    ServerId = test_server_multi_release,

    %% Accept one connection
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Release once
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count(ServerId)),

    %% Release again (should be safe no-op, not go negative)
    ok = erlmcp_connection_limiter:release_connection(ServerId),
    Count = erlmcp_connection_limiter:get_connection_count(ServerId),
    ?assert(Count >= 0).

%% @doc Test binary server ID
test_binary_server_id() ->
    ServerId = <<"binary_server_id">>,

    %% Should work with binary server ID
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerId)),

    erlmcp_connection_limiter:release_connection(ServerId).

%% @doc Test atom server ID
test_atom_server_id() ->
    ServerId = atom_server_id,

    %% Should work with atom server ID
    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertEqual(1, erlmcp_connection_limiter:get_connection_count(ServerId)),

    erlmcp_connection_limiter:release_connection(ServerId).

%% @doc Test zero connections initially
test_zero_initially() ->
    %% Initially should have zero connections
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count()),
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count(nonexistent_server)).

%%====================================================================
%% Stress Tests (Lightweight)
%%====================================================================

stress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Accept 1000 connections rapidly", fun test_rapid_accept_1000/0},
         {"Release 1000 connections rapidly", fun test_rapid_release_1000/0}
     ]}.

%% @doc Test accepting 1000 connections rapidly
test_rapid_accept_1000() ->
    ServerId = test_server_rapid,

    %% Accept 1000 rapidly
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    %% Verify count
    ?assertEqual(1000, erlmcp_connection_limiter:get_connection_count()),

    %% Cleanup
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ok.

%% @doc Test releasing 1000 connections rapidly
test_rapid_release_1000() ->
    ServerId = test_server_rapid_release,

    %% Accept 1000
    _ = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assertEqual(1000, erlmcp_connection_limiter:get_connection_count()),

    %% Release 1000 rapidly
    ReleaseResults = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 1000)],
    ?assert(lists:all(fun(ok) -> true; (_) -> false end, ReleaseResults)),

    %% Verify count
    ?assertEqual(0, erlmcp_connection_limiter:get_connection_count()).
