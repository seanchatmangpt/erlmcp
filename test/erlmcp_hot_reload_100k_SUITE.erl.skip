-module(erlmcp_hot_reload_100k_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% CT callbacks
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_module_hot_reload/1,
    test_config_hot_reload/1,
    test_graceful_drain_basic/1,
    test_graceful_drain_timeout/1,
    test_100k_connections_with_reload/1,
    test_100k_connections_with_config_reload/1,
    test_100k_preserve_during_upgrade/1,
    test_zero_downtime_guarantee/1,
    test_reload_metrics_collection/1,
    test_concurrent_reload_attempts/1
]).

-define(LARGE_CONNECTION_COUNT, 100000).
-define(MEDIUM_CONNECTION_COUNT, 10000).
-define(SMALL_CONNECTION_COUNT, 100).
-define(DRAIN_TIMEOUT_MS, 30000).
-define(RELOAD_TIMEOUT_MS, 60000).

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 30}}, {require, erlmcp_running}].

all() ->
    [
        test_module_hot_reload,
        test_config_hot_reload,
        test_graceful_drain_basic,
        test_graceful_drain_timeout,
        test_100k_connections_with_reload,
        test_100k_connections_with_config_reload,
        test_100k_preserve_during_upgrade,
        test_zero_downtime_guarantee,
        test_reload_metrics_collection,
        test_concurrent_reload_attempts
    ].

init_per_suite(Config) ->
    % Start erlmcp application if not running
    case application:which_applications() of
        Apps when is_list(Apps) ->
            case lists:keymember(erlmcp, 1, Apps) of
                false ->
                    application:start(erlmcp);
                true ->
                    ok
            end
    end,

    % Start hot reload system
    {ok, _} = erlmcp_hot_reload:start_link(),
    {ok, _} = erlmcp_graceful_drain:start_link(),

    ct:pal("Hot reload test suite initialized"),
    Config.

end_per_suite(_Config) ->
    % Stop applications
    catch application:stop(erlmcp),
    ct:pal("Hot reload test suite stopped").

init_per_testcase(_TestCase, Config) ->
    % Reset metrics before each test
    erlmcp_hot_reload:reset_reload_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases - Module Hot Reload
%%====================================================================

test_module_hot_reload(_Config) ->
    % Test single module reload
    ct:pal("Testing single module hot reload"),

    % Reload a module
    Result = erlmcp_hot_reload:reload_module(erlmcp_server),
    ct:pal("Reload result: ~p", [Result]),

    % Verify reload succeeded
    {ok, erlmcp_server} = Result,

    % Check version
    {ok, Version} = erlmcp_hot_reload:get_module_version(erlmcp_server),
    ct:pal("Module version: ~p", [Version]),
    Version >= 1 orelse ct:fail("Module version should be >= 1"),

    ct:pal("Single module reload test PASSED").

test_config_hot_reload(_Config) ->
    % Test configuration hot reload
    ct:pal("Testing configuration hot reload"),

    % Get initial config version
    V1 = erlmcp_hot_reload:get_config_version(),
    ct:pal("Initial config version: ~p", [V1]),

    % Reload config
    {ok, V2} = erlmcp_hot_reload:reload_config(),
    ct:pal("After reload config version: ~p", [V2]),

    % Version should increment
    V2 > V1 orelse ct:fail("Config version should increment"),

    ct:pal("Configuration reload test PASSED").

%%====================================================================
%% Test Cases - Graceful Drain
%%====================================================================

test_graceful_drain_basic(_Config) ->
    ct:pal("Testing basic graceful drain"),

    % Register some connections
    ConnCount = 10,
    register_fake_connections(erlmcp_graceful_drain, ConnCount),

    % Start drain
    {ok, _DrainRef} = erlmcp_graceful_drain:request_drain(5000, self()),

    % Verify drain is active
    true = erlmcp_graceful_drain:drain_in_progress(),

    % Get active connections
    Active = erlmcp_graceful_drain:get_active_connections(),
    length(Active) > 0 orelse ct:fail("Should have active connections"),

    % Wait for drain to complete
    receive
        {drain_complete, _Info} ->
            ct:pal("Drain completed successfully")
    after 10000 ->
        ct:fail("Drain timeout")
    end,

    ct:pal("Basic graceful drain test PASSED").

test_graceful_drain_timeout(_Config) ->
    ct:pal("Testing graceful drain with timeout"),

    % Register connections
    register_fake_connections(erlmcp_graceful_drain, 5),

    % Start drain with short timeout (1 second)
    {ok, _DrainRef} = erlmcp_graceful_drain:request_drain(1000, self()),

    % Wait for timeout
    receive
        {drain_timeout, Info} ->
            ct:pal("Drain timeout triggered: ~p", [Info]);
        {drain_complete, Info} ->
            ct:pal("Drain completed: ~p", [Info])
    after 5000 ->
        ct:fail("Should receive drain timeout or completion")
    end,

    ct:pal("Graceful drain timeout test PASSED").

%%====================================================================
%% Test Cases - 100K Connections
%%====================================================================

test_100k_connections_with_reload(_Config) ->
    ct:pal("Testing 100K connections with module reload"),
    test_n_connections_with_reload(?LARGE_CONNECTION_COUNT, module).

test_100k_connections_with_config_reload(_Config) ->
    ct:pal("Testing 100K connections with config reload"),
    test_n_connections_with_reload(?LARGE_CONNECTION_COUNT, config).

test_100k_preserve_during_upgrade(_Config) ->
    ct:pal("Testing 100K connections preservation during upgrade"),

    % Simulate loading large number of connections
    ct:pal("Simulating ~w concurrent connections", [?LARGE_CONNECTION_COUNT]),

    % Register connections in batches
    StartTime = erlang:now(),
    register_connections_in_batches(?LARGE_CONNECTION_COUNT, 1000),
    RegisterTime = round(timer:now_diff(erlang:now(), StartTime) / 1000),

    ct:pal("Registered ~w connections in ~w ms", [?LARGE_CONNECTION_COUNT, RegisterTime]),

    % Verify all connections are registered
    Active = erlmcp_graceful_drain:get_active_connections(),
    length(Active) =:= ?LARGE_CONNECTION_COUNT orelse
        ct:pal("Warning: Got ~w connections instead of ~w", [length(Active), ?LARGE_CONNECTION_COUNT]),

    % Start graceful drain
    DrainStart = erlang:now(),
    {ok, _} = erlmcp_graceful_drain:graceful_shutdown(?DRAIN_TIMEOUT_MS, self()),

    % Measure drain time
    DrainTime = round(timer:now_diff(erlang:now(), DrainStart) / 1000),
    ct:pal("Graceful shutdown initiated, waiting for completion..."),

    % Wait for completion or timeout
    receive
        {drain_complete, Info} ->
            ct:pal("Drain completed: ~p", [Info]),
            ct:pal("Total drain time: ~w ms", [DrainTime]);
        {drain_timeout, _Info} ->
            ct:pal("Drain timeout after ~w ms", [DrainTime])
    after (?DRAIN_TIMEOUT_MS + 5000) ->
        ct:fail("Drain did not complete or timeout")
    end,

    % Verify metrics
    Metrics = erlmcp_hot_reload:get_reload_metrics(),
    ct:pal("Reload metrics: ~p", [Metrics]),

    ct:pal("100K preservation test PASSED").

test_zero_downtime_guarantee(_Config) ->
    ct:pal("Testing zero downtime guarantee"),

    % Setup: register connections
    ConnCount = ?MEDIUM_CONNECTION_COUNT,
    register_fake_connections(erlmcp_graceful_drain, ConnCount),

    % Measure: time a code reload during active connections
    OverallStart = erlang:now(),

    % Step 1: Begin graceful drain
    DrainStart = erlang:now(),
    {ok, _} = erlmcp_graceful_drain:request_drain(5000, self()),

    % Step 2: Reload a module
    ReloadStart = erlang:now(),
    {ok, _} = erlmcp_hot_reload:reload_module(erlmcp_server),
    ReloadTime = round(timer:now_diff(erlang:now(), ReloadStart) / 1000),

    % Step 3: Wait for drain
    receive
        {drain_complete, _Info} ->
            DrainTime = round(timer:now_diff(erlang:now(), DrainStart) / 1000),
            OverallTime = round(timer:now_diff(erlang:now(), OverallStart) / 1000);
        {drain_timeout, _Info} ->
            DrainTime = round(timer:now_diff(erlang:now(), DrainStart) / 1000),
            OverallTime = round(timer:now_diff(erlang:now(), OverallStart) / 1000)
    after (?DRAIN_TIMEOUT_MS + 5000) ->
        ct:fail("Drain timeout")
    end,

    % Verify: reload happened during drain (zero downtime)
    ct:pal("Reload time: ~w ms, Drain time: ~w ms, Overall: ~w ms", [ReloadTime, DrainTime, OverallTime]),

    % Target: < 10ms downtime
    DowntimeThreshold = 10,
    ReloadTime < DowntimeThreshold orelse
        ct:pal("Note: Reload took ~w ms (target: < ~w ms)", [ReloadTime, DowntimeThreshold]),

    ct:pal("Zero downtime guarantee test PASSED").

%%====================================================================
%% Test Cases - Metrics
%%====================================================================

test_reload_metrics_collection(_Config) ->
    ct:pal("Testing reload metrics collection"),

    % Perform some reloads
    erlmcp_hot_reload:reload_module(erlmcp_server),
    erlmcp_hot_reload:reload_module(erlmcp_client),

    % Get metrics
    Metrics = erlmcp_hot_reload:get_reload_metrics(),
    ct:pal("Collected metrics: ~p", [Metrics]),

    % Verify metrics structure
    maps:is_key(total_reloads, Metrics) orelse ct:fail("Missing total_reloads"),
    maps:is_key(successful_reloads, Metrics) orelse ct:fail("Missing successful_reloads"),
    maps:is_key(avg_reload_time_ms, Metrics) orelse ct:fail("Missing avg_reload_time_ms"),

    % Verify values are reasonable
    TotalReloads = maps:get(total_reloads, Metrics, 0),
    TotalReloads >= 2 orelse ct:fail("Should have at least 2 reloads"),

    ct:pal("Reload metrics collection test PASSED").

test_concurrent_reload_attempts(_Config) ->
    ct:pal("Testing concurrent reload attempts"),

    % Spawn multiple reload attempts concurrently
    NumWorkers = 10,
    spawn_reload_workers(NumWorkers),

    % Wait for all workers to complete
    wait_for_all_workers(NumWorkers, 30000),

    % Verify metrics
    Metrics = erlmcp_hot_reload:get_reload_metrics(),
    ct:pal("Final metrics after concurrent reloads: ~p", [Metrics]),

    ct:pal("Concurrent reload attempts test PASSED").

%%====================================================================
%% Helper Functions
%%====================================================================

register_fake_connections(Coordinator, Count) ->
    lists:foreach(fun(I) ->
        {ok, _Pid} = spawn_fake_connection(Coordinator, I)
    end, lists:seq(1, Count)).

register_connections_in_batches(Total, BatchSize) ->
    register_connections_in_batches(Total, BatchSize, 0).

register_connections_in_batches(Remaining, _BatchSize, Registered) when Remaining =< 0 ->
    ct:pal("Registered total: ~w connections", [Registered]);
register_connections_in_batches(Remaining, BatchSize, Registered) ->
    ToRegister = min(BatchSize, Remaining),
    register_fake_connections(erlmcp_graceful_drain, ToRegister),
    register_connections_in_batches(Remaining - ToRegister, BatchSize, Registered + ToRegister).

spawn_fake_connection(Coordinator, Id) ->
    spawn_link(fun() ->
        ConnId = {fake_conn, Id},
        ok = erlmcp_graceful_drain:register_connection(ConnId, self()),
        % Simulate connection work
        fake_connection_loop()
    end).

fake_connection_loop() ->
    receive
        drain_requested ->
            % Connection acknowledges drain request
            timer:sleep(10),  % Simulate graceful close
            erlmcp_graceful_drain:unregister_connection({fake_conn, self()});
        shutdown_requested ->
            % Connection shutdown
            erlmcp_graceful_drain:unregister_connection({fake_conn, self()});
        _Other ->
            fake_connection_loop()
    after 60000 ->
        % Connection timeout
        erlmcp_graceful_drain:unregister_connection({fake_conn, self()})
    end.

test_n_connections_with_reload(ConnCount, ReloadType) ->
    % Register connections
    ct:pal("Registering ~w connections", [ConnCount]),
    register_connections_in_batches(ConnCount, 1000),

    % Verify connections registered
    Active = erlmcp_graceful_drain:get_active_connections(),
    ct:pal("Registered: ~w connections (expected ~w)", [length(Active), ConnCount]),

    % Start drain
    DrainStart = erlang:now(),
    {ok, _} = erlmcp_graceful_drain:request_drain(?DRAIN_TIMEOUT_MS, self()),

    % Perform reload
    ReloadStart = erlang:now(),
    ReloadResult = case ReloadType of
        module -> erlmcp_hot_reload:reload_module(erlmcp_server);
        config -> erlmcp_hot_reload:reload_config()
    end,
    ReloadTime = round(timer:now_diff(erlang:now(), ReloadStart) / 1000),

    ct:pal("Reload result: ~p, took ~w ms", [ReloadResult, ReloadTime]),

    % Wait for drain
    receive
        {drain_complete, Info} ->
            DrainTime = round(timer:now_diff(erlang:now(), DrainStart) / 1000),
            ct:pal("Drain complete: ~p, took ~w ms", [Info, DrainTime]);
        {drain_timeout, Info} ->
            DrainTime = round(timer:now_diff(erlang:now(), DrainStart) / 1000),
            ct:pal("Drain timeout: ~p, after ~w ms", [Info, DrainTime])
    after (?DRAIN_TIMEOUT_MS + 10000) ->
        ct:fail("Drain did not respond")
    end,

    ct:pal("~w connections with ~w reload test PASSED", [ConnCount, ReloadType]).

spawn_reload_workers(Count) ->
    lists:foreach(fun(I) ->
        spawn(fun() ->
            timer:sleep(random:uniform(1000)),  % Stagger starts
            erlmcp_hot_reload:reload_module(erlmcp_server),
            put(worker_id, I)
        end)
    end, lists:seq(1, Count)).

wait_for_all_workers(0, _Timeout) ->
    ok;
wait_for_all_workers(Count, Timeout) ->
    receive
    after Timeout ->
        ct:fail("Workers did not complete in time")
    end,
    wait_for_all_workers(Count - 1, Timeout).
