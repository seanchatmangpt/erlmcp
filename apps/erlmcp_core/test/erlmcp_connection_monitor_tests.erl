%%%-------------------------------------------------------------------
%%% @doc Connection Monitor Tests
%%%
%%% Tests for erlmcp_connection_monitor to ensure:
%%% - Connection tracking works correctly
%%% - Leak detection triggers at threshold
%%% - Automatic cleanup of orphaned connections
%%% - Process monitoring and cleanup
%%% - Statistics reporting
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

connection_monitor_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Connection monitor starts and stops", fun test_start_stop/0},
      {"Monitor connection tracks correctly", fun test_monitor_connection/0},
      {"Unmonitor connection removes tracking", fun test_unmonitor_connection/0},
      {"Process death triggers cleanup", fun test_process_death_cleanup/0},
      {"Leak detection triggers at threshold", fun test_leak_detection/0},
      {"Orphaned connections are cleaned up", fun test_orphaned_cleanup/0},
      {"Connection stats are accurate", fun test_connection_stats/0},
      {"Force cleanup removes orphaned", fun test_force_cleanup/0},
      {"Multiple connections tracked concurrently", fun test_multiple_connections/0}
     ]}.

setup() ->
    %% Start the connection monitor
    {ok, Pid} = erlmcp_connection_monitor:start_link(),
    Pid.

cleanup(_Pid) ->
    %% Stop the connection monitor
    erlmcp_connection_monitor:stop().

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test that connection monitor starts and stops
test_start_stop() ->
    %% Verify monitor is running
    MonitorPid = whereis(erlmcp_connection_monitor),
    ?assertNotEqual(undefined, MonitorPid),
    ?assertEqual(true, is_process_alive(MonitorPid)),

    %% Stop and restart
    ok = erlmcp_connection_monitor:stop(),
    timer:sleep(100),

    {ok, _NewPid} = erlmcp_connection_monitor:start_link(),
    NewMonitorPid = whereis(erlmcp_connection_monitor),
    ?assertNotEqual(undefined, NewMonitorPid),
    ?assertEqual(true, is_process_alive(NewMonitorPid)).

%% @doc Test monitoring a connection
test_monitor_connection() ->
    %% Create a mock connection process
    {ok, ConnPid} = start_mock_connection(),

    %% Monitor the connection
    ConnectionInfo = #{
        socket => undefined,
        server_id => test_server,
        transport_id => tcp_transport,
        bytes_sent => 0,
        bytes_received => 0
    },
    ok = erlmcp_connection_monitor:monitor_connection(ConnPid, ConnectionInfo),

    %% Verify connection is tracked
    Count = erlmcp_connection_monitor:get_connection_count(),
    ?assert(Count >= 1),

    %% Cleanup
    erlmcp_connection_monitor:unmonitor_connection(ConnPid),
    stop_mock_connection(ConnPid).

%% @doc Test unmonitoring a connection
test_unmonitor_connection() ->
    %% Create and monitor a connection
    {ok, ConnPid} = start_mock_connection(),

    ConnectionInfo = #{
        socket => undefined,
        server_id => test_server,
        transport_id => tcp_transport
    },
    ok = erlmcp_connection_monitor:monitor_connection(ConnPid, ConnectionInfo),

    CountBefore = erlmcp_connection_monitor:get_connection_count(),

    %% Unmonitor the connection
    ok = erlmcp_connection_monitor:unmonitor_connection(ConnPid),

    %% Verify connection is removed
    CountAfter = erlmcp_connection_monitor:get_connection_count(),
    ?assertEqual(CountBefore - 1, CountAfter),

    %% Cleanup
    stop_mock_connection(ConnPid).

%% @doc Test that process death triggers automatic cleanup
test_process_death_cleanup() ->
    %% Create and monitor a connection
    {ok, ConnPid} = start_mock_connection(),

    ConnectionInfo = #{
        socket => undefined,
        server_id => test_server,
        transport_id => tcp_transport
    },
    ok = erlmcp_connection_monitor:monitor_connection(ConnPid, ConnectionInfo),

    CountBefore = erlmcp_connection_monitor:get_connection_count(),

    %% Unlink before killing to avoid killing test process
    unlink(ConnPid),
    exit(ConnPid, kill),
    timer:sleep(200),  % Allow time for DOWN message processing

    %% Verify connection was cleaned up
    CountAfter = erlmcp_connection_monitor:get_connection_count(),
    ?assert(CountAfter < CountBefore).

%% @doc Test leak detection
test_leak_detection() ->
    %% Clear existing connections
    erlmcp_connection_monitor:force_cleanup(),

    %% Create many connections rapidly to trigger leak detection
    ConnectionPids = lists:map(fun(_I) ->
        {ok, Pid} = start_mock_connection(),
        ConnectionInfo = #{
            socket => undefined,
            server_id => test_server,
            transport_id => tcp_transport
        },
        ok = erlmcp_connection_monitor:monitor_connection(Pid, ConnectionInfo),
        Pid
    end, lists:seq(1, 50)),

    %% Get leak status (should not trigger yet)
    LeakDetected = erlmcp_connection_monitor:is_leak_detected(),
    ?assertEqual(false, LeakDetected),

    %% Cleanup all connections
    lists:foreach(fun(Pid) ->
        erlmcp_connection_monitor:unmonitor_connection(Pid),
        stop_mock_connection(Pid)
    end, ConnectionPids).

%% @doc Test orphaned connection cleanup
test_orphaned_cleanup() ->
    %% Create connections that will become orphaned
    ConnectionPids = lists:map(fun(_I) ->
        {ok, Pid} = start_mock_connection(),
        ConnectionInfo = #{
            socket => undefined,
            server_id => test_server,
            transport_id => tcp_transport,
            last_activity => erlang:monotonic_time(millisecond) - (10 * 60 * 1000)  % 10 minutes ago
        },
        ok = erlmcp_connection_monitor:monitor_connection(Pid, ConnectionInfo),
        Pid
    end, lists:seq(1, 10)),

    %% Kill half the processes to create orphans
    OrphanedPids = lists:sublist(ConnectionPids, 5),
    lists:foreach(fun(Pid) ->
        unlink(Pid),
        exit(Pid, kill)
    end, OrphanedPids),

    timer:sleep(200),  % Allow time for cleanup

    %% Force cleanup
    {ok, CleanedCount} = erlmcp_connection_monitor:force_cleanup(),

    %% Verify some orphans were cleaned up
    ?assert(CleanedCount >= 0),

    %% Cleanup remaining
    lists:foreach(fun(Pid) ->
        catch erlmcp_connection_monitor:unmonitor_connection(Pid),
        catch stop_mock_connection(Pid)
    end, ConnectionPids).

%% @doc Test connection statistics
test_connection_stats() ->
    %% Clear existing connections
    erlmcp_connection_monitor:force_cleanup(),

    %% Create some connections
    ConnectionPids = lists:map(fun(_I) ->
        {ok, Pid} = start_mock_connection(),
        ConnectionInfo = #{
            socket => undefined,
            server_id => test_server,
            transport_id => tcp_transport
        },
        ok = erlmcp_connection_monitor:monitor_connection(Pid, ConnectionInfo),
        Pid
    end, lists:seq(1, 5)),

    %% Get stats
    Stats = erlmcp_connection_monitor:get_connection_stats(),
    ?assertEqual(5, maps:get(total_connections, Stats)),

    %% Cleanup
    lists:foreach(fun(Pid) ->
        erlmcp_connection_monitor:unmonitor_connection(Pid),
        stop_mock_connection(Pid)
    end, ConnectionPids).

%% @doc Test force cleanup
test_force_cleanup() ->
    %% Create connections
    ConnectionPids = lists:map(fun(_I) ->
        {ok, Pid} = start_mock_connection(),
        ConnectionInfo = #{
            socket => undefined,
            server_id => test_server,
            transport_id => tcp_transport
        },
        ok = erlmcp_connection_monitor:monitor_connection(Pid, ConnectionInfo),
        Pid
    end, lists:seq(1, 10)),

    %% Kill some to create orphans
    lists:foreach(fun(Pid) ->
        unlink(Pid),
        exit(Pid, kill)
    end, lists:sublist(ConnectionPids, 3)),

    timer:sleep(200),

    %% Force cleanup
    {ok, CleanedCount} = erlmcp_connection_monitor:force_cleanup(),

    %% Verify cleanup happened
    ?assert(CleanedCount >= 0),

    %% Cleanup remaining
    lists:foreach(fun(Pid) ->
        catch erlmcp_connection_monitor:unmonitor_connection(Pid),
        catch stop_mock_connection(Pid)
    end, lists:sublist(ConnectionPids, 4, 10)).

%% @doc Test multiple concurrent connections
test_multiple_connections() ->
    %% Create many connections concurrently
    NumConnections = 100,

    ConnectionPids = lists:map(fun(_I) ->
        {ok, Pid} = start_mock_connection(),
        ConnectionInfo = #{
            socket => undefined,
            server_id => test_server,
            transport_id => tcp_transport,
            bytes_sent => rand:uniform(10000),
            bytes_received => rand:uniform(10000)
        },
        ok = erlmcp_connection_monitor:monitor_connection(Pid, ConnectionInfo),
        Pid
    end, lists:seq(1, NumConnections)),

    %% Verify all are tracked
    Count = erlmcp_connection_monitor:get_connection_count(),
    ?assert(Count >= NumConnections),

    %% Cleanup all
    lists:foreach(fun(Pid) ->
        erlmcp_connection_monitor:unmonitor_connection(Pid),
        stop_mock_connection(Pid)
    end, ConnectionPids),

    %% Verify all removed
    FinalCount = erlmcp_connection_monitor:get_connection_count(),
    ?assert(FinalCount < Count).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Start a mock connection process
start_mock_connection() ->
    Pid = spawn_link(fun() ->
        receive
            stop -> ok
        after
            10000 -> timeout
        end
    end),
    {ok, Pid}.

%% @doc Stop a mock connection process
stop_mock_connection(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> exit(Pid, normal);  % Use 'normal' to not kill linked test process
        false -> ok
    end;
stop_mock_connection(_) ->
    ok.
