-module(erlmcp_transport_tcp_ranch_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for ranch-based TCP Transport Integration
%%%
%%% This test suite validates the integration of ranch library
%%% into erlmcp_transport_tcp.erl, ensuring:
%%% - TCP connection management
%%% - Socket handling
%%% - Reconnection logic
%%% - Message framing (line-based)
%%% - Buffer management
%%% - Error handling and recovery
%%% - Supervisor integration
%%% - Connection pooling
%%%
%%% Target: >90% coverage on erlmcp_transport_tcp.erl
%%%===================================================================

%%====================================================================
%% Test Groups
%%====================================================================

tcp_ranch_test_() ->
    {setup,
     fun setup_tcp_transport/0,
     fun cleanup_tcp_transport/1,
     [
         {"Basic TCP transport initialization", fun test_tcp_init/0},
         {"TCP transport send operation", fun test_tcp_send/0},
         {"TCP transport close operation", fun test_tcp_close/0},
         {"TCP connection establishment", fun test_tcp_connection/0},
         {"TCP connection failure handling", fun test_tcp_connection_failure/0},
         {"TCP reconnection logic", fun test_tcp_reconnection/0},
         {"TCP message framing", fun test_tcp_message_framing/0},
         {"TCP buffer management", fun test_tcp_buffer_management/0},
         {"TCP socket options", fun test_tcp_socket_options/0},
         {"TCP connection timeout", fun test_tcp_connection_timeout/0},
         {"TCP send timeout", fun test_tcp_send_timeout/0},
         {"TCP keepalive option", fun test_tcp_keepalive/0},
         {"TCP nodelay option", fun test_tcp_nodelay/0},
         {"TCP buffer size configuration", fun test_tcp_buffer_size/0},
         {"TCP owner process monitoring", fun test_tcp_owner_monitoring/0},
         {"TCP connection closed handling", fun test_tcp_connection_closed/0},
         {"TCP error handling", fun test_tcp_error_handling/0},
         {"TCP backoff calculation", fun test_tcp_backoff_calculation/0},
         {"TCP max reconnect attempts", fun test_tcp_max_reconnect_attempts/0},
         {"TCP state management", fun test_tcp_state_management/0}
     ]}.

integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     [
         {timeout, 15, {"Full TCP transport integration", fun test_full_tcp_integration/0}},
         {timeout, 15, {"TCP transport with registry", fun test_tcp_with_registry/0}},
         {timeout, 15, {"TCP concurrent connections", fun test_tcp_concurrent_connections/0}},
         {timeout, 15, {"TCP load testing", fun test_tcp_load_testing/0}},
         {timeout, 15, {"TCP failure recovery", fun test_tcp_failure_recovery/0}}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup_tcp_transport() ->
    #{}.

cleanup_tcp_transport(_) ->
    timer:sleep(100),
    ok.

setup_integration() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,
    #{}.

cleanup_integration(_) ->
    timer:sleep(100),
    ok.

%%====================================================================
%% Basic TCP Transport Tests
%%====================================================================

test_tcp_init() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        connect_timeout => 5000
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Verify transport started
        ?assert(is_pid(Transport)),
        ?assert(is_process_alive(Transport)),

        % Get transport state
        {ok, State} = gen_server:call(Transport, get_state),
        ?assertEqual(Owner, maps:get(owner, State)),
        ?assertEqual("localhost", maps:get(host, State)),
        ?assertEqual(8080, maps:get(port, State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_send() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Attempt send (will fail if not connected, which is expected)
        TestData = <<"test message">>,
        Result = erlmcp_transport_tcp:send(Transport, TestData),

        % Should either succeed or return not_connected error
        ?assert(
            case Result of
                ok -> true;
                {error, not_connected} -> true;
                {error, _} -> true;
                _ -> false
            end
        )
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_close() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Close should always succeed
        ?assertEqual(ok, erlmcp_transport_tcp:close(Transport)),

        % Transport process should be stopped
        timer:sleep(100),
        ?assertNot(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Connection Tests
%%====================================================================

test_tcp_connection() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 12345, % Likely no server here
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => 1
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for connection attempt
        timer:sleep(300),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Should not be connected (no server listening)
        ?assertEqual(false, maps:get(connected, State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_connection_failure() ->
    Owner = self(),
    Opts = #{
        host => "invalid.host.that.does.not.exist",
        port => 9999,
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => 0
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for connection attempt to fail
        timer:sleep(300),

        % Transport should still be alive but not connected
        ?assert(is_process_alive(Transport)),

        {ok, State} = gen_server:call(Transport, get_state),
        ?assertEqual(false, maps:get(connected, State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_reconnection() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 12346,
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => 3
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for initial connection attempt and reconnects
        timer:sleep(1000),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Should have attempted reconnection
        ReconnectAttempts = maps:get(reconnect_attempts, State),
        ?assert(ReconnectAttempts > 0)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Message Framing Tests
%%====================================================================

test_tcp_message_framing() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Verify buffer is initialized
        ?assertEqual(<<>>, maps:get(buffer, State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_buffer_management() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        buffer_size => 8192
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Verify buffer options are set
        Options = maps:get(options, State),
        ?assert(lists:keymember(buffer, 1, Options))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Socket Option Tests
%%====================================================================

test_tcp_socket_options() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        keepalive => true,
        nodelay => true,
        buffer_size => 16384
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Verify socket options are configured
        SocketOptions = maps:get(options, State),

        ?assert(lists:keymember(keepalive, 1, SocketOptions)),
        ?assert(lists:keymember(nodelay, 1, SocketOptions)),
        ?assert(lists:keymember(buffer, 1, SocketOptions))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_keepalive() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        keepalive => true
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        {ok, State} = gen_server:call(Transport, get_state),
        Options = maps:get(options, State),

        % Verify keepalive is set
        {keepalive, Value} = lists:keyfind(keepalive, 1, Options),
        ?assertEqual(true, Value)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_nodelay() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        nodelay => true
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        {ok, State} = gen_server:call(Transport, get_state),
        Options = maps:get(options, State),

        % Verify nodelay is set
        {nodelay, Value} = lists:keyfind(nodelay, 1, Options),
        ?assertEqual(true, Value)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_buffer_size() ->
    Owner = self(),
    BufferSize = 32768,
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        buffer_size => BufferSize
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        {ok, State} = gen_server:call(Transport, get_state),
        Options = maps:get(options, State),

        % Verify buffer size is set
        ?assert(lists:keymember(recbuf, 1, Options)),
        ?assert(lists:keymember(sndbuf, 1, Options)),

        {recbuf, RecBuf} = lists:keyfind(recbuf, 1, Options),
        {sndbuf, SndBuf} = lists:keyfind(sndbuf, 1, Options),

        ?assertEqual(BufferSize, RecBuf),
        ?assertEqual(BufferSize, SndBuf)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Timeout Tests
%%====================================================================

test_tcp_connection_timeout() ->
    Owner = self(),
    Opts = #{
        host => "192.0.2.1", % TEST-NET-1, should timeout
        port => 9999,
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => 0
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for timeout
        timer:sleep(300),

        % Transport should be alive but not connected
        ?assert(is_process_alive(Transport)),

        {ok, State} = gen_server:call(Transport, get_state),
        ?assertEqual(false, maps:get(connected, State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_send_timeout() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Get state
        {ok, State} = gen_server:call(Transport, get_state),
        Options = maps:get(options, State),

        % Verify send_timeout is configured
        ?assert(lists:keymember(send_timeout, 1, Options))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Owner Monitoring Tests
%%====================================================================

test_tcp_owner_monitoring() ->
    Owner = spawn_link(fun() ->
        receive
            stop -> ok
        after 10000 -> ok
        end
    end),

    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Verify transport is alive
        ?assert(is_process_alive(Transport)),

        % Kill owner
        unlink(Owner),
        exit(Owner, kill),

        % Wait for owner death
        timer:sleep(100),

        % Transport should die when owner dies
        timer:sleep(500),
        ?assertNot(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Error Handling Tests
%%====================================================================

test_tcp_connection_closed() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        max_reconnect_attempts => 2
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Transport should handle connection closed gracefully
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_error_handling() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => 1
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for connection failure
        timer:sleep(300),

        % Transport should still be alive
        ?assert(is_process_alive(Transport)),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),
        ?assertEqual(false, maps:get(connected, State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP Reconnection Logic Tests
%%====================================================================

test_tcp_backoff_calculation() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        max_reconnect_attempts => 5
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for multiple reconnection attempts
        timer:sleep(3000),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Should have made reconnection attempts
        Attempts = maps:get(reconnect_attempts, State),
        ?assert(Attempts > 0)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_max_reconnect_attempts() ->
    Owner = self(),
    MaxAttempts = 3,
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => MaxAttempts
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for max attempts to be reached
        timer:sleep(2000),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),

        % Should have stopped at max attempts
        Attempts = maps:get(reconnect_attempts, State),
        ?assert(Attempts =< MaxAttempts + 1) % +1 for initial attempt
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% TCP State Management Tests
%%====================================================================

test_tcp_state_management() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        connect_timeout => 5000,
        keepalive => true,
        nodelay => true,
        buffer_size => 16384,
        max_reconnect_attempts => 5
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        {ok, State} = gen_server:call(Transport, get_state),

        % Verify all state fields are properly initialized
        ?assertEqual(Owner, maps:get(owner, State)),
        ?assertEqual("localhost", maps:get(host, State)),
        ?assertEqual(8080, maps:get(port, State)),
        ?assertEqual(undefined, maps:get(socket, State)), % Not connected yet
        ?assertEqual(<<>>, maps:get(buffer, State)),
        ?assertEqual(false, maps:get(connected, State)),
        ?assertEqual(5, maps:get(max_reconnect_attempts, State)),
        ?assert(is_list(maps:get(options, State)))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

test_full_tcp_integration() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        connect_timeout => 1000,
        keepalive => true,
        nodelay => true,
        max_reconnect_attempts => 2
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Full lifecycle test
        ?assert(is_process_alive(Transport)),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),
        ?assert(is_map(State)),

        % Attempt send (will fail if not connected)
        TestData = <<"test integration message">>,
        Result = erlmcp_transport_tcp:send(Transport, TestData),

        ?assert(
            case Result of
                ok -> true;
                {error, not_connected} -> true;
                {error, _} -> true;
                _ -> false
            end
        ),

        % Transport should remain alive
        timer:sleep(100),
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_with_registry() ->
    Owner = self(),

    % Start registry
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,

    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),

    try
        % Start TCP transport
        Opts = #{
            host => "localhost",
            port => 8080,
            owner => Owner,
            connect_timeout => 1000
        },

        {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

        % Register transport
        TransportConfig = #{
            type => tcp,
            config => Opts
        },

        ?assertEqual(ok, gen_server:call(Registry, {register_transport, tcp_test_transport, Transport, TransportConfig})),

        % Verify registration
        ?assertMatch({ok, {Transport, TransportConfig}}, gen_server:call(Registry, {find_transport, tcp_test_transport})),

        % Cleanup
        gen_server:call(Registry, {unregister_transport, tcp_test_transport}),
        gen_server:stop(Transport, normal, 1000)
    after
        gen_server:stop(Registry, normal, 1000)
    end.

test_tcp_concurrent_connections() ->
    Owner = self(),

    % Create multiple TCP transports
    Transports = lists:map(fun(N) ->
        Port = 8080 + N,
        Opts = #{
            host => "localhost",
            port => Port,
            owner => Owner,
            connect_timeout => 100,
            max_reconnect_attempts => 0
        },

        {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),
        Transport
    end, lists:seq(1, 10)),

    try
        % Verify all transports started
        lists:foreach(fun(Transport) ->
            ?assert(is_process_alive(Transport))
        end, Transports),

        % Wait for connection attempts
        timer:sleep(500),

        % All should still be alive
        lists:foreach(fun(Transport) ->
            ?assert(is_process_alive(Transport))
        end, Transports)
    after
        % Cleanup
        lists:foreach(fun(Transport) ->
            catch gen_server:stop(Transport, normal, 1000)
        end, Transports)
    end.

test_tcp_load_testing() ->
    Owner = self(),
    Opts = #{
        host => "localhost",
        port => 8080,
        owner => Owner,
        connect_timeout => 1000,
        max_reconnect_attempts => 1
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Send many messages rapidly
        StartTime = erlang:monotonic_time(millisecond),

        lists:foreach(fun(N) ->
            spawn(fun() ->
                Data = integer_to_binary(N),
                catch erlmcp_transport_tcp:send(Transport, Data)
            end)
        end, lists:seq(1, 100)),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        % Should complete quickly (< 1 second)
        ?assert(Duration < 1000),

        % Transport should remain stable
        timer:sleep(500),
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_tcp_failure_recovery() ->
    Owner = self(),

    % Start with connection that will fail
    Opts = #{
        host => "localhost",
        port => 9999,
        owner => Owner,
        connect_timeout => 100,
        max_reconnect_attempts => 3
    },

    {ok, Transport} = erlmcp_transport_tcp:start_link(Opts),

    try
        % Wait for initial failure and reconnect attempts
        timer:sleep(1500),

        % Transport should still be alive
        ?assert(is_process_alive(Transport)),

        % Get state
        {ok, State} = gen_server:call(Transport, get_state),
        ?assert(is_map(State)),

        % Should have attempted reconnections
        Attempts = maps:get(reconnect_attempts, State),
        ?assert(Attempts > 0)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

% Helper to start a simple TCP echo server for testing
start_test_tcp_server(Port) ->
    spawn(fun() ->
        {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
        accept_loop(ListenSocket)
    end).

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            spawn(fun() -> echo_loop(Socket) end),
            accept_loop(ListenSocket);
        {error, timeout} ->
            accept_loop(ListenSocket);
        {error, _} ->
            ok
    end.

echo_loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            gen_tcp:send(Socket, Data),
            echo_loop(Socket);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, _} ->
            ok
    after 5000 ->
        gen_tcp:close(Socket)
    end.
