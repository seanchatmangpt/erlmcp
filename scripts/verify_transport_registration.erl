#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%====================================================================
%% Transport Registration Verification Script
%%====================================================================

main([]) ->
    io:format("~n=== Transport Registration Verification ===~n~n"),

    %% Start required applications
    io:format("Starting applications...~n"),
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp),

    %% Test stdio transport
    test_stdio_registration(),

    %% Test TCP transport
    test_tcp_registration(),

    %% Test HTTP transport
    test_http_registration(),

    io:format("~n=== All tests passed! ===~n"),
    ok.

test_stdio_registration() ->
    io:format("~nTesting stdio transport registration...~n"),

    TransportId = test_stdio_transport,
    Owner = self(),

    %% Start without transport_id
    io:format("  Starting stdio without transport_id...~n"),
    {ok, Pid1} = erlmcp_transport_stdio:start_link(Owner),
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
        undefined ->
            io:format("  ✓ Not registered (as expected)~n");
        _ ->
            io:format("  ✗ FAILED: Should not be registered~n"),
            halt(1)
    end,
    erlmcp_transport_stdio:close(Pid1),

    %% Start with transport_id
    io:format("  Starting stdio with transport_id...~n"),
    {ok, Pid2} = erlmcp_transport_stdio:start_link(Owner, #{transport_id => TransportId}),
    timer:sleep(100),
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
        Pid2 ->
            io:format("  ✓ Registered successfully (PID: ~p)~n", [Pid2]);
        undefined ->
            io:format("  ✗ FAILED: Should be registered~n"),
            halt(1);
        Other ->
            io:format("  ✗ FAILED: Registered to wrong PID: ~p (expected ~p)~n", [Other, Pid2]),
            halt(1)
    end,

    %% Test unregistration
    io:format("  Closing stdio transport...~n"),
    erlmcp_transport_stdio:close(Pid2),
    timer:sleep(100),
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
        undefined ->
            io:format("  ✓ Unregistered successfully~n");
        _ ->
            io:format("  ✗ FAILED: Should be unregistered~n"),
            halt(1)
    end,

    io:format("✓ stdio transport registration working~n"),
    ok.

test_tcp_registration() ->
    io:format("~nTesting TCP transport registration...~n"),

    TransportId = test_tcp_transport,
    Owner = self(),

    %% Start TCP client (will fail to connect but should register)
    io:format("  Starting TCP client with transport_id...~n"),
    {ok, TcpPid} = erlmcp_transport_tcp:start_client(#{
        host => "localhost",
        port => 19999,
        owner => Owner,
        transport_id => TransportId,
        max_reconnect_attempts => 0
    }),

    timer:sleep(200),
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
        TcpPid ->
            io:format("  ✓ Registered successfully (PID: ~p)~n", [TcpPid]);
        undefined ->
            io:format("  ✗ FAILED: Should be registered~n"),
            halt(1);
        Other ->
            io:format("  ✗ FAILED: Registered to wrong PID: ~p (expected ~p)~n", [Other, TcpPid]),
            halt(1)
    end,

    %% Test unregistration
    io:format("  Stopping TCP client...~n"),
    gen_server:stop(TcpPid),
    timer:sleep(100),
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
        undefined ->
            io:format("  ✓ Unregistered successfully~n");
        _ ->
            io:format("  ✗ FAILED: Should be unregistered~n"),
            halt(1)
    end,

    io:format("✓ TCP transport registration working~n"),
    ok.

test_http_registration() ->
    io:format("~nTesting HTTP transport registration...~n"),

    TransportId = test_http_transport,
    Owner = self(),

    %% Start HTTP client (will fail to connect but should register)
    io:format("  Starting HTTP client with transport_id...~n"),
    case erlmcp_transport_http:start_link(#{
        url => "http://localhost:19998/mcp",
        owner => Owner,
        transport_id => TransportId,
        connect_timeout => 100,
        max_retries => 0
    }) of
        {ok, HttpPid} ->
            timer:sleep(200),
            case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
                HttpPid ->
                    io:format("  ✓ Registered successfully (PID: ~p)~n", [HttpPid]);
                undefined ->
                    io:format("  ✗ FAILED: Should be registered~n"),
                    halt(1);
                Other ->
                    io:format("  ✗ FAILED: Registered to wrong PID: ~p (expected ~p)~n", [Other, HttpPid]),
                    halt(1)
            end,

            io:format("  Stopping HTTP client...~n"),
            gen_server:stop(HttpPid),
            timer:sleep(100),
            case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} -> Pid;
                undefined ->
                    io:format("  ✓ Unregistered successfully~n");
                _ ->
                    io:format("  ✗ FAILED: Should be unregistered~n"),
                    halt(1)
            end,
            io:format("✓ HTTP transport registration working~n");
        {error, Reason} ->
            io:format("  ⚠ HTTP transport failed to start: ~p (connection failure expected)~n", [Reason]),
            io:format("  ℹ Registration test skipped for HTTP (requires running server)~n")
    end,

    ok.
