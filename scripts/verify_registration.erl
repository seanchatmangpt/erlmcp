#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -pa apps/*/ebin

%%====================================================================
%% Transport Registration Verification Script
%%====================================================================

main([]) ->
    io:format("~n=== Transport Registration Verification ===~n~n"),

    %% Add code paths
    true = code:add_pathz("_build/default/lib/gproc/ebin"),
    true = code:add_pathz("_build/default/lib/erlmcp_core/ebin"),
    true = code:add_pathz("_build/default/lib/erlmcp_transports/ebin"),

    %% Start required applications
    io:format("Starting applications...~n"),
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = erlmcp_registry:start_link(),

    %% Test stdio transport
    test_stdio_registration(),

    %% Test TCP transport
    test_tcp_registration(),

    io:format("~n=== All registration tests passed! ===~n"),
    ok.

test_stdio_registration() ->
    io:format("~nTesting stdio transport registration...~n"),

    TransportId = test_stdio_transport,
    Owner = self(),

    %% Start without transport_id
    io:format("  1. Starting stdio without transport_id...~n"),
    {ok, Pid1} = erlmcp_transport_stdio:start_link(Owner),
    Result1 = erlmcp_registry:find_transport(TransportId),
    case Result1 of
        {error, not_found} ->
            io:format("     ✓ Not registered (as expected)~n");
        _ ->
            io:format("     ✗ FAILED: Should not be registered, got: ~p~n", [Result1]),
            halt(1)
    end,
    erlmcp_transport_stdio:close(Pid1),

    %% Start with transport_id
    io:format("  2. Starting stdio with transport_id...~n"),
    {ok, Pid2} = erlmcp_transport_stdio:start_link(Owner, #{transport_id => TransportId}),
    timer:sleep(100),
    Result2 = erlmcp_registry:find_transport(TransportId),
    case Result2 of
        {ok, {Pid2, _Config}} ->
            io:format("     ✓ Registered successfully (PID: ~p)~n", [Pid2]);
        {error, not_found} ->
            io:format("     ✗ FAILED: Should be registered~n"),
            halt(1);
        Other ->
            io:format("     ✗ FAILED: Unexpected result: ~p~n", [Other]),
            halt(1)
    end,

    %% Test unregistration
    io:format("  3. Closing stdio transport...~n"),
    erlmcp_transport_stdio:close(Pid2),
    timer:sleep(100),
    Result3 = erlmcp_registry:find_transport(TransportId),
    case Result3 of
        {error, not_found} ->
            io:format("     ✓ Unregistered successfully~n");
        _ ->
            io:format("     ✗ FAILED: Should be unregistered, got: ~p~n", [Result3]),
            halt(1)
    end,

    io:format("  ✓ stdio transport registration working~n"),
    ok.

test_tcp_registration() ->
    io:format("~nTesting TCP transport registration...~n"),

    TransportId = test_tcp_transport,
    Owner = self(),

    %% Start TCP client (will fail to connect but should register)
    io:format("  1. Starting TCP client with transport_id...~n"),
    {ok, TcpPid} = erlmcp_transport_tcp:start_client(#{
        host => "localhost",
        port => 19999,
        owner => Owner,
        transport_id => TransportId,
        max_reconnect_attempts => 0
    }),

    timer:sleep(200),
    Result1 = erlmcp_registry:find_transport(TransportId),
    case Result1 of
        {ok, {TcpPid, _Config}} ->
            io:format("     ✓ Registered successfully (PID: ~p)~n", [TcpPid]);
        {error, not_found} ->
            io:format("     ✗ FAILED: Should be registered~n"),
            halt(1);
        Other ->
            io:format("     ✗ FAILED: Unexpected result: ~p~n", [Other]),
            halt(1)
    end,

    %% Test unregistration
    io:format("  2. Stopping TCP client...~n"),
    gen_server:stop(TcpPid),
    timer:sleep(100),
    Result2 = erlmcp_registry:find_transport(TransportId),
    case Result2 of
        {error, not_found} ->
            io:format("     ✓ Unregistered successfully~n");
        _ ->
            io:format("     ✗ FAILED: Should be unregistered, got: ~p~n", [Result2]),
            halt(1)
    end,

    io:format("  ✓ TCP transport registration working~n"),
    ok.
