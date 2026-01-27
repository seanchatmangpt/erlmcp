#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

%% State record definition (must be before main/1)
-record(state, {
    mode,
    transport_id,
    server_id,
    socket,
    ranch_ref,
    owner,
    host,
    port,
    options,
    buffer,
    connected,
    reconnect_timer,
    reconnect_attempts,
    max_reconnect_attempts
}).

main([]) ->
    %% Start required applications
    application:ensure_all_started(ranch),
    application:ensure_all_started(jsx),
    application:ensure_all_started(jesse),

    io:format("~n=== Testing Ranch TCP Transport Integration ===~n~n"),

    %% Test 1: Start server
    io:format("Test 1: Starting TCP server with ranch...~n"),
    ServerOpts = #{
        mode => server,
        port => 0,
        owner => self(),
        transport_id => manual_test_transport,
        server_id => manual_test_server
    },
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
    io:format("  Server started: ~p~n", [ServerPid]),

    %% Get server port
    {ok, ServerState} = gen_server:call(ServerPid, get_state),
    Port = maps:get(port, state_to_map(ServerState)),
    RanchRef = maps:get(ranch_ref, state_to_map(ServerState)),
    io:format("  Server listening on port: ~p~n", [Port]),
    io:format("  Ranch reference: ~p~n", [RanchRef]),

    %% Verify ranch status
    RanchStatus = ranch:get_status(RanchRef),
    io:format("  Ranch status: ~p~n", [RanchStatus]),

    %% Test 2: Start client
    io:format("~nTest 2: Starting TCP client...~n"),
    ClientOpts = #{
        mode => client,
        host => "localhost",
        port => Port,
        owner => self(),
        transport_id => manual_test_client,
        server_id => manual_test_server
    },
    {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),
    io:format("  Client started: ~p~n", [ClientPid]),

    %% Wait for client connection
    receive
        {transport_connected, ClientPid} ->
            io:format("  Client connected successfully~n")
    after 5000 ->
        io:format("  ERROR: Client connection timeout~n"),
        halt(1)
    end,

    %% Wait for server handler
    ServerHandler = receive
        {transport_connected, Handler} when Handler =/= ClientPid ->
            io:format("  Server accepted connection: ~p~n", [Handler]),
            Handler
    after 5000 ->
        io:format("  ERROR: Server did not accept connection~n"),
        halt(1)
    end,

    %% Test 3: Send message from client to server
    io:format("~nTest 3: Sending message from client to server...~n"),
    Message = <<"hello ranch server">>,
    ok = gen_server:call(ClientPid, {send, Message}),
    io:format("  Message sent: ~p~n", [Message]),

    %% Verify message received at server
    receive
        {transport_message, ReceivedMsg} ->
            case ReceivedMsg of
                Message ->
                    io:format("  Message received correctly: ~p~n", [ReceivedMsg]);
                Other ->
                    io:format("  ERROR: Wrong message received: ~p~n", [Other]),
                    halt(1)
            end
    after 3000 ->
        io:format("  ERROR: Message not received at server~n"),
        halt(1)
    end,

    %% Test 4: Close connections
    io:format("~nTest 4: Closing connections...~n"),
    exit(ClientPid, normal),
    timer:sleep(100),
    exit(ServerHandler, normal),
    timer:sleep(100),
    exit(ServerPid, normal),
    timer:sleep(100),
    io:format("  Connections closed~n"),

    io:format("~n=== All Tests Passed! ===~n"),
    halt(0).

%% Helper to convert state record to map
state_to_map(State) when is_tuple(State) ->
    [state | Values] = tuple_to_list(State),
    Fields = record_info(fields, state),
    maps:from_list(lists:zip(Fields, Values)).
