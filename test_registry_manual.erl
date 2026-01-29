#!/usr/bin/env escript
%%% Test script for new erlmcp_registry functions

main(_) ->
    io:format("Testing new erlmcp_registry functions...~n~n"),

    % Start gproc
    application:ensure_all_started(gproc),

    % Start the registry
    {ok, Pid} = erlmcp_registry:start_link(),
    io:format("Started registry: ~p~n", [Pid]),

    % Test 1: get_pid/0
    io:format("~n=== Test 1: get_pid/0 ===~n"),
    RegistryPid = erlmcp_registry:get_pid(),
    io:format("Registry PID: ~p~n", [RegistryPid]),
    io:format("PID matches: ~p~n", [RegistryPid =:= Pid]),

    % Test 2: get_all_state/0
    io:format("~n=== Test 2: get_all_state/0 ===~n"),
    {ok, State} = erlmcp_registry:get_all_state(),
    io:format("State: ~p~n", [State]),
    io:format("State is a tuple: ~p~n", [is_tuple(State)]),
    io:format("State record name: ~p~n", [element(1, State)]),

    % Test 3: get_queue_depth/0
    io:format("~n=== Test 3: get_queue_depth/0 ===~n"),
    QueueDepth = erlmcp_registry:get_queue_depth(),
    io:format("Queue depth: ~p~n", [QueueDepth]),
    io:format("Is non-negative: ~p~n", [QueueDepth >= 0]),

    % Test 4: restore_state/1
    io:format("~n=== Test 4: restore_state/1 ===~n"),
    % Create a new state record with setelement
    ModifiedState = setelement(3, State, #{test_transport => test_server}),
    RestoreResult = erlmcp_registry:restore_state(ModifiedState),
    io:format("Restore result: ~p~n", [RestoreResult]),
    {ok, RestoredState} = erlmcp_registry:get_all_state(),
    io:format("Restored state: ~p~n", [RestoredState]),
    io:format("Map restored: ~p~n", [element(3, RestoredState)]),

    % Test 5: route_message/2 to server
    io:format("~n=== Test 5: route_message/2 to server ===~n"),

    % Create mock server and transport
    Server = spawn(fun() ->
        receive
            {mcp_message, TransportId, Message} ->
                io:format("Server received message from ~p: ~p~n", [TransportId, Message])
        after 5000 ->
            io:format("Server timeout~n")
        end
    end),

    Transport = spawn(fun() ->
        receive
            {mcp_response, ServerId, Message} ->
                io:format("Transport received message from ~p: ~p~n", [ServerId, Message])
        after 5000 ->
            io:format("Transport timeout~n")
        end
    end),

    % Register and bind
    ok = erlmcp_registry:register_server(test_server, Server, #{}),
    ok = erlmcp_registry:register_transport(test_transport, Transport,
        #{type => stdio, server_id => test_server}),
    ok = erlmcp_registry:bind_transport_to_server(test_transport, test_server),

    % Route message to server
    TestMessage1 = #{<<"method">> => <<"test">>},
    RouteResult1 = erlmcp_registry:route_message({server, test_server}, TestMessage1),
    io:format("Route to server result: ~p~n", [RouteResult1]),

    timer:sleep(100),

    % Test 6: route_message/2 to transport
    io:format("~n=== Test 6: route_message/2 to transport ===~n"),
    TestMessage2 = #{<<"result">> => <<"success">>},
    RouteResult2 = erlmcp_registry:route_message({transport, test_transport}, TestMessage2),
    io:format("Route to transport result: ~p~n", [RouteResult2]),

    timer:sleep(100),

    % Test 7: route_message/2 error cases
    io:format("~n=== Test 7: route_message/2 error cases ===~n"),
    ErrorResult1 = erlmcp_registry:route_message({server, nonexistent}, #{<<>> => <<>>}),
    io:format("Route to nonexistent server: ~p~n", [ErrorResult1]),
    ErrorResult2 = erlmcp_registry:route_message({transport, nonexistent}, #{<<>> => <<>>}),
    io:format("Route to nonexistent transport: ~p~p~n", [ErrorResult2]),

    % Cleanup
    erlmcp_registry:unregister_server(test_server),
    erlmcp_registry:unregister_transport(test_transport),

    gen_server:stop(erlmcp_registry),

    io:format("~n=== All tests completed successfully ===~n").
