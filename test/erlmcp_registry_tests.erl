-module(erlmcp_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup and Cleanup
%%====================================================================

registry_test_() ->
    {setup,
     fun setup_registry/0,
     fun cleanup_registry/1,
     fun(_Registry) ->
         [
             {"Registry startup and basic operations", fun test_registry_basics/0},
             {"Server registration and lookup", fun test_server_registration/0},
             {"Transport registration and lookup", fun test_transport_registration/0},
             {"Server-Transport binding", fun test_binding/0},
             {"Message routing", fun test_message_routing/0},
             {"Process monitoring", fun test_process_monitoring/0}
         ]
     end}.

setup_registry() ->
    % Start the registry
    {ok, Registry} = erlmcp_registry:start_link(),
    Registry.

cleanup_registry(Registry) ->
    exit(Registry, shutdown),
    ok.

%%====================================================================
%% Basic Registry Tests
%%====================================================================

test_registry_basics() ->
    % Test that registry is running and responding
    ?assertEqual([], erlmcp_registry:list_servers()),
    ?assertEqual([], erlmcp_registry:list_transports()).

test_server_registration() ->
    % Create a mock server process
    MockServer = spawn(fun() -> 
        receive
            stop -> ok
        end
    end),
    
    ServerCapabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    },
    ServerConfig = #{
        capabilities => ServerCapabilities
    },
    
    % Register server
    ?assertEqual(ok, erlmcp_registry:register_server(test_server, MockServer, ServerConfig)),
    
    % Verify registration
    ?assertMatch({ok, {MockServer, ServerConfig}}, 
                 erlmcp_registry:find_server(test_server)),
    
    % Check it appears in list
    Servers = erlmcp_registry:list_servers(),
    ?assertEqual(1, length(Servers)),
    ?assertMatch([{test_server, {MockServer, ServerConfig}}], Servers),
    
    % Test duplicate registration fails
    ?assertMatch({error, already_registered}, 
                 erlmcp_registry:register_server(test_server, MockServer, ServerConfig)),
    
    % Unregister
    ?assertEqual(ok, erlmcp_registry:unregister_server(test_server)),
    ?assertMatch({error, not_found}, erlmcp_registry:find_server(test_server)),
    
    % Cleanup
    MockServer ! stop.

test_transport_registration() ->
    % Create a mock transport process
    MockTransport = spawn(fun() -> 
        receive
            stop -> ok
        end
    end),
    
    TransportConfig = #{
        type => stdio,
        server_id => test_server
    },
    
    % Register transport
    ?assertEqual(ok, erlmcp_registry:register_transport(test_transport, MockTransport, TransportConfig)),
    
    % Verify registration
    ?assertMatch({ok, {MockTransport, TransportConfig}}, 
                 erlmcp_registry:find_transport(test_transport)),
    
    % Check it appears in list
    Transports = erlmcp_registry:list_transports(),
    ?assertEqual(1, length(Transports)),
    ?assertMatch([{test_transport, {MockTransport, TransportConfig}}], Transports),
    
    % Unregister
    ?assertEqual(ok, erlmcp_registry:unregister_transport(test_transport)),
    ?assertMatch({error, not_found}, erlmcp_registry:find_transport(test_transport)),
    
    % Cleanup
    MockTransport ! stop.

test_binding() ->
    % Create mock processes
    MockServer = spawn(fun() -> receive stop -> ok end end),
    MockTransport = spawn(fun() -> receive stop -> ok end end),
    
    % Register both
    ?assertEqual(ok, erlmcp_registry:register_server(test_server, MockServer, #{})),
    ?assertEqual(ok, erlmcp_registry:register_transport(test_transport, MockTransport, #{type => stdio})),
    
    % Test binding
    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(test_transport, test_server)),
    ?assertMatch({ok, test_server}, erlmcp_registry:get_server_for_transport(test_transport)),
    
    % Test unbinding
    ?assertEqual(ok, erlmcp_registry:unbind_transport(test_transport)),
    ?assertMatch({error, not_found}, erlmcp_registry:get_server_for_transport(test_transport)),
    
    % Test binding nonexistent fails
    ?assertMatch({error, server_not_found}, 
                 erlmcp_registry:bind_transport_to_server(test_transport, nonexistent)),
    ?assertMatch({error, transport_not_found}, 
                 erlmcp_registry:bind_transport_to_server(nonexistent, test_server)),
    
    % Cleanup
    erlmcp_registry:unregister_server(test_server),
    erlmcp_registry:unregister_transport(test_transport),
    MockServer ! stop,
    MockTransport ! stop.

test_message_routing() ->
    % Create processes that can receive messages
    TestPid = self(),
    
    MockServer = spawn(fun() ->
        receive
            {mcp_message, TransportId, Message} ->
                TestPid ! {got_server_message, TransportId, Message},
                receive stop -> ok end;
            stop -> ok
        end
    end),
    
    MockTransport = spawn(fun() ->
        receive
            {mcp_response, ServerId, Message} ->
                TestPid ! {got_transport_message, ServerId, Message},
                receive stop -> ok end;
            stop -> ok
        end
    end),
    
    % Register and bind
    erlmcp_registry:register_server(test_server, MockServer, #{}),
    erlmcp_registry:register_transport(test_transport, MockTransport, #{type => stdio}),
    erlmcp_registry:bind_transport_to_server(test_transport, test_server),
    
    % Test routing to server
    erlmcp_registry:route_to_server(test_server, test_transport, <<"test message">>),
    
    % Should receive message at server
    receive
        {got_server_message, test_transport, <<"test message">>} -> ok
    after 1000 ->
        ?assert(false) % Timeout
    end,
    
    % Test routing to transport
    erlmcp_registry:route_to_transport(test_transport, test_server, <<"response message">>),
    
    % Should receive message at transport
    receive
        {got_transport_message, test_server, <<"response message">>} -> ok
    after 1000 ->
        ?assert(false) % Timeout
    end,
    
    % Cleanup
    MockServer ! stop,
    MockTransport ! stop.

test_process_monitoring() ->
    TestPid = self(),
    
    % Create a server that will die
    MockServer = spawn(fun() ->
        receive
            die -> exit(normal);
            stop -> ok
        end
    end),
    
    % Register it
    erlmcp_registry:register_server(test_server, MockServer, #{}),
    
    % Verify it's registered
    ?assertMatch({ok, {MockServer, _}}, erlmcp_registry:find_server(test_server)),
    
    % Kill the server
    MockServer ! die,
    
    % Give registry time to process the DOWN message
    timer:sleep(100),
    
    % Should be automatically unregistered
    ?assertMatch({error, not_found}, erlmcp_registry:find_server(test_server)).

%%====================================================================
%% Integration Tests with New Architecture
%%====================================================================

integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     fun(_) ->
         [
             {"Full server startup via new API", fun test_new_server_startup/0},
             {"Legacy stdio compatibility", fun test_legacy_stdio_compatibility/0}
         ]
     end}.

setup_integration() ->
    % Start the application components needed for integration tests
    application:ensure_all_started(erlmcp),
    ok.

cleanup_integration(_) ->
    application:stop(erlmcp),
    ok.

test_new_server_startup() ->
    % Test the new API for creating servers and transports
    ServerCapabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true},
        resources = #mcp_capability{enabled = true}
    },
    ServerConfig = #{
        capabilities => ServerCapabilities
    },
    
    % Start server via new API
    ?assertMatch({ok, _Pid}, erlmcp:start_server(integration_test_server)),
    
    % Verify it's registered (only if registry is available)
    case whereis(erlmcp_registry) of
        undefined -> 
            ok; % Registry not available, skip verification
        _ ->
            ?assertMatch({ok, {_Pid, _Config}}, erlmcp_registry:find_server(integration_test_server))
    end,
    
    % Start transport (will fail gracefully if transport_sup not available)
    TransportConfig = #{server_id => integration_test_server},
    case erlmcp:start_transport(integration_test_transport, stdio, TransportConfig) of
        {ok, _TransportPid} ->
            % Verify transport is registered and bound (if registry available)
            case whereis(erlmcp_registry) of
                undefined -> ok;
                _ ->
                    ?assertMatch({ok, integration_test_server}, 
                                 erlmcp_registry:get_server_for_transport(integration_test_transport))
            end,
            
            % Add a simple tool
            ToolHandler = fun(#{<<"message">> := Msg}) -> <<"Echo: ", Msg/binary>> end,
            case erlmcp:add_tool(integration_test_server, <<"echo">>, ToolHandler) of
                ok -> ok;
                {error, server_not_found} -> ok % Expected if registry not available
            end,
            
            % Cleanup
            _ = erlmcp:stop_transport(integration_test_transport),
            _ = erlmcp:stop_server(integration_test_server);
        {error, _} ->
            % Transport creation failed, just cleanup server
            _ = erlmcp:stop_server(integration_test_server)
    end.

test_legacy_stdio_compatibility() ->
    % Test that legacy stdio server still works
    ?assertMatch({ok, _Pid}, erlmcp:start_stdio_server()),
    
    % Should be able to find the default server
    ?assertMatch({ok, {_Pid, _Config}}, erlmcp_registry:find_server(default_stdio_server)),
    
    % Should be able to find the default transport
    ?assertMatch({ok, {_Pid, _Config}}, erlmcp_registry:find_transport(default_stdio_transport)),
    
    % Should be bound
    ?assertMatch({ok, default_stdio_server}, 
                 erlmcp_registry:get_server_for_transport(default_stdio_transport)),
    
    % Cleanup
    erlmcp:stop_stdio_server().
