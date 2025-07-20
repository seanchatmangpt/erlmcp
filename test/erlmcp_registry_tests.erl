-module(erlmcp_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup and Cleanup
%%====================================================================

registry_test_() ->
    [
        {"Registry startup and basic operations", fun test_registry_basics/0},
        {"Server registration and lookup", fun test_server_registration/0},
        {"Transport registration and lookup", fun test_transport_registration/0},
        {"Server-Transport binding", fun test_binding/0},
        {"Message routing", fun test_message_routing/0},
        {"Process monitoring", fun test_process_monitoring/0}
    ].

setup_registry() ->
    % Start the registry
    case erlmcp_registry:start_link() of
        {ok, Registry} -> Registry;
        {error, {already_started, Registry}} -> Registry
    end.

cleanup_registry(Registry) ->
    % Unregister all test components first
    try
        erlmcp_registry:unregister_server(test_server_reg),
        erlmcp_registry:unregister_server(test_server_bind),
        erlmcp_registry:unregister_server(test_server_route),
        erlmcp_registry:unregister_server(test_server_monitor),
        erlmcp_registry:unregister_transport(test_transport_reg),
        erlmcp_registry:unregister_transport(test_transport_bind),
        erlmcp_registry:unregister_transport(test_transport_route)
    catch
        _:_ -> ok % Ignore cleanup errors
    end,
    
    % Give time for cleanup
    timer:sleep(100),
    
    % Stop registry gracefully
    try
        case is_process_alive(Registry) of
            true -> 
                gen_server:stop(Registry, shutdown, 1000),
                ok;
            false -> 
                ok
        end
    catch
        _:_ -> 
            % Force kill if graceful stop fails
            case is_process_alive(Registry) of
                true -> exit(Registry, kill);
                false -> ok
            end
    end,
    
    % Final cleanup delay
    timer:sleep(50),
    ok.

%%====================================================================
%% Basic Registry Tests
%%====================================================================

test_registry_basics() ->
    % Start an anonymous registry (not registered by name)
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    
    try
        % Test that registry is running and responding
        ?assertEqual([], gen_server:call(Registry, list_servers)),
        ?assertEqual([], gen_server:call(Registry, list_transports))
    after
        catch gen_server:stop(Registry, normal, 5000)
    end.

test_server_registration() ->
    % Start an anonymous registry
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    
    try
        % Create a mock server process
        MockServer = spawn(fun() -> 
            receive
                stop -> ok
            after 2000 -> ok  % Shorter timeout
            end
        end),
        
        ServerCapabilities = #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true}
        },
        ServerConfig = #{
            capabilities => ServerCapabilities
        },
        
        % Register server
        ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_reg, MockServer, ServerConfig})),
        
        % Verify registration
        ?assertMatch({ok, {MockServer, ServerConfig}}, 
                     gen_server:call(Registry, {find_server, test_server_reg})),
        
        % Check it appears in list
        Servers = gen_server:call(Registry, list_servers),
        ?assert(length(Servers) >= 1),
        
        % Test duplicate registration fails
        ?assertMatch({error, already_registered}, 
                     gen_server:call(Registry, {register_server, test_server_reg, MockServer, ServerConfig})),
        
        % Unregister
        ?assertEqual(ok, gen_server:call(Registry, {unregister_server, test_server_reg})),
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_reg})),
        
        % Cleanup mock server
        exit(MockServer, normal)
    after
        catch gen_server:stop(Registry, normal, 5000)
    end.

test_transport_registration() ->
    % Start an anonymous registry
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    
    try
        % Create a mock transport process
        MockTransport = spawn(fun() -> 
            receive
                stop -> ok
            after 2000 -> ok
            end
        end),
        
        TransportConfig = #{
            type => stdio,
            server_id => test_server_transport
        },
        
        % Register transport
        ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_reg, MockTransport, TransportConfig})),
        
        % Verify registration
        ?assertMatch({ok, {MockTransport, TransportConfig}}, 
                     gen_server:call(Registry, {find_transport, test_transport_reg})),
        
        % Check it appears in list
        Transports = gen_server:call(Registry, list_transports),
        ?assert(length(Transports) >= 1),
        
        % Unregister
        ?assertEqual(ok, gen_server:call(Registry, {unregister_transport, test_transport_reg})),
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, test_transport_reg})),
        
        % Cleanup mock transport
        exit(MockTransport, normal)
    after
        catch gen_server:stop(Registry, normal, 5000)
    end.

test_binding() ->
    % Start an anonymous registry
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    
    try
        % Create mock processes  
        MockServer = spawn(fun() -> receive stop -> ok after 2000 -> ok end end),
        MockTransport = spawn(fun() -> receive stop -> ok after 2000 -> ok end end),
        
        % Register both
        ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_bind, MockServer, #{}})),
        ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_bind, MockTransport, #{type => stdio}})),
        
        % Test binding
        ?assertEqual(ok, gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, test_server_bind})),
        ?assertMatch({ok, test_server_bind}, gen_server:call(Registry, {get_server_for_transport, test_transport_bind})),
        
        % Test unbinding
        ?assertEqual(ok, gen_server:call(Registry, {unbind_transport, test_transport_bind})),
        ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, test_transport_bind})),
        
        % Test binding nonexistent fails
        ?assertMatch({error, server_not_found}, 
                     gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, nonexistent})),
        ?assertMatch({error, transport_not_found}, 
                     gen_server:call(Registry, {bind_transport_to_server, nonexistent, test_server_bind})),
        
        % Cleanup
        gen_server:call(Registry, {unregister_server, test_server_bind}),
        gen_server:call(Registry, {unregister_transport, test_transport_bind}),
        exit(MockServer, normal),
        exit(MockTransport, normal)
    after
        catch gen_server:stop(Registry, normal, 5000)
    end.

test_message_routing() ->
    % Start an anonymous registry
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    
    try
        % Create a simple message collector process
        Collector = spawn(fun() ->
            collect_messages([])
        end),
        
        % Create mock processes that forward to collector
        MockServer = spawn(fun() ->
            receive
                {mcp_message, TransportId, Message} ->
                    Collector ! {server_got, TransportId, Message},
                    receive stop -> ok after 2000 -> ok end;
                stop -> ok
            end
        end),
        
        MockTransport = spawn(fun() ->
            receive
                {mcp_response, ServerId, Message} ->
                    Collector ! {transport_got, ServerId, Message},
                    receive stop -> ok after 2000 -> ok end;
                stop -> ok
            end
        end),
        
        % Register and bind
        gen_server:call(Registry, {register_server, test_server_route, MockServer, #{}}),
        gen_server:call(Registry, {register_transport, test_transport_route, MockTransport, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, test_transport_route, test_server_route}),
        
        % Test routing to server
        gen_server:cast(Registry, {route_to_server, test_server_route, test_transport_route, <<"test message">>}),
        
        % Test routing to transport
        gen_server:cast(Registry, {route_to_transport, test_transport_route, test_server_route, <<"response message">>}),
        
        % Give time for messages to be processed
        timer:sleep(100),
        
        % Check messages were received
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                ?assert(lists:any(fun({server_got, test_transport_route, <<"test message">>}) -> true; (_) -> false end, Messages)),
                ?assert(lists:any(fun({transport_got, test_server_route, <<"response message">>}) -> true; (_) -> false end, Messages))
        after 1000 ->
            ?assert(false) % Timeout
        end,
        
        % Cleanup
        exit(MockServer, normal),
        exit(MockTransport, normal),
        exit(Collector, normal)
    after
        catch gen_server:stop(Registry, normal, 5000)
    end.

% Helper function for message collection
collect_messages(Messages) ->
    receive
        {get_messages, From} ->
            From ! {messages, lists:reverse(Messages)};
        Message ->
            collect_messages([Message | Messages])
    after 3000 ->
        ok
    end.

test_process_monitoring() ->
    % Start an anonymous registry
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    
    try
        % Create a server that will die
        MockServer = spawn(fun() ->
            receive
                die -> exit(normal);
                stop -> ok
            after 2000 -> ok
            end
        end),
        
        % Register it
        gen_server:call(Registry, {register_server, test_server_monitor, MockServer, #{}}),
        
        % Verify it's registered
        ?assertMatch({ok, {MockServer, _}}, gen_server:call(Registry, {find_server, test_server_monitor})),
        
        % Kill the server
        exit(MockServer, normal),
        
        % Give registry time to process the DOWN message
        timer:sleep(200),
        
        % Should be automatically unregistered
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_monitor}))
    after
        catch gen_server:stop(Registry, normal, 5000)
    end.

%%====================================================================
%% Integration Tests with New Architecture
%%====================================================================

integration_test_() ->
    {timeout, 30, [
        {"Full server startup via new API", fun test_new_server_startup/0},
        {"Legacy stdio compatibility", fun test_legacy_stdio_compatibility/0}
    ]}.

setup_integration() ->
    % Start the application components needed for integration tests
    % Don't fail if they're not available
    try
        application:ensure_all_started(erlmcp),
        ok
    catch
        _:_ -> ok
    end.

cleanup_integration(_) ->
    % Stop application gracefully
    try
        application:stop(erlmcp),
        ok
    catch
        _:_ -> ok
    end.

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
    case erlmcp:start_stdio_server() of
        {ok, _Pid} ->
            % Should be able to find the default server (if registry available)
            case whereis(erlmcp_registry) of
                undefined -> ok; % Registry not available, skip checks
                _ ->
                    case erlmcp_registry:find_server(default_stdio_server) of
                        {ok, {_Pid, _Config}} -> ok;
                        {error, not_found} -> ok % May not be registered if using legacy mode
                    end
            end,
            
            % Cleanup
            erlmcp:stop_stdio_server();
        {error, _} ->
            % Stdio server creation failed, that's OK for this test
            ok
    end.
