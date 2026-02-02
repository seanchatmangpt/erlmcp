%% @doc Comprehensive coverage tests for erlmcp_registry
%% Tests all public API functions to achieve 80%+ coverage
%% Chicago School TDD: Uses real gen_server, no mocks, state-based verification
-module(erlmcp_registry_coverage_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

registry_test_() ->
    {setup,
     fun setup_registry/0,
     fun cleanup_registry/1,
     fun run_tests/1}.

setup_registry() ->
    % Ensure gproc is started
    application:ensure_all_started(gproc),
    {ok, Pid} = erlmcp_registry:start_link(),
    Pid.

cleanup_registry(_Pid) ->
    % Clean up gproc entries
    catch erlmcp_registry:stop(),
    % Clear any gproc registrations
    gproc:cleanup(),
    ok.

run_tests(_Pid) ->
    [
     {"Server registration lifecycle", fun test_server_lifecycle/0},
     {"Transport registration lifecycle", fun test_transport_lifecycle/0},
     {"Server update functionality", fun test_server_update/0},
     {"Find operations", fun test_find_operations/0},
     {"List operations", fun test_list_operations/0},
     {"Binding operations", fun test_binding_operations/0},
     {"Routing operations", fun test_routing_operations/0},
     {"State operations", fun test_state_operations/0},
     {"Queue depth monitoring", fun test_queue_depth/0},
     {"Route message operations", fun test_route_message/0},
     {"Unregister operations", fun test_unregister_operations/0},
     {"Gproc monitoring", fun test_gproc_monitoring/0}
    ].

%%%====================================================================
%%% Server Registration Tests
%%%====================================================================

test_server_lifecycle() ->
    ServerId = test_server,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    % Register server
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),

    % Verify registration
    {ok, {Pid, Config}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(ServerPid, Pid),

    % List servers
    Servers = erlmcp_registry:list_servers(),
    ?assertMatch([{_, {_, _}}], Servers),

    % Unregister server
    ok = erlmcp_registry:unregister_server(ServerId),

    % Verify unregistration
    {error, not_found} = erlmcp_registry:find_server(ServerId),

    % Cleanup
    ServerPid ! stop.

%%%====================================================================
%%% Transport Registration Tests
%%%====================================================================

test_transport_lifecycle() ->
    TransportId = test_transport,
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{type => stdio, config => #{}},

    % Register transport
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, Config),

    % Verify registration
    {ok, {Pid, Config}} = erlmcp_registry:find_transport(TransportId),
    ?assertEqual(TransportPid, Pid),

    % List transports
    Transports = erlmcp_registry:list_transports(),
    ?assertMatch([{_, {_, _}}], Transports),

    % Unregister transport
    ok = erlmcp_registry:unregister_transport(TransportId),

    % Verify unregistration
    {error, not_found} = erlmcp_registry:find_transport(TransportId),

    % Cleanup
    TransportPid ! stop.

%%%====================================================================
%%% Server Update Tests
%%%====================================================================

test_server_update() ->
    ServerId = update_server,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    Config1 = #{capabilities => #mcp_server_capabilities{}},
    Config2 = #{capabilities => #mcp_server_capabilities{tools = #mcp_tools_capability{}}}},

    % Register server
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config1),

    % Update server config
    ok = erlmcp_registry:update_server(ServerId, Config2),

    % Verify update
    {ok, {_Pid, UpdatedConfig}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(Config2, UpdatedConfig),

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId),
    ServerPid ! stop.

%%%====================================================================
%%% Find Operations Tests
%%%====================================================================

test_find_operations() ->
    ServerId = find_server,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    % Register and find server
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),
    {ok, {Pid, _Config}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(ServerPid, Pid),

    % Find non-existent server
    {error, not_found} = erlmcp_registry:find_server(non_existent),

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId),
    ServerPid ! stop.

%%%====================================================================
%%% List Operations Tests
%%%====================================================================

test_list_operations() ->
    ServerId1 = list_server1,
    ServerId2 = list_server2,
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    % Register multiple servers
    ok = erlmcp_registry:register_server(ServerId1, Pid1, Config),
    ok = erlmcp_registry:register_server(ServerId2, Pid2, Config),

    % List all servers
    Servers = erlmcp_registry:list_servers(),
    ?assert(length(Servers) >= 2),

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId1),
    ok = erlmcp_registry:unregister_server(ServerId2),
    Pid1 ! stop,
    Pid2 ! stop.

%%%====================================================================
%%% Binding Operations Tests
%%%====================================================================

test_binding_operations() ->
    ServerId = bind_server,
    TransportId = bind_transport,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => ServerId, config => #{}},

    % Register server and transport
    ok = erlmcp_registry:register_server(ServerId, ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig),

    % Bind transport to server (auto-binding from config)
    {ok, FoundServerId} = erlmcp_registry:get_server_for_transport(TransportId),
    ?assertEqual(ServerId, FoundServerId),

    % Manual bind
    TransportId2 = bind_transport2,
    TransportPid2 = spawn(fun() -> receive stop -> ok end end),
    TransportConfig2 = #{type => stdio, config => #{}},
    ok = erlmcp_registry:register_transport(TransportId2, TransportPid2, TransportConfig2),
    ok = erlmcp_registry:bind_transport_to_server(TransportId2, ServerId),

    % Verify binding
    {ok, ServerId} = erlmcp_registry:get_server_for_transport(TransportId2),

    % Unbind transport
    ok = erlmcp_registry:unbind_transport(TransportId2),
    {error, not_found} = erlmcp_registry:get_server_for_transport(TransportId2),

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId),
    ok = erlmcp_registry:unregister_transport(TransportId),
    ok = erlmcp_registry:unregister_transport(TransportId2),
    ServerPid ! stop,
    TransportPid ! stop,
    TransportPid2 ! stop.

%%%====================================================================
%%% Routing Operations Tests
%%%====================================================================

test_routing_operations() ->
    ServerId = route_server,
    TransportId = route_transport,
    ServerPid = self(),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},

    % Register server
    ok = erlmcp_registry:register_server(ServerId, ServerPid, ServerConfig),

    % Route to server (cast operation - no return)
    ok = erlmcp_registry:route_to_server(ServerId, TransportId, test_message),

    % Verify message received
    receive
        {mcp_message, ^TransportId, test_message} ->
            ok
    after 100 ->
        error("Message not routed")
    end,

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId).

%%%====================================================================
%%% State Operations Tests
%%%====================================================================

test_state_operations() ->
    % Get registry PID
    Pid = erlmcp_registry:get_pid(),
    ?assert(is_pid(Pid)),

    % Get queue depth
    Depth = erlmcp_registry:get_queue_depth(),
    ?assert(is_integer(Depth)),

    % Get all state
    {ok, State} = erlmcp_registry:get_all_state(),
    ?assert(is_record(State, registry_state)),

    % Restore state
    NewState = #registry_state{server_transport_map = #{}},
    ok = erlmcp_registry:restore_state(NewState),

    % Verify state restored
    {ok, RestoredState} = erlmcp_registry:get_all_state(),
    ?assertEqual(0, maps:size(RestoredState#registry_state.server_transport_map)).

%%%====================================================================
%%% Queue Depth Tests
%%%====================================================================

test_queue_depth() ->
    % Initial depth
    Depth1 = erlmcp_registry:get_queue_depth(),

    % Send some messages to increase queue
    ServerId = queue_server,
    ServerPid = self(),
    Config = #{capabilities => #mcp_server_capabilities{}},

    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),

    % Send multiple messages
    ok = erlmcp_registry:route_to_server(ServerId, test_transport1, msg1),
    ok = erlmcp_registry:route_to_server(ServerId, test_transport2, msg2),

    % Clear messages
    receive {mcp_message, _, _} -> ok end,
    receive {mcp_message, _, _} -> ok end,

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId),

    % Check queue depth again
    Depth2 = erlmcp_registry:get_queue_depth(),
    ?assert(is_integer(Depth2)).

%%%====================================================================
%%% Route Message Tests
%%%====================================================================

test_route_message() ->
    ServerId = route_msg_server,
    TransportId = route_msg_transport,
    ServerPid = self(),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => ServerId, config => #{}},

    % Register server and transport
    ok = erlmcp_registry:register_server(ServerId, ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport(TransportId, ServerPid, TransportConfig),

    % Route message to server (broadcast to all transports)
    ok = erlmcp_registry:route_message({server, ServerId}, test_message),

    % Receive broadcast message
    receive
        {mcp_response, ^ServerId, test_message} ->
            ok
    after 100 ->
        error("Broadcast message not received")
    end,

    % Route message to specific transport
    ok = erlmcp_registry:route_message({transport, TransportId}, test_message2),

    % Receive directed message
    receive
        {mcp_response, ^ServerId, test_message2} ->
            ok
    after 100 ->
        error("Directed message not received")
    end,

    % Test route to non-existent server
    {error, server_not_found} = erlmcp_registry:route_message({server, no_server}, msg),

    % Cleanup
    ok = erlmcp_registry:unregister_server(ServerId),
    ok = erlmcp_registry:unregister_transport(TransportId).

%%%====================================================================
%%% Unregister Operations Tests
%%%====================================================================

test_unregister_operations() ->
    ServerId = unregister_server,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    % Register server
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),

    % Unregister (idempotent - ok even if called twice)
    ok = erlmcp_registry:unregister_server(ServerId),
    ok = erlmcp_registry:unregister_server(ServerId),

    % Verify not found
    {error, not_found} = erlmcp_registry:find_server(ServerId),

    % Cleanup
    ServerPid ! stop.

%%%====================================================================
%%% Gproc Monitoring Tests
%%%====================================================================

test_gproc_monitoring() ->
    ServerId = monitor_server,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    % Register server (gproc monitors the process)
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),

    % Verify registration
    {ok, {_, _}} = erlmcp_registry:find_server(ServerId),

    % Kill the server process
    exit(ServerPid, kill),

    % Wait for gproc to detect and clean up
    timer:sleep(100),

    % Server should be auto-unregistered
    {error, not_found} = erlmcp_registry:find_server(ServerId).
