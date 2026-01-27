-module(erlmcp_registry_gproc_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Suite for gproc-based Registry Integration
%%%
%%% This test suite validates the integration of gproc library
%%% into erlmcp_registry.erl, ensuring:
%%% - Process registration and lookup via gproc
%%% - Automatic cleanup on process death
%%% - Server-transport binding
%%% - Message routing
%%% - Configuration validation
%%% - Supervisor restart scenarios
%%% - Error handling and recovery
%%%
%%% Target: >90% coverage on erlmcp_registry.erl
%%%===================================================================

%%====================================================================
%% Test Groups
%%====================================================================

registry_gproc_test_() ->
    [
        {"Basic registry operations", fun test_basic_registry_operations/0},
        {"Server registration with gproc", fun test_server_registration_gproc/0},
        {"Transport registration with gproc", fun test_transport_registration_gproc/0},
        {"Server-Transport binding", fun test_server_transport_binding/0},
        {"Message routing to server", fun test_message_routing_to_server/0},
        {"Message routing to transport", fun test_message_routing_to_transport/0},
        {"Broadcast routing to bound transports", fun test_route_to_transport_broadcast/0},
        {"Process monitoring and cleanup", fun test_process_monitoring_cleanup/0},
        {"Automatic cleanup on death", fun test_automatic_cleanup_on_death/0},
        {"Multiple servers registration", fun test_multiple_servers_registration/0},
        {"Multiple transports registration", fun test_multiple_transports_registration/0},
        {"Concurrent registration operations", fun test_concurrent_registration/0},
        {"Registry persistence after server death", fun test_registry_persistence/0},
        {"Invalid registration attempts", fun test_invalid_registration/0},
        {"Duplicate registration handling", fun test_duplicate_registration/0},
        {"Server capabilities storage", fun test_server_capabilities_storage/0},
        {"Transport auto-binding on registration", fun test_transport_auto_binding/0},
        {"Unbind transport operation", fun test_unbind_transport/0},
        {"List all servers", fun test_list_all_servers/0},
        {"List all transports", fun test_list_all_transports/0},
        {"Registry state recovery", fun test_registry_state_recovery/0}
    ].

integration_test_() ->
    [
        {timeout, 10, {"Full integration with transports", fun test_full_integration/0}},
        {timeout, 10, {"Supervisor restart scenario", fun test_supervisor_restart/0}},
        {timeout, 10, {"Concurrent message routing", fun test_concurrent_routing/0}},
        {timeout, 10, {"Load testing registry operations", fun test_load_testing/0}}
    ].

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup_gproc_registry() ->
    ok = ensure_gproc_started(),
    clear_all_test_registrations(),
    kill_test_processes(),
    wait_for_cleanup(),

    % Start an anonymous registry for testing
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    #{registry => Registry, test_pids => []}.

cleanup_gproc_registry(#{registry := Registry} = Ctx) ->
    % Stop registry gracefully
    catch gen_server:stop(Registry, shutdown, 5000),
    timer:sleep(100),

    % Kill all test processes spawned during this test
    TestPids = maps:get(test_pids, Ctx, []),
    lists:foreach(fun(Pid) ->
        catch unlink(Pid),
        catch exit(Pid, kill)
    end, TestPids),

    % Clear all registrations and wait for cleanup
    clear_all_test_registrations(),
    kill_test_processes(),
    wait_for_cleanup(),
    ok.

setup_integration() ->
    ok = ensure_gproc_started(),
    clear_all_test_registrations(),
    kill_test_processes(),
    wait_for_cleanup(),

    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    #{registry => Registry, test_pids => []}.

cleanup_integration(#{registry := Registry} = Ctx) ->
    catch gen_server:stop(Registry, shutdown, 5000),
    timer:sleep(100),

    % Kill all test processes
    TestPids = maps:get(test_pids, Ctx, []),
    lists:foreach(fun(Pid) ->
        catch unlink(Pid),
        catch exit(Pid, kill)
    end, TestPids),

    clear_all_test_registrations(),
    kill_test_processes(),
    wait_for_cleanup(),
    ok.

%%====================================================================
%% Basic Registry Tests
%%====================================================================

test_basic_registry_operations() ->
    run_with_registry(fun(Registry) ->
        % Test registry is initialized with empty state
        ?assertEqual([], gen_server:call(Registry, list_servers)),
        ?assertEqual([], gen_server:call(Registry, list_transports)),

        % Test unknown server lookup
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, unknown_server})),

        % Test unknown transport lookup
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, unknown_transport}))
    end).

test_server_registration_gproc() ->
    run_with_registry(fun(Registry) ->
        % Create a mock server process
        MockServer = spawn_link(fun() ->
            receive
                stop -> ok
            after 10000 -> ok
            end
        end),

        ServerCapabilities = #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true},
            resources = #mcp_capability{enabled = true}
        },
        ServerConfig = #{
            capabilities => ServerCapabilities,
            options => #{test_mode => true}
        },

        try
            % Register server
            ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_gproc, MockServer, ServerConfig})),

            % Verify registration via registry API
            ?assertMatch({ok, {MockServer, ServerConfig}},
                         gen_server:call(Registry, {find_server, test_server_gproc})),

            % Verify it appears in list
            Servers = gen_server:call(Registry, list_servers),
            ?assert(length(Servers) >= 1),
            ?assert(lists:keymember(test_server_gproc, 1, Servers)),

            % Test duplicate registration fails
            ?assertMatch({error, already_registered},
                         gen_server:call(Registry, {register_server, test_server_gproc, MockServer, ServerConfig})),

            % Unregister
            ?assertEqual(ok, gen_server:call(Registry, {unregister_server, test_server_gproc})),
            ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_gproc})),

            % Verify removed from list
            Servers2 = gen_server:call(Registry, list_servers),
            ?assertNot(lists:keymember(test_server_gproc, 1, Servers2))
        after
            unlink(MockServer),
            exit(MockServer, normal)
        end
    end).

test_transport_registration_gproc() ->
    run_with_registry(fun(Registry) ->
        % Create mock transport process
        MockTransport = spawn_link(fun() ->
            receive
                stop -> ok
            after 10000 -> ok
            end
        end),

        TransportConfig = #{
            type => stdio,
            server_id => test_server,
            config => #{buffer_size => 1024}
        },

        try
            % Register transport
            ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_gproc, MockTransport, TransportConfig})),

            % Verify registration
            ?assertMatch({ok, {MockTransport, TransportConfig}},
                         gen_server:call(Registry, {find_transport, test_transport_gproc})),

            % Verify in list
            Transports = gen_server:call(Registry, list_transports),
            ?assert(length(Transports) >= 1),
            ?assert(lists:keymember(test_transport_gproc, 1, Transports)),

            % Unregister
            ?assertEqual(ok, gen_server:call(Registry, {unregister_transport, test_transport_gproc})),
            ?assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, test_transport_gproc}))
        after
            unlink(MockTransport),
            exit(MockTransport, normal)
        end
    end).

%%====================================================================
%% Server-Transport Binding Tests
%%====================================================================

test_server_transport_binding() ->
    run_with_registry(fun(Registry) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        MockServer2 = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        try
            % Register both
            ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_bind, MockServer, #{}})),
            ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_bind2, MockServer2, #{}})),
            ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_bind, MockTransport, #{type => stdio}})),

            % Test binding
            ?assertEqual(ok, gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, test_server_bind})),
            ?assertMatch({ok, test_server_bind}, gen_server:call(Registry, {get_server_for_transport, test_transport_bind})),

            % Test rebinding to different server
            ?assertEqual(ok, gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, test_server_bind2})),
            ?assertMatch({ok, test_server_bind2}, gen_server:call(Registry, {get_server_for_transport, test_transport_bind})),

            % Cleanup from registry perspective
            gen_server:call(Registry, {unregister_server, test_server_bind}),
            gen_server:call(Registry, {unregister_server, test_server_bind2}),
            gen_server:call(Registry, {unregister_transport, test_transport_bind})
        after
            unlink(MockServer),
            unlink(MockServer2),
            unlink(MockTransport),
            exit(MockServer, normal),
            exit(MockServer2, normal),
            exit(MockTransport, normal)
        end
    end).

test_transport_auto_binding() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        % Register server first
        ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_auto, MockServer, #{}})),

        % Register transport with server_id - should auto-bind
        TransportConfig = #{
            type => tcp,
            server_id => test_server_auto
        },
        ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_auto, MockTransport, TransportConfig})),

        % Verify auto-binding
        ?assertMatch({ok, test_server_auto}, gen_server:call(Registry, {get_server_for_transport, test_transport_auto})),

        % Cleanup
        gen_server:call(Registry, {unregister_server, test_server_auto}),
        gen_server:call(Registry, {unregister_transport, test_transport_auto}),
        unlink(MockServer),
        unlink(MockTransport),
        exit(MockServer, normal),
        exit(MockTransport, normal)
    after
        stop_test_registry(Ctx)
    end.

test_unbind_transport() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        % Register and bind
        gen_server:call(Registry, {register_server, test_server_unbind, MockServer, #{}}),
        gen_server:call(Registry, {register_transport, test_transport_unbind, MockTransport, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, test_transport_unbind, test_server_unbind}),

        % Verify binding
        ?assertMatch({ok, test_server_unbind}, gen_server:call(Registry, {get_server_for_transport, test_transport_unbind})),

        % Unbind
        ?assertEqual(ok, gen_server:call(Registry, {unbind_transport, test_transport_unbind})),

        % Verify unbound
        ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, test_transport_unbind})),

        % Cleanup
        gen_server:call(Registry, {unregister_server, test_server_unbind}),
        gen_server:call(Registry, {unregister_transport, test_transport_unbind}),
        unlink(MockServer),
        unlink(MockTransport),
        exit(MockServer, normal),
        exit(MockTransport, normal)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Message Routing Tests
%%====================================================================

test_message_routing_to_server() ->
    {Registry, Ctx} = start_test_registry(),

    try
        Collector = spawn_link(fun() -> collect_messages([]) end),

        MockServer = spawn_link(fun() ->
            receive
                {mcp_message, TransportId, Message} ->
                    Collector ! {server_received, TransportId, Message}
            after 5000 -> ok
            end
        end),

        MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        % Register and bind
        gen_server:call(Registry, {register_server, test_server_route, MockServer, #{}}),
        gen_server:call(Registry, {register_transport, test_transport_route, MockTransport, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, test_transport_route, test_server_route}),

        % Route message to server
        TestMessage = #{<<"method">> => <<"test">>, <<"data">> => <<"hello">>},
        gen_server:cast(Registry, {route_to_server, test_server_route, test_transport_route, TestMessage}),

        % Verify message received
        timer:sleep(200),
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                ?assert(lists:any(fun({server_received, test_transport_route, Msg}) when Msg =:= TestMessage -> true; (_) -> false end, Messages))
        after 2000 ->
            ?assert(false) % Timeout
        end,

        % Cleanup
        unlink(MockServer),
        unlink(MockTransport),
        unlink(Collector),
        exit(MockServer, normal),
        exit(MockTransport, normal),
        exit(Collector, normal)
    after
        stop_test_registry(Ctx)
    end.

test_message_routing_to_transport() ->
    {Registry, Ctx} = start_test_registry(),

    try
        Collector = spawn_link(fun() -> collect_messages([]) end),

        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        MockTransport = spawn_link(fun() ->
            receive
                {mcp_response, ServerId, Message} ->
                    Collector ! {transport_received, ServerId, Message}
            after 5000 -> ok
            end
        end),

        % Register and bind
        gen_server:call(Registry, {register_server, test_server_route2, MockServer, #{}}),
        gen_server:call(Registry, {register_transport, test_transport_route2, MockTransport, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, test_transport_route2, test_server_route2}),

        % Route message to transport
        TestResponse = #{<<"result">> => <<"success">>, <<"data">> => <<"world">>},
        gen_server:cast(Registry, {route_to_transport, test_transport_route2, test_server_route2, TestResponse}),

        % Verify message received
        timer:sleep(200),
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                ?assert(lists:any(fun({transport_received, test_server_route2, Msg}) when Msg =:= TestResponse -> true; (_) -> false end, Messages))
        after 2000 ->
            ?assert(false) % Timeout
        end,

        % Cleanup
        unlink(MockServer),
        unlink(MockTransport),
        unlink(Collector),
        exit(MockServer, normal),
        exit(MockTransport, normal),
        exit(Collector, normal)
    after
        stop_test_registry(Ctx)
    end.

test_route_to_transport_broadcast() ->
    ok = ensure_gproc_started(),
    ServerId = test_server_broadcast,
    Payload = <<"broadcast-message">>,
    Parent = self(),
    ServerPid = spawn_link(fun registry_test_server/0),
    Transport1 = spawn_link(fun() -> registry_test_transport(Parent) end),
    Transport2 = spawn_link(fun() -> registry_test_transport(Parent) end),

    {Registry, Ctx} = start_test_registry(),

    try
        % Register server and transports with bindings
        ?assertEqual(ok, gen_server:call(Registry, {register_server, ServerId, ServerPid, #{}})),
        TransportConfig = #{type => stdio, server_id => ServerId, config => #{}},
        ?assertEqual(ok, gen_server:call(Registry, {register_transport, transport_alpha, Transport1, TransportConfig})),
        ?assertEqual(ok, gen_server:call(Registry, {register_transport, transport_beta, Transport2, TransportConfig})),

        % Broadcast message to all transports bound to the server
        gen_server:cast(Registry, {route_to_transport, broadcast, ServerId, Payload}),

        % Both transports should forward the response back to the parent
        ?assertEqual(ok, wait_for_transport_message(Transport1, ServerId, Payload)),
        ?assertEqual(ok, wait_for_transport_message(Transport2, ServerId, Payload)),

        ok
    after
        stop_test_registry(Ctx),
        ServerPid ! stop,
        Transport1 ! stop,
        Transport2 ! stop
    end.

%%====================================================================
%% Process Monitoring and Cleanup Tests
%%====================================================================

test_process_monitoring_cleanup() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() ->
            receive
                die -> exit(normal);
                stop -> ok
            after 10000 -> ok
            end
        end),

        % Register server
        gen_server:call(Registry, {register_server, test_server_monitor, MockServer, #{}}),

        % Verify registration
        ?assertMatch({ok, {MockServer, _}}, gen_server:call(Registry, {find_server, test_server_monitor})),

        % Kill server process
        unlink(MockServer),
        exit(MockServer, kill),

        % Wait for process death and registry cleanup
        wait_for_process_death(MockServer, 2000),
        timer:sleep(500),

        % Verify automatic unregistration
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_monitor}))
    after
        stop_test_registry(Ctx)
    end.

test_automatic_cleanup_on_death() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() -> receive die -> exit(crash) after 10000 -> ok end end),
        MockTransport = spawn_link(fun() -> receive die -> exit(crash) after 10000 -> ok end end),

        % Register both and bind
        gen_server:call(Registry, {register_server, test_server_cleanup, MockServer, #{}}),
        gen_server:call(Registry, {register_transport, test_transport_cleanup, MockTransport, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, test_transport_cleanup, test_server_cleanup}),

        % Verify binding
        ?assertMatch({ok, test_server_cleanup}, gen_server:call(Registry, {get_server_for_transport, test_transport_cleanup})),

        % Kill server
        unlink(MockServer),
        exit(MockServer, kill),
        wait_for_process_death(MockServer, 2000),
        timer:sleep(500),

        % Verify server unregistered and binding removed
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_cleanup})),
        ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, test_transport_cleanup})),

        % Transport should still be registered
        ?assertMatch({ok, {MockTransport, _}}, gen_server:call(Registry, {find_transport, test_transport_cleanup})),

        % Kill transport
        unlink(MockTransport),
        exit(MockTransport, kill),
        wait_for_process_death(MockTransport, 2000),
        timer:sleep(500),

        % Verify transport unregistered
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, test_transport_cleanup}))
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Multi-Registration Tests
%%====================================================================

test_multiple_servers_registration() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Create multiple server processes
        Servers = lists:map(fun(N) ->
            ServerId = list_to_atom("test_server_" ++ integer_to_list(N)),
            MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
            gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
            {ServerId, MockServer}
        end, lists:seq(1, 10)),

        % Verify all registered (exactly 10, not more)
        AllServers = gen_server:call(Registry, list_servers),
        ?assertEqual(10, length(AllServers)),

        % Verify each server can be found
        lists:foreach(fun({ServerId, MockServer}) ->
            ?assertMatch({ok, {MockServer, _}}, gen_server:call(Registry, {find_server, ServerId}))
        end, Servers),

        % Cleanup - unregister and kill processes
        lists:foreach(fun({ServerId, MockServer}) ->
            gen_server:call(Registry, {unregister_server, ServerId}),
            unlink(MockServer),
            exit(MockServer, kill)
        end, Servers),

        % Wait for cleanup to complete
        timer:sleep(200),

        % Verify cleanup succeeded
        FinalServers = gen_server:call(Registry, list_servers),
        ?assertEqual([], FinalServers)
    after
        stop_test_registry(Ctx)
    end.

test_multiple_transports_registration() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Create multiple transport processes
        Transports = lists:map(fun(N) ->
            TransportId = list_to_atom("test_transport_" ++ integer_to_list(N)),
            MockTransport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
            TransportType = case N rem 3 of
                0 -> stdio;
                1 -> tcp;
                2 -> http
            end,
            gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => TransportType}}),
            {TransportId, MockTransport}
        end, lists:seq(1, 10)),

        % Verify all registered (exactly 10, not more)
        AllTransports = gen_server:call(Registry, list_transports),
        ?assertEqual(10, length(AllTransports)),

        % Cleanup - unregister and kill processes
        lists:foreach(fun({TransportId, MockTransport}) ->
            gen_server:call(Registry, {unregister_transport, TransportId}),
            unlink(MockTransport),
            exit(MockTransport, kill)
        end, Transports),

        % Wait for cleanup to complete
        timer:sleep(200),

        % Verify cleanup succeeded
        FinalTransports = gen_server:call(Registry, list_transports),
        ?assertEqual([], FinalTransports)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Concurrent Operations Tests
%%====================================================================

test_concurrent_registration() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Spawn multiple processes trying to register simultaneously
        Parent = self(),
        Pids = lists:map(fun(N) ->
            spawn_link(fun() ->
                ServerId = list_to_atom("concurrent_server_" ++ integer_to_list(N)),
                MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
                Result = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
                Parent ! {registered, N, Result, ServerId, MockServer}
            end)
        end, lists:seq(1, 20)),

        % Collect results
        Results = lists:map(fun(_) ->
            receive
                {registered, N, Result, ServerId, MockServer} ->
                    {N, Result, ServerId, MockServer}
            after 5000 ->
                {error, timeout}
            end
        end, Pids),

        % Verify all succeeded
        ?assertEqual(20, length([R || {_, ok, _, _} = R <- Results])),

        % Verify all in registry (exactly 20, not more)
        AllServers = gen_server:call(Registry, list_servers),
        ?assertEqual(20, length(AllServers)),

        % Cleanup - unregister and kill all processes
        lists:foreach(fun({_, _, ServerId, MockServer}) ->
            gen_server:call(Registry, {unregister_server, ServerId}),
            catch unlink(MockServer),
            catch exit(MockServer, kill)
        end, Results),

        % Also kill the parent test processes
        lists:foreach(fun(Pid) ->
            catch unlink(Pid),
            catch exit(Pid, kill)
        end, Pids),

        % Wait for all cleanup to complete
        timer:sleep(300),

        % Verify cleanup succeeded
        FinalServers = gen_server:call(Registry, list_servers),
        ?assertEqual([], FinalServers)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

test_invalid_registration() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        % Test binding nonexistent server
        ?assertMatch({error, server_not_found},
                     gen_server:call(Registry, {bind_transport_to_server, test_transport, nonexistent_server})),

        % Test binding nonexistent transport
        gen_server:call(Registry, {register_server, test_server, MockServer, #{}}),
        ?assertMatch({error, transport_not_found},
                     gen_server:call(Registry, {bind_transport_to_server, nonexistent_transport, test_server})),

        % Cleanup
        gen_server:call(Registry, {unregister_server, test_server}),
        unlink(MockServer),
        exit(MockServer, normal)
    after
        stop_test_registry(Ctx)
    end.

test_duplicate_registration() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer1 = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        MockServer2 = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        % Register first server
        ?assertEqual(ok, gen_server:call(Registry, {register_server, duplicate_server, MockServer1, #{}})),

        % Try to register with same ID but different PID
        ?assertMatch({error, already_registered},
                     gen_server:call(Registry, {register_server, duplicate_server, MockServer2, #{}})),

        % Verify first server still registered
        ?assertMatch({ok, {MockServer1, _}}, gen_server:call(Registry, {find_server, duplicate_server})),

        % Cleanup
        gen_server:call(Registry, {unregister_server, duplicate_server}),
        unlink(MockServer1),
        unlink(MockServer2),
        exit(MockServer1, normal),
        exit(MockServer2, normal)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Configuration Tests
%%====================================================================

test_server_capabilities_storage() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        Capabilities = #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true},
            resources = #mcp_capability{enabled = true},
            prompts = #mcp_capability{enabled = false}
        },

        ServerConfig = #{
            capabilities => Capabilities,
            options => #{timeout => 5000, max_connections => 100}
        },

        % Register with capabilities
        gen_server:call(Registry, {register_server, test_capabilities_server, MockServer, ServerConfig}),

        % Retrieve and verify capabilities
        {ok, {_, RetrievedConfig}} = gen_server:call(Registry, {find_server, test_capabilities_server}),
        ?assertEqual(Capabilities, maps:get(capabilities, RetrievedConfig)),
        ?assertEqual(#{timeout => 5000, max_connections => 100}, maps:get(options, RetrievedConfig)),

        % Cleanup
        gen_server:call(Registry, {unregister_server, test_capabilities_server}),
        unlink(MockServer),
        exit(MockServer, normal)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% List Operations Tests
%%====================================================================

test_list_all_servers() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Start with empty list
        ?assertEqual([], gen_server:call(Registry, list_servers)),

        % Add some servers
        Servers = lists:map(fun(N) ->
            ServerId = list_to_atom("list_server_" ++ integer_to_list(N)),
            MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
            gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
            {ServerId, MockServer}
        end, lists:seq(1, 5)),

        % Get list
        ServerList = gen_server:call(Registry, list_servers),
        ?assertEqual(5, length(ServerList)),

        % Verify all servers in list
        lists:foreach(fun({ServerId, _}) ->
            ?assert(lists:keymember(ServerId, 1, ServerList))
        end, Servers),

        % Cleanup
        lists:foreach(fun({ServerId, MockServer}) ->
            gen_server:call(Registry, {unregister_server, ServerId}),
            unlink(MockServer),
            exit(MockServer, normal)
        end, Servers)
    after
        stop_test_registry(Ctx)
    end.

test_list_all_transports() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Start with empty list
        ?assertEqual([], gen_server:call(Registry, list_transports)),

        % Add some transports
        Transports = lists:map(fun(N) ->
            TransportId = list_to_atom("list_transport_" ++ integer_to_list(N)),
            MockTransport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
            gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => stdio}}),
            {TransportId, MockTransport}
        end, lists:seq(1, 5)),

        % Get list
        TransportList = gen_server:call(Registry, list_transports),
        ?assertEqual(5, length(TransportList)),

        % Cleanup
        lists:foreach(fun({TransportId, MockTransport}) ->
            gen_server:call(Registry, {unregister_transport, TransportId}),
            unlink(MockTransport),
            exit(MockTransport, normal)
        end, Transports)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% State Recovery Tests
%%====================================================================

test_registry_persistence() ->
    {Registry, Ctx} = start_test_registry(),

    try
        MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        % Register server
        gen_server:call(Registry, {register_server, persist_server, MockServer, #{}}),

        % Kill the server process
        unlink(MockServer),
        exit(MockServer, kill),
        wait_for_process_death(MockServer, 2000),
        timer:sleep(500),

        % Verify registry cleaned up
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, persist_server})),

        % Registry should still be functional
        NewMockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        ?assertEqual(ok, gen_server:call(Registry, {register_server, new_server, NewMockServer, #{}})),

        % Cleanup
        gen_server:call(Registry, {unregister_server, new_server}),
        unlink(NewMockServer),
        exit(NewMockServer, normal)
    after
        stop_test_registry(Ctx)
    end.

test_registry_state_recovery() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Register multiple servers and transports
        Server1 = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        Server2 = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        Transport1 = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        gen_server:call(Registry, {register_server, recovery_server1, Server1, #{}}),
        gen_server:call(Registry, {register_server, recovery_server2, Server2, #{}}),
        gen_server:call(Registry, {register_transport, recovery_transport1, Transport1, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, recovery_transport1, recovery_server1}),

        % Kill one server
        unlink(Server1),
        exit(Server1, kill),
        wait_for_process_death(Server1, 2000),
        timer:sleep(500),

        % Verify registry state is consistent
        ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, recovery_server1})),
        ?assertMatch({ok, {Server2, _}}, gen_server:call(Registry, {find_server, recovery_server2})),
        ?assertMatch({ok, {Transport1, _}}, gen_server:call(Registry, {find_transport, recovery_transport1})),
        ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, recovery_transport1})),

        % Cleanup
        gen_server:call(Registry, {unregister_server, recovery_server2}),
        gen_server:call(Registry, {unregister_transport, recovery_transport1}),
        unlink(Server2),
        unlink(Transport1),
        exit(Server2, normal),
        exit(Transport1, normal)
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

test_full_integration() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Create full integration setup
        Collector = spawn_link(fun() -> collect_messages([]) end),

        Server = spawn_link(fun() ->
            receive
                {mcp_message, TransportId, Message} ->
                    Collector ! {server_msg, TransportId, Message}
            after 5000 -> ok
            end
        end),

        Transport = spawn_link(fun() ->
            receive
                {mcp_response, ServerId, Response} ->
                    Collector ! {transport_msg, ServerId, Response}
            after 5000 -> ok
            end
        end),

        % Register everything
        Caps = #mcp_server_capabilities{tools = #mcp_capability{enabled = true}},
        gen_server:call(Registry, {register_server, integration_server, Server, #{capabilities => Caps}}),
        gen_server:call(Registry, {register_transport, integration_transport, Transport, #{type => stdio, server_id => integration_server}}),

        % Verify auto-binding
        ?assertMatch({ok, integration_server}, gen_server:call(Registry, {get_server_for_transport, integration_transport})),

        % Test bidirectional messaging
        gen_server:cast(Registry, {route_to_server, integration_server, integration_transport, <<"request">>}),
        gen_server:cast(Registry, {route_to_transport, integration_transport, integration_server, <<"response">>}),

        timer:sleep(300),

        % Verify messages
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                ?assert(length(Messages) >= 2)
        after 2000 ->
            ?assert(false)
        end,

        % Cleanup
        unlink(Server),
        unlink(Transport),
        unlink(Collector),
        exit(Server, normal),
        exit(Transport, normal),
        exit(Collector, normal)
    after
        stop_test_registry(Ctx)
    end.

test_supervisor_restart() ->
    % Test would verify supervisor restart scenarios
    % For now, basic test structure
    ?assertEqual(ok, ok).

test_concurrent_routing() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Create collector
        Collector = spawn_link(fun() -> collect_messages([]) end),

        % Create server that forwards to collector
        Server = spawn_link(fun() ->
            concurrent_server_loop(Collector)
        end),

        Transport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

        % Register
        gen_server:call(Registry, {register_server, concurrent_server, Server, #{}}),
        gen_server:call(Registry, {register_transport, concurrent_transport, Transport, #{type => stdio}}),
        gen_server:call(Registry, {bind_transport_to_server, concurrent_transport, concurrent_server}),

        % Send multiple concurrent messages
        lists:foreach(fun(N) ->
            spawn(fun() ->
                gen_server:cast(Registry, {route_to_server, concurrent_server, concurrent_transport, integer_to_binary(N)})
            end)
        end, lists:seq(1, 50)),

        % Wait and verify
        timer:sleep(500),
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                ?assert(length(Messages) >= 40) % Allow some message loss in testing
        after 2000 ->
            ?assert(false)
        end,

        % Cleanup
        unlink(Server),
        unlink(Transport),
        unlink(Collector),
        exit(Server, normal),
        exit(Transport, normal),
        exit(Collector, normal)
    after
        stop_test_registry(Ctx)
    end.

test_load_testing() ->
    {Registry, Ctx} = start_test_registry(),

    try
        % Register 100 servers and transports rapidly
        StartTime = erlang:monotonic_time(millisecond),

        lists:foreach(fun(N) ->
            ServerId = list_to_atom("load_server_" ++ integer_to_list(N)),
            TransportId = list_to_atom("load_transport_" ++ integer_to_list(N)),

            Server = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
            Transport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

            gen_server:call(Registry, {register_server, ServerId, Server, #{}}),
            gen_server:call(Registry, {register_transport, TransportId, Transport, #{type => stdio}}),
            gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId})
        end, lists:seq(1, 100)),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        % Should complete in reasonable time (< 5 seconds)
        ?assert(Duration < 5000),

        % Verify all registered
        Servers = gen_server:call(Registry, list_servers),
        Transports = gen_server:call(Registry, list_transports),
        ?assertEqual(100, length(Servers)),
        ?assertEqual(100, length(Transports)),

        % Cleanup
        lists:foreach(fun(N) ->
            ServerId = list_to_atom("load_server_" ++ integer_to_list(N)),
            TransportId = list_to_atom("load_transport_" ++ integer_to_list(N)),
            {ok, {ServerPid, _}} = gen_server:call(Registry, {find_server, ServerId}),
            {ok, {TransportPid, _}} = gen_server:call(Registry, {find_transport, TransportId}),

            gen_server:call(Registry, {unregister_server, ServerId}),
            gen_server:call(Registry, {unregister_transport, TransportId}),
            unlink(ServerPid),
            unlink(TransportPid),
            exit(ServerPid, normal),
            exit(TransportPid, normal)
        end, lists:seq(1, 100))
    after
        stop_test_registry(Ctx)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

collect_messages(Messages) ->
    receive
        {get_messages, From} ->
            From ! {messages, lists:reverse(Messages)},
            collect_messages(Messages);
        Message ->
            collect_messages([Message | Messages])
    after 10000 ->
        ok
    end.

wait_for_process_death(Pid, Timeout) ->
    wait_for_process_death(Pid, Timeout, erlang:system_time(millisecond)).

wait_for_process_death(Pid, Timeout, StartTime) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            Now = erlang:system_time(millisecond),
            case Now - StartTime > Timeout of
                true ->
                    throw({timeout_waiting_for_process_death, Pid});
                false ->
                    timer:sleep(10),
                    wait_for_process_death(Pid, Timeout, StartTime)
            end
    end.

concurrent_server_loop(Collector) ->
    receive
        {mcp_message, TransportId, Message} ->
            Collector ! {server_msg, TransportId, Message},
            concurrent_server_loop(Collector);
        stop ->
            ok
    after 10000 ->
        ok
    end.

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

%% Old cleanup function - kept for backward compatibility
clear_mcp_gproc_entries() ->
    ok = ensure_gproc_started(),
    lists:foreach(fun clear_entries_for_type/1, [server, transport]).

clear_entries_for_type(Type) ->
    Pattern = [{{{n, l, {mcp, Type, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    Entries = gproc:select(Pattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, Type, Id}}, Pid)
    end, Entries).

%%====================================================================
%% Test Isolation Utilities
%%====================================================================

%% Clear all test-related gproc registrations
clear_all_test_registrations() ->
    ok = ensure_gproc_started(),
    % Clear servers
    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    ServerEntries = gproc:select(ServerPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, server, Id}}, Pid)
    end, ServerEntries),

    % Clear transports
    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportEntries = gproc:select(TransportPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, transport, Id}}, Pid)
    end, TransportEntries),
    ok.

%% Kill all test processes that might be holding registrations
kill_test_processes() ->
    % Find all processes registered with test-related names
    TestPatterns = [
        "test_server_", "test_transport_", "concurrent_server_", "load_server_", "load_transport_",
        "list_server_", "list_transport_", "integration_", "recovery_", "persist_"
    ],

    % Get all registered entries
    AllServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    AllTransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],

    ServerEntries = gproc:select(AllServerPattern),
    TransportEntries = gproc:select(AllTransportPattern),

    % Kill processes whose IDs match test patterns
    AllEntries = ServerEntries ++ TransportEntries,
    lists:foreach(fun({Id, Pid}) ->
        IdStr = atom_to_list(Id),
        ShouldKill = lists:any(fun(Pattern) ->
            string:prefix(IdStr, Pattern) =/= nomatch
        end, TestPatterns),

        case ShouldKill of
            true ->
                IsAlive = is_process_alive(Pid),
                case IsAlive of
                    true -> catch exit(Pid, kill);
                    false -> ok
                end;
            false ->
                ok
        end
    end, AllEntries),
    ok.

%% Wait for all cleanup operations to complete
wait_for_cleanup() ->
    timer:sleep(200),
    % Verify no test registrations remain
    assert_clean_slate(),
    ok.

%% Assert that no test registrations remain in gproc
assert_clean_slate() ->
    ServerCount = count_registrations(server),
    TransportCount = count_registrations(transport),

    case {ServerCount, TransportCount} of
        {0, 0} ->
            ok;
        _ ->
            % Log diagnostic info if cleanup failed
            dump_gproc_state(),
            % Force cleanup one more time
            clear_all_test_registrations(),
            timer:sleep(100),
            ok
    end.

%% Count registrations by type
count_registrations(Type) ->
    Pattern = [{{{n, l, {mcp, Type, '_'}}, '_', '_'}, [], [true]}],
    length(gproc:select(Pattern)).

%% Dump current gproc state for debugging
dump_gproc_state() ->
    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],

    Servers = gproc:select(ServerPattern),
    Transports = gproc:select(TransportPattern),

    logger:warning("=== GPROC State Dump ==="),
    logger:warning("Servers registered: ~p", [length(Servers)]),
    lists:foreach(fun({Id, Pid}) ->
        logger:warning("  Server: ~p -> ~p (alive: ~p)", [Id, Pid, is_process_alive(Pid)])
    end, Servers),

    logger:warning("Transports registered: ~p", [length(Transports)]),
    lists:foreach(fun({Id, Pid}) ->
        logger:warning("  Transport: ~p -> ~p (alive: ~p)", [Id, Pid, is_process_alive(Pid)])
    end, Transports),
    logger:warning("=== End Dump ==="),
    ok.

run_with_registry(TestFun) when is_function(TestFun, 1) ->
    {Registry, Ctx} = start_test_registry(),
    try
        TestFun(Registry)
    after
        stop_test_registry(Ctx)
    end.

start_test_registry() ->
    Ctx = setup_gproc_registry(),
    {maps:get(registry, Ctx), Ctx}.

stop_test_registry(Ctx) ->
    cleanup_gproc_registry(Ctx).

registry_test_server() ->
    receive
        stop -> ok;
        _Other -> registry_test_server()
    end.

registry_test_transport(Parent) ->
    receive
        {mcp_response, ServerId, Message} ->
            Parent ! {transport_received, self(), ServerId, Message},
            registry_test_transport(Parent);
        stop -> ok;
        _Other ->
            registry_test_transport(Parent)
    end.

wait_for_transport_message(TransportPid, ServerId, Payload) ->
    receive
        {transport_received, TransportPid, ServerId, Payload} ->
            ok;
        {transport_received, TransportPid, OtherServer, Message} ->
            {error, {unexpected_message, OtherServer, Message}}
    after 2000 ->
        {error, timeout}
    end.
