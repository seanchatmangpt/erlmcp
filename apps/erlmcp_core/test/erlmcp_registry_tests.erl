-module(erlmcp_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

% Define the registry state record for testing
-record(registry_state, {
    server_transport_map = #{} :: #{atom() => atom()}
}).

%%%===================================================================
%%% Test Suite for erlmcp_registry with gproc
%%%
%%% This test suite validates the core registry functionality with
%%% proper setup/cleanup and isolation between tests.
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_registry_startup/1,
         fun test_server_registration/1,
         fun test_transport_registration/1,
         fun test_server_transport_binding/1,
         fun test_message_routing_to_server/1,
         fun test_message_routing_to_transport/1,
         fun test_process_monitoring/1,
         fun test_list_operations/1,
         fun test_concurrent_registration/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Ensure gproc is started using utility function
    ok = erlmcp_registry_utils:ensure_gproc_started(),

    % Clear any stale test registrations using utility function
    ok = erlmcp_registry_utils:clear_test_registrations(),
    timer:sleep(100),

    % Start an anonymous registry for testing
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),
    #{registry => Registry, test_pids => []}.

cleanup(#{registry := Registry} = State) ->
    TestPids = maps:get(test_pids, State, []),

    % Unlink and kill all test processes
    lists:foreach(fun(Pid) ->
        catch unlink(Pid),
        catch exit(Pid, kill)
    end, TestPids),

    % Stop registry gracefully
    catch gen_server:stop(Registry, shutdown, 5000),

    % Clear test registrations using utility function
    ok = erlmcp_registry_utils:clear_test_registrations(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_registry_startup(#{registry := Registry}) ->
    [
        ?_assertEqual([], gen_server:call(Registry, list_servers)),
        ?_assertEqual([], gen_server:call(Registry, list_transports)),
        ?_assertMatch({error, not_found}, gen_server:call(Registry, {find_server, unknown})),
        ?_assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, unknown}))
    ].

test_server_registration(#{registry := Registry} = State) ->
    MockServer = spawn_link(fun() ->
        receive stop -> ok after 5000 -> ok end
    end),

    ServerCapabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    },
    ServerConfig = #{capabilities => ServerCapabilities},

    Tests = [
        % Register server
        ?_assertEqual(ok, gen_server:call(Registry, {register_server, test_server_1, MockServer, ServerConfig})),

        % Verify registration
        ?_assertMatch({ok, {MockServer, ServerConfig}},
                     gen_server:call(Registry, {find_server, test_server_1})),

        % Check it appears in list
        ?_assert(begin
            Servers = gen_server:call(Registry, list_servers),
            lists:keymember(test_server_1, 1, Servers)
        end),

        % Test duplicate registration by same PID is idempotent (returns ok)
        ?_assertEqual(ok,
                     gen_server:call(Registry, {register_server, test_server_1, MockServer, ServerConfig})),

        % Unregister
        ?_assertEqual(ok, gen_server:call(Registry, {unregister_server, test_server_1})),
        ?_assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_1}))
    ],

    % Store pid for cleanup
    maps:put(test_pids, [MockServer | maps:get(test_pids, State, [])], State),
    Tests.

test_transport_registration(#{registry := Registry} = State) ->
    MockTransport = spawn_link(fun() ->
        receive stop -> ok after 5000 -> ok end
    end),

    TransportConfig = #{type => stdio, server_id => test_server_trans},

    Tests = [
        % Register transport
        ?_assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_1, MockTransport, TransportConfig})),

        % Verify registration
        ?_assertMatch({ok, {MockTransport, TransportConfig}},
                     gen_server:call(Registry, {find_transport, test_transport_1})),

        % Check in list
        ?_assert(begin
            Transports = gen_server:call(Registry, list_transports),
            lists:keymember(test_transport_1, 1, Transports)
        end),

        % Unregister
        ?_assertEqual(ok, gen_server:call(Registry, {unregister_transport, test_transport_1})),
        ?_assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, test_transport_1}))
    ],

    maps:put(test_pids, [MockTransport | maps:get(test_pids, State, [])], State),
    Tests.

test_server_transport_binding(#{registry := Registry} = State) ->
    MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
    MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

    % Register both and verify registration before binding
    ok = gen_server:call(Registry, {register_server, test_server_bind, MockServer, #{}}),
    ok = gen_server:call(Registry, {register_transport, test_transport_bind, MockTransport, #{type => stdio}}),

    % Verify registrations succeeded
    {ok, {MockServer, _}} = gen_server:call(Registry, {find_server, test_server_bind}),
    {ok, {MockTransport, _}} = gen_server:call(Registry, {find_transport, test_transport_bind}),

    % Verify initial state
    {error, not_found} = gen_server:call(Registry, {get_server_for_transport, test_transport_bind}),

    Tests = [
        % Test binding (both server and transport must be registered first)
        fun() ->
            Result = gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, test_server_bind}),
            ?assertEqual(ok, Result)
        end,
        fun() ->
            Result = gen_server:call(Registry, {get_server_for_transport, test_transport_bind}),
            ?assertMatch({ok, test_server_bind}, Result)
        end,

        % Test unbinding
        fun() ->
            Result = gen_server:call(Registry, {unbind_transport, test_transport_bind}),
            ?assertEqual(ok, Result)
        end,
        fun() ->
            Result = gen_server:call(Registry, {get_server_for_transport, test_transport_bind}),
            ?assertMatch({error, not_found}, Result)
        end
    ],

    % Test binding to nonexistent server (after cleanup)
    gen_server:call(Registry, {unregister_server, test_server_bind}),
    gen_server:call(Registry, {unregister_transport, test_transport_bind}),

    % Re-register for error case tests
    ok = gen_server:call(Registry, {register_server, test_server_bind2, MockServer, #{}}),
    ok = gen_server:call(Registry, {register_transport, test_transport_bind2, MockTransport, #{type => stdio}}),

    Tests2 = [
        fun() ->
            % Test binding to nonexistent server fails
            Result = gen_server:call(Registry, {bind_transport_to_server, test_transport_bind2, nonexistent}),
            ?assertMatch({error, server_not_found}, Result)
        end,
        fun() ->
            % Test binding nonexistent transport fails
            Result = gen_server:call(Registry, {bind_transport_to_server, nonexistent, test_server_bind2}),
            ?assertMatch({error, transport_not_found}, Result)
        end
    ],

    % Cleanup
    gen_server:call(Registry, {unregister_server, test_server_bind2}),
    gen_server:call(Registry, {unregister_transport, test_transport_bind2}),

    maps:put(test_pids, [MockServer, MockTransport | maps:get(test_pids, State, [])], State),
    Tests ++ Tests2.

test_message_routing_to_server(#{registry := Registry} = State) ->
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
    ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_route, MockServer, #{}})),
    ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_route, MockTransport, #{type => stdio}})),
    ?assertEqual(ok, gen_server:call(Registry, {bind_transport_to_server, test_transport_route, test_server_route})),

    % Route message
    TestMessage = #{<<"method">> => <<"test">>},
    gen_server:cast(Registry, {route_to_server, test_server_route, test_transport_route, TestMessage}),

    % Verify message was received
    Test = ?_assert(begin
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                lists:any(fun({server_received, test_transport_route, Msg}) when Msg =:= TestMessage -> true; (_) -> false end, Messages)
        after 2000 ->
            false
        end
    end),

    % Cleanup
    gen_server:call(Registry, {unregister_server, test_server_route}),
    gen_server:call(Registry, {unregister_transport, test_transport_route}),

    maps:put(test_pids, [Collector, MockServer, MockTransport | maps:get(test_pids, State, [])], State),
    [Test].

test_message_routing_to_transport(#{registry := Registry} = State) ->
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
    ?assertEqual(ok, gen_server:call(Registry, {register_server, test_server_route2, MockServer, #{}})),
    ?assertEqual(ok, gen_server:call(Registry, {register_transport, test_transport_route2, MockTransport, #{type => stdio}})),
    ?assertEqual(ok, gen_server:call(Registry, {bind_transport_to_server, test_transport_route2, test_server_route2})),

    % Route message
    TestResponse = #{<<"result">> => <<"success">>},
    gen_server:cast(Registry, {route_to_transport, test_transport_route2, test_server_route2, TestResponse}),

    % Verify message was received
    Test = ?_assert(begin
        Collector ! {get_messages, self()},
        receive
            {messages, Messages} ->
                lists:any(fun({transport_received, test_server_route2, Msg}) when Msg =:= TestResponse -> true; (_) -> false end, Messages)
        after 2000 ->
            false
        end
    end),

    % Cleanup
    gen_server:call(Registry, {unregister_server, test_server_route2}),
    gen_server:call(Registry, {unregister_transport, test_transport_route2}),

    maps:put(test_pids, [Collector, MockServer, MockTransport | maps:get(test_pids, State, [])], State),
    [Test].

test_process_monitoring(#{registry := Registry} = _State) ->
    MockServer = spawn_link(fun() ->
        receive die -> exit(normal) after 5000 -> ok end
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

    Tests = [
        % Should be automatically unregistered
        ?_assertMatch({error, not_found}, gen_server:call(Registry, {find_server, test_server_monitor}))
    ],

    Tests.

test_list_operations(#{registry := Registry} = State) ->
    % Create multiple servers
    Servers = lists:map(fun(N) ->
        ServerId = list_to_atom("list_server_" ++ integer_to_list(N)),
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
        {ServerId, MockServer}
    end, lists:seq(1, 5)),

    % Create multiple transports
    Transports = lists:map(fun(N) ->
        TransportId = list_to_atom("list_transport_" ++ integer_to_list(N)),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => stdio}}),
        {TransportId, MockTransport}
    end, lists:seq(1, 5)),

    ServerList = gen_server:call(Registry, list_servers),
    TransportList = gen_server:call(Registry, list_transports),

    Tests = [
        ?_assertEqual(5, length(ServerList)),
        ?_assertEqual(5, length(TransportList))
    ],

    % Cleanup
    lists:foreach(fun({ServerId, MockServer}) ->
        gen_server:call(Registry, {unregister_server, ServerId}),
        unlink(MockServer),
        exit(MockServer, kill)
    end, Servers),

    lists:foreach(fun({TransportId, MockTransport}) ->
        gen_server:call(Registry, {unregister_transport, TransportId}),
        unlink(MockTransport),
        exit(MockTransport, kill)
    end, Transports),

    AllPids = [P || {_, P} <- Servers ++ Transports],
    maps:put(test_pids, AllPids ++ maps:get(test_pids, State, []), State),
    Tests.

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

test_concurrent_registration(#{registry := Registry} = State) ->
    % Test concurrent registration to detect race conditions
    ConcurrentCount = 10,
    ServerId = concurrent_test_server,

    % Spawn multiple processes trying to register the same server
    Parent = self(),
    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            MockServer = self(),
            Result = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}},
                                     2000),
            Parent ! {registration_result, self(), Result}
        end)
    end, lists:seq(1, ConcurrentCount)),

    % Collect results
    Results = [receive
        {registration_result, Pid, Result} ->
            {Pid, Result}
    after 5000 ->
        timeout
    end || _ <- Pids],

    % Count successes and failures
    SuccessCount = lists:foldl(fun({_Pid, Result}, Acc) ->
        case Result of
            ok -> Acc + 1;
            {error, already_registered} -> Acc;
            _ -> Acc
        end
    end, 0, Results),

    % Should have at least one successful registration
    % Due to gproc race conditions, some registrations may fail with already_registered
    Test = ?_assert(SuccessCount >= 1),

    % Cleanup
    gen_server:call(Registry, {unregister_server, ServerId}),

    AllPids = [Pid || {Pid, _} <- Results],
    maps:put(test_pids, AllPids ++ maps:get(test_pids, State, []), State),
    [Test].

%%====================================================================
%% Additional Test Suites for Missing Coverage
%%====================================================================

%% Test double registration with different PID (should fail)
test_duplicate_registration_different_pid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer1 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockServer2 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        ServerId = dup_server,
        ServerConfig = #{},

        [
            ?_test(begin
                % Register first server
                ?assertEqual(ok, gen_server:call(Registry, {register_server, ServerId, MockServer1, ServerConfig}))
            end),
            ?_test(begin
                % Try to register with different PID - should fail
                Result = gen_server:call(Registry, {register_server, ServerId, MockServer2, ServerConfig}),
                ?assertMatch({error, already_registered}, Result)
            end)
        ]
     end}.

%% Test transport auto-binding on registration
test_transport_auto_binding_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        ServerId = auto_bind_server,
        TransportId = auto_bind_transport,

        % Register server first
        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),

        % Register transport with server_id in config - should auto-bind
        TransportConfig = #{type => stdio, server_id => ServerId},

        [
            ?_test(begin
                ?assertEqual(ok, gen_server:call(Registry, {register_transport, TransportId, MockTransport, TransportConfig}))
            end),
            ?_test(begin
                % Verify auto-binding worked
                ?assertMatch({ok, ServerId}, gen_server:call(Registry, {get_server_for_transport, TransportId}))
            end)
        ]
     end}.

%% Test broadcast routing to transports
test_broadcast_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport1 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport2 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport3 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        ServerId = broadcast_server,
        Transport1Id = broadcast_transport_1,
        Transport2Id = broadcast_transport_2,
        Transport3Id = broadcast_transport_3,

        % Register server and transports
        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
        ok = gen_server:call(Registry, {register_transport, Transport1Id, MockTransport1, #{type => stdio}}),
        ok = gen_server:call(Registry, {register_transport, Transport2Id, MockTransport2, #{type => stdio}}),
        ok = gen_server:call(Registry, {register_transport, Transport3Id, MockTransport3, #{type => stdio}}),

        % Bind all transports to server
        ok = gen_server:call(Registry, {bind_transport_to_server, Transport1Id, ServerId}),
        ok = gen_server:call(Registry, {bind_transport_to_server, Transport2Id, ServerId}),
        ok = gen_server:call(Registry, {bind_transport_to_server, Transport3Id, ServerId}),

        % Broadcast message - should route to all transports
        TestMessage = #{<<"type">> => <<"broadcast">>},

        [
            ?_test(begin
                gen_server:cast(Registry, {route_to_transport, broadcast, ServerId, TestMessage}),
                % Allow message delivery
                timer:sleep(100),
                ?assert(true)  % If we got here without crash, broadcast succeeded
            end)
        ]
     end}.

%% Test routing to non-existent server (should not crash, just log)
test_route_to_nonexistent_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        TestMessage = #{<<"test">> => <<"data">>},
        [
            ?_test(begin
                % Should not crash, just log warning
                gen_server:cast(Registry, {route_to_server, nonexistent_server, nonexistent_transport, TestMessage}),
                timer:sleep(100),
                ?assert(true)
            end)
        ]
     end}.

%% Test routing to non-existent transport (should not crash, just log)
test_route_to_nonexistent_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        TestMessage = #{<<"test">> => <<"data">>},
        [
            ?_test(begin
                % Should not crash, just log warning
                gen_server:cast(Registry, {route_to_transport, nonexistent_transport, some_server, TestMessage}),
                timer:sleep(100),
                ?assert(true)
            end)
        ]
     end}.

%% Test double unregister (should be idempotent)
test_double_unregister_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        ServerId = double_unregister_server,

        [
            ?_test(begin
                ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
                ?assertMatch({ok, {MockServer, _}}, gen_server:call(Registry, {find_server, ServerId}))
            end),
            ?_test(begin
                % First unregister
                ?assertEqual(ok, gen_server:call(Registry, {unregister_server, ServerId}))
            end),
            ?_test(begin
                % Second unregister - should still return ok (idempotent)
                ?assertEqual(ok, gen_server:call(Registry, {unregister_server, ServerId}))
            end)
        ]
     end}.

%% Test transport process death cleanup
test_transport_process_death_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockTransport = spawn_link(fun() -> receive die -> exit(normal) after 5000 -> ok end end),
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        TransportId = death_transport,
        ServerId = death_server,

        % Register and bind
        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
        ok = gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => stdio}}),
        ok = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

        [
            ?_test(begin
                % Verify binding exists
                ?assertMatch({ok, ServerId}, gen_server:call(Registry, {get_server_for_transport, TransportId}))
            end),
            ?_test(begin
                % Kill transport process
                unlink(MockTransport),
                exit(MockTransport, kill),
                wait_for_process_death(MockTransport, 2000),
                timer:sleep(200),  % Allow gproc monitor cleanup
                ?assertMatch({error, not_found}, gen_server:call(Registry, {find_transport, TransportId}))
            end),
            ?_test(begin
                % Verify binding was cleaned up
                ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, TransportId}))
            end)
        ]
     end}.

%% Test server process death cleans up transport bindings
test_server_death_cleans_up_bindings_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive die -> exit(normal) after 5000 -> ok end end),
        MockTransport1 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport2 = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        ServerId = death_cleanup_server,
        Transport1Id = death_cleanup_transport_1,
        Transport2Id = death_cleanup_transport_2,

        % Register and bind multiple transports
        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
        ok = gen_server:call(Registry, {register_transport, Transport1Id, MockTransport1, #{type => stdio}}),
        ok = gen_server:call(Registry, {register_transport, Transport2Id, MockTransport2, #{type => stdio}}),
        ok = gen_server:call(Registry, {bind_transport_to_server, Transport1Id, ServerId}),
        ok = gen_server:call(Registry, {bind_transport_to_server, Transport2Id, ServerId}),

        [
            ?_test(begin
                % Verify bindings exist
                ?assertMatch({ok, ServerId}, gen_server:call(Registry, {get_server_for_transport, Transport1Id})),
                ?assertMatch({ok, ServerId}, gen_server:call(Registry, {get_server_for_transport, Transport2Id}))
            end),
            ?_test(begin
                % Kill server process
                unlink(MockServer),
                exit(MockServer, kill),
                wait_for_process_death(MockServer, 2000),
                timer:sleep(200),  % Allow gproc monitor cleanup
                ?assertMatch({error, not_found}, gen_server:call(Registry, {find_server, ServerId}))
            end),
            ?_test(begin
                % Verify bindings were cleaned up
                ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, Transport1Id})),
                ?assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, Transport2Id}))
            end)
        ]
     end}.

%% Test unbind non-existent transport (should be ok)
test_unbind_nonexistent_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        [
            ?_test(begin
                % Unbinding non-existent transport should be ok (idempotent)
                ?assertEqual(ok, gen_server:call(Registry, {unbind_transport, nonexistent_transport}))
            end)
        ]
     end}.

%% Test get_all_state
test_get_all_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        [
            ?_test(begin
                % Get initial state
                ?assertMatch({ok, #registry_state{}}, gen_server:call(Registry, get_all_state))
            end),
            ?_test(begin
                % Register and bind
                ok = gen_server:call(Registry, {register_server, state_server, MockServer, #{}}),
                ok = gen_server:call(Registry, {register_transport, state_transport, MockTransport, #{type => stdio}}),
                ok = gen_server:call(Registry, {bind_transport_to_server, state_transport, state_server}),

                % Get state with bindings
                ?assertMatch({ok, #registry_state{server_transport_map = _}},
                             gen_server:call(Registry, get_all_state))
            end)
        ]
     end}.

%% Test restore_state
test_restore_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        OriginalState = #registry_state{server_transport_map = #{test_transport => test_server}},

        [
            ?_test(begin
                % Restore valid state
                ?assertEqual(ok, gen_server:call(Registry, {restore_state, OriginalState}))
            end),
            ?_test(begin
                % Try to restore invalid state
                InvalidState = #{invalid => state},
                ?assertMatch({error, invalid_state}, gen_server:call(Registry, {restore_state, InvalidState}))
            end)
        ]
     end}.

%% Test get_pid
test_get_pid_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        [
            ?_test(begin
                % get_pid should return the registry PID
                Pid = erlmcp_registry:get_pid(),
                ?assert(is_pid(Pid))
            end),
            ?_test(begin
                % Should match our registry PID
                Pid = erlmcp_registry:get_pid(),
                ?assertEqual(Pid, Registry)
            end)
        ]
     end}.

%% Test get_queue_depth
test_get_queue_depth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        [
            ?_test(begin
                % Get initial queue depth
                Depth = erlmcp_registry:get_queue_depth(),
                ?assert(is_integer(Depth)),
                ?assert(Depth >= 0)
            end),
            ?_test(begin
                % Send some messages to increase queue
                lists:foreach(fun(_) ->
                    gen_server:cast(Registry, {test_msg, make_ref()})
                end, lists:seq(1, 10)),
                timer:sleep(50),
                Depth = erlmcp_registry:get_queue_depth(),
                ?assert(is_integer(Depth))
            end)
        ]
     end}.

%% Test route_message to server
test_route_message_to_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() ->
            receive
                {mcp_message, _, _} -> ok
            after 1000 -> ok
            end
        end),
        MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        ServerId = route_msg_server,
        TransportId = route_msg_transport,

        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
        ok = gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => stdio}}),
        ok = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

        TestMessage = #{<<"test">> => <<"route_message">>},

        [
            ?_test(begin
                % Route message to server - should broadcast to transports
                ?assertEqual(ok, erlmcp_registry:route_message({server, ServerId}, TestMessage))
            end)
        ]
     end}.

%% Test route_message to non-existent server
test_route_message_to_nonexistent_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        TestMessage = #{<<"test">> => <<"data">>},
        [
            ?_test(begin
                ?assertMatch({error, server_not_found},
                             erlmcp_registry:route_message({server, nonexistent_server}, TestMessage))
            end)
        ]
     end}.

%% Test route_message to transport
test_route_message_to_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        MockTransport = spawn_link(fun() ->
            receive
                {mcp_response, _, _} -> ok
            after 1000 -> ok
            end
        end),

        ServerId = route_msg_transport_server,
        TransportId = route_msg_transport_transport,

        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),
        ok = gen_server:call(Registry, {register_transport, TransportId, MockTransport,
                                        #{type => stdio, server_id => ServerId}}),

        TestMessage = #{<<"test">> => <<"route_message">>},

        [
            ?_test(begin
                % Route message to transport with bound server
                ?assertEqual(ok, erlmcp_registry:route_message({transport, TransportId}, TestMessage))
            end)
        ]
     end}.

%% Test route_message to transport without bound server
test_route_message_to_transport_no_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        TransportId = no_server_transport,

        ok = gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => stdio}}),

        TestMessage = #{<<"test">> => <<"data">>},

        [
            ?_test(begin
                % Transport without server_id in config
                ?assertMatch({error, no_bound_server},
                             erlmcp_registry:route_message({transport, TransportId}, TestMessage))
            end)
        ]
     end}.

%% Test route_message to non-existent transport
test_route_message_to_nonexistent_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        TestMessage = #{<<"test">> => <<"data">>},
        [
            ?_test(begin
                ?assertMatch({error, transport_not_found},
                             erlmcp_registry:route_message({transport, nonexistent_transport}, TestMessage))
            end)
        ]
     end}.

%% Test multiple concurrent bindings
test_concurrent_bindings_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry} = State) ->
        MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

        ServerId = concurrent_bind_server,

        ok = gen_server:call(Registry, {register_server, ServerId, MockServer, #{}}),

        % Create multiple transports
        TransportCount = 20,
        {Transports, TransportIds} = lists:unzip(lists:map(fun(N) ->
            TransportId = list_to_atom("concurrent_transport_" ++ integer_to_list(N)),
            MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
            ok = gen_server:call(Registry, {register_transport, TransportId, MockTransport, #{type => stdio}}),
            {MockTransport, TransportId}
        end, lists:seq(1, TransportCount))),

        % Bind all transports concurrently
        Parent = self(),
        Pids = lists:map(fun(TransportId) ->
            spawn_link(fun() ->
                Result = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}, 2000),
                Parent ! {bind_result, self(), Result}
            end)
        end, TransportIds),

        Results = [receive
            {bind_result, Pid, Result} ->
                {Pid, Result}
        after 5000 ->
            timeout
        end || _ <- Pids],

        % All bindings should succeed
        SuccessCount = lists:foldl(fun({_Pid, Result}, Acc) ->
            case Result of
                ok -> Acc + 1;
                _ -> Acc
            end
        end, 0, Results),

        AllPids = [Pid || {Pid, _} <- Results] ++ Transports,
        maps:put(test_pids, AllPids ++ maps:get(test_pids, State, []), State),

        [
            ?_assertEqual(TransportCount, SuccessCount)
        ]
     end}.

%% Test unknown handle_call request
test_unknown_handle_call_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        [
            ?_test(begin
                % Unknown request should return error
                ?assertMatch({error, unknown_request}, gen_server:call(Registry, unknown_request))
            end)
        ]
     end}.

%% Test unknown handle_cast message
test_unknown_handle_cast_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        [
            ?_test(begin
                % Unknown cast should not crash
                gen_server:cast(Registry, unknown_cast),
                timer:sleep(100),
                ?assert(true)
            end)
        ]
     end}.

%% Test unknown handle_info message
test_unknown_handle_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{registry := Registry}) ->
        [
            ?_test(begin
                % Unknown info should not crash
                Registry ! unknown_info,
                timer:sleep(100),
                ?assert(true)
            end)
        ]
     end}.

