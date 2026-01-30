-module(erlmcp_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

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

        % Test duplicate registration fails
        ?_assertMatch({error, already_registered},
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

    % Register both
    gen_server:call(Registry, {register_server, test_server_bind, MockServer, #{}}),
    gen_server:call(Registry, {register_transport, test_transport_bind, MockTransport, #{type => stdio}}),

    Tests = [
        % Test binding
        ?_assertEqual(ok, gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, test_server_bind})),
        ?_assertMatch({ok, test_server_bind}, gen_server:call(Registry, {get_server_for_transport, test_transport_bind})),

        % Test unbinding
        ?_assertEqual(ok, gen_server:call(Registry, {unbind_transport, test_transport_bind})),
        ?_assertMatch({error, not_found}, gen_server:call(Registry, {get_server_for_transport, test_transport_bind})),

        % Test binding nonexistent fails
        ?_assertMatch({error, server_not_found},
                     gen_server:call(Registry, {bind_transport_to_server, test_transport_bind, nonexistent})),
        ?_assertMatch({error, transport_not_found},
                     gen_server:call(Registry, {bind_transport_to_server, nonexistent, test_server_bind}))
    ],

    % Cleanup
    gen_server:call(Registry, {unregister_server, test_server_bind}),
    gen_server:call(Registry, {unregister_transport, test_transport_bind}),

    maps:put(test_pids, [MockServer, MockTransport | maps:get(test_pids, State, [])], State),
    Tests.

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
    gen_server:call(Registry, {register_server, test_server_route, MockServer, #{}}),
    gen_server:call(Registry, {register_transport, test_transport_route, MockTransport, #{type => stdio}}),
    gen_server:call(Registry, {bind_transport_to_server, test_transport_route, test_server_route}),

    % Route message
    TestMessage = #{<<"method">> => <<"test">>},
    gen_server:cast(Registry, {route_to_server, test_server_route, test_transport_route, TestMessage}),

    % Verify
    timer:sleep(200),
    Collector ! {get_messages, self()},

    Test = ?_assert(begin
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
    gen_server:call(Registry, {register_server, test_server_route2, MockServer, #{}}),
    gen_server:call(Registry, {register_transport, test_transport_route2, MockTransport, #{type => stdio}}),
    gen_server:call(Registry, {bind_transport_to_server, test_transport_route2, test_server_route2}),

    % Route message
    TestResponse = #{<<"result">> => <<"success">>},
    gen_server:cast(Registry, {route_to_transport, test_transport_route2, test_server_route2, TestResponse}),

    % Verify
    timer:sleep(200),
    Collector ! {get_messages, self()},

    Test = ?_assert(begin
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

    % Should have exactly one successful registration or all idempotent retries
    Test = ?_assertEqual(ConcurrentCount, SuccessCount),

    % Cleanup
    gen_server:call(Registry, {unregister_server, ServerId}),

    AllPids = [Pid || {Pid, _} <- Results],
    maps:put(test_pids, AllPids ++ maps:get(test_pids, State, []), State),
    [Test].

