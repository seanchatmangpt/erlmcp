-module(erlmcp_registry_sharded_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

registry_sharded_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_registry_startup/1,
         fun test_server_registration_basic/1,
         fun test_transport_registration_basic/1,
         fun test_concurrent_registrations/1,
         fun test_partition_isolation/1,
         fun test_server_transport_binding/1,
         fun test_message_routing_to_server/1,
         fun test_message_routing_to_transport/1,
         fun test_list_operations/1,
         fun test_partition_statistics/1
     ]}.

setup() ->
    ok = ensure_gproc_started(),
    clear_test_registrations(),
    timer:sleep(100),
    {ok, Registry} = erlmcp_registry_sharded:start_link(16),
    #{registry => Registry, test_pids => []}.

cleanup(#{registry := Registry, test_pids := TestPids}) ->
    lists:foreach(fun(Pid) ->
        catch unlink(Pid),
        catch exit(Pid, kill)
    end, TestPids),

    catch gen_server:stop(Registry, shutdown, 5000),
    clear_test_registrations(),
    timer:sleep(100),
    ok.

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

clear_test_registrations() ->
    ok = ensure_gproc_started(),
    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    ServerEntries = gproc:select(ServerPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, server, Id}}, Pid)
    end, ServerEntries),

    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportEntries = gproc:select(TransportPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, transport, Id}}, Pid)
    end, TransportEntries),
    ok.

test_registry_startup(#{registry := _Registry}) ->
    [
        ?_assertEqual([], erlmcp_registry_sharded:list_servers()),
        ?_assertEqual([], erlmcp_registry_sharded:list_transports()),
        ?_assertMatch({error, not_found}, erlmcp_registry_sharded:find_server(unknown)),
        ?_assertMatch({error, not_found}, erlmcp_registry_sharded:find_transport(unknown))
    ].

test_server_registration_basic(#{registry := _Registry} = State) ->
    MockServer = spawn_link(fun() ->
        receive stop -> ok after 10000 -> ok end
    end),

    ServerCapabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{listChanged = true}
    },
    ServerConfig = #{capabilities => ServerCapabilities},

    Tests = [
        ?_assertEqual(ok, erlmcp_registry_sharded:register_server(test_server_1, MockServer, ServerConfig)),
        ?_assertMatch({ok, {MockServer, ServerConfig}},
                     erlmcp_registry_sharded:find_server(test_server_1)),
        ?_assertEqual(true, lists:keymember(test_server_1, 1, erlmcp_registry_sharded:list_servers())),
        ?_assertMatch({error, already_registered},
                     erlmcp_registry_sharded:register_server(test_server_1, MockServer, ServerConfig)),
        ?_assertEqual(ok, erlmcp_registry_sharded:unregister_server(test_server_1)),
        ?_assertMatch({error, not_found}, erlmcp_registry_sharded:find_server(test_server_1))
    ],

    maps:put(test_pids, [MockServer | maps:get(test_pids, State, [])], State),
    Tests.

test_transport_registration_basic(#{registry := _Registry} = State) ->
    MockTransport = spawn_link(fun() ->
        receive stop -> ok after 10000 -> ok end
    end),

    TransportConfig = #{type => stdio, server_id => test_server_trans},

    Tests = [
        ?_assertEqual(ok, erlmcp_registry_sharded:register_transport(test_transport_1, MockTransport, TransportConfig)),
        ?_assertMatch({ok, {MockTransport, TransportConfig}},
                     erlmcp_registry_sharded:find_transport(test_transport_1)),
        ?_assertEqual(true, lists:keymember(test_transport_1, 1, erlmcp_registry_sharded:list_transports())),
        ?_assertEqual(ok, erlmcp_registry_sharded:unregister_transport(test_transport_1)),
        ?_assertMatch({error, not_found}, erlmcp_registry_sharded:find_transport(test_transport_1))
    ],

    maps:put(test_pids, [MockTransport | maps:get(test_pids, State, [])], State),
    Tests.

test_concurrent_registrations(#{registry := _Registry} = State) ->
    NumProcesses = 100,
    Parent = self(),

    Pids = lists:map(fun(N) ->
        spawn_link(fun() ->
            Pid = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
            ServerId = list_to_binary("concurrent_server_" ++ integer_to_list(N)),
            Result = erlmcp_registry_sharded:register_server(ServerId, Pid, #{}),
            Parent ! {registered, N, Result},
            receive stop -> ok after 10000 -> ok end
        end)
    end, lists:seq(1, NumProcesses)),

    Results = lists:map(fun(_) ->
        receive {registered, N, R} -> {N, R} end
    end, lists:seq(1, NumProcesses)),

    AllOk = lists:all(fun({_, R}) -> R =:= ok end, Results),
    RegisteredCount = length(erlmcp_registry_sharded:list_servers()),

    lists:foreach(fun(Pid) ->
        unlink(Pid),
        exit(Pid, kill)
    end, Pids),

    [
        ?_assert(AllOk),
        ?_assertEqual(NumProcesses, RegisteredCount)
    ].

test_partition_isolation(#{registry := _Registry} = State) ->
    Servers = lists:map(fun(N) ->
        Pid = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        ServerId = list_to_binary("partition_test_" ++ integer_to_list(N)),
        erlmcp_registry_sharded:register_server(ServerId, Pid, #{data => N}),
        {ServerId, Pid, N}
    end, lists:seq(1, 16)),

    FoundAll = lists:all(fun({ServerId, _, N}) ->
        case erlmcp_registry_sharded:find_server(ServerId) of
            {ok, {_, Config}} ->
                maps:get(data, Config, undefined) =:= N;
            {error, _} ->
                false
        end
    end, Servers),

    lists:foreach(fun({ServerId, Pid, _}) ->
        erlmcp_registry_sharded:unregister_server(ServerId),
        unlink(Pid),
        exit(Pid, kill)
    end, Servers),

    [?_assert(FoundAll)].

test_server_transport_binding(#{registry := _Registry} = State) ->
    MockServer = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
    MockTransport = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),

    erlmcp_registry_sharded:register_server(test_server_bind, MockServer, #{}),
    erlmcp_registry_sharded:register_transport(test_transport_bind, MockTransport, #{}),

    Tests = [
        ?_assertEqual(ok, erlmcp_registry_sharded:bind_transport_to_server(test_transport_bind, test_server_bind)),
        ?_assertMatch({ok, test_server_bind}, erlmcp_registry_sharded:get_server_for_transport(test_transport_bind)),
        ?_assertEqual(ok, erlmcp_registry_sharded:unbind_transport(test_transport_bind)),
        ?_assertMatch({error, not_found}, erlmcp_registry_sharded:get_server_for_transport(test_transport_bind))
    ],

    erlmcp_registry_sharded:unregister_server(test_server_bind),
    erlmcp_registry_sharded:unregister_transport(test_transport_bind),
    unlink(MockServer), exit(MockServer, kill),
    unlink(MockTransport), exit(MockTransport, kill),

    Tests.

test_message_routing_to_server(#{registry := _Registry} = State) ->
    Collector = spawn_link(fun() -> collect_messages([]) end),

    MockServer = spawn_link(fun() ->
        receive
            {mcp_message, TransportId, Message} ->
                Collector ! {server_received, TransportId, Message}
        after 5000 -> ok
        end
    end),

    MockTransport = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

    erlmcp_registry_sharded:register_server(test_server_route, MockServer, #{}),
    erlmcp_registry_sharded:register_transport(test_transport_route, MockTransport, #{}),
    erlmcp_registry_sharded:bind_transport_to_server(test_transport_route, test_server_route),

    TestMessage = #{<<"method">> => <<"test">>},
    erlmcp_registry_sharded:route_to_server(test_server_route, test_transport_route, TestMessage),

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

    erlmcp_registry_sharded:unregister_server(test_server_route),
    erlmcp_registry_sharded:unregister_transport(test_transport_route),

    maps:put(test_pids, [Collector, MockServer, MockTransport | maps:get(test_pids, State, [])], State),
    [Test].

test_message_routing_to_transport(#{registry := _Registry} = State) ->
    Collector = spawn_link(fun() -> collect_messages([]) end),

    MockServer = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),

    MockTransport = spawn_link(fun() ->
        receive
            {mcp_response, ServerId, Message} ->
                Collector ! {transport_received, ServerId, Message}
        after 5000 -> ok
        end
    end),

    erlmcp_registry_sharded:register_server(test_server_route2, MockServer, #{}),
    erlmcp_registry_sharded:register_transport(test_transport_route2, MockTransport, #{}),
    erlmcp_registry_sharded:bind_transport_to_server(test_transport_route2, test_server_route2),

    TestResponse = #{<<"result">> => <<"success">>},
    erlmcp_registry_sharded:route_to_transport(test_transport_route2, test_server_route2, TestResponse),

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

    erlmcp_registry_sharded:unregister_server(test_server_route2),
    erlmcp_registry_sharded:unregister_transport(test_transport_route2),

    maps:put(test_pids, [Collector, MockServer, MockTransport | maps:get(test_pids, State, [])], State),
    [Test].

test_list_operations(#{registry := _Registry} = State) ->
    Servers = lists:map(fun(N) ->
        ServerId = list_to_atom("list_server_" ++ integer_to_list(N)),
        Pid = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        erlmcp_registry_sharded:register_server(ServerId, Pid, #{}),
        {ServerId, Pid}
    end, lists:seq(1, 5)),

    Transports = lists:map(fun(N) ->
        TransportId = list_to_atom("list_transport_" ++ integer_to_list(N)),
        Pid = spawn_link(fun() -> receive stop -> ok after 5000 -> ok end end),
        erlmcp_registry_sharded:register_transport(TransportId, Pid, #{}),
        {TransportId, Pid}
    end, lists:seq(1, 5)),

    ServerList = erlmcp_registry_sharded:list_servers(),
    TransportList = erlmcp_registry_sharded:list_transports(),

    Tests = [
        ?_assertEqual(5, length(ServerList)),
        ?_assertEqual(5, length(TransportList))
    ],

    lists:foreach(fun({ServerId, Pid}) ->
        erlmcp_registry_sharded:unregister_server(ServerId),
        unlink(Pid),
        exit(Pid, kill)
    end, Servers),

    lists:foreach(fun({TransportId, Pid}) ->
        erlmcp_registry_sharded:unregister_transport(TransportId),
        unlink(Pid),
        exit(Pid, kill)
    end, Transports),

    AllPids = [P || {_, P} <- Servers ++ Transports],
    maps:put(test_pids, AllPids ++ maps:get(test_pids, State, []), State),
    Tests.

test_partition_statistics(#{registry := _Registry} = State) ->
    MockServers = lists:map(fun(N) ->
        Pid = spawn_link(fun() -> receive stop -> ok after 10000 -> ok end end),
        ServerId = list_to_binary("stats_server_" ++ integer_to_list(N)),
        erlmcp_registry_sharded:register_server(ServerId, Pid, #{}),
        {ServerId, Pid}
    end, lists:seq(1, 16)),

    Stats = erlmcp_registry_sharded:get_partition_stats(),

    StatsValid = lists:all(fun({PartId, PartStats}) ->
        is_map(PartStats) andalso
        maps:get(partition_id, PartStats, undefined) =:= PartId andalso
        maps:get(write_count, PartStats, undefined) >= 0
    end, maps:to_list(Stats)),

    lists:foreach(fun({ServerId, Pid}) ->
        erlmcp_registry_sharded:unregister_server(ServerId),
        unlink(Pid),
        exit(Pid, kill)
    end, MockServers),

    [?_assert(StatsValid)].

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
