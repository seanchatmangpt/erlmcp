-module(erlmcp_registry_distributed_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Chicago School TDD - Distributed Registry Tests
%% Real gproc integration, real processes, state-based verification
%%====================================================================

%%====================================================================
%% Test Fixtures - Setup/Teardown
%%====================================================================

registry_test_() ->
    {setup,
     fun() ->
         %% Setup: Start gproc and registry (real processes)
         application:ensure_all_started(gproc),
         {ok, Pid} = erlmcp_registry:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop registry and gproc
         erlang:exit(Pid, normal),
         timer:sleep(50),
         application:stop(gproc)
     end,
     fun(_Pid) ->
         [
          ?_test(register_server_basic()),
          ?_test(register_transport_basic()),
          ?_test(register_duplicate_server_fails()),
          ?_test(unregister_server()),
          ?_test(unregister_transport()),
          ?_test(find_server()),
          ?_test(find_transport()),
          ?_test(list_servers()),
          ?_test(list_transports()),
          ?_test(bind_transport_to_server()),
          ?_test(route_to_server()),
          ?_test(route_to_transport()),
          ?_test(route_broadcast()),
          ?_test(server_process_death_cleanup()),
          ?_test(transport_process_death_cleanup()),
          ?_test(concurrent_server_registration())
         ]
     end}.

%%====================================================================
%% Test Cases - Registration (Chicago School: Real Processes)
%%====================================================================

register_server_basic() ->
    %% Setup: Spawn real server process
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ServerId = test_server_1,
    Config = #{
        capabilities => #mcp_server_capabilities{},
        options => #{test => true}
    },

    %% Exercise: Register via API
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),

    %% Verify: Can find server (state-based verification)
    {ok, {FoundPid, FoundConfig}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(ServerPid, FoundPid),
    ?assertEqual(Config, FoundConfig),

    %% Cleanup
    ServerPid ! stop.

register_transport_basic() ->
    %% Setup: Spawn real transport process
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    TransportId = test_transport_1,
    Config = #{
        type => tcp,
        server_id => test_server,
        config => #{port => 8080}
    },

    %% Exercise: Register transport
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, Config),

    %% Verify: Can find transport (observable state)
    {ok, {FoundPid, FoundConfig}} = erlmcp_registry:find_transport(TransportId),
    ?assertEqual(TransportPid, FoundPid),
    ?assertEqual(Config, FoundConfig),

    %% Cleanup
    TransportPid ! stop.

register_duplicate_server_fails() ->
    %% Setup: Spawn and register server
    ServerPid1 = spawn(fun() -> receive stop -> ok end end),
    ServerId = test_server_dup,
    Config = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server(ServerId, ServerPid1, Config),

    %% Exercise: Try to register duplicate
    ServerPid2 = spawn(fun() -> receive stop -> ok end end),
    Result = erlmcp_registry:register_server(ServerId, ServerPid2, Config),

    %% Verify: Duplicate registration fails
    ?assertEqual({error, already_registered}, Result),

    %% Cleanup
    ServerPid1 ! stop,
    ServerPid2 ! stop,
    erlmcp_registry:unregister_server(ServerId).

unregister_server() ->
    %% Setup: Register server
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ServerId = test_server_unreg,
    Config = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),

    %% Verify: Server exists
    {ok, _} = erlmcp_registry:find_server(ServerId),

    %% Exercise: Unregister
    ok = erlmcp_registry:unregister_server(ServerId),

    %% Verify: Server no longer found (state verification)
    ?assertEqual({error, not_found}, erlmcp_registry:find_server(ServerId)),

    %% Cleanup
    ServerPid ! stop.

unregister_transport() ->
    %% Setup: Register transport
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    TransportId = test_transport_unreg,
    Config = #{type => stdio},
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, Config),

    %% Verify: Transport exists
    {ok, _} = erlmcp_registry:find_transport(TransportId),

    %% Exercise: Unregister
    ok = erlmcp_registry:unregister_transport(TransportId),

    %% Verify: Transport no longer found
    ?assertEqual({error, not_found}, erlmcp_registry:find_transport(TransportId)),

    %% Cleanup
    TransportPid ! stop.

find_server() ->
    %% Setup: Register multiple servers
    ServerPid1 = spawn(fun() -> receive stop -> ok end end),
    ServerPid2 = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_server(server_find_1, ServerPid1, #{}),
    ok = erlmcp_registry:register_server(server_find_2, ServerPid2, #{}),

    %% Exercise: Find specific server
    {ok, {Pid1, _}} = erlmcp_registry:find_server(server_find_1),
    {ok, {Pid2, _}} = erlmcp_registry:find_server(server_find_2),

    %% Verify: Correct PIDs returned
    ?assertEqual(ServerPid1, Pid1),
    ?assertEqual(ServerPid2, Pid2),

    %% Verify: Non-existent server returns error
    ?assertEqual({error, not_found}, erlmcp_registry:find_server(nonexistent)),

    %% Cleanup
    ServerPid1 ! stop,
    ServerPid2 ! stop,
    erlmcp_registry:unregister_server(server_find_1),
    erlmcp_registry:unregister_server(server_find_2).

find_transport() ->
    %% Setup: Register transports
    TransportPid1 = spawn(fun() -> receive stop -> ok end end),
    TransportPid2 = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_transport(trans_find_1, TransportPid1, #{}),
    ok = erlmcp_registry:register_transport(trans_find_2, TransportPid2, #{}),

    %% Exercise: Find transports
    {ok, {Pid1, _}} = erlmcp_registry:find_transport(trans_find_1),
    {ok, {Pid2, _}} = erlmcp_registry:find_transport(trans_find_2),

    %% Verify: Correct PIDs
    ?assertEqual(TransportPid1, Pid1),
    ?assertEqual(TransportPid2, Pid2),

    %% Cleanup
    TransportPid1 ! stop,
    TransportPid2 ! stop,
    erlmcp_registry:unregister_transport(trans_find_1),
    erlmcp_registry:unregister_transport(trans_find_2).

list_servers() ->
    %% Setup: Register 3 servers
    Servers = [
        {server_list_1, spawn(fun() -> receive stop -> ok end end)},
        {server_list_2, spawn(fun() -> receive stop -> ok end end)},
        {server_list_3, spawn(fun() -> receive stop -> ok end end)}
    ],
    lists:foreach(fun({Id, Pid}) ->
        ok = erlmcp_registry:register_server(Id, Pid, #{})
    end, Servers),

    %% Exercise: List all servers
    ServerList = erlmcp_registry:list_servers(),

    %% Verify: All servers present (state verification)
    ServerIds = [Id || {Id, _} <- ServerList],
    ?assert(lists:member(server_list_1, ServerIds)),
    ?assert(lists:member(server_list_2, ServerIds)),
    ?assert(lists:member(server_list_3, ServerIds)),

    %% Cleanup
    lists:foreach(fun({Id, Pid}) ->
        Pid ! stop,
        erlmcp_registry:unregister_server(Id)
    end, Servers).

list_transports() ->
    %% Setup: Register transports
    Transports = [
        {trans_list_1, spawn(fun() -> receive stop -> ok end end)},
        {trans_list_2, spawn(fun() -> receive stop -> ok end end)}
    ],
    lists:foreach(fun({Id, Pid}) ->
        ok = erlmcp_registry:register_transport(Id, Pid, #{})
    end, Transports),

    %% Exercise: List transports
    TransportList = erlmcp_registry:list_transports(),

    %% Verify: All transports present
    TransportIds = [Id || {Id, _} <- TransportList],
    ?assert(lists:member(trans_list_1, TransportIds)),
    ?assert(lists:member(trans_list_2, TransportIds)),

    %% Cleanup
    lists:foreach(fun({Id, Pid}) ->
        Pid ! stop,
        erlmcp_registry:unregister_transport(Id)
    end, Transports).

bind_transport_to_server() ->
    %% Setup: Register server and transport
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    ServerId = server_bind,
    TransportId = transport_bind,

    ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{server_id => ServerId}),

    %% Exercise: Bind transport to server
    ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),

    %% Verify: Binding exists (observable state)
    {ok, BoundServerId} = erlmcp_registry:get_server_for_transport(TransportId),
    ?assertEqual(ServerId, BoundServerId),

    %% Cleanup
    ServerPid ! stop,
    TransportPid ! stop,
    erlmcp_registry:unregister_server(ServerId),
    erlmcp_registry:unregister_transport(TransportId).

route_to_server() ->
    %% Setup: Register server with message receiving
    Self = self(),
    ServerPid = spawn(fun() ->
        receive
            {mcp_message, TransportId, Message} ->
                Self ! {received, TransportId, Message}
        end
    end),
    ServerId = server_route,
    ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),

    %% Exercise: Route message to server (real message passing)
    TransportId = test_transport,
    Message = {test, data},
    ok = erlmcp_registry:route_to_server(ServerId, TransportId, Message),

    %% Verify: Message received (behavior verification, Chicago School)
    receive
        {received, RecvTransportId, RecvMessage} ->
            ?assertEqual(TransportId, RecvTransportId),
            ?assertEqual(Message, RecvMessage)
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    erlmcp_registry:unregister_server(ServerId).

route_to_transport() ->
    %% Setup: Register transport with message receiving
    Self = self(),
    TransportPid = spawn(fun() ->
        receive
            {mcp_response, ServerId, Message} ->
                Self ! {transport_received, ServerId, Message}
        end
    end),
    TransportId = transport_route,
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{}),

    %% Exercise: Route to transport
    ServerId = test_server,
    Message = {response, data},
    ok = erlmcp_registry:route_to_transport(TransportId, ServerId, Message),

    %% Verify: Message received
    receive
        {transport_received, RecvServerId, RecvMessage} ->
            ?assertEqual(ServerId, RecvServerId),
            ?assertEqual(Message, RecvMessage)
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    erlmcp_registry:unregister_transport(TransportId).

route_broadcast() ->
    %% Setup: Register server with 3 bound transports
    Self = self(),
    ServerId = server_broadcast,
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),

    %% Register 3 transports bound to server
    Transports = lists:map(fun(N) ->
        TransportId = list_to_atom("transport_bc_" ++ integer_to_list(N)),
        Pid = spawn(fun() ->
            receive
                {mcp_response, _ServerId, Message} ->
                    Self ! {broadcast_recv, N, Message}
            end
        end),
        ok = erlmcp_registry:register_transport(TransportId, Pid, #{server_id => ServerId}),
        ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),
        {TransportId, Pid}
    end, lists:seq(1, 3)),

    %% Exercise: Broadcast to all transports
    Message = {broadcast, test},
    ok = erlmcp_registry:route_to_transport(broadcast, ServerId, Message),

    %% Verify: All 3 transports received message (real message passing)
    Received = lists:sort([
        receive {broadcast_recv, N, _Msg} -> N after 1000 -> timeout end,
        receive {broadcast_recv, N2, _Msg2} -> N2 after 1000 -> timeout end,
        receive {broadcast_recv, N3, _Msg3} -> N3 after 1000 -> timeout end
    ]),
    ?assertEqual([1, 2, 3], Received),

    %% Cleanup
    ServerPid ! stop,
    lists:foreach(fun({Id, _Pid}) ->
        erlmcp_registry:unregister_transport(Id)
    end, Transports),
    erlmcp_registry:unregister_server(ServerId).

%%====================================================================
%% Test Cases - Process Death and Cleanup (Chicago School)
%%====================================================================

server_process_death_cleanup() ->
    %% Setup: Register server
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ServerId = server_death,
    ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),

    %% Verify: Server registered
    {ok, _} = erlmcp_registry:find_server(ServerId),

    %% Exercise: Kill server process (real process death, no mocking)
    exit(ServerPid, kill),
    timer:sleep(100),  %% Allow gproc cleanup

    %% Verify: gproc automatically cleaned up registration (observable behavior)
    %% Note: gproc handles cleanup on process death automatically
    Result = erlmcp_registry:find_server(ServerId),
    ?assertEqual({error, not_found}, Result).

transport_process_death_cleanup() ->
    %% Setup: Register transport
    TransportPid = spawn(fun() -> receive stop -> ok end end),
    TransportId = transport_death,
    ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{}),

    %% Verify: Transport registered
    {ok, _} = erlmcp_registry:find_transport(TransportId),

    %% Exercise: Kill transport (real process death)
    exit(TransportPid, kill),
    timer:sleep(100),

    %% Verify: gproc cleanup
    Result = erlmcp_registry:find_transport(TransportId),
    ?assertEqual({error, not_found}, Result).

%%====================================================================
%% Test Cases - Concurrency (Chicago School: Real Concurrent Processes)
%%====================================================================

concurrent_server_registration() ->
    %% Exercise: 50 processes register servers concurrently (real concurrency)
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            ServerId = list_to_atom("concurrent_server_" ++ integer_to_list(N)),
            ServerPid = spawn(fun() -> receive stop -> ok end end),
            Config = #{index => N},
            ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),
            receive stop -> ServerPid ! stop end
        end)
    end, lists:seq(1, 50)),

    timer:sleep(200),  %% Let registrations complete

    %% Verify: All 50 servers registered (state verification)
    ServerList = erlmcp_registry:list_servers(),
    ConcurrentServers = [Id || {Id, _} <- ServerList,
                               lists:prefix("concurrent_server_", atom_to_list(Id))],
    ?assertEqual(50, length(ConcurrentServers)),

    %% Cleanup
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids),
    lists:foreach(fun(N) ->
        ServerId = list_to_atom("concurrent_server_" ++ integer_to_list(N)),
        erlmcp_registry:unregister_server(ServerId)
    end, lists:seq(1, 50)).
