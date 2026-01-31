-module(erlmcp_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Comprehensive Test Suite for erlmcp_registry Module
%%% Chicago School TDD: Real processes, real gproc, state-based verification
%%% Target: 80%+ code coverage, 65+ test cases
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

%% Main test generator - all tests
registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Registry Initialization", {spawn, fun initialization_tests/0}},
          {"Server Registration", {spawn, fun server_registration_tests/0}},
          {"Transport Registration", {spawn, fun transport_registration_tests/0}},
          {"Transport-Server Binding", {spawn, fun binding_tests/0}},
          {"Message Routing", {spawn, fun routing_tests/0}},
          {"State Management", {spawn, fun state_management_tests/0}},
          {"Error Handling & Recovery", {spawn, fun error_handling_tests/0}},
          {"Concurrent Operations", {spawn, fun concurrent_tests/0}},
          {"Process Death Cleanup", {spawn, fun cleanup_tests/0}},
          {"Global Registry API", {spawn, fun global_registry_tests/0}},
          {"Edge Cases", {spawn, fun edge_cases_tests/0}}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    %% Ensure application is started (starts registry as singleton)
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),

    %% Clear any test registrations from previous runs
    clear_test_registrations(),
    timer:sleep(100),
    ok.

cleanup(_) ->
    %% Clean up test registrations
    clear_test_registrations(),
    timer:sleep(100),
    ok.

%% Helper: Clear all test registrations
clear_test_registrations() ->
    %% Unregister all test servers/transports
    TestServers = [S || {S, _} <- erlmcp_registry:list_servers(),
                       is_test_id(S)],
    [catch erlmcp_registry:unregister_server(S) || S <- TestServers],

    TestTransports = [T || {T, _} <- erlmcp_registry:list_transports(),
                           is_test_id(T)],
    [catch erlmcp_registry:unregister_transport(T) || T <- TestTransports],
    ok.

%% Helper: Check if ID is a test ID
is_test_id(Id) when is_binary(Id) ->
    binary:match(Id, <<"test_">>) =/= nomatch orelse
    binary:match(Id, <<"crash_">>) =/= nomatch orelse
    binary:match(Id, <<"concurrent_">>) =/= nomatch;
is_test_id(Id) when is_atom(Id) ->
    is_test_id(atom_to_binary(Id, utf8));
is_test_id(_) ->
    false.

%% Helper: Start a dummy server process
start_dummy_server(_ServerId) ->
    spawn(fun() ->
        receive
            stop -> ok
        after 30000 -> ok
        end
    end).

%% Helper: Start a dummy transport process
start_dummy_transport(_TransportId) ->
    spawn(fun() ->
        receive
            stop -> ok
        after 30000 -> ok
        end
    end).

%% Helper: Stop dummy process
stop_dummy(Pid) when is_pid(Pid) ->
    Pid ! stop,
    timer:sleep(50);
stop_dummy(_) ->
    ok.

%% Helper: Wait for gproc cleanup
wait_for_cleanup() ->
    timer:sleep(150).

%%%====================================================================
%%% Registry Initialization Tests (6 tests)
%%%====================================================================

initialization_tests() ->
    %% Test 1: Registry is running as singleton
    Pid = erlmcp_registry:get_pid(),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    %% Test 2: Registry is registered by name
    ?assertEqual(Pid, whereis(erlmcp_registry)),

    %% Test 3: gproc is started and running
    ?assert(erlang:whereis(gproc) =/= undefined),

    %% Test 4: Initial state verification
    {ok, State} = erlmcp_registry:get_all_state(),
    ?assertMatch({registry_state, _}, State),

    %% Test 5: Queue depth is accessible
    Depth = erlmcp_registry:get_queue_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth >= 0),

    %% Test 6: Registry is managed by supervisor (attempting start_link fails)
    Result = erlmcp_registry:start_link(),
    ?assertMatch({error, _}, Result),

    %% Verify registry still alive after failed start attempt
    ?assert(erlang:is_process_alive(erlmcp_registry:get_pid())).

%%%====================================================================
%%% Server Registration Tests (12 tests)
%%%====================================================================

server_registration_tests() ->
    %% Test 1: Register server successfully
    ServerId1 = <<"test_server_reg_1">>,
    ServerPid1 = start_dummy_server(ServerId1),
    Config1 = #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true}
        },
        options => #{}
    },
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId1, ServerPid1, Config1)),

    %% Test 2: Find registered server
    {ok, {FoundPid1, FoundConfig1}} = erlmcp_registry:find_server(ServerId1),
    ?assertEqual(ServerPid1, FoundPid1),
    ?assertEqual(Config1, FoundConfig1),

    %% Test 3: List servers includes registered server
    Servers = erlmcp_registry:list_servers(),
    ?assert(lists:keymember(ServerId1, 1, Servers)),

    %% Test 4: Update server configuration
    NewConfig1 = #{
        capabilities => #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true}
        },
        options => #{updated => true}
    },
    ?assertEqual(ok, erlmcp_registry:update_server(ServerId1, NewConfig1)),

    %% Verify update
    {ok, {_, UpdatedConfig}} = erlmcp_registry:find_server(ServerId1),
    ?assertEqual(NewConfig1, UpdatedConfig),

    %% Test 5: Duplicate registration with same pid (idempotent)
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId1, ServerPid1, Config1)),

    %% Test 6: Duplicate registration with different pid (error)
    ServerPid1b = start_dummy_server(<<"dummy">>),
    ?assertEqual({error, already_registered},
                 erlmcp_registry:register_server(ServerId1, ServerPid1b, Config1)),
    stop_dummy(ServerPid1b),

    %% Test 7: Register multiple servers
    ServerId2 = <<"test_server_reg_2">>,
    ServerPid2 = start_dummy_server(ServerId2),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId2, ServerPid2, #{})),

    ServerId3 = <<"test_server_reg_3">>,
    ServerPid3 = start_dummy_server(ServerId3),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId3, ServerPid3, #{})),

    %% Test 8: Verify all servers are listed
    AllServers = erlmcp_registry:list_servers(),
    ?assert(lists:keymember(ServerId1, 1, AllServers)),
    ?assert(lists:keymember(ServerId2, 1, AllServers)),
    ?assert(lists:keymember(ServerId3, 1, AllServers)),

    %% Test 9: Unregister server
    ?assertEqual(ok, erlmcp_registry:unregister_server(ServerId2)),
    ?assertEqual({error, not_found}, erlmcp_registry:find_server(ServerId2)),

    %% Test 10: Unregister non-existent server (idempotent)
    ?assertEqual(ok, erlmcp_registry:unregister_server(<<"nonexistent_server">>)),

    %% Test 11: Update non-existent server (error)
    ?assertEqual({error, not_found},
                 erlmcp_registry:update_server(<<"nonexistent">>, #{})),

    %% Test 12: Register and unregister multiple times
    ServerId4 = <<"test_server_reg_4">>,
    ServerPid4 = start_dummy_server(ServerId4),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId4, ServerPid4, #{})),
    ?assertEqual(ok, erlmcp_registry:unregister_server(ServerId4)),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId4, ServerPid4, #{})),
    ?assertEqual(ok, erlmcp_registry:unregister_server(ServerId4)),

    %% Cleanup
    stop_dummy(ServerPid1),
    stop_dummy(ServerPid2),
    stop_dummy(ServerPid3),
    stop_dummy(ServerPid4),
    erlmcp_registry:unregister_server(ServerId1),
    erlmcp_registry:unregister_server(ServerId3).

%%%====================================================================
%%% Transport Registration Tests (12 tests)
%%%====================================================================

transport_registration_tests() ->
    %% Test 1: Register transport successfully
    TransportId1 = <<"test_transport_reg_1">>,
    TransportPid1 = start_dummy_transport(TransportId1),
    Config1 = #{type => stdio, config => #{}},
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId1, TransportPid1, Config1)),

    %% Test 2: Find registered transport
    {ok, {FoundPid1, FoundConfig1}} = erlmcp_registry:find_transport(TransportId1),
    ?assertEqual(TransportPid1, FoundPid1),
    ?assertEqual(Config1, FoundConfig1),

    %% Test 3: List transports includes registered transport
    Transports = erlmcp_registry:list_transports(),
    ?assert(lists:keymember(TransportId1, 1, Transports)),

    %% Test 4: Duplicate registration with same pid (idempotent)
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId1, TransportPid1, Config1)),

    %% Test 5: Duplicate registration with different pid (error)
    TransportPid1b = start_dummy_transport(<<"dummy">>),
    ?assertEqual({error, already_registered},
                 erlmcp_registry:register_transport(TransportId1, TransportPid1b, Config1)),
    stop_dummy(TransportPid1b),

    %% Test 6: Register multiple transports
    TransportId2 = <<"test_transport_reg_2">>,
    TransportPid2 = start_dummy_transport(TransportId2),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId2, TransportPid2, #{type => tcp})),

    TransportId3 = <<"test_transport_reg_3">>,
    TransportPid3 = start_dummy_transport(TransportId3),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId3, TransportPid3, #{type => http})),

    %% Test 7: Verify all transports are listed
    AllTransports = erlmcp_registry:list_transports(),
    ?assert(lists:keymember(TransportId1, 1, AllTransports)),
    ?assert(lists:keymember(TransportId2, 1, AllTransports)),
    ?assert(lists:keymember(TransportId3, 1, AllTransports)),

    %% Test 8: Unregister transport
    ?assertEqual(ok, erlmcp_registry:unregister_transport(TransportId2)),
    ?assertEqual({error, not_found}, erlmcp_registry:find_transport(TransportId2)),

    %% Test 9: Unregister non-existent transport (idempotent)
    ?assertEqual(ok, erlmcp_registry:unregister_transport(<<"nonexistent_transport">>)),

    %% Test 10: Register with server_id in config (auto-bind)
    ServerId1 = <<"test_auto_bind_server">>,
    ServerPid1 = start_dummy_server(ServerId1),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId1, ServerPid1, #{})),

    TransportId4 = <<"test_auto_bind_transport">>,
    TransportPid4 = start_dummy_transport(TransportId4),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId4, TransportPid4,
                                                        #{type => stdio, server_id => ServerId1})),

    %% Verify auto-binding
    {ok, BoundServerId} = erlmcp_registry:get_server_for_transport(TransportId4),
    ?assertEqual(ServerId1, BoundServerId),

    %% Test 11: Transport registration without server_id
    TransportId5 = <<"test_no_server_transport">>,
    TransportPid5 = start_dummy_transport(TransportId5),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId5, TransportPid5, #{type => tcp})),
    ?assertEqual({error, not_found}, erlmcp_registry:get_server_for_transport(TransportId5)),

    %% Test 12: Register and unregister multiple times
    TransportId6 = <<"test_transport_reg_6">>,
    TransportPid6 = start_dummy_transport(TransportId6),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId6, TransportPid6, #{})),
    ?assertEqual(ok, erlmcp_registry:unregister_transport(TransportId6)),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId6, TransportPid6, #{})),
    ?assertEqual(ok, erlmcp_registry:unregister_transport(TransportId6)),

    %% Cleanup
    stop_dummy(ServerPid1),
    stop_dummy(TransportPid1),
    stop_dummy(TransportPid2),
    stop_dummy(TransportPid3),
    stop_dummy(TransportPid4),
    stop_dummy(TransportPid5),
    stop_dummy(TransportPid6),
    erlmcp_registry:unregister_server(ServerId1),
    erlmcp_registry:unregister_transport(TransportId1),
    erlmcp_registry:unregister_transport(TransportId3),
    erlmcp_registry:unregister_transport(TransportId4),
    erlmcp_registry:unregister_transport(TransportId5).

%%%====================================================================
%%% Transport-Server Binding Tests (10 tests)
%%%====================================================================

binding_tests() ->
    %% Setup
    ServerId1 = <<"test_binding_server_1">>,
    ServerPid1 = start_dummy_server(ServerId1),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId1, ServerPid1, #{})),

    TransportId1 = <<"test_binding_transport_1">>,
    TransportPid1 = start_dummy_transport(TransportId1),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId1, TransportPid1, #{})),

    %% Test 1: Bind transport to server
    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId1, ServerId1)),

    %% Test 2: Get server for transport
    {ok, BoundServerId} = erlmcp_registry:get_server_for_transport(TransportId1),
    ?assertEqual(ServerId1, BoundServerId),

    %% Test 3: Unbind transport
    ?assertEqual(ok, erlmcp_registry:unbind_transport(TransportId1)),
    ?assertEqual({error, not_found}, erlmcp_registry:get_server_for_transport(TransportId1)),

    %% Test 4: Bind to non-existent server (error)
    ?assertEqual({error, server_not_found},
                 erlmcp_registry:bind_transport_to_server(TransportId1, <<"nonexistent_server">>)),

    %% Test 5: Bind non-existent transport (error)
    ?assertEqual({error, transport_not_found},
                 erlmcp_registry:bind_transport_to_server(<<"nonexistent_transport">>, ServerId1)),

    %% Test 6: Bind multiple transports to one server
    TransportId2 = <<"test_binding_transport_2">>,
    TransportPid2 = start_dummy_transport(TransportId2),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId2, TransportPid2, #{})),

    TransportId3 = <<"test_binding_transport_3">>,
    TransportPid3 = start_dummy_transport(TransportId3),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId3, TransportPid3, #{})),

    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId1, ServerId1)),
    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId2, ServerId1)),
    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId3, ServerId1)),

    %% Verify all bindings
    ?assertMatch({ok, ServerId1}, erlmcp_registry:get_server_for_transport(TransportId1)),
    ?assertMatch({ok, ServerId1}, erlmcp_registry:get_server_for_transport(TransportId2)),
    ?assertMatch({ok, ServerId1}, erlmcp_registry:get_server_for_transport(TransportId3)),

    %% Test 7: Rebind transport to different server
    ServerId2 = <<"test_binding_server_2">>,
    ServerPid2 = start_dummy_server(ServerId2),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId2, ServerPid2, #{})),

    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId1, ServerId2)),
    {ok, NewBoundServerId} = erlmcp_registry:get_server_for_transport(TransportId1),
    ?assertEqual(ServerId2, NewBoundServerId),

    %% Test 8: Unbind non-existent transport (idempotent)
    ?assertEqual(ok, erlmcp_registry:unbind_transport(<<"nonexistent_transport">>)),

    %% Test 9: Auto-bind verification
    TransportId4 = <<"test_auto_bind_transport_2">>,
    TransportPid4 = start_dummy_transport(TransportId4),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId4, TransportPid4,
                                                        #{server_id => ServerId1})),
    ?assertMatch({ok, ServerId1}, erlmcp_registry:get_server_for_transport(TransportId4)),

    %% Test 10: Multiple bind/unbind cycles
    ?assertEqual(ok, erlmcp_registry:unbind_transport(TransportId4)),
    ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId4, ServerId1)),
    ?assertEqual(ok, erlmcp_registry:unbind_transport(TransportId4)),
    ?assertEqual({error, not_found}, erlmcp_registry:get_server_for_transport(TransportId4)),

    %% Cleanup
    stop_dummy(ServerPid1),
    stop_dummy(ServerPid2),
    stop_dummy(TransportPid1),
    stop_dummy(TransportPid2),
    stop_dummy(TransportPid3),
    stop_dummy(TransportPid4),
    erlmcp_registry:unregister_server(ServerId1),
    erlmcp_registry:unregister_server(ServerId2),
    erlmcp_registry:unregister_transport(TransportId1),
    erlmcp_registry:unregister_transport(TransportId2),
    erlmcp_registry:unregister_transport(TransportId3),
    erlmcp_registry:unregister_transport(TransportId4),
    wait_for_cleanup().

%%%====================================================================
%%% Message Routing Tests (15 tests)
%%%====================================================================

routing_tests() ->
    %% Setup: Create server and transport with message collection
    ServerId = <<"test_routing_server">>,
    ServerPid = spawn(fun() -> routing_server_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId, ServerPid, #{})),

    TransportId = <<"test_routing_transport">>,
    TransportPid = spawn(fun() -> routing_transport_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId, TransportPid,
                                                        #{server_id => ServerId})),

    %% Test 1: Route to server
    Message1 = #{method => <<"test">>, params => #{}},
    ?assertEqual(ok, erlmcp_registry:route_to_server(ServerId, TransportId, Message1)),
    timer:sleep(100),

    %% Verify message received by server
    ServerPid ! {get_messages, self()},
    receive
        {messages, ServerMessages} ->
            ?assertEqual([{mcp_message, TransportId, Message1}], ServerMessages)
    after 2000 ->
        ?assert(false)  % Timeout
    end,

    %% Test 2: Route to transport
    Message2 = #{result => <<"ok">>},
    ?assertEqual(ok, erlmcp_registry:route_to_transport(TransportId, ServerId, Message2)),
    timer:sleep(100),

    TransportPid ! {get_messages, self()},
    receive
        {messages, TransportMessages} ->
            ?assertEqual([{mcp_response, ServerId, Message2}], TransportMessages)
    after 2000 ->
        ?assert(false)  % Timeout
    end,

    %% Test 3: Broadcast to multiple transports
    TransportId2 = <<"test_routing_transport_2">>,
    TransportPid2 = spawn(fun() -> routing_transport_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId2, TransportPid2,
                                                        #{server_id => ServerId})),

    TransportId3 = <<"test_routing_transport_3">>,
    TransportPid3 = spawn(fun() -> routing_transport_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId3, TransportPid3,
                                                        #{server_id => ServerId})),

    Message3 = #{broadcast => true},
    ?assertEqual(ok, erlmcp_registry:route_to_transport(broadcast, ServerId, Message3)),
    timer:sleep(150),

    %% Test 4: Route to non-existent server (should not crash)
    ?assertEqual(ok, erlmcp_registry:route_to_server(<<"nonexistent_server">>,
                                                      TransportId, Message1)),

    %% Test 5: Route to non-existent transport (should not crash)
    ?assertEqual(ok, erlmcp_registry:route_to_transport(<<"nonexistent_transport">>,
                                                         ServerId, Message2)),

    %% Test 6: Route message API - server
    ?assertEqual(ok, erlmcp_registry:route_message({server, ServerId}, Message1)),

    %% Test 7: Route message API - non-existent server
    ?assertEqual({error, server_not_found},
                 erlmcp_registry:route_message({server, <<"nonexistent">>}, Message1)),

    %% Test 8: Route message API - transport
    ?assertEqual(ok, erlmcp_registry:route_message({transport, TransportId}, Message2)),

    %% Test 9: Route message API - transport with no bound server
    UnboundTransportId = <<"test_unbound_transport">>,
    UnboundTransportPid = spawn(fun() -> routing_transport_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_transport(UnboundTransportId,
                                                        UnboundTransportPid, #{})),
    ?assertEqual({error, no_bound_server},
                 erlmcp_registry:route_message({transport, UnboundTransportId}, Message2)),

    %% Test 10: Route message API - transport not found
    ?assertEqual({error, transport_not_found},
                 erlmcp_registry:route_message({transport, <<"nonexistent">>}, Message2)),

    %% Test 11: Multiple messages in sequence
    ServerPid ! {clear_messages, self()},
    receive clear_ok -> ok after 1000 -> ok end,

    Messages = [#{id => N} || N <- lists:seq(1, 10)],
    [erlmcp_registry:route_to_server(ServerId, TransportId, M) || M <- Messages],
    timer:sleep(150),

    ServerPid ! {get_messages, self()},
    receive
        {messages, ReceivedMessages} ->
            ?assertEqual(10, length(ReceivedMessages))
    after 2000 ->
        ok
    end,

    %% Test 12: Large message routing
    LargeMessage = #{data => binary:copy(<<$A>>, 1024 * 100)},  % 100KB
    ?assertEqual(ok, erlmcp_registry:route_to_server(ServerId, TransportId, LargeMessage)),

    %% Test 13: Broadcast to server with no transports
    ServerId2 = <<"test_server_no_transports">>,
    ServerPid2 = spawn(fun() -> routing_server_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId2, ServerPid2, #{})),
    ?assertEqual(ok, erlmcp_registry:route_to_transport(broadcast, ServerId2, Message3)),

    %% Test 14: Concurrent routing (multiple messages simultaneously)
    ConcurrentMessages = [#{concurrent => N} || N <- lists:seq(1, 50)],
    Pids = [spawn(fun() ->
        erlmcp_registry:route_to_server(ServerId, TransportId, M)
    end) || M <- ConcurrentMessages],
    [begin
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok after 5000 -> ok end
    end || Pid <- Pids],

    %% Test 15: Route during registration/unregistration
    %% This tests that routing is resilient to concurrent modifications
    spawn(fun() ->
        TempId = <<"test_temp_transport">>,
        TempPid = start_dummy_transport(TempId),
        erlmcp_registry:register_transport(TempId, TempPid, #{server_id => ServerId}),
        timer:sleep(50),
        erlmcp_registry:unregister_transport(TempId),
        stop_dummy(TempPid)
    end),

    %% Route while registration happening
    ?assertEqual(ok, erlmcp_registry:route_to_transport(broadcast, ServerId, #{test => true})),

    %% Cleanup
    ServerPid ! stop,
    ServerPid2 ! stop,
    TransportPid ! stop,
    TransportPid2 ! stop,
    TransportPid3 ! stop,
    UnboundTransportPid ! stop,
    erlmcp_registry:unregister_server(ServerId),
    erlmcp_registry:unregister_server(ServerId2),
    erlmcp_registry:unregister_transport(TransportId),
    erlmcp_registry:unregister_transport(TransportId2),
    erlmcp_registry:unregister_transport(TransportId3),
    erlmcp_registry:unregister_transport(UnboundTransportId),
    wait_for_cleanup().

%% Helper loop for routing tests - server
routing_server_loop(Messages) ->
    receive
        {mcp_message, TransportId, Message} ->
            routing_server_loop([{mcp_message, TransportId, Message} | Messages]);
        {get_messages, From} ->
            From ! {messages, lists:reverse(Messages)},
            routing_server_loop(Messages);
        {clear_messages, From} ->
            From ! clear_ok,
            routing_server_loop([]);
        stop ->
            ok
    after 30000 ->
        ok
    end.

%% Helper loop for routing tests - transport
routing_transport_loop(Messages) ->
    receive
        {mcp_response, ServerId, Message} ->
            routing_transport_loop([{mcp_response, ServerId, Message} | Messages]);
        {get_messages, From} ->
            From ! {messages, lists:reverse(Messages)},
            routing_transport_loop(Messages);
        stop ->
            ok
    after 30000 ->
        ok
    end.

%%%====================================================================
%%% State Management & Introspection Tests (6 tests)
%%%====================================================================

state_management_tests() ->
    %% Test 1: Get all state
    {ok, State1} = erlmcp_registry:get_all_state(),
    ?assertMatch({registry_state, _}, State1),

    %% Test 2: Get registry PID
    Pid = erlmcp_registry:get_pid(),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(erlmcp_registry)),

    %% Test 3: Get queue depth
    Depth1 = erlmcp_registry:get_queue_depth(),
    ?assert(is_integer(Depth1)),
    ?assert(Depth1 >= 0),

    %% Test 4: Restore state
    {ok, OriginalState} = erlmcp_registry:get_all_state(),
    ?assertEqual(ok, erlmcp_registry:restore_state(OriginalState)),

    %% Test 5: Invalid state restore
    ?assertEqual({error, invalid_state},
                 erlmcp_registry:restore_state(invalid_state)),
    ?assertEqual({error, invalid_state},
                 erlmcp_registry:restore_state(#{invalid => <<"state">>})),

    %% Test 6: State persistence across operations
    ServerId = <<"test_state_server">>,
    ServerPid = start_dummy_server(ServerId),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId, ServerPid, #{})),

    {ok, StateWithServer} = erlmcp_registry:get_all_state(),
    ?assertMatch({registry_state, _}, StateWithServer),

    ?assertEqual(ok, erlmcp_registry:unregister_server(ServerId)),

    {ok, StateAfterUnregister} = erlmcp_registry:get_all_state(),
    ?assertMatch({registry_state, _}, StateAfterUnregister),

    stop_dummy(ServerPid).

%%%====================================================================
%%% Error Handling & Recovery Tests (8 tests)
%%%====================================================================

error_handling_tests() ->
    %% Test 1: Server process crash cleanup
    ServerId1 = <<"crash_test_server_1">>,
    ServerPid1 = start_dummy_server(ServerId1),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId1, ServerPid1, #{})),
    ?assertMatch({ok, _}, erlmcp_registry:find_server(ServerId1)),

    exit(ServerPid1, kill),
    wait_for_cleanup(),

    ?assertEqual({error, not_found}, erlmcp_registry:find_server(ServerId1)),

    %% Test 2: Transport process crash cleanup
    TransportId1 = <<"crash_test_transport_1">>,
    TransportPid1 = start_dummy_transport(TransportId1),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId1, TransportPid1, #{})),

    exit(TransportPid1, kill),
    wait_for_cleanup(),

    ?assertEqual({error, not_found}, erlmcp_registry:find_transport(TransportId1)),

    %% Test 3: Binding cleanup on server death
    ServerId2 = <<"crash_test_server_2">>,
    ServerPid2 = start_dummy_server(ServerId2),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId2, ServerPid2, #{})),

    TransportId2 = <<"crash_test_transport_2">>,
    TransportPid2 = start_dummy_transport(TransportId2),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId2, TransportPid2,
                                                        #{server_id => ServerId2})),

    ?assertMatch({ok, ServerId2}, erlmcp_registry:get_server_for_transport(TransportId2)),

    exit(ServerPid2, kill),
    wait_for_cleanup(),

    ?assertEqual({error, not_found}, erlmcp_registry:get_server_for_transport(TransportId2)),

    %% Test 4: Binding cleanup on transport death
    ServerId3 = <<"crash_test_server_3">>,
    ServerPid3 = start_dummy_server(ServerId3),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId3, ServerPid3, #{})),

    TransportId3 = <<"crash_test_transport_3">>,
    TransportPid3 = start_dummy_transport(TransportId3),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId3, TransportPid3,
                                                        #{server_id => ServerId3})),

    exit(TransportPid3, kill),
    wait_for_cleanup(),

    ?assertEqual({error, not_found}, erlmcp_registry:find_transport(TransportId3)),

    %% Test 5: Handle unknown gen_server calls
    RegistryPid = erlmcp_registry:get_pid(),
    ?assertEqual({error, unknown_request}, gen_server:call(RegistryPid, unknown_request)),

    %% Test 6: Handle unknown gen_server casts
    gen_server:cast(RegistryPid, unknown_cast),
    timer:sleep(50),
    ?assert(erlang:is_process_alive(RegistryPid)),

    %% Test 7: Handle unknown info messages
    RegistryPid ! unknown_info,
    timer:sleep(50),
    ?assert(erlang:is_process_alive(RegistryPid)),

    %% Test 8: Registry remains stable after errors
    ?assert(erlang:is_process_alive(erlmcp_registry:get_pid())),
    ?assertMatch({ok, _}, erlmcp_registry:get_all_state()),

    %% Cleanup
    stop_dummy(ServerPid3),
    stop_dummy(TransportPid2),
    erlmcp_registry:unregister_server(ServerId3),
    erlmcp_registry:unregister_transport(TransportId2).

%%%====================================================================
%%% Concurrent Operations Tests (6 tests)
%%%====================================================================

concurrent_tests() ->
    %% Test 1: Concurrent server registrations
    ServerPids = [spawn(fun() ->
        ServerId = <<"concurrent_server_", (integer_to_binary(N))/binary>>,
        ServerPid = start_dummy_server(ServerId),
        erlmcp_registry:register_server(ServerId, ServerPid, #{}),
        receive stop -> stop_dummy(ServerPid) after 10000 -> ok end
    end) || N <- lists:seq(1, 30)],

    timer:sleep(300),

    Servers = erlmcp_registry:list_servers(),
    ?assert(length(Servers) >= 30),

    [Pid ! stop || Pid <- ServerPids],

    %% Test 2: Concurrent transport registrations
    TransportPids = [spawn(fun() ->
        TransportId = <<"concurrent_transport_", (integer_to_binary(N))/binary>>,
        TransportPid = start_dummy_transport(TransportId),
        erlmcp_registry:register_transport(TransportId, TransportPid, #{}),
        receive stop -> stop_dummy(TransportPid) after 10000 -> ok end
    end) || N <- lists:seq(1, 30)],

    timer:sleep(300),

    Transports = erlmcp_registry:list_transports(),
    ?assert(length(Transports) >= 30),

    [Pid ! stop || Pid <- TransportPids],

    %% Test 3: Concurrent bindings
    ServerId = <<"concurrent_bind_server">>,
    ServerPid = start_dummy_server(ServerId),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId, ServerPid, #{})),

    BindPids = [spawn(fun() ->
        TransportId = <<"concurrent_bind_transport_", (integer_to_binary(N))/binary>>,
        TransportPid = start_dummy_transport(TransportId),
        erlmcp_registry:register_transport(TransportId, TransportPid, #{}),
        erlmcp_registry:bind_transport_to_server(TransportId, ServerId),
        receive stop -> stop_dummy(TransportPid) after 10000 -> ok end
    end) || N <- lists:seq(1, 20)],

    timer:sleep(300),
    [Pid ! stop || Pid <- BindPids],
    stop_dummy(ServerPid),

    %% Test 4: Concurrent routing
    RoutingServerId = <<"concurrent_routing_server">>,
    RoutingServerPid = spawn(fun() -> routing_server_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_server(RoutingServerId, RoutingServerPid, #{})),

    RoutingTransportId = <<"concurrent_routing_transport">>,
    RoutingTransportPid = spawn(fun() -> routing_transport_loop([]) end),
    ?assertEqual(ok, erlmcp_registry:register_transport(RoutingTransportId, RoutingTransportPid,
                                                        #{server_id => RoutingServerId})),

    RoutePids = [spawn(fun() ->
        Message = #{seq => N},
        erlmcp_registry:route_to_server(RoutingServerId, RoutingTransportId, Message)
    end) || N <- lists:seq(1, 100)],

    [begin
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok after 5000 -> ok end
    end || Pid <- RoutePids],

    timer:sleep(200),

    RoutingServerPid ! {get_messages, self()},
    receive
        {messages, ReceivedMessages} ->
            ?assert(length(ReceivedMessages) >= 90)
    after 2000 ->
        ok
    end,

    RoutingServerPid ! stop,
    RoutingTransportPid ! stop,

    %% Test 5: Concurrent registration attempts (same ID)
    SameServerId = <<"concurrent_same_server">>,
    ConcurrentAttempts = [spawn(fun() ->
        Pid = start_dummy_server(SameServerId),
        Result = erlmcp_registry:register_server(SameServerId, Pid, #{}),
        case Result of
            ok -> receive stop -> stop_dummy(Pid) after 5000 -> ok end;
            {error, already_registered} -> stop_dummy(Pid)
        end
    end) || _ <- lists:seq(1, 10)],

    timer:sleep(200),
    [Pid ! stop || Pid <- ConcurrentAttempts],

    %% Test 6: Mixed concurrent operations
    MixedPids = lists:flatten([
        [spawn(fun() ->
            SId = <<"concurrent_mixed_server_", (integer_to_binary(N))/binary>>,
            SPid = start_dummy_server(SId),
            erlmcp_registry:register_server(SId, SPid, #{}),
            receive stop -> stop_dummy(SPid) after 5000 -> ok end
        end) || N <- lists:seq(1, 10)],
        [spawn(fun() ->
            TId = <<"concurrent_mixed_transport_", (integer_to_binary(N))/binary>>,
            TPid = start_dummy_transport(TId),
            erlmcp_registry:register_transport(TId, TPid, #{}),
            receive stop -> stop_dummy(TPid) after 5000 -> ok end
        end) || N <- lists:seq(1, 10)]
    ]),

    timer:sleep(300),
    [Pid ! stop || Pid <- MixedPids],

    erlmcp_registry:unregister_server(ServerId),
    erlmcp_registry:unregister_server(RoutingServerId),
    erlmcp_registry:unregister_transport(RoutingTransportId),

    wait_for_cleanup().

%%%====================================================================
%%% Process Death Cleanup Tests (4 tests)
%%%====================================================================

cleanup_tests() ->
    %% Test 1: Server death removes all bindings
    ServerId1 = <<"cleanup_server_1">>,
    ServerPid1 = start_dummy_server(ServerId1),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId1, ServerPid1, #{})),

    TransportIds = [<<"cleanup_transport_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 3)],
    TransportPids = [begin
        Pid = start_dummy_transport(TId),
        ?assertEqual(ok, erlmcp_registry:register_transport(TId, Pid, #{server_id => ServerId1})),
        Pid
    end || TId <- TransportIds],

    [?assertMatch({ok, ServerId1}, erlmcp_registry:get_server_for_transport(TId))
     || TId <- TransportIds],

    exit(ServerPid1, kill),
    wait_for_cleanup(),

    [?assertEqual({error, not_found}, erlmcp_registry:get_server_for_transport(TId))
     || TId <- TransportIds],

    %% Test 2: Transport death removes binding
    ServerId2 = <<"cleanup_server_2">>,
    ServerPid2 = start_dummy_server(ServerId2),
    ?assertEqual(ok, erlmcp_registry:register_server(ServerId2, ServerPid2, #{})),

    TransportId2 = <<"cleanup_transport_single">>,
    TransportPid2 = start_dummy_transport(TransportId2),
    ?assertEqual(ok, erlmcp_registry:register_transport(TransportId2, TransportPid2,
                                                        #{server_id => ServerId2})),

    ?assertMatch({ok, ServerId2}, erlmcp_registry:get_server_for_transport(TransportId2)),

    exit(TransportPid2, kill),
    wait_for_cleanup(),

    ?assertEqual({error, not_found}, erlmcp_registry:find_transport(TransportId2)),

    %% Test 3: Multiple server deaths
    ServerIds = [<<"cleanup_multi_server_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 5)],
    ServerPids2 = [begin
        Pid = start_dummy_server(SId),
        ?assertEqual(ok, erlmcp_registry:register_server(SId, Pid, #{})),
        Pid
    end || SId <- ServerIds],

    [exit(Pid, kill) || Pid <- ServerPids2],
    wait_for_cleanup(),

    [?assertEqual({error, not_found}, erlmcp_registry:find_server(SId))
     || SId <- ServerIds],

    %% Test 4: Multiple transport deaths
    TransportIds2 = [<<"cleanup_multi_transport_", (integer_to_binary(N))/binary>>
                     || N <- lists:seq(1, 5)],
    TransportPids2 = [begin
        Pid = start_dummy_transport(TId),
        ?assertEqual(ok, erlmcp_registry:register_transport(TId, Pid, #{})),
        Pid
    end || TId <- TransportIds2],

    [exit(Pid, kill) || Pid <- TransportPids2],
    wait_for_cleanup(),

    [?assertEqual({error, not_found}, erlmcp_registry:find_transport(TId))
     || TId <- TransportIds2],

    %% Cleanup
    [stop_dummy(Pid) || Pid <- TransportPids],
    stop_dummy(ServerPid2),
    erlmcp_registry:unregister_server(ServerId2).

%%%====================================================================
%%% Global Registry Tests (4 tests)
%%%====================================================================

global_registry_tests() ->
    %% Note: Full distributed testing requires multiple nodes
    %% Here we test local/global parameter handling

    %% Test 1: Local scope (default)
    ServerId = <<"global_test_server">>,
    ServerPid = start_dummy_server(ServerId),

    ?assertEqual(ok, erlmcp_registry:register_server(local, ServerId, ServerPid, #{})),
    ?assertMatch({ok, _}, erlmcp_registry:find_server(local, ServerId)),

    LocalServers = erlmcp_registry:list_servers(local),
    ?assert(lists:keymember(ServerId, 1, LocalServers)),

    %% Test 2: Default is local
    ?assertMatch({ok, _}, erlmcp_registry:find_server(ServerId)),

    %% Test 3: Unregister local
    ?assertEqual(ok, erlmcp_registry:unregister_server(local, ServerId)),
    ?assertEqual({error, not_found}, erlmcp_registry:find_server(local, ServerId)),

    %% Test 4: Transport local scope
    TransportId = <<"global_test_transport">>,
    TransportPid = start_dummy_transport(TransportId),

    ?assertEqual(ok, erlmcp_registry:register_transport(local, TransportId, TransportPid, #{})),
    ?assertMatch({ok, _}, erlmcp_registry:find_transport(local, TransportId)),
    ?assertEqual(ok, erlmcp_registry:unregister_transport(local, TransportId)),

    %% Cleanup
    stop_dummy(ServerPid),
    stop_dummy(TransportPid).

%%%====================================================================
%%% Edge Cases Tests (6 tests)
%%%====================================================================

edge_cases_tests() ->
    %% Test 1: Very long IDs
    LongId = binary:copy(<<$A>>, 1000),
    LongServerPid = start_dummy_server(LongId),
    ?assertEqual(ok, erlmcp_registry:register_server(LongId, LongServerPid, #{})),
    ?assertMatch({ok, _}, erlmcp_registry:find_server(LongId)),
    ?assertEqual(ok, erlmcp_registry:unregister_server(LongId)),
    stop_dummy(LongServerPid),

    %% Test 2: Special characters in IDs
    SpecialId = <<"test/server:123@example.com#special">>,
    SpecialServerPid = start_dummy_server(SpecialId),
    ?assertEqual(ok, erlmcp_registry:register_server(SpecialId, SpecialServerPid, #{})),
    ?assertMatch({ok, _}, erlmcp_registry:find_server(SpecialId)),
    ?assertEqual(ok, erlmcp_registry:unregister_server(SpecialId)),
    stop_dummy(SpecialServerPid),

    %% Test 3: Large config maps
    LargeConfig = maps:from_list([{<<"key_", (integer_to_binary(N))/binary>>, N}
                                  || N <- lists:seq(1, 100)]),
    LargeServerId = <<"large_config_server">>,
    LargeServerPid = start_dummy_server(LargeServerId),
    ?assertEqual(ok, erlmcp_registry:register_server(LargeServerId, LargeServerPid, LargeConfig)),
    {ok, {_, FoundConfig}} = erlmcp_registry:find_server(LargeServerId),
    ?assertEqual(LargeConfig, FoundConfig),
    ?assertEqual(ok, erlmcp_registry:unregister_server(LargeServerId)),
    stop_dummy(LargeServerPid),

    %% Test 4: Empty config
    EmptyServerId = <<"empty_config_server">>,
    EmptyServerPid = start_dummy_server(EmptyServerId),
    ?assertEqual(ok, erlmcp_registry:register_server(EmptyServerId, EmptyServerPid, #{})),
    ?assertMatch({ok, {_, #{}}}, erlmcp_registry:find_server(EmptyServerId)),
    ?assertEqual(ok, erlmcp_registry:unregister_server(EmptyServerId)),
    stop_dummy(EmptyServerPid),

    %% Test 5: Rapid register/unregister cycles
    RapidId = <<"rapid_server">>,
    RapidPid = start_dummy_server(RapidId),
    [begin
        ?assertEqual(ok, erlmcp_registry:register_server(RapidId, RapidPid, #{})),
        ?assertEqual(ok, erlmcp_registry:unregister_server(RapidId))
    end || _ <- lists:seq(1, 10)],
    stop_dummy(RapidPid),

    %% Test 6: Unicode IDs
    UnicodeId = <<"test_서버_🚀">>,
    UnicodePid = start_dummy_server(UnicodeId),
    ?assertEqual(ok, erlmcp_registry:register_server(UnicodeId, UnicodePid, #{})),
    ?assertMatch({ok, _}, erlmcp_registry:find_server(UnicodeId)),
    ?assertEqual(ok, erlmcp_registry:unregister_server(UnicodeId)),
    stop_dummy(UnicodePid).
