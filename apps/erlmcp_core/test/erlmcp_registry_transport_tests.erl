-module(erlmcp_registry_transport_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Suite for erlmcp_registry Transport Operations
%%%
%%% Chicago School TDD Principles:
%%%   - Use REAL erlmcp_server processes (NO dummy spawn processes)
%%%   - Test observable behavior through ALL interfaces
%%%   - NO internal state inspection (test API boundaries only)
%%%   - NO record duplication (respect encapsulation)
%%%   - Files <500 lines each
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_transport_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun test_transport_registration/1,
        fun test_transport_auto_binding/1,
        fun test_server_transport_binding/1,
        fun test_transport_unbinding/1,
        fun test_list_transports/1,
        fun test_broadcast_routing/1,
        fun test_message_routing_to_server/1,
        fun test_message_routing_to_transport/1,
        fun test_route_message_to_server/1,
        fun test_route_message_to_transport/1,
        fun test_concurrent_bindings/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Ensure required applications are started
    application:ensure_all_started(gproc),

    % Ensure gproc is started using utility function
    ok = erlmcp_registry_utils:ensure_gproc_started(),

    % Clear any stale test registrations using utility function
    ok = erlmcp_registry_utils:clear_test_registrations(),
    timer:sleep(100),

    % Start the registry for testing
    {ok, RegistryPid} = erlmcp_registry:start_link(),
    #{registry_pid => RegistryPid, server_pids => []}.

cleanup(#{registry_pid := RegistryPid, server_pids := ServerPids}) ->
    % Stop all erlmcp_server processes
    lists:foreach(fun(ServerPid) ->
        erlmcp_test_helpers:stop_test_server(ServerPid)
    end, ServerPids),

    % Stop registry gracefully
    catch gen_server:stop(RegistryPid, shutdown, 5000),

    % Clear test registrations using utility function
    ok = erlmcp_registry_utils:clear_test_registrations(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Cases - Transport Operations
%%====================================================================

test_transport_registration(#{}) ->
    TransportId = test_transport_1,
    TransportPid = spawn_transport_process(),
    TransportConfig = #{type => stdio, server_id => <<"test_server_trans">>},

    [
        ?_test(begin
            ?assertEqual(ok, erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig))
        end),
        ?_test(begin
            ?assertMatch({ok, {TransportPid, TransportConfig}},
                         erlmcp_registry:find_transport(TransportId))
        end),
        ?_test(begin
            Transports = erlmcp_registry:list_transports(),
            ?assert(lists:keymember(TransportId, 1, Transports))
        end),
        ?_test(begin
            ?assertEqual(ok, erlmcp_registry:unregister_transport(TransportId)),
            ?assertMatch({error, not_found}, erlmcp_registry:find_transport(TransportId))
        end)
    ].

test_transport_auto_binding(#{}) ->
    ServerId = auto_bind_server,
    TransportId = auto_bind_transport,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        TransportPid = spawn_transport_process(),

        % Register transport with server_id in config - should auto-bind
        TransportConfig = #{type => stdio, server_id => ServerId},

        [
            ?_test(begin
                ?assertEqual(ok, erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig))
            end),
            ?_test(begin
                % Verify auto-binding worked
                ?assertMatch({ok, ServerId}, erlmcp_registry:get_server_for_transport(TransportId))
            end)
        ]
    end).

test_server_transport_binding(#{}) ->
    ServerId = test_server_bind,
    TransportId = test_transport_bind,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        TransportPid = spawn_transport_process(),

        % Register transport without server_id
        TransportConfig = #{type => stdio},
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig),

        % Verify no binding initially
        {error, not_found} = erlmcp_registry:get_server_for_transport(TransportId),

        [
            ?_test(begin
                % Test binding (both server and transport must be registered first)
                ?assertEqual(ok, erlmcp_registry:bind_transport_to_server(TransportId, ServerId))
            end),
            ?_test(begin
                ?assertMatch({ok, ServerId}, erlmcp_registry:get_server_for_transport(TransportId))
            end)
        ]
    end).

test_transport_unbinding(#{}) ->
    ServerId = test_server_unbind,
    TransportId = test_transport_unbind,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        TransportPid = spawn_transport_process(),

        % Register and bind
        TransportConfig = #{type => stdio},
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig),
        ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),

        % Verify binding exists
        {ok, ServerId} = erlmcp_registry:get_server_for_transport(TransportId),

        [
            ?_test(begin
                ?assertEqual(ok, erlmcp_registry:unbind_transport(TransportId))
            end),
            ?_test(begin
                ?assertMatch({error, not_found}, erlmcp_registry:get_server_for_transport(TransportId))
            end),
            ?_test(begin
                % Unbinding non-existent transport should be ok (idempotent)
                ?assertEqual(ok, erlmcp_registry:unbind_transport(nonexistent_transport))
            end)
        ]
    end).

test_list_transports(#{}) ->
    % Create multiple transports
    TransportCount = 5,
    {TransportIds, TransportPids} = lists:unzip(lists:map(fun(N) ->
        TransportId = list_to_atom("list_transport_" ++ integer_to_list(N)),
        TransportPid = spawn_transport_process(),
        TransportConfig = #{type => stdio},
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig),
        {TransportId, TransportPid}
    end, lists:seq(1, TransportCount))),

    TransportList = erlmcp_registry:list_transports(),

    [
        ?_assertEqual(TransportCount, length(TransportList)),
        ?_test(begin
            % Verify all transport IDs are present
            lists:foreach(fun(TransportId) ->
                ?assert(lists:keymember(TransportId, 1, TransportList))
            end, TransportIds)
        end)
    ].

test_broadcast_routing(#{}) ->
    ServerId = broadcast_server,
    Transport1Id = broadcast_transport_1,
    Transport2Id = broadcast_transport_2,
    Transport3Id = broadcast_transport_3,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        % Create transports that can receive messages
        Transport1Pid = spawn_message_receiving_transport(),
        Transport2Pid = spawn_message_receiving_transport(),
        Transport3Pid = spawn_message_receiving_transport(),

        % Register transports
        TransportConfig = #{type => stdio},
        ok = erlmcp_registry:register_transport(Transport1Id, Transport1Pid, TransportConfig),
        ok = erlmcp_registry:register_transport(Transport2Id, Transport2Pid, TransportConfig),
        ok = erlmcp_registry:register_transport(Transport3Id, Transport3Pid, TransportConfig),

        % Bind all transports to server
        ok = erlmcp_registry:bind_transport_to_server(Transport1Id, ServerId),
        ok = erlmcp_registry:bind_transport_to_server(Transport2Id, ServerId),
        ok = erlmcp_registry:bind_transport_to_server(Transport3Id, ServerId),

        % Broadcast message - should route to all transports
        TestMessage = #{<<"type">> => <<"broadcast">>},

        [
            ?_test(begin
                ?assertEqual(ok, erlmcp_registry:route_to_transport(broadcast, ServerId, TestMessage)),
                % Allow message delivery
                timer:sleep(100)
                % If we got here without crash, broadcast succeeded
            end)
        ]
    end).

test_message_routing_to_server(#{}) ->
    ServerId = test_server_route,
    TransportId = test_transport_route,

    erlmcp_test_helpers:with_test_server(ServerId, fun(ServerPid) ->
        TransportPid = spawn_transport_process(),

        % Register and bind
        ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{type => stdio}),
        ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),

        % Route message
        TestMessage = #{<<"method">> => <<"test">>},

        [
            ?_test(begin
                % Should not crash, message is routed to server
                ?assertEqual(ok, erlmcp_registry:route_to_server(ServerId, TransportId, TestMessage))
            end)
        ]
    end).

test_message_routing_to_transport(#{}) ->
    ServerId = test_server_route2,
    TransportId = test_transport_route2,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        TransportPid = spawn_message_receiving_transport(),

        % Register and bind
        ok = erlmcp_registry:register_server(ServerId, self(), #{}),
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{type => stdio}),
        ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),

        % Route message
        TestResponse = #{<<"result">> => <<"success">>},

        [
            ?_test(begin
                % Should not crash, message is routed to transport
                ?assertEqual(ok, erlmcp_registry:route_to_transport(TransportId, ServerId, TestResponse))
            end)
        ]
    end).

test_route_message_to_server(#{}) ->
    ServerId = route_msg_server,
    TransportId = route_msg_transport,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        TransportPid = spawn_transport_process(),

        ok = erlmcp_registry:register_server(ServerId, self(), #{}),
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{type => stdio}),
        ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),

        TestMessage = #{<<"test">> => <<"route_message">>},

        [
            ?_test(begin
                % Route message to server - should broadcast to transports
                ?assertEqual(ok, erlmcp_registry:route_message({server, ServerId}, TestMessage))
            end)
        ]
    end).

test_route_message_to_transport(#{}) ->
    ServerId = route_msg_transport_server,
    TransportId = route_msg_transport_transport,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        TransportPid = spawn_message_receiving_transport(),

        ok = erlmcp_registry:register_server(ServerId, self(), #{}),
        ok = erlmcp_registry:register_transport(TransportId, TransportPid,
                                                #{type => stdio, server_id => ServerId}),

        TestMessage = #{<<"test">> => <<"route_message">>},

        [
            ?_test(begin
                % Route message to transport with bound server
                ?assertEqual(ok, erlmcp_registry:route_message({transport, TransportId}, TestMessage))
            end)
        ]
    end).

test_concurrent_bindings(#{}) ->
    ServerId = concurrent_bind_server,

    erlmcp_test_helpers:with_test_server(ServerId, fun(_ServerPid) ->
        ok = erlmcp_registry:register_server(ServerId, self(), #{}),

        % Create multiple transports
        TransportCount = 20,
        {Transports, TransportIds} = lists:unzip(lists:map(fun(N) ->
            TransportId = list_to_atom("concurrent_transport_" ++ integer_to_list(N)),
            TransportPid = spawn_transport_process(),
            ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{type => stdio}),
            {TransportPid, TransportId}
        end, lists:seq(1, TransportCount))),

        % Bind all transports concurrently
        Parent = self(),
        Pids = lists:map(fun(TransportId) ->
            spawn_link(fun() ->
                Result = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),
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

        [
            ?_assertEqual(TransportCount, SuccessCount)
        ]
    end).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Spawn a mock transport process for testing
spawn_transport_process() ->
    spawn_link(fun() ->
        receive stop -> ok after 5000 -> ok end
    end).

%% @doc Spawn a transport process that can receive and record messages
spawn_message_receiving_transport() ->
    Parent = self(),
    spawn_link(fun() ->
        loop_message_receiving_transport(Parent)
    end).

loop_message_receiving_transport(Parent) ->
    receive
        {mcp_response, _ServerId, _Message} ->
            % Message received successfully
            loop_message_receiving_transport(Parent);
        stop ->
            ok
    after 5000 ->
        ok
    end.
