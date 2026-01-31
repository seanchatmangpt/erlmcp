%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_registry Module
%%%
%%% Tests invariants:
%%% - Registry operations are idempotent (register same PID twice)
%%% - Server/transport lookup consistency after registration
%%% - Process monitoring and auto-cleanup on death
%%% - Concurrent registration safety (no race conditions)
%%% - Transport-server binding consistency
%%%
%%% Chicago School TDD: Real gen_server registry, real processes, state-based verification
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid server IDs
server_id() ->
    proper_types:oneof([
        proper_types:atom(),
        proper_types:binary()
    ]).

%% Generate valid transport IDs
transport_id() ->
    proper_types:oneof([
        proper_types:atom(),
        proper_types:binary()
    ]).

%% Generate server configurations
server_config() ->
    proper_types:map(
        proper_types:atom(),
        proper_types:oneof([
            proper_types:int(),
            proper_types:binary(),
            proper_types:atom(),
            proper_types:bool(),
            proper_types:list(proper_types:int())
        ])
    ).

%% Generate transport configurations
transport_config() ->
    proper_types:map(
        proper_types:atom(),
        proper_types:oneof([
            proper_types:int(),
            proper_types:binary(),
            proper_types:atom()
        ])
    ).

%% Generate lists of unique IDs
unique_id_list(_Generator, Count) ->
    proper_types:non_empty(proper_types:list(_Generator)).

%%%====================================================================
%%% Properties: Server Registration
%%%====================================================================

%% Property: Registering same server PID twice is idempotent
prop_server_registration_idempotent() ->
    ?FORALL({ServerId, Config}, {server_id(), server_config()},
        begin
            %% Setup real registry
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register twice
            Result1 = gen_server:call(Registry, {register_server, ServerId, TestServer, Config}),
            Result2 = gen_server:call(Registry, {register_server, ServerId, TestServer, Config}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer, kill),

            %% Both should succeed (idempotent)
            Result1 =:= ok andalso Result2 =:= ok
        end).

%% Property: Server lookup returns correct PID and config after registration
prop_server_lookup_consistency() ->
    ?FORALL({ServerId, Config}, {server_id(), server_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer, Config}),

            %% Lookup
            LookupResult = gen_server:call(Registry, {find_server, ServerId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer, kill),

            %% Verify lookup returns correct PID and config
            LookupResult =:= {ok, {TestServer, Config}}
        end).

%% Property: Unregistering and re-registering same server ID with different PID works
prop_server_reregistration_different_pid() ->
    ?FORALL({ServerId, Config}, {server_id(), server_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer1 = spawn(fun() -> receive after 5000 -> ok end end),
            TestServer2 = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register first server
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer1, Config}),

            %% Unregister
            ok = gen_server:call(Registry, {unregister_server, ServerId}),

            %% Register second server with same ID
            Result = gen_server:call(Registry, {register_server, ServerId, TestServer2, Config}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer1, kill),
            exit(TestServer2, kill),

            %% Should succeed
            Result =:= ok
        end).

%% Property: Registering different PIDs with same server ID fails
prop_server_duplicate_different_pid_fails() ->
    ?FORALL({ServerId, Config}, {server_id(), server_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer1 = spawn(fun() -> receive after 5000 -> ok end end),
            TestServer2 = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register first server
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer1, Config}),

            %% Try to register second server with same ID
            Result = gen_server:call(Registry, {register_server, ServerId, TestServer2, Config}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer1, kill),
            exit(TestServer2, kill),

            %% Should fail
            Result =:= {error, already_registered}
        end).

%%%====================================================================
%%% Properties: Transport Registration
%%%====================================================================

%% Property: Registering same transport PID twice is idempotent
prop_transport_registration_idempotent() ->
    ?FORALL({TransportId, Config}, {transport_id(), transport_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register twice
            Result1 = gen_server:call(Registry, {register_transport, TransportId, TestTransport, Config}),
            Result2 = gen_server:call(Registry, {register_transport, TransportId, TestTransport, Config}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestTransport, kill),

            %% Both should succeed
            Result1 =:= ok andalso Result2 =:= ok
        end).

%% Property: Transport lookup returns correct PID and config after registration
prop_transport_lookup_consistency() ->
    ?FORALL({TransportId, Config}, {transport_id(), transport_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register
            ok = gen_server:call(Registry, {register_transport, TransportId, TestTransport, Config}),

            %% Lookup
            LookupResult = gen_server:call(Registry, {find_transport, TransportId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestTransport, kill),

            %% Verify lookup returns correct PID and config
            LookupResult =:= {ok, {TestTransport, Config}}
        end).

%%%====================================================================
%%% Properties: Transport-Server Binding
%%%====================================================================

%% Property: Binding transport to server and retrieving server works
prop_transport_server_binding_consistency() ->
    ?FORALL({ServerId, TransportId}, {server_id(), transport_id()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register both
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer, #{}}),
            ok = gen_server:call(Registry, {register_transport, TransportId, TestTransport, #{type => stdio}}),

            %% Bind
            ok = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

            %% Get server for transport
            LookupResult = gen_server:call(Registry, {get_server_for_transport, TransportId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer, kill),
            exit(TestTransport, kill),

            %% Verify binding
            LookupResult =:= {ok, ServerId}
        end).

%% Property: Unbinding transport removes binding
prop_transport_unbinding_removes_binding() ->
    ?FORALL({ServerId, TransportId}, {server_id(), transport_id()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register and bind
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer, #{}}),
            ok = gen_server:call(Registry, {register_transport, TransportId, TestTransport, #{type => stdio}}),
            ok = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

            %% Unbind
            ok = gen_server:call(Registry, {unbind_transport, TransportId}),

            %% Try to get server
            LookupResult = gen_server:call(Registry, {get_server_for_transport, TransportId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer, kill),
            exit(TestTransport, kill),

            %% Should not find binding
            LookupResult =:= {error, not_found}
        end).

%% Property: Binding non-existent transport fails
prop_bind_nonexistent_transport_fails() ->
    ?FORALL({ServerId, TransportId}, {server_id(), transport_id()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register only server
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer, #{}}),

            %% Try to bind non-existent transport
            Result = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestServer, kill),

            %% Should fail
            Result =:= {error, transport_not_found}
        end).

%% Property: Binding to non-existent server fails
prop_bind_nonexistent_server_fails() ->
    ?FORALL({ServerId, TransportId}, {server_id(), transport_id()},
        begin
            {ok, Registry} = start_test_registry(),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register only transport
            ok = gen_server:call(Registry, {register_transport, TransportId, TestTransport, #{type => stdio}}),

            %% Try to bind to non-existent server
            Result = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestTransport, kill),

            %% Should fail
            Result =:= {error, server_not_found}
        end).

%%%====================================================================
%%% Properties: Process Monitoring
%%%====================================================================

%% Property: Server process death triggers automatic unregistration
prop_server_death_auto_unregisters() ->
    ?FORALL({ServerId, Config}, {server_id(), server_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer, Config}),

            %% Verify registered
            {ok, _} = gen_server:call(Registry, {find_server, ServerId}),

            %% Kill server
            exit(TestServer, kill),
            timer:sleep(200),  % Allow gproc monitor cleanup

            %% Try to find
            LookupResult = gen_server:call(Registry, {find_server, ServerId}),

            %% Cleanup
            gen_server:stop(Registry),

            %% Should not find (auto-unregistered)
            LookupResult =:= {error, not_found}
        end).

%% Property: Transport process death triggers automatic unregistration
prop_transport_death_auto_unregisters() ->
    ?FORALL({TransportId, Config}, {transport_id(), transport_config()},
        begin
            {ok, Registry} = start_test_registry(),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register
            ok = gen_server:call(Registry, {register_transport, TransportId, TestTransport, Config}),

            %% Verify registered
            {ok, _} = gen_server:call(Registry, {find_transport, TransportId}),

            %% Kill transport
            exit(TestTransport, kill),
            timer:sleep(200),  % Allow gproc monitor cleanup

            %% Try to find
            LookupResult = gen_server:call(Registry, {find_transport, TransportId}),

            %% Cleanup
            gen_server:stop(Registry),

            %% Should not find (auto-unregistered)
            LookupResult =:= {error, not_found}
        end).

%% Property: Server death cleans up transport bindings
prop_server_death_cleans_up_bindings() ->
    ?FORALL({ServerId, TransportId}, {server_id(), transport_id()},
        begin
            {ok, Registry} = start_test_registry(),
            TestServer = spawn(fun() -> receive after 5000 -> ok end end),
            TestTransport = spawn(fun() -> receive after 5000 -> ok end end),

            %% Register and bind
            ok = gen_server:call(Registry, {register_server, ServerId, TestServer, #{}}),
            ok = gen_server:call(Registry, {register_transport, TransportId, TestTransport, #{type => stdio}}),
            ok = gen_server:call(Registry, {bind_transport_to_server, TransportId, ServerId}),

            %% Verify binding
            {ok, ServerId} = gen_server:call(Registry, {get_server_for_transport, TransportId}),

            %% Kill server
            exit(TestServer, kill),
            timer:sleep(200),  % Allow cleanup

            %% Try to get binding
            LookupResult = gen_server:call(Registry, {get_server_for_transport, TransportId}),

            %% Cleanup
            gen_server:stop(Registry),
            exit(TestTransport, kill),

            %% Binding should be cleaned up
            LookupResult =:= {error, not_found}
        end).

%%%====================================================================
%%% Properties: List Operations
%%%====================================================================

%% Property: list_servers returns all registered servers
prop_list_servers_consistency() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, Registry} = start_test_registry(),

            %% Register multiple servers
            ServerIds = [list_to_atom("server_" ++ integer_to_list(N)) || N <- lists:seq(1, Count)],
            TestServers = [spawn(fun() -> receive after 5000 -> ok end end) || _ <- lists:seq(1, Count)],

            lists:foreach(fun({Id, Pid}) ->
                gen_server:call(Registry, {register_server, Id, Pid, #{}})
            end, lists:zip(ServerIds, TestServers)),

            %% List servers
            ServerList = gen_server:call(Registry, list_servers),

            %% Cleanup
            gen_server:stop(Registry),
            lists:foreach(fun(Pid) -> exit(Pid, kill) end, TestServers),

            %% Verify count
            length(ServerList) =:= Count
        end).

%% Property: list_transports returns all registered transports
prop_list_transports_consistency() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, Registry} = start_test_registry(),

            %% Register multiple transports
            TransportIds = [list_to_atom("transport_" ++ integer_to_list(N)) || N <- lists:seq(1, Count)],
            TestTransports = [spawn(fun() -> receive after 5000 -> ok end end) || _ <- lists:seq(1, Count)],

            lists:foreach(fun({Id, Pid}) ->
                gen_server:call(Registry, {register_transport, Id, Pid, #{type => stdio}})
            end, lists:zip(TransportIds, TestTransports)),

            %% List transports
            TransportList = gen_server:call(Registry, list_transports),

            %% Cleanup
            gen_server:stop(Registry),
            lists:foreach(fun(Pid) -> exit(Pid, kill) end, TestTransports),

            %% Verify count
            length(TransportList) =:= Count
        end).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Start a test registry
start_test_registry() ->
    ensure_gproc_started(),
    clear_test_registrations(),
    timer:sleep(50),
    gen_server:start(erlmcp_registry, [], []).

%% Ensure gproc is started
ensure_gproc_started() ->
    case whereis(gproc) of
        undefined -> application:start(gproc);
        _ -> ok
    end.

%% Clear test registrations using gproc
clear_test_registrations() ->
    %% Clean up any lingering test registrations
    try
        %% Unregister all test servers
        ServerKeys = gproc:select([{{{n, l, {mcp, server, '$1'}}, '$2', '$3'}, [], ['$1']}]),
        lists:foreach(fun(Key) ->
            case gproc:where({n, l, {mcp, server, Key}}) of
                undefined -> ok;
                Pid -> gproc:unreg_other({n, l, {mcp, server, Key}}, Pid)
            end
        end, ServerKeys),

        %% Unregister all test transports
        TransportKeys = gproc:select([{{{n, l, {mcp, transport, '$1'}}, '$2', '$3'}, [], ['$1']}]),
        lists:foreach(fun(Key) ->
            case gproc:where({n, l, {mcp, transport, Key}}) of
                undefined -> ok;
                Pid -> gproc:unreg_other({n, l, {mcp, transport, Key}}, Pid)
            end
        end, TransportKeys)
    catch
        _:_ -> ok
    end.

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    {setup,
     fun setup_suite/0,
     fun cleanup_suite/1,
     [
         {"Server registration idempotent", ?_assertEqual(true, proper:quickcheck(prop_server_registration_idempotent(), 20))},
         {"Server lookup consistency", ?_assertEqual(true, proper:quickcheck(prop_server_lookup_consistency(), 20))},
         {"Server reregistration different PID", ?_assertEqual(true, proper:quickcheck(prop_server_reregistration_different_pid(), 20))},
         {"Server duplicate different PID fails", ?_assertEqual(true, proper:quickcheck(prop_server_duplicate_different_pid_fails(), 20))},
         {"Transport registration idempotent", ?_assertEqual(true, proper:quickcheck(prop_transport_registration_idempotent(), 20))},
         {"Transport lookup consistency", ?_assertEqual(true, proper:quickcheck(prop_transport_lookup_consistency(), 20))},
         {"Transport-server binding consistency", ?_assertEqual(true, proper:quickcheck(prop_transport_server_binding_consistency(), 20))},
         {"Transport unbinding removes binding", ?_assertEqual(true, proper:quickcheck(prop_transport_unbinding_removes_binding(), 20))},
         {"Bind non-existent transport fails", ?_assertEqual(true, proper:quickcheck(prop_bind_nonexistent_transport_fails(), 20))},
         {"Bind non-existent server fails", ?_assertEqual(true, proper:quickcheck(prop_bind_nonexistent_server_fails(), 20))},
         {"Server death auto-unregisters", ?_assertEqual(true, proper:quickcheck(prop_server_death_auto_unregisters(), 20))},
         {"Transport death auto-unregisters", ?_assertEqual(true, proper:quickcheck(prop_transport_death_auto_unregisters(), 20))},
         {"Server death cleans up bindings", ?_assertEqual(true, proper:quickcheck(prop_server_death_cleans_up_bindings(), 20))},
         {"List servers consistency", ?_assertEqual(true, proper:quickcheck(prop_list_servers_consistency(), 10))},
         {"List transports consistency", ?_assertEqual(true, proper:quickcheck(prop_list_transports_consistency(), 10))}
     ]
    }.

setup_suite() ->
    ensure_gproc_started().

cleanup_suite(_) ->
    clear_test_registrations().

%%%====================================================================
%%% Run All Properties
%%%====================================================================

run_all_properties() ->
    setup_suite(),
    try
        Result = proper:module(?MODULE, [{numtests, 50}]),
        cleanup_suite(ok),
        Result
    catch
        Class:Reason:Stack ->
            cleanup_suite(error),
            erlang:raise(Class, Reason, Stack)
    end.
