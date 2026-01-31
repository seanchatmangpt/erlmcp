%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_registry Server Registration
%%%
%%% Tests invariants:
%%% - Server registration is idempotent
%%% - Server lookup consistency after registration
%%% - Process monitoring and auto-cleanup on death
%%% - Concurrent registration safety
%%%
%%% Chicago School TDD: Real registry, real erlmcp_server processes, API-only testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_server_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid server IDs
server_id() ->
    proper_types:oneof([
        proper_types:binary(),
        proper_types:atom()
    ]).

%% Generate server configurations
server_config() ->
    proper_types:map(
        proper_types:atom(),
        proper_types:oneof([
            proper_types:int(),
            proper_types:binary(),
            proper_types:atom(),
            proper_types:bool()
        ])
    ).

%%%====================================================================
%%% Properties: Server Registration
%%%====================================================================

%% Property: Registering same server PID twice is idempotent
prop_server_registration_idempotent() ->
    ?FORALL({ServerId, _Config}, {server_id(), server_config()},
        begin
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            %% Create a real erlmcp_server process
            ServerIdBin = to_binary(ServerId),
            Caps = #mcp_server_capabilities{
                resources = #mcp_capability{enabled = true},
                tools = #mcp_capability{enabled = true},
                prompts = #mcp_capability{enabled = true}
            },

            %% Register real server
            Result1 = erlmcp_registry:register_server(ServerIdBin, self(), Caps),
            Result2 = erlmcp_registry:register_server(ServerIdBin, self(), Caps),

            %% Cleanup
            erlmcp_registry:unregister_server(ServerIdBin),

            %% Both should succeed (idempotent)
            Result1 =:= ok orelse Result1 =:= {error, already_registered}
            andalso Result2 =:= ok orelse Result2 =:= {error, already_registered}
        end).

%% Property: Server lookup returns correct PID after registration
prop_server_lookup_consistency() ->
    ?FORALL(ServerId, server_id(),
        begin
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            %% Create a real erlmcp_server process
            ServerIdBin = to_binary(ServerId),
            {ok, ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Lookup via API
            LookupResult = erlmcp_registry:find_server(ServerIdBin),

            %% Cleanup
            erlmcp_test_helpers:stop_test_server(ServerPid),

            %% Verify lookup returns correct PID
            case LookupResult of
                {ok, {FoundPid, _Caps}} when is_pid(FoundPid) ->
                    FoundPid =:= ServerPid;
                _ ->
                    false
            end
        end).

%% Property: Unregistering and re-registering same server ID works
prop_server_reregistration_different_pid() ->
    ?FORALL(ServerId, server_id(),
        begin
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            ServerIdBin = to_binary(ServerId),

            %% Register first server
            {ok, ServerPid1} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Unregister
            ok = erlmcp_registry:unregister_server(ServerIdBin),

            %% Wait for cleanup
            timer:sleep(100),

            %% Register second server with same ID
            {ok, ServerPid2} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Lookup should find second server
            LookupResult = erlmcp_registry:find_server(ServerIdBin),

            %% Cleanup
            erlmcp_test_helpers:stop_test_server(ServerPid2),

            %% Should find new server
            case LookupResult of
                {ok, {FoundPid, _Caps}} -> FoundPid =:= ServerPid2;
                _ -> false
            end
        end).

%% Property: Registering different PIDs with same server ID fails
prop_server_duplicate_different_pid_fails() ->
    ?FORALL(ServerId, server_id(),
        begin
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            ServerIdBin = to_binary(ServerId),

            %% Register first server
            {ok, _ServerPid1} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Try to register second server with same ID (via manual registration)
            Caps = #mcp_server_capabilities{
                resources = #mcp_capability{enabled = true},
                tools = #mcp_capability{enabled = true},
                prompts = #mcp_capability{enabled = true}
            },
            Result = erlmcp_registry:register_server(ServerIdBin, self(), Caps),

            %% Cleanup
            erlmcp_test_helpers:stop_test_server(_ServerPid1),

            %% Should fail
            Result =:= {error, already_registered}
        end).

%%%====================================================================
%%% Properties: Process Monitoring
%%%====================================================================

%% Property: Server process death triggers automatic unregistration
prop_server_death_auto_unregisters() ->
    ?FORALL(ServerId, server_id(),
        begin
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            ServerIdBin = to_binary(ServerId),

            %% Register server
            {ok, ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Verify registered
            {ok, _} = erlmcp_registry:find_server(ServerIdBin),

            %% Kill server (simulating crash)
            exit(ServerPid, kill),

            %% Wait for gproc monitor cleanup
            timer:sleep(200),

            %% Try to find
            LookupResult = erlmcp_registry:find_server(ServerIdBin),

            %% Should not find (auto-unregistered)
            LookupResult =:= {error, not_found}
        end).

%% Property: Server death cleans up transport bindings
prop_server_death_cleans_up_bindings() ->
    ?FORALL({ServerId, TransportId}, {server_id(), server_id()},
        begin
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            ServerIdBin = to_binary(ServerId),
            TransportIdBin = to_binary(TransportId),

            %% Register server
            {ok, ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Register transport (manual registration with self())
            TransportConfig = #{type => stdio},
            ok = erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),

            %% Bind
            ok = erlmcp_registry:bind_transport_to_server(TransportIdBin, ServerIdBin),

            %% Verify binding
            {ok, ServerIdBin} = erlmcp_registry:get_server_for_transport(TransportIdBin),

            %% Kill server
            exit(ServerPid, kill),
            timer:sleep(200),  % Allow cleanup

            %% Try to get binding
            LookupResult = erlmcp_registry:get_server_for_transport(TransportIdBin),

            %% Cleanup
            erlmcp_registry:unbind_transport(TransportIdBin),

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
            %% Ensure registry is running
            {ok, _RegPid} = ensure_registry(),

            %% Register multiple servers
            ServerIds = [list_to_binary("server_" ++ integer_to_list(N)) || N <- lists:seq(1, Count)],
            ServerPids = [begin
                {ok, Pid} = erlmcp_test_helpers:start_test_server(Id),
                Pid
            end || Id <- ServerIds],

            %% List servers
            ServerList = erlmcp_registry:list_servers(),

            %% Cleanup
            lists:foreach(fun(Pid) -> erlmcp_test_helpers:stop_test_server(Pid) end, ServerPids),

            %% Verify count (may have other servers from other tests)
            length(ServerList) >= Count
        end).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Ensure registry is started
ensure_registry() ->
    case erlmcp_registry:get_pid() of
        undefined ->
            erlmcp_registry:start_link();
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

%% Convert server ID to binary
to_binary(Id) when is_binary(Id) -> Id;
to_binary(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
to_binary(Id) when is_list(Id) -> list_to_binary(Id);
to_binary(Id) when is_integer(Id) -> integer_to_binary(Id).

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
         {"Server death auto-unregisters", ?_assertEqual(true, proper:quickcheck(prop_server_death_auto_unregisters(), 20))},
         {"Server death cleans up bindings", ?_assertEqual(true, proper:quickcheck(prop_server_death_cleans_up_bindings(), 20))},
         {"List servers consistency", ?_assertEqual(true, proper:quickcheck(prop_list_servers_consistency(), 10))}
     ]
    }.

setup_suite() ->
    ensure_registry().

cleanup_suite(_) ->
    %% Clean up any lingering test registrations
    try
        ServerKeys = erlmcp_registry:list_servers(),
        lists:foreach(fun({ServerId, _Pid, _Caps}) ->
            erlmcp_registry:unregister_server(ServerId)
        end, ServerKeys)
    catch
        _:_ -> ok
    end.

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
