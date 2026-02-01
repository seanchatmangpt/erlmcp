%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_registry Transport Registration
%%%
%%% Tests invariants:
%%% - Transport registration is idempotent
%%% - Transport lookup consistency after registration
%%% - Transport-server binding consistency
%%% - Process monitoring and auto-cleanup on death
%%% - Unbinding removes bindings
%%%
%%% Chicago School TDD: Real registry, real processes, API-only testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_transport_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid transport IDs
transport_id() ->
    proper_types:oneof([proper_types:binary(), proper_types:atom()]).

%% Generate server IDs
server_id() ->
    proper_types:oneof([proper_types:binary(), proper_types:atom()]).

%% Generate transport configurations
transport_config() ->
    proper_types:map(
        proper_types:atom(),
        proper_types:oneof([proper_types:int(), proper_types:binary(), proper_types:atom()])).

%%%====================================================================
%%% Properties: Transport Registration
%%%====================================================================

%% Property: Registering same transport PID twice is idempotent
prop_transport_registration_idempotent() ->
    ?FORALL({TransportId, _Config},
            {transport_id(), transport_config()},
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                TransportIdBin = to_binary(TransportId),
                TransportConfig = #{type => stdio},

                %% Register twice
                Result1 =
                    erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),
                Result2 =
                    erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),

                %% Cleanup
                erlmcp_registry:unregister_transport(TransportIdBin),

                %% Both should succeed
                Result1 =:= ok
                orelse Result1 =:= {error, already_registered} andalso Result2 =:= ok
                orelse Result2 =:= {error, already_registered}
            end).

%% Property: Transport lookup returns correct PID after registration
prop_transport_lookup_consistency() ->
    ?FORALL(TransportId,
            transport_id(),
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                TransportIdBin = to_binary(TransportId),
                TransportConfig = #{type => stdio},

                %% Register transport (using self() as PID)
                ok = erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),

                %% Lookup via API
                LookupResult = erlmcp_registry:find_transport(TransportIdBin),

                %% Cleanup
                erlmcp_registry:unregister_transport(TransportIdBin),

                %% Verify lookup returns correct PID
                case LookupResult of
                    {ok, {FoundPid, _Config}} when is_pid(FoundPid) ->
                        FoundPid =:= self();
                    _ ->
                        false
                end
            end).

%% Property: Transport process death triggers automatic unregistration
prop_transport_death_auto_unregisters() ->
    ?FORALL(TransportId,
            transport_id(),
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                TransportIdBin = to_binary(TransportId),
                TransportConfig = #{type => stdio},

                %% Create a dummy process for transport
                TransportPid =
                    spawn(fun() ->
                             receive after 5000 ->
                                 ok
                             end
                          end),

                %% Register transport
                ok =
                    erlmcp_registry:register_transport(TransportIdBin,
                                                       TransportPid,
                                                       TransportConfig),

                %% Verify registered
                {ok, _} = erlmcp_registry:find_transport(TransportIdBin),

                %% Kill transport
                exit(TransportPid, kill),
                timer:sleep(200),  % Allow gproc monitor cleanup

                %% Try to find
                LookupResult = erlmcp_registry:find_transport(TransportIdBin),

                %% Should not find (auto-unregistered)
                LookupResult =:= {error, not_found}
            end).

%%%====================================================================
%%% Properties: Transport-Server Binding
%%%====================================================================

%% Property: Binding transport to server and retrieving server works
prop_transport_server_binding_consistency() ->
    ?FORALL({ServerId, TransportId},
            {server_id(), transport_id()},
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                ServerIdBin = to_binary(ServerId),
                TransportIdBin = to_binary(TransportId),

                %% Register server
                {ok, _ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

                %% Register transport
                TransportConfig = #{type => stdio},
                ok = erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),

                %% Bind
                ok = erlmcp_registry:bind_transport_to_server(TransportIdBin, ServerIdBin),

                %% Get server for transport
                LookupResult = erlmcp_registry:get_server_for_transport(TransportIdBin),

                %% Cleanup
                erlmcp_registry:unbind_transport(TransportIdBin),
                erlmcp_registry:unregister_transport(TransportIdBin),
                erlmcp_test_helpers:stop_test_server(_ServerPid),

                %% Verify binding
                LookupResult =:= {ok, ServerIdBin}
            end).

%% Property: Unbinding transport removes binding
prop_transport_unbinding_removes_binding() ->
    ?FORALL({ServerId, TransportId},
            {server_id(), transport_id()},
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                ServerIdBin = to_binary(ServerId),
                TransportIdBin = to_binary(TransportId),

                %% Register server
                {ok, _ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

                %% Register and bind transport
                TransportConfig = #{type => stdio},
                ok = erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),
                ok = erlmcp_registry:bind_transport_to_server(TransportIdBin, ServerIdBin),

                %% Unbind
                ok = erlmcp_registry:unbind_transport(TransportIdBin),

                %% Try to get server
                LookupResult = erlmcp_registry:get_server_for_transport(TransportIdBin),

                %% Cleanup
                erlmcp_registry:unregister_transport(TransportIdBin),
                erlmcp_test_helpers:stop_test_server(_ServerPid),

                %% Should not find binding
                LookupResult =:= {error, not_found}
            end).

%% Property: Binding non-existent transport fails
prop_bind_nonexistent_transport_fails() ->
    ?FORALL({ServerId, TransportId},
            {server_id(), transport_id()},
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                ServerIdBin = to_binary(ServerId),
                TransportIdBin = to_binary(TransportId),

                %% Register only server
                {ok, _ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

                %% Try to bind non-existent transport
                Result = erlmcp_registry:bind_transport_to_server(TransportIdBin, ServerIdBin),

                %% Cleanup
                erlmcp_test_helpers:stop_test_server(_ServerPid),

                %% Should fail
                Result =:= {error, transport_not_found}
            end).

%% Property: Binding to non-existent server fails
prop_bind_nonexistent_server_fails() ->
    ?FORALL({ServerId, TransportId},
            {server_id(), transport_id()},
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                ServerIdBin = to_binary(ServerId),
                TransportIdBin = to_binary(TransportId),

                %% Register only transport
                TransportConfig = #{type => stdio},
                ok = erlmcp_registry:register_transport(TransportIdBin, self(), TransportConfig),

                %% Try to bind to non-existent server
                Result = erlmcp_registry:bind_transport_to_server(TransportIdBin, ServerIdBin),

                %% Cleanup
                erlmcp_registry:unregister_transport(TransportIdBin),

                %% Should fail
                Result =:= {error, server_not_found}
            end).

%%%====================================================================
%%% Properties: List Operations
%%%====================================================================

%% Property: list_transports returns all registered transports
prop_list_transports_consistency() ->
    ?FORALL(Count,
            proper_types:range(1, 10),
            begin
                %% Ensure registry is running
                {ok, _RegPid} = ensure_registry(),

                %% Register multiple transports (using self() for all)
                TransportIds =
                    [list_to_binary("transport_" ++ integer_to_list(N))
                     || N <- lists:seq(1, Count)],
                TransportConfig = #{type => stdio},

                lists:foreach(fun(Id) ->
                                 erlmcp_registry:register_transport(Id, self(), TransportConfig)
                              end,
                              TransportIds),

                %% List transports
                TransportList = erlmcp_registry:list_transports(),

                %% Cleanup
                lists:foreach(fun(Id) -> erlmcp_registry:unregister_transport(Id) end,
                              TransportIds),

                %% Verify count (may have other transports from other tests)
                length(TransportList) >= Count
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

%% Convert ID to binary
to_binary(Id) when is_binary(Id) ->
    Id;
to_binary(Id) when is_atom(Id) ->
    atom_to_binary(Id, utf8);
to_binary(Id) when is_list(Id) ->
    list_to_binary(Id);
to_binary(Id) when is_integer(Id) ->
    integer_to_binary(Id).

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    {setup,
     fun setup_suite/0,
     fun cleanup_suite/1,
     [{"Transport registration idempotent",
       ?_assertEqual(true, proper:quickcheck(prop_transport_registration_idempotent(), 20))},
      {"Transport lookup consistency",
       ?_assertEqual(true, proper:quickcheck(prop_transport_lookup_consistency(), 20))},
      {"Transport death auto-unregisters",
       ?_assertEqual(true, proper:quickcheck(prop_transport_death_auto_unregisters(), 20))},
      {"Transport-server binding consistency",
       ?_assertEqual(true, proper:quickcheck(prop_transport_server_binding_consistency(), 20))},
      {"Transport unbinding removes binding",
       ?_assertEqual(true, proper:quickcheck(prop_transport_unbinding_removes_binding(), 20))},
      {"Bind non-existent transport fails",
       ?_assertEqual(true, proper:quickcheck(prop_bind_nonexistent_transport_fails(), 20))},
      {"Bind non-existent server fails",
       ?_assertEqual(true, proper:quickcheck(prop_bind_nonexistent_server_fails(), 20))},
      {"List transports consistency",
       ?_assertEqual(true, proper:quickcheck(prop_list_transports_consistency(), 10))}]}.

setup_suite() ->
    ensure_registry().

cleanup_suite(_) ->
    %% Clean up any lingering test registrations
    try
        TransportKeys = erlmcp_registry:list_transports(),
        lists:foreach(fun({TransportId, _Pid, _Config}) ->
                         erlmcp_registry:unregister_transport(TransportId)
                      end,
                      TransportKeys)
    catch
        _:_ ->
            ok
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
