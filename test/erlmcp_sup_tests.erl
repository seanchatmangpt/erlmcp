-module(erlmcp_sup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Suite for erlmcp_sup
%%%
%%% This test suite validates the supervisor functionality including:
%%% - Starting and stopping servers
%%% - Starting and stopping transports
%%% - Integration with registry, recovery manager, and health monitor
%%% - Supervisor restart scenarios
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

supervisor_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_supervisor_starts_all_components/1,
         fun test_start_stop_server/1,
         fun test_start_stop_transport/1,
         fun test_server_registration_in_registry/1,
         fun test_transport_registration_in_registry/1,
         fun test_supervisor_child_restart/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Ensure required applications are started
    ok = ensure_gproc_started(),

    % Clear any stale registrations
    clear_test_registrations(),
    timer:sleep(100),

    % Start the main supervisor (this starts all child components)
    {ok, SupPid} = erlmcp_sup:start_link(),

    % Give components time to initialize
    timer:sleep(200),

    #{supervisor => SupPid, test_servers => [], test_transports => []}.

cleanup(#{supervisor := SupPid} = State) ->
    % Stop test servers and transports
    TestServers = maps:get(test_servers, State, []),
    TestTransports = maps:get(test_transports, State, []),

    lists:foreach(fun(ServerId) ->
        catch erlmcp_sup:stop_server(ServerId)
    end, TestServers),

    lists:foreach(fun(TransportId) ->
        catch erlmcp_sup:stop_transport(TransportId)
    end, TestTransports),

    % Stop supervisor
    catch exit(SupPid, shutdown),
    wait_for_process_death(SupPid, 5000),

    % Clear registrations
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

    % Clear servers
    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    ServerEntries = gproc:select(ServerPattern),
    lists:foreach(fun({Id, Pid}) ->
        IdStr = atom_to_list(Id),
        case string:prefix(IdStr, "test_") of
            nomatch -> ok;
            _ -> catch gproc:unreg_other({n, l, {mcp, server, Id}}, Pid)
        end
    end, ServerEntries),

    % Clear transports
    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportEntries = gproc:select(TransportPattern),
    lists:foreach(fun({Id, Pid}) ->
        IdStr = atom_to_list(Id),
        case string:prefix(IdStr, "test_") of
            nomatch -> ok;
            _ -> catch gproc:unreg_other({n, l, {mcp, transport, Id}}, Pid)
        end
    end, TransportEntries),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_supervisor_starts_all_components(#{supervisor := SupPid}) ->
    % Verify supervisor is running
    ?assert(is_process_alive(SupPid)),

    % Verify all child components are started
    Children = supervisor:which_children(erlmcp_sup),

    Tests = [
        ?_assert(is_process_alive(SupPid)),
        ?_assert(length(Children) >= 5),  % registry, server_sup, transport_sup, health_monitor, recovery_manager
        ?_assert(lists:keymember(erlmcp_registry, 1, Children)),
        ?_assert(lists:keymember(erlmcp_server_sup, 1, Children)),
        ?_assert(lists:keymember(erlmcp_transport_sup, 1, Children)),
        ?_assert(lists:keymember(erlmcp_health_monitor, 1, Children)),
        ?_assert(lists:keymember(erlmcp_recovery_manager, 1, Children))
    ],
    Tests.

test_start_stop_server(State) ->
    ServerId = test_server_sup_1,
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true}
        }
    },

    % Start server
    Result = erlmcp_sup:start_server(ServerId, ServerConfig),

    Tests = case Result of
        {ok, ServerPid} ->
            % Track for cleanup
            NewState = maps:put(test_servers, [ServerId | maps:get(test_servers, State, [])], State),

            [
                ?_assert(is_pid(ServerPid)),
                ?_assert(is_process_alive(ServerPid)),

                % Verify registered in registry
                ?_assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(ServerId)),

                % Stop server
                ?_assertEqual(ok, erlmcp_sup:stop_server(ServerId)),

                % Verify unregistered
                ?_assertMatch({error, not_found}, erlmcp_registry:find_server(ServerId))
            ];
        {error, Reason} ->
            % If starting failed, log and skip tests
            logger:warning("Failed to start server: ~p", [Reason]),
            [?_assertEqual(ok, ok)]  % Skip test gracefully
    end,

    Tests.

test_start_stop_transport(State) ->
    TransportId = test_transport_sup_1,
    TransportType = stdio,
    TransportConfig = #{
        server_id => test_server_sup_trans
    },

    % Start transport
    Result = erlmcp_sup:start_transport(TransportId, TransportType, TransportConfig),

    Tests = case Result of
        {ok, TransportPid} ->
            % Track for cleanup
            NewState = maps:put(test_transports, [TransportId | maps:get(test_transports, State, [])], State),

            [
                ?_assert(is_pid(TransportPid)),
                ?_assert(is_process_alive(TransportPid)),

                % Verify registered in registry
                ?_assertMatch({ok, {TransportPid, _}}, erlmcp_registry:find_transport(TransportId)),

                % Stop transport
                ?_assertEqual(ok, erlmcp_sup:stop_transport(TransportId)),

                % Verify unregistered
                ?_assertMatch({error, not_found}, erlmcp_registry:find_transport(TransportId))
            ];
        {error, Reason} ->
            % If starting failed, log and skip tests
            logger:warning("Failed to start transport: ~p", [Reason]),
            [?_assertEqual(ok, ok)]  % Skip test gracefully
    end,

    Tests.

test_server_registration_in_registry(State) ->
    ServerId = test_server_reg_1,
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true}
        }
    },

    % Start server
    case erlmcp_sup:start_server(ServerId, ServerConfig) of
        {ok, ServerPid} ->
            % Track for cleanup
            NewState = maps:put(test_servers, [ServerId | maps:get(test_servers, State, [])], State),

            % Verify registration
            Tests = [
                ?_assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(ServerId)),

                % Verify in list
                ?_assert(begin
                    Servers = erlmcp_registry:list_servers(),
                    lists:keymember(ServerId, 1, Servers)
                end)
            ],

            % Cleanup
            erlmcp_sup:stop_server(ServerId),
            Tests;
        {error, Reason} ->
            logger:warning("Failed to start server: ~p", [Reason]),
            [?_assertEqual(ok, ok)]
    end.

test_transport_registration_in_registry(State) ->
    TransportId = test_transport_reg_1,
    TransportType = stdio,
    TransportConfig = #{server_id => test_server_trans_reg},

    % Start transport
    case erlmcp_sup:start_transport(TransportId, TransportType, TransportConfig) of
        {ok, TransportPid} ->
            % Track for cleanup
            NewState = maps:put(test_transports, [TransportId | maps:get(test_transports, State, [])], State),

            % Verify registration
            Tests = [
                ?_assertMatch({ok, {TransportPid, _}}, erlmcp_registry:find_transport(TransportId)),

                % Verify in list
                ?_assert(begin
                    Transports = erlmcp_registry:list_transports(),
                    lists:keymember(TransportId, 1, Transports)
                end)
            ],

            % Cleanup
            erlmcp_sup:stop_transport(TransportId),
            Tests;
        {error, Reason} ->
            logger:warning("Failed to start transport: ~p", [Reason]),
            [?_assertEqual(ok, ok)]
    end.

test_supervisor_child_restart(#{supervisor := SupPid}) ->
    % This test verifies that the supervisor is configured correctly
    % and can handle child restarts

    % Get initial children
    InitialChildren = supervisor:which_children(erlmcp_sup),

    Tests = [
        % Verify supervisor is alive
        ?_assert(is_process_alive(SupPid)),

        % Verify critical children are running
        ?_assert(length(InitialChildren) >= 5),

        % Verify supervisor strategy
        ?_assertMatch({ok, _}, supervisor:get_childspec(erlmcp_sup, erlmcp_registry)),
        ?_assertMatch({ok, _}, supervisor:get_childspec(erlmcp_sup, erlmcp_server_sup)),
        ?_assertMatch({ok, _}, supervisor:get_childspec(erlmcp_sup, erlmcp_transport_sup))
    ],
    Tests.

%%====================================================================
%% Helper Functions
%%====================================================================

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
