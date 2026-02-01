%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for erlmcp_registry_distributed - Distributed Registry Backend
%%%
%%% Tests the global+pg based registry backend for multi-node deployments.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_distributed_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Register and find server", fun test_register_find_server/0},
      {"Register and find transport", fun test_register_find_transport/0},
      {"Duplicate registration prevention", fun test_duplicate_registration/0},
      {"Unregister server", fun test_unregister_server/0},
      {"Unregister transport", fun test_unregister_transport/0},
      {"List servers", fun test_list_servers/0},
      {"List transports", fun test_list_transports/0},
      {"Update server config", fun test_update_config/0},
      {"Process death cleanup", fun test_process_death_cleanup/0},
      {"Process group membership", fun test_group_membership/0},
      {"Multiple servers", fun test_multiple_servers/0},
      {"Multiple transports", fun test_multiple_transports/0},
      {"Server with capabilities groups", fun test_server_capability_groups/0},
      {"Is distributed check", fun test_is_distributed/0}]}.

setup() ->
    %% Start the registry backend
    {ok, Pid} = erlmcp_registry_distributed:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the registry backend
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(50);
        false ->
            ok
    end,

    %% Clean up any global names
    lists:foreach(fun(Name) ->
                     NameStr = atom_to_list(Name),
                     case string:prefix(NameStr, "erlmcp_") of
                         nomatch ->
                             ok;
                         _ ->
                             global:unregister_name(Name)
                     end
                  end,
                  global:registered_names()).

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_register_find_server() ->
    %% Create a test server process
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    %% Register server
    ?assertEqual(ok, erlmcp_registry_distributed:register(server, test_server_1, TestPid, Config)),

    %% Find server
    {ok, {Node, Pid, ReturnedConfig}} = erlmcp_registry_distributed:whereis(server, test_server_1),
    ?assertEqual(node(), Node),
    ?assertEqual(TestPid, Pid),
    ?assertEqual(Config, ReturnedConfig),

    %% Cleanup
    erlmcp_registry_distributed:unregister(server, test_server_1),
    exit(TestPid, kill).

test_register_find_transport() ->
    %% Create a test transport process
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config = #{type => stdio, server_id => test_server},

    %% Register transport
    ?assertEqual(ok,
                 erlmcp_registry_distributed:register(transport,
                                                      test_transport_1,
                                                      TestPid,
                                                      Config)),

    %% Find transport
    {ok, {Node, Pid, ReturnedConfig}} =
        erlmcp_registry_distributed:whereis(transport, test_transport_1),
    ?assertEqual(node(), Node),
    ?assertEqual(TestPid, Pid),
    ?assertEqual(Config, ReturnedConfig),

    %% Cleanup
    erlmcp_registry_distributed:unregister(transport, test_transport_1),
    exit(TestPid, kill).

test_duplicate_registration() ->
    %% Create a test server process
    TestPid1 =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    TestPid2 =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config = #{capabilities => #mcp_server_capabilities{}},

    %% Register first server
    ?assertEqual(ok, erlmcp_registry_distributed:register(server, dup_test, TestPid1, Config)),

    %% Try to register with different PID
    Result = erlmcp_registry_distributed:register(server, dup_test, TestPid2, Config),
    ?assertMatch({error, {already_registered, TestPid1}}, Result),

    %% Cleanup
    erlmcp_registry_distributed:unregister(server, dup_test),
    exit(TestPid1, kill),
    exit(TestPid2, kill).

test_unregister_server() ->
    %% Create and register a server
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config = #{capabilities => #mcp_server_capabilities{}},
    ?assertEqual(ok, erlmcp_registry_distributed:register(server, unreg_test, TestPid, Config)),

    %% Verify it exists
    ?assertMatch({ok, _}, erlmcp_registry_distributed:whereis(server, unreg_test)),

    %% Unregister
    ?assertEqual(ok, erlmcp_registry_distributed:unregister(server, unreg_test)),

    %% Verify it's gone
    ?assertEqual({error, not_found}, erlmcp_registry_distributed:whereis(server, unreg_test)),

    %% Cleanup
    exit(TestPid, kill).

test_unregister_transport() ->
    %% Create and register a transport
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config = #{type => tcp},
    ?assertEqual(ok,
                 erlmcp_registry_distributed:register(transport, unreg_transport, TestPid, Config)),

    %% Verify it exists
    ?assertMatch({ok, _}, erlmcp_registry_distributed:whereis(transport, unreg_transport)),

    %% Unregister
    ?assertEqual(ok, erlmcp_registry_distributed:unregister(transport, unreg_transport)),

    %% Verify it's gone
    ?assertEqual({error, not_found},
                 erlmcp_registry_distributed:whereis(transport, unreg_transport)),

    %% Cleanup
    exit(TestPid, kill).

test_list_servers() ->
    %% Register multiple servers
    Pids =
        [begin
             Pid = spawn(fun() ->
                            receive
                                stop ->
                                    ok
                            end
                         end),
             Id = list_to_atom("list_server_" ++ integer_to_list(N)),
             Config = #{capabilities => #mcp_server_capabilities{}},
             ?assertEqual(ok, erlmcp_registry_distributed:register(server, Id, Pid, Config)),
             {Id, Pid}
         end
         || N <- lists:seq(1, 3)],

    %% List all servers
    Servers = erlmcp_registry_distributed:list(server),
    ?assertEqual(3, length(Servers)),

    %% Verify all our servers are present
    ServerIds = [Id || {Id, _} <- Servers],
    lists:foreach(fun({Id, _}) -> ?assert(lists:member(Id, ServerIds)) end, Pids),

    %% Cleanup
    lists:foreach(fun({Id, Pid}) ->
                     erlmcp_registry_distributed:unregister(server, Id),
                     exit(Pid, kill)
                  end,
                  Pids).

test_list_transports() ->
    %% Register multiple transports
    Pids =
        [begin
             Pid = spawn(fun() ->
                            receive
                                stop ->
                                    ok
                            end
                         end),
             Id = list_to_atom("list_transport_" ++ integer_to_list(N)),
             Config = #{type => tcp},
             ?assertEqual(ok, erlmcp_registry_distributed:register(transport, Id, Pid, Config)),
             {Id, Pid}
         end
         || N <- lists:seq(1, 3)],

    %% List all transports
    Transports = erlmcp_registry_distributed:list(transport),
    ?assertEqual(3, length(Transports)),

    %% Verify all our transports are present
    TransportIds = [Id || {Id, _} <- Transports],
    lists:foreach(fun({Id, _}) -> ?assert(lists:member(Id, TransportIds)) end, Pids),

    %% Cleanup
    lists:foreach(fun({Id, Pid}) ->
                     erlmcp_registry_distributed:unregister(transport, Id),
                     exit(Pid, kill)
                  end,
                  Pids).

test_update_config() ->
    %% Create and register a server
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config1 = #{capabilities => #mcp_server_capabilities{}, version => 1},
    ?assertEqual(ok, erlmcp_registry_distributed:register(server, update_test, TestPid, Config1)),

    %% Update config
    Config2 = #{capabilities => #mcp_server_capabilities{}, version => 2},
    ?assertEqual(ok, erlmcp_registry_distributed:update(server, update_test, Config2)),

    %% Verify updated config
    {ok, {_Node, _Pid, ReturnedConfig}} = erlmcp_registry_distributed:whereis(server, update_test),
    ?assertEqual(Config2, ReturnedConfig),

    %% Cleanup
    erlmcp_registry_distributed:unregister(server, update_test),
    exit(TestPid, kill).

test_process_death_cleanup() ->
    %% Create and register a server
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Config = #{capabilities => #mcp_server_capabilities{}},
    ?assertEqual(ok, erlmcp_registry_distributed:register(server, death_test, TestPid, Config)),

    %% Verify it exists
    ?assertMatch({ok, _}, erlmcp_registry_distributed:whereis(server, death_test)),

    %% Kill the process
    exit(TestPid, kill),
    timer:sleep(100),

    %% Verify it's cleaned up
    ?assertEqual({error, not_found}, erlmcp_registry_distributed:whereis(server, death_test)).

test_group_membership() ->
    %% Create a test process
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),

    %% Join a group
    ?assertEqual(ok, erlmcp_registry_distributed:join_group(test_group, TestPid)),

    %% Check group membership
    Members = erlmcp_registry_distributed:get_group_members(test_group),
    ?assert(lists:member(TestPid, Members)),

    %% Leave the group
    ?assertEqual(ok, erlmcp_registry_distributed:leave_group(test_group, TestPid)),

    %% Verify no longer a member
    Members2 = erlmcp_registry_distributed:get_group_members(test_group),
    ?assertNot(lists:member(TestPid, Members2)),

    %% Cleanup
    exit(TestPid, kill).

test_multiple_servers() ->
    %% Register 10 servers
    Pids =
        [begin
             Pid = spawn(fun() ->
                            receive
                                stop ->
                                    ok
                            end
                         end),
             Id = list_to_atom("multi_server_" ++ integer_to_list(N)),
             Config = #{capabilities => #mcp_server_capabilities{}, index => N},
             ?assertEqual(ok, erlmcp_registry_distributed:register(server, Id, Pid, Config)),
             {Id, Pid}
         end
         || N <- lists:seq(1, 10)],

    %% Verify all can be found
    lists:foreach(fun({Id, Pid}) ->
                     {ok, {_Node, FoundPid, _Config}} =
                         erlmcp_registry_distributed:whereis(server, Id),
                     ?assertEqual(Pid, FoundPid)
                  end,
                  Pids),

    %% Cleanup
    lists:foreach(fun({Id, Pid}) ->
                     erlmcp_registry_distributed:unregister(server, Id),
                     exit(Pid, kill)
                  end,
                  Pids).

test_multiple_transports() ->
    %% Register 10 transports
    Pids =
        [begin
             Pid = spawn(fun() ->
                            receive
                                stop ->
                                    ok
                            end
                         end),
             Id = list_to_atom("multi_transport_" ++ integer_to_list(N)),
             Config = #{type => tcp, port => 5000 + N},
             ?assertEqual(ok, erlmcp_registry_distributed:register(transport, Id, Pid, Config)),
             {Id, Pid}
         end
         || N <- lists:seq(1, 10)],

    %% Verify all can be found
    lists:foreach(fun({Id, Pid}) ->
                     {ok, {_Node, FoundPid, _Config}} =
                         erlmcp_registry_distributed:whereis(transport, Id),
                     ?assertEqual(Pid, FoundPid)
                  end,
                  Pids),

    %% Cleanup
    lists:foreach(fun({Id, Pid}) ->
                     erlmcp_registry_distributed:unregister(transport, Id),
                     exit(Pid, kill)
                  end,
                  Pids).

test_server_capability_groups() ->
    %% Create a server with tool capabilities
    TestPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 end
              end),
    Capabilities =
        #mcp_server_capabilities{tools = #{list_tools => #{}},
                                 resources = #{},
                                 prompts = #{}},
    Config = #{capabilities => Capabilities},

    %% Register server
    ?assertEqual(ok, erlmcp_registry_distributed:register(server, tool_server, TestPid, Config)),

    %% Check it's in the all_servers group
    AllServers = erlmcp_registry_distributed:get_group_members(mcp_all_servers),
    ?assert(lists:member(TestPid, AllServers)),

    %% Check it's in the tool_servers group
    ToolServers = erlmcp_registry_distributed:get_group_members(mcp_tool_servers),
    ?assert(lists:member(TestPid, ToolServers)),

    %% Cleanup
    erlmcp_registry_distributed:unregister(server, tool_server),
    exit(TestPid, kill).

test_is_distributed() ->
    %% Verify the backend reports as distributed
    ?assertEqual(true, erlmcp_registry_distributed:is_distributed()).
