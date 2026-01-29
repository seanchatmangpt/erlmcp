-module(erlmcp_registry_dist_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    %% Start applications needed for distributed tests using utility
    ok = erlmcp_registry_utils:ensure_gproc_started(),
    ok.

cleanup(_) ->
    %% Clear test registrations
    ok = erlmcp_registry_utils:clear_test_registrations(),
    ok.

%%====================================================================
%% Unit Tests (Single Node)
%%====================================================================

basic_startup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Registry dist starts in disabled mode by default", fun test_disabled_mode/0},
         {"Registry dist rejects operations when disabled", fun test_disabled_operations/0}
     ]}.

test_disabled_mode() ->
    %% Start with clustering disabled
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, Pid} = erlmcp_registry_dist:start_link(),
    ?assert(is_pid(Pid)),
    ?assertEqual(false, erlmcp_registry_dist:is_distributed()),
    gen_server:stop(Pid),
    ok.

test_disabled_operations() ->
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    %% All operations should return error when disabled
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    ?assertEqual({error, distributed_mode_disabled},
                 erlmcp_registry_dist:register_global(server, test_server, TestPid, #{})),
    ?assertEqual({error, distributed_mode_disabled},
                 erlmcp_registry_dist:unregister_global({server, test_server})),
    ?assertEqual({error, distributed_mode_disabled},
                 erlmcp_registry_dist:whereis_global({server, test_server})),

    exit(TestPid, kill),
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Integration Tests (Local gproc global simulation)
%%====================================================================

local_global_registration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Can register globally with gproc", fun test_global_registration/0},
         {"Cannot register duplicate global names", fun test_duplicate_global/0},
         {"Can unregister global names", fun test_global_unregistration/0},
         {"Can find global processes", fun test_whereis_global/0},
         {"Can list global servers and transports", fun test_list_global/0}
     ]}.

test_global_registration() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    application:set_env(erlmcp_core, cluster_nodes, []),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    %% Register a global server
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    Config = #{capabilities => #mcp_server_capabilities{}},
    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, test_server, TestPid, Config)),

    %% Verify registration
    ?assertMatch({ok, {_Node, TestPid, _Config}}, erlmcp_registry_dist:whereis_global({server, test_server})),

    exit(TestPid, kill),
    timer:sleep(100), % Wait for cleanup
    gen_server:stop(Pid),
    ok.

test_duplicate_global() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    TestPid1 = spawn(fun() -> timer:sleep(infinity) end),
    TestPid2 = spawn(fun() -> timer:sleep(infinity) end),

    %% First registration succeeds
    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, dup_server, TestPid1, #{})),

    %% Second registration fails
    ?assertEqual({error, already_registered},
                 erlmcp_registry_dist:register_global(server, dup_server, TestPid2, #{})),

    exit(TestPid1, kill),
    exit(TestPid2, kill),
    gen_server:stop(Pid),
    ok.

test_global_unregistration() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, unreg_server, TestPid, #{})),

    %% Verify registered
    ?assertMatch({ok, _}, erlmcp_registry_dist:whereis_global({server, unreg_server})),

    %% Unregister
    ?assertEqual(ok, erlmcp_registry_dist:unregister_global({server, unreg_server})),

    %% Verify unregistered
    ?assertEqual({error, not_found}, erlmcp_registry_dist:whereis_global({server, unreg_server})),

    exit(TestPid, kill),
    gen_server:stop(Pid),
    ok.

test_whereis_global() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    %% Not found initially
    ?assertEqual({error, not_found}, erlmcp_registry_dist:whereis_global({server, where_server})),

    %% Register and find
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    Config = #{test => value},
    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, where_server, TestPid, Config)),

    {ok, {Node, FoundPid, FoundConfig}} = erlmcp_registry_dist:whereis_global({server, where_server}),
    ?assertEqual(node(), Node),
    ?assertEqual(TestPid, FoundPid),
    ?assertEqual(Config, FoundConfig),

    exit(TestPid, kill),
    gen_server:stop(Pid),
    ok.

test_list_global() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    %% Register multiple servers and transports
    Server1 = spawn(fun() -> timer:sleep(infinity) end),
    Server2 = spawn(fun() -> timer:sleep(infinity) end),
    Transport1 = spawn(fun() -> timer:sleep(infinity) end),

    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, list_server1, Server1, #{})),
    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, list_server2, Server2, #{})),
    ?assertEqual(ok, erlmcp_registry_dist:register_global(transport, list_transport1, Transport1, #{})),

    %% List servers
    Servers = erlmcp_registry_dist:list_global_servers(),
    ?assertEqual(2, length(Servers)),
    ServerIds = [Id || {Id, _} <- Servers],
    ?assert(lists:member(list_server1, ServerIds)),
    ?assert(lists:member(list_server2, ServerIds)),

    %% List transports
    Transports = erlmcp_registry_dist:list_global_transports(),
    ?assertEqual(1, length(Transports)),
    ?assertMatch([{list_transport1, _}], Transports),

    exit(Server1, kill),
    exit(Server2, kill),
    exit(Transport1, kill),
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Registry Integration Tests
%%====================================================================

registry_integration_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(gproc),
         application:set_env(erlmcp_core, cluster_enabled, true),
         {ok, DistPid} = erlmcp_registry_dist:start_link(),
         {ok, RegPid} = erlmcp_registry:start_link(),
         {DistPid, RegPid}
     end,
     fun({DistPid, RegPid}) ->
         gen_server:stop(RegPid),
         gen_server:stop(DistPid),
         application:stop(gproc)
     end,
     [
         {"Registry can use global scope via dist", fun test_registry_global_scope/0}
     ]}.

test_registry_global_scope() ->
    %% Register globally via registry API
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    Config = #{capabilities => #mcp_server_capabilities{}},
    ?assertEqual(ok, erlmcp_registry:register_server(global, scope_server, TestPid, Config)),

    %% Find via global scope
    ?assertMatch({ok, {_Node, TestPid, _}}, erlmcp_registry:find_server(global, scope_server)),

    %% List via global scope
    Servers = erlmcp_registry:list_servers(global),
    ServerIds = [Id || {Id, _} <- Servers],
    ?assert(lists:member(scope_server, ServerIds)),

    %% Unregister
    ?assertEqual(ok, erlmcp_registry:unregister_server(global, scope_server)),

    exit(TestPid, kill),
    ok.

%%====================================================================
%% Cluster Node Tests (Simulated)
%%====================================================================

cluster_nodes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Can query cluster nodes", fun test_cluster_nodes/0}
     ]}.

test_cluster_nodes() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    application:set_env(erlmcp_core, cluster_nodes, ['node1@localhost', 'node2@localhost']),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    %% Get cluster nodes (will only show current node since not actually distributed)
    Nodes = erlmcp_registry_dist:get_cluster_nodes(),
    ?assert(is_list(Nodes)),
    ?assert(lists:member(node(), Nodes)),

    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Process Monitoring Tests
%%====================================================================

process_death_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Global registration cleaned up when process dies", fun test_process_death_cleanup/0}
     ]}.

test_process_death_cleanup() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    %% Register a process
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    ?assertEqual(ok, erlmcp_registry_dist:register_global(server, death_server, TestPid, #{})),

    %% Verify registered
    ?assertMatch({ok, _}, erlmcp_registry_dist:whereis_global({server, death_server})),

    %% Kill the process
    exit(TestPid, kill),
    timer:sleep(200), % Wait for gproc cleanup

    %% Verify unregistered
    ?assertEqual({error, not_found}, erlmcp_registry_dist:whereis_global({server, death_server})),

    gen_server:stop(Pid),
    ok.
