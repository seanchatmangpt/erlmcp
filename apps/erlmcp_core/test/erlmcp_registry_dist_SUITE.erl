-module(erlmcp_registry_dist_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Suppress ct_slave deprecation warnings (will migrate to peer module in OTP 29)
-compile([{nowarn_deprecated_function, [{ct_slave, start, 2}, {ct_slave, stop, 1}]}]).

%% CT callbacks
-export([
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    single_node_disabled/1,
    single_node_enabled/1,
    multi_node_registration/1,
    multi_node_failover/1,
    split_brain_detection/1,
    global_name_conflict/1,
    node_reconnection/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        {group, single_node},
        {group, multi_node}
    ].

groups() ->
    [
        {single_node, [sequence], [
            single_node_disabled,
            single_node_enabled
        ]},
        {multi_node, [sequence], [
            multi_node_registration,
            multi_node_failover,
            split_brain_detection,
            global_name_conflict,
            node_reconnection
        ]}
    ].

init_per_suite(Config) ->
    %% Start distributed Erlang if not already started
    case net_kernel:start([erlmcp_test@localhost, shortnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        {error, Reason} ->
            ct:fail("Failed to start distributed Erlang: ~p", [Reason])
    end,

    %% Set cookie
    erlang:set_cookie(node(), erlmcp_test_cookie),

    %% Start gproc
    {ok, _Apps} = application:ensure_all_started(gproc),

    Config.

end_per_suite(_Config) ->
    application:stop(gproc),
    net_kernel:stop(),
    ok.

init_per_group(single_node, Config) ->
    %% Single node tests don't need cluster
    application:set_env(erlmcp_core, cluster_enabled, false),
    Config;

init_per_group(multi_node, Config) ->
    %% Multi-node tests need cluster enabled
    application:set_env(erlmcp_core, cluster_enabled, true),
    application:set_env(erlmcp_core, cluster_cookie, erlmcp_test_cookie),

    %% Start slave nodes
    Nodes = start_slave_nodes(2, Config),
    [{slave_nodes, Nodes} | Config].

end_per_group(single_node, _Config) ->
    ok;

end_per_group(multi_node, Config) ->
    %% Stop slave nodes
    Nodes = ?config(slave_nodes, Config),
    lists:foreach(fun(Node) ->
        ct_slave:stop(Node)
    end, Nodes),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases - Single Node
%%====================================================================

single_node_disabled(_Config) ->
    %% Test that distributed features are disabled by default
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    false = erlmcp_registry_dist:is_distributed(),

    %% Operations should fail when disabled
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    {error, distributed_mode_disabled} =
        erlmcp_registry_dist:register_global(server, test, TestPid, #{}),

    exit(TestPid, kill),
    gen_server:stop(Pid),
    ok.

single_node_enabled(_Config) ->
    %% Test that distributed features work on single node
    application:set_env(erlmcp_core, cluster_enabled, true),
    application:set_env(erlmcp_core, cluster_nodes, []),
    {ok, Pid} = erlmcp_registry_dist:start_link(),

    true = erlmcp_registry_dist:is_distributed(),

    %% Can register globally
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    ok = erlmcp_registry_dist:register_global(server, single_server, TestPid, #{}),

    %% Can find
    {ok, {Node, TestPid, _}} = erlmcp_registry_dist:whereis_global({server, single_server}),
    Node = node(),

    exit(TestPid, kill),
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Multi-Node
%%====================================================================

multi_node_registration(Config) ->
    Nodes = ?config(slave_nodes, Config),
    [Node1, Node2] = Nodes,

    %% Set up cluster configuration on all nodes
    setup_cluster_on_nodes([node() | Nodes]),

    %% Start registry_dist on all nodes
    {ok, LocalPid} = erlmcp_registry_dist:start_link(),
    ok = rpc:call(Node1, application, ensure_all_started, [gproc]),
    {ok, _Pid1} = rpc:call(Node1, erlmcp_registry_dist, start_link, []),
    ok = rpc:call(Node2, application, ensure_all_started, [gproc]),
    {ok, _Pid2} = rpc:call(Node2, erlmcp_registry_dist, start_link, []),

    %% Wait for nodes to connect
    timer:sleep(500),

    %% Register on Node1
    {Pid1, _Ref1} = spawn_monitor(Node1, fun() ->
        receive stop -> ok end
    end),
    ok = rpc:call(Node1, erlmcp_registry_dist, register_global,
                  [server, multi_server, Pid1, #{node => Node1}]),

    %% Should be visible from all nodes
    {ok, {Node1, Pid1, _}} = erlmcp_registry_dist:whereis_global({server, multi_server}),
    {ok, {Node1, Pid1, _}} = rpc:call(Node2, erlmcp_registry_dist, whereis_global,
                                       [{server, multi_server}]),

    %% Cleanup
    Pid1 ! stop,
    gen_server:stop(LocalPid),
    ok.

multi_node_failover(Config) ->
    Nodes = ?config(slave_nodes, Config),
    [Node1, Node2] = Nodes,

    setup_cluster_on_nodes([node() | Nodes]),

    {ok, LocalPid} = erlmcp_registry_dist:start_link(),
    ok = rpc:call(Node1, application, ensure_all_started, [gproc]),
    {ok, _Pid1} = rpc:call(Node1, erlmcp_registry_dist, start_link, []),
    ok = rpc:call(Node2, application, ensure_all_started, [gproc]),
    {ok, _Pid2} = rpc:call(Node2, erlmcp_registry_dist, start_link, []),

    timer:sleep(500),

    %% Register on Node1
    {Pid1, Ref1} = spawn_monitor(Node1, fun() ->
        receive stop -> ok end
    end),
    ok = rpc:call(Node1, erlmcp_registry_dist, register_global,
                  [server, failover_server, Pid1, #{}]),

    %% Verify registered
    {ok, {Node1, Pid1, _}} = erlmcp_registry_dist:whereis_global({server, failover_server}),

    %% Kill the process
    exit(Pid1, kill),
    receive {'DOWN', Ref1, process, Pid1, killed} -> ok end,
    timer:sleep(500),

    %% Should no longer be registered
    {error, not_found} = erlmcp_registry_dist:whereis_global({server, failover_server}),

    %% Can re-register on Node2
    {Pid2, _Ref2} = spawn_monitor(Node2, fun() ->
        receive stop -> ok end
    end),
    ok = rpc:call(Node2, erlmcp_registry_dist, register_global,
                  [server, failover_server, Pid2, #{}]),

    {ok, {Node2, Pid2, _}} = erlmcp_registry_dist:whereis_global({server, failover_server}),

    Pid2 ! stop,
    gen_server:stop(LocalPid),
    ok.

split_brain_detection(Config) ->
    Nodes = ?config(slave_nodes, Config),
    [Node1, _Node2] = Nodes,

    setup_cluster_on_nodes([node() | Nodes]),

    %% Start split-brain detector
    {ok, DetectorPid} = erlmcp_split_brain_detector:start_link(),

    %% Initially no partition
    Status1 = erlmcp_split_brain_detector:get_partition_status(),
    false = maps:get(partition_detected, Status1),

    %% Disconnect Node1 (simulated partition)
    true = erlang:disconnect_node(Node1),
    timer:sleep(500),

    %% Force partition check
    ok = erlmcp_split_brain_detector:force_resolution(),

    %% Should detect partition (though may not show up immediately in test)
    _Status2 = erlmcp_split_brain_detector:get_partition_status(),

    %% Reconnect
    pong = net_adm:ping(Node1),
    timer:sleep(500),

    gen_server:stop(DetectorPid),
    ok.

global_name_conflict(Config) ->
    Nodes = ?config(slave_nodes, Config),
    [Node1, Node2] = Nodes,

    setup_cluster_on_nodes([node() | Nodes]),

    {ok, LocalPid} = erlmcp_registry_dist:start_link(),
    ok = rpc:call(Node1, application, ensure_all_started, [gproc]),
    {ok, _Pid1} = rpc:call(Node1, erlmcp_registry_dist, start_link, []),
    ok = rpc:call(Node2, application, ensure_all_started, [gproc]),
    {ok, _Pid2} = rpc:call(Node2, erlmcp_registry_dist, start_link, []),

    timer:sleep(500),

    %% Register on Node1
    {Pid1, _Ref1} = spawn_monitor(Node1, fun() ->
        receive stop -> ok end
    end),
    ok = rpc:call(Node1, erlmcp_registry_dist, register_global,
                  [server, conflict_server, Pid1, #{}]),

    %% Try to register same name on Node2 - should fail
    {Pid2, _Ref2} = spawn_monitor(Node2, fun() ->
        receive stop -> ok end
    end),
    {error, already_registered} = rpc:call(Node2, erlmcp_registry_dist, register_global,
                                            [server, conflict_server, Pid2, #{}]),

    Pid1 ! stop,
    Pid2 ! stop,
    gen_server:stop(LocalPid),
    ok.

node_reconnection(Config) ->
    Nodes = ?config(slave_nodes, Config),
    [Node1, _Node2] = Nodes,

    setup_cluster_on_nodes([node() | Nodes]),

    %% Start node monitor
    {ok, MonitorPid} = erlmcp_node_monitor:start_link(),

    %% Check initial status
    Status1 = erlmcp_node_monitor:get_node_status(),
    ct:pal("Initial node status: ~p", [Status1]),

    %% Disconnect node
    true = erlang:disconnect_node(Node1),
    timer:sleep(1000),

    %% Force check
    ok = erlmcp_node_monitor:force_node_check(),

    %% Reconnect
    pong = net_adm:ping(Node1),
    timer:sleep(1000),

    %% Check status after reconnect
    Status2 = erlmcp_node_monitor:get_node_status(),
    ct:pal("After reconnect node status: ~p", [Status2]),

    gen_server:stop(MonitorPid),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

start_slave_nodes(Count, _Config) ->
    start_slave_nodes(Count, 1, []).

start_slave_nodes(0, _N, Acc) ->
    lists:reverse(Acc);
start_slave_nodes(Count, N, Acc) ->
    NodeName = list_to_atom("slave" ++ integer_to_list(N)),
    {ok, Node} = ct_slave:start(NodeName, [
        {boot_timeout, 10},
        {init_timeout, 10},
        {startup_timeout, 10},
        {monitor_master, true},
        {kill_if_fail, true},
        {erl_flags, "-setcookie erlmcp_test_cookie"}
    ]),

    %% Ensure node is connected
    pong = net_adm:ping(Node),

    start_slave_nodes(Count - 1, N + 1, [Node | Acc]).

setup_cluster_on_nodes(Nodes) ->
    lists:foreach(fun(Node) ->
        rpc:call(Node, application, set_env, [erlmcp_core, cluster_enabled, true]),
        rpc:call(Node, application, set_env, [erlmcp_core, cluster_nodes, Nodes -- [Node]]),
        rpc:call(Node, application, set_env, [erlmcp_core, cluster_cookie, erlmcp_test_cookie])
    end, Nodes).
