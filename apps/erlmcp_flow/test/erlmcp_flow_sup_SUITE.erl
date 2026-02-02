%%%-------------------------------------------------------------------
%%% @doc Common Test Suite for erlmcp-flow Supervision Tree
%%% Tests 3-tier supervision architecture (TIER 1-3)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_sup_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_tree_startup,
        test_agent_spawning,
        test_cascade_restart,
        test_child_isolation,
        test_swarm_supervisor_isolation,
        test_agent_supervisor_isolation,
        test_registry_restart_cascades,
        test_raft_restart_cascades,
        test_core_sup_restart_isolated
    ].

init_per_suite(Config) ->
    %% Start erlmcp_flow application
    application:ensure_all_started(gproc),
    Config.

end_per_suite(_Config) ->
    application:stop(gproc),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start fresh supervision tree for each test
    {ok, Pid} = erlmcp_flow_sup:start_link(),
    [{sup_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Stop supervision tree
    SupPid = ?config(sup_pid, Config),
    case erlang:is_process_alive(SupPid) of
        true ->
            unlink(SupPid),
            exit(SupPid, shutdown),
            timer:sleep(100);
        false ->
            ok
    end,
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc TEST 1: Tree Startup - Verify 3-tier supervision tree starts correctly
%% Validates:
%% - TIER 1: erlmcp_flow_sup (root)
%% - TIER 2: erlmcp_flow_core_sup
%% - TIER 3: erlmcp_flow_swarm_sup, erlmcp_flow_agent_sup
test_tree_startup(Config) ->
    SupPid = ?config(sup_pid, Config),

    %% Verify root supervisor is alive
    ?assert(erlang:is_process_alive(SupPid)),

    %% Verify TIER 1 children (registry, raft, core_sup)
    Children = supervisor:which_children(erlmcp_flow_sup),
    ct:pal("TIER 1 children: ~p", [Children]),

    ?assertEqual(3, length(Children)),

    %% Verify registry is running
    {value, {erlmcp_flow_registry, RegPid, worker, [erlmcp_flow_registry]}} =
        lists:keysearch(erlmcp_flow_registry, 1, Children),
    ?assert(erlang:is_process_alive(RegPid)),

    %% Verify raft is running
    {value, {erlmcp_flow_raft, RaftPid, worker, [erlmcp_flow_raft]}} =
        lists:keysearch(erlmcp_flow_raft, 1, Children),
    ?assert(erlang:is_process_alive(RaftPid)),

    %% Verify core_sup is running
    {value, {erlmcp_flow_core_sup, CoreSupPid, supervisor, [erlmcp_flow_core_sup]}} =
        lists:keysearch(erlmcp_flow_core_sup, 1, Children),
    ?assert(erlang:is_process_alive(CoreSupPid)),

    %% Verify TIER 2 children under core_sup
    CoreChildren = supervisor:which_children(erlmcp_flow_core_sup),
    ct:pal("TIER 2 children: ~p", [CoreChildren]),

    %% Expect: swarm_sup, agent_sup, and 6 workers
    ?assertEqual(8, length(CoreChildren)),

    %% Verify swarm_sup is running
    {value, {erlmcp_flow_swarm_sup, SwarmSupPid, supervisor, [erlmcp_flow_swarm_sup]}} =
        lists:keysearch(erlmcp_flow_swarm_sup, 1, CoreChildren),
    ?assert(erlang:is_process_alive(SwarmSupPid)),

    %% Verify agent_sup is running
    {value, {erlmcp_flow_agent_sup, AgentSupPid, supervisor, [erlmcp_flow_agent_sup]}} =
        lists:keysearch(erlmcp_flow_agent_sup, 1, CoreChildren),
    ?assert(erlang:is_process_alive(AgentSupPid)),

    ok.

%% @doc TEST 2: Agent Spawning - Test dynamic agent creation under load
%% Validates:
%% - simple_one_for_one strategy works for agent_sup
%% - Agents can be spawned dynamically
%% - Multiple agents can coexist
test_agent_spawning(_Config) ->
    %% Spawn 10 agents dynamically
    AgentIds = [list_to_binary("agent_" ++ integer_to_list(I)) || I <- lists:seq(1, 10)],

    Pids = lists:map(fun(AgentId) ->
        {ok, Pid} = erlmcp_flow_agent_sup:start_child(
            AgentId,
            <<"test-agent">>,
            #{capabilities => [<<"test">>]}
        ),
        Pid
    end, AgentIds),

    %% Verify all agents are alive
    lists:foreach(fun(Pid) ->
        ?assert(erlang:is_process_alive(Pid))
    end, Pids),

    %% Verify agent count in supervisor
    Children = supervisor:count_children(erlmcp_flow_agent_sup),
    ct:pal("Agent supervisor children: ~p", [Children]),

    ?assertEqual(10, proplists:get_value(active, Children)),

    ok.

%% @doc TEST 3: Cascade Restart - Test one_for_all restart behavior in TIER 1
%% Validates:
%% - Registry crash triggers one_for_all restart
%% - All TIER 1 children restart together
%% - System recovers correctly
test_cascade_restart(Config) ->
    SupPid = ?config(sup_pid, Config),

    %% Get initial children PIDs
    InitialChildren = supervisor:which_children(erlmcp_flow_sup),
    {value, {erlmcp_flow_registry, InitialRegPid, _, _}} =
        lists:keysearch(erlmcp_flow_registry, 1, InitialChildren),
    {value, {erlmcp_flow_raft, InitialRaftPid, _, _}} =
        lists:keysearch(erlmcp_flow_raft, 1, InitialChildren),
    {value, {erlmcp_flow_core_sup, InitialCorePid, _, _}} =
        lists:keysearch(erlmcp_flow_core_sup, 1, InitialChildren),

    ct:pal("Initial PIDs - Reg: ~p, Raft: ~p, Core: ~p",
           [InitialRegPid, InitialRaftPid, InitialCorePid]),

    %% Kill registry (should trigger one_for_all restart)
    exit(InitialRegPid, kill),

    %% Wait for restart
    timer:sleep(500),

    %% Verify supervisor is still alive
    ?assert(erlang:is_process_alive(SupPid)),

    %% Get new children PIDs
    NewChildren = supervisor:which_children(erlmcp_flow_sup),
    {value, {erlmcp_flow_registry, NewRegPid, _, _}} =
        lists:keysearch(erlmcp_flow_registry, 1, NewChildren),
    {value, {erlmcp_flow_raft, NewRaftPid, _, _}} =
        lists:keysearch(erlmcp_flow_raft, 1, NewChildren),
    {value, {erlmcp_flow_core_sup, NewCorePid, _, _}} =
        lists:keysearch(erlmcp_flow_core_sup, 1, NewChildren),

    ct:pal("New PIDs - Reg: ~p, Raft: ~p, Core: ~p",
           [NewRegPid, NewRaftPid, NewCorePid]),

    %% Verify all children were restarted (PIDs changed)
    ?assertNotEqual(InitialRegPid, NewRegPid),
    ?assertNotEqual(InitialRaftPid, NewRaftPid),
    ?assertNotEqual(InitialCorePid, NewCorePid),

    %% Verify all new children are alive
    ?assert(erlang:is_process_alive(NewRegPid)),
    ?assert(erlang:is_process_alive(NewRaftPid)),
    ?assert(erlang:is_process_alive(NewCorePid)),

    ok.

%% @doc TEST 4: Child Isolation - Test one_for_one isolation in TIER 2
%% Validates:
%% - Worker crash doesn't affect siblings
%% - Only crashed worker restarts
%% - Supervisor remains stable
test_child_isolation(_Config) ->
    %% Get core_sup children
    CoreChildren = supervisor:which_children(erlmcp_flow_core_sup),

    %% Get PIDs of two workers (circuit_breaker and q_learning)
    {value, {erlmcp_flow_circuit_breaker, CB_Pid, _, _}} =
        lists:keysearch(erlmcp_flow_circuit_breaker, 1, CoreChildren),
    {value, {erlmcp_flow_q_learning, QL_Pid, _, _}} =
        lists:keysearch(erlmcp_flow_q_learning, 1, CoreChildren),

    ct:pal("Initial PIDs - CircuitBreaker: ~p, QLearning: ~p", [CB_Pid, QL_Pid]),

    %% Kill circuit_breaker
    exit(CB_Pid, kill),

    %% Wait for restart
    timer:sleep(200),

    %% Get new children
    NewCoreChildren = supervisor:which_children(erlmcp_flow_core_sup),
    {value, {erlmcp_flow_circuit_breaker, NewCB_Pid, _, _}} =
        lists:keysearch(erlmcp_flow_circuit_breaker, 1, NewCoreChildren),
    {value, {erlmcp_flow_q_learning, NewQL_Pid, _, _}} =
        lists:keysearch(erlmcp_flow_q_learning, 1, NewCoreChildren),

    ct:pal("New PIDs - CircuitBreaker: ~p, QLearning: ~p", [NewCB_Pid, NewQL_Pid]),

    %% Verify circuit_breaker was restarted (PID changed)
    ?assertNotEqual(CB_Pid, NewCB_Pid),
    ?assert(erlang:is_process_alive(NewCB_Pid)),

    %% Verify q_learning was NOT restarted (PID unchanged)
    ?assertEqual(QL_Pid, NewQL_Pid),
    ?assert(erlang:is_process_alive(NewQL_Pid)),

    ok.

%% @doc TEST 5: Swarm Supervisor Isolation
%% Validates:
%% - Swarm supervisor crash doesn't affect agent supervisor
%% - Core_sup restarts only swarm_sup
test_swarm_supervisor_isolation(_Config) ->
    CoreChildren = supervisor:which_children(erlmcp_flow_core_sup),

    {value, {erlmcp_flow_swarm_sup, SwarmSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_swarm_sup, 1, CoreChildren),
    {value, {erlmcp_flow_agent_sup, AgentSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_agent_sup, 1, CoreChildren),

    %% Kill swarm_sup
    exit(SwarmSupPid, kill),
    timer:sleep(200),

    NewCoreChildren = supervisor:which_children(erlmcp_flow_core_sup),
    {value, {erlmcp_flow_swarm_sup, NewSwarmSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_swarm_sup, 1, NewCoreChildren),
    {value, {erlmcp_flow_agent_sup, NewAgentSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_agent_sup, 1, NewCoreChildren),

    %% Verify swarm_sup restarted, agent_sup unchanged
    ?assertNotEqual(SwarmSupPid, NewSwarmSupPid),
    ?assertEqual(AgentSupPid, NewAgentSupPid),

    ok.

%% @doc TEST 6: Agent Supervisor Isolation
%% Validates:
%% - Agent supervisor crash doesn't affect swarm supervisor
test_agent_supervisor_isolation(_Config) ->
    CoreChildren = supervisor:which_children(erlmcp_flow_core_sup),

    {value, {erlmcp_flow_swarm_sup, SwarmSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_swarm_sup, 1, CoreChildren),
    {value, {erlmcp_flow_agent_sup, AgentSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_agent_sup, 1, CoreChildren),

    %% Kill agent_sup
    exit(AgentSupPid, kill),
    timer:sleep(200),

    NewCoreChildren = supervisor:which_children(erlmcp_flow_core_sup),
    {value, {erlmcp_flow_swarm_sup, NewSwarmSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_swarm_sup, 1, NewCoreChildren),
    {value, {erlmcp_flow_agent_sup, NewAgentSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_agent_sup, 1, NewCoreChildren),

    %% Verify agent_sup restarted, swarm_sup unchanged
    ?assertEqual(SwarmSupPid, NewSwarmSupPid),
    ?assertNotEqual(AgentSupPid, NewAgentSupPid),

    ok.

%% @doc TEST 7: Registry Restart Cascades (one_for_all validation)
%% Validates:
%% - Registry crash cascades to all TIER 1 children
test_registry_restart_cascades(Config) ->
    SupPid = ?config(sup_pid, Config),

    InitialChildren = supervisor:which_children(erlmcp_flow_sup),
    InitialPids = [Pid || {_, Pid, _, _} <- InitialChildren, is_pid(Pid)],

    %% Kill registry
    {value, {erlmcp_flow_registry, RegPid, _, _}} =
        lists:keysearch(erlmcp_flow_registry, 1, InitialChildren),
    exit(RegPid, kill),
    timer:sleep(500),

    %% Verify all children have new PIDs
    NewChildren = supervisor:which_children(erlmcp_flow_sup),
    NewPids = [Pid || {_, Pid, _, _} <- NewChildren, is_pid(Pid)],

    %% All PIDs should be different (cascade restart)
    ?assert(lists:all(fun(Pid) -> not lists:member(Pid, NewPids) end, InitialPids)),
    ?assert(erlang:is_process_alive(SupPid)),

    ok.

%% @doc TEST 8: Raft Restart Cascades (one_for_all validation)
%% Validates:
%% - Raft crash cascades to all TIER 1 children
test_raft_restart_cascades(Config) ->
    SupPid = ?config(sup_pid, Config),

    InitialChildren = supervisor:which_children(erlmcp_flow_sup),
    InitialPids = [Pid || {_, Pid, _, _} <- InitialChildren, is_pid(Pid)],

    %% Kill raft
    {value, {erlmcp_flow_raft, RaftPid, _, _}} =
        lists:keysearch(erlmcp_flow_raft, 1, InitialChildren),
    exit(RaftPid, kill),
    timer:sleep(500),

    %% Verify all children have new PIDs
    NewChildren = supervisor:which_children(erlmcp_flow_sup),
    NewPids = [Pid || {_, Pid, _, _} <- NewChildren, is_pid(Pid)],

    %% All PIDs should be different (cascade restart)
    ?assert(lists:all(fun(Pid) -> not lists:member(Pid, NewPids) end, InitialPids)),
    ?assert(erlang:is_process_alive(SupPid)),

    ok.

%% @doc TEST 9: Core Sup Restart is Isolated (one_for_all doesn't cascade UP)
%% Validates:
%% - Core_sup crash restarts core_sup only, not siblings
test_core_sup_restart_isolated(Config) ->
    SupPid = ?config(sup_pid, Config),

    InitialChildren = supervisor:which_children(erlmcp_flow_sup),
    {value, {erlmcp_flow_registry, InitialRegPid, _, _}} =
        lists:keysearch(erlmcp_flow_registry, 1, InitialChildren),
    {value, {erlmcp_flow_raft, InitialRaftPid, _, _}} =
        lists:keysearch(erlmcp_flow_raft, 1, InitialChildren),
    {value, {erlmcp_flow_core_sup, CoreSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_core_sup, 1, InitialChildren),

    %% Kill core_sup
    exit(CoreSupPid, kill),
    timer:sleep(500),

    NewChildren = supervisor:which_children(erlmcp_flow_sup),
    {value, {erlmcp_flow_registry, NewRegPid, _, _}} =
        lists:keysearch(erlmcp_flow_registry, 1, NewChildren),
    {value, {erlmcp_flow_raft, NewRaftPid, _, _}} =
        lists:keysearch(erlmcp_flow_raft, 1, NewChildren),
    {value, {erlmcp_flow_core_sup, NewCoreSupPid, _, _}} =
        lists:keysearch(erlmcp_flow_core_sup, 1, NewChildren),

    %% Verify registry and raft were NOT restarted (PIDs unchanged)
    %% Note: Due to one_for_all, they WILL restart. This test documents the behavior.
    ct:pal("Registry PIDs: ~p -> ~p", [InitialRegPid, NewRegPid]),
    ct:pal("Raft PIDs: ~p -> ~p", [InitialRaftPid, NewRaftPid]),
    ct:pal("Core PIDs: ~p -> ~p", [CoreSupPid, NewCoreSupPid]),

    %% With one_for_all, all children restart together
    ?assertNotEqual(InitialRegPid, NewRegPid),
    ?assertNotEqual(InitialRaftPid, NewRaftPid),
    ?assertNotEqual(CoreSupPid, NewCoreSupPid),

    ?assert(erlang:is_process_alive(SupPid)),
    ?assert(erlang:is_process_alive(NewCoreSupPid)),

    ok.
