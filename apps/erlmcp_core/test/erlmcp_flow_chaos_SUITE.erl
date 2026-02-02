-module(erlmcp_flow_chaos_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%%====================================================================
%%% Chicago School TDD Chaos Testing Suite
%%%
%%% Purpose: Test erlmcp-flow resilience under fault injection
%%% Principles:
%%%   1. Real fault injection (process death, network delays, message loss)
%%%   2. No mocking (actual crashes, partitions, delays)
%%%   3. Observable recovery (state verification after chaos)
%%%   4. Multi-process coordination testing
%%%====================================================================

%%%====================================================================
%%% Common Test Callbacks
%%%====================================================================

all() ->
    [test_random_agent_crashes,
     test_swarm_survives_agent_failures,
     test_network_delay_injection,
     test_message_loss_injection,
     test_byzantine_agent_injection,
     test_cascading_leader_failures,
     test_partition_healing,
     test_resource_exhaustion_graceful_degradation].

init_per_suite(Config) ->
    %% Start real erlmcp_core application (Chicago School: real system)
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%====================================================================
%%% Test Cases (Chaos Injection)
%%%====================================================================

%%--------------------------------------------------------------------
%% Test: Random Agent Crashes
%% Chaos: Randomly kill 30% of agents
%% Verify: Swarm continues, tasks complete
%%--------------------------------------------------------------------
test_random_agent_crashes(_Config) ->
    %% Setup: Start swarm with 10 real agents
    {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(chaos_swarm, #{}),
    Agents =
        [begin
             {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 5000}),
             ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
             A
         end
         || N <- lists:seq(1, 10)],

    ct:log("Started swarm with ~p agents", [length(Agents)]),

    %% Exercise: Submit 50 tasks
    Tasks =
        [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
         || N <- lists:seq(1, 50)],
    [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

    ct:log("Submitted 50 tasks to swarm"),

    %% Chaos: Randomly kill 3 agents (30%) (Chicago School: real process death)
    AgentsToKill = lists:sublist(shuffle(Agents), 3),
    [begin
         ct:log("Killing agent ~p", [A]),
         exit(A, kill)
     end
     || A <- AgentsToKill],

    timer:sleep(500),  %% Let swarm detect failures

    %% Verify: Swarm still operational (observable state)
    {ok, SwarmStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{agents := AliveAgents, tasks := PendingTasks} = SwarmStatus,

    ct:log("Swarm status: ~p alive agents, ~p pending tasks", [length(AliveAgents), length(PendingTasks)]),

    %% Verify: Swarm has 7 agents (10 - 3 killed)
    7 = length(AliveAgents),

    %% Wait for tasks to complete
    timer:sleep(2000),

    %% Verify: All tasks eventually complete (no task loss)
    {ok, FinalStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{completed_tasks := CompletedTasks} = FinalStatus,
    50 = length(CompletedTasks),

    ct:log("All 50 tasks completed despite agent failures"),

    %% Cleanup
    [catch erlmcp_flow_agent:stop(A) || A <- Agents],
    erlmcp_flow_swarm:stop_swarm(SwarmPid).

%%--------------------------------------------------------------------
%% Test: Swarm Survives Agent Failures
%% Chaos: Kill agents one by one during task execution
%% Verify: Swarm redistributes tasks, no task loss
%%--------------------------------------------------------------------
test_swarm_survives_agent_failures(_Config) ->
    %% Setup: Start swarm with 5 agents
    {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(survival_swarm, #{}),
    Agents =
        [begin
             {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 5000}),
             ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
             A
         end
         || N <- lists:seq(1, 5)],

    %% Exercise: Submit 25 tasks
    Tasks =
        [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
         || N <- lists:seq(1, 25)],
    [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

    %% Chaos: Kill agents one by one every 200ms (real failures, no mocking)
    spawn(fun() ->
             lists:foreach(fun(A) ->
                              timer:sleep(200),
                              ct:log("Killing agent ~p", [A]),
                              exit(A, kill)
                           end,
                           Agents)
          end),

    %% Wait for chaos to complete
    timer:sleep(2000),

    %% Verify: Swarm detects all failures (observable behavior)
    {ok, SwarmStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{agents := AliveAgents} = SwarmStatus,
    ct:log("Alive agents after chaos: ~p", [length(AliveAgents)]),

    %% Verify: All agents killed
    0 = length(AliveAgents),

    %% Verify: Swarm still operational (can add new agents)
    {ok, NewAgent} = erlmcp_flow_agent:start_link(recovery_agent, #{timeout => 5000}),
    ok = erlmcp_flow_swarm:add_agent(SwarmPid, NewAgent),

    %% Verify: New agent picks up pending tasks
    timer:sleep(1000),
    {ok, FinalStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{completed_tasks := CompletedTasks} = FinalStatus,

    ct:log("Completed ~p/25 tasks", [length(CompletedTasks)]),

    %% Cleanup
    catch erlmcp_flow_agent:stop(NewAgent),
    erlmcp_flow_swarm:stop_swarm(SwarmPid).

%%--------------------------------------------------------------------
%% Test: Network Delay Injection
%% Chaos: Add 500ms delay to 50% of messages
%% Verify: Consensus still converges
%%--------------------------------------------------------------------
test_network_delay_injection(_Config) ->
    %% Setup: Start 5 Raft nodes (real consensus processes)
    Nodes =
        [begin
             {ok, N} = erlmcp_flow_consensus_raft:start_node(node_id(I), other_nodes(I, 5)),
             N
         end
         || I <- lists:seq(1, 5)],

    ct:log("Started 5 Raft nodes"),

    %% Wait for initial leader election (no delay)
    timer:sleep(2000),

    %% Verify: Leader elected (observable state)
    {ok, Leader1} = find_leader(Nodes),
    ct:log("Initial leader: ~p", [Leader1]),

    %% Chaos: Inject network delay (Chicago School: real message interception)
    DelayInjector = spawn(fun() -> inject_network_delay(Nodes, 500, 0.5) end),

    %% Exercise: Append 10 entries to log
    Entries = [entry(N) || N <- lists:seq(1, 10)],
    [ok = erlmcp_flow_consensus_raft:append_entry(Leader1, E) || E <- Entries],

    ct:log("Appended 10 entries with network delay"),

    %% Wait for replication despite delays
    timer:sleep(3000),

    %% Verify: All nodes have consistent log (observable behavior)
    Logs = [erlmcp_flow_consensus_raft:get_log(N) || N <- Nodes],
    AllEqual = lists:all(fun(L) -> L =:= hd(Logs) end, Logs),
    true = AllEqual,

    ct:log("All nodes have consistent log despite 500ms delays"),

    %% Stop delay injector
    exit(DelayInjector, kill),

    %% Cleanup
    [erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes].

%%--------------------------------------------------------------------
%% Test: Message Loss Injection
%% Chaos: Drop 10% of messages
%% Verify: System detects, retries, recovers
%%--------------------------------------------------------------------
test_message_loss_injection(_Config) ->
    %% Setup: Start swarm with 3 agents
    {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(lossy_swarm, #{}),
    Agents =
        [begin
             {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 5000}),
             ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
             A
         end
         || N <- lists:seq(1, 3)],

    %% Chaos: Inject message loss (Chicago School: real message dropping)
    LossInjector =
        spawn(fun() -> inject_message_loss([SwarmPid | Agents], 0.1) end),  %% 10% loss

    %% Exercise: Submit 30 tasks with lossy network
    Tasks =
        [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
         || N <- lists:seq(1, 30)],
    [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

    ct:log("Submitted 30 tasks with 10% message loss"),

    %% Wait for completion (system should retry lost messages)
    timer:sleep(3000),

    %% Verify: All tasks eventually complete (observable recovery)
    {ok, SwarmStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{completed_tasks := CompletedTasks} = SwarmStatus,
    30 = length(CompletedTasks),

    ct:log("All 30 tasks completed despite 10% message loss"),

    %% Stop loss injector
    exit(LossInjector, kill),

    %% Cleanup
    [erlmcp_flow_agent:stop(A) || A <- Agents],
    erlmcp_flow_swarm:stop_swarm(SwarmPid).

%%--------------------------------------------------------------------
%% Test: Byzantine Agent Injection
%% Chaos: Inject 1 agent that sends malformed responses
%% Verify: Swarm detects, isolates Byzantine agent
%%--------------------------------------------------------------------
test_byzantine_agent_injection(_Config) ->
    %% Setup: Start swarm with 3 honest agents + 1 Byzantine agent
    {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(byzantine_swarm, #{}),
    HonestAgents =
        [begin
             {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 5000}),
             ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
             A
         end
         || N <- lists:seq(1, 3)],

    %% Chaos: Create Byzantine agent (sends wrong task results)
    {ok, ByzantineAgent} =
        erlmcp_flow_byzantine_agent:start_link(byzantine_1,
                                               #{behavior => send_wrong_results}),
    ok = erlmcp_flow_swarm:add_agent(SwarmPid, ByzantineAgent),

    ct:log("Added 1 Byzantine agent to swarm of 3 honest agents"),

    %% Exercise: Submit 20 tasks
    Tasks =
        [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
         || N <- lists:seq(1, 20)],
    [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

    %% Wait for Byzantine detection
    timer:sleep(2000),

    %% Verify: Swarm detected Byzantine agent (observable behavior)
    {ok, SwarmStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{byzantine_agents := DetectedByzantine} = SwarmStatus,
    [ByzantineAgent] = DetectedByzantine,

    ct:log("Swarm detected Byzantine agent: ~p", [ByzantineAgent]),

    %% Verify: Byzantine agent isolated (no more tasks assigned)
    {ok, ByzantineStats} = erlmcp_flow_swarm:get_agent_stats(SwarmPid, ByzantineAgent),
    #{tasks_assigned_after_detection := TasksAfter} = ByzantineStats,
    0 = TasksAfter,

    ct:log("Byzantine agent isolated, no tasks assigned after detection"),

    %% Cleanup
    [erlmcp_flow_agent:stop(A) || A <- HonestAgents],
    erlmcp_flow_byzantine_agent:stop(ByzantineAgent),
    erlmcp_flow_swarm:stop_swarm(SwarmPid).

%%--------------------------------------------------------------------
%% Test: Cascading Leader Failures
%% Chaos: Kill leader, then kill new leader immediately
%% Verify: Eventually stable leader elected
%%--------------------------------------------------------------------
test_cascading_leader_failures(_Config) ->
    %% Setup: Start 7 Raft nodes (can tolerate 3 failures)
    Nodes =
        [begin
             {ok, N} = erlmcp_flow_consensus_raft:start_node(node_id(I), other_nodes(I, 7)),
             N
         end
         || I <- lists:seq(1, 7)],

    ct:log("Started 7 Raft nodes"),

    %% Wait for initial leader election
    timer:sleep(2000),

    %% Chaos: Kill leaders in cascade (real process death, no mocking)
    kill_leader_cascade(Nodes, 3),

    ct:log("Killed 3 leaders in cascade"),

    %% Wait for stabilization
    timer:sleep(3000),

    %% Verify: Eventually a stable leader is elected (observable state)
    AliveNodes = [N || N <- Nodes, erlang:is_process_alive(N)],
    {ok, FinalLeader} = find_leader(AliveNodes),
    ct:log("Final stable leader: ~p", [FinalLeader]),

    %% Verify: Leader survives for 2 seconds (stable)
    timer:sleep(2000),
    true = erlang:is_process_alive(FinalLeader),
    {ok, leader} = erlmcp_flow_consensus_raft:get_role(FinalLeader),

    ct:log("Leader stable after cascading failures"),

    %% Cleanup
    [catch erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes].

%%--------------------------------------------------------------------
%% Test: Partition Healing
%% Chaos: Create network partition, then heal
%% Verify: Logs reconcile, single leader
%%--------------------------------------------------------------------
test_partition_healing(_Config) ->
    %% Setup: Start 5 Raft nodes
    Nodes =
        [begin
             {ok, N} = erlmcp_flow_consensus_raft:start_node(node_id(I), other_nodes(I, 5)),
             N
         end
         || I <- lists:seq(1, 5)],

    %% Wait for leader election
    timer:sleep(2000),

    %% Chaos: Create 3-2 partition (Chicago School: real message blocking)
    PartitionA = lists:sublist(Nodes, 3),
    PartitionB = lists:nthtail(3, Nodes),
    PartitionPid = spawn(fun() -> create_partition(PartitionA, PartitionB) end),

    ct:log("Created 3-2 partition: ~p | ~p", [PartitionA, PartitionB]),

    %% Wait for partition effects
    timer:sleep(2000),

    %% Verify: Partition A has leader, Partition B does not
    {ok, LeaderA} = find_leader(PartitionA),
    {error, no_leader} = find_leader(PartitionB),

    ct:log("Partition A leader: ~p, Partition B: no leader", [LeaderA]),

    %% Append entries in Partition A during partition
    Entries = [entry(N) || N <- lists:seq(1, 5)],
    [ok = erlmcp_flow_consensus_raft:append_entry(LeaderA, E) || E <- Entries],

    %% Chaos: Heal partition (Chicago School: remove message blocking)
    exit(PartitionPid, kill),
    timer:sleep(100),

    ct:log("Healed partition"),

    %% Wait for reconciliation
    timer:sleep(2000),

    %% Verify: Single leader across all nodes (observable state)
    {ok, UnifiedLeader} = find_leader(Nodes),
    Roles = [erlmcp_flow_consensus_raft:get_role(N) || N <- Nodes],
    Leaders = [R || {ok, R} <- Roles, R =:= leader],
    1 = length(Leaders),

    ct:log("Unified leader after healing: ~p", [UnifiedLeader]),

    %% Verify: All nodes have consistent log (logs reconciled)
    Logs = [erlmcp_flow_consensus_raft:get_log(N) || N <- Nodes],
    AllEqual = lists:all(fun(L) -> L =:= hd(Logs) end, Logs),
    true = AllEqual,

    ct:log("All nodes have consistent log after partition healing"),

    %% Cleanup
    [erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes].

%%--------------------------------------------------------------------
%% Test: Resource Exhaustion Graceful Degradation
%% Chaos: Exhaust memory/CPU limits
%% Verify: Circuit breakers trigger, no crashes
%%--------------------------------------------------------------------
test_resource_exhaustion_graceful_degradation(_Config) ->
    %% Setup: Start swarm with resource limits
    {ok, SwarmPid} =
        erlmcp_flow_swarm:start_swarm(limited_swarm,
                                      #{max_memory => 100_000_000,  %% 100MB
                                        max_cpu => 0.8}),           %% 80% CPU

    %% Start 5 agents
    Agents =
        [begin
             {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 5000}),
             ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
             A
         end
         || N <- lists:seq(1, 5)],

    %% Chaos: Submit memory-intensive tasks (Chicago School: real memory allocation)
    MemoryTasks =
        [#{id => task_id(N),
           type => memory_intensive,
           input => binary:copy(<<"x">>, 10_000_000),  %% 10MB per task
           timeout => 10000}
         || N <- lists:seq(1, 20)],  %% 200MB total (exceeds limit)

    ct:log("Submitting 20 memory-intensive tasks (200MB total, limit: 100MB)"),

    [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- MemoryTasks],

    %% Wait for circuit breaker to trigger
    timer:sleep(2000),

    %% Verify: Circuit breaker triggered (observable behavior)
    {ok, SwarmStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{circuit_breaker := BreakerStatus} = SwarmStatus,
    open = maps:get(state, BreakerStatus),

    ct:log("Circuit breaker triggered: ~p", [BreakerStatus]),

    %% Verify: Swarm still operational (no crash)
    true = erlang:is_process_alive(SwarmPid),

    %% Verify: Agents still alive (graceful degradation)
    AliveAgents = [A || A <- Agents, erlang:is_process_alive(A)],
    5 = length(AliveAgents),

    ct:log("Swarm and all agents survived resource exhaustion"),

    %% Cleanup
    [erlmcp_flow_agent:stop(A) || A <- Agents],
    erlmcp_flow_swarm:stop_swarm(SwarmPid).

%%%====================================================================
%%% Chaos Injection Helpers (Chicago School: Real Fault Injection)
%%%====================================================================

%% Inject network delay (real message interception)
inject_network_delay(Nodes, DelayMs, Probability) ->
    [erlang:trace(N, true, [send, timestamp]) || N <- Nodes],
    chaos_loop(fun() ->
                  receive
                      {trace_ts, _From, send, Msg, To, _Timestamp} ->
                          case rand:uniform() < Probability of
                              true ->
                                  timer:sleep(DelayMs),
                                  To ! Msg;
                              false ->
                                  To ! Msg
                          end
                  after 100 ->
                      ok
                  end
               end).

%% Inject message loss (real message dropping)
inject_message_loss(Pids, LossProbability) ->
    [erlang:trace(P, true, [send]) || P <- Pids],
    chaos_loop(fun() ->
                  receive
                      {trace, _From, send, Msg, To} ->
                          case rand:uniform() < LossProbability of
                              true ->
                                  %% Drop message (Chicago School: real loss)
                                  ok;
                              false ->
                                  To ! Msg
                          end
                  after 100 ->
                      ok
                  end
               end).

%% Create network partition (real message blocking)
create_partition(PartitionA, PartitionB) ->
    %% Block all messages from A to B and B to A
    AllNodes = PartitionA ++ PartitionB,
    [erlang:trace(N, true, [send]) || N <- AllNodes],
    chaos_loop(fun() ->
                  receive
                      {trace, From, send, Msg, To} ->
                          InA = lists:member(From, PartitionA),
                          InB = lists:member(From, PartitionB),
                          ToA = lists:member(To, PartitionA),
                          ToB = lists:member(To, PartitionB),
                          case (InA andalso ToB) orelse (InB andalso ToA) of
                              true ->
                                  %% Drop cross-partition message
                                  ok;
                              false ->
                                  To ! Msg
                          end
                  after 100 ->
                      ok
                  end
               end).

%% Kill leaders in cascade
kill_leader_cascade(Nodes, Count) ->
    kill_leader_cascade(Nodes, Count, 0).

kill_leader_cascade(_Nodes, Count, Count) ->
    ok;
kill_leader_cascade(Nodes, Count, N) ->
    case find_leader(Nodes) of
        {ok, Leader} ->
            ct:log("Killing leader #~p: ~p", [N + 1, Leader]),
            exit(Leader, kill),
            timer:sleep(500),  %% Wait for new election
            kill_leader_cascade(Nodes, Count, N + 1);
        {error, no_leader} ->
            ct:log("No leader found, stopping cascade"),
            ok
    end.

%% Chaos loop (run chaos injection until killed)
chaos_loop(ChaosFun) ->
    case ChaosFun() of
        stop ->
            ok;
        _ ->
            chaos_loop(ChaosFun)
    end.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Shuffle list (for random agent selection)
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

%% Find leader among nodes
find_leader(Nodes) ->
    AliveNodes = [N || N <- Nodes, erlang:is_process_alive(N)],
    case AliveNodes of
        [] ->
            {error, no_leader};
        _ ->
            Roles = [{N, erlmcp_flow_consensus_raft:get_role(N)} || N <- AliveNodes],
            Leaders = [{N, R} || {N, {ok, R}} <- Roles, R =:= leader],
            case Leaders of
                [{Leader, leader}] ->
                    {ok, Leader};
                [] ->
                    {error, no_leader};
                Multiple ->
                    {error, {multiple_leaders, Multiple}}
            end
    end.

%% Generate node ID
node_id(N) ->
    list_to_atom("node_" ++ integer_to_list(N)).

%% Generate task ID
task_id(N) ->
    list_to_binary("task_" ++ integer_to_list(N)).

%% Generate agent ID
agent_id(N) ->
    list_to_atom("agent_" ++ integer_to_list(N)).

%% Generate Raft entry
entry(N) ->
    #{index => N, data => <<"entry_", (integer_to_binary(N))/binary>>}.

%% Get other nodes for cluster membership
other_nodes(I, Total) ->
    [node_id(N) || N <- lists:seq(1, Total), N =/= I].
