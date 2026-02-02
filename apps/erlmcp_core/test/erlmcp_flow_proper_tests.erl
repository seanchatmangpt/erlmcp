-module(erlmcp_flow_proper_tests).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Property-Based Testing for erlmcp-flow
%%%
%%% Purpose: Generative testing for consensus invariants
%%% Principles:
%%%   1. Generate random test cases (Proper)
%%%   2. Verify invariants hold across all cases
%%%   3. Chicago School: Real processes, observable state
%%%   4. Automatic shrinking for minimal failing cases
%%%====================================================================

%%%====================================================================
%%% Property Test Entry Points (EUnit Integration)
%%%====================================================================

%% Run all properties via EUnit
proper_test_() ->
    {timeout,
     300,  %% 5 minutes for property tests
     [{"Leader Election Invariant", ?_assert(proper:quickcheck(prop_leader_election_invariant(), [{numtests, 100}]))},
      {"Log Consistency Invariant", ?_assert(proper:quickcheck(prop_log_consistency(), [{numtests, 100}]))},
      {"Byzantine Resilience (3f+1)", ?_assert(proper:quickcheck(prop_byzantine_resilience(), [{numtests, 50}]))},
      {"Gossip Eventual Consistency", ?_assert(proper:quickcheck(prop_gossip_eventual_consistency(), [{numtests, 50}]))},
      {"Task Completion Guarantee", ?_assert(proper:quickcheck(prop_all_tasks_complete(), [{numtests, 100}]))},
      {"Queue Depth Bounded", ?_assert(proper:quickcheck(prop_queue_depth_bounded(), [{numtests, 100}]))},
      {"Load Balancing Fairness", ?_assert(proper:quickcheck(prop_load_balancing_fairness(), [{numtests, 50}]))},
      {"Partition Tolerance (CAP)", ?_assert(proper:quickcheck(prop_partition_tolerance(), [{numtests, 30}]))}]}.

%%%====================================================================
%%% Property: Leader Election Invariant
%%%
%%% Invariant: At most 1 leader exists in any partition with quorum
%%% Generator: Random number of nodes, random failures
%%%====================================================================

prop_leader_election_invariant() ->
    ?FORALL({NumNodes, NumDown}, {range(3, 10), range(0, 5)},
        begin
            %% Setup: Start real Raft nodes (Chicago School: real gen_servers)
            Nodes = start_raft_cluster(NumNodes),

            %% Wait for initial election
            timer:sleep(2000),

            %% Chaos: Kill random nodes
            NodesToPick = min(NumDown, length(Nodes)),
            NodesToKill = lists:sublist(shuffle(Nodes), NodesToPick),
            [exit(N, kill) || N <- NodesToKill],

            %% Wait for re-election
            timer:sleep(2000),

            %% Verify: Leader election invariant (Chicago School: observable state)
            AliveNodes = [N || N <- Nodes, erlang:is_process_alive(N)],
            Roles = [begin
                {ok, Role} = erlmcp_flow_consensus_raft:get_role(N),
                Role
            end || N <- AliveNodes],

            Leaders = [R || R <- Roles, R =:= leader],
            Majority = length(AliveNodes) > (NumNodes div 2),

            %% Cleanup
            [catch erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes],

            %% Invariant: Exactly 1 leader if quorum, 0 otherwise
            case Majority of
                true -> length(Leaders) =:= 1;
                false -> length(Leaders) =:= 0
            end
        end).

%%%====================================================================
%%% Property: Log Consistency Invariant
%%%
%%% Invariant: All nodes with same committed index have same log entry
%%% Generator: Random entries, random replication delays
%%%====================================================================

prop_log_consistency() ->
    ?FORALL({Entries, NumNodes}, {list(entry()), range(3, 7)},
        begin
            %% Setup: Start Raft cluster
            Nodes = start_raft_cluster(NumNodes),

            %% Wait for leader election
            timer:sleep(2000),

            %% Find leader
            {ok, Leader} = find_leader(Nodes),

            %% Exercise: Append entries (Chicago School: real log replication)
            [ok = erlmcp_flow_consensus_raft:append_entry(Leader, E) || E <- Entries],

            %% Wait for replication
            timer:sleep(1000),

            %% Verify: Log consistency invariant (observable behavior)
            Logs = [{N, erlmcp_flow_consensus_raft:get_log(N)} || N <- Nodes],
            FirstLog = element(2, hd(Logs)),
            AllEqual = lists:all(fun({_N, L}) -> L =:= FirstLog end, Logs),

            %% Cleanup
            [erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes],

            %% Invariant: All logs are identical
            AllEqual
        end).

%%%====================================================================
%%% Property: Byzantine Resilience (3f+1)
%%%
%%% Invariant: 3f+1 nodes can tolerate f Byzantine faults
%%% Generator: Random f, random Byzantine behaviors
%%%====================================================================

prop_byzantine_resilience() ->
    ?FORALL(F, range(1, 2),  %% f=1 or f=2 (more would be too slow for tests)
        begin
            NumNodes = 3 * F + 1,

            %% Setup: Start Byzantine consensus nodes
            HonestNodes = start_byzantine_cluster(NumNodes - F, honest),
            ByzantineNodes = start_byzantine_cluster(F, byzantine),
            AllNodes = HonestNodes ++ ByzantineNodes,

            %% Wait for initial setup
            timer:sleep(2000),

            %% Exercise: Propose value (Chicago School: real Byzantine consensus)
            CorrectValue = <<"correct_value">>,
            ok = erlmcp_flow_consensus_byzantine:propose(hd(HonestNodes), CorrectValue),

            %% Wait for consensus
            timer:sleep(3000),

            %% Verify: Byzantine resilience invariant (observable decision)
            Decisions =
                [begin
                     case erlmcp_flow_consensus_byzantine:get_decision(N) of
                         {ok, Decision} ->
                             Decision;
                         {error, no_decision} ->
                             no_decision
                     end
                 end
                 || N <- HonestNodes],

            %% Cleanup
            [catch erlmcp_flow_consensus_byzantine:stop_node(N) || N <- AllNodes],

            %% Invariant: All honest nodes decide on correct value
            AllCorrect = lists:all(fun(D) -> D =:= CorrectValue end, Decisions),
            AllCorrect
        end).

%%%====================================================================
%%% Property: Gossip Eventual Consistency
%%%
%%% Invariant: All nodes eventually converge to same state
%%% Generator: Random updates, random gossip patterns
%%%====================================================================

prop_gossip_eventual_consistency() ->
    ?FORALL({NumNodes, Updates}, {range(5, 15), list(kv_update())},
        begin
            %% Setup: Start gossip cluster
            Nodes = start_gossip_cluster(NumNodes),

            %% Exercise: Random updates on random nodes (Chicago School: real gossip)
            [begin
                 RandomNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
                 ok = erlmcp_flow_consensus_gossip:update_local(RandomNode, K, V)
             end
             || {K, V} <- Updates],

            %% Wait for gossip convergence (eventual consistency)
            timer:sleep(5000),

            %% Verify: Eventual consistency invariant (observable state)
            AllStates = [erlmcp_flow_consensus_gossip:get_all_values(N) || N <- Nodes],
            FirstState = hd(AllStates),
            AllConverged = lists:all(fun(S) -> S =:= FirstState end, AllStates),

            %% Cleanup
            [erlmcp_flow_consensus_gossip:stop_node(N) || N <- Nodes],

            %% Invariant: All nodes have same state
            AllConverged
        end).

%%%====================================================================
%%% Property: Task Completion Guarantee
%%%
%%% Invariant: All assigned tasks eventually complete (no task loss)
%%% Generator: Random number of tasks, random agent failures
%%%====================================================================

prop_all_tasks_complete() ->
    ?FORALL({NumTasks, NumAgents, NumFailures},
            {range(10, 100), range(3, 10), range(0, 3)},
        begin
            %% Setup: Start swarm with agents
            {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(test_swarm, #{}),
            Agents =
                [begin
                     {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 10000}),
                     ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
                     A
                 end
                 || N <- lists:seq(1, NumAgents)],

            %% Exercise: Submit tasks
            Tasks =
                [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
                 || N <- lists:seq(1, NumTasks)],
            [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

            %% Chaos: Kill random agents (Chicago School: real failures)
            AgentsToKill = lists:sublist(shuffle(Agents), min(NumFailures, length(Agents))),
            [exit(A, kill) || A <- AgentsToKill],

            %% Wait for completion
            MaxWaitTime = NumTasks * 50 + 5000,  %% Dynamic timeout
            timer:sleep(MaxWaitTime),

            %% Verify: Task completion invariant (observable behavior)
            {ok, SwarmStatus} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
            #{completed_tasks := CompletedTasks, failed_tasks := FailedTasks} = SwarmStatus,

            %% Cleanup
            [catch erlmcp_flow_agent:stop(A) || A <- Agents],
            erlmcp_flow_swarm:stop_swarm(SwarmPid),

            %% Invariant: All tasks completed or failed (no task loss)
            TotalProcessed = length(CompletedTasks) + length(FailedTasks),
            TotalProcessed =:= NumTasks
        end).

%%%====================================================================
%%% Property: Queue Depth Bounded
%%%
%%% Invariant: Queue depth never exceeds max_queue_size
%%% Generator: Random tasks, random max_queue_size
%%%====================================================================

prop_queue_depth_bounded() ->
    ?FORALL({NumTasks, MaxQueueSize}, {range(10, 200), range(10, 100)},
        begin
            %% Setup: Start agent with queue limit
            {ok, AgentPid} =
                erlmcp_flow_agent:start_link(test_agent, #{max_queue_size => MaxQueueSize}),

            %% Exercise: Submit tasks (may exceed limit)
            Tasks =
                [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
                 || N <- lists:seq(1, NumTasks)],
            [erlmcp_flow_agent:assign_task(AgentPid, T) || T <- Tasks],

            %% Verify: Queue depth invariant (observable state)
            {ok, QueueDepth} = erlmcp_flow_agent:get_queue_depth(AgentPid),

            %% Cleanup
            erlmcp_flow_agent:stop(AgentPid),

            %% Invariant: Queue depth <= max_queue_size
            QueueDepth =< MaxQueueSize
        end).

%%%====================================================================
%%% Property: Load Balancing Fairness
%%%
%%% Invariant: Tasks distributed fairly across agents (within 20% variance)
%%% Generator: Random tasks, random number of agents
%%%====================================================================

prop_load_balancing_fairness() ->
    ?FORALL({NumTasks, NumAgents}, {range(50, 200), range(3, 10)},
        begin
            %% Setup: Start swarm with agents
            {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(test_swarm, #{}),
            Agents =
                [begin
                     {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{timeout => 10000}),
                     ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
                     A
                 end
                 || N <- lists:seq(1, NumAgents)],

            %% Exercise: Submit tasks
            Tasks =
                [#{id => task_id(N), type => compute, input => <<"data">>, timeout => 10000}
                 || N <- lists:seq(1, NumTasks)],
            [ok = erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

            %% Wait for distribution
            timer:sleep(1000),

            %% Verify: Load balancing fairness invariant (observable behavior)
            {ok, Stats} = erlmcp_flow_swarm:get_load_balance_stats(SwarmPid),
            #{agent_loads := Loads} = Stats,
            LoadCounts = [Count || {_Agent, Count} <- Loads],

            %% Cleanup
            [erlmcp_flow_agent:stop(A) || A <- Agents],
            erlmcp_flow_swarm:stop_swarm(SwarmPid),

            %% Invariant: Load variance within 20% of average
            case LoadCounts of
                [] ->
                    true;  %% No agents, trivially fair
                _ ->
                    AvgLoad = lists:sum(LoadCounts) / length(LoadCounts),
                    MaxDeviation = AvgLoad * 0.2,
                    lists:all(fun(C) -> abs(C - AvgLoad) =< MaxDeviation end, LoadCounts)
            end
        end).

%%%====================================================================
%%% Property: Partition Tolerance (CAP Theorem)
%%%
%%% Invariant: System remains available under partition
%%% Generator: Random partitions, random operations
%%%====================================================================

prop_partition_tolerance() ->
    ?FORALL({NumNodes, PartitionSize}, {range(5, 9), range(2, 4)},
        begin
            %% Setup: Start Raft cluster
            Nodes = start_raft_cluster(NumNodes),

            %% Wait for leader election
            timer:sleep(2000),

            %% Chaos: Create partition (Chicago School: real message blocking)
            PartitionA = lists:sublist(Nodes, PartitionSize),
            PartitionB = lists:nthtail(PartitionSize, Nodes),
            PartitionPid = spawn(fun() -> create_partition(PartitionA, PartitionB) end),

            %% Wait for partition effects
            timer:sleep(2000),

            %% Exercise: Try to append entry in majority partition
            MajorityPartition =
                case length(PartitionA) > length(PartitionB) of
                    true ->
                        PartitionA;
                    false ->
                        PartitionB
                end,

            Result =
                case find_leader(MajorityPartition) of
                    {ok, Leader} ->
                        erlmcp_flow_consensus_raft:append_entry(Leader, #{data => <<"test">>});
                    {error, no_leader} ->
                        {error, no_leader}
                end,

            %% Heal partition
            exit(PartitionPid, kill),
            timer:sleep(2000),

            %% Cleanup
            [erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes],

            %% Invariant: Majority partition remains available
            HasMajority = length(MajorityPartition) > (NumNodes div 2),
            case HasMajority of
                true ->
                    Result =:= ok;
                false ->
                    true  %% No majority, unavailability expected
            end
        end).

%%%====================================================================
%%% Generators (Proper)
%%%====================================================================

%% Generate Raft log entry
entry() ->
    ?LET({Index, Data}, {nat(), binary()},
        #{index => Index, data => Data}).

%% Generate key-value update
kv_update() ->
    ?LET({Key, Value}, {binary(), binary()}, {Key, Value}).

%% Generate task
task() ->
    ?LET({Id, Type, Input},
         {binary(), oneof([compute, io, network]), binary()},
        #{id => Id, type => Type, input => Input, timeout => 10000}).

%%%====================================================================
%%% Helper Functions (Chicago School: Real Process Management)
%%%====================================================================

%% Start Raft cluster (real gen_servers)
start_raft_cluster(NumNodes) ->
    [begin
         OtherNodes = [node_id(N) || N <- lists:seq(1, NumNodes), N =/= I],
         {ok, Node} = erlmcp_flow_consensus_raft:start_node(node_id(I), OtherNodes),
         Node
     end
     || I <- lists:seq(1, NumNodes)].

%% Start Byzantine consensus cluster
start_byzantine_cluster(NumNodes, Type) ->
    [begin
         IsByzantine = (Type =:= byzantine),
         {ok, Node} =
             erlmcp_flow_consensus_byzantine:start_node(node_id(I),
                                                        other_nodes(I, NumNodes),
                                                        IsByzantine),
         Node
     end
     || I <- lists:seq(1, NumNodes)].

%% Start gossip cluster
start_gossip_cluster(NumNodes) ->
    [begin
         OtherNodes = [node_id(N) || N <- lists:seq(1, NumNodes), N =/= I],
         {ok, Node} = erlmcp_flow_consensus_gossip:start_node(node_id(I), OtherNodes),
         Node
     end
     || I <- lists:seq(1, NumNodes)].

%% Find leader in cluster
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
                _Multiple ->
                    {error, multiple_leaders}
            end
    end.

%% Create network partition (real message blocking)
create_partition(PartitionA, PartitionB) ->
    AllNodes = PartitionA ++ PartitionB,
    [erlang:trace(N, true, [send]) || N <- AllNodes],
    partition_loop(PartitionA, PartitionB).

partition_loop(PartitionA, PartitionB) ->
    receive
        {trace, From, send, Msg, To} ->
            InA = lists:member(From, PartitionA),
            InB = lists:member(From, PartitionB),
            ToA = lists:member(To, PartitionA),
            ToB = lists:member(To, PartitionB),
            case (InA andalso ToB) orelse (InB andalso ToA) of
                true ->
                    ok;  %% Drop cross-partition message
                false ->
                    To ! Msg
            end,
            partition_loop(PartitionA, PartitionB)
    after 100 ->
        partition_loop(PartitionA, PartitionB)
    end.

%% Shuffle list
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

%% Generate node ID
node_id(N) ->
    list_to_atom("node_" ++ integer_to_list(N)).

%% Generate task ID
task_id(N) ->
    list_to_binary("task_" ++ integer_to_list(N)).

%% Generate agent ID
agent_id(N) ->
    list_to_atom("agent_" ++ integer_to_list(N)).

%% Get other nodes
other_nodes(I, Total) ->
    [node_id(N) || N <- lists:seq(1, Total), N =/= I].
