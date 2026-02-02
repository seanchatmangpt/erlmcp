%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_flow_router
%%% Week 2 Days 3-4: Router testing (4 test cases)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_router_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

router_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Register and lookup agent", fun test_register_lookup/0},
         {"Route task to agent", fun test_route_task/0},
         {"Multi-agent discovery", fun test_agent_list/0},
         {"Routing to non-existent agent", fun test_route_not_found/0},
         {"Load balancing with multiple agents", fun test_load_balancing/0}
     ]}.

setup() ->
    % Start registry if not running
    case whereis(erlmcp_flow_registry) of
        undefined ->
            {ok, Pid} = erlmcp_flow_registry:start_link(),
            Pid;
        Pid ->
            Pid
    end.

cleanup(Pid) ->
    % Clean up any registered agents
    case is_process_alive(Pid) of
        true -> ok;
        false -> ok
    end.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test 1: Register/lookup agent
test_register_lookup() ->
    AgentId = <<"test_agent_1">>,
    TestPid = spawn(fun() -> receive _ -> ok end end),

    % Register agent
    ?assertEqual(ok, erlmcp_flow_router:register_agent(AgentId, TestPid)),

    % Lookup agent
    ?assertEqual(TestPid, erlmcp_flow_router:lookup_agent(AgentId)),

    % Lookup non-existent agent
    ?assertEqual(not_found, erlmcp_flow_router:lookup_agent(<<"nonexistent">>)),

    % Cleanup
    exit(TestPid, normal),
    timer:sleep(100).

%% @doc Test 2: Route task to agent
test_route_task() ->
    AgentId = <<"test_agent_2">>,
    Parent = self(),

    % Spawn test agent that echoes tasks
    AgentPid = spawn(fun() ->
        receive
            {task, Task} ->
                Parent ! {task_received, Task}
        after 5000 ->
            timeout
        end
    end),

    % Register agent
    ok = erlmcp_flow_router:register_agent(AgentId, AgentPid),

    % Route task
    Task = #{action => <<"test">>, data => <<"hello">>},
    ?assertEqual(ok, erlmcp_flow_router:route_task(AgentId, Task)),

    % Verify task was received
    receive
        {task_received, ReceivedTask} ->
            ?assertEqual(Task, ReceivedTask)
    after 1000 ->
        ?assert(false, "Task not received")
    end,

    % Cleanup
    exit(AgentPid, normal),
    timer:sleep(100).

%% @doc Test 3: Multi-agent discovery
test_agent_list() ->
    % Spawn 3 test agents
    Agents = [
        {<<"agent_a">>, spawn(fun() -> receive _ -> ok end end)},
        {<<"agent_b">>, spawn(fun() -> receive _ -> ok end end)},
        {<<"agent_c">>, spawn(fun() -> receive _ -> ok end end)}
    ],

    % Register all agents
    lists:foreach(fun({Id, Pid}) ->
        ok = erlmcp_flow_router:register_agent(Id, Pid)
    end, Agents),

    % Get agent list
    AgentPids = erlmcp_flow_router:agent_list(),

    % Verify all agents are listed
    ExpectedPids = [Pid || {_Id, Pid} <- Agents],
    ?assertEqual(3, length(AgentPids)),
    lists:foreach(fun(Pid) ->
        ?assert(lists:member(Pid, ExpectedPids))
    end, AgentPids),

    % Cleanup
    lists:foreach(fun({_Id, Pid}) ->
        exit(Pid, normal)
    end, Agents),
    timer:sleep(100).

%% @doc Test 4: Routing to non-existent agent
test_route_not_found() ->
    AgentId = <<"nonexistent_agent">>,
    Task = #{action => <<"test">>},

    % Try to route to non-existent agent
    Result = erlmcp_flow_router:route_task(AgentId, Task),
    ?assertEqual({error, agent_not_found}, Result).

%% @doc Test 5: Load balancing with multiple agents
test_load_balancing() ->
    % Spawn 3 test agents with different loads
    Agent1 = spawn(fun() -> receive _ -> ok end end),
    Agent2 = spawn(fun() -> receive _ -> ok end end),
    Agent3 = spawn(fun() -> receive _ -> ok end end),

    % Register agents
    ok = erlmcp_flow_router:register_agent(<<"lb_agent_1">>, Agent1),
    ok = erlmcp_flow_router:register_agent(<<"lb_agent_2">>, Agent2),
    ok = erlmcp_flow_router:register_agent(<<"lb_agent_3">>, Agent3),

    % Simulate load by routing tasks
    ok = erlmcp_flow_router:route_task(<<"lb_agent_1">>, #{task => 1}),
    ok = erlmcp_flow_router:route_task(<<"lb_agent_1">>, #{task => 2}),
    ok = erlmcp_flow_router:route_task(<<"lb_agent_2">>, #{task => 3}),

    % Check loads
    Load1 = erlmcp_flow_registry:get_agent_load(<<"lb_agent_1">>),
    Load2 = erlmcp_flow_registry:get_agent_load(<<"lb_agent_2">>),
    Load3 = erlmcp_flow_registry:get_agent_load(<<"lb_agent_3">>),

    ?assertEqual(2, Load1),
    ?assertEqual(1, Load2),
    ?assertEqual(0, Load3),

    % Route with load balancing - should go to agent_3 (lowest load)
    ok = erlmcp_flow_router:route_task_with_load_balancing(#{task => 4}),

    NewLoad3 = erlmcp_flow_registry:get_agent_load(<<"lb_agent_3">>),
    ?assertEqual(1, NewLoad3),

    % Cleanup
    exit(Agent1, normal),
    exit(Agent2, normal),
    exit(Agent3, normal),
    timer:sleep(100).
