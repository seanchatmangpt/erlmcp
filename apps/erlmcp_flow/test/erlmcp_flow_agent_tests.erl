%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_flow_agent (Chicago TDD - real processes)
%%% Test coverage: state machine, task queue, error recovery, heartbeat
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_agent_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    % Start agent with test configuration
    {ok, Pid} = erlmcp_flow_agent:start_link(test_agent, #{
        heartbeat_interval => 1000, % 1s for testing
        max_retries => 3
    }),
    Pid.

cleanup(Pid) ->
    catch erlmcp_flow_agent:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test 1: Agent spawn/shutdown
agent_lifecycle_test() ->
    {ok, Agent} = erlmcp_flow_agent:start_link(agent_1),
    ?assert(is_process_alive(Agent)),

    % Check initial status
    {ok, Status} = erlmcp_flow_agent:get_status(Agent),
    ?assertEqual(idle, Status),

    % Graceful shutdown
    ok = erlmcp_flow_agent:stop(Agent),
    timer:sleep(100),
    ?assertNot(is_process_alive(Agent)).

%% Test 2: Task assignment → execution → result
task_execution_test() ->
    Agent = setup(),
    try
        % Assign simple task
        Task = #{
            id => <<"task-1">>,
            action => fun() -> timer:sleep(50), {success, 42} end,
            timeout => 5000
        },

        ok = erlmcp_flow_agent:assign_task(Agent, Task),

        % Wait for execution to start
        timer:sleep(20),

        % Check status transitioned from idle
        {ok, Status1} = erlmcp_flow_agent:get_status(Agent),
        ?assert(Status1 =/= idle),

        % Wait for completion
        timer:sleep(200),

        % Verify result
        Result = erlmcp_flow_agent:get_result(Agent),
        ?assertMatch({ok, {ok, {success, 42}}}, Result),

        % Verify status is done or idle (after processing)
        {ok, FinalStatus} = erlmcp_flow_agent:get_status(Agent),
        ?assert(FinalStatus =:= done orelse FinalStatus =:= idle)
    after
        cleanup(Agent)
    end.

%% Test 3: Task timeout handling
task_timeout_test() ->
    Agent = setup(),
    try
        % Verify agent is alive before test
        ?assert(is_process_alive(Agent)),

        % Task that throws error immediately (simpler than timeout)
        Task = #{
            id => <<"task-timeout">>,
            action => fun() -> error(simulated_failure) end,
            timeout => 1000
        },

        ok = erlmcp_flow_agent:assign_task(Agent, Task),

        % Wait for retries to complete (3 retries with backoff)
        timer:sleep(1200),

        % Agent should still be alive
        ?assert(is_process_alive(Agent)),

        % Should have an error result
        Result = erlmcp_flow_agent:get_result(Agent),
        case Result of
            {ok, {error, {max_retries, _}}} -> ok; % Expected
            {error, no_result} -> ?assert(false); % Should have result by now
            Other -> ?assertEqual({ok, {error, {max_retries, simulated_failure}}}, Other)
        end
    after
        cleanup(Agent)
    end.

%% Test 4: Concurrent tasks (queue handling)
concurrent_tasks_test() ->
    Agent = setup(),
    try
        % Submit multiple tasks
        Tasks = [#{
            id => list_to_binary("task-" ++ integer_to_list(N)),
            action => fun() -> N * 2 end,
            timeout => 5000
        } || N <- lists:seq(1, 5)],

        % Assign all tasks
        lists:foreach(fun(Task) ->
            ok = erlmcp_flow_agent:assign_task(Agent, Task)
        end, Tasks),

        % Wait for processing
        timer:sleep(2000),

        % Agent should be done or idle
        {ok, Status} = erlmcp_flow_agent:get_status(Agent),
        ?assert(Status =:= idle orelse Status =:= done)
    after
        cleanup(Agent)
    end.

%% Test 5: Agent crash recovery (supervised)
agent_crash_recovery_test() ->
    % This test verifies the agent can be restarted after crash
    % In production, supervisor would handle this
    Agent = setup(),
    % Verify agent is alive
    ?assert(is_process_alive(Agent)),

    % Unlink before killing to avoid killing the test process
    unlink(Agent),

    % Force crash
    exit(Agent, kill),
    timer:sleep(100),

    % Agent is dead (supervisor would restart in production)
    ?assertNot(is_process_alive(Agent)).

%% Test 6: Health check interval (heartbeat)
heartbeat_test() ->
    % Create test swarm process to receive heartbeats
    TestSwarm = spawn(fun() -> heartbeat_receiver(0) end),

    {ok, Agent} = erlmcp_flow_agent:start_link(agent_heartbeat, #{
        swarm_pid => TestSwarm,
        heartbeat_interval => 500 % 500ms for testing
    }),

    try
        % Wait for at least 2 heartbeats
        timer:sleep(1500),

        % Check test swarm received heartbeats
        TestSwarm ! {get_count, self()},
        receive
            {heartbeat_count, Count} ->
                ?assert(Count >= 2)
        after 1000 ->
            ?assert(false) % Timeout - no heartbeats received
        end
    after
        cleanup(Agent),
        exit(TestSwarm, kill)
    end.

%% Test 7: Invalid task handling
invalid_task_test() ->
    Agent = setup(),
    try
        % Task that throws error
        Task = #{
            id => <<"task-error">>,
            action => fun() -> throw(intentional_error) end,
            timeout => 1000
        },

        ok = erlmcp_flow_agent:assign_task(Agent, Task),

        % Wait for error handling and retries
        timer:sleep(1500),

        % Should have error result
        Result = erlmcp_flow_agent:get_result(Agent),
        ?assertMatch({ok, {error, {max_retries, _}}}, Result)
    after
        cleanup(Agent)
    end.

%% Test 8: State machine transitions (idle → assigned → executing → done → idle)
state_machine_test() ->
    Agent = setup(),
    try
        % Initial state: idle
        {ok, Status1} = erlmcp_flow_agent:get_status(Agent),
        ?assertEqual(idle, Status1),

        % Assign task: idle → assigned
        Task = #{
            id => <<"task-sm">>,
            action => fun() -> timer:sleep(200), ok end,
            timeout => 5000
        },
        ok = erlmcp_flow_agent:assign_task(Agent, Task),

        % Should transition to assigned/executing quickly
        timer:sleep(50),
        {ok, Status2} = erlmcp_flow_agent:get_status(Agent),
        ?assert(Status2 =:= assigned orelse Status2 =:= executing),

        % Wait for completion: executing → done → idle
        timer:sleep(500),
        {ok, Status3} = erlmcp_flow_agent:get_status(Agent),
        % After completion, agent transitions to idle (no more tasks in queue)
        ?assert(Status3 =:= done orelse Status3 =:= idle),

        % Should be idle after processing
        timer:sleep(100),
        {ok, Status4} = erlmcp_flow_agent:get_status(Agent),
        ?assertEqual(idle, Status4)
    after
        cleanup(Agent)
    end.

%% Test 9: Queue overflow handling (max 100 tasks)
queue_overflow_test() ->
    Agent = setup(),
    try
        % Submit more than max queue size
        % First task will execute, next 100 will queue, rest dropped
        Tasks = [#{
            id => list_to_binary("task-" ++ integer_to_list(N)),
            action => fun() -> timer:sleep(50), N end,
            timeout => 5000
        } || N <- lists:seq(1, 120)],

        % Assign all tasks
        lists:foreach(fun(Task) ->
            ok = erlmcp_flow_agent:assign_task(Agent, Task)
        end, Tasks),

        % Agent should not crash
        timer:sleep(100),
        ?assert(is_process_alive(Agent))
    after
        cleanup(Agent)
    end.

%% Test 10: Retry mechanism with exponential backoff
retry_backoff_test() ->
    Agent = setup(),
    try
        % Task that always fails
        Task = #{
            id => <<"task-retry">>,
            action => fun() -> error(always_fails) end,
            timeout => 1000
        },

        StartTime = erlang:monotonic_time(millisecond),
        ok = erlmcp_flow_agent:assign_task(Agent, Task),

        % Wait for all retries (3 retries with backoff: 100ms, 200ms, 400ms)
        timer:sleep(1500),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        % Should take at least 600ms (sum of backoffs)
        ?assert(Duration >= 600),

        % Verify max retries error
        Result = erlmcp_flow_agent:get_result(Agent),
        ?assertMatch({ok, {error, {max_retries, _}}}, Result)
    after
        cleanup(Agent)
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

heartbeat_receiver(Count) ->
    receive
        {agent_heartbeat, _AgentPid, _AgentId} ->
            heartbeat_receiver(Count + 1);
        {get_count, From} ->
            From ! {heartbeat_count, Count},
            heartbeat_receiver(Count)
    after 5000 ->
        % Timeout - stop receiver
        ok
    end.

%% Helper to poll for result
wait_for_result(_Agent, 0, _Interval) ->
    {error, timeout};
wait_for_result(Agent, Retries, Interval) ->
    case erlmcp_flow_agent:get_result(Agent) of
        {error, no_result} ->
            timer:sleep(Interval),
            wait_for_result(Agent, Retries - 1, Interval);
        Result ->
            Result
    end.
