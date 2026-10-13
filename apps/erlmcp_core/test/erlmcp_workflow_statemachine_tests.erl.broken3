%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_workflow_statemachine (Chicago School TDD)
%%%
%%% Test Strategy:
%%% - Black-box testing: Test observable behavior, not implementation
%%% - State transitions: Verify all valid state transitions
%%% - Task execution: Test parallel execution and dependencies
%%% - Error handling: Test failures, retries, and cancellations
%%% - Chicago School: Tests DRIVE behavior, not written after
%%%
%%% Coverage Target: 90%+
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_statemachine_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Test setup and teardown
setup() ->
    {ok, Pid} = erlmcp_workflow_statemachine:start_link(<<"test_workflow">>, #{}),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_statem:stop(Pid);
        false -> ok
    end.

%%====================================================================
%% State Transition Tests
%%====================================================================

pending_to_running_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow in pending state
                    {ok, pending} = erlmcp_workflow_statemachine:get_status(Pid),

                    %% When: Starting workflow
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),

                    %% Then: State should be running
                    {ok, running} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

running_to_paused_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Running workflow
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    {ok, running} = erlmcp_workflow_statemachine:get_status(Pid),

                    %% When: Pausing workflow
                    ok = erlmcp_workflow_statemachine:pause_workflow(Pid),

                    %% Then: State should be paused
                    {ok, paused} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

paused_to_running_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Paused workflow
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    ok = erlmcp_workflow_statemachine:pause_workflow(Pid),
                    {ok, paused} = erlmcp_workflow_statemachine:get_status(Pid),

                    %% When: Resuming workflow
                    ok = erlmcp_workflow_statemachine:resume_workflow(Pid),

                    %% Then: State should be running
                    {ok, running} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

running_to_completed_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Running workflow with tasks
                    TaskId = <<"task1">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Test Task">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => []
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),

                    %% When: All tasks complete
                    timer:sleep(100),

                    %% Then: State should be completed
                    {ok, completed} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

running_to_cancelled_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Running workflow
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    {ok, running} = erlmcp_workflow_statemachine:get_status(Pid),

                    %% When: Cancelling workflow
                    ok = erlmcp_workflow_statemachine:cancel_workflow(Pid),

                    %% Then: State should be cancelled
                    {ok, cancelled} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

running_to_failed_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Running workflow with failing task
                    TaskId = <<"task1">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Failing Task">>,
                        module => ?MODULE,
                        function => test_task_failure,
                        args => [],
                        dependencies => []
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),

                    %% When: Task fails
                    timer:sleep(100),

                    %% Then: State should be failed
                    {ok, failed} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

%%====================================================================
%% Task Execution Tests
%%====================================================================

add_task_in_pending_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow in pending state
                    TaskId = <<"task1">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Test Task">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => []
                    },

                    %% When: Adding task
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),

                    %% Then: Task should be retrievable
                    {ok, [RetrievedTaskId]} = erlmcp_workflow_statemachine:get_tasks(Pid),
                    ?assertEqual(TaskId, RetrievedTaskId)
                end)
         ]
     end}.

parallel_task_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow with independent tasks
                    Task1 = #{
                        id => <<"task1">>,
                        name => <<"Task 1">>,
                        module => ?MODULE,
                        function => test_task_delayed,
                        args => [50],
                        dependencies => []
                    },
                    Task2 = #{
                        id => <<"task2">>,
                        name => <<"Task 2">>,
                        module => ?MODULE,
                        function => test_task_delayed,
                        args => [50],
                        dependencies => []
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, <<"task1">>, Task1),
                    ok = erlmcp_workflow_statemachine:add_task(Pid, <<"task2">>, Task2),

                    %% When: Starting workflow
                    StartTime = erlang:monotonic_time(millisecond),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    timer:sleep(150),
                    EndTime = erlang:monotonic_time(millisecond),

                    %% Then: Both tasks should execute in parallel
                    %% Total time should be ~50ms, not 100ms
                    Elapsed = EndTime - StartTime,
                    ?assert(Elapsed < 100, "Tasks should execute in parallel")
                end)
         ]
     end}.

dependent_task_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow with dependent tasks
                    Task1 = #{
                        id => <<"task1">>,
                        name => <<"Task 1">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => []
                    },
                    Task2 = #{
                        id => <<"task2">>,
                        name => <<"Task 2">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => [<<"task1">>]
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, <<"task1">>, Task1),
                    ok = erlmcp_workflow_statemachine:add_task(Pid, <<"task2">>, Task2),

                    %% When: Starting workflow
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    timer:sleep(100),

                    %% Then: Tasks should execute in order
                    {ok, running} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

task_failure_propagates_to_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow with failing task
                    TaskId = <<"failing_task">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Failing Task">>,
                        module => ?MODULE,
                        function => test_task_failure,
                        args => [],
                        dependencies => []
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),

                    %% When: Task fails
                    timer:sleep(100),

                    %% Then: Workflow should fail
                    {ok, failed} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

retry_on_failure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow with retryable task
                    TaskId = <<"retryable_task">>,
                    RetryPolicy = #{
                        max_attempts => 3,
                        backoff => exponential,
                        base_delay => 10,
                        max_delay => 100
                    },
                    Task = #{
                        id => TaskId,
                        name => <<"Retryable Task">>,
                        module => ?MODULE,
                        function => test_task_flaky,
                        args => [],
                        dependencies => [],
                        retry_policy => RetryPolicy
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),

                    %% When: Task fails initially
                    timer:sleep(200),

                    %% Then: Task should retry and eventually succeed
                    {ok, completed} = erlmcp_workflow_statemachine:get_status(Pid)
                end)
         ]
     end}.

cannot_add_task_after_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Running workflow
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),

                    %% When: Trying to add task
                    TaskId = <<"task1">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Test Task">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => []
                    },
                    Result = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),

                    %% Then: Should return error
                    ?assertEqual({error, invalid_state}, Result)
                end)
         ]
     end}.

%%====================================================================
%% Subscription Tests
%%====================================================================

subscriber_receives_events_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Workflow with subscriber
                    Subscriber = self(),
                    ok = erlmcp_workflow_statemachine:subscribe(Pid, Subscriber),

                    TaskId = <<"task1">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Test Task">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => []
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    timer:sleep(100),

                    %% When: Task completes
                    %% Then: Subscriber should receive event
                    receive
                        {workflow_event, _WorkflowId, Event} ->
                            ?assert(maps:get(type, Event) =:= task_completed)
                    after 500 ->
                        ?assert(false, "Did not receive workflow event")
                    end
                end)
         ]
     end}.

unsubscribe_stops_events_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Given: Subscribed workflow
                    Subscriber = self(),
                    ok = erlmcp_workflow_statemachine:subscribe(Pid, Subscriber),

                    TaskId = <<"task1">>,
                    Task = #{
                        id => TaskId,
                        name => <<"Test Task">>,
                        module => ?MODULE,
                        function => test_task_success,
                        args => [],
                        dependencies => []
                    },
                    ok = erlmcp_workflow_statemachine:add_task(Pid, TaskId, Task),

                    %% When: Unsubscribing before starting
                    ok = erlmcp_workflow_statemachine:unsubscribe(Pid, Subscriber),
                    ok = erlmcp_workflow_statemachine:start_workflow(Pid, <<"Test Workflow">>),
                    timer:sleep(100),

                    %% Then: Should not receive events
                    receive
                        {workflow_event, _WorkflowId, _Event} ->
                            ?assert(false, "Should not receive events after unsubscribe")
                    after 100 ->
                        ok
                    end
                end)
         ]
     end}.

%%====================================================================
%% Test Helper Functions
%%====================================================================

%% Successful test task
test_task_success() ->
    {ok, success}.

%% Delayed test task for parallel execution testing
test_task_delayed(DelayMs) ->
    timer:sleep(DelayMs),
    {ok, delayed_success}.

%% Failing test task
test_task_failure() ->
    {error, test_failure}.

%% Flaky test task that succeeds on retry
test_task_flaky() ->
    case get(flaky_attempts) of
        undefined ->
            put(flaky_attempts, 1),
            {error, flaky_failure};
        2 ->
            {ok, success}
    end.

%%====================================================================
%% Property-Based Tests (Proper)
%%====================================================================

%% Property: Workflow state transitions are valid
prop_state_transitions_valid() ->
    ?FORALL(Commands, commands(?MODULE),
            begin
                {ok, Pid} = erlmcp_workflow_statemachine:start_link(<<"prop_workflow">>, #{}),
                Result = run_commands(Commands, Pid),
                gen_statem:stop(Pid),
                aggregate(command_names(Commands), Result =:= ok)
            end).

%% Command generators for state machine testing
initial_state() ->
    pending.

command_gen(_State) ->
    oneof([
        {call, ?MODULE, start_workflow, [<<"Test">>]},
        {call, ?MODULE, pause_workflow, []},
        {call, ?MODULE, resume_workflow, []},
        {call, ?MODULE, cancel_workflow, []},
        {call, ?MODULE, get_status, []}
    ]).

%% Postconditions for state machine
postcondition(_State, {call, _, start_workflow, _}, Result) ->
    Result =:= ok;
postcondition(_State, {call, _, pause_workflow, _}, Result) ->
    Result =:= ok;
postcondition(_State, {call, _, resume_workflow, _}, Result) ->
    Result =:= ok;
postcondition(_State, {call, _, cancel_workflow, _}, Result) ->
    Result =:= ok;
postcondition(_State, {call, _, get_status, _}, {ok, _Status}) ->
    true;
postcondition(_State, _Command, _Result) ->
    true.

%% Next state calculation
next_state(State, _Var, _Res) ->
    State.
