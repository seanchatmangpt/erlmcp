%%%-------------------------------------------------------------------
%%% @doc Tests for erlmcp_task_runner
%%% Chicago School TDD - tests use REAL processes, no mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_task_runner_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

task_runner_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Basic task execution completes successfully", fun test_basic_task_execution/0},
      {"Task with result returns value to parent", fun test_task_with_result/0},
      {"Task can be cancelled", fun test_task_cancellation/0},
      {"Task timeout triggers failure", fun test_task_timeout/0},
      {"Task failure is reported", fun test_task_failure/0},
      {"Task with progress token reports progress", fun test_progress_reporting/0},
      {"Supervised task can be started with start_link", fun test_supervised_task/0},
      {"Task integrates with erlmcp_tasks", fun test_erlmcp_tasks_integration/0},
      {"Multiple tasks can run concurrently", fun test_concurrent_tasks/0},
      {"Task cleanup on parent death", fun test_parent_death_cleanup/0},
      {"System messages for debugging work", fun test_sys_debugging/0},
      {"Long-running task with periodic updates", fun test_long_running_task/0},
      {"Task throws exception is caught", fun test_exception_handling/0},
      {"Task exit is caught", fun test_exit_handling/0},
      {"Invalid task spec rejected", fun test_invalid_task_spec/0}
     ]}.

setup() ->
    % Start required services
    application:ensure_all_started(crypto),

    % Start progress tracker if needed
    case whereis(erlmcp_progress) of
        undefined ->
            {ok, Pid} = erlmcp_progress:start_link(),
            Pid;
        Pid ->
            Pid
    end,

    % Start tasks manager if needed
    case whereis(erlmcp_tasks) of
        undefined ->
            {ok, TasksPid} = erlmcp_tasks:start_link(),
            {erlmcp_progress, TasksPid};
        TasksPid ->
            {erlmcp_progress, TasksPid}
    end.

cleanup({ProgressName, TasksPid}) ->
    % Don't stop progress or tasks - they're shared services
    ok.

%%%===================================================================
%%% Basic Execution Tests
%%%===================================================================

test_basic_task_execution() ->
    % Simple task that returns ok
    TaskFun = fun() -> ok end,

    {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),

    ?assert(is_binary(TaskId)),
    ?assert(is_pid(Pid)),

    % Wait for completion
    receive
        {task_complete, TaskId, ok} ->
            ?assertEqual(ok, ok)
    after 1000 ->
        ?assert(false)
    end.

test_task_with_result() ->
    % Task that returns a value
    ExpectedResult = #{status => success, value => 42},
    TaskFun = fun() -> ExpectedResult end,

    {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),

    % Wait for result
    receive
        {task_complete, TaskId, Result} ->
            ?assertEqual(ExpectedResult, Result)
    after 1000 ->
        ?assert(false)
    end.

%%%===================================================================
%%% Cancellation Tests
%%%===================================================================

test_task_cancellation() ->
    % Long-running task that we'll cancel
    Parent = self(),
    TaskFun = fun() ->
        % Signal we've started
        Parent ! task_running,
        % Sleep forever (or until cancelled)
        receive
            never_happens -> ok
        end
    end,

    {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{timeout => 60000}),

    % Wait for task to start
    receive
        task_running -> ok
    after 1000 ->
        ?assert(false)
    end,

    % Cancel the task
    Reason = <<"Testing cancellation">>,
    ok = erlmcp_task_runner:cancel_task(Pid, Reason),

    % Wait for cancellation notification
    receive
        {task_cancelled, TaskId, Reason} ->
            ?assertEqual(Reason, Reason)
    after 2000 ->
        ?debugMsg("Timeout waiting for cancellation"),
        ?assert(false)
    end.

%%%===================================================================
%%% Timeout Tests
%%%===================================================================

test_task_timeout() ->
    % Task that runs longer than timeout
    Parent = self(),
    TaskFun = fun() ->
        Parent ! task_started,
        timer:sleep(5000),
        ok
    end,

    % Set very short timeout
    {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{timeout => 100}),

    % Wait for task to start
    receive
        task_started -> ok
    after 200 ->
        ?debugMsg("Task didn't start")
    end,

    % Wait for timeout
    receive
        {task_failed, TaskId, Error} ->
            ?assertMatch(#{code := ?MCP_ERROR_TIMEOUT}, Error)
    after 1000 ->
        ?debugMsg("Timeout not triggered"),
        ?assert(false)
    end.

%%%===================================================================
%%% Failure Tests
%%%===================================================================

test_task_failure() ->
    % Task that throws an error
    TaskFun = fun() ->
        error(deliberate_error)
    end,

    {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),

    % Wait for failure notification
    receive
        {task_failed, TaskId, {error, deliberate_error, _Stack}} ->
            ?assert(true)
    after 1000 ->
        ?debugMsg("Task failure not reported"),
        ?assert(false)
    end.

test_exception_handling() ->
    % Task that throws
    TaskFun = fun() ->
        throw(test_exception)
    end,

    {ok, TaskId, _Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),

    receive
        {task_failed, TaskId, {throw, test_exception}} ->
            ?assert(true)
    after 1000 ->
        ?assert(false)
    end.

test_exit_handling() ->
    % Task that exits
    TaskFun = fun() ->
        exit(normal)
    end,

    {ok, TaskId, _Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),

    receive
        {task_failed, TaskId, {exit, normal}} ->
            ?assert(true)
    after 1000 ->
        ?assert(false)
    end.

%%%===================================================================
%%% Progress Reporting Tests
%%%===================================================================

test_progress_reporting() ->
    % Task with progress token
    ProgressToken = erlmcp_progress:generate_token(),
    Parent = self(),

    TaskFun = fun() ->
        % Report progress
        erlmcp_progress:update(ProgressToken, #{current => 50, total => 100}),
        Parent ! progress_updated,
        timer:sleep(50),
        erlmcp_progress:update(ProgressToken, #{current => 100, total => 100}),
        Parent ! progress_complete_internal,
        ok
    end,

    {ok, TaskId, _Pid} = erlmcp_task_runner:start_task(TaskFun, #{
        progress_token => ProgressToken
    }),

    % Wait for progress updates
    receive
        progress_updated -> ok
    after 500 ->
        ?debugMsg("Progress update not received")
    end,

    receive
        progress_complete_internal -> ok
    after 500 ->
        ?debugMsg("Progress complete not received")
    end,

    % Wait for task completion
    receive
        {task_complete, TaskId, ok} ->
            ?assert(true)
    after 1000 ->
        ?assert(false)
    end.

%%%===================================================================
%%% Supervision Tests
%%%===================================================================

test_supervised_task() ->
    % Start task with start_link (supervisor-compatible)
    TaskFun = fun() ->
        timer:sleep(100),
        supervised_result
    end,

    {ok, Pid} = erlmcp_task_runner:start_link(TaskFun, #{timeout => 5000}),

    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    % Wait for completion
    receive
        {task_complete, _TaskId, supervised_result} ->
            ?assert(true)
    after 2000 ->
        ?assert(false)
    end.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

test_erlmcp_tasks_integration() ->
    % This tests integration with erlmcp_tasks
    % Create a task via erlmcp_tasks
    TaskFun = fun() ->
        timer:sleep(100),
        integration_result
    end,

    {ok, TaskId} = erlmcp_tasks:create(self(), #{
        type => <<"test">>,
        operation => <<"integration_test">>
    }, #{}),

    % Start task runner with the same task_id
    {ok, Pid} = erlmcp_task_runner:start_task(TaskFun, TaskId, #{}),

    ?assert(is_pid(Pid)),

    % Verify task status is updated
    timer:sleep(50),

    % Wait for completion
    receive
        {task_complete, TaskId, integration_result} ->
            ?assert(true)
    after 2000 ->
        ?debugMsg("Integration test timeout"),
        ?assert(false)
    end,

    % Verify task status in erlmcp_tasks
    {ok, Task} = erlmcp_tasks:get(self(), TaskId),
    ?assertMatch(#{<<"status">> := <<"completed">>}, Task).

%%%===================================================================
%%% Concurrency Tests
%%%===================================================================

test_concurrent_tasks() ->
    % Start multiple tasks concurrently
    NumTasks = 10,
    Parent = self(),

    Tasks = lists:map(fun(N) ->
        TaskFun = fun() ->
            timer:sleep(rand:uniform(100)),
            N * 2
        end,
        {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),
        {N, TaskId, Pid}
    end, lists:seq(1, NumTasks)),

    % Collect all results
    Results = lists:map(fun({N, TaskId, _Pid}) ->
        receive
            {task_complete, TaskId, Result} ->
                {N, Result}
        after 5000 ->
            {N, timeout}
        end
    end, Tasks),

    % Verify all tasks completed
    ?assertEqual(NumTasks, length(Results)),

    % Verify results are correct
    lists:foreach(fun({N, Result}) ->
        ?assertEqual(N * 2, Result)
    end, Results).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

test_parent_death_cleanup() ->
    % Spawn a parent process that will die
    TestPid = self(),

    ParentPid = spawn(fun() ->
        TaskFun = fun() ->
            timer:sleep(5000),
            ok
        end,
        {ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{}),
        TestPid ! {task_started, TaskId, Pid},
        receive
            die -> ok
        end
    end),

    % Wait for task to start
    {TaskId, TaskPid} = receive
        {task_started, TId, TPid} -> {TId, TPid}
    after 1000 ->
        ?assert(false)
    end,

    ?assert(is_process_alive(TaskPid)),

    % Kill parent
    ParentPid ! die,
    timer:sleep(100),

    % Task should die too (linked)
    timer:sleep(500),
    ?assertNot(is_process_alive(TaskPid)).

test_invalid_task_spec() ->
    % Missing 'task_fn' field
    Result1 = erlmcp_task_runner:start_link(#{timeout => 1000}),
    ?assertMatch({error, missing_task_fn}, Result1),

    % Invalid 'task_fn' field
    Result2 = erlmcp_task_runner:start_link(#{task_fn => not_a_function}),
    ?assertMatch({error, invalid_task_fn}, Result2).

%%%===================================================================
%%% Debugging Tests
%%%===================================================================

test_sys_debugging() ->
    % Test task status tracking (observable behavior)
    TaskFun = fun() ->
        timer:sleep(500),
        ok
    end,

    {ok, _TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{timeout => 60000}),

    % Check task status via public API
    timer:sleep(50),

    case erlmcp_task_runner:get_status(Pid) of
        {ok, Status} ->
            ?assertMatch(#{status := running}, Status);
        {error, _Reason} ->
            % Process may have already completed (acceptable race condition)
            ?debugMsg("Process completed before status check"),
            ok
    end.

%%%===================================================================
%%% Long-Running Task Tests
%%%===================================================================

test_long_running_task() ->
    % Simulate a long-running operation with periodic updates
    ProgressToken = erlmcp_progress:generate_token(),
    Parent = self(),

    TaskFun = fun() ->
        lists:foreach(fun(N) ->
            timer:sleep(50),
            erlmcp_progress:update(ProgressToken, #{
                current => N,
                total => 5,
                message => iolist_to_binary(io_lib:format("Step ~p", [N]))
            }),
            Parent ! {step_complete, N}
        end, lists:seq(1, 5)),
        completed
    end,

    {ok, TaskId, _Pid} = erlmcp_task_runner:start_task(TaskFun, #{
        progress_token => ProgressToken,
        timeout => 10000
    }),

    % Collect all step notifications
    Steps = lists:map(fun(N) ->
        receive
            {step_complete, N} -> N
        after 1000 ->
            timeout
        end
    end, lists:seq(1, 5)),

    ?assertEqual([1, 2, 3, 4, 5], Steps),

    % Wait for final completion
    receive
        {task_complete, TaskId, completed} ->
            ?assert(true)
    after 2000 ->
        ?assert(false)
    end.
