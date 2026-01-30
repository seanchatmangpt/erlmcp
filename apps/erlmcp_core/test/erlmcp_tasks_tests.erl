-module(erlmcp_tasks_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_tasks:start_link(),
    Pid.

cleanup(Pid) ->
    erlang:exit(Pid, shutdown).

%%====================================================================
%% Test Cases
%%====================================================================

%% Test task creation
create_task_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Create task successfully",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ?assert(is_binary(TaskId)),
                        ?assert(byte_size(TaskId) > 0)
                    end},

                {"Create task with metadata",
                    fun() ->
                        Metadata = #{<<"key">> => <<"value">>},
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, Metadata),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(Metadata, Task#mcp_task.metadata)
                    end}
            ]
        end}.

%% Test task retrieval
get_task_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Get existing task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(TaskId, Task#mcp_task.id),
                        ?assertEqual(<<"Test Task">>, Task#mcp_task.name),
                        ?assertEqual(working, Task#mcp_task.status)
                    end},

                {"Get non-existent task",
                    fun() ->
                        Result = erlmcp_tasks:get_task(<<"nonexistent">>),
                        ?assertEqual({error, not_found}, Result)
                    end}
            ]
        end}.

%% Test task listing
list_tasks_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"List all tasks",
                    fun() ->
                        {ok, _Id1} = erlmcp_tasks:create_task(<<"Task 1">>, #{}),
                        {ok, _Id2} = erlmcp_tasks:create_task(<<"Task 2">>, #{}),
                        {ok, Tasks} = erlmcp_tasks:list_tasks(),
                        ?assert(length(Tasks) >= 2)
                    end},

                {"List tasks with filter",
                    fun() ->
                        {ok, Id1} = erlmcp_tasks:create_task(<<"Task 1">>, #{}),
                        {ok, _Id2} = erlmcp_tasks:create_task(<<"Task 2">>, #{}),

                        % Complete one task
                        ok = erlmcp_tasks:complete_task(Id1, <<"result">>),

                        % Filter by status
                        {ok, CompletedTasks} = erlmcp_tasks:list_tasks(#{status => completed}),
                        ?assert(length(CompletedTasks) >= 1),
                        [Task | _] = CompletedTasks,
                        ?assertEqual(completed, Task#mcp_task.status)
                    end}
            ]
        end}.

%% Test task state transitions
task_lifecycle_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Complete task successfully",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        Result = <<"Task completed">>,
                        ok = erlmcp_tasks:complete_task(TaskId, Result),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(completed, Task#mcp_task.status),
                        ?assertEqual(Result, Task#mcp_task.result),
                        ?assert(Task#mcp_task.completed_at =/= undefined),
                        ?assert(Task#mcp_task.expires_at =/= undefined)
                    end},

                {"Fail task with error",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        Error = #mcp_error{
                            code = ?MCP_ERROR_TASK_FAILED,
                            message = <<"Task failed">>,
                            data = undefined
                        },
                        ok = erlmcp_tasks:fail_task(TaskId, Error),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(failed, Task#mcp_task.status),
                        ?assertEqual(Error, Task#mcp_task.error)
                    end},

                {"Cancel running task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:cancel_task(TaskId),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(cancelled, Task#mcp_task.status)
                    end},

                {"Cannot complete already completed task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:complete_task(TaskId, <<"result">>),
                        Result = erlmcp_tasks:complete_task(TaskId, <<"new result">>),
                        ?assertMatch({error, {?MCP_ERROR_TASK_ALREADY_COMPLETED, _, _}}, Result)
                    end},

                {"Cannot cancel completed task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:complete_task(TaskId, <<"result">>),
                        Result = erlmcp_tasks:cancel_task(TaskId),
                        ?assertMatch({error, {?MCP_ERROR_TASK_ALREADY_COMPLETED, _, _}}, Result)
                    end}
            ]
        end}.

%% Test task progress updates
task_progress_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Update task progress",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:update_task_progress(TaskId, 50.0, 100.0),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(50.0, Task#mcp_task.progress),
                        ?assertEqual(100.0, Task#mcp_task.total)
                    end},

                {"Cannot update progress on completed task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:complete_task(TaskId, <<"result">>),
                        Result = erlmcp_tasks:update_task_progress(TaskId, 50.0, 100.0),
                        ?assertMatch({error, {?MCP_ERROR_TASK_STATE_INVALID, _, _}}, Result)
                    end}
            ]
        end}.

%% Test task result retrieval
task_result_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Get result from completed task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        Result = <<"Task result">>,
                        ok = erlmcp_tasks:complete_task(TaskId, Result),
                        ?assertEqual({ok, Result}, erlmcp_tasks:get_task_result(TaskId))
                    end},

                {"Cannot get result from running task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        Result = erlmcp_tasks:get_task_result(TaskId),
                        ?assertEqual({error, not_ready}, Result)
                    end},

                {"Cannot get result from failed task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        Error = #mcp_error{
                            code = ?MCP_ERROR_TASK_FAILED,
                            message = <<"Failed">>,
                            data = undefined
                        },
                        ok = erlmcp_tasks:fail_task(TaskId, Error),
                        Result = erlmcp_tasks:get_task_result(TaskId),
                        ?assertMatch({error, {task_failed, _}}, Result)
                    end},

                {"Cannot get result from cancelled task",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:cancel_task(TaskId),
                        Result = erlmcp_tasks:get_task_result(TaskId),
                        ?assertEqual({error, cancelled}, Result)
                    end},

                {"Get result from non-existent task",
                    fun() ->
                        Result = erlmcp_tasks:get_task_result(<<"nonexistent">>),
                        ?assertEqual({error, not_found}, Result)
                    end}
            ]
        end}.

%% Test task status transitions
task_status_update_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Transition from working to input_required",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:update_task_status(TaskId, input_required),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(input_required, Task#mcp_task.status)
                    end},

                {"Transition from input_required to working",
                    fun() ->
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}),
                        ok = erlmcp_tasks:update_task_status(TaskId, input_required),
                        ok = erlmcp_tasks:update_task_status(TaskId, working),
                        {ok, Task} = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual(working, Task#mcp_task.status)
                    end}
            ]
        end}.

%% Test task cleanup
task_cleanup_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Cleanup expired tasks",
                    fun() ->
                        % Create a task with very short expiry (1ms)
                        {ok, TaskId} = erlmcp_tasks:create_task(<<"Test Task">>, #{}, 1),
                        ok = erlmcp_tasks:complete_task(TaskId, <<"result">>),

                        % Wait for it to expire
                        timer:sleep(10),

                        % Cleanup expired tasks
                        {ok, Count} = erlmcp_tasks:cleanup_expired_tasks(),

                        % Task should be removed
                        ?assert(Count >= 0),
                        Result = erlmcp_tasks:get_task(TaskId),
                        ?assertEqual({error, not_found}, Result)
                    end}
            ]
        end}.

%% Test error conditions
task_errors_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                {"Update non-existent task progress",
                    fun() ->
                        Result = erlmcp_tasks:update_task_progress(<<"nonexistent">>, 50.0, 100.0),
                        ?assertMatch({error, {?MCP_ERROR_TASK_NOT_FOUND, _, _}}, Result)
                    end},

                {"Complete non-existent task",
                    fun() ->
                        Result = erlmcp_tasks:complete_task(<<"nonexistent">>, <<"result">>),
                        ?assertMatch({error, {?MCP_ERROR_TASK_NOT_FOUND, _, _}}, Result)
                    end},

                {"Fail non-existent task",
                    fun() ->
                        Error = #mcp_error{code = -1, message = <<"Error">>, data = undefined},
                        Result = erlmcp_tasks:fail_task(<<"nonexistent">>, Error),
                        ?assertMatch({error, {?MCP_ERROR_TASK_NOT_FOUND, _, _}}, Result)
                    end},

                {"Cancel non-existent task",
                    fun() ->
                        Result = erlmcp_tasks:cancel_task(<<"nonexistent">>),
                        ?assertMatch({error, {?MCP_ERROR_TASK_NOT_FOUND, _, _}}, Result)
                    end}
            ]
        end}.
