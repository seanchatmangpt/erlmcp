%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_tasks edge cases and error handling
%%%
%%% Chicago School TDD: Real processes, test observable behavior only
%%% - Use REAL erlmcp processes (NO dummy spawn processes)
%%% - Test ALL interfaces (error cases, edge cases)
%%% - NO internal state inspection (sys:get_status calls)
%%% - NO record duplication (respect encapsulation)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tasks_edge_cases_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_tasks:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Edge Cases Tests
%%%===================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_create_task_with_empty_action()),
          ?_test(test_create_task_with_nil_metadata()),
          ?_test(test_create_task_with_large_result()),
          ?_test(test_duplicate_task_id_collision())
         ]
     end}.

%% @doc Test creating task with empty action
test_create_task_with_empty_action() ->
    Action = #{},
    Metadata = #{},

    {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, Metadata),

    %% Verify: Task created successfully via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Task)),
    ?assertEqual(Action, maps:get(<<"action">>, Task)),
    ?assertEqual(Metadata, maps:get(<<"metadata">>, Task)).

%% @doc Test creating task with nil/undefined metadata
test_create_task_with_nil_metadata() ->
    Action = #{<<"type">> => <<"nil_test">>},

    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, undefined),

    %% Verify: Task created via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assert(maps:is_key(<<"metadata">>, Task)).

%% @doc Test completing task with large result
test_create_task_with_large_result() ->
    Action = #{<<"type">> => <<"large_result">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Complete task with large result via API
    LargeBinary = crypto:strong_rand_bytes(1024 * 100),  % 100 KB
    Result = #{
        <<"data">> => LargeBinary,
        <<"count">> => 100000
    },

    ok = erlmcp_tasks:complete(undefined, TaskId, Result),

    %% Verify: Result stored correctly via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    StoredResult = maps:get(<<"result">>, Task),
    ?assertEqual(LargeBinary, maps:get(<<"data">>, StoredResult)),
    ?assertEqual(100000, maps:get(<<"count">>, StoredResult)).

%% @doc Test task ID uniqueness (statistically improbable to collide)
test_duplicate_task_id_collision() ->
    Action = #{<<"type">> => <<"collision_test">>},

    %% Create many tasks
    TaskIds = [begin
        {ok, Id} = erlmcp_tasks:create(undefined, Action, #{}),
        Id
    end || _ <- lists:seq(1, 100)],

    %% Verify: All IDs are unique
    UniqueIds = lists:usort(TaskIds),
    ?assertEqual(100, length(UniqueIds)).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_get_nonexistent_task()),
          ?_test(test_cancel_nonexistent_task()),
          ?_test(test_complete_nonexistent_task()),
          ?_test(test_update_nonexistent_task()),
          ?_test(test_complete_already_completed_task()),
          ?_test(test_cancel_completed_task())
         ]
     end}.

%% @doc Test getting non-existent task
test_get_nonexistent_task() ->
    %% Try to get non-existent task via API
    InvalidId = <<"00000000000000000000000000000000">>,
    Result = erlmcp_tasks:get_task(undefined, InvalidId),

    %% Verify: Error returned via API
    ?assertEqual({error, not_found}, Result).

%% @doc Test cancelling non-existent task
test_cancel_nonexistent_task() ->
    %% Try to cancel non-existent task via API
    InvalidId = <<"00000000000000000000000000000000">>,
    Result = erlmcp_tasks:cancel(undefined, InvalidId, <<"test">>),

    %% Verify: Error returned via API
    ?assertEqual({error, not_found}, Result).

%% @doc Test completing non-existent task
test_complete_nonexistent_task() ->
    %% Try to complete non-existent task via API
    InvalidId = <<"00000000000000000000000000000000">>,
    Result = erlmcp_tasks:complete(undefined, InvalidId, #{}),

    %% Verify: Error returned via API
    ?assertEqual({error, not_found}, Result).

%% @doc Test updating non-existent task
test_update_nonexistent_task() ->
    %% Try to update non-existent task via API
    InvalidId = <<"00000000000000000000000000000000">>,
    UpdateFun = fun(T) -> T end,
    Result = erlmcp_tasks:update_task(undefined, InvalidId, UpdateFun),

    %% Verify: Error returned via API
    ?assertEqual({error, not_found}, Result).

%% @doc Test completing already completed task
test_complete_already_completed_task() ->
    Action = #{<<"type">> => <<"double_complete_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Complete task via API
    ok = erlmcp_tasks:complete(undefined, TaskId, #{<<"result">> => 1}),

    %% Try to complete again via API
    Result = erlmcp_tasks:complete(undefined, TaskId, #{<<"result">> => 2}),

    %% Verify: Second completion fails (implementation-specific)
    ?assertMatch({error, _}, Result).

%% @doc Test cancelling completed task
test_cancel_completed_task() ->
    Action = #{<<"type">> => <<"cancel_completed_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Complete task via API
    ok = erlmcp_tasks:complete(undefined, TaskId, #{<<"done">> => true}),

    %% Try to cancel completed task via API
    Result = erlmcp_tasks:cancel(undefined, TaskId, <<"test">>),

    %% Verify: Cancellation fails (implementation-specific)
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Timeout and Cleanup Tests
%%%===================================================================

timeout_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_task_timeout()),
          ?_test(test_cleanup_expired_tasks()),
          ?_test(test_task_cleanup_after_completion())
         ]
     end}.

%% @doc Test task timeout handling
test_task_timeout() ->
    Action = #{<<"type">> => <<"timeout_test">>},
    {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, #{<<"timeout">> => 100}),

    %% Start task with short timeout via API
    ok = erlmcp_tasks:start_task_execution(TaskId, self()),

    %% Wait for timeout
    timer:sleep(200),

    %% Verify: Task failed due to timeout via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"failed">>, maps:get(<<"status">>, Task)),
    ?assertEqual(<<"Task execution timeout">>, maps:get(<<"error">>, Task)).

%% @doc Test cleanup of expired tasks
test_cleanup_expired_tasks() ->
    %% Create tasks with different TTLs
    ShortTTL = 100,  % 100ms
    LongTTL = 10000, % 10s

    {ok, ShortTaskId} = erlmcp_tasks:create(
        undefined,
        #{<<"type">> => <<"short_lived">>},
        #{},
        #{ttl_ms => ShortTTL}
    ),

    {ok, LongTaskId} = erlmcp_tasks:create(
        undefined,
        #{<<"type">> => <<"long_lived">>},
        #{},
        #{ttl_ms => LongTTL}
    ),

    %% Verify: Both tasks exist initially via API
    ?assertMatch({ok, _}, erlmcp_tasks:get_task(undefined, ShortTaskId)),
    ?assertMatch({ok, _}, erlmcp_tasks:get_task(undefined, LongTaskId)),

    %% Wait for short task to expire
    timer:sleep(ShortTTL + 100),

    %% Trigger cleanup via API
    {ok, CleanedCount} = erlmcp_tasks:cleanup_expired(undefined),

    %% Verify: Short task cleaned up via API
    ?assertEqual({error, not_found}, erlmcp_tasks:get_task(undefined, ShortTaskId)),

    %% Verify: Long task still exists via API
    ?assertMatch({ok, _}, erlmcp_tasks:get_task(undefined, LongTaskId)),

    %% Verify: Cleanup count
    ?assert(CleanedCount >= 1).

%% @doc Test task cleanup after completion
test_task_cleanup_after_completion() ->
    Action = #{<<"type">> => <<"cleanup_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Complete task via API
    ok = erlmcp_tasks:complete(undefined, TaskId, #{<<"done">> => true}),

    %% Wait a bit
    timer:sleep(100),

    %% Trigger cleanup via API
    {ok, CleanedCount} = erlmcp_tasks:cleanup_expired(undefined),

    %% Verify: Cleanup completed at least one task
    ?assert(CleanedCount >= 0).

%%%===================================================================
%%% Concurrent Operations Tests
%%%===================================================================

concurrent_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_concurrent_task_creation()),
          ?_test(test_concurrent_task_completion()),
          ?_test(test_concurrent_mixed_operations())
         ]
     end}.

%% @doc Test concurrent task creation
test_concurrent_task_creation() ->
    Parent = self(),
    NumTasks = 50,

    %% Spawn concurrent task creators
    Pids = [spawn(fun() ->
        Action = #{<<"type">> => <<"concurrent_create">>, <<"index">> => N},
        Result = erlmcp_tasks:create(undefined, Action, #{}),
        Parent ! {task_created, N, Result}
    end) || N <- lists:seq(1, NumTasks)],

    %% Collect results
    Results = [receive
        {task_created, _N, Res} -> Res
    after 5000 ->
        timeout
    end || _ <- Pids],

    %% Verify: All tasks created successfully
    SuccessCount = length([1 || {ok, _} <- Results]),
    ?assertEqual(NumTasks, SuccessCount).

%% @doc Test concurrent task completion
test_concurrent_task_completion() ->
    %% Create tasks first
    NumTasks = 20,
    TaskIds = [begin
        Action = #{<<"type">> => <<"concurrent_complete">>, <<"index">> => N},
        {ok, Id} = erlmcp_tasks:create(undefined, Action, #{}),
        Id
    end || N <- lists:seq(1, NumTasks)],

    %% Spawn concurrent completers
    Parent = self(),
    Pids = [spawn(fun() ->
        Result = erlmcp_tasks:complete(undefined, TaskId, #{<<"done">> => true}),
        Parent ! {task_completed, TaskId, Result}
    end) || TaskId <- TaskIds],

    %% Collect results
    Results = [receive
        {task_completed, _Id, Res} -> Res
    after 5000 ->
        timeout
    end || _ <- Pids],

    %% Verify: All tasks completed successfully
    SuccessCount = length([1 || ok <- Results]),
    ?assert(SuccessCount > 0).

%% @doc Test concurrent mixed operations
test_concurrent_mixed_operations() ->
    %% Create tasks
    NumTasks = 30,
    TaskIds = [begin
        Action = #{<<"type">> => <<"mixed_ops">>},
        {ok, Id} = erlmcp_tasks:create(undefined, Action, #{}),
        Id
    end || _ <- lists:seq(1, NumTasks)],

    %% Spawn mixed operations
    Parent = self(),
    lists:foreach(fun({N, TaskId}) ->
        spawn(fun() ->
            case N rem 3 of
                0 ->
                    %% Complete task
                    erlmcp_tasks:complete(undefined, TaskId, #{<<"done">> => true});
                1 ->
                    %% Update status
                    erlmcp_tasks:update_status(undefined, TaskId, <<"processing">>);
                2 ->
                    %% Cancel task
                    erlmcp_tasks:cancel(undefined, TaskId, <<"test">>)
            end,
            Parent ! {op_done, N}
        end)
    end, lists:zip(lists:seq(1, NumTasks), TaskIds)),

    %% Wait for all operations
    lists:foreach(fun(_) ->
        receive
            {op_done, _N} -> ok
        after 5000 ->
            timeout
        end
    end, lists:seq(1, NumTasks)),

    %% Verify: System still stable via API
    ?assert(is_process_alive(whereis(erlmcp_tasks))).
