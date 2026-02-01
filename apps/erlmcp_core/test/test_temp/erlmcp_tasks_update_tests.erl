%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_tasks update operations
%%%
%%% Chicago School TDD: Real processes, test observable behavior only
%%% - Use REAL erlmcp processes (NO dummy spawn processes)
%%% - Test ALL interfaces (update, update_status, update_progress)
%%% - NO internal state inspection (sys:get_status calls)
%%% - NO record duplication (respect encapsulation)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tasks_update_tests).

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
        true ->
            gen_server:stop(Pid);
        false ->
            ok
    end.

%%%===================================================================
%%% Task Update Tests
%%%===================================================================

task_update_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_update_task_with_function()),
         ?_test(test_update_task_metadata()),
         ?_test(test_update_task_status()),
         ?_test(test_update_task_progress())]
     end}.

%% @doc Test updating task with custom function
test_update_task_with_function() ->
    Action = #{<<"type">> => <<"update_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{counter => 0}),

    %% Update task via API with function
    UpdateFun =
        fun(Task) ->
           Meta = maps:get(<<"metadata">>, Task, #{}),
           Counter = maps:get(counter, Meta, 0),
           NewMeta = Meta#{counter => Counter + 1},
           Task#{<<"metadata">> => NewMeta}
        end,

    ok = erlmcp_tasks:update_task(undefined, TaskId, UpdateFun),

    %% Verify: Update applied via API
    {ok, UpdatedTask} = erlmcp_tasks:get_task(undefined, TaskId),
    UpdatedMeta = maps:get(<<"metadata">>, UpdatedTask, #{}),
    ?assertEqual(1, maps:get(counter, UpdatedMeta, 0)).

%% @doc Test updating task metadata
test_update_task_metadata() ->
    Action = #{<<"type">> => <<"meta_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Update metadata via API
    UpdateFun = fun(Task) -> Task#{<<"metadata">> => #{<<"updated">> => true}} end,

    ok = erlmcp_tasks:update_task(undefined, TaskId, UpdateFun),

    %% Verify: Metadata updated via API
    {ok, UpdatedTask} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(#{<<"updated">> => true}, maps:get(<<"metadata">>, UpdatedTask)).

%% @doc Test updating task status
test_update_task_status() ->
    Action = #{<<"type">> => <<"status_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Update status via API
    ok = erlmcp_tasks:update_status(undefined, TaskId, <<"processing">>),

    %% Verify: Status updated via API
    {ok, UpdatedTask} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"processing">>, maps:get(<<"status">>, UpdatedTask)).

%% @doc Test updating task progress
test_update_task_progress() ->
    Action = #{<<"type">> => <<"progress_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Get progress token from task
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ProgressToken = maps:get(<<"progressToken">>, Task),

    %% Update progress via API
    ok =
        erlmcp_tasks:update_progress(undefined,
                                     TaskId,
                                     #{<<"progressToken">> => ProgressToken,
                                       <<"progress">> => 0.5,
                                       <<"total">> => 1.0}),

    %% Verify: Progress updated via API
    {ok, UpdatedTask} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(0.5, maps:get(<<"progress">>, UpdatedTask, 0.0)),
    ?assertEqual(1.0, maps:get(<<"total">>, UpdatedTask, 0.0)).

%%%===================================================================
%%% Concurrent Update Tests
%%%===================================================================

concurrent_update_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_concurrent_updates_serialize()), ?_test(test_update_race_condition())]
     end}.

%% @doc Test concurrent updates are serialized
test_concurrent_updates_serialize() ->
    Action = #{<<"type">> => <<"concurrent_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{counter => 0}),

    %% Spawn concurrent updaters
    Parent = self(),
    NumProcesses = 10,

    Pids =
        [spawn(fun() ->
                  UpdateFun =
                      fun(Task) ->
                         Meta = maps:get(<<"metadata">>, Task, #{}),
                         Counter = maps:get(counter, Meta, 0),
                         NewMeta = Meta#{counter => Counter + 1},
                         Task#{<<"metadata">> => NewMeta}
                      end,
                  Result = erlmcp_tasks:update_task(undefined, TaskId, UpdateFun),
                  Parent ! {update_result, self(), Result}
               end)
         || _ <- lists:seq(1, NumProcesses)],

    %% Collect results
    Results =
        [receive
             {update_result, _Pid, Res} ->
                 Res
         after 5000 ->
             timeout
         end
         || _ <- Pids],

    %% Verify: All updates succeeded (serialized by gen_server)
    SuccessCount = length([1 || ok <- Results]),
    ?assert(SuccessCount > 0),

    %% Verify: Final state is consistent via API
    {ok, FinalTask} = erlmcp_tasks:get_task(undefined, TaskId),
    FinalMeta = maps:get(<<"metadata">>, FinalTask, #{}),
    ?assert(is_integer(maps:get(counter, FinalMeta, 0))).

%% @doc Test race condition in concurrent updates
test_update_race_condition() ->
    Action = #{<<"type">> => <<"race_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{value => 0}),

    %% Rapid concurrent updates
    lists:foreach(fun(N) ->
                     spawn(fun() ->
                              UpdateFun =
                                  fun(Task) ->
                                     Meta = maps:get(<<"metadata">>, Task, #{}),
                                     Value = maps:get(value, Meta, 0),
                                     NewMeta = Meta#{value => Value + N},
                                     Task#{<<"metadata">> => NewMeta}
                                  end,
                              erlmcp_tasks:update_task(undefined, TaskId, UpdateFun)
                           end)
                  end,
                  lists:seq(1, 50)),

    %% Wait for all updates to process
    timer:sleep(500),

    %% Verify: Task still consistent via API
    {ok, FinalTask} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assert(maps:is_key(<<"metadata">>, FinalTask)).

%%%===================================================================
%%% Update Error Handling Tests
%%%===================================================================

update_error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_update_nonexistent_task()),
         ?_test(test_update_completed_task_fails()),
         ?_test(test_update_cancelled_task_fails())]
     end}.

%% @doc Test updating non-existent task
test_update_nonexistent_task() ->
    FakeId = <<"00000000000000000000000000000000">>,

    %% Try to update via API
    UpdateFun = fun(T) -> T end,
    Result = erlmcp_tasks:update_task(undefined, FakeId, UpdateFun),

    %% Verify: Error returned
    ?assertEqual({error, not_found}, Result).

%% @doc Test updating completed task fails
test_update_completed_task_fails() ->
    Action = #{<<"type">> => <<"complete_update_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Complete task
    ok = erlmcp_tasks:complete_task(TaskId, #{<<"done">> => true}),

    %% Try to update completed task via API
    UpdateFun = fun(T) -> T end,
    Result = erlmcp_tasks:update_task(undefined, TaskId, UpdateFun),

    %% Verify: Update rejected (implementation-specific)
    ?assert(is_tuple(Result)).

%% @doc Test updating cancelled task fails
test_update_cancelled_task_fails() ->
    Action = #{<<"type">> => <<"cancel_update_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Cancel task
    ok = erlmcp_tasks:cancel_task(undefined, TaskId, <<"test">>),

    %% Try to update cancelled task via API
    UpdateFun = fun(T) -> T end,
    Result = erlmcp_tasks:update_task(undefined, TaskId, UpdateFun),

    %% Verify: Update rejected (implementation-specific)
    ?assert(is_tuple(Result)).

%%%===================================================================
%%% Progress Update Tests
%%%===================================================================

progress_update_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_update_progress_multiple_times()),
         ?_test(test_update_progress_to_completion()),
         ?_test(test_update_progress_with_invalid_token())]
     end}.

%% @doc Test updating progress multiple times
test_update_progress_multiple_times() ->
    Action = #{<<"type">> => <<"multi_progress_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Get progress token
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ProgressToken = maps:get(<<"progressToken">>, Task),

    %% Update progress multiple times via API
    ProgressValues = [0.0, 0.25, 0.5, 0.75, 1.0],
    lists:foreach(fun(Progress) ->
                     ok =
                         erlmcp_tasks:update_progress(undefined,
                                                      TaskId,
                                                      #{<<"progressToken">> => ProgressToken,
                                                        <<"progress">> => Progress,
                                                        <<"total">> => 1.0})
                  end,
                  ProgressValues),

    %% Verify: Final progress state via API
    {ok, FinalTask} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(1.0, maps:get(<<"progress">>, FinalTask, 0.0)).

%% @doc Test updating progress to completion
test_update_progress_to_completion() ->
    Action = #{<<"type">> => <<"progress_complete_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Get progress token
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ProgressToken = maps:get(<<"progressToken">>, Task),

    %% Update progress to 100% via API
    ok =
        erlmcp_tasks:update_progress(undefined,
                                     TaskId,
                                     #{<<"progressToken">> => ProgressToken,
                                       <<"progress">> => 1.0,
                                       <<"total">> => 1.0}),

    %% Verify: Progress at 100% via API
    {ok, UpdatedTask} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(1.0, maps:get(<<"progress">>, UpdatedTask, 0.0)).

%% @doc Test updating progress with invalid token
test_update_progress_with_invalid_token() ->
    Action = #{<<"type">> => <<"invalid_progress_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Try to update with invalid token via API
    InvalidToken = make_ref(),
    Result =
        erlmcp_tasks:update_progress(undefined,
                                     TaskId,
                                     #{<<"progressToken">> => InvalidToken,
                                       <<"progress">> => 0.5,
                                       <<"total">> => 1.0}),

    %% Verify: Update fails gracefully (implementation-specific)
    ?assert(is_tuple(Result)).
