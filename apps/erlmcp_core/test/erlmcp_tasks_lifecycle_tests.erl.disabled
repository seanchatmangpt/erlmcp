%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_tasks lifecycle operations
%%%
%%% Chicago School TDD: Real processes, test observable behavior only
%%% - Use REAL erlmcp processes (NO dummy spawn processes)
%%% - Test ALL interfaces (create, get, list, update, complete, cancel)
%%% - NO internal state inspection (sys:get_status calls)
%%% - NO record duplication (respect encapsulation)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tasks_lifecycle_tests).
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
%%% Task Creation Tests
%%%===================================================================

task_creation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_create_task_with_defaults()),
          ?_test(test_create_task_with_metadata()),
          ?_test(test_create_task_generates_unique_id()),
          ?_test(test_create_task_initial_state())
         ]
     end}.

%% @doc Test creating a task with default values
test_create_task_with_defaults() ->
    Action = #{<<"type">> => <<"test">>, <<"operation">> => <<"create">>},

    {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, #{}),

    %% Verify: Task ID is binary with proper length (UUID format: 32 hex chars)
    ?assert(is_binary(TaskId)),
    ?assertEqual(32, byte_size(TaskId)),

    %% Verify: Task can be retrieved via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Task)),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task)),
    ?assertEqual(Action, maps:get(<<"action">>, Task)).

%% @doc Test creating a task with metadata
test_create_task_with_metadata() ->
    Action = #{<<"type">> => <<"test">>},
    Metadata = #{<<"user">> => <<"alice">>, <<"project">> => <<"test_project">>},

    {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, Metadata),

    %% Verify: Metadata is stored correctly via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(Metadata, maps:get(<<"metadata">>, Task)).

%% @doc Test task IDs are unique
test_create_task_generates_unique_id() ->
    Action = #{<<"type">> => <<"test">>},

    %% Create multiple tasks
    TaskIds = [begin
        {ok, Id} = erlmcp_tasks:create_task(undefined, Action, #{}),
        Id
    end || _ <- lists:seq(1, 10)],

    %% Verify all unique
    UniqueIds = lists:usort(TaskIds),
    ?assertEqual(10, length(UniqueIds)).

%% @doc Test initial state of created task
test_create_task_initial_state() ->
    Action = #{<<"type">> => <<"test">>},

    {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, #{}),

    %% Verify initial fields via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task)),
    ?assert(maps:is_key(<<"createdAt">>, Task)),
    ?assert(maps:is_key(<<"updatedAt">>, Task)),
    ?assert(maps:is_key(<<"progressToken">>, Task)).

%%%===================================================================
%%% Task Lifecycle Tests
%%%===================================================================

task_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_task_pending_to_processing()),
          ?_test(test_task_processing_to_completed()),
          ?_test(test_task_full_lifecycle()),
          ?_test(test_task_cancellation())
         ]
     end}.

%% @doc Test transition from pending to processing
test_task_pending_to_processing() ->
    Action = #{<<"type">> => <<"lifecycle_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Verify initial state via API
    {ok, Task1} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task1)),

    %% Exercise: Transition to processing via API
    ok = erlmcp_tasks:start_task_execution(TaskId, self()),

    %% Verify processing state via API
    {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task2)).

%% @doc Test transition from processing to completed
test_task_processing_to_completed() ->
    Action = #{<<"type">> => <<"complete_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Start processing
    ok = erlmcp_tasks:start_task_execution(TaskId, self()),

    %% Complete task via API
    Result = #{<<"output">> => <<"test_result">>, <<"code">> => 0},
    ok = erlmcp_tasks:complete_task(TaskId, Result),

    %% Verify completed state via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"completed">>, maps:get(<<"status">>, Task)),
    ?assertEqual(Result, maps:get(<<"result">>, Task)).

%% @doc Test full task lifecycle
test_task_full_lifecycle() ->
    Action = #{<<"type">> => <<"full_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Verify timestamps via API
    {ok, Task1} = erlmcp_tasks:get_task(undefined, TaskId),
    CreatedAt = maps:get(<<"createdAt">>, Task1),
    UpdatedAt1 = maps:get(<<"updatedAt">>, Task1),
    ?assert(CreatedAt > 0),
    ?assert(UpdatedAt1 >= CreatedAt),

    %% Start processing
    ok = erlmcp_tasks:start_task_execution(TaskId, self()),
    {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
    UpdatedAt2 = maps:get(<<"updatedAt">>, Task2),
    ?assert(UpdatedAt2 >= UpdatedAt1),

    %% Complete task
    Result = #{<<"done">> => true},
    ok = erlmcp_tasks:complete_task(TaskId, Result),
    {ok, Task3} = erlmcp_tasks:get_task(undefined, TaskId),
    UpdatedAt3 = maps:get(<<"updatedAt">>, Task3),
    ?assert(UpdatedAt3 >= UpdatedAt2),
    ?assertEqual(Result, maps:get(<<"result">>, Task3)).

%% @doc Test task cancellation
test_task_cancellation() ->
    Action = #{<<"type">> => <<"cancellable">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Start processing
    ok = erlmcp_tasks:start_task_execution(TaskId, self()),

    %% Cancel task via API
    Reason = <<"User requested cancellation">>,
    ok = erlmcp_tasks:cancel_task(undefined, TaskId, Reason),

    %% Verify cancelled state via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, Task)),
    ?assertEqual(Reason, maps:get(<<"error">>, Task)).

%%%===================================================================
%%% Task Listing Tests
%%%===================================================================

task_listing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_list_all_tasks()),
          ?_test(test_list_tasks_with_limit()),
          ?_test(test_list_tasks_pagination()),
          ?_test(test_list_empty_tasks())
         ]
     end}.

%% @doc Test listing all tasks
test_list_all_tasks() ->
    %% Create tasks
    TaskIds = [begin
        Action = #{<<"type">> => <<"list_test">>, <<"index">> => N},
        {ok, Id} = erlmcp_tasks:create(undefined, Action, #{}),
        Id
    end || N <- lists:seq(1, 5)],

    %% List all tasks via API
    {ok, TaskListResult} = erlmcp_tasks:list_tasks(undefined, undefined, 100),
    AllTasks = maps:get(<<"tasks">>, TaskListResult, []),

    %% Verify: At least our tasks are present
    ?assert(length(AllTasks) >= 5),

    %% Verify: Our task IDs are in the list
    ListedIds = [maps:get(<<"taskId">>, T) || T <- AllTasks],
    lists:foreach(fun(Id) ->
        ?assert(lists:member(Id, ListedIds))
    end, TaskIds).

%% @doc Test listing tasks with limit
test_list_tasks_with_limit() ->
    %% Create tasks
    _TaskIds = [begin
        Action = #{<<"type">> => <<"limit_test">>},
        {ok, _Id} = erlmcp_tasks:create(undefined, Action, #{}),
        ok
    end || _ <- lists:seq(1, 10)],

    %% List with limit via API
    {ok, TaskListResult} = erlmcp_tasks:list_tasks(undefined, undefined, 5),
    AllTasks = maps:get(<<"tasks">>, TaskListResult, []),

    %% Verify: Limited to 5 tasks
    ?assert(length(AllTasks) =< 5).

%% @doc Test cursor-based pagination
test_list_tasks_pagination() ->
    %% Create tasks
    _TaskIds = [begin
        Action = #{<<"type">> => <<"page_test">>},
        {ok, _Id} = erlmcp_tasks:create(undefined, Action, #{}),
        ok
    end || _ <- lists:seq(1, 15)],

    %% Get first page via API
    {ok, FirstPageResult} = erlmcp_tasks:list_tasks(undefined, undefined, 10),
    FirstPageTasks = maps:get(<<"tasks">>, FirstPageResult, []),
    Cursor = maps:get(<<"cursor">>, FirstPageResult),

    %% Verify: Cursor present when more tasks exist
    case length(FirstPageTasks) of
        10 -> ?assert(Cursor =/= undefined);
        _ -> ok
    end.

%% @doc Test listing when no tasks exist
test_list_empty_tasks() ->
    %% List tasks when none exist via API
    {ok, TaskListResult} = erlmcp_tasks:list_tasks(undefined, undefined, 100),
    AllTasks = maps:get(<<"tasks">>, TaskListResult, []),

    %% Verify: Empty list
    ?assertEqual(0, length(AllTasks)).

%%%===================================================================
%%% Task Retrieval Tests
%%%===================================================================

task_retrieval_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_get_existing_task()),
          ?_test(test_get_nonexistent_task()),
          ?_test(test_get_task_result()),
          ?_test(test_get_task_result_before_completion())
         ]
     end}.

%% @doc Test retrieving an existing task
test_get_existing_task() ->
    Action = #{<<"type">> => <<"get_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Get task via API
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),

    %% Verify: All expected fields present
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Task)),
    ?assert(maps:is_key(<<"status">>, Task)),
    ?assert(maps:is_key(<<"action">>, Task)),
    ?assert(maps:is_key(<<"createdAt">>, Task)),
    ?assert(maps:is_key(<<"updatedAt">>, Task)).

%% @doc Test retrieving a non-existent task
test_get_nonexistent_task() ->
    %% Try to get non-existent task via API
    FakeId = <<"00000000000000000000000000000000">>,
    Result = erlmcp_tasks:get_task(undefined, FakeId),

    %% Verify: Error returned
    ?assertEqual({error, not_found}, Result).

%% @doc Test getting task result after completion
test_get_task_result() ->
    Action = #{<<"type">> => <<"result_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Complete task with result
    Result = #{<<"data">> => [1, 2, 3], <<"count">> => 3},
    ok = erlmcp_tasks:complete_task(TaskId, Result),

    %% Get result via API
    {ok, RetrievedResult} = erlmcp_tasks:get_task_result(undefined, TaskId),

    %% Verify: Result matches
    ?assertEqual(Result, RetrievedResult).

%% @doc Test getting result before task completes
test_get_task_result_before_completion() ->
    Action = #{<<"type">> => <<"no_result_test">>},
    {ok, TaskId} = erlmcp_tasks:create(undefined, Action, #{}),

    %% Try to get result before completion via API
    Result = erlmcp_tasks:get_task_result(undefined, TaskId),

    %% Verify: Error or no result
    ?assertMatch({error, _}, Result).
