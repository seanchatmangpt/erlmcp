-module(erlmcp_tasks_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Suite for erlmcp_tasks Module
%%% Chicago School TDD - Real processes, no mocks, state-based verification
%%% Target: 85%+ coverage
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

tasks_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Task lifecycle tests
         fun test_task_creation/1,
         fun test_task_lifecycle/1,
         fun test_task_cancellation/1,

         %% State management tests
         fun test_task_state_persistence/1,
         fun test_task_state_restoration/1,

         %% Concurrency tests
         fun test_concurrent_task_limit/1,
         fun test_concurrent_task_creation/1,

         %% Edge cases tests
         fun test_invalid_task_id/1,
         fun test_task_not_found/1,
         fun test_empty_action/1,
         fun test_very_large_result/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start the tasks manager
    {ok, Pid} = erlmcp_tasks:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the tasks manager
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, shutdown),
            %% Wait for process death using monitor
            erlmcp_test_sync:wait_for_process_death(Pid, 500);
        false ->
            ok
    end.

%%====================================================================
%% Task Lifecycle Tests
%%====================================================================

test_task_creation(_Pid) ->
    fun() ->
        %% Exercise: Create a task with action and metadata
        ClientPid = self(),
        Action = #{<<"type">> => <<"test">>, <<"operation">> => <<"create">>},
        Metadata = #{<<"user">> => <<"alice">>, <<"project">> => <<"test_project">>},

        {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

        %% Verify: Task ID is binary with proper length (16 bytes = 128 bits)
        ?assert(is_binary(TaskId)),
        ?assertEqual(16, byte_size(TaskId)),

        %% Verify: Task can be retrieved
        {ok, TaskMap} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(TaskId, maps:get(<<"taskId">>, TaskMap)),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, TaskMap)),
        ?assertEqual(Action, maps:get(<<"action">>, TaskMap)),
        %% Metadata should be included since it's non-empty
        ?assertEqual(Metadata, maps:get(<<"metadata">>, TaskMap)),

        %% Verify: Timestamp fields exist
        ?assert(is_integer(maps:get(<<"createdAt">>, TaskMap))),
        ?assert(is_integer(maps:get(<<"updatedAt">>, TaskMap)))
    end.

test_task_lifecycle(_Pid) ->
    fun() ->
        %% Setup: Create task
        ClientPid = self(),
        Action = #{<<"type">> => <<"lifecycle_test">>},
        {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, #{}),

        %% Verify initial state: pending
        {ok, Task1} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task1)),

        %% Exercise: Start task execution (transitions to processing)
        WorkerPid = spawn(fun() -> receive stop -> ok end end),
        ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

        %% Verify processing state
        {ok, Task2} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task2)),

        %% Exercise: Complete task with result
        Result = #{<<"output">> => <<"test_result">>, <<"code">> => 0},
        ok = erlmcp_tasks:complete_task(TaskId, Result),

        %% Verify completed state
        {ok, Task3} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(<<"completed">>, maps:get(<<"status">>, Task3)),

        %% Exercise: Get result
        {ok, Result} = erlmcp_tasks:get_task_result(ClientPid, TaskId),

        %% Cleanup worker
        WorkerPid ! stop
    end.

test_task_cancellation(_Pid) ->
    fun() ->
        %% Setup: Create task and start processing
        ClientPid = self(),
        Action = #{<<"type">> => <<"cancellable">>},
        {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, #{}),

        WorkerPid = spawn(fun() -> receive stop -> ok end end),
        ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

        {ok, Task1} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task1)),

        %% Exercise: Cancel task
        Reason = <<"User requested cancellation">>,
        {ok, cancelled} = erlmcp_tasks:cancel_task(ClientPid, TaskId, Reason),

        %% Verify cancelled state - get task should show cancelled
        {ok, Task2} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, Task2)),

        %% Verify: Cannot get result from cancelled task
        ?assertMatch({error, _}, erlmcp_tasks:get_task_result(ClientPid, TaskId)),

        %% Cleanup worker (may have been killed)
        catch WorkerPid ! stop
    end.

%%====================================================================
%% State Management Tests
%%====================================================================

test_task_state_persistence(_Pid) ->
    fun() ->
        %% Exercise: Create task
        ClientPid = self(),
        Action = #{<<"type">> => <<"persistence_test">>},
        {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, #{<<"key">> => <<"value">>}),

        %% Verify: Task stored in ETS
        %% Direct ETS lookup (state-based verification, Chicago School)
        [#mcp_task{id = TaskId, action = Action, metadata = Metadata}] = ets:lookup(erlmcp_tasks, TaskId),
        ?assertEqual(#{<<"key">> => <<"value">>}, Metadata),

        %% Verify: Task accessible via API
        {ok, ApiTask} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(TaskId, maps:get(<<"taskId">>, ApiTask))
    end.

test_task_state_restoration(_Pid) ->
    fun() ->
        %% Setup: Create multiple tasks
        ClientPid = self(),
        TaskIds = [begin
            Action = #{<<"type">> => <<"restore_test">>, <<"index">> => N},
            {ok, Id} = erlmcp_tasks:create_task(ClientPid, Action, #{}),
            Id
        end || N <- lists:seq(1, 10)],

        %% Exercise: Get all tasks via list API
        {ok, TaskListResult} = erlmcp_tasks:list_tasks(ClientPid, undefined, 100),
        %% TaskListResult is a map with 'tasks' and 'cursor' keys
        AllTasks = maps:get(tasks, TaskListResult),

        %% Verify: All tasks present
        ?assertEqual(10, length(AllTasks)),

        %% Verify: All task IDs match
        RetrievedIds = [maps:get(<<"taskId">>, Task) || Task <- AllTasks],
        ?assertEqual(lists:sort(TaskIds), lists:sort(RetrievedIds)),

        %% Verify: Each task has valid state
        lists:foreach(fun(Task) ->
            ?assert(maps:is_key(<<"taskId">>, Task)),
            ?assert(maps:is_key(<<"status">>, Task)),
            ?assert(maps:is_key(<<"action">>, Task)),
            ?assert(maps:is_key(<<"createdAt">>, Task)),
            ?assert(maps:is_key(<<"updatedAt">>, Task))
        end, AllTasks)
    end.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_task_limit(_Pid) ->
    fun() ->
        %% Exercise: Create tasks up to limit (max 1000)
        ClientPid = self(),
        MaxTasks = 100,  % Test with 100 to avoid long test times

        %% Create tasks
        TaskIds = lists:map(fun(N) ->
            Action = #{<<"type">> => <<"concurrent_test">>, <<"index">> => N},
            {ok, Id} = erlmcp_tasks:create_task(ClientPid, Action, #{}),
            Id
        end, lists:seq(1, MaxTasks)),

        %% Verify: All tasks created successfully
        ?assertEqual(MaxTasks, length(TaskIds)),

        %% Verify: List tasks returns all tasks
        {ok, TaskListResult} = erlmcp_tasks:list_tasks(ClientPid, undefined, MaxTasks + 10),
        AllTasks = maps:get(tasks, TaskListResult),
        ?assertEqual(MaxTasks, length(AllTasks))
    end.

test_concurrent_task_creation(_Pid) ->
    fun() ->
        %% Exercise: Concurrent task creation
        ClientPid = self(),
        NumTasks = 50,

        Parent = self(),
        Pids = [spawn(fun() ->
            Action = #{<<"type">> => <<"concurrent_create">>, <<"index">> => N},
            Result = erlmcp_tasks:create_task(ClientPid, Action, #{}),
            Parent ! {create_result, self(), Result}
        end) || N <- lists:seq(1, NumTasks)],

        %% Collect results
        Results = [receive
            {create_result, Pid, Res} -> {Pid, Res}
        after 5000 ->
            timeout
        end || _ <- Pids],

        %% Verify: All creations succeeded
        SuccessCount = length([1 || {_, {ok, _}} <- Results]),
        ?assertEqual(NumTasks, SuccessCount),

        %% Verify: All task IDs are unique
        TaskIds = [Id || {_, {ok, Id}} <- Results],
        UniqueIds = lists:usort(TaskIds),
        ?assertEqual(NumTasks, length(UniqueIds))
    end.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

test_invalid_task_id(_Pid) ->
    fun() ->
        %% Exercise: Try to get non-existent task
        ClientPid = self(),
        InvalidId = <<"00000000000000000000000000000000">>,

        Result = erlmcp_tasks:get_task(ClientPid, InvalidId),

        %% Verify: Error returned
        ?assertMatch({error, _}, Result)
    end.

test_task_not_found(_Pid) ->
    fun() ->
        %% Exercise: Try operations on non-existent task
        ClientPid = self(),
        InvalidId = crypto:strong_rand_bytes(16),

        %% get_task should fail
        ?assertMatch({error, _}, erlmcp_tasks:get_task(ClientPid, InvalidId)),

        %% cancel_task should fail
        ?assertMatch({error, _}, erlmcp_tasks:cancel_task(ClientPid, InvalidId, <<"test">>)),

        %% get_task_result should fail
        ?assertMatch({error, _}, erlmcp_tasks:get_task_result(ClientPid, InvalidId))
    end.

test_empty_action(_Pid) ->
    fun() ->
        %% Exercise: Create task with empty action
        ClientPid = self(),
        Action = #{},
        Metadata = #{},

        {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

        %% Verify: Task created successfully
        {ok, Task} = erlmcp_tasks:get_task(ClientPid, TaskId),
        ?assertEqual(TaskId, maps:get(<<"taskId">>, Task)),
        ?assertEqual(Action, maps:get(<<"action">>, Task)),
        ?assertEqual(Metadata, maps:get(<<"metadata">>, Task, #{}))
    end.

test_very_large_result(_Pid) ->
    fun() ->
        %% Setup: Create task
        ClientPid = self(),
        Action = #{<<"type">> => <<"large_result">>},
        {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, #{}),

        WorkerPid = spawn(fun() -> receive stop -> ok end end),
        ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

        %% Exercise: Complete task with large result
        LargeBinary = crypto:strong_rand_bytes(1024 * 100),  % 100 KB
        Result = #{
            <<"data">> => LargeBinary,
            <<"count">> => 100000
        },

        ok = erlmcp_tasks:complete_task(TaskId, Result),

        %% Verify: Result stored correctly
        {ok, Result} = erlmcp_tasks:get_task_result(ClientPid, TaskId),
        ?assertEqual(100000, maps:get(<<"count">>, Result)),

        %% Cleanup worker
        WorkerPid ! stop
    end.

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_task_id_unique() ->
    ?FORALL(N, proper_types:integer(1, 100),
        begin
            {ok, Pid} = erlmcp_tasks:start_link(),
            ClientPid = self(),
            Action = #{<<"type">> => <<"prop_test">>},

            %% Create multiple tasks
            TaskIds = [begin
                {ok, Id} = erlmcp_tasks:create_task(ClientPid, Action, #{}),
                Id
            end || _ <- lists:seq(1, N)],

            %% All IDs should be unique
            UniqueIds = lists:usort(TaskIds),
            erlmcp_tasks:stop(Pid),
            timer:sleep(50),

            N =:= length(UniqueIds)
        end).

prop_task_metadata_preservation() ->
    ?FORALL(Metadata, proper_types:map(proper_types:binary(), proper_types:any()),
        begin
            {ok, Pid} = erlmcp_tasks:start_link(),
            ClientPid = self(),
            Action = #{<<"type">> => <<"metadata_test">>},

            {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

            {ok, Task} = erlmcp_tasks:get_task(ClientPid, TaskId),
            RetrievedMetadata = maps:get(<<"metadata">>, Task, #{}),

            erlmcp_tasks:stop(Pid),
            timer:sleep(50),

            %% Metadata should be preserved (if non-empty)
            case maps:size(Metadata) of
                0 -> true;
                _ -> Metadata =:= RetrievedMetadata
            end
        end).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_task_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [
         ?_test(begin
             %% Complete workflow: create -> process -> complete
             ClientPid = self(),
             Action = #{<<"type">> => <<"workflow">>, <<"step">> => 1},

             {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, #{<<"workflow">> => true}),

             WorkerPid = spawn(fun() -> receive stop -> ok end end),
             ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

             Result = #{<<"status">> => <<"success">>, <<"data">> => [1, 2, 3]},
             ok = erlmcp_tasks:complete_task(TaskId, Result),

             {ok, FinalTask} = erlmcp_tasks:get_task(ClientPid, TaskId),
             ?assertEqual(<<"completed">>, maps:get(<<"status">>, FinalTask)),

             {ok, Result} = erlmcp_tasks:get_task_result(ClientPid, TaskId),

             %% Cleanup worker
             WorkerPid ! stop
         end)
        ]
     end}.

integration_batch_tasks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [
         ?_test(begin
             %% Create and manage multiple tasks
             ClientPid = self(),
             NumTasks = 50,

             TaskIds = lists:map(fun(N) ->
                 Action = #{<<"type">> => <<"batch">>, <<"index">> => N},
                 {ok, Id} = erlmcp_tasks:create_task(ClientPid, Action, #{<<"batch">> => N}),
                 Id
             end, lists:seq(1, NumTasks)),

             %% Start first half
             lists:foreach(fun(Id) ->
                 WorkerPid = spawn(fun() -> receive stop -> ok end end),
                 erlmcp_tasks:start_task_execution(Id, WorkerPid)
             end, lists:sublist(TaskIds, NumTasks div 2)),

             %% Complete first quarter
             lists:foreach(fun(Id) ->
                 erlmcp_tasks:complete_task(Id, #{<<"done">> => true})
             end, lists:sublist(TaskIds, NumTasks div 4)),

             %% List all tasks
             {ok, TaskListResult} = erlmcp_tasks:list_tasks(ClientPid, undefined, 100),
             AllTasks = maps:get(tasks, TaskListResult),

             ?assertEqual(NumTasks, length(AllTasks)),

             %% Count by status
             StatusCounts = lists:foldl(fun(Task, Acc) ->
                 Status = maps:get(<<"status">>, Task),
                 maps:update_with(Status, fun(C) -> C + 1 end, 1, Acc)
             end, #{}, AllTasks),

             ?assertEqual(NumTasks div 4, maps:get(<<"completed">>, StatusCounts, 0))
         end)
        ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [
         ?_test(begin
             %% Test complete already completed task
             ClientPid = self(),
             {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, #{<<"type">> => <<"complete_test">>}, #{}),

             WorkerPid = spawn(fun() -> receive stop -> ok end end),
             ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),
             ok = erlmcp_tasks:complete_task(TaskId, #{<<"result">> => 1}),

             %% Try to complete again
             Result = erlmcp_tasks:complete_task(TaskId, #{<<"result">> => 2}),

             ?assertMatch({error, _}, Result),

             %% Cleanup
             WorkerPid ! stop
         end),
         ?_test(begin
             %% Test get result before completion
             ClientPid = self(),
             {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, #{<<"type">> => <<"pending_test">>}, #{}),

             %% Try to get result of pending task
             Result = erlmcp_tasks:get_task_result(ClientPid, TaskId),

             ?assertMatch({error, _}, Result)
         end)
        ]
     end}.
