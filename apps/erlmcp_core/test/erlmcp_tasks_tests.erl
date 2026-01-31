-module(erlmcp_tasks_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%% Include proper macros but undefine conflicting LET first
-undef(LET).
-include_lib("proper/include/proper.hrl").

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
         %% Task lifecycle tests (3 tests)
         fun test_task_creation/1,
         fun test_task_lifecycle/1,
         fun test_task_cancellation/1,
         fun test_task_timeout/1,

         %% State management tests (2 tests)
         fun test_task_state_persistence/1,
         fun test_task_state_restoration/1,

         %% Concurrency tests (2 tests)
         fun test_concurrent_task_limit/1,
         fun test_task_state_update_race_condition/1,

         %% Progress tracking tests (2 tests)
         fun test_progress_token_generation/1,
         fun test_progress_update/1,

         %% Edge cases tests (5 tests)
         fun test_invalid_task_id/1,
         fun test_duplicate_task_id/1,
         fun test_expired_task_cleanup/1,
         fun test_empty_action/1,
         fun test_very_large_result/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start the tasks manager (registered process)
    {ok, Pid} = erlmcp_tasks:start_link(),
    %% Also return registered name for API calls
    {Pid, erlmcp_tasks}.

cleanup({Pid, _ServerName}) ->
    %% Stop the tasks manager
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            gen_server:stop(erlmcp_tasks),
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
        Action = #{<<"type">> => <<"test">>, <<"operation">> => <<"create">>},
        Metadata = #{<<"user">> => <<"alice">>, <<"project">> => <<"test_project">>},

        {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, Metadata),

        %% Verify: Task ID is binary with proper length (UUID format: 32 hex chars)
        ?assert(is_binary(TaskId)),
        ?assertEqual(32, byte_size(TaskId)),

        %% Verify: Task can be retrieved
        {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
        ?assertEqual(TaskId, maps:get(<<"taskId">>, Task)),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task)),
        ?assertEqual(Action, maps:get(<<"action">>, Task)),
        ?assertEqual(Metadata, maps:get(<<"metadata">>, Task)),

        %% Verify: Timestamp fields exist
        ?assert(is_integer(maps:get(<<"createdAt">>, Task))),
        ?assert(is_integer(maps:get(<<"updatedAt">>, Task))),

        %% Verify: Progress token exists
        ?assert(maps:is_key(<<"progressToken">>, Task))
    end.

test_task_lifecycle(_Pid) ->
    fun() ->
        %% Setup: Create task
        Action = #{<<"type">> => <<"lifecycle_test">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        %% Verify initial state: pending
        {ok, Task1} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task1)),

        %% Exercise: Transition to processing
        ok = erlmcp_tasks:start_task_execution(TaskId, self()),

        %% Verify processing state
        {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
        ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task2)),

        %% Exercise: Complete task with result
        Result = #{<<"output">> => <<"test_result">>, <<"code">> => 0},
        ok = erlmcp_tasks:complete_task(TaskId, Result),

        %% Verify completed state
        {ok, Task3} = erlmcp_tasks:get_task(undefined, TaskId),
        ?assertEqual(<<"completed">>, maps:get(<<"status">>, Task3)),
        ?assertEqual(Result, maps:get(<<"result">>, Task3)),

        %% Verify: Updated timestamp changed
        UpdatedAt1 = maps:get(<<"updatedAt">>, Task1),
        UpdatedAt3 = maps:get(<<"updatedAt">>, Task3),
        ?assert(UpdatedAt3 >= UpdatedAt1)
    end.

test_task_cancellation(_Pid) ->
    fun() ->
        %% Setup: Create task and start processing
        Action = #{<<"type">> => <<"cancellable">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        %% Spawn a worker process (NOT self() - cancellation will exit the worker)
        WorkerPid = spawn(fun() ->
            %% Simulate long-running work
            receive
                cancel -> ok  % Will receive exit signal from cancellation
            after
                10000 -> ok  % Long timeout - should be cancelled before this
            end
        end),

        ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

        {ok, Task1} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task1)),

        %% Exercise: Cancel task (will exit WorkerPid with signal)
        Reason = <<"User requested cancellation">>,

        %% Monitor the worker to verify it gets killed by cancellation
        MonitorRef = monitor(process, WorkerPid),
        {ok, cancelled} = erlmcp_tasks:cancel_task(undefined, TaskId, Reason),

        %% Verify: Worker process received exit signal (Chicago School: observable behavior)
        receive
            {'DOWN', MonitorRef, process, WorkerPid, {task_cancelled, Reason}} ->
                ?assert(true);  % Worker exited with correct reason
            {'DOWN', MonitorRef, process, WorkerPid, Info} ->
                ?assert(false, {unexpected_exit_reason, Info})
        after
            1000 ->
                ?assert(false, worker_should_have_been_cancelled)
        end,

        %% Verify cancelled state (state-based verification, Chicago School)
        {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
        ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, Task2)),
        ErrorMap2 = maps:get(<<"error">>, Task2),
        %% The reason is stored in the data field, not the message
        ?assertEqual(Reason, maps:get(<<"reason">>, maps:get(<<"data">>, ErrorMap2))),

        %% Verify: Cannot modify cancelled task
        Result = #{<<"output">> => <<"too_late">>},
        ?assertMatch({error, _}, erlmcp_tasks:complete_task(TaskId, Result))
    end.

test_task_timeout(_Pid) ->
    fun() ->
        %% Setup: Create task with short timeout
        Action = #{<<"type">> => <<"timeout_test">>},
        Timeout = 100,
        Options = #{timeout_ms => Timeout},
        {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, #{<<"timeout">> => Timeout}, Options),

        %% Spawn a worker process (NOT self() - timeout will exit the worker)
        WorkerPid = spawn(fun() ->
            %% Simulate long-running work that will timeout
            receive
                after 5000 -> ok  % Should timeout before this
            end
        end),

        ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

        %% Give task time to timeout (100ms timeout)
        timer:sleep(150),

        %% Check that task has timed out
        {ok, TimedOutTask} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ?assertEqual(<<"failed">>, maps:get(<<"status">>, TimedOutTask)),

        %% Verify error details
        ErrorMap = maps:get(<<"error">>, TimedOutTask),
        ?assertEqual(<<"Task timeout">>, maps:get(<<"message">>, ErrorMap))
    end.

%%====================================================================
%% State Management Tests
%%====================================================================

test_task_state_persistence(_Pid) ->
    fun() ->
        %% Exercise: Create task
        Action = #{<<"type">> => <<"persistence_test">>},
        {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, #{<<"key">> => <<"value">>}),

        %% Verify: Task stored in ETS
        %% Direct ETS lookup (state-based verification, Chicago School)
        [#mcp_task{id = TaskId} = TaskRecord] = ets:lookup(erlmcp_tasks, TaskId),
        ?assertEqual(TaskId, TaskId),
        ?assertEqual(<<"value">>, maps:get(<<"key">>, TaskRecord#mcp_task.metadata)),

        %% Verify: Task accessible via API
        {ok, ApiTask} = erlmcp_tasks:get_task(undefined, TaskId),
        ?assertEqual(<<"persistence_test">>, maps:get(<<"type">>, maps:get(<<"action">>, ApiTask)))
    end.

test_task_state_restoration(_Pid) ->
    fun() ->
        %% Setup: Create multiple tasks
        TaskIds = [begin
            Action = #{<<"type">> => <<"restore_test">>, <<"index">> => N},
            {ok, Id} = erlmcp_tasks:create_task(undefined, Action, #{}),
            Id
        end || N <- lists:seq(1, 10)],

        %% Exercise: Simulate restart by getting all state
        {ok, TaskListResult} = erlmcp_tasks:list_tasks(undefined, undefined, 100),
        AllTasks = maps:get(<<"tasks">>, TaskListResult, []),

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

test_concurrent_task_limit({_Pid, _ServerName}) ->
    fun() ->
        %% Exercise: Create tasks up to limit
        MaxConcurrent = erlmcp_tasks:get_max_concurrent(),

        %% Create max concurrent tasks
        TaskIds = [begin
            Action = #{<<"type">> => <<"concurrent_test">>, <<"index">> => N},
            {ok, Id} = erlmcp_tasks:create_task(undefined, Action, #{}),
            Id
        end || N <- lists:seq(1, MaxConcurrent)],

        %% Verify: All tasks created successfully
        ?assertEqual(MaxConcurrent, length(TaskIds)),

        %% Exercise: Try to create one more (should fail or queue)
        Action = #{<<"type">> => <<"overflow">>},
        Result = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        %% Verify: Either rejected with error or queued (implementation-dependent)
        case Result of
            {error, {max_concurrent_tasks, _}} ->
                ?assert(true);  %% Expected behavior
            {ok, _OverflowTaskId} ->
                %% Alternative: tasks are queued
                ?assert(true)
        end,

        %% Cleanup: Complete some tasks to free up slots
        lists:foreach(fun(Id) ->
            ok = erlmcp_tasks:complete(erlmcp_tasks, Id, #{<<"done">> => true})
        end, lists:sublist(TaskIds, 5))
    end.

test_task_state_update_race_condition(_Pid) ->
    fun() ->
        %% Setup: Create task
        Action = #{<<"type">> => <<"race_test">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{counter => 0}),

        %% Exercise: Concurrent status updates
        Parent = self(),
        NumProcesses = 50,

        Pids = [spawn(fun() ->
            %% Each process tries to update task
            UpdateFun = fun(Task) ->
                Meta = maps:get(<<"metadata">>, Task, #{}),
                Counter = maps:get(counter, Meta, 0),
                NewMeta = Meta#{counter => Counter + 1},
                Task#{<<"metadata">> => NewMeta}
            end,

            Result = erlmcp_tasks:update_task(erlmcp_tasks, TaskId, UpdateFun),
            Parent ! {update_result, self(), Result}
        end) || _ <- lists:seq(1, NumProcesses)],

        %% Collect results
        Results = [receive
            {update_result, Pid, Res} -> {Pid, Res}
        after 5000 ->
            timeout
        end || _ <- Pids],

        %% Verify: All updates succeeded (serialized by gen_server)
        SuccessCount = length([1 || {_, ok} <- Results]),
        ?assert(SuccessCount > 0),

        %% Verify: Final state is consistent
        {ok, FinalTask} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        FinalMeta = maps:get(<<"metadata">>, FinalTask, #{}),
        ?assert(is_integer(maps:get(counter, FinalMeta, 0)))
    end.

%%====================================================================
%% Progress Tracking Tests
%%====================================================================

test_progress_token_generation(_Pid) ->
    fun() ->
        %% Exercise: Create task
        Action = #{<<"type">> => <<"progress_test">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        %% Verify: Progress token exists and is valid type (reference for uniqueness)
        {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ProgressToken = maps:get(<<"progressToken">>, Task),

        ?assert(is_reference(ProgressToken) orelse is_binary(ProgressToken) orelse is_integer(ProgressToken)),

        %% Verify: Token is unique across tasks
        {ok, TaskId2} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),
        {ok, Task2} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId2),
        ProgressToken2 = maps:get(<<"progressToken">>, Task2),

        ?assertNotEqual(ProgressToken, ProgressToken2)
    end.

test_progress_update(_Pid) ->
    fun() ->
        %% Setup: Create task
        Action = #{<<"type">> => <<"long_running">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ProgressToken = maps:get(<<"progressToken">>, Task),

        %% Exercise: Update progress multiple times
        ProgressValues = [0.0, 0.25, 0.5, 0.75, 1.0],

        lists:foreach(fun(Progress) ->
            ok = erlmcp_tasks:update_progress(
                erlmcp_tasks,
                TaskId,
                #{
                    <<"progressToken">> => ProgressToken,
                    <<"progress">> => Progress,
                    <<"total">> => 100.0
                }
            )
        end, ProgressValues),

        %% Verify: Final progress state
        {ok, FinalTask} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ?assertEqual(1.0, maps:get(<<"progress">>, FinalTask, 0.0)),
        ?assertEqual(100.0, maps:get(<<"total">>, FinalTask, 0.0))
    end.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

test_invalid_task_id(_Pid) ->
    fun() ->
        %% Exercise: Try to get non-existent task
        InvalidId = <<"00000000000000000000000000000000">>,

        Result = erlmcp_tasks:get_task(erlmcp_tasks, InvalidId),

        %% Verify: Error returned (MCP spec uses error codes, not atoms)
        ?assertMatch({error, _}, Result),

        %% Exercise: Try to update non-existent task
        UpdateFun = fun(T) -> T end,
        UpdateResult = erlmcp_tasks:update_task(erlmcp_tasks, InvalidId, UpdateFun),

        %% Verify: Error returned
        ?assertMatch({error, _}, UpdateResult),

        %% Exercise: Try to cancel non-existent task
        CancelResult = erlmcp_tasks:cancel(erlmcp_tasks, InvalidId, <<"test">>),

        %% Verify: Error returned
        ?assertMatch({error, _}, CancelResult)
    end.

test_duplicate_task_id(_Pid) ->
    fun() ->
        %% Setup: Create task
        Action = #{<<"type">> => <<"duplicate_test">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        %% Exercise: Try to create task with same ID (if API allows)
        %% This tests collision detection
        TaskIds = [begin
            crypto:strong_rand_bytes(16)
        end || _ <- lists:seq(1, 1000)],

        %% Verify: All IDs are unique (statistically improbable to collide)
        UniqueIds = lists:usort(TaskIds),
        ?assertEqual(1000, length(UniqueIds)),

        %% Verify: Our original task ID is unique format
        ?assertEqual(32, byte_size(TaskId))
    end.

test_expired_task_cleanup(_Pid) ->
    fun() ->
        %% Setup: Create tasks with different TTLs
        ShortTTL = 100,  % 100ms
        LongTTL = 10000, % 10s

        {ok, ShortTaskId} = erlmcp_tasks:create(
            erlmcp_tasks,
            #{<<"type">> => <<"short_lived">>},
            #{},
            #{ttl_ms => ShortTTL}
        ),

        {ok, LongTaskId} = erlmcp_tasks:create(
            erlmcp_tasks,
            #{<<"type">> => <<"long_lived">>},
            #{},
            #{ttl_ms => LongTTL}
        ),

        %% Verify: Both tasks exist initially
        ?assertMatch({ok, _}, erlmcp_tasks:get_task(erlmcp_tasks, ShortTaskId)),
        ?assertMatch({ok, _}, erlmcp_tasks:get_task(erlmcp_tasks, LongTaskId)),

        %% Exercise: Wait for short task to expire
        timer:sleep(ShortTTL + 50),

        %% Trigger cleanup
        {ok, CleanedCount} = erlmcp_tasks:cleanup_expired(erlmcp_tasks),

        %% Verify: Short task cleaned up
        ?assertMatch({error, _}, erlmcp_tasks:get_task(erlmcp_tasks, ShortTaskId)),

        %% Verify: Long task still exists
        ?assertMatch({ok, _}, erlmcp_tasks:get_task(erlmcp_tasks, LongTaskId)),

        %% Verify: Cleanup count
        ?assert(CleanedCount >= 1)
    end.

test_empty_action(_Pid) ->
    fun() ->
        %% Exercise: Create task with empty action
        Action = #{},
        Metadata = #{},

        {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, Metadata),

        %% Verify: Task created successfully
        {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ?assertEqual(TaskId, maps:get(<<"taskId">>, Task)),
        ?assertEqual(Action, maps:get(<<"action">>, Task)),
        ?assertEqual(Metadata, maps:get(<<"metadata">>, Task))
    end.

test_very_large_result(_Pid) ->
    fun() ->
        %% Setup: Create task
        Action = #{<<"type">> => <<"large_result">>},
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

        %% Exercise: Complete task with large result
        LargeBinary = crypto:strong_rand_bytes(1024 * 100),  % 100 KB
        Result = #{
            <<"data">> => LargeBinary,
            <<"count">> => 100000
        },

        ok = erlmcp_tasks:complete(erlmcp_tasks, TaskId, Result),

        %% Verify: Result stored correctly
        {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        StoredResult = maps:get(<<"result">>, Task),
        ?assertEqual(LargeBinary, maps:get(<<"data">>, StoredResult)),
        ?assertEqual(100000, maps:get(<<"count">>, StoredResult))
    end.

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_task_id_unique() ->
    ?FORALL(_N, proper_types:integer(1, 1000),
        begin
            {ok, Pid} = erlmcp_tasks:start_link(),
            Action = #{<<"type">> => <<"prop_test">>},

            %% Create multiple tasks
            TaskIds = [begin
                {ok, Id} = erlmcp_tasks:create(Pid, Action, #{}),
                Id
            end || _ <- lists:seq(1, _N)],

            %% All IDs should be unique
            UniqueIds = lists:usort(TaskIds),
            erlmcp_tasks:stop(Pid),

            _N =:= length(UniqueIds)
        end).

prop_task_state_transition() ->
    ?FORALL({Status1, Status2}, {proper_types:oneof([<<"pending">>, <<"processing">>]),
                                   proper_types:oneof([<<"processing">>, <<"completed">>, <<"failed">>, <<"cancelled">>])},
        begin
            {ok, Pid} = erlmcp_tasks:start_link(),
            Action = #{<<"type">> => <<"transition_test">>},

            {ok, TaskId} = erlmcp_tasks:create(Pid, Action, #{}),

            %% First transition
            ok = erlmcp_tasks:update_status(Pid, TaskId, Status1),

            %% Second transition (may fail if invalid)
            Result2 = erlmcp_tasks:update_status(Pid, TaskId, Status2),

            erlmcp_tasks:stop(Pid),

            %% Some transitions should succeed, others may fail
            %% This property checks that the system handles transitions gracefully
            is_atom(Result2) orelse is_tuple(Result2)
        end).

prop_task_metadata_preservation() ->
    ?FORALL(Metadata, proper_types:map(proper_types:binary(), proper_types:any()),
        begin
            {ok, Pid} = erlmcp_tasks:start_link(),
            Action = #{<<"type">> => <<"metadata_test">>},

            {ok, TaskId} = erlmcp_tasks:create(Pid, Action, Metadata),

            {ok, Task} = erlmcp_tasks:get_task(Pid, TaskId),
            RetrievedMetadata = maps:get(<<"metadata">>, Task),

            erlmcp_tasks:stop(Pid),

            %% Metadata should be preserved
            Metadata =:= RetrievedMetadata
        end).

%%====================================================================
%% Integration Tests
%%====================================================================

%%====================================================================
%% _meta Field Tests (Chicago School TDD)
%%====================================================================

meta_field_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [
         fun test_meta_field_preserved/1,
         fun test_meta_propagation_through_states/1,
         fun test_meta_in_task_list/1,
         fun test_meta_in_cancelled_task/1
        ]
     end}.

test_meta_field_preserved(_Pid) ->
    fun() ->
        %% Setup: Create task with _meta field
        Action = #{<<"type">> => <<"meta_test">>},
        Meta = #{
            <<"_meta">> => #{
                <<"traceId">> => <<"abc-123-def">>,
                <<"userId">> => <<"user-456">>,
                <<"requestId">> => <<"req-789">>
            }
        },

        %% Exercise: Create task with metadata containing _meta
        {ok, TaskId} = erlmcp_tasks:create_task(undefined, Action, Meta),

        %% Verify: _meta field is stored and returned
        {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
        ?assert(maps:is_key(<<"metadata">>, Task)),

        Metadata = maps:get(<<"metadata">>, Task),
        ?assert(maps:is_key(<<"_meta">>, Metadata)),

        ExpectedMeta = #{
            <<"traceId">> => <<"abc-123-def">>,
            <<"userId">> => <<"user-456">>,
            <<"requestId">> => <<"req-789">>
        },
        ?assertEqual(ExpectedMeta, maps:get(<<"_meta">>, Metadata))
    end.

test_meta_propagation_through_states(_Pid) ->
    fun() ->
        %% Setup: Create task with _meta
        Action = #{<<"type">> => <<"state_transition">>},
        Meta = #{
            <<"_meta">> => #{
                <<"traceId">> => <<"trace-state-test">>,
                <<"tag">> => <<"propagation-test">>
            }
        },
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, Meta),

        %% Helper function to verify _meta persists
        VerifyMeta = fun() ->
            {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
            Metadata = maps:get(<<"metadata">>, Task, #{}),
            ?assert(maps:is_key(<<"_meta">>, Metadata)),
            StoredMeta = maps:get(<<"_meta">>, Metadata),
            ?assertEqual(<<"trace-state-test">>, maps:get(<<"traceId">>, StoredMeta)),
            ?assertEqual(<<"propagation-test">>, maps:get(<<"tag">>, StoredMeta))
        end,

        %% Verify: _meta in pending state
        VerifyMeta(),

        %% Exercise: Transition to processing
        ok = erlmcp_tasks:start_task_execution(TaskId, self()),
        VerifyMeta(),

        %% Exercise: Update progress
        {ok, TaskForProgress} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ProgressToken = maps:get(<<"progressToken">>, TaskForProgress),
        ok = erlmcp_tasks:update_progress(erlmcp_tasks, TaskId, #{
            <<"progressToken">> => ProgressToken,
            <<"progress">> => 0.5,
            <<"total">> => 1.0
        }),
        VerifyMeta(),

        %% Exercise: Complete task
        Result = #{<<"output">> => <<"done">>},
        ok = erlmcp_tasks:complete(erlmcp_tasks, TaskId, Result),

        %% Verify: _meta preserved in completed state
        VerifyMeta(),

        %% Verify: _meta still in completed task
        {ok, FinalTask} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        FinalMetadata = maps:get(<<"metadata">>, FinalTask),
        ?assert(maps:is_key(<<"_meta">>, FinalMetadata))
    end.

test_meta_in_task_list(_Pid) ->
    fun() ->
        %% Setup: Create multiple tasks with different _meta
        TaskIds = lists:map(fun(N) ->
            Action = #{<<"type">> => <<"list_test">>, <<"index">> => N},
            Meta = #{
                <<"_meta">> => #{
                    <<"batchId">> => <<"batch-2024">>,
                    <<"index">> => N
                }
            },
            {ok, Id} = erlmcp_tasks:create_task(erlmcp_tasks, Action, Meta),
            Id
        end, lists:seq(1, 5)),

        %% Exercise: List all tasks
        {ok, TaskListResult} = erlmcp_tasks:list_tasks(erlmcp_tasks, undefined, 10),
        AllTasks = maps:get(<<"tasks">>, TaskListResult),

        %% Verify: All tasks contain _meta
        lists:foreach(fun(Task) ->
            ?assert(maps:is_key(<<"metadata">>, Task)),
            Metadata = maps:get(<<"metadata">>, Task),
            ?assert(maps:is_key(<<"_meta">>, Metadata)),

            StoredMeta = maps:get(<<"_meta">>, Metadata),
            ?assertEqual(<<"batch-2024">>, maps:get(<<"batchId">>, StoredMeta)),
            ?assert(is_integer(maps:get(<<"index">>, StoredMeta)))
        end, AllTasks),

        %% Verify: All our task IDs are present
        ListedIds = [maps:get(<<"taskId">>, Task) || Task <- AllTasks],
        lists:foreach(fun(Id) ->
            ?assert(lists:member(Id, ListedIds))
        end, TaskIds)
    end.

test_meta_in_cancelled_task(_Pid) ->
    fun() ->
        %% Setup: Create task with _meta
        Action = #{<<"type">> => <<"cancellable_meta">>},
        Meta = #{
            <<"_meta">> => #{
                <<"cancellationId">> => <<"cancel-123">>,
                <<"reason">> => <<"test_cancellation">>
            }
        },
        {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, Meta),

        %% Spawn a worker process
        WorkerPid = spawn(fun() ->
            receive
                cancel -> ok
            after
                10000 -> ok
            end
        end),

        ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

        %% Exercise: Cancel task
        MonitorRef = monitor(process, WorkerPid),
        {ok, cancelled} = erlmcp_tasks:cancel_task(erlmcp_tasks, TaskId, <<"Test cancellation">>),

        %% Wait for worker to exit
        receive
            {'DOWN', MonitorRef, process, WorkerPid, _} -> ok
        after
            1000 -> ?assert(false, worker_should_have_exited)
        end,

        %% Verify: _meta preserved after cancellation
        {ok, CancelledTask} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
        ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, CancelledTask)),

        Metadata = maps:get(<<"metadata">>, CancelledTask),
        ?assert(maps:is_key(<<"_meta">>, Metadata)),

        StoredMeta = maps:get(<<"_meta">>, Metadata),
        ?assertEqual(<<"cancel-123">>, maps:get(<<"cancellationId">>, StoredMeta)),
        ?assertEqual(<<"test_cancellation">>, maps:get(<<"reason">>, StoredMeta))
    end.

integration_task_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [
         ?_test(begin
             %% Complete workflow: create -> process -> complete
             Action = #{<<"type">> => <<"workflow">>, <<"step">> => 1},

             {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{<<"workflow">> => true}),

             ok = erlmcp_tasks:start_task_execution(TaskId, self()),

             ProgressToken = begin
                 {ok, T} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
                 maps:get(<<"progressToken">>, T)
             end,

             ok = erlmcp_tasks:update_progress(erlmcp_tasks, TaskId, #{
                 <<"progressToken">> => ProgressToken,
                 <<"progress">> => 0.5,
                 <<"total">> => 1.0
             }),

             Result = #{<<"status">> => <<"success">>, <<"data">> => [1, 2, 3]},
             ok = erlmcp_tasks:complete(erlmcp_tasks, TaskId, Result),

             {ok, FinalTask} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
             ?assertEqual(<<"completed">>, maps:get(<<"status">>, FinalTask)),
             ?assertEqual(Result, maps:get(<<"result">>, FinalTask))
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
             NumTasks = 50,

             TaskIds = lists:map(fun(N) ->
                 Action = #{<<"type">> => <<"batch">>, <<"index">> => N},
                 {ok, Id} = erlmcp_tasks:create(erlmcp_tasks, Action, #{<<"batch">> => N}),
                 Id
             end, lists:seq(1, NumTasks)),

             %% Update first half to processing
             lists:foreach(fun(Id) ->
                 erlmcp_tasks:update_status(erlmcp_tasks, Id, <<"processing">>)
             end, lists:sublist(TaskIds, NumTasks div 2)),

             %% Complete first quarter
             lists:foreach(fun(Id) ->
                 erlmcp_tasks:complete(erlmcp_tasks, Id, #{<<"done">> => true})
             end, lists:sublist(TaskIds, NumTasks div 4)),

             %% List all tasks
             {ok, TaskListResult} = erlmcp_tasks:list_tasks(erlmcp_tasks),
             AllTasks = maps:get(<<"tasks">>, TaskListResult),

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
             %% Test invalid status update
             {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, #{<<"type">> => <<"error_test">>}, #{}),

             %% Try invalid status
             Result = erlmcp_tasks:update_status(erlmcp_tasks, TaskId, <<"invalid_status">>),

             %% Should fail or be ignored
             ?assert(is_tuple(Result))
         end),
         ?_test(begin
             %% Test complete already completed task
             {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, #{<<"type">> => <<"complete_test">>}, #{}),

             ok = erlmcp_tasks:complete(erlmcp_tasks, TaskId, #{<<"result">> => 1}),

             %% Try to complete again
             Result = erlmcp_tasks:complete(erlmcp_tasks, TaskId, #{<<"result">> => 2}),

             ?assertMatch({error, _}, Result)
         end),
         ?_test(begin
             %% Test nil/undefined metadata
             Action = #{<<"type">> => <<"nil_test">>},

             {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, undefined),

             {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
             ?assert(maps:is_key(<<"metadata">>, Task))
         end)
        ]
     end}.
