-module(erlmcp_tasks_input_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erlmcp.hrl").

%%%===================================================================
%%% Test Suite for input_required state in erlmcp_tasks
%%% Chicago School TDD - Real processes, no mocks, state-based verification
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

input_required_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_input_required_lifecycle/1,
      fun test_input_request_with_partial_results/1,
      fun test_provide_input_resume_task/1,
      fun test_input_required_invalid_transitions/1,
      fun test_multiple_input_required_cycles/1,
      fun test_input_required_with_timeout/1]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_tasks:start_link(),
    {Pid, erlmcp_tasks}.

cleanup({Pid, _ServerName}) ->
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            gen_server:stop(erlmcp_tasks),
            erlmcp_test_sync:wait_for_process_death(Pid, 500);
        false ->
            ok
    end.

%%====================================================================
%% Input Required State Tests
%%====================================================================

%% Test: Task can transition from processing to input_required
test_input_required_lifecycle(_Pid) ->
    fun() ->
       %% Setup: Create task and start processing
       Action = #{<<"type">> => <<"input_required_test">>},
       {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

       WorkerPid =
           spawn(fun() ->
                    receive
                        {task_input, _TaskId, _InputData} ->
                            ok
                    after 5000 ->
                        ok
                    end
                 end),

       ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

       {ok, Task1} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
       ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task1)),

       %% Exercise: Request input (transition processing -> input_required)
       PartialResults = #{<<"processed">> => 100, <<"total">> => 200},
       {ok, input_required} = erlmcp_tasks:request_input(TaskId, PartialResults),

       %% Verify: Task is in input_required state
       {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
       ?assertEqual(<<"input_required">>, maps:get(<<"status">>, Task2)),

       %% Verify: Partial results stored
       ?assertEqual(PartialResults, maps:get(<<"result">>, Task2)),

       %% Verify: Cannot get result while in input_required
       Result = erlmcp_tasks:get_task_result(undefined, TaskId),
       ?assertMatch({error, {task_result_not_ready, _}}, Result)
    end.

%% Test: Input request stores partial results correctly
test_input_request_with_partial_results(_Pid) ->
    fun() ->
       %% Setup: Create task in processing state
       Action = #{<<"type">> => <<"partial_results_test">>},
       {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

       WorkerPid =
           spawn(fun() ->
                    receive
                        {task_input, _TaskId, _InputData} ->
                            ok
                    after 5000 ->
                        ok
                    end
                 end),

       ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

       %% Exercise: Request input with rich partial results
       PartialResults =
           #{<<"stage">> => <<"data_processing">>,
             <<"processedItems">> => 500,
             <<"totalItems">> => 1000,
             <<"intermediateData">> => [1, 2, 3, 4, 5],
             <<"message">> => <<"Please confirm continuation">>},
       {ok, input_required} = erlmcp_tasks:request_input(TaskId, PartialResults),

       %% Verify: All partial data preserved
       {ok, Task} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
       ?assertEqual(PartialResults, maps:get(<<"result">>, Task)),

       %% Verify: Timestamp updated
       CreatedAt = maps:get(<<"createdAt">>, Task),
       UpdatedAt = maps:get(<<"updatedAt">>, Task),
       ?assert(UpdatedAt >= CreatedAt)
    end.

%% Test: Providing input resumes task from input_required to processing
test_provide_input_resume_task(_Pid) ->
    fun() ->
       %% Setup: Create task and transition to input_required
       Action = #{<<"type">> => <<"resume_test">>},
       {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

       WorkerPid =
           spawn(fun() ->
                    %% Worker will receive {task_input, TaskId, InputData}
                    receive
                        {task_input, TaskIdMsg, InputData} ->
                            %% Verify: Input data received correctly
                            ?assertEqual(TaskId, TaskIdMsg),
                            ?assertEqual(#{<<"user_choice">> => <<"continue">>}, InputData),

                            %% Complete task after receiving input
                            erlmcp_tasks:complete_task(TaskId, #{<<"final">> => <<"done">>})
                    after 5000 ->
                        ?assert(false, worker_should_have_received_input)
                    end
                 end),

       ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

       PartialResults = #{<<"stage">> => <<"awaiting_input">>},
       {ok, input_required} = erlmcp_tasks:request_input(TaskId, PartialResults),

       {ok, Task1} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
       ?assertEqual(<<"input_required">>, maps:get(<<"status">>, Task1)),

       %% Exercise: Provide input (transition input_required -> processing)
       InputData = #{<<"user_choice">> => <<"continue">>},
       ok = erlmcp_tasks:provide_input(TaskId, InputData),

       %% Verify: Task resumed to processing state
       {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
       ?assertEqual(<<"processing">>, maps:get(<<"status">>, Task2)),

       %% Give worker time to process and complete
       timer:sleep(100),

       %% Verify: Worker completed task
       {ok, Task3} = erlmcp_tasks:get_task(undefined, TaskId),
       ?assertEqual(<<"completed">>, maps:get(<<"status">>, Task3))
    end.

%% Test: Invalid state transitions are rejected
test_input_required_invalid_transitions(_Pid) ->
    fun() ->
       %% Test 1: Cannot transition from pending to input_required
       Action1 = #{<<"type">> => <<"invalid_transition_1">>},
       {ok, TaskId1} = erlmcp_tasks:create(erlmcp_tasks, Action1, #{}),

       {ok, Task1a} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId1),
       ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task1a)),

       %% Try to request input from pending (should fail)
       Result1 = erlmcp_tasks:request_input(TaskId1, #{}),
       ?assertMatch({error, {task_state_invalid, _}}, Result1),

       %% Test 2: Cannot complete from input_required
       Action2 = #{<<"type">> => <<"invalid_transition_2">>},
       {ok, TaskId2} = erlmcp_tasks:create(erlmcp_tasks, Action2, #{}),

       WorkerPid2 =
           spawn(fun() ->
                    receive after 5000 ->
                        ok
                    end
                 end),
       ok = erlmcp_tasks:start_task_execution(TaskId2, WorkerPid2),
       {ok, input_required} = erlmcp_tasks:request_input(TaskId2, #{}),

       %% Try to complete from input_required (should fail)
       Result2 = erlmcp_tasks:complete_task(TaskId2, #{<<"done">> => true}),
       ?assertMatch({error, {task_state_invalid, _}}, Result2),

       %% Test 3: Cannot fail from input_required
       Result3 = erlmcp_tasks:fail_task(TaskId2, <<"test failure">>),
       ?assertMatch({error, {task_state_invalid, _}}, Result3),

       %% Test 4: Cannot cancel from input_required
       Result4 = erlmcp_tasks:cancel_task(undefined, TaskId2, <<"test cancel">>),
       ?assertMatch({error, {task_state_invalid, _}}, Result4)
    end.

%% Test: Multiple input_required cycles are supported
test_multiple_input_required_cycles(_Pid) ->
    fun() ->
       %% Setup: Create task that will request input multiple times
       Action = #{<<"type">> => <<"multi_input_test">>},
       {ok, TaskId} = erlmcp_tasks:create(erlmcp_tasks, Action, #{}),

       WorkerPid =
           spawn(fun() ->
                    %% Cycle 1: Request first input
                    erlmcp_tasks:request_input(TaskId, #{<<"cycle">> => 1}),
                    receive
                        {task_input, TaskId, Input1} ->
                            ?assertEqual(#{<<"response">> => 1}, Input1)
                    after 1000 ->
                        ?assert(false, timeout_cycle_1)
                    end,

                    %% Cycle 2: Request second input
                    erlmcp_tasks:request_input(TaskId, #{<<"cycle">> => 2}),
                    receive
                        {task_input, TaskId, Input2} ->
                            ?assertEqual(#{<<"response">> => 2}, Input2)
                    after 1000 ->
                        ?assert(false, timeout_cycle_2)
                    end,

                    %% Complete after second input
                    erlmcp_tasks:complete_task(TaskId, #{<<"cycles">> => 2})
                 end),

       ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

       %% Cycle 1: Verify first input_required state
       timer:sleep(50),
       {ok, Task1} = erlmcp_tasks:get_task(undefined, TaskId),
       ?assertEqual(<<"input_required">>, maps:get(<<"status">>, Task1)),
       ?assertEqual(#{<<"cycle">> => 1}, maps:get(<<"result">>, Task1)),

       %% Provide first input
       ok = erlmcp_tasks:provide_input(TaskId, #{<<"response">> => 1}),

       %% Cycle 2: Verify second input_required state
       timer:sleep(50),
       {ok, Task2} = erlmcp_tasks:get_task(undefined, TaskId),
       ?assertEqual(<<"input_required">>, maps:get(<<"status">>, Task2)),
       ?assertEqual(#{<<"cycle">> => 2}, maps:get(<<"result">>, Task2)),

       %% Provide second input
       ok = erlmcp_tasks:provide_input(TaskId, #{<<"response">> => 2}),

       %% Verify: Task completed
       timer:sleep(50),
       {ok, Task3} = erlmcp_tasks:get_task(undefined, TaskId),
       ?assertEqual(<<"completed">>, maps:get(<<"status">>, Task3))
    end.

%% Test: Timeout applies during input_required state
test_input_required_with_timeout(_Pid) ->
    fun() ->
       %% Setup: Create task with short timeout
       Action = #{<<"type">> => <<"timeout_input_test">>},
       Timeout = 100,
       Options = #{timeout_ms => Timeout},
       {ok, TaskId} =
           erlmcp_tasks:create_task(undefined, Action, #{<<"timeout">> => Timeout}, Options),

       WorkerPid =
           spawn(fun() ->
                    %% Request input immediately
                    erlmcp_tasks:request_input(TaskId, #{<<"awaiting">> => true}),
                    receive
                        {task_input, _, _} ->
                            ok
                    after 5000 ->
                        ok
                    end
                 end),

       ok = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

       %% Verify: Task in input_required state
       timer:sleep(20),
       {ok, Task1} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
       ?assertEqual(<<"input_required">>, maps:get(<<"status">>, Task1)),

       %% Wait for timeout
       timer:sleep(150),

       %% Verify: Task failed due to timeout
       {ok, Task2} = erlmcp_tasks:get_task(erlmcp_tasks, TaskId),
       ?assertEqual(<<"failed">>, maps:get(<<"status">>, Task2)),

       ErrorMap = maps:get(<<"error">>, Task2),
       ?assertEqual(<<"Task timeout">>, maps:get(<<"message">>, ErrorMap))
    end.
