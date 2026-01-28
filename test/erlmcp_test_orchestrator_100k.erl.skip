%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP Test Orchestration Engine for 100K Scale
%%%
%%% Comprehensive test orchestration with:
%%% - Parallel test suite execution
%%% - Test dependency management and sequencing
%%% - Resource allocation and cleanup
%%% - Real-time progress tracking
%%% - Timeout and cancellation handling
%%% - Test result aggregation
%%% - Performance metrics collection
%%%
%%% USAGE:
%%%   Orchestrator = erlmcp_test_orchestrator_100k:new(),
%%%   erlmcp_test_orchestrator_100k:add_test_suite(Orchestrator, test_suite_1, Tests),
%%%   erlmcp_test_orchestrator_100k:execute(Orchestrator).
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_test_orchestrator_100k).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% Public API
-export([
    new/0,
    new/1,
    add_test_suite/3,
    add_test_suite/4,
    add_test_dependency/3,
    execute/1,
    execute_parallel/2,
    cancel_test/2,
    get_test_progress/1,
    get_execution_status/1,
    wait_for_completion/2,
    get_orchestration_metrics/1,
    cleanup/1,
    start_link/0,
    start_link/1,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    orchestrator_id :: binary(),
    test_suites = #{} :: map(),
    test_dependencies = #{} :: map(),
    execution_order = [] :: [binary()],
    active_tests = #{} :: map(),
    completed_tests = #{} :: map(),
    failed_tests = #{} :: map(),
    start_time :: integer() | undefined,
    end_time :: integer() | undefined,
    status = idle :: idle | running | completed | failed | cancelled,
    parallel_degree = 8 :: pos_integer(),
    resource_limits = #{
        max_concurrent_tests => 100,
        max_memory_mb => 4096,
        timeout_seconds => 300
    } :: map(),
    orchestration_metrics = #{} :: map()
}).

-record(test_suite, {
    suite_id :: binary(),
    suite_name :: binary(),
    test_function :: fun(),
    test_args = [] :: [term()],
    status = pending :: pending | running | passed | failed | skipped | cancelled,
    result :: term(),
    start_time :: integer() | undefined,
    end_time :: integer() | undefined,
    duration_ms :: integer(),
    metrics :: map(),
    dependencies = [] :: [binary()],
    retries = 0 :: non_neg_integer(),
    max_retries = 3 :: non_neg_integer()
}).

-record(orchestration_metrics, {
    total_suites :: integer(),
    passed_suites :: integer(),
    failed_suites :: integer(),
    skipped_suites :: integer(),
    total_duration_ms :: integer(),
    suite_execution_times = [] :: [integer()],
    parallelism_efficiency :: float(),
    resource_utilization :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new orchestrator
-spec new() -> pid().
new() ->
    {ok, Pid} = start_link(),
    Pid.

%% @doc Create a new orchestrator with ID
-spec new(binary()) -> pid().
new(OrchestratordId) ->
    {ok, Pid} = start_link(OrchestratordId),
    Pid.

%% @doc Add a test suite
-spec add_test_suite(pid(), binary(), fun()) -> ok | {error, term()}.
add_test_suite(OrchestratorPid, SuiteId, TestFunction) ->
    add_test_suite(OrchestratorPid, SuiteId, TestFunction, []).

%% @doc Add a test suite with arguments
-spec add_test_suite(pid(), binary(), fun(), [term()]) -> ok | {error, term()}.
add_test_suite(OrchestratorPid, SuiteId, TestFunction, Args) ->
    gen_server:call(OrchestratorPid, {add_test_suite, SuiteId, TestFunction, Args}).

%% @doc Add test dependency
-spec add_test_dependency(pid(), binary(), binary()) -> ok | {error, term()}.
add_test_dependency(OrchestratorPid, TestId, DependsOnId) ->
    gen_server:call(OrchestratorPid, {add_test_dependency, TestId, DependsOnId}).

%% @doc Execute tests sequentially
-spec execute(pid()) -> {ok, map()} | {error, term()}.
execute(OrchestratorPid) ->
    gen_server:call(OrchestratorPid, {execute, sequential}, 600000).

%% @doc Execute tests in parallel
-spec execute_parallel(pid(), pos_integer()) -> {ok, map()} | {error, term()}.
execute_parallel(OrchestratorPid, Parallelism) ->
    gen_server:call(OrchestratorPid, {execute, {parallel, Parallelism}}, 600000).

%% @doc Cancel a running test
-spec cancel_test(pid(), binary()) -> ok | {error, term()}.
cancel_test(OrchestratorPid, TestId) ->
    gen_server:call(OrchestratorPid, {cancel_test, TestId}).

%% @doc Get test progress
-spec get_test_progress(pid()) -> map() | {error, term()}.
get_test_progress(OrchestratorPid) ->
    gen_server:call(OrchestratorPid, get_test_progress).

%% @doc Get execution status
-spec get_execution_status(pid()) -> map() | {error, term()}.
get_execution_status(OrchestratorPid) ->
    gen_server:call(OrchestratorPid, get_execution_status).

%% @doc Wait for completion with timeout
-spec wait_for_completion(pid(), pos_integer()) -> {ok, map()} | {timeout, map()} | {error, term()}.
wait_for_completion(OrchestratorPid, TimeoutSeconds) ->
    gen_server:call(OrchestratorPid, {wait_for_completion, TimeoutSeconds}, (TimeoutSeconds + 10) * 1000).

%% @doc Get orchestration metrics
-spec get_orchestration_metrics(pid()) -> map() | {error, term()}.
get_orchestration_metrics(OrchestratorPid) ->
    gen_server:call(OrchestratorPid, get_orchestration_metrics).

%% @doc Cleanup resources
-spec cleanup(pid()) -> ok.
cleanup(OrchestratorPid) ->
    gen_server:call(OrchestratorPid, cleanup).

%% @doc Start the orchestrator gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    OrchestrationId = generate_orchestrator_id(),
    gen_server:start_link(?MODULE, OrchestrationId, []).

%% @doc Start with custom ID
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(OrchestratorId) ->
    gen_server:start_link(?MODULE, OrchestratorId, []).

%% @doc Stop the orchestrator
-spec stop(pid()) -> ok.
stop(OrchestratorPid) ->
    gen_server:call(OrchestratorPid, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(OrchestratorId) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{
        orchestrator_id = OrchestratorId,
        test_suites = #{},
        test_dependencies = #{},
        execution_order = [],
        active_tests = #{},
        completed_tests = #{},
        failed_tests = #{}
    }}.

handle_call({add_test_suite, SuiteId, TestFunction, Args}, _From, State) ->
    TestSuite = #test_suite{
        suite_id = SuiteId,
        suite_name = SuiteId,
        test_function = TestFunction,
        test_args = Args,
        status = pending,
        start_time = undefined,
        end_time = undefined,
        duration_ms = 0,
        metrics = #{}
    },
    NewSuites = maps:put(SuiteId, TestSuite, State#state.test_suites),
    NewState = State#state{test_suites = NewSuites},
    {reply, ok, NewState};

handle_call({add_test_dependency, TestId, DependsOnId}, _From, State) ->
    Dependencies = maps:get(TestId, State#state.test_dependencies, []),
    NewDependencies = maps:put(TestId, [DependsOnId | Dependencies], State#state.test_dependencies),
    NewState = State#state{test_dependencies = NewDependencies},
    {reply, ok, NewState};

handle_call({execute, ExecutionMode}, _From, State) ->
    ExecutionStartTime = erlang:system_time(millisecond),
    try
        case ExecutionMode of
            sequential ->
                {Results, NewState} = execute_sequential(State#state{start_time = ExecutionStartTime}),
                {reply, {ok, Results}, NewState#state{status = completed, end_time = erlang:system_time(millisecond)}};
            {parallel, Parallelism} ->
                {Results, NewState} = execute_parallel_internal(
                    State#state{
                        start_time = ExecutionStartTime,
                        parallel_degree = Parallelism
                    }
                ),
                {reply, {ok, Results}, NewState#state{status = completed, end_time = erlang:system_time(millisecond)}}
        end
    catch
        _:Error ->
            logger:error("Execution failed: ~p", [Error]),
            {reply, {error, Error}, State#state{status = failed}}
    end;

handle_call({cancel_test, TestId}, _From, State) ->
    case maps:find(TestId, State#state.active_tests) of
        {ok, Pid} ->
            catch exit(Pid, kill),
            NewActiveTests = maps:remove(TestId, State#state.active_tests),
            NewFailedTests = maps:put(TestId, {cancelled, erlang:system_time(millisecond)}, State#state.failed_tests),
            NewState = State#state{
                active_tests = NewActiveTests,
                failed_tests = NewFailedTests
            },
            {reply, ok, NewState};
        error ->
            {reply, {error, test_not_found}, State}
    end;

handle_call(get_test_progress, _From, State) ->
    Progress = #{
        total_tests => maps:size(State#state.test_suites),
        completed_tests => maps:size(State#state.completed_tests),
        active_tests => maps:size(State#state.active_tests),
        failed_tests => maps:size(State#state.failed_tests),
        active_test_ids => maps:keys(State#state.active_tests)
    },
    {reply, Progress, State};

handle_call(get_execution_status, _From, State) ->
    Status = #{
        orchestrator_id => State#state.orchestrator_id,
        status => State#state.status,
        start_time => State#state.start_time,
        end_time => State#state.end_time,
        duration_ms => case {State#state.start_time, State#state.end_time} of
            {undefined, _} -> undefined;
            {_, undefined} -> erlang:system_time(millisecond) - State#state.start_time;
            {Start, End} -> End - Start
        end,
        total_tests => maps:size(State#state.test_suites),
        passed => maps:size(State#state.completed_tests),
        failed => maps:size(State#state.failed_tests),
        active => maps:size(State#state.active_tests)
    },
    {reply, Status, State};

handle_call({wait_for_completion, TimeoutSeconds}, _From, State) ->
    Deadline = erlang:system_time(millisecond) + (TimeoutSeconds * 1000),
    Status = wait_until_complete(State, Deadline),
    {reply, {ok, Status}, State};

handle_call(get_orchestration_metrics, _From, State) ->
    Metrics = extract_orchestration_metrics(State),
    {reply, Metrics, State};

handle_call(cleanup, _From, State) ->
    % Kill all active test processes
    maps:foreach(fun(_TestId, Pid) ->
        catch exit(Pid, kill)
    end, State#state.active_tests),

    NewState = State#state{
        active_tests = #{},
        status = idle
    },
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Execute tests sequentially
execute_sequential(State) ->
    ExecutionOrder = determine_execution_order(State),
    {Results, NewState} = lists:foldl(
        fun(TestId, {Acc, StateAcc}) ->
            {Result, UpdatedState} = execute_single_test(TestId, StateAcc),
            {Acc ++ [Result], UpdatedState}
        end,
        {[], State},
        ExecutionOrder
    ),
    {maps:from_list([{test, Results}]), NewState}.

%% Execute tests in parallel
execute_parallel_internal(State) ->
    ExecutionOrder = determine_execution_order(State),
    MaxConcurrent = maps:get(max_concurrent_tests, State#state.resource_limits, 100),

    % Split into batches for concurrent execution
    Batches = split_into_batches(ExecutionOrder, MaxConcurrent),

    {Results, NewState} = lists:foldl(
        fun(Batch, {Acc, StateAcc}) ->
            {BatchResults, UpdatedState} = execute_batch_parallel(Batch, StateAcc),
            {Acc ++ BatchResults, UpdatedState}
        end,
        {[], State},
        Batches
    ),

    {maps:from_list([{test, Results}]), NewState}.

%% Determine execution order based on dependencies
determine_execution_order(State) ->
    % Topological sort based on dependencies
    TestIds = maps:keys(State#state.test_suites),
    Dependencies = State#state.test_dependencies,

    topological_sort(TestIds, Dependencies).

%% Topological sort
topological_sort(Nodes, Dependencies) ->
    topological_sort_internal(Nodes, Dependencies, []).

topological_sort_internal([], _Dependencies, Result) ->
    Result;
topological_sort_internal(Nodes, Dependencies, Result) ->
    % Find nodes with no dependencies
    Ready = [N || N <- Nodes, not has_unmet_dependencies(N, Dependencies, Result)],

    case Ready of
        [] ->
            % Circular dependency or no ready nodes
            Result ++ Nodes;
        _ ->
            % Process ready nodes
            Remaining = Nodes -- Ready,
            topological_sort_internal(Remaining, Dependencies, Result ++ Ready)
    end.

%% Check if node has unmet dependencies
has_unmet_dependencies(Node, Dependencies, Completed) ->
    case maps:find(Node, Dependencies) of
        {ok, Deps} ->
            lists:any(fun(Dep) -> not lists:member(Dep, Completed) end, Deps);
        error ->
            false
    end.

%% Execute a single test
execute_single_test(TestId, State) ->
    case maps:find(TestId, State#state.test_suites) of
        {ok, TestSuite} ->
            StartTime = erlang:system_time(millisecond),
            try
                Result = apply(TestSuite#test_suite.test_function, TestSuite#test_suite.test_args),
                EndTime = erlang:system_time(millisecond),

                UpdatedTestSuite = TestSuite#test_suite{
                    status = passed,
                    result = Result,
                    start_time = StartTime,
                    end_time = EndTime,
                    duration_ms = EndTime - StartTime
                },

                NewCompletedTests = maps:put(TestId, UpdatedTestSuite, State#state.completed_tests),
                NewSuites = maps:put(TestId, UpdatedTestSuite, State#state.test_suites),

                {
                    #{test_id => TestId, status => passed, duration_ms => EndTime - StartTime},
                    State#state{
                        test_suites = NewSuites,
                        completed_tests = NewCompletedTests
                    }
                }
            catch
                _:Error ->
                    EndTime = erlang:system_time(millisecond),
                    logger:error("Test ~w failed: ~p", [TestId, Error]),

                    UpdatedTestSuite = TestSuite#test_suite{
                        status = failed,
                        result = Error,
                        start_time = StartTime,
                        end_time = EndTime,
                        duration_ms = EndTime - StartTime
                    },

                    NewFailedTests = maps:put(TestId, UpdatedTestSuite, State#state.failed_tests),
                    NewSuites = maps:put(TestId, UpdatedTestSuite, State#state.test_suites),

                    {
                        #{test_id => TestId, status => failed, duration_ms => EndTime - StartTime, error => Error},
                        State#state{
                            test_suites = NewSuites,
                            failed_tests = NewFailedTests
                        }
                    }
            end;
        error ->
            {
                #{test_id => TestId, status => not_found},
                State
            }
    end.

%% Execute batch of tests in parallel
execute_batch_parallel(Batch, State) ->
    % Spawn processes for each test
    Pids = lists:map(fun(TestId) ->
        Pid = spawn(fun() ->
            {Result, _} = execute_single_test(TestId, State),
            exit({test_result, TestId, Result})
        end),
        {TestId, Pid}
    end, Batch),

    % Wait for all to complete
    Results = lists:map(fun({TestId, Pid}) ->
        receive
            {'EXIT', Pid, {test_result, TestId, Result}} ->
                Result
        after 300000 ->
            logger:warning("Test ~w timed out", [TestId]),
            #{test_id => TestId, status => timeout}
        end
    end, Pids),

    {Results, State}.

%% Split list into batches
split_into_batches(List, BatchSize) ->
    split_into_batches_internal(List, BatchSize, []).

split_into_batches_internal([], _BatchSize, Result) ->
    lists:reverse(Result);
split_into_batches_internal(List, BatchSize, Result) ->
    {Batch, Rest} = lists:split(min(BatchSize, length(List)), List),
    split_into_batches_internal(Rest, BatchSize, [Batch | Result]).

%% Wait until completion
wait_until_complete(State, Deadline) ->
    case State#state.status of
        completed ->
            #{status => completed};
        failed ->
            #{status => failed};
        _ ->
            case erlang:system_time(millisecond) > Deadline of
                true ->
                    #{status => timeout};
                false ->
                    timer:sleep(100),
                    wait_until_complete(State, Deadline)
            end
    end.

%% Extract orchestration metrics
extract_orchestration_metrics(State) ->
    ExecutionTimes = [
        maps:get(duration_ms, maps:get(suite_id, State#state.test_suites, #{}), 0)
        || _ <- maps:keys(State#state.completed_tests)
    ],

    TotalDuration = case {State#state.start_time, State#state.end_time} of
        {undefined, _} -> 0;
        {_, undefined} -> erlang:system_time(millisecond) - State#state.start_time;
        {Start, End} -> End - Start
    end,

    AvgExecutionTime = case ExecutionTimes of
        [] -> 0;
        _ -> lists:sum(ExecutionTimes) / length(ExecutionTimes)
    end,

    IdealParallelTime = case ExecutionTimes of
        [] -> 0;
        _ -> lists:max(ExecutionTimes)
    end,

    EffectiveParallelism = case IdealParallelTime of
        0 -> 0.0;
        _ -> AvgExecutionTime * length(ExecutionTimes) / TotalDuration
    end,

    #{
        total_tests => maps:size(State#state.test_suites),
        passed_tests => maps:size(State#state.completed_tests),
        failed_tests => maps:size(State#state.failed_tests),
        total_duration_ms => TotalDuration,
        avg_test_duration_ms => round(AvgExecutionTime),
        parallelism_efficiency => round(EffectiveParallelism * 100) / 100,
        execution_times => ExecutionTimes
    }.

%% Generate orchestrator ID
generate_orchestrator_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("orchestrator_~w_~w", [Timestamp, Random])).
