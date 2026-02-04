%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow Engine Performance Benchmark
%%%
%%% Benchmarks DAG execution performance with 1-50 parallel steps.
%%% Tests workflow definition, execution, and parallel step handling.
%%%
%%% Benchmark Scenarios:
%%% - Simple sequential workflow execution
%%% - Parallel workflow with 1-50 parallel steps
%%% - Complex DAG with multiple dependencies
%%% - Workflow cancellation performance
%%% - Large workflow definition parsing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_engine_bench).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Export all functions for testing
-export([all/0, setup/0, cleanup/0,
         sequential_workflow/1,
         parallel_workflow_scaling/1,
         complex_dag_execution/1,
         workflow_cancellation/1,
         large_workflow_parsing/1]).

%%====================================================================
%% Constants
%%====================================================================
-define(DEFAULT_WORKFLOW_COUNT, 100).
-define(MAX_PARALLEL_STEPS, 50).
-define(DEFAULT_STEP_TIMEOUT, 30).
-define(DEFAULT_EXECUTION_COUNT, 100).

%%====================================================================
%% Test Configuration
%%====================================================================
all() ->
    [
        sequential_workflow,
        parallel_workflow_scaling,
        complex_dag_execution,
        workflow_cancellation,
        large_workflow_parsing
    ].

setup() ->
    %% Start workflow engine
    case erlmcp_workflow_engine:start_link() of
        {ok, _Pid} ->
            %% Define test workflows
            define_test_workflows(),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start workflow engine: ~p", [Reason]),
            error(start_failed)
    end.

cleanup() ->
    %% Stop workflow engine
    erlmcp_workflow_engine:stop(),
    ok.

%%====================================================================
%% Benchmark Tests
%%====================================================================

%% @doc Simple sequential workflow execution
sequential_workflow(_Config) ->
    WorkflowId = <<"sequential_workflow">>,
    InputData = #{test_input => "sequential_test"},

    %% Warmup
    [begin
         _ = erlmcp_workflow_engine:execute_workflow(WorkflowId, InputData)
     end || _ <- lists:seq(1, 10)],

    %% Measurement phase
    Results = [begin
                 StartTime = erlang:monotonic_time(millisecond),
                 Result = erlmcp_workflow_engine:execute_workflow(WorkflowId, InputData),
                 EndTime = erlang:monotonic_time(millisecond),

                 case Result of
                     {ok, ExecutionId} ->
                         %% Wait for completion
                         case wait_for_completion(ExecutionId, 5000) of
                             {ok, _Execution} -> EndTime - StartTime;
                             {error, _} -> -1
                         end;
                     {error, _} -> -1
                 end
             end || _ <- lists:seq(1, ?DEFAULT_EXECUTION_COUNT)],

    %% Analyze results
    Successful = [T || T <- Results, T > 0],
    Failed = Results -- Successful,

    #{
        test => sequential_workflow,
        total_executions => ?DEFAULT_EXECUTION_COUNT,
        successful => length(Successful),
        failed => length(Failed),
        success_rate => length(Successful) / ?DEFAULT_EXECUTION_COUNT,
        average_latency => lists:sum(Successful) / length(Successful),
        min_latency => lists:min(Successful),
        max_latency => lists:max(Successful),
        p95_latency => calculate_percentile(Successful, 95),
        p99_latency => calculate_percentile(Successful, 99),
        throughput => length(Successful) / (lists:sum(Successful) / 1000)
    }.

%% @doc Parallel workflow scaling (1-50 parallel steps)
parallel_workflow_scaling(_Config) ->
    ParallelStepCounts = [1, 5, 10, 20, 30, 40, 50],

    Results = lists:map(fun(StepCount) ->
        %% Create workflow with specified number of parallel steps
        Workflow = create_parallel_workflow(StepCount),
        WorkflowId = list_to_binary("parallel_workflow_" ++ integer_to_list(StepCount)),

        erlmcp_workflow_engine:define_workflow(Workflow),

        InputData = #{test_input => "parallel_test", step_count => StepCount},

        %% Execute workflow multiple times
        Results = [begin
                     StartTime = erlang:monotonic_time(millisecond),
                     Result = erlmcp_workflow_engine:execute_workflow(WorkflowId, InputData),
                     EndTime = erlang:monotonic_time(millisecond),

                     case Result of
                         {ok, ExecutionId} ->
                             case wait_for_completion(ExecutionId, 10000) of
                                 {ok, _Execution} -> EndTime - StartTime;
                                 {error, _} -> -1
                             end;
                         {error, _} -> -1
                     end
                 end || _ <- lists:seq(1, div(?DEFAULT_EXECUTION_COUNT, max(1, StepCount div 10)))],

        Successful = [T || T <- Results, T > 0],

        #{
            parallel_steps => StepCount,
            total_executions => length(Results),
            successful => length(Successful),
            success_rate => length(Successful) / length(Results),
            average_latency => case Successful of
                                 [] -> 0;
                                 _ -> lists:sum(Successful) / length(Successful)
                             end,
            throughput => case length(Successful) > 0 of
                            true -> length(Successful) / (lists:sum(Successful) / 1000);
                            false -> 0
                         end,
            scaling_efficiency => calculate_scaling_efficiency(Successful, StepCount)
        }
    end, ParallelStepCounts),

    #{
        test => parallel_workflow_scaling,
        results => Results
    }.

%% @doc Complex DAG execution
complex_dag_execution(_Config) ->
    WorkflowId = <<"complex_dag_workflow">>,

    %% Define complex DAG workflow
    ComplexWorkflow = create_complex_dag_workflow(),
    erlmcp_workflow_engine:define_workflow(ComplexWorkflow),

    InputData = #{complex_input => "dag_test", data_size => 1000},

    %% Execute workflow multiple times
    Results = [begin
                 StartTime = erlang:monotonic_time(millisecond),
                 Result = erlmcp_workflow_engine:execute_workflow(WorkflowId, InputData),
                 EndTime = erlang:monotonic_time(millisecond),

                 case Result of
                     {ok, ExecutionId} ->
                         case wait_for_completion(ExecutionId, 15000) of
                             {ok, Execution} ->
                                 EndTime - StartTime;
                                 {error, _} -> -1
                         end;
                     {error, _} -> -1
                 end
             end || _ <- lists:seq(1, ?DEFAULT_WORKFLOW_COUNT)],

    Successful = [T || T <- Results, T > 0],

    #{
        test => complex_dag_execution,
        total_executions => ?DEFAULT_WORKFLOW_COUNT,
        successful => length(Successful),
        failed => ?DEFAULT_WORKFLOW_COUNT - length(Successful),
        success_rate => length(Successful) / ?DEFAULT_WORKFLOW_COUNT,
        average_latency => lists:sum(Successful) / length(Successful),
        min_latency => lists:min(Successful),
        max_latency => lists:max(Successful),
        p95_latency => calculate_percentile(Successful, 95),
        p99_latency => calculate_percentile(Successful, 99),
        throughput => length(Successful) / (lists:sum(Successful) / 1000)
    }.

%% @doc Workflow cancellation performance
workflow_cancellation(_Config) ->
    WorkflowId = <<"cancellation_test_workflow">>,

    %% Create long-running workflow
    CancellationWorkflow = create_long_running_workflow(),
    erlmcp_workflow_engine:define_workflow(CancellationWorkflow),

    InputData = #{duration => 60}, % 60 second delay steps

    %% Start multiple workflows
    Executions = [begin
                     {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(WorkflowId, InputData),
                     {ExecutionId, self()}
                 end || _ <- lists:seq(1, 20)],

    %% Cancel half of them after varying delays
    Cancellations = lists:map(fun({ExecutionId, Pid}) ->
                                timer:sleep(rand:uniform(1000)),
                                Result = erlmcp_workflow_engine:cancel_execution(ExecutionId),
                                {ExecutionId, Result, Pid}
                            end, lists:sublist(Executions, 10)),

    %% Check cancellation results
    SuccessfulCancellations = [R || {_, {ok, _}, _} <- Cancellations],
    FailedCancellations = [R || {_, {error, _}, _} <- Cancellations],

    %% Measure cancellation latency
    CancellationLatencies = [begin
                               CancelStartTime = erlang:monotonic_time(millisecond),
                               _ = erlmcp_workflow_engine:cancel_execution(ExecutionId),
                               CancelEndTime = erlang:monotonic_time(millisecond),
                               CancelEndTime - CancelStartTime
                           end || {ExecutionId, _, _} <- SuccessfulCancellations],

    #{
        test => workflow_cancellation,
        total_executions => length(Executions),
        started_workflows => length(Executions),
        cancellation_requests => length(Cancellations),
        successful_cancellations => length(SuccessfulCancellations),
        failed_cancellations => length(FailedCancellations),
        success_rate => length(SuccessfulCancellations) / length(Cancellations),
        average_cancellation_latency => case CancellationLatencies of
                                          [] -> 0;
                                          _ -> lists:sum(CancellationLatencies) / length(CancellationLatencies)
                                      end,
        min_cancellation_latency => case CancellationLatencies of
                                      [] -> 0;
                                      _ -> lists:min(CancellationLatencies)
                                  end,
        max_cancellation_latency => case CancellationLatencies of
                                      [] -> 0;
                                      _ -> lists:max(CancellationLatencies)
                                  end
    }.

%% @doc Large workflow definition parsing
large_workflow_parsing(_Config) ->
    WorkflowSizes = [10, 50, 100, 200, 500],

    Results = lists:map(fun(Size) ->
        %% Create large workflow definition
        LargeWorkflow = create_large_workflow(Size),

        %% Measure parsing time
        StartTime = erlang:monotonic_time(millisecond),
        Result = erlmcp_workflow_engine:define_workflow(LargeWorkflow),
        EndTime = erlang:monotonic_time(millisecond),

        ParseTime = EndTime - StartTime,

        case Result of
            ok ->
                %% Measure parsing complexity
                StepsCount = length(maps:get(steps, LargeWorkflow)),
                TransitionsCount = length(maps:get(transitions, LargeWorkflow)),

                #{
                    workflow_size => Size,
                    parse_time => ParseTime,
                    steps_count => StepsCount,
                    transitions_count => TransitionsCount,
                    parse_efficiency => StepsCount / ParseTime,
                    complexity_ratio => StepsCount / TransitionsCount
                };
            {error, Reason} ->
                #{
                    workflow_size => Size,
                    parse_error => Reason,
                    parse_time => ParseTime
                }
        end
    end, WorkflowSizes),

    #{
        test => large_workflow_parsing,
        results => Results
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

define_test_workflows() ->
    %% Define simple sequential workflow
    SequentialWorkflow = #{
        id => <<"sequential_workflow">>,
        steps => [
            #{
                id => <<"step1">>,
                type => tool,
                tool_name => <<"sequential_tool1">>,
                tool_arguments => #{operation => "process"},
                max_retries => 2,
                timeout_sec => 5
            },
            #{
                id => <<"step2">>,
                type => tool,
                tool_name => <<"sequential_tool2">>,
                tool_arguments => #{operation => "transform"},
                max_retries => 2,
                timeout_sec => 5
            },
            #{
                id => <<"step3">>,
                type => tool,
                tool_name => <<"sequential_tool3">>,
                tool_arguments => #{operation => "finalize"},
                max_retries => 2,
                timeout_sec => 5
            }
        ],
        transitions => [
            #{from => <<"step1">>, to => <<"step2">>, condition => success},
            #{from => <<"step2">>, to => <<"step3">>, condition => success}
        ]
    },

    %% Define complex parallel workflow
    ParallelWorkflow = #{
        id => <<"parallel_workflow_base">>,
        steps => [
            #{
                id => <<"step1">>,
                type => tool,
                tool_name => <<"parallel_tool1">>,
                tool_arguments => #{operation => "start"},
                max_retries => 1,
                timeout_sec => 5
            },
            #{
                id => <<"parallel_branch">>,
                type => parallel,
                child_steps => [
                    #{
                        id => <<"parallel_a">>,
                        type => tool,
                        tool_name => <<"parallel_tool_a">>,
                        tool_arguments => #{operation => "process_a"},
                        max_retries => 1,
                        timeout_sec => 10
                    },
                    #{
                        id => <<"parallel_b">>,
                        type => tool,
                        tool_name => <<"parallel_tool_b">>,
                        tool_arguments => #{operation => "process_b"},
                        max_retries => 1,
                        timeout_sec => 10
                    },
                    #{
                        id => <<"parallel_c">>,
                        type => tool,
                        tool_name => <<"parallel_tool_c">>,
                        tool_arguments => #{operation => "process_c"},
                        max_retries => 1,
                        timeout_sec => 10
                    }
                ]
            },
            #{
                id => <<"step2">>,
                type => tool,
                tool_name => <<"parallel_tool2">>,
                tool_arguments => #{operation => "aggregate"},
                max_retries => 1,
                timeout_sec => 5
            }
        ],
        transitions => [
            #{from => <<"step1">>, to => <<"parallel_branch">>, condition => success},
            #{from => <<"parallel_branch">>, to => <<"step2">>, condition => success}
        ]
    },

    erlmcp_workflow_engine:define_workflow(SequentialWorkflow),
    erlmcp_workflow_engine:define_workflow(ParallelWorkflow).

create_parallel_workflow(StepCount) ->
    Steps = lists:map(fun(I) ->
                           StepId = list_to_binary("parallel_step_" ++ integer_to_list(I)),
                           #{
                               id => StepId,
                               type => tool,
                               tool_name => list_to_binary("parallel_tool_" ++ integer_to_list(I)),
                               tool_arguments => #{operation => "process", id => I},
                               max_retries => 1,
                               timeout_sec => 5
                           }
                       end, lists:seq(1, StepCount)),

    Transitions = [#{from => list_to_binary("parallel_step_" ++ integer_to_list(I)),
                     to => <<"finalize">>,
                     condition => success}
                  || I <- lists:seq(1, StepCount)],

    #{
        id => list_to_binary("generated_parallel_" ++ integer_to_list(StepCount)),
        steps => Steps ++ [
            #{
                id => <<"finalize">>,
                type => tool,
                tool_name => <<"finalize_tool">>,
                tool_arguments => #{operation => "complete"},
                max_retries => 1,
                timeout_sec => 5
            }
        ],
        transitions => Transitions
    }.

create_complex_dag_workflow() ->
    %% Create a complex DAG with multiple dependencies
    BaseSteps = [
        #{
            id => <<"start">>,
            type => tool,
            tool_name => <<"initialize">>,
            tool_arguments => #{operation => "init"},
            max_retries => 1,
            timeout_sec => 5
        },
        #{
            id => <<"preprocess1">>,
            type => tool,
            tool_name => <<"preprocess1">>,
            tool_arguments => #{operation => "preprocess1"},
            max_retries => 2,
            timeout_sec => 10
        },
        #{
            id => <<"preprocess2">>,
            type => tool,
            tool_name => <<"preprocess2">>,
            tool_arguments => #{operation => "preprocess2"},
            max_retries => 2,
            timeout_sec => 10
        }
    ],

    ParallelSteps = lists:map(fun(I) ->
                                  StepId = list_to_binary("parallel_" ++ integer_to_list(I)),
                                  #{
                                      id => StepId,
                                      type => tool,
                                      tool_name => list_to_binary("process_" ++ integer_to_list(I)),
                                      tool_arguments => #{operation => "process", id => I},
                                      max_retries => 3,
                                      timeout_sec => 15
                                  }
                              end, lists:seq(1, 10)),

    AggregationSteps = [
        #{
            id => <<"aggregate1">>,
            type => tool,
            tool_name => <<"aggregate1">>,
            tool_arguments => #{operation => "agg1"},
            max_retries => 2,
            timeout_sec => 10
        },
        #{
            id => <<"aggregate2">>,
            type => tool,
            tool_name => <<"aggregate2">>,
            tool_arguments => #{operation => "agg2"},
            max_retries => 2,
            timeout_sec => 10
        }
    ],

    FinalSteps = [
        #{
            id => <<"finalize">>,
            type => tool,
            tool_name => <<"finalize">>,
            tool_arguments => #{operation => "final"},
            max_retries => 1,
            timeout_sec => 5
        }
    ],

    AllSteps = BaseSteps ++ ParallelSteps ++ AggregationSteps ++ FinalSteps,

    %% Create complex transitions
    BaseTransitions = [
        #{from => <<"start">>, to => <<"preprocess1">>, condition => success},
        #{from => <<"start">>, to => <<"preprocess2">>, condition => success}
    ],

    ParallelTransitions = [#{from => <<"preprocess1">>, to => P, condition => success} || P <-
                             [list_to_binary("parallel_" ++ integer_to_list(I)) || I <- lists:seq(1, 5)]],
    ParallelTransitions2 = [#{from => <<"preprocess2">>, to => P, condition => success} || P <-
                              [list_to_binary("parallel_" ++ integer_to_list(I)) || I <- lists:seq(6, 10)]],

    AggregationTransitions = [#{from => P, to => <<"aggregate1">>, condition => success} || P <-
                               [list_to_binary("parallel_" ++ integer_to_list(I)) || I <- [1, 3, 5, 7, 9]]],
    AggregationTransitions2 = [#{from => P, to => <<"aggregate2">>, condition => success} || P <-
                                [list_to_binary("parallel_" ++ integer_to_list(I)) || I <- [2, 4, 6, 8, 10]]],

    FinalTransitions = [
        #{from => <<"aggregate1">>, to => <<"finalize">>, condition => success},
        #{from => <<"aggregate2">>, to => <<"finalize">>, condition => success}
    ],

    AllTransitions = BaseTransitions ++ ParallelTransitions ++ ParallelTransitions2 ++
                     AggregationTransitions ++ AggregationTransitions2 ++ FinalTransitions,

    #{
        id => <<"complex_dag_workflow">>,
        steps => AllSteps,
        transitions => AllTransitions
    }.

create_long_running_workflow() ->
    #{
        id => <<"long_running_workflow">>,
        steps => [
            #{
                id => <<"delay_step">>,
                type => delay,
                timeout_sec => 60
            },
            #{
                id => <<"processing_step">>,
                type => tool,
                tool_name => <<"long_process">>,
                tool_arguments => #{duration => 60},
                timeout_sec => 120
            }
        ],
        transitions => [
            #{from => <<"delay_step">>, to => <<"processing_step">>, condition => success}
        ]
    }.

create_large_workflow(Size) ->
    %% Create workflow with specified number of steps
    Steps = lists:map(fun(I) ->
                           StepId = list_to_binary("step_" ++ integer_to_list(I)),
                           #{
                               id => StepId,
                               type => tool,
                               tool_name => list_to_binary("tool_" ++ integer_to_list(I)),
                               tool_arguments => #{id => I, data => lists:seq(1, 100)},
                               max_retries => 1,
                               timeout_sec => 5
                           }
                       end, lists:seq(1, Size)),

    %% Create some transitions (not all steps connected)
    Transitions = [#{from => list_to_binary("step_" ++ integer_to_list(I)),
                     to => list_to_binary("step_" ++ integer_to_list(I + 1)),
                     condition => success}
                  || I <- lists:seq(1, Size - 1)],

    #{
        id => list_to_binary("large_workflow_" ++ integer_to_list(Size)),
        steps => Steps,
        transitions => Transitions
    }.

wait_for_completion(ExecutionId, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),

    wait_for_completion_loop(ExecutionId, StartTime, Timeout).

wait_for_completion_loop(ExecutionId, StartTime, Timeout) ->
    case erlang:monotonic_time(millisecond) - StartTime > Timeout of
        true ->
            {error, timeout};
        false ->
            case erlmcp_workflow_engine:get_execution_status(ExecutionId) of
                {ok, #{status := Status}} when Status =:= completed; Status =:= failed ->
                    {ok, Status};
                {ok, _} ->
                    timer:sleep(100),
                    wait_for_completion_loop(ExecutionId, StartTime, Timeout);
                {error, not_found} ->
                    {error, not_found}
            end
    end.

calculate_percentile(List, Percentile) when length(List) > 0 ->
    Sorted = lists:sort(List),
    Index = trunc((Percentile / 100) * length(Sorted)),
    lists:nth(min(Index + 1, length(Sorted)), Sorted).

calculate_scaling_efficiency(Latencies, StepCount) ->
    case Latencies of
        [] -> 0;
        _ ->
            AvgLatency = lists:sum(Latencies) / length(Latencies),
            IdealLatency = 1000, % Ideal latency in ms
            ScalingFactor = math:log(StepCount + 1),
            Efficiency = IdealLatency / (AvgLatency * ScalingFactor),
            min(1.0, Efficiency)
    end.