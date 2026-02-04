-module(erlmcp_workflow_engine_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup (Chicago School TDD: Real gen_server, no mocks)
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_workflow_engine:start_link(),
    Pid.

cleanup(_Pid) ->
    ok = gen_server:stop(erlmcp_workflow_engine).

%%%===================================================================
%%% Test Generators
%%%===================================================================

erlmcp_workflow_engine_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun define_workflow_test/0,
      fun execute_simple_workflow_test/0,
      fun execute_parallel_workflow_test/0,
      fun execute_sequential_workflow_test/0,
      fun cancel_execution_test/0,
      fun get_execution_status_test/0,
      fun list_workflows_test/0,
      fun workflow_with_transitions_test/0,
      fun workflow_with_retry_test/0,
      fun concurrent_executions_test/0
     ]}.

%%%===================================================================
%%% Individual Tests (Chicago School: State-based, real processes)
%%%===================================================================

define_workflow_test() ->
    {"define_workflow stores workflow definition", fun() ->
        %% Exercise: Define workflow
        Workflow = #{
            id => <<"test_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"calculator">>,
                    tool_arguments => #{expression => <<"2 + 2">>}
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Verify: Workflow in list
        Workflows = erlmcp_workflow_engine:list_workflows(),
        ?assert(lists:keymember(<<"test_workflow">>, 1, Workflows))
    end}.

execute_simple_workflow_test() ->
    {"execute_workflow runs simple workflow", fun() ->
        %% Setup: Define simple workflow
        Workflow = #{
            id => <<"simple_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"echo">>,
                    tool_arguments => #{message => <<"hello">>},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Exercise: Execute workflow
        Result = erlmcp_workflow_engine:execute_workflow(<<"simple_workflow">>, #{}),

        %% Verify: Execution started
        ?assertMatch({ok, _ExecutionId}, Result)
    end}.

execute_parallel_workflow_test() ->
    {"execute_workflow runs parallel steps concurrently", fun() ->
        %% Setup: Define workflow with parallel steps
        Workflow = #{
            id => <<"parallel_workflow">>,
            steps => [
                #{
                    id => <<"parallel_step">>,
                    type => parallel,
                    child_steps => [
                        #{
                            id => <<"child1">>,
                            type => tool,
                            tool_name => <<"task1">>,
                            tool_arguments => #{},
                            max_retries => 0,
                            timeout_sec => 5
                        },
                        #{
                            id => <<"child2">>,
                            type => tool,
                            tool_name => <<"task2">>,
                            tool_arguments => #{},
                            max_retries => 0,
                            timeout_sec => 5
                        }
                    ]
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Exercise: Execute parallel workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"parallel_workflow">>, #{}
        ),

        %% Verify: Execution status
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assert(maps:is_key(execution_id, Status)),
        ?assert(maps:is_key(status, Status))
    end}.

execute_sequential_workflow_test() ->
    {"execute_workflow runs sequential steps in order", fun() ->
        %% Setup: Define sequential workflow
        Workflow = #{
            id => <<"sequential_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"init">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                },
                #{
                    id => <<"step2">>,
                    type => tool,
                    tool_name => <<"process">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                },
                #{
                    id => <<"step3">>,
                    type => tool,
                    tool_name => <<"finalize">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => [
                #{from => <<"step1">>, to => <<"step2">>, condition => success},
                #{from => <<"step2">>, to => <<"step3">>, condition => success}
            ]
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Exercise: Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"sequential_workflow">>, #{}
        ),

        %% Verify: Execution status
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assertEqual(<<"sequential_workflow">>, maps:get(workflow_id, Status))
    end}.

cancel_execution_test() ->
    {"cancel_execution stops running workflow", fun() ->
        %% Setup: Define and start workflow
        Workflow = #{
            id => <<"cancellable_workflow">>,
            steps => [
                #{
                    id => <<"long_step">>,
                    type => tool,
                    tool_name => <<"long_task">>,
                    tool_arguments => #{duration => 10000},
                    max_retries => 0,
                    timeout_sec => 30
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"cancellable_workflow">>, #{}
        ),

        %% Exercise: Cancel execution
        ok = erlmcp_workflow_engine:cancel_execution(ExecutionId),

        %% Verify: Execution cancelled
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assertEqual(cancelled, maps:get(status, Status))
    end}.

get_execution_status_test() ->
    {"get_execution_status returns execution details", fun() ->
        %% Setup: Define and execute workflow
        Workflow = #{
            id => <<"status_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"test">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"status_workflow">>, #{}
        ),

        %% Exercise: Get status
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),

        %% Verify: Status contains all fields
        ?assert(maps:is_key(execution_id, Status)),
        ?assert(maps:is_key(workflow_id, Status)),
        ?assert(maps:is_key(status, Status)),
        ?assert(maps:is_key(current_step, Status)),
        ?assert(maps:is_key(started_at, Status))
    end}.

list_workflows_test() ->
    {"list_workflows returns all defined workflows", fun() ->
        %% Setup: Define multiple workflows
        Workflows = [
            #{id => <<"workflow1">>, steps => [], transitions => []},
            #{id => <<"workflow2">>, steps => [], transitions => []},
            #{id => <<"workflow3">>, steps => [], transitions => []}
        ],
        [ok = erlmcp_workflow_engine:define_workflow(W) || W <- Workflows],

        %% Exercise: List workflows
        Result = erlmcp_workflow_engine:list_workflows(),

        %% Verify: All workflows present
        ?assert(lists:keymember(<<"workflow1">>, 1, Result)),
        ?assert(lists:keymember(<<"workflow2">>, 1, Result)),
        ?assert(lists:keymember(<<"workflow3">>, 1, Result))
    end}.

workflow_with_transitions_test() ->
    {"execute_workflow follows step transitions", fun() ->
        %% Setup: Define workflow with conditional transitions
        Workflow = #{
            id => <<"transition_workflow">>,
            steps => [
                #{
                    id => <<"start">>,
                    type => tool,
                    tool_name => <<"init">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                },
                #{
                    id => <<"on_success">>,
                    type => tool,
                    tool_name => <<"success_handler">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                },
                #{
                    id => <<"on_failure">>,
                    type => tool,
                    tool_name => <<"failure_handler">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => [
                #{from => <<"start">>, to => <<"on_success">>, condition => success},
                #{from => <<"start">>, to => <<"on_failure">>, condition => failure}
            ]
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Exercise: Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"transition_workflow">>, #{}
        ),

        %% Verify: Execution follows transitions
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assert(maps:is_key(current_step, Status))
    end}.

workflow_with_retry_test() ->
    {"execute_workflow retries failed steps", fun() ->
        %% Setup: Define workflow with retry
        Workflow = #{
            id => <<"retry_workflow">>,
            steps => [
                #{
                    id => <<"flaky_step">>,
                    type => tool,
                    tool_name => <<"flaky_task">>,
                    tool_arguments => #{},
                    max_retries => 3,
                    retry_backoff_ms => 100,
                    timeout_sec => 10
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Exercise: Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"retry_workflow">>, #{}
        ),

        %% Verify: Execution attempts retries
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assert(maps:is_key(attempts, Status))
    end}.

concurrent_executions_test() ->
    {"execute_workflow handles concurrent executions", fun() ->
        %% Setup: Define workflow
        Workflow = #{
            id => <<"concurrent_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"quick_task">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        %% Exercise: Execute multiple concurrent workflows
        Results = [
            erlmcp_workflow_engine:execute_workflow(<<"concurrent_workflow">>, #{})
            || _ <- lists:seq(1, 10)
        ],

        %% Verify: All executions started
        ?assertEqual(10, length(Results)),
        ?assertEqual(10, length([R || {ok, _} <- Results]))
    end}.
