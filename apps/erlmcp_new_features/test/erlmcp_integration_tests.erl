-module(erlmcp_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Integration Test Suite
%%%===================================================================

setup() ->
    % Start all applications
    {ok, _} = application:ensure_all_started(erlmcp_new_features),
    % Initialize key components
    case whereis(erlmcp_event_bus) of
        undefined -> {ok, _} = erlmcp_event_bus:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_mcp_proxy_relay) of
        undefined -> {ok, _} = erlmcp_mcp_proxy_relay:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_batch_processor) of
        undefined -> {ok, _} = erlmcp_batch_processor:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_json_schema_validator) of
        undefined -> {ok, _} = erlmcp_json_schema_validator:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_workflow_engine) of
        undefined -> {ok, _} = erlmcp_workflow_engine:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_tool_sandbox) of
        undefined -> {ok, _} = erlmcp_tool_sandbox:start_link();
        _ -> ok
    end.

cleanup(_) ->
    % Stop all applications
    application:stop(erlmcp_new_features).

%%%===================================================================
%%% Test Generators
%%%===================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun event_bus_and_proxy_integration/0,
      fun workflow_and_sandbox_integration/0,
      fun batch_processor_and_schema_validator_integration/0,
      fun event_bus_workflow_integration/0,
      fun mcp_relay_workflow_integration/0,
      fun concurrent_operations_integration/0,
      fun error_propagation_integration/0,
      fun resource_monitoring_integration/0,
      end_to_end_workflow_integration/0
     ]}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

event_bus_and_proxy_integration() ->
    {"Event Bus and MCP Proxy Relay integration", fun() ->
        % Subscribe to events
        {ok, SubId} = erlmcp_event_bus:subscribe(proxy_events, undefined),

        % Configure proxy relay
        ok = erlmcp_mcp_proxy_relay:add_upstream(
            integration_test,
            <<"http://localhost:9999">>  % Non-existent, for testing
        ),

        % Publish event that should trigger proxy activity
        Event = #{type => proxy_test, data => #{message => "integration test"}},
        ok = erlmcp_event_bus:publish(proxy_test_event, Event),

        % Wait for event processing
        timer:sleep(100),

        % Verify event was processed
        Subscribers = erlmcp_event_bus:list_subscribers(proxy_test_event),
        ?assert(lists:member(SubId, Subscribers))
    end}.

workflow_and_sandbox_integration() ->
    {"Workflow Engine and Tool Sandbox integration", fun() ->
        % Register tools in sandbox
        ToolDef = #{
            name => <<"workflow_tool">>,
            command => ["/bin/echo"],
            args => [<<"workflow test">>],
            timeout_ms => 3000
        },
        ok = erlmcp_tool_sandbox:register_tool(workflow_tool, ToolDef),

        % Define workflow that uses sandboxed tools
        Workflow = #{
            id => <<"sandbox_workflow">>,
            steps => [
                #{
                    id => <<"tool_step">>,
                    type => tool,
                    tool_name => <<"workflow_tool">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"sandbox_workflow">>, #{}
        ),

        % Wait for completion
        timer:sleep(200),
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assertEqual(completed, maps:get(status, Status))
    end}.

batch_processor_and_schema_validator_integration() ->
    {"Batch Processor and Schema Validator integration", fun() ->
        % Define schema for batch items
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"id">> => #{<<"type">> => <<"string">>},
                <<"data">> => #{<<"type">> => <<"string">>}
            },
            <<"required">> => [<<"id">>, <<"data">>]
        },
        ok = erlmcp_json_schema_validator:load_schema(batch_item_schema, Schema),

        % Configure batch processor
        ok = erlmcp_batch_processor:configure(max_batch_size, 5),
        ok = erlmcp_batch_processor:configure(batch_timeout, 1000),

        % Create batch items with schema validation
        BatchItems = [
            #{
                id => list_to_binary("item" ++ integer_to_list(I)),
                data => <<"test data " ++ integer_to_list(I)>
            }
            || I <- lists:seq(1, 5)
        ],

        % Process batch with schema validation
        {ok, Result} = erlmcp_batch_processor:process_batch(BatchItems),

        % Verify schema validation occurred
        ?assert(is_map(Result)),
        ?assert(maps:is_key(success_count, Result)),
        ?assert(maps:is_key(failure_count, Result))
    end}.

event_bus_workflow_integration() ->
    {"Event Bus and Workflow Engine integration", fun() ->
        % Subscribe to workflow events
        {ok, SubId} = erlmcp_event_bus:subscribe(workflow_events, undefined),

        % Define workflow that publishes events
        Workflow = #{
            id => <<"event_workflow">>,
            steps => [
                #{
                    id => <<"publish_step">>,
                    type => tool,
                    tool_name => <<"event_publisher">>,
                    tool_arguments => #{event_type => <<"integration_event">>},
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"event_workflow">>, #{}
        ),

        % Wait for event publishing
        timer:sleep(200),

        % Verify event was published
        Subscribers = erlmcp_event_bus:list_subscribers(workflow_events),
        ?assert(lists:member(SubId, Subscribers))
    end}.

mcp_relay_workflow_integration() ->
    {"MCP Relay and Workflow Engine integration", fun() ->
        % Configure MCP relay for workflow execution
        ok = erlmcp_mcp_proxy_relay:add_upstream(
            workflow_endpoint,
            <<"http://localhost:8080">>
        ),

        % Define workflow that uses MCP calls
        Workflow = #{
            id => <<"mcp_workflow">>,
            steps => [
                #{
                    id => <<"mcp_step">>,
                    type => tool,
                    tool_name => <<"mcp_call">>,
                    tool_arguments => #{
                        method => <<"tools/call">>,
                        params => #{name => <<"echo">>, arguments => #{text => "hello"}}
                    },
                    max_retries => 2,
                    timeout_sec => 10
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"mcp_workflow">>, #{}
        ),

        % Check execution status
        timer:sleep(500),
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assert(maps:is_key(status, Status))
    end}.

concurrent_operations_integration() ->
    {"Handle concurrent operations across all modules", fun() ->
        % Start multiple concurrent operations
        Operations = [
            % Event bus operations
            fun() ->
                erlmcp_event_bus:publish(concurrent_test, #{data => <<"op1">>})
            end,
            % Workflow operations
            fun() ->
                Workflow = #{
                    id => <<"concurrent_wf">>,
                    steps => [#{id => <<"step">>, type => tool, tool_name => <<"fast">>}],
                    transitions => []
                },
                ok = erlmcp_workflow_engine:define_workflow(Workflow),
                erlmcp_workflow_engine:execute_workflow(<<"concurrent_wf">>, #{})
            end,
            % Batch processor operations
            fun() ->
                Items = [#{id => <<"batch_item", I/binary>>, data => <<"data">>} || I <- [1, 2]],
                erlmcp_batch_processor:process_batch(Items)
            end,
            % Proxy relay operations
            fun() ->
                erlmcp_mcp_proxy_relay:add_upstream(concurrent_upstream, <<"http://test">>)
            end
        ],

        % Execute all operations concurrently
        Pids = [spawn(Op) || Op <- Operations],

        % Wait for completion
        [wait_for_completion(Pid) || Pid <- Pids],

        % Verify all modules are still responsive
        {ok, _} = erlmcp_event_bus:list_subscribers(test),
        {ok, _} = erlmcp_workflow_engine:list_workflows(),
        {ok, _} = erlmcp_batch_processor:get_metrics(),
        {ok, _} = erlmcp_mcp_proxy_relay:list_upstreams()
    end}.

error_propagation_integration() ->
    {"Error propagation across modules", fun() ->
        % Define workflow with failing step
        Workflow = #{
            id => <<"error_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"error_step">>,
                    max_retries => 0,
                    timeout_sec => 5
                },
                % This should not execute if step1 fails
                #{
                    id => <<"step2">>,
                    type => tool,
                    tool_name => <<"never_reached">>,
                    max_retries => 0,
                    timeout_sec => 5
                }
            ],
            transitions => [
                #{from => <<"step1">>, to => <<"step2">>, condition => success}
            ]
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"error_workflow">>, #{}
        ),

        % Check error propagation
        timer:sleep(300),
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),
        ?assertEqual(failed, maps:get(status, Status)),

        % Verify error metrics updated
        WorkflowMetrics = erlmcp_workflow_engine:get_metrics(),
        ?assert(maps:is_key(failed_executions, WorkflowMetrics))
    end}.

resource_monitoring_integration() ->
    {"Resource monitoring across all modules", fun() ->
        % Perform operations to generate metrics
        % Event bus metrics
        erlmcp_event_bus:publish(monitor_test, #{data => <<"test">>}),

        % Workflow metrics
        Workflow = #{
            id => <<"metric_workflow">>,
            steps => [#{id => <<"step">>, type => tool, tool_name => <<"calc">>}],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),
        {ok, _} = erlmcp_workflow_engine:execute_workflow(<<"metric_workflow">>, #{}),

        % Batch processor metrics
        Items = [#{id => <<"item1">>, data => <<"data">>}],
        {ok, _} = erlmcp_batch_processor:process_batch(Items),

        % Check aggregated metrics
        EventMetrics = erlmcp_event_bus:get_metrics(),
        WorkflowMetrics = erlmcp_workflow_engine:get_metrics(),
        BatchMetrics = erlmcp_batch_processor:get_metrics(),

        ?assert(maps:is_key(events_published, EventMetrics)),
        ?assert(maps:is_key(workflows_executed, WorkflowMetrics)),
        ?assert(maps:is_key(batches_processed, BatchMetrics))
    end}.

end_to_end_workflow_integration() ->
    {"End-to-end workflow integration", fun() ->
        % Step 1: Set up schema validation
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"input">> => #{<<"type">> => <<"string">>}
            }
        },
        ok = erlmcp_json_schema_validator:load_schema(e2e_schema, Schema),

        % Step 2: Configure batch processor
        ok = erlmcp_batch_processor:configure(max_batch_size, 3),
        ok = erlmcp_batch_processor:configure(batch_timeout, 2000),

        % Step 3: Register tools
        ToolDef = #{
            name => <<"processor">>,
            command => ["/bin/echo"],
            args => [<<"processed">>],
            timeout_ms => 1000
        },
        ok = erlmcp_tool_sandbox:register_tool(processor_tool, ToolDef),

        % Step 4: Define complex workflow
        Workflow = #{
            id => <<"e2e_workflow">>,
            steps => [
                % Validate input
                #{
                    id => <<"validate">>,
                    type => tool,
                    tool_name => <<"validator">>,
                    tool_arguments => #{schema => e2e_schema},
                    max_retries => 0,
                    timeout_sec => 5
                },
                % Process in batch
                #{
                    id => <<"batch_process">>,
                    type => parallel,
                    child_steps => [
                        #{
                            id => <<"process1">>,
                            type => tool,
                            tool_name => <<"processor">>,
                            tool_arguments => #{input => <<"batch1">>},
                            max_retries => 0,
                            timeout_sec := 5
                        },
                        #{
                            id => <<"process2">>,
                            type => tool,
                            tool_name => <<"processor">>,
                            tool_arguments => #{input => <<"batch2">>},
                            max_retries => 0,
                            timeout_sec := 5
                        },
                        #{
                            id => <<"process3">>,
                            type => tool,
                            tool_name => <<"processor">>,
                            tool_arguments => #{input => <<"batch3">>},
                            max_retries => 0,
                            timeout_sec := 5
                        }
                    ]
                },
                % Final step
                #{
                    id => <<"finalize">>,
                    type => tool,
                    tool_name => <<"completer">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec := 5
                }
            ],
            transitions => [
                #{from => <<"validate">>, to => <<"batch_process">>, condition => success},
                #{from => <<"batch_process">>, to => <<"finalize">>, condition => success}
            ]
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Step 5: Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(
            <<"e2e_workflow">>, #{input => <<"test">>}
        ),

        % Step 6: Monitor execution
        timer:sleep(1000),
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecutionId),

        % Verify complete execution
        ?assertEqual(completed, maps:get(status, Status)),

        % Step 7: Verify all components processed
        EventMetrics = erlmcp_event_bus:get_metrics(),
        WorkflowMetrics = erlmcp_workflow_engine:get_metrics(),
        BatchMetrics = erlmcp_batch_processor:get_metrics(),

        ?assert(maps:get(events_published, EventMetrics) > 0),
        ?assert(maps:get(workflows_executed, WorkflowMetrics) > 0),
        ?assert(maps:get(batches_processed, BatchMetrics) > 0)
    end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

wait_for_completion(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after 5000 ->
        error(wait_timeout)
    end.