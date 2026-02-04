-module(erlmcp_end_to_end_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% End-to-End Integration Test Suite
%%%===================================================================
%%% Comprehensive test of all 5 modules working together
%%% 1. Event Bus + MCP Proxy Relay
%%% 2. Workflow Engine + Tool Sandbox
%%% 3. Batch Processor + Schema Validator
%%% 4. Distributed Lock + Message Queue
%%% 5. Security Sandbox with malicious patterns
%%%===================================================================

setup() ->
    % Start all applications
    {ok, _} = application:ensure_all_started(erlmcp_new_features),

    % Initialize all components
    initialize_components(),
    ok.

initialize_components() ->
    % Event Bus
    case whereis(erlmcp_event_bus) of
        undefined -> {ok, _} = erlmcp_event_bus:start_link();
        _ -> ok
    end,

    % MCP Proxy Relay
    case whereis(erlmcp_mcp_proxy_relay) of
        undefined -> {ok, _} = erlmcp_mcp_proxy_relay:start_link();
        _ -> ok
    end,

    % Workflow Engine
    case whereis(erlmcp_workflow_engine) of
        undefined -> {ok, _} = erlmcp_workflow_engine:start_link();
        _ -> ok
    end,

    % Tool Sandbox
    case whereis(erlmcp_tool_sandbox) of
        undefined -> {ok, _} = erlmcp_tool_sandbox:start_link();
        _ -> ok
    end,

    % Batch Processor
    case whereis(erlmcp_batch_processor) of
        undefined -> {ok, _} = erlmcp_batch_processor:start_link();
        _ -> ok
    end,

    % JSON Schema Validator
    case whereis(erlmcp_json_schema_validator) of
        undefined -> {ok, _} = erlmcp_json_schema_validator:start_link();
        _ -> ok
    end,

    % Distributed Lock
    case whereis(erlmcp_distributed_lock) of
        undefined -> {ok, _} = erlmcp_distributed_lock:start_link();
        _ -> ok
    end,

    % Message Queue
    case whereis(erlmcp_message_queue) of
        undefined -> {ok, _} = erlmcp_message_queue:start_link();
        _ -> ok
    end,

    % Security monitor
    case whereis(erlmcp_security_monitor) of
        undefined -> {ok, _} = erlmcp_security_monitor:start_link();
        _ -> ok
    end,

    % Wait for initialization
    timer:sleep(500).

cleanup(_) ->
    % Stop all applications
    application:stop(erlmcp_new_features).

%%%===================================================================
%%% Test Cases
%%%===================================================================

end_to_end_workflow_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun complete_workflow_integration/0,
      fun concurrent_operations_test/0,
      fun fault_tolerance_test/0,
      fun performance_benchmark_test/0,
      fun security_isolation_test/0
     ]}.

%%%===================================================================
%%% Test Implementations
%%%===================================================================

complete_workflow_integration() ->
    {"Complete end-to-end workflow integration", fun() ->
        % Step 1: Create a schema-validated batch
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"id">> => #{<<"type">> => <<"string">>},
                <<"payload">> => #{<<"type">> => <<"string">>}
            },
            <<"required">> => [<<"id">>, <<"payload">>]
        },
        ok = erlmcp_json_schema_validator:load_schema(e2e_schema, Schema),

        % Step 2: Create workflow that uses sandboxed tools
        ToolDef = #{
            name => <<"e2e_tool">>,
            command => ["/bin/echo"],
            args => [<<"workflow execution">>],
            timeout_ms => 5000,
            security_level => normal
        },
        ok = erlmcp_tool_sandbox:register_tool(e2e_tool, ToolDef),

        Workflow = #{
            id => <<"e2e_workflow">>,
            steps => [
                #{
                    id => <<"validation_step">>,
                    type => validate,
                    schema => e2e_schema,
                    data => #{id => <<"test_001">>, payload => <<"e2e test">>}
                },
                #{
                    id => <<"tool_step">>,
                    type => tool,
                    tool_name => <<"e2e_tool">>,
                    tool_arguments => #{},
                    max_retries => 2,
                    timeout_sec => 10
                },
                #{
                    id => <<"queue_step">>,
                    type => enqueue,
                    queue_name => <<"e2e_results">>,
                    priority => high,
                    data => #{result => <<"completed">>, timestamp => erlang:system_time(millisecond)}
                }
            ],
            transitions => [
                {validation_step, tool_step},
                {tool_step, queue_step}
            ]
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Step 3: Lock resources for execution
        {ok, LockRef} = erlmcp_distributed_lock:acquire(e2e_workflow_lock, 30000),

        % Step 4: Subscribe to workflow events
        {ok, SubId} = erlmcp_event_bus:subscribe(workflow_events, undefined),

        % Step 5: Execute workflow
        {ok, ExecutionId} = erlmcp_workflow_engine:execute_workflow(<<"e2e_workflow">>, #{correlation_id => <<"e2e_test">>}),

        % Step 6: Monitor execution
        monitor_execution(ExecutionId),

        % Step 7: Verify message was queued
        timer:sleep(2000),
        {ok, Messages} = erlmcp_message_queue:list_messages(e2e_results),
        ?assert(length(Messages) > 0, "Expected messages in queue"),

        % Step 8: Check event bus for completion
        Events = erlmcp_event_bus:list_events(workflow_events),
        ?assert(lists:any(fun(E) -> maps:get(type, E) =:= workflow_completed end, Events)),

        % Cleanup
        erlmcp_distributed_lock:release(LockRef),
        erlmcp_event_bus:unsubscribe(workflow_events, SubId)
    end}.

concurrent_operations_test() ->
    {"Concurrent operations with 100+ clients", fun() ->
        % Configure for high concurrency
        ok = erlmcp_batch_processor:configure(max_batch_size, 50),
        ok = erlmcp_batch_processor:configure(batch_timeout, 1000),

        % Start concurrent clients
        {ok, Pids} = start_concurrent_clients(150),

        % Monitor for conflicts
        {ok, _} = erlmcp_event_bus:subscribe(conflict_events, undefined),

        % Process batch operations
        BatchItems = create_test_batch_items(1000),
        ok = erlmcp_batch_processor:enqueue_items(BatchItems),

        % Wait for processing
        timer:sleep(10000),

        % Check results
        {ok, Processed} = erlmcp_batch_processor:get_batch_results(),
        ?assert(length(Processed) >= 100, "Expected at least 100 processed items"),

        % Check for conflicts
        Conflicts = erlmcp_event_bus:list_events(conflict_events),
        ?assert(length(Conflicts) < 10, "Expected minimal conflicts"),

        % Cleanup clients
        stop_concurrent_clients(Pids)
    end}.

fault_tolerance_test() ->
    {"Fault tolerance and graceful degradation", fun() ->
        % Simulate component failures
        simulate_component_failure(erlmcp_event_bus),
        simulate_component_failure(erlmcp_mcp_proxy_relay),

        % Test basic operations still work
        ok = erlmcp_tool_sandbox:register_tool(fault_tool, #{
            name => <<"fault_test">>,
            command => ["/bin/true"],
            args => [],
            timeout_ms => 1000,
            security_level => safe
        }),

        % Workflow should fail gracefully
        {error, Reason} = erlmcp_workflow_engine:execute_workflow(<<"nonexistent_workflow">>, #{}),
        ?assert(is_list(Reason), "Expected error message"),

        % Test retry mechanism
        ok = erlmcp_batch_processor:configure(max_retries, 3),
        ok = erlmcp_batch_processor:configure(retry_delay, 500),

        % Failure-prone operation
        BadItems = create_failure_prone_items(10),
        ok = erlmcp_batch_processor:enqueue_items(BadItems),

        % Should retry and process some
        timer:sleep(5000),
        {ok, Results} = erlmcp_batch_processor:get_batch_results(),
        ?assert(length(Results) > 0, "Expected some successful retries")
    end}.

performance_benchmark_test() ->
    {"Performance benchmark with 1000+ operations", fun() ->
        % Configure for performance
        ok = erlmcp_batch_processor:configure(max_batch_size, 100),
        ok = erlmcp_batch_processor:configure(batch_timeout, 500),

        % Start performance monitoring
        {ok, _} = erlmcp_event_bus:subscribe(performance_events, undefined),

        % Execute large workload
        {StartTime, _} = timer:tc(fun() ->
            LargeBatch = create_test_batch_items(1000),
            ok = erlmcp_batch_processor:enqueue_items(LargeBatch),

            % Monitor progress
            monitor_progress(10, 1000)
        end),

        ElapsedMs = StartTime / 1000,
        Throughput = 1000 / (ElapsedMs / 1000),

        ?assert(Throughput > 50, "Expected throughput > 50 ops/sec"),
        ?assert(ElapsedMs < 30000, "Expected completion within 30s"),

        % Check for performance events
        PerfEvents = erlmcp_event_bus:list_events(performance_events),
        ?assert(length(PerfEvents) > 0, "Expected performance monitoring events")
    end}.

security_isolation_test() ->
    {"Security sandbox isolation with malicious patterns", fun() ->
        % Test malicious pattern detection
        MaliciousTools = [
            #{
                name => <<"malicious_rm">>,
                command => ["/bin/rm"],
                args => [<<"-rf", "/", "important">>],
                timeout_ms => 1000
            },
            #{
                name => <<"malicious_net">>,
                command => ["/bin/nc"],
                args => [<<"-l", "-p", "4444">>],
                timeout_ms => 1000
            },
            #{
                name => <<"malicious_file">>,
                command => ["/bin/cat"],
                args => [<<"/etc/passwd">>],
                timeout_ms => 1000
            }
        ],

        % Malicious tools should be blocked
        lists:foreach(fun(ToolDef) ->
            case erlmcp_tool_sandbox:register_tool(malicious, ToolDef) of
                {error, security_violation} ->
                    ok; % Expected
                Unexpected ->
                    ?assert(false, lists:concat(["Unexpected registration: ", Unexpected]))
            end
        end, MaliciousTools),

        % Safe tools should work
        SafeTool = #{
            name => <<"safe_echo">>,
            command => ["/bin/echo"],
            args => [<<"safe test">>],
            timeout_ms => 1000,
            security_level => safe
        },
        ok = erlmcp_tool_sandbox:register_tool(safe_echo, SafeTool),

        % Test workflow execution with sandbox
        SafeWorkflow = #{
            id => <<"safe_workflow">>,
            steps => [
                #{
                    id => <<"safe_step">>,
                    type => tool,
                    tool_name => <<"safe_echo">>,
                    tool_arguments => #{},
                    timeout_sec => 5
                }
            ],
            transitions => []
        },
        {ok, _} = erlmcp_workflow_engine:define_workflow(SafeWorkflow),
        {ok, ExecId} = erlmcp_workflow_engine:execute_workflow(<<"safe_workflow">>, #{}),

        % Should complete successfully
        timer:sleep(1000),
        {ok, Status} = erlmcp_workflow_engine:get_execution_status(ExecId),
        ?assertEqual(completed, maps:get(status, Status))
    end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

start_concurrent_clients(N) when N > 0 ->
    start_concurrent_clients(N, []).

start_concurrent_clients(0, Pids) ->
    {ok, Pids};
start_concurrent_clients(N, Pids) ->
    % Each client performs independent operations
    Self = self(),
    Pid = spawn(fun() ->
        client_operations(Self)
    end),
    start_concurrent_clients(N-1, [Pid | Pids]).

client_operations(Parent) ->
    % Simulate client operations
    Operations = [
        fun() -> erlmcp_event_bus:publish(client_event, #{client => self(), timestamp => erlang:system_time(millisecond)}) end,
        fun() ->
            Schema = #{<<"type">> => string},
            erlmcp_json_schema_validator:validate(Schema, #{<<"test">> => <<"data">>})
        end,
        fun() ->
            {ok, _} = erlmcp_distributed_lock:acquire(client_lock, 1000),
            timer:sleep(100),
            erlmcp_distributed_lock:release(client_lock)
        end
    ],

    lists:foreach(fun(Op) ->
        try Op()
        catch _:_ -> ok
        end
    end, Operations),

    Parent ! {client_complete, self()}.

stop_concurrent_clients(Pids) ->
    lists:foreach(fun(Pid) ->
        Pid ! stop
    end),
    lists:foreach(fun(Pid) ->
        receive
            {client_complete, Pid} -> ok
        after 2000 ->
            ok
        end
    end, Pids).

monitor_execution(ExecutionId) ->
    monitor_loop(ExecutionId, 30).

monitor_loop(_ExecutionId, 0) ->
    ?assert(false, "Workflow did not complete in time");
monitor_loop(ExecutionId, Attempts) ->
    case erlmcp_workflow_engine:get_execution_status(ExecutionId) of
        {ok, Status} ->
            case maps:get(status, Status) of
                completed -> ok;
                running ->
                    timer:sleep(1000),
                    monitor_loop(ExecutionId, Attempts-1);
                failed ->
                    ?assert(false, "Workflow failed")
            end;
        {error, not_found} ->
            timer:sleep(1000),
            monitor_loop(ExecutionId, Attempts-1)
    end.

monitor_progress(_CheckCount, 0) ->
    ok;
monitor_progress(CheckCount, Remaining) ->
    timer:sleep(1000),
    {ok, Processed} = erlmcp_batch_processor:get_batch_results(),
    ProcessedCount = length(Processed),
    case ProcessedCount >= Remaining - 50 of
        true -> ok;
        false ->
            case CheckCount > 0 of
                true -> monitor_progress(CheckCount-1, Remaining);
                false -> ?assert(false, "Progress too slow")
            end
    end.

create_test_batch_items(N) when N > 0 ->
    lists:map(fun(I) ->
        #{
            id => list_to_binary("item_" ++ integer_to_list(I)),
            data => <<"test data " ++ integer_to_list(I)>,
            timestamp => erlang:system_time(millisecond),
            priority => case I rem 10 of 0 -> high; _ -> normal end
        }
    end, lists:seq(1, N)).

create_failure_prone_items(N) ->
    % Create items that will sometimes fail
    lists:map(fun(I) ->
        case I rem 5 of
            0 -> % These will fail
                #{id => list_to_binary("fail_" ++ integer_to_list(I)), should_fail => true};
            _ -> % These will succeed
                #{id => list_to_binary("ok_" ++ integer_to_list(I)), should_fail => false}
        end
    end, lists:seq(1, N)).

simulate_component_failure(Pid) ->
    % Simulate component crash
    exit(Pid, shutdown),
    timer:sleep(100),
    % Restart should handle gracefully
    {ok, _} = erlmcp_event_bus:start_link().