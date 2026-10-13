-module(erlmcp_performance_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Performance Regression Tests for erlmcp_new_features
%%%===================================================================

setup() ->
    % Start all components
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
    % Stop all components
    erlmcp_event_bus:stop(),
    erlmcp_mcp_proxy_relay:stop(),
    erlmcp_batch_processor:stop(),
    erlmcp_json_schema_validator:stop(),
    erlmcp_workflow_engine:stop(),
    erlmcp_tool_sandbox:stop().

%%%===================================================================
%%% Performance Test Generators
%%%===================================================================

performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         [
             event_bus_publish_performance(),
             event_bus_subscribe_performance(),
             workflow_definition_performance(),
             workflow_execution_performance(),
             batch_processing_performance(),
             proxy_relay_performance(),
             schema_validation_performance(),
             tool_sandbox_performance(),
             concurrent_operations_performance(),
             memory_usage_performance(),
             startup_performance(),
             throughput_performance()
         ]
     end
    }.

%%%===================================================================
%%% Individual Performance Tests
%%%===================================================================

event_bus_publish_performance() ->
    {"Event Bus Publish Performance", fun() ->
        setup(),
        % Warm up
        [erlmcp_event_bus:publish(warmup, #{}) || _ <- lists:seq(1, 100)],

        % Measure publish performance
        Start = erlang:monotonic_time(millisecond),
        [erlmcp_event_bus:publish(perf_test, #{data => <<"test">>, timestamp => erlang:system_time(millisecond)})
         || _ <- lists:seq(1, 1000)],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        % Assert it's under 1 second (1ms per event)
        ?assert(Duration < 1000, "Event bus publish too slow: ~p ms", [Duration]),
        io:format("Event bus publish 1000 events in ~p ms (~.2f events/ms)~n",
                 [Duration, 1000.0 / Duration])
    end}.

event_bus_subscribe_performance() ->
    {"Event Bus Subscribe Performance", fun() ->
        setup(),
        % Measure subscribe performance
        Start = erlang:monotonic_time(millisecond),
        SubIds = [erlmcp_event_bus:subscribe(list_to_binary("sub_" ++ integer_to_list(I)), undefined)
                 || I <- lists:seq(1, 100)],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        % Clean up
        [erlmcp_event_bus:unsubscribe(SubId) || SubId <- SubIds],

        % Assert it's under 500ms
        ?assert(Duration < 500, "Event bus subscribe too slow: ~p ms", [Duration]),
        io:format("Event bus subscribe 100 subscriptions in ~p ms~n", [Duration])
    end}.

workflow_definition_performance() ->
    {"Workflow Definition Performance", fun() ->
        setup(),
        % Create complex workflows
        Workflows = [begin
            Steps = [begin
                #{
                    id => list_to_binary("step" ++ integer_to_list(I)),
                    type => tool,
                    tool_name => <<"calc">>,
                    tool_arguments => #{expression => list_to_binary(integer_to_list(I))},
                    max_retries => 0,
                    timeout_sec => 1
                }
            end || I <- lists:seq(1, 10)],
            #{
                id => list_to_binary("workflow_" ++ integer_to_list(I)),
                steps => Steps,
                transitions => [#{from => <<"step1">>, to => <<"step2">>, condition => success}]
            }
        end || I <- lists:seq(1, 100)],

        % Measure definition performance
        Start = erlang:monotonic_time(millisecond),
        [erlmcp_workflow_engine:define_workflow(W) || W <- Workflows],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        % Clean up
        [erlmcp_workflow_engine:define_workflow(#{id => W#{id}, steps => [], transitions => []})
         || W <- Workflows],

        % Assert it's under 5 seconds
        ?assert(Duration < 5000, "Workflow definition too slow: ~p ms", [Duration]),
        io:format("Defined 100 workflows in ~p ms (~.2f workflows/ms)~n",
                 [Duration, 1000.0 / Duration])
    end}.

workflow_execution_performance() ->
    {"Workflow Execution Performance", fun() ->
        setup(),
        % Define simple workflow
        Workflow = #{
            id => <<"perf_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"fast">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 1
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Measure execution performance
        Start = erlang:monotonic_time(millisecond),
        Results = [erlmcp_workflow_engine:execute_workflow(<<"perf_workflow">>, #{})
                 || _ <- lists:seq(1, 100)],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        % Clean up
        [case R of
             {ok, ExecId} -> erlmcp_workflow_engine:cancel_execution(ExecId);
             _ -> ok
         end || R <- Results],

        % Assert it's under 5 seconds
        ?assert(Duration < 5000, "Workflow execution too slow: ~p ms", [Duration]),
        io:format("Executed 100 workflows in ~p ms (~.2f workflows/ms)~n",
                 [Duration, 1000.0 / Duration])
    end}.

batch_processing_performance() ->
    {"Batch Processing Performance", fun() ->
        setup(),
        % Configure batch processor
        ok = erlmcp_batch_processor:configure(max_batch_size, 100),
        ok = erlmcp_batch_processor:configure(batch_timeout, 1000),

        % Create test batches
        SmallBatch = [#{id => list_to_binary("item" ++ integer_to_list(I)), data => <<"test">>}
                     || I <- lists:seq(1, 10)],
        MediumBatch = [#{id => list_to_binary("item" ++ integer_to_list(I)), data => <<"test">>}
                      || I <- lists:seq(1, 50)],
        LargeBatch = [#{id => list_to_binary("item" ++ integer_to_list(I)), data => <<"test">>}
                     || I <- lists:seq(1, 100)],

        % Measure small batch performance
        Start1 = erlang:monotonic_time(millisecond),
        [erlmcp_batch_processor:process_batch(SmallBatch) || _ <- lists:seq(1, 100)],
        End1 = erlang:monotonic_time(millisecond),
        Duration1 = End1 - Start1,

        % Measure medium batch performance
        Start2 = erlang:monotonic_time(millisecond),
        [erlmcp_batch_processor:process_batch(MediumBatch) || _ <- lists:seq(1, 50)],
        End2 = erlang:monotonic_time(millisecond),
        Duration2 = End2 - Start2,

        % Measure large batch performance
        Start3 = erlang:monotonic_time(millisecond),
        [erlmcp_batch_processor:process_batch(LargeBatch) || _ <- lists:seq(1, 20)],
        End3 = erlang:monotonic_time(millisecond),
        Duration3 = End3 - Start3,

        io:format("Small batch (10 items): ~p ms (~.2f items/ms)~n",
                 [Duration1, (10 * 100) / Duration1]),
        io:format("Medium batch (50 items): ~p ms (~.2f items/ms)~n",
                 [Duration2, (50 * 50) / Duration2]),
        io:format("Large batch (100 items): ~p ms (~.2f items/ms)~n",
                 [Duration3, (100 * 20) / Duration3]),

        % Assert reasonable performance
        ?assert(Duration1 < 5000, "Small batch too slow: ~p ms", [Duration1]),
        ?assert(Duration2 < 5000, "Medium batch too slow: ~p ms", [Duration2]),
        ?assert(Duration3 < 5000, "Large batch too slow: ~p ms", [Duration3])
    end}.

proxy_relay_performance() ->
    {"MCP Proxy Relay Performance", fun() ->
        setup(),
        % Add upstreams
        ok = erlmcp_mcp_proxy_relay:add_upstream(perf_upstream, <<"http://localhost:9999">>),

        % Measure list performance
        Start1 = erlang:monotonic_time(millisecond),
        [erlmcp_mcp_proxy_relay:list_upstreams() || _ <- lists:seq(1, 1000)],
        End1 = erlang:monotonic_time(millisecond),
        Duration1 = End1 - Start1,

        % Measure stats performance
        Start2 = erlang:monotonic_time(millisecond),
        [erlmcp_mcp_proxy_relay:get_stats() || _ <- lists:seq(1, 1000)],
        End2 = erlang:monotonic_time(millisecond),
        Duration2 = End2 - Start2,

        io:format("Proxy relay list: ~p ms (~.2f calls/ms)~n", [Duration1, 1000.0 / Duration1]),
        io:format("Proxy relay stats: ~p ms (~.2f calls/ms)~n", [Duration2, 1000.0 / Duration2]),

        % Assert reasonable performance
        ?assert(Duration1 < 2000, "List operations too slow: ~p ms", [Duration1]),
        ?assert(Duration2 < 2000, "Stats operations too slow: ~p ms", [Duration2])
    end}.

schema_validation_performance() ->
    {"Schema Validation Performance", fun() ->
        setup(),
        % Load schema
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>},
                <<"age">> => #{<<"type">> => <<"integer">>},
                <<"email">> => #{<<"type">> => <<"string">>>,
                               <<"format">> => <<"email">>},
                <<"tags">> => #{<<"type">> => <<"array">>,
                              <<"items">> => #{<<"type">> => <<"string">>}}}
            },
            <<"required">> => [<<"name">>, <<"email">>]
        },
        ok = erlmcp_json_schema_validator:load_schema(perf_schema, Schema),

        % Create test data
        ValidData = #{
            name => <<"John Doe">>,
            age => 30,
            email => <<"john@example.com">>,
            tags => [<<"tag1">>, <<"tag2">>]
        },

        % Measure validation performance
        Start = erlang:monotonic_time(millisecond),
        [erlmcp_json_schema_validator:validate(perf_schema, ValidData)
         || _ <- lists:seq(1, 1000)],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        io:format("Schema validation 1000 times: ~p ms (~.2f validations/ms)~n",
                 [Duration, 1000.0 / Duration]),

        % Assert it's under 2 seconds
        ?assert(Duration < 2000, "Schema validation too slow: ~p ms", [Duration])
    end}.

tool_sandbox_performance() ->
    {"Tool Sandbox Performance", fun() ->
        setup(),
        % Register test tool
        ToolDef = #{
            name => <<"perf_test">>,
            command => ["/bin/echo"],
            args => [<<"performance test">>],
            timeout_ms => 1000
        },
        ok = erlmcp_tool_sandbox:register_tool(perf_tool, ToolDef),

        % Measure execution performance
        Start = erlang:monotonic_time(millisecond),
        [erlmcp_tool_sandbox:execute_tool(perf_tool, #{}) || _ <- lists:seq(1, 100)],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        io:format("Tool sandbox 100 executions: ~p ms (~.2f executions/ms)~n",
                 [Duration, 1000.0 / Duration]),

        % Assert it's under 5 seconds
        ?assert(Duration < 5000, "Tool execution too slow: ~p ms", [Duration])
    end}.

concurrent_operations_performance() ->
    {"Concurrent Operations Performance", fun() ->
        setup(),
        % Measure concurrent performance
        Start = erlang:monotonic_time(millisecond),

        % Start many concurrent operations
        Operations = lists:map(fun(I) ->
            spawn(fun() ->
                % Mix of operations
                erlmcp_event_bus:publish(concurrent, #{id => I}),
                erlmcp_batch_processor:process_batch([#{id => list_to_binary("item" ++ integer_to_list(I))}]),
                timer:sleep(10)
            end)
        end, lists:seq(1, 100)),

        % Wait for completion
        [wait_for_operation(Op) || Op <- Operations],

        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        io:format("Concurrent 100 operations: ~p ms~n", [Duration]),

        % Assert reasonable concurrency performance
        ?assert(Duration < 10000, "Concurrent operations too slow: ~p ms", [Duration])
    end}.

memory_usage_performance() ->
    {"Memory Usage Performance", fun() ->
        setup(),
        InitialMemory = memory_usage(),

        % Perform memory-intensive operations
        Operations = [
            % Create many workflows
            fun() ->
                Wfs = [#{id => list_to_binary("wf" ++ integer_to_list(I)),
                        steps => [], transitions => []} || I <- lists:seq(1, 100)],
                [erlmcp_workflow_engine:define_workflow(W) || W <- Wfs]
            end,
            % Create many events
            fun() ->
                [erlmcp_event_bus:publish(mem_test, #{data => <<"large_data">>})
                 || _ <- lists:seq(1, 1000)]
            end,
            % Create many batch items
            fun() ->
                Batches = [[#{id => list_to_binary("batch" ++ integer_to_list(I) ++ "_" ++ integer_to_list(J)),
                              data => <<"test">>}
                            || J <- lists:seq(1, 50)] || I <- lists:seq(1, 20)],
                [erlmcp_batch_processor:process_batch(B) || B <- Batches]
            end
        ],

        % Execute operations
        [Op() || Op <- Operations],

        % Measure memory after operations
        MiddleMemory = memory_usage(),

        % Clean up and measure final memory
        erlmcp_workflow_engine:stop(),
        erlmcp_event_bus:stop(),
        erlmcp_batch_processor:stop(),
        FinalMemory = memory_usage(),

        io:format("Initial memory: ~p MB~n", [InitialMemory]),
        io:format("Middle memory: ~p MB~n", [MiddleMemory]),
        io:format("Final memory: ~p MB~n", [FinalMemory]),
        io:format("Peak memory usage: ~p MB~n", [MiddleMemory - InitialMemory]),
        io:format("Memory cleanup: ~p MB~n", [MiddleMemory - FinalMemory]),

        % Assert memory cleanup is effective
        CleanupEfficiency = (MiddleMemory - FinalMemory) / (MiddleMemory - InitialMemory),
        ?assert(CleanupEfficiency > 0.8, "Memory cleanup not efficient: ~.2f", [CleanupEfficiency])
    end}.

startup_performance() ->
    {"Application Startup Performance", fun() ->
        % Measure cold start
        Start = erlang:monotonic_time(millisecond),

        % Restart application
        application:stop(erlmcp_new_features),
        application:unload(erlmcp_new_features),
        {ok, _} = application:ensure_all_started(erlmcp_new_features),

        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        io:format("Cold start: ~p ms~n", [Duration]),

        % Assert startup is reasonable
        ?assert(Duration < 10000, "Application startup too slow: ~p ms", [Duration])
    end}.

throughput_performance() ->
    {"System Throughput Performance", fun() ->
        setup(),

        % Test different throughput scenarios
        Scenarios = [
            {100, 1, "100 sequential operations"},
            {10, 10, "10 concurrent batches of 10"},
            {5, 20, "5 concurrent batches of 20"}
        ],

        [begin
             io:format("Testing: ~s~n", [Desc]),

             % Measure throughput
             Start = erlang:monotonic_time(millisecond),

             Ops = lists:map(fun(I) ->
                 spawn(fun() ->
                     [erlmcp_event_bus:publish(throughput, #{data => <<"test", I/binary>>)
                      || _ <- lists:seq(1, BatchSize)]
                 end)
             end, lists:seq(1, NumBatches)),

             [wait_for_operation(Op) || Op <- Ops],

             End = erlang:monotonic_time(millisecond),
             Duration = End - Start,
             TotalOps = NumBatches * BatchSize,
             Throughput = TotalOps / (Duration / 1000.0),

             io:format("  ~p operations in ~p ms: ~.2f ops/sec~n",
                      [TotalOps, Duration, Throughput]),

             % Assert minimum throughput
             ?assert(Throughput > 100, "Throughput too low: ~.2f ops/sec", [Throughput])
         end || {NumBatches, BatchSize, Desc} <- Scenarios]
    end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

memory_usage() ->
    {memory, Mem} = erlang:process_info(whereis(erlmcp_event_bus), memory),
    {memory, Mem} = erlang:process_info(whereis(erlmcp_mcp_proxy_relay), memory),
    {memory, Mem} = erlang:process_info(whereis(erlmcp_batch_processor), memory),
    {memory, Mem} = erlang:process_info(whereis(erlmcp_json_schema_validator), memory),
    {memory, Mem} = erlang:process_info(whereis(erlmcp_workflow_engine), memory),
    {memory, Mem} = erlang:process_info(whereis(erlmcp_tool_sandbox), memory),

    % Convert to MB
    erlang:round((Mem * 6) / (1024 * 1024)).

wait_for_operation(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after 30000 ->
        exit(wait_timeout)
    end.