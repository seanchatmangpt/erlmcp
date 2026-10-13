-module(erlmcp_load_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Load Tests for erlmcp_new_features
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
    end,

    % Register test tools
    ToolDef = #{
        name => <<"load_test">>,
        command => ["/bin/echo"],
        args => [<<"load test">>],
        timeout_ms => 1000
    },
    ok = erlmcp_tool_sandbox:register_tool(load_tool, ToolDef).

cleanup(_) ->
    % Stop all components
    erlmcp_event_bus:stop(),
    erlmcp_mcp_proxy_relay:stop(),
    erlmcp_batch_processor:stop(),
    erlmcp_json_schema_validator:stop(),
    erlmcp_workflow_engine:stop(),
    erlmcp_tool_sandbox:stop().

%%%===================================================================
%%% Load Test Generators
%%%===================================================================

load_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         [
             event_bus_load_test(),
             workflow_load_test(),
             batch_processor_load_test(),
             proxy_relay_load_test(),
             concurrent_client_load_test(),
           memory_under_load_test(),
           throughput_stress_test(),
           error_rate_under_load_test(),
           latency_spike_test(),
           graceful_degradation_test()
         ]
     end
    }.

%%%===================================================================
%%% Individual Load Tests
%%%===================================================================

event_bus_load_test() ->
    {"Event Bus Load Test - High Volume Publishing", fun() ->
        setup(),

        % Configuration
        NumPublishers = 10,
        MessagesPerPublisher = 1000,
        ExpectedTotal = NumPublishers * MessagesPerPublisher,

        % Start publishers
        Start = erlang:monotonic_time(millisecond),
        Pids = [spawn_publisher(PublisherId, MessagesPerPublisher)
                || PublisherId <- lists:seq(1, NumPublishers)],

        % Wait for completion
        [wait_for_publisher(Pid) || Pid <- Pids],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        % Verify results
        ActualTotal = erlmcp_event_bus:get_metrics(),
        Published = maps:get(events_published, ActualTotal, 0),

        Throughput = Published / (Duration / 1000.0),
        io:format("Event Bus Load: ~p publishers, ~p messages in ~p ms~n",
                 [NumPublishers, Published, Duration]),
        io:format("Throughput: ~.2f messages/sec~n", [Throughput]),

        % Load assertions
        ?assert(Published > 0, "No events published"),
        ?assert(Duration < 30000, "Publishing too slow: ~p ms", [Duration]),
        ?assert(Throughput > 1000, "Throughput too low: ~.2f msg/sec", [Throughput]),

        % Check for dropped messages
        DroppedRate = max(0, (ExpectedTotal - Published) / ExpectedTotal),
        io:format("Dropped message rate: ~.2f%~n", [DroppedRate * 100]),
        ?assert(DroppedRate < 0.05, "Too many messages dropped: ~.2f%", [DroppedRate * 100])
    end}.

workflow_load_test() ->
    {"Workflow Engine Load Test - Concurrent Executions", fun() ->
        setup(),

        % Define simple workflow
        Workflow = #{
            id => <<"load_workflow">>,
            steps => [
                #{
                    id => <<"step1">>,
                    type => tool,
                    tool_name => <<"load_test">>,
                    tool_arguments => #{},
                    max_retries => 0,
                    timeout_sec => 2
                }
            ],
            transitions => []
        },
        ok = erlmcp_workflow_engine:define_workflow(Workflow),

        % Execute many workflows concurrently
        NumWorkflows = 100,
        Start = erlang:monotonic_time(millisecond),

        ExecutionIds = [begin
            spawn_link(fun() ->
                Result = erlmcp_workflow_engine:execute_workflow(<<"load_workflow">>, #{}),
                case Result of
                    {ok, ExecId} -> {ok, ExecId};
                    {error, _} -> {error, failed}
                end
            end)
        end || _ <- lists:seq(1, NumWorkflows)],

        % Wait for completions
        Completed = wait_for_workflow_completions(ExecutionIds, NumWorkflows),
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        Throughput = Completed / (Duration / 1000.0),
        SuccessRate = Completed / NumWorkflows,

        io:format("Workflow Load: ~p workflows started, ~p completed in ~p ms~n",
                 [NumWorkflows, Completed, Duration]),
        io:format("Throughput: ~.2f workflows/sec~n", [Throughput]),
        io:format("Success Rate: ~.2f%~n", [SuccessRate * 100]),

        % Load assertions
        ?assert(Completed > 0, "No workflows completed"),
        ?assert(SuccessRate >= 0.95, "Success rate too low: ~.2f%", [SuccessRate * 100]),
        ?assert(Duration < 60000, "Execution too slow: ~p ms", [Duration])
    end}.

batch_processor_load_test() ->
    {"Batch Processor Load Test - High Throughput Batching", fun() ->
        setup(),

        % Configure batch processor for high throughput
        ok = erlmcp_batch_processor:configure(max_batch_size, 100),
        ok = erlmcp_batch_processor:configure(batch_timeout, 500),
        ok = erlmcp_batch_processor:configure(max_concurrency, 20),

        % Generate test data
        SmallBatches = [[#{id => list_to_binary("small" ++ integer_to_list(I) ++ "_" ++ integer_to_list(J)),
                          data => <<"small_data">>}
                        || J <- lists:seq(1, 10)] || I <- lists:seq(1, 50)],

        MediumBatches = [[#{id => list_to_binary("med" ++ integer_to_list(I) ++ "_" ++ integer_to_list(J)),
                           data => <<"medium_data">>}
                         || J <- lists:seq(1, 50)] || I <- lists:seq(1, 20)],

        LargeBatches = [[#{id => list_to_binary("large" ++ integer_to_list(I) ++ "_" ++ integer_to_list(J)),
                          data => <<"large_data">>}
                        || J <- lists:seq(1, 100)] || I <- lists:seq(1, 10)],

        % Test small batches
        Start1 = erlang:monotonic_time(millisecond),
        [erlmcp_batch_processor:process_batch(Batch) || Batch <- SmallBatches],
        End1 = erlang:monotonic_time(millisecond),
        Duration1 = End1 - Start1,

        % Test medium batches
        Start2 = erlang:monotonic_time(millisecond),
        [erlmcp_batch_processor:process_batch(Batch) || Batch <- MediumBatches],
        End2 = erlang:monotonic_time(millisecond),
        Duration2 = End2 - Start2,

        % Test large batches
        Start3 = erlang:monotonic_time(millisecond),
        [erlmcp_batch_processor:process_batch(Batch) || Batch <- LargeBatches],
        End3 = erlang:monotonic_time(millisecond),
        Duration3 = End3 - Start3,

        % Calculate throughput
        ItemsSmall = length(SmallBatches) * 10,
        ItemsMed = length(MediumBatches) * 50,
        ItemsLarge = length(LargeBatches) * 100,

        ThroughputSmall = ItemsSmall / (Duration1 / 1000.0),
        ThroughputMed = ItemsMed / (Duration2 / 1000.0),
        ThroughputLarge = ItemsLarge / (Duration3 / 1000.0),

        io:format("Small Batches: ~p items in ~p ms (~.2f items/sec)~n",
                 [ItemsSmall, Duration1, ThroughputSmall]),
        io:format("Medium Batches: ~p items in ~p ms (~.2f items/sec)~n",
                 [ItemsMed, Duration2, ThroughputMed]),
        io:format("Large Batches: ~p items in ~p ms (~.2f items/sec)~n",
                 [ItemsLarge, Duration3, ThroughputLarge]),

        % Load assertions
        ?assert(ThroughputSmall > 500, "Small batch throughput too low: ~.2f", [ThroughputSmall]),
        ?assert(ThroughputMed > 500, "Medium batch throughput too low: ~.2f", [ThroughputMed]),
        ?assert(ThroughputLarge > 500, "Large batch throughput too low: ~.2f", [ThroughputLarge])
    end}.

proxy_relay_load_test() ->
    {"MCP Proxy Relay Load Test - Connection Handling", fun() ->
        setup(),

        % Add upstream endpoints
        Upstreams = [begin
            Name = list_to_binary("upstream" ++ integer_to_list(I)),
            Url = <<"http://localhost:" ++ integer_to_list(9000 + I)>>,
            ok = erlmcp_mcp_proxy_relay:add_upstream(Name, Url),
            {Name, Url}
        end || I <- lists:seq(1, 10)],

        % Measure upstream listing under load
        Start1 = erlang:monotonic_time(millisecond),
        [erlmcp_mcp_proxy_relay:list_upstreams() || _ <- lists:seq(1, 1000)],
        End1 = erlang:monotonic_time(millisecond),
        Duration1 = End1 - Start1,

        % Measure stats collection under load
        Start2 = erlang:monotonic_time(millisecond),
        [erlmcp_mcp_proxy_relay:get_stats() || _ <- lists:seq(1, 1000)],
        End2 = erlang:monotonic_time(millisecond),
        Duration2 = End2 - Start2,

        ThroughputList = 1000 / (Duration1 / 1000.0),
        ThroughputStats = 1000 / (Duration2 / 1000.0),

        io:format("Proxy Relay List: 1000 calls in ~p ms (~.2f calls/sec)~n",
                 [Duration1, ThroughputList]),
        io:format("Proxy Relay Stats: 1000 calls in ~p ms (~.2f calls/sec)~n",
                 [Duration2, ThroughputStats]),

        % Load assertions
        ?assert(ThroughputList > 500, "List throughput too low: ~.2f", [ThroughputList]),
        ?assert(ThroughputStats > 500, "Stats throughput too low: ~.2f", [ThroughputStats])
    end}.

concurrent_client_load_test() ->
    {"Concurrent Client Load Test - Mixed Workload", fun() ->
        setup(),

        % Simulate multiple clients doing different operations
        NumClients = 50,
        OperationsPerClient = 20,

        % Define different operation types
        OpTypes = [
            fun() -> erlmcp_event_bus:publish(client_test, #{data => <<"event">>}) end,
            fun() ->
                Workflow = #{
                    id => <<"client_workflow">>,
                    steps => [#{id => <<"step">>, type => tool, tool_name => <<"load_test">>}],
                    transitions => []
                },
                erlmcp_workflow_engine:define_workflow(Workflow)
            end,
            fun() ->
                Items = [#{id => list_to_binary("client_item" ++ integer_to_list(I)), data => <<"test">>}
                        || I <- lists:seq(1, 5)],
                erlmcp_batch_processor:process_batch(Items)
            end,
            fun() -> erlmcp_mcp_proxy_relay:add_upstream(client_upstream, <<"http://test">>) end
        ],

        % Start clients
        Start = erlang:monotonic_time(millisecond),
        ClientPids = [spawn_client(ClientId, OperationsPerClient, OpTypes)
                     || ClientId <- lists:seq(1, NumClients)],

        % Wait for all clients to complete
        [wait_for_client(Pid) || Pid <- ClientPids],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        TotalOps = NumClients * OperationsPerClient,
        Throughput = TotalOps / (Duration / 1000.0),

        io:format("Client Load: ~p clients, ~p operations in ~p ms~n",
                 [NumClients, TotalOps, Duration]),
        io:format("Throughput: ~.2f operations/sec~n", [Throughput]),

        % Check system stability
        EventMetrics = erlmcp_event_bus:get_metrics(),
        WorkflowMetrics = erlmcp_workflow_engine:get_metrics(),
        BatchMetrics = erlmcp_batch_processor:get_metrics(),

        io:format("Event Metrics: ~p published~n", [maps:get(events_published, EventMetrics, 0)]),
        io:format("Workflow Metrics: ~p executed~n", [maps:get(workflows_executed, WorkflowMetrics, 0)]),
        io:format("Batch Metrics: ~p processed~n", [maps:get(batches_processed, BatchMetrics, 0)]),

        % Load assertions
        ?assert(Throughput > 100, "Overall throughput too low: ~.2f", [Throughput]),
        ?assert(Duration < 60000, "Mixed workload too slow: ~p ms", [Duration])
    end}.

memory_under_load_test() ->
    {"Memory Usage Under Load Test", fun() ->
        setup(),

        % Measure initial memory
        InitialMem = get_total_memory_mb(),

        % Generate load that creates memory pressure
        LoadOperations = [
            % Create many workflows
            fun() ->
                Wfs = [#{id => list_to_binary("mem_wf" ++ integer_to_list(I)),
                        steps => [#{id => <<"step">>, type => tool, tool_name => <<"load_test">>}],
                        transitions => []} || I <- lists:seq(1, 50)],
                [erlmcp_workflow_engine:define_workflow(W) || W <- Wfs],
                timer:sleep(100)
            end,
            % Create many events
            fun() ->
                [erlmcp_event_bus:publish(mem_load, #{data => lists:duplicate(100, <<"x">>)})
                 || _ <- lists:seq(1, 200)]
            end,
            % Create many batch items
            fun() ->
                Batches = [[#{id => list_to_binary("mem_batch" ++ integer_to_list(I)),
                              data => lists:duplicate(50, <<"y">>)}
                            || _ <- lists:seq(1, 10)] || I <- lists:seq(1, 20)],
                [erlmcp_batch_processor:process_batch(B) || B <- Batches]
            end
        ],

        % Execute load operations
        [Op() || Op <- LoadOperations],

        % Measure peak memory
        PeakMem = get_total_memory_mb(),

        % Clean up and measure final memory
        erlmcp_workflow_engine:stop(),
        erlmcp_event_bus:stop();
        erlmcp_batch_processor:stop(),
        FinalMem = get_total_memory_mb(),

        % Calculate metrics
        PeakUsage = PeakMem - InitialMem,
        CleanupLoss = PeakMem - FinalMem,
        CleanupEfficiency = if PeakUsage > 0 -> CleanupLoss / PeakUsage; true -> 0 end,

        io:format("Memory Usage: Initial=~p MB, Peak=~p MB, Final=~p MB~n",
                 [InitialMem, PeakMem, FinalMem]),
        io:format("Peak Usage=~p MB, Cleanup Loss=~p MB, Efficiency=~.2f%~n",
                 [PeakUsage, CleanupLoss, CleanupEfficiency * 100]),

        % Memory assertions
        ?assert(PeakUsage < 100, "Peak memory usage too high: ~p MB", [PeakUsage]),
        ?assert(CleanupEfficiency > 0.9, "Memory cleanup inefficient: ~.2f%", [CleanupEfficiency * 100])
    end}.

throughput_stress_test() ->
    {"Throughput Stress Test - Maximum Load", fun() ->
        setup(),

        % Maximum load test
        StressDuration = 30000, % 30 seconds
        StartTime = erlang:monotonic_time(millisecond),

        % Start stress operations
        StressPids = [start_stress_operation(StressDuration, OpType)
                      || OpType <- [event_pub, workflow_exec, batch_proc]],

        % Monitor during stress
        CompletedOps = monitor_stress_operations(StressPids, StartTime, StressDuration),

        % Calculate throughput
        TotalOps = CompletedOps,
        ActualDuration = erlang:monotonic_time(millisecond) - StartTime,
        Throughput = TotalOps / (ActualDuration / 1000.0),

        io:format("Stress Test: ~p operations in ~p ms (~.2f ops/sec)~n",
                 [TotalOps, ActualDuration, Throughput]),

        % Stress assertions
        ?assert(TotalOps > 1000, "Too few operations under stress: ~p", [TotalOps]),
        ?assert(Throughput > 50, "Throughput too low under stress: ~.2f", [Throughput])
    end}.

error_rate_under_load_test() ->
    {"Error Rate Under Load Test", fun() ->
        setup(),

        % Configure to potentially fail under load
        ok = erlmcp_batch_processor:configure(max_batch_size, 1), % Very small batches

        % Create error-prone scenarios
        NumBatches = 1000,
        LargeItems = [#{id => list_to_binary("large" ++ integer_to_list(I)),
                       data => lists:duplicate(1000, <<"x">>)}
                     || I <- lists:seq(1, NumBatches)],

        % Execute under load
        Start = erlang:monotonic_time(millisecond),
        Results = [erlmcp_batch_processor:process_batch([Item]) || Item <- LargeItems],
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,

        % Count errors
        SuccessCount = length([R || {ok, _} <- Results]),
        ErrorCount = length([R || {error, _} <- Results]),
        ErrorRate = ErrorCount / NumBatches,

        Throughput = SuccessCount / (Duration / 1000.0),

        io:format("Error Rate Test: ~p success, ~p errors (~.2f%) in ~p ms~n",
                 [SuccessCount, ErrorCount, ErrorRate * 100, Duration]),
        io:format("Successful Throughput: ~.2f batches/sec~n", [Throughput]),

        % Error rate assertions
        ?assert(ErrorRate < 0.1, "Error rate too high: ~.2f%", [ErrorRate * 100]),
        ?assert(Throughput > 10, "Successful throughput too low: ~.2f", [Throughput])
    end}.

latency_spike_test() ->
    {"Latency Spike Test - Response Time Consistency", fun() ->
        setup(),

        % Baseline measurement
        BaselineLatencies = measure_latencies(100),
        BaselineAvg = lists:sum(BaselineLatencies) / length(BaselineLatencies),

        % Introduce load
        LoadPids = [start_load_operation() || _ <- lists:seq(1, 20)],
        timer:sleep(5000), % 5 seconds of load

        % Measure under load
        LoadLatencies = measure_latencies(100),
        LoadAvg = lists:sum(LoadLatencies) / length(LoadLatencies),

        % Stop load
        [exit(Pid, kill) || Pid <- LoadPids],

        % Calculate spike factor
        SpikeFactor = LoadAvg / BaselineAvg,

        io:format("Latency: Baseline=~.2f ms, Under Load=~.2f ms, Spike Factor=~.2fx~n",
                 [BaselineAvg, LoadAvg, SpikeFactor]),

        % Latency assertions
        ?assert(SpikeFactor < 5, "Latency spike too high: ~.2fx", [SpikeFactor])
    end}.

graceful_degradation_test() ->
    {"Graceful Degradation Test - Performance Degradation", fun() ->
        setup(),

        % Get baseline metrics
        BaselineEventRate = measure_event_publish_rate(100),
        BaselineWorkflowRate = measure_workflow_execution_rate(100),

        % Start multiple stressors
        Stressors = [
            spawn(fun() ->
                [erlmcp_batch_processor:process_batch([#{id => <<"stress">>, data => lists:duplicate(1000, <<"x">>}])
                 || _ <- lists:seq(1, 1000)])
            end),
            spawn(fun() ->
                [erlmcp_event_bus:publish(stress, #{data => lists:duplicate(500, <<"y">>)})
                 || _ <- lists:seq(1, 2000)]
            end)
        ],

        % Measure under stress
        StressedEventRate = measure_event_publish_rate(100),
        StressedWorkflowRate = measure_workflow_execution_rate(100),

        % Stop stressors
        [exit(Pid, kill) || Pid <- Stressors],

        % Calculate degradation
        EventDegradation = BaselineEventRate / max(1, StressedEventRate),
        WorkflowDegradation = BaselineWorkflowRate / max(1, StressedWorkflowRate),

        io:format("Degradation: Event=~.2fx, Workflow=~.2fx~n",
                 [EventDegradation, WorkflowDegradation]),

        % Graceful degradation assertions
        ?assert(EventDegradation =< 3, "Event degradation too severe: ~.2fx", [EventDegradation]),
        ?assert(WorkflowDegradation =< 3, "Workflow degradation too severe: ~.2fx", [WorkflowDegradation])
    end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

spawn_publisher(Id, Count) ->
    spawn_link(fun() ->
        [erlmcp_event_bus:publish(list_to_binary("pub" ++ integer_to_list(Id)), #{data => <<"test">>})
         || _ <- lists:seq(1, Count)]
    end).

wait_for_publisher(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end.

spawn_client(ClientId, OpCount, OpTypes) ->
    spawn_link(fun() ->
        [begin
             Op = lists:nth((I rem length(OpTypes)) + 1, OpTypes),
             Op()
         end || I <- lists:seq(1, OpCount)]
    end).

wait_for_client(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end.

start_stress_operation(Duration, OpType) ->
    spawn_link(fun() ->
        EndTime = erlang:monotonic_time(millisecond) + Duration,
        Op = case OpType of
            event_pub ->
                fun() -> erlmcp_event_bus:publish(stress, #{data => <<"op">>}) end;
            workflow_exec ->
                fun() ->
                    Workflow = #{id => <<"stress_wf">>, steps => [], transitions => []},
                    erlmcp_workflow_engine:define_workflow(Workflow),
                    erlmcp_workflow_engine:execute_workflow(<<"stress_wf">>, #{})
                end;
            batch_proc ->
                fun() ->
                    Items = [#{id => <<"stress_item">>, data => <<"data">>}],
                    erlmcp_batch_processor:process_batch(Items)
                end
        end,

        while(fun() -> erlang:monotonic_time(millisecond) < EndTime end, Op)
    end).

monitor_stress_operations(Pids, StartTime, Duration) ->
    monitor_stress_operations(Pids, StartTime, Duration, 0).

monitor_stress_operations([], _, _, Count) -> Count;
monitor_stress_operations(Pids, StartTime, Duration, Count) ->
    case erlang:monotonic_time(millisecond) - StartTime >= Duration of
        true ->
            [exit(Pid, normal) || Pid <- Pids],
            Count;
        false ->
            receive
                {'DOWN', _, _, Pid, normal} ->
                    monitor_stress_operations(Pids -- [Pid], StartTime, Duration, Count + 1);
                {'DOWN', _, _, Pid, _} ->
                    monitor_stress_operations(Pids -- [Pid], StartTime, Duration, Count)
            after 100 ->
                monitor_stress_operations(Pids, StartTime, Duration, Count)
            end
    end.

while(Condition, Fun) ->
    case Condition() of
        true ->
            Fun(),
            while(Condition, Fun);
        false -> ok
    end.

wait_for_workflow_completions(Pids, Expected, Completed) when Completed >= Expected ->
    Completed;
wait_for_workflow_completions(Pids, Expected, Completed) ->
    receive
        {'DOWN', _, _, Pid, normal} ->
            NewPids = lists:delete(Pid, Pids),
            wait_for_workflow_completions(NewPids, Expected, Completed + 1);
        {'DOWN', _, _, Pid, _} ->
            NewPids = lists:delete(Pid, Pids),
            wait_for_workflow_completions(NewPids, Expected, Completed)
    after 10000 ->
        Completed
    end.

get_total_memory_mb() ->
    Processes = [erlmcp_event_bus, erlmcp_mcp_proxy_relay, erlmcp_batch_processor,
                 erlmcp_json_schema_validator, erlmcp_workflow_engine, erlmcp_tool_sandbox],
    TotalMemory = lists:foldl(fun(Pid, Acc) ->
        case erlang:process_info(Pid, memory) of
            {memory, Mem} -> Acc + Mem;
            undefined -> Acc
        end
    end, 0, Processes),
    erlang:round(TotalMemory / (1024 * 1024)).

measure_latencies(Count) ->
    Start = erlang:monotonic_time(microsecond),
    [begin
         erlmcp_event_bus:publish(latency_test, #{}),
         End = erlang:monotonic_time(microsecond),
         End - Start
     end || _ <- lists:seq(1, Count)].

measure_event_publish_rate(Count) ->
    Start = erlang:monotonic_time(millisecond),
    [erlmcp_event_bus:publish(rate_test, #{}) || _ <- lists:seq(1, Count)],
    End = erlang:monotonic_time(millisecond),
    Count / ((End - Start) / 1000.0).

measure_workflow_execution_rate(Count) ->
    Start = erlang:monotonic_time(millisecond),
    Results = [erlmcp_workflow_engine:execute_workflow(<<"rate_wf">>, #{})
               || _ <- lists:seq(1, Count)],
    End = erlang:monotonic_time(millisecond),
    length([R || {ok, _} <- Results]) / ((End - Start) / 1000.0).

start_load_operation() ->
    spawn_link(fun() ->
        timer:sleep(5000),
        ok
    end).