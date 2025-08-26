-module(erlmcp_routing_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

%% Common Test callbacks
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

%% Test cases - End-to-End Message Flow
-export([
    test_transport_to_server_flow/1,
    test_server_to_transport_flow/1,
    test_bidirectional_message_flow/1,
    test_multiple_transport_routing/1
]).

%% Test cases - Load Balancing & Scaling
-export([
    test_round_robin_load_balancing/1,
    test_weighted_load_balancing/1,
    test_circuit_breaker_patterns/1,
    test_adaptive_routing/1
]).

%% Test cases - Performance Testing
-export([
    test_throughput_10k_messages/1,
    test_latency_p99_under_1ms/1,
    test_concurrent_routing_stress/1,
    test_memory_efficiency_validation/1,
    test_message_ordering_guarantees/1
]).

%% Test cases - Failure Scenarios
-export([
    test_destination_unreachable/1,
    test_message_timeout_handling/1,
    test_queue_overflow_protection/1,
    test_backpressure_mechanisms/1,
    test_dead_letter_queues/1
]).

%% Test cases - Monitoring & Metrics
-export([
    test_message_count_tracking/1,
    test_latency_histograms/1,
    test_error_rate_monitoring/1,
    test_queue_depth_metrics/1
]).

%% Helper functions
-export([
    start_mock_server/1,
    start_mock_transport/1,
    create_test_message/1,
    measure_latency/1,
    collect_metrics/0,
    setup_load_balancer/1
]).

%%====================================================================
%% Common Test callbacks
%%====================================================================

all() ->
    [
        % End-to-End Message Flow Tests
        test_transport_to_server_flow,
        test_server_to_transport_flow,
        test_bidirectional_message_flow,
        test_multiple_transport_routing,
        
        % Load Balancing & Scaling Tests
        test_round_robin_load_balancing,
        test_weighted_load_balancing,
        test_circuit_breaker_patterns,
        test_adaptive_routing,
        
        % Performance Tests
        test_throughput_10k_messages,
        test_latency_p99_under_1ms,
        test_concurrent_routing_stress,
        test_memory_efficiency_validation,
        test_message_ordering_guarantees,
        
        % Failure Scenarios
        test_destination_unreachable,
        test_message_timeout_handling,
        test_queue_overflow_protection,
        test_backpressure_mechanisms,
        test_dead_letter_queues,
        
        % Monitoring & Metrics
        test_message_count_tracking,
        test_latency_histograms,
        test_error_rate_monitoring,
        test_queue_depth_metrics
    ].

init_per_suite(Config) ->
    % Start the application and required services
    application:ensure_all_started(erlmcp),
    
    % Start enhanced registry with routing capabilities
    {ok, RegistryPid} = erlmcp_registry:start_link(),
    
    % Initialize metrics collection
    erlmcp_metrics:init(),
    
    % Setup tracing
    erlmcp_tracing:init(),
    
    [{registry_pid, RegistryPid} | Config].

end_per_suite(Config) ->
    RegistryPid = ?config(registry_pid, Config),
    gen_server:stop(RegistryPid),
    application:stop(erlmcp),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    
    % Reset metrics for each test
    erlmcp_metrics:reset(),
    
    % Start test-specific monitoring
    TestMetrics = #{
        test_case => TestCase,
        start_time => erlang:system_time(microsecond),
        message_count => 0,
        error_count => 0,
        latencies => []
    },
    
    [{test_metrics, TestMetrics} | Config].

end_per_testcase(TestCase, Config) ->
    TestMetrics = ?config(test_metrics, Config),
    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - maps:get(start_time, TestMetrics),
    
    ct:pal("Test case ~p completed in ~p microseconds", [TestCase, Duration]),
    ct:pal("Final metrics: ~p", [TestMetrics]),
    
    ok.

%%====================================================================
%% End-to-End Message Flow Tests
%%====================================================================

test_transport_to_server_flow(Config) ->
    % Test: Transport → Registry → Server message flow
    RegistryPid = ?config(registry_pid, Config),
    
    % Start mock server
    {ok, ServerPid} = start_mock_server("test_server"),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server("test_server", ServerPid, ServerConfig),
    
    % Start mock transport
    {ok, TransportPid} = start_mock_transport("test_transport"),
    TransportConfig = #{type => stdio, server_id => "test_server"},
    ok = erlmcp_registry:register_transport("test_transport", TransportPid, TransportConfig),
    
    % Send test message through routing system
    TestMessage = create_test_message("test_payload"),
    StartTime = erlang:system_time(microsecond),
    
    ok = erlmcp_registry:route_to_server("test_server", "test_transport", TestMessage),
    
    % Verify message reached server
    receive
        {server_received, "test_server", "test_transport", TestMessage} ->
            EndTime = erlang:system_time(microsecond),
            Latency = EndTime - StartTime,
            ct:pal("Message routed in ~p microseconds", [Latency]),
            ?assert(Latency < 1000), % Less than 1ms
            ok
    after 5000 ->
        ct:fail("Message not received by server within timeout")
    end.

test_server_to_transport_flow(Config) ->
    % Test: Server → Registry → Transport message flow
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup server and transport
    {ok, ServerPid} = start_mock_server("response_server"),
    {ok, TransportPid} = start_mock_transport("response_transport"),
    
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => "response_server"},
    
    ok = erlmcp_registry:register_server("response_server", ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport("response_transport", TransportPid, TransportConfig),
    
    % Send response from server to transport
    ResponseMessage = create_test_message("response_payload"),
    StartTime = erlang:system_time(microsecond),
    
    ok = erlmcp_registry:route_to_transport("response_transport", "response_server", ResponseMessage),
    
    % Verify response reached transport
    receive
        {transport_received, "response_transport", "response_server", ResponseMessage} ->
            EndTime = erlang:system_time(microsecond),
            Latency = EndTime - StartTime,
            ct:pal("Response routed in ~p microseconds", [Latency]),
            ?assert(Latency < 1000), % Less than 1ms
            ok
    after 5000 ->
        ct:fail("Response not received by transport within timeout")
    end.

test_bidirectional_message_flow(Config) ->
    % Test complete request/response cycle
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup bidirectional communication
    {ok, ServerPid} = start_mock_server("bidirectional_server"),
    {ok, TransportPid} = start_mock_transport("bidirectional_transport"),
    
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => "bidirectional_server"},
    
    ok = erlmcp_registry:register_server("bidirectional_server", ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport("bidirectional_transport", TransportPid, TransportConfig),
    
    % Send request and measure round-trip time
    RequestMessage = create_test_message("ping"),
    StartTime = erlang:system_time(microsecond),
    
    ok = erlmcp_registry:route_to_server("bidirectional_server", "bidirectional_transport", RequestMessage),
    
    % Server should auto-respond with pong
    receive
        {transport_received, "bidirectional_transport", "bidirectional_server", ResponseMessage} ->
            EndTime = erlang:system_time(microsecond),
            RoundTripTime = EndTime - StartTime,
            ct:pal("Round-trip completed in ~p microseconds", [RoundTripTime]),
            ?assert(RoundTripTime < 2000), % Less than 2ms for round trip
            
            % Verify response content
            ExpectedResponse = create_test_message("pong"),
            ?assertEqual(ExpectedResponse, ResponseMessage),
            ok
    after 5000 ->
        ct:fail("Bidirectional communication failed")
    end.

test_multiple_transport_routing(Config) ->
    % Test routing to multiple transports simultaneously
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup one server with multiple transports
    {ok, ServerPid} = start_mock_server("multi_server"),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server("multi_server", ServerPid, ServerConfig),
    
    % Start multiple transports
    TransportPids = lists:map(fun(N) ->
        TransportId = lists:flatten(io_lib:format("transport_~p", [N])),
        {ok, Pid} = start_mock_transport(TransportId),
        TransportConfig = #{type => stdio, server_id => "multi_server"},
        ok = erlmcp_registry:register_transport(TransportId, Pid, TransportConfig),
        {TransportId, Pid}
    end, lists:seq(1, 5)),
    
    % Send messages to server from different transports
    Messages = lists:map(fun({TransportId, _}) ->
        Message = create_test_message(lists:flatten(io_lib:format("msg_from_~s", [TransportId]))),
        ok = erlmcp_registry:route_to_server("multi_server", TransportId, Message),
        {TransportId, Message}
    end, TransportPids),
    
    % Verify all messages received
    ReceivedCount = receive_multiple_messages(length(Messages), 5000),
    ?assertEqual(length(Messages), ReceivedCount),
    
    ct:pal("Successfully routed ~p messages from multiple transports", [ReceivedCount]).

%%====================================================================
%% Load Balancing & Scaling Tests
%%====================================================================

test_round_robin_load_balancing(Config) ->
    % Test round-robin distribution across multiple servers
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup multiple servers
    ServerPids = lists:map(fun(N) ->
        ServerId = lists:flatten(io_lib:format("lb_server_~p", [N])),
        {ok, Pid} = start_mock_server(ServerId),
        ServerConfig = #{capabilities => #mcp_server_capabilities{}},
        ok = erlmcp_registry:register_server(ServerId, Pid, ServerConfig),
        {ServerId, Pid}
    end, lists:seq(1, 3)),
    
    % Setup load balancer
    LoadBalancer = setup_load_balancer(round_robin),
    
    % Send messages and verify distribution
    NumMessages = 12,
    lists:foreach(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("msg_~p", [N]))),
        
        % Load balancer should select server in round-robin fashion
        ServerId = select_server_round_robin(ServerPids, N),
        ok = erlmcp_registry:route_to_server(ServerId, "test_transport", Message)
    end, lists:seq(1, NumMessages)),
    
    % Verify each server received equal number of messages
    ExpectedPerServer = NumMessages div length(ServerPids),
    lists:foreach(fun({ServerId, _}) ->
        ReceivedCount = count_messages_for_server(ServerId),
        ?assertEqual(ExpectedPerServer, ReceivedCount)
    end, ServerPids).

test_weighted_load_balancing(Config) ->
    % Test weighted distribution based on server capacity
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup servers with different weights
    ServerConfigs = [
        {"weighted_server_1", 1}, % Weight 1
        {"weighted_server_2", 2}, % Weight 2  
        {"weighted_server_3", 3}  % Weight 3
    ],
    
    ServerPids = lists:map(fun({ServerId, Weight}) ->
        {ok, Pid} = start_mock_server(ServerId),
        ServerConfig = #{
            capabilities => #mcp_server_capabilities{},
            weight => Weight
        },
        ok = erlmcp_registry:register_server(ServerId, Pid, ServerConfig),
        {ServerId, Pid, Weight}
    end, ServerConfigs),
    
    % Send messages using weighted distribution
    NumMessages = 60, % Should distribute as 10:20:30 based on weights
    lists:foreach(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("weighted_msg_~p", [N]))),
        ServerId = select_server_weighted(ServerPids, N),
        ok = erlmcp_registry:route_to_server(ServerId, "test_transport", Message)
    end, lists:seq(1, NumMessages)),
    
    % Verify distribution matches weights
    TotalWeight = lists:sum([W || {_, _, W} <- ServerPids]),
    lists:foreach(fun({ServerId, _, Weight}) ->
        Expected = (NumMessages * Weight) div TotalWeight,
        Actual = count_messages_for_server(ServerId),
        ?assert(abs(Expected - Actual) =< 2) % Allow small variance
    end, ServerPids).

test_circuit_breaker_patterns(Config) ->
    % Test circuit breaker for failing servers
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup server that will fail
    {ok, ServerPid} = start_mock_server("failing_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        circuit_breaker => #{
            failure_threshold => 3,
            timeout => 5000,
            retry_timeout => 10000
        }
    },
    ok = erlmcp_registry:register_server("failing_server", ServerPid, ServerConfig),
    
    % Setup backup server
    {ok, BackupPid} = start_mock_server("backup_server"),
    BackupConfig = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server("backup_server", BackupPid, BackupConfig),
    
    % Simulate server failures
    simulate_server_failures("failing_server", 5),
    
    % Verify circuit breaker opened
    ?assertEqual(open, get_circuit_breaker_state("failing_server")),
    
    % Send messages - should route to backup
    Message = create_test_message("circuit_breaker_test"),
    ok = erlmcp_registry:route_to_server("failing_server", "test_transport", Message),
    
    % Verify message went to backup server
    receive
        {server_received, "backup_server", "test_transport", Message} ->
            ct:pal("Circuit breaker successfully routed to backup"),
            ok
    after 3000 ->
        ct:fail("Circuit breaker did not route to backup")
    end.

test_adaptive_routing(Config) ->
    % Test adaptive routing based on server load
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup servers with load monitoring
    ServerPids = lists:map(fun(N) ->
        ServerId = lists:flatten(io_lib:format("adaptive_server_~p", [N])),
        {ok, Pid} = start_mock_server(ServerId),
        ServerConfig = #{
            capabilities => #mcp_server_capabilities{},
            adaptive_routing => true,
            max_concurrent => 10
        },
        ok = erlmcp_registry:register_server(ServerId, Pid, ServerConfig),
        {ServerId, Pid}
    end, lists:seq(1, 3)),
    
    % Simulate high load on first server
    simulate_high_load("adaptive_server_1", 15), % Exceeds max_concurrent
    
    % Send burst of messages
    lists:foreach(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("adaptive_msg_~p", [N]))),
        
        % Router should avoid overloaded server
        ServerId = select_server_adaptive(ServerPids),
        ?assertNotEqual("adaptive_server_1", ServerId),
        
        ok = erlmcp_registry:route_to_server(ServerId, "test_transport", Message)
    end, lists:seq(1, 20)),
    
    % Verify load was distributed away from overloaded server
    Server1Load = get_server_load("adaptive_server_1"),
    Server2Load = get_server_load("adaptive_server_2"),
    Server3Load = get_server_load("adaptive_server_3"),
    
    ?assert(Server2Load + Server3Load > Server1Load * 2).

%%====================================================================
%% Performance Tests
%%====================================================================

test_throughput_10k_messages(Config) ->
    % Test sustained throughput of 10,000 messages per second
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup high-performance server
    {ok, ServerPid} = start_mock_server("throughput_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        high_performance => true
    },
    ok = erlmcp_registry:register_server("throughput_server", ServerPid, ServerConfig),
    
    % Setup transport
    {ok, TransportPid} = start_mock_transport("throughput_transport"),
    TransportConfig = #{type => stdio, server_id => "throughput_server"},
    ok = erlmcp_registry:register_transport("throughput_transport", TransportPid, TransportConfig),
    
    % Prepare messages
    NumMessages = 10000,
    Messages = [create_test_message(lists:flatten(io_lib:format("throughput_msg_~p", [N]))) 
                || N <- lists:seq(1, NumMessages)],
    
    % Measure throughput
    StartTime = erlang:system_time(microsecond),
    
    % Send all messages as fast as possible
    lists:foreach(fun(Message) ->
        ok = erlmcp_registry:route_to_server("throughput_server", "throughput_transport", Message)
    end, Messages),
    
    % Wait for all messages to be processed
    ReceivedCount = wait_for_messages(NumMessages, 10000), % 10 second timeout
    EndTime = erlang:system_time(microsecond),
    
    Duration = (EndTime - StartTime) / 1000000, % Convert to seconds
    Throughput = NumMessages / Duration,
    
    ct:pal("Processed ~p messages in ~p seconds (~p msg/sec)", 
           [ReceivedCount, Duration, Throughput]),
    
    ?assertEqual(NumMessages, ReceivedCount),
    ?assert(Throughput >= 10000), % At least 10k msg/sec
    
    ok.

test_latency_p99_under_1ms(Config) ->
    % Test that 99th percentile latency is under 1ms
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup low-latency server
    {ok, ServerPid} = start_mock_server("latency_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        low_latency => true
    },
    ok = erlmcp_registry:register_server("latency_server", ServerPid, ServerConfig),
    
    % Setup transport
    {ok, TransportPid} = start_mock_transport("latency_transport"),
    TransportConfig = #{type => stdio, server_id => "latency_server"},
    ok = erlmcp_registry:register_transport("latency_transport", TransportPid, TransportConfig),
    
    % Measure latencies for 1000 messages
    NumSamples = 1000,
    Latencies = lists:map(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("latency_msg_~p", [N]))),
        measure_latency(fun() ->
            ok = erlmcp_registry:route_to_server("latency_server", "latency_transport", Message)
        end)
    end, lists:seq(1, NumSamples)),
    
    % Calculate percentiles
    SortedLatencies = lists:sort(Latencies),
    P50 = lists:nth(500, SortedLatencies),   % 50th percentile
    P95 = lists:nth(950, SortedLatencies),   % 95th percentile  
    P99 = lists:nth(990, SortedLatencies),   % 99th percentile
    
    ct:pal("Latency percentiles - P50: ~p μs, P95: ~p μs, P99: ~p μs", [P50, P95, P99]),
    
    % Verify requirements
    ?assert(P99 < 1000), % P99 under 1ms (1000 μs)
    ?assert(P95 < 500),  % P95 under 0.5ms
    ?assert(P50 < 100),  % P50 under 0.1ms
    
    ok.

test_concurrent_routing_stress(Config) ->
    % Test concurrent routing under extreme load
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup multiple servers for load distribution
    NumServers = 10,
    ServerPids = lists:map(fun(N) ->
        ServerId = lists:flatten(io_lib:format("stress_server_~p", [N])),
        {ok, Pid} = start_mock_server(ServerId),
        ServerConfig = #{capabilities => #mcp_server_capabilities{}},
        ok = erlmcp_registry:register_server(ServerId, Pid, ServerConfig),
        {ServerId, Pid}
    end, lists:seq(1, NumServers)),
    
    % Setup multiple transports
    NumTransports = 20,
    TransportPids = lists:map(fun(N) ->
        TransportId = lists:flatten(io_lib:format("stress_transport_~p", [N])),
        {ok, Pid} = start_mock_transport(TransportId),
        TransportConfig = #{type => stdio},
        ok = erlmcp_registry:register_transport(TransportId, Pid, TransportConfig),
        {TransportId, Pid}
    end, lists:seq(1, NumTransports)),
    
    % Launch concurrent senders
    NumProcesses = 50,
    MessagesPerProcess = 200,
    StartTime = erlang:system_time(microsecond),
    
    SenderPids = lists:map(fun(N) ->
        spawn_link(fun() ->
            lists:foreach(fun(M) ->
                ServerId = select_random_server(ServerPids),
                TransportId = select_random_transport(TransportPids),
                Message = create_test_message(lists:flatten(io_lib:format("stress_~p_~p", [N, M]))),
                ok = erlmcp_registry:route_to_server(ServerId, TransportId, Message)
            end, lists:seq(1, MessagesPerProcess))
        end)
    end, lists:seq(1, NumProcesses)),
    
    % Wait for all senders to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, normal} -> ok
        after 30000 ->
            exit(Pid, kill),
            ct:fail("Stress test sender timeout")
        end
    end, SenderPids),
    
    EndTime = erlang:system_time(microsecond),
    Duration = (EndTime - StartTime) / 1000000,
    
    TotalMessages = NumProcesses * MessagesPerProcess,
    Throughput = TotalMessages / Duration,
    
    ct:pal("Stress test: ~p messages in ~p seconds (~p msg/sec)", 
           [TotalMessages, Duration, Throughput]),
    
    % Verify system remained stable
    ?assert(Throughput > 5000), % At least 5k msg/sec under stress
    
    % Check for memory leaks
    MemoryAfter = erlang:memory(total),
    ct:pal("Memory usage after stress test: ~p bytes", [MemoryAfter]),
    
    ok.

test_memory_efficiency_validation(Config) ->
    % Test memory usage remains bounded under load
    RegistryPid = ?config(registry_pid, Config),
    
    % Record initial memory usage
    MemoryInitial = erlang:memory(total),
    
    % Setup test components
    {ok, ServerPid} = start_mock_server("memory_server"),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server("memory_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("memory_transport"),
    TransportConfig = #{type => stdio, server_id => "memory_server"},
    ok = erlmcp_registry:register_transport("memory_transport", TransportPid, TransportConfig),
    
    % Send waves of messages with measurement
    NumWaves = 10,
    MessagesPerWave = 1000,
    MemoryMeasurements = [],
    
    FinalMeasurements = lists:foldl(fun(Wave, Acc) ->
        % Send batch of messages
        lists:foreach(fun(N) ->
            Message = create_test_message(lists:flatten(io_lib:format("memory_wave_~p_~p", [Wave, N]))),
            ok = erlmcp_registry:route_to_server("memory_server", "memory_transport", Message)
        end, lists:seq(1, MessagesPerWave)),
        
        % Wait for processing
        timer:sleep(100),
        
        % Force garbage collection
        erlang:garbage_collect(),
        
        % Measure memory
        MemoryNow = erlang:memory(total),
        [{Wave, MemoryNow} | Acc]
    end, MemoryMeasurements, lists:seq(1, NumWaves)),
    
    % Analyze memory growth
    Memories = [Mem || {_, Mem} <- lists:reverse(FinalMeasurements)],
    MaxMemory = lists:max(Memories),
    MemoryGrowth = MaxMemory - MemoryInitial,
    
    ct:pal("Memory growth: ~p bytes (~p KB)", [MemoryGrowth, MemoryGrowth div 1024]),
    ct:pal("Memory measurements: ~p", [FinalMeasurements]),
    
    % Verify memory usage is reasonable
    MaxAllowedGrowth = 50 * 1024 * 1024, % 50 MB
    ?assert(MemoryGrowth < MaxAllowedGrowth),
    
    % Verify no significant memory leak
    FinalMemory = lists:last(Memories),
    EarlyMemory = lists:nth(3, Memories), % After initial ramp-up
    MemoryLeak = FinalMemory - EarlyMemory,
    MaxAllowedLeak = 10 * 1024 * 1024, % 10 MB
    ?assert(MemoryLeak < MaxAllowedLeak),
    
    ok.

test_message_ordering_guarantees(Config) ->
    % Test that message ordering is preserved per transport
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup ordered message server
    {ok, ServerPid} = start_mock_server("ordered_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        preserve_order => true
    },
    ok = erlmcp_registry:register_server("ordered_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("ordered_transport"),
    TransportConfig = #{type => stdio, server_id => "ordered_server"},
    ok = erlmcp_registry:register_transport("ordered_transport", TransportPid, TransportConfig),
    
    % Send sequence of ordered messages
    NumMessages = 100,
    OrderedMessages = lists:map(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("ordered_msg_~4..0w", [N]))),
        ok = erlmcp_registry:route_to_server("ordered_server", "ordered_transport", Message),
        Message
    end, lists:seq(1, NumMessages)),
    
    % Collect received messages in order
    ReceivedMessages = collect_ordered_messages(NumMessages, 5000),
    
    % Verify ordering preserved
    ?assertEqual(length(OrderedMessages), length(ReceivedMessages)),
    
    lists:foreach(fun({Sent, Received}) ->
        ?assertEqual(Sent, Received)
    end, lists:zip(OrderedMessages, ReceivedMessages)),
    
    ct:pal("Message ordering verified for ~p messages", [NumMessages]),
    ok.

%%====================================================================
%% Failure Scenarios
%%====================================================================

test_destination_unreachable(Config) ->
    % Test handling when destination server/transport is unreachable
    RegistryPid = ?config(registry_pid, Config),
    
    % Try to route to non-existent server
    Message = create_test_message("unreachable_test"),
    
    % This should not crash the registry
    Result1 = erlmcp_registry:route_to_server("nonexistent_server", "some_transport", Message),
    ?assertEqual(ok, Result1), % Cast always returns ok, but message is dropped
    
    % Try to route to non-existent transport  
    Result2 = erlmcp_registry:route_to_transport("nonexistent_transport", "some_server", Message),
    ?assertEqual(ok, Result2), % Cast always returns ok, but message is dropped
    
    % Setup server and transport, then kill server
    {ok, ServerPid} = start_mock_server("killable_server"),
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server("killable_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("killable_transport"),
    TransportConfig = #{type => stdio, server_id => "killable_server"},
    ok = erlmcp_registry:register_transport("killable_transport", TransportPid, TransportConfig),
    
    % Kill the server
    exit(ServerPid, kill),
    timer:sleep(100), % Allow cleanup
    
    % Try to route message - should handle gracefully
    Result3 = erlmcp_registry:route_to_server("killable_server", "killable_transport", Message),
    ?assertEqual(ok, Result3),
    
    % Verify server was cleaned up from registry
    ?assertEqual({error, not_found}, erlmcp_registry:find_server("killable_server")),
    
    ok.

test_message_timeout_handling(Config) ->
    % Test handling of message timeouts
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup slow server that doesn't respond quickly
    {ok, ServerPid} = start_mock_server("slow_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        processing_delay => 5000 % 5 second delay
    },
    ok = erlmcp_registry:register_server("slow_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("timeout_transport"),
    TransportConfig = #{
        type => stdio, 
        server_id => "slow_server",
        message_timeout => 2000 % 2 second timeout
    },
    ok = erlmcp_registry:register_transport("timeout_transport", TransportPid, TransportConfig),
    
    % Send message that will timeout
    Message = create_test_message("timeout_test"),
    StartTime = erlang:system_time(microsecond),
    
    ok = erlmcp_registry:route_to_server("slow_server", "timeout_transport", Message),
    
    % Should receive timeout notification, not the response
    receive
        {timeout, "timeout_transport", Message} ->
            EndTime = erlang:system_time(microsecond),
            Duration = (EndTime - StartTime) div 1000,
            ct:pal("Message timeout after ~p ms", [Duration]),
            ?assert(Duration >= 2000), % At least timeout duration
            ?assert(Duration < 3000),  % But not too much longer
            ok
    after 10000 ->
        ct:fail("Timeout notification not received")
    end.

test_queue_overflow_protection(Config) ->
    % Test queue overflow protection mechanisms
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup server with limited queue capacity
    {ok, ServerPid} = start_mock_server("limited_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        max_queue_size => 10,
        processing_delay => 100 % Slow processing to fill queue
    },
    ok = erlmcp_registry:register_server("limited_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("overflow_transport"),
    TransportConfig = #{type => stdio, server_id => "limited_server"},
    ok = erlmcp_registry:register_transport("overflow_transport", TransportPid, TransportConfig),
    
    % Send more messages than queue capacity
    NumMessages = 20, % More than max_queue_size (10)
    OverflowCount = 0,
    
    FinalOverflowCount = lists:foldl(fun(N, Acc) ->
        Message = create_test_message(lists:flatten(io_lib:format("overflow_msg_~p", [N]))),
        ok = erlmcp_registry:route_to_server("limited_server", "overflow_transport", Message),
        
        % Check if we get overflow notification
        receive
            {queue_overflow, "limited_server", Message} ->
                Acc + 1
        after 50 ->
            Acc
        end
    end, OverflowCount, lists:seq(1, NumMessages)),
    
    ct:pal("Overflow protection triggered ~p times out of ~p messages", 
           [FinalOverflowCount, NumMessages]),
    
    % Should have some overflow protection
    ?assert(FinalOverflowCount >= 5), % At least half should overflow
    
    ok.

test_backpressure_mechanisms(Config) ->
    % Test backpressure when downstream is overwhelmed
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup slow downstream server
    {ok, ServerPid} = start_mock_server("backpressure_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        processing_delay => 50, % Slow processing
        enable_backpressure => true
    },
    ok = erlmcp_registry:register_server("backpressure_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("backpressure_transport"),
    TransportConfig = #{type => stdio, server_id => "backpressure_server"},
    ok = erlmcp_registry:register_transport("backpressure_transport", TransportPid, TransportConfig),
    
    % Send burst of messages
    NumMessages = 100,
    StartTime = erlang:system_time(microsecond),
    
    % Monitor backpressure signals
    BackpressureCount = monitor_backpressure(fun() ->
        lists:foreach(fun(N) ->
            Message = create_test_message(lists:flatten(io_lib:format("backpressure_msg_~p", [N]))),
            ok = erlmcp_registry:route_to_server("backpressure_server", "backpressure_transport", Message)
        end, lists:seq(1, NumMessages))
    end),
    
    EndTime = erlang:system_time(microsecond),
    Duration = (EndTime - StartTime) div 1000,
    
    ct:pal("Backpressure engaged ~p times during ~p ms burst", 
           [BackpressureCount, Duration]),
    
    % Verify backpressure was activated
    ?assert(BackpressureCount > 0),
    
    % Verify throughput was throttled appropriately
    ActualThroughput = NumMessages / (Duration / 1000),
    MaxExpectedThroughput = 500, % msgs/sec given 50ms processing delay
    ?assert(ActualThroughput =< MaxExpectedThroughput * 1.2), % Allow 20% variance
    
    ok.

test_dead_letter_queues(Config) ->
    % Test dead letter queue for undeliverable messages
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup server that rejects certain messages
    {ok, ServerPid} = start_mock_server("rejecting_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        reject_pattern => "reject_me_",
        dead_letter_queue => true
    },
    ok = erlmcp_registry:register_server("rejecting_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("dlq_transport"),
    TransportConfig = #{type => stdio, server_id => "rejecting_server"},
    ok = erlmcp_registry:register_transport("dlq_transport", TransportPid, TransportConfig),
    
    % Send mix of acceptable and rejectable messages
    AcceptableMessages = [create_test_message("accept_me_1"), create_test_message("accept_me_2")],
    RejectableMessages = [create_test_message("reject_me_1"), create_test_message("reject_me_2")],
    
    % Send all messages
    lists:foreach(fun(Message) ->
        ok = erlmcp_registry:route_to_server("rejecting_server", "dlq_transport", Message)
    end, AcceptableMessages ++ RejectableMessages),
    
    % Wait for processing
    timer:sleep(1000),
    
    % Check dead letter queue
    DeadLetters = get_dead_letter_queue("rejecting_server"),
    AcceptedMessages = get_accepted_messages("rejecting_server"),
    
    ct:pal("Dead letters: ~p", [DeadLetters]),
    ct:pal("Accepted messages: ~p", [AcceptedMessages]),
    
    % Verify correct routing
    ?assertEqual(length(RejectableMessages), length(DeadLetters)),
    ?assertEqual(length(AcceptableMessages), length(AcceptedMessages)),
    
    % Verify dead letter content
    lists:foreach(fun(DeadMessage) ->
        MessageContent = extract_message_content(DeadMessage),
        ?assert(string:str(MessageContent, "reject_me_") > 0)
    end, DeadLetters),
    
    ok.

%%====================================================================
%% Monitoring & Metrics Tests  
%%====================================================================

test_message_count_tracking(Config) ->
    % Test accurate message count tracking
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup components with metrics
    {ok, ServerPid} = start_mock_server("metrics_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        enable_metrics => true
    },
    ok = erlmcp_registry:register_server("metrics_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("metrics_transport"),
    TransportConfig = #{type => stdio, server_id => "metrics_server"},
    ok = erlmcp_registry:register_transport("metrics_transport", TransportPid, TransportConfig),
    
    % Reset counters
    reset_message_counters(),
    
    % Send known number of messages
    NumMessages = 42,
    lists:foreach(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("count_msg_~p", [N]))),
        ok = erlmcp_registry:route_to_server("metrics_server", "metrics_transport", Message)
    end, lists:seq(1, NumMessages)),
    
    % Wait for all messages to be processed
    timer:sleep(1000),
    
    % Check counters
    Metrics = collect_metrics(),
    ServerReceived = maps:get(server_messages_received, Metrics, 0),
    TransportSent = maps:get(transport_messages_sent, Metrics, 0),
    RegistryRouted = maps:get(registry_messages_routed, Metrics, 0),
    
    ct:pal("Message counters - Server: ~p, Transport: ~p, Registry: ~p", 
           [ServerReceived, TransportSent, RegistryRouted]),
    
    % Verify counts match
    ?assertEqual(NumMessages, ServerReceived),
    ?assertEqual(NumMessages, TransportSent),
    ?assertEqual(NumMessages, RegistryRouted),
    
    ok.

test_latency_histograms(Config) ->
    % Test latency histogram collection
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup components with latency tracking
    {ok, ServerPid} = start_mock_server("latency_hist_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        track_latency => true
    },
    ok = erlmcp_registry:register_server("latency_hist_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("latency_hist_transport"),
    TransportConfig = #{type => stdio, server_id => "latency_hist_server"},
    ok = erlmcp_registry:register_transport("latency_hist_transport", TransportPid, TransportConfig),
    
    % Send messages with variable processing times
    NumMessages = 200,
    lists:foreach(fun(N) ->
        ProcessingDelay = (N rem 10) * 10, % 0-90ms variable delay
        Message = create_test_message(#{
            payload => lists:flatten(io_lib:format("latency_msg_~p", [N])),
            processing_delay => ProcessingDelay
        }),
        ok = erlmcp_registry:route_to_server("latency_hist_server", "latency_hist_transport", Message)
    end, lists:seq(1, NumMessages)),
    
    % Wait for processing
    timer:sleep(5000),
    
    % Get latency histogram
    Histogram = get_latency_histogram("latency_hist_server"),
    
    ct:pal("Latency histogram: ~p", [Histogram]),
    
    % Verify histogram structure
    ?assertMatch(#{
        count := Count,
        buckets := Buckets,
        percentiles := Percentiles
    } when Count >= NumMessages, Histogram),
    
    % Verify reasonable distribution
    Buckets = maps:get(buckets, Histogram),
    ?assert(maps:size(Buckets) > 5), % Multiple buckets used
    
    Percentiles = maps:get(percentiles, Histogram),
    P50 = maps:get(p50, Percentiles),
    P95 = maps:get(p95, Percentiles),
    P99 = maps:get(p99, Percentiles),
    
    % Basic sanity checks
    ?assert(P50 > 0),
    ?assert(P95 >= P50),
    ?assert(P99 >= P95),
    
    ok.

test_error_rate_monitoring(Config) ->
    % Test error rate tracking and alerting
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup error-prone server
    {ok, ServerPid} = start_mock_server("error_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        error_rate => 0.1, % 10% error rate
        track_errors => true
    },
    ok = erlmcp_registry:register_server("error_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("error_transport"),
    TransportConfig = #{type => stdio, server_id => "error_server"},
    ok = erlmcp_registry:register_transport("error_transport", TransportPid, TransportConfig),
    
    % Reset error counters
    reset_error_counters(),
    
    % Send batch of messages
    NumMessages = 100,
    lists:foreach(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("error_msg_~p", [N]))),
        ok = erlmcp_registry:route_to_server("error_server", "error_transport", Message)
    end, lists:seq(1, NumMessages)),
    
    % Wait for processing
    timer:sleep(2000),
    
    % Check error metrics
    ErrorMetrics = get_error_metrics("error_server"),
    TotalMessages = maps:get(total_messages, ErrorMetrics),
    ErrorCount = maps:get(error_count, ErrorMetrics),
    ErrorRate = maps:get(error_rate, ErrorMetrics),
    
    ct:pal("Error metrics - Total: ~p, Errors: ~p, Rate: ~p", 
           [TotalMessages, ErrorCount, ErrorRate]),
    
    % Verify error tracking
    ?assertEqual(NumMessages, TotalMessages),
    ?assert(ErrorCount > 5), % Should have some errors
    ?assert(ErrorCount < 20), % But not too many
    
    ExpectedRate = ErrorCount / TotalMessages,
    ?assert(abs(ErrorRate - ExpectedRate) < 0.01), % Within 1%
    
    ok.

test_queue_depth_metrics(Config) ->
    % Test queue depth monitoring
    RegistryPid = ?config(registry_pid, Config),
    
    % Setup slow server to build up queues
    {ok, ServerPid} = start_mock_server("queue_depth_server"),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{},
        processing_delay => 100, % Slow processing
        track_queue_depth => true
    },
    ok = erlmcp_registry:register_server("queue_depth_server", ServerPid, ServerConfig),
    
    {ok, TransportPid} = start_mock_transport("queue_depth_transport"),
    TransportConfig = #{type => stdio, server_id => "queue_depth_server"},
    ok = erlmcp_registry:register_transport("queue_depth_transport", TransportPid, TransportConfig),
    
    % Send burst to build up queue
    NumMessages = 50,
    lists:foreach(fun(N) ->
        Message = create_test_message(lists:flatten(io_lib:format("queue_msg_~p", [N]))),
        ok = erlmcp_registry:route_to_server("queue_depth_server", "queue_depth_transport", Message)
    end, lists:seq(1, NumMessages)),
    
    % Monitor queue depth over time
    QueueDepths = monitor_queue_depth("queue_depth_server", 10, 200), % 10 samples, 200ms apart
    
    ct:pal("Queue depth samples: ~p", [QueueDepths]),
    
    % Analyze queue behavior
    MaxDepth = lists:max(QueueDepths),
    MinDepth = lists:min(QueueDepths),
    AvgDepth = lists:sum(QueueDepths) / length(QueueDepths),
    
    ct:pal("Queue depth - Max: ~p, Min: ~p, Avg: ~p", [MaxDepth, MinDepth, AvgDepth]),
    
    % Verify queue built up initially then drained
    ?assert(MaxDepth > 10), % Queue should build up
    ?assert(MinDepth =:= 0), % Should eventually drain
    ?assert(AvgDepth > 0),   % Average should be positive
    
    % Verify queue draining trend (last samples should be lower)
    EarlyAvg = lists:sum(lists:sublist(QueueDepths, 5)) / 5,
    LateAvg = lists:sum(lists:nthtail(5, QueueDepths)) / 5,
    ?assert(LateAvg < EarlyAvg), % Should be draining
    
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

start_mock_server(ServerId) ->
    ServerPid = spawn_link(fun() ->
        mock_server_loop(ServerId, #{})
    end),
    {ok, ServerPid}.

mock_server_loop(ServerId, State) ->
    receive
        {mcp_message, TransportId, Message} ->
            % Simulate processing
            ProcessingDelay = maps:get(processing_delay, State, 0),
            case ProcessingDelay of
                0 -> ok;
                Delay -> timer:sleep(Delay)
            end,
            
            % Send confirmation
            self() ! {server_received, ServerId, TransportId, Message},
            
            % Auto-respond for bidirectional tests
            case extract_message_content(Message) of
                "ping" ->
                    ResponseMessage = create_test_message("pong"),
                    erlmcp_registry:route_to_transport(TransportId, ServerId, ResponseMessage);
                _ ->
                    ok
            end,
            
            mock_server_loop(ServerId, State);
            
        {config, NewConfig} ->
            mock_server_loop(ServerId, maps:merge(State, NewConfig));
            
        stop ->
            ok
    end.

start_mock_transport(TransportId) ->
    TransportPid = spawn_link(fun() ->
        mock_transport_loop(TransportId, #{})
    end),
    {ok, TransportPid}.

mock_transport_loop(TransportId, State) ->
    receive
        {mcp_response, ServerId, Message} ->
            % Send confirmation
            self() ! {transport_received, TransportId, ServerId, Message},
            mock_transport_loop(TransportId, State);
            
        {config, NewConfig} ->
            mock_transport_loop(TransportId, maps:merge(State, NewConfig));
            
        stop ->
            ok
    end.

create_test_message(Content) ->
    #{
        id => erlang:unique_integer([positive]),
        timestamp => erlang:system_time(microsecond),
        content => Content
    }.

extract_message_content(Message) when is_map(Message) ->
    maps:get(content, Message, "");
extract_message_content(Message) ->
    Message.

measure_latency(Fun) ->
    StartTime = erlang:system_time(microsecond),
    Fun(),
    EndTime = erlang:system_time(microsecond),
    EndTime - StartTime.

collect_metrics() ->
    #{
        server_messages_received => erlmcp_metrics:get_counter(server_messages_received),
        transport_messages_sent => erlmcp_metrics:get_counter(transport_messages_sent),
        registry_messages_routed => erlmcp_metrics:get_counter(registry_messages_routed)
    }.

setup_load_balancer(Type) ->
    #{type => Type, state => #{}}.

select_server_round_robin(ServerPids, N) ->
    Index = (N - 1) rem length(ServerPids) + 1,
    {ServerId, _} = lists:nth(Index, ServerPids),
    ServerId.

select_server_weighted(ServerPids, N) ->
    TotalWeight = lists:sum([W || {_, _, W} <- ServerPids]),
    Target = (N rem TotalWeight) + 1,
    select_by_weight(ServerPids, Target, 0).

select_by_weight([{ServerId, _, Weight} | _], Target, Acc) when Acc + Weight >= Target ->
    ServerId;
select_by_weight([{_, _, Weight} | Rest], Target, Acc) ->
    select_by_weight(Rest, Target, Acc + Weight).

select_server_adaptive(ServerPids) ->
    % Simple implementation - select least loaded server
    LoadedServers = [{get_server_load(ServerId), ServerId} || {ServerId, _} <- ServerPids],
    SortedByLoad = lists:sort(LoadedServers),
    {_, ServerId} = hd(SortedByLoad),
    ServerId.

select_random_server(ServerPids) ->
    {ServerId, _} = lists:nth(rand:uniform(length(ServerPids)), ServerPids),
    ServerId.

select_random_transport(TransportPids) ->
    {TransportId, _} = lists:nth(rand:uniform(length(TransportPids)), TransportPids),
    TransportId.

receive_multiple_messages(ExpectedCount, Timeout) ->
    receive_multiple_messages(ExpectedCount, Timeout, 0).

receive_multiple_messages(ExpectedCount, Timeout, ReceivedCount) when ReceivedCount >= ExpectedCount ->
    ReceivedCount;
receive_multiple_messages(ExpectedCount, Timeout, ReceivedCount) ->
    receive
        {server_received, _, _, _} ->
            receive_multiple_messages(ExpectedCount, Timeout, ReceivedCount + 1)
    after Timeout ->
        ReceivedCount
    end.

wait_for_messages(ExpectedCount, Timeout) ->
    wait_for_messages(ExpectedCount, Timeout, 0, erlang:system_time(millisecond)).

wait_for_messages(ExpectedCount, Timeout, ReceivedCount, StartTime) when ReceivedCount >= ExpectedCount ->
    ReceivedCount;
wait_for_messages(ExpectedCount, Timeout, ReceivedCount, StartTime) ->
    Now = erlang:system_time(millisecond),
    case Now - StartTime > Timeout of
        true -> ReceivedCount;
        false ->
            receive
                {server_received, _, _, _} ->
                    wait_for_messages(ExpectedCount, Timeout, ReceivedCount + 1, StartTime)
            after 100 ->
                wait_for_messages(ExpectedCount, Timeout, ReceivedCount, StartTime)
            end
    end.

collect_ordered_messages(ExpectedCount, Timeout) ->
    collect_ordered_messages(ExpectedCount, Timeout, []).

collect_ordered_messages(0, _, Acc) ->
    lists:reverse(Acc);
collect_ordered_messages(ExpectedCount, Timeout, Acc) ->
    receive
        {server_received, _, _, Message} ->
            collect_ordered_messages(ExpectedCount - 1, Timeout, [Message | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

% Placeholder implementations for advanced features
count_messages_for_server(_ServerId) -> 0.
get_circuit_breaker_state(_ServerId) -> closed.
simulate_server_failures(_ServerId, _Count) -> ok.
simulate_high_load(_ServerId, _Load) -> ok.
get_server_load(_ServerId) -> 0.
reset_message_counters() -> ok.
monitor_backpressure(_Fun) -> 0.
get_dead_letter_queue(_ServerId) -> [].
get_accepted_messages(_ServerId) -> [].
get_latency_histogram(_ServerId) -> #{count => 0, buckets => #{}, percentiles => #{}}.
reset_error_counters() -> ok.
get_error_metrics(_ServerId) -> #{total_messages => 0, error_count => 0, error_rate => 0.0}.
monitor_queue_depth(_ServerId, _Samples, _Interval) -> [0].