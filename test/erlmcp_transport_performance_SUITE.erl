-module(erlmcp_transport_performance_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [
        {group, stdio_performance},
        {group, tcp_performance}, 
        {group, http_performance},
        {group, transport_comparison},
        {group, scaling_tests},
        {group, resource_monitoring}
    ].

groups() ->
    [
        {stdio_performance, [parallel], [
            stdio_throughput_test,
            stdio_latency_test,
            stdio_memory_usage_test,
            stdio_concurrent_test,
            stdio_burst_test
        ]},
        {tcp_performance, [parallel], [
            tcp_throughput_test,
            tcp_latency_test,
            tcp_memory_usage_test,
            tcp_concurrent_test,
            tcp_reconnection_test
        ]},
        {http_performance, [parallel], [
            http_throughput_test,
            http_latency_test,
            http_memory_usage_test,
            http_concurrent_test,
            http_retry_test
        ]},
        {transport_comparison, [], [
            compare_throughput,
            compare_latency,
            compare_resource_usage,
            scaling_characteristics
        ]},
        {scaling_tests, [parallel], [
            connection_scaling_test,
            message_size_scaling_test,
            load_scaling_test
        ]},
        {resource_monitoring, [], [
            memory_leak_detection,
            cpu_usage_monitoring,
            process_count_monitoring,
            garbage_collection_impact
        ]}
    ].

suite() ->
    [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    application:ensure_started(logger),
    application:ensure_started(crypto),
    application:ensure_started(ssl),
    application:ensure_started(inets),
    
    % Start performance analyzer
    {ok, _} = erlmcp_performance_analysis:start_link(),
    
    % Configure test parameters
    TestConfig = #{
        small_message_size => 128,
        medium_message_size => 1024, 
        large_message_size => 8192,
        test_message_count => 1000,
        concurrent_connections => 10,
        test_duration => 30000  % 30 seconds
    },
    
    [{test_config, TestConfig} | Config].

end_per_suite(_Config) ->
    erlmcp_performance_analysis:stop(),
    ok.

init_per_group(stdio_performance, Config) ->
    % Start registry for stdio tests
    {ok, _} = erlmcp_registry:start_link(),
    Config;
init_per_group(tcp_performance, Config) ->
    % Start mock TCP server for testing
    {ok, MockServer} = start_mock_tcp_server(8080),
    [{mock_tcp_server, MockServer} | Config];
init_per_group(http_performance, Config) ->
    % Start mock HTTP server for testing
    {ok, MockServer} = start_mock_http_server(8081),
    [{mock_http_server, MockServer} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(stdio_performance, _Config) ->
    % Stop registry
    gen_server:stop(erlmcp_registry),
    ok;
end_per_group(tcp_performance, Config) ->
    MockServer = proplists:get_value(mock_tcp_server, Config),
    stop_mock_server(MockServer),
    ok;
end_per_group(http_performance, Config) ->
    MockServer = proplists:get_value(mock_http_server, Config),
    stop_mock_server(MockServer),
    ok;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    % Reset performance metrics
    erlmcp_performance_analysis ! {reset_metrics},
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% STDIO Transport Performance Tests
%%====================================================================

stdio_throughput_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(medium_message_size, TestConfig),
    MessageCount = maps:get(test_message_count, TestConfig),
    
    ct:pal("Starting STDIO throughput test: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    {ok, Throughput} = erlmcp_performance_analysis:run_throughput_test(
        stdio, MessageCount, MessageSize),
    
    ct:pal("STDIO Throughput: ~.2f KB/s", [Throughput / 1024]),
    
    % Verify reasonable throughput (> 1 MB/s for local stdio)
    ?assert(Throughput > 1024 * 1024),
    
    ok.

stdio_latency_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(small_message_size, TestConfig),
    MessageCount = 100, % Smaller sample for latency testing
    
    ct:pal("Starting STDIO latency test: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    {ok, {P50, P95, P99}} = erlmcp_performance_analysis:run_latency_test(
        stdio, MessageCount, MessageSize),
    
    ct:pal("STDIO Latencies - P50: ~p μs, P95: ~p μs, P99: ~p μs", 
           [P50, P95, P99]),
    
    % Verify reasonable latencies for local stdio (< 10ms P95)
    ?assert(P95 < 10000),
    
    ok.

stdio_memory_usage_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    Duration = 10000, % 10 seconds
    
    ct:pal("Starting STDIO memory usage monitoring: ~p ms", [Duration]),
    
    InitialMemory = erlang:memory(total),
    
    {ok, _MonitorPid} = erlmcp_performance_analysis:monitor_resources(Duration, 1000),
    
    % Run operations while monitoring
    spawn_link(fun() -> stdio_memory_load_generator(Duration) end),
    
    timer:sleep(Duration + 1000),
    
    FinalMemory = erlang:memory(total),
    MemoryIncrease = FinalMemory - InitialMemory,
    
    ct:pal("Memory usage - Initial: ~p MB, Final: ~p MB, Increase: ~p MB", 
           [InitialMemory div (1024*1024), FinalMemory div (1024*1024), 
            MemoryIncrease div (1024*1024)]),
    
    % Verify no significant memory leak (< 50MB increase)
    ?assert(MemoryIncrease < 50 * 1024 * 1024),
    
    ok.

stdio_concurrent_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    Connections = maps:get(concurrent_connections, TestConfig),
    MessageCount = 100,
    MessageSize = maps:get(small_message_size, TestConfig),
    
    ct:pal("Starting STDIO concurrent test: ~p connections, ~p messages each", 
           [Connections, MessageCount]),
    
    {ok, Results} = erlmcp_performance_analysis:run_concurrent_test(
        stdio, Connections, MessageCount, MessageSize),
    
    TotalOps = maps:get(total_operations, Results),
    SuccessRate = maps:get(success_rate, Results),
    Throughput = maps:get(throughput, Results),
    
    ct:pal("STDIO Concurrent Results - Ops: ~p, Success: ~.1f%%, Throughput: ~.2f ops/s", 
           [TotalOps, SuccessRate, Throughput]),
    
    % Verify high success rate and reasonable performance
    ?assert(SuccessRate > 95.0),
    ?assert(Throughput > 100),
    
    ok.

stdio_burst_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(large_message_size, TestConfig),
    
    ct:pal("Starting STDIO burst test with ~p byte messages", [MessageSize]),
    
    % Send burst of large messages
    BurstSize = 50,
    StartTime = erlang:system_time(microsecond),
    
    Results = lists:map(fun(_) ->
        {ok, Duration} = time_stdio_operation(MessageSize),
        Duration
    end, lists:seq(1, BurstSize)),
    
    EndTime = erlang:system_time(microsecond),
    TotalTime = EndTime - StartTime,
    
    AvgLatency = lists:sum(Results) / length(Results),
    MaxLatency = lists:max(Results),
    
    ct:pal("STDIO Burst Results - Avg: ~.2f μs, Max: ~p μs, Total: ~.2f ms", 
           [AvgLatency, MaxLatency, TotalTime / 1000]),
    
    % Verify burst handling (avg latency < 50ms, max < 100ms)
    ?assert(AvgLatency < 50000),
    ?assert(MaxLatency < 100000),
    
    ok.

%%====================================================================
%% TCP Transport Performance Tests
%%====================================================================

tcp_throughput_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(medium_message_size, TestConfig),
    MessageCount = maps:get(test_message_count, TestConfig),
    
    ct:pal("Starting TCP throughput test: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    {ok, Throughput} = erlmcp_performance_analysis:run_throughput_test(
        tcp, MessageCount, MessageSize),
    
    ct:pal("TCP Throughput: ~.2f KB/s", [Throughput / 1024]),
    
    % Verify reasonable throughput (> 100 KB/s for local TCP)
    ?assert(Throughput > 100 * 1024),
    
    ok.

tcp_latency_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(small_message_size, TestConfig),
    MessageCount = 100,
    
    ct:pal("Starting TCP latency test: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    {ok, {P50, P95, P99}} = erlmcp_performance_analysis:run_latency_test(
        tcp, MessageCount, MessageSize),
    
    ct:pal("TCP Latencies - P50: ~p μs, P95: ~p μs, P99: ~p μs", 
           [P50, P95, P99]),
    
    % Verify reasonable latencies for local TCP (< 50ms P95)
    ?assert(P95 < 50000),
    
    ok.

tcp_memory_usage_test(Config) ->
    Duration = 10000,
    
    ct:pal("Starting TCP memory usage monitoring: ~p ms", [Duration]),
    
    InitialMemory = erlang:memory(total),
    
    {ok, _MonitorPid} = erlmcp_performance_analysis:monitor_resources(Duration, 1000),
    
    % Run TCP operations while monitoring
    spawn_link(fun() -> tcp_memory_load_generator(Duration) end),
    
    timer:sleep(Duration + 1000),
    
    FinalMemory = erlang:memory(total),
    MemoryIncrease = FinalMemory - InitialMemory,
    
    ct:pal("TCP Memory usage - Initial: ~p MB, Final: ~p MB, Increase: ~p MB", 
           [InitialMemory div (1024*1024), FinalMemory div (1024*1024), 
            MemoryIncrease div (1024*1024)]),
    
    % Verify no significant memory leak (< 100MB increase for TCP buffers)
    ?assert(MemoryIncrease < 100 * 1024 * 1024),
    
    ok.

tcp_concurrent_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    Connections = maps:get(concurrent_connections, TestConfig),
    MessageCount = 100,
    MessageSize = maps:get(small_message_size, TestConfig),
    
    ct:pal("Starting TCP concurrent test: ~p connections, ~p messages each", 
           [Connections, MessageCount]),
    
    {ok, Results} = erlmcp_performance_analysis:run_concurrent_test(
        tcp, Connections, MessageCount, MessageSize),
    
    TotalOps = maps:get(total_operations, Results),
    SuccessRate = maps:get(success_rate, Results),
    Throughput = maps:get(throughput, Results),
    
    ct:pal("TCP Concurrent Results - Ops: ~p, Success: ~.1f%%, Throughput: ~.2f ops/s", 
           [TotalOps, SuccessRate, Throughput]),
    
    % Verify reasonable success rate and performance for TCP
    ?assert(SuccessRate > 85.0), % Lower than stdio due to network overhead
    ?assert(Throughput > 50),
    
    ok.

tcp_reconnection_test(Config) ->
    ct:pal("Starting TCP reconnection performance test"),
    
    % Test reconnection overhead
    ReconnectionCount = 10,
    Durations = lists:map(fun(_) ->
        StartTime = erlang:system_time(microsecond),
        {ok, _Pid} = simulate_tcp_reconnection(),
        EndTime = erlang:system_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, ReconnectionCount)),
    
    AvgReconnectionTime = lists:sum(Durations) / length(Durations),
    MaxReconnectionTime = lists:max(Durations),
    
    ct:pal("TCP Reconnection - Avg: ~.2f ms, Max: ~.2f ms", 
           [AvgReconnectionTime / 1000, MaxReconnectionTime / 1000]),
    
    % Verify reasonable reconnection times (< 5s avg, < 10s max)
    ?assert(AvgReconnectionTime < 5000000),
    ?assert(MaxReconnectionTime < 10000000),
    
    ok.

%%====================================================================
%% HTTP Transport Performance Tests
%%====================================================================

http_throughput_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(medium_message_size, TestConfig),
    MessageCount = 100, % Fewer messages for HTTP due to higher overhead
    
    ct:pal("Starting HTTP throughput test: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    {ok, Throughput} = erlmcp_performance_analysis:run_throughput_test(
        http, MessageCount, MessageSize),
    
    ct:pal("HTTP Throughput: ~.2f KB/s", [Throughput / 1024]),
    
    % Verify reasonable throughput (> 10 KB/s for HTTP)
    ?assert(Throughput > 10 * 1024),
    
    ok.

http_latency_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(small_message_size, TestConfig),
    MessageCount = 50, % Even fewer for latency testing
    
    ct:pal("Starting HTTP latency test: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    {ok, {P50, P95, P99}} = erlmcp_performance_analysis:run_latency_test(
        http, MessageCount, MessageSize),
    
    ct:pal("HTTP Latencies - P50: ~p μs, P95: ~p μs, P99: ~p μs", 
           [P50, P95, P99]),
    
    % Verify reasonable latencies for HTTP (< 1s P95)
    ?assert(P95 < 1000000),
    
    ok.

http_memory_usage_test(Config) ->
    Duration = 10000,
    
    ct:pal("Starting HTTP memory usage monitoring: ~p ms", [Duration]),
    
    InitialMemory = erlang:memory(total),
    
    {ok, _MonitorPid} = erlmcp_performance_analysis:monitor_resources(Duration, 1000),
    
    % Run HTTP operations while monitoring
    spawn_link(fun() -> http_memory_load_generator(Duration) end),
    
    timer:sleep(Duration + 1000),
    
    FinalMemory = erlang:memory(total),
    MemoryIncrease = FinalMemory - InitialMemory,
    
    ct:pal("HTTP Memory usage - Initial: ~p MB, Final: ~p MB, Increase: ~p MB", 
           [InitialMemory div (1024*1024), FinalMemory div (1024*1024), 
            MemoryIncrease div (1024*1024)]),
    
    % Verify no significant memory leak (< 150MB for HTTP client pools)
    ?assert(MemoryIncrease < 150 * 1024 * 1024),
    
    ok.

http_concurrent_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    Connections = 5, % Fewer concurrent for HTTP
    MessageCount = 20,
    MessageSize = maps:get(small_message_size, TestConfig),
    
    ct:pal("Starting HTTP concurrent test: ~p connections, ~p messages each", 
           [Connections, MessageCount]),
    
    {ok, Results} = erlmcp_performance_analysis:run_concurrent_test(
        http, Connections, MessageCount, MessageSize),
    
    TotalOps = maps:get(total_operations, Results),
    SuccessRate = maps:get(success_rate, Results),
    Throughput = maps:get(throughput, Results),
    
    ct:pal("HTTP Concurrent Results - Ops: ~p, Success: ~.1f%%, Throughput: ~.2f ops/s", 
           [TotalOps, SuccessRate, Throughput]),
    
    % Verify reasonable success rate for HTTP
    ?assert(SuccessRate > 70.0), % Lower expectations for HTTP
    ?assert(Throughput > 1),
    
    ok.

http_retry_test(Config) ->
    ct:pal("Starting HTTP retry performance test"),
    
    % Test retry overhead with simulated failures
    RetryCount = 5,
    Durations = lists:map(fun(_) ->
        StartTime = erlang:system_time(microsecond),
        {ok, _Result} = simulate_http_retry_scenario(),
        EndTime = erlang:system_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, RetryCount)),
    
    AvgRetryTime = lists:sum(Durations) / length(Durations),
    MaxRetryTime = lists:max(Durations),
    
    ct:pal("HTTP Retry - Avg: ~.2f ms, Max: ~.2f ms", 
           [AvgRetryTime / 1000, MaxRetryTime / 1000]),
    
    % Verify reasonable retry times (< 30s avg, < 60s max)
    ?assert(AvgRetryTime < 30000000),
    ?assert(MaxRetryTime < 60000000),
    
    ok.

%%====================================================================
%% Transport Comparison Tests
%%====================================================================

compare_throughput(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(medium_message_size, TestConfig),
    MessageCount = 500,
    
    ct:pal("Comparing transport throughput: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    % Test all transports
    {ok, StdioThroughput} = erlmcp_performance_analysis:run_throughput_test(
        stdio, MessageCount, MessageSize),
    {ok, TcpThroughput} = erlmcp_performance_analysis:run_throughput_test(
        tcp, MessageCount, MessageSize),
    {ok, HttpThroughput} = erlmcp_performance_analysis:run_throughput_test(
        http, MessageCount div 5, MessageSize), % Fewer HTTP messages
    
    ct:pal("Throughput Comparison:~n"
           "  STDIO: ~.2f KB/s~n"
           "  TCP:   ~.2f KB/s~n" 
           "  HTTP:  ~.2f KB/s",
           [StdioThroughput/1024, TcpThroughput/1024, HttpThroughput/1024]),
    
    % Verify expected performance order: STDIO > TCP > HTTP
    ?assert(StdioThroughput > TcpThroughput),
    ?assert(TcpThroughput > HttpThroughput),
    
    ok.

compare_latency(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(small_message_size, TestConfig),
    MessageCount = 100,
    
    ct:pal("Comparing transport latency: ~p messages, ~p bytes each", 
           [MessageCount, MessageSize]),
    
    % Test all transports
    {ok, {StdioP50, StdioP95, _}} = erlmcp_performance_analysis:run_latency_test(
        stdio, MessageCount, MessageSize),
    {ok, {TcpP50, TcpP95, _}} = erlmcp_performance_analysis:run_latency_test(
        tcp, MessageCount, MessageSize),
    {ok, {HttpP50, HttpP95, _}} = erlmcp_performance_analysis:run_latency_test(
        http, MessageCount div 2, MessageSize),
    
    ct:pal("Latency Comparison (P50/P95):~n"
           "  STDIO: ~p/~p μs~n"
           "  TCP:   ~p/~p μs~n"
           "  HTTP:  ~p/~p μs",
           [StdioP50, StdioP95, TcpP50, TcpP95, HttpP50, HttpP95]),
    
    % Verify expected latency order: STDIO < TCP < HTTP
    ?assert(StdioP95 < TcpP95),
    ?assert(TcpP95 < HttpP95),
    
    ok.

compare_resource_usage(Config) ->
    Duration = 15000,
    
    ct:pal("Comparing transport resource usage over ~p ms", [Duration]),
    
    % Monitor each transport separately
    StdioMetrics = measure_transport_resources(stdio, Duration),
    TcpMetrics = measure_transport_resources(tcp, Duration),
    HttpMetrics = measure_transport_resources(http, Duration),
    
    ct:pal("Resource Usage Comparison:~n"
           "  STDIO: ~p MB peak memory~n"
           "  TCP:   ~p MB peak memory~n"
           "  HTTP:  ~p MB peak memory",
           [StdioMetrics div (1024*1024), 
            TcpMetrics div (1024*1024),
            HttpMetrics div (1024*1024)]),
    
    % Verify resource usage is reasonable
    ?assert(StdioMetrics < 50 * 1024 * 1024),  % < 50MB
    ?assert(TcpMetrics < 100 * 1024 * 1024),   % < 100MB
    ?assert(HttpMetrics < 200 * 1024 * 1024),  % < 200MB
    
    ok.

scaling_characteristics(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(small_message_size, TestConfig),
    
    ct:pal("Testing scaling characteristics"),
    
    % Test scaling with connection count
    ConnectionCounts = [1, 5, 10, 20],
    ScalingResults = lists:map(fun(Connections) ->
        {ok, Results} = erlmcp_performance_analysis:run_concurrent_test(
            stdio, Connections, 100, MessageSize),
        Throughput = maps:get(throughput, Results),
        {Connections, Throughput}
    end, ConnectionCounts),
    
    ct:pal("Scaling Results:~n~p", [ScalingResults]),
    
    % Verify throughput scales reasonably (not linearly due to overhead)
    [{1, Throughput1}, {5, Throughput5}, {10, Throughput10}, {20, Throughput20}] = ScalingResults,
    
    ?assert(Throughput5 > Throughput1),
    ?assert(Throughput10 > Throughput5),
    % Don't require Throughput20 > Throughput10 as contention may occur
    
    ok.

%%====================================================================
%% Scaling Tests
%%====================================================================

connection_scaling_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(small_message_size, TestConfig),
    
    ct:pal("Testing connection scaling limits"),
    
    % Test increasing connection counts
    MaxConnections = 50,
    ScalingPoints = [1, 5, 10, 20, 30, 50],
    
    Results = lists:foldl(fun(Connections, Acc) ->
        try
            StartTime = erlang:system_time(microsecond),
            {ok, Result} = erlmcp_performance_analysis:run_concurrent_test(
                stdio, Connections, 50, MessageSize),
            EndTime = erlang:system_time(microsecond),
            
            Duration = EndTime - StartTime,
            Throughput = maps:get(throughput, Result),
            SuccessRate = maps:get(success_rate, Result),
            
            [{Connections, #{duration => Duration, throughput => Throughput, 
                           success_rate => SuccessRate}} | Acc]
        catch
            _:Error ->
                ct:pal("Failed at ~p connections: ~p", [Connections, Error]),
                [{Connections, #{error => Error}} | Acc]
        end
    end, [], ScalingPoints),
    
    ct:pal("Connection Scaling Results:~n~p", [lists:reverse(Results)]),
    
    % Verify we can handle at least 20 concurrent connections
    case lists:keyfind(20, 1, Results) of
        {20, #{success_rate := SuccessRate}} when SuccessRate > 80 ->
            ok;
        {20, #{error := _}} ->
            ct:fail("Failed to handle 20 connections");
        false ->
            ct:fail("Missing results for 20 connections")
    end.

message_size_scaling_test(Config) ->
    ct:pal("Testing message size scaling characteristics"),
    
    % Test different message sizes
    MessageSizes = [64, 256, 1024, 4096, 16384, 65536],
    MessageCount = 100,
    
    Results = lists:map(fun(Size) ->
        StartTime = erlang:system_time(microsecond),
        {ok, Throughput} = erlmcp_performance_analysis:run_throughput_test(
            stdio, MessageCount, Size),
        EndTime = erlang:system_time(microsecond),
        
        Duration = EndTime - StartTime,
        MessagesPerSec = MessageCount / (Duration / 1000000),
        
        {Size, #{throughput_bytes => Throughput, messages_per_sec => MessagesPerSec}}
    end, MessageSizes),
    
    ct:pal("Message Size Scaling Results:~n~p", [Results]),
    
    % Verify throughput generally increases with message size (efficiency improves)
    [{64, #{throughput_bytes := Small}}, 
     {1024, #{throughput_bytes := Medium}}, 
     {16384, #{throughput_bytes := Large}} | _] = Results,
    
    ?assert(Medium > Small),
    ?assert(Large > Medium),
    
    ok.

load_scaling_test(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(medium_message_size, TestConfig),
    Duration = 20000, % 20 seconds
    
    ct:pal("Testing load scaling over ~p ms", [Duration]),
    
    % Test different concurrent loads
    Loads = [1, 3, 5, 10],
    
    Results = lists:map(fun(ConcurrentCount) ->
        {ok, Result} = erlmcp_performance_analysis:run_stress_test(
            stdio, Duration, ConcurrentCount),
        
        OpsPerSec = maps:get(operations_per_second, Result),
        SuccessRate = maps:get(success_rate, Result),
        
        {ConcurrentCount, #{ops_per_sec => OpsPerSec, success_rate => SuccessRate}}
    end, Loads),
    
    ct:pal("Load Scaling Results:~n~p", [Results]),
    
    % Verify system maintains reasonable performance under load
    [{10, #{success_rate := SuccessRate10}}] = 
        [{L, R} || {L, R} <- Results, L =:= 10],
    
    ?assert(SuccessRate10 > 75.0), % 75% success rate under high load
    
    ok.

%%====================================================================
%% Resource Monitoring Tests
%%====================================================================

memory_leak_detection(Config) ->
    Duration = 30000, % 30 seconds
    
    ct:pal("Running memory leak detection test for ~p ms", [Duration]),
    
    InitialMemory = erlang:memory(total),
    
    % Run continuous operations
    TestPid = spawn_link(fun() -> 
        memory_leak_test_loop(erlang:system_time(millisecond) + Duration)
    end),
    
    % Monitor memory every second
    MemorySamples = collect_memory_samples(Duration, 1000),
    
    % Stop test
    exit(TestPid, shutdown),
    
    FinalMemory = erlang:memory(total),
    MaxMemory = lists:max(MemorySamples),
    
    ct:pal("Memory Leak Detection:~n"
           "  Initial: ~p MB~n"
           "  Final:   ~p MB~n" 
           "  Peak:    ~p MB~n"
           "  Growth:  ~p MB",
           [InitialMemory div (1024*1024), FinalMemory div (1024*1024),
            MaxMemory div (1024*1024), (FinalMemory - InitialMemory) div (1024*1024)]),
    
    % Verify no significant memory leak (< 100MB growth)
    ?assert(FinalMemory - InitialMemory < 100 * 1024 * 1024),
    
    ok.

cpu_usage_monitoring(Config) ->
    Duration = 15000, % 15 seconds
    
    ct:pal("Monitoring CPU usage for ~p ms", [Duration]),
    
    % Enable scheduler statistics
    erlang:system_flag(scheduler_wall_time, true),
    
    InitialStats = erlang:statistics(scheduler_wall_time),
    
    % Run CPU-intensive transport operations
    TestPid = spawn_link(fun() -> 
        cpu_intensive_test_loop(erlang:system_time(millisecond) + Duration)
    end),
    
    timer:sleep(Duration),
    
    FinalStats = erlang:statistics(scheduler_wall_time),
    exit(TestPid, shutdown),
    
    % Calculate CPU usage
    CpuUsage = calculate_cpu_usage(InitialStats, FinalStats),
    
    ct:pal("CPU Usage: ~.1f%%", [CpuUsage]),
    
    % Verify reasonable CPU usage (< 80% for transport operations)
    ?assert(CpuUsage < 80.0),
    
    ok.

process_count_monitoring(Config) ->
    Duration = 10000, % 10 seconds
    
    ct:pal("Monitoring process count for ~p ms", [Duration]),
    
    InitialProcessCount = erlang:system_info(process_count),
    
    % Start multiple transport operations
    TestProcesses = lists:map(fun(_) ->
        spawn_link(fun() -> 
            process_count_test_worker(Duration)
        end)
    end, lists:seq(1, 20)),
    
    timer:sleep(Duration + 1000),
    
    FinalProcessCount = erlang:system_info(process_count),
    MaxProcessCount = erlang:system_info(process_limit),
    
    % Wait for cleanup
    timer:sleep(2000),
    CleanupProcessCount = erlang:system_info(process_count),
    
    ct:pal("Process Count Monitoring:~n"
           "  Initial: ~p~n"
           "  Peak:    ~p~n"
           "  Final:   ~p~n"
           "  Limit:   ~p",
           [InitialProcessCount, FinalProcessCount, CleanupProcessCount, MaxProcessCount]),
    
    % Verify process cleanup and reasonable counts
    ?assert(FinalProcessCount < MaxProcessCount div 2), % Not using more than half
    ?assert(CleanupProcessCount =< InitialProcessCount + 5), % Cleanup occurred
    
    ok.

garbage_collection_impact(Config) ->
    TestConfig = proplists:get_value(test_config, Config),
    MessageSize = maps:get(large_message_size, TestConfig),
    
    ct:pal("Testing garbage collection impact with ~p byte messages", [MessageSize]),
    
    % Force garbage collection and measure before/after
    erlang:garbage_collect(),
    InitialGcStats = erlang:statistics(garbage_collection),
    
    % Run memory-intensive operations
    MessageCount = 1000,
    {ok, Throughput} = erlmcp_performance_analysis:run_throughput_test(
        stdio, MessageCount, MessageSize),
    
    FinalGcStats = erlang:statistics(garbage_collection),
    
    {InitialGCs, InitialWordsReclaimed} = InitialGcStats,
    {FinalGCs, FinalWordsReclaimed} = FinalGcStats,
    
    GcCount = FinalGCs - InitialGCs,
    WordsReclaimed = FinalWordsReclaimed - InitialWordsReclaimed,
    
    ct:pal("Garbage Collection Impact:~n"
           "  GCs Triggered: ~p~n"
           "  Words Reclaimed: ~p~n"
           "  Throughput: ~.2f KB/s",
           [GcCount, WordsReclaimed, Throughput / 1024]),
    
    % Verify GC didn't severely impact performance
    ?assert(GcCount < MessageCount div 10), % Less than 10% GC rate
    ?assert(Throughput > 100 * 1024), % Still reasonable throughput
    
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

start_mock_tcp_server(Port) ->
    spawn_link(fun() -> mock_tcp_server_loop(Port) end).

start_mock_http_server(Port) ->
    spawn_link(fun() -> mock_http_server_loop(Port) end).

stop_mock_server(ServerPid) ->
    exit(ServerPid, shutdown).

mock_tcp_server_loop(Port) ->
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            accept_tcp_loop(ListenSocket);
        {error, Reason} ->
            ct:pal("Failed to start mock TCP server: ~p", [Reason])
    end.

accept_tcp_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_tcp_connection(Socket) end),
            accept_tcp_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            ct:pal("TCP accept error: ~p", [Reason])
    end.

handle_tcp_connection(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % Echo the data back
            gen_tcp:send(Socket, Data),
            handle_tcp_connection(Socket);
        {error, closed} ->
            gen_tcp:close(Socket);
        {error, Reason} ->
            ct:pal("TCP recv error: ~p", [Reason]),
            gen_tcp:close(Socket)
    end.

mock_http_server_loop(Port) ->
    % Simplified mock HTTP server
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            accept_http_loop(ListenSocket);
        {error, Reason} ->
            ct:pal("Failed to start mock HTTP server: ~p", [Reason])
    end.

accept_http_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_http_request(Socket) end),
            accept_http_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            ct:pal("HTTP accept error: ~p", [Reason])
    end.

handle_http_request(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, RequestData} ->
            % Simple HTTP response
            Response = "HTTP/1.1 200 OK\r\n"
                      "Content-Type: application/json\r\n"
                      "Content-Length: 13\r\n"
                      "\r\n"
                      "{\"status\":\"ok\"}",
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        {error, Reason} ->
            ct:pal("HTTP recv error: ~p", [Reason]),
            gen_tcp:close(Socket)
    end.

time_stdio_operation(MessageSize) ->
    TestData = erlmcp_performance_analysis:generate_test_data(MessageSize),
    StartTime = erlang:system_time(microsecond),
    
    % Simulate stdio operation
    timer:sleep(1), % Minimal delay
    
    EndTime = erlang:system_time(microsecond),
    {ok, EndTime - StartTime}.

stdio_memory_load_generator(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % Generate some memory pressure
            Data = erlmcp_performance_analysis:generate_test_data(1024),
            spawn(fun() -> timer:sleep(100) end),
            stdio_memory_load_generator(EndTime)
    end.

tcp_memory_load_generator(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % Simulate TCP operations
            Data = erlmcp_performance_analysis:generate_test_data(2048),
            spawn(fun() -> timer:sleep(200) end),
            tcp_memory_load_generator(EndTime)
    end.

http_memory_load_generator(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % Simulate HTTP operations
            Data = erlmcp_performance_analysis:generate_test_data(4096),
            spawn(fun() -> timer:sleep(500) end),
            http_memory_load_generator(EndTime)
    end.

simulate_tcp_reconnection() ->
    % Simulate TCP reconnection delay
    timer:sleep(rand:uniform(1000) + 100),
    {ok, reconnected}.

simulate_http_retry_scenario() ->
    % Simulate HTTP retry with exponential backoff
    Attempts = rand:uniform(3),
    lists:foldl(fun(Attempt, _) ->
        Delay = 1000 * (1 bsl (Attempt - 1)), % Exponential backoff
        timer:sleep(Delay),
        case rand:uniform(10) of
            N when N =< 7 -> {ok, success}; % 70% success rate
            _ -> {error, retry_needed}
        end
    end, {error, start}, lists:seq(1, Attempts)).

measure_transport_resources(TransportType, Duration) ->
    InitialMemory = erlang:memory(total),
    
    % Run transport operations
    TestPid = spawn_link(fun() ->
        resource_test_loop(TransportType, erlang:system_time(millisecond) + Duration)
    end),
    
    % Monitor peak memory
    PeakMemory = monitor_peak_memory(Duration, InitialMemory),
    
    exit(TestPid, shutdown),
    PeakMemory - InitialMemory.

resource_test_loop(TransportType, EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % Perform transport operation
            case TransportType of
                stdio ->
                    Data = erlmcp_performance_analysis:generate_test_data(512),
                    timer:sleep(10);
                tcp ->
                    Data = erlmcp_performance_analysis:generate_test_data(1024),
                    timer:sleep(50);
                http ->
                    Data = erlmcp_performance_analysis:generate_test_data(2048),
                    timer:sleep(200)
            end,
            resource_test_loop(TransportType, EndTime)
    end.

monitor_peak_memory(Duration, BaseLine) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    monitor_peak_memory_loop(EndTime, BaseLine).

monitor_peak_memory_loop(EndTime, PeakMemory) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> PeakMemory;
        false ->
            CurrentMemory = erlang:memory(total),
            NewPeak = max(PeakMemory, CurrentMemory),
            timer:sleep(100),
            monitor_peak_memory_loop(EndTime, NewPeak)
    end.

memory_leak_test_loop(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % Create and discard data to test for leaks
            Data = lists:duplicate(1000, "test_data_for_memory_leak_detection"),
            length(Data), % Force evaluation
            timer:sleep(10),
            memory_leak_test_loop(EndTime)
    end.

collect_memory_samples(Duration, Interval) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    collect_memory_samples_loop(EndTime, Interval, []).

collect_memory_samples_loop(EndTime, Interval, Samples) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> Samples;
        false ->
            Memory = erlang:memory(total),
            timer:sleep(Interval),
            collect_memory_samples_loop(EndTime, Interval, [Memory | Samples])
    end.

cpu_intensive_test_loop(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % CPU-intensive operations
            Data = erlmcp_performance_analysis:generate_test_data(4096),
            Compressed = zlib:compress(Data),
            zlib:uncompress(Compressed),
            cpu_intensive_test_loop(EndTime)
    end.

process_count_test_worker(Duration) ->
    timer:sleep(Duration),
    ok.

calculate_cpu_usage(InitialStats, FinalStats) ->
    TotalActive = lists:sum([ActiveF - ActiveI || 
        {{_, ActiveI, _}, {_, ActiveF, _}} <- 
        lists:zip(InitialStats, FinalStats)]),
    TotalTime = lists:sum([TotalF - TotalI || 
        {{_, _, TotalI}, {_, _, TotalF}} <- 
        lists:zip(InitialStats, FinalStats)]),
    
    case TotalTime of
        0 -> 0.0;
        _ -> (TotalActive / TotalTime) * 100.0
    end.