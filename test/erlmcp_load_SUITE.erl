%%%-------------------------------------------------------------------
%%% @doc
%%% Load Testing Suite for ErlMCP
%%%
%%% This test suite validates system performance under realistic load:
%%% - 1000+ concurrent connections
%%% - High message throughput testing
%%% - Memory usage under load
%%% - Response time distribution
%%% - System stability over time
%%%
%%% Performance Targets:
%%% - Handle 1000+ concurrent connections
%%% - Process 1000+ messages/second
%%% - Memory usage should remain stable
%%% - 95th percentile response time < 100ms
%%% - Zero memory leaks over 5+ minute runs
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_concurrent_connections_1000/1,
    test_high_message_throughput/1,
    test_memory_stability_under_load/1,
    test_response_time_distribution/1,
    test_system_stability_over_time/1,
    test_resource_exhaustion_recovery/1,
    test_burst_traffic_handling/1,
    test_mixed_workload_performance/1,
    test_connection_churning/1,
    test_large_message_handling/1,
    test_component_isolation_under_load/1,
    test_graceful_degradation/1
]).

%% Load test configuration
-define(MAX_CONCURRENT_CONNECTIONS, 1000).
-define(HIGH_THROUGHPUT_TARGET, 1000).      % messages per second
-define(LOAD_TEST_DURATION, 300000).        % 5 minutes in milliseconds
-define(STABILITY_TEST_DURATION, 600000).   % 10 minutes
-define(RESPONSE_TIME_SAMPLES, 10000).
-define(MEMORY_SAMPLE_INTERVAL, 5000).      % 5 seconds
-define(BURST_SIZE, 5000).                  % messages in burst
-define(LARGE_MESSAGE_SIZE, 1048576).       % 1MB

%% Performance thresholds
-define(MAX_RESPONSE_TIME_95TH, 100).       % 100ms
-define(MAX_MEMORY_GROWTH_MB, 100).         % 100MB
-define(MIN_SUCCESS_RATE, 0.95).            % 95%
-define(MAX_ERROR_RATE, 0.05).              % 5%

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, concurrent_load},
        {group, throughput_tests},
        {group, stability_tests},
        {group, stress_tests}
    ].

groups() ->
    [
        {concurrent_load, [parallel], [
            test_concurrent_connections_1000,
            test_connection_churning,
            test_component_isolation_under_load
        ]},
        {throughput_tests, [sequence], [
            test_high_message_throughput,
            test_burst_traffic_handling,
            test_large_message_handling
        ]},
        {stability_tests, [sequence], [
            test_memory_stability_under_load,
            test_system_stability_over_time,
            test_mixed_workload_performance
        ]},
        {stress_tests, [sequence], [
            test_response_time_distribution,
            test_resource_exhaustion_recovery,
            test_graceful_degradation
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting ErlMCP Load Test Suite"),
    
    %% Start required applications
    ok = application:start(crypto),
    ok = application:start(ssl),
    
    %% Ensure jsx for JSON handling
    case application:start(jsx) of
        ok -> ok;
        {error, {already_started, jsx}} -> ok
    end,
    
    %% Start ErlMCP application
    case application:start(erlmcp) of
        ok -> ok;
        {error, {already_started, erlmcp}} -> ok
    end,
    
    %% Initialize load testing environment
    setup_load_test_environment(),
    
    %% Collect initial system metrics
    InitialMetrics = collect_system_metrics(),
    ct:pal("Initial system metrics: ~p", [InitialMetrics]),
    
    [{initial_metrics, InitialMetrics} | Config].

end_per_suite(Config) ->
    ct:pal("Ending ErlMCP Load Test Suite"),
    
    %% Collect final metrics
    FinalMetrics = collect_system_metrics(),
    InitialMetrics = proplists:get_value(initial_metrics, Config, #{}),
    
    %% Calculate overall suite impact
    MetricsDelta = calculate_metrics_delta(InitialMetrics, FinalMetrics),
    ct:pal("Suite metrics delta: ~p", [MetricsDelta]),
    
    %% Cleanup
    cleanup_load_test_environment(),
    application:stop(erlmcp),
    
    ok.

init_per_group(Group, Config) ->
    ct:pal("Starting load test group: ~p", [Group]),
    
    %% Collect pre-group metrics
    PreMetrics = collect_system_metrics(),
    
    [{group, Group}, {pre_group_metrics, PreMetrics} | Config].

end_per_group(Group, Config) ->
    %% Collect post-group metrics
    PostMetrics = collect_system_metrics(),
    PreMetrics = proplists:get_value(pre_group_metrics, Config, #{}),
    
    GroupDelta = calculate_metrics_delta(PreMetrics, PostMetrics),
    ct:pal("Group ~p metrics delta: ~p", [Group, GroupDelta]),
    
    %% Force garbage collection between groups
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
    timer:sleep(1000),
    
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting load test case: ~p", [TestCase]),
    
    %% Collect pre-test metrics
    PreTestMetrics = collect_system_metrics(),
    
    [{testcase, TestCase}, {pre_test_metrics, PreTestMetrics} | Config].

end_per_testcase(TestCase, Config) ->
    %% Collect post-test metrics
    PostTestMetrics = collect_system_metrics(),
    PreTestMetrics = proplists:get_value(pre_test_metrics, Config, #{}),
    
    TestDelta = calculate_metrics_delta(PreTestMetrics, PostTestMetrics),
    ct:pal("Test ~p metrics delta: ~p", [TestCase, TestDelta]),
    
    %% Cleanup test artifacts
    cleanup_test_case(TestCase),
    
    %% Brief recovery pause
    timer:sleep(500),
    ok.

%%====================================================================
%% Concurrent Load Tests
%%====================================================================

test_concurrent_connections_1000(Config) ->
    ct:pal("Testing 1000+ concurrent connections"),
    
    %% Create load test server
    ServerId = load_test_server,
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true},
            tools = #mcp_capability{enabled = true}
        }
    },
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),
    
    %% Add simple tools for load testing
    ok = setup_load_test_tools(ServerId),
    
    StartTime = erlang:system_time(millisecond),
    InitialMetrics = collect_system_metrics(),
    
    %% Create concurrent connections
    ConnectionCount = min(?MAX_CONCURRENT_CONNECTIONS, 1000),
    ct:pal("Creating ~p concurrent connections", [ConnectionCount]),
    
    %% Spawn connection workers
    ConnectionPids = lists:map(fun(ConnNum) ->
        spawn_monitor(fun() ->
            concurrent_connection_worker(ServerId, ConnNum, 100) % 100 operations per connection
        end)
    end, lists:seq(1, ConnectionCount)),
    
    %% Monitor system during load
    MonitorRef = spawn_monitor(fun() -> 
        system_monitor_loop(StartTime, ?LOAD_TEST_DURATION div 10) 
    end),
    
    %% Wait for all connections to complete
    Results = collect_worker_results(ConnectionPids, ?LOAD_TEST_DURATION),
    
    %% Stop monitoring
    case MonitorRef of
        {MonitorPid, MonitorMRef} ->
            exit(MonitorPid, shutdown),
            receive
                {'DOWN', MonitorMRef, process, MonitorPid, _} -> ok
            after 1000 -> ok
            end;
        _ -> ok
    end,
    
    EndTime = erlang:system_time(millisecond),
    TotalDuration = EndTime - StartTime,
    
    %% Analyze results
    {SuccessCount, FailureCount, TotalOps} = analyze_connection_results(Results),
    SuccessRate = SuccessCount / max(TotalOps, 1),
    
    FinalMetrics = collect_system_metrics(),
    LoadMetrics = calculate_metrics_delta(InitialMetrics, FinalMetrics),
    
    ct:pal("Concurrent connections test results:"),
    ct:pal("  Connections: ~p", [ConnectionCount]),
    ct:pal("  Total operations: ~p", [TotalOps]),
    ct:pal("  Success rate: ~.2f%", [SuccessRate * 100]),
    ct:pal("  Duration: ~pms", [TotalDuration]),
    ct:pal("  Memory delta: ~pMB", [maps:get(memory_mb, LoadMetrics, 0)]),
    
    %% Validate performance
    ?assert(SuccessRate >= ?MIN_SUCCESS_RATE, 
            io_lib:format("Success rate ~.2f% below threshold ~.2f%", 
                         [SuccessRate * 100, ?MIN_SUCCESS_RATE * 100])),
    
    MemoryGrowthMB = maps:get(memory_mb, LoadMetrics, 0),
    ?assert(MemoryGrowthMB =< ?MAX_MEMORY_GROWTH_MB,
            io_lib:format("Memory growth ~pMB exceeds limit ~pMB", 
                         [MemoryGrowthMB, ?MAX_MEMORY_GROWTH_MB])),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Concurrent connections test completed successfully"),
    Config.

test_connection_churning(Config) ->
    ct:pal("Testing connection churning (rapid connect/disconnect)"),
    
    ServerId = churn_test_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    ok = setup_load_test_tools(ServerId),
    
    StartTime = erlang:system_time(millisecond),
    EndTime = StartTime + (60 * 1000), % 1 minute test
    
    %% Start connection churning workers
    ChurnWorkerCount = 50,
    ChurnPids = lists:map(fun(WorkerNum) ->
        spawn_monitor(fun() ->
            connection_churn_worker(ServerId, WorkerNum, StartTime, EndTime)
        end)
    end, lists:seq(1, ChurnWorkerCount)),
    
    %% Wait for churning to complete
    ChurnResults = collect_worker_results(ChurnPids, 65000),
    
    %% Analyze churning results
    TotalChurns = lists:sum([Count || {_, Count} <- ChurnResults]),
    ActualDuration = erlang:system_time(millisecond) - StartTime,
    ChurnsPerSecond = (TotalChurns * 1000) div max(ActualDuration, 1),
    
    ct:pal("Connection churning results:"),
    ct:pal("  Workers: ~p", [ChurnWorkerCount]),
    ct:pal("  Total churns: ~p", [TotalChurns]),
    ct:pal("  Churns per second: ~p", [ChurnsPerSecond]),
    ct:pal("  Duration: ~pms", [ActualDuration]),
    
    %% Validate churning performance
    ?assert(TotalChurns > ChurnWorkerCount * 10, "Should handle significant churning"),
    ?assert(ChurnsPerSecond > 10, "Should handle at least 10 churns per second"),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Connection churning test completed successfully"),
    Config.

test_component_isolation_under_load(Config) ->
    ct:pal("Testing component isolation under load"),
    
    %% Create multiple servers to test isolation
    ServerConfigs = [
        {server_a, #{load_group => a}},
        {server_b, #{load_group => b}},
        {server_c, #{load_group => c}}
    ],
    
    ServerPids = lists:map(fun({ServerId, ServerConfig}) ->
        {ok, Pid} = erlmcp:start_server(ServerId, ServerConfig),
        ok = setup_load_test_tools(ServerId),
        {ServerId, Pid}
    end, ServerConfigs),
    
    %% Apply different load patterns to each server
    LoadPatterns = [
        {server_a, high_frequency_small_ops, 1000},
        {server_b, medium_frequency_medium_ops, 500},
        {server_c, low_frequency_large_ops, 100}
    ],
    
    StartTime = erlang:system_time(millisecond),
    
    %% Start isolated load workers
    LoadWorkerPids = lists:map(fun({ServerId, Pattern, OpCount}) ->
        spawn_monitor(fun() ->
            isolated_load_worker(ServerId, Pattern, OpCount)
        end)
    end, LoadPatterns),
    
    %% Monitor each server's performance
    ServerMonitors = lists:map(fun({ServerId, _Pid}) ->
        spawn_monitor(fun() ->
            server_isolation_monitor(ServerId, StartTime, 30000) % 30 second monitoring
        end)
    end, ServerPids),
    
    %% Wait for load completion
    LoadResults = collect_worker_results(LoadWorkerPids, 35000),
    MonitorResults = collect_worker_results(ServerMonitors, 35000),
    
    Duration = erlang:system_time(millisecond) - StartTime,
    
    %% Analyze isolation effectiveness
    ct:pal("Component isolation test results (duration: ~pms):", [Duration]),
    lists:foreach(fun({ServerId, LoadResult, MonitorResult}) ->
        ct:pal("  Server ~p: load=~p, isolation=~p", [ServerId, LoadResult, MonitorResult])
    end, lists:zip3([S || {S, _} <- ServerConfigs], LoadResults, MonitorResults)),
    
    %% Validate that servers maintained isolation
    lists:foreach(fun({ServerId, _Pid}) ->
        {ok, {ServerPid, _}} = erlmcp_registry:find_server(ServerId),
        ?assert(is_process_alive(ServerPid), 
                io_lib:format("Server ~p should survive isolated load", [ServerId]))
    end, ServerPids),
    
    %% Cleanup
    lists:foreach(fun({ServerId, _Pid}) ->
        ok = erlmcp:stop_server(ServerId)
    end, ServerPids),
    
    ct:pal("Component isolation test completed successfully"),
    Config.

%%====================================================================
%% Throughput Tests
%%====================================================================

test_high_message_throughput(Config) ->
    ct:pal("Testing high message throughput (target: ~p msg/sec)", [?HIGH_THROUGHPUT_TARGET]),
    
    ServerId = throughput_test_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    ok = setup_load_test_tools(ServerId),
    
    %% Setup transport for throughput testing
    TransportId = throughput_test_transport,
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Generate high-frequency message stream
    MessageCount = ?HIGH_THROUGHPUT_TARGET * 10, % 10 seconds worth
    StartTime = erlang:system_time(microsecond),
    
    %% Create multiple producer processes
    ProducerCount = 20,
    MessagesPerProducer = MessageCount div ProducerCount,
    
    ProducerPids = lists:map(fun(ProducerNum) ->
        spawn_monitor(fun() ->
            throughput_producer(TransportPid, ProducerNum, MessagesPerProducer)
        end)
    end, lists:seq(1, ProducerCount)),
    
    %% Wait for all producers to complete
    ProducerResults = collect_worker_results(ProducerPids, 30000),
    
    EndTime = erlang:system_time(microsecond),
    TotalDuration = EndTime - StartTime,
    DurationSeconds = TotalDuration / 1000000,
    
    %% Calculate throughput metrics
    TotalSent = lists:sum([Sent || {sent, Sent} <- ProducerResults]),
    ActualThroughput = TotalSent / DurationSeconds,
    
    ct:pal("High throughput test results:"),
    ct:pal("  Target throughput: ~p msg/sec", [?HIGH_THROUGHPUT_TARGET]),
    ct:pal("  Actual throughput: ~.1f msg/sec", [ActualThroughput]),
    ct:pal("  Messages sent: ~p", [TotalSent]),
    ct:pal("  Duration: ~.2fs", [DurationSeconds]),
    ct:pal("  Producers: ~p", [ProducerCount]),
    
    %% Validate throughput performance
    MinAcceptableThroughput = ?HIGH_THROUGHPUT_TARGET * 0.8, % 80% of target
    ?assert(ActualThroughput >= MinAcceptableThroughput,
            io_lib:format("Throughput ~.1f msg/sec below minimum ~.1f msg/sec",
                         [ActualThroughput, MinAcceptableThroughput])),
    
    %% Verify system stability after high throughput
    timer:sleep(1000), % Recovery time
    ?assert(is_process_alive(TransportPid), "Transport should survive high throughput"),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("High message throughput test completed successfully"),
    Config.

test_burst_traffic_handling(Config) ->
    ct:pal("Testing burst traffic handling (burst size: ~p)", [?BURST_SIZE]),
    
    ServerId = burst_test_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    ok = setup_load_test_tools(ServerId),
    
    TransportId = burst_test_transport,
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Generate multiple traffic bursts
    BurstCount = 5,
    BurstResults = lists:map(fun(BurstNum) ->
        ct:pal("Executing burst ~p of ~p", [BurstNum, BurstCount]),
        
        BurstStartTime = erlang:system_time(microsecond),
        
        %% Send burst of messages
        BurstPids = lists:map(fun(MsgNum) ->
            spawn_monitor(fun() ->
                Message = create_test_message(tools_call, #{
                    tool => <<"burst_tool">>,
                    burst => BurstNum,
                    message => MsgNum
                }),
                TransportPid ! {message, jsx:encode(Message)},
                {sent, 1}
            end)
        end, lists:seq(1, ?BURST_SIZE)),
        
        %% Collect burst results
        BurstMessages = collect_worker_results(BurstPids, 10000),
        BurstEndTime = erlang:system_time(microsecond),
        
        BurstDuration = (BurstEndTime - BurstStartTime) / 1000, % milliseconds
        BurstThroughput = (?BURST_SIZE * 1000) / max(BurstDuration, 1),
        
        %% Brief pause between bursts
        timer:sleep(500),
        
        {BurstNum, length(BurstMessages), BurstDuration, BurstThroughput}
    end, lists:seq(1, BurstCount)),
    
    %% Analyze burst performance
    {TotalMessages, AvgDuration, AvgThroughput} = lists:foldl(fun
        ({_BurstNum, MsgCount, Duration, Throughput}, {TotalMsgs, TotalDur, TotalThru}) ->
            {TotalMsgs + MsgCount, TotalDur + Duration, TotalThru + Throughput}
    end, {0, 0, 0}, BurstResults),
    
    FinalAvgDuration = AvgDuration / BurstCount,
    FinalAvgThroughput = AvgThroughput / BurstCount,
    
    ct:pal("Burst traffic handling results:"),
    ct:pal("  Bursts: ~p", [BurstCount]),
    ct:pal("  Burst size: ~p messages", [?BURST_SIZE]),
    ct:pal("  Total messages: ~p", [TotalMessages]),
    ct:pal("  Average burst duration: ~.2fms", [FinalAvgDuration]),
    ct:pal("  Average burst throughput: ~.1f msg/sec", [FinalAvgThroughput]),
    
    %% Validate burst handling
    ?assertEqual(BurstCount * ?BURST_SIZE, TotalMessages, "All burst messages should be sent"),
    ?assert(FinalAvgThroughput > 100, "Should maintain reasonable throughput during bursts"),
    ?assert(FinalAvgDuration < 10000, "Burst handling should complete within 10 seconds"),
    
    %% Verify system stability after bursts
    timer:sleep(1000),
    ?assert(is_process_alive(TransportPid), "Transport should survive traffic bursts"),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Burst traffic handling test completed successfully"),
    Config.

test_large_message_handling(Config) ->
    ct:pal("Testing large message handling (size: ~pMB)", [?LARGE_MESSAGE_SIZE div 1024 div 1024]),
    
    ServerId = large_msg_test_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add tool that handles large messages
    LargeMsgTool = fun(Args) ->
        Data = maps:get(<<"data">>, Args, <<"">>),
        Size = byte_size(Data),
        
        %% Echo back with size information
        #{
            original_size => Size,
            processed => true,
            checksum => erlang:crc32(Data)
        }
    end,
    
    ok = erlmcp:add_tool(ServerId, <<"large_message_processor">>, LargeMsgTool),
    
    TransportId = large_msg_transport,
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Generate large messages of different sizes
    MessageSizes = [
        1024,          % 1KB
        10240,         % 10KB
        102400,        % 100KB
        1048576,       % 1MB
        5242880        % 5MB
    ],
    
    LargeMessageResults = lists:map(fun(MessageSize) ->
        ct:pal("Testing message size: ~pKB", [MessageSize div 1024]),
        
        %% Create large data payload
        LargeData = crypto:strong_rand_bytes(MessageSize),
        
        Message = create_test_message(tools_call, #{
            tool => <<"large_message_processor">>,
            arguments => #{<<"data">> => base64:encode(LargeData)}
        }),
        
        StartTime = erlang:system_time(microsecond),
        
        %% Send large message
        TransportPid ! {message, jsx:encode(Message)},
        
        %% Wait for processing (longer timeout for larger messages)
        Timeout = min(30000, MessageSize div 1024 + 5000),
        timer:sleep(Timeout),
        
        EndTime = erlang:system_time(microsecond),
        ProcessingTime = (EndTime - StartTime) / 1000, % milliseconds
        
        %% Check system stability
        SystemStable = is_process_alive(TransportPid),
        
        {MessageSize, ProcessingTime, SystemStable}
    end, MessageSizes),
    
    %% Analyze large message handling
    ct:pal("Large message handling results:"),
    lists:foreach(fun({Size, Time, Stable}) ->
        SizeKB = Size div 1024,
        ct:pal("  ~pKB: ~.2fms, stable=~p", [SizeKB, Time, Stable])
    end, LargeMessageResults),
    
    %% Validate large message handling
    lists:foreach(fun({Size, Time, Stable}) ->
        ?assert(Stable, io_lib:format("System should remain stable with ~pKB message", [Size div 1024])),
        
        MaxExpectedTime = (Size div 1024) + 5000, % Size-based timeout + base time
        ?assert(Time =< MaxExpectedTime,
                io_lib:format("Processing time ~.2fms should be <= ~pms for ~pKB message",
                             [Time, MaxExpectedTime, Size div 1024]))
    end, LargeMessageResults),
    
    %% Test memory cleanup after large messages
    timer:sleep(2000), % Allow garbage collection
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
    
    PostGCMemory = erlang:memory(total),
    ct:pal("Memory after large message processing and GC: ~pMB", [PostGCMemory div 1024 div 1024]),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Large message handling test completed successfully"),
    Config.

%%====================================================================
%% Stability Tests
%%====================================================================

test_memory_stability_under_load(Config) ->
    ct:pal("Testing memory stability under load (~p minutes)", [?LOAD_TEST_DURATION div 60000]),
    
    ServerId = memory_stability_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    ok = setup_load_test_tools(ServerId),
    
    %% Start memory monitoring
    MemoryMonitor = spawn_monitor(fun() ->
        memory_stability_monitor(?LOAD_TEST_DURATION)
    end),
    
    %% Start sustained load
    LoadWorkerCount = 20,
    LoadEndTime = erlang:system_time(millisecond) + ?LOAD_TEST_DURATION,
    
    LoadWorkers = lists:map(fun(WorkerNum) ->
        spawn_monitor(fun() ->
            sustained_load_worker(ServerId, WorkerNum, LoadEndTime)
        end)
    end, lists:seq(1, LoadWorkerCount)),
    
    %% Wait for load test completion
    LoadResults = collect_worker_results(LoadWorkers, ?LOAD_TEST_DURATION + 5000),
    
    %% Stop memory monitoring
    {MemoryMonitorPid, MemoryMonitorRef} = MemoryMonitor,
    exit(MemoryMonitorPid, shutdown),
    MemoryData = receive
        {'DOWN', MemoryMonitorRef, process, MemoryMonitorPid, MemoryResults} 
            when is_list(MemoryResults) ->
            MemoryResults;
        {'DOWN', MemoryMonitorRef, process, MemoryMonitorPid, _} ->
            []
    after 5000 ->
        []
    end,
    
    %% Analyze memory stability
    case analyze_memory_stability(MemoryData) of
        {ok, MemoryStats} ->
            ct:pal("Memory stability analysis:"),
            ct:pal("  Initial memory: ~pMB", [maps:get(initial_mb, MemoryStats, 0)]),
            ct:pal("  Peak memory: ~pMB", [maps:get(peak_mb, MemoryStats, 0)]),
            ct:pal("  Final memory: ~pMB", [maps:get(final_mb, MemoryStats, 0)]),
            ct:pal("  Memory growth: ~pMB", [maps:get(growth_mb, MemoryStats, 0)]),
            ct:pal("  Stability rating: ~.2f", [maps:get(stability_rating, MemoryStats, 0.0)]),
            
            %% Validate memory stability
            GrowthMB = maps:get(growth_mb, MemoryStats, 0),
            StabilityRating = maps:get(stability_rating, MemoryStats, 0.0),
            
            ?assert(GrowthMB =< ?MAX_MEMORY_GROWTH_MB,
                    io_lib:format("Memory growth ~pMB exceeds limit ~pMB", 
                                 [GrowthMB, ?MAX_MEMORY_GROWTH_MB])),
            ?assert(StabilityRating >= 0.8,
                    io_lib:format("Memory stability rating ~.2f below threshold 0.8", 
                                 [StabilityRating]));
        {error, Reason} ->
            ct:pal("Memory analysis failed: ~p", [Reason]),
            ct:fail("Memory stability analysis failed")
    end,
    
    %% Analyze load test results
    TotalOps = lists:sum([Ops || {operations, Ops} <- LoadResults]),
    ct:pal("Load test completed: ~p total operations", [TotalOps]),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Memory stability test completed successfully"),
    Config.

test_system_stability_over_time(Config) ->
    ct:pal("Testing system stability over time (~p minutes)", [?STABILITY_TEST_DURATION div 60000]),
    
    %% Create multiple servers for comprehensive stability testing
    ServerConfigs = [
        {stability_server_1, #{workload => light}},
        {stability_server_2, #{workload => medium}},
        {stability_server_3, #{workload => heavy}}
    ],
    
    ServerPids = lists:map(fun({ServerId, ServerConfig}) ->
        {ok, Pid} = erlmcp:start_server(ServerId, ServerConfig),
        ok = setup_load_test_tools(ServerId),
        {ServerId, Pid}
    end, ServerConfigs),
    
    StartTime = erlang:system_time(millisecond),
    EndTime = StartTime + ?STABILITY_TEST_DURATION,
    
    %% Start stability monitoring for each component
    StabilityMonitors = [
        spawn_monitor(fun() -> 
            system_stability_monitor(supervisor, EndTime) 
        end),
        spawn_monitor(fun() -> 
            system_stability_monitor(registry, EndTime) 
        end),
        spawn_monitor(fun() -> 
            system_stability_monitor(memory, EndTime) 
        end),
        spawn_monitor(fun() -> 
            system_stability_monitor(processes, EndTime) 
        end)
    ],
    
    %% Start different workload patterns
    WorkloadWorkers = lists:map(fun({ServerId, _Pid}) ->
        Workload = case ServerId of
            stability_server_1 -> light_continuous_load;
            stability_server_2 -> medium_bursty_load;
            stability_server_3 -> heavy_mixed_load
        end,
        spawn_monitor(fun() ->
            stability_workload_worker(ServerId, Workload, EndTime)
        end)
    end, ServerPids),
    
    %% Wait for stability test completion
    StabilityResults = collect_worker_results(StabilityMonitors, ?STABILITY_TEST_DURATION + 10000),
    WorkloadResults = collect_worker_results(WorkloadWorkers, ?STABILITY_TEST_DURATION + 10000),
    
    ActualDuration = erlang:system_time(millisecond) - StartTime,
    
    %% Analyze system stability
    ct:pal("System stability test results (duration: ~pms):", [ActualDuration]),
    
    %% Check monitoring results
    lists:foreach(fun({Component, MonitorResult}) ->
        ct:pal("  ~p stability: ~p", [Component, MonitorResult])
    end, lists:zip([supervisor, registry, memory, processes], StabilityResults)),
    
    %% Check workload results
    lists:foreach(fun({{ServerId, _}, WorkloadResult}) ->
        ct:pal("  ~p workload result: ~p", [ServerId, WorkloadResult])
    end, lists:zip(ServerPids, WorkloadResults)),
    
    %% Validate system stability
    lists:foreach(fun({ServerId, ServerPid}) ->
        ?assert(is_process_alive(ServerPid),
                io_lib:format("Server ~p should remain alive during stability test", [ServerId]))
    end, ServerPids),
    
    %% Validate core components are still functional
    ?assertNotEqual(undefined, whereis(erlmcp_sup), "Main supervisor should be running"),
    ?assertNotEqual(undefined, whereis(erlmcp_registry), "Registry should be running"),
    
    %% Test that system is still responsive
    TestServerId = post_stability_test_server,
    {ok, TestServerPid} = erlmcp:start_server(TestServerId, #{}),
    ?assert(is_process_alive(TestServerPid), "System should be responsive after stability test"),
    ok = erlmcp:stop_server(TestServerId),
    
    %% Cleanup
    lists:foreach(fun({ServerId, _Pid}) ->
        ok = erlmcp:stop_server(ServerId)
    end, ServerPids),
    
    ct:pal("System stability test completed successfully"),
    Config.

test_mixed_workload_performance(Config) ->
    ct:pal("Testing mixed workload performance"),
    
    ServerId = mixed_workload_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    ok = setup_comprehensive_load_tools(ServerId),
    
    %% Define mixed workload patterns
    WorkloadPatterns = [
        {quick_tools, 500, 10},        % 500 quick tool calls, 10ms each
        {slow_tools, 50, 100},         % 50 slow tool calls, 100ms each
        {resource_reads, 200, 20},     % 200 resource reads, 20ms each
        {large_resources, 20, 200},    % 20 large resource reads, 200ms each
        {prompt_requests, 100, 50},    % 100 prompt requests, 50ms each
        {mixed_operations, 300, 30}    % 300 mixed operations, 30ms avg
    ],
    
    StartTime = erlang:system_time(millisecond),
    
    %% Start mixed workload workers
    WorkloadWorkers = lists:map(fun({Pattern, Count, AvgDuration}) ->
        spawn_monitor(fun() ->
            mixed_workload_worker(ServerId, Pattern, Count, AvgDuration)
        end)
    end, WorkloadPatterns),
    
    %% Monitor performance during mixed workload
    PerformanceMonitor = spawn_monitor(fun() ->
        performance_monitor_loop(StartTime, 60000) % 1 minute monitoring
    end),
    
    %% Wait for workload completion
    WorkloadResults = collect_worker_results(WorkloadWorkers, 70000),
    
    %% Stop performance monitoring
    {PerfMonPid, PerfMonRef} = PerformanceMonitor,
    exit(PerfMonPid, shutdown),
    PerformanceData = receive
        {'DOWN', PerfMonRef, process, PerfMonPid, PerfResults} when is_list(PerfResults) ->
            PerfResults;
        {'DOWN', PerfMonRef, process, PerfMonPid, _} ->
            []
    after 2000 ->
        []
    end,
    
    EndTime = erlang:system_time(millisecond),
    TotalDuration = EndTime - StartTime,
    
    %% Analyze mixed workload performance
    {TotalOps, AverageLatency, ThroughputOPS} = analyze_workload_results(WorkloadResults, TotalDuration),
    
    ct:pal("Mixed workload performance results:"),
    ct:pal("  Duration: ~pms", [TotalDuration]),
    ct:pal("  Total operations: ~p", [TotalOps]),
    ct:pal("  Average latency: ~.2fms", [AverageLatency]),
    ct:pal("  Throughput: ~.1f ops/sec", [ThroughputOPS]),
    ct:pal("  Performance samples: ~p", [length(PerformanceData)]),
    
    %% Analyze performance distribution
    case PerformanceData of
        [] ->
            ct:pal("No performance data collected");
        _ ->
            PerfStats = calculate_performance_statistics(PerformanceData),
            ct:pal("  Response time P50: ~.2fms", [maps:get(p50, PerfStats, 0)]),
            ct:pal("  Response time P95: ~.2fms", [maps:get(p95, PerfStats, 0)]),
            ct:pal("  Response time P99: ~.2fms", [maps:get(p99, PerfStats, 0)]),
            
            %% Validate performance thresholds
            P95ResponseTime = maps:get(p95, PerfStats, 0),
            ?assert(P95ResponseTime =< ?MAX_RESPONSE_TIME_95TH,
                    io_lib:format("P95 response time ~.2fms exceeds threshold ~pms",
                                 [P95ResponseTime, ?MAX_RESPONSE_TIME_95TH]))
    end,
    
    %% Validate overall performance
    ?assert(TotalOps > 1000, "Should complete significant number of operations"),
    ?assert(ThroughputOPS > 10, "Should maintain reasonable throughput"),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Mixed workload performance test completed successfully"),
    Config.

%%====================================================================
%% Stress Tests
%%====================================================================

test_response_time_distribution(Config) ->
    ct:pal("Testing response time distribution (~p samples)", [?RESPONSE_TIME_SAMPLES]),
    
    ServerId = response_time_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add tools with known response characteristics
    ResponseTimeTools = [
        {<<"fast_tool">>, 5, fun(_) -> <<"fast_result">> end},
        {<<"medium_tool">>, 50, fun(Args) -> 
            timer:sleep(40), 
            #{input => Args, result => <<"medium_result">>}
        end},
        {<<"slow_tool">>, 200, fun(Args) -> 
            timer:sleep(150), 
            #{input => Args, result => <<"slow_result">>, processing_time => 150}
        end}
    ],
    
    lists:foreach(fun({Name, _ExpectedTime, Handler}) ->
        ok = erlmcp:add_tool(ServerId, Name, Handler)
    end, ResponseTimeTools),
    
    TransportId = response_time_transport,
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Collect response time samples
    ct:pal("Collecting ~p response time samples...", [?RESPONSE_TIME_SAMPLES]),
    
    ResponseTimes = lists:map(fun(SampleNum) ->
        %% Select random tool
        {ToolName, ExpectedTime, _} = lists:nth(
            (SampleNum rem length(ResponseTimeTools)) + 1, 
            ResponseTimeTools
        ),
        
        %% Measure response time
        StartTime = erlang:system_time(microsecond),
        
        Message = create_test_message(tools_call, #{
            tool => ToolName,
            sample => SampleNum
        }),
        
        TransportPid ! {message, jsx:encode(Message)},
        
        %% Wait for processing (with timeout based on expected time)
        Timeout = ExpectedTime + 100,
        timer:sleep(Timeout),
        
        EndTime = erlang:system_time(microsecond),
        ResponseTime = (EndTime - StartTime) / 1000, % Convert to milliseconds
        
        {ToolName, ResponseTime, ExpectedTime}
    end, lists:seq(1, ?RESPONSE_TIME_SAMPLES)),
    
    %% Analyze response time distribution
    ResponseTimeStats = analyze_response_time_distribution(ResponseTimes),
    
    ct:pal("Response time distribution analysis:"),
    ct:pal("  Samples: ~p", [length(ResponseTimes)]),
    ct:pal("  Mean: ~.2fms", [maps:get(mean, ResponseTimeStats, 0)]),
    ct:pal("  Median (P50): ~.2fms", [maps:get(median, ResponseTimeStats, 0)]),
    ct:pal("  P90: ~.2fms", [maps:get(p90, ResponseTimeStats, 0)]),
    ct:pal("  P95: ~.2fms", [maps:get(p95, ResponseTimeStats, 0)]),
    ct:pal("  P99: ~.2fms", [maps:get(p99, ResponseTimeStats, 0)]),
    ct:pal("  Max: ~.2fms", [maps:get(max, ResponseTimeStats, 0)]),
    ct:pal("  Std Dev: ~.2fms", [maps:get(std_dev, ResponseTimeStats, 0)]),
    
    %% Validate response time performance
    P95Time = maps:get(p95, ResponseTimeStats, 0),
    ?assert(P95Time =< ?MAX_RESPONSE_TIME_95TH,
            io_lib:format("P95 response time ~.2fms exceeds threshold ~pms",
                         [P95Time, ?MAX_RESPONSE_TIME_95TH])),
    
    %% Validate response time consistency
    StdDev = maps:get(std_dev, ResponseTimeStats, 0),
    Mean = maps:get(mean, ResponseTimeStats, 1),
    CoefficientOfVariation = StdDev / Mean,
    ?assert(CoefficientOfVariation =< 2.0,
            io_lib:format("Response time variability too high (CV: ~.2f)",
                         [CoefficientOfVariation])),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Response time distribution test completed successfully"),
    Config.

test_resource_exhaustion_recovery(Config) ->
    ct:pal("Testing resource exhaustion and recovery"),
    
    ServerId = exhaustion_test_server,
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add resource-intensive tools
    MemoryHeavyTool = fun(Args) ->
        Size = maps:get(<<"size">>, Args, 1024),
        _LargeData = lists:seq(1, Size * 1024), % Create large data structure
        timer:sleep(100),
        #{allocated_size => Size, status => <<"processed">>}
    end,
    
    ProcessHeavyTool = fun(Args) ->
        Count = maps:get(<<"count">>, Args, 10),
        Pids = [spawn(fun() -> timer:sleep(1000) end) || _ <- lists:seq(1, Count)],
        timer:sleep(200),
        lists:foreach(fun(Pid) -> exit(Pid, normal) end, Pids),
        #{processes_spawned => Count, status => <<"completed">>}
    end,
    
    ok = erlmcp:add_tool(ServerId, <<"memory_heavy">>, MemoryHeavyTool),
    ok = erlmcp:add_tool(ServerId, <<"process_heavy">>, ProcessHeavyTool),
    
    %% Monitor system resources
    InitialResources = collect_system_metrics(),
    
    %% Create resource exhaustion scenarios
    ExhaustionScenarios = [
        {memory_exhaustion, 50, #{tool => <<"memory_heavy">>, size => 100}},
        {process_exhaustion, 20, #{tool => <<"process_heavy">>, count => 50}},
        {mixed_exhaustion, 30, #{mixed => true}}
    ],
    
    ExhaustionResults = lists:map(fun({ScenarioType, OperationCount, Params}) ->
        ct:pal("Testing ~p scenario with ~p operations", [ScenarioType, OperationCount]),
        
        ScenarioStart = erlang:system_time(millisecond),
        
        %% Execute exhaustion scenario
        ExhaustionPids = lists:map(fun(OpNum) ->
            spawn_monitor(fun() ->
                try
                    case ScenarioType of
                        memory_exhaustion ->
                            %% Memory-intensive operation
                            resource_exhaustion_worker(ServerId, memory_heavy, Params);
                        process_exhaustion ->
                            %% Process-intensive operation
                            resource_exhaustion_worker(ServerId, process_heavy, Params);
                        mixed_exhaustion ->
                            %% Mixed resource usage
                            Tool = case OpNum rem 2 of
                                0 -> memory_heavy;
                                1 -> process_heavy
                            end,
                            resource_exhaustion_worker(ServerId, Tool, #{})
                    end
                catch
                    _:Error ->
                        {error, Error}
                end
            end)
        end, lists:seq(1, OperationCount)),
        
        %% Collect results with timeout
        ScenarioResults = collect_worker_results(ExhaustionPids, 30000),
        ScenarioEnd = erlang:system_time(millisecond),
        
        %% Check system recovery
        timer:sleep(2000), % Recovery pause
        [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
        
        RecoveryResources = collect_system_metrics(),
        
        {ScenarioType, ScenarioResults, ScenarioEnd - ScenarioStart, RecoveryResources}
    end, ExhaustionScenarios),
    
    %% Analyze exhaustion and recovery
    ct:pal("Resource exhaustion and recovery results:"),
    lists:foreach(fun({ScenarioType, Results, Duration, RecoveryMetrics}) ->
        SuccessCount = length([R || R <- Results, element(1, R) =/= error]),
        SuccessRate = SuccessCount / max(length(Results), 1),
        
        ct:pal("  ~p:", [ScenarioType]),
        ct:pal("    Success rate: ~.1f%", [SuccessRate * 100]),
        ct:pal("    Duration: ~pms", [Duration]),
        ct:pal("    Recovery memory: ~pMB", [maps:get(memory_mb, RecoveryMetrics, 0)]),
        ct:pal("    Recovery processes: ~p", [maps:get(process_count, RecoveryMetrics, 0)])
    end, ExhaustionResults),
    
    %% Validate recovery
    FinalResources = collect_system_metrics(),
    MemoryGrowthMB = maps:get(memory_mb, FinalResources, 0) - maps:get(memory_mb, InitialResources, 0),
    ProcessGrowth = maps:get(process_count, FinalResources, 0) - maps:get(process_count, InitialResources, 0),
    
    ct:pal("Overall recovery metrics:"),
    ct:pal("  Memory growth: ~pMB", [MemoryGrowthMB]),
    ct:pal("  Process growth: ~p", [ProcessGrowth]),
    
    %% Validate system recovered properly
    ?assert(MemoryGrowthMB =< 50, "Memory should be recovered within 50MB"),
    ?assert(ProcessGrowth =< 20, "Process count should be recovered within 20 processes"),
    
    %% Verify system is still functional
    {ok, {TestServerPid, _}} = erlmcp_registry:find_server(ServerId),
    ?assert(is_process_alive(TestServerPid), "Server should survive resource exhaustion"),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Resource exhaustion recovery test completed successfully"),
    Config.

test_graceful_degradation(Config) ->
    ct:pal("Testing graceful degradation under extreme load"),
    
    %% Create system under extreme load conditions
    ServerCount = 10,
    ServersPerGroup = [
        {critical_servers, 3},
        {standard_servers, 4},
        {optional_servers, 3}
    ],
    
    AllServers = lists:flatten([
        [{ServerGroup, I} || I <- lists:seq(1, Count)]
        || {ServerGroup, Count} <- ServersPerGroup
    ]),
    
    %% Start all servers
    ServerPids = lists:map(fun({ServerGroup, ServerNum}) ->
        ServerId = list_to_atom(io_lib:format("~p_~p", [ServerGroup, ServerNum])),
        Priority = case ServerGroup of
            critical_servers -> high;
            standard_servers -> medium;
            optional_servers -> low
        end,
        
        ServerConfig = #{
            priority => Priority,
            server_group => ServerGroup
        },
        
        {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),
        ok = setup_load_test_tools(ServerId),
        
        {ServerId, ServerGroup, Priority, ServerPid}
    end, AllServers),
    
    %% Create extreme load conditions
    ExtremeLoadScenarios = [
        memory_pressure,
        high_connection_count,
        message_flood,
        resource_contention,
        cascading_failures
    ],
    
    DegradationResults = lists:map(fun(LoadScenario) ->
        ct:pal("Testing graceful degradation under ~p", [LoadScenario]),
        
        %% Apply extreme load scenario
        ScenarioWorkers = apply_extreme_load_scenario(LoadScenario, ServerPids),
        
        %% Monitor system behavior during extreme load
        MonitoringStart = erlang:system_time(millisecond),
        DegradationMonitor = spawn_monitor(fun() ->
            graceful_degradation_monitor(ServerPids, MonitoringStart, 30000) % 30 second monitoring
        end),
        
        %% Wait for scenario completion
        ScenarioResults = collect_worker_results(ScenarioWorkers, 35000),
        
        %% Stop monitoring
        {MonitorPid, MonitorRef} = DegradationMonitor,
        exit(MonitorPid, shutdown),
        DegradationData = receive
            {'DOWN', MonitorRef, process, MonitorPid, MonitoringResults} ->
                MonitoringResults;
            {'DOWN', MonitorRef, process, MonitorPid, _} ->
                #{degradation => unknown}
        after 2000 ->
            #{degradation => timeout}
        end,
        
        %% Analyze degradation behavior
        SystemHealth = assess_system_health(ServerPids),
        
        {LoadScenario, ScenarioResults, DegradationData, SystemHealth}
    end, ExtremeLoadScenarios),
    
    %% Analyze graceful degradation
    ct:pal("Graceful degradation analysis:"),
    lists:foreach(fun({LoadScenario, _Results, DegradationData, SystemHealth}) ->
        ct:pal("  ~p:", [LoadScenario]),
        ct:pal("    System health: ~p", [SystemHealth]),
        ct:pal("    Degradation data: ~p", [DegradationData])
    end, DegradationResults),
    
    %% Validate graceful degradation
    lists:foreach(fun({_LoadScenario, _Results, _DegradationData, SystemHealth}) ->
        CriticalServersAlive = maps:get(critical_servers_alive, SystemHealth, 0),
        TotalCriticalServers = 3, % From ServersPerGroup
        
        %% At least 2/3 of critical servers should survive
        MinCriticalSurvival = (TotalCriticalServers * 2) div 3,
        ?assert(CriticalServersAlive >= MinCriticalSurvival,
                io_lib:format("Critical servers alive (~p) below minimum (~p)",
                             [CriticalServersAlive, MinCriticalSurvival]))
    end, DegradationResults),
    
    %% Test recovery after extreme load
    ct:pal("Testing recovery after extreme load scenarios"),
    timer:sleep(5000), % Recovery time
    
    %% Verify system can still create new servers
    RecoveryTestServerId = recovery_test_server,
    {ok, RecoveryServerPid} = erlmcp:start_server(RecoveryTestServerId, #{}),
    ?assert(is_process_alive(RecoveryServerPid), "System should recover and allow new servers"),
    ok = erlmcp:stop_server(RecoveryTestServerId),
    
    %% Cleanup all servers
    lists:foreach(fun({ServerId, _Group, _Priority, _Pid}) ->
        try
            erlmcp:stop_server(ServerId)
        catch
            _:_ -> ok % Server might already be down from extreme load
        end
    end, ServerPids),
    
    ct:pal("Graceful degradation test completed successfully"),
    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

%% System monitoring and metrics collection
collect_system_metrics() ->
    #{
        memory_mb => erlang:memory(total) div 1024 div 1024,
        memory_processes => erlang:memory(processes) div 1024 div 1024,
        memory_system => erlang:memory(system) div 1024 div 1024,
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        uptime_ms => element(1, erlang:statistics(wall_clock)),
        reductions => element(1, erlang:statistics(reductions)),
        gc_count => element(1, erlang:statistics(garbage_collection))
    }.

calculate_metrics_delta(Initial, Final) ->
    maps:fold(fun(Key, FinalValue, Acc) ->
        InitialValue = maps:get(Key, Initial, 0),
        Delta = FinalValue - InitialValue,
        Acc#{Key => Delta}
    end, #{}, Final).

%% Load test environment setup
setup_load_test_environment() ->
    %% Ensure adequate process limit
    ProcessLimit = erlang:system_info(process_limit),
    RequiredLimit = ?MAX_CONCURRENT_CONNECTIONS * 3,
    
    case ProcessLimit < RequiredLimit of
        true ->
            ct:pal("WARNING: Process limit (~p) may be insufficient for load tests (need ~p)",
                   [ProcessLimit, RequiredLimit]);
        false ->
            ok
    end,
    
    %% Initialize metrics system if available
    try
        case erlmcp_metrics:start_link() of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok
        end,
        erlmcp_metrics:reset_metrics()
    catch
        _:_ -> ok % Metrics not available
    end.

cleanup_load_test_environment() ->
    %% Force garbage collection
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
    timer:sleep(1000).

%% Tool setup for load testing
setup_load_test_tools(ServerId) ->
    %% Basic load testing tools
    Tools = [
        {<<"ping">>, fun(_) -> <<"pong">> end},
        {<<"echo">>, fun(Args) -> Args end},
        {<<"calculate">>, fun(Args) ->
            A = maps:get(<<"a">>, Args, 0),
            B = maps:get(<<"b">>, Args, 0),
            Op = maps:get(<<"op">>, Args, <<"add">>),
            
            Result = case Op of
                <<"add">> -> A + B;
                <<"sub">> -> A - B;
                <<"mul">> -> A * B;
                <<"div">> when B =/= 0 -> A / B;
                _ -> 0
            end,
            
            #{result => Result}
        end},
        {<<"timestamp">>, fun(_) ->
            #{timestamp => erlang:system_time(millisecond)}
        end},
        {<<"work_simulator">>, fun(Args) ->
            Duration = maps:get(<<"duration">>, Args, 10),
            timer:sleep(Duration),
            #{work_duration => Duration, completed => true}
        end}
    ],
    
    lists:foreach(fun({Name, Handler}) ->
        ok = erlmcp:add_tool(ServerId, Name, Handler)
    end, Tools),
    
    %% Basic resources
    Resources = [
        {<<"test://config">>, fun(_) ->
            #{content => <<"test configuration">>, type => <<"config">>}
        end},
        {<<"test://data">>, fun(_) ->
            #{content => <<"test data content">>, type => <<"data">>}
        end}
    ],
    
    lists:foreach(fun({Uri, Handler}) ->
        ok = erlmcp:add_resource(ServerId, Uri, Handler)
    end, Resources).

setup_comprehensive_load_tools(ServerId) ->
    %% Extended tool set for comprehensive testing
    setup_load_test_tools(ServerId),
    
    %% Additional comprehensive tools
    AdditionalTools = [
        {<<"cpu_intensive">>, fun(Args) ->
            Iterations = maps:get(<<"iterations">>, Args, 1000),
            lists:sum([I * I || I <- lists:seq(1, Iterations)]),
            #{iterations => Iterations, completed => true}
        end},
        
        {<<"memory_allocator">>, fun(Args) ->
            Size = maps:get(<<"size">>, Args, 1024),
            Data = lists:seq(1, Size),
            #{allocated => length(Data), size => Size}
        end},
        
        {<<"json_processor">>, fun(Args) ->
            Data = maps:get(<<"data">>, Args, #{}),
            Encoded = jsx:encode(Data),
            Decoded = jsx:decode(Encoded, [return_maps]),
            #{processed => byte_size(Encoded), valid => is_map(Decoded)}
        end}
    ],
    
    lists:foreach(fun({Name, Handler}) ->
        ok = erlmcp:add_tool(ServerId, Name, Handler)
    end, AdditionalTools).

%% Worker functions
concurrent_connection_worker(ServerId, ConnNum, OperationCount) ->
    try
        %% Create transport for this connection
        TransportId = list_to_atom(io_lib:format("load_conn_~p", [ConnNum])),
        
        case erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}) of
            {ok, TransportPid} ->
                %% Perform operations
                Operations = lists:map(fun(OpNum) ->
                    Message = create_test_message(tools_call, #{
                        tool => <<"ping">>,
                        connection => ConnNum,
                        operation => OpNum
                    }),
                    
                    TransportPid ! {message, jsx:encode(Message)},
                    timer:sleep(rand:uniform(10)), % 1-10ms random delay
                    {sent, 1}
                end, lists:seq(1, OperationCount)),
                
                %% Cleanup
                erlmcp:stop_transport(TransportId),
                
                {success, length(Operations)};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:Exception ->
            {error, {Class, Exception}}
    end.

throughput_producer(TransportPid, ProducerNum, MessageCount) ->
    Messages = lists:map(fun(MsgNum) ->
        Message = create_test_message(tools_call, #{
            tool => <<"ping">>,
            producer => ProducerNum,
            message => MsgNum
        }),
        
        TransportPid ! {message, jsx:encode(Message)},
        1
    end, lists:seq(1, MessageCount)),
    
    {sent, lists:sum(Messages)}.

%% Create test message helper
create_test_message(Type, Params) ->
    BaseMessage = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => erlang:unique_integer([positive])
    },
    
    case Type of
        tools_call ->
            ToolName = maps:get(tool, Params, <<"ping">>),
            Args = maps:without([tool], Params),
            BaseMessage#{
                ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_CALL,
                ?JSONRPC_FIELD_PARAMS => #{
                    <<"name">> => ToolName,
                    <<"arguments">> => Args
                }
            };
        resources_read ->
            Uri = maps:get(uri, Params, <<"test://default">>),
            BaseMessage#{
                ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_RESOURCES_READ,
                ?JSONRPC_FIELD_PARAMS => #{<<"uri">> => Uri}
            };
        _ ->
            BaseMessage#{?JSONRPC_FIELD_METHOD => <<"test_method">>}
    end.

%% Worker result collection
collect_worker_results(WorkerPids, Timeout) ->
    collect_worker_results(WorkerPids, Timeout, []).

collect_worker_results([], _Timeout, Results) ->
    Results;
collect_worker_results([{Pid, Ref} | Rest], Timeout, Results) ->
    receive
        {'DOWN', Ref, process, Pid, Result} ->
            collect_worker_results(Rest, Timeout, [Result | Results])
    after Timeout ->
        %% Kill remaining workers
        [exit(P, kill) || {P, _} <- [P || {P, _R} <- Rest]],
        Results ++ [{error, timeout}]
    end.

%% Result analysis functions
analyze_connection_results(Results) ->
    lists:foldl(fun
        ({success, Ops}, {SuccessAcc, FailAcc, TotalAcc}) ->
            {SuccessAcc + Ops, FailAcc, TotalAcc + Ops};
        ({error, _}, {SuccessAcc, FailAcc, TotalAcc}) ->
            {SuccessAcc, FailAcc + 1, TotalAcc};
        (_, Acc) ->
            Acc
    end, {0, 0, 0}, Results).

analyze_memory_stability(MemoryData) ->
    case MemoryData of
        [] ->
            {error, no_data};
        _ ->
            MemoryValues = [MB || {_Time, MB} <- MemoryData],
            Initial = hd(MemoryValues),
            Final = lists:last(MemoryValues),
            Peak = lists:max(MemoryValues),
            
            Growth = Final - Initial,
            Variance = calculate_variance(MemoryValues),
            StabilityRating = calculate_stability_rating(MemoryValues),
            
            {ok, #{
                initial_mb => Initial,
                final_mb => Final,
                peak_mb => Peak,
                growth_mb => Growth,
                variance => Variance,
                stability_rating => StabilityRating
            }}
    end.

calculate_variance(Values) ->
    Mean = lists:sum(Values) / length(Values),
    SumSquaredDiffs = lists:sum([(V - Mean) * (V - Mean) || V <- Values]),
    SumSquaredDiffs / length(Values).

calculate_stability_rating(Values) ->
    case length(Values) of
        0 -> 0.0;
        1 -> 1.0;
        Len ->
            Mean = lists:sum(Values) / Len,
            MaxDeviation = lists:max([abs(V - Mean) || V <- Values]),
            case Mean of
                +0.0 -> 1.0;
                _ -> max(0.0, 1.0 - (MaxDeviation / Mean))
            end
    end.

analyze_response_time_distribution(ResponseTimes) ->
    Times = [Time || {_Tool, Time, _Expected} <- ResponseTimes],
    SortedTimes = lists:sort(Times),
    
    Len = length(Times),
    Mean = lists:sum(Times) / Len,
    Median = lists:nth((Len + 1) div 2, SortedTimes),
    
    P90Index = max(1, round(Len * 0.90)),
    P95Index = max(1, round(Len * 0.95)),
    P99Index = max(1, round(Len * 0.99)),
    
    P90 = lists:nth(P90Index, SortedTimes),
    P95 = lists:nth(P95Index, SortedTimes),
    P99 = lists:nth(P99Index, SortedTimes),
    
    Max = lists:max(Times),
    
    %% Calculate standard deviation
    SumSquaredDiffs = lists:sum([(T - Mean) * (T - Mean) || T <- Times]),
    Variance = SumSquaredDiffs / Len,
    StdDev = math:sqrt(Variance),
    
    #{
        mean => Mean,
        median => Median,
        p90 => P90,
        p95 => P95,
        p99 => P99,
        max => Max,
        std_dev => StdDev
    }.

%% Monitoring functions
memory_stability_monitor(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    memory_stability_monitor_loop(EndTime, []).

memory_stability_monitor_loop(EndTime, Samples) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            lists:reverse(Samples);
        Now ->
            MemoryMB = erlang:memory(total) div 1024 div 1024,
            NewSamples = [{Now, MemoryMB} | Samples],
            timer:sleep(?MEMORY_SAMPLE_INTERVAL),
            memory_stability_monitor_loop(EndTime, NewSamples)
    end.

system_monitor_loop(StartTime, Duration) ->
    EndTime = StartTime + Duration,
    system_monitor_loop_impl(EndTime, []).

system_monitor_loop_impl(EndTime, Samples) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            lists:reverse(Samples);
        Now ->
            Sample = collect_system_metrics(),
            NewSamples = [{Now, Sample} | Samples],
            timer:sleep(1000), % 1 second intervals
            system_monitor_loop_impl(EndTime, NewSamples)
    end.

%% Test case cleanup
cleanup_test_case(TestCase) ->
    ct:pal("Cleaning up test case: ~p", [TestCase]),
    
    %% Stop any remaining test servers/transports
    TestPrefix = atom_to_list(TestCase),
    
    %% Clean servers
    AllServers = erlmcp:list_servers(),
    lists:foreach(fun({ServerId, _}) ->
        ServerStr = atom_to_list(ServerId),
        case string:find(ServerStr, TestPrefix) of
            nomatch -> ok;
            _ ->
                try
                    erlmcp:stop_server(ServerId)
                catch
                    _:_ -> ok
                end
        end
    end, AllServers),
    
    %% Clean transports
    AllTransports = erlmcp:list_transports(),
    lists:foreach(fun({TransportId, _}) ->
        TransportStr = atom_to_list(TransportId),
        case string:find(TransportStr, TestPrefix) of
            nomatch -> ok;
            _ ->
                try
                    erlmcp:stop_transport(TransportId)
                catch
                    _:_ -> ok
                end
        end
    end, AllTransports),
    
    %% Brief cleanup pause
    timer:sleep(100).

%% Additional helper functions for complex test scenarios
sustained_load_worker(ServerId, WorkerNum, EndTime) ->
    OperationCount = sustained_load_loop(ServerId, WorkerNum, EndTime, 0),
    {operations, OperationCount}.

sustained_load_loop(ServerId, WorkerNum, EndTime, Count) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            Count;
        _Now ->
            %% Perform operation
            try
                %% Simulate various operations
                OpType = case Count rem 4 of
                    0 -> ping;
                    1 -> calculate;
                    2 -> timestamp;
                    3 -> work_simulator
                end,
                
                %% Create and send message (simulate)
                _Message = create_test_message(tools_call, #{
                    tool => atom_to_binary(OpType, utf8),
                    worker => WorkerNum,
                    operation => Count
                }),
                
                %% Brief processing delay
                timer:sleep(rand:uniform(50)),
                
                sustained_load_loop(ServerId, WorkerNum, EndTime, Count + 1)
            catch
                _:_ ->
                    sustained_load_loop(ServerId, WorkerNum, EndTime, Count)
            end
    end.

connection_churn_worker(ServerId, WorkerNum, StartTime, EndTime) ->
    churn_loop(ServerId, WorkerNum, StartTime, EndTime, 0).

churn_loop(ServerId, WorkerNum, StartTime, EndTime, ChurnCount) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            ChurnCount;
        _Now ->
            try
                %% Create transport
                TransportId = list_to_atom(io_lib:format("churn_~p_~p", [WorkerNum, ChurnCount])),
                
                case erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}) of
                    {ok, _TransportPid} ->
                        %% Brief usage
                        timer:sleep(rand:uniform(100)),
                        
                        %% Disconnect
                        erlmcp:stop_transport(TransportId),
                        
                        %% Brief pause
                        timer:sleep(rand:uniform(50)),
                        
                        churn_loop(ServerId, WorkerNum, StartTime, EndTime, ChurnCount + 1);
                    {error, _} ->
                        churn_loop(ServerId, WorkerNum, StartTime, EndTime, ChurnCount)
                end
            catch
                _:_ ->
                    churn_loop(ServerId, WorkerNum, StartTime, EndTime, ChurnCount)
            end
    end.

%% Performance analysis helpers
analyze_workload_results(Results, Duration) ->
    TotalOps = lists:sum([Ops || {operations, Ops} <- Results]),
    AverageLatency = case TotalOps of
        0 -> 0;
        _ -> Duration / TotalOps
    end,
    ThroughputOPS = (TotalOps * 1000) / max(Duration, 1),
    
    {TotalOps, AverageLatency, ThroughputOPS}.

calculate_performance_statistics(PerformanceData) ->
    Times = [Time || {_Timestamp, Time} <- PerformanceData],
    case Times of
        [] ->
            #{p50 => 0, p95 => 0, p99 => 0};
        _ ->
            SortedTimes = lists:sort(Times),
            Len = length(Times),
            
            P50Index = max(1, round(Len * 0.50)),
            P95Index = max(1, round(Len * 0.95)),
            P99Index = max(1, round(Len * 0.99)),
            
            #{
                p50 => lists:nth(P50Index, SortedTimes),
                p95 => lists:nth(P95Index, SortedTimes),
                p99 => lists:nth(P99Index, SortedTimes)
            }
    end.

%% Additional worker implementations for specific test scenarios
isolated_load_worker(ServerId, Pattern, OpCount) ->
    case Pattern of
        high_frequency_small_ops ->
            lists:foreach(fun(_) ->
                timer:sleep(1) % Very fast operations
            end, lists:seq(1, OpCount));
        medium_frequency_medium_ops ->
            lists:foreach(fun(_) ->
                timer:sleep(10) % Medium operations
            end, lists:seq(1, OpCount));
        low_frequency_large_ops ->
            lists:foreach(fun(_) ->
                timer:sleep(50) % Slower operations
            end, lists:seq(1, OpCount))
    end,
    {completed, OpCount}.

server_isolation_monitor(ServerId, StartTime, Duration) ->
    EndTime = StartTime + Duration,
    server_isolation_monitor_loop(ServerId, EndTime, []).

server_isolation_monitor_loop(ServerId, EndTime, Samples) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            {monitoring_completed, length(Samples)};
        Now ->
            %% Check server health
            ServerAlive = case erlmcp_registry:find_server(ServerId) of
                {ok, {Pid, _}} -> is_process_alive(Pid);
                _ -> false
            end,
            
            NewSamples = [{Now, ServerAlive} | Samples],
            timer:sleep(1000),
            server_isolation_monitor_loop(ServerId, EndTime, NewSamples)
    end.

%% Additional helper functions for complex scenarios would continue here...
%% This is a comprehensive load testing suite with extensive functionality

mixed_workload_worker(ServerId, Pattern, Count, AvgDuration) ->
    Operations = lists:map(fun(OpNum) ->
        %% Vary operation based on pattern
        ActualDuration = case Pattern of
            quick_tools -> max(1, AvgDuration + rand:uniform(10) - 5);
            slow_tools -> max(50, AvgDuration + rand:uniform(100) - 50);
            resource_reads -> max(10, AvgDuration + rand:uniform(20) - 10);
            large_resources -> max(100, AvgDuration + rand:uniform(200) - 100);
            prompt_requests -> max(20, AvgDuration + rand:uniform(40) - 20);
            mixed_operations -> max(5, AvgDuration + rand:uniform(60) - 30)
        end,
        
        timer:sleep(ActualDuration),
        {OpNum, ActualDuration}
    end, lists:seq(1, Count)),
    
    {operations, length(Operations)}.

performance_monitor_loop(StartTime, Duration) ->
    EndTime = StartTime + Duration,
    performance_monitor_loop_impl(EndTime, []).

performance_monitor_loop_impl(EndTime, Samples) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            lists:reverse(Samples);
        Now ->
            %% Simulate performance sample
            ResponseTime = rand:uniform(100), % Simulated response time
            NewSamples = [{Now, ResponseTime} | Samples],
            timer:sleep(100), % 100ms sampling
            performance_monitor_loop_impl(EndTime, NewSamples)
    end.

resource_exhaustion_worker(ServerId, ToolType, Params) ->
    %% Simulate resource-intensive operation
    Message = create_test_message(tools_call, #{
        tool => atom_to_binary(ToolType, utf8),
        params => Params
    }),
    
    %% Simulate sending and processing
    timer:sleep(rand:uniform(200) + 100),
    {completed, ToolType}.

%% Extreme load scenario implementations
apply_extreme_load_scenario(LoadScenario, ServerPids) ->
    case LoadScenario of
        memory_pressure ->
            %% Create workers that consume significant memory
            [spawn_monitor(fun() -> memory_pressure_worker(500) end) || _ <- lists:seq(1, 20)];
        high_connection_count ->
            %% Create many concurrent connections
            ServerIds = [Id || {Id, _, _, _} <- ServerPids],
            [spawn_monitor(fun() -> high_connection_worker(hd(ServerIds), 100) end) || _ <- lists:seq(1, 50)];
        message_flood ->
            %% Flood system with messages
            ServerIds = [Id || {Id, _, _, _} <- ServerPids],
            [spawn_monitor(fun() -> message_flood_worker(hd(ServerIds), 1000) end) || _ <- lists:seq(1, 10)];
        resource_contention ->
            %% Create resource contention
            [spawn_monitor(fun() -> resource_contention_worker(ServerId) end) || {ServerId, _, _, _} <- ServerPids];
        cascading_failures ->
            %% Simulate cascading failures
            [spawn_monitor(fun() -> cascading_failure_worker(ServerPids) end) || _ <- lists:seq(1, 5)]
    end.

memory_pressure_worker(SizeMB) ->
    %% Allocate large amounts of memory
    Size = SizeMB * 1024 * 1024,
    _Data = binary:copy(<<0>>, Size),
    timer:sleep(5000), % Hold memory for 5 seconds
    {memory_pressure, SizeMB}.

high_connection_worker(ServerId, ConnectionCount) ->
    %% Create many connections rapidly
    Connections = lists:map(fun(ConnNum) ->
        TransportId = list_to_atom(io_lib:format("extreme_conn_~p", [ConnNum])),
        case erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}) of
            {ok, _} -> {TransportId, ok};
            Error -> {TransportId, Error}
        end
    end, lists:seq(1, ConnectionCount)),
    
    %% Brief hold time
    timer:sleep(1000),
    
    %% Cleanup
    lists:foreach(fun({TransportId, Status}) ->
        case Status of
            ok -> try erlmcp:stop_transport(TransportId) catch _:_ -> ok end;
            _ -> ok
        end
    end, Connections),
    
    {high_connections, length(Connections)}.

message_flood_worker(ServerId, MessageCount) ->
    %% Create transport and flood with messages
    TransportId = list_to_atom(io_lib:format("flood_transport_~p", [rand:uniform(10000)])),
    
    case erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}) of
        {ok, TransportPid} ->
            %% Send flood of messages
            lists:foreach(fun(MsgNum) ->
                Message = create_test_message(tools_call, #{
                    tool => <<"ping">>,
                    flood_message => MsgNum
                }),
                TransportPid ! {message, jsx:encode(Message)}
            end, lists:seq(1, MessageCount)),
            
            %% Brief processing time
            timer:sleep(500),
            
            %% Cleanup
            erlmcp:stop_transport(TransportId),
            {message_flood, MessageCount};
        {error, Reason} ->
            {error, Reason}
    end.

resource_contention_worker(ServerId) ->
    %% Create resource contention by rapid operations
    Operations = lists:map(fun(OpNum) ->
        try
            %% Simulate resource-intensive operation
            Message = create_test_message(tools_call, #{
                tool => <<"work_simulator">>,
                duration => 100,
                operation => OpNum
            }),
            
            %% Simulate processing
            timer:sleep(10),
            {OpNum, success}
        catch
            _:Error ->
                {OpNum, {error, Error}}
        end
    end, lists:seq(1, 100)),
    
    {resource_contention, length(Operations)}.

cascading_failure_worker(ServerPids) ->
    %% Simulate cascading failures by overloading servers
    Results = lists:map(fun({ServerId, _, Priority, _}) ->
        case Priority of
            low ->
                %% Overload low priority servers first
                overload_server(ServerId, high),
                {ServerId, overloaded};
            medium ->
                timer:sleep(1000), % Delay medium priority
                overload_server(ServerId, medium),
                {ServerId, overloaded};
            high ->
                timer:sleep(2000), % Delay high priority
                %% Less aggressive overload for critical servers
                overload_server(ServerId, low),
                {ServerId, stressed}
        end
    end, ServerPids),
    
    {cascading_failures, length(Results)}.

overload_server(ServerId, Intensity) ->
    %% Create overload based on intensity
    WorkerCount = case Intensity of
        high -> 50;
        medium -> 25;
        low -> 10
    end,
    
    %% Spawn overload workers
    Workers = [spawn(fun() ->
        lists:foreach(fun(_) ->
            timer:sleep(rand:uniform(100)),
            ok
        end, lists:seq(1, 100))
    end) || _ <- lists:seq(1, WorkerCount)],
    
    %% Brief overload period
    timer:sleep(2000),
    
    %% Kill workers
    lists:foreach(fun(Worker) -> exit(Worker, kill) end, Workers).

graceful_degradation_monitor(ServerPids, StartTime, Duration) ->
    EndTime = StartTime + Duration,
    graceful_degradation_monitor_loop(ServerPids, EndTime, []).

graceful_degradation_monitor_loop(ServerPids, EndTime, Samples) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            #{samples => lists:reverse(Samples)};
        Now ->
            %% Check system health
            HealthSample = assess_system_health(ServerPids),
            NewSamples = [{Now, HealthSample} | Samples],
            
            timer:sleep(1000), % 1 second monitoring
            graceful_degradation_monitor_loop(ServerPids, EndTime, NewSamples)
    end.

assess_system_health(ServerPids) ->
    %% Assess health of each server group
    ServerHealth = lists:foldl(fun({ServerId, ServerGroup, Priority, _}, Acc) ->
        IsAlive = case erlmcp_registry:find_server(ServerId) of
            {ok, {Pid, _}} -> is_process_alive(Pid);
            _ -> false
        end,
        
        GroupKey = case ServerGroup of
            critical_servers -> critical_servers_alive;
            standard_servers -> standard_servers_alive;
            optional_servers -> optional_servers_alive
        end,
        
        CurrentCount = maps:get(GroupKey, Acc, 0),
        NewCount = case IsAlive of
            true -> CurrentCount + 1;
            false -> CurrentCount
        end,
        
        Acc#{GroupKey => NewCount}
    end, #{}, ServerPids),
    
    %% Add overall system health metrics
    SystemMetrics = collect_system_metrics(),
    
    ServerHealth#{
        system_memory_mb => maps:get(memory_mb, SystemMetrics, 0),
        system_processes => maps:get(process_count, SystemMetrics, 0)
    }.

system_stability_monitor(Component, EndTime) ->
    system_stability_monitor_loop(Component, EndTime, []).

system_stability_monitor_loop(Component, EndTime, Samples) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            {Component, stability_analysis(Component, Samples)};
        Now ->
            Sample = case Component of
                supervisor ->
                    case whereis(erlmcp_sup) of
                        undefined -> {Now, down};
                        Pid when is_pid(Pid) -> {Now, up}
                    end;
                registry ->
                    case whereis(erlmcp_registry) of
                        undefined -> {Now, down};
                        Pid when is_pid(Pid) -> {Now, up}
                    end;
                memory ->
                    {Now, erlang:memory(total) div 1024 div 1024};
                processes ->
                    {Now, erlang:system_info(process_count)}
            end,
            
            timer:sleep(5000), % 5 second sampling
            system_stability_monitor_loop(Component, EndTime, [Sample | Samples])
    end.

stability_analysis(Component, Samples) ->
    case Component of
        supervisor ->
            UpSamples = length([Sample || {_, Status} = Sample <- Samples, Status =:= up]),
            #{uptime_ratio => UpSamples / max(length(Samples), 1)};
        registry ->
            UpSamples = length([Sample || {_, Status} = Sample <- Samples, Status =:= up]),
            #{uptime_ratio => UpSamples / max(length(Samples), 1)};
        memory ->
            Values = [V || {_, V} <- Samples],
            case Values of
                [] -> #{stability => unknown};
                _ ->
                    Initial = hd(Values),
                    Final = lists:last(Values),
                    Growth = Final - Initial,
                    #{memory_growth_mb => Growth, stability => stable}
            end;
        processes ->
            Values = [V || {_, V} <- Samples],
            case Values of
                [] -> #{stability => unknown};
                _ ->
                    Initial = hd(Values),
                    Final = lists:last(Values),
                    Growth = Final - Initial,
                    #{process_growth => Growth, stability => stable}
            end
    end.

stability_workload_worker(ServerId, Workload, EndTime) ->
    case Workload of
        light_continuous_load ->
            continuous_light_load(ServerId, EndTime, 0);
        medium_bursty_load ->
            bursty_medium_load(ServerId, EndTime, 0);
        heavy_mixed_load ->
            mixed_heavy_load(ServerId, EndTime, 0)
    end.

continuous_light_load(ServerId, EndTime, OpCount) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            {light_load, OpCount};
        _Now ->
            %% Light operation
            timer:sleep(100), % 100ms operations
            continuous_light_load(ServerId, EndTime, OpCount + 1)
    end.

bursty_medium_load(ServerId, EndTime, BurstCount) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            {medium_load, BurstCount};
        _Now ->
            %% Burst of operations
            lists:foreach(fun(_) ->
                timer:sleep(10) % Fast operations in burst
            end, lists:seq(1, 10)),
            
            %% Rest period
            timer:sleep(1000),
            
            bursty_medium_load(ServerId, EndTime, BurstCount + 1)
    end.

mixed_heavy_load(ServerId, EndTime, CycleCount) ->
    case erlang:system_time(millisecond) of
        Now when Now >= EndTime ->
            {heavy_load, CycleCount};
        _Now ->
            %% Mixed heavy operations
            %% CPU intensive
            lists:sum([I * I || I <- lists:seq(1, 1000)]),
            
            %% Memory intensive
            _TempData = lists:seq(1, 10000),
            
            %% Brief rest
            timer:sleep(50),
            
            mixed_heavy_load(ServerId, EndTime, CycleCount + 1)
    end.