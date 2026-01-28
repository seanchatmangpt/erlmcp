-module(erlmcp_routing_benchmark).

%% Performance benchmarking suite for the enhanced message routing system
%% Validates 10k+ msg/sec throughput and <1ms p99 latency requirements

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

-export([
    run_benchmark/0,
    run_benchmark/1,
    benchmark_throughput/1,
    benchmark_latency/1,
    benchmark_concurrent_routing/1,
    benchmark_memory_efficiency/1,
    generate_performance_report/1
]).

%% Configuration
-define(DEFAULT_MESSAGE_COUNT, 10000).
-define(DEFAULT_CONCURRENT_PROCESSES, 10).
-define(DEFAULT_BENCHMARK_DURATION, 60). % seconds
-define(THROUGHPUT_THRESHOLD, 10000). % messages per second
-define(LATENCY_P99_THRESHOLD, 1000). % microseconds (1ms)

%%====================================================================
%% Public API
%%====================================================================

-spec run_benchmark() -> map().
run_benchmark() ->
    run_benchmark(#{}).

-spec run_benchmark(map()) -> map().
run_benchmark(Config) ->
    ct:pal("Starting ERLMCP routing performance benchmark"),
    
    % Initialize routing system
    setup_routing_system(),
    
    % Run benchmarks
    Results = #{
        throughput => benchmark_throughput(Config),
        latency => benchmark_latency(Config),
        concurrent_routing => benchmark_concurrent_routing(Config),
        memory_efficiency => benchmark_memory_efficiency(Config)
    },
    
    % Generate report
    Report = generate_performance_report(Results),
    ct:pal("Benchmark completed~n~s", [Report]),
    
    % Cleanup
    cleanup_routing_system(),
    
    Results.

%%====================================================================
%% Throughput Benchmarking
%%====================================================================

-spec benchmark_throughput(map()) -> map().
benchmark_throughput(Config) ->
    MessageCount = maps:get(message_count, Config, ?DEFAULT_MESSAGE_COUNT),
    
    ct:pal("Running throughput benchmark with ~p messages", [MessageCount]),
    
    % Setup high-performance components
    {ok, ServerPid} = start_high_performance_server("throughput_server"),
    {ok, TransportPid} = start_high_performance_transport("throughput_transport"),
    
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => "throughput_server"},
    
    ok = erlmcp_registry:register_server("throughput_server", ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport("throughput_transport", TransportPid, TransportConfig),
    
    % Prepare messages
    Messages = generate_test_messages(MessageCount),
    
    % Measure throughput
    StartTime = erlang:system_time(microsecond),
    
    % Send all messages
    lists:foreach(fun(Message) ->
        ok = erlmcp_registry:route_to_server("throughput_server", "throughput_transport", Message)
    end, Messages),
    
    % Wait for processing completion
    ProcessedCount = wait_for_message_processing(MessageCount, 30000),
    EndTime = erlang:system_time(microsecond),
    
    Duration = (EndTime - StartTime) / 1000000, % Convert to seconds
    Throughput = ProcessedCount / Duration,
    
    Result = #{
        message_count => MessageCount,
        processed_count => ProcessedCount,
        duration_seconds => Duration,
        throughput_msg_per_sec => Throughput,
        meets_requirement => Throughput >= ?THROUGHPUT_THRESHOLD
    },
    
    ct:pal("Throughput benchmark: ~p msg/sec (requirement: ~p msg/sec)", 
           [trunc(Throughput), ?THROUGHPUT_THRESHOLD]),
    
    Result.

%%====================================================================
%% Latency Benchmarking
%%====================================================================

-spec benchmark_latency(map()) -> map().
benchmark_latency(Config) ->
    SampleCount = maps:get(latency_samples, Config, 1000),
    
    ct:pal("Running latency benchmark with ~p samples", [SampleCount]),
    
    % Setup low-latency components
    {ok, ServerPid} = start_low_latency_server("latency_server"),
    {ok, TransportPid} = start_low_latency_transport("latency_transport"),
    
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => "latency_server"},
    
    ok = erlmcp_registry:register_server("latency_server", ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport("latency_transport", TransportPid, TransportConfig),
    
    % Measure latencies
    Latencies = lists:map(fun(N) ->
        Message = create_benchmark_message(N),
        StartTime = erlang:system_time(microsecond),
        ok = erlmcp_registry:route_to_server("latency_server", "latency_transport", Message),
        
        % Wait for processing
        receive
            {benchmark_response, N} ->
                EndTime = erlang:system_time(microsecond),
                EndTime - StartTime
        after 5000 ->
            ct:fail("Latency measurement timeout for sample ~p", [N])
        end
    end, lists:seq(1, SampleCount)),
    
    % Calculate percentiles
    SortedLatencies = lists:sort(Latencies),
    P50 = percentile(SortedLatencies, 50),
    P95 = percentile(SortedLatencies, 95),
    P99 = percentile(SortedLatencies, 99),
    P999 = percentile(SortedLatencies, 99.9),
    
    Mean = lists:sum(Latencies) / length(Latencies),
    Min = lists:min(Latencies),
    Max = lists:max(Latencies),
    
    Result = #{
        sample_count => SampleCount,
        mean_us => Mean,
        min_us => Min,
        max_us => Max,
        p50_us => P50,
        p95_us => P95,
        p99_us => P99,
        p999_us => P999,
        meets_p99_requirement => P99 =< ?LATENCY_P99_THRESHOLD
    },
    
    ct:pal("Latency benchmark: P99=~p μs, P95=~p μs, Mean=~p μs (requirement: P99 <= ~p μs)", 
           [trunc(P99), trunc(P95), trunc(Mean), ?LATENCY_P99_THRESHOLD]),
    
    Result.

%%====================================================================
%% Concurrent Routing Benchmarking
%%====================================================================

-spec benchmark_concurrent_routing(map()) -> map().
benchmark_concurrent_routing(Config) ->
    ProcessCount = maps:get(concurrent_processes, Config, ?DEFAULT_CONCURRENT_PROCESSES),
    MessagesPerProcess = maps:get(messages_per_process, Config, 1000),
    
    ct:pal("Running concurrent routing benchmark with ~p processes, ~p messages each", 
           [ProcessCount, MessagesPerProcess]),
    
    % Setup multiple servers for load distribution
    ServerCount = min(ProcessCount, 5),
    Servers = setup_multiple_servers("concurrent_server", ServerCount),
    Transports = setup_multiple_transports("concurrent_transport", ProcessCount),
    
    % Launch concurrent routing processes
    StartTime = erlang:system_time(microsecond),
    
    WorkerPids = lists:map(fun(N) ->
        spawn_link(fun() ->
            concurrent_routing_worker(N, MessagesPerProcess, Servers, Transports)
        end)
    end, lists:seq(1, ProcessCount)),
    
    % Wait for all workers to complete
    lists:foreach(fun(Pid) ->
        receive
            {worker_completed, Pid, Stats} ->
                ct:pal("Worker ~p completed: ~p", [Pid, Stats])
        after 60000 ->
            exit(Pid, kill),
            ct:fail("Concurrent routing worker timeout")
        end
    end, WorkerPids),
    
    EndTime = erlang:system_time(microsecond),
    Duration = (EndTime - StartTime) / 1000000,
    
    TotalMessages = ProcessCount * MessagesPerProcess,
    OverallThroughput = TotalMessages / Duration,
    
    Result = #{
        process_count => ProcessCount,
        messages_per_process => MessagesPerProcess,
        total_messages => TotalMessages,
        duration_seconds => Duration,
        overall_throughput => OverallThroughput,
        concurrency_efficiency => OverallThroughput / ProcessCount
    },
    
    ct:pal("Concurrent routing: ~p msg/sec with ~p processes (~p msg/sec per process)", 
           [trunc(OverallThroughput), ProcessCount, trunc(OverallThroughput / ProcessCount)]),
    
    Result.

%%====================================================================
%% Memory Efficiency Benchmarking
%%====================================================================

-spec benchmark_memory_efficiency(map()) -> map().
benchmark_memory_efficiency(Config) ->
    MessageWaves = maps:get(message_waves, Config, 10),
    MessagesPerWave = maps:get(messages_per_wave, Config, 1000),
    
    ct:pal("Running memory efficiency benchmark with ~p waves of ~p messages", 
           [MessageWaves, MessagesPerWave]),
    
    % Record initial memory
    InitialMemory = erlang:memory(total),
    
    % Setup components
    {ok, ServerPid} = start_memory_efficient_server("memory_server"),
    {ok, TransportPid} = start_memory_efficient_transport("memory_transport"),
    
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    TransportConfig = #{type => stdio, server_id => "memory_server"},
    
    ok = erlmcp_registry:register_server("memory_server", ServerPid, ServerConfig),
    ok = erlmcp_registry:register_transport("memory_transport", TransportPid, TransportConfig),
    
    % Run waves of messages with memory monitoring
    MemoryMeasurements = lists:map(fun(Wave) ->
        % Send batch of messages
        Messages = generate_test_messages(MessagesPerWave),
        lists:foreach(fun(Message) ->
            ok = erlmcp_registry:route_to_server("memory_server", "memory_transport", Message)
        end, Messages),
        
        % Wait for processing
        timer:sleep(100),
        
        % Force garbage collection
        erlang:garbage_collect(),
        
        % Measure memory
        CurrentMemory = erlang:memory(total),
        {Wave, CurrentMemory}
    end, lists:seq(1, MessageWaves)),
    
    FinalMemory = erlang:memory(total),
    MaxMemory = lists:max([M || {_, M} <- MemoryMeasurements]),
    MemoryGrowth = MaxMemory - InitialMemory,
    
    % Analyze memory stability (check for leaks)
    EarlyMemory = element(2, lists:nth(3, MemoryMeasurements)),
    LateMemory = element(2, lists:last(MemoryMeasurements)),
    MemoryLeak = LateMemory - EarlyMemory,
    
    Result = #{
        initial_memory_bytes => InitialMemory,
        final_memory_bytes => FinalMemory,
        max_memory_bytes => MaxMemory,
        memory_growth_bytes => MemoryGrowth,
        memory_growth_mb => MemoryGrowth / (1024 * 1024),
        potential_leak_bytes => MemoryLeak,
        memory_stable => MemoryLeak < (10 * 1024 * 1024), % Less than 10MB growth
        measurements => MemoryMeasurements
    },
    
    ct:pal("Memory efficiency: ~p MB growth, ~p MB potential leak", 
           [trunc(MemoryGrowth / (1024 * 1024)), trunc(MemoryLeak / (1024 * 1024))]),
    
    Result.

%%====================================================================
%% Helper Functions
%%====================================================================

-spec setup_routing_system() -> ok.
setup_routing_system() ->
    % Ensure registry is running
    case whereis(erlmcp_registry) of
        undefined ->
            {ok, _} = erlmcp_registry:start_link();
        _ ->
            ok
    end,
    
    % Start router if available
    case whereis(erlmcp_router) of
        undefined ->
            case erlmcp_router:start_link() of
                {ok, _} -> ok;
                {error, _} -> ok % Router module might not be loaded
            end;
        _ ->
            ok
    end,
    
    % Initialize metrics
    case erlmcp_routing_metrics:init() of
        ok -> ok;
        {error, _} -> ok % Metrics module might not be available
    end,
    
    ok.

-spec cleanup_routing_system() -> ok.
cleanup_routing_system() ->
    % Basic cleanup - in production you'd be more thorough
    case whereis(erlmcp_router) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    ok.

-spec start_high_performance_server(string()) -> {ok, pid()}.
start_high_performance_server(ServerId) ->
    ServerPid = spawn_link(fun() ->
        high_performance_server_loop(ServerId, #{
            processing_delay => 0,
            batch_processing => true
        })
    end),
    {ok, ServerPid}.

-spec start_high_performance_transport(string()) -> {ok, pid()}.
start_high_performance_transport(TransportId) ->
    TransportPid = spawn_link(fun() ->
        high_performance_transport_loop(TransportId, #{
            ack_mode => async
        })
    end),
    {ok, TransportPid}.

-spec start_low_latency_server(string()) -> {ok, pid()}.
start_low_latency_server(ServerId) ->
    ServerPid = spawn_link(fun() ->
        low_latency_server_loop(ServerId, #{
            processing_delay => 0,
            immediate_response => true
        })
    end),
    {ok, ServerPid}.

-spec start_low_latency_transport(string()) -> {ok, pid()}.
start_low_latency_transport(TransportId) ->
    TransportPid = spawn_link(fun() ->
        low_latency_transport_loop(TransportId, #{
            immediate_ack => true
        })
    end),
    {ok, TransportPid}.

-spec start_memory_efficient_server(string()) -> {ok, pid()}.
start_memory_efficient_server(ServerId) ->
    ServerPid = spawn_link(fun() ->
        memory_efficient_server_loop(ServerId, #{
            message_buffer_limit => 100,
            garbage_collect_interval => 1000
        })
    end),
    {ok, ServerPid}.

-spec start_memory_efficient_transport(string()) -> {ok, pid()}.
start_memory_efficient_transport(TransportId) ->
    TransportPid = spawn_link(fun() ->
        memory_efficient_transport_loop(TransportId, #{
            buffer_limit => 100
        })
    end),
    {ok, TransportPid}.

-spec setup_multiple_servers(string(), non_neg_integer()) -> [string()].
setup_multiple_servers(BaseId, Count) ->
    lists:map(fun(N) ->
        ServerId = lists:flatten(io_lib:format("~s_~p", [BaseId, N])),
        {ok, ServerPid} = start_high_performance_server(ServerId),
        ServerConfig = #{capabilities => #mcp_server_capabilities{}},
        ok = erlmcp_registry:register_server(ServerId, ServerPid, ServerConfig),
        ServerId
    end, lists:seq(1, Count)).

-spec setup_multiple_transports(string(), non_neg_integer()) -> [string()].
setup_multiple_transports(BaseId, Count) ->
    lists:map(fun(N) ->
        TransportId = lists:flatten(io_lib:format("~s_~p", [BaseId, N])),
        {ok, TransportPid} = start_high_performance_transport(TransportId),
        TransportConfig = #{type => stdio},
        ok = erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig),
        TransportId
    end, lists:seq(1, Count)).

-spec generate_test_messages(non_neg_integer()) -> [map()].
generate_test_messages(Count) ->
    [create_benchmark_message(N) || N <- lists:seq(1, Count)].

-spec create_benchmark_message(non_neg_integer()) -> map().
create_benchmark_message(N) ->
    #{
        id => N,
        timestamp => erlang:system_time(microsecond),
        payload => lists:flatten(io_lib:format("benchmark_message_~p", [N])),
        size => 100 % Standardized message size
    }.

-spec wait_for_message_processing(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
wait_for_message_processing(ExpectedCount, TimeoutMs) ->
    wait_for_message_processing(ExpectedCount, TimeoutMs, 0, erlang:system_time(millisecond)).

wait_for_message_processing(ExpectedCount, TimeoutMs, ProcessedCount, StartTime) 
    when ProcessedCount >= ExpectedCount ->
    ProcessedCount;
wait_for_message_processing(ExpectedCount, TimeoutMs, ProcessedCount, StartTime) ->
    Now = erlang:system_time(millisecond),
    case Now - StartTime > TimeoutMs of
        true -> 
            ProcessedCount;
        false ->
            receive
                {message_processed, _} ->
                    wait_for_message_processing(ExpectedCount, TimeoutMs, ProcessedCount + 1, StartTime)
            after 100 ->
                wait_for_message_processing(ExpectedCount, TimeoutMs, ProcessedCount, StartTime)
            end
    end.

-spec concurrent_routing_worker(non_neg_integer(), non_neg_integer(), [string()], [string()]) -> ok.
concurrent_routing_worker(WorkerId, MessageCount, Servers, Transports) ->
    StartTime = erlang:system_time(microsecond),
    
    lists:foreach(fun(N) ->
        ServerId = lists:nth((N rem length(Servers)) + 1, Servers),
        TransportId = lists:nth((WorkerId rem length(Transports)) + 1, Transports),
        Message = create_benchmark_message(N),
        ok = erlmcp_registry:route_to_server(ServerId, TransportId, Message)
    end, lists:seq(1, MessageCount)),
    
    EndTime = erlang:system_time(microsecond),
    Duration = (EndTime - StartTime) / 1000000,
    Throughput = MessageCount / Duration,
    
    self() ! {worker_completed, self(), #{
        worker_id => WorkerId,
        messages => MessageCount,
        duration => Duration,
        throughput => Throughput
    }}.

-spec percentile([number()], number()) -> float().
percentile(SortedList, Percentile) ->
    Length = length(SortedList),
    Index = max(1, trunc(Length * (Percentile / 100))),
    lists:nth(Index, SortedList) / 1.0.

-spec generate_performance_report(map()) -> string().
generate_performance_report(Results) ->
    ThroughputResults = maps:get(throughput, Results),
    LatencyResults = maps:get(latency, Results),
    ConcurrentResults = maps:get(concurrent_routing, Results),
    MemoryResults = maps:get(memory_efficiency, Results),
    
    ThroughputStatus = case maps:get(meets_requirement, ThroughputResults) of
        true -> "✓ PASS";
        false -> "✗ FAIL"
    end,
    
    LatencyStatus = case maps:get(meets_p99_requirement, LatencyResults) of
        true -> "✓ PASS";
        false -> "✗ FAIL"
    end,
    
    MemoryStatus = case maps:get(memory_stable, MemoryResults) of
        true -> "✓ STABLE";
        false -> "⚠ UNSTABLE"
    end,
    
    io_lib:format("
================================================================================
                        ERLMCP ROUTING PERFORMANCE REPORT
================================================================================

THROUGHPUT BENCHMARK                                                    ~s
  Target:              >= ~p msg/sec
  Achieved:            ~p msg/sec
  Messages:            ~p
  Duration:            ~.2f seconds

LATENCY BENCHMARK                                                       ~s
  Target:              P99 <= ~p μs (1ms)
  P99 Latency:         ~p μs
  P95 Latency:         ~p μs
  Mean Latency:        ~p μs
  Samples:             ~p

CONCURRENT ROUTING BENCHMARK
  Processes:           ~p
  Messages/Process:    ~p
  Total Messages:      ~p
  Overall Throughput:  ~p msg/sec
  Per-Process:         ~p msg/sec

MEMORY EFFICIENCY BENCHMARK                                             ~s
  Memory Growth:       ~p MB
  Potential Leak:      ~p MB
  Max Memory:          ~p MB
  Stability:           ~s

================================================================================
", [
    ThroughputStatus,
    ?THROUGHPUT_THRESHOLD,
    trunc(maps:get(throughput_msg_per_sec, ThroughputResults)),
    maps:get(message_count, ThroughputResults),
    maps:get(duration_seconds, ThroughputResults),
    
    LatencyStatus,
    ?LATENCY_P99_THRESHOLD,
    trunc(maps:get(p99_us, LatencyResults)),
    trunc(maps:get(p95_us, LatencyResults)),
    trunc(maps:get(mean_us, LatencyResults)),
    maps:get(sample_count, LatencyResults),
    
    maps:get(process_count, ConcurrentResults),
    maps:get(messages_per_process, ConcurrentResults),
    maps:get(total_messages, ConcurrentResults),
    trunc(maps:get(overall_throughput, ConcurrentResults)),
    trunc(maps:get(concurrency_efficiency, ConcurrentResults)),
    
    MemoryStatus,
    trunc(maps:get(memory_growth_mb, MemoryResults)),
    trunc(maps:get(potential_leak_bytes, MemoryResults) / (1024 * 1024)),
    trunc(maps:get(max_memory_bytes, MemoryResults) / (1024 * 1024)),
    case maps:get(memory_stable, MemoryResults) of
        true -> "Stable";
        false -> "Unstable"
    end
]).

%%====================================================================
%% Mock Server/Transport Implementations
%%====================================================================

high_performance_server_loop(ServerId, Config) ->
    receive
        {mcp_message, TransportId, Message} ->
            % Immediate processing for high throughput
            self() ! {message_processed, Message},
            high_performance_server_loop(ServerId, Config);
        stop ->
            ok
    end.

high_performance_transport_loop(TransportId, Config) ->
    receive
        {mcp_response, ServerId, Message} ->
            % Async acknowledgment
            high_performance_transport_loop(TransportId, Config);
        stop ->
            ok
    end.

low_latency_server_loop(ServerId, Config) ->
    receive
        {mcp_message, TransportId, Message} ->
            % Immediate response for latency measurement
            MessageId = maps:get(id, Message, 0),
            self() ! {benchmark_response, MessageId},
            low_latency_server_loop(ServerId, Config);
        stop ->
            ok
    end.

low_latency_transport_loop(TransportId, Config) ->
    receive
        {mcp_response, ServerId, Message} ->
            % Immediate acknowledgment
            low_latency_transport_loop(TransportId, Config);
        stop ->
            ok
    end.

memory_efficient_server_loop(ServerId, Config) ->
    receive
        {mcp_message, TransportId, Message} ->
            % Periodic garbage collection
            case rand:uniform(100) of
                1 -> erlang:garbage_collect();
                _ -> ok
            end,
            memory_efficient_server_loop(ServerId, Config);
        stop ->
            ok
    end.

memory_efficient_transport_loop(TransportId, Config) ->
    receive
        {mcp_response, ServerId, Message} ->
            memory_efficient_transport_loop(TransportId, Config);
        stop ->
            ok
    end.