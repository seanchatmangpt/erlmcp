%%%-------------------------------------------------------------------
%%% @doc
%%% TCP Transport Benchmark Module
%%%
%%% Measures TCP transport performance:
%%% - Throughput (messages/sec) across message sizes
%%% - Latency (p50, p95, p99 in microseconds)
%%% - Memory usage per connection (heap MiB)
%%% - Sustained load over 30 seconds
%%%
%%% Workloads: 1KB, 10KB, 100KB, 1MB messages
%%% Baseline target: 43K msg/s @ 4KB (from existing benchmarks)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_tcp).

-export([run/1, run_all/0, get_baseline/0]).

-include_lib("kernel/include/logger.hrl").

-define(DURATION_SEC, 30).
-define(WARMUP_SEC, 5).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run all TCP benchmark workloads
-spec run_all() -> ok.
run_all() ->
    ct:pal("=== TCP Transport Benchmarks ===~n"),

    Workloads = [{tcp_1kb, 1024}, {tcp_10kb, 10240}, {tcp_100kb, 102400}, {tcp_1mb, 1048576}],

    Results =
        lists:map(fun({WorkloadId, MessageSize}) ->
                     run(#{workload_id => WorkloadId,
                           message_size => MessageSize,
                           duration_s => ?DURATION_SEC,
                           warmup_s => ?WARMUP_SEC})
                  end,
                  Workloads),

    print_summary(Results),
    ok.

%% @doc Run specific TCP benchmark workload
-spec run(map()) -> map().
run(#{workload_id := WorkloadId, message_size := MessageSize} = Config) ->
    Duration = maps:get(duration_s, Config, ?DURATION_SEC),
    Warmup = maps:get(warmup_s, Config, ?WARMUP_SEC),

    ct:pal("~nRunning TCP benchmark: ~p (message size: ~p bytes)~n", [WorkloadId, MessageSize]),

    % Start TCP server
    Port = 19000 + rand:uniform(1000),
    {ok, ServerPid} =
        erlmcp_transport_tcp:start_server(#{transport_id =>
                                                list_to_atom("tcp_bench_server_"
                                                             ++ atom_to_list(WorkloadId)),
                                            server_id =>
                                                list_to_atom("tcp_bench_server_"
                                                             ++ atom_to_list(WorkloadId)),
                                            owner => self(),
                                            port => Port,
                                            max_connections => infinity}),

    % Start TCP client
    {ok, ClientPid} =
        erlmcp_transport_tcp:start_client(#{transport_id =>
                                                list_to_atom("tcp_bench_client_"
                                                             ++ atom_to_list(WorkloadId)),
                                            owner => self(),
                                            host => "localhost",
                                            port => Port}),

    % Wait for connection
    receive
        {transport_connected, ClientPid} ->
            ok
    after 5000 ->
        ct:fail("TCP client connection timeout")
    end,

    {ok, ClientState} = gen_server:call(ClientPid, get_state),

    % Create test message
    TestMessage = create_test_message(MessageSize),
    EncodedMessage = jsx:encode(TestMessage),

    % Warmup phase
    ct:pal("Warmup phase (~p seconds)...~n", [Warmup]),
    warmup(ClientState, EncodedMessage, Warmup),

    % Benchmark phase
    ct:pal("Benchmark phase (~p seconds)...~n", [Duration]),
    StartTime = erlang:monotonic_time(millisecond),
    {MessageCount, LatencySamples} =
        benchmark_loop(ClientState, EncodedMessage, StartTime, Duration * 1000, 0, []),
    EndTime = erlang:monotonic_time(millisecond),

    % Calculate metrics
    ElapsedMs = EndTime - StartTime,
    ThroughputMsgPerSec = MessageCount * 1000 div ElapsedMs,

    % Calculate latency percentiles
    SortedLatencies = lists:sort(LatencySamples),
    {P50, P95, P99} = calculate_percentiles(SortedLatencies),

    % Get memory usage
    {ok, Info} = gen_server:call(ClientPid, get_state),
    ProcessInfo = erlang:process_info(ClientPid, [memory, heap_size]),
    MemoryBytes = proplists:get_value(memory, ProcessInfo, 0),
    HeapSize = proplists:get_value(heap_size, ProcessInfo, 0),
    MemoryMiB = MemoryBytes / (1024 * 1024),

    Result =
        #{workload_id => WorkloadId,
          transport => tcp,
          message_size_bytes => MessageSize,
          duration_s => Duration,
          message_count => MessageCount,
          throughput_msg_per_s => ThroughputMsgPerSec,
          latency_p50_us => P50,
          latency_p95_us => P95,
          latency_p99_us => P99,
          memory_heap_mib_per_conn => MemoryMiB,
          scope => per_connection_heap,
          precision => microseconds},

    % Cleanup
    erlmcp_transport_tcp:close(ClientState),
    gen_server:stop(ServerPid),

    print_result(Result),
    Result.

%% @doc Get baseline performance target
-spec get_baseline() -> map().
get_baseline() ->
    #{transport => tcp,
      message_size_bytes => 4096,
      throughput_msg_per_s => 43000,
      latency_p99_us => 5000,
      source => <<"erlmcp v1.5.0 network benchmarks">>}.

%%====================================================================
%% Internal Functions
%%====================================================================

warmup(ClientState, Message, WarmupSec) ->
    EndTime = erlang:monotonic_time(millisecond) + WarmupSec * 1000,
    warmup_loop(ClientState, Message, EndTime).

warmup_loop(ClientState, Message, EndTime) ->
    case erlang:monotonic_time(millisecond) < EndTime of
        true ->
            erlmcp_transport_tcp:send(ClientState, Message),
            warmup_loop(ClientState, Message, EndTime);
        false ->
            ok
    end.

benchmark_loop(ClientState, Message, StartTime, DurationMs, Count, LatencySamples) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,

    case Elapsed < DurationMs of
        true ->
            % Measure latency for this message
            SendStart = erlang:monotonic_time(microsecond),
            ok = erlmcp_transport_tcp:send(ClientState, Message),
            SendEnd = erlang:monotonic_time(microsecond),
            Latency = SendEnd - SendStart,

            % Sample latency every 100 messages to avoid memory overhead
            NewSamples =
                case Count rem 100 of
                    0 ->
                        [Latency | LatencySamples];
                    _ ->
                        LatencySamples
                end,

            benchmark_loop(ClientState, Message, StartTime, DurationMs, Count + 1, NewSamples);
        false ->
            {Count, LatencySamples}
    end.

create_test_message(TargetSize) ->
    % Create JSON-RPC message with payload approaching target size
    % Account for JSON overhead
    BaseMessage =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => <<"benchmark/test">>,
          <<"params">> => #{<<"data">> => <<>>},
          <<"id">> => 1},

    BaseSize = byte_size(jsx:encode(BaseMessage)),
    PayloadSize = max(0, TargetSize - BaseSize - 100), % Leave margin for JSON overhead

    Payload = binary:copy(<<"x">>, PayloadSize),

    BaseMessage#{<<"params">> => #{<<"data">> => base64:encode(Payload)}}.

calculate_percentiles([]) ->
    {0, 0, 0};
calculate_percentiles(SortedList) ->
    Len = length(SortedList),
    P50_Idx = max(1, Len * 50 div 100),
    P95_Idx = max(1, Len * 95 div 100),
    P99_Idx = max(1, Len * 99 div 100),

    P50 = lists:nth(P50_Idx, SortedList),
    P95 = lists:nth(P95_Idx, SortedList),
    P99 = lists:nth(P99_Idx, SortedList),

    {P50, P95, P99}.

print_result(#{workload_id := WorkloadId,
               throughput_msg_per_s := Throughput,
               latency_p50_us := P50,
               latency_p95_us := P95,
               latency_p99_us := P99,
               memory_heap_mib_per_conn := Memory}) ->
    ct:pal("~n=== Results: ~p ===~n", [WorkloadId]),
    ct:pal("Throughput:  ~p msg/s~n", [Throughput]),
    ct:pal("Latency p50: ~p μs~n", [P50]),
    ct:pal("Latency p95: ~p μs~n", [P95]),
    ct:pal("Latency p99: ~p μs~n", [P99]),
    ct:pal("Memory:      ~.2f MiB/conn~n", [Memory]),
    ok.

print_summary(Results) ->
    ct:pal("~n~n=== TCP Benchmark Summary ===~n"),
    ct:pal("~-15s ~-12s ~-12s ~-12s ~-12s~n",
           ["Workload", "Throughput", "p50 (μs)", "p99 (μs)", "Memory"]),
    ct:pal("~s~n", [lists:duplicate(65, $-)]),

    lists:foreach(fun(#{workload_id := Id,
                        throughput_msg_per_s := Throughput,
                        latency_p50_us := P50,
                        latency_p99_us := P99,
                        memory_heap_mib_per_conn := Mem}) ->
                     ct:pal("~-15s ~12w ~12w ~12w ~10.2f MB~n", [Id, Throughput, P50, P99, Mem])
                  end,
                  Results),

    % Compare to baseline
    Baseline = get_baseline(),
    BaselineThroughput = maps:get(throughput_msg_per_s, Baseline),

    % Find 4KB result for comparison
    Result4KB =
        lists:filter(fun(#{message_size_bytes := Size}) -> Size >= 4000 andalso Size =< 5000 end,
                     Results),

    case Result4KB of
        [#{throughput_msg_per_s := Actual}] ->
            Regression = (BaselineThroughput - Actual) / BaselineThroughput,
            ct:pal("~nBaseline comparison (4KB): ~p msg/s~n", [BaselineThroughput]),
            ct:pal("Actual: ~p msg/s~n", [Actual]),
            ct:pal("Regression: ~.1f%~n", [Regression * 100]),

            case Regression > 0.1 of
                true ->
                    ct:pal("WARNING: Performance regression > 10%~n");
                false ->
                    ct:pal("Performance: PASS~n")
            end;
        _ ->
            ct:pal("~nNo 4KB result for baseline comparison~n")
    end,
    ok.
