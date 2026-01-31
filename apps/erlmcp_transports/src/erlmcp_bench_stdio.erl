%%%-------------------------------------------------------------------
%%% @doc
%%% STDIO Transport Benchmark Module
%%%
%%% Measures STDIO transport performance:
%%% - Throughput (messages/sec) via stdin/stdout
%%% - Latency (p50, p95, p99 in microseconds)
%%% - Process I/O overhead
%%% - Line buffering impact
%%%
%%% Note: STDIO is typically slower due to process I/O limits
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_stdio).

-export([run/1, run_all/0, get_baseline/0]).

-include_lib("kernel/include/logger.hrl").

-define(DURATION_SEC, 30).
-define(WARMUP_SEC, 5).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run all STDIO benchmark workloads
-spec run_all() -> ok.
run_all() ->
    ct:pal("=== STDIO Transport Benchmarks ===~n"),

    Workloads = [
        {stdio_1kb, 1024},
        {stdio_10kb, 10240},
        {stdio_100kb, 102400}
    ],

    Results = lists:map(fun({WorkloadId, MessageSize}) ->
        run(#{
            workload_id => WorkloadId,
            message_size => MessageSize,
            duration_s => ?DURATION_SEC,
            warmup_s => ?WARMUP_SEC
        })
    end, Workloads),

    print_summary(Results),
    ok.

%% @doc Run specific STDIO benchmark workload
-spec run(map()) -> map().
run(#{workload_id := WorkloadId, message_size := MessageSize} = Config) ->
    Duration = maps:get(duration_s, Config, ?DURATION_SEC),
    Warmup = maps:get(warmup_s, Config, ?WARMUP_SEC),

    ct:pal("~nRunning STDIO benchmark: ~p (message size: ~p bytes)~n",
           [WorkloadId, MessageSize]),

    % Start STDIO transport in test mode
    {ok, TransportPid} = erlmcp_transport_stdio:start_link(self(), #{
        transport_id => list_to_atom("stdio_bench_" ++ atom_to_list(WorkloadId)),
        test_mode => true
    }),

    % Create test message
    TestMessage = create_test_message(MessageSize),
    EncodedMessage = jsx:encode(TestMessage),

    % Warmup phase
    ct:pal("Warmup phase (~p seconds)...~n", [Warmup]),
    warmup(TransportPid, EncodedMessage, Warmup),

    % Benchmark phase
    ct:pal("Benchmark phase (~p seconds)...~n", [Duration]),
    StartTime = erlang:monotonic_time(millisecond),
    {MessageCount, LatencySamples} = benchmark_loop(
        TransportPid,
        EncodedMessage,
        StartTime,
        Duration * 1000,
        0,
        []
    ),
    EndTime = erlang:monotonic_time(millisecond),

    % Calculate metrics
    ElapsedMs = EndTime - StartTime,
    ThroughputMsgPerSec = (MessageCount * 1000) div ElapsedMs,

    % Calculate latency percentiles
    SortedLatencies = lists:sort(LatencySamples),
    {P50, P95, P99} = calculate_percentiles(SortedLatencies),

    % Get memory usage
    ProcessInfo = erlang:process_info(TransportPid, [memory]),
    MemoryBytes = proplists:get_value(memory, ProcessInfo, 0),
    MemoryMiB = MemoryBytes / (1024 * 1024),

    Result = #{
        workload_id => WorkloadId,
        transport => stdio,
        message_size_bytes => MessageSize,
        duration_s => Duration,
        message_count => MessageCount,
        throughput_msg_per_s => ThroughputMsgPerSec,
        latency_p50_us => P50,
        latency_p95_us => P95,
        latency_p99_us => P99,
        memory_heap_mib_per_conn => MemoryMiB,
        scope => per_connection_heap,
        precision => microseconds
    },

    % Cleanup
    erlmcp_transport_stdio:close(TransportPid),

    print_result(Result),
    Result.

%% @doc Get baseline performance target
-spec get_baseline() -> map().
get_baseline() ->
    #{
        transport => stdio,
        message_size_bytes => 4096,
        throughput_msg_per_s => 10000,
        latency_p99_us => 15000,
        source => <<"estimated I/O limited">>
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

warmup(TransportPid, Message, WarmupSec) ->
    EndTime = erlang:monotonic_time(millisecond) + (WarmupSec * 1000),
    warmup_loop(TransportPid, Message, EndTime).

warmup_loop(TransportPid, Message, EndTime) ->
    case erlang:monotonic_time(millisecond) < EndTime of
        true ->
            erlmcp_transport_stdio:send(TransportPid, Message),
            warmup_loop(TransportPid, Message, EndTime);
        false ->
            ok
    end.

benchmark_loop(TransportPid, Message, StartTime, DurationMs, Count, LatencySamples) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,

    case Elapsed < DurationMs of
        true ->
            % Measure send latency
            SendStart = erlang:monotonic_time(microsecond),
            ok = erlmcp_transport_stdio:send(TransportPid, Message),
            SendEnd = erlang:monotonic_time(microsecond),
            Latency = SendEnd - SendStart,

            % Sample latency every 100 messages
            NewSamples = case Count rem 100 of
                0 -> [Latency | LatencySamples];
                _ -> LatencySamples
            end,

            benchmark_loop(TransportPid, Message, StartTime, DurationMs, Count + 1, NewSamples);
        false ->
            {Count, LatencySamples}
    end.

create_test_message(TargetSize) ->
    BaseMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"benchmark/stdio">>,
        <<"params">> => #{
            <<"data">> => <<>>
        },
        <<"id">> => 1
    },

    BaseSize = byte_size(jsx:encode(BaseMessage)),
    PayloadSize = max(0, TargetSize - BaseSize - 100),
    Payload = binary:copy(<<"i">>, PayloadSize),

    BaseMessage#{
        <<"params">> => #{
            <<"data">> => base64:encode(Payload)
        }
    }.

calculate_percentiles([]) ->
    {0, 0, 0};
calculate_percentiles(SortedList) ->
    Len = length(SortedList),
    P50_Idx = max(1, (Len * 50) div 100),
    P95_Idx = max(1, (Len * 95) div 100),
    P99_Idx = max(1, (Len * 99) div 100),

    P50 = lists:nth(P50_Idx, SortedList),
    P95 = lists:nth(P95_Idx, SortedList),
    P99 = lists:nth(P99_Idx, SortedList),

    {P50, P95, P99}.

print_result(#{
    workload_id := WorkloadId,
    throughput_msg_per_s := Throughput,
    latency_p50_us := P50,
    latency_p95_us := P95,
    latency_p99_us := P99,
    memory_heap_mib_per_conn := Memory
}) ->
    ct:pal("~n=== Results: ~p ===~n", [WorkloadId]),
    ct:pal("Throughput:  ~p msg/s~n", [Throughput]),
    ct:pal("Latency p50: ~p μs~n", [P50]),
    ct:pal("Latency p95: ~p μs~n", [P95]),
    ct:pal("Latency p99: ~p μs~n", [P99]),
    ct:pal("Memory:      ~.2f MiB~n", [Memory]),
    ok.

print_summary(Results) ->
    ct:pal("~n~n=== STDIO Benchmark Summary ===~n"),
    ct:pal("~-15s ~-12s ~-12s ~-12s ~-12s~n",
           ["Workload", "Throughput", "p50 (μs)", "p99 (μs)", "Memory"]),
    ct:pal("~s~n", [lists:duplicate(65, $-)]),

    lists:foreach(fun(#{
        workload_id := Id,
        throughput_msg_per_s := Throughput,
        latency_p50_us := P50,
        latency_p99_us := P99,
        memory_heap_mib_per_conn := Mem
    }) ->
        ct:pal("~-15s ~12w ~12w ~12w ~10.2f MB~n",
               [Id, Throughput, P50, P99, Mem])
    end, Results),

    % Compare to baseline
    Baseline = get_baseline(),
    BaselineThroughput = maps:get(throughput_msg_per_s, Baseline),
    ct:pal("~nBaseline target: ~p msg/s~n", [BaselineThroughput]),
    ok.
