%%%-------------------------------------------------------------------
%%% @doc
%%% Server-Sent Events (SSE) Transport Benchmark Module
%%%
%%% Measures SSE transport performance:
%%% - Throughput (events/sec) for unidirectional server → client
%%% - Latency from emit to receive (p50, p95, p99)
%%% - Connection overhead
%%% - Event stream parsing performance
%%%
%%% Target: 20K+ msg/s (SSE is unidirectional, lower than bidirectional)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_sse).

-export([run/1, run_all/0, get_baseline/0]).

-include_lib("kernel/include/logger.hrl").

-define(DURATION_SEC, 30).
-define(WARMUP_SEC, 5).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run all SSE benchmark workloads
-spec run_all() -> ok.
run_all() ->
    ct:pal("=== SSE Transport Benchmarks ===~n"),

    Workloads = [
        {sse_1kb, 1024},
        {sse_10kb, 10240},
        {sse_100kb, 102400}
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

%% @doc Run specific SSE benchmark workload
-spec run(map()) -> map().
run(#{workload_id := WorkloadId, message_size := MessageSize} = Config) ->
    Duration = maps:get(duration_s, Config, ?DURATION_SEC),
    Warmup = maps:get(warmup_s, Config, ?WARMUP_SEC),

    ct:pal("~nRunning SSE benchmark: ~p (message size: ~p bytes)~n",
           [WorkloadId, MessageSize]),

    % NOTE: SSE benchmarks measure event formatting and parsing
    % Full end-to-end requires HTTP server setup

    % Create test event
    TestMessage = create_test_message(MessageSize),
    EncodedMessage = jsx:encode(TestMessage),

    % Measure SSE event formatting overhead
    StartFormat = erlang:monotonic_time(microsecond),
    _Event = format_sse_event(EncodedMessage),
    EndFormat = erlang:monotonic_time(microsecond),
    FormattingLatencyUs = EndFormat - StartFormat,

    % Warmup phase
    ct:pal("Warmup phase (~p seconds)...~n", [Warmup]),
    warmup(EncodedMessage, Warmup),

    % Benchmark phase
    ct:pal("Benchmark phase (~p seconds)...~n", [Duration]),
    StartTime = erlang:monotonic_time(millisecond),
    {EventCount, LatencySamples} = benchmark_loop(
        EncodedMessage,
        StartTime,
        Duration * 1000,
        0,
        []
    ),
    EndTime = erlang:monotonic_time(millisecond),

    % Calculate metrics
    ElapsedMs = EndTime - StartTime,
    ThroughputMsgPerSec = (EventCount * 1000) div ElapsedMs,

    % Calculate latency percentiles
    SortedLatencies = lists:sort(LatencySamples),
    {P50, P95, P99} = calculate_percentiles(SortedLatencies),

    Result = #{
        workload_id => WorkloadId,
        transport => sse,
        message_size_bytes => MessageSize,
        duration_s => Duration,
        event_count => EventCount,
        throughput_msg_per_s => ThroughputMsgPerSec,
        latency_p50_us => P50,
        latency_p95_us => P95,
        latency_p99_us => P99,
        formatting_overhead_us => FormattingLatencyUs,
        scope => event_formatting,
        precision => microseconds
    },

    print_result(Result),
    Result.

%% @doc Get baseline performance target
-spec get_baseline() -> map().
get_baseline() ->
    #{
        transport => sse,
        message_size_bytes => 4096,
        throughput_msg_per_s => 20000,
        latency_p99_us => 10000,
        source => <<"estimated unidirectional target">>
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

warmup(Message, WarmupSec) ->
    EndTime = erlang:monotonic_time(millisecond) + (WarmupSec * 1000),
    warmup_loop(Message, EndTime).

warmup_loop(Message, EndTime) ->
    case erlang:monotonic_time(millisecond) < EndTime of
        true ->
            _Event = format_sse_event(Message),
            warmup_loop(Message, EndTime);
        false ->
            ok
    end.

benchmark_loop(Message, StartTime, DurationMs, Count, LatencySamples) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,

    case Elapsed < DurationMs of
        true ->
            % Measure formatting latency
            FormatStart = erlang:monotonic_time(microsecond),
            Event = format_sse_event(Message),
            FormatEnd = erlang:monotonic_time(microsecond),

            % Measure parsing latency
            ParseStart = erlang:monotonic_time(microsecond),
            _Parsed = parse_sse_event(Event),
            ParseEnd = erlang:monotonic_time(microsecond),

            Latency = (FormatEnd - FormatStart) + (ParseEnd - ParseStart),

            % Sample latency every 100 events
            NewSamples = case Count rem 100 of
                0 -> [Latency | LatencySamples];
                _ -> LatencySamples
            end,

            benchmark_loop(Message, StartTime, DurationMs, Count + 1, NewSamples);
        false ->
            {Count, LatencySamples}
    end.

%% @doc Format message as SSE event
format_sse_event(Data) ->
    EventId = integer_to_binary(erlang:unique_integer([positive])),
    [
        <<"event: message\n">>,
        <<"id: ">>, EventId, <<"\n">>,
        <<"data: ">>, Data, <<"\n">>,
        <<"\n">>
    ].

%% @doc Parse SSE event (simplified)
parse_sse_event(Event) ->
    Binary = iolist_to_binary(Event),
    Lines = binary:split(Binary, <<"\n">>, [global]),

    parse_sse_lines(Lines, #{}).

parse_sse_lines([], Acc) ->
    Acc;
parse_sse_lines([<<>> | Rest], Acc) ->
    parse_sse_lines(Rest, Acc);
parse_sse_lines([Line | Rest], Acc) ->
    case binary:split(Line, <<": ">>) of
        [<<"event">>, EventType] ->
            parse_sse_lines(Rest, Acc#{event => EventType});
        [<<"id">>, EventId] ->
            parse_sse_lines(Rest, Acc#{id => EventId});
        [<<"data">>, Data] ->
            parse_sse_lines(Rest, Acc#{data => Data});
        _ ->
            parse_sse_lines(Rest, Acc)
    end.

create_test_message(TargetSize) ->
    BaseMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"benchmark/sse">>,
        <<"params">> => #{
            <<"data">> => <<>>
        }
    },

    BaseSize = byte_size(jsx:encode(BaseMessage)),
    PayloadSize = max(0, TargetSize - BaseSize - 100),
    Payload = binary:copy(<<"s">>, PayloadSize),

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
    formatting_overhead_us := FmtOverhead
}) ->
    ct:pal("~n=== Results: ~p ===~n", [WorkloadId]),
    ct:pal("Throughput:         ~p events/s~n", [Throughput]),
    ct:pal("Latency p50:        ~p μs~n", [P50]),
    ct:pal("Latency p95:        ~p μs~n", [P95]),
    ct:pal("Latency p99:        ~p μs~n", [P99]),
    ct:pal("Formatting overhead: ~p μs~n", [FmtOverhead]),
    ok.

print_summary(Results) ->
    ct:pal("~n~n=== SSE Benchmark Summary ===~n"),
    ct:pal("~-15s ~-12s ~-12s ~-12s~n",
           ["Workload", "Throughput", "p50 (μs)", "p99 (μs)"]),
    ct:pal("~s~n", [lists:duplicate(55, $-)]),

    lists:foreach(fun(#{
        workload_id := Id,
        throughput_msg_per_s := Throughput,
        latency_p50_us := P50,
        latency_p99_us := P99
    }) ->
        ct:pal("~-15s ~12w ~12w ~12w~n",
               [Id, Throughput, P50, P99])
    end, Results),

    % Compare to baseline
    Baseline = get_baseline(),
    BaselineThroughput = maps:get(throughput_msg_per_s, Baseline),
    ct:pal("~nBaseline target: ~p events/s~n", [BaselineThroughput]),
    ok.
