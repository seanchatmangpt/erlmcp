%%%====================================================================
%%% ERLMCP JSON BENCHMARK - OTP 28 NATIVE JSON VS JSX
%%%====================================================================
%%% Module: erlmcp_bench_json_otp28
%%% Purpose: Compare OTP 28 native json module vs jsx for MCP protocol
%%% Target: 2-3x improvement for native json on OTP 28
%%% Workloads: 1K, 10K, 100K JSON objects (MCP messages)
%%% Measures: Throughput (ops/s), latency (us), memory allocations
%%%====================================================================

-module(erlmcp_bench_json_otp28).

-export([
    run/0,
    run/1,
    run_all/0,
    workloads/0,
    benchmark_encode/2,
    benchmark_decode/2,
    benchmark_roundtrip/2
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Workload Definitions
%%====================================================================

-spec workloads() -> [map()].
workloads() ->
    [
        #{id => <<"json_1k">>, operations => 1000, message_size => small},
        #{id => <<"json_10k">>, operations => 10000, message_size => medium},
        #{id => <<"json_100k">>, operations => 100000, message_size => large}
    ].

%%====================================================================
%% Main Entry Points
%%====================================================================

-spec run() -> ok.
run() ->
    run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    Workloads = workloads(),
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, Workloads) of
        [] ->
            io:format("ERROR: Unknown workload: ~s~n", [WorkloadId]),
            {error, {unknown_workload, WorkloadId}};
        [Workload] ->
            run_workload(Workload)
    end;
run(WorkloadId) when is_list(WorkloadId) ->
    run(list_to_binary(WorkloadId)).

-spec run_all() -> ok.
run_all() ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP JSON BENCHMARK - OTP 28 NATIVE vs JSX~n"),
    io:format("==============================================~n~n"),

    OtpVersion = erlang:system_info(otp_release),
    io:format("OTP Version: ~s~n", [OtpVersion]),
    io:format("Native json module available: ~p~n~n", [has_native_json()]),

    lists:foreach(fun(Workload) ->
        run_workload(Workload)
    end, workloads()),

    io:format("~n==============================================~n"),
    io:format("JSON benchmarks complete. Results in bench/results/~n"),
    io:format("==============================================~n~n"),
    ok.

%%====================================================================
%% Workload Execution
%%====================================================================

-spec run_workload(map()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, operations := Ops, message_size := Size} = _Workload) ->
    io:format("~n--- Workload: ~s (~p ops, ~p messages) ---~n", [WorkloadId, Ops, Size]),

    %% Generate test MCP message
    TestMessage = generate_mcp_message(Size),
    
    %% Capture environment
    Env = capture_environment(),
    
    %% Warmup
    io:format("Warming up...~n"),
    warmup(TestMessage, 100),

    %% Benchmark JSX
    io:format("Benchmarking JSX...~n"),
    JsxResults = benchmark_jsx(TestMessage, Ops),

    %% Benchmark native json (if available)
    NativeResults = case has_native_json() of
        true ->
            io:format("Benchmarking native json...~n"),
            benchmark_native_json(TestMessage, Ops);
        false ->
            io:format("Native json not available (OTP < 27)~n"),
            undefined
    end,

    %% Build report
    Report = build_report(WorkloadId, Env, Size, Ops, JsxResults, NativeResults),

    %% Validate and write
    case validate_report(Report) of
        ok ->
            Timestamp = erlang:system_time(second),
            Filename = io_lib:format("bench/results/json_otp28_~s_~p.json", [WorkloadId, Timestamp]),
            write_report(Filename, Report),
            io:format("✓ Report written: ~s~n", [Filename]),
            
            %% Display comparison
            display_comparison(JsxResults, NativeResults),
            ok;
        {error, ValidationError} ->
            io:format("✗ Validation failed: ~p~n", [ValidationError]),
            {error, {validation_failed, ValidationError}}
    end.

%%====================================================================
%% Message Generation
%%====================================================================

%% Generate MCP protocol messages of different sizes
-spec generate_mcp_message(small | medium | large) -> map().
generate_mcp_message(small) ->
    %% Small MCP request (10 fields) - typical tool call
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"echo">>,
            <<"arguments">> => #{
                <<"message">> => <<"Hello, World!">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"client_id">> => <<"bench_client_001">>
            }
        }
    };

generate_mcp_message(medium) ->
    %% Medium MCP response (50 fields) - resource list response
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"resources">> => [
                generate_resource(N) || N <- lists:seq(1, 10)
            ],
            <<"metadata">> => #{
                <<"total">> => 10,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"server_version">> => <<"2.1.0">>
            }
        }
    };

generate_mcp_message(large) ->
    %% Large MCP response (1000 fields) - large tool result
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"result">> => #{
            <<"content">> => [
                #{
                    <<"type">> => <<"text">>,
                    <<"text">> => generate_large_text(100)
                }
            ],
            <<"metadata">> => #{
                <<"chunks">> => [
                    generate_chunk(N) || N <- lists:seq(1, 100)
                ],
                <<"total_size">> => 100000,
                <<"timestamp">> => erlang:system_time(millisecond)
            }
        }
    }.

-spec generate_resource(integer()) -> map().
generate_resource(N) ->
    #{
        <<"uri">> => iolist_to_binary([<<"file://resource_">>, integer_to_binary(N), <<".txt">>]),
        <<"name">> => iolist_to_binary([<<"Resource ">>, integer_to_binary(N)]),
        <<"mimeType">> => <<"text/plain">>,
        <<"description">> => <<"Test resource for benchmarking">>
    }.

-spec generate_chunk(integer()) -> map().
generate_chunk(N) ->
    #{
        <<"id">> => N,
        <<"offset">> => N * 1000,
        <<"size">> => 1000,
        <<"data">> => base64:encode(crypto:strong_rand_bytes(100))
    }.

-spec generate_large_text(integer()) -> binary().
generate_large_text(Lines) ->
    iolist_to_binary([
        [<<"Line ">>, integer_to_binary(N), <<": Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n">>]
        || N <- lists:seq(1, Lines)
    ]).

%%====================================================================
%% JSX Benchmarks
%%====================================================================

-spec benchmark_jsx(map(), pos_integer()) -> map().
benchmark_jsx(Message, Operations) ->
    %% Measure encode
    {EncodeLatencies, EncodedData} = benchmark_encode(jsx, Message, Operations),
    
    %% Measure decode
    DecodeLatencies = benchmark_decode(jsx, EncodedData, Operations),
    
    %% Measure roundtrip
    RoundtripLatencies = benchmark_roundtrip(jsx, Message, Operations),
    
    %% Calculate memory usage
    MemoryBefore = erlang:memory(total),
    _ = jsx:encode(Message),
    MemoryAfter = erlang:memory(total),
    MemoryPerOp = (MemoryAfter - MemoryBefore),
    
    #{
        library => jsx,
        encode => calculate_metrics(EncodeLatencies),
        decode => calculate_metrics(DecodeLatencies),
        roundtrip => calculate_metrics(RoundtripLatencies),
        memory_per_op_bytes => MemoryPerOp,
        encoded_size_bytes => byte_size(EncodedData)
    }.

%%====================================================================
%% Native JSON Benchmarks (OTP 27+)
%%====================================================================

-spec benchmark_native_json(map(), pos_integer()) -> map().
benchmark_native_json(Message, Operations) ->
    case has_native_json() of
        false ->
            #{library => native_json, available => false};
        true ->
            %% Measure encode
            {EncodeLatencies, EncodedData} = benchmark_encode(native, Message, Operations),
            
            %% Measure decode
            DecodeLatencies = benchmark_decode(native, EncodedData, Operations),
            
            %% Measure roundtrip
            RoundtripLatencies = benchmark_roundtrip(native, Message, Operations),
            
            %% Calculate memory usage
            MemoryBefore = erlang:memory(total),
            _ = json:encode(Message),
            MemoryAfter = erlang:memory(total),
            MemoryPerOp = (MemoryAfter - MemoryBefore),
            
            #{
                library => native_json,
                encode => calculate_metrics(EncodeLatencies),
                decode => calculate_metrics(DecodeLatencies),
                roundtrip => calculate_metrics(RoundtripLatencies),
                memory_per_op_bytes => MemoryPerOp,
                encoded_size_bytes => byte_size(EncodedData)
            }
    end.

%%====================================================================
%% Benchmark Operations
%%====================================================================

-spec benchmark_encode(jsx | native, map(), pos_integer()) -> {[non_neg_integer()], binary()}.
benchmark_encode(Library, Message, Operations) ->
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        Encoded = case Library of
            jsx -> jsx:encode(Message);
            native -> json:encode(Message)
        end,
        End = erlang:monotonic_time(microsecond),
        {End - Start, Encoded}
    end, lists:seq(1, Operations)),
    
    {[L || {L, _} <- Latencies], element(2, hd(Latencies))}.

-spec benchmark_decode(jsx | native, binary(), pos_integer()) -> [non_neg_integer()].
benchmark_decode(Library, EncodedData, Operations) ->
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _ = case Library of
            jsx -> jsx:decode(EncodedData, [return_maps]);
            native -> json:decode(EncodedData)
        end,
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Operations)).

-spec benchmark_roundtrip(jsx | native, map(), pos_integer()) -> [non_neg_integer()].
benchmark_roundtrip(Library, Message, Operations) ->
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        Encoded = case Library of
            jsx -> jsx:encode(Message);
            native -> json:encode(Message)
        end,
        _Decoded = case Library of
            jsx -> jsx:decode(Encoded, [return_maps]);
            native -> json:decode(Encoded)
        end,
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Operations)).

%%====================================================================
%% Metrics Calculation
%%====================================================================

-spec calculate_metrics([non_neg_integer()]) -> map().
calculate_metrics([]) ->
    #{p50 => 0.0, p95 => 0.0, p99 => 0.0, avg => 0.0, min => 0.0, max => 0.0};
calculate_metrics(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Avg = lists:sum(Sorted) / Len,
    Min = lists:min(Sorted),
    Max = lists:max(Sorted),
    
    ThroughputOpsPerS = 1_000_000 / Avg, % Convert us to ops/s
    
    #{
        p50_us => round_float(P50, 1),
        p95_us => round_float(P95, 1),
        p99_us => round_float(P99, 1),
        avg_us => round_float(Avg, 1),
        min_us => round_float(Min, 1),
        max_us => round_float(Max, 1),
        throughput_ops_per_s => round_float(ThroughputOpsPerS, 1)
    }.

-spec percentile([number()], float()) -> float().
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

%%====================================================================
%% Report Building
%%====================================================================

-spec build_report(binary(), map(), atom(), pos_integer(), map(), map() | undefined) -> map().
build_report(WorkloadId, Env, MessageSize, Operations, JsxResults, NativeResults) ->
    BaseReport = #{
        workload_id => WorkloadId,
        benchmark => <<"json_otp28">>,
        timestamp => erlang:system_time(second),
        environment => Env,
        message_size => MessageSize,
        operations => Operations,
        precision => <<"microsecond">>,
        scope => <<"per_node">>,
        jsx_results => JsxResults
    },
    
    case NativeResults of
        undefined ->
            BaseReport;
        _ ->
            %% Calculate improvement ratio
            JsxThroughput = maps:get(throughput_ops_per_s, maps:get(roundtrip, JsxResults)),
            NativeThroughput = maps:get(throughput_ops_per_s, maps:get(roundtrip, NativeResults)),
            Improvement = (NativeThroughput / JsxThroughput) * 100 - 100,
            
            BaseReport#{
                native_json_results => NativeResults,
                improvement_percent => round_float(Improvement, 1),
                target_improvement_percent => 150.0  % 2-3x = 150% improvement
            }
    end.

%%====================================================================
%% Display & Output
%%====================================================================

-spec display_comparison(map(), map() | undefined) -> ok.
display_comparison(JsxResults, undefined) ->
    io:format("~n--- Results (JSX only) ---~n"),
    display_library_results(jsx, JsxResults),
    ok;
display_comparison(JsxResults, NativeResults) ->
    io:format("~n--- Performance Comparison ---~n~n"),
    
    io:format("JSX:~n"),
    display_library_results(jsx, JsxResults),
    
    io:format("~nNative JSON:~n"),
    display_library_results(native, NativeResults),
    
    %% Calculate improvement
    JsxRoundtrip = maps:get(roundtrip, JsxResults),
    NativeRoundtrip = maps:get(roundtrip, NativeResults),
    
    JsxThroughput = maps:get(throughput_ops_per_s, JsxRoundtrip),
    NativeThroughput = maps:get(throughput_ops_per_s, NativeRoundtrip),
    
    SpeedupFactor = NativeThroughput / JsxThroughput,
    Improvement = (SpeedupFactor - 1.0) * 100,
    
    io:format("~n--- Improvement ---~n"),
    io:format("Speedup: ~.2fx~n", [SpeedupFactor]),
    io:format("Improvement: ~.1f%~n", [Improvement]),
    io:format("Target: 2-3x (150% improvement)~n"),
    
    Status = if
        SpeedupFactor >= 2.0 -> "✓ TARGET MET";
        SpeedupFactor >= 1.5 -> "⚠ CLOSE TO TARGET";
        true -> "✗ BELOW TARGET"
    end,
    io:format("Status: ~s~n", [Status]),
    
    ok.

-spec display_library_results(atom(), map()) -> ok.
display_library_results(_Library, Results) ->
    Encode = maps:get(encode, Results),
    Decode = maps:get(decode, Results),
    Roundtrip = maps:get(roundtrip, Results),
    
    io:format("  Encode:    ~.1f us (p50), ~.2f K ops/s~n", 
              [maps:get(p50_us, Encode), maps:get(throughput_ops_per_s, Encode) / 1000]),
    io:format("  Decode:    ~.1f us (p50), ~.2f K ops/s~n",
              [maps:get(p50_us, Decode), maps:get(throughput_ops_per_s, Decode) / 1000]),
    io:format("  Roundtrip: ~.1f us (p50), ~.2f K ops/s~n",
              [maps:get(p50_us, Roundtrip), maps:get(throughput_ops_per_s, Roundtrip) / 1000]),
    io:format("  Memory:    ~p bytes/op~n", [maps:get(memory_per_op_bytes, Results)]),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

-spec has_native_json() -> boolean().
has_native_json() ->
    %% Check if json module is available (OTP 27+)
    erlang:function_exported(json, encode, 1).

-spec warmup(map(), pos_integer()) -> ok.
warmup(Message, Iterations) ->
    lists:foreach(fun(_) ->
        _ = jsx:encode(Message),
        case has_native_json() of
            true -> _ = json:encode(Message);
            false -> ok
        end
    end, lists:seq(1, Iterations)),
    ok.

-spec capture_environment() -> map().
capture_environment() ->
    {ok, Hostname} = inet:gethostname(),
    OtpRelease = erlang:system_info(otp_release),
    
    #{
        hostname => list_to_binary(Hostname),
        erlang_version => list_to_binary("OTP-" ++ OtpRelease),
        native_json_available => has_native_json()
    }.

-spec validate_report(map()) -> ok | {error, term()}.
validate_report(Report) ->
    RequiredFields = [workload_id, benchmark, timestamp, environment, jsx_results],
    case lists:all(fun(F) -> maps:is_key(F, Report) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

-spec write_report(string(), map()) -> ok | {error, term()}.
write_report(Filename, Report) ->
    filelib:ensure_dir(Filename),
    Json = jsx:encode(Report, [{space, 2}, {indent, 2}]),
    file:write_file(Filename, Json).

-spec round_float(number(), integer()) -> float().
round_float(Value, DecimalPlaces) ->
    Multiplier = math:pow(10, DecimalPlaces),
    round(Value * Multiplier) / Multiplier.
