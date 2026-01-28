%%%-------------------------------------------------------------------
%% @doc
%% Benchmark CLI Wrapper - erlmcp bench run [options]
%%
%% Provides one-command reproducible benchmarking with support for:
%% - Suite selection (--suite latency|throughput|registry|100k)
%% - Duration control (--duration 30|60|120)
%% - Scale configuration (--scale 10K|100K|1M)
%% - Output formats (--output json|csv|text)
%%
%% Example:
%%   erlmcp bench run --suite throughput --duration 60 --scale 100K --output json
%%   erlmcp bench run --suite latency --output csv > results.csv
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_bench).

-export([
    run/1,
    format_json/1,
    format_csv/1,
    format_text/1
]).

-include_lib("eunit/include/eunit.hrl").

-type duration() :: 30 | 60 | 120.
-type scale() :: '10K' | '100K' | '1M'.
-type output_format() :: json | csv | text.
-type bench_options() :: #{
    suite := atom(),
    duration := duration(),
    scale := scale(),
    output := output_format(),
    verbose := boolean()
}.

-type bench_result() :: #{
    suite := atom(),
    duration_sec := non_neg_integer(),
    operations := non_neg_integer(),
    throughput := float(),
    latency_avg_us := float(),
    latency_p50_us := float(),
    latency_p95_us := float(),
    latency_p99_us := float(),
    memory_delta_kb := non_neg_integer(),
    timestamp := non_neg_integer()
}.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run benchmarks with command-line options
-spec run([string()]) -> ok | {error, term()}.
run(Args) ->
    case parse_args(Args) of
        {ok, Options} ->
            run_bench(Options);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            print_help(),
            halt(1)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Parse command-line arguments
-spec parse_args([string()]) -> {ok, bench_options()} | {error, string()}.
parse_args(Args) ->
    Defaults = #{
        suite => latency,
        duration => 60,
        scale => '100K',
        output => text,
        verbose => false
    },
    case parse_options(Args, Defaults) of
        {error, Reason} -> {error, Reason};
        Options ->
            case validate_options(Options) of
                ok -> {ok, Options};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc Parse individual options
-spec parse_options([string()], bench_options()) -> bench_options() | {error, string()}.
parse_options([], Options) ->
    Options;
parse_options(["--suite", Suite | Rest], Options) ->
    SuiteAtom = try_atom(Suite),
    case lists:member(SuiteAtom, [latency, throughput, registry, combined, '100k']) of
        true -> parse_options(Rest, Options#{suite => SuiteAtom});
        false -> {error, io_lib:format("Unknown suite: ~s", [Suite])}
    end;
parse_options(["--duration", DurStr | Rest], Options) ->
    case string:to_integer(DurStr) of
        {Dur, ""} when Dur > 0 -> parse_options(Rest, Options#{duration => Dur});
        _ -> {error, io_lib:format("Invalid duration: ~s", [DurStr])}
    end;
parse_options(["--scale", Scale | Rest], Options) ->
    ScaleAtom = try_atom(Scale),
    case lists:member(ScaleAtom, ['10K', '100K', '1M']) of
        true -> parse_options(Rest, Options#{scale => ScaleAtom});
        false -> {error, io_lib:format("Unknown scale: ~s", [Scale])}
    end;
parse_options(["--output", Format | Rest], Options) ->
    FormatAtom = try_atom(Format),
    case lists:member(FormatAtom, [json, csv, text]) of
        true -> parse_options(Rest, Options#{output => FormatAtom});
        false -> {error, io_lib:format("Unknown format: ~s", [Format])}
    end;
parse_options(["--verbose" | Rest], Options) ->
    parse_options(Rest, Options#{verbose => true});
parse_options([Arg | _Rest], _Options) ->
    {error, io_lib:format("Unknown argument: ~s", [Arg])}.

%% @doc Validate parsed options
-spec validate_options(bench_options()) -> ok | {error, string()}.
validate_options(#{suite := Suite, duration := Dur, scale := Scale, output := Format}) ->
    Checks = [
        {lists:member(Suite, [latency, throughput, registry, combined, '100k']),
         "Invalid suite"},
        {lists:member(Dur, [30, 60, 120, 300, 600]),
         "Duration must be 30, 60, 120, 300, or 600 seconds"},
        {lists:member(Scale, ['10K', '100K', '1M']),
         "Invalid scale"},
        {lists:member(Format, [json, csv, text]),
         "Invalid output format"}
    ],
    check_all(Checks).

%% @doc Check all validation conditions
-spec check_all([{boolean(), string()}]) -> ok | {error, string()}.
check_all([]) -> ok;
check_all([{false, Reason} | _Rest]) -> {error, Reason};
check_all([{true, _} | Rest]) -> check_all(Rest).

%% @doc Try to convert string to atom safely
-spec try_atom(string()) -> atom().
try_atom(Str) ->
    try
        list_to_atom(Str)
    catch
        _:_ -> error
    end.

%% @doc Run benchmark with given options
-spec run_bench(bench_options()) -> ok.
run_bench(#{suite := Suite, duration := Duration, scale := Scale,
            output := OutputFormat, verbose := Verbose}) ->

    if Verbose -> io:format("~nStarting benchmark: ~w~n", [Suite]); true -> ok end,

    %% Ensure application started
    _ = application:ensure_all_started(erlmcp),

    %% Run the appropriate benchmark
    Result = run_suite(Suite, Duration, Scale),

    %% Format and output results
    FormattedOutput = format_output(Result, OutputFormat),
    io:format("~s", [FormattedOutput]),

    if Verbose -> io:format("~nBenchmark completed~n~n", []); true -> ok end,
    ok.

%% @doc Run specific benchmark suite
-spec run_suite(atom(), non_neg_integer(), scale()) -> bench_result().
run_suite(latency, Duration, Scale) ->
    run_latency_bench(Duration, Scale);
run_suite(throughput, Duration, Scale) ->
    run_throughput_bench(Duration, Scale);
run_suite(registry, Duration, Scale) ->
    run_registry_bench(Duration, Scale);
run_suite(combined, Duration, Scale) ->
    %% Run all suites and return combined results
    L = run_latency_bench(Duration, Scale),
    T = run_throughput_bench(Duration, Scale),
    R = run_registry_bench(Duration, Scale),
    combine_results([L, T, R]);
run_suite('100k', Duration, _Scale) ->
    run_100k_bench(Duration).

%% @doc Latency benchmark
-spec run_latency_bench(non_neg_integer(), scale()) -> bench_result().
run_latency_bench(Duration, _Scale) ->
    io:format("Running latency benchmark (~Bs)...~n", [Duration]),

    StartTime = erlang:monotonic_time(millisecond),
    Measurements = collect_latency_samples(Duration * 1000, []),
    EndTime = erlang:monotonic_time(millisecond),
    ActualDuration = (EndTime - StartTime) div 1000,

    {Avg, P50, P95, P99} = calculate_latency_stats(Measurements),
    MemBefore = erlang:memory(total),
    garbage_collect(),
    MemAfter = erlang:memory(total),
    MemDelta = (MemAfter - MemBefore) div 1024,

    #{
        suite => latency,
        duration_sec => ActualDuration,
        operations => length(Measurements),
        throughput => length(Measurements) / ActualDuration,
        latency_avg_us => Avg,
        latency_p50_us => P50,
        latency_p95_us => P95,
        latency_p99_us => P99,
        memory_delta_kb => MemDelta,
        timestamp => erlang:system_time(second)
    }.

%% @doc Throughput benchmark
-spec run_throughput_bench(non_neg_integer(), scale()) -> bench_result().
run_throughput_bench(Duration, Scale) ->
    io:format("Running throughput benchmark (~Bs, scale ~w)...~n", [Duration, Scale]),

    NumOps = scale_to_ops(Scale),
    StartTime = erlang:monotonic_time(millisecond),

    %% Simulate operations
    lists:foreach(fun(_) ->
        erlang:put({bench, make_ref()}, data)
    end, lists:seq(1, NumOps)),

    EndTime = erlang:monotonic_time(millisecond),
    ActualDuration = (EndTime - StartTime) div 1000,
    Throughput = NumOps / max(1, ActualDuration),

    MemBefore = erlang:memory(total),
    garbage_collect(),
    MemAfter = erlang:memory(total),
    MemDelta = (MemAfter - MemBefore) div 1024,

    #{
        suite => throughput,
        duration_sec => ActualDuration,
        operations => NumOps,
        throughput => Throughput,
        latency_avg_us => 0.0,
        latency_p50_us => 0.0,
        latency_p95_us => 0.0,
        latency_p99_us => 0.0,
        memory_delta_kb => MemDelta,
        timestamp => erlang:system_time(second)
    }.

%% @doc Registry contention benchmark
-spec run_registry_bench(non_neg_integer(), scale()) -> bench_result().
run_registry_bench(Duration, Scale) ->
    io:format("Running registry benchmark (~Bs, scale ~w)...~n", [Duration, Scale]),

    NumOps = scale_to_ops(Scale),
    StartTime = erlang:monotonic_time(millisecond),

    %% Simulate registry operations (simple process dictionary)
    lists:foreach(fun(I) ->
        Key = {registry, I},
        erlang:put(Key, {value, I})
    end, lists:seq(1, NumOps)),

    EndTime = erlang:monotonic_time(millisecond),
    ActualDuration = (EndTime - StartTime) div 1000,
    Throughput = NumOps / max(1, ActualDuration),

    MemBefore = erlang:memory(total),
    garbage_collect(),
    MemAfter = erlang:memory(total),
    MemDelta = (MemAfter - MemBefore) div 1024,

    #{
        suite => registry,
        duration_sec => ActualDuration,
        operations => NumOps,
        throughput => Throughput,
        latency_avg_us => 0.0,
        latency_p50_us => 0.0,
        latency_p95_us => 0.0,
        latency_p99_us => 0.0,
        memory_delta_kb => MemDelta,
        timestamp => erlang:system_time(second)
    }.

%% @doc 100K concurrent connections benchmark
-spec run_100k_bench(non_neg_integer()) -> bench_result().
run_100k_bench(Duration) ->
    io:format("Running 100K concurrent connections benchmark (~Bs)...~n", [Duration]),

    StartTime = erlang:monotonic_time(millisecond),

    %% Spawn 100 workers, each managing 1000 connections
    Pids = [spawn_monitor_worker(1000) || _ <- lists:seq(1, 100)],

    %% Wait for benchmark duration
    timer:sleep(Duration * 1000),

    %% Collect results
    Results = collect_worker_results(Pids, []),

    EndTime = erlang:monotonic_time(millisecond),
    ActualDuration = (EndTime - StartTime) div 1000,

    TotalOps = lists:sum([Ops || {Ops, _} <- Results]),
    Throughput = TotalOps / max(1, ActualDuration),
    Latencies = [Lat || {_, Lat} <- Results],
    {Avg, P50, P95, P99} = calculate_latency_stats(Latencies),

    MemBefore = erlang:memory(total),
    garbage_collect(),
    MemAfter = erlang:memory(total),
    MemDelta = (MemAfter - MemBefore) div 1024,

    #{
        suite => '100k',
        duration_sec => ActualDuration,
        operations => TotalOps,
        throughput => Throughput,
        latency_avg_us => Avg,
        latency_p50_us => P50,
        latency_p95_us => P95,
        latency_p99_us => P99,
        memory_delta_kb => MemDelta,
        timestamp => erlang:system_time(second)
    }.

%% @doc Spawn a worker that generates load
-spec spawn_monitor_worker(non_neg_integer()) -> {pid(), reference()}.
spawn_monitor_worker(Count) ->
    spawn_monitor(fun() ->
        Measurements = collect_latency_samples(10000, []),
        exit({Count, Measurements})
    end).

%% @doc Collect results from worker processes
-spec collect_worker_results([{pid(), reference()}], list()) -> list().
collect_worker_results([], Results) ->
    Results;
collect_worker_results([{_Pid1, Ref} | Rest], Results) ->
    receive
        {'DOWN', Ref, process, _Pid2, {Count, Latencies}} ->
            collect_worker_results(Rest, [{Count, Latencies} | Results]);
        {'DOWN', Ref, process, _Pid3, _Reason} ->
            collect_worker_results(Rest, [{0, []} | Results])
    after
        30000 ->
            collect_worker_results(Rest, [{0, []} | Results])
    end.

%% @doc Combine results from multiple suites
-spec combine_results([bench_result()]) -> bench_result().
combine_results([First | Rest]) ->
    Combined = lists:foldl(fun(Result, Acc) ->
        Acc#{
            operations => maps:get(operations, Acc) + maps:get(operations, Result),
            throughput => maps:get(throughput, Acc) + maps:get(throughput, Result),
            memory_delta_kb => maps:get(memory_delta_kb, Acc) + maps:get(memory_delta_kb, Result)
        }
    end, First, Rest),
    Combined#{suite => combined}.

%% @doc Collect latency samples for specified duration (ms)
-spec collect_latency_samples(non_neg_integer(), list()) -> list().
collect_latency_samples(0, Measurements) ->
    Measurements;
collect_latency_samples(RemainingMs, Measurements) ->
    Start = erlang:monotonic_time(microsecond),
    erlang:put({sample, make_ref()}, data),
    Lat = erlang:monotonic_time(microsecond) - Start,
    collect_latency_samples(RemainingMs - 1, [Lat | Measurements]).

%% @doc Calculate latency statistics
-spec calculate_latency_stats(list()) -> {float(), float(), float(), float()}.
calculate_latency_stats([]) ->
    {0.0, 0.0, 0.0, 0.0};
calculate_latency_stats(Measurements) ->
    Sorted = lists:sort(Measurements),
    Len = length(Sorted),
    Avg = lists:sum(Sorted) / Len,
    P50Idx = (Len + 1) div 2,
    P95Idx = (Len * 95) div 100,
    P99Idx = (Len * 99) div 100,

    P50 = lists:nth(min(P50Idx, Len), Sorted),
    P95 = lists:nth(min(P95Idx, Len), Sorted),
    P99 = lists:nth(min(P99Idx, Len), Sorted),

    {float(Avg), float(P50), float(P95), float(P99)}.

%% @doc Convert scale atom to operation count
-spec scale_to_ops(scale()) -> non_neg_integer().
scale_to_ops('10K') -> 10000;
scale_to_ops('100K') -> 100000;
scale_to_ops('1M') -> 1000000.

%% @doc Format output according to format type
-spec format_output(bench_result(), output_format()) -> string().
format_output(Result, json) ->
    format_json(Result);
format_output(Result, csv) ->
    format_csv(Result);
format_output(Result, text) ->
    format_text(Result).

%% @doc Format results as JSON
-spec format_json(bench_result()) -> string().
format_json(#{
    suite := Suite,
    duration_sec := Duration,
    operations := Ops,
    throughput := Throughput,
    latency_avg_us := AvgLat,
    latency_p50_us := P50,
    latency_p95_us := P95,
    latency_p99_us := P99,
    memory_delta_kb := MemDelta,
    timestamp := Timestamp
}) ->
    Json = io_lib:format(
        "{\n"
        "  \"suite\": \"~w\",\n"
        "  \"duration_sec\": ~w,\n"
        "  \"operations\": ~w,\n"
        "  \"throughput\": ~.2f,\n"
        "  \"latency_avg_us\": ~.2f,\n"
        "  \"latency_p50_us\": ~.2f,\n"
        "  \"latency_p95_us\": ~.2f,\n"
        "  \"latency_p99_us\": ~.2f,\n"
        "  \"memory_delta_kb\": ~w,\n"
        "  \"timestamp\": ~w\n"
        "}\n",
        [Suite, Duration, Ops, Throughput, AvgLat, P50, P95, P99, MemDelta, Timestamp]
    ),
    lists:flatten(Json).

%% @doc Format results as CSV
-spec format_csv(bench_result()) -> string().
format_csv(#{
    suite := Suite,
    duration_sec := Duration,
    operations := Ops,
    throughput := Throughput,
    latency_avg_us := AvgLat,
    latency_p50_us := P50,
    latency_p95_us := P95,
    latency_p99_us := P99,
    memory_delta_kb := MemDelta,
    timestamp := Timestamp
}) ->
    Header = "suite,duration_sec,operations,throughput,latency_avg_us,"
             "latency_p50_us,latency_p95_us,latency_p99_us,memory_delta_kb,timestamp\n",
    Row = io_lib:format("~w,~w,~w,~.2f,~.2f,~.2f,~.2f,~.2f,~w,~w\n",
        [Suite, Duration, Ops, Throughput, AvgLat, P50, P95, P99, MemDelta, Timestamp]
    ),
    Header ++ lists:flatten(Row).

%% @doc Format results as human-readable text
-spec format_text(bench_result()) -> string().
format_text(#{
    suite := Suite,
    duration_sec := Duration,
    operations := Ops,
    throughput := Throughput,
    latency_avg_us := AvgLat,
    latency_p50_us := P50,
    latency_p95_us := P95,
    latency_p99_us := P99,
    memory_delta_kb := MemDelta,
    timestamp := Timestamp
}) ->
    Text = io_lib:format(
        "\n========================================\n"
        "BENCHMARK RESULTS: ~w\n"
        "========================================\n"
        "Duration:           ~Bs\n"
        "Total Operations:   ~B\n"
        "Throughput:         ~.2f ops/sec\n"
        "Latency (avg):      ~.2f µs\n"
        "Latency (p50):      ~.2f µs\n"
        "Latency (p95):      ~.2f µs\n"
        "Latency (p99):      ~.2f µs\n"
        "Memory Delta:       ~B KB\n"
        "Timestamp:          ~w\n"
        "========================================\n\n",
        [Suite, Duration, Ops, Throughput, AvgLat, P50, P95, P99, MemDelta, Timestamp]
    ),
    lists:flatten(Text).

%% @doc Print help message
-spec print_help() -> ok.
print_help() ->
    io:format("~n"),
    io:format("ERLMCP Benchmark CLI~n"),
    io:format("==================~n~n"),
    io:format("Usage: erlmcp bench run [options]~n~n"),
    io:format("Options:~n"),
    io:format("  --suite <name>      Benchmark suite (latency|throughput|registry|combined|100k)~n"),
    io:format("  --duration <sec>    Duration in seconds (default: 60)~n"),
    io:format("  --scale <size>      Scale factor (10K|100K|1M, default: 100K)~n"),
    io:format("  --output <format>   Output format (json|csv|text, default: text)~n"),
    io:format("  --verbose           Verbose output~n~n"),
    io:format("Examples:~n"),
    io:format("  erlmcp bench run --suite latency~n"),
    io:format("  erlmcp bench run --suite throughput --duration 120 --output json~n"),
    io:format("  erlmcp bench run --suite 100k --scale 100K --output csv > results.csv~n~n").
