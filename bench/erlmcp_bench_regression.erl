%%%====================================================================
%%% ERLMCP REGRESSION BENCHMARK SUITE
%%%====================================================================
%%% Fast regression tests for CI/CD pipeline
%%% Detects >10% performance degradation before merge
%%%====================================================================

-module(erlmcp_bench_regression).

-export([run_all/0, run/1, regression_suite/0, check_regressions/1]).

-include_lib("kernel/include/logger.hrl").

%% Fast regression benchmark suite (target: <5 min)
-spec regression_suite() -> [map()].
regression_suite() -> [
    #{id => <<"json_encode_small">>, 
      category => json_rpc,
      operations => 1000,
      target_p95_ms => 2.0},
    
    #{id => <<"json_decode_small">>, 
      category => json_rpc,
      operations => 1000,
      target_p95_ms => 2.0},
    
    #{id => <<"tool_call_simple">>, 
      category => mcp,
      operations => 100,
      target_p95_ms => 12.0},
    
    #{id => <<"resource_read">>, 
      category => mcp,
      operations => 1000,
      target_p95_ms => 3.0},
    
    #{id => <<"registry_lookup">>, 
      category => core,
      operations => 10000,
      target_p95_us => 100}
].

%% Run all regression benchmarks
-spec run_all() -> ok | {error, term()}.
run_all() ->
    io:format("~n╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  ERLMCP REGRESSION BENCHMARK SUITE                         ║~n"),
    io:format("║  Fast benchmarks for CI/CD performance checks              ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n~n"),

    Timestamp = erlang:system_time(second),
    
    %% Run all benchmarks
    Results = lists:map(fun(Benchmark) ->
        run_benchmark(Benchmark)
    end, regression_suite()),

    %% Generate report
    Report = generate_report(Results, Timestamp),
    
    %% Write to file
    Filename = io_lib:format("bench/results/regression_~p.json", [Timestamp]),
    write_json(Filename, Report),
    
    io:format("~n▶ Report written: ~s~n", [Filename]),

    %% Check for regressions against baseline
    check_regressions(Report).

%% Run a single benchmark
-spec run(binary()) -> ok | {error, term()}.
run(BenchmarkId) ->
    case lists:filter(fun(#{id := Id}) -> Id =:= BenchmarkId end, regression_suite()) of
        [] ->
            io:format("ERROR: Unknown benchmark: ~s~n", [BenchmarkId]),
            {error, {unknown_benchmark, BenchmarkId}};
        [Benchmark] ->
            Result = run_benchmark(Benchmark),
            io:format("~nResult: ~p~n", [Result]),
            ok
    end.

%% Run a single benchmark
run_benchmark(#{id := Id, category := Category, operations := Ops} = Benchmark) ->
    io:format("~n▶ Running ~s (~p ops)...~n", [Id, Ops]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Run benchmark based on category
    Latencies = case Category of
        json_rpc -> run_json_rpc_benchmark(Id, Ops);
        mcp -> run_mcp_benchmark(Id, Ops);
        core -> run_core_benchmark(Id, Ops)
    end,
    
    EndTime = erlang:monotonic_time(microsecond),
    Duration = (EndTime - StartTime) / 1_000_000,
    
    %% Calculate percentiles
    Percentiles = calculate_percentiles(Latencies),
    Throughput = Ops / Duration,
    
    io:format("  ✓ P50=~.2fms, P95=~.2fms, P99=~.2fms, ~.0f ops/s~n", 
        [maps:get(p50, Percentiles) / 1000,
         maps:get(p95, Percentiles) / 1000,
         maps:get(p99, Percentiles) / 1000,
         Throughput]),
    
    Benchmark#{
        duration_s => Duration,
        throughput_ops_per_s => Throughput,
        latency_p50_us => maps:get(p50, Percentiles),
        latency_p95_us => maps:get(p95, Percentiles),
        latency_p99_us => maps:get(p99, Percentiles),
        latency_min_us => maps:get(min, Percentiles),
        latency_max_us => maps:get(max, Percentiles),
        latency_avg_us => maps:get(avg, Percentiles)
    }.

%% Run JSON-RPC benchmarks
run_json_rpc_benchmark(<<"json_encode_small">>, Ops) ->
    Data = #{<<"method">> => <<"test">>, <<"params">> => #{<<"key">> => <<"value">>}},
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _Json = jsx:encode(Data),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops));

run_json_rpc_benchmark(<<"json_decode_small">>, Ops) ->
    Json = jsx:encode(#{<<"method">> => <<"test">>, <<"params">> => #{<<"key">> => <<"value">>}}),
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _Data = jsx:decode(Json, [return_maps]),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops)).

%% Run MCP benchmarks
run_mcp_benchmark(<<"tool_call_simple">>, Ops) ->
    %% Simulate simple tool call encoding (no server required)
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _Request = erlmcp_json_rpc:encode_request(
            rand:uniform(1000000),
            <<"tools/call">>,
            #{<<"name">> => <<"echo">>, <<"arguments">> => #{<<"message">> => <<"test">>}}
        ),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops));

run_mcp_benchmark(<<"resource_read">>, Ops) ->
    %% Simulate resource URI matching
    Resources = [
        {<<"file://~/.erlmcp/config.json">>, #{type => config}},
        {<<"file://~/.erlmcp/state.db">>, #{type => state}}
    ],
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _Match = lists:filter(fun({Uri, _}) -> 
            binary:match(Uri, <<"config">>) =/= nomatch
        end, Resources),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops)).

%% Run core benchmarks
run_core_benchmark(<<"registry_lookup">>, Ops) ->
    %% Simulate map lookup (ETS would be more accurate)
    Registry = maps:from_list([{I, {pid, I}} || I <- lists:seq(1, 1000)]),
    lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _Value = maps:get(rand:uniform(1000), Registry, undefined),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops)).

%% Generate regression report
generate_report(Results, Timestamp) ->
    #{
        <<"report_type">> => <<"regression">>,
        <<"timestamp">> => Timestamp,
        <<"timestamp_iso">> => format_timestamp(Timestamp),
        <<"environment">> => capture_environment(),
        <<"benchmarks">> => Results,
        <<"summary">> => generate_summary(Results)
    }.

%% Generate summary statistics
generate_summary(Results) ->
    AllLatencies = lists:flatten([
        [maps:get(latency_p50_us, R), 
         maps:get(latency_p95_us, R), 
         maps:get(latency_p99_us, R)]
        || R <- Results
    ]),
    
    #{
        <<"benchmark_count">> => length(Results),
        <<"total_operations">> => lists:sum([maps:get(operations, R) || R <- Results]),
        <<"avg_p95_us">> => lists:sum([maps:get(latency_p95_us, R) || R <- Results]) / length(Results),
        <<"max_p95_us">> => lists:max([maps:get(latency_p95_us, R) || R <- Results])
    }.

%% Check for regressions against baseline
-spec check_regressions(map()) -> ok | {error, [term()]}.
check_regressions(Report) ->
    BaselineFile = "bench/baseline/main_latest.json",
    
    case file:read_file(BaselineFile) of
        {ok, BaselineBinary} ->
            Baseline = jsx:decode(BaselineBinary, [return_maps]),
            Violations = compare_reports(Report, Baseline, 0.10),
            
            case Violations of
                [] ->
                    io:format("~n✓ No performance regressions detected~n"),
                    ok;
                _ ->
                    io:format("~n✗ Performance regressions detected:~n"),
                    lists:foreach(fun({BenchId, Metric, PctChange}) ->
                        io:format("  - ~s (~s): +~.1f% regression~n", 
                            [BenchId, Metric, PctChange * 100])
                    end, Violations),
                    {error, {regressions_detected, Violations}}
            end;
        {error, enoent} ->
            io:format("~n⚠ Baseline not found, skipping regression check~n"),
            ok;
        {error, Reason} ->
            io:format("~n✗ Failed to read baseline: ~p~n", [Reason]),
            {error, {baseline_read_failed, Reason}}
    end.

%% Compare current report with baseline
compare_reports(CurrentReport, BaselineReport, Threshold) ->
    CurrentBenchmarks = maps:get(<<"benchmarks">>, CurrentReport),
    BaselineBenchmarks = maps:get(<<"benchmarks">>, BaselineReport, []),
    
    %% Build baseline lookup map
    BaselineMap = maps:from_list([
        {maps:get(id, B), B} || B <- BaselineBenchmarks
    ]),
    
    %% Check each benchmark for regression
    lists:filtermap(fun(Current) ->
        CurrentId = maps:get(id, Current),
        case maps:get(CurrentId, BaselineMap, undefined) of
            undefined ->
                false;  % No baseline, skip
            Baseline ->
                check_benchmark_regression(CurrentId, Current, Baseline, Threshold)
        end
    end, CurrentBenchmarks).

%% Check a single benchmark for regression
check_benchmark_regression(Id, Current, Baseline, Threshold) ->
    Metrics = [
        {<<"latency_p50_us">>, higher_is_worse},
        {<<"latency_p95_us">>, higher_is_worse},
        {<<"latency_p99_us">>, higher_is_worse},
        {<<"throughput_ops_per_s">>, lower_is_worse}
    ],
    
    Violations = lists:filtermap(fun({Metric, Direction}) ->
        CurrentVal = maps:get(Metric, Current, 0),
        BaselineVal = maps:get(Metric, Baseline, 0),
        
        case BaselineVal of
            0 -> false;
            _ ->
                PctChange = (CurrentVal - BaselineVal) / BaselineVal,
                IsRegression = case Direction of
                    higher_is_worse -> PctChange > Threshold;
                    lower_is_worse -> PctChange < -Threshold
                end,
                
                case IsRegression of
                    true -> {true, {Id, Metric, abs(PctChange)}};
                    false -> false
                end
        end
    end, Metrics),
    
    case Violations of
        [] -> false;
        _ -> {true, lists:hd(Violations)}
    end.

%% Calculate percentiles
calculate_percentiles([]) ->
    #{p50 => 0.0, p95 => 0.0, p99 => 0.0, min => 0.0, max => 0.0, avg => 0.0};
calculate_percentiles(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    #{
        p50 => percentile(Sorted, 0.50),
        p95 => percentile(Sorted, 0.95),
        p99 => percentile(Sorted, 0.99),
        min => lists:min(Sorted),
        max => lists:max(Sorted),
        avg => lists:sum(Sorted) / Len
    }.

percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

%% Capture environment
capture_environment() ->
    {ok, Hostname} = inet:gethostname(),
    #{
        <<"hostname">> => list_to_binary(Hostname),
        <<"otp_release">> => list_to_binary(erlang:system_info(otp_release)),
        <<"os">> => list_to_binary(erlang:system_info(system_architecture)),
        <<"cores">> => erlang:system_info(logical_processors)
    }.

%% Write JSON
write_json(Filename, Data) ->
    filelib:ensure_dir(Filename),
    Json = jsx:encode(Data, [{space, 2}, {indent, 2}]),
    file:write_file(Filename, Json).

%% Format timestamp
format_timestamp(UnixSeconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(UnixSeconds, second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).
