%%%====================================================================
%%% ERLMCP BASELINE BENCHMARK RUNNER
%%%====================================================================
%%% Aggregates all baseline benchmarks into a single run
%%% Generates master baseline report for regression detection
%%%====================================================================

-module(erlmcp_bench_baseline).

-export([run_all/0, update_baseline/0, generate_report/1]).

-include_lib("kernel/include/logger.hrl").

%% Run all baseline benchmarks
-spec run_all() -> ok.
run_all() ->
    io:format("~n╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  ERLMCP BASELINE BENCHMARK SUITE                           ║~n"),
    io:format("║  Establishing performance baseline for regression testing  ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n~n"),

    Timestamp = erlang:system_time(second),
    ResultsDir = "bench/results",
    filelib:ensure_dir(ResultsDir ++ "/"),

    %% Run all benchmark categories
    Results = #{
        core_ops => run_category("Core Operations", fun erlmcp_bench_core_ops:run_all/0),
        mcp_features => run_category("MCP Features", fun erlmcp_bench_mcp_features:run_all/0),
        transports => run_category("Transports", fun erlmcp_bench_transports:run_all/0)
    },

    %% Generate master baseline report
    BaselineReport = generate_report(Results),
    
    %% Write baseline
    BaselineFile = io_lib:format("bench/baseline/main_~s.json", 
        [format_date(Timestamp)]),
    write_json(BaselineFile, BaselineReport),
    
    %% Symlink to latest
    LatestFile = "bench/baseline/main_latest.json",
    file:delete(LatestFile),
    file:make_symlink(filename:basename(BaselineFile), LatestFile),

    io:format("~n╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  BASELINE COMPLETE                                         ║~n"),
    io:format("║  Report: ~-49s  ║~n", [BaselineFile]),
    io:format("║  Latest: ~-49s  ║~n", [LatestFile]),
    io:format("╚════════════════════════════════════════════════════════════╝~n~n"),

    ok.

%% Run a benchmark category
run_category(Name, Fun) ->
    io:format("~n▶ Running ~s benchmarks...~n", [Name]),
    StartTime = erlang:monotonic_time(millisecond),
    
    try
        Fun(),
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,
        io:format("  ✓ ~s complete (~p ms)~n", [Name, Duration]),
        {ok, Duration}
    catch
        Class:Error:Stacktrace ->
            io:format("  ✗ ~s failed: ~p:~p~n", [Name, Class, Error]),
            ?LOG_ERROR("Benchmark ~s failed: ~p", [Name, Stacktrace]),
            {error, Error}
    end.

%% Generate aggregated baseline report
-spec generate_report(map()) -> map().
generate_report(Results) ->
    Env = capture_environment(),
    Timestamp = erlang:system_time(second),
    
    %% Aggregate results from all categories
    AllResultFiles = filelib:wildcard("bench/results/*.json"),
    CategoryResults = lists:filtermap(fun(File) ->
        case file:read_file(File) of
            {ok, Binary} ->
                try jsx:decode(Binary, [return_maps]) of
                    Result -> {true, Result}
                catch
                    _:_ -> false
                end;
            _ ->
                false
        end
    end, AllResultFiles),

    %% Calculate aggregate metrics
    AggregateMetrics = calculate_aggregate_metrics(CategoryResults),

    #{
        <<"baseline_version">> => <<"1.0">>,
        <<"timestamp">> => Timestamp,
        <<"timestamp_iso">> => format_timestamp(Timestamp),
        <<"environment">> => Env,
        <<"categories">> => Results,
        <<"aggregate_metrics">> => AggregateMetrics,
        <<"benchmark_count">> => length(CategoryResults),
        <<"result_files">> => [list_to_binary(F) || F <- AllResultFiles]
    }.

%% Calculate aggregate metrics across all benchmarks
calculate_aggregate_metrics(Results) ->
    Latencies = lists:flatten([
        extract_latencies(R) || R <- Results
    ]),
    
    case Latencies of
        [] ->
            #{};
        _ ->
            Sorted = lists:sort(Latencies),
            #{
                <<"overall_p50_us">> => percentile(Sorted, 0.50),
                <<"overall_p95_us">> => percentile(Sorted, 0.95),
                <<"overall_p99_us">> => percentile(Sorted, 0.99),
                <<"overall_min_us">> => lists:min(Sorted),
                <<"overall_max_us">> => lists:max(Sorted),
                <<"overall_avg_us">> => lists:sum(Sorted) / length(Sorted),
                <<"sample_count">> => length(Sorted)
            }
    end.

%% Extract latencies from a result
extract_latencies(Result) when is_map(Result) ->
    case maps:get(<<"latency_p50_us">>, Result, undefined) of
        undefined -> [];
        P50 ->
            P95 = maps:get(<<"latency_p95_us">>, Result, P50),
            P99 = maps:get(<<"latency_p99_us">>, Result, P95),
            [P50, P95, P99]
    end;
extract_latencies(_) ->
    [].

%% Update baseline after merge to main
-spec update_baseline() -> ok.
update_baseline() ->
    io:format("Updating baseline from main branch...~n"),
    run_all().

%% Capture environment information
capture_environment() ->
    {ok, Hostname} = inet:gethostname(),
    #{
        <<"hostname">> => list_to_binary(Hostname),
        <<"otp_release">> => list_to_binary(erlang:system_info(otp_release)),
        <<"erts_version">> => list_to_binary(erlang:system_info(version)),
        <<"os">> => list_to_binary(erlang:system_info(system_architecture)),
        <<"cores">> => erlang:system_info(logical_processors),
        <<"schedulers">> => erlang:system_info(schedulers_online),
        <<"memory_total_mb">> => erlang:memory(total) / 1024 / 1024
    }.

%% Write JSON to file
write_json(Filename, Data) ->
    filelib:ensure_dir(Filename),
    Json = jsx:encode(Data, [{space, 2}, {indent, 2}]),
    file:write_file(Filename, Json).

%% Format timestamp as ISO8601
format_timestamp(UnixSeconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(UnixSeconds, second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).

%% Format date as YYYY-MM-DD
format_date(UnixSeconds) ->
    {{Year, Month, Day}, _} =
        calendar:system_time_to_universal_time(UnixSeconds, second),
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

%% Calculate percentile
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).
