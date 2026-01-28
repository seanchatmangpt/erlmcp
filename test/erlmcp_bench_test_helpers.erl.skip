%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_test_helpers - Test helper functions
%%%
%%% Provides reusable utilities for benchmark testing:
%%% - Determinism checking (±2% variance validation)
%%% - Regression detection (>10% degradation)
%%% - Metrology validation wrappers
%%% - Test data cleanup
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_test_helpers).

-export([
    run_workload_and_validate/2,
    check_determinism/2,
    check_regression/2,
    cleanup_test_data/0,
    load_baseline/2,
    save_baseline/3,
    calculate_variance/1,
    format_benchmark_result/1
]).

%%====================================================================
%% Public API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Run workload and validate metrology compliance
%% @end
%%--------------------------------------------------------------------
-spec run_workload_and_validate(module(), term()) ->
    {ok, map()} | {error, term()}.
run_workload_and_validate(BenchmarkModule, WorkloadSpec) ->
    %% Run benchmark
    case BenchmarkModule:run(WorkloadSpec) of
        {ok, Result} ->
            %% Validate metrology compliance
            case erlmcp_metrology_validator:validate_report(Result) of
                ok ->
                    {ok, Result};
                {error, Violations} ->
                    {error, {metrology_violations, Violations}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Check determinism across multiple runs
%% Validates that results are within TolerancePercent variance
%% @end
%%--------------------------------------------------------------------
-spec check_determinism(list(number()), float()) ->
    {ok, float()} | {error, term()}.
check_determinism([], _TolerancePercent) ->
    {error, empty_results};
check_determinism([_Single], _TolerancePercent) ->
    {ok, 0.0}; % Single result is perfectly deterministic
check_determinism(Values, TolerancePercent) ->
    Mean = lists:sum(Values) / length(Values),
    MaxDeviation = lists:max([abs(V - Mean) / Mean * 100 || V <- Values]),

    case MaxDeviation =< TolerancePercent of
        true -> {ok, MaxDeviation};
        false -> {error, {variance_too_high, MaxDeviation, TolerancePercent}}
    end.

%%--------------------------------------------------------------------
%% @doc Check for performance regression against baseline
%% Fails if degradation exceeds ThresholdPercent (default: 10%)
%% @end
%%--------------------------------------------------------------------
-spec check_regression(map(), map()) -> ok | {error, term()}.
check_regression(Current, Baseline) ->
    check_regression(Current, Baseline, 10.0).

-spec check_regression(map(), map(), float()) -> ok | {error, term()}.
check_regression(Current, Baseline, ThresholdPercent) ->
    %% Extract throughput metrics
    CurrentThroughput = maps:get(throughput_msg_per_s, Current, 0),
    BaselineThroughput = maps:get(throughput_msg_per_s, Baseline, 1),

    Degradation = (BaselineThroughput - CurrentThroughput) /
                  BaselineThroughput * 100,

    case Degradation =< ThresholdPercent of
        true -> ok;
        false -> {error, {regression_detected, Degradation, ThresholdPercent}}
    end.

%%--------------------------------------------------------------------
%% @doc Calculate variance across a set of values
%% Returns {Mean, Variance, StdDev}
%% @end
%%--------------------------------------------------------------------
-spec calculate_variance(list(number())) ->
    {Mean :: float(), Variance :: float(), StdDev :: float()}.
calculate_variance([]) ->
    {0.0, 0.0, 0.0};
calculate_variance(Values) ->
    N = length(Values),
    Mean = lists:sum(Values) / N,

    SumSquaredDiff = lists:sum([
        math:pow(V - Mean, 2) || V <- Values
    ]),

    Variance = SumSquaredDiff / N,
    StdDev = math:sqrt(Variance),

    {Mean, Variance, StdDev}.

%%--------------------------------------------------------------------
%% @doc Load baseline results from file
%% @end
%%--------------------------------------------------------------------
-spec load_baseline(WorkloadId :: binary(), BaselineDir :: string()) ->
    {ok, map()} | {error, term()}.
load_baseline(WorkloadId, BaselineDir) ->
    Filename = filename:join(
        BaselineDir,
        binary_to_list(WorkloadId) ++ "_baseline.json"
    ),

    case file:read_file(Filename) of
        {ok, Binary} ->
            try jsx:decode(Binary, [return_maps]) of
                Baseline when is_map(Baseline) ->
                    {ok, Baseline};
                _ ->
                    {error, invalid_baseline_format}
            catch
                _:_ ->
                    {error, json_decode_failed}
            end;
        {error, enoent} ->
            {error, no_baseline};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Save baseline results to file
%% @end
%%--------------------------------------------------------------------
-spec save_baseline(WorkloadId :: binary(), Result :: map(),
                   BaselineDir :: string()) -> ok | {error, term()}.
save_baseline(WorkloadId, Result, BaselineDir) ->
    filelib:ensure_dir(BaselineDir ++ "/"),

    Filename = filename:join(
        BaselineDir,
        binary_to_list(WorkloadId) ++ "_baseline.json"
    ),

    Json = jsx:encode(Result),
    file:write_file(Filename, Json).

%%--------------------------------------------------------------------
%% @doc Clean up test data and temporary files
%% @end
%%--------------------------------------------------------------------
-spec cleanup_test_data() -> ok.
cleanup_test_data() ->
    %% Clean up /tmp/erlmcp_* test files
    TempFiles = filelib:wildcard("/tmp/erlmcp_*.json"),
    lists:foreach(fun file:delete/1, TempFiles),

    %% Clean up test baseline directory
    BaselineDir = "/tmp/erlmcp_bench_baseline",
    case file:list_dir(BaselineDir) of
        {ok, Files} ->
            [file:delete(filename:join(BaselineDir, F)) || F <- Files],
            file:del_dir(BaselineDir);
        _ ->
            ok
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc Format benchmark result for display
%% @end
%%--------------------------------------------------------------------
-spec format_benchmark_result(map()) -> binary().
format_benchmark_result(Result) ->
    #{workload_id := WorkloadId,
      throughput_msg_per_s := Throughput,
      latency_p99_us := P99,
      duration_seconds := Duration} = Result,

    iolist_to_binary(io_lib:format(
        "Workload: ~s~n"
        "Duration: ~.2f s~n"
        "Throughput: ~p msg/s~n"
        "Latency P99: ~p µs (~.2f ms)~n",
        [WorkloadId, Duration, Throughput, P99, P99 / 1000]
    )).
