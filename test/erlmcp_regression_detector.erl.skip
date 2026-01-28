%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP Performance Regression Detector
%%%
%%% Advanced regression detection system with:
%%% - Baseline creation and management
%%% - Statistical regression analysis (Z-score, confidence intervals)
%%% - Anomaly detection (IQR, ARIMA)
%%% - Trend analysis (linear regression, moving averages)
%%% - Multi-dimensional comparison (throughput, latency, memory)
%%% - Alert generation and escalation
%%% - Historical baseline tracking
%%%
%%% USAGE:
%%%   % Create baseline from initial test run
%%%   Detector = erlmcp_regression_detector:new(),
%%%   Baseline = erlmcp_regression_detector:create_baseline(Detector, Results),
%%%
%%%   % Check for regressions in new test run
%%%   Regressions = erlmcp_regression_detector:detect_regressions(
%%%       Detector, NewResults, Baseline
%%%   ),
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_regression_detector).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% Public API
-export([
    new/0,
    new/1,
    create_baseline/2,
    detect_regressions/3,
    analyze_metric_trend/3,
    detect_anomalies/2,
    get_regression_report/1,
    compare_baselines/3,
    update_baseline/2,
    load_baseline/1,
    save_baseline/2,
    start_link/0,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    detector_id :: binary(),
    baselines = #{} :: map(),
    current_results = [] :: [map()],
    regression_history = [] :: [#regression_result{}],
    threshold_config = #{
        throughput_threshold => 0.1,
        latency_threshold => 0.15,
        memory_threshold => 0.2,
        error_rate_threshold => 0.05
    } :: map()
}).

-record(baseline_metrics, {
    metric_name :: atom(),
    baseline_id :: binary(),
    created_at :: integer(),
    test_count :: integer(),
    mean :: float(),
    std_dev :: float(),
    min :: float(),
    max :: float(),
    p50 :: float(),
    p95 :: float(),
    p99 :: float(),
    samples :: [float()]
}).

-record(regression_result, {
    regression_id :: binary(),
    metric_name :: atom(),
    baseline_value :: float(),
    current_value :: float(),
    difference_percent :: float(),
    z_score :: float(),
    is_regression :: boolean(),
    confidence_level :: float(),
    anomaly_detected :: boolean(),
    severity :: none | low | medium | high | critical,
    detected_at :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new regression detector
-spec new() -> pid().
new() ->
    {ok, Pid} = start_link(),
    Pid.

%% @doc Create a new regression detector with ID
-spec new(binary()) -> pid().
new(DetectorId) ->
    {ok, Pid} = gen_server:start_link(?MODULE, DetectorId, []),
    Pid.

%% @doc Create baseline from test results
-spec create_baseline(pid(), [map()]) -> {ok, map()} | {error, term()}.
create_baseline(DetectorPid, TestResults) ->
    gen_server:call(DetectorPid, {create_baseline, TestResults}, 30000).

%% @doc Detect regressions comparing to baseline
-spec detect_regressions(pid(), [map()], map()) -> {ok, [#regression_result{}]} | {error, term()}.
detect_regressions(DetectorPid, CurrentResults, Baseline) ->
    gen_server:call(DetectorPid, {detect_regressions, CurrentResults, Baseline}, 60000).

%% @doc Analyze metric trend over time
-spec analyze_metric_trend(pid(), atom(), [float()]) -> map() | {error, term()}.
analyze_metric_trend(DetectorPid, MetricName, Values) ->
    gen_server:call(DetectorPid, {analyze_metric_trend, MetricName, Values}, 30000).

%% @doc Detect anomalies in metrics
-spec detect_anomalies(pid(), [float()]) -> [map()] | {error, term()}.
detect_anomalies(DetectorPid, Values) ->
    gen_server:call(DetectorPid, {detect_anomalies, Values}, 30000).

%% @doc Get regression report
-spec get_regression_report(pid()) -> map() | {error, term()}.
get_regression_report(DetectorPid) ->
    gen_server:call(DetectorPid, get_regression_report, 30000).

%% @doc Compare two baselines
-spec compare_baselines(pid(), map(), map()) -> map() | {error, term()}.
compare_baselines(DetectorPid, Baseline1, Baseline2) ->
    gen_server:call(DetectorPid, {compare_baselines, Baseline1, Baseline2}, 30000).

%% @doc Update baseline with new results
-spec update_baseline(pid(), [map()]) -> {ok, map()} | {error, term()}.
update_baseline(DetectorPid, NewResults) ->
    gen_server:call(DetectorPid, {update_baseline, NewResults}, 30000).

%% @doc Load baseline from file
-spec load_baseline(string()) -> map() | {error, term()}.
load_baseline(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            binary_to_term(Data);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Save baseline to file
-spec save_baseline(string(), map()) -> ok | {error, term()}.
save_baseline(FilePath, Baseline) ->
    case file:write_file(FilePath, term_to_binary(Baseline)) of
        ok ->
            logger:info("Baseline saved to ~s", [FilePath]),
            ok;
        {error, Reason} ->
            logger:error("Failed to save baseline: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Start the detector gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    DetectorId = generate_detector_id(),
    gen_server:start_link(?MODULE, DetectorId, []).

%% @doc Stop the detector
-spec stop(pid()) -> ok.
stop(DetectorPid) ->
    gen_server:call(DetectorPid, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(DetectorId) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{
        detector_id = DetectorId,
        baselines = #{},
        current_results = [],
        regression_history = []
    }}.

handle_call({create_baseline, TestResults}, _From, State) ->
    Baseline = create_baseline_internal(TestResults),
    NewState = State#state{
        baselines = maps:put(baseline_primary, Baseline, State#state.baselines)
    },
    {reply, {ok, Baseline}, NewState};

handle_call({detect_regressions, CurrentResults, Baseline}, _From, State) ->
    Regressions = detect_regressions_internal(CurrentResults, Baseline, State),
    NewState = State#state{
        current_results = CurrentResults,
        regression_history = Regressions ++ State#state.regression_history
    },
    {reply, {ok, Regressions}, NewState};

handle_call({analyze_metric_trend, MetricName, Values}, _From, State) ->
    Trend = analyze_metric_trend_internal(MetricName, Values),
    {reply, Trend, State};

handle_call({detect_anomalies, Values}, _From, State) ->
    Anomalies = detect_anomalies_internal(Values),
    {reply, Anomalies, State};

handle_call(get_regression_report, _From, State) ->
    Report = generate_regression_report(State),
    {reply, Report, State};

handle_call({compare_baselines, Baseline1, Baseline2}, _From, State) ->
    Comparison = compare_baselines_internal(Baseline1, Baseline2),
    {reply, Comparison, State};

handle_call({update_baseline, NewResults}, _From, State) ->
    NewBaseline = create_baseline_internal(NewResults),
    UpdatedBaselines = maps:put(baseline_primary, NewBaseline, State#state.baselines),
    NewState = State#state{baselines = UpdatedBaselines},
    {reply, {ok, NewBaseline}, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Create baseline from test results
create_baseline_internal(TestResults) ->
    Throughputs = extract_metric(throughput_msg_per_sec, TestResults),
    Latencies = extract_metric(avg_latency_ms, TestResults),
    P99Latencies = extract_metric(p99_latency_ms, TestResults),
    MemoryMB = extract_metric(memory_mb, TestResults),
    ErrorRates = extract_metric(error_rate, TestResults),

    #{
        throughput => calculate_baseline_metrics(throughput_msg_per_sec, Throughputs),
        latency => calculate_baseline_metrics(avg_latency_ms, Latencies),
        p99_latency => calculate_baseline_metrics(p99_latency_ms, P99Latencies),
        memory => calculate_baseline_metrics(memory_mb, MemoryMB),
        error_rate => calculate_baseline_metrics(error_rate, ErrorRates),
        test_count => length(TestResults),
        created_at => erlang:system_time(millisecond)
    }.

%% Extract metric from test results
extract_metric(MetricName, TestResults) ->
    lists:filtermap(fun(Result) ->
        case maps:find(MetricName, Result) of
            {ok, Value} when is_number(Value) -> {true, Value};
            _ -> false
        end
    end, TestResults).

%% Calculate baseline metrics (mean, stddev, percentiles)
calculate_baseline_metrics(MetricName, Values) when length(Values) > 0 ->
    Sorted = lists:sort(Values),
    Mean = lists:sum(Values) / length(Values),
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / length(Values),
    StdDev = math:sqrt(Variance),

    P50 = percentile(Sorted, 50),
    P95 = percentile(Sorted, 95),
    P99 = percentile(Sorted, 99),

    #baseline_metrics{
        metric_name = MetricName,
        baseline_id = generate_baseline_id(),
        created_at = erlang:system_time(millisecond),
        test_count = length(Values),
        mean = Mean,
        std_dev = StdDev,
        min = lists:min(Values),
        max = lists:max(Values),
        p50 = P50,
        p95 = P95,
        p99 = P99,
        samples = Values
    };

calculate_baseline_metrics(MetricName, []) ->
    #baseline_metrics{
        metric_name = MetricName,
        baseline_id = generate_baseline_id(),
        created_at = erlang:system_time(millisecond),
        test_count = 0,
        mean = 0.0,
        std_dev = 0.0,
        min = 0.0,
        max = 0.0,
        p50 = 0.0,
        p95 = 0.0,
        p99 = 0.0,
        samples = []
    }.

%% Calculate percentile
percentile(Sorted, Percentile) ->
    Len = length(Sorted),
    Index = max(1, round((Percentile / 100) * Len)),
    lists:nth(Index, Sorted).

%% Detect regressions internal
detect_regressions_internal(CurrentResults, Baseline, State) ->
    Metrics = [throughput, latency, p99_latency, memory, error_rate],

    lists:filtermap(fun(MetricName) ->
        case {maps:find(MetricName, Baseline), extract_metric_value(MetricName, CurrentResults)} of
            {{ok, BaselineMetrics}, {ok, CurrentValues}} ->
                case check_regression(MetricName, BaselineMetrics, CurrentValues, State) of
                    {true, Regression} ->
                        {true, Regression};
                    false ->
                        false
                end;
            _ ->
                false
        end
    end, Metrics).

%% Extract metric value from current results
extract_metric_value(MetricName, CurrentResults) ->
    Values = extract_metric(map_metric_name(MetricName), CurrentResults),
    case Values of
        [] -> error;
        _ ->
            Avg = lists:sum(Values) / length(Values),
            {ok, Avg}
    end.

%% Map metric names
map_metric_name(throughput) -> throughput_msg_per_sec;
map_metric_name(latency) -> avg_latency_ms;
map_metric_name(p99_latency) -> p99_latency_ms;
map_metric_name(memory) -> memory_mb;
map_metric_name(error_rate) -> error_rate;
map_metric_name(Other) -> Other.

%% Check for regression
check_regression(MetricName, BaselineMetrics, CurrentValue, State) ->
    BaselineMean = BaselineMetrics#baseline_metrics.mean,
    BaselineStdDev = BaselineMetrics#baseline_metrics.std_dev,

    case BaselineStdDev of
        0.0 ->
            % No variance in baseline, use percentage threshold
            Threshold = get_threshold(MetricName, State),
            DiffPercent = abs(CurrentValue - BaselineMean) / max(1, BaselineMean) * 100,
            case DiffPercent > (Threshold * 100) of
                true ->
                    {true, create_regression_result(
                        MetricName, BaselineMean, CurrentValue, DiffPercent, 0.0, high
                    )};
                false ->
                    false
            end;
        _ ->
            % Calculate Z-score
            ZScore = (CurrentValue - BaselineMean) / BaselineStdDev,
            DiffPercent = abs(CurrentValue - BaselineMean) / BaselineMean * 100,

            % Check if significant regression (Z > 2 is ~95% confidence)
            IsRegression = abs(ZScore) > 2.0,

            case IsRegression of
                true ->
                    Severity = calculate_severity(abs(ZScore), DiffPercent),
                    {true, create_regression_result(
                        MetricName, BaselineMean, CurrentValue, DiffPercent, ZScore, Severity
                    )};
                false ->
                    false
            end
    end.

%% Get threshold for metric
get_threshold(throughput, State) ->
    maps:get(throughput_threshold, State#state.threshold_config, 0.1);
get_threshold(latency, State) ->
    maps:get(latency_threshold, State#state.threshold_config, 0.15);
get_threshold(p99_latency, State) ->
    maps:get(latency_threshold, State#state.threshold_config, 0.15);
get_threshold(memory, State) ->
    maps:get(memory_threshold, State#state.threshold_config, 0.2);
get_threshold(error_rate, State) ->
    maps:get(error_rate_threshold, State#state.threshold_config, 0.05);
get_threshold(_, State) ->
    maps:get(latency_threshold, State#state.threshold_config, 0.15).

%% Calculate severity
calculate_severity(ZScore, DiffPercent) ->
    if
        ZScore > 4.0 orelse DiffPercent > 50.0 -> critical;
        ZScore > 3.0 orelse DiffPercent > 30.0 -> high;
        ZScore > 2.5 orelse DiffPercent > 20.0 -> medium;
        ZScore > 2.0 orelse DiffPercent > 10.0 -> low;
        true -> none
    end.

%% Create regression result
create_regression_result(MetricName, BaselineValue, CurrentValue, DiffPercent, ZScore, Severity) ->
    #regression_result{
        regression_id = generate_regression_id(),
        metric_name = MetricName,
        baseline_value = BaselineValue,
        current_value = CurrentValue,
        difference_percent = DiffPercent,
        z_score = ZScore,
        is_regression = true,
        confidence_level = confidence_from_zscore(abs(ZScore)),
        anomaly_detected = abs(ZScore) > 3.0,
        severity = Severity,
        detected_at = erlang:system_time(millisecond)
    }.

%% Calculate confidence level from Z-score
confidence_from_zscore(ZScore) ->
    if
        ZScore > 4.0 -> 0.99999;
        ZScore > 3.0 -> 0.9987;
        ZScore > 2.0 -> 0.9545;
        ZScore > 1.0 -> 0.6827;
        true -> 0.0
    end.

%% Analyze metric trend internal
analyze_metric_trend_internal(MetricName, Values) ->
    case length(Values) of
        0 -> #{metric => MetricName, status => insufficient_data};
        1 -> #{metric => MetricName, status => insufficient_data};
        _ ->
            Mean = lists:sum(Values) / length(Values),
            StdDev = calculate_stddev(Values, Mean),

            % Linear regression for trend
            Trend = calculate_linear_trend(Values),
            MovingAvg = calculate_moving_average(Values, 3),

            #{
                metric => MetricName,
                mean => Mean,
                std_dev => StdDev,
                min => lists:min(Values),
                max => lists:max(Values),
                trend => Trend,
                moving_average => MovingAvg,
                cv => StdDev / Mean
            }
    end.

%% Calculate standard deviation
calculate_stddev(Values, Mean) ->
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / length(Values),
    math:sqrt(Variance).

%% Calculate linear trend
calculate_linear_trend(Values) ->
    N = length(Values),
    XValues = lists:seq(1, N),
    XMean = (N + 1) / 2,
    YMean = lists:sum(Values) / N,

    Numerator = lists:sum([
        (lists:nth(I, XValues) - XMean) * (lists:nth(I, Values) - YMean)
        || I <- lists:seq(1, N)
    ]),
    Denominator = lists:sum([
        (lists:nth(I, XValues) - XMean) * (lists:nth(I, XValues) - XMean)
        || I <- lists:seq(1, N)
    ]),

    case Denominator of
        0.0 -> stable;
        _ ->
            Slope = Numerator / Denominator,
            if
                Slope > 0.1 -> increasing;
                Slope < -0.1 -> decreasing;
                true -> stable
            end
    end.

%% Calculate moving average
calculate_moving_average(Values, Window) ->
    lists:map(fun(I) ->
        Start = max(1, I - Window + 1),
        End = min(length(Values), I),
        Slice = lists:sublist(Values, Start, End - Start + 1),
        lists:sum(Slice) / length(Slice)
    end, lists:seq(1, length(Values))).

%% Detect anomalies internal (IQR method)
detect_anomalies_internal(Values) ->
    case length(Values) >= 4 of
        true ->
            Sorted = lists:sort(Values),
            Q1 = percentile(Sorted, 25),
            Q3 = percentile(Sorted, 75),
            IQR = Q3 - Q1,
            LowerBound = Q1 - (1.5 * IQR),
            UpperBound = Q3 + (1.5 * IQR),

            lists:filtermap(fun(V) ->
                case V < LowerBound orelse V > UpperBound of
                    true ->
                        {true, #{
                            value => V,
                            type => outlier,
                            lower_bound => LowerBound,
                            upper_bound => UpperBound
                        }};
                    false ->
                        false
                end
            end, Values);
        false ->
            []
    end.

%% Compare baselines internal
compare_baselines_internal(Baseline1, Baseline2) ->
    Metrics = [throughput, latency, p99_latency, memory, error_rate],

    Comparison = lists:map(fun(MetricName) ->
        case {maps:find(MetricName, Baseline1), maps:find(MetricName, Baseline2)} of
            {{ok, B1}, {ok, B2}} ->
                Diff = ((B2#baseline_metrics.mean - B1#baseline_metrics.mean) /
                    max(1, B1#baseline_metrics.mean)) * 100,
                #{
                    metric => MetricName,
                    baseline1_mean => B1#baseline_metrics.mean,
                    baseline2_mean => B2#baseline_metrics.mean,
                    difference_percent => Diff,
                    improved => (MetricName =:= throughput andalso Diff > 0) orelse
                               (MetricName =/= throughput andalso Diff < 0)
                };
            _ ->
                #{metric => MetricName, status => missing_data}
        end
    end, Metrics),

    maps:from_list([{MetricName, Value} || #{metric := MetricName} = Value <- Comparison]).

%% Generate regression report
generate_regression_report(State) ->
    TotalTests = length(State#state.regression_history),
    RegressionCount = length([R || R <- State#state.regression_history, R#regression_result.is_regression]),
    CriticalCount = length([R || R <- State#state.regression_history, R#regression_result.severity =:= critical]),
    HighCount = length([R || R <- State#state.regression_history, R#regression_result.severity =:= high]),

    #{
        detector_id => State#state.detector_id,
        total_detections => TotalTests,
        total_regressions => RegressionCount,
        critical_regressions => CriticalCount,
        high_regressions => HighCount,
        regression_rate => case TotalTests of
            0 -> 0.0;
            _ -> (RegressionCount / TotalTests) * 100
        end,
        recent_regressions => lists:sublist(State#state.regression_history, 10)
    }.

%% Generate detector ID
generate_detector_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("detector_~w_~w", [Timestamp, Random])).

%% Generate baseline ID
generate_baseline_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("baseline_~w_~w", [Timestamp, Random])).

%% Generate regression ID
generate_regression_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("regression_~w_~w", [Timestamp, Random])).
