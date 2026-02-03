%%%-------------------------------------------------------------------
%%% @doc
%%% Anomaly Detection Module for erlmcp v3 Adaptive Learning
%%%
%%% Implements statistical anomaly detection using:
%%% - Z-score analysis (sigma thresholds)
%%% - Moving average baselines
%%% - Exponentially weighted moving variance
%%%
%%% Detects operational anomalies that may indicate:
%%% - Performance degradation
%%% - Resource exhaustion
%%% - Behavioral deviations
%%% - Emerging failure modes
%%%
%%% == Algorithm ==
%%%
%%% Anomalies are detected when:
%%%   |observed - mean| / std_dev > sigma_threshold
%%%
%%% Default threshold: 3 sigma (99.7% confidence interval)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_anomaly_detector).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([observe/3, observe/4]).
-export([check/2]).
-export([get_baseline/1, get_all_baselines/0]).
-export([set_threshold/2]).
-export([get_recent_anomalies/1, get_recent_anomalies/2]).
-export([reset_baselines/0, reset_baseline/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include("erlmcp_learning.hrl").

%%%====================================================================
%%% Type Definitions
%%%====================================================================

-type detector_config() :: #{
    window_size => pos_integer(),
    sigma_threshold => float(),
    min_observations => pos_integer()
}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the anomaly detector with default config
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Observe a metric value (for building baseline)
-spec observe(binary(), number()) -> ok.
observe(MetricName, Value) ->
    observe(MetricName, Value, #{}).

-spec observe(binary(), number(), map()) -> ok.
observe(MetricName, Value, Metadata) ->
    gen_server:cast(?MODULE, {observe, MetricName, Value, Metadata}).

%% @doc check features for anomalies
-spec check(#anomaly_detector{}, feature_map()) -> {[#anomaly{}], #anomaly_detector{}}.
check(Detector, Features) ->
    Baselines = Detector#anomaly_detector.baselines,
    SigmaThreshold = Detector#anomaly_detector.sigma_threshold,
    MinObs = Detector#anomaly_detector.min_observations,

    {Anomalies, UpdatedBaselines} = maps:fold(fun(MetricName, Value, {AnomAcc, BaselineAcc}) ->
        case maps:get(MetricName, Baselines, undefined) of
            undefined ->
                %% Create new baseline
                NewBaseline = initialize_baseline(MetricName, Value),
                {AnomAcc, maps:put(MetricName, NewBaseline, BaselineAcc)};
            Baseline ->
                case Baseline#statistical_baseline.sample_count of
                    N when N < MinObs ->
                        %% Not enough observations yet, update baseline
                        NewBaseline = update_baseline(Baseline, Value),
                        {AnomAcc, maps:put(MetricName, NewBaseline, BaselineAcc)};
                    _ ->
                        %% Check for anomaly
                        SigmaScore = calculate_sigma_score(Value, Baseline),
                        IsAnomaly = SigmaScore > SigmaThreshold,

                        NewBaseline = update_baseline(Baseline, Value),
                        NewAnomalies = case IsAnomaly of
                            true ->
                                Anomaly = create_anomaly(MetricName, Value, Baseline, SigmaScore),
                                [Anomaly | AnomAcc];
                            false ->
                                AnomAcc
                        end,
                        {NewAnomalies, maps:put(MetricName, NewBaseline, BaselineAcc)}
                end
        end
    end, {[], Baselines}, Features),

    NewDetector = Detector#anomaly_detector{baselines = UpdatedBaselines},

    %% Notify alert handlers if anomalies detected
    case Anomalies of
        [] -> ok;
        _ -> notify_anomaly_handlers(Anomalies, NewDetector)
    end,

    {Anomalies, NewDetector}.

%% @doc Get baseline for a specific metric
-spec get_baseline(binary()) -> {ok, #statistical_baseline{}} | {error, not_found}.
get_baseline(MetricName) ->
    gen_server:call(?MODULE, {get_baseline, MetricName}).

%% @doc Get all baselines
-spec get_all_baselines() -> {ok, #{binary() => #statistical_baseline{}}}.
get_all_baselines() ->
    gen_server:call(?MODULE, get_all_baselines).

%% @doc Set sigma threshold for anomaly detection
-spec set_threshold(binary() | number(), number()) -> ok.
set_threshold(MetricName, Threshold) when is_number(Threshold) ->
    gen_server:call(?MODULE, {set_threshold, global, Threshold});
set_threshold(MetricName, Threshold) ->
    gen_server:call(?MODULE, {set_threshold, MetricName, Threshold}).

%% @doc Get recent anomalies
-spec get_recent_anomalies(pos_integer()) -> [#anomaly{}].
get_recent_anomalies(Limit) ->
    get_recent_anomalies(?MODULE, Limit).

%% @doc Get recent anomalies from a detector instance
-spec get_recent_anomalies(#anomaly_detector{} | pid(), pos_integer()) -> [#anomaly{}].
get_recent_anomalies(Detector, Limit) when is_record(Detector, anomaly_detector) ->
    %% Return from internal state
    lists:sublist(Detector#anomaly_detector.recent_anomalies, Limit);
get_recent_anomalies(DetectorPid, Limit) when is_pid(DetectorPid) ->
    gen_server:call(DetectorPid, {get_anomalies, Limit}).

%% @doc Reset all baselines
-spec reset_baselines() -> ok.
reset_baselines() ->
    gen_server:call(?MODULE, reset_baselines).

%% @doc Reset a specific baseline
-spec reset_baseline(binary()) -> ok.
reset_baseline(MetricName) ->
    gen_server:call(?MODULE, {reset_baseline, MetricName}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init(Opts) ->
    WindowSize = maps:get(window_size, Opts, ?ANOMALY_WINDOW_SIZE),
    SigmaThreshold = maps:get(sigma_threshold, Opts, ?ANOMALY_SIGMA_THRESHOLD),
    MinObs = maps:get(min_observations, Opts, ?ANOMALY_MIN_OBSERVATIONS),

    State = #anomaly_detector{
        baselines = #{},
        window_size = WindowSize,
        sigma_threshold = SigmaThreshold,
        min_observations = MinObs,
        alert_handlers = [],
        recent_anomalies = []
    },

    ?LOG_INFO("Anomaly detector initialized: sigma=~p, window=~p",
              [SigmaThreshold, WindowSize]),

    {ok, State}.

handle_call({get_baseline, MetricName}, _From, State) ->
    case maps:get(MetricName, State#anomaly_detector.baselines, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Baseline -> {reply, {ok, Baseline}, State}
    end;

handle_call(get_all_baselines, _From, State) ->
    {reply, {ok, State#anomaly_detector.baselines}, State};

handle_call({set_threshold, MetricName, Threshold}, _From, State) ->
    NewState = case MetricName of
        global ->
            State#anomaly_detector{sigma_threshold = Threshold};
        _ ->
            %% Set per-metric threshold (stored in baseline metadata)
            Baselines = State#anomaly_detector.baselines,
            UpdatedBaselines = maps:update_with(MetricName,
                fun(B) -> B#statistical_baseline{metadata = maps:put(sigma_threshold, Threshold,
                                                                  B#statistical_baseline.metadata)} end,
                #statistical_baseline{metric_name = MetricName,
                                     metadata = #{sigma_threshold => Threshold}},
                Baselines),
            State#anomaly_detector{baselines = UpdatedBaselines}
    end,
    {reply, ok, NewState};

handle_call({get_anomalies, Limit}, _From, State) ->
    Anomalies = lists:sublist(State#anomaly_detector.recent_anomalies, Limit),
    {reply, Anomalies, State};

handle_call(reset_baselines, _From, State) ->
    {reply, ok, State#anomaly_detector{baselines = #{}, recent_anomalies = []}};

handle_call({reset_baseline, MetricName}, _From, State) ->
    NewBaselines = maps:remove(MetricName, State#anomaly_detector.baselines),
    {reply, ok, State#anomaly_detector{baselines = NewBaselines}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({observe, MetricName, Value, Metadata}, State) ->
    %% Direct observation (doesn't check for anomalies)
    NewBaselines = case maps:get(MetricName, State#anomaly_detector.baselines, undefined) of
        undefined ->
            NewBaseline = initialize_baseline(MetricName, Value),
            maps:put(MetricName, NewBaseline, State#anomaly_detector.baselines);
        Baseline ->
            UpdatedBaseline = update_baseline(Baseline, Value),
            maps:put(MetricName, UpdatedBaseline, State#anomaly_detector.baselines)
    end,
    {noreply, State#anomaly_detector{baselines = NewBaselines}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @private Initialize a new baseline
-spec initialize_baseline(binary(), number()) -> #statistical_baseline{}.
initialize_baseline(MetricName, InitialValue) ->
    #statistical_baseline{
        metric_name = MetricName,
        mean = InitialValue * 1.0,
        variance = 0.0,
        std_dev = 0.0,
        min = InitialValue,
        max = InitialValue,
        sample_count = 1,
        last_updated = erlang:system_time(millisecond)
    }.

%% @private Update baseline with new observation
-spec update_baseline(#statistical_baseline{}, number()) -> #statistical_baseline{}.
update_baseline(Baseline, Value) ->
    OldCount = Baseline#statistical_baseline.sample_count,
    OldMean = Baseline#statistical_baseline.mean,
    OldVariance = Baseline#statistical_baseline.variance,

    %% Welford's online algorithm for variance
    NewCount = OldCount + 1,
    NewMean = OldMean + (Value - OldMean) / NewCount,

    NewVariance = case OldCount < 2 of
        true -> 0.0;
        false ->
            Delta = Value - OldMean,
            Delta2 = Value - NewMean,
            OldVariance + (Delta * Delta2) / NewCount
    end,

    NewStdDev = math:sqrt(max(0.0, NewVariance)),

    #statistical_baseline{
        metric_name = Baseline#statistical_baseline.metric_name,
        mean = NewMean,
        variance = NewVariance,
        std_dev = NewStdDev,
        min = min(Baseline#statistical_baseline.min, Value),
        max = max(Baseline#statistical_baseline.max, Value),
        sample_count = NewCount,
        last_updated = erlang:system_time(millisecond)
    }.

%% @private Calculate sigma score (z-score)
-spec calculate_sigma_score(number(), #statistical_baseline{}) -> float().
calculate_sigma_score(Value, Baseline) ->
    StdDev = Baseline#statistical_baseline.std_dev,
    Mean = Baseline#statistical_baseline.mean,

    case StdDev > 0 of
        true -> abs(Value - Mean) / StdDev;
        false -> 0.0
    end.

%% @private Create anomaly record
-spec create_anomaly(binary(), number(), #statistical_baseline{}, float()) -> #anomaly{}.
create_anomaly(MetricName, ObservedValue, Baseline, SigmaScore) ->
    Severity = case SigmaScore of
        S when S >= 5.0 -> critical;
        S when S >= 4.0 -> high;
        S when S >= 3.0 -> medium;
        _ -> low
    end,

    #anomaly{
        id = base64:encode(crypto:strong_rand_bytes(12)),
        severity = Severity,
        metric_name = MetricName,
        observed_value = ObservedValue,
        expected_value = Baseline#statistical_baseline.mean,
        deviation = ObservedValue - Baseline#statistical_baseline.mean,
        sigma_score = SigmaScore,
        timestamp = erlang:system_time(millisecond),
        context = #{
            baseline_samples => Baseline#statistical_baseline.sample_count,
            std_dev => Baseline#statistical_baseline.std_dev,
            baseline_age => erlang:system_time(millisecond) - Baseline#statistical_baseline.last_updated
        }
    }.

%% @private Notify alert handlers of anomalies
-spec notify_anomaly_handlers([#anomaly{}], #anomaly_detector{}) -> ok.
notify_anomaly_handlers(_Anomalies, _Detector) ->
    %% In production, send to registered handlers
    ok.
