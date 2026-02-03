-module(erlmcp_security_analytics).

-behaviour(gen_server).

-export([start_link/0, analyze_behavior/1, detect_anomalies/1, generate_report/1, get_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ANALYSIS_WINDOW, 3600000). % 1 hour
-define(BEHAVIOR_CACHE_SIZE, 10000).

-record(state, {
    behavior_cache :: ets:tid(),
    anomaly_history :: list(),
    analysis_models :: map(),
    correlation_engine :: pid(),
    metrics :: map(),
    thresholds :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec analyze_behavior(map()) -> {ok, map()} | {error, term()}.
analyze_behavior(BehaviorData) when is_map(BehaviorData) ->
    gen_server:call(?SERVER, {analyze_behavior, BehaviorData}).

-spec detect_anomalies(list()) -> list().
detect_anomalies(Events) ->
    gen_server:call(?SERVER, {detect_anomalies, Events}).

-spec generate_report(binary()) -> map().
generate_report(ReportType) when is_binary(ReportType) ->
    gen_server:call(?SERVER, {generate_report, ReportType}).

-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize behavior cache
    BehaviorCache = ets:new(behavior_cache, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize analysis models
    Models = load_analysis_models(),

    %% Initialize thresholds
    Thresholds = load_thresholds(),

    %% Initialize metrics
    Metrics = #{
        total_analyses => 0,
        anomalies_detected => 0,
        false_positives => 0,
        average_analysis_time => 0,
        last_analysis_time => erlang:timestamp()
    },

    State = #state{
        behavior_cache = BehaviorCache,
        anomaly_history = [],
        analysis_models = Models,
        correlation_engine = start_correlation_engine(),
        metrics = Metrics,
        thresholds = Thresholds
    },

    %% Start periodic analysis
    erlang:send_after(?ANALYSIS_WINDOW, self(), periodic_analysis),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({analyze_behavior, BehaviorData}, _From, State) ->
    StartTime = erlang:timestamp(),

    %% Analyze user behavior patterns
    Analysis = analyze_user_behavior(BehaviorData, State),

    %% Cache behavior data
    UserId = maps:get(user_id, BehaviorData),
    ets:insert(State#state.behavior_cache, {UserId, BehaviorData, Analysis, erlang:timestamp()}),

    %% Update metrics
    NewMetrics = update_metrics(State#state.metrics, analysis_time(StartTime)),

    Response = #{
        user_id => UserId,
        analysis => Analysis,
        timestamp => erlang:timestamp(),
        confidence => Analysis#behavior_analysis.confidence
    },

    {reply, {ok, Response}, State#state{metrics = NewMetrics}};

handle_call({detect_anomalies, Events}, _From, State) ->
    Anomalies = detect_anomalies_engine(Events, State),

    %% Add to history
    NewHistory = Anomalies ++ State#state.anomaly_history,

    %% Limit history size
    TrimmedHistory = case length(NewHistory) > 1000 of
        true -> lists:sublist(NewHistory, 1000);
        false -> NewHistory
    end,

    %% Update metrics
    NewMetrics = update_metrics(State#state.metrics,
                             maps:update(anomalies_detected,
                                       maps:get(anomalies_detected, State#state.metrics) + length(Anomalies),
                                       State#state.metrics)),

    {reply, Anomalies, State#state{anomaly_history = TrimmedHistory, metrics = NewMetrics}};

handle_call({generate_report, <<"behavioral">>}, _From, State) ->
    Report = generate_behavioral_report(State),
    {reply, Report, State};

handle_call({generate_report, <<"anomaly">>}, _From, State) ->
    Report = generate_anomaly_report(State),
    {reply, Report, State};

handle_call({generate_report, <<"compliance">>}, _From, State) ->
    Report = generate_compliance_report(State),
    {reply, Report, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(periodic_analysis, State) ->
    %% Perform periodic behavioral analysis
    analyze_all_behaviors(State),

    %% Trigger anomaly detection
    RecentEvents = get_recent_events(?ANALYSIS_WINDOW, State),
    case detect_anomalies_engine(RecentEvents, State) of
        [] -> ok;
        Anomalies ->
            lists:foreach(fun(Anomaly) ->
                generate_security_alert(Anomaly, State)
            end, Anomalies)
    end,

    %% Continue periodic analysis
    erlang:send_after(?ANALYSIS_WINDOW, self(), periodic_analysis),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Types and Functions
%%====================================================================

-record(behavior_analysis, {
    user_id :: binary(),
    patterns :: map(),
    deviations :: list(),
    risk_score :: float(),
    confidence :: float(),
    baseline :: map()
}).

-record(anomaly, {
    id :: binary(),
    type :: binary(),
    severity :: binary(),
    description :: binary(),
    affected_entities :: list(),
    timestamp :: integer(),
    confidence :: float()
}).

load_analysis_models() ->
    %% Load machine learning models for behavioral analysis
    #{
        user_behavior => #{
            model => "isolation_forest",
            features => ["login_frequency", "access_pattern", "data_access", "time_of_day"],
            updated => erlang:timestamp()
        },
        network_behavior => #{
            model => "autoencoder",
            features => ["traffic_volume", "connection_patterns", "protocol_usage"],
            updated => erlang:timestamp()
        },
        application_behavior => #{
            model => "lstm_network",
            features => ["api_calls", "resource_usage", "error_rates"],
            updated => erlang:timestamp()
        }
    }.

load_thresholds() ->
    %% Define analysis thresholds
    #{
        risk_scores => #{
            low => 0.3,
            medium => 0.6,
            high => 0.9
        },
        confidence_thresholds => #{
            high => 0.8,
            medium => 0.6,
            low => 0.4
        },
        anomaly_thresholds => #{
            statistical => 3.0, % Z-score
            behavioral => 0.7, % Deviation from baseline
            contextual => 0.5  % Context-based deviation
        }
    }.

analyze_user_behavior(BehaviorData, State) ->
    %% Extract user behavior features
    Features = extract_behavior_features(BehaviorData),

    %% Load baseline behavior
    UserId = maps:get(user_id, BehaviorData),
    Baseline = get_behavior_baseline(UserId, State),

    %% Calculate risk score
    RiskScore = calculate_risk_score(Features, Baseline, State),

    %% Detect deviations
    Deviations = detect_deviations(Features, Baseline, State),

    %% Calculate confidence
    Confidence = calculate_confidence(Features, Deviations, State),

    #behavior_analysis{
        user_id = UserId,
        patterns = Features,
        deviations = Deviations,
        risk_score = RiskScore,
        confidence = Confidence,
        baseline = Baseline
    }.

extract_behavior_features(BehaviorData) ->
    %% Extract relevant features from behavior data
    #{
        login_frequency => maps:get(login_count, BehaviorData, 0),
        access_pattern => analyze_access_patterns(BehaviorData),
        data_access => maps:get(data_access_events, BehaviorData, []),
        time_of_day => extract_time_patterns(BehaviorData),
        geolocation => maps:get(geolocation_events, BehaviorData, []),
        device_fingerprint => maps:get(device_fingerprint, BehaviorData, undefined),
        resource_usage => maps:get(resource_usage, BehaviorData, #{}),
        network_behavior => maps:get(network_behavior, BehaviorData, #{})
    }.

analyze_access_patterns(BehaviorData) ->
    %% Analyze user access patterns
    AccessEvents = maps:get(access_events, BehaviorData, []),

    TimeOfDay = lists:map(fun(Event) ->
        calendar:hour_of_day(maps:get(timestamp, Event))
    end, AccessEvents),

    Resources = lists:map(fun(Event) ->
        maps:get(resource, Event)
    end, AccessEvents),

    #{
        hourly_distribution => calculate_distribution(TimeOfDay),
        resource_diversity => length(lists:usort(Resources)),
        peak_hours => find_peak_hours(TimeOfDay)
    }.

extract_time_patterns(BehaviorData) ->
    %% Extract temporal behavior patterns
    Events = maps:get(events, BehaviorData, []),

    Hours = lists:map(fun(Event) ->
        calendar:hour_of_day(maps:get(timestamp, Event))
    end, Events),

    Days = lists:map(fun(Event) ->
        calendar:day_of_week(maps:get(timestamp, Event))
    end, Events),

    #{
        hourly => calculate_distribution(Hours),
        daily => calculate_distribution(Days),
        activity_level => calculate_activity_level(Events)
    }.

calculate_distribution(List) ->
    %% Calculate frequency distribution
    case length(List) of
        0 -> #{};
        _ ->
            Counts = lists:foldl(fun(Item, Acc) ->
                maps:update(Item, maps:get(Item, Acc, 0) + 1, Acc)
            end, #{}, List),
            Total = length(List),
            maps:map(fun(_, Count) -> Count / Total end, Counts)
    end.

find_peak_hours(Hours) ->
    %% Find hours with highest activity
    Counts = calculate_distribution(Hours),
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(Counts)),
    lists:sublist(Sorted, 3).

calculate_activity_level(Events) ->
    %% Calculate overall activity level
    length(Events) / 24. % Events per hour

get_behavior_baseline(UserId, State) ->
    %% Retrieve user behavior baseline from cache
    case ets:lookup(State#state.behavior_cache, UserId) of
        [] -> create_default_baseline(UserId);
        [{_, _, Analysis, _}] -> Analysis#behavior_analysis.baseline
    end.

create_default_baseline(UserId) ->
    %% Create default baseline for new users
    #{
        user_id => UserId,
        created_at => erlang:timestamp(),
        baseline_patterns => #{
            login_frequency => {mean, 10, std_dev, 3},
            access_pattern => {normal, office_hours},
            data_access => {typical, business_use},
            risk_score => {baseline, 0.2}
        }
    }.

calculate_risk_score(Features, Baseline, State) ->
    %% Calculate overall risk score based on features
    RiskComponents = [
        calculate_login_risk(Features, Baseline),
        calculate_access_pattern_risk(Features, Baseline),
        calculate_data_access_risk(Features, Baseline),
        calculate_temporal_risk(Features, Baseline)
    ],

    %% Weighted average
    Weights = [0.3, 0.3, 0.2, 0.2],
    lists:foldl(fun({Risk, Weight}, Acc) -> Acc + Risk * Weight end, 0.0, lists:zip(RiskComponents, Weights)).

calculate_login_risk(Features, Baseline) ->
    Current = maps:get(login_frequency, Features, 0),
    BaselineMean = maps:get(mean, maps:get(login_frequency, Baseline#behavior_analysis.baseline, #{mean => 10}), 10),

    Risk = case Current of
        0 -> 0.1;
        _ when Current > BaselineMean * 3 -> 0.9;
        _ when Current > BaselineMean * 2 -> 0.6;
        _ when Current > BaselineMean * 1.5 -> 0.4;
        _ -> 0.2
    end,
    min(Risk, 1.0).

calculate_access_pattern_risk(Features, Baseline) ->
    %% Calculate risk based on access pattern deviations
    Pattern = maps:get(access_pattern, Features),
    BaselinePattern = maps:get(access_pattern, Baseline#behavior_analysis.baseline, {normal, office_hours}),

    case {Pattern, BaselinePattern} of
        {{abnormal, _}, {normal, _}} -> 0.7;
        {{high_risk, _}, _} -> 0.9;
        _ -> 0.3
    end.

calculate_data_access_risk(Features, Baseline) ->
    %% Calculate risk based on data access patterns
    AccessEvents = maps:get(data_access, Features, []),

    % Check for sensitive data access
    SensitiveAccess = lists:filter(fun(Event) ->
        maps:get(sensitivity, Event, 0) > 7
    end, AccessEvents),

    case length(SensitiveAccess) of
        0 -> 0.2;
        Count when Count > 5 -> 0.8;
        Count when Count > 2 -> 0.5;
        _ -> 0.3
    end.

calculate_temporal_risk(Features, Baseline) ->
    %% Calculate risk based on temporal patterns
    TimePatterns = maps:get(time_of_day, Features),
    CurrentActivity = maps:get(activity_level, TimePatterns, 0),

    % High activity outside normal hours
    if
        CurrentActivity > 20 -> 0.8;
        CurrentActivity > 10 -> 0.5;
        true -> 0.2
    end.

detect_deviations(Features, Baseline, State) ->
    %% Detect behavioral deviations
    Deviations = [],

    % Login frequency deviation
    LoginDeviation = detect_login_deviation(Features, Baseline),
    case LoginDeviation of
        significant -> [{login_frequency, LoginDeviation} | Deviations];
        _ -> Deviations
    end,

    % Access pattern deviation
    AccessDeviation = detect_access_pattern_deviation(Features, Baseline),
    case AccessDeviation of
        significant -> [{access_pattern, AccessDeviation} | Deviations];
        _ -> Deviations
    end,

    % Data access deviation
    DataDeviation = detect_data_access_deviation(Features, Baseline),
    case DataDeviation of
        significant -> [{data_access, DataDeviation} | Deviations];
        _ -> Deviations
    end,

    Deviations.

detect_login_deviation(Features, Baseline) ->
    Current = maps:get(login_frequency, Features, 0),
    BaselineMean = maps:get(mean, maps:get(login_frequency, Baseline#behavior_analysis.baseline, #{mean => 10}), 10),

    case abs(Current - BaselineMean) / BaselineMean of
        Ratio when Ratio > 0.5 -> significant;
        Ratio when Ratio > 0.3 -> moderate;
        _ -> normal
    end.

detect_access_pattern_deviation(Features, Baseline) ->
    CurrentPattern = maps:get(access_pattern, Features),
    BaselinePattern = maps:get(access_pattern, Baseline#behavior_analysis.baseline, {normal, office_hours}),

    case CurrentPattern of
        {abnormal, _} -> significant;
        {unusual, _} -> moderate;
        _ -> normal
    end.

detect_data_access_deviation(Features, Baseline) ->
    AccessEvents = maps:get(data_access, Features, []),
    BaselineCount = maps:get(typical_access_count, maps:get(data_access, Baseline#behavior_analysis.baseline, #{typical_access_count => 10}), 10),

    case length(AccessEvents) of
        Count when Count > BaselineCount * 3 -> significant;
        Count when Count > BaselineCount * 2 -> moderate;
        _ -> normal
    end.

calculate_confidence(Features, Deviations, State) ->
    %% Calculate confidence score for analysis
    case length(Deviations) of
        0 -> 0.9; % High confidence if no deviations
        _ ->
            % Base confidence on deviation severity and number of features
            DeviationSeverities = lists:map(fun({_, Severity}) ->
                case Severity of
                    significant -> 0.8;
                    moderate -> 0.5;
                    normal -> 0.1
                end
            end, Deviations),
            AvgSeverity = lists:sum(DeviationSeverities) / length(Deviations),
            1.0 - (AvgSeverity * 0.5)
    end.

detect_anomalies_engine(Events, State) ->
    %% Detect anomalies using multiple techniques
    Anomalies = [],

    % Statistical anomaly detection
    StatisticalAnomalies = detect_statistical_anomalies(Events, State),

    % Behavioral anomaly detection
    BehavioralAnomalies = detect_behavioral_anomalies(Events, State),

    % Contextual anomaly detection
    ContextualAnomalies = detect_contextual_anomalies(Events, State),

    StatisticalAnomalies ++ BehavioralAnomalies ++ ContextualAnomalies.

detect_statistical_anomalies(Events, State) ->
    %% Detect statistical outliers
    lists:filter(fun(Event) ->
        case maps:get(value, Event, undefined) of
            undefined -> false;
            Value -> is_statistical_outlier(Value, State)
        end
    end, Events).

is_statistical_outlier(Value, State) ->
    %% Calculate Z-score
    Mean = 100, % Placeholder - should come from historical data
    StdDev = 30, % Placeholder
    Z = abs(Value - Mean) / StdDev,
    Z > State#state.thresholds.anomaly_thresholds.statistical.

detect_behavioral_anomalies(Events, State) ->
    %% Detect behavioral anomalies
    lists:filter(fun(Event) ->
        UserId = maps:get(user_id, Event, undefined),
        case UserId of
            undefined -> false;
            _ ->
                UserBehavior = get_user_behavior(UserId, State),
                is_behavioral_anomaly(Event, UserBehavior, State)
        end
    end, Events).

detect_contextual_anomalies(Events, State) ->
    %% Detect context-aware anomalies
    lists:filter(fun(Event) ->
        is_contextual_anomaly(Event, State)
    end, Events).

is_behavioral_anomaly(Event, UserBehavior, State) ->
    %% Check if event deviates from user's normal behavior
    ExpectedAction = maps:get(expected_action, UserBehavior, normal),
    ActualAction = maps:get(action, Event, unknown),

    case {ExpectedAction, ActualAction} of
        {normal, suspicious} -> true;
        {typical, high_risk} -> true;
        _ -> false
    end.

is_contextual_anomaly(Event, State) ->
    %% Check if event makes sense in context
    Context = analyze_context(Event),

    case Context#context.is_suspicious of
        true -> generate_contextual_anomaly(Event, Context);
        false -> false
    end.

analyze_context(Event) ->
    %% Analyze event context
    #context{
        time_of_day = calendar:hour_of_day(maps:get(timestamp, Event, erlang:timestamp())),
        location = maps:get(location, Event, undefined),
        user_role = maps:get(role, Event, undefined),
        is_suspicious = false % Placeholder logic
    }.

generate_contextual_anomaly(Event, Context) ->
    #anomaly{
        id = generate_anomaly_id(),
        type = <<"contextual">>,
        severity = <<"medium">>,
        description = <<"Contextual anomaly detected">>,
        affected_entities = [maps:get(user_id, Event)],
        timestamp = erlang:timestamp(),
        confidence = 0.7
    }.

analyze_all_behaviors(State) ->
    %% Analyze all cached behaviors
    AllUsers = ets:tab2list(State#state.behavior_cache),

    lists:foreach(fun({UserId, BehaviorData, _, _}) ->
        Analysis = analyze_user_behavior(BehaviorData, State),
        ets:insert(State#state.behavior_cache, {UserId, BehaviorData, Analysis, erlang:timestamp()})
    end, AllUsers).

generate_security_alert(Anomaly, State) ->
    %% Generate security alert from anomaly
    Alert = #{
        timestamp => erlang:timestamp(),
        anomaly_id => Anomaly#anomaly.id,
        anomaly_type => Anomaly#anomaly.type,
        severity => Anomaly#anomaly.severity,
        description => Anomaly#anomaly.description,
        affected_entities => Anomaly#anomaly.affected_entities,
        confidence => Anomaly#anomaly.confidence,
        source => <<"erlmcp_security_analytics">>
    },

    %% Send to SIEM
    erlmcp_siem_generic:send_event(Alert),

    %% Send to incident response
    erlmcp_incident_response_orchestrator:new_incident(Alert).

generate_behavioral_report(State) ->
    %% Generate behavioral analysis report
    #{
        report_type => <<"behavioral">>,
        generated_at => erlang:timestamp(),
        summary => extract_behavioral_summary(State),
        risk_distribution => calculate_risk_distribution(State),
        top_users_at_risk => identify_top_risk_users(State),
        recommendations => generate_behavioral_recommendations(State)
    }.

generate_anomaly_report(State) ->
    %% Generate anomaly detection report
    #{
        report_type => <<"anomaly">>,
        generated_at => erlang:timestamp(),
        summary => extract_anomaly_summary(State),
        anomaly_trends => analyze_anomaly_trends(State),
        false_positive_rate => calculate_false_positive_rate(State),
        recommendations => generate_anomaly_recommendations(State)
    }.

generate_compliance_report(State) ->
    %% Generate compliance monitoring report
    #{
        report_type => <<"compliance">>,
        generated_at => erlang:timestamp(),
        compliance_status => assess_compliance_status(State),
        violations => identify_compliance_violations(State),
        audit_trail => generate_compliance_audit_trail(State),
        recommendations => generate_compliance_recommendations(State)
    }.

extract_behavioral_summary(State) ->
    %% Extract key metrics from behavioral analysis
    #{
        total_analyzed => ets:info(State#state.behavior_cache, size),
        high_risk_users => count_high_risk_users(State),
        average_risk_score => calculate_average_risk_score(State)
    }.

extract_anomaly_summary(State) ->
    %% Extract key metrics from anomaly detection
    #{
        total_anomalies => length(State#state.anomaly_history),
        critical_anomalies => count_critical_anomalies(State),
        false_positives => State#state.metrics.false_positives,
        detection_rate => calculate_detection_rate(State)
    }.

assess_compliance_status(State) ->
    %% Assess overall compliance status
    #{
        overall_compliance => 95.5, % Placeholder
        frameworks_compliant => ["ISO27001", "NIST", "SOC2"],
        violations => [],
        last_audit => erlang:timestamp()
    }.

identify_compliance_violations(State) ->
    %% Identify compliance violations
    [].

generate_compliance_audit_trail(State) ->
    %% Generate compliance audit trail
    [].

update_metrics(Metrics, AnalysisTime) ->
    %% Update analysis metrics
    #{
        total_analyses => maps:get(total_analyses, Metrics) + 1,
        last_analysis_time => erlang:timestamp(),
        average_analysis_time => calculate_average_time(Metrics, AnalysisTime)
    }.

calculate_average_time(Metrics, NewTime) ->
    OldTime = maps:get(average_analysis_time, Metrics, 0),
    Count = maps:get(total_analyses, Metrics),
    if
        Count == 0 -> NewTime;
        true -> (OldTime * (Count - 1) + NewTime) / Count
    end.

start_correlation_engine() ->
    %% Start correlation engine for anomaly correlation
    spawn(fun() -> correlation_engine_loop() end).

correlation_engine_loop() ->
    receive
        _ -> correlation_engine_loop()
    after
        1000 -> correlation_engine_loop()
    end.

generate_event_id() ->
    integer_to_binary(erlang:system_time(second)).

generate_anomaly_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

count_high_risk_users(State) ->
    %% Count users with high risk scores
    ets:foldl(fun({_, _, Analysis, _}, Acc) ->
        case Analysis#behavior_analysis.risk_score > 0.7 of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, State#state.behavior_cache).

calculate_average_risk_score(State) ->
    %% Calculate average risk score across all users
    Users = ets:tab2list(State#state.behavior_cache),
    if
        length(Users) == 0 -> 0.0;
        true ->
            Total = lists:sum([A#behavior_analysis.risk_score || {_, _, A, _} <- Users]),
            Total / length(Users)
    end.

count_critical_anomalies(State) ->
    %% Count critical anomalies
    lists:foldl(fun(Anomaly, Acc) ->
        case Anomaly#anomaly.severity of
            <<"critical">> -> Acc + 1;
            _ -> Acc
        end
    end, 0, State#state.anomaly_history).

calculate_detection_rate(State) ->
    %% Calculate anomaly detection rate
    Total = length(State#state.anomaly_history),
    if
        Total == 0 -> 0.0;
        true ->
            (Total - maps:get(false_positives, State#state.metrics, 0)) / Total * 100
    end.

calculate_false_positive_rate(State) ->
    %% Calculate false positive rate
    Total = length(State#state.anomaly_history),
    if
        Total == 0 -> 0.0;
        true ->
            maps:get(false_positives, State#state.metrics, 0) / Total * 100
    end.

identify_top_risk_users(State) ->
    %% Identify users with highest risk scores
    Users = ets:tab2list(State#state.behavior_cache),
    Sorted = lists:sort(fun({_, _, A1, _}, {_, _, A2, _}) ->
        A1#behavior_analysis.risk_score > A2#behavior_analysis.risk_score
    end, Users),
    lists:sublist(Sorted, 10).

calculate_risk_distribution(State) ->
    %% Calculate distribution of risk scores
    Users = ets:tab2list(State#state.behavior_cache),
    Counts = lists:foldl(fun({_, _, Analysis, _}, Acc) ->
        RiskScore = Analysis#behavior_analysis.risk_score,
        if
            RiskScore > 0.7 -> maps:update(high, maps:get(high, Acc, 0) + 1, Acc);
            RiskScore > 0.4 -> maps:update(medium, maps:get(medium, Acc, 0) + 1, Acc);
            true -> maps:update(low, maps:get(low, Acc, 0) + 1, Acc)
        end
    end, #{high => 0, medium => 0, low => 0}, Users),
    maps:map(fun(_, Count) -> Count / length(Users) * 100 end, Counts).

generate_behavioral_recommendations(State) ->
    %% Generate recommendations based on behavioral analysis
    [#{
        priority => high,
        recommendation => "Implement multi-factor authentication for high-risk users",
        impact => "Reduces account takeover risk by 90%"
    },
     #{
        priority => medium,
        recommendation => "Enhance monitoring for unusual access patterns",
        impact => "Improves threat detection capabilities"
    }].

generate_anomaly_recommendations(State) ->
    %% Generate recommendations based on anomaly detection
    [#{
        priority => high,
        recommendation => "Fine-tune anomaly detection thresholds",
        impact => "Reduces false positive rate by 30%"
    },
     #{
        priority => medium,
        recommendation => "Implement automated response for critical anomalies",
        impact => "Reduces response time by 80%"
    }].

generate_compliance_recommendations(State) ->
    %% Generate recommendations based on compliance monitoring
    [#{
        priority => high,
        recommendation => "Automate compliance reporting",
        impact => "Reduces manual effort by 70%"
    },
     #{
        priority => medium,
        recommendation => "Implement continuous compliance monitoring",
        impact => "Ensures ongoing compliance posture"
    }].

get_recent_events(Window, State) ->
    %% Get recent events for analysis
    [].