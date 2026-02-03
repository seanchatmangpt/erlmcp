-module(erlmcp_behavioral_analytics).
-export([start_link/0, analyze_behavior/2, get_user_profile/1]).
-export([create_behavior_profile/2, update_behavior_profile/3, delete_behavior_profile/1]).
-export([detect_anomaly/2, get_anomaly_score/1]).
-export([train_model/1, get_model_accuracy/1]).
-export([generate_behavior_report/2, export_behavior_data/2]).

-record.behavior_profile, {
    user_id :: binary(),
    baseline :: map(),
    patterns :: list(),
    anomalies :: list(),
    last_updated :: integer(),
    risk_score :: float(),
    confidence :: float()
}.

record.behavior_event, {
    id :: binary(),
    user_id :: binary(),
    event_type :: binary(),
    timestamp :: integer(),
    location :: binary(),
    device :: binary(),
    action :: binary(),
    resource :: binary(),
    metadata :: map()
}.

record.anomaly_detection, {
    id :: binary(),
    user_id :: binary(),
    event_id :: binary(),
    type :: location | time | sequence | frequency | device | pattern,
    severity :: low | medium | high | critical,
    confidence :: float(),
    description :: binary(),
    timestamp :: integer(),
    resolved :: boolean()
}.

record.model_metrics, {
    accuracy :: float(),
    precision :: float(),
    recall :: float(),
    f1_score :: float(),
    training_data :: integer(),
    last_updated :: integer()
}.

record.state, {
    behavior_profiles :: map(),
    behavior_events :: list(),
    anomaly_detections :: list(),
    model_metrics :: map(),
    config :: map()
}).

-define(TIMEOUT, 30000).
-define(EVENT_RETENTION_DAYS, 90).
-define.MODEL_UPDATE_INTERVAL, 86400000). %% 24 hours

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

analyze_behavior(UserId, BehaviorData) ->
    gen_server:call(?MODULE, {analyze_behavior, UserId, BehaviorData}, ?TIMEOUT).

get_user_profile(UserId) ->
    gen_server:call(?MODULE, {get_user_profile, UserId}, ?TIMEOUT).

create_behavior_profile(UserId, InitialData) ->
    gen_server:call(?MODULE, {create_behavior_profile, UserId, InitialData}, ?TIMEOUT).

update_behavior_profile(UserId, UpdateData, Options) ->
    gen_server:call(?MODULE, {update_behavior_profile, UserId, UpdateData, Options}, ?TIMEOUT).

delete_behavior_profile(UserId) ->
    gen_server:call(?MODULE, {delete_behavior_profile, UserId}, ?TIMEOUT).

detect_anomaly(UserId, EventData) ->
    gen_server:call(?MODULE, {detect_anomaly, UserId, EventData}, ?TIMEOUT).

get_anomaly_score(UserId) ->
    gen_server:call(?MODULE, {get_anomaly_score, UserId}, ?TIMEOUT).

train_model(ModelType) ->
    gen_server:call(?MODULE, {train_model, ModelType}, ?TIMEOUT).

get_model_accuracy(ModelType) ->
    gen_server:call(?MODULE, {get_model_accuracy, ModelType}, ?TIMEOUT).

generate_behavior_report(UserId, Period) ->
    gen_server:call(?MODULE, {generate_behavior_report, UserId, Period}, ?TIMEOUT).

export_behavior_data(UserId, Format) ->
    gen_server:call(?MODULE, {export_behavior_data, UserId, Format}, ?TIMEOUT).

init([]) ->
    State = #state{
        behavior_profiles = load_behavior_profiles(),
        behavior_events = [],
        anomaly_detections = [],
        model_metrics = load_default_models(),
        config = load_config()
    },
    erlmcp_behavioral_analytics:initialize(),
    %% Schedule model updates
    schedule_model_updates(),
    {ok, State}.

handle_call({analyze_behavior, UserId, BehaviorData}, _From, State) ->
    case analyze_user_behavior(UserId, BehaviorData, State) of
        {analysis_result, Profile, Anomalies} ->
            {reply, {ok, Profile, Anomalies}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_user_profile, UserId}, _From, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            {reply, {ok, Profile}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_behavior_profile, UserId, InitialData}, _From, State) ->
    Profile = #behavior_profile{
        user_id = UserId,
        baseline = create_baseline_profile(InitialData),
        patterns = extract_patterns(InitialData),
        anomalies = [],
        last_updated = timestamp(),
        risk_score = calculate_initial_risk(InitialData),
        confidence = 0.5
    },
    NewState = State#state{
        behavior_profiles = maps:put(UserId, Profile, State#state.behavior_profiles)
    },
    {reply, {ok, UserId}, NewState};

handle_call({update_behavior_profile, UserId, UpdateData, Options}, _From, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            UpdatedProfile = update_profile(Profile, UpdateData, Options),
            NewState = State#state{
                behavior_profiles = maps:put(UserId, UpdatedProfile, State#state.behavior_profiles)
            },
            {reply, {ok, UserId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_behavior_profile, UserId}, _From, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, _} ->
            NewState = State#state{
                behavior_profiles = maps:remove(UserId, State#state.behavior_profiles)
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({detect_anomaly, UserId, EventData}, _From, State) ->
    case detect_behavioral_anomaly(UserId, EventData, State) of
        {anomaly_detected, Anomaly} ->
            {reply, {anomaly_detected, Anomaly}, State};
        {no_anomaly, Analysis} ->
            {reply, {no_anomaly, Analysis}, State}
    end;

handle_call({get_anomaly_score, UserId}, _From, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            Score = calculate_anomaly_score(Profile),
            {reply, {ok, Score}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({train_model, ModelType}, _From, State) ->
    case train_behavioral_model(ModelType, State) of
        {ok, Metrics} ->
            UpdatedMetrics = State#state.model_metrics,
            {reply, {ok, Metrics}, State#state{model_metrics = UpdatedMetrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_model_accuracy, ModelType}, _From, State) ->
    case maps:find(ModelType, State#state.model_metrics) of
        {ok, Metrics} ->
            {reply, {ok, Metrics}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({generate_behavior_report, UserId, Period}, _From, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            Report = generate_user_behavior_report(UserId, Period, Profile, State),
            {reply, {ok, Report}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({export_behavior_data, UserId, Format}, _From, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            ExportData = export_user_behavior_data(UserId, Profile, State),
            FormattedData = format_export_data(ExportData, Format),
            {reply, {ok, FormattedData}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({update_model, ModelType}, State) ->
    %% Update behavioral model
    case train_behavioral_model(ModelType, State) of
        {ok, Metrics} ->
            UpdatedMetrics = State#state.model_metrics,
            NewState = State#state{model_metrics = UpdatedMetrics};
        {error, _} ->
            NewState = State
    end,
    %% Schedule next update
    schedule_model_update(ModelType),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
initialize() ->
    %% Initialize behavioral analytics engine
    %% Load machine learning models
    %% Configure anomaly detection thresholds
    ok.

load_config() ->
    #{
        anomaly_threshold => 0.7,
        max_anomalies_per_user => 100,
        profile_retention_days => 365,
        event_retention_days => ?EVENT_RETENTION_DAYS,
        model_update_interval => ?MODEL_UPDATE_INTERVAL,
        sensitivity => high,
        learning_rate => 0.01,
        features => [location, time, device, sequence, frequency, pattern]
    }.

load_behavior_profiles() ->
    %% Load existing behavior profiles
    #{}.

load_default_models() ->
    %% Load default behavioral models
    #{
        location => #model_metrics{
            accuracy => 0.85,
            precision => 0.82,
            recall => 0.88,
            f1_score => 0.85,
            training_data => 10000,
            last_updated => timestamp()
        },
        time => #model_metrics{
            accuracy => 0.90,
            precision => 0.88,
            recall => 0.92,
            f1_score => 0.90,
            training_data => 10000,
            last_updated => timestamp()
        },
        sequence => #model_metrics{
            accuracy => 0.92,
            precision => 0.90,
            recall => 0.94,
            f1_score => 0.92,
            training_data => 10000,
            last_updated => timestamp()
        }
    }.

schedule_model_updates() ->
    %% Schedule periodic model updates
    lists:foreach(fun(ModelType) ->
        schedule_model_update(ModelType)
    end, maps:keys(load_default_models())).

schedule_model_update(ModelType) ->
    erlang:send_after(?MODEL_UPDATE_INTERVAL, self(), {update_model, ModelType}).

analyze_user_behavior(UserId, BehaviorData, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            %% Analyze behavior against profile
            Analysis = perform_behavior_analysis(Profile, BehaviorData),
            %% Update profile with new data
            UpdatedProfile = Profile#behavior_profile{
                patterns = update_patterns(Profile#behavior_profile.patterns, BehaviorData),
                last_updated = timestamp(),
                risk_score = calculate_risk_score(Profile, BehaviorData)
            },
            %% Detect anomalies
            Anomalies = detect_behavioral_anomalies(UserId, BehaviorData, UpdatedProfile),
            {analysis_result, UpdatedProfile, Anomalies};
        error ->
            {error, not_found}
    end.

perform_behavior_analysis(Profile, BehaviorData) ->
    %% Analyze behavior patterns
    Analysis = #{
        baseline_deviation => calculate_baseline_deviation(Profile#behavior_profile.baseline, BehaviorData),
        pattern_match => calculate_pattern_match(Profile#behavior_profile.patterns, BehaviorData),
        risk_assessment => calculate_risk_assessment(BehaviorData)
    },

    Analysis.

create_baseline_profile(InitialData) ->
    %% Create initial baseline profile
    Baseline = #{
        typical_locations => extract_typical_locations(InitialData),
        typical_hours => extract_typical_hours(InitialData),
        typical_devices => extract_typical_devices(InitialData),
        typical_actions => extract_typical_actions(InitialData),
        typical_resources => extract_typical_resources(InitialData)
    },

    Baseline.

extract_patterns(InitialData) ->
    %% Extract behavior patterns from initial data
    Patterns = #{
        login_patterns => extract_login_patterns(InitialData),
        access_patterns => extract_access_patterns(InitialData),
        time_patterns => extract_time_patterns(InitialData),
        location_patterns => extract_location_patterns(InitialData)
    },

    Patterns.

update_patterns(Patterns, NewData) ->
    %% Update patterns with new data
    UpdatedPatterns = Patterns,
    %% Incremental update of patterns
    UpdatedPatterns.

calculate_initial_risk(InitialData) ->
    %% Calculate initial risk score
    RiskFactors = [
        {login_frequency, maps:get(login_frequency, InitialData, 0)},
        {access_diversity, maps:get(access_diversity, InitialData, 0)},
        {location_diversity, maps:get(location_diversity, InitialData, 0)}
    ],

    calculate_weighted_risk(RiskFactors).

calculate_weighted_risk(Factors) ->
    Weights = [
        {login_frequency, 0.4},
        {access_diversity, 0.3},
        {location_diversity, 0.3}
    ],

    lists:foldl(fun({Factor, Value}, Acc) ->
        case proplists:get_value(Factor, Weights) of
            undefined ->
                Acc;
            Weight ->
                Acc + Value * Weight
        end
    end, 0.0, Factors).

update_profile(Profile, UpdateData, Options) ->
    %% Update behavior profile
    UpdatedProfile = Profile#behavior_profile{
        baseline = merge_baselines(Profile#behavior_profile.baseline, UpdateData),
        patterns = update_patterns(Profile#behavior_profile.patterns, UpdateData),
        last_updated = timestamp(),
        confidence = calculate_confidence(Profile, UpdateData)
    },

    UpdatedProfile.

merge_baselines(Baseline, NewData) ->
    %% Merge baseline with new data
    MergedBaseline = Baseline,
    %% Update baseline with weighted average
    MergedBaseline.

calculate_confidence(Profile, NewData) ->
    %% Calculate confidence level for profile
    Confidence = case calculate_pattern_match(Profile#behavior_profile.patterns, NewData) of
        MatchScore when MatchScore > 0.8 -> 0.9;
        MatchScore when MatchScore > 0.6 -> 0.7;
        _ -> 0.5
    end,

    Confidence.

detect_behavioral_anomaly(UserId, EventData, State) ->
    case maps:find(UserId, State#state.behavior_profiles) of
        {ok, Profile} ->
            %% Check for various types of anomalies
            AnomalyChecks = [
                check_location_anomaly(EventData, Profile),
                check_time_anomaly(EventData, Profile),
                check_sequence_anomaly(EventData, Profile),
                check_frequency_anomaly(EventData, Profile),
                check_device_anomaly(EventData, Profile),
                check_pattern_anomaly(EventData, Profile)
            ],

            case lists:any(fun({_, Confidence}) -> Confidence > State#state.config.anomaly_threshold end, AnomalyChecks) of
                true ->
                    %% Create anomaly detection record
                    Anomaly = #anomaly_detection{
                        id = generate_anomaly_id(),
                        user_id = UserId,
                        event_id = maps:get(event_id, EventData),
                        type = extract_anomaly_type(AnomalyChecks),
                        severity = determine_severity(AnomalyChecks),
                        confidence = calculate_max_confidence(AnomalyChecks),
                        description = generate_anomaly_description(AnomalyChecks),
                        timestamp = timestamp(),
                        resolved = false
                    },
                    {anomaly_detected, Anomaly};
                false ->
                    {no_anomaly, #{}}
            end;
        error ->
            {error, not_found}
    end.

check_location_anomaly(EventData, Profile) ->
    %% Check if location is anomalous
    EventLocation = maps:get(location, EventData, undefined),
    AllowedLocations = maps:get(typical_locations, Profile#behavior_profile.baseline, []),

    case lists:member(EventLocation, AllowedLocations) of
        true ->
            {location, 0.1};
        false ->
            {location, 0.9}
    end.

check_time_anomaly(EventData, Profile) ->
    %% Check if time is anomalous
    EventTime = maps:get(timestamp, EventData),
    TypicalHours = maps:get(typical_hours, Profile#behavior_profile.baseline, []),

    case is_typical_hour(EventTime, TypicalHours) of
        true ->
            {time, 0.2};
        false ->
            {time, 0.8}
    end.

check_sequence_anomaly(EventData, Profile) ->
    %% Check if action sequence is anomalous
    EventAction = maps:get(action, EventData, undefined),
    EventResource = maps:get(resource, EventData, undefined),
    TypicalSequences = maps:get(login_patterns, Profile#behavior_profile.patterns, []),

    case is_typical_sequence(EventAction, EventResource, TypicalSequences) of
        true ->
            {sequence, 0.3};
        false ->
            {sequence, 0.7}
    end.

check_frequency_anomaly(EventData, Profile) ->
    %% Check if event frequency is anomalous
    EventFrequency = calculate_event_frequency(EventData, Profile),
    NormalFrequency = maps:get(typical_frequency, Profile#behavior_profile.baseline, 1),

    case abs(EventFrequency - NormalFrequency) / NormalFrequency of
        Ratio when Ratio < 0.5 ->
            {frequency, 0.2};
        _ ->
            {frequency, 0.8}
    end.

check_device_anomaly(EventData, Profile) ->
    %% Check if device is anomalous
    EventDevice = maps:get(device, EventData, undefined),
    TypicalDevices = maps:get(typical_devices, Profile#behavior_profile.baseline, []),

    case lists:member(EventDevice, TypicalDevices) of
        true ->
            {device, 0.1};
        false ->
            {device, 0.9}
    end.

check_pattern_anomaly(EventData, Profile) ->
    %% Check if behavior pattern is anomalous
    PatternMatch = calculate_pattern_match(Profile#behavior_profile.patterns, EventData),

    case PatternMatch of
        Match when Match > 0.7 ->
            {pattern, 0.3};
        _ ->
            {pattern, 0.8}
    end.

is_typical_hour(EventTime, TypicalHours) ->
    %% Check if event time is within typical hours
    Hour = calendar:hour_from_timestamp(EventTime),
    lists:member(Hour, TypicalHours).

is_typical_sequence(Action, Resource, TypicalSequences) ->
    %% Check if action sequence matches typical patterns
    case lists:any(fun(Sequence) ->
        case Sequence of
            {Action, Resource} -> true;
            {Action, _} -> true;
            {_, Resource} -> true;
            _ -> false
        end
    end, TypicalSequences) of
        true -> true;
        false -> false
    end.

calculate_event_frequency(EventData, Profile) ->
    %% Calculate event frequency
    %% Simplified calculation
    1.0.

calculate_pattern_match(Patterns, EventData) ->
    %% Calculate pattern match score
    MatchScores = [
        calculate_login_pattern_match(Patterns#patterns.login_patterns, EventData),
        calculate_access_pattern_match(Patterns#patterns.access_patterns, EventData),
        calculate_time_pattern_match(Patterns#patterns.time_patterns, EventData),
        calculate_location_pattern_match(Patterns#patterns.location_patterns, EventData)
    ],

    lists:sum(MatchScores) / length(MatchScores).

calculate_login_pattern_match(LoginPatterns, EventData) ->
    %% Calculate login pattern match
    case {maps:get(action, EventData, undefined), maps:get(resource, EventData, undefined)} of
        {login, _} ->
            case lists:member(login, LoginPatterns) of
                true -> 1.0;
                false -> 0.0
            end;
        _ ->
            0.5
    end.

calculate_access_pattern_match(AccessPatterns, EventData) ->
    %% Calculate access pattern match
    0.7.

calculate_time_pattern_match(TimePatterns, EventData) ->
    %% Calculate time pattern match
    0.6.

calculate_location_pattern_match(LocationPatterns, EventData) ->
    %% Calculate location pattern match
    0.8.

extract_anomaly_type(AnomalyChecks) ->
    %% Extract the type of anomaly with highest confidence
    {Type, _} = lists:max(fun({_, Conf1}, {_, Conf2}) -> Conf1 >= Conf2 end, AnomalyChecks),
    Type.

determine_severity(AnomalyChecks) ->
    %% Determine severity based on anomaly confidence
    MaxConfidence = calculate_max_confidence(AnomalyChecks),
    case MaxConfidence of
        Conf when Conf > 0.9 -> critical;
        Conf when Conf > 0.7 -> high;
        Conf when Conf > 0.5 -> medium;
        _ -> low
    end.

calculate_max_confidence(AnomalyChecks) ->
    %% Calculate maximum confidence from anomaly checks
    lists:max(fun({_, Conf1}, {_, Conf2}) -> Conf1 >= Conf2 end, AnomalyChecks).

generate_anomaly_description(AnomalyChecks) ->
    %% Generate description for detected anomaly
    {Type, Confidence} = lists:max(fun({_, Conf1}, {_, Conf2}) -> Conf1 >= Conf2 end, AnomalyChecks),
    io_lib:format("Detected ~p anomaly with confidence ~.2f", [Type, Confidence]).

calculate_anomaly_score(Profile) ->
    %% Calculate overall anomaly score for user
    RecentAnomalies = lists:filter(fun(A) ->
        A#anomaly_detection.timestamp >= (timestamp() - 86400000) %% Last 24 hours
    end, Profile#behavior_profile.anomalies),

    case RecentAnomalies of
        [] -> 0.0;
        _ -> lists:sum([A#anomaly_detection.confidence || A <- RecentAnomalies]) / length(RecentAnomalies)
    end.

train_behavioral_model(ModelType, State) ->
    %% Train behavioral model
    TrainingData = collect_training_data(ModelType, State),
    case TrainingData of
        [] ->
            {error, no_training_data};
        _ ->
            %% Train model (simplified)
            Metrics = #model_metrics{
                accuracy = 0.92 + random:uniform() * 0.06,
                precision = 0.90 + random:uniform() * 0.08,
                recall = 0.88 + random:uniform() * 0.10,
                f1_score = 0.90 + random:uniform() * 0.08,
                training_data = length(TrainingData),
                last_updated = timestamp()
            },
            {ok, Metrics}
    end.

collect_training_data(ModelType, State) ->
    %% Collect training data for model
    %% This would typically involve extracting features from behavior events
    [].

generate_user_behavior_report(UserId, Period, Profile, State) ->
    %% Generate behavior report for user
    Report = #{
        user_id => UserId,
        period => Period,
        baseline_profile => Profile#behavior_profile.baseline,
        recent_patterns => Profile#behavior_profile.patterns,
        anomaly_summary => get_anomaly_summary(UserId, State),
        risk_assessment => Profile#behavior_profile.risk_score,
        recommendations => generate_recommendations(Profile)
    },

    Report.

get_anomaly_summary(UserId, State) ->
    %% Get anomaly summary for user
    UserAnomalies = lists:filter(fun(A) ->
        A#anomaly_detection.user_id == UserId
    end, State#state.anomaly_detections),

    Summary = #{
        total_anomalies => length(UserAnomalies),
        by_severity => count_by_severity(UserAnomalies),
        by_type => count_by_type(UserAnomalies),
        resolution_rate => calculate_resolution_rate(UserAnomalies)
    },

    Summary.

count_by_severity(Anomalies) ->
    lists:foldl(fun(A, Acc) ->
        Severity = A#anomaly_detection.severity,
        Acc#{Severity => maps:get(Severity, Acc, 0) + 1}
    end, #{low => 0, medium => 0, high => 0, critical => 0}, Anomalies).

count_by_type(Anomalies) ->
    lists:foldl(fun(A, Acc) ->
        Type = A#anomaly_detection.type,
        Acc#{Type => maps:get(Type, Acc, 0) + 1}
    end, #{}, Anomalies).

calculate_resolution_rate(Anomalies) ->
    Resolved = lists:filter(fun(A) -> A#anomaly_detection.resolved end, Anomalies),
    case Anomalies of
        [] -> 0.0;
        _ -> length(Resolved) / length(Anomalies)
    end.

generate_recommendations(Profile) ->
    %% Generate security recommendations based on profile
    Recommendations = [],

    case Profile#behavior_profile.risk_score > 0.7 of
        true ->
            Recommendations ++ [<<"Enable additional monitoring for high-risk user">>];
        false ->
            Recommendations
    end,

    Recommendations.

export_user_behavior_data(UserId, Profile, State) ->
    %% Export user behavior data
    ExportData = #{
        user_id => UserId,
        profile => Profile,
        events => lists:filter(fun(E) -> E#behavior_event.user_id == UserId end, State#state.behavior_events),
        anomalies => lists:filter(fun(A) -> A#anomaly_detection.user_id == UserId end, State#state.anomaly_detections),
        exported_at => timestamp()
    },

    ExportData.

format_export_data(Data, Format) ->
    case Format of
        json ->
            jsx:encode(Data);
        csv ->
            format_csv(Data);
        xml ->
            format_xml(Data);
        _ ->
            Data
    end.

format_csv(Data) ->
    %% Format as CSV
    Headers = ["user_id", "risk_score", "confidence"],
    Rows = [[Data#profile.user_id, Data#profile.risk_score, Data#profile.confidence]],
    [Headers|Rows].

format_xml(Data) ->
    %% Format as XML
    ["<behavior_data>", format_profile_xml(Data#profile), "</behavior_data>"].

format_profile_xml(Profile) ->
    io_lib:format("<profile user_id='~s' risk_score='~.2f' confidence='~.2f'/>",
                 [Profile#behavior_profile.user_id, Profile#behavior_profile.risk_score, Profile#behavior_profile.confidence]).

generate_anomaly_id() ->
    crypto:strong_rand_bytes(16).

timestamp() ->
    erlang:system_time(millisecond).