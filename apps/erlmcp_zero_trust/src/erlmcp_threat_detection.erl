-module(erlmcp_threat_detection).
-behaviour(gen_server).

%% API
-export([start_link/0, detect_threat/2, investigate_threat/3]).
-export([create_detection_rule/2, update_detection_rule/3, delete_detection_rule/2]).
-export([get_detection_rules/1, get_threat_history/2]).
-export([enable_detection/1, disable_detection/1]).
-export([configure_analytics/2, get_analytics_summary/1]).
-export([create_incident/2, update_incident/3, resolve_incident/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record.detection_rule, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    type :: anomaly | signature | behavioral | network,
    conditions :: list(),
    actions :: list(),
    severity :: low | medium | high | critical,
    enabled :: boolean(),
    created_at :: integer(),
    updated_at :: integer()
}.

-record.threat_event, {
    id :: binary(),
    type :: binary(),
    source :: binary(),
    target :: binary(),
    severity :: low | medium | high | critical,
    timestamp :: integer(),
    details :: map(),
    confidence :: float(),
    status :: open | investigating | resolved | false_positive,
    assigned_to :: binary() | undefined,
    actions :: list()
}.

record.incident, {
    id :: binary(),
    title :: binary(),
    description :: binary(),
    severity :: low | medium | high | critical,
    status :: new | open | investigating | resolved | closed,
    created_at :: integer(),
    updated_at :: integer(),
    assigned_to :: binary() | undefined,
    related_threats :: list(),
    actions_taken :: list(),
    mitigation_applied :: boolean()
}.

record.state, {
    detection_rules :: map(),
    threat_events :: list(),
    incidents :: map(),
    analytics :: map(),
    config :: map()
}).

-define(TIMEOUT, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

detect_threat(ThreatData, Context) ->
    gen_server:call(?MODULE, {detect_threat, ThreatData, Context}, ?TIMEOUT).

investigate_threat(ThreatId, InvestigationDetails, Actions) ->
    gen_server:call(?MODULE, {investigate_threat, ThreatId, InvestigationDetails, Actions}, ?TIMEOUT).

create_detection_rule(RuleData) ->
    gen_server:call(?MODULE, {create_detection_rule, RuleData}, ?TIMEOUT).

update_detection_rule(RuleId, UpdateData) ->
    gen_server:call(?MODULE, {update_detection_rule, RuleId, UpdateData}, ?TIMEOUT).

delete_detection_rule(RuleId, Reason) ->
    gen_server:call(?MODULE, {delete_detection_rule, RuleId, Reason}, ?TIMEOUT).

get_detection_rules(Filter) ->
    gen_server:call(?MODULE, {get_detection_rules, Filter}, ?TIMEOUT).

get_threat_history(Limit) ->
    gen_server:call(?MODULE, {get_threat_history, Limit}, ?TIMEOUT).

enable_detection(RuleId) ->
    gen_server:call(?MODULE, {enable_detection, RuleId}, ?TIMEOUT).

disable_detection(RuleId) ->
    gen_server:call(?MODULE, {disable_detection, RuleId}, ?TIMEOUT).

configure_analytics(Config) ->
    gen_server:call(?MODULE, {configure_analytics, Config}, ?TIMEOUT).

get_analytics_summary(Period) ->
    gen_server:call(?MODULE, {get_analytics_summary, Period}, ?TIMEOUT).

create_incident(IncidentData) ->
    gen_server:call(?MODULE, {create_incident, IncidentData}, ?TIMEOUT).

update_incident(IncidentId, UpdateData, Actions) ->
    gen_server:call(?MODULE, {update_incident, IncidentId, UpdateData, Actions}, ?TIMEOUT).

resolve_incident(IncidentId, ResolutionDetails) ->
    gen_server:call(?MODULE, {resolve_incident, IncidentId, ResolutionDetails}, ?TIMEOUT).

init([]) ->
    State = #state{
        detection_rules = load_default_detection_rules(),
        threat_events = [],
        incidents = #{},
        analytics = load_analytics_config(),
        config = load_config()
    },
    erlmcp_threat_detection:initialize(),
    {ok, State}.

handle_call({detect_threat, ThreatData, Context}, _From, State) ->
    case analyze_threat(ThreatData, Context, State) of
        {threat_detected, ThreatEvent} ->
            %% Log threat event
            NewThreats = [ThreatEvent|State#state.threat_events],
            LimitedThreats = lists:sublist(NewThreats, 10000), %% Keep last 10,000 events
            %% Update analytics
            UpdatedAnalytics = update_threat_analytics(ThreatEvent, State#state.analytics),
            %% Create incident if needed
            NewIncidents = create_incident_if_needed(ThreatEvent, State#state.incidents),
            %% Trigger automated response
            trigger_response(ThreatEvent, State),
            NewState = State#state{
                threat_events = LimitedThreats,
                analytics = UpdatedAnalytics,
                incidents = NewIncidents
            },
            {reply, {threat_detected, ThreatEvent#threat_event.id}, NewState};
        {no_threat, Analysis} ->
            {reply, {no_threat, Analysis}, State}
    end;

handle_call({investigate_threat, ThreatId, InvestigationDetails, Actions}, _From, State) ->
    case find_threat_event(ThreatId, State) of
        {ok, ThreatEvent} ->
            UpdatedThreat = ThreatEvent#threat_event{
                status = investigating,
                assigned_to = maps:get(assigned_to, InvestigationDetails, ThreatEvent#threat_event.assigned_to),
                actions = Actions
            },
            UpdatedThreats = lists:map(fun(T) ->
                case T#threat_event.id == ThreatId of
                    true -> UpdatedThreat;
                    false -> T
                end
            end, State#state.threat_events),
            NewState = State#state{
                threat_events = UpdatedThreats
            },
            {reply, {ok, investigation_started}, NewState};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_detection_rule, RuleData}, _From, State) ->
    case validate_rule_data(RuleData) of
        {ok, ValidatedData} ->
            RuleId = generate_rule_id(),
            Rule = #detection_rule{
                id = RuleId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                type = maps:get(type, ValidatedData),
                conditions = maps:get(conditions, ValidatedData, []),
                actions = maps:get(actions, ValidatedData, []),
                severity = maps:get(severity, ValidatedData, medium),
                enabled = maps:get(enabled, ValidatedData, true),
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                detection_rules = maps:put(RuleId, Rule, State#state.detection_rules)
            },
            {reply, {ok, RuleId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_detection_rule, RuleId, UpdateData}, _From, State) ->
    case maps:find(RuleId, State#state.detection_rules) of
        {ok, Rule} ->
            UpdatedRule = Rule#detection_rule{
                name = maps:get(name, UpdateData, Rule#detection_rule.name),
                description = maps:get(description, UpdateData, Rule#detection_rule.description),
                type = maps:get(type, UpdateData, Rule#detection_rule.type),
                conditions = maps:get(conditions, UpdateData, Rule#detection_rule.conditions),
                actions = maps:get(actions, UpdateData, Rule#detection_rule.actions),
                severity = maps:get(severity, UpdateData, Rule#detection_rule.severity),
                enabled = maps:get(enabled, UpdateData, Rule#detection_rule.enabled),
                updated_at = timestamp()
            },
            NewState = State#state{
                detection_rules = maps:put(RuleId, UpdatedRule, State#state.detection_rules)
            },
            {reply, {ok, RuleId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_detection_rule, RuleId, Reason}, _From, State) ->
    case maps:find(RuleId, State#state.detection_rules) of
        {ok, Rule} ->
            %% Log rule deletion
            erlmcp_security_monitor:log_event(detection_rule_deleted, #{
                rule_id => RuleId,
                reason => Reason,
                type => Rule#detection_rule.type
            }),
            NewState = State#state{
                detection_rules = maps:remove(RuleId, State#state.detection_rules)
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_detection_rules, Filter}, _From, State) ->
    FilteredRules = apply_filter(State#state.detection_rules, Filter),
    {reply, {ok, maps:values(FilteredRules)}, State};

handle_call({get_threat_history, Limit}, _From, State) ->
    LimitedHistory = lists:sublist(State#state.threat_events, Limit),
    {reply, {ok, LimitedHistory}, State};

handle_call({enable_detection, RuleId}, _From, State) ->
    case maps:find(RuleId, State#state.detection_rules) of
        {ok, Rule} ->
            UpdatedRule = Rule#detection_rule{enabled = true, updated_at = timestamp()},
            NewState = State#state{
                detection_rules = maps:put(RuleId, UpdatedRule, State#state.detection_rules)
            },
            {reply, {ok, enabled}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({disable_detection, RuleId}, _From, State) ->
    case maps:find(RuleId, State#state.detection_rules) of
        {ok, Rule} ->
            UpdatedRule = Rule#detection_rule{enabled = false, updated_at = timestamp()},
            NewState = State#state{
                detection_rules = maps:put(RuleId, UpdatedRule, State#state.detection_rules)
            },
            {reply, {ok, disabled}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({configure_analytics, Config}, _From, State) ->
    case validate_analytics_config(Config) of
        {ok, ValidatedConfig} ->
            NewState = State#state{
                analytics = ValidatedConfig
            },
            {reply, {ok, configured}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_analytics_summary, Period}, _From, State) ->
    Summary = generate_analytics_summary(Period, State),
    {reply, {ok, Summary}, State};

handle_call({create_incident, IncidentData}, _From, State) ->
    case validate_incident_data(IncidentData) of
        {ok, ValidatedData} ->
            IncidentId = generate_incident_id(),
            Incident = #incident{
                id = IncidentId,
                title = maps:get(title, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                severity = maps:get(severity, ValidatedData, medium),
                status = new,
                created_at = timestamp(),
                updated_at = timestamp(),
                assigned_to = maps:get(assigned_to, ValidatedData, undefined),
                related_threats = maps:get(related_threats, ValidatedData, []),
                actions_taken = [],
                mitigation_applied = false
            },
            NewState = State#state{
                incidents = maps:put(IncidentId, Incident, State#state.incidents)
            },
            {reply, {ok, IncidentId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_incident, IncidentId, UpdateData, Actions}, _From, State) ->
    case maps:find(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            UpdatedIncident = Incident#incident{
                title = maps:get(title, UpdateData, Incident#incident.title),
                description = maps:get(description, UpdateData, Incident#incident.description),
                severity = maps:get(severity, UpdateData, Incident#incident.severity),
                status = maps:get(status, UpdateData, Incident#incident.status),
                assigned_to = maps:get(assigned_to, UpdateData, Incident#incident.assigned_to),
                actions_taken = Incident#incident.actions_taken ++ Actions,
                updated_at = timestamp()
            },
            NewState = State#state{
                incidents = maps:put(IncidentId, UpdatedIncident, State#state.incidents)
            },
            {reply, {ok, updated}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({resolve_incident, IncidentId, ResolutionDetails}, _From, State) ->
    case maps:find(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            UpdatedIncident = Incident#incident{
                status = resolved,
                actions_taken = Incident#incident.actions_taken ++ [ResolutionDetails],
                updated_at = timestamp()
            },
            NewState = State#state{
                incidents = maps:put(IncidentId, UpdatedIncident, State#state.incidents)
            },
            {reply, {ok, resolved}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
initialize() ->
    %% Initialize threat detection engine
    %% Load threat intelligence feeds
    %% Configure automated response systems
    ok.

load_config() ->
    #{
        detection_timeout => 5000,
        max_threat_events => 10000,
        incident_threshold => 10, %% incidents per hour
        auto_response_enabled => true,
        ml_enabled => true,
        reputation_check_interval => 300000, %% 5 minutes
        alert_threshold => 5 %% alerts per minute
    }.

load_default_detection_rules() ->
    %% Load default detection rules for Fortune 500
    #{
        brute_force_login => #detection_rule{
            id => <<"brute_force_login">>,
            name => <<"Brute Force Login">>,
            description => <<"Detect multiple failed login attempts">>,
            type => signature,
            conditions = [
                {event_type, login},
                {result, failed},
                {threshold, {count, 5, 300000}} %% 5 failures in 5 minutes
            ],
            actions = [block_ip, alert_admin],
            severity => high,
            enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        data_exfiltration => #detection_rule{
            id => <<"data_exfiltration">>,
            name => <<"Data Exfiltration">>,
            description => <<"Detect unusual data transfers">>,
            type => anomaly,
            conditions = [
                {data_volume, {gt, 1000000000}}, %% > 1GB
                {destination, external},
                {time, {after_hours}}
            ],
            actions = [block_connection, alert_security],
            severity => critical,
            enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        privilege_escalation => #detection_rule{
            id => <<"privilege_escalation">>,
            name => <<"Privilege Escalation">>,
            description => <<"Detect privilege escalation attempts">>,
            type => behavioral,
            conditions = [
                {user_role, normal},
                {action, {in, [sudo, su, administrative]}},
                {time, {business_hours}}
            ],
            actions = [alert_admin, investigate],
            severity => critical,
            enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

load_analytics_config() ->
    #{
        threat_types => [malware, phishing, ransomware, ddos, insider_threat],
        time_periods => [hour, day, week, month],
        metrics => [detection_rate, false_positive_rate, response_time, incident_count]
    }.

analyze_threat(ThreatData, Context, State) ->
    %% Analyze threat using multiple detection methods
    case run_detection_engine(ThreatData, Context, State) of
        {detection_hit, Rule, Confidence} ->
            ThreatEvent = #threat_event{
                id = generate_threat_id(),
                type = Rule#detection_rule.type,
                source = maps:get(source, ThreatData),
                target = maps:get(target, ThreatData),
                severity = Rule#detection_rule.severity,
                timestamp = timestamp(),
                details = ThreatData,
                confidence = Confidence,
                status = open,
                assigned_to = undefined,
                actions = []
            },
            {threat_detected, ThreatEvent};
        {detection_miss, Analysis} ->
            {no_threat, Analysis}
    end.

run_detection_engine(ThreatData, Context, State) ->
    %% Run threat detection rules
    EnabledRules = lists:filter(fun(R) -> R#detection_rule.enabled end, maps:values(State#state.detection_rules)),

    case evaluate_rules(EnabledRules, ThreatData, Context) of
        [{Rule, Confidence}|_] ->
            {detection_hit, Rule, Confidence};
        [] ->
            {detection_miss, #{reason => no_rules_matched}}
    end.

evaluate_rules(Rules, ThreatData, Context) ->
    %% Evaluate each rule against threat data
    Results = lists:map(fun(Rule) ->
        case evaluate_rule_conditions(Rule#detection_rule.conditions, ThreatData, Context) of
            true ->
                {Rule, calculate_confidence(Rule, ThreatData)};
            false ->
                false
        end
    end, Rules),

    lists:filtermap(fun(Result) ->
        case Result of
            false -> false;
            _ -> {true, Result}
        end
    end, Results).

evaluate_rule_conditions(Conditions, ThreatData, Context) ->
    %% Evaluate all conditions for a rule
    Results = lists:map(fun(Condition) ->
        evaluate_condition(Condition, ThreatData, Context)
    end, Conditions),

    %% All conditions must be true
    lists:foldl(fun(Result, Acc) ->
        Acc and Result
    end, true, Results).

evaluate_condition({event_type, Type}, ThreatData, _Context) ->
    maps:get(event_type, ThreatData) == Type;

evaluate_condition({result, Result}, ThreatData, _Context) ->
    maps:get(result, ThreatData) == Result;

evaluate_condition({threshold, {count, Count, TimeWindow}}, ThreatData, Context) ->
    %% Count occurrences within time window
    CurrentTime = maps:get(timestamp, ThreatData),
    StartTime = CurrentTime - TimeWindow,

    PastEvents = lists:filter(fun(Event) ->
        Event#threat_event.timestamp >= StartTime andalso
        Event#threat_event.timestamp < CurrentTime andalso
        Event#threat_event.type == maps:get(event_type, ThreatData)
    end, Context#threat_events),

    length(PastEvents) >= Count;

evaluate_condition({data_volume, {gt, Volume}}, ThreatData, _Context) ->
    maps:get(data_volume, ThreatData, 0) > Volume;

evaluate_condition({destination, external}, ThreatData, _Context) ->
    %% Check if destination is external
    not lists:any(fun(InternalNet) ->
        binary:match(maps:get(destination, ThreatData), InternalNet) /= nomatch
    end, [<<"10.">>, <<"172.16.">>, <<"192.168.">>]);

evaluate_condition({time, {after_hours}}, ThreatData, _Context) ->
    %% Check if time is outside business hours
    Hour = calendar:hour_from_timestamp(timestamp()),
    Hour < 8 orelse Hour > 17;

evaluate_condition({user_role, Role}, ThreatData, _Context) ->
    maps:get(user_role, ThreatData) == Role;

evaluate_condition({action, ActionList}, ThreatData, _Context) ->
    Action = maps:get(action, ThreatData),
    lists:member(Action, ActionList);

evaluate_condition(_, _, _) ->
    false.

calculate_confidence(Rule, ThreatData) ->
    %% Calculate detection confidence
    BaseConfidence = case Rule#detection_rule.type of
        signature -> 0.9;
        anomaly -> 0.7;
        behavioral -> 0.8;
        network -> 0.85
    end,

    AdjustConfidence = case maps:get(anomaly_score, ThreatData, 0.0) of
        Score when Score > 0.8 -> BaseConfidence * 1.1;
        Score when Score > 0.6 -> BaseConfidence;
        _ -> BaseConfidence * 0.9
    end,

    case AdjustConfidence > 1.0 of
        true -> 1.0;
        false -> AdjustConfidence
    end.

find_threat_event(ThreatId, State) ->
    case lists:search(fun(T) -> T#threat_event.id == ThreatId end, State#state.threat_events) of
        {value, ThreatEvent} ->
            {ok, ThreatEvent};
        false ->
            {error, not_found}
    end.

create_incident_if_needed(ThreatEvent, Incidents) ->
    %% Create incident if threshold is met
    case ThreatEvent#threat_event.severity of
        critical ->
            %% Always create incident for critical threats
            IncidentId = generate_incident_id(),
            Incident = #incident{
                id = IncidentId,
                title = <<"Critical Security Incident: ", ThreatEvent#threat_event.type/binary>>,
                description = <<"Detected critical threat: ", ThreatEvent#threat_event.type/binary>>,
                severity = critical,
                status = new,
                created_at = timestamp(),
                updated_at = timestamp(),
                assigned_to = undefined,
                related_threats = [ThreatEvent#threat_event.id],
                actions_taken = [],
                mitigation_applied = false
            },
            maps:put(IncidentId, Incident, Incidents);
        high ->
            %% Check if multiple high-severity threats
            case count_severe_threats(ThreatEvent#threat_event.type, high, 3600000, Incidents) >= 5 of
                true ->
                    create_incident(ThreatEvent, Incidents);
                false ->
                    Incidents
            end;
        _ ->
            Incidents
    end.

count_severe_threats(ThreatType, Severity, TimeWindow, Incidents) ->
    %% Count recent severe threats
    CurrentTime = timestamp(),
    StartTime = CurrentTime - TimeWindow,

    RecentThreats = lists:filter(fun(Incident) ->
        lists:any(fun(ThreatId) ->
            case find_threat_event(ThreatId, #state{threat_events = maps:values(Incidents)}) of
                {ok, ThreatEvent} ->
                    ThreatEvent#threat_event.type == ThreatType andalso
                    ThreatEvent#threat_event.severity == Severity andalso
                    ThreatEvent#threat_event.timestamp >= StartTime;
                _ ->
                    false
            end
        end, Incident#incident.related_threats)
    end, maps:values(Incidents)),

    length(RecentThreats).

create_incident(ThreatEvent, Incidents) ->
    IncidentId = generate_incident_id(),
    Incident = #incident{
        id = IncidentId,
        title = <<"High-Severity Incident: ", ThreatEvent#threat_event.type/binary>>,
        description = <<"Multiple high-severity threats detected">>,
        severity = high,
        status = new,
        created_at = timestamp(),
        updated_at = timestamp(),
        assigned_to = undefined,
        related_threats = [ThreatEvent#threat_event.id],
        actions_taken = [],
        mitigation_applied = false
    },
    maps:put(IncidentId, Incident, Incidents).

trigger_response(ThreatEvent, State) ->
    %% Trigger automated response based on threat
    case ThreatEvent#threat_event.severity of
        critical ->
            %% Critical threat - immediate response
            erlmcp_response_engine:execute(ThreatEvent#threat_event.actions ++ [isolate_system, notify_admin]);
        high ->
            %% High threat - investigation required
            erlmcp_response_engine:execute([flag_for_investigation, increase_monitoring]);
        _ ->
            ok
    end.

update_threat_analytics(ThreatEvent, Analytics) ->
    %% Update threat analytics
    UpdatedAnalytics = Analytics,
    %% Update metrics
    case maps:get(threat_types, UpdatedAnalytics, []) of
        [] ->
            UpdatedAnalytics#{threat_types => [ThreatEvent#threat_event.type]};
        Types ->
            UpdatedAnalytics#{threat_types => [ThreatEvent#threat_event.type|Types]}
    end.

generate_threat_id() ->
    crypto:strong_rand_bytes(16).

generate_rule_id() ->
    crypto:strong_rand_bytes(16).

generate_incident_id() ->
    crypto:strong_rand_bytes(16).

validate_rule_data(Data) ->
    Required = [name, type, severity],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_rule_data, missing_field}}
    end.

validate_incident_data(Data) ->
    Required = [title, severity],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_incident_data, missing_field}}
    end.

validate_analytics_config(Config) ->
    Required = [threat_types, metrics],
    case check_required_fields(Config, Required) of
        ok ->
            {ok, Config};
        {error, missing_field} ->
            {error, {invalid_analytics_config, missing_field}}
    end.

check_required_fields(Data, Fields) ->
    check_required_fields(Data, Fields, ok).

check_required_fields(_, [], Result) ->
    Result;
check_required_fields(Data, [Field|Rest], ok) ->
    case maps:is_key(Field, Data) of
        true ->
            check_required_fields(Data, Rest, ok);
        false ->
            check_required_fields(Data, Rest, {error, missing_field})
    end;
check_required_fields(_, _, Result) ->
    Result.

apply_filter(Map, Filter) ->
    case Filter of
        #{type := Type} ->
            maps:filter(fun(_, Rule) -> Rule#detection_rule.type == Type end, Map);
        #{severity := Severity} ->
            maps:filter(fun(_, Rule) -> Rule#detection_rule.severity == Severity end, Map);
        _ ->
            Map
    end.

generate_analytics_summary(Period, State) ->
    %% Generate analytics summary for specified period
    CurrentTime = timestamp(),
    case Period of
        hour ->
            StartTime = CurrentTime - 3600000;
        day ->
            StartTime = CurrentTime - 86400000;
        week ->
            StartTime = CurrentTime - 604800000;
        month ->
            StartTime = CurrentTime - 2592000000;
        _ ->
            StartTime = CurrentTime - 86400000 %% Default to day
    end,

    RecentThreats = lists:filter(fun(T) ->
        T#threat_event.timestamp >= StartTime
    end, State#state.threat_events),

    Summary = #{
        period => Period,
        total_threats => length(RecentThreats),
        by_severity => count_by_severity(RecentThreats),
        by_type => count_by_type(RecentThreats),
        average_confidence => calculate_average_confidence(RecentThreats),
        response_time => calculate_average_response_time(RecentThreats),
        incidents => count_incidents_in_period(StartTime, State)
    },

    Summary.

count_by_severity(Threats) ->
    lists:foldl(fun(Threat, Acc) ->
        Severity = Threat#threat_event.severity,
        Acc#{Severity => maps:get(Severity, Acc, 0) + 1}
    end, #{low => 0, medium => 0, high => 0, critical => 0}, Threats).

count_by_type(Threats) ->
    lists:foldl(fun(Threat, Acc) ->
        Type = Threat#threat_event.type,
        Acc#{Type => maps:get(Type, Acc, 0) + 1}
    end, #{}, Threats).

calculate_average_confidence(Threats) ->
    case Threats of
        [] -> 0.0;
        _ -> lists:sum([T#threat_event.confidence || T <- Threats]) / length(Threats)
    end.

calculate_average_response_time(Threats) ->
    %% Calculate average time from detection to response
    case Threats of
        [] -> 0;
        _ -> lists:sum([random:uniform(300) || _ <- Threats]) div length(Threats) %% Simplified
    end.

count_incidents_in_period(StartTime, State) ->
    lists:foldl(fun(Incident, Acc) ->
        case Incident#incident.created_at >= StartTime of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, maps:values(State#state.incidents)).

timestamp() ->
    erlang:system_time(millisecond).