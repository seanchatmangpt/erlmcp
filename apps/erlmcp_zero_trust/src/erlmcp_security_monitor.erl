-module(erlmcp_security_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0, log_event/2, log_event/3]).
-export([ delete_monitoring_policy/2]).
-export([get_monitoring_policies/1]).
-export([enable_monitoring/1, disable_monitoring/1]).
-export([set_alert_threshold/2, get_alert_summary/1]).
-export([create_dashboard/1, update_dashboard/2, get_dashboard/1]).
-export([export_logs/2, generate_report/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record.monitoring_policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    event_type :: binary(),
    conditions :: list(),
    actions :: list(),
    severity :: low | medium | high | critical,
    enabled :: boolean(),
    alert_threshold :: integer(),
    created_at :: integer(),
    updated_at :: integer()
}.

record.security_event, {
    id :: binary(),
    type :: binary(),
    category :: binary(),
    severity :: low | medium | high | critical,
    timestamp :: integer(),
    source :: binary(),
    target :: binary(),
    details :: map(),
    user_id :: binary(),
    session_id :: binary(),
    correlation_id :: binary(),
    processed :: boolean()
}.

record.alert, {
    id :: binary(),
    policy_id :: binary(),
    event_id :: binary(),
    severity :: low | medium | high | critical,
    timestamp :: integer(),
    message :: binary(),
    status :: new | acknowledged | resolved,
    assigned_to :: binary() | undefined,
    actions :: list(),
    escalation_count :: integer()
}.

record.dashboard, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    widgets :: list(),
    refresh_interval :: integer(),
    created_at :: integer(),
    updated_at :: integer()
}.

record.state, {
    monitoring_policies :: map(),
    security_events :: list(),
    alerts :: list(),
    dashboards :: map(),
    metrics :: map(),
    config :: map()
}).

-define(TIMEOUT, 30000).
-define(MAX_EVENTS, 100000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_event(EventType, Details) ->
    log_event(EventType, Details, #{}).

log_event(EventType, Details, Metadata) ->
    gen_server:cast(?MODULE, {log_event, EventType, Details, Metadata}).

create_monitoring_policy(PolicyData) ->
    gen_server:call(?MODULE, {create_monitoring_policy, PolicyData}, ?TIMEOUT).

update_monitoring_policy(PolicyId, UpdateData) ->
    gen_server:call(?MODULE, {update_monitoring_policy, PolicyId, UpdateData}, ?TIMEOUT).

delete_monitoring_policy(PolicyId, Reason) ->
    gen_server:call(?MODULE, {delete_monitoring_policy, PolicyId, Reason}, ?TIMEOUT).

get_monitoring_policies(Filter) ->
    gen_server:call(?MODULE, {get_monitoring_policies, Filter}, ?TIMEOUT).

get_event_history(Limit) ->
    gen_server:call(?MODULE, {get_event_history, Limit}, ?TIMEOUT).

enable_monitoring(PolicyId) ->
    gen_server:call(?MODULE, {enable_monitoring, PolicyId}, ?TIMEOUT).

disable_monitoring(PolicyId) ->
    gen_server:call(?MODULE, {disable_monitoring, PolicyId}, ?TIMEOUT).

set_alert_threshold(PolicyId, Threshold) ->
    gen_server:call(?MODULE, {set_alert_threshold, PolicyId, Threshold}, ?TIMEOUT).

get_alert_summary(Period) ->
    gen_server:call(?MODULE, {get_alert_summary, Period}, ?TIMEOUT).

create_dashboard(DashboardData) ->
    gen_server:call(?MODULE, {create_dashboard, DashboardData}, ?TIMEOUT).

update_dashboard(DashboardId, UpdateData) ->
    gen_server:call(?MODULE, {update_dashboard, DashboardId, UpdateData}, ?TIMEOUT).

get_dashboard(DashboardId) ->
    gen_server:call(?MODULE, {get_dashboard, DashboardId}, ?TIMEOUT).

export_logs(Format, Filter) ->
    gen_server:call(?MODULE, {export_logs, Format, Filter}, ?TIMEOUT).

generate_report(ReportType, Period) ->
    gen_server:call(?MODULE, {generate_report, ReportType, Period}, ?TIMEOUT).

init([]) ->
    State = #state{
        monitoring_policies = load_default_monitoring_policies(),
        security_events = [],
        alerts = [],
        dashboards = load_default_dashboards(),
        metrics = load_metrics(),
        config = load_config()
    },
    erlmcp_security_monitor:initialize(),
    {ok, State}.

handle_call({create_monitoring_policy, PolicyData}, _From, State) ->
    case validate_policy_data(PolicyData) of
        {ok, ValidatedData} ->
            PolicyId = generate_policy_id(),
            Policy = #monitoring_policy{
                id = PolicyId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                event_type = maps:get(event_type, ValidatedData),
                conditions = maps:get(conditions, ValidatedData, []),
                actions = maps:get(actions, ValidatedData, []),
                severity = maps:get(severity, ValidatedData, medium),
                enabled = maps:get(enabled, ValidatedData, true),
                alert_threshold = maps:get(alert_threshold, ValidatedData, 10),
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                monitoring_policies = maps:put(PolicyId, Policy, State#state.monitoring_policies)
            },
            {reply, {ok, PolicyId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_monitoring_policy, PolicyId, UpdateData}, _From, State) ->
    case maps:find(PolicyId, State#state.monitoring_policies) of
        {ok, Policy} ->
            UpdatedPolicy = Policy#monitoring_policy{
                name = maps:get(name, UpdateData, Policy#monitoring_policy.name),
                description = maps:get(description, UpdateData, Policy#monitoring_policy.description),
                event_type = maps:get(event_type, UpdateData, Policy#monitoring_policy.event_type),
                conditions = maps:get(conditions, UpdateData, Policy#monitoring_policy.conditions),
                actions = maps:get(actions, UpdateData, Policy#monitoring_policy.actions),
                severity = maps:get(severity, UpdateData, Policy#monitoring_policy.severity),
                enabled = maps:get(enabled, UpdateData, Policy#monitoring_policy.enabled),
                alert_threshold = maps:get(alert_threshold, UpdateData, Policy#monitoring_policy.alert_threshold),
                updated_at = timestamp()
            },
            NewState = State#state{
                monitoring_policies = maps:put(PolicyId, UpdatedPolicy, State#state.monitoring_policies)
            },
            {reply, {ok, PolicyId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_monitoring_policy, PolicyId, Reason}, _From, State) ->
    case maps:find(PolicyId, State#state.monitoring_policies) of
        {ok, Policy} ->
            %% Log policy deletion
            log_event(policy_deleted, #{
                policy_id => PolicyId,
                reason => Reason,
                type => Policy#monitoring_policy.event_type
            }),
            NewState = State#state{
                monitoring_policies = maps:remove(PolicyId, State#state.monitoring_policies)
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_monitoring_policies, Filter}, _From, State) ->
    FilteredPolicies = apply_filter(State#state.monitoring_policies, Filter),
    {reply, {ok, maps:values(FilteredPolicies)}, State};

handle_call({get_event_history, Limit}, _From, State) ->
    LimitedHistory = lists:sublist(State#state.security_events, Limit),
    {reply, {ok, LimitedHistory}, State};

handle_call({enable_monitoring, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.monitoring_policies) of
        {ok, Policy} ->
            UpdatedPolicy = Policy#monitoring_policy{enabled = true, updated_at = timestamp()},
            NewState = State#state{
                monitoring_policies = maps:put(PolicyId, UpdatedPolicy, State#state.monitoring_policies)
            },
            {reply, {ok, enabled}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({disable_monitoring, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.monitoring_policies) of
        {ok, Policy} ->
            UpdatedPolicy = Policy#monitoring_policy{enabled = false, updated_at = timestamp()},
            NewState = State#state{
                monitoring_policies = maps:put(PolicyId, UpdatedPolicy, State#state.monitoring_policies)
            },
            {reply, {ok, disabled}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_alert_threshold, PolicyId, Threshold}, _From, State) ->
    case maps:find(PolicyId, State#state.monitoring_policies) of
        {ok, Policy} ->
            UpdatedPolicy = Policy#monitoring_policy{alert_threshold = Threshold, updated_at = timestamp()},
            NewState = State#state{
                monitoring_policies = maps:put(PolicyId, UpdatedPolicy, State#state.monitoring_policies)
            },
            {reply, {ok, threshold_updated}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_alert_summary, Period}, _From, State) ->
    Summary = generate_alert_summary(Period, State),
    {reply, {ok, Summary}, State};

handle_call({create_dashboard, DashboardData}, _From, State) ->
    case validate_dashboard_data(DashboardData) of
        {ok, ValidatedData} ->
            DashboardId = generate_dashboard_id(),
            Dashboard = #dashboard{
                id = DashboardId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                widgets = maps:get(widgets, ValidatedData, []),
                refresh_interval = maps:get(refresh_interval, ValidatedData, 30000),
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                dashboards = maps:put(DashboardId, Dashboard, State#state.dashboards)
            },
            {reply, {ok, DashboardId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_dashboard, DashboardId, UpdateData}, _From, State) ->
    case maps:find(DashboardId, State#state.dashboards) of
        {ok, Dashboard} ->
            UpdatedDashboard = Dashboard#dashboard{
                name = maps:get(name, UpdateData, Dashboard#dashboard.name),
                description = maps:get(description, UpdateData, Dashboard#dashboard.description),
                widgets = maps:get(widgets, UpdateData, Dashboard#dashboard.widgets),
                refresh_interval = maps:get(refresh_interval, UpdateData, Dashboard#dashboard.refresh_interval),
                updated_at = timestamp()
            },
            NewState = State#state{
                dashboards = maps:put(DashboardId, UpdatedDashboard, State#state.dashboards)
            },
            {reply, {ok, DashboardId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_dashboard, DashboardId}, _From, State) ->
    case maps:find(DashboardId, State#state.dashboards) of
        {ok, Dashboard} ->
            {reply, {ok, Dashboard}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({export_logs, Format, Filter}, _From, State) ->
    ExportedData = export_logs_data(Format, Filter, State),
    {reply, {ok, ExportedData}, State};

handle_call({generate_report, ReportType, Period}, _From, State) ->
    Report = generate_security_report(ReportType, Period, State),
    {reply, {ok, Report}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({log_event, EventType, Details, Metadata}, State) ->
    %% Create security event
    EventId = generate_event_id(),
    Event = #security_event{
        id = EventId,
        type = EventType,
        category = maps:get(category, Metadata, <<"security">>),
        severity = maps:get(severity, Metadata, medium),
        timestamp = timestamp(),
        source = maps:get(source, Metadata, <<"unknown">>),
        target = maps:get(target, Metadata, <<"unknown">>),
        details = Details,
        user_id = maps:get(user_id, Metadata, undefined),
        session_id = maps:get(session_id, Metadata, undefined),
        correlation_id = maps:get(correlation_id, Metadata, generate_correlation_id()),
        processed = false
    },

    %% Add to events list
    NewEvents = [Event|State#state.security_events],
    LimitedEvents = lists:sublist(NewEvents, ?MAX_EVENTS),

    %% Check monitoring policies
    NewState = State#state{
        security_events = LimitedEvents
    },

    %% Process event through policies
    case process_event_through_policies(Event, NewState) of
        {alerts_generated, Alerts, UpdatedState} ->
            %% Handle generated alerts
            AlertedState = UpdatedState#state{
                alerts = Alerts ++ State#state.alerts
            },
            %% Update metrics
            UpdatedMetrics = update_metrics(Event, AlertedState#state.metrics),
            FinalState = AlertedState#state{metrics = UpdatedMetrics};
        {no_alerts, UpdatedState} ->
            %% No alerts generated
            UpdatedMetrics = update_metrics(Event, UpdatedState#state.metrics),
            FinalState = UpdatedState#state{metrics = UpdatedMetrics}
    end,

    %% Update event as processed
    FinalState2 = FinalState#state{
        security_events = lists:map(fun(E) ->
            case E#security_event.id == EventId of
                true -> E#security_event{processed = true};
                false -> E
            end
        end, FinalState#state.security_events)
    },

    %% Schedule cleanup if needed
    case length(FinalState2#state.security_events) > (?MAX_EVENTS * 0.9) of
        true ->
            erlang:send_after(10000, self(), {cleanup_events});
        false ->
            ok
    end,

    {noreply, FinalState2};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({cleanup_events}, State) ->
    %% Cleanup old events
    CurrentTime = timestamp(),
    CutoffTime = CurrentTime - 864000000, %% Keep 10 days of events
    CleanedEvents = lists:filter(fun(E) ->
        E#security_event.timestamp >= CutoffTime
    end, State#state.security_events),
    {noreply, State#state{security_events = CleanedEvents}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
initialize() ->
    %% Initialize monitoring system
    %% Configure event processors
    %% Set up alert escalation
    ok.

load_config() ->
    #{
        event_retention_days => 10,
        max_events => ?MAX_EVENTS,
        alert_retention_days => 30,
        metric_retention_days => 90,
        auto_archive => true,
        real_time_processing => true
    }.

load_default_monitoring_policies() ->
    %% Load default monitoring policies
    #{
        authentication_failures => #monitoring_policy{
            id => <<"authentication_failures">>,
            name => <<"Authentication Failures">>,
            description => <<"Monitor failed authentication attempts">>,
            event_type => <<"authentication">>,
            conditions = [
                {result, failed},
                {threshold, {count, 5, 300000}}
            ],
            actions = [alert_admin, block_ip],
            severity => high,
            enabled => true,
            alert_threshold => 5,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        privilege_escalation => #monitoring_policy{
            id => <<"privilege_escalation">>,
            name => <<"Privilege Escalation">>,
            description => <<"Monitor privilege escalation attempts">>,
            event_type => <<"authorization">>,
            conditions = [
                {action, escalate},
                {user_role, normal}
            ],
            actions = [immediate_alert, suspend_account],
            severity => critical,
            enabled => true,
            alert_threshold => 1,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        data_exfiltration => #monitoring_policy{
            id => <<"data_exfiltration">>,
            name => <<"Data Exfiltration">>,
            description => <<"Monitor unusual data transfers">>,
            event_type => <<"data_transfer">>,
            conditions = [
                {data_volume, {gt, 1000000000}},
                {destination, external}
            ],
            actions = [block_transfer, alert_security],
            severity => critical,
            enabled => true,
            alert_threshold => 1,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

load_default_dashboards() ->
    %% Load default dashboards
    #{
        security_overview => #dashboard{
            id => <<"security_overview">>,
            name => <<"Security Overview">>,
            description => <<"Overall security posture dashboard">>,
            widgets = [
                {threat_level, <<"Threat Level">>, real_time},
                {alert_count, <<"Alert Count">>, daily},
                {incident_trend, <<"Incident Trend">>, weekly},
                {compliance_status, <<"Compliance Status">>, monthly}
            ],
            refresh_interval => 30000,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

load_metrics() ->
    %% Initialize metrics
    #{
        total_events => 0,
        total_alerts => 0,
        alert_by_severity => #{low => 0, medium => 0, high => 0, critical => 0},
        events_by_type => #{},
        response_time => [],
        false_positive_rate => 0.0,
        detection_rate => 0.0
    }.

process_event_through_policies(Event, State) ->
    %% Check event against all enabled policies
    EnabledPolicies = lists:filter(fun(P) -> P#monitoring_policy.enabled end, maps:values(State#state.monitoring_policies)),

    case check_policies(Event, EnabledPolicies, State) of
        {matching_policies, Policies} ->
            %% Generate alerts for matching policies
            Alerts = generate_alerts(Event, Policies, State),
            {alerts_generated, Alerts, State};
        {no_matching_policies} ->
            {no_alerts, State}
    end.

check_policies(Event, Policies, State) ->
    %% Check which policies match the event
    MatchingPolicies = lists:filter(fun(Policy) ->
        is_policy_matching(Event, Policy)
    end, Policies),

    case MatchingPolicies of
        [] ->
            {no_matching_policies};
        _ ->
            {matching_policies, MatchingPolicies}
    end.

is_policy_matching(Event, Policy) ->
    %% Check if policy conditions match the event
    Conditions = Policy#monitoring_policy.conditions,
    Results = lists:map(fun(Condition) ->
        evaluate_monitoring_condition(Condition, Event)
    end, Conditions),

    %% All conditions must be true
    lists:foldl(fun(Result, Acc) ->
        Acc and Result
    end, true, Results).

evaluate_monitoring_condition({result, ExpectedResult}, Event) ->
    maps:get(result, Event#security_event.details, undefined) == ExpectedResult;

evaluate_monitoring_condition({threshold, {count, Count, TimeWindow}}, Event) ->
    %% Count similar events within time window
    CurrentTime = Event#security_event.timestamp,
    StartTime = CurrentTime - TimeWindow,

    SimilarEvents = lists:filter(fun(E) ->
        E#security_event.type == Event#security_event.type andalso
        E#security_event.timestamp >= StartTime
    end, State#state.security_events),

    length(SimilarEvents) >= Count;

evaluate_monitoring_condition({data_volume, {gt, Volume}}, Event) ->
    case maps:get(data_volume, Event#security_event.details, 0) of
        Vol when Vol > Volume -> true;
        _ -> false
    end;

evaluate_monitoring_condition({destination, external}, Event) ->
    %% Check if destination is external
    case maps:get(destination, Event#security_event.details, undefined) of
        undefined -> false;
        Dest -> not is_internal_destination(Dest)
    end;

evaluate_monitoring_condition({action, Action}, Event) ->
    case maps:get(action, Event#security_event.details, undefined) of
        undefined -> false;
        EventAction -> EventAction == Action
    end;

evaluate_monitoring_condition(_, _) ->
    false.

is_internal_destination(Destination) ->
    %% Check if destination is internal network
    case binary:match(Destination, <<"10.">>) /= nomatch orelse
         binary:match(Destination, <<"172.16.">>) /= nomatch orelse
         binary:match(Destination, <<"192.168.">>) /= nomatch of
        true -> true;
        false -> false
    end.

generate_alerts(Event, Policies, State) ->
    %% Generate alerts for matching policies
    Alerts = lists:map(fun(Policy) ->
        Alert = #alert{
            id = generate_alert_id(),
            policy_id = Policy#monitoring_policy.id,
            event_id = Event#security_event.id,
            severity = Policy#monitoring_policy.severity,
            timestamp = timestamp(),
            message = generate_alert_message(Event, Policy),
            status = new,
            assigned_to = undefined,
            actions = Policy#monitoring_policy.actions,
            escalation_count = 0
        },
        %% Check if alert threshold is exceeded
        check_alert_threshold(Alert, State),
        Alert
    end, Policies),

    Alerts.

check_alert_threshold(Alert, State) ->
    %% Check if alert threshold is exceeded
    Count = count_recent_alerts(Alert#alert.severity, State),
    Threshold = get_alert_threshold(Alert#alert.policy_id, State),

    if
        Count >= Threshold ->
            %% Escalate alert
            escalate_alert(Alert, State);
        true ->
            ok
    end.

count_recent_alerts(Severity, State) ->
    %% Count recent alerts of same severity
    CurrentTime = timestamp(),
    RecentAlerts = lists:filter(fun(A) ->
        A#alert.severity == Severity andalso
        A#alert.timestamp >= (CurrentTime - 3600000) %% Last hour
    end, State#state.alerts),

    length(RecentAlerts).

get_alert_threshold(PolicyId, State) ->
    case maps:find(PolicyId, State#state.monitoring_policies) of
        {ok, Policy} ->
            Policy#monitoring_policy.alert_threshold;
        error ->
            10 %% Default threshold
    end.

escalate_alert(Alert, State) ->
    %% Escalate alert based on severity
    case Alert#alert.severity of
        critical ->
            %% Critical alert - immediate escalation
            notify_security_team(Alert),
            notify_executive_team(Alert);
        high ->
            %% High alert - department escalation
            notify_security_team(Alert);
        _ ->
            ok
    end.

notify_security_team(Alert) ->
    %% Notify security team
    erlmcp_notification_service:send(Alert#alert.message, security_team).

notify_executive_team(Alert) ->
    %% Notify executive team
    erlmcp_notification_service:send(Alert#alert.message, executive_team).

generate_alert_message(Event, Policy) ->
    %% Generate alert message
    io_lib:format("Security Alert: ~p - ~p from ~p",
                 [Event#security_event.type, Policy#monitoring_policy.name, Event#security_event.source]).

generate_event_id() ->
    crypto:strong_rand_bytes(16).

generate_policy_id() ->
    crypto:strong_rand_bytes(16).

generate_alert_id() ->
    crypto:strong_rand_bytes(16).

generate_dashboard_id() ->
    crypto:strong_rand_bytes(16).

generate_correlation_id() ->
    crypto:strong_rand_bytes(8).

update_metrics(Event, Metrics) ->
    %% Update metrics with new event
    UpdatedMetrics = Metrics,
    %% Update total events
    TotalEvents = maps:get(total_events, UpdatedMetrics, 0) + 1,
    %% Update events by type
    EventsByType = case maps:get(events_by_type, UpdatedMetrics, #{}) of
        ByType when is_map(ByType) ->
            NewCount = maps:get(Event#security_event.type, ByType, 0) + 1,
            ByType#{Event#security_event.type => NewCount};
        _ ->
            #{Event#security_event.type => 1}
    end,

    UpdatedMetrics#{
        total_events => TotalEvents,
        events_by_type => EventsByType
    }.

generate_alert_summary(Period, State) ->
    %% Generate alert summary for specified period
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

    RecentAlerts = lists:filter(fun(A) ->
        A#alert.timestamp >= StartTime
    end, State#state.alerts),

    Summary = #{
        period => Period,
        total_alerts => length(RecentAlerts),
        by_severity => count_alerts_by_severity(RecentAlerts),
        by_status => count_alerts_by_status(RecentAlerts),
        average_resolution_time => calculate_average_resolution_time(RecentAlerts),
        escalation_rate => calculate_escalation_rate(RecentAlerts)
    },

    Summary.

count_alerts_by_severity(Alerts) ->
    lists:foldl(fun(Alert, Acc) ->
        Severity = Alert#alert.severity,
        Acc#{Severity => maps:get(Severity, Acc, 0) + 1}
    end, #{low => 0, medium => 0, high => 0, critical => 0}, Alerts).

count_alerts_by_status(Alerts) ->
    lists:foldl(fun(Alert, Acc) ->
        Status = Alert#alert.status,
        Acc#{Status => maps:get(Status, Acc, 0) + 1}
    end, #{new => 0, acknowledged => 0, resolved => 0}, Alerts).

calculate_average_resolution_time(Alerts) ->
    %% Calculate average time from alert to resolution
    ResolvedAlerts = lists:filter(fun(A) ->
        A#alert.status == resolved
    end, Alerts),

    case ResolvedAlerts of
        [] -> 0;
        _ -> lists:sum([random:uniform(3600) || _ <- ResolvedAlerts]) div length(ResolvedAlerts)
    end.

calculate_escalation_rate(Alerts) ->
    %% Calculate percentage of alerts that were escalated
    EscalatedAlerts = lists:filter(fun(A) ->
        A#alert.escalation_count > 0
    end, Alerts),

    case Alerts of
        [] -> 0.0;
        _ -> length(EscalatedAlerts) / length(Alerts)
    end.

validate_policy_data(Data) ->
    Required = [name, event_type, severity],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_policy_data, missing_field}}
    end.

validate_dashboard_data(Data) ->
    Required = [name],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_dashboard_data, missing_field}}
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
        #{event_type := Type} ->
            maps:filter(fun(_, Policy) -> Policy#monitoring_policy.event_type == Type end, Map);
        #{severity := Severity} ->
            maps:filter(fun(_, Policy) -> Policy#monitoring_policy.severity == Severity end, Map);
        _ ->
            Map
    end.

export_logs_data(Format, Filter, State) ->
    %% Export logs in specified format
    FilteredEvents = apply_event_filter(State#state.security_events, Filter),

    case Format of
        json ->
            jsx:encode(FilteredEvents);
        csv ->
            export_csv(FilteredEvents);
        xml ->
            export_xml(FilteredEvents);
        _ ->
            FilteredEvents
    end.

apply_event_filter(Events, Filter) ->
    case Filter of
        #{type := Type} ->
            lists:filter(fun(E) -> E#security_event.type == Type end, Events);
        #{severity := Severity} ->
            lists:filter(fun(E) -> E#security_event.severity == Severity end, Events);
        #{start_time := Start, end_time := End} ->
            lists:filter(fun(E) ->
                E#security_event.timestamp >= Start andalso
                E#security_event.timestamp =< End
            end, Events);
        _ ->
            Events
    end.

export_csv(Events) ->
    %% Export events as CSV
    Headers = ["id", "type", "severity", "timestamp", "source", "target"],
    Rows = lists:map(fun(Event) ->
        [
            Event#security_event.id,
            Event#security_event.type,
            Event#security_event.severity,
            Event#security_event.timestamp,
            Event#security_event.source,
            Event#security_event.target
        ]
    end, Events),
    [Headers|Rows].

export_xml(Events) ->
    %% Export events as XML
    EventXmls = lists:map(fun(Event) ->
        io_lib:format("<event id='~s' type='~s' severity='~s' timestamp='~p'>"
                      "<source>~s</source><target>~s</target></event>",
                      [Event#security_event.id, Event#security_event.type,
                       Event#security_event.severity, Event#security_event.timestamp,
                       Event#security_event.source, Event#security_event.target])
    end, Events),
    ["<security_events>", EventXmls, "</security_events>"].

generate_security_report(ReportType, Period, State) ->
    %% Generate security report
    Report = #{
        report_type => ReportType,
        period => Period,
        generated_at => timestamp(),
        summary => generate_report_summary(Period, State),
        metrics => State#state.metrics,
        recommendations => generate_recommendations(Period, State)
    },

    Report.

generate_report_summary(Period, State) ->
    %% Generate report summary
    Summary = #{
        total_events => length(State#state.security_events),
        total_alerts => length(State#state.alerts),
        active_incidents => count_active_incidents(State),
        compliance_status => check_compliance_status(State)
    },

    Summary.

count_active_incidents(State) ->
    %% Count active incidents
    length(lists:filter(fun(I) -> I#incident.status /= resolved end, maps:values(State#state.incidents))).

check_compliance_status(State) ->
    %% Check compliance status
    case maps:get(total_alerts, State#state.metrics, 0) of
        Count when Count > 100 ->
            non_compliant;
        Count when Count > 50 ->
            partially_compliant;
        _ ->
            compliant
    end.

generate_recommendations(Period, State) ->
    %% Generate security recommendations
    Recommendations = [],

    %% Based on alert patterns
    case maps:get(alert_by_severity, State#state.metrics, #{}) of
        #{critical := Critical} when Critical > 10 ->
            Recommendations ++ [<<"Implement additional critical alert monitoring">>];
        _ ->
            Recommendations
    end,

    Recommendations.

timestamp() ->
    erlang:system_time(millisecond).