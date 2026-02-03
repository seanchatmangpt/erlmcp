%% @doc Compliance Monitoring Service
%% Continuously monitors system compliance with various standards
%%
%% Responsibilities:
%% - Real-time compliance monitoring
%% - Alert generation for violations
%% - Continuous compliance scoring
%% - Trend analysis and forecasting
%% - Integration with monitoring systems
%%
%% @end
-module(erlmcp_compliance_monitor).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Monitoring API
-export([
    add_monitor/2,
    remove_monitor/1,
    get_monitoring_status/0,
    get_compliance_score/1,
    get_violations/2,
    generate_alert/3,
    pause_monitoring/1,
    resume_monitoring/1
]).

%% Types
-type compliance_standard() :: soc2_type_i | soc2_type_ii | hipaa | gdpr | iso27001 | finance | healthcare.
-type monitor_type() :: real_time | scheduled | continuous.
-type alert_type() :: violation | warning | info.
-type alert_level() :: low | medium | high | critical.

-record(monitor_config, {
    id :: binary(),
    standard :: compliance_standard(),
    type :: monitor_type(),
    frequency :: integer(),  % seconds
    enabled :: boolean(),
    rules :: [map()],
    thresholds :: map()
}).

-record(alert, {
    id :: binary(),
    type :: alert_type(),
    level :: alert_level(),
    standard :: compliance_standard(),
    message :: binary(),
    timestamp :: erlang:timestamp(),
    details :: map(),
    acknowledged :: boolean(),
    resolved :: boolean()
}).

-record(compliance_score, {
    standard :: compliance_standard(),
    score :: float(),
    trend :: increasing | decreasing | stable,
    last_updated :: erlang:timestamp(),
    history :: [float()]
}).

-record(violation, {
    id :: binary(),
    standard :: compliance_standard(),
    rule_id :: binary(),
    description :: binary(),
    severity :: alert_level(),
    detected_at :: erlang:timestamp(),
    resolved_at :: erlang:timestamp() | undefined,
    status :: open | resolved | escalated,
    details :: map()
}).

-record(state, {
    monitors :: #{binary() => #monitor_config{}},
    alerts :: [alert()],
    compliance_scores :: #{compliance_standard() => #compliance_score{}},
    violations :: [violation()],
    alerts_config :: map(),
    metrics :: map()
}).

%% Constants
-define(SERVER, ?MODULE).
-define(DEFAULT_MONITOR_INTERVAL, 60).  % seconds
-define(ALERT_RETENTION_DAYS, 90).
-define(VIOLATION_RETENTION_DAYS, 365).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the compliance monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the compliance monitor
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Add a new monitoring configuration
-spec add_monitor(binary(), map()) -> ok | {error, term()}.
add_monitor(MonitorId, Config) ->
    gen_server:call(?SERVER, {add_monitor, MonitorId, Config}).

%% @doc Remove a monitoring configuration
-spec remove_monitor(binary()) -> ok | {error, term()}.
remove_monitor(MonitorId) ->
    gen_server:call(?SERVER, {remove_monitor, MonitorId}).

%% @doc Get overall monitoring status
-spec get_monitoring_status() -> map().
get_monitoring_status() ->
    gen_server:call(?SERVER, get_monitoring_status).

%% @doc Get compliance score for a standard
-spec get_compliance_score(compliance_standard()) -> {ok, map()} | {error, term()}.
get_compliance_score(Standard) ->
    gen_server:call(?SERVER, {get_compliance_score, Standard}).

%% @doc Get violations for a standard
-spec get_violations(compliance_standard(), map()) -> {ok, [map()]} | {error, term()}.
get_violations(Standard, Filter) ->
    gen_server:call(?SERVER, {get_violations, Standard, Filter}).

%% @doc Generate an alert
-spec generate_alert(compliance_standard(), alert_type(), map()) -> ok.
generate_alert(Standard, AlertType, Details) ->
    gen_server:cast(?SERVER, {generate_alert, Standard, AlertType, Details}).

%% @doc Pause monitoring for a standard
-spec pause_monitoring(compliance_standard()) -> ok.
pause_monitoring(Standard) ->
    gen_server:call(?SERVER, {pause_monitoring, Standard}).

%% @doc Resume monitoring for a standard
-spec resume_monitoring(compliance_standard()) -> ok.
resume_monitoring(Standard) ->
    gen_server:call(?SERVER, {resume_monitoring, Standard}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize with default monitoring configurations
    DefaultMonitors = init_default_monitors(),
    AlertsConfig = init_alerts_config(),
    State = #state{
        monitors = DefaultMonitors,
        alerts = [],
        compliance_scores = init_compliance_scores(),
        violations = [],
        alerts_config = AlertsConfig,
        metrics = init_metrics()
    },

    %% Start monitoring timers
    {ok, State, 0}.

handle_call({add_monitor, MonitorId, Config}, _From, State) ->
    Monitor = validate_and_create_monitor(MonitorId, Config),
    UpdatedMonitors = maps:put(MonitorId, Monitor, State#state.monitors),

    %% Start monitoring if enabled
    case Monitor#monitor_config.enabled of
        true -> start_monitoring(Monitor);
        false -> ok
    end,

    {reply, ok, State#state{monitors = UpdatedMonitors}};

handle_call({remove_monitor, MonitorId}, _From, State) ->
    UpdatedMonitors = maps:remove(MonitorId, State#state.monitors),
    stop_monitoring(MonitorId),
    {reply, ok, State#state{monitors = UpdatedMonitors}};

handle_call(get_monitoring_status, _From, State) ->
    Status = generate_monitoring_status(State),
    {reply, Status, State};

handle_call({get_compliance_score, Standard}, _From, State) ->
    case maps:find(Standard, State#state.compliance_scores) of
        {ok, Score} -> {reply, {ok, Score}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({get_violations, Standard, Filter}, _From, State) ->
    FilteredViolations = filter_violations(State#state.violations, Standard, Filter),
    {reply, {ok, FilteredViolations}, State};

handle_call({pause_monitoring, Standard}, _From, State) ->
    UpdatedMonitors = pause_monitors_by_standard(Standard, State#state.monitors),
    {reply, ok, State#state{monitors = UpdatedMonitors}};

handle_call({resume_monitoring, Standard}, _From, State) ->
    UpdatedMonitors = resume_monitors_by_standard(Standard, State#state.monitors),
    {reply, ok, State#state{monitors = UpdatedMonitors}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({generate_alert, Standard, AlertType, Details}, State) ->
    Alert = create_alert(Standard, AlertType, Details),
    UpdatedAlerts = [Alert | State#state.alerts],
    TruncatedAlerts = truncate_alerts(UpdatedAlerts),

    %% Check if alert needs escalation
    case should_escalate(Alert) of
        true -> escalate_alert(Alert);
        false -> ok
    end,

    {noreply, State#state{alerts = TruncatedAlerts}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    %% Perform compliance checks
    NewState = perform_compliance_checks(State),

    %% Schedule next check
    {noreply, NewState, ?DEFAULT_MONITOR_INTERVAL};

handle_info(compliance_check, State) ->
    %% Perform scheduled compliance checks
    NewState = perform_compliance_checks(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

init_default_monitors() ->
    #{
        "soc2_realtime" => #monitor_config{
            id => "soc2_realtime",
            standard => soc2_type_ii,
            type => real_time,
            frequency => 30,
            enabled => true,
            rules [
                #{
                    id => "access_control_violation",
                    condition => "failed_access_attempts > 5",
                    action => "alert"
                },
                #{
                    id => "audit_trail_gap",
                    condition => "audit_events_missing > 10",
                    action => "critical"
                }
            ],
            thresholds => #{
                failed_access_attempts => 5,
                audit_events_missing => 10
            }
        },
        "hipaa_continuous" => #monitor_config{
            id => "hipaa_continuous",
            standard => hipaa,
            type => continuous,
            frequency => 300,  % 5 minutes
            enabled => true,
            rules => [
                #{
                    id => "phi_access_without_auth",
                    condition => "unauthorized_phi_access > 0",
                    action => "critical"
                },
                #{
                    id => "data_breach_detected",
                    condition => "data_breach_indicator = true",
                    action => "critical"
                }
            ],
            thresholds => #{
                unauthorized_phi_access => 0,
                data_breach_indicator => true
            }
        },
        "gdpr_daily" => #monitor_config{
            id => "gdpr_daily",
            type => scheduled,
            standard => gdpr,
            frequency => 86400,  % 24 hours
            enabled => true,
            rules => [
                #{
                    id => "consent_expiry_check",
                    condition => "expired_consent > 0",
                    action => "alert"
                },
                #{
                    id => "data_subject_request",
                    condition => "pending_requests > 5",
                    action => "warning"
                }
            ],
            thresholds => #{
                expired_consent => 0,
                pending_requests => 5
            }
        },
        "iso27001_weekly" => #monitor_config{
            id => "iso27001_weekly",
            type => scheduled,
            standard => iso27001,
            frequency => 604800,  % 7 days
            enabled => true,
            rules => [
                #{
                    id => "control_failure",
                    condition => "failed_controls > 2",
                    action => "warning"
                },
                #{
                    id => "audit_findings",
                    condition => "unresolved_findings > 5",
                    action => "alert"
                }
            ],
            thresholds => #{
                failed_controls => 2,
                unresolved_findings => 5
            }
        }
    }.

init_alerts_config() ->
    #{
        escalation_rules => [
            #{
                condition => "level = critical",
                action => "immediate_notification",
                recipients => ["security_team", "executive_management"]
            },
            #{
                condition => "level = high AND standard = hipaa",
                action => "regulatory_notification",
                recipients => ["compliance_officer", "legal"]
            },
            #{
                condition => "type = violation AND consecutive_count > 3",
                action => "escalation",
                recipients => ["management"]
            }
        ],
        notification_channels => [
            #{type => email, enabled => true},
            #{type => sms, enabled => true, level => critical},
            #{type => slack, enabled => true},
            #{type => pagerduty, enabled => true, level => high}
        ]
    }.

init_compliance_scores() ->
    #{
        soc2_type_i => #compliance_score{
            standard => soc2_type_i,
            score => 0.0,
            trend => stable,
            last_updated => erlang:timestamp(),
            history => []
        },
        soc2_type_ii => #compliance_score{
            standard => soc2_type_ii,
            score => 0.0,
            trend => stable,
            last_updated => erlang:timestamp(),
            history => []
        },
        hipaa => #compliance_score{
            standard => hipaa,
            score => 0.0,
            trend => stable,
            last_updated => erlang:timestamp(),
            history => []
        },
        gdpr => #compliance_score{
            standard => gdpr,
            score => 0.0,
            trend => stable,
            last_updated => erlang:timestamp(),
            history => []
        },
        iso27001 => #compliance_score{
            standard => iso27001,
            score => 0.0,
            trend => stable,
            last_updated => erlang:timestamp(),
            history => []
        }
    }.

init_metrics() ->
    #{
        total_checks => 0,
        violations_detected => 0,
        alerts_generated => 0,
        last_check => undefined,
        average_response_time => 0.0,
        uptime_start => erlang:timestamp()
    }.

validate_and_create_monitor(MonitorId, Config) ->
    %% Validate monitor configuration
    RequiredFields = [standard, type, frequency, enabled, rules, thresholds],
    lists:foreach(fun(Field) ->
        case maps:is_key(Field, Config) of
            false -> error({missing_required_field, Field});
            true -> ok
        end
    end, RequiredFields),

    #monitor_config{
        id = MonitorId,
        standard = maps:get(standard, Config),
        type = maps:get(type, Config),
        frequency = maps:get(frequency, Config),
        enabled = maps:get(enabled, Config),
        rules = maps:get(rules, Config),
        thresholds = maps:get(thresholds, Config)
    }.

start_monitoring(#monitor_config{id = MonitorId, frequency = Frequency, type = real_time}) ->
    %% Start real-time monitoring timer
    erlang:send_after(Frequency * 1000, self(), {monitor_tick, MonitorId});

start_monitoring(#monitor_config{id = MonitorId, frequency = Frequency, type = continuous}) ->
    %% Start continuous monitoring
    erlang:send_after(Frequency * 1000, self(), {monitor_tick, MonitorId});

start_monitoring(#monitor_config{id = MonitorId, frequency = Frequency, type = scheduled}) ->
    %% Schedule monitoring at specific times
    schedule_scheduled_monitoring(MonitorId, Frequency).

stop_monitoring(MonitorId) ->
    %% Cancel any pending monitoring timers for this monitor
    %% Implementation depends on how timers are tracked
    ok.

schedule_scheduled_monitoring(MonitorId, Frequency) ->
    %% Schedule monitoring at regular intervals
    erlang:send_after(Frequency * 1000, self(), {monitor_tick, MonitorId}).

pause_monitors_by_standard(Standard, Monitors) ->
    maps:map(fun(_, Monitor) ->
        case Monitor#monitor_config.standard of
            Standard -> Monitor#monitor_config{enabled = false};
            _ -> Monitor
        end
    end, Monitors).

resume_monitors_by_standard(Standard, Monitors) ->
    maps:map(fun(_, Monitor) ->
        case Monitor#monitor_config.standard of
            Standard -> Monitor#monitor_config{enabled = true};
            _ -> Monitor
        end
    end, Monitors).

perform_compliance_checks(State) ->
    %% Check each enabled monitor
    EnabledMonitors = [M || M <- maps:values(State#state.monitors), M#monitor_config.enabled],
    Results = [check_monitor(Monitor) || Monitor <- EnabledMonitors],

    %% Update compliance scores
    UpdatedScores = update_compliance_scores(Results, State#state.compliance_scores),

    %% Process violations
    NewViolations = process_violations(Results, State#state.violations),

    %% Update metrics
    UpdatedMetrics = update_metrics(State#state.metrics, Results),

    %% Schedule next check
    erlang:send_after(?DEFAULT_MONITOR_INTERVAL * 1000, self(), compliance_check),

    State#state{
        compliance_scores = UpdatedScores,
        violations = NewViolations,
        metrics = UpdatedMetrics
    }.

check_monitor(#monitor_config{standard = soc2_type_ii, rules = Rules}) ->
    %% Perform SOC2 Type II compliance checks
    Results = [check_soc2_rule(Rule) || Rule <- Rules],
    Results;

check_monitor(#monitor_config{standard = hipaa, rules = Rules}) ->
    %% Perform HIPAA compliance checks
    Results = [check_hipaa_rule(Rule) || Rule <- Rules],
    Results;

check_monitor(#monitor_config{standard = gdpr, rules = Rules}) ->
    %% Perform GDPR compliance checks
    Results = [check_gdpr_rule(Rule) || Rule <- Rules],
    Results;

check_monitor(#monitor_config{standard = iso27001, rules = Rules}) ->
    %% Perform ISO27001 compliance checks
    Results = [check_iso_rule(Rule) || Rule <- Rules],
    Results.

check_soc2_rule(Rule) ->
    %% Implement SOC2 rule checking logic
    case Rule of
        #{id := "access_control_violation", condition := Condition} ->
            check_condition(Condition);
        #{id := "audit_trail_gap", condition := Condition} ->
            check_condition(Condition);
        _ -> ok
    end.

check_hipaa_rule(Rule) ->
    %% Implement HIPAA rule checking logic
    case Rule of
        #{id := "phi_access_without_auth", condition := Condition} ->
            check_condition(Condition);
        #{id := "data_breach_detected", condition := Condition} ->
            check_condition(Condition);
        _ -> ok
    end.

check_gdpr_rule(Rule) ->
    %% Implement GDPR rule checking logic
    case Rule of
        #{id := "consent_expiry_check", condition := Condition} ->
            check_condition(Condition);
        #{id := "data_subject_request", condition := Condition} ->
            check_condition(Condition);
        _ -> ok
    end.

check_iso_rule(Rule) ->
    %% Implement ISO27001 rule checking logic
    case Rule of
        #{id := "control_failure", condition := Condition} ->
            check_condition(Condition);
        #{id := "audit_findings", condition := Condition} ->
            check_condition(Condition);
        _ -> ok
    end.

check_condition(Condition) ->
    %% Evaluate condition against current system state
    %% This is a simplified version - actual implementation would be more complex
    case Condition of
        "failed_access_attempts > 5" -> check_failed_access_attempts();
        "audit_events_missing > 10" -> check_audit_events_missing();
        "unauthorized_phi_access > 0" -> check_unauthorized_phi_access();
        "data_breach_indicator = true" -> check_data_breach_indicator();
        "expired_consent > 0" -> check_expired_consent();
        "pending_requests > 5" -> check_pending_requests();
        "failed_controls > 2" -> check_failed_controls();
        "unresolved_findings > 5" -> check_unresolved_findings();
        _ -> ok
    end.

check_failed_access_attempts() ->
    %% Check for excessive failed access attempts
    case get_failed_access_attempts() of
        Count when Count > 5 -> violation;
        _ -> ok
    end.

check_audit_events_missing() ->
    %% Check for missing audit events
    case get_missing_audit_events() of
        Count when Count > 10 -> violation;
        _ -> ok
    end.

check_unauthorized_phi_access() ->
    %% Check for unauthorized PHI access
    case get_unauthorized_phi_access() of
        Count when Count > 0 -> violation;
        _ -> ok
    end.

check_data_breach_indicator() ->
    %% Check for data breach indicators
    case get_data_breach_indicators() of
        true -> violation;
        false -> ok
    end.

check_expired_consent() ->
    %% Check for expired consents
    case get_expired_consent_count() of
        Count when Count > 0 -> warning;
        _ -> ok
    end.

check_pending_requests() ->
    %% Check for pending data subject requests
    case get_pending_requests_count() of
        Count when Count > 5 -> warning;
        _ -> ok
    end.

check_failed_controls() ->
    %% Check for failed ISO controls
    case get_failed_controls_count() of
        Count when Count > 2 -> warning;
        _ -> ok
    end.

check_unresolved_findings() ->
    %% Check for unresolved audit findings
    case get_unresolved_findings_count() of
        Count when Count > 5 -> warning;
        _ -> ok
    end.

get_failed_access_attempts() -> 0.
get_missing_audit_events() -> 0.
get_unauthorized_phi_access() -> 0.
get_data_breach_indicators() -> false.
get_expired_consent_count() -> 0.
get_pending_requests_count() -> 0.
get_failed_controls_count() -> 0.
get_unresolved_findings_count() -> 0.

update_compliance_scores(Results, Scores) ->
    %% Update compliance scores based on check results
    lists:foldl(fun(Result, AccScores) ->
        case Result of
            #{standard := Standard, score := Score} ->
                maps:update(Standard, update_score(AccScores, Standard, Score), AccScores);
            _ -> AccScores
        end
    end, Scores, Results).

update_score(Scores, Standard, NewScore) ->
    case maps:find(Standard, Scores) of
        {ok, CurrentScore} ->
            History = CurrentScore#compliance_score.history,
            UpdatedHistory = lists:sublist([NewScore | History], 10),  % Keep last 10 scores
            Trend = calculate_trend([NewScore | History]),
            CurrentScore#compliance_score{
                score = NewScore,
                trend = Trend,
                last_updated => erlang:timestamp(),
                history = UpdatedHistory
            };
        error ->
            #compliance_score{
                standard = Standard,
                score = NewScore,
                trend = stable,
                last_updated => erlang:timestamp(),
                history = [NewScore]
            }
    end.

calculate_trend([_]) -> stable;
calculate_trend([_, _]) -> stable;
calculate_trend([Recent, Prev | _]) when Recent > Prev -> increasing;
calculate_trend([Recent, Prev | _]) when Recent < Prev -> decreasing;
calculate_trend(_) -> stable.

process_violations(Results, ExistingViolations) ->
    %% Process violations from compliance checks
    NewViolations = lists:foldl(fun(Result, AccViolations) ->
        case Result of
            #{type := violation, details := Details} ->
                create_violation(Details, AccViolations);
            _ -> AccViolations
        end
    end, [], Results),

    %% Merge with existing violations
    ExistingViolations ++ NewViolations.

create_violation(Details, ExistingViolations) ->
    Violation = #violation{
        id = generate_violation_id(),
        standard = maps:get(standard, Details),
        rule_id = maps:get(rule_id, Details),
        description = maps:get(description, Details),
        severity = maps:get(severity, Details),
        detected_at = erlang:timestamp(),
        status = open,
        details = Details
    },
    [Violation | ExistingViolations].

generate_violation_id() ->
    integer_to_binary(erlang:system_time(second)).

update_metrics(Metrics, Results) ->
    %% Update monitoring metrics
    TotalChecks = Metrics#total_checks + length(Results),
    Violations = length([R || R <- Results, maps:get(type, R, ok) =:= violation]),
    Alerts = length([R || R <- Results, maps:get(type, R, ok) =:= alert]),

    Metrics#{
        total_checks => TotalChecks,
        violations_detected => Metrics#violations_detected + Violations,
        alerts_generated => Metrics#alerts_generated + Alerts,
        last_check => erlang:timestamp()
    }.

create_alert(Standard, AlertType, Details) ->
    AlertLevel = maps:get(level, Details, medium),
    Alert = #alert{
        id = generate_alert_id(),
        type = AlertType,
        level = AlertLevel,
        standard = Standard,
        message = maps:get(message, Details, "No message"),
        timestamp => erlang:timestamp(),
        details = Details,
        acknowledged = false,
        resolved = false
    },
    Alert.

generate_alert_id() ->
    integer_to_binary(erlang:system_time(second)).

filter_violations(Violations, Standard, Filter) ->
   Filtered = lists:filter(fun(V) ->
        V#violation.standard =:= Standard
    end, Violations),

    case maps:is_key(resolved, Filter) of
        true ->
            ResolvedFilter = maps:get(resolved, Filter),
            lists:filter(fun(V) ->
                V#violation.status =:= case ResolvedFilter of
                    true -> resolved;
                    false -> open
                end
            end, Filtered);
        false -> Filtered
    end.

truncate_alerts(Alerts) when length(Alerts) > 10000 ->
    %% Keep only recent alerts
    lists:sublist(Alerts, 10000);
truncate_alerts(Alerts) -> Alerts.

should_escalate(#alert{level = critical}) -> true;
should_escalate(#alert{level = high, standard = hipaa}) -> true;
should_escalate(#alert{type = violation}) -> true;
should_escalate(_) -> false.

escalate_alert(Alert) ->
    %% Escalate alert to appropriate teams
    %% Implementation would send notifications to escalation teams
    ok.

generate_monitoring_status(State) ->
    #{
        total_monitors => maps:size(State#state.monitors),
        enabled_monitors => length([M || M <- maps:values(State#state.monitors), M#monitor_config.enabled]),
        disabled_monitors => length([M || M <- maps:values(State#state.monitors), not M#monitor_config.enabled]),
        total_alerts => length(State#state.alerts),
        unresolved_alerts => length([A || A <- State#state.alerts, not A#alert.acknowledged]),
        total_violations => length(State#state.violations),
        open_violations => length([V || V <- State#state.violations, V#violation.status =:= open]),
        resolved_violations => length([V || V <- State#state.violations, V#violation.status =:= resolved]),
        uptime => calculate_uptime(State#state.metrics),
        last_check => State#state.metrics#last_check
    }.

calculate_uptime(Metrics) ->
    case maps:find(uptime_start, Metrics) of
        {ok, Start} ->
            erlang:time_diff(erlang:timestamp(), Start);
        error -> 0
    end.

%%====================================================================
%% End of File
%%====================================================================