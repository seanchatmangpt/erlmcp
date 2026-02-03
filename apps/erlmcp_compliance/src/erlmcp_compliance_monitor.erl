%% @doc erlmcp Compliance Monitor
%% Real-time compliance monitoring and alerting system
%% Monitors compliance status across all frameworks and generates alerts
-module(erlmcp_compliance_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, monitor_compliance/1, check_control_status/2,
         get_violations/1, generate_alert/5, run_continuous_check/0,
         get_compliance_dashboard/0, set_compliance_threshold/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(control_status, {
    id :: binary(),
    framework :: soc2 | hipaa | gdpr | iso27001,
    name :: binary(),
    status :: compliant | non_compliant | partial_compliance | not_implemented,
    last_check :: erlang:timestamp() | null,
    effectiveness :: float(),
    violations :: list(),
    next_review :: erlang:timestamp()
}).

-record.monitor_config, {
    check_interval :: integer(),  % milliseconds
    alert_threshold :: integer(), % severity level (1-5)
    escalation_policy :: list(),
    notifications :: list(),
    reporting_enabled :: boolean(),
    continuous_monitoring :: boolean()
}.

-record(state, {
    control_statuses :: #{binary() => #control_status{}},
    alerts :: list(),
    violation_history :: list(),
    monitoring_config :: #monitor_config{},
    active_checks :: list(),
    metrics :: #{},
    last_continuous_check :: erlang:timestamp() | null
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

monitor_compliance(Framework) ->
    gen_server:call(?SERVER, {monitor_compliance, Framework}).

check_control_status(Framework, ControlId) ->
    gen_server:call(?SERVER, {check_control_status, Framework, ControlId}).

get_violations(Framework) ->
    gen_server:call(?SERVER, {get_violations, Framework}).

generate_alert(Framework, Severity, Message, ControlId, Details) ->
    gen_server:cast(?SERVER, {generate_alert, Framework, Severity, Message, ControlId, Details}).

run_continuous_check() ->
    gen_server:cast(?SERVER, run_continuous_check).

get_compliance_dashboard() ->
    gen_server:call(?SERVER, get_compliance_dashboard).

set_compliance_threshold(Framework, Threshold, Metric) ->
    gen_server:call(?SERVER, {set_compliance_threshold, Framework, Threshold, Metric}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ControlStatuses0 = init_control_statuses(),
    MonitoringConfig0 = #monitor_config{
        check_interval = 300000,  % 5 minutes
        alert_threshold = 3,
        escalation_policy = [
            {1, ["security_team"]},
            {2, ["security_team", "compliance_officer"]},
            {3, ["security_team", "compliance_officer", "cto"]},
            {4, ["security_team", "compliance_officer", "cto", "ceo"]},
            {5, ["security_team", "compliance_officer", "cto", "ceo", "board"]}
        ],
        notifications = ["email", "sms", "slack", "webhook"],
        reporting_enabled = true,
        continuous_monitoring = true
    },
    State0 = #state{
        control_statuses = ControlStatuses0,
        alerts = [],
        violation_history = [],
        monitoring_config = MonitoringConfig0,
        active_checks = [],
        metrics = #{},
        last_continuous_check = null
    },
    erlmcp_telemetry:counter("compliance.monitor.initialized", 1,
                           #{component => "compliance"}),
    {ok, State0}.

handle_call({monitor_compliance, Framework}, _From, State) ->
    ControlStatuses = get_controls_for_framework(Framework, State),
    ComplianceScore = calculate_compliance_score(ControlStatuses),

    Result = #{
        framework => Framework,
        total_controls => length(ControlStatuses),
        compliant_controls => length([C || C <- ControlStatuses, C#control_status.status =:= compliant]),
        non_compliant_controls => length([C || C <- ControlStatuses, C#control_status.status =:= non_compliant]),
        partial_controls => length([C || C <- ControlStatuses, C#control_status.status =:= partial_compliance]),
        not_implemented_controls => length([C || C <- ControlStatuses, C#control_status.status =:= not_implemented]),
        compliance_score => ComplianceScore,
        last_check => erlang:timestamp()
    },

    {reply, {ok, Result}, State};

handle_call({check_control_status, Framework, ControlId}, _From, State) ->
    case maps:get(ControlId, State#state.control_statuses, undefined) of
        undefined ->
            {reply, {error, control_not_found}, State};
        ControlStatus ->
            {reply, {ok, ControlStatus}, State}
    end;

handle_call({get_violations, Framework}, _From, State) ->
    Violations = lists:filter(
        fun(Control) ->
            case Control#control_status.status of
                non_compliant -> true;
                partial_compliance -> true;
                _ -> false
            end,
            Control#control_status.framework =:= Framework
        end, maps:values(State#state.control_statuses)),
    {reply, {ok, Violations}, State};

handle_call(get_compliance_dashboard, _From, State) ->
    Dashboard = build_compliance_dashboard(State),
    {reply, {ok, Dashboard}, State};

handle_call({set_compliance_threshold, Framework, Threshold, Metric}, _From, State) ->
    Metrics1 = maps:put({Framework, Metric}, Threshold, State#state.metrics),
    {reply, {ok, threshold_updated}, State#state{metrics = Metrics1}}.

handle_cast({generate_alert, Framework, Severity, Message, ControlId, Details}, State) ->
    Alert = create_alert(Framework, Severity, Message, ControlId, Details),
    Alerts1 = [Alert | State#state.alerts],

    % Send notifications based on severity
    send_notifications(Alert, State#state.monitoring_config),

    erlmcp_telemetry:counter("compliance.alert.generated", 1,
                           #{framework => atom_to_list(Framework),
                             severity => integer_to_list(Severity)}),

    {noreply, State#state{alerts = Alerts1}};

handle_cast(run_continuous_check, State) ->
    Now = erlang:timestamp(),
    case State#state.monitoring_config.continuous_monitoring of
        true ->
            % Check all frameworks
            Frameworks = [soc2, hipaa, gdpr, iso27001],
            ActiveChecks = [start_framework_check(F, Now) || F <- Frameworks],

            % Update violation history
            NewViolations = collect_violations(State),
            ViolationHistory1 = [Now | NewViolations] ++ State#state.violation_history,

            % Update metrics
            Metrics1 = update_metrics(State),

            State1 = State#state{
                active_checks = ActiveChecks,
                violation_history = ViolationHistory1,
                last_continuous_check = Now,
                metrics = Metrics1
            },

            % Schedule next check
            schedule_next_check(State1),

            {noreply, State1};
        false ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_control_statuses() ->
    #{
        %% SOC2 Controls
        <<"soc2-cc6-1">> => #control_status{
            id = <<"soc2-cc6-1">>,
            framework = soc2,
            name = "Logical and Physical Access Controls",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"soc2-cc6-6">> => #control_status{
            id = <<"soc2-cc6-6">>,
            framework = soc2,
            name = "Security Event Logging",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"soc2-cc1-1">> => #control_status{
            id = <<"soc2-cc1-1">>,
            framework = soc2,
            name = "System Monitoring",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },

        %% HIPAA Controls
        <<"hipaa-164-306">> => #control_status{
            id = <<"hipaa-164-306">>,
            framework = hipaa,
            name = "Administrative Safeguards",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"hipaa-164-310">> => #control_status{
            id = <<"hipaa-164-310">>,
            framework = hipaa,
            name = "Physical Safeguards",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"hipaa-164-312">> => #control_status{
            id = <<"hipaa-164-312">>,
            framework = hipaa,
            name = "Technical Safeguards",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },

        %% GDPR Controls
        <<"gdpr-art-5">> => #control_status{
            id = <<"gdpr-art-5">>,
            framework = gdpr,
            name = "Lawful, Fair and Transparent Processing",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"gdpr-art-16">> => #control_status{
            id = <<"gdpr-art-16">>,
            framework = gdpr,
            name = "Right to Erasure",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"gdpr-art-32">> => #control_status{
            id = <<"gdpr-art-32">>,
            framework = gdpr,
            name = "Security of Processing",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },

        %% ISO27001 Controls
        <<"iso-27001-A.9">> => #control_status{
            id = <<"iso-27001-A.9">>,
            framework = iso27001,
            name = "Access Control",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"iso-27001-A.12">> => #control_status{
            id = <<"iso-27001-A.12">>,
            framework = iso27001,
            name = "Information Systems Acquisition, Development and Maintenance",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        },
        <<"iso-27001-A.16">> => #control_status{
            id = <<"iso-27001-A.16">>,
            framework = iso27001,
            name = "Incident Management",
            status = not_implemented,
            last_check = null,
            effectiveness = 0.0,
            violations = [],
            next_review = erlang:timestamp()
        }
    }.

get_controls_for_framework(Framework, State) ->
    lists:filter(
        fun(Control) ->
            Control#control_status.framework =:= Framework
        end, maps:values(State#state.control_statuses)).

calculate_compliance_score(ControlStatuses) ->
    TotalControls = length(ControlStatuses),
    if TotalControls == 0 ->
        0.0;
    true ->
        Compliant = length([C || C <- ControlStatuses, C#control_status.status =:= compliant]),
        Partial = length([C || C <- ControlStatuses, C#control_status.status =:= partial_compliance]),
        (Compliant + Partial * 0.5) / TotalControls
    end.

create_alert(Framework, Severity, Message, ControlId, Details) ->
    #{
        id => generate_alert_id(),
        timestamp => erlang:timestamp(),
        framework => Framework,
        severity => Severity,
        message => Message,
        control_id => ControlId,
        details => Details,
        acknowledged => false,
        assigned_to => null,
        resolved_at => null
    }.

generate_alert_id() ->
    iolist_to_binary(io_lib:format("alert-~s-~s-~w", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(6) |> binary:encode_hex(),
        erlang:unique_integer()
    ])).

send_notifications(Alert, Config) ->
    Severity = Alert#{severity},
    case Severity of
        Level when Level >= Config#monitor_config.alert_threshold ->
            % Send notifications based on escalation policy
            Escalation = get_escalation_for_severity(Level, Config),
            lists:foreach(
                fun(NotificationType) ->
                    send_notification(NotificationType, Alert)
                end, Config#monitor_config.notifications);
        _ ->
            ok
    end.

get_escalation_for_severity(Severity, Config) ->
    case lists:keyfind(Severity, 1, Config#monitor_config.escalation_policy) of
        {Severity, Recipients} -> Recipients;
        _ -> []
    end.

send_notification(NotificationType, Alert) ->
    % This would integrate with notification systems
    % For now, just log
    erlmcp_telemetry:counter("compliance.notification.sent", 1,
                           #{type => atom_to_list(NotificationType)}),
    ok.

start_framework_check(Framework, StartTime) ->
    CheckId = generate_check_id(),
    % Start a monitoring process for this framework
    #{id => CheckId, framework => Framework, start_time => StartTime, status => running}.

generate_check_id() ->
    iolist_to_binary(io_lib:format("check-~s-~s", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(6) |> binary:encode_hex()
    ])).

collect_violations(State) ->
    % Get current violations
    Violations = lists:filter(
        fun(Control) ->
            case Control#control_status.status of
                non_compliant -> true;
                partial_compliance -> true;
                _ -> false
            end
        end, maps:values(State#state.control_statuses)),
    Violations.

update_metrics(State) ->
    % Update monitoring metrics
    #{
        total_controls => maps:size(State#state.control_statuses),
        violated_controls => length(State#state.violation_history),
        active_alerts => length([A || A <- State#state.alerts, not maps:get(acknowledged, A, false)]),
        average_effectiveness => calculate_average_effectiveness(State#state.control_statuses)
    }.

calculate_average_effectiveness(ControlStatuses) ->
    Effectiveness = [C#control_status.effectiveness || C <- ControlStatuses],
    if Effectiveness == [] ->
        0.0;
    true ->
        lists:sum(Effectiveness) / length(Effectiveness)
    end.

schedule_next_check(State) ->
    CheckInterval = State#state.monitoring_config.check_interval,
    erlang:send_after(CheckInterval, self(), run_continuous_check).

build_compliance_dashboard(State) ->
    ControlStatuses = maps:values(State#state.control_statuses),

    % Calculate overall metrics
    TotalControls = length(ControlStatuses),
    CompliantControls = length([C || C <- ControlStatuses, C#control_status.status =:= compliant]),
    NonCompliantControls = length([C || C <- ControlStatuses, C#control_status.status =:= non_compliant]),
    PartialControls = length([C || C <- ControlStatuses, C#control_status.status =:= partial_compliance]),

    OverallScore = (CompliantControls + PartialControls * 0.5) / TotalControls,

    % Framework-specific scores
    Frameworks = [soc2, hipaa, gdpr, iso27001],
    FrameworkScores = lists:map(fun(Framework) ->
        Controls = get_controls_for_framework(Framework, State),
        Score = calculate_compliance_score(Controls),
        {Framework, Score}
    end, Frameworks),

    % Recent violations
    RecentViolations = lists:sublist(State#state.violation_history, 10),

    % Active alerts
    ActiveAlerts = lists:filter(fun(Alert) ->
        not maps:get(acknowledged, Alert, false)
    end, State#state.alerts),

    % Dashboard data
    #{
        overall_compliance => OverallScore,
        total_controls => TotalControls,
        compliant_controls => CompliantControls,
        non_compliant_controls => NonCompliantControls,
        partial_controls => PartialControls,
        framework_scores => FrameworkScores,
        recent_violations => RecentViolations,
        active_alerts => length(ActiveAlerts),
        metrics => State#state.metrics,
        last_updated => erlang:timestamp()
    }.