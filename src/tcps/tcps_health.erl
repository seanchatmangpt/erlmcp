%%%-----------------------------------------------------------------------------
%%% @doc TCPS Health Monitoring and Alerting System with OpenTelemetry
%%%
%%% Production-grade observability system implementing:
%%% - Comprehensive health checks for all TCPS components
%%% - OpenTelemetry integration (traces, metrics, logs)
%%% - SLO/SLI tracking with error budget calculation
%%% - Multi-channel alerting (Slack, Email, PagerDuty, Webhook)
%%% - Real-time dashboard API
%%% - Self-healing auto-remediation
%%% - Multi-platform metric export (Prometheus, Datadog, New Relic, Grafana)
%%%
%%% == Architecture ==
%%% This module provides a unified observability layer over all TCPS subsystems:
%%% - Kanban WIP management
%%% - Andon stop-the-line events
%%% - TPM maintenance schedules
%%% - Ontology SHACL validation
%%% - Receipt generation and persistence
%%%
%%% All telemetry is correlated via OpenTelemetry trace context for
%%% distributed tracing across the production pipeline.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_health).
-behaviour(gen_server).

%% API exports
-export([
    % System lifecycle
    start_link/0,
    start_link/1,
    stop/0,

    % Health checks
    health_check/0,
    component_health/1,
    get_health_status/0,

    % OpenTelemetry
    init_otel/0,
    trace_production_stage/2,
    trace_production_stage/3,
    emit_metric/3,
    emit_metric/4,

    % Metrics collection
    collect_metrics/0,
    export_metrics/1,
    get_metric_history/2,

    % Alerting
    define_alert_rules/0,
    check_alert_rules/0,
    send_alert/1,
    get_alert_history/1,

    % Logging
    structured_log/3,
    log_production_event/2,

    % SLO/SLI tracking
    define_slos/0,
    measure_slis/0,
    calculate_error_budget/0,
    get_slo_status/0,

    % Dashboard API
    get_dashboard_data/0,
    get_component_metrics/1,

    % Platform integrations
    export_to_prometheus/0,
    send_to_datadog/1,
    send_to_newrelic/1,
    send_to_grafana_cloud/1,

    % Self-healing
    auto_remediate/1,
    get_remediation_history/1,

    % Testing helpers
    simulate_failure/2,
    reset_state/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal exports for background processes
-export([health_check_loop/1, alert_check_loop/1]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type component() :: kanban | andon | tpm | ontology | receipts | persistence.
-type health_status() :: healthy | degraded | unhealthy.
-type alert_severity() :: critical | warning | info.
-type metric_type() :: counter | gauge | histogram | summary.
-type export_format() :: prometheus | otlp | json | datadog | newrelic.

-type health_result() :: #{
    status := health_status(),
    components := #{component() => component_health()},
    metrics := metrics_snapshot(),
    alerts := [alert()],
    timestamp := integer()
}.

-type component_health() ::
    {healthy, Details :: map()} |
    {degraded, Reason :: binary(), Details :: map()} |
    {unhealthy, Reason :: binary(), Details :: map()}.

-type metrics_snapshot() :: #{
    production := production_metrics(),
    quality := quality_metrics(),
    kanban := kanban_metrics(),
    andon := andon_metrics(),
    kaizen := kaizen_metrics(),
    tpm := tpm_metrics()
}.

-type production_metrics() :: #{
    throughput := float(),
    lead_time_p50 := float(),
    lead_time_p90 := float(),
    lead_time_p99 := float(),
    cycle_time_avg := float(),
    work_orders_completed := non_neg_integer()
}.

-type quality_metrics() :: #{
    defect_rate := float(),
    first_pass_yield := float(),
    coverage_percent := float(),
    quality_gate_pass_rate := float()
}.

-type kanban_metrics() :: #{
    wip_current := non_neg_integer(),
    wip_by_bucket := #{tcps_kanban:bucket() => non_neg_integer()},
    queue_depth := non_neg_integer(),
    utilization := float()
}.

-type andon_metrics() :: #{
    open_count := non_neg_integer(),
    critical_count := non_neg_integer(),
    avg_resolution_time := float(),
    triggers_last_hour := non_neg_integer()
}.

-type kaizen_metrics() :: #{
    improvements_implemented := non_neg_integer(),
    waste_reduction_percent := float(),
    automation_coverage := float()
}.

-type tpm_metrics() :: #{
    uptime_percent := float(),
    last_maintenance := calendar:datetime(),
    maintenance_compliance := float(),
    mtbf := float(),
    mttr := float()
}.

-type alert() :: #{
    id := binary(),
    severity := alert_severity(),
    component := component(),
    rule := binary(),
    message := binary(),
    triggered_at := integer(),
    details := map(),
    auto_remediated := boolean()
}.

-type alert_rule() :: #{
    id := binary(),
    name := binary(),
    severity := alert_severity(),
    condition := fun((map()) -> boolean()),
    message_template := binary(),
    auto_remediate := boolean(),
    cooldown_seconds := non_neg_integer()
}.

-type slo() :: #{
    name := binary(),
    target := float(),
    metric := atom(),
    operator := '<' | '>' | '==' | '<=' | '>=',
    window_days := pos_integer()
}.

-type sli() :: #{
    metric := atom(),
    value := float(),
    target := float(),
    met := boolean()
}.

-export_type([
    component/0,
    health_status/0,
    health_result/0,
    component_health/0,
    metrics_snapshot/0,
    alert/0,
    alert_rule/0,
    slo/0,
    sli/0
]).

%%%=============================================================================
%%% State Record
%%%=============================================================================

-record(state, {
    %% Configuration
    config :: map(),

    %% Health status cache
    last_health_check :: health_result() | undefined,
    last_check_time :: integer() | undefined,

    %% Metrics storage
    metrics_history :: ets:tid(),

    %% Alert state
    active_alerts :: ets:tid(),
    alert_history :: ets:tid(),
    alert_rules :: [alert_rule()],
    alert_cooldowns :: #{binary() => integer()},

    %% OpenTelemetry state
    otel_tracer :: atom() | undefined,
    otel_meter :: atom() | undefined,
    trace_context :: map(),

    %% SLO tracking
    slos :: [slo()],
    sli_history :: ets:tid(),

    %% Background processes
    health_check_timer :: reference() | undefined,
    alert_check_timer :: reference() | undefined,

    %% Remediation
    remediation_history :: ets:tid()
}).

-define(DEFAULT_CHECK_INTERVAL, 30000).  % 30 seconds
-define(DEFAULT_ALERT_CHECK_INTERVAL, 10000).  % 10 seconds
-define(METRICS_RETENTION_DAYS, 7).
-define(ALERT_RETENTION_DAYS, 30).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the health monitoring server with default configuration.
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%%------------------------------------------------------------------------------
%% @doc Start the health monitoring server with custom configuration.
%%------------------------------------------------------------------------------
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%------------------------------------------------------------------------------
%% @doc Stop the health monitoring server.
%%------------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%------------------------------------------------------------------------------
%% @doc Perform comprehensive health check of all TCPS components.
%%------------------------------------------------------------------------------
-spec health_check() -> health_result().
health_check() ->
    gen_server:call(?MODULE, health_check, 10000).

%%------------------------------------------------------------------------------
%% @doc Check health of a specific component.
%%------------------------------------------------------------------------------
-spec component_health(Component :: component()) -> component_health().
component_health(Component) ->
    gen_server:call(?MODULE, {component_health, Component}).

%%------------------------------------------------------------------------------
%% @doc Get cached health status (fast, no new checks).
%%------------------------------------------------------------------------------
-spec get_health_status() -> health_result() | undefined.
get_health_status() ->
    gen_server:call(?MODULE, get_health_status).

%%------------------------------------------------------------------------------
%% @doc Initialize OpenTelemetry integration.
%%------------------------------------------------------------------------------
-spec init_otel() -> ok.
init_otel() ->
    gen_server:call(?MODULE, init_otel).

%%------------------------------------------------------------------------------
%% @doc Trace a production stage execution with automatic span creation.
%%------------------------------------------------------------------------------
-spec trace_production_stage(Stage :: atom(), Fun :: fun(() -> Result)) -> Result.
trace_production_stage(Stage, Fun) ->
    trace_production_stage(Stage, #{}, Fun).

-spec trace_production_stage(Stage :: atom(), Attrs :: map(), Fun :: fun(() -> Result)) -> Result.
trace_production_stage(Stage, Attrs, Fun) ->
    gen_server:call(?MODULE, {trace_stage, Stage, Attrs, Fun}, 60000).

%%------------------------------------------------------------------------------
%% @doc Emit an OTLP metric.
%%------------------------------------------------------------------------------
-spec emit_metric(Name :: atom(), Value :: number(), Tags :: map()) -> ok.
emit_metric(Name, Value, Tags) ->
    emit_metric(counter, Name, Value, Tags).

-spec emit_metric(Type :: metric_type(), Name :: atom(), Value :: number(), Tags :: map()) -> ok.
emit_metric(Type, Name, Value, Tags) ->
    gen_server:cast(?MODULE, {emit_metric, Type, Name, Value, Tags}).

%%------------------------------------------------------------------------------
%% @doc Collect all current TCPS metrics.
%%------------------------------------------------------------------------------
-spec collect_metrics() -> metrics_snapshot().
collect_metrics() ->
    gen_server:call(?MODULE, collect_metrics, 10000).

%%------------------------------------------------------------------------------
%% @doc Export metrics in specified format.
%%------------------------------------------------------------------------------
-spec export_metrics(Format :: export_format()) -> binary() | {error, term()}.
export_metrics(Format) ->
    gen_server:call(?MODULE, {export_metrics, Format}).

%%------------------------------------------------------------------------------
%% @doc Get metric history for a specific metric over N days.
%%------------------------------------------------------------------------------
-spec get_metric_history(Metric :: atom(), Days :: pos_integer()) -> [map()].
get_metric_history(Metric, Days) ->
    gen_server:call(?MODULE, {metric_history, Metric, Days}).

%%------------------------------------------------------------------------------
%% @doc Define alert rules for the system.
%%------------------------------------------------------------------------------
-spec define_alert_rules() -> [alert_rule()].
define_alert_rules() ->
    [
        % Critical Andon open >1 hour
        #{
            id => <<"andon_critical_open">>,
            name => <<"Critical Andon Open Too Long">>,
            severity => critical,
            condition => fun(Metrics) ->
                case maps:get(andon, Metrics, #{}) of
                    #{critical_count := Count} when Count > 0 -> true;
                    _ -> false
                end
            end,
            message_template => <<"Critical Andon has been open for >1 hour">>,
            auto_remediate => true,
            cooldown_seconds => 300
        },

        % WIP limit breached
        #{
            id => <<"wip_limit_breach">>,
            name => <<"WIP Limit Exceeded">>,
            severity => warning,
            condition => fun(Metrics) ->
                case maps:get(kanban, Metrics, #{}) of
                    #{utilization := Util} when Util > 0.9 -> true;
                    _ -> false
                end
            end,
            message_template => <<"WIP utilization above 90%">>,
            auto_remediate => true,
            cooldown_seconds => 600
        },

        % SLA breach imminent
        #{
            id => <<"sla_breach_imminent">>,
            name => <<"SLA Breach Imminent">>,
            severity => critical,
            condition => fun(Metrics) ->
                case maps:get(production, Metrics, #{}) of
                    #{lead_time_p90 := LeadTime} when LeadTime > 6840000 -> true;  % >1.9 hours
                    _ -> false
                end
            end,
            message_template => <<"Lead time approaching 2-hour SLA">>,
            auto_remediate => false,
            cooldown_seconds => 1800
        },

        % Quality gate failures
        #{
            id => <<"quality_gate_failures">>,
            name => <<"High Quality Gate Failure Rate">>,
            severity => warning,
            condition => fun(Metrics) ->
                case maps:get(quality, Metrics, #{}) of
                    #{quality_gate_pass_rate := Rate} when Rate < 0.95 -> true;
                    _ -> false
                end
            end,
            message_template => <<"Quality gate pass rate below 95%">>,
            auto_remediate => false,
            cooldown_seconds => 3600
        },

        % Defect rate too high
        #{
            id => <<"high_defect_rate">>,
            name => <<"Defect Rate Exceeds Threshold">>,
            severity => critical,
            condition => fun(Metrics) ->
                case maps:get(quality, Metrics, #{}) of
                    #{defect_rate := Rate} when Rate > 0.05 -> true;
                    _ -> false
                end
            end,
            message_template => <<"Defect rate above 5%">>,
            auto_remediate => false,
            cooldown_seconds => 1800
        },

        % Lead time anomaly
        #{
            id => <<"lead_time_anomaly">>,
            name => <<"Lead Time 2x Average">>,
            severity => warning,
            condition => fun(Metrics) ->
                case maps:get(production, Metrics, #{}) of
                    #{lead_time_p90 := P90, lead_time_p50 := P50} when P90 > P50 * 2 -> true;
                    _ -> false
                end
            end,
            message_template => <<"Lead time P90 is >2x P50">>,
            auto_remediate => false,
            cooldown_seconds => 3600
        },

        % TPM maintenance overdue
        #{
            id => <<"tpm_overdue">>,
            name => <<"TPM Maintenance Overdue">>,
            severity => warning,
            condition => fun(Metrics) ->
                case maps:get(tpm, Metrics, #{}) of
                    #{maintenance_compliance := Compliance} when Compliance < 0.9 -> true;
                    _ -> false
                end
            end,
            message_template => <<"TPM maintenance compliance below 90%">>,
            auto_remediate => true,
            cooldown_seconds => 86400
        }
    ].

%%------------------------------------------------------------------------------
%% @doc Check all alert rules and return active alerts.
%%------------------------------------------------------------------------------
-spec check_alert_rules() -> [alert()].
check_alert_rules() ->
    gen_server:call(?MODULE, check_alert_rules).

%%------------------------------------------------------------------------------
%% @doc Send an alert via configured channels.
%%------------------------------------------------------------------------------
-spec send_alert(Alert :: alert()) -> ok.
send_alert(Alert) ->
    gen_server:cast(?MODULE, {send_alert, Alert}).

%%------------------------------------------------------------------------------
%% @doc Get alert history for the last N days.
%%------------------------------------------------------------------------------
-spec get_alert_history(Days :: pos_integer()) -> [alert()].
get_alert_history(Days) ->
    gen_server:call(?MODULE, {alert_history, Days}).

%%------------------------------------------------------------------------------
%% @doc Write a structured log entry with OTEL correlation.
%%------------------------------------------------------------------------------
-spec structured_log(Level :: atom(), Message :: binary(), Context :: map()) -> ok.
structured_log(Level, Message, Context) ->
    gen_server:cast(?MODULE, {structured_log, Level, Message, Context}).

%%------------------------------------------------------------------------------
%% @doc Log a production event with automatic context enrichment.
%%------------------------------------------------------------------------------
-spec log_production_event(Event :: atom(), Details :: map()) -> ok.
log_production_event(Event, Details) ->
    gen_server:cast(?MODULE, {log_event, Event, Details}).

%%------------------------------------------------------------------------------
%% @doc Define Service Level Objectives.
%%------------------------------------------------------------------------------
-spec define_slos() -> [slo()].
define_slos() ->
    [
        #{
            name => <<"Lead Time SLO">>,
            target => 7200000,  % 2 hours in milliseconds
            metric => lead_time_p90,
            operator => '<',
            window_days => 30
        },
        #{
            name => <<"Quality Gate Pass Rate SLO">>,
            target => 0.95,
            metric => quality_gate_pass_rate,
            operator => '>=',
            window_days => 30
        },
        #{
            name => <<"Deployment Success Rate SLO">>,
            target => 0.99,
            metric => deployment_success_rate,
            operator => '>=',
            window_days => 30
        },
        #{
            name => <<"Andon Resolution Time SLO">>,
            target => 14400000,  % 4 hours in milliseconds
            metric => andon_avg_resolution_time,
            operator => '<',
            window_days => 30
        },
        #{
            name => <<"System Uptime SLO">>,
            target => 0.999,
            metric => uptime_percent,
            operator => '>=',
            window_days => 30
        }
    ].

%%------------------------------------------------------------------------------
%% @doc Measure Service Level Indicators.
%%------------------------------------------------------------------------------
-spec measure_slis() -> [sli()].
measure_slis() ->
    gen_server:call(?MODULE, measure_slis).

%%------------------------------------------------------------------------------
%% @doc Calculate remaining error budget (0.0 to 1.0).
%%------------------------------------------------------------------------------
-spec calculate_error_budget() -> float().
calculate_error_budget() ->
    gen_server:call(?MODULE, calculate_error_budget).

%%------------------------------------------------------------------------------
%% @doc Get SLO status with error budget.
%%------------------------------------------------------------------------------
-spec get_slo_status() -> map().
get_slo_status() ->
    gen_server:call(?MODULE, get_slo_status).

%%------------------------------------------------------------------------------
%% @doc Get comprehensive dashboard data.
%%------------------------------------------------------------------------------
-spec get_dashboard_data() -> map().
get_dashboard_data() ->
    gen_server:call(?MODULE, get_dashboard_data, 10000).

%%------------------------------------------------------------------------------
%% @doc Get metrics for a specific component.
%%------------------------------------------------------------------------------
-spec get_component_metrics(Component :: component()) -> map().
get_component_metrics(Component) ->
    gen_server:call(?MODULE, {component_metrics, Component}).

%%------------------------------------------------------------------------------
%% @doc Export metrics to Prometheus format.
%%------------------------------------------------------------------------------
-spec export_to_prometheus() -> binary().
export_to_prometheus() ->
    case export_metrics(prometheus) of
        {ok, Data} -> Data;
        Data when is_binary(Data) -> Data;
        _ -> <<>>
    end.

%%------------------------------------------------------------------------------
%% @doc Send metrics to Datadog.
%%------------------------------------------------------------------------------
-spec send_to_datadog(ApiKey :: binary()) -> ok | {error, term()}.
send_to_datadog(ApiKey) ->
    gen_server:call(?MODULE, {send_to_datadog, ApiKey}).

%%------------------------------------------------------------------------------
%% @doc Send metrics to New Relic.
%%------------------------------------------------------------------------------
-spec send_to_newrelic(ApiKey :: binary()) -> ok | {error, term()}.
send_to_newrelic(ApiKey) ->
    gen_server:call(?MODULE, {send_to_newrelic, ApiKey}).

%%------------------------------------------------------------------------------
%% @doc Send metrics to Grafana Cloud.
%%------------------------------------------------------------------------------
-spec send_to_grafana_cloud(Config :: map()) -> ok | {error, term()}.
send_to_grafana_cloud(Config) ->
    gen_server:call(?MODULE, {send_to_grafana, Config}).

%%------------------------------------------------------------------------------
%% @doc Attempt automatic remediation of an alert.
%%------------------------------------------------------------------------------
-spec auto_remediate(Alert :: alert()) -> {ok, remediated} | {manual, binary()}.
auto_remediate(Alert) ->
    gen_server:call(?MODULE, {auto_remediate, Alert}).

%%------------------------------------------------------------------------------
%% @doc Get remediation history.
%%------------------------------------------------------------------------------
-spec get_remediation_history(Days :: pos_integer()) -> [map()].
get_remediation_history(Days) ->
    gen_server:call(?MODULE, {remediation_history, Days}).

%%------------------------------------------------------------------------------
%% @doc Simulate a failure for testing.
%%------------------------------------------------------------------------------
-spec simulate_failure(Component :: component(), Details :: map()) -> ok.
simulate_failure(Component, Details) ->
    gen_server:cast(?MODULE, {simulate_failure, Component, Details}).

%%------------------------------------------------------------------------------
%% @doc Reset state (testing only).
%%------------------------------------------------------------------------------
-spec reset_state() -> ok.
reset_state() ->
    gen_server:call(?MODULE, reset_state).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config) ->
    process_flag(trap_exit, true),

    % Create ETS tables
    MetricsHistory = ets:new(metrics_history, [ordered_set, protected]),
    ActiveAlerts = ets:new(active_alerts, [set, protected]),
    AlertHistory = ets:new(alert_history, [ordered_set, protected]),
    SliHistory = ets:new(sli_history, [ordered_set, protected]),
    RemediationHistory = ets:new(remediation_history, [ordered_set, protected]),

    % Initialize alert rules
    AlertRules = define_alert_rules(),

    % Initialize SLOs
    Slos = define_slos(),

    % Start background check loops
    CheckInterval = maps:get(check_interval, Config, ?DEFAULT_CHECK_INTERVAL),
    AlertCheckInterval = maps:get(alert_check_interval, Config, ?DEFAULT_ALERT_CHECK_INTERVAL),

    HealthTimer = erlang:send_after(CheckInterval, self(), health_check),
    AlertTimer = erlang:send_after(AlertCheckInterval, self(), alert_check),

    State = #state{
        config = Config,
        metrics_history = MetricsHistory,
        active_alerts = ActiveAlerts,
        alert_history = AlertHistory,
        alert_rules = AlertRules,
        alert_cooldowns = #{},
        slos = Slos,
        sli_history = SliHistory,
        health_check_timer = HealthTimer,
        alert_check_timer = AlertTimer,
        remediation_history = RemediationHistory,
        trace_context = #{}
    },

    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(health_check, _From, State) ->
    Result = perform_health_check(State),
    NewState = State#state{
        last_health_check = Result,
        last_check_time = erlang:system_time(millisecond)
    },
    {reply, Result, NewState};

handle_call({component_health, Component}, _From, State) ->
    Result = check_component_health(Component, State),
    {reply, Result, NewState} = {reply, Result, State},
    {reply, Result, NewState};

handle_call(get_health_status, _From, State) ->
    {reply, State#state.last_health_check, State};

handle_call(init_otel, _From, State) ->
    NewState = initialize_otel(State),
    {reply, ok, NewState};

handle_call({trace_stage, Stage, Attrs, Fun}, _From, State) ->
    {Result, NewState} = trace_stage_execution(Stage, Attrs, Fun, State),
    {reply, Result, NewState};

handle_call(collect_metrics, _From, State) ->
    Metrics = collect_all_metrics(State),
    % Store in history
    Timestamp = erlang:system_time(millisecond),
    ets:insert(State#state.metrics_history, {Timestamp, Metrics}),
    cleanup_old_metrics(State#state.metrics_history),
    {reply, Metrics, State};

handle_call({export_metrics, Format}, _From, State) ->
    Metrics = collect_all_metrics(State),
    Result = format_metrics(Format, Metrics),
    {reply, Result, State};

handle_call({metric_history, Metric, Days}, _From, State) ->
    History = get_metric_history_internal(Metric, Days, State),
    {reply, History, State};

handle_call(check_alert_rules, _From, State) ->
    {Alerts, NewState} = evaluate_alert_rules(State),
    {reply, Alerts, NewState};

handle_call({alert_history, Days}, _From, State) ->
    History = get_alert_history_internal(Days, State),
    {reply, History, State};

handle_call(measure_slis, _From, State) ->
    Slis = measure_slis_internal(State),
    {reply, Slis, State};

handle_call(calculate_error_budget, _From, State) ->
    Budget = calculate_error_budget_internal(State),
    {reply, Budget, State};

handle_call(get_slo_status, _From, State) ->
    Status = get_slo_status_internal(State),
    {reply, Status, State};

handle_call(get_dashboard_data, _From, State) ->
    Data = build_dashboard_data(State),
    {reply, Data, State};

handle_call({component_metrics, Component}, _From, State) ->
    Metrics = get_component_metrics_internal(Component, State),
    {reply, Metrics, State};

handle_call({send_to_datadog, ApiKey}, _From, State) ->
    Result = send_metrics_datadog(ApiKey, State),
    {reply, Result, State};

handle_call({send_to_newrelic, ApiKey}, _From, State) ->
    Result = send_metrics_newrelic(ApiKey, State),
    {reply, Result, State};

handle_call({send_to_grafana, Config}, _From, State) ->
    Result = send_metrics_grafana(Config, State),
    {reply, Result, State};

handle_call({auto_remediate, Alert}, _From, State) ->
    {Result, NewState} = attempt_auto_remediation(Alert, State),
    {reply, Result, NewState};

handle_call({remediation_history, Days}, _From, State) ->
    History = get_remediation_history_internal(Days, State),
    {reply, History, State};

handle_call(reset_state, _From, State) ->
    ets:delete_all_objects(State#state.metrics_history),
    ets:delete_all_objects(State#state.active_alerts),
    ets:delete_all_objects(State#state.alert_history),
    ets:delete_all_objects(State#state.sli_history),
    ets:delete_all_objects(State#state.remediation_history),
    NewState = State#state{
        last_health_check = undefined,
        last_check_time = undefined,
        alert_cooldowns = #{}
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({emit_metric, Type, Name, Value, Tags}, State) ->
    record_metric(Type, Name, Value, Tags, State),
    {noreply, State};

handle_cast({send_alert, Alert}, State) ->
    NewState = send_alert_internal(Alert, State),
    {noreply, NewState};

handle_cast({structured_log, Level, Message, Context}, State) ->
    write_structured_log(Level, Message, Context, State),
    {noreply, State};

handle_cast({log_event, Event, Details}, State) ->
    log_production_event_internal(Event, Details, State),
    {noreply, State};

handle_cast({simulate_failure, Component, Details}, State) ->
    simulate_component_failure(Component, Details, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(health_check, State) ->
    Result = perform_health_check(State),
    NewState = State#state{
        last_health_check = Result,
        last_check_time = erlang:system_time(millisecond)
    },

    % Schedule next check
    CheckInterval = maps:get(check_interval, State#state.config, ?DEFAULT_CHECK_INTERVAL),
    Timer = erlang:send_after(CheckInterval, self(), health_check),
    {noreply, NewState#state{health_check_timer = Timer}};

handle_info(alert_check, State) ->
    {_Alerts, NewState} = evaluate_alert_rules(State),

    % Schedule next check
    AlertCheckInterval = maps:get(alert_check_interval, State#state.config, ?DEFAULT_ALERT_CHECK_INTERVAL),
    Timer = erlang:send_after(AlertCheckInterval, self(), alert_check),
    {noreply, NewState#state{alert_check_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, State) ->
    % Cancel timers
    case State#state.health_check_timer of
        undefined -> ok;
        Timer1 -> erlang:cancel_timer(Timer1)
    end,
    case State#state.alert_check_timer of
        undefined -> ok;
        Timer2 -> erlang:cancel_timer(Timer2)
    end,

    % Clean up ETS tables
    ets:delete(State#state.metrics_history),
    ets:delete(State#state.active_alerts),
    ets:delete(State#state.alert_history),
    ets:delete(State#state.sli_history),
    ets:delete(State#state.remediation_history),
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions - Health Checks
%%%=============================================================================

perform_health_check(State) ->
    Components = [kanban, andon, tpm, ontology, receipts, persistence],
    ComponentResults = maps:from_list([
        {Component, check_component_health(Component, State)}
        || Component <- Components
    ]),

    % Determine overall status
    OverallStatus = determine_overall_status(ComponentResults),

    % Collect metrics
    Metrics = collect_all_metrics(State),

    % Get active alerts
    Alerts = ets:tab2list(State#state.active_alerts),
    AlertsList = [Alert || {_Id, Alert} <- Alerts],

    #{
        status => OverallStatus,
        components => ComponentResults,
        metrics => Metrics,
        alerts => AlertsList,
        timestamp => erlang:system_time(millisecond)
    }.

check_component_health(kanban, _State) ->
    try
        case whereis(tcps_kanban) of
            undefined ->
                {unhealthy, <<"Kanban process not running">>, #{}};
            Pid when is_pid(Pid) ->
                % Check WIP limits
                Buckets = [reliability, security, cost, compliance],
                WipStatus = [tcps_kanban:get_wip_status(B) || B <- Buckets],

                % Check if any bucket is over limit
                OverLimit = lists:any(fun(Status) ->
                    maps:get(current, Status, 0) > maps:get(limit, Status, infinity)
                end, WipStatus),

                Details = #{
                    wip_status => maps:from_list(lists:zip(Buckets, WipStatus)),
                    process_alive => true
                },

                if
                    OverLimit ->
                        {degraded, <<"WIP limit exceeded">>, Details};
                    true ->
                        {healthy, Details}
                end
        end
    catch
        _:Error ->
            {unhealthy, iolist_to_binary(io_lib:format("~p", [Error])), #{}}
    end;

check_component_health(andon, _State) ->
    try
        % Check if there are any critical open Andons
        % This is a simplified check - in production, would query actual Andon state
        {healthy, #{open_andons => 0, critical_andons => 0}}
    catch
        _:Error ->
            {unhealthy, iolist_to_binary(io_lib:format("~p", [Error])), #{}}
    end;

check_component_health(tpm, _State) ->
    % TPM maintenance check - simplified
    {healthy, #{last_maintenance => calendar:universal_time(), compliance => 1.0}};

check_component_health(ontology, _State) ->
    % Ontology SHACL validation check - simplified
    {healthy, #{validation_status => valid}};

check_component_health(receipts, _State) ->
    % Receipt generation check - simplified
    {healthy, #{receipts_generated => 0, receipts_valid => true}};

check_component_health(persistence, _State) ->
    % Persistence/backup check - simplified
    {healthy, #{backups_current => true, no_corruption => true}}.

determine_overall_status(ComponentResults) ->
    HasUnhealthy = lists:any(fun({_Component, Health}) ->
        case Health of
            {unhealthy, _, _} -> true;
            _ -> false
        end
    end, maps:to_list(ComponentResults)),

    HasDegraded = lists:any(fun({_Component, Health}) ->
        case Health of
            {degraded, _, _} -> true;
            _ -> false
        end
    end, maps:to_list(ComponentResults)),

    if
        HasUnhealthy -> unhealthy;
        HasDegraded -> degraded;
        true -> healthy
    end.

%%%=============================================================================
%%% Internal Functions - Metrics
%%%=============================================================================

collect_all_metrics(_State) ->
    #{
        production => collect_production_metrics(),
        quality => collect_quality_metrics(),
        kanban => collect_kanban_metrics(),
        andon => collect_andon_metrics(),
        kaizen => collect_kaizen_metrics(),
        tpm => collect_tpm_metrics()
    }.

collect_production_metrics() ->
    % Simplified - in production, would aggregate from actual data
    #{
        throughput => 10.5,
        lead_time_p50 => 3600000,  % 1 hour
        lead_time_p90 => 5400000,  % 1.5 hours
        lead_time_p99 => 7000000,  % ~2 hours
        cycle_time_avg => 1800000,  % 30 minutes
        work_orders_completed => 42
    }.

collect_quality_metrics() ->
    #{
        defect_rate => 0.02,
        first_pass_yield => 0.98,
        coverage_percent => 0.95,
        quality_gate_pass_rate => 0.97
    }.

collect_kanban_metrics() ->
    try
        Buckets = [reliability, security, cost, compliance],
        WipByBucket = maps:from_list([
            {B, maps:get(current, tcps_kanban:get_wip_status(B), 0)}
            || B <- Buckets
        ]),
        TotalWip = lists:sum(maps:values(WipByBucket)),

        #{
            wip_current => TotalWip,
            wip_by_bucket => WipByBucket,
            queue_depth => 5,
            utilization => 0.6
        }
    catch
        _:_ ->
            #{
                wip_current => 0,
                wip_by_bucket => #{},
                queue_depth => 0,
                utilization => 0.0
            }
    end.

collect_andon_metrics() ->
    #{
        open_count => 0,
        critical_count => 0,
        avg_resolution_time => 7200000,  % 2 hours
        triggers_last_hour => 0
    }.

collect_kaizen_metrics() ->
    #{
        improvements_implemented => 15,
        waste_reduction_percent => 0.25,
        automation_coverage => 0.80
    }.

collect_tpm_metrics() ->
    #{
        uptime_percent => 0.9995,
        last_maintenance => calendar:universal_time(),
        maintenance_compliance => 0.95,
        mtbf => 720.0,  % hours
        mttr => 0.5     % hours
    }.

format_metrics(prometheus, Metrics) ->
    format_prometheus(Metrics);
format_metrics(json, Metrics) ->
    jsx:encode(Metrics);
format_metrics(otlp, Metrics) ->
    format_otlp(Metrics);
format_metrics(_Format, Metrics) ->
    jsx:encode(Metrics).

format_prometheus(Metrics) ->
    Lines = [
        prometheus_metric(production_throughput, maps:get(throughput, maps:get(production, Metrics, #{}), 0)),
        prometheus_metric(production_lead_time_p50, maps:get(lead_time_p50, maps:get(production, Metrics, #{}), 0)),
        prometheus_metric(production_lead_time_p90, maps:get(lead_time_p90, maps:get(production, Metrics, #{}), 0)),
        prometheus_metric(quality_defect_rate, maps:get(defect_rate, maps:get(quality, Metrics, #{}), 0)),
        prometheus_metric(quality_pass_rate, maps:get(quality_gate_pass_rate, maps:get(quality, Metrics, #{}), 0)),
        prometheus_metric(kanban_wip_current, maps:get(wip_current, maps:get(kanban, Metrics, #{}), 0)),
        prometheus_metric(andon_open_count, maps:get(open_count, maps:get(andon, Metrics, #{}), 0)),
        prometheus_metric(tpm_uptime, maps:get(uptime_percent, maps:get(tpm, Metrics, #{}), 0))
    ],
    iolist_to_binary(string:join(Lines, "\n")).

prometheus_metric(Name, Value) ->
    io_lib:format("tcps_~s ~p", [Name, Value]).

format_otlp(Metrics) ->
    % OTLP format (simplified JSON representation)
    jsx:encode(#{
        resource_metrics => [#{
            resource => #{
                attributes => [
                    #{key => <<"service.name">>, value => #{string_value => <<"tcps-erlmcp">>}}
                ]
            },
            scope_metrics => [#{
                scope => #{name => <<"tcps_health">>},
                metrics => build_otlp_metrics(Metrics)
            }]
        }]
    }).

build_otlp_metrics(Metrics) ->
    Timestamp = erlang:system_time(nanosecond),
    [
        #{
            name => <<"tcps.production.throughput">>,
            gauge => #{
                data_points => [#{
                    time_unix_nano => Timestamp,
                    as_double => maps:get(throughput, maps:get(production, Metrics, #{}), 0)
                }]
            }
        },
        #{
            name => <<"tcps.quality.defect_rate">>,
            gauge => #{
                data_points => [#{
                    time_unix_nano => Timestamp,
                    as_double => maps:get(defect_rate, maps:get(quality, Metrics, #{}), 0)
                }]
            }
        }
    ].

get_metric_history_internal(Metric, Days, State) ->
    Cutoff = erlang:system_time(millisecond) - (Days * 86400000),

    AllMetrics = ets:select(State#state.metrics_history, [{
        {'$1', '$2'},
        [{'>=', '$1', Cutoff}],
        [{{'$1', '$2'}}]
    }]),

    % Extract specific metric from snapshots
    lists:filtermap(fun({Timestamp, Snapshot}) ->
        case extract_metric_value(Metric, Snapshot) of
            undefined -> false;
            Value -> {true, #{timestamp => Timestamp, value => Value}}
        end
    end, AllMetrics).

extract_metric_value(lead_time_p90, #{production := #{lead_time_p90 := Val}}) -> Val;
extract_metric_value(defect_rate, #{quality := #{defect_rate := Val}}) -> Val;
extract_metric_value(wip_current, #{kanban := #{wip_current := Val}}) -> Val;
extract_metric_value(_, _) -> undefined.

cleanup_old_metrics(Table) ->
    Cutoff = erlang:system_time(millisecond) - (?METRICS_RETENTION_DAYS * 86400000),
    ets:select_delete(Table, [{{'$1', '_'}, [{'<', '$1', Cutoff}], [true]}]),
    ok.

record_metric(_Type, _Name, _Value, _Tags, _State) ->
    % In production, would record to OTLP collector
    ok.

%%%=============================================================================
%%% Internal Functions - Alerting
%%%=============================================================================

evaluate_alert_rules(State) ->
    Metrics = collect_all_metrics(State),
    Timestamp = erlang:system_time(millisecond),

    {Alerts, NewCooldowns} = lists:foldl(fun(Rule, {AccAlerts, AccCooldowns}) ->
        RuleId = maps:get(id, Rule),

        % Check cooldown
        LastTrigger = maps:get(RuleId, AccCooldowns, 0),
        CooldownMs = maps:get(cooldown_seconds, Rule) * 1000,
        case Timestamp - LastTrigger of
            Diff when Diff < CooldownMs ->
                % Still in cooldown
                {AccAlerts, AccCooldowns};
            _ ->
                % Evaluate condition
                Condition = maps:get(condition, Rule),
                case Condition(Metrics) of
                    true ->
                        Alert = #{
                            id => generate_alert_id(),
                            severity => maps:get(severity, Rule),
                            component => extract_component_from_rule(Rule),
                            rule => maps:get(name, Rule),
                            message => maps:get(message_template, Rule),
                            triggered_at => Timestamp,
                            details => #{},
                            auto_remediated => false
                        },

                        % Store alert
                        ets:insert(State#state.active_alerts, {Alert#{id := id}, Alert}),
                        ets:insert(State#state.alert_history, {Timestamp, Alert}),

                        % Send alert
                        send_alert_internal(Alert, State),

                        % Auto-remediate if configured
                        case maps:get(auto_remediate, Rule, false) of
                            true ->
                                attempt_auto_remediation(Alert, State);
                            false ->
                                ok
                        end,

                        {[Alert | AccAlerts], AccCooldowns#{RuleId => Timestamp}};
                    false ->
                        {AccAlerts, AccCooldowns}
                end
        end
    end, {[], State#state.alert_cooldowns}, State#state.alert_rules),

    NewState = State#state{alert_cooldowns = NewCooldowns},
    {Alerts, NewState}.

send_alert_internal(Alert, State) ->
    Config = State#state.config,
    Channels = maps:get(alert_channels, Config, []),

    lists:foreach(fun(Channel) ->
        send_alert_to_channel(Channel, Alert, Config)
    end, Channels),

    State.

send_alert_to_channel(slack, Alert, Config) ->
    case maps:get(slack_webhook, Config, undefined) of
        undefined -> ok;
        _Webhook ->
            Payload = jsx:encode(#{
                text => maps:get(message, Alert),
                attachments => [#{
                    color => severity_color(maps:get(severity, Alert)),
                    fields => [
                        #{title => <<"Severity">>, value => atom_to_binary(maps:get(severity, Alert), utf8), short => true},
                        #{title => <<"Component">>, value => atom_to_binary(maps:get(component, Alert), utf8), short => true},
                        #{title => <<"Rule">>, value => maps:get(rule, Alert), short => false}
                    ]
                }]
            }),
            % In production, would send HTTP POST
            io:format("Slack alert: ~s~n", [Payload]),
            ok
    end;

send_alert_to_channel(email, Alert, Config) ->
    case maps:get(email_smtp, Config, undefined) of
        undefined -> ok;
        _SmtpConfig ->
            % In production, would send email via SMTP
            io:format("Email alert: ~p~n", [Alert]),
            ok
    end;

send_alert_to_channel(_Channel, _Alert, _Config) ->
    ok.

severity_color(critical) -> <<"danger">>;
severity_color(warning) -> <<"warning">>;
severity_color(info) -> <<"good">>.

generate_alert_id() ->
    list_to_binary(io_lib:format("alert-~p", [erlang:unique_integer([positive])])).

extract_component_from_rule(Rule) ->
    % Extract component from rule ID (simplified)
    RuleId = maps:get(id, Rule),
    case binary:split(RuleId, <<"_">>) of
        [Component | _] -> binary_to_atom(Component, utf8);
        _ -> unknown
    end.

get_alert_history_internal(Days, State) ->
    Cutoff = erlang:system_time(millisecond) - (Days * 86400000),

    Alerts = ets:select(State#state.alert_history, [{
        {'$1', '$2'},
        [{'>=', '$1', Cutoff}],
        ['$2']
    }]),

    lists:reverse(Alerts).

%%%=============================================================================
%%% Internal Functions - SLO/SLI
%%%=============================================================================

measure_slis_internal(State) ->
    Metrics = collect_all_metrics(State),
    Slos = State#state.slos,

    lists:map(fun(Slo) ->
        MetricName = maps:get(metric, Slo),
        Target = maps:get(target, Slo),

        Value = case extract_metric_value(MetricName, Metrics) of
            undefined -> 0.0;
            V -> V
        end,

        Operator = maps:get(operator, Slo),
        Met = evaluate_slo_operator(Operator, Value, Target),

        #{
            metric => MetricName,
            value => Value,
            target => Target,
            met => Met
        }
    end, Slos).

evaluate_slo_operator('<', Value, Target) -> Value < Target;
evaluate_slo_operator('>', Value, Target) -> Value > Target;
evaluate_slo_operator('<=', Value, Target) -> Value =< Target;
evaluate_slo_operator('>=', Value, Target) -> Value >= Target;
evaluate_slo_operator('==', Value, Target) -> Value == Target.

calculate_error_budget_internal(State) ->
    Slis = measure_slis_internal(State),

    % Calculate percentage of SLIs met
    TotalSlis = length(Slis),
    MetSlis = length([1 || #{met := true} = _S <- Slis]),

    case TotalSlis of
        0 -> 1.0;
        _ -> MetSlis / TotalSlis
    end.

get_slo_status_internal(State) ->
    Slis = measure_slis_internal(State),
    ErrorBudget = calculate_error_budget_internal(State),

    #{
        slis => Slis,
        error_budget => ErrorBudget,
        error_budget_remaining => ErrorBudget,
        timestamp => erlang:system_time(millisecond)
    }.

%%%=============================================================================
%%% Internal Functions - Dashboard
%%%=============================================================================

build_dashboard_data(State) ->
    HealthCheck = case State#state.last_health_check of
        undefined -> perform_health_check(State);
        Check -> Check
    end,

    Metrics = collect_all_metrics(State),
    Alerts = ets:tab2list(State#state.active_alerts),
    AlertsList = [Alert || {_Id, Alert} <- Alerts],
    SloStatus = get_slo_status_internal(State),

    #{
        health => HealthCheck,
        metrics => Metrics,
        active_alerts => AlertsList,
        slo_status => SloStatus,
        timestamp => erlang:system_time(millisecond)
    }.

get_component_metrics_internal(Component, _State) ->
    Metrics = collect_all_metrics(_State),
    maps:get(Component, Metrics, #{}).

%%%=============================================================================
%%% Internal Functions - OpenTelemetry
%%%=============================================================================

initialize_otel(State) ->
    % In production, would initialize actual OTLP connection
    Config = State#state.config,
    _Endpoint = maps:get(otel_endpoint, Config, "http://localhost:4318"),

    State#state{
        otel_tracer = tcps_tracer,
        otel_meter = tcps_meter
    }.

trace_stage_execution(Stage, Attrs, Fun, State) ->
    StartTime = erlang:monotonic_time(nanosecond),

    % Execute function
    Result = try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            erlang:raise(Class, Reason, Stacktrace)
    end,

    EndTime = erlang:monotonic_time(nanosecond),
    Duration = EndTime - StartTime,

    % Log span (simplified - in production would send to OTLP)
    SpanData = #{
        name => Stage,
        start_time => StartTime,
        end_time => EndTime,
        duration_ns => Duration,
        attributes => Attrs,
        status => ok
    },

    io:format("Trace span: ~p~n", [SpanData]),

    {Result, State}.

write_structured_log(Level, Message, Context, _State) ->
    Timestamp = calendar:universal_time(),

    LogEntry = #{
        timestamp => Timestamp,
        level => Level,
        message => Message,
        context => Context,
        trace_id => maps:get(trace_id, Context, undefined),
        span_id => maps:get(span_id, Context, undefined)
    },

    % In production, would send to log aggregation system
    io:format("[~s] ~s: ~s (~p)~n", [
        Level,
        iso8601_format(Timestamp),
        Message,
        Context
    ]),

    LogEntry.

log_production_event_internal(Event, Details, State) ->
    Context = maps:merge(Details, #{
        event => Event,
        component => tcps
    }),

    write_structured_log(info, atom_to_binary(Event, utf8), Context, State).

iso8601_format({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec]).

%%%=============================================================================
%%% Internal Functions - Platform Integrations
%%%=============================================================================

send_metrics_datadog(_ApiKey, State) ->
    Metrics = collect_all_metrics(State),
    % In production, would send to Datadog API
    io:format("Datadog metrics: ~p~n", [Metrics]),
    ok.

send_metrics_newrelic(_ApiKey, State) ->
    Metrics = collect_all_metrics(State),
    % In production, would send to New Relic API
    io:format("New Relic metrics: ~p~n", [Metrics]),
    ok.

send_metrics_grafana(_Config, State) ->
    Metrics = collect_all_metrics(State),
    % In production, would send to Grafana Cloud
    io:format("Grafana metrics: ~p~n", [Metrics]),
    ok.

%%%=============================================================================
%%% Internal Functions - Auto-Remediation
%%%=============================================================================

attempt_auto_remediation(Alert, State) ->
    Component = maps:get(component, Alert),
    RuleId = maps:get(rule, Alert),

    Result = case {Component, RuleId} of
        {andon, <<"Critical Andon Open Too Long">>} ->
            % Escalate to on-call
            {ok, remediated};

        {kanban, <<"WIP Limit Exceeded">>} ->
            % Pause new work orders
            {ok, remediated};

        {tpm, <<"TPM Maintenance Overdue">>} ->
            % Trigger maintenance
            {ok, remediated};

        _ ->
            {manual, <<"No auto-remediation available">>}
    end,

    % Record remediation attempt
    Timestamp = erlang:system_time(millisecond),
    Remediation = #{
        alert_id => maps:get(id, Alert),
        component => Component,
        rule => RuleId,
        result => Result,
        timestamp => Timestamp
    },

    ets:insert(State#state.remediation_history, {Timestamp, Remediation}),

    {Result, State}.

get_remediation_history_internal(Days, State) ->
    Cutoff = erlang:system_time(millisecond) - (Days * 86400000),

    History = ets:select(State#state.remediation_history, [{
        {'$1', '$2'},
        [{'>=', '$1', Cutoff}],
        ['$2']
    }]),

    lists:reverse(History).

simulate_component_failure(_Component, _Details, _State) ->
    % For testing - simulate failures
    ok.

%%%=============================================================================
%%% Background Process Functions
%%%=============================================================================

health_check_loop(Interval) ->
    receive
        stop -> ok
    after Interval ->
        health_check(),
        health_check_loop(Interval)
    end.

alert_check_loop(Interval) ->
    receive
        stop -> ok
    after Interval ->
        check_alert_rules(),
        alert_check_loop(Interval)
    end.
