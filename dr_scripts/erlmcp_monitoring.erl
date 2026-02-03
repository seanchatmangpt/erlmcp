%% @private
%% @doc Monitoring System for erlmcp v3 DR Solution
%% Provides comprehensive monitoring for multi-site resilience
-module(erlmcp_monitoring).

-behaviour(gen_server).

%% API
-export([start_link/0, get_metrics/1, add_alert_rule/3, get_alert_history/1,
         check_service_health/1, validate_slas/0, generate_dashboard/0]).
-export([register_metric/2, get_thresholds/1, set_thresholds/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_types.hrl").

%% Records for monitoring data structures
-record(metric_definition, {
    name :: string(),
    type :: counter | gauge | histogram | summary,
    description :: string(),
    unit :: string(),
    labels :: [label_name()],
    aggregation :: sum | avg | max | min
}).

-record(metric_value, {
    name :: string(),
    value :: number(),
    timestamp :: integer(),
    labels :: map()
}).

-record(alert_rule, {
    id :: alert_id(),
    metric :: metric_name(),
    condition :: condition(),
    threshold :: number(),
    duration :: pos_integer(),  % milliseconds
    severity :: critical | high | medium | low,
    message :: string(),
    actions :: [alert_action()],
    enabled :: boolean()
}).

-record(alert_history, {
    id :: alert_id(),
    metric :: metric_name(),
    value :: number(),
    threshold :: number(),
    severity :: severity(),
    timestamp :: integer(),
    acknowledged :: boolean(),
    resolved :: boolean(),
    actions_taken :: [string()],
    duration :: pos_integer()
}).

-record(sla_target, {
    service :: service_id(),
    metric :: metric_name(),
    target :: number(),
    timeframe :: time_frame(),  % minute | hour | day | month
    weight :: number(),  % 0.0 to 1.0
    criticality :: mission_critical | essential | important | standard
}).

-record(health_status, {
    service :: service_id(),
    status :: healthy | degraded | critical,
    metrics :: map(),
    issues :: [issue()],
    last_check :: integer(),
    trend :: improving | stable | deteriorating
}).

-record(issue, {
    id :: issue_id(),
    description :: string(),
    severity :: severity(),
    timestamp :: integer(),
    resolved :: boolean(),
    resolution :: string()
}).

-record(state, {
    metrics :: map(),  % metric_name() => metric_definition()
    values :: map(),   % metric_name() => [metric_value()]
    alerts :: map(),   % alert_id() => alert_rule()
    history :: [alert_history()],
    thresholds :: map(),  % metric_name() => thresholds()
    sla_targets :: [sla_target()],
    health_status :: map(),  % service_id() => health_status()
    dashboards :: map(),
    aggregation :: pid(),  % Metric aggregation process
    alerting :: pid(),    % Alerting process
    storage :: pid()      % Metric storage process
}).

-type condition() :: greater_than | less_than | equal_to | not_equal_to.
-type severity() :: critical | high | medium | low.
-type time_frame() :: minute | hour | day | month.
-type metric_name() :: string().
-type label_name() :: string().
-type alert_action() :: notify | escalate | auto_resolve | execute_command.
-type issue_id() :: string().

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_metrics(metric_name()) -> {ok, [metric_value()]} | {error, not_found}.
get_metrics(MetricName) ->
    gen_server:call(?MODULE, {get_metrics, MetricName}).

-spec add_alert_rule(alert_id(), metric_name(), condition()) -> ok | {error, term()}.
add_alert_rule(AlertId, MetricName, Condition) ->
    gen_server:call(?MODULE, {add_alert_rule, AlertId, MetricName, Condition}).

-spec get_alert_history(alert_id()) -> [alert_history()].
get_alert_history(AlertId) ->
    gen_server:call(?MODULE, {get_alert_history, AlertId}).

-spec check_service_health(service_id()) -> health_status().
check_service_health(ServiceId) ->
    gen_server:call(?MODULE, {check_service_health, ServiceId}).

-spec validate_slas() -> sla_validation_result().
validate_slas() ->
    gen_server:call(?MODULE, validate_slas).

-spec generate_dashboard() -> dashboard_data().
generate_dashboard() ->
    gen_server:call(?MODULE, generate_dashboard).

-spec register_metric(metric_name(), metric_definition()) -> ok.
register_metric(MetricName, Definition) ->
    gen_server:call(?MODULE, {register_metric, MetricName, Definition}).

-spec get_thresholds(metric_name()) -> {ok, thresholds()} | {error, not_found}.
get_thresholds(MetricName) ->
    gen_server:call(?MODULE, {get_thresholds, MetricName}).

-spec set_thresholds(metric_name(), critical(), warning()) -> ok.
set_thresholds(MetricName, Critical, Warning) ->
    gen_server:call(?MODULE, {set_thresholds, MetricName, Critical, Warning}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Initialize monitoring state
    State = #state{
        metrics = initialize_metrics(),
        values = initialize_values(),
        alerts = initialize_alerts(),
        history = [],
        thresholds = initialize_thresholds(),
        sla_targets = initialize_sla_targets(),
        health_status = initialize_health_status(),
        dashboards = initialize_dashboards(),
        aggregation = start_aggregation(),
        alerting = start_alerting(),
        storage = start_storage()
    },

    % Start periodic checks
    erlang:send_after(5000, self(), periodic_health_check),
    erlang:send_after(30000, self(), periodic_metrics_collection),
    erlang:send_after(60000, self(), periodic_sla_validation),

    % Initialize metrics
    erlmcp_metrics:register(monitoring_metrics),

    {ok, State}.

handle_call({get_metrics, MetricName}, _From, State) ->
    case maps:get(MetricName, State#state.values, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Values ->
            % Filter by time range if needed
            FilteredValues = filter_time_range(Values, last_hour),
            {reply, {ok, FilteredValues}, State}
    end;

handle_call({add_alert_rule, AlertId, MetricName, Condition}, _From, State) ->
    % Create alert rule
    AlertRule = #alert_rule{
        id = AlertId,
        metric = MetricName,
        condition = Condition,
        threshold = get_default_threshold(MetricName, Condition),
        duration = 300000,  % 5 minutes
        severity = medium,
        message = generate_alert_message(AlertId, MetricName, Condition),
        actions = [notify],
        enabled = true
    },

    % Validate rule
    case validate_alert_rule(AlertRule, State) of
        ok ->
            UpdatedAlerts = maps:put(AlertId, AlertRule, State#state.alerts),
            {reply, ok, State#state{alerts = UpdatedAlerts}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_alert_history, AlertId}, _From, State) ->
    FilteredHistory = lists:filter(fun(Record) ->
        Record#alert_history.id =:= AlertId
    end, State#state.history),
    {reply, FilteredHistory, State};

handle_call({check_service_health, ServiceId}, _From, State) ->
    HealthStatus = check_and_update_service_health(ServiceId, State),
    {reply, HealthStatus, State};

handle_call(validate_slas, _From, State) ->
    ValidationResult = validate_sla_compliance(State),
    {reply, ValidationResult, State};

handle_call(generate_dashboard, _From, State) ->
    Dashboard = generate_dashboard_data(State),
    {reply, Dashboard, State};

handle_call({register_metric, MetricName, Definition}, _From, State) ->
    % Validate metric definition
    case validate_metric_definition(Definition) of
        ok ->
            UpdatedMetrics = maps:put(MetricName, Definition, State#state.metrics),
            {reply, ok, State#state{metrics = UpdatedMetrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_thresholds, MetricName}, _From, State) ->
    case maps:get(MetricName, State#state.thresholds, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Thresholds ->
            {reply, {ok, Thresholds}, State}
    end;

handle_call({set_thresholds, MetricName, Critical, Warning}, _From, State) ->
    Thresholds = #thresholds{
        critical = Critical,
        warning = Warning,
        last_updated = erlang:system_time(millisecond)
    },

    UpdatedThresholds = maps:put(MetricName, Thresholds, State#state.thresholds),
    {reply, ok, State#state{thresholds = UpdatedThresholds}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(periodic_health_check, State) ->
    % Check health of all services
    UpdatedHealthStatus = maps:fold(fun(ServiceId, _Status, Acc) ->
        NewStatus = check_service_health(ServiceId),
        Acc#{ServiceId => NewStatus}
    end, #{}, State#state.health_status),

    % Check for critical health issues
    CriticalIssues = identify_critical_health_issues(UpdatedHealthStatus),

    % Escalate if needed
    lists:foreach(fun(Issue) ->
        escalate_health_issue(Issue, State)
    end, CriticalIssues),

    % Update state
    UpdatedState = State#state{health_status = UpdatedHealthStatus},

    % Schedule next check
    erlang:send_after(5000, self(), periodic_health_check),
    {noreply, UpdatedState};

handle_info(periodic_metrics_collection, State) ->
    % Collect metrics from all services
    CollectedMetrics = collect_service_metrics(State),

    % Store metrics
    UpdatedValues = store_metrics(CollectedMetrics, State#state.values),

    % Check alert conditions
    TriggeredAlerts = check_alert_conditions(UpdatedValues, State),

    % Process triggered alerts
    lists:foreach(fun(Alert) ->
        process_alert(Alert, State)
    end, TriggeredAlerts),

    % Update state
    UpdatedState = State#state{values = UpdatedValues},

    % Schedule next collection
    erlang:send_after(30000, self(), periodic_metrics_collection),
    {noreply, UpdatedState};

handle_info(periodic_sla_validation, State) ->
    % Validate SLA compliance
    ValidationResult = validate_sla_compliance(State),

    % Generate SLA report
    SLAReport = generate_sla_report(ValidationResult),

    % Store report
    erlmcp_storage:store(sla_report, SLAReport),

    % Notify if SLAs are violated
    case ValidationResult#sla_validation_result.compliant of
        false ->
            notify_sla_violation(ValidationResult);
        true ->
            ok
    end,

    % Schedule next validation
    erlang:send_after(60000, self(), periodic_sla_validation),
    {noreply, State};

handle_info(alert_triggered, AlertId, State) ->
    % Handle triggered alert
    UpdatedState = process_alert(AlertId, State),
    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Metric Management
-spec initialize_metrics() -> map().
initialize_metrics() ->
    #{
        "service_availability" => #metric_definition{
            name = "service_availability",
            type = gauge,
            description = "Service availability percentage",
            unit = "percent",
            labels = ["service"],
            aggregation = avg
        },
        "response_time" => #metric_definition{
            name = "response_time",
            type = histogram,
            description = "Service response time in milliseconds",
            unit = "milliseconds",
            labels = ["service"],
            aggregation = avg
        },
        "error_rate" => #metric_definition{
            name = "error_rate",
            type = gauge,
            description = "Service error rate percentage",
            unit = "percent",
            labels = ["service"],
            aggregation = avg
        },
        "throughput" => #metric_definition{
            name = "throughput",
            type = counter,
            description = "Service throughput per second",
            unit = "req/s",
            labels = ["service"],
            aggregation = sum
        },
        "rto_compliance" => #metric_definition{
            name = "rto_compliance",
            type = gauge,
            description = "RTO compliance percentage",
            unit = "percent",
            labels = ["service"],
            aggregation = avg
        },
        "rpo_compliance" => #metric_definition{
            name = "rpo_compliance",
            type = gauge,
            description = "RPO compliance percentage",
            unit = "percent",
            labels = ["service"],
            aggregation = avg
        }
    }.

-spec initialize_values() -> map().
initialize_values() ->
    % Initialize empty metric value maps
    lists:foldl(fun(MetricName, Acc) ->
        Acc#{MetricName => []}
    end, #{}, maps:keys(initialize_metrics())).

-spec store_metrics(map(), map()) -> map().
store_metrics(CollectedMetrics, CurrentValues) ->
    lists:foldl(fun({MetricName, Value}, Acc) ->
        ExistingValues = maps:get(MetricName, Acc, []),
        UpdatedValues = [Value | lists:sublist(ExistingValues, 1000)],  % Keep last 1000 values
        Acc#{MetricName => UpdatedValues}
    end, CurrentValues, CollectedMetrics).

%% Health Monitoring
-spec check_and_update_service_health(service_id(), state()) -> health_status().
check_and_update_service_health(ServiceId, State) ->
    % Get current health status
    CurrentStatus = maps:get(ServiceId, State#state.health_status, #health_status{
        service = ServiceId,
        status = healthy,
        metrics = #{},
        issues = [],
        last_check = erlang:system_time(millisecond),
        trend = stable
    }),

    % Check individual metrics
    Metrics = collect_service_metrics(ServiceId),
    UpdatedIssues = analyze_metric_issues(Metrics, CurrentStatus#health_status.issues),

    % Determine overall status
    OverallStatus = calculate_overall_status(UpdatedIssues),

    % Determine trend
    Trend = calculate_health_trend(ServiceId, CurrentStatus, State),

    % Update status
    #health_status{
        service = ServiceId,
        status = OverallStatus,
        metrics = Metrics,
        issues = UpdatedIssues,
        last_check = erlang:system_time(millisecond),
        trend = Trend
    }.

-spec analyze_metric_issues(map(), [issue()]) -> [issue()].
analyze_metric_issues(Metrics, CurrentIssues) ->
    % Analyze each metric for issues
    lists:foldl(fun(MetricName, Acc) ->
        MetricValue = maps:get(MetricName, Metrics),
        Thresholds = get_thresholds_for_metric(MetricName),

        case check_metric_violation(MetricValue, Thresholds) of
            {violation, Severity} ->
                Issue = #issue{
                    id = generate_issue_id(),
                    description = io_lib:format("Metric ~s violation: ~w", [MetricName, MetricValue]),
                    severity = Severity,
                    timestamp = erlang:system_time(millisecond),
                    resolved = false,
                    resolution = ""
                },
                [Issue | Acc];
            no_violation ->
                Acc
        end
    end, CurrentIssues, maps:keys(Metrics)).

%% Alert Management
-spec check_alert_conditions(map(), state()) -> [alert_rule()].
check_alert_conditions(Metrics, State) ->
    TriggeredAlerts = [],

    % Check each alert rule
    lists:foldl(fun(AlertId, AlertRule, Acc) ->
        case AlertRule#alert_rule.enabled of
            true ->
                MetricValues = maps:get(AlertRule#alert_rule.metric, Metrics, []),
                case check_alert_condition(MetricValues, AlertRule) of
                    true -> [AlertRule | Acc];
                    false -> Acc
                end;
            false ->
                Acc
        end
    end, TriggeredAlerts, maps:values(State#state.alerts)).

-spec process_alert(alert_rule(), state()) -> state().
process_alert(AlertRule, State) ->
    % Create alert record
    Alert = #alert_history{
        id = AlertRule#alert_rule.id,
        metric = AlertRule#alert_rule.metric,
        value = get_current_metric_value(AlertRule#alert_rule.metric),
        threshold = AlertRule#alert_rule.threshold,
        severity = AlertRule#alert_rule.severity,
        timestamp = erlang:system_time(millisecond),
        acknowledged = false,
        resolved = false,
        actions_taken = [],
        duration = 0
    },

    % Execute alert actions
    lists:foreach(fun(Action) ->
        execute_alert_action(Action, Alert)
    end, AlertRule#alert_rule.actions),

    % Add to history
    UpdatedHistory = [Alert | State#state.history],

    % Check for escalations
    case Alert#alert_history.severity of
        critical ->
            escalate_alert(Alert, State);
        high ->
            schedule_escalation(Alert, 300000);  % 5 minutes
        _ ->
            ok
    end,

    State#state{history = UpdatedHistory}.

%% SLA Validation
-spec validate_sla_compliance(state()) -> sla_validation_result().
validate_sla_compliance(State) ->
    % Check each SLA target
    ComplianceResults = lists:foldl(fun(SLATarget, Acc) ->
        Compliance = check_sla_compliance(SLATarget, State),
        Acc#{SLATarget#sla_target.service => Compliance}
    end, #{}, State#state.sla_targets),

    % Calculate overall compliance
    TotalSLAs = length(State#state.sla_targets),
    CompliantSLAs = length(lists:filter(fun({_, C}) -> C end, maps:to_list(ComplianceResults))),
    CompliancePercentage = (CompliantSLAs / TotalSLAs) * 100,

    #sla_validation_result{
        compliant = CompliancePercentage >= 99.9,  % Require 99.9% SLA compliance
        compliance_percentage = CompliancePercentage,
        targets = State#state.sla_targets,
        results = ComplianceResults,
        timestamp = erlang:system_time(millisecond)
    }.

-spec check_sla_compliance(sla_target(), state()) -> boolean().
check_sla_compliance(SLATarget, State) ->
    MetricName = SLATarget#sla_target.metric,
    MetricValues = maps:get(MetricName, State#state.values, []),

    case MetricValues of
        [] ->
            false;  % No data available
        _ ->
            AverageValue = calculate_average(MetricValues),
            AverageValue =< SLATarget#sla_target.target
    end.

%% Dashboard Generation
-spec generate_dashboard_data(state()) -> dashboard_data().
generate_dashboard_data(State) ->
    #{
        timestamp => erlang:system_time(millisecond),
        health_overview => generate_health_overview(State),
        metrics_summary => generate_metrics_summary(State),
        active_alerts => get_active_alerts(State),
        sla_status => generate_sla_status(State),
        service_trends => generate_service_trends(State),
        system_load => generate_system_load(State),
        recovery_status => generate_recovery_status(State)
    }.

-spec generate_health_overview(state()) -> map().
generate_health_overview(State) ->
    HealthStatus = State#state.health_status,

    % Count services by status
    StatusCounts = lists:foldl(fun(_, Status, Acc) ->
        maps:update_with(Status, fun(X) -> X + 1 end, 1, Acc)
    end, #{}, [Status#health_status.status || Status <- maps:values(HealthStatus)]),

    #{
        total_services => maps:size(HealthStatus),
        healthy => maps:get(healthy, StatusCounts, 0),
        degraded => maps:get(degraded, StatusCounts, 0),
        critical => maps:get(critical, StatusCounts, 0),
        overall_health => calculate_overall_health_percentage(StatusCounts)
    }.

%% State Management
-spec initialize_alerts() -> map().
initialize_alerts() ->
    #{
        "high_error_rate" => #alert_rule{
            id = "high_error_rate",
            metric = "error_rate",
            condition = greater_than,
            threshold = 5.0,  % 5%
            duration = 300000,
            severity = high,
            message = "Error rate exceeded 5%",
            actions = [notify, escalate],
            enabled = true
        },
        "rto_violation" => #alert_rule{
            id = "rto_violation",
            metric = "rto_compliance",
            condition = less_than,
            threshold = 95.0,  % 95%
            duration = 600000,
            severity = critical,
            message = "RTO compliance below 95%",
            actions = [notify, escalate, auto_resolve],
            enabled = true
        },
        "service_down" => #alert_rule{
            id = "service_down",
            metric = "service_availability",
            condition = less_than,
            threshold = 99.0,  % 99%
            duration = 30000,
            severity = critical,
            message = "Service availability below 99%",
            actions = [notify, escalate, execute_command],
            enabled = true
        }
    }.

-spec initialize_thresholds() -> map().
initialize_thresholds() ->
    #{
        "service_availability" => #thresholds{
            critical = 95.0,
            warning = 98.0,
            last_updated = erlang:system_time(millisecond)
        },
        "response_time" => #thresholds{
            critical = 1000,  % 1000ms
            warning = 500,   % 500ms
            last_updated = erlang:system_time(millisecond)
        },
        "error_rate" => #thresholds{
            critical = 5.0,
            warning = 1.0,
            last_updated = erlang:system_time(millisecond)
        },
        "throughput" => #thresholds{
            critical = 1000,  % 1000 req/s
            warning = 5000,   % 5000 req/s
            last_updated = erlang:system_time(millisecond)
        }
    }.

-spec initialize_sla_targets() -> [sla_target()].
initialize_sla_targets() ->
    [
        #sla_target{
            service = "core_mcp",
            metric = "service_availability",
            target = 99.99,
            timeframe = "day",
            weight = 1.0,
            criticality = mission_critical
        },
        #sla_target{
            service = "session_management",
            metric = "response_time",
            target = 100,
            timeframe = "hour",
            weight = 0.8,
            criticality = mission_critical
        },
        #sla_target{
            service = "registry_service",
            metric = "throughput",
            target = 10000,
            timeframe = "minute",
            weight = 0.6,
            criticality = essential
        }
    ].

-spec initialize_health_status() -> map().
initialize_health_status() ->
    % Initialize empty health status for all services
    lists:foldl(fun(ServiceId, Acc) ->
        Acc#{ServiceId => #health_status{
            service = ServiceId,
            status = healthy,
            metrics = #{},
            issues = [],
            last_check = erlang:system_time(millisecond),
            trend = stable
        }}
    end, #{}, ["core_mcp", "session_management", "registry_service"]).

%% Helper Functions
-spec generate_issue_id() -> issue_id().
generate_issue_id() ->
    list_to_binary("issue-" + integer_to_list(erlang:system_time(millisecond))).

-spec generate_alert_message(alert_id(), metric_name(), condition()) -> string().
generate_alert_message(AlertId, MetricName, Condition) ->
    list_to_binary("Alert ~s triggered for metric ~s with condition ~w", [AlertId, MetricName, Condition]).

-spec get_current_metric_value(metric_name()) -> number().
get_current_metric_value(MetricName) ->
    % Get current metric value from system
    case erlmcp_metrics:get_current_value(MetricName) of
        {ok, Value} -> Value;
        {error, _} -> 0.0
    end.

-spec check_metric_violation(number(), thresholds()) -> {violation, severity()} | no_violation.
check_metric_violation(Value, Thresholds) ->
    Critical = Thresholds#thresholds.critical,
    Warning = Thresholds#thresholds.warning,

    case Value >= Critical of
        true -> {violation, critical};
        false ->
            case Value >= Warning of
                true -> {violation, high};
                false -> no_violation
            end
    end.

-spec calculate_average([number()]) -> number().
calculate_average(Values) ->
    case Values of
        [] -> 0.0;
        _ -> lists:sum(Values) / length(Values)
    end.

-spec calculate_overall_status([issue()]) -> health_status_status().
calculate_overall_status(Issues) ->
    CriticalIssues = lists:filter(fun(Issue) -> Issue#issue.severity =:= critical end, Issues),
    HighIssues = lists:filter(fun(Issue) -> Issue#issue.severity =:= high end, Issues),

    case CriticalIssues of
        [] ->
            case HighIssues of
                [] -> healthy;
                _ -> degraded
            end;
        _ -> critical
    end.

%%====================================================================
%% Test Functions
%%====================================================================

-spec test_metric_collection() -> ok.
test_metric_collection() ->
    % Test metric collection functionality
    MetricName = "test_metric",
    Definition = #metric_definition{
        name = MetricName,
        type = gauge,
        description = "Test metric",
        unit = "test",
        labels = [],
        aggregation = avg
    },

    % Register metric
    ok = erlmcp_monitoring:register_metric(MetricName, Definition),

    % Check if metric is registered
    case erlmcp_monitoring:get_metrics(MetricName) of
        {ok, _Values} -> ok;
        {error, not_found} -> {error, metric_not_registered}
    end.

-spec test_alert_system() -> ok.
test_alert_system() ->
    % Test alert system
    AlertId = "test_alert",
    MetricName = "error_rate",
    Condition = greater_than,

    % Add alert rule
    ok = erlmcp_monitoring:add_alert_rule(AlertId, MetricName, Condition),

    % Check alert history
    History = erlmcp_monitoring:get_alert_history(AlertId),
    case History of
        [] -> ok;  % No history yet
        _ -> ok
    end.

-spec test_health_check() -> health_status().
test_health_check() ->
    % Test service health check
    ServiceId = "core_mcp",

    erlmcp_monitoring:check_service_health(ServiceId).

-spec test_sla_validation() -> sla_validation_result().
test_sla_validation() ->
    % Test SLA validation
    erlmcp_monitoring:validate_slas().

-spec test_dashboard_generation() -> dashboard_data().
test_dashboard_generation() ->
    % Test dashboard generation
    erlmcp_monitoring:generate_dashboard().