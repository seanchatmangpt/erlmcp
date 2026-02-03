%%%-------------------------------------------------------------------
%%% @doc
%%% Alert Manager for erlmcp Monitoring Stack
%%%
%%% Manages alert routing, escalation policies, and suppression rules
%%% for comprehensive alerting across all monitoring components.
%%%
%%% Features:
%%% - Rule-based alert routing
%%% - Escalation policies
%%% - Alert suppression
%%% - Multiple notification channels
%%% - Alert deduplication
%%% - Performance metrics
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_alert_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         add_route/1, remove_route/1,
         add_policy/1, remove_policy/1,
         add_suppression/1, remove_suppression/1,
         send_alert/1, send_alert/2,
         get_active_alerts/0, get_alert_history/1,
         get_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(alert_route, {
    id :: binary(),
    name :: binary(),
    condition :: binary(),  % Alert condition string
    targets :: [binary()],  % Target services
    template :: binary()    % Notification template
}).

-record.alert_policy, {
    id :: binary(),
    name :: binary(),
    severity :: low | medium | high | critical,
    delay :: binary(),      % Time delay (e.g., "5m", "1h")
    steps :: [binary()],    % Escalation steps
    active :: boolean()
}).

-record.alert_suppression, {
    id :: binary(),
    condition :: binary(),
    duration :: binary(),   % Duration (e.g., "30m", "1h")
    expires :: integer()    % Expiration timestamp
}.

-record.alert, {
    id :: binary(),
    timestamp :: integer(),
    source :: binary(),
    severity :: low | medium | high | critical,
    title :: binary(),
    message :: binary(),
    details :: map(),
    labels :: map(),
    silenced :: boolean(),
    acknowledged :: boolean(),
    acknowledged_by :: binary() | undefined,
    acknowledged_at :: integer() | undefined,
    resolved :: boolean(),
    resolved_at :: integer() | undefined
}.

-record.state, {
    routes :: #{binary() => #alert_route{}},
    policies :: #{binary() => #alert_policy{}},
    suppressions :: #{binary() => #alert_suppression{}},
    active_alerts :: #{binary() => #alert{}},
    alert_history :: queue:queue(#alert{}),
    metrics :: map(),
    last_alert_id :: binary()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the alert manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Add alert routing rule
-spec add_route(map()) -> ok.
add_route(Config) ->
    gen_server:call(?MODULE, {add_route, Config}).

%% @doc Remove alert routing rule
-spec remove_route(binary()) -> ok.
remove_route(RouteId) ->
    gen_server:call(?MODULE, {remove_route, RouteId}).

%% @doc Add escalation policy
-spec add_policy(map()) -> ok.
add_policy(Config) ->
    gen_server:call(?MODULE, {add_policy, Config}).

%% @doc Remove escalation policy
-spec remove_policy(PolicyId) -> ok.
remove_policy(PolicyId) ->
    gen_server:call(?MODULE, {remove_policy, PolicyId}).

%% @doc Add suppression rule
-spec add_suppression(map()) -> ok.
add_suppression(Config) ->
    gen_server:call(?MODULE, {add_suppression, Config}).

%% @doc Remove suppression rule
-spec remove_suppression(SuppressionId) -> ok.
remove_suppression(SuppressionId) ->
    gen_server:call(?MODULE, {remove_suppression, SuppressionId}).

%% @doc Send an alert
-spec send_alert(map()) -> ok.
send_alert(AlertData) ->
    gen_server:call(?MODULE, {send_alert, AlertData}).

%% @doc Send an alert with explicit notification
-spec send_alert(map(), binary()) -> ok.
send_alert(AlertData, NotificationType) ->
    gen_server:call(?MODULE, {send_alert, AlertData, NotificationType}).

%% @doc Get active alerts
-spec get_active_alerts() -> [#alert{}].
get_active_alerts() ->
    gen_server:call(?MODULE, get_active_alerts).

%% @doc Get alert history
-spec get_alert_history(binary()) -> [#alert{}].
get_alert_history(Source) ->
    gen_server:call(?MODULE, {get_alert_history, Source}).

%% @doc Get alert manager metrics
-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Load initial configuration
    Routes = load_routes(Config),
    Policies = load_policies(Config),
    Suppressions = load_suppressions(Config),

    %% Initialize state
    State = #state{
        routes = Routes,
        policies = Policies,
        suppressions = Suppressions,
        active_alerts = #{},
        alert_history = queue:new(),
        metrics = #{
            total_alerts => 0,
            active_alerts => 0,
            silenced_alerts => 0,
            acknowledged_alerts => 0,
            resolved_alerts => 0,
            alerts_by_severity => #{low => 0, medium => 0, high => 0, critical => 0}
        },
        last_alert_id => generate_alert_id()
    },

    %% Start cleanup timer
    erlang:send_after(60000, self(), cleanup_suppressions),

    {ok, State}.

handle_call({add_route, Config}, _From, State) ->
    Route = create_alert_route(Config),
    Routes = maps:put(Route#alert_route.id, Route, State#state.routes),

    ?LOG_INFO("Added alert route: ~s", [Route#alert_route.name]),

    {reply, ok, State#state{routes = Routes}};

handle_call({remove_route, RouteId}, _From, State) ->
    Routes = maps:remove(RouteId, State#state.routes),

    ?LOG_INFO("Removed alert route: ~s", [RouteId]),

    {reply, ok, State#state{routes = Routes}};

handle_call({add_policy, Config}, _From, State) ->
    Policy = create_alert_policy(Config),
    Policies = maps:put(Policy#alert_policy.id, Policy, State#state.policies),

    ?LOG_INFO("Added alert policy: ~s", [Policy#alert_policy.name]),

    {reply, ok, State#state{policies = Policies}};

handle_call({remove_policy, PolicyId}, _From, State) ->
    Policies = maps:remove(PolicyId, State#state.policies),

    ?LOG_INFO("Removed alert policy: ~s", [PolicyId]),

    {reply, ok, State#state{policies = Policies}};

handle_call({add_suppression, Config}, _From, State) ->
    Suppression = create_alert_suppression(Config),
    Suppressions = maps:put(Suppression#alert_suppression.id, Suppression, State#state.suppressions),

    ?LOG_INFO("Added alert suppression: ~s", [Suppression#alert_suppression.id]),

    {reply, ok, State#state{suppressions = Suppressions}};

handle_call({remove_suppression, SuppressionId}, _From, State) ->
    Suppressions = maps:remove(SuppressionId, State#state.suppressions),

    ?LOG_INFO("Removed alert suppression: ~s", [SuppressionId]),

    {reply, ok, State#state{suppressions = Suppressions}};

handle_call({send_alert, AlertData}, _From, State) ->
    %% Create alert record
    Alert = create_alert(AlertData, State#state.last_alert_id),
    NewAlertId = Alert#alert.id,

    %% Check if alert is suppressed
    case is_alert_suppressed(Alert, State#state.suppressions) of
        true ->
            ?LOG_DEBUG("Alert suppressed: ~s", [NewAlertId]),
            SilencedAlert = Alert#alert{silenced = true},
            UpdatedState = process_alert(SilencedAlert, State);
        false ->
            %% Process alert
            UpdatedState = process_alert(Alert, State)
    end,

    {reply, ok, UpdatedState};

handle_call({send_alert, AlertData, NotificationType}, _From, State) ->
    %% Add notification type to alert
    AlertDataWithNotification = maps:put(notification_type, NotificationType, AlertData),
    handle_call({send_alert, AlertDataWithNotification}, _From, State);

handle_call(get_active_alerts, _From, State) ->
    %% Convert map to list
    ActiveAlerts = maps:values(State#state.active_alerts),
    {reply, ActiveAlerts, State};

handle_call({get_alert_history, Source}, _From, State) ->
    %% Filter history by source
    History = queue:to_list(State#state.alert_history),
    Filtered = lists:filter(fun(A) -> A#alert.source =:= Source end, History),
    {reply, Filtered, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_suppressions, State) ->
    %% Clean up expired suppressions
    Now = erlang:system_time(second),
    ActiveSuppressions = maps:filter(fun(_Id, Suppression) ->
        Suppression#alert_suppression.expires > Now
    end, State#state.suppressions),

    %% Schedule next cleanup
    erlang:send_after(60000, self(), cleanup_suppressions),

    {noreply, State#state{suppressions = ActiveSuppressions}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create alert route from config
create_alert_route(Config) ->
    #alert_route{
        id = maps:get(id, Config, generate_alert_id()),
        name = maps:get(name, Config),
        condition = maps:get(condition, Config),
        targets = maps:get(targets, Config, []),
        template = maps:get(template, Config, <<"default">>)
    }.

%% @doc Create alert policy from config
create_alert_policy(Config) ->
    #alert_policy{
        id = maps:get(id, Config, generate_alert_id()),
        name = maps:get(name, Config),
        severity = maps:get(severity, Config, medium),
        delay = maps:get(delay, Config, <<"5m">>),
        steps = maps:get(steps, Config, [notify_team]),
        active = maps:get(active, Config, true)
    }.

%% @doc Create alert suppression from config
create_alert_suppression(Config) ->
    Now = erlang:system_time(second),
    DurationSeconds = parse_duration(maps:get(duration, Config)),
    Expires = Now + DurationSeconds,

    #alert_suppression{
        id = maps:get(id, Config, generate_alert_id()),
        condition = maps:get(condition, Config),
        duration = maps:get(duration, Config),
        expires = Expires
    }.

%% @doc Create alert record from data
create_alert(AlertData, BaseId) ->
    Id = maps:get(id, AlertData, generate_alert_id()),

    #alert{
        id = Id,
        timestamp = maps:get(timestamp, AlertData, erlang:system_time(millisecond)),
        source = maps:get(source, AlertData),
        severity = maps:get(severity, AlertData, medium),
        title = maps:get(title, AlertData),
        message = maps:get(message, AlertData),
        details = maps:get(details, AlertData, #{}),
        labels = maps:get(labels, AlertData, #{}),
        silenced = false,
        acknowledged = false,
        resolved = false
    }.

%% @doc Process an alert
process_alert(Alert, State) ->
    %% Update metrics
    Metrics = update_alert_metrics(Alert, State#state.metrics),

    %% Add to active alerts
    ActiveAlerts = maps:put(Alert#alert.id, Alert, State#state.active_alerts),

    %% Add to history
    History = queue:in(Alert, State#state.alert_history),
    TrimmedHistory = trim_history(History, 1000),

    %% Route alert
    route_alert(Alert, State#state.routes),

    %% Apply escalation policies
    escalate_alert(Alert, State#state.policies),

    %% Generate alert ID for next alert
    NewAlertId = generate_alert_id(),

    State#state{
        active_alerts = ActiveAlerts,
        alert_history = TrimmedHistory,
        metrics = Metrics,
        last_alert_id = NewAlertId
    }.

%% @doc Check if alert is suppressed
is_alert_suppressed(Alert, Suppressions) ->
    Now = erlang:system_time(second),
    AlertCondition = generate_alert_condition(Alert),

    maps:any(fun(_Id, Suppression) ->
        Suppression#alert_suppression.expires > Now andalso
        matches_condition(AlertCondition, Suppression#alert_suppression.condition)
    end, Suppressions).

%% @doc Generate condition string for alert
generate_alert_condition(Alert) ->
    io_lib:format("source=~s,severity=~s", [Alert#alert.source, Alert#alert.severity]).

%% @doc Check if condition matches
matches_condition(AlertCondition, SuppressionCondition) ->
    %% Simple pattern matching - can be enhanced with regex
    string:find(AlertCondition, SuppressionCondition) =/= nomatch.

%% @doc Route alert to targets
route_alert(Alert, Routes) ->
    %% Find matching routes
    MatchingRoutes = [R || R <- maps:values(Routes),
                        matches_condition(Alert, R#alert_route.condition)],

    %% Send notifications
    lists:foreach(fun(Route) ->
        lists:foreach(fun(Target) ->
            send_notification(Alert, Target, Route#alert_route.template)
        end, Route#alert_route.targets)
    end, MatchingRoutes).

%% @doc Send notification via target
send_notification(Alert, Target, Template) ->
    %% This would integrate with various notification services
    Notification = format_notification(Alert, Template),

    case Target of
        <<"slack">> -> send_slack_notification(Notification);
        <<"email">> -> send_email_notification(Notification);
        <<"pagerduty">> -> send_pagerduty_notification(Notification);
        <<"webhook">> -> send_webhook_notification(Notification);
        _ -> ?LOG_INFO("Unknown target: ~s", [Target])
    end.

%% @doc Format notification from alert and template
format_notification(Alert, Template) ->
    Base = #{
        id => Alert#alert.id,
        source => Alert#alert.source,
        severity => Alert#alert.severity,
        title => Alert#alert.title,
        message => Alert#alert.message,
        timestamp => Alert#alert.timestamp
    },

    case Template of
        <<"slack">> ->
            format_slack_notification(Alert);
        <<"email">> ->
            format_email_notification(Alert);
        _ ->
            Base
    end.

%% @doc Format Slack notification
format_slack_notification(Alert) ->
    #{
        attachments => [#{
            color => get_severity_color(Alert#alert.severity),
            title => Alert#alert.title,
            text => Alert#alert.message,
            fields => [#{
                title => "Source",
                value => Alert#alert.source,
                short => true
            }, #{
                title => "Severity",
                value => atom_to_binary(Alert#alert.severity),
                short => true
            }]
        }]
    }.

%% @doc Format email notification
format_email_notification(Alert) ->
    #{
        subject => io_lib:format("[~s] ~s", [atom_to_binary(Alert#alert.severity), Alert#alert.title]),
        body => io_lib:format("Alert: ~s~nSource: ~s~nMessage: ~s", [
            Alert#alert.title,
            Alert#alert.source,
            Alert#alert.message
        ])
    }.

%% @doc Get severity color
get_severity_color(low) -> "#36a64f";      % Green
get_severity_color(medium) -> "#ff9500";   % Orange
get_severity_color(high) -> "#ff0000";     % Red
get_severity_color(critical) -> "#990000".   % Dark Red

%% @doc Send Slack notification (stub)
send_slack_notification(Notification) ->
    ?LOG_INFO("Slack notification: ~p", [Notification]).

%% @doc Send email notification (stub)
send_email_notification(Notification) ->
    ?LOG_INFO("Email notification: ~p", [Notification]).

%% @doc Send PagerDuty notification (stub)
send_pagerduty_notification(Notification) ->
    ?LOG_INFO("PagerDuty notification: ~p", [Notification]).

%% @doc Send webhook notification (stub)
send_webhook_notification(Notification) ->
    ?LOG_INFO("Webhook notification: ~p", [Notification]).

%% @doc Apply escalation policies
escalate_alert(Alert, Policies) ->
    %% Find matching policies
    MatchingPolicies = [P || P <- maps:values(Policies),
                            P#alert_policy.active =:= true,
                            Alert#alert.severity =:= P#alert_policy.severity],

    %% Apply policies
    lists:foreach(fun(Policy) ->
        apply_escalation_policy(Alert, Policy)
    end, MatchingPolicies).

%% @doc Apply escalation policy
apply_escalation_policy(Alert, Policy) ->
    %% Parse delay
    DelaySeconds = parse_duration(Policy#alert_policy.delay),

    %% Schedule escalation steps
    Now = erlang:system_time(millisecond),
    lists:foldl(fun(Step, Acc) ->
        Delay = Acc * 1000,  % Convert to milliseconds
        erlang:send_after(Delay, self(), {escalate, Alert#alert.id, Step}),
        Acc + 1
    end, 1, Policy#alert_policy.steps).

%% @doc Escalate alert
escalate(AlertId, Step) ->
    %% Get current alert
    Alert = gen_server:call(?MODULE, {get_alert, AlertId}),

    %% Send escalation notification
    EscalationMessage = io_lib:format("Escalation step: ~s", [Step]),
    EscalationAlert = Alert#alert{
        message <<Alert#alert.message/binary, "\n", EscalationMessage/binary>>
    },

    %% Send notification
    send_notification(EscalationAlert, <<"escalation">>, <<"default">>).

%% @doc Update alert metrics
update_alert_metrics(Alert, Metrics) ->
    SeverityMap = Metrics#{
        alerts_by_severity => maps:update_with(
            Alert#alert.severity,
            fun(V) -> V + 1 end,
            1
        )
    },

    SeverityMap#{
        total_alerts => Metrics#metrics.total_alerts + 1,
        active_alerts => Metrics#metrics.active_alerts + 1,
        silenced_alerts => if Alert#alert.silenced -> Metrics#metrics.silenced_alerts + 1; true -> Metrics#metrics.silenced_alerts end,
        acknowledged_alerts => if Alert#alert.acknowledged -> Metrics#metrics.acknowledged_alerts + 1; true -> Metrics#metrics.acknowledged_alerts end,
        resolved_alerts => if Alert#alert.resolved -> Metrics#metrics.resolved_alerts + 1; true -> Metrics#metrics.resolved_alerts end
    }.

%% @brief Trim history queue to size limit
trim_history(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of
        true -> queue:drop(Queue);
        false -> Queue
    end.

%% @brief Parse duration string
parse_duration(Duration) when is_binary(Duration) ->
    parse_duration(binary_to_list(Duration));
parse_duration(Duration) ->
    %% Parse "5m", "1h", "30s" etc.
    case re:run(Duration, "^(\\d+)([smh])$", [{capture, all, list}]) of
        nomatch -> 0;
        {match, [_, Value, Unit]} ->
            Num = list_to_integer(Value),
            case Unit of
                "s" -> Num;
                "m" -> Num * 60;
                "h" -> Num * 3600
            end
    end.

%% @brief Load routes from configuration
load_routes(Config) ->
    case maps:get(routes, Config, []) of
        Routes when is_list(Routes) ->
            [create_alert_route(R) || R <- Routes];
        _ ->
            []
    end.

%% @brief Load policies from configuration
load_policies(Config) ->
    case maps:get(policies, Config, []) of
        Policies when is_list(Policies) ->
            [create_alert_policy(P) || P <- Policies];
        _ ->
            []
    end.

%% @brief Load suppressions from configuration
load_suppressions(Config) ->
    case maps:get(suppressions, Config, []) of
        Suppressions when is_list(Suppressions) ->
            [create_alert_suppression(S) || S <- Suppressions];
        _ ->
            []
    end.

%% @brief Generate unique alert ID
generate_alert_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).