%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow monitoring and metrics for erlmcp v3
%%% Provides real-time progress tracking and observability.
%%%
%%% == Features ==
%%% - Real-time progress tracking
%%% - OTEL integration for distributed tracing
%%% - Metrics collection (duration, throughput, errors)
%%% - Dashboard integration
%%% - Alert thresholds
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_monitor).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, start_link/1,
         track_workflow/2, update_progress/3,
         record_metric/3, record_event/2,
         get_workflow_metrics/1,
         get_system_metrics/0,
         set_alert_threshold/2,
         check_alerts/1,
         subscribe_to_updates/2,
         unsubscribe_from_updates/1,
         export_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type definitions
-type workflow_id() :: binary().
-type metric_name() :: binary().
-type metric_value() :: number().
-type event_type() :: started | completed | failed | paused | resumed | cancelled.

-record(workflow_metrics,
        {workflow_id :: workflow_id(),
         started_at :: integer(),
         completed_at :: integer() | undefined,
         progress = 0.0 :: float(),
         tasks_total = 0 :: non_neg_integer(),
         tasks_completed = 0 :: non_neg_integer(),
         tasks_failed = 0 :: non_neg_integer(),
         duration_ms = 0 :: non_neg_integer(),
         error_count = 0 :: non_neg_integer(),
         last_updated :: integer()}).

-record(alert_threshold,
        {metric :: metric_name(),
         operator :: gt | lt | eq,
         value :: metric_value(),
         action :: term()}).

-record(subscriber,
        {pid :: pid(),
         monitor_ref :: reference(),
         filters :: [event_type()]}).

-record(state,
        {workflows = #{} :: #{workflow_id() => #workflow_metrics{}},
         metrics = #{} :: #{metric_name() => [metric_value()]},
         events = [] :: [map()],
         alert_thresholds = [] :: [#alert_threshold{}],
         subscribers = [] :: [#subscriber{}],
         max_events = 10000 :: pos_integer(),
         metrics_window = 60000 :: pos_integer()}).

-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-spec track_workflow(workflow_id(), map()) -> ok.
track_workflow(WorkflowId, Metadata) ->
    gen_server:cast(?MODULE, {track_workflow, WorkflowId, Metadata}).

-spec update_progress(workflow_id(), float(), map()) -> ok.
update_progress(WorkflowId, Progress, Metadata) ->
    gen_server:cast(?MODULE, {update_progress, WorkflowId, Progress, Metadata}).

-spec record_metric(workflow_id(), metric_name(), metric_value()) -> ok.
record_metric(WorkflowId, MetricName, Value) ->
    gen_server:cast(?MODULE, {record_metric, WorkflowId, MetricName, Value}).

-spec record_event(workflow_id(), event_type()) -> ok.
record_event(WorkflowId, EventType) ->
    gen_server:cast(?MODULE, {record_event, WorkflowId, EventType, erlang:system_time(millisecond)}).

-spec get_workflow_metrics(workflow_id()) -> {ok, map()} | {error, not_found}.
get_workflow_metrics(WorkflowId) ->
    gen_server:call(?MODULE, {get_workflow_metrics, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec get_system_metrics() -> {ok, map()}.
get_system_metrics() ->
    gen_server:call(?MODULE, get_system_metrics, ?DEFAULT_TIMEOUT).

-spec set_alert_threshold(metric_name(), map()) -> ok.
set_alert_threshold(MetricName, Threshold) ->
    gen_server:call(?MODULE, {set_alert_threshold, MetricName, Threshold}, ?DEFAULT_TIMEOUT).

-spec check_alerts(workflow_id()) -> [map()].
check_alerts(WorkflowId) ->
    gen_server:call(?MODULE, {check_alerts, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec subscribe_to_updates(pid(), [event_type()]) -> ok.
subscribe_to_updates(Pid, EventTypes) ->
    gen_server:call(?MODULE, {subscribe, Pid, EventTypes}, ?DEFAULT_TIMEOUT).

-spec unsubscribe_from_updates(pid()) -> ok.
unsubscribe_from_updates(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}, ?DEFAULT_TIMEOUT).

-spec export_metrics() -> {ok, map()}.
export_metrics() ->
    gen_server:call(?MODULE, export_metrics, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, #state{}}.
init([Options]) ->
    logger:info("Initializing workflow monitor"),

    State = #state{
        max_events = maps:get(max_events, Options, 10000),
        metrics_window = maps:get(metrics_window, Options, 60000)
    },

    %% Start metrics cleanup timer
    schedule_metrics_cleanup(State),

    logger:info("Workflow monitor initialized"),
    {ok, State}.

handle_call({get_workflow_metrics, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Metrics ->
            MetricsMap = #{
                workflow_id => Metrics#workflow_metrics.workflow_id,
                progress => Metrics#workflow_metrics.progress,
                tasks_total => Metrics#workflow_metrics.tasks_total,
                tasks_completed => Metrics#workflow_metrics.tasks_completed,
                tasks_failed => Metrics#workflow_metrics.tasks_failed,
                duration_ms => Metrics#workflow_metrics.duration_ms,
                error_count => Metrics#workflow_metrics.error_count,
                started_at => Metrics#workflow_metrics.started_at,
                completed_at => Metrics#workflow_metrics.completed_at
            },
            {reply, {ok, MetricsMap}, State}
    end;

handle_call(get_system_metrics, _From, State) ->
    WorkflowsList = maps:values(State#state.workflows),

    TotalTasks = lists:sum([M#workflow_metrics.tasks_total || M <- WorkflowsList]),
    CompletedTasks = lists:sum([M#workflow_metrics.tasks_completed || M <- WorkflowsList]),
    FailedTasks = lists:sum([M#workflow_metrics.tasks_failed || M <- WorkflowsList]),

    SystemMetrics = #{
        total_workflows => maps:size(State#state.workflows),
        total_tasks => TotalTasks,
        completed_tasks => CompletedTasks,
        failed_tasks => FailedTasks,
        success_rate => calculate_success_rate(CompletedTasks, FailedTasks),
        avg_duration => calculate_avg_duration(WorkflowsList),
        total_errors => lists:sum([M#workflow_metrics.error_count || M <- WorkflowsList]),
        events_stored => length(State#state.events),
        subscribers => length(State#state.subscribers)
    },
    {reply, {ok, SystemMetrics}, State};

handle_call({set_alert_threshold, MetricName, Threshold}, _From, State) ->
    Alert = #alert_threshold{
        metric = MetricName,
        operator = maps:get(operator, Threshold, gt),
        value = maps:get(value, Threshold, 0),
        action = maps:get(action, Threshold, log)
    },
    NewAlerts = [Alert | State#state.alert_thresholds],
    {reply, ok, State#state{alert_thresholds = NewAlerts}};

handle_call({check_alerts, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, [], State};
        Metrics ->
            Alerts = evaluate_alerts(Metrics, State#state.alert_thresholds),
            {reply, Alerts, State}
    end;

handle_call({subscribe, Pid, EventTypes}, _From, State) ->
    MonRef = erlang:monitor(process, Pid),
    Subscriber = #subscriber{
        pid = Pid,
        monitor_ref = MonRef,
        filters = EventTypes
    },
    NewSubscribers = [Subscriber | State#state.subscribers],
    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call({unsubscribe, Pid}, _From, State) ->
    NewSubscribers = lists:filter(
        fun(#subscriber{pid = Pid1}) -> Pid1 =/= Pid end,
        State#state.subscribers
    ),
    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call(export_metrics, _From, State) ->
    Export = #{
        workflows => maps:to_list(State#state.workflows),
        metrics => maps:to_list(State#state.metrics),
        system_metrics => #{
            total_workflows => maps:size(State#state.workflows),
            total_events => length(State#state.events)
        },
        exported_at => erlang:system_time(millisecond)
    },
    {reply, {ok, Export}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({track_workflow, WorkflowId, Metadata}, State) ->
    Now = erlang:system_time(millisecond),
    Metrics = #workflow_metrics{
        workflow_id = WorkflowId,
        started_at = Now,
        tasks_total = maps:get(tasks_total, Metadata, 0),
        last_updated = Now
    },
    NewWorkflows = maps:put(WorkflowId, Metrics, State#state.workflows),

    %% Emit to OTEL
    emit_workflow_event(started, WorkflowId, Metadata),

    {noreply, State#state{workflows = NewWorkflows}};

handle_cast({update_progress, WorkflowId, Progress, Metadata}, State) ->
    NewWorkflows = case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            State#state.workflows;
        Metrics ->
            UpdatedMetrics = Metrics#workflow_metrics{
                progress = Progress,
                tasks_completed = maps:get(tasks_completed, Metadata, Metrics#workflow_metrics.tasks_completed),
                tasks_failed = maps:get(tasks_failed, Metadata, Metrics#workflow_metrics.tasks_failed),
                last_updated = erlang:system_time(millisecond)
            },
            maps:put(WorkflowId, UpdatedMetrics, State#state.workflows)
    end,
    {noreply, State#state{workflows = NewWorkflows}};

handle_cast({record_metric, WorkflowId, MetricName, Value}, State) ->
    %% Store metric value
    Now = erlang:system_time(millisecond),
    MetricKey = {WorkflowId, MetricName},
    NewMetrics = maps:update_with(MetricKey, fun(Values) ->
        [{Value, Now} | Values]
    end, [{Value, Now}], State#state.metrics),

    %% Emit to OTEL
    emit_metric(MetricName, Value, [{workflow_id, WorkflowId}]),

    {noreply, State#state{metrics = NewMetrics}};

handle_cast({record_event, WorkflowId, EventType, Timestamp}, State) ->
    Event = #{
        workflow_id => WorkflowId,
        event_type => EventType,
        timestamp => Timestamp
    },
    NewEvents = [Event | State#state.events],

    %% Trim events if needed
    TrimmedEvents = case length(NewEvents) > State#state.max_events of
        true -> lists:sublist(NewEvents, State#state.max_events);
        false -> NewEvents
    end,

    %% Update workflow metrics
    NewWorkflows = case EventType of
        completed ->
            update_workflow_on_completion(WorkflowId, Timestamp, State#state.workflows);
        failed ->
            update_workflow_on_failure(WorkflowId, Timestamp, State#state.workflows);
        _ ->
            State#state.workflows
    end,

    %% Notify subscribers
    notify_subscribers(State#state.subscribers, Event),

    %% Emit to OTEL
    emit_workflow_event(EventType, WorkflowId, #{}),

    {noreply, State#state{events = TrimmedEvents, workflows = NewWorkflows}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    logger:debug("Subscriber ~p down: ~p", [Pid, Reason]),
    NewSubscribers = lists:filter(
        fun(#subscriber{monitor_ref = Ref}) -> Ref =/= MonRef end,
        State#state.subscribers
    ),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(metrics_cleanup, State) ->
    NewMetrics = cleanup_old_metrics(State#state.metrics, State#state.metrics_window),
    schedule_metrics_cleanup(State),
    {noreply, State#state{metrics = NewMetrics}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec calculate_success_rate(non_neg_integer(), non_neg_integer()) -> float().
calculate_success_rate(Completed, Failed) ->
    Total = Completed + Failed,
    case Total of
        0 -> 100.0;
        _ -> (Completed / Total) * 100.0
    end.

-spec calculate_avg_duration([#workflow_metrics{}]) -> non_neg_integer().
calculate_avg_duration(Workflows) ->
    CompletedWorkflows = [W || W <- Workflows, W#workflow_metrics.completed_at =/= undefined],
    case CompletedWorkflows of
        [] -> 0;
        List ->
            TotalDuration = lists:sum([W#workflow_metrics.duration_ms || W <- List]),
            TotalDuration div length(List)
    end.

-spec update_workflow_on_completion(workflow_id(), integer(), #{workflow_id() => #workflow_metrics{}}) ->
                                       #{workflow_id() => #workflow_metrics{}}.
update_workflow_on_completion(WorkflowId, CompletedAt, Workflows) ->
    case maps:get(WorkflowId, Workflows, undefined) of
        undefined -> Workflows;
        Metrics ->
            Duration = CompletedAt - Metrics#workflow_metrics.started_at,
            UpdatedMetrics = Metrics#workflow_metrics{
                completed_at = CompletedAt,
                duration_ms = Duration
            },
            maps:put(WorkflowId, UpdatedMetrics, Workflows)
    end.

-spec update_workflow_on_failure(workflow_id(), integer(), #{workflow_id() => #workflow_metrics{}}) ->
                                     #{workflow_id() => #workflow_metrics{}}.
update_workflow_on_failure(WorkflowId, FailedAt, Workflows) ->
    case maps:get(WorkflowId, Workflows, undefined) of
        undefined -> Workflows;
        Metrics ->
            Duration = FailedAt - Metrics#workflow_metrics.started_at,
            UpdatedMetrics = Metrics#workflow_metrics{
                completed_at = FailedAt,
                duration_ms = Duration,
                error_count = Metrics#workflow_metrics.error_count + 1
            },
            maps:put(WorkflowId, UpdatedMetrics, Workflows)
    end.

-spec evaluate_alerts(#workflow_metrics{}, [#alert_threshold{}]) -> [map()].
evaluate_alerts(Metrics, Thresholds) ->
    lists:filtermap(fun(Alert) ->
        MetricValue = get_metric_value(Alert#alert_threshold.metric, Metrics),
        case check_threshold(MetricValue, Alert#alert_threshold.operator, Alert#alert_threshold.value) of
            true ->
                AlertMap = #{
                    metric => Alert#alert_threshold.metric,
                    value => MetricValue,
                    threshold => Alert#alert_threshold.value,
                    operator => Alert#alert_threshold.operator,
                    action => Alert#alert_threshold.action
                },
                {true, AlertMap};
            false ->
                false
        end
    end, Thresholds).

-spec get_metric_value(binary(), #workflow_metrics{}) -> number().
get_metric_value(<<"progress">>, Metrics) -> Metrics#workflow_metrics.progress;
get_metric_value(<<"error_count">>, Metrics) -> Metrics#workflow_metrics.error_count;
get_metric_value(<<"duration_ms">>, Metrics) -> Metrics#workflow_metrics.duration_ms;
get_metric_value(<<"tasks_failed">>, Metrics) -> Metrics#workflow_metrics.tasks_failed;
get_metric_value(_, _) -> 0.

-spec check_threshold(number(), gt | lt | eq, number()) -> boolean().
check_threshold(Value, gt, Threshold) when Value > Threshold -> true;
check_threshold(Value, lt, Threshold) when Value < Threshold -> true;
check_threshold(Value, eq, Threshold) when Value =:= Threshold -> true;
check_threshold(_, _, _) -> false.

-spec notify_subscribers([#subscriber{}], map()) -> ok.
notify_subscribers(Subscribers, Event) ->
    lists:foreach(fun(#subscriber{pid = Pid, filters = Filters}) ->
        case lists:member(maps:get(event_type, Event), Filters) of
            true -> Pid ! {workflow_monitor_event, Event};
            false -> ok
        end
    end, Subscribers),
    ok.

-spec cleanup_old_metrics({{{workflow_id(), metric_name()}, [{metric_value(), integer()}]}}, pos_integer()) ->
                             {{{workflow_id(), metric_name()}, [{metric_value(), integer()}]}}.
cleanup_old_metrics(Metrics, WindowMs) ->
    CutoffTime = erlang:system_time(millisecond) - WindowMs,
    maps:map(fun(_Key, Values) ->
        [{V, T} || {V, T} <- Values, T >= CutoffTime]
    end, Metrics).

-spec schedule_metrics_cleanup(#state{}) -> ok.
schedule_metrics_cleanup(_State) ->
    erlang:send_after(60000, self(), metrics_cleanup),
    ok.

-spec emit_workflow_event(event_type(), workflow_id(), map()) -> ok.
emit_workflow_event(EventType, WorkflowId, Metadata) ->
    Event = #{
        event_type => EventType,
        workflow_id => WorkflowId,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    },
    case whereis(erlmcp_otel) of
        undefined -> ok;
        _ -> erlmcp_otel:emit_event(workflow, Event)
    end,
    logger:debug("Workflow event: ~p", [Event]),
    ok.

-spec emit_metric(binary(), metric_value(), [{atom(), term()}]) -> ok.
emit_metric(MetricName, Value, Attributes) ->
    Metric = #{
        name => MetricName,
        value => Value,
        attributes => Attributes,
        timestamp => erlang:system_time(millisecond)
    },
    case whereis(erlmcp_otel) of
        undefined -> ok;
        _ -> erlmcp_otel:emit_metric(Metric)
    end,
    logger:debug("Metric: ~p", [Metric]),
    ok.
