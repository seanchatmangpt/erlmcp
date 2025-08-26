%%%-------------------------------------------------------------------
%%% @doc Chaos Engineering Monitor
%%% Monitors system behavior and metrics during chaos experiments
%%% to ensure observability and proper experiment tracking.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([start_monitoring/2, stop_monitoring/1]).
-export([get_metrics/1, get_real_time_metrics/1]).
-export([set_alert_thresholds/2, get_alerts/1]).
-export([export_metrics/2, generate_report/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-record(state, {
    monitors = #{},          % Active monitoring sessions
    metrics_history = #{},   % Historical metrics data
    alert_thresholds = #{},  % Alert configuration
    active_alerts = #{},     % Current alerts
    config = #{}            % Monitor configuration
}).

-record(monitor_session, {
    id,
    experiment_id,
    start_time,
    end_time,
    interval_ms = 1000,
    metrics = [],
    alerts = [],
    status = active
}).

-record(system_metrics, {
    timestamp,
    cpu_usage,
    memory_usage,
    disk_io,
    network_io,
    process_count,
    message_queue_lengths,
    gc_metrics,
    scheduler_utilization,
    error_rates,
    response_times
}).

-record(chaos_alert, {
    id,
    type,
    severity,
    message,
    timestamp,
    threshold_value,
    actual_value,
    acknowledged = false
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link([]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Start monitoring for a chaos experiment
start_monitoring(ExperimentId, MonitorConfig) ->
    gen_server:call(?MODULE, {start_monitoring, ExperimentId, MonitorConfig}).

%% @doc Stop monitoring session
stop_monitoring(MonitorId) ->
    gen_server:call(?MODULE, {stop_monitoring, MonitorId}).

%% @doc Get collected metrics for monitoring session
get_metrics(MonitorId) ->
    gen_server:call(?MODULE, {get_metrics, MonitorId}).

%% @doc Get real-time metrics snapshot
get_real_time_metrics(MetricTypes) ->
    gen_server:call(?MODULE, {get_real_time_metrics, MetricTypes}).

%% @doc Set alert thresholds for monitoring
set_alert_thresholds(MonitorId, Thresholds) ->
    gen_server:call(?MODULE, {set_alert_thresholds, MonitorId, Thresholds}).

%% @doc Get active alerts
get_alerts(MonitorId) ->
    gen_server:call(?MODULE, {get_alerts, MonitorId}).

%% @doc Export metrics to specified format
export_metrics(MonitorId, Format) ->
    gen_server:call(?MODULE, {export_metrics, MonitorId, Format}).

%% @doc Generate comprehensive monitoring report
generate_report(MonitorId) ->
    gen_server:call(?MODULE, {generate_report, MonitorId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
    process_flag(trap_exit, true),
    schedule_system_monitoring(),
    {ok, #state{config = maps:from_list(Config)}}.

handle_call({start_monitoring, ExperimentId, MonitorConfig}, _From, State) ->
    MonitorId = generate_monitor_id(),
    
    SpanCtx = otel_tracer:start_span(<<"chaos_monitor.start">>),
    otel_span:set_attributes(SpanCtx, [
        {<<"monitor.id">>, MonitorId},
        {<<"experiment.id">>, ExperimentId},
        {<<"monitor.interval_ms">>, maps:get(interval_ms, MonitorConfig, 1000)}
    ]),
    
    try
        Monitor = #monitor_session{
            id = MonitorId,
            experiment_id = ExperimentId,
            start_time = erlang:system_time(millisecond),
            interval_ms = maps:get(interval_ms, MonitorConfig, 1000),
            metrics = [],
            alerts = [],
            status = active
        },
        
        % Schedule regular metric collection
        schedule_metric_collection(MonitorId, Monitor#monitor_session.interval_ms),
        
        NewMonitors = maps:put(MonitorId, Monitor, State#state.monitors),
        
        otel_span:set_status(SpanCtx, {ok, <<"Monitoring started successfully">>}),
        {reply, {ok, MonitorId}, State#state{monitors = NewMonitors}}
    catch
        Error:Reason:Stack ->
            otel_span:record_exception(SpanCtx, Error, Reason, Stack),
            otel_span:set_status(SpanCtx, {error, Reason}),
            {reply, {error, {monitoring_start_failed, Error, Reason}}, State}
    after
        otel_span:end_span(SpanCtx)
    end;

handle_call({stop_monitoring, MonitorId}, _From, State) ->
    case maps:get(MonitorId, State#state.monitors, undefined) of
        undefined ->
            {reply, {error, monitor_not_found}, State};
        Monitor ->
            UpdatedMonitor = Monitor#monitor_session{
                end_time = erlang:system_time(millisecond),
                status = completed
            },
            
            % Cancel metric collection timer
            cancel_metric_collection(MonitorId),
            
            NewMonitors = maps:put(MonitorId, UpdatedMonitor, State#state.monitors),
            {reply, ok, State#state{monitors = NewMonitors}}
    end;

handle_call({get_metrics, MonitorId}, _From, State) ->
    case maps:get(MonitorId, State#state.monitors, undefined) of
        undefined ->
            {reply, {error, monitor_not_found}, State};
        Monitor ->
            {reply, {ok, Monitor#monitor_session.metrics}, State}
    end;

handle_call({get_real_time_metrics, MetricTypes}, _From, State) ->
    SpanCtx = otel_tracer:start_span(<<"chaos_monitor.real_time_metrics">>),
    otel_span:set_attributes(SpanCtx, [
        {<<"metrics.types">>, length(MetricTypes)}
    ]),
    
    try
        Metrics = collect_real_time_metrics(MetricTypes),
        otel_span:set_status(SpanCtx, {ok, <<"Real-time metrics collected">>}),
        {reply, {ok, Metrics}, State}
    catch
        Error:Reason:Stack ->
            otel_span:record_exception(SpanCtx, Error, Reason, Stack),
            otel_span:set_status(SpanCtx, {error, Reason}),
            {reply, {error, {metrics_collection_failed, Error, Reason}}, State}
    after
        otel_span:end_span(SpanCtx)
    end;

handle_call({set_alert_thresholds, MonitorId, Thresholds}, _From, State) ->
    NewThresholds = maps:put(MonitorId, Thresholds, State#state.alert_thresholds),
    {reply, ok, State#state{alert_thresholds = NewThresholds}};

handle_call({get_alerts, MonitorId}, _From, State) ->
    case maps:get(MonitorId, State#state.monitors, undefined) of
        undefined ->
            {reply, {error, monitor_not_found}, State};
        Monitor ->
            {reply, {ok, Monitor#monitor_session.alerts}, State}
    end;

handle_call({export_metrics, MonitorId, Format}, _From, State) ->
    case maps:get(MonitorId, State#state.monitors, undefined) of
        undefined ->
            {reply, {error, monitor_not_found}, State};
        Monitor ->
            case export_metrics_to_format(Monitor, Format) of
                {ok, ExportedData} ->
                    {reply, {ok, ExportedData}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({generate_report, MonitorId}, _From, State) ->
    case maps:get(MonitorId, State#state.monitors, undefined) of
        undefined ->
            {reply, {error, monitor_not_found}, State};
        Monitor ->
            Report = generate_monitoring_report(Monitor),
            {reply, {ok, Report}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({collect_metrics, MonitorId}, State) ->
    case maps:get(MonitorId, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        Monitor when Monitor#monitor_session.status =:= active ->
            NewMonitor = collect_and_store_metrics(Monitor),
            
            % Check for alerts
            UpdatedMonitor = check_alert_conditions(NewMonitor, State#state.alert_thresholds),
            
            % Schedule next collection
            schedule_metric_collection(MonitorId, Monitor#monitor_session.interval_ms),
            
            NewMonitors = maps:put(MonitorId, UpdatedMonitor, State#state.monitors),
            {noreply, State#state{monitors = NewMonitors}};
        _Monitor ->
            % Monitor is not active, don't schedule next collection
            {noreply, State}
    end;

handle_info(system_monitoring_tick, State) ->
    % Collect system-wide metrics
    schedule_system_monitoring(),
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_monitor_id() ->
    erlang:system_time(microsecond).

schedule_metric_collection(MonitorId, IntervalMs) ->
    timer:send_after(IntervalMs, self(), {collect_metrics, MonitorId}).

cancel_metric_collection(_MonitorId) ->
    % Implementation for canceling specific timer would go here
    ok.

schedule_system_monitoring() ->
    timer:send_after(5000, self(), system_monitoring_tick).

collect_real_time_metrics(MetricTypes) ->
    Timestamp = erlang:system_time(millisecond),
    
    Metrics = #system_metrics{
        timestamp = Timestamp,
        cpu_usage = collect_cpu_metrics(),
        memory_usage = collect_memory_metrics(),
        disk_io = collect_disk_metrics(),
        network_io = collect_network_metrics(),
        process_count = collect_process_metrics(),
        message_queue_lengths = collect_message_queue_metrics(),
        gc_metrics = collect_gc_metrics(),
        scheduler_utilization = collect_scheduler_metrics(),
        error_rates = collect_error_metrics(),
        response_times = collect_response_time_metrics()
    },
    
    % Filter metrics based on requested types
    filter_metrics_by_type(Metrics, MetricTypes).

collect_and_store_metrics(Monitor) ->
    Metrics = collect_real_time_metrics([all]),
    UpdatedMetrics = [Metrics | Monitor#monitor_session.metrics],
    Monitor#monitor_session{metrics = UpdatedMetrics}.

check_alert_conditions(Monitor, AlertThresholds) ->
    case maps:get(Monitor#monitor_session.id, AlertThresholds, undefined) of
        undefined ->
            Monitor;
        Thresholds ->
            LatestMetrics = case Monitor#monitor_session.metrics of
                [] -> #system_metrics{timestamp = erlang:system_time(millisecond)};
                [Latest | _] -> Latest
            end,
            
            NewAlerts = check_thresholds(LatestMetrics, Thresholds),
            Monitor#monitor_session{alerts = NewAlerts ++ Monitor#monitor_session.alerts}
    end.

check_thresholds(Metrics, Thresholds) ->
    lists:foldl(fun({ThresholdType, ThresholdValue}, Alerts) ->
        case evaluate_threshold(Metrics, ThresholdType, ThresholdValue) of
            {alert, ActualValue} ->
                Alert = #chaos_alert{
                    id = generate_alert_id(),
                    type = ThresholdType,
                    severity = determine_severity(ThresholdType, ThresholdValue, ActualValue),
                    message = format_alert_message(ThresholdType, ThresholdValue, ActualValue),
                    timestamp = erlang:system_time(millisecond),
                    threshold_value = ThresholdValue,
                    actual_value = ActualValue,
                    acknowledged = false
                },
                [Alert | Alerts];
            ok ->
                Alerts
        end
    end, [], maps:to_list(Thresholds)).

evaluate_threshold(Metrics, cpu_usage, Threshold) ->
    case Metrics#system_metrics.cpu_usage of
        undefined -> ok;
        CpuUsage when CpuUsage > Threshold -> {alert, CpuUsage};
        _ -> ok
    end;
evaluate_threshold(Metrics, memory_usage, Threshold) ->
    case Metrics#system_metrics.memory_usage of
        undefined -> ok;
        MemUsage when MemUsage > Threshold -> {alert, MemUsage};
        _ -> ok
    end;
evaluate_threshold(Metrics, error_rate, Threshold) ->
    case Metrics#system_metrics.error_rates of
        undefined -> ok;
        ErrorRate when ErrorRate > Threshold -> {alert, ErrorRate};
        _ -> ok
    end;
evaluate_threshold(Metrics, response_time, Threshold) ->
    case Metrics#system_metrics.response_times of
        undefined -> ok;
        RespTime when RespTime > Threshold -> {alert, RespTime};
        _ -> ok
    end;
evaluate_threshold(_Metrics, _Type, _Threshold) ->
    ok.

determine_severity(_Type, Threshold, Actual) when Actual > Threshold * 2 ->
    critical;
determine_severity(_Type, Threshold, Actual) when Actual > Threshold * 1.5 ->
    high;
determine_severity(_Type, _Threshold, _Actual) ->
    medium.

format_alert_message(Type, Threshold, Actual) ->
    io_lib:format("~p threshold exceeded: ~p (threshold: ~p)", [Type, Actual, Threshold]).

generate_alert_id() ->
    erlang:system_time(microsecond).

export_metrics_to_format(Monitor, json) ->
    try
        JsonData = metrics_to_json(Monitor#monitor_session.metrics),
        {ok, JsonData}
    catch
        Error:Reason ->
            {error, {json_export_failed, Error, Reason}}
    end;
export_metrics_to_format(Monitor, csv) ->
    try
        CsvData = metrics_to_csv(Monitor#monitor_session.metrics),
        {ok, CsvData}
    catch
        Error:Reason ->
            {error, {csv_export_failed, Error, Reason}}
    end;
export_metrics_to_format(_Monitor, Format) ->
    {error, {unsupported_format, Format}}.

generate_monitoring_report(Monitor) ->
    #{
        monitor_id => Monitor#monitor_session.id,
        experiment_id => Monitor#monitor_session.experiment_id,
        duration_ms => case Monitor#monitor_session.end_time of
            undefined -> erlang:system_time(millisecond) - Monitor#monitor_session.start_time;
            EndTime -> EndTime - Monitor#monitor_session.start_time
        end,
        metrics_count => length(Monitor#monitor_session.metrics),
        alerts_count => length(Monitor#monitor_session.alerts),
        status => Monitor#monitor_session.status,
        summary => generate_metrics_summary(Monitor#monitor_session.metrics),
        alerts_summary => generate_alerts_summary(Monitor#monitor_session.alerts)
    }.

%% Metric collection functions
collect_cpu_metrics() ->
    case cpu_sup:avg1() of
        {error, _} -> undefined;
        Value -> Value / 256 * 100  % Convert to percentage
    end.

collect_memory_metrics() ->
    case erlang:memory() of
        Memory when is_list(Memory) ->
            Total = proplists:get_value(total, Memory, 0),
            System = proplists:get_value(system, Memory, 0),
            #{total => Total, system => System, usage_percent => (System / Total) * 100};
        _ ->
            undefined
    end.

collect_disk_metrics() ->
    % Platform-specific disk I/O metrics would go here
    #{read_ops => 0, write_ops => 0, read_bytes => 0, write_bytes => 0}.

collect_network_metrics() ->
    % Network I/O metrics would go here
    #{bytes_sent => 0, bytes_received => 0, connections => 0}.

collect_process_metrics() ->
    #{
        process_count => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),
        port_count => erlang:system_info(port_count),
        port_limit => erlang:system_info(port_limit)
    }.

collect_message_queue_metrics() ->
    Processes = erlang:processes(),
    QueueLengths = [element(2, erlang:process_info(Pid, message_queue_len)) || 
                   Pid <- Processes, 
                   erlang:process_info(Pid, message_queue_len) =/= undefined],
    case QueueLengths of
        [] -> #{max => 0, avg => 0, total => 0};
        _ ->
            Total = lists:sum(QueueLengths),
            Max = lists:max(QueueLengths),
            Avg = Total / length(QueueLengths),
            #{max => Max, avg => Avg, total => Total}
    end.

collect_gc_metrics() ->
    GcInfo = erlang:statistics(garbage_collection),
    #{
        number_of_gcs => element(1, GcInfo),
        words_reclaimed => element(2, GcInfo),
        time_spent_ms => element(3, GcInfo)
    }.

collect_scheduler_metrics() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined -> undefined;
        SchedulerTimes ->
            TotalTime = lists:sum([Active + Total || {_, Active, Total} <- SchedulerTimes]),
            ActiveTime = lists:sum([Active || {_, Active, _} <- SchedulerTimes]),
            case TotalTime of
                0 -> 0;
                _ -> (ActiveTime / TotalTime) * 100
            end
    end.

collect_error_metrics() ->
    % Error rate metrics would be collected from application-specific sources
    #{error_rate_per_minute => 0, total_errors => 0}.

collect_response_time_metrics() ->
    % Response time metrics would be collected from application-specific sources
    #{avg_response_time_ms => 0, p95_response_time_ms => 0, p99_response_time_ms => 0}.

filter_metrics_by_type(Metrics, [all]) ->
    Metrics;
filter_metrics_by_type(Metrics, Types) ->
    % Filter metrics based on requested types
    FilteredMetrics = #system_metrics{timestamp = Metrics#system_metrics.timestamp},
    lists:foldl(fun(Type, Acc) ->
        case Type of
            cpu_usage -> Acc#system_metrics{cpu_usage = Metrics#system_metrics.cpu_usage};
            memory_usage -> Acc#system_metrics{memory_usage = Metrics#system_metrics.memory_usage};
            disk_io -> Acc#system_metrics{disk_io = Metrics#system_metrics.disk_io};
            network_io -> Acc#system_metrics{network_io = Metrics#system_metrics.network_io};
            process_count -> Acc#system_metrics{process_count = Metrics#system_metrics.process_count};
            message_queue_lengths -> Acc#system_metrics{message_queue_lengths = Metrics#system_metrics.message_queue_lengths};
            gc_metrics -> Acc#system_metrics{gc_metrics = Metrics#system_metrics.gc_metrics};
            scheduler_utilization -> Acc#system_metrics{scheduler_utilization = Metrics#system_metrics.scheduler_utilization};
            error_rates -> Acc#system_metrics{error_rates = Metrics#system_metrics.error_rates};
            response_times -> Acc#system_metrics{response_times = Metrics#system_metrics.response_times};
            _ -> Acc
        end
    end, FilteredMetrics, Types).

metrics_to_json(Metrics) ->
    % Convert metrics to JSON format
    jsx:encode([metric_to_map(M) || M <- Metrics]).

metrics_to_csv(Metrics) ->
    % Convert metrics to CSV format
    Header = "timestamp,cpu_usage,memory_usage,process_count,error_rate,response_time\n",
    Rows = [metric_to_csv_row(M) || M <- Metrics],
    Header ++ lists:flatten(Rows).

metric_to_map(Metric) ->
    #{
        timestamp => Metric#system_metrics.timestamp,
        cpu_usage => Metric#system_metrics.cpu_usage,
        memory_usage => Metric#system_metrics.memory_usage,
        disk_io => Metric#system_metrics.disk_io,
        network_io => Metric#system_metrics.network_io,
        process_count => Metric#system_metrics.process_count,
        message_queue_lengths => Metric#system_metrics.message_queue_lengths,
        gc_metrics => Metric#system_metrics.gc_metrics,
        scheduler_utilization => Metric#system_metrics.scheduler_utilization,
        error_rates => Metric#system_metrics.error_rates,
        response_times => Metric#system_metrics.response_times
    }.

metric_to_csv_row(Metric) ->
    io_lib:format("~p,~p,~p,~p,~p,~p~n", [
        Metric#system_metrics.timestamp,
        Metric#system_metrics.cpu_usage,
        format_memory_usage(Metric#system_metrics.memory_usage),
        format_process_count(Metric#system_metrics.process_count),
        format_error_rate(Metric#system_metrics.error_rates),
        format_response_time(Metric#system_metrics.response_times)
    ]).

format_memory_usage(undefined) -> 0;
format_memory_usage(#{usage_percent := Percent}) -> Percent;
format_memory_usage(_) -> 0.

format_process_count(undefined) -> 0;
format_process_count(#{process_count := Count}) -> Count;
format_process_count(_) -> 0.

format_error_rate(undefined) -> 0;
format_error_rate(#{error_rate_per_minute := Rate}) -> Rate;
format_error_rate(_) -> 0.

format_response_time(undefined) -> 0;
format_response_time(#{avg_response_time_ms := Time}) -> Time;
format_response_time(_) -> 0.

generate_metrics_summary(Metrics) ->
    case Metrics of
        [] -> #{count => 0};
        _ ->
            #{
                count => length(Metrics),
                duration_ms => calculate_metrics_duration(Metrics),
                avg_cpu_usage => calculate_avg_cpu_usage(Metrics),
                avg_memory_usage => calculate_avg_memory_usage(Metrics),
                max_process_count => calculate_max_process_count(Metrics)
            }
    end.

generate_alerts_summary(Alerts) ->
    case Alerts of
        [] -> #{count => 0};
        _ ->
            #{
                count => length(Alerts),
                critical_count => count_alerts_by_severity(Alerts, critical),
                high_count => count_alerts_by_severity(Alerts, high),
                medium_count => count_alerts_by_severity(Alerts, medium),
                acknowledged_count => count_acknowledged_alerts(Alerts)
            }
    end.

calculate_metrics_duration([]) -> 0;
calculate_metrics_duration([Latest | Rest]) ->
    case lists:last(Rest) of
        Oldest -> Latest#system_metrics.timestamp - Oldest#system_metrics.timestamp;
        _ -> 0
    end.

calculate_avg_cpu_usage(Metrics) ->
    CpuValues = [M#system_metrics.cpu_usage || M <- Metrics, 
                 M#system_metrics.cpu_usage =/= undefined],
    case CpuValues of
        [] -> 0;
        _ -> lists:sum(CpuValues) / length(CpuValues)
    end.

calculate_avg_memory_usage(Metrics) ->
    MemValues = [maps:get(usage_percent, M#system_metrics.memory_usage, 0) || 
                 M <- Metrics, M#system_metrics.memory_usage =/= undefined],
    case MemValues of
        [] -> 0;
        _ -> lists:sum(MemValues) / length(MemValues)
    end.

calculate_max_process_count(Metrics) ->
    ProcessCounts = [maps:get(process_count, M#system_metrics.process_count, 0) || 
                     M <- Metrics, M#system_metrics.process_count =/= undefined],
    case ProcessCounts of
        [] -> 0;
        _ -> lists:max(ProcessCounts)
    end.

count_alerts_by_severity(Alerts, Severity) ->
    length([A || A <- Alerts, A#chaos_alert.severity =:= Severity]).

count_acknowledged_alerts(Alerts) ->
    length([A || A <- Alerts, A#chaos_alert.acknowledged =:= true]).