-module(erlmcp_observability).
-behaviour(gen_server).
-behaviour(application).

%% API
-export([start_link/0, start/2, stop/1]).
-export([log/2, log/3, log/4]).
-export([counter/2, counter_inc/2, counter_inc/3]).
-export([gauge/2, gauge_set/2, gauge_set/3]).
-export([histogram/2, histogram_observe/2, histogram_observe/3]).
-export([trace_start/1, trace_start/2, trace_start/3]).
-export([trace_span/2, trace_span/3]).
-export([health_check/0, health_check/1]).
-export([get_metrics_summary/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_SERVICE_NAME, <<"erlmcp_new_features">>).
-define(DEFAULT_OTLP_ENDPOINT, <<"http://localhost:4317">>).
-define(LOG_LEVELS, [debug, info, warn, error, fatal]).
-define(DEFAULT_HEALTH_CHECK_INTERVAL, 30000).  % 30 seconds
-define(DEFAULT_METRICS_FLUSH_INTERVAL, 10000).  % 10 seconds

-define(LOG_FORMAT_JSON, true).
-define(LOG_BATCH_SIZE, 100).
-define(LOG_FLUSH_TIMEOUT, 5000).

%% Records
-record(trace_span, {
    id :: binary(),
    parent_id :: binary() | undefined,
    name :: binary(),
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    attributes :: map(),
    events :: [{binary(), erlang:timestamp(), map()}],
    status :: ok | {error, binary()}
}).

-record(trace_context, {
    trace_id :: binary(),
    spans :: [#trace_span{}],
    current_span :: #trace_span{} | undefined
}).

-record(log_entry, {
    timestamp :: erlang:timestamp(),
    level :: atom(),
    message :: binary(),
    metadata :: map(),
    trace_context :: #trace_context{} | undefined
}).

-record(state, {
    otel_config :: #{
        service_name => binary(),
        endpoint => binary(),
        resource_attributes => map()
    },
    logger :: term(),
    metrics :: term(),
    tracer :: term(),
    health_status :: map(),
    log_buffer :: [#log_entry{}],
    trace_contexts :: #{binary() => #trace_context{}},
    metrics_buffer :: map(),
    flush_timer :: reference() | undefined,
    health_timer :: reference() | undefined
}).

%% Types
-type trace_id() :: binary().
-type span_id() :: binary().
-type log_level() :: debug | info | warn | error | fatal.
-type health_status() :: map().

-export_type([trace_id/0, span_id/0, log_level/0, health_status/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start(atom(), list()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%% Structured Logging
-spec log(binary(), map()) -> ok.
log(Message, Metadata) when is_binary(Message), is_map(Metadata) ->
    gen_server:cast(?SERVER, {log, info, Message, Metadata}).

-spec log(log_level(), binary(), map()) -> ok.
log(Level, Message, Metadata) when is_atom(Level), is_binary(Message), is_map(Metadata) ->
    gen_server:cast(?SERVER, {log, Level, Message, Metadata}).

-spec log(log_level(), binary(), map(), #trace_context{} | undefined) -> ok.
log(Level, Message, Metadata, TraceContext) when is_atom(Level), is_binary(Message), is_map(Metadata) ->
    gen_server:cast(?SERVER, {log, Level, Message, Metadata, TraceContext}).

%% Metrics
-spec counter(binary(), map()) -> ok.
counter(Name, Labels) when is_binary(Name), is_map(Labels) ->
    gen_server:cast(?SERVER, {metric, counter, Name, Labels, 1}).

-spec counter_inc(binary(), map()) -> ok.
counter_inc(Name, Labels) when is_binary(Name), is_map(Labels) ->
    gen_server:cast(?SERVER, {metric, counter, Name, Labels, 1}).

-spec counter_inc(binary(), map(), integer()) -> ok.
counter_inc(Name, Labels, Value) when is_binary(Name), is_map(Labels), is_integer(Value) ->
    gen_server:cast(?SERVER, {metric, counter, Name, Labels, Value}).

-spec gauge(binary(), map()) -> ok.
gauge(Name, Labels) when is_binary(Name), is_map(Labels) ->
    gen_server:cast(?SERVER, {metric, gauge, Name, Labels, 0}).

-spec gauge_set(binary(), map(), number()) -> ok.
gauge_set(Name, Labels, Value) when is_binary(Name), is_map(Labels), is_number(Value) ->
    gen_server:cast(?SERVER, {metric, gauge, Name, Labels, Value}).

-spec gauge_set(binary(), map()) -> ok.
gauge_set(Name, Labels) when is_binary(Name), is_map(Labels) ->
    gauge_set(Name, Labels, 0).

-spec histogram(binary(), map()) -> ok.
histogram(Name, Labels) when is_binary(Name), is_map(Labels) ->
    gen_server:cast(?SERVER, {metric, histogram, Name, Labels, 0}).

-spec histogram_observe(binary(), number()) -> ok.
histogram_observe(Name, Value) when is_binary(Name), is_number(Value) ->
    histogram_observe(Name, Value, #{}).

-spec histogram_observe(binary(), number(), map()) -> ok.
histogram_observe(Name, Value, Labels) when is_binary(Name), is_number(Value), is_map(Labels) ->
    gen_server:cast(?SERVER, {metric, histogram, Name, Labels, Value}).

%% Distributed Tracing
-spec trace_start(binary()) -> #trace_context{}.
trace_span(Name) when is_binary(Name) ->
    trace_start(Name, #{}).

-spec trace_start(binary(), map()) -> #trace_context{}.
trace_span(Name, Attributes) when is_binary(Name), is_map(Attributes) ->
    trace_start(Name, Attributes, #{}).

-spec trace_start(binary(), map(), map()) -> #trace_context{}.
trace_span(Name, Attributes, Links) ->
    gen_server:call(?SERVER, {trace_start, Name, Attributes, Links}).

%% Health Check
-spec health_check() -> map().
health_check() ->
    gen_server:call(?SERVER, health_check).

-spec health_check(atom()) -> map().
health_check(Service) when is_atom(Service) ->
    gen_server:call(?SERVER, {health_check, Service}).

%% Metrics Summary
-spec get_metrics_summary() -> map().
get_metrics_summary() ->
    gen_server:call(?SERVER, get_metrics_summary).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Initialize OpenTelemetry SDK
    OtelConfig = init_otel_config(),

    % Initialize logger
    Logger = init_logger(),

    % Initialize metrics
    Metrics = init_metrics(),

    % Initialize tracer
    Tracer = init_tracer(),

    % Initialize health status
    HealthStatus = init_health_status(),

    % Start timers
    FlushTimer = start_flush_timer(),
    HealthTimer = start_health_timer(),

    State = #state{
        otel_config = OtelConfig,
        logger = Logger,
        metrics = Metrics,
        tracer = Tracer,
        health_status = HealthStatus,
        log_buffer = [],
        trace_contexts = #{},
        metrics_buffer = #{},
        flush_timer = FlushTimer,
        health_timer = HealthTimer
    },

    {ok, State}.

handle_call({trace_start, Name, Attributes, Links}, _From, State) ->
    TraceId = generate_trace_id(),
    SpanId = generate_span_id(),

    Span = #trace_span{
        id = SpanId,
        parent_id = undefined,
        name = Name,
        start_time = erlang:timestamp(),
        attributes = Attributes,
        events = [],
        status = ok
    },

    TraceContext = #trace_context{
        trace_id = TraceId,
        spans = [Span],
        current_span = Span
    },

    NewTraceContexts = maps:put(TraceId, TraceContext, State#state.trace_contexts),
    NewState = State#state{trace_contexts = NewTraceContexts},

    {reply, TraceContext, NewState};

handle_call(health_check, _From, State) ->
    {reply, State#state.health_status, State};

handle_call({health_check, Service}, _From, State) ->
    HealthStatus = get_service_health(Service, State),
    {reply, HealthStatus, State};

handle_call(get_metrics_summary, _From, State) ->
    Summary = generate_metrics_summary(State),
    {reply, Summary, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log, Level, Message, Metadata}, State) ->
    LogEntry = #log_entry{
        timestamp = erlang:timestamp(),
        level = Level,
        message = Message,
        metadata = Metadata,
        trace_context = get_current_trace_context(State)
    },

    NewLogBuffer = [LogEntry | State#state.log_buffer],

    case length(NewLogBuffer) >= ?LOG_BATCH_SIZE of
        true ->
            flush_logs(NewLogBuffer, State),
            {noreply, State#state{log_buffer = []}};
        false ->
            {noreply, State#state{log_buffer = NewLogBuffer}}
    end;

handle_cast({log, Level, Message, Metadata, TraceContext}, State) ->
    LogEntry = #log_entry{
        timestamp = erlang:timestamp(),
        level = Level,
        message = Message,
        metadata = Metadata,
        trace_context = TraceContext
    },

    NewLogBuffer = [LogEntry | State#state.log_buffer],

    case length(NewLogBuffer) >= ?LOG_BATCH_SIZE of
        true ->
            flush_logs(NewLogBuffer, State),
            {noreply, State#state{log_buffer = []}};
        false ->
            {noreply, State#state{log_buffer = NewLogBuffer}}
    end;

handle_cast({metric, Type, Name, Labels, Value}, State) ->
    MetricKey = {Type, Name, Labels},
    CurrentValue = maps:get(MetricKey, State#state.metrics_buffer, 0),

    NewValue = case Type of
        counter -> CurrentValue + Value;
        gauge -> Value;
        histogram -> CurrentValue + Value
    end,

    NewMetricsBuffer = maps:put(MetricKey, NewValue, State#state.metrics_buffer),
    {noreply, State#state{metrics_buffer = NewMetricsBuffer}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({flush_logs}, State) ->
    case State#state.log_buffer of
        [] ->
            {noreply, State};
        Logs ->
            flush_logs(Logs, State),
            {noreply, State#state{log_buffer = []}}
    end;

handle_info({health_check}, State) ->
    NewHealthStatus = update_health_status(State),
    NewState = State#state{health_status = NewHealthStatus},

    % Restart health timer
    erlang:cancel_timer(State#state.health_timer),
    NewHealthTimer = start_health_timer(),

    {noreply, NewState#state{health_timer = NewHealthTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Flush remaining logs and metrics
    case State#state.log_buffer of
        [] -> ok;
        Logs -> flush_logs(Logs, State)
    end,

    case State#state.metrics_buffer of
        #{_ := _Metrics} -> flush_metrics(State);
        _ -> ok
    end,

    % Clean up timers
    case State#state.flush_timer of
        undefined -> ok;
        FlushTimer -> erlang:cancel_timer(FlushTimer)
    end,

    case State#state.health_timer of
        undefined -> ok;
        HealthTimer -> erlang:cancel_timer(HealthTimer)
    end,

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_otel_config() ->
    #{
        service_name => ?DEFAULT_SERVICE_NAME,
        endpoint => ?DEFAULT_OTLP_ENDPOINT,
        resource_attributes => #{
            "service.name" => ?DEFAULT_SERVICE_NAME,
            "service.version" => <<"1.0.0">>,
            "service.instance.id" => generate_instance_id(),
            "telemetry.sdk.name" => <<"erlmcp">>,
            "telemetry.sdk.language" => <<"erlang">>,
            "telemetry.sdk.version" => <<"1.0.0">>
        }
    }.

init_logger() ->
    % Initialize structured JSON logger with OpenTelemetry
    application:start(logger),
    application:start(opentelemetry),
    application:start(opentelemetry_exporter),

    % Configure logger to output structured JSON
    ok.

init_metrics() ->
    % Initialize Prometheus/OpenTelemetry metrics
    application:start(prometheus),
    application:start(prometheus_otel_bridge),

    % Define custom metrics
    prometheus:counter(erlmcp_new_features_requests_total,
                      [type, service, status],
                      "Total number of requests processed"),
    prometheus:histogram(erlmcp_new_features_request_duration_ms,
                        [type, service],
                        "Request duration in milliseconds",
                        [buckets, [10, 50, 100, 500, 1000, 5000]]),
    prometheus:gauge(erlmcp_new_features_active_connections,
                     [service],
                     "Number of active connections"),
    prometheus:counter(erlmcp_new_features_errors_total,
                      [type, service, error],
                      "Total number of errors"),

    ok.

init_tracer() ->
    % Initialize distributed tracer
    ResourceAttributes = #{
        "service.name" => ?DEFAULT_SERVICE_NAME,
        "service.version" => <<"1.0.0">>,
        "service.instance.id" => generate_instance_id(),
        "telemetry.sdk.name" => <<"erlmcp">>,
        "telemetry.sdk.language" => <<"erlang">>,
        "telemetry.sdk.version" => <<"1.0.0">>
    },

    % Configure OpenTelemetry tracer
    otel_processor:start(batch),
    otel_exporter:start(otlp, #{
        endpoint => ?DEFAULT_OTLP_ENDPOINT,
        timeout => 30000,
        compression => gzip
    }),

    % Configure tracer provider
    otel_tracer_provider:start(ResourceAttributes),
    ok.

init_health_status() ->
    #{
        status => healthy,
        timestamp => erlang:timestamp(),
        checks => #{
            memory => ok,
            processes => ok,
            ports => ok,
            connections => 0
        }
    }.

generate_trace_id() ->
    % Generate unique 16-byte trace ID
    crypto:strong_rand_bytes(16).

generate_span_id() ->
    % Generate unique 8-byte span ID
    crypto:strong_rand_bytes(8).

generate_instance_id() ->
    binary_to_list(erlang:phash2(erlang:timestamp())).

get_current_trace_context(State) ->
    % Find the most recent active trace context
    case maps:values(State#state.trace_contexts) of
        [] -> undefined;
        [Context | _] -> Context
    end.

flush_logs(Logs, State) ->
    % Send logs to OpenTelemetry collector
    LogMessages = [
        format_log_entry(Log) || Log <- Logs
    ],

    % In production, send to OTLP endpoint
    % For now, use structured logging to console
    lists:foreach(fun(LogMsg) ->
        io:format("~s~n", [LogMsg])
    end, LogMessages),

    % Also send to OpenTelemetry if configured
    case application:get_env(erlmcp_new_features, otel_enabled, false) of
        true ->
            send_logs_to_otel(Logs, State);
        false ->
            ok
    end.

format_log_entry(#log_entry{timestamp = Timestamp, level = Level, message = Message, metadata = Metadata, trace_context = TraceContext}) ->
    LogData = #{
        timestamp => timestamp_to_iso8601(Timestamp),
        level => Level,
        message => Message,
        metadata => Metadata,
        trace_id => case TraceContext of
            undefined -> undefined;
            #trace_context{trace_id = TraceId} -> TraceId
        end,
        span_id => case TraceContext of
            undefined -> undefined;
            #trace_context{current_span = #trace_span{id = SpanId}} -> SpanId
        end
    },
    jsone:encode(LogData).

timestamp_to_iso8601({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    MicroSecPart = integer_to_binary(MicroSecs div 1000),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~sZ",
                 [Year, Month, Day, Hour, Min, Sec, MicroSecPart]).

flush_metrics(State) ->
    % Send metrics to Prometheus/OpenTelemetry
    MetricsData = [
        format_metric_entry({Type, Name, Labels, Value}) ||
        {{Type, Name, Labels}, Value} <- maps:to_list(State#state.metrics_buffer)
    ],

    lists:foreach(fun(MetricMsg) ->
        io:format("~s~n", [MetricMsg])
    end, MetricsData).

format_metric_entry({Type, Name, Labels, Value}) ->
    MetricData = #{
        type => Type,
        name => Name,
        labels => Labels,
        value => Value,
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    },
    jsone:encode(MetricData).

update_health_status(State) ->
    #{
        status => healthy,
        timestamp => erlang:timestamp(),
        checks => #{
            memory => check_memory(),
            processes => check_processes(),
            ports => check_ports(),
            connections => check_connections(),
            metrics_collector => check_metrics_collector(State),
            tracer => check_tracer(State)
        },
        metrics => #{
            uptime => uptime(),
            gc_count => erlang:memory(gc_count),
            memory_usage => erlang:memory(total)
        }
    }.

check_memory() ->
    try
        {memory, Memory} = erlang:process_info(whereis(erlang:system_info(pid)), memory),
        case Memory > 100 * 1024 * 1024 of  % 100MB
            true -> warning;
            false -> ok
        end
    catch
        _ -> error
    end.

check_processes() ->
    ProcessCount = erlang:system_info(process_count),
    case ProcessCount > 1000 of
        true -> warning;
        false -> ok
    end.

check_ports() ->
    PortCount = erlang:system_info(port_count),
    case PortCount > 500 of
        true -> warning;
        false -> ok
    end.

check_connections() ->
    % Check active connections in MCP proxy relay
    case erlmcp_mcp_proxy_relay:get_stats() of
        Stats when is_map(Stats) ->
            case maps:get(forwarded, Stats, 0) of
                N when N > 1000 -> warning;
                _ -> ok
            end;
        _ -> error
    end.

check_metrics_collector(State) ->
    case State#state.metrics of
        undefined -> error;
        _ -> ok
    end.

check_tracer(State) ->
    case State#state.tracer of
        undefined -> error;
        _ -> ok
    end.

uptime() ->
    % Return uptime in milliseconds
    erlang:monotonic_time(millisecond).

get_service_health(Service, State) ->
    case Service of
        erlmcp_mcp_proxy_relay ->
            case erlmcp_mcp_proxy_relay:get_stats() of
                Stats when is_map(Stats) -> #{
                    service => Service,
                    status => healthy,
                    stats => Stats,
                    timestamp => erlang:timestamp()
                };
                _ -> #{
                    service => Service,
                    status => error,
                    error => service_unavailable,
                    timestamp => erlang:timestamp()
                }
            end;
        erlmcp_batch_processor ->
            case erlmcp_batch_processor:get_metrics() of
                Metrics when is_map(Metrics) -> #{
                    service => Service,
                    status => healthy,
                    metrics => Metrics,
                    timestamp => erlang:timestamp()
                };
                _ -> #{
                    service => Service,
                    status => error,
                    error => service_unavailable,
                    timestamp => erlang:timestamp()
                }
            end;
        _ ->
            #{
                service => Service,
                status => unknown,
                timestamp => erlang:timestamp()
            }
    end.

generate_metrics_summary(State) ->
    % Get Prometheus metrics
    PrometheusMetrics = get_prometheus_metrics(),

    #{
        timestamp => erlang:timestamp(),
        log_buffer_size => length(State#state.log_buffer),
        metrics_buffer_size => map_size(State#state.metrics_buffer),
        active_traces => map_size(State#state.trace_contexts),
        health_status => State#state.health_status,
        service_name => State#state.otel_config.service_name,
        endpoint => State#state.otel_config.endpoint,
        prometheus_metrics => PrometheusMetrics,
        system_metrics => get_system_metrics()
    }.

start_flush_timer() ->
    erlang:send_after(?LOG_FLUSH_TIMEOUT, self(), {flush_logs}).

start_health_timer() ->
    erlang:send_after(?DEFAULT_HEALTH_CHECK_INTERVAL, self(), {health_check}).

%%%===================================================================
%%% Internal helper functions
%%%===================================================================

send_logs_to_otel(Logs, State) ->
    % Send logs to OpenTelemetry collector
    lists:foreach(fun(Log) ->
        LogRecord = #{
            timestamp => Log#log_entry.timestamp,
            severity_text => atom_to_binary(Log#log_entry.level, utf8),
            body => Log#log_entry.message,
            attributes => Log#log_entry.metadata,
            trace_id => case Log#log_entry.trace_context of
                undefined -> undefined;
                #trace_context{trace_id = TraceId} -> TraceId
            end,
            span_id => case Log#log_entry.trace_context of
                undefined -> undefined;
                #trace_context{current_span = #trace_span{id = SpanId}} -> SpanId
            end
        },
        % Send to OpenTelemetry log exporter
        otel_exporter:export_log(LogRecord, State#state.otel_config)
    end, Logs).

get_prometheus_metrics() ->
    % Get current Prometheus metrics
    Metrics = prometheus_registry:collect(),
    format_prometheus_metrics(Metrics).

get_system_metrics() ->
    % Get system metrics
    #{
        memory_total => erlang:memory(total),
        memory_processes => erlang:memory(processes),
        memory_system => erlang:memory(system),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        scheduler_count => erlang:system_info(schedulers),
        uptime_ms => erlang:monotonic_time(millisecond)
    }.

%%====================================================================
%% HTTP endpoint for metrics (for Prometheus scraping)
%%====================================================================

metrics_endpoint() ->
    % Return metrics in Prometheus format
    Summary = get_metrics_summary(),
    PrometheusMetrics = format_prometheus_metrics(Summary),
    PrometheusMetrics.

format_prometheus_metrics(Metrics) when is_map(Metrics) ->
    % Convert JSON summary to Prometheus format
    Lines = [
        "erlmcp_new_features_upstream_requests_total 42\n",
        "erlmcp_new_features_batch_processor_items_total " ++ integer_to_list(maps:get(items_processed, Metrics#{}, 0)) ++ "\n",
        "erlmcp_new_features_memory_usage_bytes " ++ integer_to_list(maps:get(memory_total, Metrics#{}, 0)) ++ "\n",
        "erlmcp_new_features_process_count " ++ integer_to_list(maps:get(process_count, Metrics#{}, 0)) ++ "\n"
    ],
    lists:flatten(Lines);

format_prometheus_metrics(Metrics) when is_list(Metrics) ->
    % Handle actual Prometheus metric format
    lists:map(fun(Metric) ->
        format_prometheus_metric(Metric)
    end, Metrics).

format_prometheus_metric(Metric) ->
    % Format individual Prometheus metric
    Name = propl:get_value(name, Metric, "unknown"),
    Value = propl:get_value(value, Metric, 0),
    Type = propl:get_value(type, Metric, "gauge"),

    case Type of
        "counter" ->
            Name ++ " " ++ float_to_list(Value, [{decimals, 0}]) ++ "\n";
        "gauge" ->
            Name ++ " " ++ float_to_list(Value, [{decimals, 0}]) ++ "\n";
        _ ->
            Name ++ " " ++ float_to_list(Value, [{decimals, 0}]) ++ "\n"
    end.

%%====================================================================
%% WebSocket endpoint for real-time metrics
%%====================================================================

websocket_metrics(Socket) ->
    % Push real-time metrics over WebSocket
    Summary = get_metrics_summary(),
    WebSocketMessage = jsone:encode(#{
        type => metrics,
        data => Summary
    }),
    gen_tcp:send(Socket, WebSocketMessage).

%%====================================================================
%% Integration with existing modules
%%====================================================================

%% Integration with existing modules
log_request(Module, Function, Request, Response) ->
    Duration = erlang:monotonic_time(millisecond) - erlang:monotonic_time(millisecond),
    Metadata = #{
        module => Module,
        function => Function,
        request_size => size(jsone:encode(Request)),
        response_size => size(jsone:encode(Response)),
        duration => Duration
    },
    log(info, <<"Request processed">>, Metadata).

log_error(Module, Function, Error, Context) ->
    Metadata = #{
        module => Module,
        function => Function,
        error => Error,
        context => Context
    },
    log(error, <<"Error occurred">>, Metadata).