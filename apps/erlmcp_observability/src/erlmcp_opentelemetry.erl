%%%-------------------------------------------------------------------
%%% @doc
%%% OpenTelemetry instrumentation module for erlmcp v3
%%% Provides comprehensive telemetry collection with distributed tracing,
%%% metrics, and logging for enterprise-grade observability
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_opentelemetry).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         instrument_request/3,
         instrument_span/3,
         record_metric/3,
         record_exception/3,
         get_tracer/1,
         get_meter/1,
         export_trace/0,
         export_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_EXPORT_INTERVAL, 5000).
-define(MAX_SPANS, 1000).
-define(DEFAULT_ATTRIBUTES, #{
    service => "erlmcp",
    version => ?ERLMCP_VERSION,
    environment => <<"production">>
}).

-include_lib("erlmcp_observability/include/erlmcp_observability.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the OpenTelemetry service with default configuration
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(#{
        export_interval => ?DEFAULT_EXPORT_INTERVAL,
        max_spans => ?MAX_SPANS,
        enable_tracing => true,
        enable_metrics => true,
        enable_logging => true
    }).

%%--------------------------------------------------------------------
%% @doc
%% Start the OpenTelemetry service with custom configuration
%% @end
%%--------------------------------------------------------------------
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc
%% Instrument an incoming request with automatic span creation
%% @end
%%--------------------------------------------------------------------
-spec instrument_request(binary(), map(), fun()) -> term().
instrument_request(SpanName, Attributes, Fun) ->
    instrument_span(SpanName, Attributes, Fun).

%%--------------------------------------------------------------------
%% @doc
%% Create and execute a span with the given name and attributes
%% @end
%%--------------------------------------------------------------------
-spec instrument_span(binary(), map(), fun()) -> term().
instrument_span(SpanName, Attributes, Fun) ->
    Tracer = get_tracer(erlmcp),
    ParentContext = opentelemetry:get_current_context(),
    SpanCtx = opentelemetry:start_span(Tracer, SpanName, #{attributes => Attributes}),

    try
        %% Set span context
        opentelemetry:set_current_context(SpanCtx),

        %% Execute function
        Result = Fun(),

        %% Mark span as successful
        opentelemetry:set_status(SpanCtx, ok),
        Result
    catch
        Error:Reason ->
            %% Mark span as failed
            opentelemetry:set_status(SpanCtx, {error, Reason}),
            opentelemetry:record_exception(SpanCtx, Error, Reason, []),
            throw(Error)
    after
        %% End span
        opentelemetry:end_span(SpanCtx),
        %% Restore parent context
        opentelemetry:set_current_context(ParentContext)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Record a metric with the given name, value, and attributes
%% @end
%%--------------------------------------------------------------------
-spec record_metric(binary(), number(), map()) -> ok.
record_metric(Name, Value, Attributes) ->
    Meter = get_meter(erlmcp),
    Meter:record(Name, Value, Attributes).

%%--------------------------------------------------------------------
%% @doc
%% Record an exception for the current span
%% @end
%%--------------------------------------------------------------------
-spec record_exception(term(), term(), map()) -> ok.
record_exception(Exception, Reason, Attributes) ->
    CurrentCtx = opentelemetry:get_current_context(),
    opentelemetry:record_exception(CurrentCtx, Exception, Reason, Attributes).

%%--------------------------------------------------------------------
%% @doc
%% Get a tracer for the given service name
%% @end
%%--------------------------------------------------------------------
-spec get_tracer(binary()) -> any().
get_tracer(ServiceName) ->
    case erlang:get({tracer, ServiceName}) of
        undefined ->
            Tracer = opentelemetry:get_tracer(ServiceName),
            erlang:put({tracer, ServiceName}, Tracer),
            Tracer;
        Tracer ->
            Tracer
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get a meter for the given service name
%% @end
%%--------------------------------------------------------------------
-spec get_meter(binary()) -> any().
get_meter(ServiceName) ->
    case erlang:get({meter, ServiceName}) of
        undefined ->
            Meter = opentelemetry:get_meter(ServiceName),
            erlang:put({meter, ServiceName}, Meter),
            Meter;
        Meter ->
            Meter
    end.

%%--------------------------------------------------------------------
%% @doc
%% Export traces to the configured exporter
%% @end
%%--------------------------------------------------------------------
export_trace() ->
    gen_server:call(?SERVER, export_trace).

%%--------------------------------------------------------------------
%% @doc
%% Export metrics to the configured exporter
%% @end
%%--------------------------------------------------------------------
export_metrics() ->
    gen_server:call(?SERVER, export_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    %% Initialize OpenTelemetry components
    init_config(Config),
    init_exporters(Config),
    init_instrumentation(),

    %% Start periodic export
    ExportInterval = maps:get(export_interval, Config, ?DEFAULT_EXPORT_INTERVAL),
    erlang:send_after(ExportInterval, self(), export_metrics),
    erlang:send_after(ExportInterval, self(), export_trace),

    State = #{
        config => Config,
        spans => #{},
        metrics => #{},
        exporters => #{}
    },

    {ok, State}.

handle_call(export_trace, _From, State) ->
    Exporters = maps:get(exporters, State),
    lists:foreach(fun(Exporter) ->
        case Exporter of
            {trace, Tracer} ->
                opentelemetry:export(Tracer);
            _ -> ok
        end
    end, maps:values(Exporters)),
    {reply, ok, State};

handle_call(export_metrics, _From, State) ->
    Exporters = maps:get(exporters, State),
    lists:foreach(fun(Exporter) ->
        case Exporter of
            {metrics, Meter} ->
                opentelemetry:export(Meter);
            _ -> ok
        end
    end, maps:values(Exporters)),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(export_metrics, State) ->
    %% Export metrics
    export_metrics(),

    %% Schedule next export
    ExportInterval = maps:get(export_interval, maps:get(config, State), ?DEFAULT_EXPORT_INTERVAL),
    erlang:send_after(ExportInterval, self(), export_metrics),
    {noreply, State};

handle_info(export_trace, State) ->
    %% Export traces
    export_trace(),

    %% Schedule next export
    ExportInterval = maps:get(export_interval, maps:get(config, State), ?DEFAULT_EXPORT_INTERVAL),
    erlang:send_after(ExportInterval, self(), export_trace),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Shutdown exporters
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

init_config(Config) ->
    %% Set up OpenTelemetry configuration
    otel_batch_processor:init(),

    %% Set global attributes
    GlobalAttributes = maps:merge(?DEFAULT_ATTRIBUTES, Config),
    opentelemetry:set_global_attributes(GlobalAttributes).

init_exporters(Config) ->
    %% Initialize trace exporters
    case maps:get(enable_tracing, Config, true) of
        true ->
            %% OTLP exporter
            init_otlp_exporter(Config);
        false ->
            %% No tracing
            ok
    end,

    %% Initialize metric exporters
    case maps:get(enable_metrics, Config, true) of
        true ->
            init_metrics_exporter(Config);
        false ->
            %% No metrics
            ok
    end.

init_otlp_exporter(Config) ->
    %% Configure OTLP exporter
    Endpoint = maps:get(trace_endpoint, Config, "http://localhost:4317"),
    Timeout = maps:get(trace_timeout, Config, 10000),

    %% Create OTLP exporter
    Exporter = otel_otlp_exporter:new(#{
        endpoint => Endpoint,
        timeout => Timeout
    }),

    %% Register exporter
    opentelemetry:add_exporter(trace, Exporter).

init_metrics_exporter(Config) ->
    %% Configure metrics exporter
    Endpoint = maps:get(metrics_endpoint, Config, "http://localhost:4317"),
    Timeout = maps:get(metrics_timeout, Config, 10000),

    %% Create metrics exporter
    Exporter = otel_metrics_exporter:new(#{
        endpoint => Endpoint,
        timeout => Timeout
    }),

    %% Register exporter
    opentelemetry:add_exporter(metrics, Exporter).

init_instrumentation() ->
    %% Initialize instrumentation libraries
    %% HTTP instrumentation
    init_http_instrumentation(),

    %% Database instrumentation
    init_db_instrumentation(),

    %% Message queue instrumentation
    init_mq_instrumentation().

init_http_instrumentation() ->
    %% Instrument HTTP requests
    otel_http_instrumenter:new(#{
        service => "erlmcp",
        name => "http"
    }),

    %% Common HTTP attributes
    otel_http_instrumenter:set_attributes(#{
        http_method => <<"GET">>,
        http_status_code => 200,
        network_protocol_name => <<"http">>
    }).

init_db_instrumentation() ->
    %% Database instrumentation
    otel_db_instrumenter:new(#{
        service => "erlmcp",
        name => "database"
    }),

    %% Common DB attributes
    otel_db_instrumenter:set_attributes(#{
        db_system => <<"erlang">>,
        db_operation => <<"query">>
    }).

init_mq_instrumentation() ->
    %% Message queue instrumentation
    otel_mq_instrumenter:new(#{
        service => "erlmcp",
        name => "messaging"
    }),

    %% Common MQ attributes
    otel_mq_instrumenter:set_attributes(#{
        messaging_system => <<"rabbitmq">>,
        messaging_destination_kind => <<"queue">>
    }).

%%====================================================================
%% Utility Functions
%%====================================================================

%% Create distributed trace context
create_trace_context(RequestId) ->
    %% Generate trace ID
    TraceId = crypto:strong_rand_bytes(16),

    %% Generate span ID
    SpanId = crypto:strong_rand_bytes(8),

    %% Create trace context
    TraceCtx = opentelemetry:trace_context(TraceId, SpanId, 0),
    opentelemetry:set_current_context(TraceCtx).

%% Extract trace headers from request
extract_trace_headers(RequestHeaders) ->
    %% Extract traceparent header
    TraceParent = proplists:get_value(<<"traceparent">>, RequestHeaders, undefined),

    case TraceParent of
        undefined ->
            %% Create new trace
            create_trace_context(undefined);
        _ ->
            ParseTraceParent = fun(TP) ->
                Parts = binary:split(TP, <<"-">>, [global]),
                case Parts of
                    [TraceId, SpanId, Flags, _Rest] ->
                        #{
                            trace_id => TraceId,
                            span_id => SpanId,
                            trace_flags => Flags
                        };
                    _ ->
                        undefined
                end
            end,
            ParseTraceParent(TraceParent)
    end.

%% Format trace ID for logging
format_trace_id(TraceId) ->
    case TraceId of
        undefined -> "undefined";
        _ -> binary_to_hex(TraceId)
    end.

binary_to_hex(Binary) ->
    lists:flatten([io_lib:format("~2.16.0B", [B]) || B <- binary_to_list(Binary)]).