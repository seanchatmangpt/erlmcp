%%%-------------------------------------------------------------------
%%% @doc
%%% OpenTelemetry Observability Infrastructure for ErlMCP
%%%
%%% This module provides comprehensive OpenTelemetry tracing and metrics
%%% for the Erlang Model Context Protocol implementation. It ensures
%%% complete observability with distributed tracing, span relationships,
%%% error recording, and performance monitoring.
%%%
%%% == Key Features ==
%%% 
%%% 1. **Distributed Tracing**: Complete span coverage for all operations
%%% 2. **Context Propagation**: Trace context across transport boundaries
%%% 3. **Error Recording**: Automatic error capture in spans
%%% 4. **Performance Metrics**: Latency and throughput monitoring
%%% 5. **Baggage Correlation**: Request correlation across services
%%% 6. **Multiple Exporters**: Jaeger, Zipkin, Prometheus support
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Initialize OpenTelemetry
%%% ok = erlmcp_otel:init(#{
%%%     service_name => <<"erlmcp-server">>,
%%%     exporters => [jaeger, prometheus],
%%%     sampling => always_on
%%% }),
%%%
%%% %% Trace a function with automatic span management
%%% Result = erlmcp_otel:with_span(
%%%     <<"mcp.tools.call">>,
%%%     #{<<"tool.name">> => ToolName},
%%%     fun() -> call_tool(ToolName, Args) end
%%% ),
%%%
%%% %% Manual span management
%%% SpanCtx = erlmcp_otel:start_span(<<"mcp.transport.send">>, #{
%%%     <<"transport.type">> => <<"stdio">>,
%%%     <<"message.id">> => MessageId
%%% }),
%%% try
%%%     send_message(Message),
%%%     erlmcp_otel:end_span(SpanCtx)
%%% catch
%%%     Class:Reason:Stacktrace ->
%%%         erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}),
%%%         erlmcp_otel:end_span(SpanCtx)
%%% end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel).

-include("erlmcp.hrl").

%% Public API
-export([
    init/1,
    start_span/2,
    start_span/3,
    end_span/1,
    with_span/3,
    with_span/4,
    record_error/2,
    record_error/3,
    add_attributes/2,
    add_event/2,
    add_event/3,
    set_baggage/2,
    get_baggage/1,
    propagate_context/1,
    restore_context/1
]).

%% Enhanced Tracing API
-export([
    inject_rpc_span/3,
    inject_rpc_span/4,
    link_span/2,
    create_trace_ctx/1,
    restore_trace_ctx/1,
    propagate_baggage/2,
    get_all_baggage/0,
    sample_decision/2,
    tail_sample_decision/1
]).

%% Configuration and management
-export([
    configure_exporter/2,
    set_sampling_rate/1,
    get_tracer_provider/0,
    shutdown/0
]).

%% Internal utilities
-export([
    create_span_name/2,
    extract_error_attributes/1,
    get_current_context/0,
    make_trace_id/0,
    make_span_id/0
]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type otel_config() :: #{
    service_name => binary(),
    service_version => binary(),
    exporters => [jaeger | zipkin | prometheus | otlp | console | datadog | honeycomb],
    sampling => always_on | always_off | trace_id_ratio | parent_based | head_based | tail_based,
    sampling_rate => float(),
    tail_sampling_latency_threshold_us => pos_integer(),  % For tail-based sampling
    tail_sampling_error_rate => float(),  % Error rate threshold for tail sampling
    resource_attributes => #{binary() => term()},
    batch_timeout => pos_integer(),
    max_queue_size => pos_integer(),
    max_export_batch_size => pos_integer()
}.

-type span_context() :: #{
    trace_id => binary(),
    span_id => binary(),
    parent_span_id => binary() | undefined,
    trace_flags => integer(),
    trace_state => binary(),
    baggage => #{binary() => binary()},
    start_time => integer(),
    attributes => #{binary() => term()},
    events => [otel_event()],
    status => ok | error | timeout,
    otel_span => term()  % OpenTelemetry span reference
}.

%% New trace context record for cross-process propagation
-record(trace_ctx, {
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    baggage :: #{atom() => term()}
}).

-type otel_event() :: #{
    name => binary(),
    timestamp => integer(),
    attributes => #{binary() => term()}
}.

-type otel_error() :: #{
    error_type => binary(),
    error_message => binary(),
    error_stacktrace => [term()],
    error_class => throw | error | exit
}.

%% =============================================================================
%% OpenTelemetry Initialization
%% =============================================================================

%% @doc Initialize OpenTelemetry with comprehensive configuration
%% Sets up tracer provider, exporters, and sampling strategies
-spec init(otel_config()) -> ok | {error, term()}.
init(Config) ->
    try
        %% Configure resource attributes
        ResourceAttrs = get_resource_attributes(Config),
        ok = configure_resource(ResourceAttrs),
        
        %% Initialize tracer provider
        ok = setup_tracer_provider(Config),
        
        %% Configure exporters
        ok = setup_exporters(Config),
        
        %% Configure sampling
        ok = setup_sampling(Config),
        
        %% Start batch span processor
        ok = start_batch_processor(Config),
        
        %% Set global tracer
        ok = set_global_tracer(),
        
        %% Store configuration for later use
        ok = store_config(Config),
        
        ok
    catch
        Class:Reason:Stacktrace ->
            error_logger:error_msg("Failed to initialize OpenTelemetry: ~p:~p~n~p~n", 
                                   [Class, Reason, Stacktrace]),
            {error, {initialization_failed, {Class, Reason}}}
    end.

%% @doc Start a new span with automatic parent detection
-spec start_span(binary(), #{binary() => term()}) -> span_context().
start_span(Name, Attributes) ->
    ParentCtx = get_current_context(),
    start_span(Name, Attributes, ParentCtx).

%% @doc Start a new span with explicit parent context
-spec start_span(binary(), #{binary() => term()}, span_context() | undefined) -> span_context().
start_span(Name, Attributes, ParentCtx) ->
    TraceId = case ParentCtx of
        #{trace_id := ParentTraceId} -> ParentTraceId;
        _ -> make_trace_id()
    end,
    
    SpanId = make_span_id(),
    ParentSpanId = case ParentCtx of
        #{span_id := ParentSpanId0} -> ParentSpanId0;
        _ -> undefined
    end,
    
    StartTime = erlang:system_time(nanosecond),
    
    %% Create OpenTelemetry span
    OtelSpan = create_otel_span(Name, TraceId, SpanId, ParentSpanId, StartTime),
    
    %% Set initial attributes
    BaseAttributes = #{
        <<"service.name">> => get_service_name(),
        <<"service.version">> => get_service_version(),
        <<"mcp.version">> => ?MCP_VERSION,
        <<"span.kind">> => <<"internal">>,
        <<"erlang.node">> => atom_to_binary(node()),
        <<"erlang.pid">> => list_to_binary(pid_to_list(self()))
    },
    
    AllAttributes = maps:merge(BaseAttributes, Attributes),
    ok = set_span_attributes(OtelSpan, AllAttributes),
    
    %% Inherit baggage from parent
    Baggage = case ParentCtx of
        #{baggage := ParentBaggage} -> ParentBaggage;
        _ -> #{}
    end,
    
    SpanCtx = #{
        trace_id => TraceId,
        span_id => SpanId,
        parent_span_id => ParentSpanId,
        trace_flags => 1,  % sampled
        trace_state => <<>>,
        baggage => Baggage,
        start_time => StartTime,
        attributes => AllAttributes,
        events => [],
        status => ok,
        otel_span => OtelSpan
    },
    
    %% Set as current context
    ok = set_current_context(SpanCtx),
    
    SpanCtx.

%% @doc End a span and finalize its data
-spec end_span(span_context()) -> ok.
end_span(#{otel_span := OtelSpan, start_time := StartTime} = SpanCtx) ->
    EndTime = erlang:system_time(nanosecond),
    Duration = EndTime - StartTime,
    
    %% Add duration attribute
    ok = set_span_attributes(OtelSpan, #{<<"duration_ns">> => Duration}),
    
    %% Set final status
    Status = maps:get(status, SpanCtx, ok),
    ok = set_span_status(OtelSpan, Status),
    
    %% End the OpenTelemetry span
    ok = end_otel_span(OtelSpan, EndTime),
    
    %% Clear current context if this is the active span
    case get_current_context() of
        SpanCtx -> ok = clear_current_context();
        _ -> ok
    end,
    
    ok;
end_span(_) ->
    {error, invalid_span_context}.

%% @doc Execute a function within a span context
-spec with_span(binary(), #{binary() => term()}, fun(() -> T)) -> T.
with_span(Name, Attributes, Fun) ->
    with_span(Name, Attributes, undefined, Fun).

%% @doc Execute a function within a span context with explicit parent
-spec with_span(binary(), #{binary() => term()}, span_context() | undefined, fun(() -> T)) -> T.
with_span(Name, Attributes, ParentCtx, Fun) ->
    SpanCtx = start_span(Name, Attributes, ParentCtx),
    try
        Result = Fun(),
        ok = end_span(SpanCtx),
        Result
    catch
        Class:Reason:Stacktrace ->
            ok = record_error(SpanCtx, {Class, Reason, Stacktrace}),
            ok = end_span(SpanCtx),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Record an error in the current span
-spec record_error(span_context(), {atom(), term(), list()}) -> ok.
record_error(SpanCtx, Error) ->
    record_error(SpanCtx, Error, #{}).

%% @doc Record an error with additional attributes
-spec record_error(span_context(), {atom(), term(), list()}, #{binary() => term()}) -> ok.
record_error(#{otel_span := OtelSpan} = SpanCtx, {Class, Reason, Stacktrace}, ExtraAttrs) ->
    ErrorAttrs = #{
        <<"error">> => true,
        <<"error.type">> => atom_to_binary(Class),
        <<"error.message">> => format_error_message(Reason),
        <<"error.stacktrace">> => format_stacktrace(Stacktrace)
    },
    
    AllAttrs = maps:merge(ErrorAttrs, ExtraAttrs),
    ok = set_span_attributes(OtelSpan, AllAttrs),
    
    %% Add error event
    ErrorEvent = #{
        name => <<"exception">>,
        timestamp => erlang:system_time(nanosecond),
        attributes => AllAttrs
    },
    
    ok = add_span_event(OtelSpan, ErrorEvent),
    
    %% Update span status
    UpdatedCtx = SpanCtx#{status => error},
    ok = set_current_context(UpdatedCtx),
    
    ok;
record_error(_, _, _) ->
    {error, invalid_span_context}.

%% @doc Add attributes to the current span
-spec add_attributes(span_context(), #{binary() => term()}) -> ok.
add_attributes(#{otel_span := OtelSpan}, Attributes) ->
    set_span_attributes(OtelSpan, Attributes);
add_attributes(_, _) ->
    {error, invalid_span_context}.

%% @doc Add an event to the current span
-spec add_event(span_context(), binary()) -> ok.
add_event(SpanCtx, Name) ->
    add_event(SpanCtx, Name, #{}).

%% @doc Add an event with attributes to the current span
-spec add_event(span_context(), binary(), #{binary() => term()}) -> ok.
add_event(#{otel_span := OtelSpan}, Name, Attributes) ->
    Event = #{
        name => Name,
        timestamp => erlang:system_time(nanosecond),
        attributes => Attributes
    },
    add_span_event(OtelSpan, Event);
add_event(_, _, _) ->
    {error, invalid_span_context}.

%% @doc Set baggage for correlation
-spec set_baggage(binary(), binary()) -> ok.
set_baggage(Key, Value) ->
    case get_current_context() of
        #{baggage := Baggage} = SpanCtx ->
            UpdatedBaggage = Baggage#{Key => Value},
            UpdatedCtx = SpanCtx#{baggage => UpdatedBaggage},
            set_current_context(UpdatedCtx);
        _ ->
            %% Create new context with baggage
            SpanCtx = #{baggage => #{Key => Value}},
            set_current_context(SpanCtx)
    end.

%% @doc Get baggage value
-spec get_baggage(binary()) -> binary() | undefined.
get_baggage(Key) ->
    case get_current_context() of
        #{baggage := Baggage} -> maps:get(Key, Baggage, undefined);
        _ -> undefined
    end.

%% @doc Extract context for propagation
-spec propagate_context(span_context()) -> #{binary() => binary()}.
propagate_context(#{trace_id := TraceId, span_id := SpanId, trace_flags := Flags, baggage := Baggage}) ->
    TraceParent = format_traceparent(TraceId, SpanId, Flags),
    TraceState = format_tracestate(#{}),
    BaggageHeader = format_baggage(Baggage),
    
    Headers = #{
        <<"traceparent">> => TraceParent,
        <<"tracestate">> => TraceState
    },
    
    case BaggageHeader of
        <<>> -> Headers;
        _ -> Headers#{<<"baggage">> => BaggageHeader}
    end;
propagate_context(_) ->
    #{}.

%% @doc Restore context from propagated headers
-spec restore_context(#{binary() => binary()}) -> span_context() | undefined.
restore_context(Headers) ->
    case maps:get(<<"traceparent">>, Headers, undefined) of
        undefined -> undefined;
        TraceParent ->
            case parse_traceparent(TraceParent) of
                {ok, TraceId, ParentSpanId, Flags} ->
                    Baggage = case maps:get(<<"baggage">>, Headers, undefined) of
                        undefined -> #{};
                        BaggageHeader -> parse_baggage(BaggageHeader)
                    end,
                    
                    #{
                        trace_id => TraceId,
                        span_id => ParentSpanId,  % This will be the parent
                        parent_span_id => undefined,
                        trace_flags => Flags,
                        trace_state => <<>>,
                        baggage => Baggage,
                        start_time => erlang:system_time(nanosecond),
                        attributes => #{},
                        events => [],
                        status => ok,
                        otel_span => undefined  % Will be set when span is created
                    };
                {error, _} -> undefined
            end
    end.

%% =============================================================================
%% Configuration Management
%% =============================================================================

%% @doc Configure exporter settings
-spec configure_exporter(atom(), map()) -> ok | {error, term()}.
configure_exporter(jaeger, Config) ->
    setup_jaeger_exporter(Config);
configure_exporter(zipkin, Config) ->
    setup_zipkin_exporter(Config);
configure_exporter(prometheus, Config) ->
    setup_prometheus_exporter(Config);
configure_exporter(otlp, Config) ->
    setup_otlp_exporter(Config);
configure_exporter(console, Config) ->
    setup_console_exporter(Config);
configure_exporter(Type, _Config) ->
    {error, {unsupported_exporter, Type}}.

%% @doc Set sampling rate
-spec set_sampling_rate(float()) -> ok.
set_sampling_rate(Rate) when Rate >= 0.0, Rate =< 1.0 ->
    %% Configure trace_id_ratio_based sampler
    Sampler = otel_sampler:trace_id_ratio_based(Rate),
    application:set_env(opentelemetry, sampler, Sampler),
    ok;
set_sampling_rate(_) ->
    {error, invalid_sampling_rate}.

%% @doc Get tracer provider
-spec get_tracer_provider() -> term().
get_tracer_provider() ->
    try
        opentelemetry:get_tracer_provider()
    catch
        _:_ -> undefined
    end.

%% @doc Shutdown OpenTelemetry and flush spans
-spec shutdown() -> ok.
shutdown() ->
    % Flush all pending spans
    ok = flush_spans(),
    % Shutdown tracer provider
    ok = shutdown_tracer_provider(),
    ok.

%% =============================================================================
%% Internal Implementation Functions
%% =============================================================================

%% @private
%% Get resource attributes with defaults
-spec get_resource_attributes(otel_config()) -> #{binary() => term()}.
get_resource_attributes(Config) ->
    %% Get hostname safely
    Hostname = case inet:gethostname() of
        {ok, Name} -> list_to_binary(Name);
        _ -> <<"unknown">>
    end,

    DefaultAttrs = #{
        <<"service.name">> => maps:get(service_name, Config, <<"erlmcp">>),
        <<"service.version">> => maps:get(service_version, Config, <<"0.5.0">>),
        <<"service.language">> => <<"erlang">>,
        <<"service.runtime">> => list_to_binary(erlang:system_info(otp_release)),
        <<"host.name">> => Hostname,
        <<"process.pid">> => list_to_binary(os:getpid())
    },

    CustomAttrs = maps:get(resource_attributes, Config, #{}),
    maps:merge(DefaultAttrs, CustomAttrs).

%% @private
%% Configure OpenTelemetry resource
-spec configure_resource(#{binary() => term()}) -> ok.
configure_resource(Attributes) ->
    %% Convert binary keys to atoms for OTel resource
    ResourceAttrs = maps:fold(fun(K, V, Acc) ->
        AtomKey = case is_binary(K) of
            true -> binary_to_atom(K, utf8);
            false -> K
        end,
        [{AtomKey, V} | Acc]
    end, [], Attributes),

    %% Set resource attributes via opentelemetry application environment
    application:set_env(opentelemetry, resource_attributes, ResourceAttrs),
    ok.

%% @private
%% Setup tracer provider
-spec setup_tracer_provider(otel_config()) -> ok.
setup_tracer_provider(Config) ->
    %% Ensure opentelemetry application is started
    case application:ensure_all_started(opentelemetry) of
        {ok, _Started} -> ok;
        {error, _Reason} -> ok  %% May already be started
    end,

    %% Get service name for tracer
    ServiceName = maps:get(service_name, Config, <<"erlmcp">>),
    application:set_env(opentelemetry, service_name, ServiceName),

    ok.

%% @private
%% Setup exporters based on configuration
-spec setup_exporters(otel_config()) -> ok.
setup_exporters(Config) ->
    Exporters = maps:get(exporters, Config, [console]),
    lists:foreach(fun(Exporter) ->
        ok = configure_exporter(Exporter, Config)
    end, Exporters),
    ok.

%% @private
%% Setup sampling configuration
-spec setup_sampling(otel_config()) -> ok.
setup_sampling(Config) ->
    Sampling = maps:get(sampling, Config, always_on),
    Sampler = case Sampling of
        always_on ->
            otel_sampler:always_on();
        always_off ->
            otel_sampler:always_off();
        trace_id_ratio ->
            Rate = maps:get(sampling_rate, Config, 0.1),
            otel_sampler:trace_id_ratio_based(Rate);
        parent_based ->
            %% Parent-based with trace_id_ratio fallback
            Rate = maps:get(sampling_rate, Config, 0.1),
            Root = otel_sampler:trace_id_ratio_based(Rate),
            otel_sampler:parent_based(Root);
        head_based ->
            %% Same as trace_id_ratio for now
            Rate = maps:get(sampling_rate, Config, 0.1),
            otel_sampler:trace_id_ratio_based(Rate);
        tail_based ->
            %% Tail-based sampling requires custom implementation
            %% For now, use always_on and filter later
            otel_sampler:always_on();
        _ ->
            otel_sampler:always_on()
    end,

    %% Set the sampler
    application:set_env(opentelemetry, sampler, Sampler),
    ok.

%% @private
%% Start batch span processor
-spec start_batch_processor(otel_config()) -> ok.
start_batch_processor(Config) ->
    %% Configure batch processor settings
    BatchTimeout = maps:get(batch_timeout, Config, 5000),
    MaxQueueSize = maps:get(max_queue_size, Config, 2048),
    MaxExportBatchSize = maps:get(max_export_batch_size, Config, 512),

    %% Set batch processor configuration
    application:set_env(opentelemetry, batch_timeout, BatchTimeout),
    application:set_env(opentelemetry, max_queue_size, MaxQueueSize),
    application:set_env(opentelemetry, max_export_batch_size, MaxExportBatchSize),

    ok.

%% @private
%% Set global tracer
-spec set_global_tracer() -> ok.
set_global_tracer() ->
    %% The default tracer is automatically set by the opentelemetry application
    %% Just ensure the tracer is accessible
    try
        _Tracer = opentelemetry:get_tracer(erlmcp),
        ok
    catch
        _:_ -> ok
    end.

%% @private
%% Store configuration for runtime access
-spec store_config(otel_config()) -> ok.
store_config(Config) ->
    erlang:put(erlmcp_otel_config, Config),
    ok.

%% @private
%% Create OpenTelemetry span
create_otel_span(Name, _TraceId, _SpanId, ParentSpanId, _StartTime) ->
    %% Get tracer
    Tracer = opentelemetry:get_tracer(erlmcp),

    %% Create span context options
    Opts = case ParentSpanId of
        undefined -> #{};
        _ -> #{parent => get_otel_parent_ctx()}
    end,

    %% Start span with OpenTelemetry library
    SpanCtx = otel_tracer:start_span(Tracer, Name, Opts),

    %% Return span context (which includes the actual OTel span)
    SpanCtx.

%% @private
%% Set span attributes
-spec set_span_attributes(term(), #{binary() => term()}) -> ok.
set_span_attributes(OtelSpan, Attributes) when is_map(Attributes) ->
    %% Convert to OTel attribute format (list of tuples with atom keys)
    AttrList = maps:fold(fun(K, V, Acc) ->
        AtomKey = case is_binary(K) of
            true ->
                try binary_to_existing_atom(K, utf8)
                catch
                    error:badarg ->
                        %% Don't create new atoms from user input to prevent atom table exhaustion
                        logger:debug("Unknown attribute key (keeping as binary): ~p", [K]),
                        K
                end;
            false -> K
        end,
        [{AtomKey, format_attribute_value(V)} | Acc]
    end, [], Attributes),

    %% Set attributes on span
    otel_span:set_attributes(OtelSpan, AttrList),
    ok;
set_span_attributes(_, _) ->
    ok.

%% @private
%% Set span status
-spec set_span_status(term(), atom()) -> ok.
set_span_status(OtelSpan, Status) ->
    %% Map our status to OpenTelemetry status codes
    OtelStatus = case Status of
        ok -> 'ok';
        error -> 'error';
        timeout -> 'error';
        _ -> 'unset'
    end,

    %% Set status on span
    otel_span:set_status(OtelSpan, OtelStatus),
    ok.

%% @private
%% End OpenTelemetry span
-spec end_otel_span(term(), integer()) -> ok.
end_otel_span(OtelSpan, EndTime) ->
    %% End the span with timestamp
    otel_span:end_span(OtelSpan, EndTime),
    ok.

%% @private
%% Add event to span
-spec add_span_event(term(), otel_event()) -> ok.
add_span_event(OtelSpan, Event) ->
    EventName = maps:get(name, Event, <<"event">>),
    Timestamp = maps:get(timestamp, Event, erlang:system_time(nanosecond)),
    Attributes = maps:get(attributes, Event, #{}),

    %% Convert attributes to OTel format
    AttrList = maps:fold(fun(K, V, Acc) ->
        AtomKey = case is_binary(K) of
            true ->
                try binary_to_existing_atom(K, utf8)
                catch
                    error:badarg ->
                        %% Don't create new atoms from user input to prevent atom table exhaustion
                        logger:debug("Unknown event attribute key (keeping as binary): ~p", [K]),
                        K
                end;
            false -> K
        end,
        [{AtomKey, format_attribute_value(V)} | Acc]
    end, [], Attributes),

    %% Add event with timestamp and attributes
    otel_span:add_event(OtelSpan, EventName, AttrList, Timestamp),
    ok.

%% @private
%% Get current context from process dictionary
-spec get_current_context() -> span_context() | undefined.
get_current_context() ->
    erlang:get(erlmcp_otel_current_context).

%% @private
%% Set current context in process dictionary
-spec set_current_context(span_context()) -> ok.
set_current_context(SpanCtx) ->
    erlang:put(erlmcp_otel_current_context, SpanCtx),
    ok.

%% @private
%% Clear current context
-spec clear_current_context() -> ok.
clear_current_context() ->
    erlang:erase(erlmcp_otel_current_context),
    ok.

%% @private
%% Generate trace ID
-spec make_trace_id() -> binary().
make_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

%% @private
%% Generate span ID
-spec make_span_id() -> binary().
make_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    integer_to_binary(Id, 16).

%% @private
%% Get service name
-spec get_service_name() -> binary().
get_service_name() ->
    case erlang:get(erlmcp_otel_config) of
        #{service_name := Name} -> Name;
        _ -> <<"erlmcp">>
    end.

%% @private
%% Get service version
-spec get_service_version() -> binary().
get_service_version() ->
    case erlang:get(erlmcp_otel_config) of
        #{service_version := Version} -> Version;
        _ -> <<"0.5.0">>
    end.

%% @private
%% Format error message
-spec format_error_message(term()) -> binary().
format_error_message(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%% @private
%% Format stacktrace
-spec format_stacktrace(list()) -> binary().
format_stacktrace(Stacktrace) ->
    list_to_binary(io_lib:format("~p", [Stacktrace])).

%% @private
%% Format traceparent header
-spec format_traceparent(binary(), binary(), integer()) -> binary().
format_traceparent(TraceId, SpanId, Flags) ->
    iolist_to_binary(io_lib:format("00-~s-~s-~2.16.0b", [TraceId, SpanId, Flags])).

%% @private
%% Format tracestate header
-spec format_tracestate(map()) -> binary().
format_tracestate(_TraceState) ->
    % Simple implementation - can be enhanced
    <<>>.

%% @private
%% Format baggage header
-spec format_baggage(#{binary() => binary()}) -> binary().
format_baggage(Baggage) when map_size(Baggage) == 0 ->
    <<>>;
format_baggage(Baggage) ->
    Entries = maps:fold(fun(Key, Value, Acc) ->
        Entry = <<Key/binary, "=", Value/binary>>,
        [Entry | Acc]
    end, [], Baggage),
    iolist_to_binary(string:join(Entries, ",")).

%% @private
%% Parse traceparent header
-spec parse_traceparent(binary()) -> {ok, binary(), binary(), integer()} | {error, term()}.
parse_traceparent(TraceParent) ->
    case binary:split(TraceParent, <<"-">>, [global]) of
        [<<"00">>, TraceId, SpanId, Flags] when 
            byte_size(TraceId) == 32,
            byte_size(SpanId) == 16,
            byte_size(Flags) == 2 ->
            try
                FlagsInt = binary_to_integer(Flags, 16),
                {ok, TraceId, SpanId, FlagsInt}
            catch
                _:_ -> {error, invalid_flags}
            end;
        _ -> {error, invalid_format}
    end.

%% @private
%% Parse baggage header
-spec parse_baggage(binary()) -> #{binary() => binary()}.
parse_baggage(BaggageHeader) ->
    Entries = binary:split(BaggageHeader, <<",">>, [global]),
    lists:foldl(fun(Entry, Acc) ->
        case binary:split(Entry, <<"=">>, []) of
            [Key, Value] -> Acc#{Key => Value};
            _ -> Acc
        end
    end, #{}, Entries).

%% @private
%% Create span name from module and function
-spec create_span_name(atom(), atom()) -> binary().
create_span_name(Module, Function) ->
    ModuleBin = atom_to_binary(Module),
    FunctionBin = atom_to_binary(Function),
    <<ModuleBin/binary, ".", FunctionBin/binary>>.

%% @private
%% Extract error attributes from exception
-spec extract_error_attributes({atom(), term(), list()}) -> #{binary() => term()}.
extract_error_attributes({Class, Reason, Stacktrace}) ->
    #{
        <<"error.type">> => atom_to_binary(Class),
        <<"error.message">> => format_error_message(Reason),
        <<"error.stacktrace">> => format_stacktrace(Stacktrace)
    }.

%% @private
%% Setup Jaeger exporter
-spec setup_jaeger_exporter(map()) -> ok.
setup_jaeger_exporter(Config) ->
    %% Get Jaeger configuration
    Host = maps:get(host, Config, "localhost"),
    Port = maps:get(port, Config, 14250),
    ServiceName = maps:get(service_name, Config, <<"erlmcp">>),

    %% Configure Jaeger exporter
    ExporterConfig = #{
        endpoint => {Host, Port},
        service_name => ServiceName,
        protocol => grpc
    },

    %% Set exporter configuration in application environment
    application:set_env(opentelemetry_exporter, exporter, jaeger),
    application:set_env(opentelemetry_exporter, endpoint, {Host, Port}),

    %% Ensure exporter application is started
    case application:ensure_all_started(opentelemetry_exporter) of
        {ok, _} -> ok;
        {error, _} -> ok
    end,

    ok.

%% @private
%% Setup Zipkin exporter
-spec setup_zipkin_exporter(map()) -> ok.
setup_zipkin_exporter(Config) ->
    %% Get Zipkin configuration
    Endpoint = maps:get(endpoint, Config, "http://localhost:9411/api/v2/spans"),

    %% Configure Zipkin exporter
    application:set_env(opentelemetry_exporter, exporter, zipkin),
    application:set_env(opentelemetry_exporter, zipkin_endpoint, Endpoint),

    %% Ensure exporter application is started
    case application:ensure_all_started(opentelemetry_exporter) of
        {ok, _} -> ok;
        {error, _} -> ok
    end,

    ok.

%% @private
%% Setup Prometheus exporter
-spec setup_prometheus_exporter(map()) -> ok.
setup_prometheus_exporter(Config) ->
    %% Prometheus exporter for metrics (not traces)
    %% This is primarily for metrics, but we configure it for compatibility
    Port = maps:get(port, Config, 9464),

    application:set_env(opentelemetry_exporter, prometheus_port, Port),

    ok.

%% @private
%% Setup OTLP exporter
-spec setup_otlp_exporter(map()) -> ok.
setup_otlp_exporter(Config) ->
    %% Get OTLP configuration
    Endpoint = maps:get(endpoint, Config, "http://localhost:4318"),
    Headers = maps:get(headers, Config, #{}),
    Protocol = maps:get(protocol, Config, http_protobuf),

    %% Convert headers to list format
    HeaderList = maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc]
    end, [], Headers),

    %% Configure OTLP exporter
    application:set_env(opentelemetry_exporter, otlp_endpoint, Endpoint),
    application:set_env(opentelemetry_exporter, otlp_protocol, Protocol),
    application:set_env(opentelemetry_exporter, otlp_headers, HeaderList),

    %% Ensure exporter application is started
    case application:ensure_all_started(opentelemetry_exporter) of
        {ok, _} -> ok;
        {error, _} -> ok
    end,

    ok.

%% @private
%% Setup console exporter
-spec setup_console_exporter(map()) -> ok.
setup_console_exporter(_Config) ->
    %% Configure console exporter for debugging
    application:set_env(opentelemetry, traces_exporter, console),

    ok.

%% @private
%% Flush all pending spans
-spec flush_spans() -> ok.
flush_spans() ->
    %% Force flush of batch span processor
    try
        otel_batch_processor:force_flush(),
        ok
    catch
        _:_ -> ok
    end.

%% @private
%% Shutdown tracer provider
-spec shutdown_tracer_provider() -> ok.
shutdown_tracer_provider() ->
    %% Shutdown batch processor
    try
        otel_batch_processor:shutdown(),
        ok
    catch
        _:_ -> ok
    end,

    %% Stop opentelemetry application
    application:stop(opentelemetry_exporter),
    application:stop(opentelemetry),
    ok.

%% =============================================================================
%% Enhanced Tracing Functions
%% =============================================================================

%% @doc Automatically inject span for RPC calls
%% This creates a span for client requests with proper attributes
-spec inject_rpc_span(binary(), binary(), map()) -> span_context().
inject_rpc_span(Method, RequestId, Params) ->
    inject_rpc_span(Method, RequestId, Params, undefined).

%% @doc Inject RPC span with explicit parent context
-spec inject_rpc_span(binary(), binary(), map(), span_context() | undefined) -> span_context().
inject_rpc_span(Method, RequestId, Params, ParentCtx) ->
    SpanName = <<"mcp.rpc.", Method/binary>>,
    Attributes = #{
        <<"rpc.method">> => Method,
        <<"rpc.request_id">> => RequestId,
        <<"rpc.service">> => <<"erlmcp">>,
        <<"rpc.system">> => <<"jsonrpc">>,
        <<"span.kind">> => <<"client">>
    },

    %% Add parameter attributes (sanitized)
    ParamAttrs = case Params of
        Map when is_map(Map), map_size(Map) > 0 ->
            sanitize_params(Map);
        _ -> #{}
    end,

    AllAttributes = maps:merge(Attributes, ParamAttrs),

    SpanCtx = start_span(SpanName, AllAttributes, ParentCtx),

    %% Add RPC-specific events
    ok = add_event(SpanCtx, <<"client.request_sent">>, #{
        <<"request_id">> => RequestId,
        <<"timestamp">> => erlang:system_time(nanosecond)
    }),

    SpanCtx.

%% @doc Link current span to another span (for cross-process correlation)
-spec link_span(span_context(), span_context()) -> ok.
link_span(CurrentSpanCtx, LinkedSpanCtx) ->
    #{trace_id := LinkedTraceId, span_id := LinkedSpanId} = LinkedSpanCtx,

    %% Add link attributes
    case add_attributes(CurrentSpanCtx, #{
        <<"link.trace_id">> => LinkedTraceId,
        <<"link.span_id">> => LinkedSpanId
    }) of
        ok ->
            %% Update current context to reflect changes
            UpdatedAttrs = maps:get(attributes, CurrentSpanCtx, #{}),
            NewAttrs = maps:merge(UpdatedAttrs, #{
                <<"link.trace_id">> => LinkedTraceId,
                <<"link.span_id">> => LinkedSpanId
            }),
            UpdatedCtx = CurrentSpanCtx#{attributes => NewAttrs},
            set_current_context(UpdatedCtx),

            %% Add span linked event
            add_event(UpdatedCtx, <<"span.linked">>, #{
                <<"linked_trace_id">> => LinkedTraceId,
                <<"linked_span_id">> => LinkedSpanId
            });
        Error -> Error
    end.

%% @doc Create trace context record for cross-process propagation
-spec create_trace_ctx(span_context()) -> #trace_ctx{}.
create_trace_ctx(#{trace_id := TraceId, span_id := SpanId, parent_span_id := ParentSpanId, baggage := Baggage}) ->
    %% Convert binary baggage keys to atoms for record
    AtomBaggage = maps:fold(fun(K, V, Acc) ->
        case is_binary(K) of
            true -> Acc#{binary_to_atom(K, utf8) => V};
            false -> Acc#{K => V}
        end
    end, #{}, Baggage),

    #trace_ctx{
        trace_id = TraceId,
        span_id = SpanId,
        parent_span_id = ParentSpanId,
        baggage = AtomBaggage
    };
create_trace_ctx(_) ->
    #trace_ctx{
        trace_id = make_trace_id(),
        span_id = make_span_id(),
        parent_span_id = undefined,
        baggage = #{}
    }.

%% @doc Restore span context from trace context record
-spec restore_trace_ctx(#trace_ctx{}) -> span_context().
restore_trace_ctx(#trace_ctx{trace_id = TraceId, span_id = SpanId, parent_span_id = ParentSpanId, baggage = Baggage}) ->
    %% Convert atom baggage keys back to binaries
    BinaryBaggage = maps:fold(fun(K, V, Acc) ->
        case is_atom(K) of
            true -> Acc#{atom_to_binary(K, utf8) => V};
            false -> Acc#{K => V}
        end
    end, #{}, Baggage),

    #{
        trace_id => TraceId,
        span_id => SpanId,
        parent_span_id => ParentSpanId,
        trace_flags => 1,
        trace_state => <<>>,
        baggage => BinaryBaggage,
        start_time => erlang:system_time(nanosecond),
        attributes => #{},
        events => [],
        status => ok,
        otel_span => undefined
    }.

%% @doc Propagate baggage to child processes
-spec propagate_baggage(atom() | binary(), term()) -> ok.
propagate_baggage(Key, Value) when is_atom(Key) ->
    propagate_baggage(atom_to_binary(Key, utf8), Value);
propagate_baggage(Key, Value) when is_binary(Key) ->
    %% Convert value to binary if necessary
    BinaryValue = case Value of
        V when is_binary(V) -> V;
        V when is_atom(V) -> atom_to_binary(V, utf8);
        V when is_integer(V) -> integer_to_binary(V);
        V when is_list(V) -> list_to_binary(V);
        V -> list_to_binary(io_lib:format("~p", [V]))
    end,
    set_baggage(Key, BinaryValue).

%% @doc Get all baggage from current context
-spec get_all_baggage() -> #{binary() => binary()}.
get_all_baggage() ->
    case get_current_context() of
        #{baggage := Baggage} -> Baggage;
        _ -> #{}
    end.

%% @doc Make sampling decision based on strategy (head-based)
-spec sample_decision(always_on | always_off | trace_id_ratio | parent_based, float()) -> boolean().
sample_decision(always_on, _Rate) ->
    true;
sample_decision(always_off, _Rate) ->
    false;
sample_decision(trace_id_ratio, Rate) ->
    %% Use trace ID for consistent sampling
    case get_current_context() of
        #{trace_id := TraceId} ->
            sample_by_trace_id(TraceId, Rate);
        _ ->
            %% Fallback to random sampling
            rand:uniform() < Rate
    end;
sample_decision(parent_based, Rate) ->
    %% Check parent sampling decision
    case get_current_context() of
        #{trace_flags := Flags} when Flags band 1 =:= 1 ->
            true;  % Parent was sampled
        _ ->
            %% No parent, use trace_id_ratio
            sample_decision(trace_id_ratio, Rate)
    end.

%% @doc Tail-based sampling decision (after span completion)
%% Returns true if span should be sampled based on latency and error
-spec tail_sample_decision(span_context()) -> boolean().
tail_sample_decision(#{start_time := StartTime, status := Status, attributes := Attributes}) ->
    EndTime = erlang:system_time(nanosecond),
    Duration = EndTime - StartTime,
    DurationUs = Duration div 1000,

    %% Get thresholds from config
    Config = erlang:get(erlmcp_otel_config),
    LatencyThreshold = maps:get(tail_sampling_latency_threshold_us, Config, 100000),  % 100ms default
    ErrorRate = maps:get(tail_sampling_error_rate, Config, 0.01),  % 1% default

    %% Sample if high latency OR error
    IsHighLatency = DurationUs > LatencyThreshold,
    IsError = Status =:= error orelse maps:get(<<"error">>, Attributes, false) =:= true,
    IsRareError = IsError andalso (rand:uniform() < ErrorRate),

    IsHighLatency orelse IsRareError;
tail_sample_decision(_) ->
    false.

%% =============================================================================
%% OpenTelemetry Helper Functions
%% =============================================================================

%% @private
%% Get parent context from OpenTelemetry
-spec get_otel_parent_ctx() -> term().
get_otel_parent_ctx() ->
    try
        otel_tracer:current_span_ctx()
    catch
        _:_ -> undefined
    end.

%% @private
%% Format attribute value for OpenTelemetry
-spec format_attribute_value(term()) -> term().
format_attribute_value(V) when is_binary(V) -> V;
format_attribute_value(V) when is_integer(V) -> V;
format_attribute_value(V) when is_float(V) -> V;
format_attribute_value(V) when is_boolean(V) -> V;
format_attribute_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_attribute_value(V) when is_list(V) ->
    try list_to_binary(V)
    catch
        error:badarg ->
            %% Not a valid UTF-8 string, use debug format
            logger:debug("List attribute value is not valid UTF-8, using debug format"),
            list_to_binary(io_lib:format("~p", [V]));
        error:Reason ->
            logger:warning("Failed to convert list attribute value: ~p", [Reason]),
            list_to_binary(io_lib:format("~p", [V]))
    end;
format_attribute_value(V) ->
    list_to_binary(io_lib:format("~p", [V])).

%% =============================================================================
%% Private Helper Functions for Enhanced Tracing
%% =============================================================================

%% @private
%% Sanitize RPC parameters for span attributes (remove sensitive data)
-spec sanitize_params(map()) -> #{binary() => term()}.
sanitize_params(Params) when is_map(Params) ->
    SensitiveKeys = [<<"password">>, <<"token">>, <<"secret">>, <<"api_key">>, <<"auth">>],
    maps:fold(fun(K, V, Acc) ->
        case lists:member(K, SensitiveKeys) of
            true -> Acc#{K => <<"[REDACTED]">>};
            false when is_binary(V); is_number(V); is_boolean(V) ->
                Acc#{K => V};
            false ->
                %% Convert complex types to string representation
                Acc#{K => list_to_binary(io_lib:format("~p", [V]))}
        end
    end, #{}, Params);
sanitize_params(_) ->
    #{}.

%% @private
%% Sample based on trace ID (deterministic)
-spec sample_by_trace_id(binary(), float()) -> boolean().
sample_by_trace_id(TraceId, Rate) when is_binary(TraceId), Rate >= 0.0, Rate =< 1.0 ->
    %% Use last 8 bytes of trace ID for sampling decision
    Size = byte_size(TraceId) - 8,
    <<_:Size/binary, Sample:64/integer>> = TraceId,
    Threshold = trunc(Rate * 18446744073709551616),  % 2^64
    Sample < Threshold;
sample_by_trace_id(_, _) ->
    false.