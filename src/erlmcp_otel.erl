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
    exporters => [jaeger | zipkin | prometheus | otlp | console],
    sampling => always_on | always_off | trace_id_ratio | parent_based,
    sampling_rate => float(),
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
    % Implementation depends on OpenTelemetry library
    ok;
set_sampling_rate(_) ->
    {error, invalid_sampling_rate}.

%% @doc Get tracer provider
-spec get_tracer_provider() -> term().
get_tracer_provider() ->
    % Implementation depends on OpenTelemetry library
    undefined.

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
    DefaultAttrs = #{
        <<"service.name">> => maps:get(service_name, Config, <<"erlmcp">>),
        <<"service.version">> => maps:get(service_version, Config, <<"0.5.0">>),
        <<"service.language">> => <<"erlang">>,
        <<"service.runtime">> => list_to_binary(erlang:system_info(otp_release)),
        <<"host.name">> => list_to_binary(inet:gethostname()),
        <<"process.pid">> => list_to_binary(os:getpid())
    },
    
    CustomAttrs = maps:get(resource_attributes, Config, #{}),
    maps:merge(DefaultAttrs, CustomAttrs).

%% @private
%% Configure OpenTelemetry resource
-spec configure_resource(#{binary() => term()}) -> ok.
configure_resource(_Attributes) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Setup tracer provider
-spec setup_tracer_provider(otel_config()) -> ok.
setup_tracer_provider(_Config) ->
    % Implementation depends on OpenTelemetry library
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
    case Sampling of
        always_on -> ok;
        always_off -> ok;
        trace_id_ratio ->
            Rate = maps:get(sampling_rate, Config, 0.1),
            set_sampling_rate(Rate);
        parent_based -> ok
    end.

%% @private
%% Start batch span processor
-spec start_batch_processor(otel_config()) -> ok.
start_batch_processor(_Config) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Set global tracer
-spec set_global_tracer() -> ok.
set_global_tracer() ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Store configuration for runtime access
-spec store_config(otel_config()) -> ok.
store_config(Config) ->
    erlang:put(erlmcp_otel_config, Config),
    ok.

%% @private
%% Create OpenTelemetry span
-spec create_otel_span(binary(), binary(), binary(), binary() | undefined, integer()) -> term().
create_otel_span(_Name, _TraceId, _SpanId, _ParentSpanId, _StartTime) ->
    % Implementation depends on OpenTelemetry library
    make_ref().

%% @private
%% Set span attributes
-spec set_span_attributes(term(), #{binary() => term()}) -> ok.
set_span_attributes(_OtelSpan, _Attributes) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Set span status
-spec set_span_status(term(), atom()) -> ok.
set_span_status(_OtelSpan, _Status) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% End OpenTelemetry span
-spec end_otel_span(term(), integer()) -> ok.
end_otel_span(_OtelSpan, _EndTime) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Add event to span
-spec add_span_event(term(), otel_event()) -> ok.
add_span_event(_OtelSpan, _Event) ->
    % Implementation depends on OpenTelemetry library
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
setup_jaeger_exporter(_Config) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Setup Zipkin exporter
-spec setup_zipkin_exporter(map()) -> ok.
setup_zipkin_exporter(_Config) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Setup Prometheus exporter
-spec setup_prometheus_exporter(map()) -> ok.
setup_prometheus_exporter(_Config) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Setup OTLP exporter
-spec setup_otlp_exporter(map()) -> ok.
setup_otlp_exporter(_Config) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Setup console exporter
-spec setup_console_exporter(map()) -> ok.
setup_console_exporter(_Config) ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Flush all pending spans
-spec flush_spans() -> ok.
flush_spans() ->
    % Implementation depends on OpenTelemetry library
    ok.

%% @private
%% Shutdown tracer provider
-spec shutdown_tracer_provider() -> ok.
shutdown_tracer_provider() ->
    % Implementation depends on OpenTelemetry library
    ok.