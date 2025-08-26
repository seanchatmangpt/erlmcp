-module(erlmcp_simple_trace).

%% Simple tracing API using only logger
-export([
    start_trace/1, start_trace/2,
    add_span/2, add_span/3, add_span/4,
    end_trace/1,
    current_trace/0,
    format_trace_json/1
]).

%% Convenience exports for common operations
-export([
    trace_transport_operation/3,
    trace_server_operation/3,
    trace_registry_operation/2,
    trace_message_processing/4
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types and Records
%%====================================================================

-type trace_id() :: binary().
-type span_id() :: binary().
-type timestamp() :: integer().
-type trace_context() :: #{
    trace_id => trace_id(),
    spans => [span()],
    start_time => timestamp(),
    metadata => map()
}.

-type span() :: #{
    span_id => span_id(),
    parent_id => span_id() | undefined,
    operation_name => binary(),
    start_time => timestamp(),
    end_time => timestamp() | undefined,
    duration_us => integer() | undefined,
    attributes => map(),
    events => [event()],
    status => ok | error,
    error_details => map() | undefined
}.

-type event() :: #{
    timestamp => timestamp(),
    name => binary(),
    attributes => map()
}.

%%====================================================================
%% Process Dictionary Keys
%%====================================================================

-define(TRACE_KEY, '$erlmcp_trace_context').
-define(SPAN_STACK_KEY, '$erlmcp_span_stack').

%%====================================================================
%% API Functions
%%====================================================================

%% Start a new trace with a name
-spec start_trace(binary()) -> trace_id().
start_trace(TraceName) ->
    start_trace(TraceName, #{}).

%% Start a new trace with name and metadata
-spec start_trace(binary(), map()) -> trace_id().
start_trace(TraceName, Metadata) ->
    TraceId = generate_trace_id(),
    StartTime = erlang:monotonic_time(microsecond),
    
    Context = #{
        trace_id => TraceId,
        spans => [],
        start_time => StartTime,
        metadata => Metadata#{trace_name => TraceName}
    },
    
    put(?TRACE_KEY, Context),
    put(?SPAN_STACK_KEY, []),
    
    ?LOG_INFO("Trace started", #{
        trace_id => TraceId,
        trace_name => TraceName,
        timestamp => StartTime,
        metadata => Metadata
    }),
    
    TraceId.

%% Add a span to the current trace
-spec add_span(binary(), map()) -> span_id().
add_span(OperationName, Attributes) ->
    add_span(OperationName, Attributes, #{}).

%% Add a span with options
-spec add_span(binary(), map(), map()) -> span_id().
add_span(OperationName, Attributes, Options) ->
    case get(?TRACE_KEY) of
        undefined ->
            %% Auto-start trace if none exists
            start_trace(<<"auto_trace">>),
            add_span(OperationName, Attributes, Options);
        Context ->
            add_span_to_context(Context, OperationName, Attributes, Options)
    end.

%% Add a span with parent span ID
-spec add_span(binary(), map(), map(), span_id() | undefined) -> span_id().
add_span(OperationName, Attributes, Options, ParentSpanId) ->
    case get(?TRACE_KEY) of
        undefined ->
            start_trace(<<"auto_trace">>),
            add_span(OperationName, Attributes, Options, ParentSpanId);
        Context ->
            add_span_to_context(Context, OperationName, Attributes, Options, ParentSpanId)
    end.

%% End the current trace
-spec end_trace(trace_id()) -> ok.
end_trace(TraceId) ->
    case get(?TRACE_KEY) of
        #{trace_id := TraceId} = Context ->
            end_trace_context(Context);
        #{trace_id := OtherTraceId} ->
            ?LOG_WARNING("Attempted to end trace with wrong ID", #{
                expected_trace_id => TraceId,
                current_trace_id => OtherTraceId
            }),
            ok;
        undefined ->
            ?LOG_WARNING("No active trace to end", #{trace_id => TraceId}),
            ok
    end.

%% Get current trace context
-spec current_trace() -> trace_context() | undefined.
current_trace() ->
    get(?TRACE_KEY).

%% Format trace as JSON for external analysis
-spec format_trace_json(trace_context()) -> binary().
format_trace_json(Context) ->
    JsonData = format_trace_for_json(Context),
    jsx:encode(JsonData).

%%====================================================================
%% Convenience Functions
%%====================================================================

%% Trace a transport operation
-spec trace_transport_operation(binary(), atom(), fun(() -> Result)) -> Result.
trace_transport_operation(Operation, TransportType, Fun) ->
    SpanId = add_span(Operation, #{
        component => transport,
        transport_type => TransportType,
        operation => Operation
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    try
        Result = Fun(),
        EndTime = erlang:monotonic_time(microsecond),
        end_span(SpanId, ok, EndTime - StartTime),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime2 = erlang:monotonic_time(microsecond),
            end_span(SpanId, error, EndTime2 - StartTime, #{
                error_class => Class,
                error_reason => format_term(Reason),
                stacktrace => format_stacktrace(Stacktrace)
            }),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Trace a server operation
-spec trace_server_operation(binary(), atom(), fun(() -> Result)) -> Result.
trace_server_operation(Operation, ServerId, Fun) ->
    SpanId = add_span(Operation, #{
        component => server,
        server_id => ServerId,
        operation => Operation
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    try
        Result = Fun(),
        EndTime = erlang:monotonic_time(microsecond),
        end_span(SpanId, ok, EndTime - StartTime),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime2 = erlang:monotonic_time(microsecond),
            end_span(SpanId, error, EndTime2 - StartTime, #{
                error_class => Class,
                error_reason => format_term(Reason),
                stacktrace => format_stacktrace(Stacktrace)
            }),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Trace a registry operation
-spec trace_registry_operation(binary(), fun(() -> Result)) -> Result.
trace_registry_operation(Operation, Fun) ->
    SpanId = add_span(Operation, #{
        component => registry,
        operation => Operation
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    try
        Result = Fun(),
        EndTime = erlang:monotonic_time(microsecond),
        end_span(SpanId, ok, EndTime - StartTime),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime2 = erlang:monotonic_time(microsecond),
            end_span(SpanId, error, EndTime2 - StartTime, #{
                error_class => Class,
                error_reason => format_term(Reason),
                stacktrace => format_stacktrace(Stacktrace)
            }),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Trace message processing
-spec trace_message_processing(binary(), binary(), integer(), fun(() -> Result)) -> Result.
trace_message_processing(Method, MessageId, MessageSize, Fun) ->
    SpanId = add_span(<<"message_processing">>, #{
        component => message_handler,
        method => Method,
        message_id => MessageId,
        message_size => MessageSize
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    try
        Result = Fun(),
        EndTime = erlang:monotonic_time(microsecond),
        end_span(SpanId, ok, EndTime - StartTime, #{
            processing_time_us => EndTime - StartTime,
            throughput_msgs_per_sec => 1000000 / (EndTime - StartTime)
        }),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime2 = erlang:monotonic_time(microsecond),
            end_span(SpanId, error, EndTime2 - StartTime, #{
                error_class => Class,
                error_reason => format_term(Reason),
                stacktrace => format_stacktrace(Stacktrace)
            }),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Generate a unique trace ID
-spec generate_trace_id() -> trace_id().
generate_trace_id() ->
    <<A:32, B:32, C:32, D:32>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b~8.16.0b~8.16.0b~8.16.0b", [A, B, C, D])).

%% Generate a unique span ID
-spec generate_span_id() -> span_id().
generate_span_id() ->
    <<A:32, B:32>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("~8.16.0b~8.16.0b", [A, B])).

%% Add span to context with auto-generated parent
-spec add_span_to_context(trace_context(), binary(), map(), map()) -> span_id().
add_span_to_context(Context, OperationName, Attributes, Options) ->
    SpanStack = get(?SPAN_STACK_KEY),
    ParentSpanId = case SpanStack of
        [CurrentSpanId | _] -> CurrentSpanId;
        [] -> undefined
    end,
    add_span_to_context(Context, OperationName, Attributes, Options, ParentSpanId).

%% Add span to context with explicit parent
-spec add_span_to_context(trace_context(), binary(), map(), map(), span_id() | undefined) -> span_id().
add_span_to_context(#{trace_id := TraceId, spans := Spans} = Context, 
                   OperationName, Attributes, _Options, ParentSpanId) ->
    SpanId = generate_span_id(),
    StartTime = erlang:monotonic_time(microsecond),
    
    Span = #{
        span_id => SpanId,
        parent_id => ParentSpanId,
        operation_name => OperationName,
        start_time => StartTime,
        end_time => undefined,
        duration_us => undefined,
        attributes => Attributes,
        events => [],
        status => ok,
        error_details => undefined
    },
    
    UpdatedContext = Context#{spans => [Span | Spans]},
    put(?TRACE_KEY, UpdatedContext),
    
    %% Update span stack
    SpanStack = get(?SPAN_STACK_KEY),
    put(?SPAN_STACK_KEY, [SpanId | SpanStack]),
    
    ?LOG_DEBUG("Span started", #{
        trace_id => TraceId,
        span_id => SpanId,
        parent_span_id => ParentSpanId,
        operation_name => OperationName,
        attributes => Attributes,
        timestamp => StartTime
    }),
    
    SpanId.

%% End a span
-spec end_span(span_id(), ok | error, integer()) -> ok.
end_span(SpanId, Status, DurationUs) ->
    end_span(SpanId, Status, DurationUs, #{}).

%% End a span with additional attributes
-spec end_span(span_id(), ok | error, integer(), map()) -> ok.
end_span(SpanId, Status, DurationUs, AdditionalAttrs) ->
    case get(?TRACE_KEY) of
        #{spans := Spans} = Context ->
            EndTime2 = erlang:monotonic_time(microsecond),
            
            UpdatedSpans = lists:map(fun(Span) ->
                case maps:get(span_id, Span) of
                    SpanId ->
                        UpdatedAttributes = maps:merge(
                            maps:get(attributes, Span, #{}),
                            AdditionalAttrs
                        ),
                        ErrorDetails = case Status of
                            error -> AdditionalAttrs;
                            ok -> undefined
                        end,
                        Span#{
                            end_time => EndTime2,
                            duration_us => DurationUs,
                            status => Status,
                            attributes => UpdatedAttributes,
                            error_details => ErrorDetails
                        };
                    _ ->
                        Span
                end
            end, Spans),
            
            put(?TRACE_KEY, Context#{spans => UpdatedSpans}),
            
            %% Update span stack
            SpanStack = get(?SPAN_STACK_KEY),
            UpdatedStack = lists:delete(SpanId, SpanStack),
            put(?SPAN_STACK_KEY, UpdatedStack),
            
            ?LOG_DEBUG("Span ended", #{
                span_id => SpanId,
                status => Status,
                duration_us => DurationUs,
                timestamp => EndTime2,
                additional_attributes => AdditionalAttrs
            }),
            ok;
        undefined ->
            ?LOG_WARNING("No active trace context for span", #{span_id => SpanId}),
            ok
    end.

%% End trace context and log final summary
-spec end_trace_context(trace_context()) -> ok.
end_trace_context(#{trace_id := TraceId, start_time := StartTime, spans := Spans, metadata := Metadata}) ->
    EndTime = erlang:monotonic_time(microsecond),
    TotalDuration = EndTime - StartTime,
    
    %% Calculate trace statistics
    SpanCount = length(Spans),
    ErrorCount = length([S || S <- Spans, maps:get(status, S) =:= error]),
    AvgSpanDuration = case SpanCount of
        0 -> 0;
        _ ->
            %% Calculate duration for spans that have it, otherwise use current time - start time
            TotalSpanDuration = lists:sum([
                case maps:get(duration_us, S, undefined) of
                    undefined ->
                        %% Calculate duration if span wasn't explicitly ended
                        SpanStartTime = maps:get(start_time, S),
                        EndTime - SpanStartTime;
                    Duration ->
                        Duration
                end || S <- Spans
            ]),
            TotalSpanDuration / SpanCount
    end,
    
    TraceJson = format_trace_json(#{
        trace_id => TraceId,
        spans => Spans,
        start_time => StartTime,
        metadata => Metadata
    }),
    
    ?LOG_INFO("Trace completed", #{
        trace_id => TraceId,
        total_duration_us => TotalDuration,
        span_count => SpanCount,
        error_count => ErrorCount,
        avg_span_duration_us => round(AvgSpanDuration),
        metadata => Metadata,
        trace_json => TraceJson
    }),
    
    %% Clear process dictionary
    erase(?TRACE_KEY),
    erase(?SPAN_STACK_KEY),
    ok.

%% Format trace for JSON export
-spec format_trace_for_json(trace_context()) -> map().
format_trace_for_json(#{trace_id := TraceId, spans := Spans, start_time := StartTime, metadata := Metadata}) ->
    FormattedSpans = [format_span_for_json(Span) || Span <- Spans],
    #{
        trace_id => TraceId,
        start_time => StartTime,
        end_time => erlang:monotonic_time(microsecond),
        metadata => Metadata,
        spans => FormattedSpans
    }.

%% Format span for JSON export
-spec format_span_for_json(span()) -> map().
format_span_for_json(Span) ->
    BaseSpan = #{
        span_id => maps:get(span_id, Span),
        parent_id => maps:get(parent_id, Span, null),
        operation_name => maps:get(operation_name, Span),
        start_time => maps:get(start_time, Span),
        end_time => maps:get(end_time, Span, null),
        duration_us => maps:get(duration_us, Span, null),
        status => maps:get(status, Span),
        attributes => maps:get(attributes, Span, #{}),
        events => maps:get(events, Span, [])
    },
    case maps:get(error_details, Span) of
        undefined -> BaseSpan;
        ErrorDetails -> BaseSpan#{error_details => ErrorDetails}
    end.

%% Format term for logging
-spec format_term(term()) -> binary().
format_term(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

%% Format stacktrace for logging
-spec format_stacktrace(list()) -> list().
format_stacktrace(Stacktrace) when length(Stacktrace) > 10 ->
    %% Limit stacktrace to prevent log spam
    {First, _} = lists:split(10, Stacktrace),
    First ++ [{'...', truncated, []}];
format_stacktrace(Stacktrace) ->
    Stacktrace.