-module(erlmcp_tracing).

-export([start_span/1, start_span/2, start_span/3, end_span/1, set_attributes/2,
         record_exception/3, record_exception/4, with_span/2, with_span/3, with_span/4,
         set_status/2, add_event/2, add_event/3, current_span_ctx/0, get_tracer/0,
         start_transport_span/3, start_server_span/2, start_registry_span/1,
         record_performance_metrics/2, record_message_metrics/3, record_error_details/3,
         normalize_attr_key/1, normalize_attr_value/1]).

    % Convenience functions

    % Testing helpers

%% Opentelemetry header may be unavailable in minimal test env; keep include for when present
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Default tracer name
-define(TRACER_NAME, 'erlmcp.tracer').
%% Common attribute keys
-define(TRANSPORT_ID, <<"transport.id">>).
-define(TRANSPORT_TYPE, <<"transport.type">>).
-define(SERVER_ID, <<"server.id">>).
-define(MESSAGE_SIZE, <<"message.size">>).
-define(REQUEST_ID, <<"request.id">>).
-define(METHOD, <<"method">>).
-define(RESOURCE_URI, <<"resource.uri">>).
-define(TOOL_NAME, <<"tool.name">>).
-define(ERROR_TYPE, <<"error.type">>).
-define(ERROR_MESSAGE, <<"error.message">>).
-define(OPERATION, <<"operation">>).
-define(COMPONENT, <<"component">>).
%% Performance metrics
-define(LATENCY_MS, <<"latency.ms">>).
-define(THROUGHPUT_OPS, <<"throughput.ops_per_sec">>).
-define(BUFFER_SIZE, <<"buffer.size">>).
-define(QUEUE_LENGTH, <<"queue.length">>).
-define(CONNECTION_COUNT, <<"connection.count">>).
-define(RETRY_COUNT, <<"retry.count">>).
-define(MEMORY_USAGE, <<"memory.usage_bytes">>).

%%====================================================================
%% Public API
%%====================================================================

%% Start a span with just a name
-spec start_span(binary()) -> term().
start_span(Name) ->
    start_span(Name, #{}).

%% Start a span with name and attributes
-spec start_span(binary(), map()) -> term().
start_span(Name, Attributes) ->
    start_span(Name, Attributes, #{}).

%% Start a span with name, attributes, and options
-spec start_span(binary(), map(), map()) -> term().
start_span(Name, Attributes, Options) ->
    case otel_available() of
        true ->
            Tracer = get_tracer(),
            SpanCtx = otel_tracer:start_span(Tracer, Name, Options),
            set_attributes(SpanCtx, Attributes),
            SpanCtx;
        false ->
            undefined
    end.

%% End a span
-spec end_span(term()) -> ok.
end_span(SpanCtx) ->
    case otel_available() of
        true ->
            otel_span:end_span(SpanCtx);
        false ->
            ok
    end,
    ok.

%% Set multiple attributes on a span
-spec set_attributes(term(), map()) -> ok.
set_attributes(_SpanCtx, Attrs) when map_size(Attrs) =:= 0 ->
    ok;
set_attributes(SpanCtx, Attributes) ->
    case otel_available() of
        true ->
            AttrList =
                maps:fold(fun(K, V, Acc) -> [{normalize_attr_key(K), normalize_attr_value(V)} | Acc]
                          end,
                          [],
                          Attributes),
            otel_span:set_attributes(SpanCtx, AttrList);
        false ->
            ok
    end,
    ok.

%% Record an exception with class and reason
-spec record_exception(term(), atom(), term()) -> ok.
record_exception(SpanCtx, Class, Reason) ->
    record_exception(SpanCtx, Class, Reason, []).

%% Record an exception with class, reason, and stacktrace
-spec record_exception(term(), atom(), term(), list()) -> ok.
record_exception(SpanCtx, Class, Reason, Stacktrace) ->
    case otel_available() of
        true ->
            otel_span:record_exception(SpanCtx, Class, Reason, Stacktrace),
            otel_span:set_status(SpanCtx, {error, format_error(Class, Reason)});
        false ->
            ok
    end,
    ok.

%% Execute a function within a span context
-spec with_span(binary(), fun(() -> Result)) -> Result.
with_span(Name, Fun) ->
    with_span(Name, #{}, Fun).

%% Execute a function within a span context with attributes
-spec with_span(binary(), map(), fun(() -> Result)) -> Result.
with_span(Name, Attributes, Fun) ->
    with_span(Name, Attributes, #{}, Fun).

%% Execute a function within a span context with attributes and options
-spec with_span(binary(), map(), map(), fun(() -> Result)) -> Result.
with_span(Name, Attributes, Options, Fun) ->
    case otel_available() of
        true ->
            SpanCtx = start_span(Name, Attributes, Options),
            try
                Result = Fun(),
                set_status(SpanCtx, ok),
                Result
            catch
                Class:Reason:Stacktrace ->
                    record_exception(SpanCtx, Class, Reason, Stacktrace),
                    erlang:raise(Class, Reason, Stacktrace)
            after
                end_span(SpanCtx)
            end;
        false ->
            Fun()
    end.

%% Set span status
-spec set_status(term(), ok | {error, term()}) -> ok.
set_status(SpanCtx, ok) ->
    case otel_available() of
        true ->
            otel_span:set_status(SpanCtx, opentelemetry:status(ok));
        false ->
            ok
    end,
    ok;
set_status(SpanCtx, {error, Reason}) ->
    case otel_available() of
        true ->
            otel_span:set_status(SpanCtx, opentelemetry:status(error, format_error(error, Reason)));
        false ->
            ok
    end,
    ok.

%% Add an event to the span
-spec add_event(term(), binary()) -> ok.
add_event(SpanCtx, Name) ->
    add_event(SpanCtx, Name, #{}).

%% Add an event with attributes to the span
-spec add_event(term(), binary(), map()) -> ok.
add_event(SpanCtx, Name, Attributes) ->
    case otel_available() of
        true ->
            AttrList =
                maps:fold(fun(K, V, Acc) -> [{normalize_attr_key(K), normalize_attr_value(V)} | Acc]
                          end,
                          [],
                          Attributes),
            otel_span:add_event(SpanCtx, Name, AttrList);
        false ->
            ok
    end,
    ok.

%% Get current span context
-spec current_span_ctx() -> term() | undefined.
current_span_ctx() ->
    case otel_available() of
        true ->
            otel_tracer:current_span_ctx();
        false ->
            undefined
    end.

%% Get the configured tracer
-spec get_tracer() -> term().
get_tracer() ->
    case otel_available() of
        true ->
            otel_tracer:get_tracer(?TRACER_NAME);
        false ->
            undefined
    end.

%%====================================================================
%% Convenience Functions for Transport Operations
%%====================================================================

%% Start transport operation span
start_transport_span(Operation, TransportId, TransportType) ->
    start_span(Operation,
               #{?COMPONENT => <<"transport">>,
                 ?TRANSPORT_ID => TransportId,
                 ?TRANSPORT_TYPE => TransportType,
                 ?OPERATION => Operation}).

%% Start server operation span
start_server_span(Operation, ServerId) ->
    start_span(Operation,
               #{?COMPONENT => <<"server">>,
                 ?SERVER_ID => ServerId,
                 ?OPERATION => Operation}).

%% Start registry operation span
-spec start_registry_span(binary()) -> term().
start_registry_span(Operation) ->
    start_span(Operation, #{?COMPONENT => <<"registry">>, ?OPERATION => Operation}).

%% Record performance metrics
-spec record_performance_metrics(term(), map()) -> ok.
record_performance_metrics(SpanCtx, Metrics) ->
    PerfAttrs =
        maps:fold(fun (latency, Value, Acc) when is_number(Value) ->
                          Acc#{?LATENCY_MS => Value};
                      (throughput, Value, Acc) when is_number(Value) ->
                          Acc#{?THROUGHPUT_OPS => Value};
                      (buffer_size, Value, Acc) when is_integer(Value) ->
                          Acc#{?BUFFER_SIZE => Value};
                      (queue_length, Value, Acc) when is_integer(Value) ->
                          Acc#{?QUEUE_LENGTH => Value};
                      (connection_count, Value, Acc) when is_integer(Value) ->
                          Acc#{?CONNECTION_COUNT => Value};
                      (retry_count, Value, Acc) when is_integer(Value) ->
                          Acc#{?RETRY_COUNT => Value};
                      (memory_usage, Value, Acc) when is_integer(Value) ->
                          Acc#{?MEMORY_USAGE => Value};
                      (_, _, Acc) ->
                          Acc
                  end,
                  #{},
                  Metrics),
    set_attributes(SpanCtx, PerfAttrs).

%% Record message processing metrics
-spec record_message_metrics(term(), binary(), integer()) -> ok.
record_message_metrics(SpanCtx, Method, Size) ->
    set_attributes(SpanCtx, #{?METHOD => Method, ?MESSAGE_SIZE => Size}).

%% Record error details
-spec record_error_details(term(), atom(), term()) -> ok.
%% Check if otel modules are available at runtime
otel_available() ->
    case code:which(otel_tracer) of
        non_existing ->
            false;
        _ ->
            true
    end.

record_error_details(SpanCtx, Type, Message) ->
    set_attributes(SpanCtx,
                   #{?ERROR_TYPE => Type, ?ERROR_MESSAGE => format_error(Type, Message)}).

%%====================================================================
%% Internal Functions
%%====================================================================

%% Normalize attribute key to binary
-spec normalize_attr_key(term()) -> binary().
normalize_attr_key(Key) when is_binary(Key) ->
    Key;
normalize_attr_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
normalize_attr_key(Key) when is_list(Key) ->
    list_to_binary(Key);
normalize_attr_key(Key) ->
    iolist_to_binary(io_lib:format("~p", [Key])).

%% Normalize attribute value
-spec normalize_attr_value(term()) -> term().
normalize_attr_value(Value) when is_binary(Value) ->
    Value;
normalize_attr_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize_attr_value(Value) when is_list(Value) ->
    list_to_binary(Value);
normalize_attr_value(Value) when is_integer(Value) ->
    Value;
normalize_attr_value(Value) when is_float(Value) ->
    Value;
normalize_attr_value(Value) when is_boolean(Value) ->
    Value;
normalize_attr_value(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

%% Format error for consistent error reporting
-spec format_error(atom(), term()) -> binary().
format_error(Class, Reason) ->
    iolist_to_binary(io_lib:format("~p:~p", [Class, Reason])).
