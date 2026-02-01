%%%-------------------------------------------------------------------
%%% @doc
%%% Tracing wrapper module for ErlMCP
%%%
%%% This module provides a simplified tracing interface that wraps
%%% erlmcp_otel functionality for backwards compatibility with existing
%%% code. It serves as an abstraction layer over OpenTelemetry operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tracing).

%% Public API
-export([start_span/1, start_span/2, start_server_span/2, start_transport_span/3, end_span/1,
         set_attributes/2, add_span_attribute/3, set_status/2, record_error_details/3,
         record_exception/4, record_exception/3, record_performance_metrics/2,
         record_message_metrics/3, log/2, normalize_attr_key/1, normalize_attr_value/1]).

%% Include OTEL tracer header (disabled until dependency is available)
%% -include_lib("opentelemetry_api/include/otel_tracer.hrl").

%%====================================================================
%% Types
%%====================================================================

-type span_context() :: term().
-type attr_key() :: binary() | atom().
-type attr_value() :: binary() | atom() | number() | map() | list().

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a new span with default attributes
-spec start_span(binary()) -> span_context().
start_span(Name) ->
    start_span(Name, #{}).

%% @doc Start a new span with custom attributes
-spec start_span(binary(), map()) -> span_context().
start_span(Name, Attributes) ->
    %% Delegate to erlmcp_otel
    case catch erlmcp_otel:start_span(Name, normalize_attributes(Attributes)) of
        {'EXIT', {undef, _}} ->
            %% Fallback if erlmcp_otel is not available
            create_fallback_span(Name, Attributes);
        SpanCtx ->
            SpanCtx
    end.

%% @doc Start a server span with server ID
-spec start_server_span(binary(), atom()) -> span_context().
start_server_span(Name, ServerId) ->
    Attributes = #{<<"span.kind">> => <<"server">>, <<"server.id">> => ServerId},
    start_span(Name, Attributes).

%% @doc Start a transport span with transport details
-spec start_transport_span(binary(), term(), atom()) -> span_context().
start_transport_span(Name, TransportId, TransportType) ->
    Attributes =
        #{<<"span.kind">> => <<"client">>,
          <<"transport.id">> => TransportId,
          <<"transport.type">> => TransportType},
    start_span(Name, Attributes).

%% @doc End a span
-spec end_span(span_context()) -> ok.
end_span(SpanCtx) ->
    case catch erlmcp_otel:end_span(SpanCtx) of
        {'EXIT', {undef, _}} ->
            %% Fallback: do nothing
            ok;
        Result ->
            Result
    end.

%% @doc Set multiple attributes on a span
-spec set_attributes(span_context(), map()) -> ok.
set_attributes(SpanCtx, Attributes) ->
    case catch erlmcp_otel:add_attributes(SpanCtx, normalize_attributes(Attributes)) of
        {'EXIT', {undef, _}} ->
            %% Fallback: do nothing
            ok;
        Result ->
            Result
    end.

%% @doc Add a single attribute to a span
-spec add_span_attribute(span_context(), attr_key(), attr_value()) -> ok.
add_span_attribute(SpanCtx, Key, Value) ->
    NormalizedKey = normalize_attr_key(Key),
    NormalizedValue = normalize_attr_value(Value),
    set_attributes(SpanCtx, #{NormalizedKey => NormalizedValue}).

%% @doc Set the status of a span
-spec set_status(span_context(), atom()) -> ok.
set_status(SpanCtx, Status) when Status =:= ok; Status =:= error ->
    %% For now, we record this as an attribute
    %% In full OTEL implementation, this would set span status
    set_attributes(SpanCtx, #{<<"status">> => Status});
set_status(_SpanCtx, _Status) ->
    ok.

%% @doc Record error details with a specific error type
-spec record_error_details(span_context(), atom(), term()) -> ok.
record_error_details(SpanCtx, ErrorType, Details) ->
    ErrorAttrs =
        #{<<"error.type">> => normalize_attr_key(ErrorType),
          <<"error.details">> => normalize_attr_value(Details),
          <<"error">> => true},
    case catch erlmcp_otel:add_event(SpanCtx, <<"error.details">>, ErrorAttrs) of
        {'EXIT', {undef, _}} ->
            %% Fallback to attributes
            set_attributes(SpanCtx, ErrorAttrs);
        Result ->
            Result
    end.

%% @doc Record an exception (Class, Reason, Stacktrace)
-spec record_exception(span_context(), atom(), term(), list()) -> ok.
record_exception(SpanCtx, Class, Reason, Stacktrace) ->
    ErrorTuple = {Class, Reason, Stacktrace},
    case catch erlmcp_otel:record_error(SpanCtx, ErrorTuple) of
        {'EXIT', {undef, _}} ->
            %% Fallback: record as attributes
            ErrorAttrs =
                #{<<"error.class">> => normalize_attr_key(Class),
                  <<"error.reason">> => normalize_attr_value(Reason),
                  <<"error.stacktrace">> => normalize_attr_value(Stacktrace),
                  <<"error">> => true},
            set_attributes(SpanCtx, ErrorAttrs);
        Result ->
            Result
    end.

%% @doc Record an exception (Class, Reason)
-spec record_exception(span_context(), atom(), term()) -> ok.
record_exception(SpanCtx, Class, Reason) ->
    record_exception(SpanCtx, Class, Reason, []).

%% @doc Record performance metrics as span attributes
-spec record_performance_metrics(span_context(), map()) -> ok.
record_performance_metrics(SpanCtx, Metrics) ->
    %% Normalize and add as attributes
    NormalizedMetrics = normalize_attributes(Metrics),
    set_attributes(SpanCtx, NormalizedMetrics).

%% @doc Record message metrics
-spec record_message_metrics(span_context(), binary(), non_neg_integer()) -> ok.
record_message_metrics(SpanCtx, Method, DataSize) ->
    Metrics = #{<<"rpc.method">> => normalize_attr_value(Method), <<"message.size">> => DataSize},
    record_performance_metrics(SpanCtx, Metrics).

%% @doc Log a message with context (delegates to logger)
-spec log(binary(), list()) -> ok.
log(Format, Args) ->
    logger:info(Format, Args).

%% @doc Normalize attribute keys to binary format
-spec normalize_attr_key(attr_key()) -> binary().
normalize_attr_key(Key) when is_binary(Key) ->
    Key;
normalize_attr_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
normalize_attr_key(Key) when is_list(Key) ->
    list_to_binary(Key);
normalize_attr_key(Key) when is_integer(Key) ->
    integer_to_binary(Key).

%% @doc Normalize attribute values to OTEL-compatible format
-spec normalize_attr_value(attr_value()) -> binary() | number() | boolean().
normalize_attr_value(Value) when is_binary(Value); is_number(Value); is_boolean(Value) ->
    Value;
normalize_attr_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize_attr_value(Value) when is_list(Value) ->
    try
        list_to_binary(Value)
    catch
        error:_ ->
            list_to_binary(io_lib:format("~p", [Value]))
    end;
normalize_attr_value(Value) when is_map(Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
normalize_attr_value(Value) when is_pid(Value) ->
    list_to_binary(pid_to_list(Value));
normalize_attr_value(Value) when is_reference(Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
normalize_attr_value(Value) when is_port(Value) ->
    list_to_binary(erlang:port_to_list(Value));
normalize_attr_value(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Normalize all attribute keys and values in a map
-spec normalize_attributes(map()) -> map().
normalize_attributes(Attributes) when is_map(Attributes) ->
    maps:fold(fun(Key, Value, Acc) ->
                 NormalizedKey = normalize_attr_key(Key),
                 NormalizedValue = normalize_attr_value(Value),
                 Acc#{NormalizedKey => NormalizedValue}
              end,
              #{},
              Attributes).

%% @private
%% Create a fallback span context when OTEL is not available
-spec create_fallback_span(binary(), map()) -> map().
create_fallback_span(Name, Attributes) ->
    #{span_name => Name,
      attributes => normalize_attributes(Attributes),
      start_time => erlang:system_time(nanosecond),
      fallback => true}.
