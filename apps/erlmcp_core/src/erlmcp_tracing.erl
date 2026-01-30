%%% @doc Tracing stub module for erlmcp (placeholder for future OpenTelemetry integration)
%%% This module provides no-op tracing functions to avoid crashes when
%%% tracing is referenced in the code. Full tracing implementation will
%%% be added when OpenTelemetry dependencies are integrated.

-module(erlmcp_tracing).

%% API exports - all no-op for now
-export([
    start_server_span/2,
    start_span/1,
    start_span/2,
    end_span/1,
    set_attributes/2,
    set_status/2,
    record_exception/4,
    record_error_details/3,
    record_message_metrics/3,
    log/2
]).

%% Types
-type span_ctx() :: reference().
-type attribute_key() :: binary().
-type attribute_value() :: binary() | integer() | float() | boolean() | map().
-type attributes() :: #{attribute_key() => attribute_value()}.
-type status() :: ok | error | unset.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a server span (no-op)
-spec start_server_span(binary(), term()) -> span_ctx().
start_server_span(_Name, _ServerId) ->
    make_ref().

%% @doc Start a generic span (no-op)
-spec start_span(binary()) -> span_ctx().
start_span(_Name) ->
    make_ref().

%% @doc Start a span with parent (no-op)
-spec start_span(binary(), span_ctx() | undefined) -> span_ctx().
start_span(_Name, _Parent) ->
    make_ref().

%% @doc End a span (no-op)
-spec end_span(span_ctx()) -> ok.
end_span(_SpanCtx) ->
    ok.

%% @doc Set span attributes (no-op)
-spec set_attributes(span_ctx(), attributes()) -> ok.
set_attributes(_SpanCtx, _Attributes) ->
    ok.

%% @doc Set span status (no-op)
-spec set_status(span_ctx(), status()) -> ok.
set_status(_SpanCtx, _Status) ->
    ok.

%% @doc Record exception (no-op)
-spec record_exception(span_ctx(), term(), term(), term()) -> ok.
record_exception(_SpanCtx, _Class, _Reason, _Stacktrace) ->
    ok.

%% @doc Record error details (no-op)
-spec record_error_details(span_ctx(), term(), term()) -> ok.
record_error_details(_SpanCtx, _ErrorType, _Details) ->
    ok.

%% @doc Record message metrics (no-op)
-spec record_message_metrics(span_ctx(), binary(), non_neg_integer()) -> ok.
record_message_metrics(_SpanCtx, _Method, _DataSize) ->
    ok.

%% @doc Log a message (no-op)
-spec log(binary(), list()) -> ok.
log(_Format, _Args) ->
    ok.
