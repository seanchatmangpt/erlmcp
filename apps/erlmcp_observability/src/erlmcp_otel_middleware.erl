%%%-------------------------------------------------------------------
%%% @doc
%%% OpenTelemetry Middleware for Automatic Transport Tracing
%%%
%%% This module provides automatic tracing for all transport layer operations
%%% in erlmcp. It intercepts requests and responses, creating spans with
%%% proper annotations, error tracking, and context propagation.
%%%
%%% == Key Features ==
%%%
%%% 1. **Automatic Span Creation**: Wraps all transport operations
%%% 2. **Request/Response Tracking**: Annotates spans with timing and data
%%% 3. **Error Capture**: Automatic error recording with stack traces
%%% 4. **Span Events**: Standard events for request lifecycle
%%% 5. **Context Propagation**: Maintains trace context across boundaries
%%%
%%% == Span Events ==
%%%
%%% - `server.request_received` - When request arrives at transport
%%% - `server.processing_started` - Handler begins processing
%%% - `server.processing_completed` - Handler finishes successfully
%%% - `server.response_sent` - Response sent to client
%%% - `client.request_sent` - Client sends request
%%% - `client.response_received` - Client receives response
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Wrap transport operation
%%% Result = erlmcp_otel_middleware:trace_transport(
%%%     <<"tcp">>,
%%%     <<"send">>,
%%%     fun() -> Transport:send(Socket, Data) end,
%%%     #{socket => Socket, data_size => byte_size(Data)}
%%% ),
%%%
%%% %% Trace handler execution
%%% Response = erlmcp_otel_middleware:trace_handler(
%%%     <<"tools/call">>,
%%%     RequestId,
%%%     fun() -> handle_tool_call(ToolName, Args) end
%%% ).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_middleware).

-include("erlmcp.hrl").

%% Public API
-export([
    trace_transport/4,
    trace_handler/3,
    trace_handler/4,
    annotate_request/2,
    annotate_response/2,
    record_transport_error/3,
    wrap_rpc_call/3,
    wrap_rpc_response/3
]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type transport_type() :: tcp | http | stdio | websocket.
-type operation_name() :: binary().
-type handler_fun() :: fun(() -> term()).
-type middleware_opts() :: #{
    transport => transport_type(),
    operation => operation_name(),
    request_id => binary(),
    method => binary(),
    attributes => #{binary() => term()}
}.

%% =============================================================================
%% Public API - Transport Tracing
%% =============================================================================

%% @doc Trace a transport operation with automatic span management
%% Creates a span, executes the operation, and handles errors
-spec trace_transport(transport_type(), operation_name(), handler_fun(), map()) -> term().
trace_transport(TransportType, Operation, HandlerFun, Attributes) ->
    SpanName = iolist_to_binary([
        <<"mcp.transport.">>,
        atom_to_binary(TransportType),
        <<".">>,
        Operation
    ]),

    SpanAttributes = maps:merge(#{
        <<"transport.type">> => atom_to_binary(TransportType),
        <<"transport.operation">> => Operation,
        <<"span.kind">> => <<"internal">>
    }, Attributes),

    erlmcp_otel:with_span(SpanName, SpanAttributes, fun() ->
        %% Add start event
        SpanCtx = erlmcp_otel:get_current_context(),
        case SpanCtx of
            undefined -> ok;
            _ -> erlmcp_otel:add_event(SpanCtx, <<"transport.operation_started">>, #{
                <<"operation">> => Operation,
                <<"timestamp">> => erlang:system_time(nanosecond)
            })
        end,

        %% Execute operation
        Result = HandlerFun(),

        %% Add completion event
        case SpanCtx of
            undefined -> ok;
            _ -> erlmcp_otel:add_event(SpanCtx, <<"transport.operation_completed">>, #{
                <<"operation">> => Operation,
                <<"timestamp">> => erlang:system_time(nanosecond)
            })
        end,

        Result
    end).

%% @doc Trace a handler execution (server-side)
-spec trace_handler(binary(), binary(), handler_fun()) -> term().
trace_handler(Method, RequestId, HandlerFun) ->
    trace_handler(Method, RequestId, HandlerFun, #{}).

%% @doc Trace a handler execution with custom attributes
-spec trace_handler(binary(), binary(), handler_fun(), map()) -> term().
trace_handler(Method, RequestId, HandlerFun, CustomAttrs) ->
    SpanName = <<"mcp.handler.", Method/binary>>,

    Attributes = maps:merge(#{
        <<"rpc.method">> => Method,
        <<"rpc.request_id">> => RequestId,
        <<"span.kind">> => <<"server">>,
        <<"handler.type">> => classify_method(Method)
    }, CustomAttrs),

    erlmcp_otel:with_span(SpanName, Attributes, fun() ->
        SpanCtx = erlmcp_otel:get_current_context(),

        %% Event: request received
        case SpanCtx of
            undefined -> ok;
            _ -> erlmcp_otel:add_event(SpanCtx, <<"server.request_received">>, #{
                <<"request_id">> => RequestId,
                <<"method">> => Method
            })
        end,

        %% Event: processing started
        case SpanCtx of
            undefined -> ok;
            _ -> erlmcp_otel:add_event(SpanCtx, <<"server.processing_started">>, #{
                <<"timestamp">> => erlang:system_time(nanosecond)
            })
        end,

        %% Execute handler
        Result = HandlerFun(),

        %% Event: processing completed
        case SpanCtx of
            undefined -> ok;
            _ -> erlmcp_otel:add_event(SpanCtx, <<"server.processing_completed">>, #{
                <<"timestamp">> => erlang:system_time(nanosecond),
                <<"result_type">> => classify_result(Result)
            })
        end,

        %% Event: response sent
        case SpanCtx of
            undefined -> ok;
            _ -> erlmcp_otel:add_event(SpanCtx, <<"server.response_sent">>, #{
                <<"request_id">> => RequestId
            })
        end,

        Result
    end).

%% @doc Annotate span with request details
-spec annotate_request(map(), binary()) -> ok.
annotate_request(Request, RequestId) ->
    case erlmcp_otel:get_current_context() of
        undefined -> ok;
        SpanCtx ->
            Method = maps:get(<<"method">>, Request, <<"unknown">>),
            Params = maps:get(<<"params">>, Request, #{}),

            Attributes = #{
                <<"request.method">> => Method,
                <<"request.id">> => RequestId,
                <<"request.has_params">> => map_size(Params) > 0,
                <<"request.param_count">> => map_size(Params)
            },

            erlmcp_otel:add_attributes(SpanCtx, Attributes)
    end.

%% @doc Annotate span with response details
-spec annotate_response(term(), binary()) -> ok.
annotate_response(Response, RequestId) ->
    case erlmcp_otel:get_current_context() of
        undefined -> ok;
        SpanCtx ->
            Attributes = #{
                <<"response.id">> => RequestId,
                <<"response.type">> => classify_response(Response),
                <<"response.success">> => is_success_response(Response)
            },

            erlmcp_otel:add_attributes(SpanCtx, Attributes)
    end.

%% @doc Record transport error with stack trace
-spec record_transport_error(transport_type(), term(), list()) -> ok.
record_transport_error(TransportType, Reason, Stacktrace) ->
    case erlmcp_otel:get_current_context() of
        undefined -> ok;
        SpanCtx ->
            ExtraAttrs = #{
                <<"transport.type">> => atom_to_binary(TransportType),
                <<"transport.error">> => true
            },
            erlmcp_otel:record_error(SpanCtx, {error, Reason, Stacktrace}, ExtraAttrs)
    end.

%% @doc Wrap RPC call with automatic span injection
-spec wrap_rpc_call(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
wrap_rpc_call(Method, RequestId, Params) ->
    try
        %% Inject RPC span
        SpanCtx = erlmcp_otel:inject_rpc_span(Method, RequestId, Params),

        %% Add baggage for correlation
        ok = erlmcp_otel:propagate_baggage(request_id, RequestId),
        ok = erlmcp_otel:propagate_baggage(method, Method),

        %% Create trace context for propagation
        TraceCtx = erlmcp_otel:create_trace_ctx(SpanCtx),

        {ok, #{
            span_context => SpanCtx,
            trace_context => TraceCtx,
            headers => erlmcp_otel:propagate_context(SpanCtx)
        }}
    catch
        Class:Reason:Stack ->
            error_logger:error_msg("Failed to wrap RPC call: ~p:~p~n~p~n", [Class, Reason, Stack]),
            {error, {wrap_failed, Reason}}
    end.

%% @doc Wrap RPC response with span completion
-spec wrap_rpc_response(map(), term(), binary()) -> ok.
wrap_rpc_response(#{span_context := SpanCtx}, Response, RequestId) ->
    try
        %% Annotate with response details
        ok = annotate_response(Response, RequestId),

        %% Add response received event
        ok = erlmcp_otel:add_event(SpanCtx, <<"client.response_received">>, #{
            <<"request_id">> => RequestId,
            <<"timestamp">> => erlang:system_time(nanosecond)
        }),

        %% End span
        erlmcp_otel:end_span(SpanCtx)
    catch
        Class:Reason:Stack ->
            error_logger:error_msg("Failed to wrap RPC response: ~p:~p~n~p~n", [Class, Reason, Stack]),
            ok
    end;
wrap_rpc_response(_, _, _) ->
    ok.

%% =============================================================================
%% Private Helper Functions
%% =============================================================================

%% @private
%% Classify method by category
-spec classify_method(binary()) -> binary().
classify_method(<<"tools/", _/binary>>) -> <<"tool">>;
classify_method(<<"resources/", _/binary>>) -> <<"resource">>;
classify_method(<<"prompts/", _/binary>>) -> <<"prompt">>;
classify_method(<<"initialize">>) -> <<"lifecycle">>;
classify_method(<<"notifications/", _/binary>>) -> <<"notification">>;
classify_method(_) -> <<"other">>.

%% @private
%% Classify result type
-spec classify_result(term()) -> binary().
classify_result({ok, _}) -> <<"success">>;
classify_result({error, _}) -> <<"error">>;
classify_result(#mcp_content{}) -> <<"content">>;
classify_result(List) when is_list(List) -> <<"list">>;
classify_result(Map) when is_map(Map) -> <<"map">>;
classify_result(_) -> <<"unknown">>.

%% @private
%% Classify response type
-spec classify_response(term()) -> binary().
classify_response(#{<<"result">> := _}) -> <<"result">>;
classify_response(#{<<"error">> := _}) -> <<"error">>;
classify_response(#{<<"method">> := _}) -> <<"notification">>;
classify_response(_) -> <<"unknown">>.

%% @private
%% Check if response is successful
-spec is_success_response(term()) -> boolean().
is_success_response(#{<<"result">> := _}) -> true;
is_success_response({ok, _}) -> true;
is_success_response(_) -> false.
