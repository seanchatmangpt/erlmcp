%%%-------------------------------------------------------------------
%%% @doc
%%% Jaeger Exporter for OpenTelemetry
%%%
%%% Exports traces to Jaeger via OTLP HTTP protocol. Supports both
%%% Jaeger native protocol and OTLP endpoints.
%%%
%%% == Configuration ==
%%%
%%% ```erlang
%%% {jaeger, #{
%%%     endpoint => "http://localhost:4318/v1/traces",  % OTLP endpoint
%%%     protocol => http_protobuf,
%%%     batch_timeout => 5000,
%%%     max_queue_size => 2048,
%%%     headers => [{"x-custom-header", "value"}]
%%% }}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_jaeger).

-include("erlmcp.hrl").

%% Public API
-export([
    init/1,
    export_spans/2,
    shutdown/1
]).

%% Internal API
-export([
    format_span/1,
    encode_batch/1,
    send_batch/2
]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type jaeger_config() :: #{
    endpoint := binary(),
    protocol := http_protobuf | http_json,
    batch_timeout := pos_integer(),
    max_queue_size := pos_integer(),
    headers := [{string(), string()}],
    service_name := binary()
}.

-type jaeger_state() :: #{
    config := jaeger_config(),
    queue := queue:queue(),
    timer := reference() | undefined
}.

-type jaeger_span() :: #{
    trace_id := binary(),
    span_id := binary(),
    parent_span_id := binary() | undefined,
    name := binary(),
    start_time := integer(),
    end_time := integer(),
    attributes := #{binary() => term()},
    events := [map()],
    status := atom()
}.

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Initialize Jaeger exporter
-spec init(jaeger_config()) -> {ok, jaeger_state()} | {error, term()}.
init(Config) ->
    %% Validate configuration
    case validate_config(Config) of
        ok ->
            State = #{
                config => Config,
                queue => queue:new(),
                timer => start_batch_timer(Config)
            },
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Export spans to Jaeger
-spec export_spans([jaeger_span()], jaeger_state()) -> {ok, jaeger_state()} | {error, term()}.
export_spans(Spans, #{queue := Queue, config := Config} = State) ->
    %% Add spans to queue
    NewQueue = lists:foldl(fun(Span, Q) ->
        queue:in(Span, Q)
    end, Queue, Spans),

    %% Check if we should flush
    QueueSize = queue:len(NewQueue),
    MaxQueueSize = maps:get(max_queue_size, Config, 2048),

    case QueueSize >= MaxQueueSize of
        true ->
            %% Flush immediately
            case flush_queue(NewQueue, Config) of
                ok -> {ok, State#{queue => queue:new()}};
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {ok, State#{queue => NewQueue}}
    end.

%% @doc Shutdown exporter and flush remaining spans
-spec shutdown(jaeger_state()) -> ok.
shutdown(#{queue := Queue, config := Config, timer := Timer}) ->
    %% Cancel timer
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,

    %% Flush remaining spans
    _ = flush_queue(Queue, Config),
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% @doc Format span for Jaeger export
-spec format_span(jaeger_span()) -> map().
format_span(#{
    trace_id := TraceId,
    span_id := SpanId,
    parent_span_id := ParentSpanId,
    name := Name,
    start_time := StartTime,
    end_time := EndTime,
    attributes := Attributes,
    events := Events,
    status := Status
}) ->
    #{
        <<"traceId">> => TraceId,
        <<"spanId">> => SpanId,
        <<"parentSpanId">> => ParentSpanId,
        <<"name">> => Name,
        <<"startTimeUnixNano">> => integer_to_binary(StartTime),
        <<"endTimeUnixNano">> => integer_to_binary(EndTime),
        <<"attributes">> => format_attributes(Attributes),
        <<"events">> => format_events(Events),
        <<"status">> => format_status(Status),
        <<"kind">> => <<"SPAN_KIND_INTERNAL">>
    }.

%% @doc Encode batch of spans for OTLP
-spec encode_batch([jaeger_span()]) -> binary().
encode_batch(Spans) ->
    FormattedSpans = [format_span(Span) || Span <- Spans],

    Batch = #{
        <<"resourceSpans">> => [#{
            <<"resource">> => #{
                <<"attributes">> => []
            },
            <<"scopeSpans">> => [#{
                <<"scope">> => #{
                    <<"name">> => <<"erlmcp">>,
                    <<"version">> => <<"2.0.0">>
                },
                <<"spans">> => FormattedSpans
            }]
        }]
    },

    jsx:encode(Batch).

%% @doc Send batch to Jaeger endpoint
-spec send_batch([jaeger_span()], jaeger_config()) -> ok | {error, term()}.
send_batch(Spans, #{endpoint := Endpoint, protocol := Protocol, headers := Headers}) ->
    Payload = encode_batch(Spans),

    ContentType = case Protocol of
        http_protobuf -> "application/x-protobuf";
        http_json -> "application/json"
    end,

    AllHeaders = [
        {"Content-Type", ContentType},
        {"Content-Length", integer_to_list(byte_size(Payload))}
        | Headers
    ],

    %% Send HTTP POST with timeout to prevent indefinite hangs
    %% 5000ms total timeout, 2000ms connect timeout - adequate for network round-trip
    HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
    case httpc:request(post, {binary_to_list(Endpoint), AllHeaders, ContentType, Payload}, HttpOptions, []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Code, _}, _, Body}} ->
            error_logger:error_msg("Jaeger export failed: HTTP ~p~n~p~n", [Code, Body]),
            {error, {http_error, Code}};
        {error, Reason} ->
            error_logger:error_msg("Jaeger export failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% =============================================================================
%% Private Helper Functions
%% =============================================================================

%% @private
%% Validate configuration
-spec validate_config(jaeger_config()) -> ok | {error, term()}.
validate_config(#{endpoint := Endpoint}) when is_binary(Endpoint) ->
    ok;
validate_config(_) ->
    {error, missing_endpoint}.

%% @private
%% Format attributes for Jaeger
-spec format_attributes(#{binary() => term()}) -> [map()].
format_attributes(Attributes) ->
    maps:fold(fun(Key, Value, Acc) ->
        [#{
            <<"key">> => Key,
            <<"value">> => format_attribute_value(Value)
        } | Acc]
    end, [], Attributes).

%% @private
%% Format attribute value
-spec format_attribute_value(term()) -> map().
format_attribute_value(Value) when is_binary(Value) ->
    #{<<"stringValue">> => Value};
format_attribute_value(Value) when is_integer(Value) ->
    #{<<"intValue">> => integer_to_binary(Value)};
format_attribute_value(Value) when is_float(Value) ->
    #{<<"doubleValue">> => float_to_binary(Value)};
format_attribute_value(Value) when is_boolean(Value) ->
    #{<<"boolValue">> => Value};
format_attribute_value(Value) ->
    #{<<"stringValue">> => list_to_binary(io_lib:format("~p", [Value]))}.

%% @private
%% Format events
-spec format_events([map()]) -> [map()].
format_events(Events) ->
    [format_event(Event) || Event <- Events].

%% @private
%% Format single event
-spec format_event(map()) -> map().
format_event(#{name := Name, timestamp := Timestamp, attributes := Attributes}) ->
    #{
        <<"name">> => Name,
        <<"timeUnixNano">> => integer_to_binary(Timestamp),
        <<"attributes">> => format_attributes(Attributes)
    }.

%% @private
%% Format status
-spec format_status(atom()) -> map().
format_status(ok) ->
    #{<<"code">> => <<"STATUS_CODE_OK">>};
format_status(error) ->
    #{<<"code">> => <<"STATUS_CODE_ERROR">>};
format_status(_) ->
    #{<<"code">> => <<"STATUS_CODE_UNSET">>}.

%% @private
%% Start batch timer
-spec start_batch_timer(jaeger_config()) -> reference().
start_batch_timer(#{batch_timeout := Timeout}) ->
    erlang:send_after(Timeout, self(), flush_batch).

%% @private
%% Flush queue to Jaeger
-spec flush_queue(queue:queue(), jaeger_config()) -> ok | {error, term()}.
flush_queue(Queue, Config) ->
    case queue:is_empty(Queue) of
        true -> ok;
        false ->
            Spans = queue:to_list(Queue),
            send_batch(Spans, Config)
    end.
