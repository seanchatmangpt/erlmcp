%%%-------------------------------------------------------------------
%%% @doc
%%% Honeycomb Exporter for OpenTelemetry
%%%
%%% Exports traces to Honeycomb.io via OTLP HTTP protocol.
%%% Includes automatic sampling and rich event data.
%%%
%%% == Configuration ==
%%%
%%% ```erlang
%%% {honeycomb, #{
%%%     endpoint => "https://api.honeycomb.io",
%%%     api_key => <<"YOUR_HONEYCOMB_API_KEY">>,
%%%     dataset => <<"erlmcp-traces">>,
%%%     sample_rate => 10,  % Sample 1 in 10 traces
%%%     batch_timeout => 5000
%%% }}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_honeycomb).

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
    add_honeycomb_metadata/2,
    calculate_sample_rate/1
]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type honeycomb_config() :: #{
    endpoint := binary(),
    api_key := binary(),
    dataset := binary(),
    sample_rate => pos_integer(),
    batch_timeout := pos_integer(),
    max_queue_size := pos_integer(),
    environment => binary()
}.

-type honeycomb_state() :: #{
    config := honeycomb_config(),
    queue := queue:queue(),
    timer := reference() | undefined
}.

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Initialize Honeycomb exporter
-spec init(honeycomb_config()) -> {ok, honeycomb_state()} | {error, term()}.
init(Config) ->
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

%% @doc Export spans to Honeycomb
-spec export_spans([map()], honeycomb_state()) -> {ok, honeycomb_state()} | {error, term()}.
export_spans(Spans, #{queue := Queue, config := Config} = State) ->
    %% Apply sampling
    SampleRate = maps:get(sample_rate, Config, 1),
    SampledSpans = [Span || Span <- Spans, should_sample(Span, SampleRate)],

    %% Add Honeycomb metadata
    EnrichedSpans = [add_honeycomb_metadata(Span, Config) || Span <- SampledSpans],

    %% Add to queue
    NewQueue = lists:foldl(fun(Span, Q) ->
        queue:in(Span, Q)
    end, Queue, EnrichedSpans),

    %% Check if we should flush
    QueueSize = queue:len(NewQueue),
    MaxQueueSize = maps:get(max_queue_size, Config, 2048),

    case QueueSize >= MaxQueueSize of
        true ->
            case flush_queue(NewQueue, Config) of
                ok -> {ok, State#{queue => queue:new()}};
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {ok, State#{queue => NewQueue}}
    end.

%% @doc Shutdown exporter
-spec shutdown(honeycomb_state()) -> ok.
shutdown(#{queue := Queue, config := Config, timer := Timer}) ->
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,
    _ = flush_queue(Queue, Config),
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% @doc Format span for Honeycomb
-spec format_span(map()) -> map().
format_span(#{
    trace_id := TraceId,
    span_id := SpanId,
    parent_span_id := ParentSpanId,
    name := Name,
    start_time := StartTime,
    end_time := EndTime,
    attributes := Attributes
} = Span) ->
    DurationMs = (EndTime - StartTime) div 1000000,

    %% Honeycomb event format
    #{
        <<"trace.trace_id">> => TraceId,
        <<"trace.span_id">> => SpanId,
        <<"trace.parent_id">> => ParentSpanId,
        <<"name">> => Name,
        <<"timestamp">> => format_timestamp(StartTime),
        <<"duration_ms">> => DurationMs,
        <<"meta.span_type">> => <<"span">>,
        <<"attributes">> => Attributes
    }.

%% @doc Add Honeycomb-specific metadata
-spec add_honeycomb_metadata(map(), honeycomb_config()) -> map().
add_honeycomb_metadata(#{attributes := Attributes} = Span, Config) ->
    Metadata = #{
        <<"meta.dataset">> => maps:get(dataset, Config, <<"erlmcp">>),
        <<"meta.environment">> => maps:get(environment, Config, <<"development">>),
        <<"meta.local_hostname">> => list_to_binary(inet:gethostname()),
        <<"meta.beamtime">> => erlang:monotonic_time()
    },

    MergedAttributes = maps:merge(Attributes, Metadata),
    Span#{attributes => MergedAttributes}.

%% @doc Calculate sample rate for trace
-spec calculate_sample_rate(map()) -> pos_integer().
calculate_sample_rate(#{attributes := Attributes}) ->
    %% Higher sample rate for errors or high latency
    IsError = maps:get(<<"error">>, Attributes, false),
    DurationMs = maps:get(<<"duration_ms">>, Attributes, 0),

    case {IsError, DurationMs > 1000} of
        {true, _} -> 1;  % Always sample errors
        {_, true} -> 2;  % High sample rate for slow requests
        _ -> 10  % Normal sample rate
    end.

%% =============================================================================
%% Private Helper Functions
%% =============================================================================

%% @private
%% Validate configuration
-spec validate_config(honeycomb_config()) -> ok | {error, term()}.
validate_config(#{endpoint := Endpoint, api_key := ApiKey})
  when is_binary(Endpoint), is_binary(ApiKey) ->
    ok;
validate_config(_) ->
    {error, missing_required_fields}.

%% @private
%% Check if span should be sampled
-spec should_sample(map(), pos_integer()) -> boolean().
should_sample(_Span, 1) -> true;  % Always sample when rate is 1
should_sample(#{trace_id := TraceId}, SampleRate) ->
    %% Deterministic sampling based on trace ID
    <<Hash:64, _/binary>> = crypto:hash(md5, TraceId),
    (Hash rem SampleRate) =:= 0.

%% @private
%% Format timestamp for Honeycomb (ISO 8601)
-spec format_timestamp(integer()) -> binary().
format_timestamp(NanoTimestamp) ->
    MicroTimestamp = NanoTimestamp div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(MicroTimestamp div 1000000, microsecond),

    Micros = MicroTimestamp rem 1000000,

    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0BZ",
                      [Year, Month, Day, Hour, Minute, Second, Micros])
    ).

%% @private
%% Start batch timer
-spec start_batch_timer(honeycomb_config()) -> reference().
start_batch_timer(#{batch_timeout := Timeout}) ->
    erlang:send_after(Timeout, self(), flush_batch);
start_batch_timer(_) ->
    erlang:send_after(5000, self(), flush_batch).

%% @private
%% Flush queue
-spec flush_queue(queue:queue(), honeycomb_config()) -> ok | {error, term()}.
flush_queue(Queue, Config) ->
    case queue:is_empty(Queue) of
        true -> ok;
        false ->
            Spans = queue:to_list(Queue),
            send_batch(Spans, Config)
    end.

%% @private
%% Send batch to Honeycomb
-spec send_batch([map()], honeycomb_config()) -> ok | {error, term()}.
send_batch(Spans, #{endpoint := Endpoint, api_key := ApiKey, dataset := Dataset}) ->
    FormattedSpans = [format_span(Span) || Span <- Spans],
    Payload = jsx:encode(FormattedSpans),

    Headers = [
        {"X-Honeycomb-Team", binary_to_list(ApiKey)},
        {"X-Honeycomb-Dataset", binary_to_list(Dataset)},
        {"Content-Type", "application/json"},
        {"Content-Length", integer_to_list(byte_size(Payload))}
    ],

    URL = binary_to_list(<<Endpoint/binary, "/1/batch/", Dataset/binary>>),

    %% Send HTTP POST with timeout to prevent indefinite hangs
    %% 5000ms total timeout, 2000ms connect timeout - adequate for network round-trip
    HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
    case httpc:request(post, {URL, Headers, "application/json", Payload}, HttpOptions, []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, Code, _}, _, Body}} ->
            error_logger:error_msg("Honeycomb export failed: HTTP ~p~n~p~n", [Code, Body]),
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.
