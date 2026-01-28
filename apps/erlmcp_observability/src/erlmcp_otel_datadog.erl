%%%-------------------------------------------------------------------
%%% @doc
%%% Datadog Exporter for OpenTelemetry
%%%
%%% Exports traces to Datadog Agent via OTLP HTTP protocol.
%%% Supports Datadog-specific tags and metrics.
%%%
%%% == Configuration ==
%%%
%%% ```erlang
%%% {datadog, #{
%%%     endpoint => "http://localhost:4318/v1/traces",  % Datadog Agent OTLP
%%%     api_key => <<"YOUR_DD_API_KEY">>,  % Optional, for direct API
%%%     env => <<"production">>,
%%%     service => <<"erlmcp">>,
%%%     version => <<"2.0.0">>,
%%%     tags => #{<<"team">> => <<"platform">>}
%%% }}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_datadog).

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
    add_datadog_tags/2,
    encode_batch/1
]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type datadog_config() :: #{
    endpoint := binary(),
    api_key => binary(),
    env := binary(),
    service := binary(),
    version := binary(),
    tags := #{binary() => binary()},
    batch_timeout := pos_integer(),
    max_queue_size := pos_integer()
}.

-type datadog_state() :: #{
    config := datadog_config(),
    queue := queue:queue(),
    timer := reference() | undefined
}.

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Initialize Datadog exporter
-spec init(datadog_config()) -> {ok, datadog_state()} | {error, term()}.
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

%% @doc Export spans to Datadog
-spec export_spans([map()], datadog_state()) -> {ok, datadog_state()} | {error, term()}.
export_spans(Spans, #{queue := Queue, config := Config} = State) ->
    %% Add Datadog-specific tags to spans
    TaggedSpans = [add_datadog_tags(Span, Config) || Span <- Spans],

    %% Add to queue
    NewQueue = lists:foldl(fun(Span, Q) ->
        queue:in(Span, Q)
    end, Queue, TaggedSpans),

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
-spec shutdown(datadog_state()) -> ok.
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

%% @doc Format span with Datadog conventions
-spec format_span(map()) -> map().
format_span(#{attributes := Attributes} = Span) ->
    %% Datadog expects specific attribute naming
    DDAttributes = maps:fold(fun(K, V, Acc) ->
        DDKey = map_to_datadog_key(K),
        Acc#{DDKey => V}
    end, #{}, Attributes),

    Span#{attributes => DDAttributes}.

%% @doc Add Datadog-specific tags to span
-spec add_datadog_tags(map(), datadog_config()) -> map().
add_datadog_tags(#{attributes := Attributes} = Span, Config) ->
    DDTags = #{
        <<"env">> => maps:get(env, Config, <<"development">>),
        <<"service">> => maps:get(service, Config, <<"erlmcp">>),
        <<"version">> => maps:get(version, Config, <<"2.0.0">>)
    },

    %% Merge custom tags
    CustomTags = maps:get(tags, Config, #{}),
    AllTags = maps:merge(maps:merge(Attributes, DDTags), CustomTags),

    Span#{attributes => AllTags}.

%% @doc Encode batch for Datadog
-spec encode_batch([map()]) -> binary().
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

%% =============================================================================
%% Private Helper Functions
%% =============================================================================

%% @private
%% Validate configuration
-spec validate_config(datadog_config()) -> ok | {error, term()}.
validate_config(#{endpoint := Endpoint}) when is_binary(Endpoint) ->
    ok;
validate_config(_) ->
    {error, missing_endpoint}.

%% @private
%% Map attribute keys to Datadog conventions
-spec map_to_datadog_key(binary()) -> binary().
map_to_datadog_key(<<"span.kind">>) -> <<"span.kind">>;
map_to_datadog_key(<<"service.name">>) -> <<"service">>;
map_to_datadog_key(<<"service.version">>) -> <<"version">>;
map_to_datadog_key(Key) -> Key.

%% @private
%% Start batch timer
-spec start_batch_timer(datadog_config()) -> reference().
start_batch_timer(#{batch_timeout := Timeout}) ->
    erlang:send_after(Timeout, self(), flush_batch);
start_batch_timer(_) ->
    erlang:send_after(5000, self(), flush_batch).

%% @private
%% Flush queue
-spec flush_queue(queue:queue(), datadog_config()) -> ok | {error, term()}.
flush_queue(Queue, Config) ->
    case queue:is_empty(Queue) of
        true -> ok;
        false ->
            Spans = queue:to_list(Queue),
            send_batch(Spans, Config)
    end.

%% @private
%% Send batch to Datadog
-spec send_batch([map()], datadog_config()) -> ok | {error, term()}.
send_batch(Spans, #{endpoint := Endpoint} = Config) ->
    Payload = encode_batch(Spans),

    Headers = case maps:get(api_key, Config, undefined) of
        undefined -> [];
        ApiKey -> [{"DD-API-KEY", binary_to_list(ApiKey)}]
    end,

    AllHeaders = [
        {"Content-Type", "application/json"},
        {"Content-Length", integer_to_list(byte_size(Payload))}
        | Headers
    ],

    case httpc:request(post, {binary_to_list(Endpoint), AllHeaders, "application/json", Payload}, [], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, 202, _}, _, _}} -> ok;  % Datadog accepts 202
        {ok, {{_, Code, _}, _, Body}} ->
            error_logger:error_msg("Datadog export failed: HTTP ~p~n~p~n", [Code, Body]),
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.
