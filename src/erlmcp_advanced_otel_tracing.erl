%%%-------------------------------------------------------------------
%%% @doc
%%% Advanced OpenTelemetry Tracing for ErlMCP
%%%
%%% Extends basic OTEL support with distributed tracing capabilities:
%%% - Automatic correlation ID generation and propagation
%%% - W3C Trace Context (traceparent/tracestate) header handling
%%% - Distributed trace context across process boundaries
%%% - Advanced metrics aggregation (histograms, gauges, distributions)
%%% - Baggage correlation for request tracking
%%% - Request ID correlation across async boundaries
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_advanced_otel_tracing).

-export([
    %% Context management
    init_trace_context/2,
    init_trace_context/3,
    get_trace_context/0,
    set_trace_context/1,
    extract_trace_context/0,
    clear_trace_context/0,

    %% Correlation ID
    get_correlation_id/0,
    set_correlation_id/1,
    generate_correlation_id/0,

    %% W3C Trace Context
    w3c_traceparent/1,
    parse_w3c_traceparent/1,
    extract_w3c_from_headers/1,
    inject_w3c_to_headers/2,

    %% Advanced metrics
    record_histogram/2,
    record_gauge/2,
    record_counter/1,
    record_counter/2,
    get_metric_stats/1,

    %% Baggage correlation
    set_baggage/2,
    get_baggage/1,
    get_all_baggage/0,
    clear_baggage/0,

    %% Utilities
    create_span_with_correlation/2,
    create_span_with_correlation/3,
    propagate_to_spawn/1
]).

-include("erlmcp.hrl").

%% Process dictionary keys
-define(TRACE_CONTEXT_KEY, '$erlmcp_trace_context').
-define(CORRELATION_ID_KEY, '$erlmcp_correlation_id').
-define(BAGGAGE_KEY, '$erlmcp_baggage').

%% Metrics ETS tables
-define(HISTOGRAM_TABLE, erlmcp_adv_otel_histograms).
-define(GAUGE_TABLE, erlmcp_adv_otel_gauges).
-define(COUNTER_TABLE, erlmcp_adv_otel_counters).

%% Type definitions
-type trace_context() :: #{
    trace_id => binary(),
    parent_span_id => binary(),
    span_id => binary(),
    trace_flags => binary()
}.

-type baggage() :: #{binary() => binary()}.

%%====================================================================
%% Public API - Context Management
%%====================================================================

%% @doc Initialize trace context with correlation ID and parent span ID
-spec init_trace_context(binary(), binary()) -> ok.
init_trace_context(CorrelationId, ParentSpanId) ->
    init_trace_context(CorrelationId, ParentSpanId, <<"1">>).

%% @doc Initialize trace context with full parameters
-spec init_trace_context(binary(), binary(), binary()) -> ok.
init_trace_context(CorrelationId, ParentSpanId, TraceFlags) ->
    Context = #{
        trace_id => CorrelationId,
        parent_span_id => ParentSpanId,
        span_id => generate_span_id(),
        trace_flags => TraceFlags
    },
    put(?TRACE_CONTEXT_KEY, Context),
    put(?CORRELATION_ID_KEY, CorrelationId),
    ok.

%% @doc Get current trace context
-spec get_trace_context() -> trace_context() | undefined.
get_trace_context() ->
    get(?TRACE_CONTEXT_KEY).

%% @doc Set trace context from map
-spec set_trace_context(trace_context()) -> ok.
set_trace_context(Context) when is_map(Context) ->
    put(?TRACE_CONTEXT_KEY, Context),
    case maps:get(trace_id, Context, undefined) of
        undefined -> ok;
        TraceId -> put(?CORRELATION_ID_KEY, TraceId)
    end,
    ok.

%% @doc Extract trace context from process dictionary
-spec extract_trace_context() -> trace_context() | undefined.
extract_trace_context() ->
    get(?TRACE_CONTEXT_KEY).

%% @doc Clear trace context
-spec clear_trace_context() -> ok.
clear_trace_context() ->
    erase(?TRACE_CONTEXT_KEY),
    erase(?CORRELATION_ID_KEY),
    ok.

%%====================================================================
%% Public API - Correlation ID
%%====================================================================

%% @doc Get current correlation ID
-spec get_correlation_id() -> binary() | undefined.
get_correlation_id() ->
    get(?CORRELATION_ID_KEY).

%% @doc Set correlation ID
-spec set_correlation_id(binary()) -> ok.
set_correlation_id(CorrelationId) when is_binary(CorrelationId) ->
    put(?CORRELATION_ID_KEY, CorrelationId),
    ok.

%% @doc Generate a new correlation ID (UUID v4)
-spec generate_correlation_id() -> binary().
generate_correlation_id() ->
    erlang:list_to_binary(uuid:to_string(uuid:uuid4())).

%%====================================================================
%% Public API - W3C Trace Context
%%====================================================================

%% @doc Generate W3C traceparent header value from span context
-spec w3c_traceparent(trace_context() | undefined) -> binary().
w3c_traceparent(undefined) ->
    Context = get_trace_context(),
    w3c_traceparent(Context);
w3c_traceparent(Context) when is_map(Context) ->
    TraceId = maps:get(trace_id, Context, generate_correlation_id()),
    SpanId = maps:get(span_id, Context, generate_span_id()),
    TraceFlags = maps:get(trace_flags, Context, <<"01">>),
    Version = <<"00">>,
    Traceparent = <<Version/binary, "-", TraceId/binary, "-", SpanId/binary, "-", TraceFlags/binary>>,
    Traceparent.

%% @doc Parse W3C traceparent header value
-spec parse_w3c_traceparent(binary()) -> trace_context() | {error, invalid}.
parse_w3c_traceparent(Traceparent) when is_binary(Traceparent) ->
    try
        [Version, TraceId, SpanId, TraceFlags] = binary:split(Traceparent, <<"-">>, [global]),
        case Version of
            <<"00">> ->
                #{
                    trace_id => TraceId,
                    parent_span_id => SpanId,
                    span_id => generate_span_id(),
                    trace_flags => TraceFlags
                };
            _ -> {error, invalid}
        end
    catch
        _:_ -> {error, invalid}
    end.

%% @doc Extract W3C trace context from HTTP headers
-spec extract_w3c_from_headers(list() | map()) -> trace_context() | undefined.
extract_w3c_from_headers(Headers) when is_list(Headers) ->
    case lists:keyfind(<<"traceparent">>, 1, Headers) of
        {_, Value} -> parse_w3c_traceparent(Value);
        false -> undefined
    end;
extract_w3c_from_headers(Headers) when is_map(Headers) ->
    case maps:get(<<"traceparent">>, Headers, undefined) of
        undefined -> undefined;
        Value -> parse_w3c_traceparent(Value)
    end.

%% @doc Inject W3C trace context into HTTP headers
-spec inject_w3c_to_headers(trace_context() | undefined, list() | map()) -> list() | map().
inject_w3c_to_headers(Context, Headers) when is_list(Headers) ->
    Traceparent = w3c_traceparent(Context),
    lists:keystore(<<"traceparent">>, 1, Headers, {<<"traceparent">>, Traceparent});
inject_w3c_to_headers(Context, Headers) when is_map(Headers) ->
    Traceparent = w3c_traceparent(Context),
    maps:put(<<"traceparent">>, Traceparent, Headers).

%%====================================================================
%% Public API - Advanced Metrics
%%====================================================================

%% @doc Record histogram metric
-spec record_histogram(binary(), number()) -> ok.
record_histogram(MetricName, Value) when is_binary(MetricName), is_number(Value) ->
    ensure_tables_exist(),
    Key = MetricName,
    case ets:lookup(?HISTOGRAM_TABLE, Key) of
        [] ->
            ets:insert(?HISTOGRAM_TABLE, {Key, [Value]});
        [{Key, Values}] ->
            NewValues = [Value | Values],
            ets:update_element(?HISTOGRAM_TABLE, Key, {2, NewValues})
    end,
    ok.

%% @doc Record gauge metric (single value, not aggregated)
-spec record_gauge(binary(), number()) -> ok.
record_gauge(MetricName, Value) when is_binary(MetricName), is_number(Value) ->
    ensure_tables_exist(),
    ets:insert(?GAUGE_TABLE, {MetricName, Value, erlang:system_time(millisecond)}),
    ok.

%% @doc Record counter increment
-spec record_counter(binary()) -> ok.
record_counter(MetricName) ->
    record_counter(MetricName, 1).

%% @doc Record counter with specific increment
-spec record_counter(binary(), pos_integer()) -> ok.
record_counter(MetricName, Increment) when is_binary(MetricName), is_integer(Increment) ->
    ensure_tables_exist(),
    Key = MetricName,
    case ets:lookup(?COUNTER_TABLE, Key) of
        [] ->
            ets:insert(?COUNTER_TABLE, {Key, Increment});
        [{Key, CurrentCount}] ->
            ets:update_element(?COUNTER_TABLE, Key, {2, CurrentCount + Increment})
    end,
    ok.

%% @doc Get histogram statistics (min, max, mean, percentiles)
-spec get_metric_stats(binary()) -> #{
    count := non_neg_integer(),
    min := number(),
    max := number(),
    mean := number(),
    p50 := number(),
    p95 := number(),
    p99 := number()
} | undefined.
get_metric_stats(MetricName) ->
    ensure_tables_exist(),
    case ets:lookup(?HISTOGRAM_TABLE, MetricName) of
        [] -> undefined;
        [{_, Values}] ->
            SortedValues = lists:sort(Values),
            Count = length(SortedValues),
            Min = lists:min(SortedValues),
            Max = lists:max(SortedValues),
            Mean = calculate_mean(SortedValues),
            P50 = calculate_percentile(SortedValues, 50),
            P95 = calculate_percentile(SortedValues, 95),
            P99 = calculate_percentile(SortedValues, 99),
            #{
                count => Count,
                min => Min,
                max => Max,
                mean => Mean,
                p50 => P50,
                p95 => P95,
                p99 => P99
            }
    end.

%%====================================================================
%% Public API - Baggage Correlation
%%====================================================================

%% @doc Set baggage item for correlation tracking
-spec set_baggage(binary(), binary()) -> ok.
set_baggage(Key, Value) when is_binary(Key), is_binary(Value) ->
    Baggage = get_all_baggage(),
    NewBaggage = maps:put(Key, Value, Baggage),
    put(?BAGGAGE_KEY, NewBaggage),
    ok.

%% @doc Get baggage item
-spec get_baggage(binary()) -> binary() | undefined.
get_baggage(Key) when is_binary(Key) ->
    Baggage = get_all_baggage(),
    maps:get(Key, Baggage, undefined).

%% @doc Get all baggage items
-spec get_all_baggage() -> baggage().
get_all_baggage() ->
    case get(?BAGGAGE_KEY) of
        undefined -> #{};
        Baggage -> Baggage
    end.

%% @doc Clear all baggage
-spec clear_baggage() -> ok.
clear_baggage() ->
    erase(?BAGGAGE_KEY),
    ok.

%%====================================================================
%% Public API - Utilities
%%====================================================================

%% @doc Create span with automatic correlation ID injection
-spec create_span_with_correlation(binary(), map()) -> term().
create_span_with_correlation(SpanName, Attributes) ->
    create_span_with_correlation(SpanName, Attributes, #{}).

%% @doc Create span with correlation and options
-spec create_span_with_correlation(binary(), map(), map()) -> term().
create_span_with_correlation(SpanName, Attributes, Options) ->
    CorrelationId = get_correlation_id(),
    AttrsWithCorr = maps:put(<<"correlation.id">>, CorrelationId, Attributes),
    erlmcp_tracing:start_span(SpanName, AttrsWithCorr, Options).

%% @doc Prepare context propagation for spawned process
-spec propagate_to_spawn(fun()) -> fun().
propagate_to_spawn(Fun) when is_function(Fun) ->
    Context = extract_trace_context(),
    Baggage = get_all_baggage(),
    CorrelationId = get_correlation_id(),
    fun() ->
        set_trace_context(Context),
        put(?BAGGAGE_KEY, Baggage),
        put(?CORRELATION_ID_KEY, CorrelationId),
        Fun()
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate span ID (16 hex characters)
-spec generate_span_id() -> binary().
generate_span_id() ->
    Rand = crypto:strong_rand_bytes(8),
    erlang:list_to_binary(lists:flatten(io_lib:format("~16.16.0b", [binary:decode_unsigned(Rand)]))).

%% @doc Calculate mean of values
-spec calculate_mean(list(number())) -> float().
calculate_mean([]) -> 0.0;
calculate_mean(Values) ->
    Sum = lists:sum(Values),
    Sum / length(Values).

%% @doc Calculate percentile of sorted values
-spec calculate_percentile(list(number()), 0..100) -> number().
calculate_percentile([], _Percentile) -> 0;
calculate_percentile(Values, Percentile) ->
    Count = length(Values),
    Index = max(0, erlang:ceil((Percentile / 100.0) * Count) - 1),
    lists:nth(Index + 1, Values).

%% @doc Ensure ETS tables exist
-spec ensure_tables_exist() -> ok.
ensure_tables_exist() ->
    case ets:info(?HISTOGRAM_TABLE) of
        undefined ->
            ets:new(?HISTOGRAM_TABLE, [named_table, public, {write_concurrency, true}]),
            ets:new(?GAUGE_TABLE, [named_table, public, {write_concurrency, true}]),
            ets:new(?COUNTER_TABLE, [named_table, public, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.
