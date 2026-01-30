%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_metrics_aggregator - Time-Series Metrics Aggregation
%%%
%%% Aggregates metrics into time-series buckets with configurable retention.
%%% Calculates percentiles (p50, p95, p99, p999) for latency distributions.
%%%
%%% Buckets: 60s (last minute), 5min (last hour), 1hr (last 24 hours)
%%% Retention: 24 hours of historical data
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_aggregator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    record_metric/3,
    get_current_metrics/0,
    get_historical_metrics/2,
    get_percentiles/1,
    export_metrics/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(bucket, {
    timestamp :: integer(),
    throughput = 0 :: non_neg_integer(),
    latencies = [] :: [number()],
    errors = 0 :: non_neg_integer(),
    connections = 0 :: non_neg_integer(),
    memory_mb = 0 :: number(),
    cpu_percent = 0 :: number()
}).

-record(state, {
    current_bucket :: #bucket{},
    buckets_60s = [] :: [#bucket{}],    % Last 60 seconds
    buckets_5min = [] :: [#bucket{}],   % Last 12 x 5-minute buckets = 1 hour
    buckets_1hr = [] :: [#bucket{}],    % Last 24 x 1-hour buckets = 24 hours
    bucket_timer :: reference() | undefined,
    alert_thresholds = #{} :: map()
}).

-define(BUCKET_INTERVAL_60S, 1000).      % 1 second
-define(BUCKET_INTERVAL_5MIN, 300000).   % 5 minutes
-define(BUCKET_INTERVAL_1HR, 3600000).   % 1 hour
-define(MAX_BUCKETS_60S, 60).
-define(MAX_BUCKETS_5MIN, 12).
-define(MAX_BUCKETS_1HR, 24).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the metrics aggregator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Record a metric measurement
-spec record_metric(atom(), term(), number()) -> ok.
record_metric(Type, Name, Value) ->
    gen_server:cast(?MODULE, {record_metric, Type, Name, Value}).

%% @doc Get current aggregated metrics
-spec get_current_metrics() -> {ok, map()} | {error, term()}.
get_current_metrics() ->
    gen_server:call(?MODULE, get_current_metrics).

%% @doc Get historical metrics for a time range
-spec get_historical_metrics(pos_integer(), pos_integer()) -> {ok, [map()]} | {error, term()}.
get_historical_metrics(StartTime, EndTime) ->
    gen_server:call(?MODULE, {get_historical_metrics, StartTime, EndTime}).

%% @doc Calculate percentiles for a list of values
-spec get_percentiles([number()]) -> #{p50 => number(), p95 => number(), p99 => number(), p999 => number()}.
get_percentiles(Values) ->
    calculate_percentiles(Values).

%% @doc Export metrics to CSV or JSON format
-spec export_metrics(csv | json) -> {ok, binary()} | {error, term()}.
export_metrics(Format) ->
    gen_server:call(?MODULE, {export_metrics, Format}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?LOG_INFO("Starting metrics aggregator"),

    % Start bucket rotation timer
    {ok, TimerRef} = timer:send_interval(?BUCKET_INTERVAL_60S, self(), rotate_bucket),

    % Load default alert thresholds
    Thresholds = #{
        latency_p99_ms => 100,
        error_rate_percent => 5.0,
        memory_mb => 1024,
        connections => 10000
    },

    {ok, #state{
        current_bucket = #bucket{timestamp = erlang:system_time(millisecond)},
        bucket_timer = TimerRef,
        alert_thresholds = Thresholds
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call(get_current_metrics, _From, State) ->
    Metrics = build_current_metrics(State),
    {reply, {ok, Metrics}, State};

handle_call({get_historical_metrics, StartTime, EndTime}, _From, State) ->
    Historical = get_buckets_in_range(State, StartTime, EndTime),
    {reply, {ok, Historical}, State};

handle_call({export_metrics, Format}, _From, State) ->
    Result = export_metrics_internal(State, Format),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({record_metric, throughput, _Name, Value}, State) ->
    Bucket = State#state.current_bucket,
    NewBucket = Bucket#bucket{throughput = Bucket#bucket.throughput + round(Value)},
    {noreply, State#state{current_bucket = NewBucket}};

handle_cast({record_metric, latency, _Name, Value}, State) ->
    Bucket = State#state.current_bucket,
    NewLatencies = [Value | Bucket#bucket.latencies],
    NewBucket = Bucket#bucket{latencies = NewLatencies},
    {noreply, State#state{current_bucket = NewBucket}};

handle_cast({record_metric, error, _Name, _Value}, State) ->
    Bucket = State#state.current_bucket,
    NewBucket = Bucket#bucket{errors = Bucket#bucket.errors + 1},
    {noreply, State#state{current_bucket = NewBucket}};

handle_cast({record_metric, connections, _Name, Value}, State) ->
    Bucket = State#state.current_bucket,
    NewBucket = Bucket#bucket{connections = round(Value)},
    {noreply, State#state{current_bucket = NewBucket}};

handle_cast({record_metric, memory, _Name, Value}, State) ->
    Bucket = State#state.current_bucket,
    NewBucket = Bucket#bucket{memory_mb = Value},
    {noreply, State#state{current_bucket = NewBucket}};

handle_cast({record_metric, cpu, _Name, Value}, State) ->
    Bucket = State#state.current_bucket,
    NewBucket = Bucket#bucket{cpu_percent = Value},
    {noreply, State#state{current_bucket = NewBucket}};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(rotate_bucket, State) ->
    % Move current bucket to 60s history
    NewState = rotate_buckets(State),

    % Check for threshold violations and trigger alerts
    check_alerts(NewState),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.bucket_timer of
        undefined -> ok;
        TimerRef -> timer:cancel(TimerRef)
    end,
    ?LOG_INFO("Metrics aggregator stopped"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Rotate buckets and aggregate into larger time windows
-spec rotate_buckets(#state{}) -> #state{}.
rotate_buckets(State) ->
    CurrentBucket = State#state.current_bucket,
    Buckets60s = [CurrentBucket | State#state.buckets_60s],

    % Trim 60s buckets to max size
    TrimmedBuckets60s = lists:sublist(Buckets60s, ?MAX_BUCKETS_60S),

    % Every 5 minutes, aggregate into 5-minute bucket
    Now = erlang:system_time(millisecond),
    Buckets5min = maybe_aggregate_5min(Now, TrimmedBuckets60s, State#state.buckets_5min),

    % Every hour, aggregate into 1-hour bucket
    Buckets1hr = maybe_aggregate_1hr(Now, Buckets5min, State#state.buckets_1hr),

    State#state{
        current_bucket = #bucket{timestamp = Now},
        buckets_60s = TrimmedBuckets60s,
        buckets_5min = Buckets5min,
        buckets_1hr = Buckets1hr
    }.

%% @doc Maybe aggregate 60s buckets into 5-minute bucket
-spec maybe_aggregate_5min(integer(), [#bucket{}], [#bucket{}]) -> [#bucket{}].
maybe_aggregate_5min(Now, Buckets60s, Buckets5min) ->
    case length(Buckets60s) >= 300 of  % 5 minutes = 300 seconds
        true ->
            Aggregated = aggregate_buckets(lists:sublist(Buckets60s, 300)),
            TrimmedBuckets5min = lists:sublist([Aggregated#bucket{timestamp = Now} | Buckets5min], ?MAX_BUCKETS_5MIN),
            TrimmedBuckets5min;
        false ->
            Buckets5min
    end.

%% @doc Maybe aggregate 5-minute buckets into 1-hour bucket
-spec maybe_aggregate_1hr(integer(), [#bucket{}], [#bucket{}]) -> [#bucket{}].
maybe_aggregate_1hr(Now, Buckets5min, Buckets1hr) ->
    case length(Buckets5min) >= 12 of  % 1 hour = 12 x 5-minute buckets
        true ->
            Aggregated = aggregate_buckets(lists:sublist(Buckets5min, 12)),
            TrimmedBuckets1hr = lists:sublist([Aggregated#bucket{timestamp = Now} | Buckets1hr], ?MAX_BUCKETS_1HR),
            TrimmedBuckets1hr;
        false ->
            Buckets1hr
    end.

%% @doc Aggregate multiple buckets into a single bucket
-spec aggregate_buckets([#bucket{}]) -> #bucket{}.
aggregate_buckets(Buckets) ->
    TotalThroughput = lists:sum([B#bucket.throughput || B <- Buckets]),
    AllLatencies = lists:flatten([B#bucket.latencies || B <- Buckets]),
    TotalErrors = lists:sum([B#bucket.errors || B <- Buckets]),
    AvgConnections = round(lists:sum([B#bucket.connections || B <- Buckets]) / length(Buckets)),
    AvgMemory = lists:sum([B#bucket.memory_mb || B <- Buckets]) / length(Buckets),
    AvgCpu = lists:sum([B#bucket.cpu_percent || B <- Buckets]) / length(Buckets),

    #bucket{
        timestamp = erlang:system_time(millisecond),
        throughput = TotalThroughput,
        latencies = AllLatencies,
        errors = TotalErrors,
        connections = AvgConnections,
        memory_mb = AvgMemory,
        cpu_percent = AvgCpu
    }.

%% @doc Build current metrics map
-spec build_current_metrics(#state{}) -> map().
build_current_metrics(State) ->
    Bucket = State#state.current_bucket,
    Percentiles = calculate_percentiles(Bucket#bucket.latencies),

    #{
        timestamp => Bucket#bucket.timestamp,
        throughput => Bucket#bucket.throughput,
        latency_p50_us => maps:get(p50, Percentiles, 0),
        latency_p95_us => maps:get(p95, Percentiles, 0),
        latency_p99_us => maps:get(p99, Percentiles, 0),
        latency_p999_us => maps:get(p999, Percentiles, 0),
        errors => Bucket#bucket.errors,
        connections => Bucket#bucket.connections,
        memory_mb => round(Bucket#bucket.memory_mb * 10) / 10,
        cpu_percent => round(Bucket#bucket.cpu_percent * 10) / 10,

        % Historical trends (last 60 seconds)
        throughput_1min => calculate_throughput_trend(State#state.buckets_60s),
        latency_trend_1min => calculate_latency_trend(State#state.buckets_60s),
        error_rate_1min => calculate_error_rate(State#state.buckets_60s)
    }.

%% @doc Calculate percentiles for latency values
-spec calculate_percentiles([number()]) -> map().
calculate_percentiles([]) ->
    #{p50 => 0, p95 => 0, p99 => 0, p999 => 0};
calculate_percentiles(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    #{
        p50 => percentile(Sorted, Len, 0.50),
        p95 => percentile(Sorted, Len, 0.95),
        p99 => percentile(Sorted, Len, 0.99),
        p999 => percentile(Sorted, Len, 0.999)
    }.

%% @doc Get percentile value from sorted list using linear interpolation
%% Uses the "linear interpolation of closest ranks" method which is more accurate
%% than the simple nearest-rank method, especially for small datasets.
%%
%% Algorithm:
%%   Position = 1 + (N - 1) * p
%%   Interpolate between floor(Position) and ceil(Position)
%%
-spec percentile([number()], pos_integer(), float()) -> number().
percentile([SingleValue], _Len, _Percent) ->
    SingleValue;
percentile(Sorted, Len, Percent) ->
    % Calculate position using linear interpolation method
    % This is the same method used by numpy.percentile with interpolation='linear'
    Pos = 1.0 + (Len - 1) * Percent,

    LowerIdx = max(1, floor(Pos)),
    UpperIdx = min(Len, ceil(Pos)),

    LowerVal = lists:nth(LowerIdx, Sorted),
    UpperVal = lists:nth(UpperIdx, Sorted),

    if LowerIdx =:= UpperIdx ->
        % Exact index match, no interpolation needed
        LowerVal;
    true ->
        % Linear interpolation between the two values
        Fraction = Pos - LowerIdx,
        LowerVal + Fraction * (UpperVal - LowerVal)
    end.

%% @doc Calculate throughput trend
-spec calculate_throughput_trend([#bucket{}]) -> number().
calculate_throughput_trend([]) -> 0;
calculate_throughput_trend(Buckets) ->
    lists:sum([B#bucket.throughput || B <- Buckets]) / length(Buckets).

%% @doc Calculate latency trend (p95)
-spec calculate_latency_trend([#bucket{}]) -> number().
calculate_latency_trend([]) -> 0;
calculate_latency_trend(Buckets) ->
    AllLatencies = lists:flatten([B#bucket.latencies || B <- Buckets]),
    Percentiles = calculate_percentiles(AllLatencies),
    maps:get(p95, Percentiles, 0).

%% @doc Calculate error rate (errors per second)
-spec calculate_error_rate([#bucket{}]) -> float().
calculate_error_rate([]) -> 0.0;
calculate_error_rate(Buckets) ->
    TotalErrors = lists:sum([B#bucket.errors || B <- Buckets]),
    TotalErrors / length(Buckets).

%% @doc Get buckets in time range
-spec get_buckets_in_range(#state{}, integer(), integer()) -> [map()].
get_buckets_in_range(State, StartTime, EndTime) ->
    AllBuckets = State#state.buckets_60s ++ State#state.buckets_5min ++ State#state.buckets_1hr,
    FilteredBuckets = lists:filter(fun(B) ->
        B#bucket.timestamp >= StartTime andalso B#bucket.timestamp =< EndTime
    end, AllBuckets),
    [bucket_to_map(B) || B <- FilteredBuckets].

%% @doc Convert bucket record to map
-spec bucket_to_map(#bucket{}) -> map().
bucket_to_map(Bucket) ->
    Percentiles = calculate_percentiles(Bucket#bucket.latencies),
    #{
        timestamp => Bucket#bucket.timestamp,
        throughput => Bucket#bucket.throughput,
        latency_p99 => maps:get(p99, Percentiles, 0),
        errors => Bucket#bucket.errors,
        connections => Bucket#bucket.connections,
        memory_mb => Bucket#bucket.memory_mb,
        cpu_percent => Bucket#bucket.cpu_percent
    }.

%% @doc Export metrics to CSV or JSON
-spec export_metrics_internal(#state{}, csv | json) -> {ok, binary()} | {error, term()}.
export_metrics_internal(State, json) ->
    AllBuckets = [State#state.current_bucket | State#state.buckets_60s]
                 ++ State#state.buckets_5min
                 ++ State#state.buckets_1hr,
    Data = [bucket_to_map(B) || B <- AllBuckets],
    {ok, jsx:encode(Data)};

export_metrics_internal(State, csv) ->
    AllBuckets = [State#state.current_bucket | State#state.buckets_60s]
                 ++ State#state.buckets_5min
                 ++ State#state.buckets_1hr,
    Header = <<"timestamp,throughput,latency_p99,errors,connections,memory_mb,cpu_percent\n">>,
    Rows = lists:map(fun(B) ->
        #{p99 := P99} = calculate_percentiles(B#bucket.latencies),
        io_lib:format("~p,~p,~p,~p,~p,~.2f,~.2f\n", [
            B#bucket.timestamp,
            B#bucket.throughput,
            P99,
            B#bucket.errors,
            B#bucket.connections,
            B#bucket.memory_mb,
            B#bucket.cpu_percent
        ])
    end, AllBuckets),
    {ok, iolist_to_binary([Header | Rows])}.

%% @doc Check alert thresholds and log warnings
-spec check_alerts(#state{}) -> ok.
check_alerts(State) ->
    Metrics = build_current_metrics(State),
    Thresholds = State#state.alert_thresholds,

    % Check latency threshold
    case maps:get(latency_p99_us, Metrics, 0) > maps:get(latency_p99_ms, Thresholds, 100) * 1000 of
        true ->
            ?LOG_WARNING("ALERT: Latency p99 exceeded threshold: ~p us", [maps:get(latency_p99_us, Metrics)]);
        false -> ok
    end,

    % Check error rate threshold
    ErrorRate = maps:get(error_rate_1min, Metrics, 0),
    case ErrorRate > maps:get(error_rate_percent, Thresholds, 5.0) of
        true ->
            ?LOG_WARNING("ALERT: Error rate exceeded threshold: ~p errors/sec", [ErrorRate]);
        false -> ok
    end,

    % Check memory threshold
    case maps:get(memory_mb, Metrics, 0) > maps:get(memory_mb, Thresholds, 1024) of
        true ->
            ?LOG_WARNING("ALERT: Memory usage exceeded threshold: ~p MB", [maps:get(memory_mb, Metrics)]);
        false -> ok
    end,

    ok.
