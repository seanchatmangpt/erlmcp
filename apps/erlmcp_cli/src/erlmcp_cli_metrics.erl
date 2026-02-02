%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_metrics - CLI Metrics Integration
%%%
%%% Integrates CLI operations with OTEL metrics collection system.
%%% Provides comprehensive metrics tracking for CLI operations.
%%%
%%% == Key Features ==
%%%
%%% 1. **Command Metrics**: Command execution success/failure rates
%%% 2. **Session Metrics**: Session lifecycle and activity metrics
%%% 3. **Performance Metrics**: Latency, throughput, and resource usage
%%% 4. **Error Metrics**: Error categorization and rate tracking
%%% 5. **Custom Metrics**: Application-specific metric collection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0, record_metric/2, record_counter/2, record_gauge/2,
         get_metrics/0, get_metric/1, reset_metrics/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(metric_data, {
    name :: binary(),
    type :: counter | gauge | histogram,
    value :: number(),
    timestamp :: integer(),
    tags :: map()
}).

-record(metrics_state, {
    metrics :: map(),                  % Metric name to metric data
    aggregators :: map(),              % Metric aggregators
    exporters :: list(),               % Active exporters
    config :: map()                    % Configuration
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_METRICS, #{
    "cli.commands.total" => 0,
    "cli.commands.success" => 0,
    "cli.commands.failed" => 0,
    "cli.commands.latency.p50" => 0,
    "cli.commands.latency.p95" => 0,
    "cli.commands.latency.p99" => 0,
    "cli.sessions.active" => 0,
    "cli.sessions.created" => 0,
    "cli.sessions.terminated" => 0,
    "cli.sessions.duration" => 0,
    "cli.throughput" => 0.0,
    "cli.errors.total" => 0,
    "cli.errors.timeout" => 0,
    "cli.errors.parsing" => 0,
    "cli.errors.execution" => 0,
    "cli.registry.commands" => 0,
    "cli.registry.lookups" => 0,
    "cli.registry.executions" => 0
}).

-define(DEFAULT_CONFIG, #{
    aggregation_interval => 10000,      % 10 seconds
    retention_period => 86400000,      % 24 hours
    exporters => [prometheus],
    enable_histograms => true,
    enable_tags => true
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the metrics system
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Record a metric value
-spec record_metric(binary(), number()) -> ok.
record_metric(Name, Value) when is_binary(Name), is_number(Value) ->
    gen_server:cast(?SERVER, {record_metric, Name, Value}).

%% @doc Record a counter increment
-spec record_counter(binary(), integer()) -> ok.
record_counter(Name, Increment) when is_binary(Name), is_integer(Increment) ->
    gen_server:cast(?SERVER, {record_counter, Name, Increment}).

%% @doc Record a gauge value
-spec record_gauge(binary(), number()) -> ok.
record_gauge(Name, Value) when is_binary(Name), is_number(Value) ->
    gen_server:cast(?SERVER, {record_gauge, Name, Value}).

%% @doc Get all metrics
-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%% @doc Get a specific metric
-spec get_metric(binary()) -> {ok, #metric_data{}} | {error, term()}.
get_metric(Name) ->
    gen_server:call(?SERVER, {get_metric, Name}).

%% @doc Reset all metrics
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:cast(?SERVER, reset_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the metrics system
-spec init(list()) -> {ok, #metrics_state{}}.
init(_Opts) ->
    %% Create OTEL span for metrics initialization
    SpanCtx = erlmcp_otel:with_span("cli.metrics.init", #{
        <<"metrics.init">> => true
    }, fun() ->
        ok
    end),

    %% Initialize state
    State = #metrics_state{
        metrics = ?DEFAULT_METRICS,
        aggregators = #{},
        exporters = init_exporters(),
        config = ?DEFAULT_CONFIG
    },

    %% Start aggregation timer
    AggregationTimer = erlang:start_timer(State#metrics_state.config#{aggregation_interval},
                                         self(), aggregate_metrics),

    %% Record initialization
    erlmcp_metrics:record("cli.metrics.initialized", 1),

    {ok, State#metrics_state{aggregators = #{aggregation_timer => AggregationTimer}}}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #metrics_state{}) ->
    {reply, term(), #metrics_state{}}.
handle_call(get_metrics, _From, State) ->
    %% Create span for metrics retrieval
    erlmcp_otel:inject_rpc_span(<<"cli.metrics.get">>,
                               make_request_id(),
                               #{},
                               undefined),

    %% Get all metrics
    MetricsMap = State#metrics_state.metrics,
    Aggregators = State#metrics_state.aggregators,

    %% Create response with aggregators
    Response = #{metrics => MetricsMap, aggregators => Aggregators},

    %% Record retrieval metrics
    erlmcp_metrics:record("cli.metrics.retrieved", 1),

    {reply, Response, State};

handle_call({get_metric, Name}, _From, State) ->
    %% Create span for metric retrieval
    erlmcp_otel:inject_rpc_span(<<"cli.metrics.get_single">>,
                               make_request_id(),
                               #{
                                   <<"metric.name">> => Name
                               },
                               undefined),

    case maps:find(Name, State#metrics_state.metrics) of
        {ok, MetricData} ->
            %% Record retrieval
            erlmcp_metrics:record("cli.metric.lookups", 1),
            {reply, {ok, MetricData}, State};
        error ->
            %% Record error
            erlmcp_metrics:record("cli.metric.errors", 1),
            {reply, {error, metric_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #metrics_state{}) -> {noreply, #metrics_state{}}.
handle_cast({record_metric, Name, Value}, State) ->
    %% Create span for metric recording
    erlmcp_otel:inject_rpc_span(<<"cli.metrics.record">>,
                               make_request_id(),
                               #{
                                   <<"metric.name">> => Name,
                                   <<"metric.value">> => Value
                               },
                               undefined),

    %% Record metric
    MetricData = #metric_data{
        name = Name,
        type = determine_metric_type(Name),
        value = Value,
        timestamp = erlang:system_time(millisecond),
        tags = extract_metric_tags(Name)
    },

    %% Update metrics
    Metrics = maps:put(Name, MetricData, State#metrics_state.metrics),

    %% Add to aggregator
    Aggregators = add_to_aggregator(Name, MetricData, State#metrics_state.aggregators),

    %% Export to external systems
    export_metric(MetricData, State#metrics_state.exporters),

    %% Update state
    NewState = State#metrics_state{
        metrics = Metrics,
        aggregators = Aggregators
    },

    {noreply, NewState};

handle_cast({record_counter, Name, Increment}, State) ->
    %% Get current value
    CurrentValue = case maps:find(Name, State#metrics_state.metrics) of
        {ok, #metric_data{value = Value}} -> Value;
        error -> 0
    end,

    %% Record as metric
    record_metric(Name, CurrentValue + Increment),

    {noreply, State};

handle_cast({record_gauge, Name, Value}, State) ->
    %% Record as metric
    record_metric(Name, Value),

    {noreply, State};

handle_cast(reset_metrics, State) ->
    %% Create span for metrics reset
    erlmcp_otel:inject_rpc_span(<<"cli.metrics.reset">>,
                               make_request_id(),
                               #{},
                               undefined),

    %% Reset all metrics
    ResetMetrics = maps:map(fun(_, _) -> 0 end, ?DEFAULT_METRICS),
    ResetAggregators = maps:map(fun(_, _) -> [] end, State#metrics_state.aggregators),

    %% Record reset
    erlmcp_metrics:record("cli.metrics.reset", 1),

    %% Export reset event
    export_reset_event(State#metrics_state.exporters),

    {noreply, State#metrics_state{
        metrics = ResetMetrics,
        aggregators = ResetAggregators
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #metrics_state{}) -> {noreply, #metrics_state{}}.
handle_info({timeout, Timer, aggregate_metrics}, State) ->
    %% Aggregation timer expired
    case Timer of
        State#metrics_state.aggregators#{aggregation_timer} ->
            %% Aggregate metrics
            Aggregated = aggregate_metrics(State#metrics_state.aggregators),

            %% Update metrics with aggregated values
            UpdatedMetrics = maps:merge(State#metrics_state.metrics, Aggregated),

            %% Reset aggregators
            ResetAggregators = maps:map(fun(_, _) -> [] end, State#metrics_state.aggregators),

            %% Start new timer
            NewTimer = erlang:start_timer(State#metrics_state.config#{aggregation_interval},
                                         self(), aggregate_metrics),

            %% Record aggregation
            erlmcp_metrics:record("cli.metrics.aggregated", 1),

            NewState = State#metrics_state{
                metrics = UpdatedMetrics,
                aggregators = maps:remove(aggregation_timer, ResetAggregators),
                aggregators = maps:put(aggregation_timer, NewTimer, NewState#metrics_state.aggregators)
            },

            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the metrics system
-spec terminate(term(), #metrics_state{}) -> ok.
terminate(_Reason, State) ->
    %% Shutdown all exporters
    lists:foreach(fun(Exporter) ->
        shutdown_exporter(Exporter)
    end, State#metrics_state.exporters),

    %% Finalize metrics
    export_final_metrics(State#metrics_state.metrics, State#metrics_state.exporters),

    ok.

%% @doc Code change
-spec code_change(term(), #metrics_state{}, term()) -> {ok, #metrics_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize exporters
-spec init_exporters() -> list().
init_exporters() ->
    Config = ?DEFAULT_CONFIG#{exporters},
    lists:map(fun(ExporterType) ->
        init_exporter(ExporterType)
    end, Config).

%% @doc Initialize a specific exporter
-spec init_exporter(atom()) -> {atom(), pid() | undefined}.
init_exporter(prometheus) ->
    %% Initialize Prometheus exporter
    case erlmcp_prometheus_exporter:start() of
        {ok, Pid} -> {prometheus, Pid};
        {error, _} -> undefined
    end;

init_exporter(otlp) ->
    %% Initialize OTLP exporter
    case erlmcp_otlp_exporter:start() of
        {ok, Pid} -> {otlp, Pid};
        {error, _} -> undefined
    end;

init_exporter(console) ->
    %% Console exporter (always available)
    {console, undefined};

init_exporter(_Exporter) ->
    undefined.

%% @doc Determine metric type
-spec determine_metric_type(binary()) -> counter | gauge | histogram.
determine_metric_type(Name) ->
    case Name of
        _ when binary:matches(Name, <<".counter">>) ->
            counter;
        _ when binary:matches(Name, <<".gauge">>) ->
            gauge;
        _ when binary:matches(Name, <<".histogram">>) ->
            histogram;
        _ when binary:matches(Name, [<<"latency">>, <<"throughput">>]) ->
            histogram;
        _ ->
            gauge
    end.

%% @doc Extract metric tags
-spec extract_metric_tags(binary()) -> map().
extract_metric_tags(Name) ->
    %% Extract tags from metric name
    Tags = #{},

    %% Add category tag
    Category = case Name of
        _ when binary:matches(Name, [<<"cli.commands">>]) ->
            "commands";
        _ when binary:matches(Name, [<<"cli.sessions">>]) ->
            "sessions";
        _ when binary:matches(Name, [<<"cli.errors">>]) ->
            "errors";
        _ when binary:matches(Name, [<<"cli.registry">>]) ->
            "registry";
        _ ->
            "other"
    end,

    Tags#{category => Category}.

%% @doc Add metric to aggregator
-spec add_to_aggregator(binary(), #metric_data{}, map()) -> map().
add_to_aggregator(Name, MetricData, Aggregators) ->
    case Aggregators of
        #{Name := Values} ->
            %% Add to existing list
            UpdatedValues = [MetricData | Values],
            maps:put(Name, UpdatedValues, Aggregators);
        _ ->
            %% Create new list
            maps:put(Name, [MetricData], Aggregators)
    end.

%% @doc Aggregate metrics
-spec aggregate_metrics(map()) -> map().
aggregate_metrics(Aggregators) ->
    maps:fold(fun(Name, Values, Acc) ->
        case aggregate_metric_values(Name, Values) of
            {ok, AggregatedValue} ->
                Acc#{Name => AggregatedValue};
            {error, _} ->
                Acc
        end
    end, #{}, Aggregators).

%% @doc Aggregate metric values
-spec aggregate_metric_values(binary(), list()) -> {ok, number()} | {error, term()}.
aggregate_metric_values(Name, Values) ->
    case Values of
        [] ->
            {error, no_values};
        _ ->
            %% Determine aggregation type
            case determine_metric_type(Name) of
                counter ->
                    %% Sum all values
                    Sum = lists:sum([V#metric_data.value || V <- Values]),
                    {ok, Sum};
                gauge ->
                    %% Use latest value
                    Latest = lists:last(Values),
                    {ok, Latest#metric_data.value};
                histogram ->
                    %% Calculate percentiles
                    ValuesList = [V#metric_data.value || V <- Values],
                    Percentiles = calculate_histogram_percentiles(ValuesList),
                    {ok, Percentiles}
            end
    end.

%% @doc Calculate histogram percentiles
-spec calculate_histogram_percentiles(list()) -> number().
calculate_histogram_percentiles(Values) ->
    Sorted = lists:sort(Values),
    Length = length(Sorted),

    %% Calculate p50, p95, p99
    P50 = lists:nth(max(1, trunc(Length * 0.5)), Sorted),
    P95 = lists:nth(max(1, trunc(Length * 0.95)), Sorted),
    P99 = lists:nth(max(1, trunc(Length * 0.99)), Sorted),

    %% Return average of percentiles
    (P50 + P95 + P99) / 3.

%% @doc Export metric
-spec export_metric(#metric_data{}, list()) -> ok.
export_metric(MetricData, Exporters) ->
    lists:foreach(fun(Exporter) ->
        case Exporter of
            {prometheus, Pid} ->
                erlmcp_prometheus_exporter:record(Pid, MetricData);
            {otlp, Pid} ->
                erlmcp_otlp_exporter:record(Pid, MetricData);
            {console, _} ->
                %% Console output for debugging
                io:format("CLI Metric: ~s = ~p~n", [MetricData#metric_data.name, MetricData#metric_data.value]);
            _ ->
                ok
        end
    end, Exporters).

%% @doc Export reset event
-spec export_reset_event(list()) -> ok.
export_reset_event(Exporters) ->
    ResetEvent = #metric_data{
        name = <<"cli.metrics.reset">>,
        type = counter,
        value = 1,
        timestamp = erlang:system_time(millisecond),
        tags = #{event => "reset"}
    },

    export_metric(ResetEvent, Exporters).

%% @doc Export final metrics
-spec export_final_metrics(map(), list()) -> ok.
export_final_metrics(Metrics, Exporters) ->
    FinalEvent = #metric_data{
        name = <<"cli.metrics.shutdown">>,
        type = counter,
        value = 1,
        timestamp = erlang:system_time(millisecond),
        tags = #{event => "shutdown"}
    },

    export_metric(FinalEvent, Exporters).

%% @doc Shutdown exporter
-spec shutdown_exporter({atom(), pid() | undefined}) -> ok.
shutdown_exporter({prometheus, Pid}) ->
    case Pid of
        undefined -> ok;
        _ -> erlmcp_prometheus_exporter:stop(Pid)
    end;
shutdown_exporter({otlp, Pid}) ->
    case Pid of
        undefined -> ok;
        _ -> erlmcp_otlp_exporter:stop(Pid)
    end;
shutdown_exporter(_) ->
    ok.

%% @doc Make request ID
-spec make_request_id() -> binary().
make_request_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).