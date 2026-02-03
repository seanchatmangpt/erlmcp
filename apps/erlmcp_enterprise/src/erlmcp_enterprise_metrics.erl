%% @doc Enterprise Metrics Collector
%% Central metrics collection for enterprise integrations
-module(erlmcp_enterprise_metrics).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([record_metric/2, get_metrics/1, clear_metrics/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    metrics :: map(),  % Category -> metrics
    aggregators :: map(),  % Metric -> aggregator function
    retention :: integer()  % milliseconds
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec record_metric(Category :: atom(), Metric :: {atom(), number()}) -> ok.
record_metric(Category, Metric) ->
    gen_server:call(?MODULE, {record_metric, Category, Metric}).

-spec get_metrics(Category :: atom()) -> {ok, map()} | {error, not_found}.
get_metrics(Category) ->
    gen_server:call(?MODULE, {get_metrics, Category}).

-spec clear_metrics(Category :: atom()) -> ok.
clear_metrics(Category) ->
    gen_server:call(?MODULE, {clear_metrics, Category}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize metrics structure
    Metrics = #{
        identity => #{},
        monitoring => #{},
        logging => #{},
        business_intel => #{},
        service_bus => #{},
        data_warehouse => #{},
        devops => #{},
        api_gateway => #{},
        cloud => #{},
        security => #{},
        config_mgmt => #{},
        container => #{}
    },

    %% Initialize aggregators
    Aggregators = #{
        counter => fun metrics_counter/2,
        gauge => fun metrics_gauge/2,
        histogram => fun metrics_histogram/2,
        summary => fun metrics_summary/2
    },

    %% Set retention period (30 days)
    Retention = 2592000000,

    State = #state{
        metrics = Metrics,
        aggregators = Aggregators,
        retention = Retention
    },

    %% Start cleanup timer
    erlang:send_after(Retention, self(), cleanup_metrics),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({record_metric, Category, Metric}, _From, State) ->
    %% Record a metric
    CurrentMetrics = maps:get(Category, State#state.metrics, #{}),
    UpdatedMetrics = record_current_metric(CurrentMetrics, Metric),

    NewMetrics = State#state.metrics#{Category => UpdatedMetrics},

    %% Notify interested parties
    erlmcp_enterprise_bus:publish(metric_recorded, {Category, Metric}),

    {reply, ok, State#state{metrics = NewMetrics}};

handle_call({get_metrics, Category}, _From, State) ->
    case maps:find(Category, State#state.metrics) of
        {ok, Metrics} ->
            {reply, {ok, Metrics}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({clear_metrics, Category}, _From, State) ->
    %% Clear metrics for a category
    NewMetrics = maps:remove(Category, State#state.metrics),
    Metrics = NewMetrics#{Category => #{}},

    {reply, ok, State#state{metrics = Metrics}};

handle_call(get_all_metrics, _From, State) ->
    {reply, {ok, State#state.metrics}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup_metrics, State) ->
    %% Clean up old metrics
    CurrentTime = erlang:system_time(millisecond),
    Cutoff = CurrentTime - State#state.retention,

    %% Remove old metrics
    CleanedMetrics = maps:map(fun(_Category, Metrics) ->
        maps:filter(fun(_Key, Metric) ->
            case Metric of
                #{timestamp := Timestamp} when Timestamp > Cutoff ->
                    true;
                _ ->
                    true
            end
        end, Metrics)
    end, State#state.metrics),

    %% Schedule next cleanup
    erlang:send_after(State#state.retention, self(), cleanup_metrics),

    {noreply, State#state{metrics = CleanedMetrics}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec record_current_metric(map(), {atom(), number()}) -> map().
record_current_metric(Metrics, {Name, Value}) ->
    %% Record metric with current timestamp
    Timestamp = erlang:system_time(millisecond),
    MetricData = #{
        name => Name,
        value => Value,
        timestamp => Timestamp,
        node => node()
    },

    case maps:find(Name, Metrics) of
        {ok, Existing} ->
            %% Update existing metric
            case Existing of
                #{type := counter} ->
                    %% Counter - increment
                    CurrentValue = maps:get(value, Existing, 0),
                    Updated = Existing#{value => CurrentValue + Value};
                #{type := gauge} ->
                    %% Gauge - set value
                    Updated = Existing#{value => Value};
                #{type := histogram} ->
                    %% Histogram - add to bucket
                    Bucket = calculate_histogram_bucket(Value),
                    Buckets = maps:get(buckets, Existing, #{}),
                    NewBuckets = maps:update_with(Bucket, fun(V) -> V + 1 end, 1, Buckets),
                    Updated = Existing#{buckets => NewBuckets, count => maps:get(count, Existing, 0) + 1};
                #{type := summary} ->
                    %% Summary - calculate quantiles
                    Values = maps:get(values, Existing, []),
                    NewValues = [Value | Values],
                    Updated = Existing#{values => NewValues, count => maps:get(count, Existing, 0) + 1};
                _ ->
                    Updated = Existing#{value => Value}
            end,
            Metrics#{Name => Updated};
        error ->
            %% New metric
            Metrics#{Name => MetricData#{type => determine_metric_type(Value)}}
    end.

-spec calculate_histogram_bucket(number()) -> integer().
calculate_histogram_bucket(Value) ->
    %% Simple bucket calculation - can be customized
    BucketSize = 10,
    trunc(Value / BucketSize) * BucketSize.

-spec determine_metric_type(number()) -> atom().
determine_metric_type(Value) when is_integer(Value) ->
    %% Default to counter for integer values
    counter;
determine_metric_type(_) ->
    %% Default to gauge for float values
    gauge.

-spec metrics_counter(map(), number()) -> map().
metrics_counter(Metric, Value) ->
    Current = maps:get(value, Metric, 0),
    Metric#{value => Current + Value}.

-spec metrics_gauge(map(), number()) -> map().
metrics_gauge(Metric, Value) ->
    Metric#{value => Value}.

-spec metrics_histogram(map(), number()) -> map().
metrics_histogram(Metric, Value) ->
    Bucket = calculate_histogram_bucket(Value),
    Buckets = maps:get(buckets, Metric, #{}),
    NewBuckets = maps:update_with(Bucket, fun(V) -> V + 1 end, 1, Buckets),
    Count = maps:get(count, Metric, 0) + 1,
    Metric#{buckets => NewBuckets, count => Count}.

-spec metrics_summary(map(), number()) -> map().
metrics_summary(Metric, Value) ->
    Values = maps:get(values, Metric, []),
    NewValues = [Value | Values],
    Count = maps:get(count, Metric, 0) + 1,
    Metric#{values => NewValues, count => Count}.