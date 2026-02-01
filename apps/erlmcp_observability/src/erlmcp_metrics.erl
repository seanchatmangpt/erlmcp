-module(erlmcp_metrics).

-export([start_link/0, record_transport_operation/4, record_server_operation/4,
         record_registry_operation/3, get_metrics/0, get_metrics/1, reset_metrics/0,
         get_performance_summary/0, with_metrics/3]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Disable opentelemetry for now until dependency is available
% -include_lib("opentelemetry/include/otel_tracer.hrl").

%%====================================================================
%% Types
%%====================================================================

-type metric_name() :: binary().
-type metric_value() :: number().
-type metric_labels() :: #{binary() => term()}.

-record(metric,
        {name :: metric_name(),
         value :: metric_value(),
         labels :: metric_labels(),
         timestamp :: integer()}).
-record(state,
        {metrics = [] :: [#metric{}],
         counters = #{} :: #{metric_name() => metric_value()},
         histograms = #{} :: #{metric_name() => [metric_value()]},
         gauges = #{} :: #{metric_name() => metric_value()},
         start_time :: integer()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec record_transport_operation(atom(), atom(), binary(), metric_value()) -> ok.
record_transport_operation(TransportId, TransportType, Operation, Duration) ->
    Labels =
        #{<<"transport_id">> => TransportId,
          <<"transport_type">> => TransportType,
          <<"operation">> => Operation},
    gen_server:cast(?MODULE,
                    {record_metric, <<"transport_operation_duration_ms">>, Duration, Labels}).

-spec record_server_operation(atom(), binary(), metric_value(), metric_labels()) -> ok.
record_server_operation(ServerId, Operation, Duration, ExtraLabels) ->
    Labels = ExtraLabels#{<<"server_id">> => ServerId, <<"operation">> => Operation},
    gen_server:cast(?MODULE, {record_metric, <<"server_operation_duration_ms">>, Duration, Labels}).

-spec record_registry_operation(binary(), metric_value(), metric_labels()) -> ok.
record_registry_operation(Operation, Duration, ExtraLabels) ->
    Labels = ExtraLabels#{<<"component">> => <<"registry">>, <<"operation">> => Operation},
    gen_server:cast(?MODULE,
                    {record_metric, <<"registry_operation_duration_ms">>, Duration, Labels}).

-spec get_metrics() -> [#metric{}].
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

-spec get_metrics(metric_name()) -> [#metric{}].
get_metrics(MetricName) ->
    gen_server:call(?MODULE, {get_metrics, MetricName}).

-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

-spec get_performance_summary() -> map().
get_performance_summary() ->
    gen_server:call(?MODULE, get_performance_summary).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Starting MCP metrics collector"),
    {ok, #state{start_time = erlang:system_time(millisecond)}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_metrics, _From, #state{metrics = Metrics} = State) ->
    {reply, Metrics, State};
handle_call({get_metrics, MetricName}, _From, #state{metrics = Metrics} = State) ->
    Filtered = lists:filter(fun(#metric{name = Name}) -> Name =:= MetricName end, Metrics),
    {reply, Filtered, State};
handle_call(reset_metrics, _From, State) ->
    NewState =
        State#state{metrics = [],
                    counters = #{},
                    histograms = #{},
                    gauges = #{},
                    start_time = erlang:system_time(millisecond)},
    {reply, ok, NewState};
handle_call(get_performance_summary, _From, State) ->
    Summary = calculate_performance_summary(State),
    {reply, Summary, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_metric, Name, Value, Labels}, State) ->
    NewState = record_metric_internal(Name, Value, Labels, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("MCP metrics collector terminating"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec record_metric_internal(metric_name(), metric_value(), metric_labels(), #state{}) -> #state{}.
record_metric_internal(Name,
                       Value,
                       Labels,
                       #state{metrics = Metrics,
                              counters = Counters,
                              histograms = Histograms,
                              gauges = Gauges} =
                           State) ->
    Timestamp = erlang:system_time(millisecond),

    Metric =
        #metric{name = Name,
                value = Value,
                labels = Labels,
                timestamp = Timestamp},

    NewMetrics = [Metric | lists:sublist(Metrics, 999)], % Keep last 1000 metrics

    % Update aggregated data structures
    NewCounters = update_counter(Name, Labels, Counters),
    NewHistograms = update_histogram(Name, Value, Labels, Histograms),
    NewGauges = update_gauge(Name, Value, Labels, Gauges),

    State#state{metrics = NewMetrics,
                counters = NewCounters,
                histograms = NewHistograms,
                gauges = NewGauges}.

-spec update_counter(metric_name(), metric_labels(), map()) -> map().
update_counter(Name, _Labels, Counters) ->
    CounterKey = Name,
    CurrentValue = maps:get(CounterKey, Counters, 0),
    maps:put(CounterKey, CurrentValue + 1, Counters).

-spec update_histogram(metric_name(), metric_value(), metric_labels(), map()) -> map().
update_histogram(Name, Value, _Labels, Histograms) ->
    HistogramKey = Name,
    CurrentValues = maps:get(HistogramKey, Histograms, []),
    NewValues = [Value | lists:sublist(CurrentValues, 99)], % Keep last 100 values
    maps:put(HistogramKey, NewValues, Histograms).

-spec update_gauge(metric_name(), metric_value(), metric_labels(), map()) -> map().
update_gauge(Name, Value, _Labels, Gauges) ->
    GaugeKey = Name,
    maps:put(GaugeKey, Value, Gauges).

-spec calculate_performance_summary(#state{}) -> map().
calculate_performance_summary(#state{metrics = Metrics,
                                     counters = Counters,
                                     histograms = Histograms,
                                     gauges = Gauges,
                                     start_time = StartTime}) ->
    Now = erlang:system_time(millisecond),
    UptimeMs = Now - StartTime,

    #{<<"uptime_ms">> => UptimeMs,
      <<"total_metrics_recorded">> => length(Metrics),
      <<"counters">> => format_counters(Counters),
      <<"histograms">> => format_histograms(Histograms),
      <<"gauges">> => format_gauges(Gauges),
      <<"rates">> => calculate_rates(Counters, UptimeMs),
      <<"percentiles">> => calculate_percentiles(Histograms),
      <<"system_info">> => get_system_info()}.

-spec format_counters(map()) -> map().
format_counters(Counters) ->
    maps:fold(fun(Key, Value, Acc) -> Acc#{Key => Value} end, #{}, Counters).

-spec format_histograms(map()) -> map().
format_histograms(Histograms) ->
    maps:fold(fun(Key, Values, Acc) ->
                 case Values of
                     [] ->
                         Acc#{Key =>
                                  #{<<"count">> => 0,
                                    <<"min">> => 0,
                                    <<"max">> => 0,
                                    <<"avg">> => 0}};
                     _ ->
                         Acc#{Key =>
                                  #{<<"count">> => length(Values),
                                    <<"min">> => lists:min(Values),
                                    <<"max">> => lists:max(Values),
                                    <<"avg">> => lists:sum(Values) / length(Values)}}
                 end
              end,
              #{},
              Histograms).

-spec format_gauges(map()) -> map().
format_gauges(Gauges) ->
    maps:fold(fun(Key, Value, Acc) -> Acc#{Key => Value} end, #{}, Gauges).

-spec calculate_rates(map(), integer()) -> map().
calculate_rates(Counters, UptimeMs) when UptimeMs > 0 ->
    UptimeSeconds = UptimeMs / 1000,
    maps:fold(fun(Key, Count, Acc) ->
                 Rate = Count / UptimeSeconds,
                 RateKey = <<Key/binary, "_per_second">>,
                 Acc#{RateKey => Rate}
              end,
              #{},
              Counters);
calculate_rates(_, _) ->
    #{}.

-spec calculate_percentiles(map()) -> map().
calculate_percentiles(Histograms) ->
    maps:fold(fun(Key, Values, Acc) ->
                 case Values of
                     [] ->
                         Acc;
                     _ ->
                         SortedValues = lists:sort(Values),
                         P50 = percentile(SortedValues, 50),
                         P90 = percentile(SortedValues, 90),
                         P95 = percentile(SortedValues, 95),
                         P99 = percentile(SortedValues, 99),

                         PercentilesKey = <<Key/binary, "_percentiles">>,
                         Acc#{PercentilesKey =>
                                  #{<<"p50">> => P50,
                                    <<"p90">> => P90,
                                    <<"p95">> => P95,
                                    <<"p99">> => P99}}
                 end
              end,
              #{},
              Histograms).

-spec percentile([number()], integer()) -> number().
percentile(SortedValues, Percentile) ->
    N = length(SortedValues),
    Index = ceil(N * Percentile / 100),
    ClampedIndex = max(1, min(Index, N)),
    lists:nth(ClampedIndex, SortedValues).

-spec get_system_info() -> map().
get_system_info() ->
    #{<<"memory_total">> => erlang:memory(total),
      <<"memory_processes">> => erlang:memory(processes),
      <<"memory_system">> => erlang:memory(system),
      <<"process_count">> => erlang:system_info(process_count),
      <<"run_queue">> => erlang:statistics(run_queue),
      <<"scheduler_utilization">> => get_scheduler_utilization()}.

-spec get_scheduler_utilization() -> map().
get_scheduler_utilization() ->
    try
        case erlang:statistics(scheduler_wall_time) of
            undefined ->
                #{};
            SchedulerTimes ->
                TotalActive = lists:sum([Active || {_, Active, _} <- SchedulerTimes]),
                TotalTotal = lists:sum([Total || {_, _, Total} <- SchedulerTimes]),
                case TotalTotal of
                    0 ->
                        #{<<"utilization_percent">> => 0};
                    _ ->
                        #{<<"utilization_percent">> => TotalActive / TotalTotal * 100}
                end
        end
    catch
        _:_ ->
            #{}
    end.

%%====================================================================
%% Instrumentation Helper Functions
%%====================================================================

%% Helper function to instrument a function call with metrics
-spec with_metrics(metric_name(), metric_labels(), fun(() -> Result)) -> Result.
with_metrics(MetricName, Labels, Fun) ->
    StartTime = erlang:system_time(millisecond),
    try
        Result = Fun(),
        EndTime = erlang:system_time(millisecond),
        Duration = EndTime - StartTime,
        gen_server:cast(?MODULE, {record_metric, MetricName, Duration, Labels}),
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime2 = erlang:system_time(millisecond),
            Duration2 = EndTime2 - StartTime,
            ErrorLabels = Labels#{<<"error">> => true, <<"error_class">> => Class},
            gen_server:cast(?MODULE, {record_metric, MetricName, Duration2, ErrorLabels}),
            erlang:raise(Class, Reason, Stacktrace)
    end.
