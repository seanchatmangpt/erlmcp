%% @doc Enterprise Metrics Collection for erlmcp v3
%% This module implements comprehensive metrics collection and reporting
%% for all erlmcp v3 components.
-module(erlmcp_metrics).

-export([start_link/0, collect_metrics/0, get_metrics_summary/0,
         create_counter/2, create_counter/3,
         create_gauge/2, create_gauge/3,
         create_histogram/2, create_histogram/3,
         inc/2, inc/3, dec/2, dec/3,
         gauge/2, gauge/3, observe/2, observe/3,
         record_counter/3, record_gauge/3, record_histogram/3]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the metrics collection server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Collect all metrics from erlmcp components
-spec collect_metrics() -> map().
collect_metrics() ->
    gen_server:call(?MODULE, collect_metrics).

%% @doc Get metrics summary
-spec get_metrics_summary() -> map().
get_metrics_summary() ->
    gen_server:call(?MODULE, get_summary).

%% @doc Create a new counter metric
-spec create_counter(Name :: binary(), Description :: binary()) -> ok.
create_counter(Name, Description) ->
    create_counter(Name, Description, #{}).

%% @doc Create a new counter metric with labels
-spec create_counter(Name :: binary(), Description :: binary(), Labels :: map()) -> ok.
create_counter(Name, Description, Labels) ->
    gen_server:cast(?MODULE, {create_counter, Name, Description, Labels}).

%% @doc Create a new gauge metric
-spec create_gauge(Name :: binary(), Description :: binary()) -> ok.
create_gauge(Name, Description) ->
    create_gauge(Name, Description, #{}).

%% @doc Create a new gauge metric with labels
-spec create_gauge(Name :: binary(), Description :: binary(), Labels :: map()) -> ok.
create_gauge(Name, Description, Labels) ->
    gen_server:cast(?MODULE, {create_gauge, Name, Description, Labels}).

%% @doc Create a new histogram metric
-spec create_histogram(Name :: binary(), Description :: binary()) -> ok.
create_histogram(Name, Description) ->
    create_histogram(Name, Description, #{}).

%% @doc Create a new histogram metric with labels
-spec create_histogram(Name :: binary(), Description :: binary(), Labels :: map()) -> ok.
create_histogram(Name, Description, Labels) ->
    gen_server:cast(?MODULE, {create_histogram, Name, Description, Labels}).

%% @doc Increment a counter
-spec inc(Name :: binary(), Value :: number()) -> ok.
inc(Name, Value) ->
    inc(Name, Value, #{}).

%% @doc Increment a counter with labels
-spec inc(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
inc(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {inc, Name, Value, Labels}).

%% @doc Decrement a counter
-spec dec(Name :: binary(), Value :: number()) -> ok.
dec(Name, Value) ->
    dec(Name, Value, #{}).

%% @doc Decrement a counter with labels
-spec dec(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
dec(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {dec, Name, Value, Labels}).

%% @doc Set a gauge value
-spec gauge(Name :: binary(), Value :: number()) -> ok.
gauge(Name, Value) ->
    gauge(Name, Value, #{}).

%% @doc Set a gauge value with labels
-spec gauge(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
gauge(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {gauge, Name, Value, Labels}).

%% @doc Observe a value for histogram
-spec observe(Name :: binary(), Value :: number()) -> ok.
observe(Name, Value) ->
    observe(Name, Value, #{}).

%% @doc Observe a value for histogram with labels
-spec observe(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
observe(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {observe, Name, Value, Labels}).

%% @doc Record a counter metric
-spec record_counter(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
record_counter(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {record_counter, Name, Value, Labels}).

%% @doc Record a gauge metric
-spec record_gauge(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
record_gauge(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {record_gauge, Name, Value, Labels}).

%% @doc Record a histogram metric
-spec record_histogram(Name :: binary(), Value :: number(), Labels :: map()) -> ok.
record_histogram(Name, Value, Labels) ->
    gen_server:cast(?MODULE, {record_histogram, Name, Value, Labels}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize metrics storage
    Metrics = #{
        counters => #{},
        gauges => #{},
        histograms => #{},
        registry => erlmcp_registry:create(metrics_registry),
        last_updated => erlang:system_time(millisecond)
    },

    %% Start periodic collection
    erlang:send_after(1000, self(), collect_metrics),

    {ok, Metrics}.

handle_call(collect_metrics, _From, State) ->
    %% Collect metrics from all components
    Metrics = collect_all_metrics(State),
    {reply, Metrics, State#{metrics := Metrics}};

handle_call(get_summary, _From, State) ->
    Summary = create_summary(State),
    {reply, Summary, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({create_counter, Name, Description, Labels}, State) ->
    Counters = maps:get(counters, State, #{}),
    NewCounters = maps:put(Name, #{type => counter, description => Description, labels => Labels, value => 0}, Counters),
    {noreply, State#{counters := NewCounters}};

handle_cast({create_gauge, Name, Description, Labels}, State) ->
    Gauges = maps:get(gauges, State, #{}),
    NewGauges = maps:put(Name, #{type => gauge, description => Description, labels => Labels, value => 0}, Gauges),
    {noreply, State#{gauges := NewGauges}};

handle_cast({create_histogram, Name, Description, Labels}, State) ->
    Histograms = maps:get(histograms, State, #{}),
    NewHistograms = maps:put(Name, #{type => histogram, description => Description, labels => Labels, buckets => [], count => 0, sum => 0}, Histograms),
    {noreply, State#{histograms := NewHistograms}};

handle_cast({inc, Name, Value, Labels}, State) ->
    Counters = maps:get(counters, State, #{}),
    case maps:get(Name, Counters, undefined) of
        undefined ->
            %% Auto-create counter if it doesn't exist
            NewCounters = maps:put(Name, #{type => counter, description => auto_created, labels => Labels, value => Value}, Counters),
            {noreply, State#{counters := NewCounters}};
        Counter ->
            NewValue = maps:get(value, Counter, 0) + Value,
            NewCounter = Counter#{value => NewValue, labels => Labels},
            NewCounters = maps:put(Name, NewCounter, Counters),
            {noreply, State#{counters := NewCounters}}
    end;

handle_cast({dec, Name, Value, Labels}, State) ->
    Counters = maps:get(counters, State, #{}),
    case maps:get(Name, Counters, undefined) of
        undefined ->
            %% Auto-create counter if it doesn't exist
            NewCounters = maps:put(Name, #{type => counter, description => auto_created, labels => Labels, value => -Value}, Counters),
            {noreply, State#{counters := NewCounters}};
        Counter ->
            NewValue = maps:get(value, Counter, 0) - Value,
            NewCounter = Counter#{value => NewValue, labels => Labels},
            NewCounters = maps:put(Name, NewCounter, Counters),
            {noreply, State#{counters := NewCounters}}
    end;

handle_cast({gauge, Name, Value, Labels}, State) ->
    Gauges = maps:get(gauges, State, #{}),
    NewGauge = maps:get(Name, Gauges, #{description => auto_created, labels => Labels, value => 0})#{value => Value, labels => Labels},
    NewGauges = maps:put(Name, NewGauge, Gauges),
    {noreply, State#{gauges := NewGauges}};

handle_cast({observe, Name, Value, Labels}, State) ->
    Histograms = maps:get(histograms, State, #{}),
    case maps:get(Name, Histograms, undefined) of
        undefined ->
            %% Auto-create histogram if it doesn't exist
            NewHistogram = create_histogram_value(Value, Labels),
            NewHistograms = maps:put(Name, NewHistogram, Histograms),
            {noreply, State#{histograms := NewHistograms}};
        Histogram ->
            NewHistogram = update_histogram_value(Histogram, Value, Labels),
            NewHistograms = maps:put(Name, NewHistogram, Histograms),
            {noreply, State#{histograms := NewHistograms}}
    end;

handle_cast({record_counter, Name, Value, Labels}, State) ->
    %% Send metrics to Prometheus
    send_metric_to_prometheus(Name, Value, counter, Labels),

    %% Also store locally
    handle_cast({inc, Name, Value, Labels}, State);

handle_cast({record_gauge, Name, Value, Labels}, State) ->
    %% Send metrics to Prometheus
    send_metric_to_prometheus(Name, Value, gauge, Labels),

    %% Also store locally
    handle_cast({gauge, Name, Value, Labels}, State);

handle_cast({record_histogram, Name, Value, Labels}, State) ->
    %% Send metrics to Prometheus
    send_metric_to_prometheus(Name, Value, histogram, Labels),

    %% Also store locally
    handle_cast({observe, Name, Value, Labels}, State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect metrics from all components
    collect_system_metrics(),
    collect_component_metrics(),

    %% Schedule next collection
    erlang:send_after(1000, self(), collect_metrics),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Collect all metrics
collect_all_metrics(State) ->
    Counters = maps:get(counters, State, #{}),
    Gauges = maps:get(gauges, State, #{}),
    Histograms = maps:get(histograms, State, #{}),

    #{
        counters => Counters,
        gauges => Gauges,
        histograms => Histograms,
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Create metrics summary
create_summary(State) ->
    Counters = maps:get(counters, State, #{}),
    Gauges = maps:get(gauges, State, #{}),
    Histograms = maps:get(histograms, State, #{}),

    #{
        metrics_count => maps:size(Counters) + maps:size(Gauges) + maps:size(Histograms),
        counters_count => maps:size(Counters),
        gauges_count => maps:size(Gauges),
        histograms_count => maps:size(Histograms),
        last_updated => maps:get(last_updated, State, erlang:system_time(millisecond))
    }.

%% @private Collect system metrics
collect_system_metrics() ->
    %% Collect CPU metrics
    CpuUsage = cpu_sup:util([time, busy, idle]),
    ?MODULE:record_gauge(<<"system_cpu_usage">>, CpuUsage, #{}),

    %% Collect memory metrics
    {memory, MemoryInfo} = erlang:memory(),
    ?MODULE:record_gauge(<<"system_memory_total_bytes">>, proplists:get_value(total, MemoryInfo, 0), #{}),
    ?MODULE:record_gauge(<<"system_memory_processes_bytes">>, proplists:get_value(processes, MemoryInfo, 0), #{}),
    ?MODULE:record_gauge(<<"system_memory_system_bytes">>, proplists:get_value(system, MemoryInfo, 0), #{}),

    %% Collect process metrics
    ProcessCount = erlang:system_info(process_count),
    ?MODULE:record_gauge(<<"system_process_count">>, ProcessCount, #{}),

    %% Collect load metrics
    LoadAvg = get_load_average(),
    maps:foreach(fun(Key, Value) ->
        ?MODULE:record_gauge(<<"system_load_average_", Key/binary>>, Value, #{})
    end, LoadAvg).

%% @private Collect component metrics
collect_component_metrics() ->
    %% Collect erlmcp core metrics
    case erlmcp_core:metrics() of
        {ok, Metrics} ->
            record_component_metrics(<<"erlmcp_core">>, Metrics);
        _ ->
            ok
    end,

    %% Collect erlmcp transport metrics
    case erlmcp_transports:metrics() of
        {ok, Metrics} ->
            record_component_metrics(<<"erlmcp_transports">>, Metrics);
        _ ->
            ok
    end,

    %% Collect erlmcp session metrics
    case erlmcp_sessions:metrics() of
        {ok, Metrics} ->
            record_component_metrics(<<"erlmcp_sessions">>, Metrics);
        _ ->
            ok
    end,

    %% Collect erlmcp registry metrics
    case erlmcp_registry:metrics() of
        {ok, Metrics} ->
            record_component_metrics(<<"erlmcp_registry">>, Metrics);
        _ ->
            ok
    end.

%% @private Record component metrics
record_component_metrics(Component, Metrics) ->
    maps:foreach(fun(Name, Value) ->
        ?MODULE:record_gauge(<<Component/binary, "_", Name/binary>>, Value, #{
            <<"component">> => Component
        })
    end, Metrics).

%% @private Create histogram value
create_histogram_value(Value, Labels) ->
    #{
        type => histogram,
        description => auto_created,
        labels => Labels,
        buckets => [Value],
        count => 1,
        sum => Value,
        min => Value,
        max => Value
    }.

%% @private Update histogram value
update_histogram_value(Histogram, Value, Labels) ->
    Buckets = maps:get(buckets, Histogram, []),
    NewBuckets = lists:sort([Value | Buckets]),
    Count = maps:get(count, Histogram, 0) + 1,
    Sum = maps:get(sum, Histogram, 0) + Value,
    Min = maps:get(min, Histogram, Value) min Value,
    Max = maps:get(max, Histogram, Value) max Value,

    Histogram#{
        buckets => NewBuckets,
        count => Count,
        sum => Sum,
        min => Min,
        max => Max,
        labels => Labels
    }.

%% @private Send metric to Prometheus
send_metric_to_prometheus(Name, Value, Type, Labels) ->
    %% Format metric for Prometheus
    MetricName = binary_to_list(Name),
    LabelString = format_labels(Labels),

    case Type of
        counter ->
            Metric = format_counter(MetricName, LabelString, Value),
            send_to_prometheus(Metric);
        gauge ->
            Metric = format_gauge(MetricName, LabelString, Value),
            send_to_prometheus(Metric);
        histogram ->
            Metric = format_histogram(MetricName, LabelString, Value),
            send_to_prometheus(Metric)
    end.

%% @private Format labels
format_labels(Labels) ->
    case maps:size(Labels) of
        0 -> "";
        _ ->
            lists:foldl(fun({Key, Value}, Acc) ->
                case Acc of
                    "" -> io_lib:format("{~s=~p}", [Key, Value]);
                    _ -> io_lib:format(",{~s=~p}", [Key, Value])
                end
            end, "", maps:to_list(Labels))
    end.

%% @private Format counter metric
format_counter(Name, Labels, Value) ->
    io_lib:format("~s~s ~p\n", [Name, Labels, Value]).

%% @private Format gauge metric
format_gauge(Name, Labels, Value) ->
    io_lib:format("~s~s ~p\n", [Name, Labels, Value]).

%% @private Format histogram metric
format_histogram(Name, Labels, Value) ->
    io_lib:format("~s~s ~p\n", [Name, Labels, Value]).

%% @private Send metric to Prometheus
send_to_prometheus(Metric) ->
    %% In a real implementation, this would send to Prometheus HTTP endpoint
    %% or write to a file for Prometheus to scrape
    ?LOG_DEBUG("Sending metric to Prometheus: ~s", [Metric]).

%% @private Get load average
get_load_average() ->
    case os:type() of
        {unix, _} ->
            case file:read_file("/proc/loadavg") of
                {ok, Content} ->
                    case binary:split(Content, <<" ">>, [global, trim]) of
                        [Load1, Load2, Load3 | _] ->
                            #{
                                <<"1min">> => binary_to_float(Load1),
                                <<"5min">> => binary_to_float(Load2),
                                <<"15min">> => binary_to_float(Load3)
                            };
                        _ -> #{}
                    end;
                _ -> #{}
            end;
        _ -> #{}
    end.