%%%-------------------------------------------------------------------
%%% @doc Capacity Monitor for erlmcp
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_capacity_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         set_baseline/2, get_baseline/1,
         set_thresholds/2, get_thresholds/1,
         collect_metrics/0, get_metrics/0,
         get_capacity_report/0, get_scaling_recommendations/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(capability_baseline, {
    timestamp,
    cpu,
    memory,
    network,
    disk,
    connections,
    throughput
}).

-record(scaling_threshold, {
    resource,
    scale_up,
    scale_down,
    scale_up_count,
    scale_down_count
}).

-record(capacity_metrics, {
    timestamp,
    cpu,
    memory,
    network,
    disk,
    connections,
    throughput,
    load
}).

-record(scaling_recommendation, {
    resource,
    current_utilization,
    recommended_action,
    confidence,
    estimated_impact,
    timeline
}).

-record(state, {
    baselines,
    thresholds,
    metrics_history,
    forecast_data,
    timer_ref,
    last_collection,
    collection_interval,
    enabled,
    forecast_window
}).

-define(DEFAULT_INTERVAL, 30000).
-define(HISTORY_SIZE, 1000).
-define(FORECAST_WINDOW, 24 * 60 * 60 * 1000).

%% API
start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

set_baseline(Resource, BaselineData) ->
    gen_server:call(?MODULE, {set_baseline, Resource, BaselineData}).

get_baseline(Resource) ->
    gen_server:call(?MODULE, {get_baseline, Resource}).

set_thresholds(Resource, ThresholdData) ->
    gen_server:call(?MODULE, {set_thresholds, Resource, ThresholdData}).

get_thresholds(Resource) ->
    gen_server:call(?MODULE, {get_thresholds, Resource}).

collect_metrics() ->
    gen_server:cast(?MODULE, collect_metrics).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

get_capacity_report() ->
    gen_server:call(?MODULE, get_capacity_report).

get_scaling_recommendations() ->
    gen_server:call(?MODULE, get_scaling_recommendations).

%% gen_server callbacks
init(Config) ->
    process_flag(trap_exit, true),
    DefaultThresholds = #{},
    State = #state{
        baselines = #{},
        thresholds = DefaultThresholds,
        metrics_history = queue:new(),
        forecast_data = #{},
        timer_ref = undefined,
        last_collection = erlang:system_time(millisecond),
        collection_interval = maps:get(collection_interval, Config, ?DEFAULT_INTERVAL),
        enabled = maps:get(enabled, Config, true),
        forecast_window = maps:get(forecast_window, Config, ?FORECAST_WINDOW)
    },
    {ok, State}.

handle_call({set_baseline, Resource, BaselineData}, _From, State) ->
    Baseline = create_baseline_record(Resource, BaselineData),
    Baselines = maps:put(Resource, Baseline, State#state.baselines),
    {reply, ok, State#state{baselines = Baselines}};

handle_call({get_baseline, Resource}, _From, State) ->
    Baseline = maps:get(Resource, State#state.baselines, undefined),
    {reply, Baseline, State};

handle_call({set_thresholds, Resource, ThresholdData}, _From, State) ->
    Threshold = create_threshold_record(Resource, ThresholdData),
    Thresholds = maps:put(Resource, Threshold, State#state.thresholds),
    {reply, ok, State#state{thresholds = Thresholds}};

handle_call({get_thresholds, Resource}, _From, State) ->
    Threshold = maps:get(Resource, State#state.thresholds, undefined),
    {reply, Threshold, State};

handle_call(get_metrics, _From, State) ->
    Metrics = get_latest_metrics(State#state.metrics_history),
    {reply, Metrics, State};

handle_call(get_capacity_report, _From, State) ->
    Report = generate_capacity_report(State),
    {reply, Report, State};

handle_call(get_scaling_recommendations, _From, State) ->
    Recommendations = [],
    {reply, Recommendations, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(collect_metrics, State) ->
    Metrics = collect_capacity_metrics(),
    NewState = store_metrics(Metrics, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, collect_metrics}, State) ->
    TimerRef = erlang:send_after(State#state.collection_interval, self(), collect_metrics),
    NewState = State#state{last_collection = erlang:system_time(millisecond)},
    handle_cast(collect_metrics, NewState#state{timer_ref = TimerRef});

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of undefined -> ok; Ref -> erlang:cancel_timer(Ref) end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
create_baseline_record(Resource, Data) ->
    #capability_baseline{
        timestamp = maps:get(timestamp, Data, erlang:system_time(millisecond)),
        cpu = maps:get(cpu, Data, #{}),
        memory = maps:get(memory, Data, #{}),
        network = maps:get(network, Data, #{}),
        disk = maps:get(disk, Data, #{}),
        connections = maps:get(connections, Data, #{}),
        throughput = maps:get(throughput, Data, #{})
    }.

create_threshold_record(Resource, Data) ->
    #scaling_threshold{
        resource = Resource,
        scale_up = maps:get(scale_up, Data, 80.0),
        scale_down = maps:get(scale_down, Data, 20.0),
        scale_up_count = maps:get(scale_up_count, Data, 3),
        scale_down_count = maps:get(scale_down_count, Data, 5)
    }.

collect_capacity_metrics() ->
    Memory = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    #capacity_metrics{
        timestamp = erlang:system_time(millisecond),
        cpu = #{utilization => 50.0},
        memory = #{utilization => calculate_memory_utilization(Memory)},
        network = #{utilization => 0.0},
        disk = #{utilization => 0.0},
        connections = #{count => ProcessCount},
        throughput = #{},
        load = #{}
    }.

calculate_memory_utilization(Memory) ->
    Total = proplists:get_value(total, Memory),
    Used = proplists:get_value(processes, Memory) + proplists:get_value(system, Memory),
    case Total of 0 -> 0.0; _ -> (Used / Total) * 100.0 end.

store_metrics(Metrics, State) ->
    History = queue:in(Metrics, State#state.metrics_history),
    TrimmedHistory = trim_history(History, ?HISTORY_SIZE),
    State#state{metrics_history = TrimmedHistory}.

generate_capacity_report(State) ->
    Metrics = get_latest_metrics(State#state.metrics_history),
    #{
        timestamp => Metrics#capacity_metrics.timestamp,
        current_metrics => Metrics,
        health_score => 85
    }.

get_latest_metrics(History) ->
    case queue:out(History) of
        {empty, _} -> #capacity_metrics{timestamp = 0, cpu = #{}, memory = #{}, network = #{}, disk = #{}, connections = #{}, throughput = #{}, load = #{}};
        {{value, Metrics}, _} -> Metrics
    end.

trim_history(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of true -> queue:drop(Queue); false -> Queue end.
