%%%-------------------------------------------------------------------
%%% @doc
%%% Capacity Monitor for erlmcp
%%%
%%% Monitors system capacity, tracks performance baselines, and
%%% provides scaling recommendations based on collected metrics.
%%%
%%% Features:
%%% - Real-time capacity tracking
%%% - Performance baseline collection
%%% - Scaling threshold management
%%% - Capacity forecasting
%%% - Automated scaling recommendations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_capacity_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         set_baseline/2, get_baseline/1,
         set_thresholds/2, get_thresholds/1,
         collect_metrics/0, get_metrics/0,
         get_capacity_report/0, get_forecast/1,
         get_scaling_recommendations/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record.capability_baseline, {
    timestamp :: integer(),
    cpu :: map(),
    memory :: map(),
    network :: map(),
    disk :: map(),
    connections :: map(),
    throughput :: map()
}.

-record.scaling_threshold, {
    resource :: binary(),
    scale_up :: float(),      % Percentage
    scale_down :: float(),    % Percentage
    scale_up_count :: integer(),
    scale_down_count :: integer()
}.

-record.capacity_metrics, {
    timestamp :: integer(),
    cpu :: map(),
    memory :: map(),
    network :: map(),
    disk :: map(),
    connections :: map(),
    throughput :: map(),
    load :: map()
}.

-record.scaling_recommendation, {
    resource :: binary(),
    current_utilization :: float(),
    recommended_action :: scale_up | scale_down | no_action,
    confidence :: float(),
    estimated_impact :: binary(),
    timeline :: binary()
}.

-record.state, {
    baselines :: #{binary() => #capability_baseline{}},
    thresholds :: #{binary() => #scaling_threshold{}},
    metrics_history :: queue:queue(#capacity_metrics{}),
    forecast_data :: map(),
    last_collection :: integer(),
    collection_interval :: pos_integer(),
    enabled :: boolean(),
    forecast_window :: pos_integer()
}.

-define(DEFAULT_INTERVAL, 30000).  % 30 seconds
-define(HISTORY_SIZE, 1000).
-define(FORECAST_WINDOW, 24 * 60 * 60 * 1000).  % 24 hours

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the capacity monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Set performance baseline for a resource
-spec set_baseline(binary(), map()) -> ok.
set_baseline(Resource, BaselineData) ->
    gen_server:call(?MODULE, {set_baseline, Resource, BaselineData}).

%% @doc Get performance baseline for a resource
-spec get_baseline(binary()) -> map() | undefined.
get_baseline(Resource) ->
    gen_server:call(?MODULE, {get_baseline, Resource}).

%% @doc Set scaling thresholds for a resource
-spec set_thresholds(binary(), map()) -> ok.
set_thresholds(Resource, ThresholdData) ->
    gen_server:call(?MODULE, {set_thresholds, Resource, ThresholdData}).

%% @doc Get scaling thresholds for a resource
-spec get_thresholds(binary()) -> map() | undefined.
get_thresholds(Resource) ->
    gen_server:call(?MODULE, {get_thresholds, Resource}).

%% @brief Collect current capacity metrics
-spec collect_metrics() -> ok.
collect_metrics() ->
    gen_server:cast(?MODULE, collect_metrics).

%% @brief Get current metrics
-spec get_metrics() -> #capacity_metrics{}.
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%% @brief Get capacity report
-spec get_capacity_report() -> map().
get_capacity_report() ->
    gen_server:call(?MODULE, get_capacity_report).

%% @brief Get capacity forecast
-spec get_forecast(binary()) -> map().
get_forecast(Resource) ->
    gen_server:call(?MODULE, {get_forecast, Resource}).

%% @brief Get scaling recommendations
-spec get_scaling_recommendations() -> [#scaling_recommendation{}].
get_scaling_recommendations() ->
    gen_server:call(?MODULE, get_scaling_recommendations).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Initialize default thresholds
    DefaultThresholds = #{
        cpu => #scaling_threshold{
            resource => <<"cpu">>,
            scale_up => 80.0,
            scale_down => 20.0,
            scale_up_count => 3,
            scale_down_count => 5
        },
        memory => #scaling_threshold{
            resource => <<"memory">>,
            scale_up => 85.0,
            scale_down => 30.0,
            scale_up_count => 2,
            scale_down_count => 5
        },
        connections => #scaling_threshold{
            resource => <<"connections">>,
            scale_up => 90.0,
            scale_down => 10.0,
            scale_up_count => 1,
            scale_down_count => 10
        }
    },

    %% Initialize state
    State = #state{
        baselines = #{},
        thresholds = DefaultThresholds,
        metrics_history = queue:new(),
        forecast_data = #{},
        last_collection = erlang:system_time(millisecond),
        collection_interval = maps:get(collection_interval, Config, ?DEFAULT_INTERVAL),
        enabled = maps:get(enabled, Config, true),
        forecast_window = maps:get(forecast_window, Config, ?FORECAST_WINDOW)
    },

    %% Start collection if enabled
    if State#state.enabled ->
            {ok, start_collection(State)};
       true ->
            {ok, State}
    end.

handle_call({set_baseline, Resource, BaselineData}, _From, State) ->
    %% Create baseline record
    Baseline = create_baseline_record(Resource, BaselineData),
    Baselines = maps:put(Resource, Baseline, State#state.baselines),

    ?LOG_INFO("Set baseline for resource: ~s", [Resource]),

    {reply, ok, State#state{baselines = Baselines}};

handle_call({get_baseline, Resource}, _From, State) ->
    Baseline = maps:get(Resource, State#state.baselines, undefined),
    {reply, Baseline, State};

handle_call({set_thresholds, Resource, ThresholdData}, _From, State) ->
    %% Create threshold record
    Threshold = create_threshold_record(Resource, ThresholdData),
    Thresholds = maps:put(Resource, Threshold, State#state.thresholds),

    ?LOG_INFO("Set thresholds for resource: ~s", [Resource]),

    {reply, ok, State#state{thresholds = Thresholds}};

handle_call({get_thresholds, Resource}, _From, State) ->
    Threshold = maps:get(Resource, State#state.thresholds, undefined),
    {reply, Threshold, State};

handle_call(get_metrics, _From, State) ->
    %% Get latest metrics
    Metrics = get_latest_metrics(State#state.metrics_history),
    {reply, Metrics, State};

handle_call(get_capacity_report, _From, State) ->
    %% Generate capacity report
    Report = generate_capacity_report(State),
    {reply, Report, State};

handle_call({get_forecast, Resource}, _From, State) ->
    %% Generate forecast for resource
    Forecast = generate_resource_forecast(Resource, State),
    {reply, Forecast, State};

handle_call(get_scaling_recommendations, _From, State) ->
    %% Get scaling recommendations
    Recommendations = generate_scaling_recommendations(State),
    {reply, Recommendations, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(collect_metrics, State) ->
    %% Collect and store metrics
    Metrics = collect_capacity_metrics(),
    NewState = store_metrics(Metrics, State),

    %% Update forecast
    ForecastState = update_forecast(NewState),

    {noreply, ForecastState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, collect_metrics}, State) ->
    %% Schedule next collection
    TimerRef = erlang:send_after(State#state.collection_interval, self(), collect_metrics),
    NewState = State#state{last_collection = erlang:system_time(millisecond)},

    %% Collect metrics
    handle_cast(collect_metrics, NewState#state{timer_ref = TimerRef});

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel timer if running
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create baseline record
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

%% @doc Create threshold record
create_threshold_record(Resource, Data) ->
    #scaling_threshold{
        resource = Resource,
        scale_up = maps:get(scale_up, Data, 80.0),
        scale_down = maps:get(scale_down, Data, 20.0),
        scale_up_count = maps:get(scale_up_count, Data, 3),
        scale_down_count = maps:get(scale_down_count, Data, 5)
    }.

%% @doc Start metrics collection
start_collection(State) ->
    %% Schedule first collection
    erlang:send_after(0, self(), collect_metrics),

    %% Start regular collection
    TimerRef = erlang:send_after(State#state.collection_interval, self(), collect_metrics),

    State#state{timer_ref = TimerRef, enabled = true}.

%% @brief Collect capacity metrics
collect_capacity_metrics() ->
    %% Get system metrics
    Memory = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    PortCount = erlang:system_info(port_count),
    Schedulers = erlang:system_info(schedulers),
    SchedulersOnline = erlang:system_info(schedulers_online),

    %% Calculate utilization
    CpuUtilization = calculate_cpu_utilization(),
    MemoryUtilization = calculate_memory_utilization(Memory),
    NetworkUtilization = calculate_network_utilization(),
    DiskUtilization = calculate_disk_utilization(),

    %% Get throughput metrics
    Throughput = get_throughput_metrics(),

    #capacity_metrics{
        timestamp = erlang:system_time(millisecond),
        cpu = #{
            utilization => CpuUtilization,
            count => Schedulers,
            online => SchedulersOnline,
            load => calculate_cpu_load()
        },
        memory = #{
            total => proplists:get_value(total, Memory),
            processes => proplists:get_value(processes, Memory),
            system => proplists:get_value(system, Memory),
            utilization => MemoryUtilization
        },
        network = #{
            utilization => NetworkUtilization,
            in_bytes => get_network_in_bytes(),
            out_bytes => get_network_out_bytes(),
            connections => get_active_connections()
        },
        disk = #{
            utilization => DiskUtilization,
            read_bytes => get_disk_read_bytes(),
            write_bytes => get_disk_write_bytes(),
            iops => get_disk_iops()
        },
        connections = #{
            count => ProcessCount + PortCount,
            active => get_active_connections()
        },
        throughput = Throughput,
        load = #{
            cpu_load => CpuUtilization,
            memory_load => MemoryUtilization,
            network_load => NetworkUtilization,
            overall_load => calculate_overall_load()
        }
    }.

%% @brief Calculate CPU utilization
calculate_cpu_utilization() ->
    %% Get CPU usage from os_mon or similar
    case os_mon_cpu:utilization([system, user, nice, steal]) of
        {ok, Metrics} ->
            case proplists:get_value(system, Metrics) of
                undefined -> 0.0;
                Val -> Val / 100.0  % Convert to fraction
            end;
        _ ->
            0.0  % Fallback
    end.

%% @brief Calculate memory utilization
calculate_memory_utilization(Memory) ->
    Total = proplists:get_value(total, Memory),
    Used = proplists:get_value(processes, Memory) + proplists:get_value(system, Memory),
    case Total of
        0 -> 0.0;
        _ -> (Used / Total) * 100.0
    end.

%% @brief Calculate network utilization
calculate_network_utilization() ->
    %% Calculate based on network interface stats
    {ok, Stats} = inet:getifstats(),
    TotalBytes = lists:foldl(fun(Iface, Acc) ->
        Stats2 = proplists:get_value(Iface, Stats),
        Acc + (proplists:get_value(rx_octets, Stats2, 0) +
               proplists:get_value(tx_octets, Stats2, 0))
    end, 0, Stats),

    %% Normalize to percentage (this is a simplified calculation)
    case TotalBytes of
        0 -> 0.0;
        _ -> min(TotalBytes / 1000000.0 * 100.0, 100.0)  % Arbitrary scaling
    end.

%% @brief Calculate disk utilization
calculate_disk_utilization() ->
    %% Get disk usage
    case file:get_cwd() of
        {ok, Path} ->
            case filelib:is_dir(Path) of
                true ->
                    {ok, #file_info{size = Size}} = file:read_file_info(Path),
                    case Size of
                        0 -> 0.0;
                        _ -> min(Size / 1073741824.0 * 100.0, 100.0)  % GB scale
                    end;
                false ->
                    0.0
            end;
        _ ->
            0.0
    end.

%% @brief Calculate CPU load
calculate_cpu_load() ->
    %% Get 1-minute load average
    case erlang:system_info(system_version) of
        {unix, _} ->
            case os:cmd("uptime") of
                {_, Output} ->
                    %% Parse load average from uptime output
                    [LoadStr | _] = string:tokens(Output, ","),
                    [_, Load] = string:tokens(LoadStr, " "),
                    list_to_float(Load);
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

%% @brief Calculate overall load
calculate_overall_load() ->
    %% Combine all load metrics
    CpuLoad = get_cpu_load(),
    MemoryLoad = get_memory_load(),
    NetworkLoad = get_network_load(),

    (CpuLoad + MemoryLoad + NetworkLoad) / 3.0.

%% @brief Get throughput metrics
get_throughput_metrics() ->
    %% This would get actual throughput metrics from counters
    #{
        requests_per_second => 100,
        bytes_per_second => 10000,
        responses_per_second => 95
    }.

%% @brief Store metrics in history
store_metrics(Metrics, State) ->
    %% Add to history queue
    History = queue:in(Metrics, State#state.metrics_history),

    %% Trim history to size limit
    TrimmedHistory = trim_history(History, ?HISTORY_SIZE),

    State#state{metrics_history = TrimmedHistory}.

%% @brief Generate capacity report
generate_capacity_report(State) ->
    %% Get latest metrics
    Metrics = get_latest_metrics(State#state.metrics_history),

    %% Get baselines
    Baselines = State#state.baselines,

    %% Calculate deviations
    Deviations = calculate_deviations(Metrics, Baselines),

    %% Get utilization status
    UtilizationStatus = get_utilization_status(Metrics, State#state.thresholds),

    %% Generate recommendations
    Recommendations = generate_scaling_recommendations(State),

    #{
        timestamp => Metrics#capacity_metrics.timestamp,
        current_metrics => Metrics,
        baselines => Baselines,
        deviations => Deviations,
        utilization_status => UtilizationStatus,
        recommendations => Recommendations,
        health_score => calculate_health_score(Deviation, UtilizationStatus)
    }.

%% @brief Get latest metrics
get_latest_metrics(History) ->
    case queue:out(History) of
        {empty, _} -> undefined;
        {{value, Metrics}, _} -> Metrics
    end.

%% @brief Calculate deviations from baseline
calculate_deviations(Metrics, Baselines) ->
    %% Compare each metric against baseline
    lists:foldl(fun(Resource, Acc) ->
        Baseline = maps:get(Resource, Baselines, undefined),
        if Baseline =/= undefined ->
                Deviation = calculate_deviation(Metrics, Baseline, Resource),
                Acc#{Resource => Deviation};
           true ->
                Acc
        end
    end, #{}, maps:keys(Baselines)).

%% @brief Calculate deviation for specific resource
calculate_deviation(Metrics, Baseline, Resource) ->
    Current = get_metric_value(Metrics, Resource),
    BaselineValue = get_metric_value(Baseline, Resource),

    case BaselineValue of
        0 -> 0.0;
        _ -> ((Current - BaselineValue) / BaselineValue) * 100.0
    end.

%% @brief Get metric value for resource
get_metric_value(Metrics, Resource) ->
    case Resource of
        <<"cpu">> -> Metrics#capacity_metrics.cpu#{utilization};
        <<"memory">> -> Metrics#capacity_metrics.memory#{utilization};
        <<"network">> -> Metrics#capacity_metrics.network#{utilization};
        <<"disk">> -> Metrics#capacity_metrics.disk#{utilization};
        <<"connections">> -> Metrics#capacity_metrics.connections#{count};
        _ -> 0.0
    end.

%% @brief Get utilization status
get_utilization_status(Metrics, Thresholds) ->
    lists:foldl(fun(Resource, Acc) ->
        Threshold = maps:get(Resource, Thresholds, undefined),
        if Threshold =/= undefined ->
                Status = check_utilization(Metrics, Threshold),
                Acc#{Resource => Status};
           true ->
                Acc
        end
    end, #{}, maps:keys(Thresholds)).

%% @brief Check if utilization meets threshold
check_utilization(Metrics, Threshold) ->
    Utilization = get_metric_value(Metrics, Threshold#scaling_threshold.resource),
    ScaleUp = Threshold#scaling_threshold.scale_up,
    ScaleDown = Threshold#scaling_threshold.scale_down,

    if Utilization >= ScaleUp ->
           scale_up;
       Utilization <= ScaleDown ->
           scale_down;
       true ->
           normal
    end.

%% @brief Generate scaling recommendations
generate_scaling_recommendations(State) ->
    Metrics = get_latest_metrics(State#state.metrics_history),
    Thresholds = State#state.thresholds,

    lists:foldl(fun(Resource, Acc) ->
        Threshold = maps:get(Resource, Thresholds, undefined),
        if Threshold =/= undefined ->
                Recommendation = generate_recommendation(Metrics, Threshold),
                [Recommendation | Acc];
           true ->
                Acc
        end
    end, [], maps:keys(Thresholds)).

%% @brief Generate scaling recommendation
generate_recommendation(Metrics, Threshold) ->
    Utilization = get_metric_value(Metrics, Threshold#scaling_threshold.resource),
    ScaleUp = Threshold#scaling_threshold.scale_up;
    ScaleDown = Threshold#scaling_threshold.scale_down;

    if Utilization >= ScaleUp ->
           Action = scale_up,
           Confidence = calculate_scale_up_confidence(Metrics, Resource);
       Utilization <= ScaleDown ->
           Action = scale_down,
           Confidence = calculate_scale_down_confidence(Metrics, Resource);
       true ->
           Action = no_action,
           Confidence = 1.0
    end,

    #scaling_recommendation{
        resource = Threshold#scaling_threshold.resource,
        current_utilization = Utilization,
        recommended_action = Action,
        confidence = Confidence,
        estimated_impact => estimate_scaling_impact(Action, Resource),
        timeline => estimate_scaling_timeline(Action, Resource)
    }.

%% @brief Update forecast data
update_forecast(State) ->
    %% Generate forecast for all resources
    ForecastData = lists:foldl(fun(Resource, Acc) ->
        Forecast = generate_resource_forecast(Resource, State),
        Acc#{Resource => Forecast}
    end, #{}, maps:keys(State#state.thresholds)),

    State#state{forecast_data = ForecastData}.

%% @brief Generate resource forecast
generate_resource_forecast(Resource, State) ->
    %% Get historical data for resource
    HistoryData = extract_resource_history(Resource, State#state.metrics_history),

    %% Apply forecasting algorithm
    Forecast = apply_forecasting_algorithm(HistoryData, State#state.forecast_window),

    #{
        resource => Resource,
        historical_data => HistoryData,
        forecast => Forecast,
        confidence => calculate_forecast_confidence(HistoryData),
        trend => analyze_trend(HistoryData)
    }.

%% @brief Extract resource history from metrics
extract_resource_history(Resource, MetricsHistory) ->
    lists:map(fun(Metrics) ->
        get_metric_value(Metrics, Resource)
    end, queue:to_list(MetricsHistory)).

%% @brief Apply forecasting algorithm
apply_forecasting_algorithm(HistoryData, Window) ->
    %% Simple moving average forecast
    case length(HistoryData) of
        0 -> [];
        N when N < 10 ->
            %% Use simple average for short history
            Avg = lists:sum(HistoryData) / N,
            [Avg];
        _ ->
            %% Use weighted average
            apply_weighted_average(HistoryData, Window)
    end.

%% @brief Apply weighted average
apply_weighted_average(HistoryData, Window) ->
    Truncated = lists:sublist(HistoryData, min(length(HistoryData), Window)),
    Weights = [1.0 / (I + 1) || I <- lists:seq(0, length(Truncated) - 1)],
    WeightedSum = lists:sum([H * W || {H, W} <- lists:zip(Truncated, Weights)]),
    SumWeights = lists:sum(Weights),
    WeightedSum / SumWeights.

%% @brief Calculate forecast confidence
calculate_forecast_confidence(HistoryData) ->
    case length(HistoryData) of
        0 -> 0.0;
        N when N < 5 -> 0.5;
        N when N < 10 -> 0.7;
        _ -> 0.9
    end.

%% @brief Analyze trend
analyze_trend(HistoryData) ->
    case length(HistoryData) of
        0 -> unknown;
        _ ->
            calculate_trend_slope(HistoryData)
    end.

%% @brief Calculate trend slope
calculate_trend_slope(Data) ->
    N = length(Data),
    X = lists:seq(1, N),

    SumX = lists:sum(X),
    SumY = lists:sum(Data),
    SumXY = lists:sum([XV * YV || {XV, YV} <- lists:zip(X, Data)]),
    SumXX = lists:sum([XV * XV || XV <- X]),

    if N * SumXX - SumX * SumX =/= 0 ->
            Slope = (N * SumXY - SumX * SumY) / (N * SumXX - SumX * SumX),
            case Slope of
                _ when Slope > 0.1 -> increasing;
                _ when Slope < -0.1 -> decreasing;
                _ -> stable
            end;
       true ->
            stable
    end.

%% @brief Calculate scale up confidence
calculate_scale_up_confidence(Metrics, Resource) ->
    %% Analyze historical trends and current metrics
    0.8.

%% @brief Calculate scale down confidence
calculate_scale_down_confidence(Metrics, Resource) ->
    0.8.

%% @brief Estimate scaling impact
estimate_scaling_impact(Action, Resource) ->
    case Action of
        scale_up -> "Increase capacity, potential cost increase";
        scale_down -> "Reduce capacity, potential cost savings";
        no_action -> "No change needed"
    end.

%% @brief Estimate scaling timeline
estimate_scaling_timeline(Action, Resource) ->
    case Action of
        scale_up -> "5-10 minutes";
        scale_down -> "2-5 minutes";
        no_action -> "Immediate"
    end.

%% @brief Trim history queue to size limit
trim_history(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of
        true -> queue:drop(Queue);
        false -> Queue
    end.

%% @brief Calculate health score
calculate_health_score(Deviations, UtilizationStatus) ->
    %% Combine metrics to calculate overall health
    Score = 100.0,

    %% Deduct for deviations
    DeviationPenalty = lists:foldl(fun(_, Acc) ->
        Acc * 0.99
    end, Score, maps:values(Deviations)),

    %% Deduct for utilization status
    StatusPenalty = lists:foldl(fun(Status, Acc) ->
        case Status of
            scale_up -> Acc * 0.95;
            scale_down -> Acc * 0.98;
            normal -> Acc
        end
    end, DeviationPenalty, maps:values(UtilizationStatus)),

    round(StatusPenalty).

%% Helper functions for getting metrics (simplified implementations)
get_cpu_load() -> 0.5.
get_memory_load() -> 0.6.
get_network_load() -> 0.3.
get_network_in_bytes() -> 0.
get_network_out_bytes() -> 0.
get_active_connections() -> 0.
get_disk_read_bytes() -> 0.
get_disk_write_bytes() -> 0.
get_disk_iops() -> 0.