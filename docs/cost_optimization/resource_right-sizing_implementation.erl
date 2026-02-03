%%====================================================================
%% erlmcp Resource Right-Sizing Implementation
%%====================================================================
%%
%% This module implements comprehensive resource right-sizing for erlmcp v3
%% based on actual workload analysis and optimization patterns.
%%

-module(erlmcp_resource_right_sizer).

-behaviour(gen_server).

-export([start_link/0, analyze_workload/1, get_recommendations/0,
         apply_recommendations/1, monitor_resources/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records for resource analysis
-record(resource_metrics,
        {timestamp :: integer(),
         component :: atom(),
         cpu_usage :: float(),
         memory_usage :: float(),
         disk_usage :: float(),
         network_in :: float(),
         network_out :: float(),
         active_connections :: integer(),
         request_rate :: float(),
         response_time :: float()}).

-record(workload_profile,
        {type :: development | staging | production | burst,
         peak_requests :: pos_integer(),
         avg_requests :: pos_integer(),
         request_patterns :: constant | bursty | periodic,
         storage_needs :: low | medium | high,
         observability_level :: minimal | standard | comprehensive,
         time_series :: [#resource_metrics{}]}).

-record(right_sizing_recommendation,
        {component :: atom(),
         current_config :: map(),
         recommended_config :: map(),
         potential_savings :: float(),
         risk_level :: low | medium | high,
         implementation_priority :: immediate | short_term | long_term}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec analyze_workload(binary()) -> {ok, #workload_profile{}} | {error, term()}.
analyze_workload(TimeRange) ->
    gen_server:call(?MODULE, {analyze_workload, TimeRange}, 30000).

-spec get_recommendations() -> [#right_sizing_recommendation{}].
get_recommendations() ->
    gen_server:call(?MODULE, get_recommendations, 10000).

-spec apply_recommendations([#right_sizing_recommendation{}]) -> ok | {error, term()}.
apply_recommendations(Recommendations) ->
    gen_server:cast(?MODULE, {apply_recommendations, Recommendations}).

-spec monitor_resources() -> ok.
monitor_resources() ->
    gen_server:cast(?MODULE, monitor_resources).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, map()}.
init([]) ->
    %% Initialize resource monitoring
    schedule_resource_collection(),

    %% Load historical data
    HistoricalData = load_historical_data(),

    State = #{
        historical_data => HistoricalData,
        current_profile => undefined,
        recommendations => [],
        last_analysis => undefined,
        monitoring_interval => 60000  % 1 minute
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, term(), map()}.
handle_call({analyze_workload, TimeRange}, _From, State) ->
    %% Analyze historical resource usage
    Metrics = collect_historical_metrics(TimeRange),

    %% Generate workload profile
    Profile = generate_workload_profile(Metrics),

    %% Generate recommendations
    Recommendations = generate_right_sizing_recommendations(Profile),

    %% Update state
    NewState = State#{
        current_profile => Profile,
        recommendations => Recommendations,
        last_analysis => os:system_time(millisecond)
    },

    {reply, {ok, Profile, Recommendations}, NewState};

handle_call(get_recommendations, _From, State) ->
    {reply, State#recommendations, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({apply_recommendations, Recommendations}, State) ->
    %% Apply recommendations safely
    case apply_recommendations_safely(Recommendations) of
        ok ->
            %% Log successful application
            log_recommendation_application(Recommendations),
            %% Update state
            NewState = State#{
                recommendations => []
            },
            {noreply, NewState};
        {error, Reason} ->
            %% Log error and keep recommendations
            log_error("Failed to apply recommendations: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(monitor_resources, State) ->
    %% Collect current resource metrics
    CurrentMetrics = collect_current_metrics(),

    %% Compare with expected profile
    ExpectedProfile = State#current_profile,
    if
        ExpectedProfile =/= undefined ->
            Analysis = compare_with_expected(CurrentMetrics, ExpectedProfile),
            handle_deviation(Analysis);
        true ->
            ok
    end,

    %% Schedule next monitoring
    schedule_resource_collection(),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(resource_collection, State) ->
    %% Collect and store resource metrics
    Metrics = collect_current_metrics(),
    store_metrics(Metrics),

    %% Trigger resource monitoring cast
    ?MODULE:monitor_resources(),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Schedule resource collection
schedule_resource_collection() ->
    erlang:send_after(?MODULE, resource_collection, get_env(monitoring_interval, 60000)).

%% Collect historical metrics
collect_historical_metrics(TimeRange) ->
    %% Query metrics database
    Query = build_metrics_query(TimeRange),
    Metrics = execute_metrics_query(Query),

    %% Filter and aggregate
    FilteredMetrics = filter_metrics(Metrics),
    AggregatedMetrics = aggregate_metrics(FilteredMetrics),

    AggregatedMetrics.

%% Generate workload profile from metrics
generate_workload_profile(Metrics) ->
    %% Analyze request patterns
    RequestPatterns = analyze_request_patterns(Metrics),

    %% Analyze resource usage patterns
    ResourcePatterns = analyze_resource_patterns(Metrics),

    %% Determine workload type
    WorkloadType = determine_workload_type(RequestPatterns, ResourcePatterns),

    %% Calculate requirements
    PeakRequests = calculate_peak_requests(RequestPatterns),
    AvgRequests = calculate_average_requests(RequestPatterns),

    %% Storage needs
    StorageNeeds = determine_storage_needs(ResourcePatterns),

    %% Observability level
    ObservabilityLevel = determine_observability_level(Metrics),

    #workload_profile{
        type = WorkloadType,
        peak_requests = PeakRequests,
        avg_requests = AvgRequests,
        request_patterns = RequestPatterns#patterns.type,
        storage_needs = StorageNeeds,
        observability_level = ObservabilityLevel,
        time_series = Metrics
    }.

%% Generate right-sizing recommendations
generate_right_sizing_recommendations(Profile) ->
    Recommendations = [],

    %% Server recommendations
    ServerRec = recommend_server_config(Profile),
    Recommendations = [ServerRec | Recommendations],

    %% Transport recommendations
    TransportRec = recommend_transport_config(Profile),
    Recommendations = [TransportRec | Recommendations],

    %% Storage recommendations
    StorageRec = recommend_storage_config(Profile),
    Recommendations = [StorageRec | Recommendations],

    %% Performance recommendations
    PerformanceRec = recommend_performance_config(Profile),
    Recommendations = [PerformanceRec | Recommendations],

    %% Monitoring recommendations
    MonitoringRec = recommend_monitoring_config(Profile),
    Recommendations = [MonitoringRec | Recommendations],

    %% Sort by priority
    SortedRecommendations = sort_recommendations_by_priority(Recommendations),

    SortedRecommendations.

%% Server configuration recommendations
recommend_server_config(Profile) ->
    CurrentConfig = get_current_server_config(),

    RecommendedConfig = case Profile#workload_profile.type of
        development ->
            #{
                max_concurrent_requests => 100,
                request_timeout => 15000,
                max_subscriptions_per_resource => 1000,
                max_progress_tokens => 5000
            };
        staging ->
            #{
                max_concurrent_requests => 500,
                request_timeout => 30000,
                max_subscriptions_per_resource => 2500,
                max_progress_tokens => 25000
            };
        production ->
            #{
                max_concurrent_requests => Profile#workload_profile.peak_requests,
                request_timeout => 30000,
                max_subscriptions_per_resource => Profile#workload_profile.peak_requests * 5,
                max_progress_tokens => Profile#workload_profile.peak_requests * 10
            };
        burst ->
            #{
                max_concurrent_requests => Profile#workload_profile.peak_requests * 2,
                request_timeout => 10000,
                max_subscriptions_per_resource => Profile#workload_profile.peak_requests * 10,
                max_progress_tokens => Profile#workload_profile.peak_requests * 20
            }
    end,

    Calculate savings
    CurrentCost = calculate_server_cost(CurrentConfig),
    RecommendedCost = calculate_server_cost(RecommendedConfig),
    Savings = CurrentCost - RecommendedCost,

    #right_sizing_recommendation{
        component = server,
        current_config = CurrentConfig,
        recommended_config = RecommendedConfig,
        potential_savings = Savings,
        risk_level = calculate_risk_level(server, CurrentConfig, RecommendedConfig),
        implementation_priority = determine_priority(server, Savings)
    }.

%% Transport configuration recommendations
recommend_transport_config(Profile) ->
    CurrentConfig = get_current_transport_config(),

    RecommendedConfig = case Profile#workload_profile.type of
        development ->
            #{
                max_connections => 200,
                tcp => #{
                    max_connections => 200,
                    backlog => 256
                }
            };
        staging ->
            #{
                max_connections => 1000,
                tcp => #{
                    max_connections => 1000,
                    backlog => 512
                }
            };
        production ->
            #{
                max_connections => Profile#workload_profile.peak_requests,
                tcp => #{
                    max_connections => Profile#workload_profile.peak_requests,
                    backlog => Profile#workload_profile.peak_requests div 4
                }
            };
        burst ->
            #{
                max_connections => Profile#workload_profile.peak_requests * 2,
                tcp => #{
                    max_connections => Profile#workload_profile.peak_requests * 2,
                    backlog => Profile#workload_profile.peak_requests div 2
                }
            }
    end,

    %% Calculate savings
    CurrentCost = calculate_transport_cost(CurrentConfig),
    RecommendedCost = calculate_transport_cost(RecommendedConfig),
    Savings = CurrentCost - RecommendedCost,

    #right_sizing_recommendation{
        component = transport,
        current_config = CurrentConfig,
        recommended_config = RecommendedConfig,
        potential_savings = Savings,
        risk_level = calculate_risk_level(transport, CurrentConfig, RecommendedConfig),
        implementation_priority = determine_priority(transport, Savings)
    }.

%% Storage configuration recommendations
recommend_storage_config(Profile) ->
    CurrentConfig = get_current_storage_config(),

    RecommendedConfig = case Profile#workload_profile.storage_needs of
        low ->
            #{
                retention_days => 7,
                compression => true,
                replication => 1
            };
        medium ->
            #{
                retention_days => 30,
                compression => true,
                replication => 2
            };
        high ->
            #{
                retention_days => 90,
                compression => true,
                replication => 3
            }
    end,

    %% Calculate savings
    CurrentCost = calculate_storage_cost(CurrentConfig),
    RecommendedCost = calculate_storage_cost(RecommendedConfig),
    Savings = CurrentCost - RecommendedCost,

    #right_sizing_recommendation{
        component = storage,
        current_config = CurrentConfig,
        recommended_config = RecommendedConfig,
        potential_savings = Savings,
        risk_level = calculate_risk_level(storage, CurrentConfig, RecommendedConfig),
        implementation_priority = determine_priority(storage, Savings)
    }.

%% Performance configuration recommendations
recommend_performance_config(Profile) ->
    CurrentConfig = get_current_performance_config(),

    RecommendedConfig = case Profile#workload_profile.type of
        development ->
            #{
                process_limit => 16384,
                max_heap_size => 1073741824,  % 1GB
                gc_strategy => normal
            };
        staging ->
            #{
                process_limit => 65536,
                max_heap_size => 2147483648,  % 2GB
                gc_strategy => aggressive
            };
        production ->
            #{
                process_limit => 262144,
                max_heap_size => 4294967296,  % 4GB
                gc_strategy => adaptive
            };
        burst ->
            #{
                process_limit => 524288,
                max_heap_size => 8589934592,  % 8GB
                gc_strategy => aggressive
            }
    end,

    %% Calculate savings
    CurrentCost = calculate_performance_cost(CurrentConfig),
    RecommendedCost = calculate_performance_cost(RecommendedConfig),
    Savings = CurrentCost - RecommendedCost,

    #right_sizing_recommendation{
        component = performance,
        current_config = CurrentConfig,
        recommended_config = RecommendedConfig,
        potential_savings = Savings,
        risk_level = calculate_risk_level(performance, CurrentConfig, RecommendedConfig),
        implementation_priority = determine_priority(performance, Savings)
    }.

%% Monitoring configuration recommendations
recommend_monitoring_config(Profile) ->
    CurrentConfig = get_current_monitoring_config(),

    RecommendedConfig = case Profile#workload_profile.observability_level of
        minimal ->
            #{
                enabled => true,
                metrics_retention_days => 7,
                health_check_interval => 60,
                prometheus_port => 9090
            };
        standard ->
            #{
                enabled => true,
                metrics_retention_days => 30,
                health_check_interval => 30,
                prometheus_port => 9090
            };
        comprehensive ->
            #{
                enabled => true,
                metrics_retention_days => 90,
                health_check_interval => 15,
                prometheus_port => 9090
            }
    end,

    %% Calculate savings
    CurrentCost = calculate_monitoring_cost(CurrentConfig),
    RecommendedConfig = calculate_monitoring_cost(RecommendedConfig),
    Savings = CurrentCost - RecommendedCost,

    #right_sizing_recommendation{
        component = monitoring,
        current_config = CurrentConfig,
        recommended_config = RecommendedConfig,
        potential_savings = Savings,
        risk_level => calculate_risk_level(monitoring, CurrentConfig, RecommendedConfig),
        implementation_priority = determine_priority(monitoring, Savings)
    }.

%% Apply recommendations safely
apply_recommendations_safely(Recommendations) ->
    %% Validate recommendations
    ValidRecommendations = validate_recommendations(Recommendations),
    if
        length(ValidRecommendations) =/= length(Recommendations) ->
            {error, invalid_recommendations};
        true ->
            %% Apply in order of priority
            apply_by_priority(ValidRecommendations)
    end.

%% Validate recommendations
validate_recommendations(Recommendations) ->
    lists:filter(fun validate_recommendation/1, Recommendations).

validate_recommendation(Recommendation) ->
    %% Validate configuration values
    CurrentConfig = Recommendation#right_sizing_recommendation.current_config,
    RecommendedConfig = Recommendation#right_sizing_recommendation.recommended_config,

    %% Check for valid ranges
    case is_valid_config_range(RecommendedConfig) of
        true ->
            %% Check for potential conflicts
            case check_config_conflicts(RecommendedConfig) of
                no_conflict ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
    end.

%% Apply recommendations by priority
apply_by_priority(Recommendations) ->
    %% Sort by implementation priority
    Sorted = lists:sort(
        fun(R1, R2) ->
            Priority1 = priority_to_integer(R1#right_sizing_recommendation.implementation_priority),
            Priority2 = priority_to_integer(R2#right_sizing_recommendation.implementation_priority),
            Priority1 =< Priority2
        end,
        Recommendations
    ),

    %% Apply each recommendation
    lists:foreach(fun apply_single_recommendation/1, Sorted),

    ok.

%% Apply single recommendation
apply_single_recommendation(Recommendation) ->
    Component = Recommendation#right_sizing_recommendation.component,
    Config = Recommendation#right_sizing_recommendation.recommended_config,

    case Component of
        server ->
            apply_server_config(Config);
        transport ->
            apply_transport_config(Config);
        storage ->
            apply_storage_config(Config);
        performance ->
            apply_performance_config(Config);
        monitoring ->
            apply_monitoring_config(Config);
        _ ->
            log_error("Unknown component: ~p", [Component])
    end.

%% Log recommendation application
log_recommendation_application(Recommendations) ->
    TotalSavings = lists:sum([
        R#right_sizing_recommendation.potential_savings || R <- Recommendations
    ]),

    log_info("Applied ~p recommendations with total savings: ~p", [
        length(Recommendations),
        TotalSavings
    ]),

    %% Store application record
    ApplicationRecord = #{
        timestamp => os:system_time(millisecond),
        recommendations => Recommendations,
        total_savings => TotalSavings
    },

    store_application_record(ApplicationRecord).

%% Compare current metrics with expected profile
compare_with_expected(CurrentMetrics, ExpectedProfile) ->
    %% Calculate deviations
    ExpectedCPU = calculate_expected_cpu(ExpectedProfile),
    ActualCPU = CurrentMetrics#resource_metrics.cpu_usage,

    ExpectedMemory = calculate_expected_memory(ExpectedProfile),
    ActualMemory = CurrentMetrics#resource_metrics.memory_usage,

    ExpectedNetwork = calculate_expected_network(ExpectedProfile);
    ActualNetworkIn = CurrentMetrics#resource_metrics.network_in,
    ActualNetworkOut = CurrentMetrics#resource_metrics.network_out,

    %% Create deviation report
    Deviation = #{
        cpu_deviation => abs(ActualCPU - ExpectedCPU) / ExpectedCPU,
        memory_deviation => abs(ActualMemory - ExpectedMemory) / ExpectedMemory,
        network_in_deviation => abs(ActualNetworkIn - ExpectedNetwork) / ExpectedNetwork,
        network_out_deviation => abs(ActualNetworkOut - ExpectedNetwork) / ExpectedNetwork
    },

    Deviation.

%% Handle metric deviations
handle_deviation(Deviation) ->
    %% Check for significant deviations
    CPUDeviation = maps:get(cpu_deviation, Deviation, 0),
    MemoryDeviation = maps:get(memory_deviation, Deviation, 0),

    if
        CPUDeviation > 0.2 orelse MemoryDeviation > 0.2 ->
            %% Trigger alert
            alert_resource_deviation(Deviation);
        true ->
            ok
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Priority to integer conversion
priority_to_integer(immediate) -> 1;
priority_to_integer(short_term) -> 2;
priority_to_integer(long_term) -> 3.

%% Get environment variable
get_env(Key, Default) ->
    case application:get_env(erlmcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

%% Log info
log_info(Format, Args) ->
    erlmcp_logger:info(Format, Args).

%% Log error
log_error(Format, Args) ->
    erlmcp_logger:error(Format, Args).

%% Store metrics
store_metrics(Metrics) ->
    %% Store in time series database
    erlmcp_metrics:store_time_series(Metrics).

%% Load historical data
load_historical_data() ->
    %% Query time series database
    HistoricalMetrics = erlmcp_metrics:query_time_series(
        os:system_time(millisecond) - 864000000,  % 10 days
        os:system_time(millisecond)
    ),

    HistoricalMetrics.

%% Calculate server cost
calculate_server_cost(Config) ->
    %% Calculate based on max_concurrent_requests
    MaxConcurrent = maps:get(max_concurrent_requests, Config, 1000),
    CostPerRequest = 0.001,  $0.001 per request
    MaxConcurrent * CostPerRequest.

%% Calculate transport cost
calculate_transport_cost(Config) ->
    %% Calculate based on max_connections
    MaxConnections = maps:get(max_connections, Config, 1000),
    CostPerConnection = 0.0005,  $0.0005 per connection
    MaxConnections * CostPerConnection.

%% Calculate storage cost
calculate_storage_cost(Config) ->
    %% Calculate based on retention_days
    RetentionDays = maps:get(retention_days, Config, 30),
    CostPerDay = 10,  $10 per day
    RetentionDays * CostPerDay.

%% Calculate performance cost
calculate_performance_cost(Config) ->
    %% Calculate based on process_limit
    ProcessLimit = maps:get(process_limit, Config, 65536),
    CostPerProcess = 0.0001,  $0.0001 per process
    ProcessLimit * CostPerProcess.

%% Calculate monitoring cost
calculate_monitoring_cost(Config) ->
    %% Calculate based on metrics_retention_days
    RetentionDays = maps:get(metrics_retention_days, Config, 30),
    CostPerDay = 5,  $5 per day
    RetentionDays * CostPerDay.

%% Build metrics query
build_metrics_query(TimeRange) ->
    %% Build time range filter
    StartTime = TimeRange#time_range.start,
    EndTime = TimeRange#time_range.end,

    Query = #{
        start_time => StartTime,
        end_time => EndTime,
        metrics => [
            cpu_usage, memory_usage, disk_usage,
            network_in, network_out, active_connections,
            request_rate, response_time
        ]
    },

    Query.

%% Execute metrics query
execute_metrics_query(Query) ->
    %% Query metrics database
    Results = erlmcp_metrics:query(Query),

    %% Convert to record format
    lists:map(fun metric_to_record/1, Results).

%% Convert metric to record
metric_to_record(Metric) ->
    #resource_metrics{
        timestamp = maps:get(timestamp, Metric),
        component = maps:get(component, Metric),
        cpu_usage = maps:get(cpu_usage, Metric),
        memory_usage = maps:get(memory_usage, Metric),
        disk_usage = maps:get(disk_usage, Metric),
        network_in = maps:get(network_in, Metric),
        network_out = maps:get(network_out, Metric),
        active_connections = maps:get(active_connections, Metric),
        request_rate = maps:get(request_rate, Metric),
        response_time = maps:get(response_time, Metric)
    }.

%% Filter metrics
filter_metrics(Metrics) ->
    %% Filter out outliers
    lists:filter(fun filter_metric/1, Metrics).

filter_metric(Metric) ->
    CPU = Metric#resource_metrics.cpu_usage,
    Memory = Metric#resource_metrics.memory_usage,

    %% Remove metrics with 0% CPU or memory (likely errors)
    CPU > 0 andalso Memory > 0.

%% Aggregate metrics
aggregate_metrics(Metrics) ->
    %% Group by component and time window
    Groups = group_metrics_by_time_window(Metrics, 3600000),  % 1 hour windows

    %% Calculate averages for each window
    lists:map(fun calculate_window_average/1, Groups).

%% Group metrics by time window
group_metrics_by_time_window(Metrics, WindowSize) ->
    %% Create time windows
    Windows = create_time_windows(Metrics, WindowSize),

    %% Group metrics into windows
    lists:map(fun group_metrics_into_window/1, Windows).

%% Create time windows
create_time_windows(Metrics, WindowSize) ->
    %% Find time range
    Times = [M#resource_metrics.timestamp || M <- Metrics],
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),

    %% Create windows
    Windows = lists:foldl(fun create_window/2, [], lists:seq(MinTime, MaxTime, WindowSize)),

    Windows.

%% Create window
create_window(Time, Acc) ->
    Window = #{
        start_time => Time,
        end_time => Time + WindowSize,
        metrics => []
    },
    [Window | Acc].

%% Group metrics into window
group_metrics_into_window(Window, Metrics) ->
    StartTime = Window#window.start_time,
    EndTime = Window#window.end_time,

    WindowMetrics = lists:filter(fun(M) ->
        T = M#resource_metrics.timestamp,
        T >= StartTime andalso T =< EndTime
    end, Metrics),

    Window#window{metrics = WindowMetrics}.

%% Calculate window average
calculate_window_average(Window) ->
    Metrics = Window#window.metrics,
    Count = length(Metrics),

    if
        Count > 0 ->
            AvgCPU = lists:sum([M#resource_metrics.cpu_usage || M <- Metrics]) / Count,
            AvgMemory = lists:sum([M#resource_metrics.memory_usage || M <- Metrics]) / Count,
            AvgNetworkIn = lists:sum([M#resource_metrics.network_in || M <- Metrics]) / Count,
            AvgNetworkOut = lists:sum([M#resource_metrics.network_out || M <- Metrics]) / Count,

            Window#window{
                metrics = [#resource_metrics{
                    timestamp = (Window#window.start_time + Window#window.end_time) div 2,
                    cpu_usage = AvgCPU,
                    memory_usage = AvgMemory,
                    network_in = AvgNetworkIn,
                    network_out = AvgNetworkOut
                }]
            };
        true ->
            Window
    end.

%% Analyze request patterns
analyze_request_patterns(Metrics) ->
    %% Extract request rates
    RequestRates = [M#resource_metrics.request_rate || M <- Metrics],

    %% Calculate statistics
    Mean = lists:sum(RequestRates) / length(RequestRates),
    StdDev = calculate_standard_deviation(RequestRates),

    %% Determine pattern type
    CoefficientOfVariation = StdDev / Mean,

    PatternType = case CoefficientOfVariation of
        CV when CV < 0.1 -> constant;
        CV when CV < 0.3 -> periodic;
        _ -> bursty
    end,

    #patterns{
        type = PatternType,
        mean = Mean,
        std_dev = StdDev,
        coefficient_of_variation = CoefficientOfVariation
    }.

%% Analyze resource patterns
analyze_resource_patterns(Metrics) ->
    %% Extract resource usage
    CPUUsage = [M#resource_metrics.cpu_usage || M <- Metrics],
    MemoryUsage = [M#resource_metrics.memory_usage || M <- Metrics],

    %% Calculate resource utilization
    MaxCPU = lists:max(CPUUsage),
    MaxMemory = lists:max(MemoryUsage),
    AvgCPU = lists:sum(CPUUsage) / length(CPUUsage),
    AvgMemory = lists:sum(MemoryUsage) / length(MemoryUsage),

    #resource_patterns{
        max_cpu = MaxCPU,
        max_memory = MaxMemory,
        avg_cpu = AvgCPU,
        avg_memory = AvgMemory,
        utilization_peak_ratio = max(MaxCPU, MaxMemory) / max(AvgCPU, AvgMemory)
    }.

%% Determine workload type
determine_workload_type(RequestPatterns, ResourcePatterns) ->
    RequestType = RequestPatterns#patterns.type,
    ResourcePeak = ResourcePatterns#resource_patterns.utilization_peak_ratio,

    case {RequestType, ResourcePeak} of
        {constant, Ratio} when Ratio < 1.2 -> development;
        {periodic, Ratio} when Ratio < 1.5 -> staging;
        {_, Ratio} when Ratio < 2.0 -> production;
        {_, _} -> burst
    end.

%% Calculate peak requests
calculate_peak_requests(RequestPatterns) ->
    Mean = RequestPatterns#patterns.mean,
    StdDev = RequestPatterns#patterns.std_dev,

    %% Use 95th percentile
    Peak = Mean + 1.645 * StdDev,

    trunc(Peak).

%% Calculate average requests
calculate_average_requests(RequestPatterns) ->
    trunc(RequestPatterns#patterns.mean).

%% Determine storage needs
determine_storage_needs(ResourcePatterns) ->
    MaxMemory = ResourcePatterns#resource_patterns.max_memory,
    MaxCPU = ResourcePatterns#resource_patterns.max_cpu,

    case {MaxMemory, MaxCPU} of
        {Mem, CPU} when Mem < 50 andalso CPU < 50 -> low;
        {Mem, CPU} when Mem < 80 andalso CPU < 80 -> medium;
        _ -> high
    end.

%% Determine observability level
determine_observability_level(Metrics) ->
    %% Check for observability in metrics
    HasMetrics = lists:any(fun(M) ->
        M#resource_metrics.request_rate > 0
    end, Metrics),

    HasNetwork = lists:any(fun(M) ->
        M#resource_metrics.network_in > 0 orelse M#resource_metrics.network_out > 0
    end, Metrics),

    if
        HasMetrics andalso HasNetwork -> comprehensive;
        HasMetrics -> standard;
        true -> minimal
    end.

%% Calculate risk level
calculate_risk_level(Component, CurrentConfig, RecommendedConfig) ->
    %% Compare configuration values
    case Component of
        server ->
            CurrentMax = maps:get(max_concurrent_requests, CurrentConfig, 1000),
            RecommendedMax = maps:get(max_concurrent_requests, RecommendedConfig, 1000),

            if
                RecommendedMax < CurrentMax * 0.5 -> high;
                RecommendedMax < CurrentMax * 0.8 -> medium;
                true -> low
            end;
        transport ->
            CurrentMax = maps:get(max_connections, CurrentConfig, 1000),
            RecommendedMax = maps:get(max_connections, RecommendedConfig, 1000),

            if
                RecommendedMax < CurrentMax * 0.5 -> high;
                RecommendedMax < CurrentMax * 0.8 -> medium;
                true -> low
            end;
        _ ->
            low
    end.

%% Determine priority
determine_priority(Component, Savings) ->
    %% Calculate priority based on savings and component
    BasePriority = case Component of
        server -> immediate;
        transport -> immediate;
        storage -> short_term;
        performance -> short_term;
        monitoring -> long_term
    end,

    %% Adjust based on savings
    if
        Savings > 1000 -> BasePriority;
        Savings > 500 -> case BasePriority of
            immediate -> immediate;
            _ -> short_term
        end;
        true -> long_term
    end.

%% Sort recommendations by priority
sort_recommendations_by_priority(Recommendations) ->
    lists:sort(fun(R1, R2) ->
        Priority1 = priority_to_integer(R1#right_sizing_recommendation.implementation_priority),
        Priority2 = priority_to_integer(R2#right_sizing_recommendation.implementation_priority),
        Priority1 =< Priority2
    end, Recommendations).

%% Calculate standard deviation
calculate_standard_deviation(List) ->
    Mean = lists:sum(List) / length(List),
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- List]) / length(List),
    math:sqrt(Variance).

%% Calculate expected CPU
calculate_expected_cpu(WorkloadProfile) ->
    case WorkloadProfile#workload_profile.type of
        development -> 30;
        staging -> 50;
        production -> 70;
        burst -> 90
    end.

%% Calculate expected memory
calculate_expected_memory(WorkloadProfile) ->
    case WorkloadProfile#workload_profile.type of
        development -> 40;
        staging -> 60;
        production -> 80;
        burst -> 95
    end.

%% Calculate expected network
calculate_expected_network(WorkloadProfile) ->
    case WorkloadProfile#workload_profile.type of
        development -> 10;
        staging -> 30;
        production -> 60;
        burst -> 80
    end.

%% Check configuration conflicts
check_config_conflicts(Config) ->
    %% Check for conflicts between different components
    case Config of
        #{server := #{max_concurrent_requests := MaxReq},
          transport := #{max_connections := MaxConn}} ->
            if
                MaxConn < MaxReq -> conflict;
                true -> no_conflict
            end;
        _ ->
            no_conflict
    end.

%% Check if configuration is within valid range
is_valid_config_range(Config) ->
    %% Check for valid ranges
    case Config of
        #{max_concurrent_requests := MaxReq} ->
            MaxReq > 0 andalso MaxReq =< 100000;
        #{max_connections := MaxConn} ->
            MaxConn > 0 andalso MaxConn =< 100000;
        #{process_limit := ProcLimit} ->
            ProcLimit > 0 andalso ProcLimit =< 1048576;
        _ ->
            true
    end.

%% Apply server configuration
apply_server_config(Config) ->
    %% Update server configuration
    application:set_env(erlmcp, server_defaults, Config).

%% Apply transport configuration
apply_transport_config(Config) ->
    %% Update transport configuration
    application:set_env(erlmcp, transport_defaults, Config).

%% Apply storage configuration
apply_storage_config(Config) ->
    %% Update storage configuration
    application:set_env(erlmcp, storage, Config).

%% Apply performance configuration
apply_performance_config(Config) ->
    %% Update performance configuration
    application:set_env(erlmcp, performance, Config).

%% Apply monitoring configuration
apply_monitoring_config(Config) ->
    %% Update monitoring configuration
    application:set_env(erlmcp, monitoring, Config).

%% Alert resource deviation
alert_resource_deviation(Deviation) ->
    %% Send alert through notification system
    Alert = #{
        type => resource_deviation,
        severity => warning,
        deviation => Deviation,
        timestamp => os:system_time(millisecond)
    },

    erlmcp_alert_manager:send_alert(Alert).

%% Store application record
store_application_record(Record) ->
    %% Store in application history
    erlmcp_application_history:store(Record).