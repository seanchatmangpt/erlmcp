%% @doc High-Throughput Benchmark Suite
%% Implements enterprise-level throughput testing capable of handling
## 10K+ requests per second with sub-second response times.
%% @copyright 2026 erlmcp
%% @version 3.0.0
-module(erlmcp_benchmark_throughtput).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    make_config/1,
    run/2,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Types
-type test_config() :: #{
    duration => pos_integer(),
    rate => pos_integer(),
    nodes => pos_integer(),
    warmup => pos_integer(),
    scenario => atom(),
    metrics => [atom()]
}.

-type throughput_result() :: #{
    timestamp => integer(),
    scenario => atom(),
    throughput => float(),
    success_rate => float(),
    average_response_time => float(),
    p95_response_time => float(),
    p99_response_time => float(),
    error_count => integer(),
    throughput_distribution => map(),
    error_analysis => map(),
    resource_utilization => map()
}.

-type request_metrics() :: #{
    request_id => binary(),
    timestamp => integer(),
    response_time => integer(),
    success => boolean(),
    error => binary() | undefined
}.

-define(TAB, erlmcp_benchmark_throughtput_metrics).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

make_config(Config) ->
    DefaultConfig = #{
        duration => 60000,    % 1 minute
        rate => 1000,        % 1000 requests/second
        nodes => 10,
        warmup => 10000,     % 10 seconds of warmup
        scenario => default_scenario,
        metrics => [throughput, response_time, errors]
    },
    maps:merge(DefaultConfig, Config).

run(Scenario, Config) ->
    gen_server:call(?SERVER, {run_benchmark, Scenario, Config}).

stop() ->
    gen_server:call(?SERVER, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize metrics collection tables
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [
                set,
                public,
                named_table,
                {keypos, #request_metrics.timestamp},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,

    %% Initialize throughput generator
    ok = initialize_throughput_generator(),

    %% Configure enterprise monitoring
    ok = configure_enterprise_monitoring(),

    State = #{
        start_time => undefined,
        end_time => undefined,
        config => undefined,
        scenario => undefined,
        metrics => [],
        error_analysis => #{},
        resource_monitor => undefined
    },

    {ok, State}.

handle_call({run_benchmark, Scenario, Config}, _From, State) ->
    %% Validate configuration
    case validate_config(Config) of
        {ok, ValidConfig} ->
            %% Prepare benchmark environment
            case prepare_benchmark_environment(ValidConfig) of
                {ok, Environment} ->
                    %% Start benchmark
                    {ok, Results} = execute_throughput_benchmark(
                        Scenario, ValidConfig, Environment, State
                    ),

                    %% Process results
                    ProcessedResults = process_results(Results, ValidConfig),

                    %% Generate report
                    Report = generate_throughput_report(ProcessedResults),

                    {reply, {ok, Report}, update_state(State, ValidConfig, Scenario, Report)};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({request_complete, Metrics}, State) ->
    %% Store completed request metrics
    ets:insert(?TAB, Metrics),

    %% Update aggregate metrics
    UpdatedState = update_aggregate_metrics(State, Metrics),

    %% Check for completion
    case check_benchmark_completion(State) of
        true ->
            gen_server:cast(?SERVER, benchmark_complete);
        false ->
            ok
    end,

    {noreply, UpdatedState};

handle_info(benchmark_complete, State) ->
    %% Process final metrics and generate report
    FinalResults = compile_final_metrics(State),
    Report = generate_throughput_report(FinalResults),

    %% Store benchmark results
    save_benchmark_report(Report),

    %% Trigger notification
    ok = notify_benchmark_complete(Report),

    {noreply, State#{end_time => erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup resources
    case maps:get(resource_monitor, State, undefined) of
        undefined -> ok;
        RM -> ok = stop_resource_monitor(RM)
    end,

    %% Export metrics
    ok = export_metrics(),

    %% Clear metrics table
    ets:delete_all_objects(?TAB),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

validate_config(Config) ->
    Required = [duration, rate, scenario],
    Missing = [Field || Field <- Required, not maps:is_key(Field, Config)],

    case Missing of
        [] ->
            %% Check for valid rate
            case maps:get(rate, Config) of
                Rate when Rate > 0, Rate =< 1000000 -> % Max 1M req/s
                    {ok, Config};
                _ ->
                    {error, invalid_rate}
            end;
        _ ->
            {error, {missing_fields, Missing}}
    end.

initialize_throughput_generator() ->
    %% Initialize enterprise throughput generator
    ok = erlmcp_throughput_generator:start(),
    ok = erlmcp_throughput_generator:configure_enterprise_params(),
    ok.

configure_enterprise_monitoring() ->
    %% Configure enterprise monitoring for throughput testing
    ok = erlmcp_metrics_collector:start(),
    ok = erlmcp_metrics_collector:configure_throughput_metrics(),
    ok.

prepare_benchmark_environment(Config) ->
    %% Initialize test cluster
    NodeCount = maps:get(nodes, Config, 10),
    case erlmcp_cluster_manager:prepare_benchmark_cluster(NodeCount) of
        {ok, ClusterConfig} ->
            %% Configure load balancers
            ok = configure_load_balancers(Config),

            %% Configure connection pools
            ok = configure_connection_pools(Config),

            %% Initialize request generators
            ok = initialize_request_generators(Config),

            %% Setup monitoring
            ok = setup_monitoring(Config),

            {ok, ClusterConfig};
        {error, Reason} ->
            {error, Reason}
    end.

execute_throughput_benchmark(Scenario, Config, Environment, State) ->
    %% Start warmup phase
    WarmupConfig = maps:merge(Config, #{phase => warmup}),
    ok = start_warmup_phase(WarmupConfig),

    %% Main benchmark phase
    MainConfig = maps:merge(Config, #{phase => main}),
    ok = start_main_phase(MainConfig, State),

    %% Initialize metrics collection
    ok = start_metrics_collection(State),

    %% Schedule benchmark completion
    erlang:send_after(maps:get(duration, Config), self(), benchmark_complete),

    %% Start background monitoring
    MonitorRef = start_background_monitoring(),

    %% Store monitor reference
    UpdatedState = State#{resource_monitor => MonitorRef},

    {ok, UpdatedState}.

start_warmup_phase(Config) ->
    WarmupDuration = maps:get(warmup, Config),
    Rate = maps:get(rate, Config),

    %% Start warmup request generation
    ok = erlmcp_throughput_generator:start_warmup(WarmupDuration, Rate),

    %% Monitor warmup metrics
    ok = monitor_warmup_metrics(),

    ok.

start_main_phase(Config, State) ->
    Rate = maps:get(rate, Config),
    Duration = maps:get(duration, Config),

    %% Start main request generation
    ok = erlmcp_throughput_generator:start_main(Rate, Duration),

    %% Start concurrent monitoring
    ok = start_concurrent_monitoring(),

    ok.

start_metrics_collection(State) ->
    %% Start comprehensive metrics collection
    ok = erlmcp_metrics_collector:start_collection([
        request_count,
        response_times,
        errors,
        throughput,
        resource_utilization
    ]),

    ok.

start_concurrent_monitoring() ->
    %% Start multiple monitoring processes for comprehensive coverage
    Pids = [
        spawn_monitor(fun() -> monitor_cpu_utilization() end),
        spawn_monitor(fun() -> monitor_memory_usage() end),
        spawn_monitor(fun() -> monitor_network_performance() end)
    ],

    ok = store_monitor_pids(Pids),
    ok.

monitor_cpu_utilization() ->
    monitor_utilization(cpu, fun() -> get_cpu_utilization() end).

monitor_memory_usage() ->
    monitor_utilization(memory, fun() -> get_memory_usage() end).

monitor_network_performance() ->
    monitor_utilization(network, fun() -> get_network_performance() end).

monitor_utilization(Type, GetFunc) ->
    %% Monitor utilization with high frequency
    Interval = 100, % 100ms interval
    monitor_loop(Type, GetFunc, Interval, []).

monitor_loop(Type, GetFunc, Interval, History) ->
    Utilization = GetFunc(),
    Timestamp = erlang:system_time(millisecond),

    %% Store utilization measurement
    store_utilization_measurement(Type, Timestamp, Utilization),

    %% Check for alerts
    case check_utilization_alert(Type, Utilization) of
        true ->
            send_utilization_alert(Type, Utilization, Timestamp);
        false ->
            ok
    end,

    timer:sleep(Interval),
    monitor_loop(Type, GetFunc, Interval, [Utilization | History]).

start_background_monitoring() ->
    %% Start comprehensive background monitoring
    Ref = erlang:send_after(5000, self(), {check_metrics, #{}]),
    {ok, Ref}.

store_monitor_pids(Pids) ->
    %% Store monitoring process PIDs for cleanup
    ets:new(?MODULE, [set, public, named_table]),
    lists:foreach(fun(Pid) ->
        ets:insert(?MODULE, {monitor_pid, Pid})
    end, Pids),
    ok.

update_aggregate_metrics(State, Metrics) ->
    %% Update aggregate metrics based on new request metrics
    NewMetrics = case maps:get(metrics, State, undefined) of
        undefined -> [Metrics];
        Existing -> [Metrics | Existing]
    end,

    State#{metrics => NewMetrics}.

check_benchmark_completion(State) ->
    %% Check if benchmark duration has been reached
    case maps:get(end_time, State, undefined) of
        undefined ->
            case maps:get(start_time, State, undefined) of
                undefined -> false;
                StartTime ->
                    erlang:system_time(millisecond) >= StartTime + maps:get(duration, State)
            end;
        _ -> true
    end.

compile_final_metrics(State) ->
    %% Compile all metrics into final result
    Metrics = ets:tab2list(?TAB),

    Throughput = calculate_throughput(Metrics),
    ResponseTimes = extract_response_times(Metrics),
    ErrorAnalysis = analyze_errors(Metrics),
    ResourceUtilization = compile_resource_metrics(State),

    #{
        timestamp => erlang:system_time(millisecond),
        throughput => Throughput,
        response_time_distribution => calculate_percentiles(ResponseTimes),
        error_analysis => ErrorAnalysis,
        resource_utilization => ResourceUtilization,
        metrics_count => length(Metrics)
    }.

calculate_throughput(Metrics) ->
    TotalRequests = length(Metrics),
    Duration = case Metrics of
        [] -> 1;
        _ -> Last = lists:last(Metrics),
             First = lists:nth(1, Metrics),
             maps:get(timestamp, Last) - maps:get(timestamp, First)
    end,

    case Duration of
        0 -> 0;
        _ -> (TotalRequests / Duration) * 1000 % Convert to requests/second
    end.

extract_response_times(Metrics) ->
    [maps:get(response_time, M) || M <- Metrics, maps:is_key(response_time, M)].

analyze_errors(Metrics) ->
    Errors = [M || M <- Metrics, maps:get(success, M, false) =:= false],

    case Errors of
        [] -> #{error_count => 0, error_rate => 0.0, error_types => #{}};
        _ ->
            ErrorTypes = error_distribution(Errors),
            ErrorRate = length(Errors) / length(Metrics),
            #{error_count => length(Errors), error_rate => ErrorRate, error_types => ErrorTypes}
    end.

error_distribution(Errors) ->
    lists:foldl(fun(M, Acc) ->
        ErrorType = case maps:get(error, M, undefined) of
            undefined -> no_error;
            E -> E
        end,
        maps:update_counter(ErrorType, 1, Acc)
    end, #{}, Errors).

calculate_percentiles(ResponseTimes) ->
    Sorted = lists:sort(ResponseTimes),
    Length = length(Sorted),

    #{
        p50 => calculate_percentile(50, Sorted, Length),
        p95 => calculate_percentile(95, Sorted, Length),
        p99 => calculate_percentile(99, Sorted, Length),
        p99_9 => calculate_percentile(99.9, Sorted, Length)
    }.

calculate_percentile(P, Sorted, Length) ->
    Index = trunc((P / 100) * Length),
    case Sorted of
        [] -> 0;
        _ when Index > 0 -> lists:nth(Index, Sorted);
        _ -> 0
    end.

compile_resource_metrics(State) ->
    %% Compile resource utilization metrics
    ResourceMeasurements = maps:get(resource_measurements, State, #{}),

    #{
        cpu => calculate_resource_summary(maps:get(cpu, ResourceMeasurements, [])),
        memory => calculate_resource_summary(maps:get(memory, ResourceMeasurements, [])),
        network => calculate_resource_summary(maps:get(network, ResourceMeasurements, []))
    }.

calculate_resource_summary(Measurements) ->
    case Measurements of
        [] -> #{average => 0.0, max => 0.0};
        _ ->
            #{average => lists:sum(Measurements) / length(Measurements),
              max => lists:max(Measurements)}
    end.

generate_throughput_report(Results) ->
    #{
        timestamp => maps:get(timestamp, Results),
        scenario => current_benchmark_scenario,

        %% Primary throughput metrics
        throughput => maps:get(throughput, Results, 0),
        success_rate => calculate_success_rate(Results),

        %% Response time metrics
        response_times => maps:get(response_time_distribution, Results, #{}),

        %% Error analysis
        error_analysis => maps:get(error_analysis, Results, #{}),

        %% Resource utilization
        resource_utilization => maps:get(resource_utilization, Results, #{}),

        %% Performance indicators
        performance_indicators => calculate_performance_indicators(Results),

        %% Recommendations
        recommendations => generate_throughput_recommendations(Results)
    }.

calculate_success_rate(Results) ->
    ErrorRate = maps:get(error_rate, maps:get(error_analysis, Results, #{}), 0),
    1.0 - ErrorRate.

calculate_performance_indicators(Results) ->
    Throughput = maps:get(throughput, Results, 0),
    ResponseTimes = maps:get(response_times, Results, #{}),

    #{
        efficiency => calculate_efficiency(Results),
        scalability => calculate_scalability_factor(Results),
        cost_effectiveness => calculate_cost_effectiveness(Results),
        sustainability => calculate_sustainability_factor(Results)
    }.

calculate_efficiency(Results) ->
    %% Calculate operational efficiency
    Throughput = maps:get(throughput, Results, 0),
    ResourceUtilization = maps:get(resource_utilization, Results, #{}),

    case maps:is_key(cpu, ResourceUtilization) of
        false -> 0.0;
        true ->
            CPUAvg = maps:get(average, maps:get(cpu, ResourceUtilization, #{}), 0),
            case CPUAvg of
                0.0 -> 0.0;
                _ -> Throughput / (CPUAvg * 1000)  % throughput per CPU percentage
            end
    end.

calculate_scalability_factor(Results) ->
    %% Calculate scalability efficiency
    Throughput = maps:get(throughput, Results, 0),
    ThisRate = maps:get(request_rate, current_benchmark_config, 0),

    case ThisRate of
        0 -> 0.0;
        _ -> min(1.0, ThisRate / Throughput)
    end.

calculate_cost_effectiveness(Results) ->
    %% Calculate cost effectiveness (throughput per unit cost)
    Throughput = maps:get(throughput, Results, 0),
    CostPerRequest = estimate_cost_per_request(Results),

    case CostPerRequest of
        0 -> 0.0;
        _ -> Throughput / CostPerRequest
    end.

estimate_cost_per_request(Results) ->
    %% Estimate cost per request based on resource usage
    ResourceUtilization = maps:get(resource_utilization, Results, #{}),

    %% Simplified cost calculation
    CPUCost = maps:get(average, maps:get(cpu, ResourceUtilization, #{}), 0) * 0.01,
    MemoryCost = maps:get(average, maps:get(memory, ResourceUtilization, #{}), 0) * 0.005,

    CPUCost + MemoryCost.

calculate_sustainability_factor(Results) ->
    %% Calculate sustainability factor based on error rates and resource efficiency
    ErrorRate = maps:get(error_rate, maps:get(error_analysis, Results, #{}), 1.0),
    Efficiency = calculate_efficiency(Results),

    Efficiency * (1.0 - ErrorRate).

generate_throughput_recommendations(Results) ->
    %% Generate specific recommendations based on benchmark results
    Recommendations = #{},

    %% Check for throughput bottlenecks
    Throughput = maps:get(throughput, Results, 0),
    case Throughput < 10000 of
        true ->
            Recommendations#{throughput => optimize_throughput_bottlenecks};
        false ->
            Recommendations
    end,

    %% Check for latency issues
    ResponseTimes = maps:get(response_times, Results, #{}),
    case maps:get(p99, ResponseTimes, 0) > 200 of
        true ->
            Recommendations#{latency => optimize_latency};
        false ->
            Recommendations
    end,

    %% Check for resource utilization
    ResourceUtilization = maps:get(resource_utilization, Results, #{}),
    case maps:get(max, maps:get(cpu, ResourceUtilization, #{}), 0) > 0.90 of
        true ->
            Recommendations#{cpu => optimize_cpu_utilization};
        false ->
            Recommendations
    end,

    %% Check for error rates
    ErrorAnalysis = maps:get(error_analysis, Results, #{}),
    case maps:get(error_rate, ErrorAnalysis, 0) > 0.01 of
        true ->
            Recommendations#{reliability => improve_reliability};
        false ->
            Recommendations
    end,

    Recommendations.

save_benchmark_report(Report) ->
    %% Save benchmark report to persistent storage
    BenchmarkId = generate_benchmark_id(),
    ok = erlmcp_benchmark_storage:save_throughput_report(BenchmarkId, Report),
    ok.

notify_benchmark_complete(Report) ->
    %% Notify external systems of benchmark completion
    ok = erlmcp_notification_manager:notify(
        benchmark_complete,
        throughput,
        Report
    ),
    ok.

configure_load_balancers(Config) ->
    %% Configure enterprise load balancers for throughput testing
    ok = erlmcp_load_balancer:configure_throughput_mode(Config),
    ok.

configure_connection_pools(Config) ->
    %% Configure connection pools for high throughput
    PoolSize = maps:get(pool_size, Config, 100),
    MaxConcurrent = maps:get(max_concurrent, Config, 5000),

    ok = erlmcp_connection_pool:configure(
        enterprise,
        #{size => PoolSize, max_concurrent => MaxConcurrent}
    ),
    ok.

initialize_request_generators(Config) ->
    %% Initialize request generators for high throughput
    Rate = maps:get(rate, Config),
    NodeCount = maps:get(nodes, Config, 10),

    ok = erlmcp_request_generator:setup(
        Rate,
        NodeCount,
        enterprise_profile
    ),
    ok.

setup_monitoring(Config) ->
    %% Setup comprehensive monitoring
    Metrics = maps:get(metrics, Config, []),
    ok = erlmcp_monitoring:setup_metrics(Metrics),
    ok.

store_utilization_measurement(Type, Timestamp, Utilization) ->
    %% Store utilization measurement for later analysis
    ok = erlmcp_utilization_storage:store(
        Type,
        Timestamp,
        Utilization
    ),
    ok.

check_utilization_alert(Type, Utilization) ->
    %% Check if utilization exceeds alert threshold
    Thresholds = #{
        cpu => 0.95,
        memory => 0.90,
        network => 0.85
    },

    case maps:get(Type, Thresholds, 1.0) of
        Threshold when Utilization > Threshold -> true;
        _ -> false
    end.

send_utilization_alert(Type, Utilization, Timestamp) ->
    %% Send utilization alert
    Alert = #{
        type => utilization_alert,
        metric => Type,
        value => Utilization,
        threshold => maps:get(Type, #{
            cpu => 0.95,
            memory => 0.90,
            network => 0.85
        }, 1.0),
        timestamp => Timestamp
    },

    ok = erlmcp_alert_manager:send_alert(Alert),
    ok.

update_state(State, Config, Scenario, Report) ->
    State#{
        start_time => erlang:system_time(millisecond),
        config => Config,
        scenario => Scenario,
        report => Report
    }.

export_metrics() ->
    %% Export all collected metrics
    Metrics = ets:tab2list(?TAB),
    ok = erlmcp_metrics_exporter:export(Metrics),
    ok.

get_cpu_utilization() ->
    %% Get current CPU utilization
    case erlmcp_system_monitor:get_cpu() of
        {ok, Utilization} -> Utilization;
        {error, _} -> 0.0
    end.

get_memory_usage() ->
    %% Get current memory usage
    case erlmcp_system_monitor:get_memory() of
        {ok, Usage} -> Usage;
        {error, _} -> 0.0
    end.

get_network_performance() ->
    %% Get current network performance
    case erlmcp_system_monitor:get_network() of
        {ok, Performance} -> Performance;
        {error, _} -> 0.0
    end.

stop_resource_monitor(MonitorRef) ->
    %% Stop resource monitoring
    case MonitorRef of
        undefined -> ok;
        _ -> ok = erlmcp_resource_monitor:stop(MonitorRef)
    end,
    ok.

generate_benchmark_id() ->
    "tb_" ++ integer_to_list(erlang:system_time(millisecond), 36).