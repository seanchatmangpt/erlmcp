%% @doc Scalability validation module for erlmcp v3
%% Implements comprehensive scale validation including:
%% - Horizontal scaling validation
%% - Sharding validation
%% - Load balancing validation
%% - Performance validation
%% - Capacity planning validation
%% - Multi-region validation
%% - Cost optimization validation

-module(erlmcp_scalability_validator).
-behaviour(gen_server).
-export([start_link/0, validate_scaling/1, run_benchmark/2, get_scale_report/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("erlmcp_core/include/erlmcp.hrl").

%% Records
-record(scale_metrics, {
    connections :: non_neg_integer(),
    rps :: non_neg_integer(),
    latency_ms :: non_neg_integer(),
    cpu_percent :: non_neg_integer(),
    memory_percent :: non_neg_integer(),
    error_rate :: float(),
    timestamp :: integer()
}).

-record(scale_validation_result, {
    test_id :: binary(),
    test_name :: binary(),
    status :: passed | failed | warning,
    metrics :: list(#scale_metrics{}),
    violations :: list(binary()),
    recommendations :: list(binary()),
    timestamp :: integer()
}).

%% State record
-record(state, {
    validators :: list({atom(), pid()}),
    results :: list(#scale_validation_result{}),
    benchmarks :: list(map()),
    config :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

validate_scaling(Config) ->
    gen_server:call(?MODULE, {validate_scaling, Config}).

run_benchmark(BenchmarkType, Options) ->
    gen_server:call(?MODULE, {run_benchmark, BenchmarkType, Options}).

get_scale_report() ->
    gen_server:call(?MODULE, get_scale_report).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize scalability validators
    Validators = [
        {horizontal, erlmcp_horizontal_scalability_validator},
        {sharding, erlmcp_sharding_validator},
        {load_balancing, erlmcp_load_balancing_validator},
        {performance, erlmcp_performance_validator},
        {capacity, erlmcp_capacity_validator},
        {multi_region, erlmcp_multi_region_validator},
        {cost_optimization, erlmcp_cost_optimizer}
    ],

    %% Start validator processes
    ValidatorPids = lists:map(fun({Name, Module}) ->
        Pid = spawn_link(fun() -> validator_loop(Module) end),
        {Name, Pid}
    end, Validators),

    %% State initialization
    State = #state{
        validators = ValidatorPids,
        results = [],
        benchmarks = [],
        config = load_config()
    },

    %% Schedule periodic validation
    schedule_validation(),

    {ok, State}.

handle_call({validate_scaling, Config}, _From, State) ->
    %% Run all scalability validations
    Results = run_validations(Config, State#state.validators),
    NewState = State#state{results = Results},

    %% Generate report
    Report = generate_scale_report(Results),

    {reply, Report, NewState};

handle_call({run_benchmark, BenchmarkType, Options}, _From, State) ->
    %% Run specific benchmark
    Result = run_benchmark(BenchmarkType, Options),
    NewState = State#state{benchmarks = [Result | State#state.benchmarks]},

    {reply, Result, NewState};

handle_call(get_scale_report, _From, State) ->
    Report = generate_scale_report(State#state.results),
    {reply, Report, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(validate, State) ->
    %% Periodic validation
    Results = run_validations(State#state.config, State#state.validators),
    NewState = State#state{results = Results},

    %% Check for critical issues
    check_critical_issues(Results),

    {noreply, NewState}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Load configuration
load_config() ->
    case file:read_config("scalability.config") of
        {ok, Config} -> Config;
        {error, _} -> default_config()
    end.

default_config() ->
    #{
        scale_targets => #{
            connections => 100000,
            rps => 1000000,
            latency_p99 => 100
        },
        validation_intervals => #{
            horizontal => 60000,
            sharding => 300000,
            load_balancing => 60000,
            performance => 300000,
            capacity => 3600000,
            multi_region => 600000,
            cost => 3600000
        },
        benchmarks => #{
            steady_state => #{
                duration => 3600000,
                rps => 1000000,
                connections => 100000
            },
            burst => #{
                duration => 600000,
                max_rps => 10000000,
                rampup => 60000
            },
            failover => #{
                duration => 1800000,
                rps => 500000,
                failure_injection => true
            }
        }
    }.

%% Run all validations
run_validations(Config, Validators) ->
    lists:map(fun({Name, Pid}) ->
        gen_server:call(Pid, {validate, Config}, 30000)
    end, Validators).

%% Run benchmark
run_benchmark(Type, Options) ->
    case Type of
        steady_state ->
            run_steady_state_benchmark(Options);
        burst ->
            run_burst_benchmark(Options);
        failover ->
            run_failover_benchmark(Options);
        _ ->
            {error, unknown_benchmark_type}
    end.

%% Steady state benchmark
run_steady_state_benchmark(Options) ->
    #{
        duration := Duration,
        rps := RPS,
        connections := Connections
    } = maps:merge(get_benchmark_config(steady_state), Options),

    %% Setup test environment
    setup_test_environment(),

    %% Generate load
    Start = erlang:monotonic_time(millisecond),
    generate_load(Duration, Connections, RPS),

    %% Collect metrics
    Metrics = collect_performance_metrics(),

    %% Generate report
    #{
        type => steady_state,
        start_time => Start,
        duration => Duration,
        rps => RPS,
        connections => Connections,
        metrics => Metrics,
        timestamp => erlang:system_time(millisecond)
    }.

%% Burst benchmark
run_burst_benchmark(Options) ->
    #{
        duration := Duration,
        max_rps := MaxRPS,
        rampup := Rampup
    } = maps:merge(get_benchmark_config(burst), Options),

    %% Setup test environment
    setup_test_environment(),

    %% Ramped load generation
    Start = erlang:monotonic_time(millisecond),
    generate_ramped_load(Duration, MaxRPS, Rampup),

    %% Collect metrics
    Metrics = collect_performance_metrics(),

    %% Generate report
    #{
        type => burst,
        start_time => Start,
        duration => Duration,
        max_rps => MaxRPS,
        rampup => Rampup,
        metrics => Metrics,
        timestamp => erlang:system_time(millisecond)
    }.

%% Failover benchmark
run_failover_benchmark(Options) ->
    #{
        duration := Duration,
        rps := RPS,
        connections := Connections
    } = maps:merge(get_benchmark_config(failover), Options),

    %% Setup test environment
    setup_test_environment(),

    %% Start normal load
    generate_load(Duration, Connections, RPS),

    %% Inject failure
    inject_failover_failure(),

    %% Continue load after failure
    generate_load(Duration, Connections, RPS),

    %% Collect metrics
    Metrics = collect_performance_metrics(),

    %% Analyze failover performance
    FailoverAnalysis = analyze_failover(Metrics),

    %% Generate report
    #{
        type => failover,
        start_time => erlang:monotonic_time(millisecond),
        duration => Duration,
        rps => RPS,
        connections => Connections,
        metrics => Metrics,
        failover_analysis => FailoverAnalysis,
        timestamp => erlang:system_time(millisecond)
    }.

%% Generate scale report
generate_scale_report(Results) ->
    #{
        generated_at => erlang:system_time(millisecond),
        summary => generate_summary(Results),
        validations => Results,
        recommendations => generate_recommendations(Results),
        capacity_planning => generate_capacity_report(),
        cost_analysis => generate_cost_analysis(),
        performance_benchmarks => get_recent_benchmarks()
    }.

%% Generate summary
generate_summary(Results) ->
    Passed = lists:filter(fun(R) -> R#scale_validation_result.status =:= passed end, Results),
    Failed = lists:filter(fun(R) -> R#scale_validation_result.status =:= failed end, Results),
    Warnings = lists:filter(fun(R) -> R#scale_validation_result.status =:= warning end, Results),

    #{
        total => length(Results),
        passed => length(Passed),
        failed => length(Failed),
        warnings => length(Warnings),
        score => calculate_score(Results)
    }.

%% Calculate score
calculate_score(Results) ->
    Total = length(Results),
    if
        Total =:= 0 -> 0;
        true ->
            Passed = length(lists:filter(fun(R) -> R#scale_validation_result.status =:= passed end, Results)),
            (Passed / Total) * 100
    end.

%% Check critical issues
check_critical_issues(Results) ->
    Failed = lists:filter(fun(R) -> R#scale_validation_result.status =:= failed end, Results),
    case Failed of
        [] -> ok;
        _ ->
            %% Send alert for critical failures
            alert_team(critical_scaling_failure, Failed)
    end.

%% Alert team
alert_team(Type, Data) ->
    %% Integration with alerting system
    %% Could be PagerDuty, Slack, Email, etc.
    erlmcp_alerting:send_alert(Type, Data).

%% Schedule validation
schedule_validation() ->
    Schedule = get_validation_schedule(),
    timer:send_interval(Schedule, ?MODULE, validate).

%% Get validation schedule
get_validation_schedule() ->
    case application:get_env(erlmcp, validation_interval) of
        {ok, Interval} -> Interval;
        undefined -> 300000  % 5 minutes
    end.

%% Validator loop
validator_loop(Module) ->
    receive
        {validate, Config} ->
            Result = Module:validate(Config),
            validator_loop(Module);
        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            validator_loop(Module)
    after 60000 ->
        validator_loop(Module)
    end.

%% Benchmark configuration
get_benchmark_config(Type) ->
    case application:get_env(erlmcp, benchmarks) of
        {ok, Benchmarks} ->
            maps:get(Type, Benchmarks, #{});
        undefined ->
            #{
                steady_state => #{
                    duration => 3600000,
                    rps => 1000000,
                    connections => 100000
                },
                burst => #{
                    duration => 600000,
                    max_rps => 10000000,
                    rampup => 60000
                },
                failover => #{
                    duration => 1800000,
                    rps => 500000,
                    connections => 50000
                }
            }
    end.

%% Setup test environment
setup_test_environment() ->
    %% Setup test nodes
    setup_test_nodes(),

    %% Setup test clients
    setup_test_clients(),

    %% Setup monitoring
    setup_test_monitoring().

%% Setup test nodes
setup_test_nodes() ->
    %% Create temporary test nodes
    Nodes = start_test_nodes(),
    erlmcp_test_utils:set_nodes(Nodes).

%% Setup test clients
setup_test_clients() ->
    %% Create concurrent test clients
    Clients = create_test_clients(),
    erlmcp_test_utils:set_clients(Clients).

%% Setup test monitoring
setup_test_monitoring() ->
    %% Start monitoring for test
    erlmcp_monitoring:start_test_session().

%% Generate load
generate_load(Duration, Connections, RPS) ->
    %% Create concurrent users
    Users = create_concurrent_users(Connections),

    %% Start load generation
    Start = erlang:monotonic_time(millisecond),
    End = Start + Duration,

    Users ! {start_load, RPS, End},

    %% Monitor progress
    monitor_load_progress(Users, End).

%% Generate ramped load
generate_ramped_load(Duration, MaxRPS, Rampup) ->
    CalculateRPS = fun(CurrentTime, StartTime) ->
        Elapsed = CurrentTime - StartTime,
        if
            Elapsed =< Rampup ->
                (Elapsed / Rampup) * MaxRPS;
            true ->
                MaxRPS
        end
    end,

    Users = create_concurrent_users(100000),
    Start = erlang:monotonic_time(millisecond),
    End = Start + Duration,

    Users ! {start_ramped_load, fun() -> CalculateRPS(erlang:monotonic_time(millisecond), Start) end, End},

    monitor_load_progress(Users, End).

%% Collect performance metrics
collect_performance_metrics() ->
    %% Collect system metrics
    SystemMetrics = collect_system_metrics(),

    %% Collect application metrics
    AppMetrics = collect_app_metrics(),

    %% Collect network metrics
    NetworkMetrics = collect_network_metrics(),

    %% Combine metrics
    SystemMetrics ++ AppMetrics ++ NetworkMetrics.

%% Collect system metrics
collect_system_metrics() ->
    %% CPU usage
    Cpu = cpu_usage(),

    %% Memory usage
    Memory = memory_usage(),

    %% Disk usage
    Disk = disk_usage(),

    %% Network I/O
    NetworkIO = network_io(),

    [
        #scale_metrics{
            connections = 0,
            rps = 0,
            latency_ms = 0,
            cpu_percent = Cpu,
            memory_percent = Memory,
            error_rate = 0.0,
            timestamp = erlang:system_time(millisecond)
        }
    ].

%% Collect application metrics
collect_app_metrics() ->
    %% Get application metrics
    Metrics = erlmcp_metrics:collect_all(),

    %% Convert to scale_metrics
    lists:map(fun(M) ->
        #scale_metrics{
            connections = maps:get(connections, M, 0),
            rps = maps:get(rps, M, 0),
            latency_ms = maps:get(latency_ms, M, 0),
            cpu_percent = maps:get(cpu_percent, M, 0),
            memory_percent = maps:get(memory_percent, M, 0),
            error_rate = maps:get(error_rate, M, 0.0),
            timestamp = erlang:system_time(millisecond)
        }
    end, Metrics).

%% Collect network metrics
collect_network_metrics() ->
    %% Network throughput
    Throughput = network_throughput(),

    %% Network latency
    Latency = network_latency(),

    [
        #scale_metrics{
            connections = Throughput div 1024, % Convert to KB
            rps = Throughput div 1024, % Estimate
            latency_ms = Latency,
            cpu_percent = 0,
            memory_percent = 0,
            error_rate = 0.0,
            timestamp = erlang:monotonic_time(millisecond)
        }
    ].

%% CPU usage
cpu_usage() ->
    case cpu_sup:util() of
        {_, Percent, _, _} -> Percent;
        _ -> 0
    end.

%% Memory usage
memory_usage() ->
    case memory_info() of
        {_, Total, Used} -> (Used / Total) * 100;
        _ -> 0
    end.

%% Disk usage
disk_usage() ->
    case disk_info() of
        {_, Total, Used} -> (Used / Total) * 100;
        _ -> 0
    end.

%% Network I/O
network_io() ->
    case net:getifaddrs() of
        {ok, IfAddrs} ->
            lists:foldl(fun({_, Stats}, Acc) ->
                Acc + (Stats#interface.in + Stats#interface.out)
            end, 0, IfAddrs);
        _ -> 0
    end.

%% Network throughput
network_throughput() ->
    %% Measure network throughput
    {ok, Stats} = net:getifstats(),
    lists:foldl(fun({_, Interface}, Acc) ->
        Acc + (Interface#interface.bytes_recv + Interface#interface.bytes_sent)
    end, 0, Stats).

%% Network latency
network_latency() ->
    %% Measure network latency
    case erlmcp_network:ping_test() of
        {ok, Latency} -> Latency;
        _ -> 0
    end.

%% Inject failover failure
inject_failover_failure() ->
    %% Stop a node
    Node = select_random_node(),
    stop_node(Node),

    %% Wait for failover
    timer:sleep(5000),

    %% Restart node
    start_node(Node).

%% Select random node
select_random_node() ->
    Nodes = erlmcp_registry:get_nodes(),
    case Nodes of
        [] -> error(no_nodes);
        _ ->
            lists:nth(rand:uniform(length(Nodes)), Nodes)
    end.

%% Stop node
stop_node(Node) ->
    rpc:call(Node, erlmcp_node, stop, []).

%% Start node
start_node(Node) ->
    rpc:call(Node, erlmcp_node, start, []).

%% Analyze failover
analyze_failover(Metrics) ->
    %% Analyze metrics before and after failure
    BeforeFailure = lists:takewhile(fun(M) -> M#scale_metrics.timestamp < erlang:monotonic_time(millisecond) - 5000 end, Metrics),
    AfterFailure = lists:dropwhile(fun(M) -> M#scale_metrics.timestamp < erlang:monotonic_time(millisecond) - 5000 end, Metrics),

    %% Calculate metrics
    BeforeLatency = calculate_p99_latency(BeforeFailure),
    AfterLatency = calculate_p99_latency(AfterFailure),

    BeforeThroughput = calculate_throughput(BeforeFailure),
    AfterThroughput = calculate_throughput(AfterFailure),

    #{
        failover_time => 5000,
        latency_increase => AfterLatency - BeforeLatency,
        throughput_decrease => BeforeThroughput - AfterThroughput,
        recovery_time => calculate_recovery_time(Metrics),
        error_spike => calculate_error_spike(Metrics)
    }.

%% Calculate P99 latency
calculate_p99_latency(Metrics) ->
    Latencies = [M#scale_metrics.latency_ms || M <- Metrics, M#scale_metrics.latency_ms > 0],
    case Latencies of
        [] -> 0;
        _ ->
            Sorted = lists:sort(Latencies),
            Index = trunc(length(Sorted) * 0.99),
            lists:nth(Index + 1, Sorted)
    end.

%% Calculate throughput
calculate_throughput(Metrics) ->
    Throughputs = [M#scale_metrics.rps || M <- Metrics],
    case Throughputs of
        [] -> 0;
        _ ->
            lists:sum(Throughputs) / length(Throughputs)
    end.

%% Calculate recovery time
calculate_recovery_time(Metrics) ->
    %% Find when metrics return to normal
    NormalState = calculate_normal_state(Metrics),
    RecoveryPoint = find_recovery_point(Metrics, NormalState),
    case RecoveryPoint of
        undefined -> 0;
        _ -> RecoveryPoint - erlang:monotonic_time(millisecond) + 5000
    end.

%% Calculate normal state
calculate_normal_state(Metrics) ->
    %% Baseline before failure
    lists:last(Metrics).

%% Find recovery point
find_recovery_point(Metrics, NormalState) ->
    lists:foldl(fun(M, Acc) ->
        case is_recovered(M, NormalState) of
            true -> M#scale_metrics.timestamp;
            false -> Acc
        end
    end, undefined, Metrics).

%% Check if recovered
is_recovered(Current, Normal) ->
    abs(Current#scale_metrics.latency_ms - Normal#scale_metrics.latency_ms) < 10 andalso
    abs(Current#scale_metrics.rps - Normal#scale_metrics.rps) < 1000.

%% Calculate error spike
calculate_error_spike(Metrics) ->
    MaxError = lists:max([M#scale_metrics.error_rate || M <- Metrics]),
    MinError = lists:min([M#scale_metrics.error_rate || M <- Metrics]),
    MaxError - MinError.

%% Generate recommendations
generate_recommendations(Results) ->
    lists:foldl(fun(Result, Acc) ->
        case Result#scale_validation_result.status of
            passed -> Acc;
            failed ->
                [Result#scale_validation_result.recommendations | Acc];
            warning ->
                [Result#scale_validation_result.recommendations | Acc]
        end
    end, [], Results).

%% Generate capacity report
generate_capacity_report() ->
    %% Analyze current capacity
    CurrentCapacity = analyze_current_capacity(),

    %% Project future capacity
    FutureCapacity = project_future_capacity(CurrentCapacity),

    #{
        current => CurrentCapacity,
        projected => FutureCapacity,
        recommendations => generate_capacity_recommendations(CurrentCapacity, FutureCapacity)
    }.

%% Analyze current capacity
analyze_current_capacity() ->
    %% Get current metrics
    Metrics = erlmcp_metrics:collect_all(),

    #{
        connections => lists:sum([M#scale_metrics.connections || M <- Metrics]),
        rps => lists:sum([M#scale_metrics.rps || M <- Metrics]),
        latency_p99 => calculate_p99_latency(Metrics),
        cpu_utilization => lists:sum([M#scale_metrics.cpu_percent || M <- Metrics]) / length(Metrics),
        memory_utilization => lists:sum([M#scale_metrics.memory_percent || M <- Metrics]) / length(Metrics),
        nodes => length(Metrics)
    }.

%% Project future capacity
project_future_capacity(Current) ->
    %% Simple projection based on growth rate
    GrowthRate = 1.2, % 20% growth

    #{
        connections => round(Current#connections * GrowthRate),
        rps => round(Current#rps * GrowthRate),
        latency_p99 => round(Current#latency_p99 * GrowthRate),
        cpu_utilization => min(100, round(Current#cpu_utilization * GrowthRate)),
        memory_utilization => min(100, round(Current#memory_utilization * GrowthRate)),
        nodes => round(Current#nodes * GrowthRate)
    }.

%% Generate capacity recommendations
generate_capacity_recommendations(Current, Future) ->
    Recommendations = [],

    %% Check CPU
    if
        Future#cpu_utilization > 80 ->
            [recommend_scale_up(Current#nodes * 2) | Recommendations];
        true ->
            Recommendations
    end,

    %% Check memory
    if
        Future#memory_utilization > 80 ->
            [recommend_optimize_memory() | Recommendations];
        true ->
            Recommendations
    end,

    %% Check latency
    if
        Future#latency_p99 > 100 ->
            [recommend_network_optimization() | Recommendations];
        true ->
            Recommendations
    end.

    %% Return recommendations
    Recommendations.

%% Recommend scale up
recommend_scale_up(Nodes) ->
    #{
        action => scale_up,
        nodes => Nodes,
        reason => "High CPU utilization projected"
    }.

%% Recommend optimize memory
recommend_optimize_memory() ->
    #{
        action => optimize_memory,
        reason => "High memory utilization projected"
    }.

%% Recommend network optimization
recommend_network_optimization() ->
    #{
        action => optimize_network,
        reason => "High latency projected"
    }.

%% Generate cost analysis
generate_cost_analysis() ->
    %% Calculate current costs
    CurrentCosts = calculate_current_costs(),

    %% Project future costs
    FutureCosts = project_future_costs(),

    %% Identify optimization opportunities
    Opportunities = identify_cost_opportunities(CurrentCosts),

    #{
        current => CurrentCosts,
        projected => FutureCosts,
        opportunities => Opportunities,
        savings_potential => calculate_savings_potential(Opportunities)
    }.

%% Calculate current costs
calculate_current_costs() ->
    %% Calculate costs based on resources
    Nodes = erlmcp_registry:get_nodes(),
    CostPerNode = calculate_cost_per_node(),

    #{
        compute => length(Nodes) * CostPerNode,
        storage => calculate_storage_costs(),
        network => calculate_network_costs(),
        total => calculate_total_costs()
    }.

%% Calculate cost per node
calculate_cost_per_node() ->
    %% Based on instance type and region
    case application:get_env(erlmcp, instance_type) of
        {ok, large} -> 2.0; % $2/hour
        {ok, medium} -> 1.0; % $1/hour
        {ok, small} -> 0.5; % $0.5/hour
        _ -> 1.0
    end.

%% Calculate storage costs
calculate_storage_costs() ->
    %% Based on storage usage
    StorageGB = calculate_storage_usage(),
    StorageGB * 0.10. % $0.10/GB/month

%% Calculate network costs
calculate_network_costs() ->
    %% Based on data transfer
    DataTransferGB = calculate_data_transfer(),
    DataTransferGB * 0.09. % $0.09/GB

%% Calculate total costs
calculate_total_costs() ->
    Compute = calculate_current_costs().compute,
    Storage = calculate_storage_costs(),
    Network = calculate_network_costs(),
    Compute + Storage + Network.

%% Calculate storage usage
calculate_storage_usage() ->
    %% Get total storage usage
    Total = erlmcp_storage:get_total(),
    Total div (1024 * 1024 * 1024). % Convert to GB

%% Calculate data transfer
calculate_data_transfer() ->
    %% Get total data transfer
    Total = erlmcp_network:get_total_transfer(),
    Total div (1024 * 1024 * 1024). % Convert to GB

%% Project future costs
project_future_costs() ->
    Current = calculate_current_costs(),
    GrowthRate = 1.2,

    #{
        compute => Current#compute * GrowthRate,
        storage => Current#storage * GrowthRate,
        network => Current#network * GrowthRate,
        total => Current#total * GrowthRate
    }.

%% Identify cost optimization opportunities
identify_cost_opportunities(CurrentCosts) ->
    Opportunities = [],

    %% Check compute optimization
    if
        CurrentCosts#compute > 1000 ->
            [recommend_spot_instances() | Opportunities];
        true ->
            Opportunities
    end,

    %% Check storage optimization
    if
        CurrentCosts#storage > 500 ->
            [recommend_storage_optimization() | Opportunities];
        true ->
            Opportunities
    end,

    %% Check network optimization
    if
        CurrentCosts#network > 300 ->
            [recommend_network_optimization() | Opportunities];
        true ->
            Opportunities
    end.

    %% Return opportunities
    Opportunities.

%% Recommend spot instances
recommend_spot_instances() ->
    #{
        action => use_spot_instances,
        potential_savings => 0.3,
        risk => "possible termination"
    }.

%% Recommend storage optimization
recommend_storage_optimization() ->
    #{
        action => optimize_storage,
        potential_savings => 0.2,
        risk => "data migration required"
    }.

%% Recommend network optimization
recommend_network_optimization() ->
    #{
        action => optimize_network,
        potential_savings => 0.15,
        risk => "configuration changes required"
    }.

%% Calculate savings potential
calculate_savings_potential(Opportunities) ->
    lists:sum([O#potential_savings || O <- Opportunities]).

%% Get recent benchmarks
get_recent_benchmarks() ->
    %% Get benchmark results from database
    erlmcp_benchmark:recent_results(10).

%% Create concurrent users
create_concurrent_users(Count) ->
    %% Spawn processes for concurrent users
    Pids = lists:map(fun(_) ->
        spawn_link(fun() -> concurrent_user_loop() end)
    end, lists:seq(1, Count)),

    %% Create process group
    erlang:group_leader(self(), self()),
    {users, Pids}.

%% Concurrent user loop
concurrent_user_loop() ->
    receive
        {start_load, RPS, EndTime} ->
            concurrent_user_load(RPS, EndTime);
        {start_ramped_load, RPSFun, EndTime} ->
            concurrent_user_ramped_load(RPSFun, EndTime);
        stop ->
            ok
    end.

%% Concurrent user load
concurrent_user_load(RPS, EndTime) ->
    while(fun() -> erlang:monotonic_time(millisecond) < EndTime end,
        fun() ->
            %% Make requests at specified RPS
            make_request(),
            timer:sleep(1000 div RPS)
        end).

%% Concurrent user ramped load
concurrent_user_ramped_load(RPSFun, EndTime) ->
    while(fun() -> erlang:monotonic_time(millisecond) < EndTime end,
        fun() ->
            RPS = RPSFun(),
            make_request(),
            timer:sleep(1000 div RPS)
        end).

%% Make request
make_request() ->
    %% Make HTTP request to erlmcp
    {ok, Response} = httpc:request("http://localhost:8080/mcp", post, [], []),
    Response.

%% While loop
while(Pred, Fun) ->
    case Pred() of
        true ->
            Fun(),
            while(Pred, Fun);
        false ->
            ok
    end.

%% Monitor load progress
monitor_load_progress(Users, EndTime) ->
    while(fun() -> erlang:monotonic_time(millisecond) < EndTime end,
        fun() ->
            %% Collect progress metrics
            Progress = collect_progress_metrics(Users),
            report_progress(Progress),
            timer:sleep(1000)
        end).

%% Collect progress metrics
collect_progress_metrics(Users) ->
    %% Get metrics from users
    {users, Pids} = Users,

    %% Collect from each user
    Responses = lists:map(fun(Pid) ->
        Pid ! {get_metrics, self()},
        receive
            {metrics, Metrics} -> Metrics;
            _ -> error
        after 1000 ->
            error
        end
    end, Pids),

    %% Aggregate metrics
    lists:filtermap(fun(Response) ->
        case Response of
            error -> false;
            Metrics -> {true, Metrics}
        end
    end, Responses).

%% Report progress
report_progress(Metrics) ->
    %% Calculate aggregate metrics
    TotalRequests = lists:sum([maps:get(requests, M, 0) || M <- Metrics]),
    FailedRequests = lists:sum([maps:get(failed, M, 0) || M <- Metrics]),
    SuccessRate = if
        TotalRequests > 0 -> (TotalRequests - FailedRequests) / TotalRequests;
        true -> 1.0
    end,

    %% Report to monitoring
    erlmcp_monitoring:report_progress(#{
        total_requests => TotalRequests,
        failed_requests => FailedRequests,
        success_rate => SuccessRate,
        timestamp => erlang:system_time(millisecond)
    }.

%% Start test nodes
start_test_nodes() ->
    %% Start temporary test nodes
    Nodes = [],
    Nodes.

%% Create test clients
create_test_clients() ->
    %% Create test clients
    Clients = [],
    Clients.

%% Setup test monitoring
setup_test_monitoring() ->
    %% Setup monitoring for test
    ok.