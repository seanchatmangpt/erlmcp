-module(erlmcp_stress_test).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

-behaviour(gen_server).

%% API exports
-export([
    start/1,
    stop/1,
    add_stress_scenario/2,
    execute_stress_scenario/2,
    get_system_limits/1,
    identify_breaking_points/1,
    monitor_system_resilience/1
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% TYPE DEFINITIONS
%%====================================================================

-type stress_scenario() :: #{
    name := binary(),
    description := binary(),
    load_profile := load_profile(),
    failure_injection := [failure_injection()],
    duration := pos_integer(),
    expected_breaking_point => pos_integer() | undefined
}.

-type load_profile() :: #{
    type := linear | exponential | sinusoidal | step | spike,
    base_load := pos_integer(),
    max_load => pos_integer(),
    duration := pos_integer(),
    characteristics := map()
}.

-type failure_injection() :: #{
    type := process_crash | network_partition | resource_exhaustion |
             data_corruption | overload_scenario,
    target := specific | cluster | global,
    probability := float(),
    severity := low | medium | high | critical,
    recovery_time => pos_integer() | undefined
}.

-type stress_config() :: #{
    scenarios := [stress_scenario()],
    system_limits => map(),
    monitoring_interval => pos_integer(),
    recovery_threshold => pos_integer(),
    abort_on_failure => boolean()
}.

-type system_monitor() :: #{
    cpu_usage => float(),
    memory_usage => float(),
    disk_usage => float(),
    network_io => map(),
    process_count => pos_integer(),
    connection_count => pos_integer(),
    response_time => pos_integer(),
    error_rate => float()
}.

-type breaking_point() :: #{
    scenario => binary(),
    load_level => pos_integer(),
    system_metrics => map(),
    failure_type => binary(),
    recovery_time => pos_integer(),
    impact => map()
}.

%%====================================================================
%% GEN_SERVER STATE
##====================================================================

-record(state, {
    config :: stress_config(),
    active_scenarios :: [stress_scenario()],
    system_metrics :: map(),
    breaking_points :: [breaking_point()],
    recovery_history :: map(),
    system_health :: map(),
    monitoring_timer :: reference() | undefined,
    scenario_timer :: reference() | undefined,
    test_start_time :: integer(),
    test_duration :: pos_integer(),
    stress_intensity :: float()
}).

%%====================================================================
%% API FUNCTIONS
##====================================================================

-spec start(stress_config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop, 30000).

-spec add_stress_scenario(pid(), stress_scenario()) -> ok.
add_stress_scenario(Pid, Scenario) ->
    gen_server:call(Pid, {add_scenario, Scenario}, 5000).

-spec execute_stress_scenario(pid(), binary()) -> ok.
execute_stress_scenario(Pid, ScenarioName) ->
    gen_server:call(Pid, {execute_scenario, ScenarioName}, 10000).

-spec get_system_limits(pid()) -> map().
get_system_limits(Pid) ->
    gen_server:call(Pid, get_system_limits, 5000).

-spec identify_breaking_points(pid()) -> [breaking_point()].
identify_breaking_points(Pid) ->
    gen_server:call(Pid, identify_breaking_points, 5000).

-spec monitor_system_resilience(pid()) -> map().
monitor_system_resilience(Pid) ->
    gen_server:call(Pid, monitor_resilience, 5000).

%%====================================================================
## GEN_SERVER CALLBACKS
##====================================================================

init(Config) ->
    ?LOG_INFO("Starting stress test with config: ~p", [Config]),

    State = #state{
        config = Config,
        active_scenarios = [],
        system_metrics = initialize_system_metrics(),
        breaking_points = [],
        recovery_history = #{},
        system_health = #{overall => 100.0, components => #{}},
        monitoring_timer = undefined,
        scenario_timer = undefined,
        test_start_time = erlang:system_time(millisecond),
        test_duration = 3600000,  % 1 hour default
        stress_intensity = 0.0
    },

    %% Start monitoring
    MonitoringTimer = erlang:send_after(?config(monitoring_interval, Config), self(), monitor_system),

    {ok, State}.

handle_call({add_scenario, Scenario}, _From, State) ->
    ?LOG_INFO("Adding stress scenario: ~p", [Scenario#scenario.name]),

    %% Validate scenario
    case validate_scenario(Scenario) of
        {ok, ValidatedScenario} ->
            NewScenarios = [ValidatedScenario | State#state.active_scenarios],
            {reply, ok, State#state{active_scenarios = NewScenarios}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({execute_scenario, ScenarioName}, _From, State) ->
    ?LOG_INFO("Executing stress scenario: ~p", [ScenarioName]),

    case find_scenario(ScenarioName, State#state.active_scenarios) of
        {ok, Scenario} ->
            {reply, ok, execute_scenario(State, Scenario)};
        {error, not_found} ->
            {reply, {error, scenario_not_found}, State}
    end;

handle_call(get_system_limits, _From, State) ->
    Limits = determine_system_limits(State),
    {reply, Limits, State};

handle_call(identify_breaking_points, _From, State) ->
    Analysis = analyze_breaking_points(State#state.breaking_points),
    {reply, Analysis, State};

handle_call(monitor_resilience, _From, State) ->
    Resilience = calculate_system_resilience(State#state.system_metrics,
                                           State#state.breaking_points),
    {reply, Resilience, State};

handle_call(stop, _From, State) ->
    %% Graceful shutdown
    ScenarioTimer = erlang:send_after(5000, self(), graceful_shutdown),
    {reply, ok, State#state{scenario_timer = ScenarioTimer}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_system, State) ->
    %% Collect system metrics
    Metrics = collect_system_metrics(),

    %% Analyze system health
    Health = analyze_system_health(Metrics),

    %% Check for breaking points
    BreakingPoints = check_for_breaking_points(Metrics, State),

    %% Update state
    NewState = State#state{
        system_metrics = Metrics,
        system_health = Health,
        breaking_points = BreakingPoints ++ State#state.breaking_points,
        stress_intensity = calculate_stress_intensity(Metrics)
    },

    %% Schedule next monitoring
    MonitoringTimer = erlang:send_after(?config(monitoring_interval, State#state.config),
                                    self(), monitor_system),

    {noreply, NewState#state{monitoring_timer = MonitoringTimer}};

handle_info(start_scenario, State) ->
    %% Execute next stress scenario
    case State#state.active_scenarios of
        [] ->
            ?LOG_INFO("No more scenarios to execute"),
            {noreply, State};
        [Scenario | Rest] ->
            NewState = execute_scenario(State, Scenario),
            ScenarioTimer = erlang:send_after(Scenario#scenario.duration, self(), scenario_completed),
            {noreply, NewState#state{
                active_scenarios = Rest,
                scenario_timer = ScenarioTimer
            }}
    end;

handle_info(scenario_completed, State) ->
    %% Record scenario completion
    RecoveryTime = record_recovery_time(State),

    ?LOG_INFO("Scenario completed, recovery time: ~p ms", [RecoveryTime]),

    %% Start next scenario or finish
    ScenarioTimer = erlang:send_after(5000, self(), start_scenario),
    {noreply, State#state{scenario_timer = ScenarioTimer}};

handle_info(graceful_shutdown, State) ->
    %% Shutdown all active scenarios
    lists:foreach(fun(Scenario) ->
        stop_scenario(Scenario, State)
    end, State#state.active_scenarios),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating stress test, executed scenarios: ~p",
             [length(State#state.breaking_points)]),

    %% Stop all timers
    [erlang:cancel_timer(Timer) || Timer <-
        [State#state.monitoring_timer, State#state.scenario_timer]
    ],

    %% Generate final report
    generate_stress_test_report(State),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## INTERNAL FUNCTIONS
##====================================================================

initialize_system_metrics() ->
    #{
        timestamp => erlang:system_time(millisecond),
        cpu => #{
            usage => 0.0,
            cores => erlang:system_info(schedulers),
            load_average => [0.0, 0.0, 0.0]
        },
        memory => #{
            total => 0,
            used => 0,
            available => 0,
            usage => 0.0
        },
        disk => #{
            read_bytes => 0,
            write_bytes => 0,
            read_ops => 0,
            write_ops => 0
        },
        network => #{
            bytes_in => 0,
            bytes_out => 0,
            packets_in => 0,
            packets_out => 0,
            connections => 0
        },
        processes => #{
            count => 0,
            memory => 0,
            message_queue => 0
        },
        application => #{
            response_time => 0,
            throughput => 0,
            error_rate => 0.0
        }
    }.

validate_scenario(Scenario) ->
    %% Validate stress scenario configuration
    case Scenario#scenario.load_profile of
        #load_profile{type = Type, base_load = Base, max_load = Max}
            when Type =:= linear orelse Type =:= exponential orelse Type =:= sinusoidal orelse Type =:= step orelse Type =:= spike ->
            if Base > 0 andalso Max > Base ->
                {ok, Scenario};
            true ->
                {error, invalid_load_profile}
            end;
        _ ->
            {error, invalid_load_profile}
    end.

find_scenario(Name, Scenarios) ->
    case lists:filter(fun(S) -> S#scenario.name =:= Name end, Scenarios) of
        [Scenario | _] -> {ok, Scenario};
        [] -> {error, not_found}
    end.

execute_scenario(State, Scenario) ->
    ?LOG_INFO("Executing stress scenario: ~p", [Scenario#scenario.name]),

    %% Initialize scenario monitoring
    MonitorPid = start_scenario_monitoring(Scenario, State),

    %% Apply load profile
    apply_load_profile(Scenario#scenario.load_profile, MonitorPid),

    %% Setup failure injection
    setup_failure_injection(Scenario#scenario.failure_injection, MonitorPid),

    %% Monitor scenario execution
    ScenarioState = monitor_scenario_execution(Scenario, MonitorPid, State),

    %% Generate scenario report
    generate_scenario_report(Scenario, ScenarioState),

    ScenarioState.

apply_load_profile(Profile, MonitorPid) ->
    case Profile#load_profile.type of
        linear ->
            linear_ramp(Profile, MonitorPid);
        exponential ->
            exponential_ramp(Profile, MonitorPid);
        sinusoidal ->
            sinusoidal_ramp(Profile, MonitorPid);
        step ->
            step_ramp(Profile, MonitorPid);
        spike ->
            spike_ramp(Profile, MonitorPid)
    end.

linear_ramp(Profile, MonitorPid) ->
    Base = Profile#load_profile.base_load,
    Max = Profile#load_profile.max_load,
    Duration = Profile#load_profile.duration,
    StepTime = Duration div (Max - Base),

    %% Start at base load
    apply_load(Base, MonitorPid),

    %% Ramp up linearly
    lists:foreach(fun(Load) ->
        apply_load(Load, MonitorPid),
        timer:sleep(StepTime)
    end, lists:seq(Base + 1, Max)).

exponential_ramp(Profile, MonitorPid) ->
    Base = Profile#load_profile.base_load,
    Max = Profile#load_profile.max_load,
    Duration = Profile#load_profile.duration,

    %% Calculate exponential growth
    GrowthFactor = math:log(Max / Base) / (Duration / 1000),

    %% Exponential ramp
    lists:foreach(fun(_) ->
        CurrentLoad = round(Base * math:exp(GrowthFactor * erlang:system_time(second))),
        if CurrentLoad < Max ->
            apply_load(min(CurrentLoad, Max), MonitorPid);
        true ->
            ok
        end,
        timer:sleep(1000)
    end, lists:seq(1, Duration div 1000)).

sinusoidal_ramp(Profile, MonitorPid) ->
    Base = Profile#load_profile.base_load,
    Max = Profile#load_profile.max_load,
    Duration = Profile#load_profile.duration,

    %% Sinusoidal load pattern
    lists:foreach(fun(Time) ->
        Load = Base + (Max - Base) * (0.5 + 0.5 * math:sin(2 * math:pi() * Time / (Duration div 1000))),
        apply_load(round(Load), MonitorPid),
        timer:sleep(1000)
    end, lists:seq(0, Duration div 1000 - 1)).

step_ramp(Profile, MonitorPid) ->
    Base = Profile#load_profile.base_load,
    Max = Profile#load_profile.max_load,
    Duration = Profile#load_profile.duration,
    Steps = 10,
    StepSize = (Max - Base) div Steps,
    StepDuration = Duration div Steps,

    %% Step ramp
    lists:foreach(fun(Step) ->
        Load = Base + Step * StepSize,
        apply_load(Load, MonitorPid),
        timer:sleep(StepDuration)
    end, lists:seq(1, Steps)).

spike_ramp(Profile, MonitorPid) ->
    Base = Profile#load_profile.base_load,
    Max = Profile#load_profile.max_load,
    Duration = Profile#load_profile.duration,

    %% Spike pattern
    lists:foreach(fun(Time) ->
        if Time rem 20 == 0 ->
            apply_load(Max, MonitorPid);
        true ->
            apply_load(Base, MonitorPid)
        end,
        timer:sleep(1000)
    end, lists:seq(0, Duration div 1000 - 1)).

apply_load(Load, MonitorPid) ->
    %% Apply load using monitoring pid
    MonitorPid ! {apply_load, Load}.

setup_failure_injection(FailureTypes, MonitorPid) ->
    lists:foreach(fun(Failure) ->
        MonitorPid ! {setup_failure, Failure}
    end, FailureTypes).

start_scenario_monitoring(Scenario, State) ->
    %% Start scenario-specific monitoring
    spawn_link(fun() -> scenario_monitor_loop(Scenario, State) end).

scenario_monitor_loop(Scenario, State) ->
    receive
        {apply_load, Load} ->
            apply_scenario_load(Scenario, Load),
            scenario_monitor_loop(Scenario, State);
        {setup_failure, Failure} ->
            setup_scenario_failure(Scenario, Failure),
            scenario_monitor_loop(Scenario, State);
        stop ->
            ok
    after 1000 ->
        scenario_monitor_loop(Scenario, State)
    end.

apply_scenario_load(Scenario, Load) ->
    %% Apply actual load to erlmcp system
    case Scenario#scenario.name of
        <<"cpu_intensive">> ->
            erlmcp_stress_cpu:apply_load(Load);
        <<"memory_intensive">> ->
            erlmcp_stress_memory:apply_load(Load);
        <<"network_intensive">> ->
            erlmcp_stress_network:apply_load(Load);
        _ ->
            erlmcp_stress_general:apply_load(Load)
    end.

setup_scenario_failure(Scenario, Failure) ->
    %% Setup failure injection
    case Failure#failure_injection.type of
        process_crash ->
            erlmcp_stress_failures:inject_process_crash(Failure);
        network_partition ->
            erlmcp_stress_failures:inject_network_partition(Failure);
        resource_exhaustion ->
            erlmcp_stress_failures:inject_resource_exhaustion(Failure);
        data_corruption ->
            erlmcp_stress_failures:inject_data_corruption(Failure);
        overload_scenario ->
            erlmcp_stress_failures:inject_overload(Failure)
    end.

monitor_scenario_execution(Scenario, MonitorPid, State) ->
    Duration = Scenario#scenario.duration,
    StartTime = erlang:system_time(millisecond),
    Results = [],

    scenario_execution_loop(Scenario, MonitorPid, StartTime, Duration, Results).

scenario_execution_loop(Scenario, MonitorPid, StartTime, Duration, Results) ->
    case erlang:system_time(millisecond) - StartTime < Duration of
        true ->
            %% Collect metrics
            Metrics = collect_scenario_metrics(Scenario, MonitorPid),

            %% Check for breaking point
            BreakingPoint = check_scenario_breaking_point(Metrics, Scenario),

            NewResults = [Metrics#{breaking_point => BreakingPoint} | Results],

            scenario_execution_loop(Scenario, MonitorPid, StartTime, Duration, NewResults);
        false ->
            MonitorPid ! stop,
            Results
    end.

collect_system_metrics() ->
    %% Collect comprehensive system metrics
    #{
        timestamp => erlang:system_time(millisecond),
        cpu => collect_cpu_metrics(),
        memory => collect_memory_metrics(),
        disk => collect_disk_metrics(),
        network => collect_network_metrics(),
        processes => collect_process_metrics(),
        application => collect_application_metrics()
    }.

collect_cpu_metrics() ->
    %% CPU metrics
    #{
        usage => get_cpu_usage(),
        cores => erlang:system_info(schedulers),
        load_average => get_load_average()
    }.

collect_memory_metrics() ->
    %% Memory metrics
    #{
        total => get_total_memory(),
        used => get_used_memory(),
        available => get_available_memory(),
        usage => get_memory_usage()
    }.

collect_disk_metrics() ->
    %% Disk metrics
    #{
        read_bytes => get_disk_read_bytes(),
        write_bytes => get_disk_write_bytes(),
        read_ops => get_disk_read_ops(),
        write_ops => get_disk_write_ops()
    }.

collect_network_metrics() ->
    %% Network metrics
    #{
        bytes_in => get_network_bytes_in(),
        bytes_out => get_network_bytes_out(),
        packets_in => get_network_packets_in(),
        packets_out => get_network_packets_out(),
        connections => get_connection_count()
    }.

collect_process_metrics() ->
    %% Process metrics
    #{
        count => get_process_count(),
        memory => get_process_memory(),
        message_queue => get_message_queue_size()
    }.

collect_application_metrics() ->
    %% Application-specific metrics
    #{
        response_time => get_response_time(),
        throughput => get_throughput(),
        error_rate => get_error_rate()
    }.

collect_scenario_metrics(Scenario, MonitorPid) ->
    %% Collect metrics specific to scenario
    BaseMetrics = collect_system_metrics(),

    %% Add scenario-specific metrics
    ScenarioMetrics = #{
        scenario => Scenario#scenario.name,
        load_level => get_current_load(MonitorPid),
        timestamp => erlang:system_time(millisecond)
    },

    maps:merge(BaseMetrics, ScenarioMetrics).

check_for_breaking_points(Metrics, State) ->
    %% Check system metrics for breaking points
    BreakingPoints = [],

    %% Check CPU breaking point
    case is_cpu_breaking_point(Metrics) of
        true ->
            BreakingPoints = add_cpu_breaking_point(BreakingPoints, Metrics);
        false ->
            ok
    end,

    %% Check memory breaking point
    case is_memory_breaking_point(Metrics) of
        true ->
            BreakingPoints = add_memory_breaking_point(BreakingPoints, Metrics);
        false ->
            ok
    end,

    %% Check network breaking point
    case is_network_breaking_point(Metrics) of
        true ->
            BreakingPoints = add_network_breaking_point(BreakingPoints, Metrics);
        false ->
            ok
    end,

    %% Check application breaking point
    case is_application_breaking_point(Metrics) of
        true ->
            BreakingPoints = add_application_breaking_point(BreakingPoints, Metrics);
        false ->
            ok
    end,

    BreakingPoints.

is_cpu_breaking_point(Metrics) ->
    CPU = Metrics#system_metrics.cpu,
    CPU#cpu.usage > 95.0.

is_memory_breaking_point(Metrics) ->
    Memory = Metrics#system_metrics.memory,
    Memory#memory.usage > 90.0.

is_network_breaking_point(Metrics) ->
    Network = Metrics#system_metrics.network,
    Network#network.bytes_out > 100 * 1024 * 1024.  % 100 MB/s

is_application_breaking_point(Metrics) ->
    App = Metrics#system_metrics.application,
    App#application.error_rate > 0.05.  % 5% error rate

add_cpu_breaking_point(BreakingPoints, Metrics) ->
    #{
        scenario => cpu_breaking_point,
        load_level => get_current_load(?MODULE),
        system_metrics => Metrics,
        failure_type => cpu_overload,
        recovery_time => record_cpu_recovery_time(Metrics),
        impact => #{
            severity => critical,
            affected_components => [cpu],
            duration_ms => estimate_recovery_time(Metrics)
        }
    } ++ BreakingPoints.

add_memory_breaking_point(BreakingPoints, Metrics) ->
    #{
        scenario => memory_breaking_point,
        load_level => get_current_load(?MODULE),
        system_metrics => Metrics,
        failure_type => memory_exhaustion,
        recovery_time => record_memory_recovery_time(Metrics),
        impact => #{
            severity => critical,
            affected_components => [memory],
            duration_ms => estimate_recovery_time(Metrics)
        }
    } ++ BreakingPoints.

add_network_breaking_point(BreakingPoints, Metrics) ->
    #{
        scenario => network_breaking_point,
        load_level => get_current_load(?MODULE),
        system_metrics => Metrics,
        failure_type => network_saturation,
        recovery_time => record_network_recovery_time(Metrics),
        impact => #{
            severity => high,
            affected_components => [network],
            duration_ms => estimate_recovery_time(Metrics)
        }
    } ++ BreakingPoints.

add_application_breaking_point(BreakingPoints, Metrics) ->
    #{
        scenario => application_breaking_point,
        load_level => get_current_load(?MODULE),
        system_metrics => Metrics,
        failure_type => application_error,
        recovery_time => record_application_recovery_time(Metrics),
        impact => #{
            severity => high,
            affected_components => [application],
            duration_ms => estimate_recovery_time(Metrics)
        }
    } ++ BreakingPoints.

analyze_system_health(Metrics) ->
    %% Analyze overall system health
    HealthScore = calculate_health_score(Metrics),
    Components = analyze_component_health(Metrics),

    #{
        overall => HealthScore,
        components => Components,
        status => determine_system_status(HealthScore, Components)
    }.

calculate_health_score(Metrics) ->
    CPU = Metrics#system_metrics.cpu#cpu.usage,
    Memory = Metrics#system_metrics.memory#memory.usage,
    Network = get_network_utilization(Metrics),
    Application = Metrics#system_metrics.application#application.error_rate,

    WeightedScore = 0.3 * (100 - CPU) +
                    0.3 * (100 - Memory) +
                    0.2 * (100 - Network) +
                    0.2 * (100 - Application * 100),

    max(0, min(100, WeightedScore)).

analyze_component_health(Metrics) ->
    #{
        cpu => #{
            usage => Metrics#system_metrics.cpu#cpu.usage,
            status => determine_cpu_status(Metrics#system_metrics.cpu#cpu.usage),
            trend => determine_cpu_trend(Metrics)
        },
        memory => #{
            usage => Metrics#system_metrics.memory#memory.usage,
            status => determine_memory_status(Metrics#system_metrics.memory#memory.usage),
            trend => determine_memory_trend(Metrics)
        },
        network => #{
            usage => get_network_utilization(Metrics),
            status => determine_network_status(get_network_utilization(Metrics)),
            trend => determine_network_trend(Metrics)
        },
        application => #{
            error_rate => Metrics#system_metrics.application#application.error_rate,
            status => determine_application_status(Metrics#system_metrics.application#application.error_rate),
            trend => determine_application_trend(Metrics)
        }
    }.

determine_system_status(HealthScore, Components) ->
    case HealthScore of
        Score when Score >= 90 -> optimal;
        Score when Score >= 70 -> healthy;
        Score when Score >= 50 -> warning;
        Score when Score >= 30 -> critical;
        _ -> failure
    end.

determine_cpu_status(Usage) ->
    case Usage of
        U when U < 50 -> optimal;
        U when U < 80 -> warning;
        U when U < 95 -> critical;
        _ -> failure
    end.

determine_memory_status(Usage) ->
    case Usage of
        U when U < 60 -> optimal;
        U when U < 85 -> warning;
        U when U < 95 -> critical;
        _ -> failure
    end.

determine_network_status(Usage) ->
    case Usage of
        U when U < 40 -> optimal;
        U when U < 70 -> warning;
        U when U < 90 -> critical;
        _ -> failure
    end.

determine_application_status(ErrorRate) ->
    case ErrorRate of
        E when E < 0.01 -> optimal;
        E when E < 0.05 -> warning;
        E when E < 0.1 -> critical;
        _ -> failure
    end.

calculate_stress_intensity(Metrics) ->
    %% Calculate overall stress intensity
    CPU = Metrics#system_metrics.cpu#cpu.usage,
    Memory = Metrics#system_metrics.memory#memory.usage,
    Network = get_network_utilization(Metrics),
    Application = Metrics#system_metrics.application#application.error_rate,

    (CPU + Memory + Network + Application * 100) / 400.0.

determine_system_limits(State) ->
    %% Determine system limits based on historical data
    BreakingPoints = State#state.breaking_points,

    Limits = #{
        max_throughput => determine_max_throughput(BreakingPoints),
        max_concurrent_users => determine_max_concurrent_users(BreakingPoints),
        max_memory_usage => determine_max_memory_usage(BreakingPoints),
        max_cpu_usage => determine_max_cpu_usage(BreakingPoints),
        max_network_bandwidth => determine_max_network_bandwidth(BreakingPoints)
    },

    Limits.

analyze_breaking_points(BreakingPoints) ->
    %% Analyze breaking points and identify patterns
    Analysis = #{
        total_breaking_points => length(BreakingPoints),
        breaking_point_types => get_breaking_point_types(BreakingPoints),
        common_patterns => identify_common_patterns(BreakingPoints),
        recovery_times => get_recovery_times(BreakingPoints),
        impact_analysis => analyze_impact(BreakingPoints)
    },

    Analysis.

get_breaking_point_types(BreakingPoints) ->
    Types = lists:foldl(fun(BP, Acc) ->
        Type = maps:get(failure_type, BP#breaking_point.impact),
        maps:get(Type, Acc, 0) + 1
    end, #{}, BreakingPoints),

    lists:fold(fun({Type, Count}, Acc) ->
        [#{type => Type, count => Count} | Acc]
    end, [], maps:to_list(Types)).

identify_common_patterns(BreakingPoints) ->
    %% Identify common patterns in breaking points
    Patterns = [],

    %% Check for threshold patterns
    ThresholdPatterns = check_threshold_patterns(BreakingPoints),
    Patterns ++ ThresholdPatterns.

check_threshold_patterns(BreakingPoints) ->
    %% Check for threshold-based patterns
    CPUThresholds = [BP#breaking_point.system_metrics#system_metrics.cpu#cpu.usage ||
                   BP <- BreakingPoints,
                   maps:get(failure_type, BP#breaking_point.impact) =:= cpu_overload],

    case CPUThresholds of
        [] -> [];
        _ -> [#{pattern => cpu_threshold, values => CPUThresholds}]
    end.

get_recovery_times(BreakingPoints) ->
    lists:foldl(fun(BP, Acc) ->
        maps:get(failure_type, BP#breaking_point.impact) =>
            [BP#breaking_point.recovery_time |
             maps:get(maps:get(failure_type, BP#breaking_point.impact), Acc, [])]
    end, #{}, BreakingPoints).

analyze_impact(BreakingPoints) ->
    %% Analyze impact of breaking points
    Impact = #{},

    lists:foldl(fun(BP, Acc) ->
        FailureType = maps:get(failure_type, BP#breaking_point.impact),
        Severity = maps:get(severity, BP#breaking_point.impact),

        maps:update(FailureType, [#{severity => Severity, count => 1} |
                                 maps:get(FailureType, Acc, [])], Acc)
    end, Impact, BreakingPoints).

calculate_system_resilience(SystemMetrics, BreakingPoints) ->
    %% Calculate system resilience
    ResilienceScore = calculate_resilience_score(SystemMetrics, BreakingPoints),
    RecoveryTime = calculate_average_recovery_time(BreakingPoints),
    FailureTolerance = calculate_failure_tolerance(BreakingPoints),

    #{
        score => ResilienceScore,
        recovery_time => RecoveryTime,
        failure_tolerance => FailureTolerance,
        recommendations => generate_resilience_recommendations(ResilienceScore, BreakingPoints)
    }.

calculate_resilience_score(SystemMetrics, BreakingPoints) ->
    %% Calculate resilience score based on system health and breaking points
    HealthScore = calculate_health_score(SystemMetrics),
    BreakingPointCount = length(BreakingPoints),

    WeightedScore = 0.7 * HealthScore + 0.3 * (100 - BreakingPointCount * 5),

    max(0, min(100, WeightedScore)).

calculate_average_recovery_time(BreakingPoints) ->
    RecoveryTimes = [BP#breaking_point.recovery_time || BP <- BreakingPoints],
    case RecoveryTimes of
        [] -> 0;
        _ -> lists:sum(RecoveryTimes) / length(RecoveryTimes)
    end.

calculate_failure_tolerance(BreakingPoints) ->
    %% Calculate failure tolerance
    CriticalBreaks = [BP || BP <- BreakingPoints,
                         maps:get(severity, BP#breaking_point.impact) =:= critical],
    case CriticalBreaks of
        [] -> 1.0;
        _ -> length(CriticalBreaks) / length(BreakingPoints)
    end.

generate_resilience_recommendations(ResilienceScore, BreakingPoints) ->
    %% Generate resilience improvement recommendations
    Recommendations = [],

    case ResilienceScore < 80 of
        true ->
            [#{type => capacity, priority => high, recommendation => "Increase system capacity"} | Recommendations];
        false ->
            Recommendations
    end.

stop_scenario(Scenario, State) ->
    %% Stop specific scenario
    ?LOG_INFO("Stopping scenario: ~p", [Scenario#scenario.name]),

    %% Cleanup scenario resources
    cleanup_scenario_resources(Scenario, State),

    ok.

cleanup_scenario_resources(Scenario, State) ->
    %% Clean up scenario-specific resources
    case Scenario#scenario.name of
        <<"cpu_intensive">> ->
            erlmcp_stress_cpu:cleanup();
        <<"memory_intensive">> ->
            erlmcp_stress_memory:cleanup();
        <<"network_intensive">> ->
            erlmcp_stress_network:cleanup();
        _ ->
            erlmcp_stress_general:cleanup()
    end.

generate_stress_test_report(State) ->
    %% Generate comprehensive stress test report
    Report = #{
        test_duration => erlang:system_time(millisecond) - State#state.test_start_time,
        breaking_points => State#state.breaking_points,
        system_metrics => State#state.system_metrics,
        recovery_history => State#state.recovery_history,
        recommendations => generate_stress_test_recommendations(State)
    },

    %% Save report
    save_stress_test_report(Report),

    ok.

generate_stress_test_recommendations(State) ->
    %% Generate recommendations based on stress test results
    Recommendations = [],

    %% Capacity recommendations
    case State#state.system_health#system_health.overall < 80 of
        true ->
            [#{type => capacity, priority => high,
               recommendation => "Increase system capacity"} | Recommendations];
        false ->
            Recommendations
    end,

    %% Resilience recommendations
    Resilience = calculate_system_resilience(State#state.system_metrics, State#state.breaking_points),
    case Resilience#resilience.score < 70 of
        true ->
            [#{type => resilience, priority => high,
               recommendation => "Implement resilience measures"} | Recommendations];
        false ->
            Recommendations
    end,

    Recommendations.

save_stress_test_report(Report) ->
    %% Save stress test report to file
    ReportFile = "/Users/sac/erlmcp/load_test_reports/stress_test_report.json",
    ok = file:write_file(ReportFile, jsx:encode(Report)).