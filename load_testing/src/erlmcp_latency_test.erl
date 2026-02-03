-module(erlmcp_latency_test).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

-behaviour(gen_server).

%% API exports
-export([
    start/1,
    stop/1,
    collect_measurements/1,
    analyze_distribution/2,
    identify_optimization_opportunities/1,
    apply_optimizations/1,
    generate_latency_report/1
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
##====================================================================

-type latency_measurement() :: #{
    id := binary(),
    timestamp := integer(),
    total_latency := integer(),
    phases := #{
        submission := integer(),
        consensus := integer(),
        application := integer()
    },
    success := boolean(),
    request_type := binary(),
    payload_size := pos_integer(),
    response_size := pos_integer()
}.

-type latency_config() :: #{
    percentiles := [float()],
    latency_thresholds := #{
        critical := integer(),
        warning := integer(),
        acceptable := integer()
    },
    optimization_strategies := [map()],
    sampling_rate := pos_integer(),
    measurement_duration := pos_integer(),
    warmup_duration := pos_integer(),
    test_scenarios := [map()]
}.

-type latency_analysis() :: #{
    percentiles := #{float() => integer()},
    mean => float(),
    median => integer(),
    standard_deviation => float(),
    percentiles := #{float() => integer()},
    threshold_violations := [map()],
    optimization_opportunities := [map()],
    bottlenecks => map(),
    recommendations => [map()]
}.

-type optimization_opportunity() :: #{
    type := binary(),
    severity := low | medium | high,
    impact := float(),
    description := binary(),
    expected_improvement => float(),
    confidence => float(),
    implementation => map()
}.

%%====================================================================
## GEN_SERVER STATE
##====================================================================

-record(state, {
    config :: latency_config(),
    measurements :: [latency_measurement()],
    test_start_time :: integer(),
    test_duration :: pos_integer(),
    current_percentiles :: [float()],
    threshold_violations :: [map()],
    optimization_opportunities :: [optimization_opportunity()],
    monitoring_timer :: reference() | undefined,
    metrics_timer :: reference() | undefined,
    scenario_index :: integer(),
    active_scenarios :: [map()]
}).

%%====================================================================
## API FUNCTIONS
##====================================================================

-spec start(latency_config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop, 30000).

-spec collect_measurements(pid()) -> [latency_measurement()].
collect_measurements(Pid) ->
    gen_server:call(Pid, collect_measurements, 5000).

-spec analyze_distribution(pid(), [latency_measurement()]) -> latency_analysis().
analyze_distribution(Pid, Measurements) ->
    gen_server:call(Pid, {analyze_distribution, Measurements}, 5000).

-spec identify_optimization_opportunities(pid()) -> [optimization_opportunity()].
identify_optimization_opportunities(Pid) ->
    gen_server:call(Pid, identify_optimization_opportunities, 5000).

-spec apply_optimizations(pid(), [optimization_opportunity()]) -> ok.
apply_optimizations(Pid, Opportunities) ->
    gen_server:call(Pid, {apply_optimizations, Opportunities}, 10000).

-spec generate_latency_report(pid()) -> map().
generate_latency_report(Pid) ->
    gen_server:call(Pid, generate_latency_report, 5000).

%%====================================================================
## GEN_SERVER CALLBACKS
##====================================================================

init(Config) ->
    ?LOG_INFO("Starting latency test with config: ~p", [Config]),

    State = #state{
        config = Config,
        measurements = [],
        test_start_time = erlang:system_time(millisecond),
        test_duration = ?config(measurement_duration, Config),
        current_percentiles = ?config(percentiles, Config),
        threshold_violations = [],
        optimization_opportunities = [],
        monitoring_timer = undefined,
        metrics_timer = undefined,
        scenario_index = 1,
        active_scenarios = []
    },

    %% Start monitoring
    MonitoringTimer = erlang:send_after(?config(sampling_rate, Config), self(), collect_metrics),

    %% Start metrics collection
    MetricsTimer = erlang:send_after(?config(sampling_rate, Config), self(), process_measurements),

    {ok, State}.

handle_call(collect_measurements, _From, State) ->
    Measurements = State#state.measurements,
    {reply, Measurements, State};

handle_call({analyze_distribution, Measurements}, _From, State) ->
    Analysis = analyze_latency_distribution(Measurements, State#state.config),
    {reply, Analysis, State};

handle_call(identify_optimization_opportunities, _From, State) ->
    Opportunities = identify_optimization_opportunities_from_data(State),
    {reply, Opportunities, State};

handle_call({apply_optimizations, Opportunities}, _From, State) ->
    %% Apply optimization opportunities
    Results = lists:foldl(fun(Opportunity, Acc) ->
        apply_optimization(Opportunity, State),
        Acc ++ [Opportunity]
    end, [], Opportunities),

    {reply, Results, State};

handle_call(generate_latency_report, _From, State) ->
    Report = generate_report(State),
    {reply, Report, State};

handle_call(stop, _From, State) ->
    %% Initiate graceful shutdown
    ShutdownTimer = erlang:send_after(5000, self(), graceful_shutdown),
    {reply, ok, State#state{monitoring_timer = ShutdownTimer}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect latency measurements
    Measurements = collect_latency_measurements(State),

    %% Update measurements
    NewMeasurements = State#state.measurements ++ Measurements,

    %% Schedule next measurement collection
    MonitoringTimer = erlang:send_after(?config(sampling_rate, State#state.config),
                                    self(), collect_metrics),

    {noreply, State#state{measurements = NewMeasurements,
                          monitoring_timer = MonitoringTimer}};

handle_info(process_measurements, State) ->
    %% Process collected measurements
    Analysis = analyze_latency_distribution(State#state.measurements, State#state.config),

    %% Update optimization opportunities
    Opportunities = identify_optimization_opportunities_from_data(State),

    %% Update threshold violations
    ThresholdViolations = detect_threshold_violations(State#state.measurements, State#state.config),

    %% Schedule next processing
    MetricsTimer = erlang:send_after(?config(sampling_rate, State#state.config),
                                   self(), process_measurements),

    {noreply, State#state{
        threshold_violations = ThresholdViolations,
        optimization_opportunities = Opportunities,
        metrics_timer = MetricsTimer
    }};

handle_info(scenario_timeout, State) ->
    %% Move to next scenario
    NewState = next_scenario(State),
    {noreply, NewState};

handle_info(graceful_shutdown, State) ->
    %% Generate final report
    Report = generate_report(State),
    save_latency_report(Report),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating latency test, collected measurements: ~p",
             [length(State#state.measurements)]),

    %% Stop all timers
    [erlang:cancel_timer(Timer) || Timer <-
        [State#state.monitoring_timer, State#state.metrics_timer]
    ],

    %% Generate final report
    Report = generate_report(State),
    save_latency_report(Report),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## INTERNAL FUNCTIONS
##====================================================================

collect_latency_measurements(State) ->
    %% Collect latency measurements for current scenarios
    Scenarios = ?config(test_scenarios, State#state.config),
    CurrentScenario = lists:nth(State#state.scenario_index, Scenarios),

    Measurements = collect_scenario_measurements(CurrentScenario, State),

    %% Add scenario context
    [Measurement#{scenario => CurrentScenario#scenario.name} || Measurement <- Measurements].

collect_scenario_measurements(Scenario, State) ->
    %% Collect measurements for specific scenario
    ScenarioMeasurements = [],

    %% Based on scenario type, collect measurements
    case Scenario#scenario.type of
        constant_load ->
            collect_constant_load_measurements(Scenario, ScenarioMeasurements);
        ramping_load ->
            collect_ramping_load_measurements(Scenario, ScenarioMeasurements);
        burst_load ->
            collect_burst_load_measurements(Scenario, ScenarioMeasurements);
        spike_load ->
            collect_spike_load_measurements(Scenario, ScenarioMeasurements);
        _ ->
            collect_generic_measurements(Scenario, ScenarioMeasurements)
    end.

collect_constant_load_measurements(Scenario, Acc) ->
    %% Collect measurements with constant load
    TargetLatency = Scenario#scenario.target_latency,
    SampleCount = Scenario#scenario.sample_count,

    collect_latency_samples(TargetLatency, SampleCount, Acc).

collect_ramping_load_measurements(Scenario, Acc) ->
    %% Collect measurements with ramping load
    InitialLatency = Scenario#scenario.initial_latency,
    FinalLatency = Scenario#scenario.final_latency,
    SampleCount = Scenario#scenario.sample_count,

    Step = (FinalLatency - InitialLatency) / SampleCount,
    Latencies = [round(InitialLatency + Step * N) || N <- lists:seq(0, SampleCount - 1)],

    collect_latency_samples(Latencies, Acc).

collect_burst_load_measurements(Scenario, Acc) ->
    %% Collect measurements with burst load
    BurstCount = Scenario#scenario.burst_count,
    BurstSize = Scenario#scenario.burst_size,
    LatencyRange = Scenario#scenario.latency_range,

    BurstMeasurements = lists:foldl(fun(_, BurstAcc) ->
        BurstLatencies = [rand:uniform(LatencyRange) + 50 ||
                          _ <- lists:seq(1, BurstSize)],
        collect_latency_samples(BurstLatencies, BurstAcc)
    end, [], lists:seq(1, BurstCount)),

    Acc ++ BurstMeasurements.

collect_spike_load_measurements(Scenario, Acc) ->
    %% Collect measurements with spike load
    SpikeCount = Scenario#scenario.spike_count,
    SpikeIntensity = Scenario#scenario.spike_intensity,
    NormalLatency = Scenario#scenario.normal_latency,

    SpikeMeasurements = lists:foldl(fun(_, SpikeAcc) ->
        SpikeLatencies = [round(NormalLatency * SpikeIntensity) ||
                          _ <- lists:seq(1, 10)],  % 10 samples per spike
        collect_latency_samples(SpikeLatencies, SpikeAcc)
    end, [], lists:seq(1, SpikeCount)),

    Acc ++ SpikeMeasurements.

collect_latency_samples(Latency, Count, Acc) when is_integer(Latency) ->
    collect_latency_samples([Latency || _ <- lists:seq(1, Count)], Acc);

collect_latency_samples(Latencies, Acc) ->
    %% Convert latency values to measurements
    lists:foldl(fun(Latency, MeasurementAcc) ->
        Measurement = create_latency_measurement(Latency),
        [Measurement | MeasurementAcc]
    end, Acc, Latencies).

create_latency_measurement(Latency) ->
    #{
        id => erlmcp_utils:uuid(),
        timestamp => erlang:system_time(millisecond),
        total_latency => Latency,
        phases => #{
            submission => round(Latency * 0.2),
            consensus => round(Latency * 0.5),
            application => round(Latency * 0.3)
        },
        success => true,
        request_type => <<"generic">>,
        payload_size => 1024,
        response_size => 2048
    }.

analyze_latency_distribution(Measurements, Config) ->
    %% Analyze latency distribution with percentiles
    Percentiles = ?config(percentiles, Config),

    %% Extract total latencies
    TotalLatencies = [M#measurement.total_latency || M <- Measurements],
    SuccessMeasurements = [M || M <- Measurements, M#measurement.success],

    %% Calculate basic statistics
    Count = length(SuccessMeasurements),
    Mean = lists:sum(TotalLatencies) / Count,
    Median = calculate_median(TotalLatencies),
    StdDev = calculate_standard_deviation(TotalLatencies),

    %% Calculate percentiles
    SortedLatencies = lists:sort(TotalLatencies),
    PercentileResults = calculate_percentiles(SortedLatencies, Percentiles),

    %% Identify bottlenecks
    Bottlenecks = identify_latency_bottlenecks(Measurements),

    ##% Detect threshold violations
    ThresholdViolations = detect_threshold_violations(Measurements, Config),

    #{
        percentiles => PercentileResults,
        mean => Mean,
        median => Median,
        standard_deviation => StdDev,
        count => Count,
        threshold_violations => ThresholdViolations,
        bottlenecks => Bottlenecks
    }.

calculate_percentiles(SortedLatencies, Percentiles) ->
    lists:foldl(fun(Percentile, Acc) ->
        Value = calculate_percentile_value(SortedLatencies, Percentile),
        Acc#{Percentile => Value}
    end, #{}, Percentiles).

calculate_percentile_value(SortedList, Percentile) ->
    Length = length(SortedList),
    Index = ceil(Length * Percentile / 100),
    if Index > 0 andalso Index =< Length ->
        lists:nth(Index, SortedList);
    true -> 0
    end.

calculate_median(List) ->
    Length = length(List),
    if Length =:= 0 ->
        0;
    Length rem 2 =:= 0 ->
        (lists:nth(Length div 2, List) + lists:nth(Length div 2 + 1, List)) div 2;
    true ->
        lists:nth((Length + 1) div 2, List)
    end.

calculate_standard_deviation(List) ->
    Mean = lists:sum(List) / length(List),
    SumSquares = lists:foldl(fun(X, Acc) -> Acc + math:pow(X - Mean, 2) end, 0.0, List),
    math:sqrt(SumSquares / length(List)).

detect_threshold_violations(Measurements, Config) ->
    Thresholds = ?config(latency_thresholds, Config),
    Percentiles = ?config(percentiles, Config),

    Violations = [],

    %% Check percentiles against thresholds
    lists:foldl(fun(Percentile, Acc) ->
        case lists:member(Percentile, Percentiles) of
            true ->
                PercentileValue = calculate_percentile_value(
                    [M#measurement.total_latency || M <- Measurements], Percentile),
                case PercentileValue > Thresholds#critical of
                    true ->
                        Violation = #{
                            percentile => Percentile,
                            value => PercentileValue,
                            threshold => Thresholds#critical,
                            severity => critical,
                            timestamp => erlang:system_time(millisecond)
                        },
                        [Violation | Acc];
                    false ->
                        Acc
                end;
            false ->
                Acc
        end
    end, [], Percentiles).

identify_latency_bottlenecks(Measurements) ->
    %% Identify bottlenecks in latency
    Bottlenecks = #{},

    %% Analyze phase breakdown
    PhaseAnalysis = analyze_phase_breakdown(Measurements),

    %% Check for phase-specific bottlenecks
    lists:foldl(fun(Phase, Acc) ->
        PhaseLatencies = [M#measurement.phases[Phase] || M <- Measurements],
        AveragePhaseLatency = lists:sum(PhaseLatencies) / length(PhaseLatencies),
        TotalAverage = lists:sum([M#measurement.total_latency || M <- Measurements]) / length(Measurements),

        if AveragePhaseLatency > TotalAverage * 0.5 ->
            Bottleneck = #{
                phase => Phase,
                average_latency => AveragePhaseLatency,
                contribution => AveragePhaseLatency / TotalAverage,
                samples => length(PhaseLatencies),
                optimization => identify_phase_optimization(Phase)
            },
            Acc#{Phase => Bottleneck};
        true ->
            Acc
        end
    end, Bottlenecks, [submission, consensus, application]).

analyze_phase_breakdown(Measurements) ->
    %% Analyze latency breakdown by phase
    PhaseBreakdown = #{},

    lists:foldl(fun(Measurement, Acc) ->
        lists:fold(fun({Phase, Latency}, PhaseAcc) ->
            maps:update(Phase, [Latency | maps:get(Phase, PhaseAcc, [])], PhaseAcc)
        end, Acc, maps:to_list(Measurement#measurement.phases))
    end, #{}, Measurements).

identify_phase_optimization(Phase) ->
    %% Identify optimization opportunities for specific phase
    case Phase of
        submission ->
            #{
                type => batch_requests,
                impact => moderate,
                description => "Implement request batching to reduce submission overhead"
            };
        consensus ->
            #{
                type => optimize_consensus,
                impact => high,
                description => "Optimize consensus algorithm for faster decision making"
            };
        application ->
            #{
                type => application_optimization,
                impact => moderate,
                description => "Optimize application processing logic"
            }
    end.

identify_optimization_opportunities_from_data(State) ->
    %% Identify optimization opportunities from latency data
    Opportunities = [],

    %% Check for overall optimization opportunities
    OverallOpportunities = identify_overall_opportunities(State),
    Opportunities ++ OverallOpportunities,

    %% Check for phase-specific opportunities
    PhaseOpportunities = identify_phase_opportunities(State),
    Opportunities ++ PhaseOpportunities,

    %% Check for system optimization opportunities
    SystemOpportunities = identify_system_optimization_opportunities(State),
    Opportunities ++ SystemOpportunities.

identify_overall_opportunities(State) ->
    %% Identify overall optimization opportunities
    Measurements = State#state.measurements,
    Analysis = analyze_latency_distribution(Measurements, State#state.config),

    Opportunities = [],

    %% Check for high standard deviation
    case Analysis#analysis.standard_deviation > 100 of
        true ->
            Opportunity = #{
                type => reduce_latency_variance,
                severity => medium,
                impact => 0.3,
                description => "High latency variance detected - implement more consistent processing",
                expected_improvement => 0.15,
                confidence => 0.8,
                implementation => #{
                    strategy => process_optimization,
                    steps => ["identify_variance_sources", "implement_stable_processing", "monitor_impact"]
                }
            },
            [Opportunity | Opportunities];
        false ->
            Opportunities
    end.

identify_phase_opportunities(State) ->
    %% Identify phase-specific optimization opportunities
    Measurements = State#state.measurements,
    PhaseAnalysis = analyze_phase_breakdown(Measurements),

    Opportunities = [],

    %% Check each phase for optimization opportunities
    lists:fold(fun({Phase, Latencies}, Acc) ->
        AverageLatency = lists:sum(Latencies) / length(Latencies),
        TotalAverage = lists:sum([M#measurement.total_latency || M <- Measurements]) / length(Measurements),

        if AverageLatency > TotalAverage * 0.4 ->
            Opportunity = create_phase_optportunity(Phase, AverageLatency, TotalAverage),
            [Opportunity | Acc];
        true ->
            Acc
        end
    end, Opportunities, maps:to_list(PhaseAnalysis)).

create_phase_optportunity(Phase, PhaseLatency, TotalLatency) ->
    ##% Create optimization opportunity for specific phase
    Contribution = PhaseLatency / TotalLatency,

    #{
        type => phase_optimization,
        phase => Phase,
        severity => case Contribution of
                     C when C > 0.6 -> high;
                     C when C > 0.4 -> medium;
                     _ -> low
                  end,
        impact => Contribution,
        description => string:concat("Optimize ", atom_to_list(Phase)),
        expected_improvement => Contribution * 0.5,
        confidence => 0.7,
        implementation => #{
            phase => Phase,
            strategy => case Phase of
                         submission -> request_batching;
                         consensus -> consensus_optimization;
                         application -> application_tuning
                       end,
            steps => identify_optimization_steps(Phase)
        }
    }.

identify_optimization_steps(Phase) ->
    case Phase of
        submission ->
            ["Implement request batching", "Optimize submission protocol", "Reduce round trips"];
        consensus ->
            ["Optimize consensus algorithm", "Reduce message overhead", "Improve decision making"];
        application ->
            ["Cache responses", "Optimize processing logic", "Reduce dependencies"]
    end.

identify_system_optimization_opportunities(State) ->
    %% Identify system-level optimization opportunities
    Measurements = State#state.measurements,
    Analysis = analyze_latency_distribution(Measurements, State#state.config),

    Opportunities = [],

    %% Check for high overall latency
    case Analysis#analysis.mean > 500 of
        true ->
            SystemOpportunity = #{
                type => system_scaling,
                severity => high,
                impact => 0.4,
                description => "High system latency detected - consider scaling or optimization",
                expected_improvement => 0.3,
                confidence => 0.9,
                implementation => #{
                    strategy => vertical_scaling,
                    steps => ["identify_bottlenecks", "vertical_scale", "monitor_performance"]
                }
            },
            [SystemOpportunity | Opportunities];
        false ->
            Opportunities
    end.

apply_optimization(Opportunity, State) ->
    %% Apply optimization opportunity
    ?LOG_INFO("Applying optimization: ~p", [Opportunity#optimization_opportunity.type]),

    case Opportunity#optimization_opportunity.type of
        phase_optimization ->
            apply_phase_optimization(Opportunity, State);
        reduce_latency_variance ->
            apply_variance_reduction(Opportunity, State);
        system_scaling ->
            apply_system_scaling(Opportunity, State);
        _ ->
            apply_generic_optimization(Opportunity, State)
    end.

apply_phase_optimization(Opportunity, State) ->
    %% Apply phase-specific optimization
    Phase = Opportunity#optimization_opportunity.phase,
    Implementation = Opportunity#optimization_opportunity.implementation,

    case Phase of
        submission ->
            erlmcp_optimization:apply_submission_optimization(Implementation);
        consensus ->
            erlmcp_optimization:apply_consensus_optimization(Implementation);
        application ->
            erlmcp_optimization:apply_application_optimization(Implementation)
    end.

apply_variance_reduction(Opportunity, State) ->
    %% Apply variance reduction optimization
    erlmcp_optimization:apply_variance_reduction(Opportunity#optimization_opportunity.implementation).

apply_system_scaling(Opportunity, State) ->
    %% Apply system scaling optimization
    erlmcp_optimization:apply_system_scaling(Opportunity#optimization_opportunity.implementation).

apply_generic_optimization(Opportunity, State) ->
    %% Apply generic optimization
    erlmcp_optimization:apply_optimization(Opportunity#optimization_opportunity.implementation).

generate_report(State) ->
    %% Generate comprehensive latency report
    Measurements = State#state.measurements,
    Analysis = analyze_latency_distribution(Measurements, State#state.config),
    Opportunities = State#state.optimization_opportunities,
    ThresholdViolations = State#state.threshold_violations,

    Report = #{
        test_summary => #{
            duration => erlang:system_time(millisecond) - State#state.test_start_time,
            measurements => length(Measurements),
            scenarios_executed => length(?config(test_scenarios, State#state.config))
        },
        analysis => Analysis,
        threshold_violations => ThresholdViolations,
        optimization_opportunities => Opportunities,
        recommendations => generate_recommendations(Opportunities, ThresholdViolations),
        timestamp => erlang:system_time(millisecond)
    },

    Report.

generate_recommendations(Opportunities, ThresholdViolations) ->
    ##% Generate recommendations based on analysis
    Recommendations = [],

    %% Generate priority recommendations based on violations
    case ThresholdViolations of
        [] -> Recommendations;
        _ ->
            PriorityReco = #{
                type => immediate_action,
                priority => high,
                action => "Address threshold violations",
                impact => critical,
                violations => length(ThresholdViolations)
            },
            [PriorityReco | Recommendations]
    end,

    %% Add optimization recommendations
    case Opportunities of
        [] -> Recommendations;
        _ ->
            OptReco = #{
                type => optimization,
                priority => medium,
                action => "Implement identified optimizations",
                impact => improvement,
                opportunities => length(Opportunities)
            },
            [OptReco | Recommendations]
    end.

save_latency_report(Report) ->
    %% Save latency test report
    ReportFile = "/Users/sac/erlmcp/load_test_reports/latency_test_report.json",
    ok = file:write_file(ReportFile, jsx:encode(Report)).

next_scenario(State) ->
    %% Move to next scenario
    Scenarios = ?config(test_scenarios, State#state.config),
    CurrentIndex = State#state.scenario_index,

    if CurrentIndex < length(Scenarios) ->
        NewIndex = CurrentIndex + 1,
        ScenarioTimeout = erlang:send_after(?config(sampling_rate, State#state.config) *
                                        ?config(sample_count, lists:nth(NewIndex, Scenarios)),
                                        self(), scenario_timeout),
        State#state{scenario_index = NewIndex, active_scenarios = [lists:nth(NewIndex, Scenarios)]};
    true ->
        State#state{scenario_index = length(Scenarios) + 1, active_scenarios = []}
    end.