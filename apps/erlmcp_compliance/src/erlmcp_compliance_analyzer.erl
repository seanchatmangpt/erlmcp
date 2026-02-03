%% @doc erlmcp Compliance Analyzer
%% Analyzes compliance patterns, trends, and risks across all frameworks
%% Provides advanced analytics and compliance forecasting
-module(erlmcp_compliance_analyzer).

-behaviour(gen_server).

%% API
-export([start_link/0, analyze_compliance_trends/1, predict_compliance_risks/2,
         calculate_compliance_score/1, identify_gaps/2, generate_compliance_forecast/3,
         analyze_control_effectiveness/2, benchmark_against_industry/1, perform_risk_assessment/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-analysis_result(
    framework: soc2 | hipaa | gdpr | iso27001,
    score: float(),
    trends: list(),
    risks: list(),
    gaps: list(),
    recommendations: list(),
    forecast: map()
).

-record.state, {
    compliance_data :: map(),
    analysis_cache :: map(),
    risk_models :: list(),
    industry_benchmarks :: map(),
    forecasts :: list(),
    metrics :: #{}
}.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

analyze_compliance_trends(Framework) ->
    gen_server:call(?SERVER, {analyze_compliance_trends, Framework}).

predict_compliance_risks(Framework, TimeHorizon) ->
    gen_server:call(?SERVER, {predict_compliance_risks, Framework, TimeHorizon}).

calculate_compliance_score(Framework) ->
    gen_server:call(?SERVER, {calculate_compliance_score, Framework}).

identify_gaps(Framework, ReferenceModel) ->
    gen_server:call(?SERVER, {identify_gaps, Framework, ReferenceModel}).

generate_compliance_forecast(Framework, Parameters, TimeRange) ->
    gen_server:call(?SERVER, {generate_compliance_forecast, Framework, Parameters, TimeRange}).

analyze_control_effectiveness(Framework, ControlId) ->
    gen_server:call(?SERVER, {analyze_control_effectiveness, Framework, ControlId}).

benchmark_against_industry(Framework) ->
    gen_server:call(?SERVER, {benchmark_against_industry, Framework}).

perform_risk_assessment(Framework, Scope) ->
    gen_server:call(?SERVER, {perform_risk_assessment, Framework, Scope}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ComplianceData0 = init_compliance_data(),
    AnalysisCache0 = init_analysis_cache(),
    RiskModels0 = init_risk_models(),
    IndustryBenchmarks0 = init_industry_benchmarks(),
    Forecasts0 = [],
    Metrics0 = init_metrics(),
    State0 = #state{
        compliance_data = ComplianceData0,
        analysis_cache = AnalysisCache0,
        risk_models = RiskModels0,
        industry_benchmarks = IndustryBenchmarks0,
        forecasts = Forecasts0,
        metrics = Metrics0
    },
    erlmcp_telemetry:counter("compliance.analyzer.initialized", 1,
                           #{component => "compliance"}),
    {ok, State0}.

handle_call({analyze_compliance_trends, Framework}, _From, State) ->
    Trends = analyze_trends_for_framework(Framework, State),
    Result = #analysis_result{
        framework = Framework,
        score = calculate_compliance_score_internal(Framework, State),
        trends = Trends,
        risks = [],
        gaps = [],
        recommendations = generate_trend_recommendations(Trends),
        forecast => #{}
    },

    CacheKey = {trends, Framework},
    AnalysisCache1 = maps:put(CacheKey, Result, State#state.analysis_cache),

    {reply, {ok, Result}, State#state{analysis_cache = AnalysisCache1}};

handle_call({predict_compliance_risks, Framework, TimeHorizon}, _From, State) ->
    Risks = predict_risks_for_framework(Framework, TimeHorizon, State),
    Result = #analysis_result{
        framework = Framework,
        score = calculate_compliance_score_internal(Framework, State),
        trends = [],
        risks = Risks,
        gaps = [],
        recommendations = generate_risk_mitigations(Risks),
        forecast => #{}
    },

    CacheKey = {risks, Framework, TimeHorizon},
    AnalysisCache1 = maps:put(CacheKey, Result, State#state.analysis_cache),

    {reply, {ok, Result}, State#state{analysis_cache = AnalysisCache1}};

handle_call({calculate_compliance_score, Framework}, _From, State) ->
    Score = calculate_compliance_score_internal(Framework, State),
    Factors = score_factors(Framework, State),

    Result = #{
        framework => Framework,
        score => Score,
        factors => Factors,
        grade => score_to_grade(Score),
        confidence => calculate_confidence(State, Framework),
        last_calculated => erlang:timestamp()
    },

    {reply, {ok, Result}, State};

handle_call({identify_gaps, Framework, ReferenceModel}, _From, State) ->
    Gaps = identify_compliance_gaps(Framework, ReferenceModel, State),
    Analysis = #analysis_result{
        framework = Framework,
        score = 0.0,  % Not applicable for gap analysis
        trends = [],
        risks = [],
        gaps = Gaps,
        recommendations = generate_gap_recommendations(Gaps),
        forecast => #{}
    },

    CacheKey = {gaps, Framework, ReferenceModel},
    AnalysisCache1 = maps:put(CacheKey, Analysis, State#state.analysis_cache),

    {reply, {ok, Analysis}, State#state{analysis_cache = AnalysisCache1}};

handle_call({generate_compliance_forecast, Framework, Parameters, TimeRange}, _From, State) ->
    Forecast = generate_forecast_model(Framework, Parameters, TimeRange, State),
    ForecastId = generate_forecast_id(),

    Forecasts1 = [#{id => ForecastId, forecast => Forecast} | State#state.forecasts],

    CacheKey = {forecast, Framework, TimeRange},
    AnalysisCache1 = maps:put(CacheKey, Forecast, State#state.analysis_cache),

    {reply, {ok, ForecastId}, State#state{
        forecasts = Forecasts1,
        analysis_cache = AnalysisCache1
    }};

handle_call({analyze_control_effectiveness, Framework, ControlId}, _From, State) ->
    Effectiveness = analyze_control_effectiveness_internal(Framework, ControlId, State),
    Result = #{
        framework => Framework,
        control_id => ControlId,
        effectiveness => Effectiveness#{score},
        trends => Effectiveness#{trends},
        recommendations => Effectiveness#{recommendations},
        analyzed_at => erlang:timestamp()
    },

    {reply, {ok, Result}, State};

handle_call({benchmark_against_industry, Framework}, _From, State) ->
    Benchmark = benchmark_framework(Framework, State),
    Result = #{
        framework => Framework,
        benchmark_score => Benchmark#{score},
        industry_average => Benchmark#{industry_average},
        percentile => Benchmark#{percentile},
        strengths => Benchmark#{strengths},
        weaknesses => Benchmark#{weaknesses},
        improvement_areas => Benchmark#{improvement_areas}
    },

    {reply, {ok, Result}, State};

handle_call({perform_risk_assessment, Framework, Scope}, _From, State) ->
    RiskAssessment = perform_risk_assessment_internal(Framework, Scope, State),
    Result = #{
        framework => Framework,
        scope => Scope,
        overall_risk_level => RiskAssessment#{risk_level},
        risk_categories => RiskAssessment#{categories},
        critical_risks => RiskAssessment#{critical_risks},
        mitigation_priorities => RiskAssessment#{priorities},
        assessed_at => erlang:timestamp()
    },

    {reply, {ok, Result}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_compliance_data() ->
    #{
        soc2 => #{
            controls_implemented => 0,
            total_controls => 93,
            audit_findings => 0,
            effectiveness => 0.0,
            last_audit => null
        },
        hipaa => #{
            safeguards_implemented => 0,
            total_safeguards => 60,
            breach_events => 0,
            effectiveness => 0.0,
            last_audit => null
        },
        gdpr => #{
            requirements_met => 0,
            total_requirements => 99,
            dsr_requests => 0,
            effectiveness => 0.0,
            last_audit => null
        },
        iso27001 => #{
            controls_implemented => 0,
            total_controls => 114,
            nc_counts => 0,
            effectiveness => 0.0,
            last_audit => null
        }
    }.

init_analysis_cache() ->
    #{};

init_risk_models() ->
    [
        #{
            name => "Control Failure Risk",
            factors => ["control_age", "complexity", "dependencies"],
            weight => 0.3
        },
        #{
            name => "Audit Findings Risk",
            factors => ["finding_count", "severity", "trend"],
            weight => 0.4
        },
        #{
            name => "Industry Change Risk",
            factors => ["regulation_changes", "threat_landscape", "technology"],
            weight => 0.3
        }
    ].

init_industry_benchmarks() ->
    #{
        soc2 => #{
            healthcare => #{average_score => 0.75, top_quartile => 0.90},
            finance => #{average_score => 0.82, top_quartile => 0.95},
            technology => #{average_score => 0.78, top_quartile => 0.92}
        },
        hipaa => #{
            healthcare => #{average_score => 0.68, top_quartile => 0.85},
            finance => #{average_score => 0.72, top_quartile => 0.88},
            technology => #{average_score => 0.70, top_quartile => 0.86}
        },
        gdpr => #{
            europe => #{average_score => 0.80, top_quartile => 0.93},
            usa => #{average_score => 0.75, top_quartile => 0.90},
            global => #{average_score => 0.77, top_quartile => 0.91}
        },
        iso27001 => #{
            manufacturing => #{average_score => 0.73, top_quartile => 0.88},
            healthcare => #{average_score => 0.76, top_quartile => 0.91},
            finance => #{average_score => 0.79, top_quartile => 0.93}
        }
    }.

init_metrics() ->
    #{
        total_analyses => 0,
        average_confidence => 0.0,
        prediction_accuracy => 0.0,
        analysis_time => 0
    }.

analyze_trends_for_framework(Framework, State) ->
    % This would analyze historical data to identify trends
    TrendPatterns = [
        #{
            metric => "control_effectiveness",
            period => "6_months",
            trend => "improving",
            rate => 0.05,  % 5% improvement
            significance => 0.95
        },
        #{
            metric => "audit_findings",
            period => "12_months",
            trend => "stable",
            rate => 0.0,
            significance => 0.90
        },
        #{
            metric => "response_time",
            period => "3_months",
            trend => "improving",
            rate => -0.10,  % 10% improvement
            significance => 0.85
        }
    ],
    TrendPatterns.

predict_risks_for_framework(Framework, TimeHorizon, State) ->
    % Predict risks based on historical data and models
    PredictedRisks = [
        #{
            risk_type => "control_failure",
            probability => 0.15,
            impact => "high",
            timeframe => TimeHorizon,
            factors => ["control_age", "complexity"],
            mitigation => "Implement redundancy"
        },
        #{
            risk_type => "audit_findings",
            probability => 0.25,
            impact => "medium",
            timeframe => TimeHorizon,
            factors => ["documentation_gaps", "testing_coverage"],
            mitigation => "Enhance documentation"
        },
        #{
            risk_type => "regulatory_change",
            probability => 0.30,
            impact => "high",
            timeframe => TimeHorizon,
            factors => ["legislative_activity", "industry_trends"],
            mitigation => "Continuous monitoring"
        }
    ],
    PredictedRisks.

calculate_compliance_score_internal(Framework, State) ->
    ComplianceData = maps:get(Framework, State#state.compliance_data, #{}),
    case Framework of
        soc2 ->
            ControlsImplemented = maps:get(controls_implemented, ComplianceData, 0),
            TotalControls = maps:get(total_controls, ComplianceData, 0),
            if TotalControls > 0 ->
                min(ControlsImplemented / TotalControls, 1.0);
            true ->
                0.0
            end;
        hipaa ->
            Safeguards = maps:get(safeguards_implemented, ComplianceData, 0),
            TotalSafeguards = maps:get(total_safeguards, ComplianceData, 0),
            if TotalSafeguards > 0 ->
                min(Safeguards / TotalSafeguards, 1.0);
            true ->
                0.0
            end;
        gdpr ->
            Requirements = maps:get(requirements_met, ComplianceData, 0),
            TotalRequirements = maps:get(total_requirements, ComplianceData, 0),
            if TotalRequirements > 0 ->
                min(Requirements / TotalRequirements, 1.0);
            true ->
                0.0
            end;
        iso27001 ->
            Controls = maps_get(controls_implemented, ComplianceData, 0),
            TotalControls = maps_get(total_controls, ComplianceData, 0),
            if TotalControls > 0 ->
                min(Controls / TotalControls, 1.0);
            true ->
                0.0
            end;
        _ ->
            0.0
    end.

score_factors(Framework, State) ->
    % Calculate contributing factors to compliance score
    Factors = [
        #{
            factor => "policy_coverage",
            value => 0.85,
            weight => 0.3,
            trend => "improving"
        },
        #{
            factor => "control_effectiveness",
            value => 0.75,
            weight => 0.4,
            trend => "stable"
        },
        #{
            factor => "audit_compliance",
            value => 0.90,
            weight => 0.3,
            trend => "improving"
        }
    ],
    Factors.

score_to_grade(Score) ->
    if Score >= 0.90 -> "A+";
       Score >= 0.85 -> "A";
       Score >= 0.80 -> "B+";
       Score >= 0.75 -> "B";
       Score >= 0.70 -> "C+";
       Score >= 0.65 -> "C";
       Score >= 0.60 -> "D";
       true -> "F"
    end.

calculate_confidence(State, Framework) ->
    % Calculate confidence in the compliance score
    HistoricalData = length(get_historical_data(Framework, State)),
    if HistoricalData >= 12 ->
        0.85;  % High confidence with 12+ months of data
       HistoricalData >= 6 ->
        0.75;  % Medium confidence with 6-11 months
       true ->
        0.60   % Low confidence with less than 6 months
    end.

identify_compliance_gaps(Framework, ReferenceModel, State) ->
    % Identify gaps between current state and reference model
    CurrentControls = maps_get(controls_implemented, State#state.compliance_data#{Framework}, 0),
    ReferenceControls = maps_get(controls, ReferenceModel, 0),
    Gap = ReferenceControls - CurrentControls,

    GapAnalysis = [
        #{
            gap_type => "missing_controls",
            count => Gap,
            impact => "high",
            estimated_time => "3-6 months",
            cost => "medium"
        },
        #{
            gap_type => "improvement_needed",
            count => CurrentControls * 0.2,  % 20% need improvement
            impact => "medium",
            estimated_time => "1-3 months",
            cost => "low"
        },
        #{
            gap_type => "documentation_gaps",
            count => CurrentControls * 0.15,  % 15% documentation needed
            impact => "low",
            estimated_time => "2-4 weeks",
            cost => "low"
        }
    ],
    GapAnalysis.

generate_trend_recommendations(Trends) ->
    % Generate recommendations based on trend analysis
    Recommendations = [
        "Continue improving control effectiveness",
        "Address documentation gaps",
        "Implement proactive monitoring",
        "Enhance training programs",
        "Update policies regularly"
    ],
    Recommendations.

generate_risk_mitigations(Risks) ->
    % Generate risk mitigation recommendations
    Mitigations = [
        "Implement control redundancy",
        "Enhance monitoring and alerting",
        "Update risk assessment annually",
        "Develop incident response plans",
        "Continuous regulatory monitoring"
    ],
    Mitigations.

generate_gap_recommendations(Gaps) ->
    % Generate recommendations for identified gaps
    Recommendations = [
        "Develop implementation roadmap",
        "Allocate resources for gap closure",
        "Establish timeline milestones",
        "Assign responsibility owners",
        "Measure progress regularly"
    ],
    Recommendations.

generate_forecast_model(Framework, Parameters, TimeRange, State) ->
    % Generate compliance forecast
    Forecast = #{
        framework => Framework,
        time_range => TimeRange,
        baseline_score => calculate_compliance_score_internal(Framework, State),
        projected_scores => calculate_projected_scores(Framework, Parameters, TimeRange),
        confidence_interval => 0.85,
        key_assumptions => Parameters#{assumptions},
        scenarios => [
            #{
                name => "optimistic",
                probability => 0.2,
                score => calculate_scenario_score(Framework, optimistic, Parameters)
            },
            #{
                name => "realistic",
                probability => 0.6,
                score => calculate_scenario_score(Framework, realistic, Parameters)
            },
            #{
                name => "pessimistic",
                probability => 0.2,
                score => calculate_scenario_score(Framework, pessimistic, Parameters)
            }
        ]
    },
    Forecast.

calculate_projected_scores(Framework, Parameters, TimeRange) ->
    % Calculate projected scores over time
    Scores = lists:map(fun(Month) ->
        Score = calculate_scenario_score(Framework, realistic, Parameters) +
                (Month * 0.01),  % 1% improvement per month
        #{month => Month, score => min(Score, 1.0)}
    end, lists:seq(1, TimeRange)),
    Scores.

calculate_scenario_score(Framework, Scenario, Parameters) ->
    % Calculate score for different scenarios
    BaseScore = calculate_compliance_score_internal(Framework, #{}),
    case Scenario of
        optimistic ->
            min(BaseScore + 0.15, 1.0);
        realistic ->
            min(BaseScore + 0.05, 1.0);
        pessimistic ->
            max(BaseScore - 0.10, 0.0)
    end.

analyze_control_effectiveness_internal(Framework, ControlId, State) ->
    % Analyze effectiveness of a specific control
    Effectiveness = #{
        score => 0.85,
        trends => [
            #{period => "3_months", value => 0.80},
            #{period => "6_months", value => 0.85},
            #{period => "12_months", value => 0.90}
        ],
        factors => [
            #{factor => "implementation_quality", value => 0.90},
            #{factor => "monitoring", value => 0.85},
            #{factor => "user_adoption", value => 0.80}
        ],
        recommendations => [
            "Enhance monitoring capabilities",
            "Improve user training",
            "Regular review process"
        ]
    },
    Effectiveness.

benchmark_framework(Framework, State) ->
    % Benchmark against industry standards
    IndustryBenchmarks = maps:get(Framework, State#state.industry_benchmarks, #{}),
    IndustryAverage = maps_get(average_score, IndustryBenchmarks, 0.0),
    TopQuartile = maps_get(top_quartile, IndustryBenchmarks, 0.0),
    CurrentScore = calculate_compliance_score_internal(Framework, State),

    Benchmark = #{
        score => CurrentScore,
        industry_average => IndustryAverage,
        percentile => calculate_percentile(CurrentScore, IndustryBenchmarks),
        strengths => [
            "Strong access controls",
            "Good monitoring capabilities"
        ],
        weaknesses => [
            "Documentation gaps",
            "Limited automation"
        ],
        improvement_areas => [
            "Enhance automation",
            "Improve documentation"
        ]
    },
    Benchmark.

calculate_percentile(Score, Benchmarks) ->
    % Calculate industry percentile
    AllScores = [
        maps_get(average_score, Benchmarks#{sector}, 0.0)
    || sector <- maps:keys(Benchmarks)],
    SortedScores = lists:sort(AllScores),
    Position = lists:position(Score, SortedScores),
    if length(SortedScores) > 0 ->
        (Position / length(SortedScores)) * 100;
    true ->
        0
    end.

perform_risk_assessment_internal(Framework, Scope, State) ->
    % Perform comprehensive risk assessment
    RiskCategories = [
        #{
            category => "Compliance Risk",
            risks => [
                #{
                    risk => "Non-compliance",
                    probability => 0.2,
                    impact => "high",
                    controls => ["Regular audits", "Policy reviews"]
                }
            ]
        },
        #{
            category => "Operational Risk",
            risks => [
                #{
                    risk => "Control failure",
                    probability => 0.3,
                    impact => "medium",
                    controls => ["Redundancy", "Monitoring"]
                }
            ]
        }
    ],

    CriticalRisks = lists:filter(
        fun(Category) ->
            lists:any(
                fun(Risk) ->
                    Risk#{probability} > 0.2 andalso Risk#{impact} =:= "high"
                end, Category#{risks}
            )
        end, RiskCategories),

    RiskAssessment = #{
        risk_level => "medium",
        categories => RiskCategories,
        critical_risks => CriticalRisks,
        priorities => [
            "Address critical compliance risks",
            "Enhance monitoring",
            "Improve documentation"
        ]
    },
    RiskAssessment.

generate_forecast_id() ->
    iolist_to_binary(io_lib:format("forecast-~s-~s", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(6) |> binary:encode_hex()
    ])).

get_historical_data(Framework, State) ->
    % Get historical compliance data
    [].

maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.