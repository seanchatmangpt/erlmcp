%%%-----------------------------------------------------------------------------
%%% @doc TCPS Kaizen Continuous Improvement Automation
%%%
%%% Implements Kaizen (continuous improvement) principles for the Toyota Code
%%% Production System. Collects metrics from receipts, identifies waste points,
%%% proposes improvements, and automates their application where possible.
%%%
%%% Core Capabilities:
%%% - Metrics collection from TCPS receipts (lead time, defect rate, rework)
%%% - Waste identification (compilation time, flaky tests, manual steps)
%%% - Improvement proposal generation with benefit quantification
%%% - Automated improvement application (template updates, constraints)
%%% - Trend analysis for week-over-week tracking
%%% - Weekly Kaizen report generation
%%%
%%% Target: 5% improvement per week across all metrics
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_kaizen).

-compile({no_auto_import, [atom_to_binary/1]}).

%% Public API
-export([
    collect_metrics/1,
    identify_waste_points/0,
    identify_waste_points/1,
    propose_improvements/1,
    apply_improvement/1,
    analyze_trends/1,
    generate_weekly_report/1,
    get_metric_history/2,
    record_receipt/1,
    get_current_metrics/0
]).

%% Types
-type date() :: calendar:date().
-type datetime() :: calendar:datetime().
-type time_period() :: {StartDate :: date(), EndDate :: date()}.
-type stage() :: atom().
-type waste_type() :: compilation_slow | flaky_test | manual_intervention |
                     repeat_andon | long_lead_time | excessive_rework.
-type improvement_status() :: proposed | applied | measuring | rejected.

-type metrics() :: #{
    lead_time => float(),              % Hours from work_order to SKU published
    defect_rate => float(),            % Andon events per 100 SKUs
    rework_pct => float(),             % Percentage of SKUs requiring fixes
    cycle_time => float(),             % Average time per stage
    first_pass_yield => float(),       % SKUs passing without rework
    throughput => float()              % SKUs per day
}.

-type waste_point() :: #{
    stage => stage(),
    waste_type => waste_type(),
    impact => float(),                 % Hours wasted per occurrence
    frequency => integer(),            % Occurrences per week
    total_waste => float(),            % Total hours wasted per week
    examples => [binary()],            % Receipt IDs showing this waste
    root_cause => binary()
}.

-type improvement() :: #{
    id => binary(),
    description => binary(),
    expected_benefit => binary(),      % Quantified benefit
    waste_addressed => [waste_point()],
    implementation_effort => low | medium | high,
    status => improvement_status(),
    created_at => datetime(),
    applied_at => datetime() | undefined,
    actual_benefit => float() | undefined
}.

-type trend_analysis() :: #{
    metric => atom(),
    improving => boolean(),
    rate_of_improvement => float(),    % Percentage per week
    weeks_analyzed => integer(),
    current_value => float(),
    target_value => float(),
    on_track => boolean()
}.

-type receipt() :: #{
    id => binary(),
    type => work_order | sku_generated | sku_published | andon_event | test_run,
    timestamp => datetime(),
    stage => stage(),
    duration => float() | undefined,   % Seconds
    work_order_id => binary() | undefined,
    sku_id => binary() | undefined,
    success => boolean(),
    metadata => map()
}.

-export_type([
    metrics/0,
    waste_point/0,
    improvement/0,
    trend_analysis/0,
    receipt/0,
    time_period/0
]).

%%%=============================================================================
%%% Public API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Collect metrics from TCPS receipts over a time period.
%%
%% Analyzes receipts to compute:
%% - Lead time: work_order created → SKU published
%% - Defect rate: Andon events / total SKUs
%% - Rework percentage: SKUs requiring fixes / total
%% - Cycle time: Average time per production stage
%% - First pass yield: SKUs passing without rework
%% - Throughput: SKUs produced per day
%%
%% @end
%%------------------------------------------------------------------------------
-spec collect_metrics(TimePeriod :: time_period()) -> metrics().
collect_metrics({StartDate, EndDate}) ->
    Receipts = load_receipts(StartDate, EndDate),

    %% Group receipts by type
    WorkOrders = filter_receipts(Receipts, work_order),
    SkusGenerated = filter_receipts(Receipts, sku_generated),
    SkusPublished = filter_receipts(Receipts, sku_published),
    AndonEvents = filter_receipts(Receipts, andon_event),

    %% Calculate metrics
    LeadTime = calculate_lead_time(WorkOrders, SkusPublished),
    DefectRate = calculate_defect_rate(AndonEvents, SkusPublished),
    ReworkPct = calculate_rework_percentage(Receipts),
    CycleTime = calculate_cycle_time(Receipts),
    FirstPassYield = calculate_first_pass_yield(Receipts),
    Throughput = calculate_throughput(SkusPublished, StartDate, EndDate),

    #{
        lead_time => LeadTime,
        defect_rate => DefectRate,
        rework_pct => ReworkPct,
        cycle_time => CycleTime,
        first_pass_yield => FirstPassYield,
        throughput => Throughput,
        collected_at => calendar:universal_time(),
        period => {StartDate, EndDate},
        sample_size => length(SkusPublished)
    }.

%%------------------------------------------------------------------------------
%% @doc Identify waste points in the production system.
%%
%% Analyzes current receipts to find where time/effort is wasted:
%% - Long compilation times (>threshold)
%% - Flaky tests (inconsistent results)
%% - Manual intervention points
%% - Repeat Andon events (same root cause)
%% - Long lead times in specific stages
%% - Excessive rework loops
%%
%% Returns list sorted by total_waste (highest impact first).
%% @end
%%------------------------------------------------------------------------------
-spec identify_waste_points() -> [waste_point()].
identify_waste_points() ->
    %% Default to last 7 days
    EndDate = calendar:universal_time(),
    StartDate = subtract_days(EndDate, 7),
    identify_waste_points({date_part(StartDate), date_part(EndDate)}).

-spec identify_waste_points(TimePeriod :: time_period()) -> [waste_point()].
identify_waste_points({StartDate, EndDate}) ->
    Receipts = load_receipts(StartDate, EndDate),

    WastePoints = lists:flatten([
        identify_compilation_waste(Receipts),
        identify_flaky_tests(Receipts),
        identify_manual_interventions(Receipts),
        identify_repeat_andons(Receipts),
        identify_stage_bottlenecks(Receipts),
        identify_rework_waste(Receipts)
    ]),

    %% Sort by total waste (impact * frequency)
    lists:sort(
        fun(#{total_waste := W1}, #{total_waste := W2}) -> W1 >= W2 end,
        WastePoints
    ).

%%------------------------------------------------------------------------------
%% @doc Generate improvement proposals from identified waste points.
%%
%% For each significant waste point, proposes actionable improvements with
%% quantified expected benefits. Examples:
%% - "Cache dependencies → reduce compile time 40%"
%% - "Add deterministic test seeds → eliminate flakes"
%% - "Automate X → eliminate manual step"
%%
%% @end
%%------------------------------------------------------------------------------
-spec propose_improvements(WastePoints :: [waste_point()]) -> [improvement()].
propose_improvements(WastePoints) ->
    %% Filter to significant waste (>=1 hour/week impact)
    SignificantWaste = lists:filter(
        fun(#{total_waste := Waste}) -> Waste >= 1.0 end,
        WastePoints
    ),

    %% Generate proposals for each waste type
    Proposals = lists:flatmap(
        fun(WastePoint) -> generate_proposals_for_waste(WastePoint) end,
        SignificantWaste
    ),

    %% Prioritize by ROI (benefit / effort)
    prioritize_improvements(Proposals).

%%------------------------------------------------------------------------------
%% @doc Apply an improvement automatically if possible.
%%
%% For automatable improvements:
%% - Update templates
%% - Add SHACL constraints
%% - Modify standard work definitions
%% - Update configuration
%%
%% Generates a receipt showing before/after metrics.
%%
%% Returns {ok, Receipt} if successfully applied, {error, Reason} otherwise.
%% @end
%%------------------------------------------------------------------------------
-spec apply_improvement(ImprovementId :: binary()) ->
    {ok, receipt()} | {error, term()}.
apply_improvement(ImprovementId) ->
    case lookup_improvement(ImprovementId) of
        {ok, Improvement} ->
            BeforeMetrics = get_current_metrics(),

            Result = case is_automatable(Improvement) of
                true ->
                    apply_automated_improvement(Improvement);
                false ->
                    {error, {manual_intervention_required,
                            "This improvement requires manual implementation"}}
            end,

            case Result of
                {ok, Changes} ->
                    AfterMetrics = get_current_metrics(),
                    Receipt = create_improvement_receipt(
                        Improvement, BeforeMetrics, AfterMetrics, Changes
                    ),
                    record_receipt(Receipt),
                    mark_improvement_applied(ImprovementId),
                    {ok, Receipt};
                {error, _} = Error ->
                    Error
            end;
        {error, not_found} ->
            {error, {improvement_not_found, ImprovementId}}
    end.

%%------------------------------------------------------------------------------
%% @doc Analyze trends to determine if metrics are improving over time.
%%
%% Tracks if metrics improving at target rate (5% per week).
%% Returns trend analysis for each metric with:
%% - Whether improving
%% - Rate of improvement (% per week)
%% - Whether on track to meet targets
%%
%% @end
%%------------------------------------------------------------------------------
-spec analyze_trends(MetricHistory :: [#{date => date(), metrics => metrics()}])
    -> #{atom() => trend_analysis()}.
analyze_trends(MetricHistory) when length(MetricHistory) < 2 ->
    #{error => insufficient_data, minimum_required => 2};

analyze_trends(MetricHistory) ->
    %% Sort by date
    Sorted = lists:sort(
        fun(#{date := D1}, #{date := D2}) -> D1 =< D2 end,
        MetricHistory
    ),

    %% Analyze each metric
    Metrics = [lead_time, defect_rate, rework_pct, cycle_time, first_pass_yield, throughput],

    maps:from_list([
        {Metric, analyze_metric_trend(Metric, Sorted)}
        || Metric <- Metrics
    ]).

%%------------------------------------------------------------------------------
%% @doc Generate comprehensive weekly Kaizen report.
%%
%% Includes:
%% - Current metrics vs targets
%% - Waste points identified (top 10)
%% - Improvements proposed and applied
%% - Trend graphs data
%% - Week-over-week comparison
%% - Recommendations for next week
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_weekly_report(WeekEndDate :: date()) -> map().
generate_weekly_report(WeekEndDate) ->
    WeekStartDate = subtract_days(WeekEndDate, 7),
    PrevWeekStartDate = subtract_days(WeekStartDate, 7),

    %% Current week metrics
    CurrentMetrics = collect_metrics({WeekStartDate, WeekEndDate}),
    PreviousMetrics = collect_metrics({PrevWeekStartDate, WeekStartDate}),

    %% Waste analysis
    WastePoints = identify_waste_points({WeekStartDate, WeekEndDate}),
    Top10Waste = lists:sublist(WastePoints, 10),

    %% Improvements
    ProposedImprovements = propose_improvements(WastePoints),
    AppliedImprovements = get_applied_improvements({WeekStartDate, WeekEndDate}),

    %% Trend analysis
    MetricHistory = get_metric_history(subtract_days(WeekEndDate, 56), WeekEndDate),
    Trends = analyze_trends(MetricHistory),

    %% Week-over-week changes
    WoWChanges = calculate_wow_changes(CurrentMetrics, PreviousMetrics),

    #{
        report_type => weekly_kaizen,
        week_ending => WeekEndDate,
        generated_at => calendar:universal_time(),

        %% Summary
        summary => #{
            current_metrics => CurrentMetrics,
            targets => get_target_metrics(),
            achievement => calculate_target_achievement(CurrentMetrics)
        },

        %% Waste analysis
        waste_analysis => #{
            total_waste_hours => sum_total_waste(WastePoints),
            top_waste_points => Top10Waste,
            waste_by_category => group_waste_by_category(WastePoints)
        },

        %% Improvements
        improvements => #{
            proposed_this_week => ProposedImprovements,
            applied_this_week => AppliedImprovements,
            total_proposed => length(ProposedImprovements),
            total_applied => length(AppliedImprovements),
            benefit_realized => sum_realized_benefits(AppliedImprovements)
        },

        %% Trends
        trends => Trends,

        %% Week-over-week
        week_over_week => WoWChanges,

        %% Recommendations
        recommendations => generate_recommendations(
            CurrentMetrics, WastePoints, Trends
        ),

        %% Visualization data
        charts => #{
            metrics_trend => format_metrics_for_chart(MetricHistory),
            waste_distribution => format_waste_for_chart(Top10Waste),
            improvement_pipeline => format_improvements_for_chart(
                ProposedImprovements, AppliedImprovements
            )
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Get metric history for a date range.
%% @end
%%------------------------------------------------------------------------------
-spec get_metric_history(StartDate :: date(), EndDate :: date()) ->
    [#{date => date(), metrics => metrics()}].
get_metric_history(StartDate, EndDate) ->
    %% Generate weekly snapshots
    Weeks = generate_week_boundaries(StartDate, EndDate),

    [#{
        date => WeekEnd,
        metrics => collect_metrics({WeekStart, WeekEnd})
    } || {WeekStart, WeekEnd} <- Weeks].

%%------------------------------------------------------------------------------
%% @doc Record a receipt in the system.
%% @end
%%------------------------------------------------------------------------------
-spec record_receipt(Receipt :: receipt()) -> ok.
record_receipt(Receipt) ->
    %% In production, this would persist to storage
    %% For now, we'll use ETS or similar
    ReceiptId = maps:get(id, Receipt, generate_receipt_id()),
    ReceiptWithId = Receipt#{id => ReceiptId},

    %% Store in ETS table (created on module init)
    ensure_receipt_table(),
    ets:insert(tcps_receipts, {ReceiptId, ReceiptWithId}),
    ok.

%%------------------------------------------------------------------------------
%% @doc Get current metrics (last 24 hours).
%% @end
%%------------------------------------------------------------------------------
-spec get_current_metrics() -> metrics().
get_current_metrics() ->
    Now = calendar:universal_time(),
    Yesterday = subtract_days(Now, 1),
    collect_metrics({date_part(Yesterday), date_part(Now)}).

%%%=============================================================================
%%% Internal Functions - Metrics Calculation
%%%=============================================================================

calculate_lead_time(WorkOrders, SkusPublished) ->
    %% Match work orders to published SKUs and compute time difference
    LeadTimes = lists:filtermap(
        fun(#{work_order_id := WOId, timestamp := PublishTime}) ->
            case find_work_order(WOId, WorkOrders) of
                {ok, #{timestamp := CreateTime}} ->
                    {true, time_diff_hours(CreateTime, PublishTime)};
                _ ->
                    false
            end
        end,
        SkusPublished
    ),

    case LeadTimes of
        [] -> 0.0;
        _ -> average(LeadTimes)
    end.

calculate_defect_rate(AndonEvents, SkusPublished) ->
    case length(SkusPublished) of
        0 -> 0.0;
        N -> (length(AndonEvents) / N) * 100.0
    end.

calculate_rework_percentage(Receipts) ->
    %% SKUs that required fixes / total SKUs
    SkuIds = extract_unique_sku_ids(Receipts),

    ReworkCount = length([
        Id || Id <- SkuIds,
        requires_rework(Id, Receipts)
    ]),

    case length(SkuIds) of
        0 -> 0.0;
        N -> (ReworkCount / N) * 100.0
    end.

calculate_cycle_time(Receipts) ->
    %% Average duration across all stages
    Durations = [D || #{duration := D} <- Receipts, D =/= undefined],
    case Durations of
        [] -> 0.0;
        _ -> average(Durations) / 3600.0  % Convert to hours
    end.

calculate_first_pass_yield(Receipts) ->
    %% Percentage of SKUs passing without rework
    100.0 - calculate_rework_percentage(Receipts).

calculate_throughput(SkusPublished, StartDate, EndDate) ->
    DaysDiff = calendar:date_to_gregorian_days(EndDate) -
               calendar:date_to_gregorian_days(StartDate),
    case DaysDiff of
        0 -> 0.0;
        N -> length(SkusPublished) / N
    end.

%%%=============================================================================
%%% Internal Functions - Waste Identification
%%%=============================================================================

identify_compilation_waste(Receipts) ->
    CompileReceipts = filter_by_stage(Receipts, compile),

    %% Find compilations exceeding threshold (5 minutes)
    Threshold = 300.0,  % seconds
    SlowCompiles = [R || #{duration := D} = R <- CompileReceipts,
                         D =/= undefined, D > Threshold],

    case SlowCompiles of
        [] -> [];
        _ ->
            AvgWaste = average([D - Threshold || #{duration := D} <- SlowCompiles]) / 3600.0,
            [#{
                stage => compile,
                waste_type => compilation_slow,
                impact => AvgWaste,
                frequency => length(SlowCompiles),
                total_waste => AvgWaste * length(SlowCompiles),
                examples => [maps:get(id, R) || R <- lists:sublist(SlowCompiles, 5)],
                root_cause => <<"Compilation time exceeds 5-minute threshold">>
            }]
    end.

identify_flaky_tests(Receipts) ->
    %% Find tests that fail intermittently
    TestReceipts = filter_by_stage(Receipts, test),

    %% Group by test name, look for inconsistent results
    TestsByName = group_receipts_by(fun get_test_name/1, TestReceipts),

    FlakyTests = maps:fold(
        fun(TestName, Tests, Acc) ->
            SuccessCount = length([1 || #{success := true} <- Tests]),
            FailCount = length([1 || #{success := false} <- Tests]),

            %% If both successes and failures, it's flaky
            if
                SuccessCount > 0 andalso FailCount > 0 ->
                    [{TestName, Tests} | Acc];
                true ->
                    Acc
            end
        end,
        [],
        TestsByName
    ),

    case FlakyTests of
        [] -> [];
        _ ->
            [#{
                stage => test,
                waste_type => flaky_test,
                impact => 0.25,  % 15 minutes per flake
                frequency => length(FlakyTests),
                total_waste => 0.25 * length(FlakyTests),
                examples => [Name || {Name, _} <- lists:sublist(FlakyTests, 5)],
                root_cause => <<"Tests with inconsistent results indicate non-determinism">>
            }]
    end.

identify_manual_interventions(Receipts) ->
    %% Find receipts marked as manual intervention
    ManualReceipts = [R || #{metadata := Meta} = R <- Receipts,
                          maps:get(manual_intervention, Meta, false)],

    case ManualReceipts of
        [] -> [];
        _ ->
            [#{
                stage => manual,
                waste_type => manual_intervention,
                impact => 1.0,  % 1 hour per manual step
                frequency => length(ManualReceipts),
                total_waste => 1.0 * length(ManualReceipts),
                examples => [maps:get(id, R) || R <- lists:sublist(ManualReceipts, 5)],
                root_cause => <<"Process requires manual human intervention">>
            }]
    end.

identify_repeat_andons(Receipts) ->
    AndonEvents = filter_receipts(Receipts, andon_event),

    %% Group by root cause
    ByRootCause = group_receipts_by(
        fun(#{metadata := Meta}) -> maps:get(root_cause, Meta, unknown) end,
        AndonEvents
    ),

    %% Find causes appearing multiple times
    RepeatCauses = maps:fold(
        fun(Cause, Events, Acc) when length(Events) > 1 ->
                [#{
                    stage => andon,
                    waste_type => repeat_andon,
                    impact => 2.0,  % 2 hours per Andon
                    frequency => length(Events),
                    total_waste => 2.0 * length(Events),
                    examples => [maps:get(id, E) || E <- lists:sublist(Events, 5)],
                    root_cause => iolist_to_binary([
                        <<"Repeated Andon events from same root cause: ">>,
                        ensure_binary(Cause)
                    ])
                } | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        ByRootCause
    ),

    RepeatCauses.

identify_stage_bottlenecks(Receipts) ->
    %% Find stages with long average durations
    ByStage = group_receipts_by(fun(#{stage := S}) -> S end, Receipts),

    Bottlenecks = maps:fold(
        fun(Stage, StageReceipts, Acc) ->
            Durations = [D || #{duration := D} <- StageReceipts, D =/= undefined],
            case Durations of
                [] -> Acc;
                _ ->
                    AvgDuration = average(Durations) / 3600.0,
                    %% Threshold: 1 hour average
                    if
                        AvgDuration > 1.0 ->
                            [#{
                                stage => Stage,
                                waste_type => long_lead_time,
                                impact => AvgDuration - 1.0,
                                frequency => length(StageReceipts),
                                total_waste => (AvgDuration - 1.0) * length(StageReceipts),
                                examples => [maps:get(id, R) || R <- lists:sublist(StageReceipts, 5)],
                                root_cause => iolist_to_binary([
                                    <<"Stage ">>, atom_to_binary(Stage),
                                    <<" exceeds 1-hour average">>
                                ])
                            } | Acc];
                        true ->
                            Acc
                    end
            end
        end,
        [],
        ByStage
    ),

    Bottlenecks.

identify_rework_waste(Receipts) ->
    %% Calculate waste from rework loops
    SkuIds = extract_unique_sku_ids(Receipts),

    ReworkWaste = lists:filtermap(
        fun(SkuId) ->
            SkuReceipts = [R || #{sku_id := Id} = R <- Receipts, Id =:= SkuId],
            ReworkCount = count_rework_cycles(SkuReceipts),

            if
                ReworkCount > 0 ->
                    {true, #{
                        sku_id => SkuId,
                        rework_cycles => ReworkCount,
                        waste_hours => ReworkCount * 2.0  % 2 hours per rework
                    }};
                true ->
                    false
            end
        end,
        SkuIds
    ),

    case ReworkWaste of
        [] -> [];
        _ ->
            TotalWaste = sum([W || #{waste_hours := W} <- ReworkWaste]),
            [#{
                stage => rework,
                waste_type => excessive_rework,
                impact => average([W || #{waste_hours := W} <- ReworkWaste]),
                frequency => length(ReworkWaste),
                total_waste => TotalWaste,
                examples => [Id || #{sku_id := Id} <- lists:sublist(ReworkWaste, 5)],
                root_cause => <<"SKUs requiring multiple rework cycles">>
            }]
    end.

%%%=============================================================================
%%% Internal Functions - Improvement Proposals
%%%=============================================================================

generate_proposals_for_waste(#{waste_type := compilation_slow} = Waste) ->
    #{total_waste := TotalWaste} = Waste,
    ExpectedReduction = 0.4,  % 40% reduction

    [#{
        id => generate_improvement_id(),
        description => <<"Cache compilation dependencies">>,
        expected_benefit => iolist_to_binary(io_lib:format(
            "Reduce compilation time by ~w%, saving ~.1f hours/week",
            [round(ExpectedReduction * 100), TotalWaste * ExpectedReduction]
        )),
        waste_addressed => [Waste],
        implementation_effort => medium,
        status => proposed,
        created_at => calendar:universal_time(),
        applied_at => undefined,
        actual_benefit => undefined,
        roi => 0.0  % Will be calculated by prioritize_improvements
    }];

generate_proposals_for_waste(#{waste_type := flaky_test} = Waste) ->
    [#{
        id => generate_improvement_id(),
        description => <<"Add deterministic test seeds and isolation">>,
        expected_benefit => <<"Eliminate flaky tests, saving ",
                             (format_waste(Waste))/binary, " hours/week">>,
        waste_addressed => [Waste],
        implementation_effort => low,
        status => proposed,
        created_at => calendar:universal_time(),
        applied_at => undefined,
        actual_benefit => undefined,
        roi => 0.0
    }];

generate_proposals_for_waste(#{waste_type := manual_intervention, stage := Stage} = Waste) ->
    #{total_waste := TotalWaste} = Waste,
    [#{
        id => generate_improvement_id(),
        description => iolist_to_binary([
            <<"Automate ">>, atom_to_binary(Stage), <<" stage">>
        ]),
        expected_benefit => iolist_to_binary(io_lib:format(
            "Eliminate manual intervention, saving ~.1f hours/week",
            [TotalWaste]
        )),
        waste_addressed => [Waste],
        implementation_effort => high,
        status => proposed,
        created_at => calendar:universal_time(),
        applied_at => undefined,
        actual_benefit => undefined,
        roi => 0.0
    }];

generate_proposals_for_waste(#{waste_type := repeat_andon, root_cause := Cause} = Waste) ->
    #{total_waste := TotalWaste} = Waste,
    [#{
        id => generate_improvement_id(),
        description => iolist_to_binary([
            <<"Add SHACL constraint to prevent: ">>, Cause
        ]),
        expected_benefit => iolist_to_binary(io_lib:format(
            "Prevent repeat Andon events, saving ~.1f hours/week",
            [TotalWaste]
        )),
        waste_addressed => [Waste],
        implementation_effort => low,
        status => proposed,
        created_at => calendar:universal_time(),
        applied_at => undefined,
        actual_benefit => undefined,
        roi => 0.0
    }];

generate_proposals_for_waste(#{waste_type := long_lead_time, stage := Stage} = Waste) ->
    #{total_waste := TotalWaste} = Waste,
    [#{
        id => generate_improvement_id(),
        description => iolist_to_binary([
            <<"Optimize ">>, atom_to_binary(Stage), <<" stage performance">>
        ]),
        expected_benefit => iolist_to_binary(io_lib:format(
            "Reduce stage duration by 50%, saving ~.1f hours/week",
            [TotalWaste * 0.5]
        )),
        waste_addressed => [Waste],
        implementation_effort => medium,
        status => proposed,
        created_at => calendar:universal_time(),
        applied_at => undefined,
        actual_benefit => undefined,
        roi => 0.0
    }];

generate_proposals_for_waste(#{waste_type := excessive_rework} = Waste) ->
    #{total_waste := TotalWaste} = Waste,
    [#{
        id => generate_improvement_id(),
        description => <<"Improve first-pass quality checks">>,
        expected_benefit => iolist_to_binary(io_lib:format(
            "Reduce rework by 50%, saving ~.1f hours/week",
            [TotalWaste * 0.5]
        )),
        waste_addressed => [Waste],
        implementation_effort => medium,
        status => proposed,
        created_at => calendar:universal_time(),
        applied_at => undefined,
        actual_benefit => undefined,
        roi => 0.0
    }];
generate_proposals_for_waste(_Waste) ->
    %% Unknown waste type - no proposals
    [].

prioritize_improvements(Proposals) ->
    %% Calculate ROI for each proposal
    WithROI = [calculate_roi(P) || P <- Proposals],

    %% Sort by ROI (highest first)
    lists:sort(
        fun(#{roi := ROI1}, #{roi := ROI2}) -> ROI1 >= ROI2 end,
        WithROI
    ).

calculate_roi(#{waste_addressed := Waste, implementation_effort := Effort} = Proposal) ->
    TotalWaste = sum([maps:get(total_waste, W, 0.0) || W <- Waste]),
    EffortHours = effort_to_hours(Effort),

    ROI = case EffortHours of
        0 -> 0.0;
        _ -> TotalWaste / EffortHours
    end,

    Proposal#{roi => ROI}.

effort_to_hours(low) -> 4.0;
effort_to_hours(medium) -> 16.0;
effort_to_hours(high) -> 40.0.

%%%=============================================================================
%%% Internal Functions - Improvement Application
%%%=============================================================================

is_automatable(#{waste_addressed := Waste}) ->
    %% Check if all waste types can be automatically addressed
    AutomatableTypes = [flaky_test, repeat_andon],

    lists:all(
        fun(#{waste_type := Type}) -> lists:member(Type, AutomatableTypes) end,
        Waste
    ).

apply_automated_improvement(#{waste_addressed := Waste} = Improvement) ->
    Results = [apply_waste_fix(W) || W <- Waste],

    case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
        true ->
            Changes = [C || {ok, C} <- Results],
            {ok, Changes};
        false ->
            Errors = [E || {error, E} <- Results],
            {error, {partial_failure, Errors}}
    end.

apply_waste_fix(#{waste_type := flaky_test, examples := Examples}) ->
    %% Add deterministic seeds to flaky tests
    Updates = [
        add_test_seed(TestName) || TestName <- Examples
    ],
    {ok, #{
        type => test_fix,
        tests_updated => length(Updates),
        changes => Updates
    }};

apply_waste_fix(#{waste_type := repeat_andon, root_cause := Cause}) ->
    %% Add SHACL constraint
    Constraint = generate_shacl_constraint(Cause),
    {ok, #{
        type => shacl_constraint,
        constraint => Constraint,
        root_cause_prevented => Cause
    }};

apply_waste_fix(#{waste_type := Type}) ->
    {error, {not_automatable, Type}}.

add_test_seed(TestName) ->
    %% In production, this would update the test file
    #{
        test => TestName,
        change => <<"Added deterministic seed">>,
        seed_value => rand:uniform(1000000)
    }.

generate_shacl_constraint(Cause) ->
    %% Generate SHACL constraint based on root cause
    #{
        type => <<"sh:PropertyShape">>,
        message => Cause,
        severity => <<"sh:Violation">>,
        constraint => <<"Generated constraint to prevent: ", Cause/binary>>
    }.

create_improvement_receipt(Improvement, BeforeMetrics, AfterMetrics, Changes) ->
    #{
        id => generate_receipt_id(),
        type => improvement_applied,
        timestamp => calendar:universal_time(),
        stage => kaizen,
        improvement_id => maps:get(id, Improvement),
        description => maps:get(description, Improvement),
        before_metrics => BeforeMetrics,
        after_metrics => AfterMetrics,
        changes => Changes,
        success => true,
        metadata => #{
            benefit_realized => calculate_benefit_realized(BeforeMetrics, AfterMetrics)
        }
    }.

%%%=============================================================================
%%% Internal Functions - Trend Analysis
%%%=============================================================================

analyze_metric_trend(Metric, History) ->
    %% Extract values for this metric
    Values = [maps:get(Metric, maps:get(metrics, H)) || H <- History],
    Dates = [maps:get(date, H) || H <- History],

    %% Calculate improvement rate
    {FirstVal, LastVal} = {hd(Values), lists:last(Values)},
    WeeksSpanned = length(History) - 1,

    %% Lower is better for: lead_time, defect_rate, rework_pct, cycle_time
    %% Higher is better for: first_pass_yield, throughput
    BetterDirection = case Metric of
        M when M =:= first_pass_yield; M =:= throughput -> higher;
        _ -> lower
    end,

    Improving = case BetterDirection of
        lower -> LastVal < FirstVal;
        higher -> LastVal > FirstVal
    end,

    RateOfImprovement = case {WeeksSpanned, FirstVal} of
        {0, _} -> 0.0;
        {_, 0.0} -> 0.0;
        {Weeks, Base} ->
            PercentChange = ((LastVal - Base) / Base) * 100.0,
            case BetterDirection of
                lower -> -PercentChange / Weeks;  % Negative change is improvement
                higher -> PercentChange / Weeks
            end
    end,

    Target = get_target_for_metric(Metric),
    OnTrack = is_on_track(RateOfImprovement, 5.0),  % 5% per week target

    #{
        metric => Metric,
        improving => Improving,
        rate_of_improvement => RateOfImprovement,
        weeks_analyzed => WeeksSpanned + 1,
        current_value => LastVal,
        target_value => Target,
        on_track => OnTrack,
        history => lists:zip(Dates, Values)
    }.

is_on_track(ActualRate, TargetRate) when ActualRate >= TargetRate * 0.8 -> true;
is_on_track(_, _) -> false.

%%%=============================================================================
%%% Internal Functions - Report Generation
%%%=============================================================================

get_applied_improvements({StartDate, EndDate}) ->
    Receipts = load_receipts(StartDate, EndDate),
    [R || #{type := improvement_applied} = R <- Receipts].

calculate_wow_changes(Current, Previous) ->
    Metrics = [lead_time, defect_rate, rework_pct, cycle_time, first_pass_yield, throughput],

    maps:from_list([
        {M, #{
            current => maps:get(M, Current),
            previous => maps:get(M, Previous),
            change_pct => percent_change(maps:get(M, Previous), maps:get(M, Current)),
            improved => is_improved(M, maps:get(M, Previous), maps:get(M, Current))
        }}
        || M <- Metrics
    ]).

is_improved(Metric, Old, New) ->
    case Metric of
        M when M =:= first_pass_yield; M =:= throughput -> New > Old;
        _ -> New < Old
    end.

generate_recommendations(Metrics, WastePoints, Trends) ->
    Recs = [],

    %% Check if any metrics are declining
    DecliningMetrics = maps:fold(
        fun(M, #{improving := false}, Acc) -> [M | Acc];
           (_, _, Acc) -> Acc
        end,
        [],
        Trends
    ),

    Recs2 = case DecliningMetrics of
        [] -> Recs;
        _ -> [#{
            priority => high,
            category => metric_decline,
            message => iolist_to_binary([
                <<"Metrics declining: ">>,
                string:join([atom_to_list(M) || M <- DecliningMetrics], ", ")
            ]),
            action => <<"Investigate root causes and implement countermeasures">>
        } | Recs]
    end,

    %% Check for high-impact waste
    HighImpactWaste = [W || #{total_waste := TW} = W <- WastePoints, TW > 10.0],

    Recs3 = case HighImpactWaste of
        [] -> Recs2;
        [TopWaste | _] ->
            [#{
                priority => high,
                category => high_waste,
                message => iolist_to_binary([
                    <<"High-impact waste: ">>,
                    atom_to_binary(maps:get(waste_type, TopWaste)),
                    <<" (">>,
                    io_lib:format("~.1f hrs/week", [maps:get(total_waste, TopWaste)]),
                    <<")">>
                ]),
                action => <<"Address this waste point as highest priority">>
            } | Recs2]
    end,

    %% Check throughput vs target
    #{throughput := Throughput} = Metrics,
    TargetThroughput = get_target_for_metric(throughput),

    Recs4 = if
        Throughput < TargetThroughput * 0.9 ->
            [#{
                priority => medium,
                category => throughput,
                message => <<"Throughput below target (90%)">>,
                action => <<"Review WIP limits and stage parallelization">>
            } | Recs3];
        true ->
            Recs3
    end,

    lists:reverse(Recs4).

sum_total_waste(WastePoints) ->
    sum([maps:get(total_waste, W) || W <- WastePoints]).

group_waste_by_category(WastePoints) ->
    ByType = lists:foldl(
        fun(#{waste_type := Type, total_waste := Waste} = WP, Acc) ->
            Existing = maps:get(Type, Acc, []),
            Acc#{Type => [WP | Existing]}
        end,
        #{},
        WastePoints
    ),

    maps:map(
        fun(_Type, Wastes) ->
            #{
                count => length(Wastes),
                total_waste => sum([maps:get(total_waste, W) || W <- Wastes]),
                examples => lists:sublist(Wastes, 3)
            }
        end,
        ByType
    ).

sum_realized_benefits(AppliedImprovements) ->
    sum([
        maps:get(benefit, maps:get(metadata, R, #{}), 0.0)
        || R <- AppliedImprovements
    ]).

format_metrics_for_chart(History) ->
    [#{
        date => format_date(Date),
        metrics => Metrics
    } || #{date := Date, metrics := Metrics} <- History].

format_waste_for_chart(WastePoints) ->
    [#{
        category => atom_to_binary(maps:get(waste_type, W)),
        stage => atom_to_binary(maps:get(stage, W)),
        waste_hours => maps:get(total_waste, W),
        frequency => maps:get(frequency, W)
    } || W <- WastePoints].

format_improvements_for_chart(Proposed, Applied) ->
    #{
        proposed => length(Proposed),
        applied => length(Applied),
        in_progress => length(Proposed) - length(Applied),
        by_effort => #{
            low => length([1 || #{implementation_effort := low} <- Proposed]),
            medium => length([1 || #{implementation_effort := medium} <- Proposed]),
            high => length([1 || #{implementation_effort := high} <- Proposed])
        }
    }.

calculate_target_achievement(Metrics) ->
    Targets = get_target_metrics(),

    %% Calculate achievement percentage for each metric
    Achievements = maps:fold(
        fun(Metric, TargetVal, Acc) ->
            CurrentVal = maps:get(Metric, Metrics, 0.0),
            Achievement = calculate_metric_achievement(Metric, CurrentVal, TargetVal),
            Acc#{Metric => Achievement}
        end,
        #{},
        Targets
    ),

    %% Overall achievement (average)
    OverallAchievement = average(maps:values(Achievements)),

    Achievements#{overall => OverallAchievement}.

calculate_metric_achievement(Metric, Current, Target) ->
    %% For metrics where lower is better
    LowerBetter = [lead_time, defect_rate, rework_pct, cycle_time],

    Achievement = case lists:member(Metric, LowerBetter) of
        true ->
            %% If current is at or below target, 100%
            %% Otherwise, proportional
            if
                Current =< Target -> 100.0;
                Target =:= 0.0 -> 0.0;
                true -> (Target / Current) * 100.0
            end;
        false ->
            %% Higher is better
            if
                Current >= Target -> 100.0;
                Target =:= 0.0 -> 0.0;
                true -> (Current / Target) * 100.0
            end
    end,

    min(Achievement, 100.0).

%%%=============================================================================
%%% Internal Functions - Data Access
%%%=============================================================================

load_receipts(StartDate, EndDate) ->
    %% Load from ETS or persistent storage
    ensure_receipt_table(),

    StartDateTime = {StartDate, {0, 0, 0}},
    EndDateTime = {EndDate, {23, 59, 59}},

    AllReceipts = ets:tab2list(tcps_receipts),

    [Receipt || {_Id, Receipt} <- AllReceipts,
                in_date_range(maps:get(timestamp, Receipt), StartDateTime, EndDateTime)].

filter_receipts(Receipts, Type) ->
    [R || #{type := T} = R <- Receipts, T =:= Type].

filter_by_stage(Receipts, Stage) ->
    [R || #{stage := S} = R <- Receipts, S =:= Stage].

find_work_order(WOId, WorkOrders) ->
    case [WO || #{work_order_id := Id} = WO <- WorkOrders, Id =:= WOId] of
        [WO | _] -> {ok, WO};
        [] -> {error, not_found}
    end.

extract_unique_sku_ids(Receipts) ->
    SkuIds = [Id || #{sku_id := Id} <- Receipts, Id =/= undefined],
    lists:usort(SkuIds).

requires_rework(SkuId, Receipts) ->
    SkuReceipts = [R || #{sku_id := Id} = R <- Receipts, Id =:= SkuId],
    count_rework_cycles(SkuReceipts) > 0.

count_rework_cycles(SkuReceipts) ->
    %% Count how many times SKU went through fix cycle
    FixReceipts = [R || #{metadata := Meta} = R <- SkuReceipts,
                       maps:get(is_fix, Meta, false)],
    length(FixReceipts).

group_receipts_by(KeyFun, Receipts) ->
    lists:foldl(
        fun(Receipt, Acc) ->
            Key = KeyFun(Receipt),
            Existing = maps:get(Key, Acc, []),
            Acc#{Key => [Receipt | Existing]}
        end,
        #{},
        Receipts
    ).

get_test_name(#{metadata := Meta}) ->
    maps:get(test_name, Meta, <<"unknown">>);
get_test_name(_) ->
    <<"unknown">>.

lookup_improvement(ImprovementId) ->
    %% In production, look up from storage
    %% For now, return error
    {error, not_found}.

mark_improvement_applied(ImprovementId) ->
    %% Mark in storage
    ok.

%%%=============================================================================
%%% Internal Functions - Utilities
%%%=============================================================================

ensure_receipt_table() ->
    case ets:whereis(tcps_receipts) of
        undefined ->
            ets:new(tcps_receipts, [named_table, public, set, {keypos, 1}]);
        _ ->
            ok
    end.

generate_receipt_id() ->
    iolist_to_binary([
        <<"receipt_">>,
        integer_to_binary(erlang:system_time(microsecond))
    ]).

generate_improvement_id() ->
    iolist_to_binary([
        <<"improvement_">>,
        integer_to_binary(erlang:system_time(microsecond))
    ]).

time_diff_hours({Date1, Time1}, {Date2, Time2}) ->
    Seconds1 = calendar:datetime_to_gregorian_seconds({Date1, Time1}),
    Seconds2 = calendar:datetime_to_gregorian_seconds({Date2, Time2}),
    (Seconds2 - Seconds1) / 3600.0.

subtract_days({{Y, M, D}, Time}, Days) ->
    GregorianDays = calendar:date_to_gregorian_days({Y, M, D}),
    NewDate = calendar:gregorian_days_to_date(GregorianDays - Days),
    {NewDate, Time};
subtract_days({Y, M, D}, Days) when is_integer(Y), is_integer(M), is_integer(D) ->
    GregorianDays = calendar:date_to_gregorian_days({Y, M, D}),
    calendar:gregorian_days_to_date(GregorianDays - Days).

date_part({Date, _Time}) -> Date;
date_part(Date) when is_tuple(Date), tuple_size(Date) =:= 3 -> Date.

in_date_range(DateTime, StartDateTime, EndDateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    StartSeconds = calendar:datetime_to_gregorian_seconds(StartDateTime),
    EndSeconds = calendar:datetime_to_gregorian_seconds(EndDateTime),
    Seconds >= StartSeconds andalso Seconds =< EndSeconds.

generate_week_boundaries(StartDate, EndDate) ->
    %% Generate weekly periods
    Days = calendar:date_to_gregorian_days(EndDate) -
           calendar:date_to_gregorian_days(StartDate),
    Weeks = Days div 7,

    [begin
        WS = calendar:gregorian_days_to_date(
            calendar:date_to_gregorian_days(StartDate) + (W * 7)
        ),
        WE = calendar:gregorian_days_to_date(
            calendar:date_to_gregorian_days(StartDate) + ((W + 1) * 7) - 1
        ),
        {WS, min_date(WE, EndDate)}
     end || W <- lists:seq(0, Weeks - 1)].

min_date(Date1, Date2) ->
    case calendar:date_to_gregorian_days(Date1) <
         calendar:date_to_gregorian_days(Date2) of
        true -> Date1;
        false -> Date2
    end.

average([]) -> 0.0;
average(List) -> sum(List) / length(List).

sum(List) -> lists:sum(List).

percent_change(_Old, _New) when _Old =:= +0.0 -> +0.0;
percent_change(Old, New) ->
    ((New - Old) / Old) * 100.0.

format_waste(#{total_waste := Waste}) ->
    iolist_to_binary(io_lib:format("~.1f", [Waste])).

format_date({Y, M, D}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D])).

ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Atom) when is_atom(Atom) -> erlang:atom_to_binary(Atom, utf8);
ensure_binary(List) when is_list(List) -> iolist_to_binary(List).

atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8).

get_target_metrics() ->
    #{
        lead_time => 2.0,          % 2 hours target
        defect_rate => 1.0,        % 1% defect rate
        rework_pct => 5.0,         % 5% rework
        cycle_time => 0.5,         % 30 minutes per stage
        first_pass_yield => 95.0,  % 95% first pass
        throughput => 10.0         % 10 SKUs per day
    }.

get_target_for_metric(Metric) ->
    maps:get(Metric, get_target_metrics(), 0.0).

calculate_benefit_realized(BeforeMetrics, AfterMetrics) ->
    %% Simple calculation: improvement in lead time
    Before = maps:get(lead_time, BeforeMetrics, 0.0),
    After = maps:get(lead_time, AfterMetrics, 0.0),
    max(0.0, Before - After).
