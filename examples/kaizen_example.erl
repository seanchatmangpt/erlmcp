%%%-----------------------------------------------------------------------------
%%% @doc TCPS Kaizen Usage Examples
%%%
%%% Demonstrates how to use tcps_kaizen for continuous improvement.
%%%
%%% Run with:
%%%   rebar3 shell
%%%   c(examples/kaizen_example).
%%%   kaizen_example:run_all().
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kaizen_example).

-export([
    run_all/0,
    example_1_collect_metrics/0,
    example_2_identify_waste/0,
    example_3_propose_improvements/0,
    example_4_weekly_report/0,
    example_5_trend_analysis/0,
    simulate_production_week/0
]).

%%%=============================================================================
%%% Public API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Run all examples in sequence.
%% @end
%%------------------------------------------------------------------------------
run_all() ->
    io:format("~n=== TCPS Kaizen Examples ===~n~n"),

    %% Simulate a week of production
    io:format("1. Simulating production week...~n"),
    simulate_production_week(),

    %% Example 1: Collect metrics
    io:format("~n2. Collecting metrics...~n"),
    example_1_collect_metrics(),

    %% Example 2: Identify waste
    io:format("~n3. Identifying waste points...~n"),
    example_2_identify_waste(),

    %% Example 3: Propose improvements
    io:format("~n4. Proposing improvements...~n"),
    example_3_propose_improvements(),

    %% Example 4: Weekly report
    io:format("~n5. Generating weekly report...~n"),
    example_4_weekly_report(),

    %% Example 5: Trend analysis
    io:format("~n6. Analyzing trends...~n"),
    example_5_trend_analysis(),

    io:format("~n=== Examples complete ===~n"),
    ok.

%%------------------------------------------------------------------------------
%% @doc Example 1: Collect metrics from receipts.
%% @end
%%------------------------------------------------------------------------------
example_1_collect_metrics() ->
    %% Get last week's metrics
    Today = calendar:universal_time(),
    WeekAgo = subtract_days(Today, 7),

    Metrics = tcps_kaizen:collect_metrics({date_part(WeekAgo), date_part(Today)}),

    io:format("  Lead Time: ~.2f hours~n", [maps:get(lead_time, Metrics)]),
    io:format("  Defect Rate: ~.2f%~n", [maps:get(defect_rate, Metrics)]),
    io:format("  Rework: ~.2f%~n", [maps:get(rework_pct, Metrics)]),
    io:format("  Cycle Time: ~.2f hours~n", [maps:get(cycle_time, Metrics)]),
    io:format("  First Pass Yield: ~.2f%~n", [maps:get(first_pass_yield, Metrics)]),
    io:format("  Throughput: ~.2f SKUs/day~n", [maps:get(throughput, Metrics)]),

    %% Compare to targets
    Targets = get_targets(),
    Achievement = calculate_achievement(Metrics, Targets),
    io:format("  Overall Achievement: ~.1f%~n", [Achievement]),

    Metrics.

%%------------------------------------------------------------------------------
%% @doc Example 2: Identify waste points.
%% @end
%%------------------------------------------------------------------------------
example_2_identify_waste() ->
    Today = calendar:universal_time(),
    WeekAgo = subtract_days(Today, 7),

    WastePoints = tcps_kaizen:identify_waste_points(
        {date_part(WeekAgo), date_part(Today)}
    ),

    io:format("  Found ~p waste points~n", [length(WastePoints)]),

    %% Show top 5
    Top5 = lists:sublist(WastePoints, 5),
    io:format("~n  Top 5 Waste Points:~n"),
    lists:foreach(
        fun(#{waste_type := Type, total_waste := Waste, frequency := Freq}) ->
            io:format("    - ~s: ~.1f hrs/week (~p occurrences)~n",
                     [Type, Waste, Freq])
        end,
        Top5
    ),

    WastePoints.

%%------------------------------------------------------------------------------
%% @doc Example 3: Propose improvements.
%% @end
%%------------------------------------------------------------------------------
example_3_propose_improvements() ->
    Today = calendar:universal_time(),
    WeekAgo = subtract_days(Today, 7),

    WastePoints = tcps_kaizen:identify_waste_points(
        {date_part(WeekAgo), date_part(Today)}
    ),
    Proposals = tcps_kaizen:propose_improvements(WastePoints),

    io:format("  Generated ~p improvement proposals~n", [length(Proposals)]),

    %% Show top 3 by ROI
    Top3 = lists:sublist(Proposals, 3),
    io:format("~n  Top 3 Proposals (by ROI):~n"),
    lists:foreach(
        fun(#{description := Desc, expected_benefit := Benefit,
              implementation_effort := Effort, roi := ROI}) ->
            io:format("    - ~s~n", [Desc]),
            io:format("      Benefit: ~s~n", [Benefit]),
            io:format("      Effort: ~s, ROI: ~.2f~n", [Effort, ROI])
        end,
        Top3
    ),

    Proposals.

%%------------------------------------------------------------------------------
%% @doc Example 4: Generate weekly Kaizen report.
%% @end
%%------------------------------------------------------------------------------
example_4_weekly_report() ->
    Today = date_part(calendar:universal_time()),

    Report = tcps_kaizen:generate_weekly_report(Today),

    %% Summary
    #{summary := #{
        current_metrics := Metrics,
        achievement := Achievement
    }} = Report,

    io:format("  Week ending: ~p~n", [Today]),
    io:format("  Overall achievement: ~.1f%~n", [maps:get(overall, Achievement)]),

    %% Waste
    #{waste_analysis := #{
        total_waste_hours := TotalWaste,
        top_waste_points := TopWaste
    }} = Report,

    io:format("  Total waste: ~.1f hours~n", [TotalWaste]),
    io:format("  Top waste category: ~s (~.1f hrs)~n",
        case TopWaste of
            [#{waste_type := Type, total_waste := W} | _] ->
                [atom_to_list(Type), W];
            [] ->
                ["none", 0.0]
        end
    ),

    %% Improvements
    #{improvements := #{
        total_proposed := Proposed,
        total_applied := Applied,
        benefit_realized := Benefit
    }} = Report,

    io:format("  Improvements proposed: ~p~n", [Proposed]),
    io:format("  Improvements applied: ~p~n", [Applied]),
    io:format("  Benefit realized: ~.1f hours~n", [Benefit]),

    %% Recommendations
    #{recommendations := Recs} = Report,
    HighPriority = [R || #{priority := high} = R <- Recs],

    io:format("~n  High Priority Recommendations: ~p~n", [length(HighPriority)]),
    lists:foreach(
        fun(#{message := Msg, action := Action}) ->
            io:format("    - ~s~n", [Msg]),
            io:format("      Action: ~s~n", [Action])
        end,
        HighPriority
    ),

    Report.

%%------------------------------------------------------------------------------
%% @doc Example 5: Analyze trends over 8 weeks.
%% @end
%%------------------------------------------------------------------------------
example_5_trend_analysis() ->
    Today = date_part(calendar:universal_time()),
    EightWeeksAgo = subtract_days({Today, {0, 0, 0}}, 56),

    History = tcps_kaizen:get_metric_history(date_part(EightWeeksAgo), Today),
    Trends = tcps_kaizen:analyze_trends(History),

    io:format("  Analyzed ~p weeks of data~n", [length(History)]),
    io:format("~n  Trend Analysis:~n"),

    %% Show each metric's trend
    Metrics = [lead_time, defect_rate, rework_pct, cycle_time,
               first_pass_yield, throughput],

    lists:foreach(
        fun(Metric) ->
            case maps:get(Metric, Trends, #{}) of
                #{improving := Improving,
                  rate_of_improvement := Rate,
                  on_track := OnTrack} ->
                    Status = case {Improving, OnTrack} of
                        {true, true} -> "✓ Improving, on track";
                        {true, false} -> "↗ Improving, below target";
                        {false, _} -> "✗ Declining"
                    end,
                    io:format("    ~s: ~s (~.1f%/week)~n",
                             [Metric, Status, Rate]);
                _ ->
                    io:format("    ~s: No data~n", [Metric])
            end
        end,
        Metrics
    ),

    Trends.

%%------------------------------------------------------------------------------
%% @doc Simulate a week of TCPS production with receipts.
%% @end
%%------------------------------------------------------------------------------
simulate_production_week() ->
    %% Generate sample receipts for the past week
    Today = calendar:universal_time(),

    %% Day 1: Good production
    record_sku_lifecycle(Today, <<"sku_001">>, <<"wo_001">>, 1800, true),
    record_sku_lifecycle(Today, <<"sku_002">>, <<"wo_002">>, 2100, true),

    %% Day 2: Slow compilation
    Yesterday = subtract_days(Today, 1),
    record_sku_lifecycle(Yesterday, <<"sku_003">>, <<"wo_003">>, 3600, true),
    tcps_kaizen:record_receipt(#{
        id => <<"slow_compile_1">>,
        type => sku_generated,
        timestamp => Yesterday,
        stage => compile,
        duration => 420.0,  % 7 minutes
        sku_id => <<"sku_003">>,
        success => true,
        metadata => #{}
    }),

    %% Day 3: Flaky test
    TwoDaysAgo = subtract_days(Today, 2),
    record_sku_lifecycle(TwoDaysAgo, <<"sku_004">>, <<"wo_004">>, 2000, true),
    tcps_kaizen:record_receipt(#{
        id => <<"flaky_test_1">>,
        type => test_run,
        timestamp => TwoDaysAgo,
        stage => test,
        duration => 60.0,
        sku_id => <<"sku_004">>,
        success => false,
        metadata => #{test_name => <<"test_timing_sensitive">>}
    }),
    tcps_kaizen:record_receipt(#{
        id => <<"flaky_test_2">>,
        type => test_run,
        timestamp => TwoDaysAgo,
        stage => test,
        duration => 60.0,
        sku_id => <<"sku_004">>,
        success => true,
        metadata => #{test_name => <<"test_timing_sensitive">>}
    }),

    %% Day 4: Andon event
    ThreeDaysAgo = subtract_days(Today, 3),
    record_sku_lifecycle(ThreeDaysAgo, <<"sku_005">>, <<"wo_005">>, 1900, false),
    tcps_kaizen:record_receipt(#{
        id => <<"andon_1">>,
        type => andon_event,
        timestamp => ThreeDaysAgo,
        stage => validate,
        sku_id => <<"sku_005">>,
        success => false,
        metadata => #{
            root_cause => <<"SHACL validation failed: missing required property">>,
            severity => high
        }
    }),

    %% Day 5: Manual intervention
    FourDaysAgo = subtract_days(Today, 4),
    record_sku_lifecycle(FourDaysAgo, <<"sku_006">>, <<"wo_006">>, 2200, true),
    tcps_kaizen:record_receipt(#{
        id => <<"manual_1">>,
        type => sku_generated,
        timestamp => FourDaysAgo,
        stage => deploy,
        duration => 3600.0,
        sku_id => <<"sku_006">>,
        success => true,
        metadata => #{
            manual_intervention => true,
            reason => <<"Required security approval">>
        }
    }),

    %% Day 6-7: More good production
    FiveDaysAgo = subtract_days(Today, 5),
    SixDaysAgo = subtract_days(Today, 6),
    record_sku_lifecycle(FiveDaysAgo, <<"sku_007">>, <<"wo_007">>, 1950, true),
    record_sku_lifecycle(SixDaysAgo, <<"sku_008">>, <<"wo_008">>, 2050, true),

    io:format("  Simulated 8 SKUs over 7 days~n"),
    ok.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

record_sku_lifecycle(BaseTime, SkuId, WorkOrderId, Duration, Success) ->
    %% Work order
    tcps_kaizen:record_receipt(#{
        id => iolist_to_binary([<<"wo_receipt_">>, WorkOrderId]),
        type => work_order,
        timestamp => BaseTime,
        stage => intake,
        work_order_id => WorkOrderId,
        success => true,
        metadata => #{}
    }),

    %% SKU generation
    GenTime = add_seconds(BaseTime, 300),
    tcps_kaizen:record_receipt(#{
        id => iolist_to_binary([<<"gen_receipt_">>, SkuId]),
        type => sku_generated,
        timestamp => GenTime,
        stage => generate,
        duration => float(Duration),
        work_order_id => WorkOrderId,
        sku_id => SkuId,
        success => Success,
        metadata => #{}
    }),

    %% SKU published (if successful)
    case Success of
        true ->
            PubTime = add_seconds(GenTime, Duration),
            tcps_kaizen:record_receipt(#{
                id => iolist_to_binary([<<"pub_receipt_">>, SkuId]),
                type => sku_published,
                timestamp => PubTime,
                stage => publish,
                duration => 600.0,
                work_order_id => WorkOrderId,
                sku_id => SkuId,
                success => true,
                metadata => #{}
            });
        false ->
            ok
    end.

subtract_days({{Y, M, D}, Time}, Days) ->
    GregorianDays = calendar:date_to_gregorian_days({Y, M, D}),
    NewDate = calendar:gregorian_days_to_date(GregorianDays - Days),
    {NewDate, Time};
subtract_days({Date, Time}, Days) ->
    subtract_days({Date, Time}, Days).

date_part({Date, _Time}) -> Date;
date_part(Date) when is_tuple(Date) -> Date.

add_seconds({{Y, M, D}, {H, Min, S}}, Seconds) ->
    TotalSeconds = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, S}}),
    calendar:gregorian_seconds_to_datetime(TotalSeconds + Seconds).

get_targets() ->
    #{
        lead_time => 2.0,
        defect_rate => 1.0,
        rework_pct => 5.0,
        cycle_time => 0.5,
        first_pass_yield => 95.0,
        throughput => 10.0
    }.

calculate_achievement(Metrics, Targets) ->
    %% Simple average of individual achievements
    Achievements = maps:fold(
        fun(Metric, Target, Acc) ->
            Current = maps:get(Metric, Metrics, 0.0),
            Achievement = calculate_metric_achievement(Metric, Current, Target),
            [Achievement | Acc]
        end,
        [],
        Targets
    ),
    case Achievements of
        [] -> 0.0;
        _ -> lists:sum(Achievements) / length(Achievements)
    end.

calculate_metric_achievement(Metric, Current, Target) ->
    LowerBetter = [lead_time, defect_rate, rework_pct, cycle_time],

    case lists:member(Metric, LowerBetter) of
        true ->
            if
                Current =< Target -> 100.0;
                Target =:= 0.0 -> 0.0;
                true -> min(100.0, (Target / Current) * 100.0)
            end;
        false ->
            if
                Current >= Target -> 100.0;
                Target =:= 0.0 -> 0.0;
                true -> min(100.0, (Current / Target) * 100.0)
            end
    end.
