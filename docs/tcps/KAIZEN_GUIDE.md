# TCPS Kaizen Continuous Improvement Guide

## Overview

TCPS Kaizen implements Toyota's continuous improvement philosophy for software production. It automatically collects metrics, identifies waste, proposes improvements, and tracks progress toward the target of **5% improvement per week**.

## Core Concepts

### 1. Metrics Collection

Kaizen tracks six key metrics from TCPS receipts:

- **Lead Time**: Work order created → SKU published (hours)
- **Defect Rate**: Andon events per 100 SKUs
- **Rework Percentage**: SKUs requiring fixes / total SKUs
- **Cycle Time**: Average time per production stage (hours)
- **First Pass Yield**: SKUs passing without rework (%)
- **Throughput**: SKUs produced per day

### 2. Waste Identification

Six types of waste are automatically detected:

- **compilation_slow**: Compilation time exceeds 5-minute threshold
- **flaky_test**: Tests with inconsistent results
- **manual_intervention**: Process requires manual human intervention
- **repeat_andon**: Same root cause triggers multiple Andon events
- **long_lead_time**: Stage averages over 1 hour
- **excessive_rework**: SKUs requiring multiple rework cycles

### 3. Improvement Proposals

For each significant waste point (>1 hour/week), Kaizen generates actionable proposals with:

- Description of the improvement
- Expected benefit (quantified)
- Implementation effort (low/medium/high)
- ROI calculation (benefit/effort)

### 4. Automated Application

Some improvements can be applied automatically:

- Adding deterministic test seeds (flaky tests)
- Adding SHACL constraints (repeat Andons)
- Updating configuration files

Others require manual implementation but are tracked.

## Usage Examples

### Collect Weekly Metrics

```erlang
%% Collect metrics for last week
{ok, Today} = calendar:universal_time(),
WeekAgo = subtract_days(Today, 7),

Metrics = tcps_kaizen:collect_metrics({date_part(WeekAgo), date_part(Today)}).

%% Results:
#{
    lead_time => 2.3,          % 2.3 hours average
    defect_rate => 3.2,        % 3.2 defects per 100 SKUs
    rework_pct => 8.5,         % 8.5% require rework
    cycle_time => 0.8,         % 48 minutes per stage
    first_pass_yield => 91.5,  % 91.5% pass first time
    throughput => 12.3,        % 12.3 SKUs/day
    collected_at => {{2026,1,26},{10,0,0}},
    period => {{2026,1,19}, {2026,1,26}},
    sample_size => 86
}
```

### Identify Waste Points

```erlang
%% Find waste in last week
WastePoints = tcps_kaizen:identify_waste_points({{2026,1,19}, {2026,1,26}}).

%% Results (sorted by impact):
[
    #{
        stage => compile,
        waste_type => compilation_slow,
        impact => 2.3,              % Hours per occurrence
        frequency => 24,            % Times per week
        total_waste => 55.2,        % Total hours wasted
        examples => [<<"receipt_compile_042">>, ...],
        root_cause => <<"Compilation time exceeds 5-minute threshold">>
    },
    #{
        stage => manual,
        waste_type => manual_intervention,
        impact => 1.0,
        frequency => 12,
        total_waste => 12.0,
        examples => [<<"receipt_deploy_015">>, ...],
        root_cause => <<"Process requires manual approval">>
    },
    ...
]
```

### Generate Improvement Proposals

```erlang
%% Propose improvements for identified waste
Proposals = tcps_kaizen:propose_improvements(WastePoints).

%% Results (sorted by ROI):
[
    #{
        id => <<"improvement_1706265600">>,
        description => <<"Cache compilation dependencies">>,
        expected_benefit => <<"Reduce compilation time by 40%, saving 22.1 hours/week">>,
        waste_addressed => [#{waste_type => compilation_slow, ...}],
        implementation_effort => medium,
        status => proposed,
        created_at => {{2026,1,26},{10,0,0}},
        roi => 1.38
    },
    #{
        id => <<"improvement_1706265601">>,
        description => <<"Add deterministic test seeds and isolation">>,
        expected_benefit => <<"Eliminate flaky tests, saving 3.5 hours/week">>,
        waste_addressed => [#{waste_type => flaky_test, ...}],
        implementation_effort => low,
        status => proposed,
        created_at => {{2026,1,26},{10,0,0}},
        roi => 0.88
    },
    ...
]
```

### Apply Improvements Automatically

```erlang
%% Apply an automatable improvement
{ok, Receipt} = tcps_kaizen:apply_improvement(<<"improvement_1706265601">>).

%% Receipt shows before/after metrics
#{
    id => <<"receipt_improvement_001">>,
    type => improvement_applied,
    timestamp => {{2026,1,26},{10,15,0}},
    stage => kaizen,
    improvement_id => <<"improvement_1706265601">>,
    description => <<"Add deterministic test seeds and isolation">>,
    before_metrics => #{lead_time => 2.3, defect_rate => 3.2, ...},
    after_metrics => #{lead_time => 2.3, defect_rate => 2.8, ...},
    changes => [
        #{test => <<"test_random_behavior">>,
          change => <<"Added deterministic seed">>,
          seed_value => 742891}
    ],
    success => true,
    metadata => #{benefit_realized => 0.4}
}
```

### Track Trends Over Time

```erlang
%% Get 8 weeks of metric history
History = tcps_kaizen:get_metric_history({2025,12,1}, {2026,1,26}).

%% Analyze trends
Trends = tcps_kaizen:analyze_trends(History).

%% Results:
#{
    lead_time => #{
        metric => lead_time,
        improving => true,
        rate_of_improvement => 6.2,  % 6.2% per week
        weeks_analyzed => 8,
        current_value => 2.3,
        target_value => 2.0,
        on_track => true,
        history => [
            {{2025,12,1}, 3.8},
            {{2025,12,8}, 3.5},
            {{2025,12,15}, 3.2},
            ...
            {{2026,1,26}, 2.3}
        ]
    },
    defect_rate => #{...},
    ...
}
```

### Generate Weekly Kaizen Report

```erlang
%% Generate comprehensive weekly report
Report = tcps_kaizen:generate_weekly_report({2026,1,26}).

%% Report structure:
#{
    report_type => weekly_kaizen,
    week_ending => {2026,1,26},
    generated_at => {{2026,1,26},{10,30,0}},

    %% Summary section
    summary => #{
        current_metrics => #{lead_time => 2.3, ...},
        targets => #{lead_time => 2.0, ...},
        achievement => #{
            lead_time => 87.0,    % 87% of target achieved
            defect_rate => 68.0,
            ...
            overall => 82.3
        }
    },

    %% Waste analysis
    waste_analysis => #{
        total_waste_hours => 78.4,
        top_waste_points => [...],  % Top 10 by impact
        waste_by_category => #{
            compilation_slow => #{count => 1, total_waste => 55.2, ...},
            manual_intervention => #{count => 1, total_waste => 12.0, ...},
            ...
        }
    },

    %% Improvements tracking
    improvements => #{
        proposed_this_week => [...],   % All proposals
        applied_this_week => [...],    % Applied improvements
        total_proposed => 5,
        total_applied => 2,
        benefit_realized => 4.2        % Hours saved this week
    },

    %% Trend analysis
    trends => #{lead_time => #{...}, defect_rate => #{...}, ...},

    %% Week-over-week comparison
    week_over_week => #{
        lead_time => #{
            current => 2.3,
            previous => 2.5,
            change_pct => -8.0,
            improved => true
        },
        ...
    },

    %% Recommendations for next week
    recommendations => [
        #{
            priority => high,
            category => high_waste,
            message => <<"High-impact waste: compilation_slow (55.2 hrs/week)">>,
            action => <<"Address this waste point as highest priority">>
        },
        ...
    ],

    %% Visualization data
    charts => #{
        metrics_trend => [...],
        waste_distribution => [...],
        improvement_pipeline => #{
            proposed => 5,
            applied => 2,
            in_progress => 3,
            by_effort => #{low => 2, medium => 2, high => 1}
        }
    }
}
```

## Integration with TCPS

### Recording Receipts

Every TCPS stage should record receipts:

```erlang
%% Work order receipt
tcps_kaizen:record_receipt(#{
    id => generate_id(),
    type => work_order,
    timestamp => calendar:universal_time(),
    stage => intake,
    work_order_id => <<"wo_12345">>,
    success => true,
    metadata => #{}
}).

%% SKU generation receipt
tcps_kaizen:record_receipt(#{
    id => generate_id(),
    type => sku_generated,
    timestamp => calendar:universal_time(),
    stage => generate,
    duration => Duration,  % Seconds
    work_order_id => <<"wo_12345">>,
    sku_id => <<"sku_67890">>,
    success => true,
    metadata => #{}
}).

%% Andon event receipt
tcps_kaizen:record_receipt(#{
    id => generate_id(),
    type => andon_event,
    timestamp => calendar:universal_time(),
    stage => test,
    sku_id => <<"sku_67890">>,
    success => false,
    metadata => #{
        root_cause => <<"Missing dependency: erlang/jsx">>,
        severity => high
    }
}).
```

### Automated Weekly Reports

Set up a scheduled job to generate weekly reports:

```erlang
%% In production, use cron or similar
schedule_weekly_report() ->
    %% Every Monday at 9:00 AM
    timer:apply_interval(
        7 * 24 * 60 * 60 * 1000,  % 1 week in ms
        fun() ->
            Today = calendar:universal_time(),
            Report = tcps_kaizen:generate_weekly_report(date_part(Today)),

            %% Publish report
            publish_report(Report),

            %% Send notifications if metrics declining
            case check_metrics_health(Report) of
                {alert, Reasons} -> send_alert(Reasons);
                ok -> ok
            end
        end
    ).
```

## Target Metrics

Default targets (configurable):

- **Lead Time**: ≤ 2.0 hours
- **Defect Rate**: ≤ 1.0% (1 per 100 SKUs)
- **Rework %**: ≤ 5.0%
- **Cycle Time**: ≤ 0.5 hours (30 minutes)
- **First Pass Yield**: ≥ 95.0%
- **Throughput**: ≥ 10.0 SKUs/day

## Improvement Rate Target

**5% improvement per week** on all metrics.

Example:
- Week 1: Lead time = 4.0 hours
- Week 2: Lead time = 3.8 hours (-5%)
- Week 3: Lead time = 3.61 hours (-5%)
- Week 4: Lead time = 3.43 hours (-5%)

Over 4 weeks: 14.25% total improvement.

## Best Practices

### 1. Record Every Stage

Every production stage must emit receipts:
- Work order creation
- SKU generation start/end
- Compilation
- Test runs
- Publishing
- Andon events
- Manual interventions

### 2. Include Meaningful Metadata

```erlang
metadata => #{
    root_cause => <<"Specific cause">>,
    severity => high | medium | low,
    manual_intervention => boolean(),
    is_fix => boolean(),
    test_name => <<"test_xyz">>,
    stage_details => ...
}
```

### 3. Review Reports Weekly

- Discuss top waste points in team meetings
- Prioritize improvements by ROI
- Track trend graphs
- Celebrate improvements

### 4. Act on Recommendations

Report recommendations are prioritized:
- **High**: Address immediately
- **Medium**: Schedule this sprint
- **Low**: Backlog for future sprints

### 5. Measure Impact

After applying improvements, verify:
- Did metrics improve as expected?
- Was the benefit realized?
- Should we adjust the improvement?

## Example Weekly Cycle

### Monday Morning (30 minutes)

```erlang
%% 1. Generate report
Report = tcps_kaizen:generate_weekly_report(last_sunday()).

%% 2. Review metrics vs targets
#{summary := #{achievement := Achievement}} = Report,
io:format("Overall achievement: ~.1f%~n", [maps:get(overall, Achievement)]).

%% 3. Review top waste
#{waste_analysis := #{top_waste_points := TopWaste}} = Report,
[TopIssue | _] = TopWaste,
io:format("Top waste: ~s (~.1f hrs/week)~n",
    [maps:get(root_cause, TopIssue), maps:get(total_waste, TopIssue)]).

%% 4. Review recommendations
#{recommendations := Recs} = Report,
HighPriority = [R || #{priority := high} = R <- Recs],
io:format("High priority actions: ~p~n", [length(HighPriority)]).
```

### Tuesday (2 hours)

- Analyze top 3 waste points
- Brainstorm solutions
- Create improvement proposals
- Assign owners

### Wednesday-Thursday

- Implement improvements
- Apply automatable fixes
- Test changes

### Friday

- Verify improvements applied
- Check early metrics
- Document lessons learned

## Advanced Usage

### Custom Metric History Queries

```erlang
%% Get specific metric over time
get_lead_time_history(StartDate, EndDate) ->
    History = tcps_kaizen:get_metric_history(StartDate, EndDate),
    [{Date, maps:get(lead_time, Metrics)}
     || #{date := Date, metrics := Metrics} <- History].
```

### Custom Waste Detection

```erlang
%% Detect waste in specific subsystem
identify_subsystem_waste(Subsystem) ->
    AllWaste = tcps_kaizen:identify_waste_points(),
    [W || #{metadata := Meta} = W <- AllWaste,
          maps:get(subsystem, Meta, none) =:= Subsystem].
```

### Export Reports

```erlang
%% Export to JSON for dashboards
export_report_json(Report) ->
    JSON = jsx:encode(Report),
    file:write_file("kaizen_report.json", JSON).

%% Export to CSV for spreadsheets
export_metrics_csv(History) ->
    CSV = [["Date", "Lead Time", "Defect Rate", ...] |
           [[format_date(D), maps:get(lead_time, M), ...]
            || #{date := D, metrics := M} <- History]],
    write_csv("metrics_history.csv", CSV).
```

## Troubleshooting

### No Waste Detected

- Verify receipts are being recorded
- Check date ranges include data
- Ensure thresholds are appropriate

### Metrics Not Improving

- Review if improvements were actually applied
- Check if new waste sources appeared
- Analyze root causes more deeply
- Adjust improvement proposals

### High ROI Improvements Not Working

- Validate assumptions about waste impact
- Measure actual benefit after application
- Iterate on solution approach

## See Also

- [TCPS Overview](../TCPS.md)
- [TCPS Certification](../TCPS-certification.md)
- [TCPS Checklist](../TCPS-checklist.md)
- Toyota Production System literature
- Lean Six Sigma methodology
