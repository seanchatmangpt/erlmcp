# TCPS Kaizen Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                     TCPS PRODUCTION SYSTEM                          │
│                                                                     │
│  Work Orders → Generate → Compile → Test → Publish → Deploy       │
│        │          │          │        │        │         │         │
│        └──────────┴──────────┴────────┴────────┴─────────┘         │
│                            │                                        │
│                       RECEIPTS                                      │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      KAIZEN ENGINE                                  │
│                                                                     │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐        │
│  │   COLLECT    │───▶│   ANALYZE    │───▶│   IMPROVE    │        │
│  │   METRICS    │    │    WASTE     │    │   PROPOSE    │        │
│  └──────────────┘    └──────────────┘    └──────────────┘        │
│         │                    │                    │                │
│         │                    │                    │                │
│         ▼                    ▼                    ▼                │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐        │
│  │   TREND      │    │   QUANTIFY   │    │   APPLY      │        │
│  │   ANALYSIS   │    │   IMPACT     │    │   AUTOMATED  │        │
│  └──────────────┘    └──────────────┘    └──────────────┘        │
│         │                    │                    │                │
│         └────────────────────┴────────────────────┘                │
│                              │                                      │
│                              ▼                                      │
│                    ┌──────────────────┐                           │
│                    │  WEEKLY REPORT   │                           │
│                    │  & RECOMMENDATIONS│                           │
│                    └──────────────────┘                           │
└─────────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌──────────────────┐
                    │  ACTION & TRACK  │
                    └──────────────────┘
```

## Data Flow

### 1. Receipt Collection

```
TCPS Stage    Receipt Type           Metadata
───────────   ────────────────────   ─────────────────────
Work Order  → work_order            work_order_id
Generate    → sku_generated         duration, sku_id
Compile     → sku_generated         duration, stage=compile
Test        → test_run              test_name, success
Publish     → sku_published         sku_id, work_order_id
Andon       → andon_event           root_cause, severity
Manual      → (any)                 manual_intervention=true
```

### 2. Metrics Calculation

```
Receipt Stream → Group by Period → Calculate Metrics → Store History

Metrics Computed:
┌────────────────────┬──────────────────────────────────────────┐
│ Lead Time          │ work_order.timestamp →                   │
│                    │ sku_published.timestamp                  │
├────────────────────┼──────────────────────────────────────────┤
│ Defect Rate        │ (andon_events / skus) × 100             │
├────────────────────┼──────────────────────────────────────────┤
│ Rework %           │ (skus_with_fixes / total_skus) × 100    │
├────────────────────┼──────────────────────────────────────────┤
│ Cycle Time         │ average(all_durations)                   │
├────────────────────┼──────────────────────────────────────────┤
│ First Pass Yield   │ 100 - Rework %                          │
├────────────────────┼──────────────────────────────────────────┤
│ Throughput         │ skus_published / days                    │
└────────────────────┴──────────────────────────────────────────┘
```

### 3. Waste Detection Patterns

```
Waste Type          Detection Pattern                Threshold
─────────────────   ──────────────────────────────   ─────────
compilation_slow    duration > threshold             5 minutes
flaky_test          same test: success AND failure   2+ results
manual_intervention metadata.manual_intervention     = true
repeat_andon        same root_cause                  2+ events
long_lead_time      avg(stage_duration)              > 1 hour
excessive_rework    rework_cycles                    > 1
```

### 4. Improvement Generation Pipeline

```
┌─────────────┐
│ Waste Point │
│ total_waste │ ───┐
└─────────────┘    │
                   │
┌─────────────┐    │    ┌─────────────────┐
│ Filter      │◀───┘    │ For each waste: │
│ >= 1hr/week │─────────▶│ Generate        │
└─────────────┘         │ Proposal        │
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │ Calculate ROI   │
                        │ benefit / effort│
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │ Sort by ROI     │
                        │ (highest first) │
                        └────────┬────────┘
                                 │
                                 ▼
                        Prioritized Proposals
```

### 5. Automated Application Decision Tree

```
                    ┌──────────────┐
                    │ Improvement  │
                    │   Proposal   │
                    └──────┬───────┘
                           │
                    ┌──────▼───────┐
                    │ Is Waste Type│
                    │ Automatable? │
                    └──┬────────┬──┘
                       │        │
                   YES │        │ NO
                       │        │
          ┌────────────▼─┐    ┌─▼────────────────┐
          │ Apply Fix:   │    │ Require Manual:  │
          │              │    │                  │
          │ • Add test   │    │ • Architecture   │
          │   seeds      │    │ • Manual steps   │
          │ • Add SHACL  │    │ • Design changes │
          │   constraints│    │                  │
          └──────┬───────┘    └──────────────────┘
                 │
          ┌──────▼───────┐
          │ Generate     │
          │ Before/After │
          │ Receipt      │
          └──────────────┘
```

## Weekly Kaizen Cycle

```
Monday          Tuesday         Wednesday       Thursday        Friday
──────          ───────         ─────────       ────────        ──────
Generate        Analyze Top     Implement       Continue        Verify
Report          Waste Points    Improvements    Implementation  Metrics
│               │               │               │               │
│               │               │               │               │
▼               ▼               ▼               ▼               ▼
┌─────────┐   ┌─────────┐    ┌─────────┐     ┌─────────┐    ┌─────────┐
│ Metrics │   │ Select  │    │ Apply   │     │ Test    │    │ Early   │
│ vs      │──▶│ Top 3   │───▶│ Auto-   │────▶│ Changes │───▶│ Trend   │
│ Targets │   │ Proposals│    │ matable │     │         │    │ Check   │
└─────────┘   └─────────┘    └─────────┘     └─────────┘    └─────────┘
│               │               │               │               │
│               │               │               │               │
▼               ▼               ▼               ▼               ▼
Review          Assign          Schedule        Update          Document
Recommendations Owners          Manual Work     Templates       Learnings
```

## Metric Targets and Thresholds

```
Metric              Target      Threshold     Improvement Goal
──────              ──────      ─────────     ────────────────
Lead Time           ≤ 2.0 hrs   Alert > 3.0   5% per week
Defect Rate         ≤ 1.0%      Alert > 2.0   5% per week
Rework %            ≤ 5.0%      Alert > 10.0  5% per week
Cycle Time          ≤ 0.5 hrs   Alert > 1.0   5% per week
First Pass Yield    ≥ 95.0%     Alert < 85.0  5% per week
Throughput          ≥ 10 SKUs/d Alert < 5.0   5% per week

Waste Significance Threshold: ≥ 1.0 hour/week
ROI Calculation: total_waste / effort_hours
Effort Mapping: low=4hrs, medium=16hrs, high=40hrs
```

## Integration Points

### 1. Receipt Recording Hook

```erlang
%% In every TCPS stage:
stage_complete(StageData) ->
    %% Main work
    Result = execute_stage(StageData),

    %% Record receipt
    Receipt = create_receipt(StageData, Result),
    tcps_kaizen:record_receipt(Receipt),

    Result.
```

### 2. Weekly Report Scheduler

```erlang
%% Application startup
start() ->
    %% Schedule Monday 9:00 AM reports
    schedule_weekly_reports(),

    %% Start receipt collection
    start_receipt_collector(),

    ok.

schedule_weekly_reports() ->
    %% 7 days in milliseconds
    WeeklyInterval = 7 * 24 * 60 * 60 * 1000,

    timer:apply_interval(WeeklyInterval, fun() ->
        Report = tcps_kaizen:generate_weekly_report(today()),
        distribute_report(Report)
    end).
```

### 3. Real-time Alerts

```erlang
%% On receipt recording
record_receipt_with_alerts(Receipt) ->
    tcps_kaizen:record_receipt(Receipt),

    %% Check current metrics
    CurrentMetrics = tcps_kaizen:get_current_metrics(),

    %% Alert on threshold violations
    case check_thresholds(CurrentMetrics) of
        {alert, Violations} ->
            send_alert(Violations);
        ok ->
            ok
    end.
```

## Storage Architecture

```
┌────────────────────────────────────────────────────────────┐
│                      Storage Layer                         │
│                                                            │
│  ┌──────────────────┐         ┌──────────────────┐       │
│  │  ETS: Receipts   │         │ ETS: Improvements│       │
│  │  (in-memory)     │         │  (in-memory)     │       │
│  │                  │         │                  │       │
│  │ Key: receipt_id  │         │ Key: improve_id  │       │
│  │ Val: receipt_map │         │ Val: improve_map │       │
│  └────────┬─────────┘         └────────┬─────────┘       │
│           │                            │                  │
│           │  Periodic Flush            │                  │
│           ▼                            ▼                  │
│  ┌──────────────────┐         ┌──────────────────┐       │
│  │ Persistent Store │         │ Persistent Store │       │
│  │  (disk/database) │         │  (disk/database) │       │
│  └──────────────────┘         └──────────────────┘       │
└────────────────────────────────────────────────────────────┘
```

## Performance Characteristics

```
Operation               Complexity    Typical Time    Notes
─────────────────────   ──────────    ────────────    ─────────────────
record_receipt/1        O(1)          < 1ms           ETS insert
collect_metrics/1       O(n)          10-100ms        n = receipts
identify_waste/1        O(n log n)    50-200ms        Sorting
propose_improvements/1  O(m)          10-50ms         m = waste points
analyze_trends/1        O(w)          20-100ms        w = weeks
generate_report/1       O(n + m + w)  100-500ms       Full report

Where:
  n = number of receipts in period (typically 100-1000)
  m = number of waste points (typically 5-20)
  w = number of weeks in history (typically 8-12)
```

## API Summary

```erlang
%% Core API
-spec collect_metrics(time_period()) -> metrics().
-spec identify_waste_points() -> [waste_point()].
-spec propose_improvements([waste_point()]) -> [improvement()].
-spec apply_improvement(binary()) -> {ok, receipt()} | {error, term()}.
-spec analyze_trends([metric_snapshot()]) -> #{atom() => trend_analysis()}.
-spec generate_weekly_report(date()) -> report().

%% Helper API
-spec record_receipt(receipt()) -> ok.
-spec get_current_metrics() -> metrics().
-spec get_metric_history(date(), date()) -> [metric_snapshot()].

%% Types
-type time_period() :: {start_date(), end_date()}.
-type metrics() :: #{lead_time => float(), defect_rate => float(), ...}.
-type waste_point() :: #{waste_type => atom(), total_waste => float(), ...}.
-type improvement() :: #{id => binary(), roi => float(), ...}.
-type receipt() :: #{type => atom(), timestamp => datetime(), ...}.
```

## Error Handling

```
Error Scenario                  Handling Strategy
────────────────────────────    ─────────────────────────────────
No receipts in period           Return zero metrics
Invalid date range              Return error with reason
Unknown waste type              Skip with warning
Improvement not found           Return {error, not_found}
Insufficient trend data         Return {error, insufficient_data}
Receipt table not initialized   Auto-create on first use
Invalid receipt structure       Log error, reject receipt
```

## Extensibility Points

### 1. Custom Waste Detection

```erlang
identify_custom_waste(Receipts) ->
    %% Add your pattern detection
    CustomWaste = detect_your_pattern(Receipts),

    %% Return standard waste_point() structure
    [#{
        stage => your_stage,
        waste_type => your_custom_type,
        impact => Hours,
        frequency => Count,
        total_waste => Hours * Count,
        examples => ReceiptIds,
        root_cause => Description
    }].
```

### 2. Custom Metrics

```erlang
collect_custom_metric(Receipts) ->
    %% Calculate domain-specific metric
    Value = your_calculation(Receipts),

    %% Add to metrics map
    #{your_metric => Value}.
```

### 3. Custom Improvement Types

```erlang
generate_custom_proposal(WastePoint) ->
    %% Generate improvement specific to your context
    [#{
        id => generate_improvement_id(),
        description => <<"Your improvement">>,
        expected_benefit => <<"Quantified benefit">>,
        implementation_effort => low | medium | high,
        ...
    }].
```

---

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Status**: Production-Ready
