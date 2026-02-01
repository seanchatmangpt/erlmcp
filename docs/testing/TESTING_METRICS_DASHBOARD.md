# Testing Metrics Dashboard Specification

**Version:** 2.1.0
**Last Updated:** 2026-01-31
**Status:** Production-Ready

---

## Overview

This document specifies the design and implementation of a comprehensive testing metrics dashboard for erlmcp. The dashboard provides real-time visibility into test coverage, execution time, failure rates, and quality gate compliance.

---

## Dashboard Architecture

### System Components

```mermaid
graph TB
    subgraph "Data Collection"
        Tests[Test Execution]
        Cover[Coverage Analysis]
        Bench[Benchmark Suite]
        Quality[Quality Gates]
    end

    subgraph "Processing Layer"
        Parse[Result Parser]
        Calc[Metrics Calculator]
        Aggregate[Aggregator]
    end

    subgraph "Storage Layer"
        TSDB[Time Series DB]
        Cache[Redis Cache]
        Files[JSON Files]
    end

    subgraph "Visualization Layer"
        API[Dashboard API]
        Web[Web Interface]
        CLI[CLI Tools]
    end

    subgraph "Alert Layer"
        Threshold[Threshold Checker]
        Notify[Notification Service]
        Report[Report Generator]
    end

    Tests --> Parse
    Cover --> Parse
    Bench --> Parse
    Quality --> Parse

    Parse --> Calc
    Calc --> Aggregate

    Aggregate --> TSDB
    Aggregate --> Cache
    Aggregate --> Files

    TSDB --> API
    Cache --> API
    Files --> API

    API --> Web
    API --> CLI

    Aggregate --> Threshold
    Threshold --> Notify
    Threshold --> Report

    style API fill:#51cf66
    style Web fill:#51cf66
```

---

## Dashboard Layout

### Main Dashboard View

```mermaid
graph TB
    subgraph "Header"
        H1[Title: erlmcp Testing Dashboard]
        H2[Last Updated: 2026-01-31 14:30:00]
        H3[Auto-Refresh: 30s]
    end

    subgraph "Summary Cards (Row 1)"
        C1[Total Tests: 245]
        C2[Pass Rate: 100%]
        C3[Coverage: 51%]
        C4[Target Gap: -29%]
    end

    subgraph "Charts (Row 2)"
        CH1[Coverage Trend<br/>Line Chart]
        CH2[Test Execution Time<br/>Box Plot]
        CH3[Failure Rate<br/>Pareto Chart]
        CH4[Quality Gate Status<br/>Status Indicators]
    end

    subgraph "Tables (Row 3)"
        T1[Top 10 Untested Modules]
        T2[Recent Test Failures]
        T3[Coverage by App]
    end

    subgraph "Details (Row 4)"
        D1[Module Coverage Heatmap]
        D2[Test Execution Timeline]
        D3[Quality Gate Compliance]
    end

    H1 --> C1
    C1 --> CH1
    CH1 --> T1
    T1 --> D1

    style C1 fill:#51cf66
    style C2 fill:#51cf66
    style C3 fill:#ffd43b
    style C4 fill:#ff6b6b
```

---

## Metrics Specifications

### 1. Coverage Metrics

#### Overall Coverage Gauge

```mermaid
graph LR
    subgraph "Coverage Gauge"
        Gauge[Arc Gauge]
        Current[51%]
        Target[80%]
        Status[⚠️ Needs Improvement]
    end

    subgraph "Breakdown"
        Core[erlmcp_core: 49%]
        Obs[erlmcp_observability: 44%]
        Trans[erlmcp_transports: 73%]
        TCPS[tcps_erlmcp: 49%]
    end

    Current --> Status
    Core --> Gauge
    Obs --> Gauge
    Trans --> Gauge
    TCPS --> Gauge
```

**Data Source:** `rebar3 cover --verbose`

**Update Frequency:** After each test run

**Visual Elements:**
- Semi-circle gauge (0-100%)
- Color-coded: Red (<50%), Yellow (50-79%), Green (≥80%)
- Target marker at 80%
- Trend arrow (+/- from last run)

#### Coverage Trend Line

```mermaid
xychart-beta
    title "Coverage Trend (Last 30 Days)"
    x-axis ["Jan 1", "Jan 7", "Jan 14", "Jan 21", "Jan 28", "Jan 31"]
    y-axis "Coverage %" 0 --> 100
    line [35, 42, 48, 49, 50, 51]
```

**Data Points:** Daily coverage measurements

**Features:**
- Trend line with data points
- Moving average (7-day)
- Target line (80%)
- Projection to target date

#### Module Coverage Heatmap

```mermaid
heatmap
    title "Module Coverage Heatmap (Top 20 Critical Modules)"
    x-axis ["Coverage %"]
    y-axis ["Modules"]
    "" : 0, 20, 40, 60, 80, 100
    "erlmcp_server" : 0, 0, 0, 0, 0, 100
    "erlmcp_client" : 0, 0, 0, 0, 0, 100
    "erlmcp_registry" : 0, 0, 0, 0, 0, 85
    "erlmcp_hooks" : 0, 0, 0, 0, 0, 0
    "erlmcp_pricing_receipt" : 0, 0, 0, 0, 0, 0
```

**Color Scale:**
- 🟢 80-100%: Green
- 🟡 60-79%: Yellow
- 🟠 40-59%: Orange
- 🔴 0-39%: Red

### 2. Test Execution Metrics

#### Execution Time Box Plot

```mermaid
graph TB
    subgraph "Execution Time Distribution"
        Min[Min: 45s]
        Q1[Q1: 52s]
        Median[Median: 58s]
        Q3[Q3: 65s]
        Max[Max: 78s]
        Avg[Average: 58s]
    end

    subgraph "Breakdown"
        EUnit[EUnit: 30s]
        CT[Common Test: 25s]
        Proper[Proper: 3s]
    end

    Min --> Q1
    Q1 --> Median
    Median --> Q3
    Q3 --> Max

    EUnit --> Total[Total: 58s]
    CT --> Total
    Proper --> Total

    style Median fill:#51cf66
    style Avg fill:#ffd43b
```

**Data Points:** Last 100 test runs

**Features:**
- Box-and-whisker plot
- Outlier detection (values > 1.5 * IQR)
- Trend line overlay
- SLA threshold marker (<5min)

#### Execution Timeline

```mermaid
gantt
    title Test Execution Timeline (Last 24 Hours)
    dateFormat YYYY-MM-DD HH:mm
    axisFormat %H:%M

    section EUnit
    Run 1    :test1, 2026-01-31 08:00, 30s
    Run 2    :test2, 2026-01-31 12:00, 32s
    Run 3    :test3, 2026-01-31 16:00, 28s

    section Common Test
    Run 1    :ct1, after test1, 25s
    Run 2    :ct2, after test2, 27s
    Run 3    :ct3, after test3, 24s

    section Proper
    Run 1    :prop1, after ct1, 3s
    Run 2    :prop2, after ct2, 4s
    Run 3    :prop3, after ct3, 3s
```

### 3. Failure Rate Metrics

#### Pareto Chart of Failures

```mermaid
xychart-beta
    title "Test Failures by Category (Pareto Analysis)"
    x-axis ["Runtime", "Assertion", "Timeout", "Setup", "Teardown"]
    y-axis "Failure Count" 0 --> 50
    bar [45, 30, 15, 8, 2]
    line [45, 75, 90, 98, 100]
```

**Features:**
- Bar chart: Failure count by category
- Line chart: Cumulative percentage
- Pareto principle line (80% threshold)
- Top 3 failure types highlighted

#### Recent Failures Table

| Time | Test Suite | Test Case | Error | Status |
|------|------------|-----------|-------|--------|
| 14:25 | erlmcp_server_tests | concurrent_test | Timeout | 🔴 Open |
| 13:45 | erlmcp_registry_SUITE | distributed_test | Assertion fail | 🟡 Fixed |
| 11:30 | erlmcp_client_tests | connection_test | Process crash | 🟢 Verified |

**Update Frequency:** Real-time

**Features:**
- Sort by time (most recent first)
- Color-coded status
- Click to view full error log
- Link to code location

### 4. Quality Gate Metrics

#### Gate Compliance Status

```mermaid
graph TB
    subgraph "Quality Gates"
        G1[Compilation<br/>✅ Pass]
        G2[Tests<br/>✅ 100% Pass]
        G3[Coverage<br/>⚠️ 51% (< 80%)]
        G4[Chicago School<br/>✅ No Mocks]
        G5[Benchmarks<br/>✅ <10% Regression]
    end

    subgraph "Overall Status"
        Overall[Status: ⚠️ Partial Pass]
        Blocking[Blocking: Coverage Gate]
    end

    G3 --> Blocking
    Blocking --> Overall

    style G1 fill:#51cf66
    style G2 fill:#51cf66
    style G3 fill:#ff6b6b
    style G4 fill:#51cf66
    style G5 fill:#51cf66
```

#### Gate History Timeline

```mermaid
xychart-beta
    title "Quality Gate Pass Rate (Last 30 Days)"
    x-axis ["Week 1", "Week 2", "Week 3", "Week 4"]
    y-axis "Pass Rate %" 0 --> 100
    line [85, 90, 88, 92]
```

**Features:**
- Pass rate percentage per week
- Individual gate breakdown
- Failure reasons drill-down

---

## Real-Time Monitoring

### Test Execution Monitor

```mermaid
sequenceDiagram
    participant Dashboard as Dashboard
    participant Runner as Test Runner
    participant WebSocket as WebSocket
    participant Browser as Web Browser

    Dashboard->>Runner: Start test run
    Runner->>WebSocket: Progress updates

    loop Every 5 seconds
        WebSocket->>Browser: Test progress
        Browser->>Browser: Update UI
    end

    Runner->>WebSocket: Complete
    WebSocket->>Browser: Final results
    Browser->>Dashboard: Store results
```

**Real-Time Metrics:**
- Tests executed / total tests
- Current test running
- Estimated time remaining
- Failures detected (live)

### Live Coverage Tracking

```mermaid
graph LR
    subgraph "Coverage Updates"
        Test[Test Running]
        Module[Module Coverage]
        Line[Line Coverage]
    end

    subgraph "WebSocket Events"
        E1[test_started]
        E2[test_passed]
        E3[test_failed]
        E4[coverage_updated]
    end

    Test --> E1
    Test --> E2
    Test --> E3

    E2 --> E4
    E3 --> E4

    E4 --> Module
    E4 --> Line

    style E4 fill:#51cf66
```

**WebSocket Events:**
```json
{
  "event": "coverage_updated",
  "timestamp": "2026-01-31T14:30:00Z",
  "data": {
    "module": "erlmcp_server",
    "coverage": 75.5,
    "lines_covered": 450,
    "lines_total": 600
  }
}
```

---

## Alerting System

### Alert Thresholds

```mermaid
graph TB
    subgraph "Alert Levels"
        L1[🔴 Critical<br/>Coverage < 40%<br/>Pass rate < 80%]
        L2[🟡 Warning<br/>Coverage 40-79%<br/>Pass rate 80-94%]
        L3[🟢 Info<br/>Coverage ≥ 80%<br/>Pass rate ≥ 95%]
    end

    subgraph "Notification Channels"
        Slack[Slack #testing]
        Email[Email to team]
        Dashboard[Dashboard banner]
    end

    L1 --> Slack
    L1 --> Email

    L2 --> Slack
    L2 --> Dashboard

    L3 --> Dashboard

    style L1 fill:#ff6b6b
    style L2 fill:#ffd43b
    style L3 fill:#51cf66
```

### Alert Examples

**Critical Alert:**
```json
{
  "level": "critical",
  "title": "Coverage Drop Alert",
  "message": "Overall coverage dropped from 52% to 48%",
  "timestamp": "2026-01-31T14:30:00Z",
  "actions": [
    {"text": "View Coverage Report", "url": "/coverage"},
    {"text": "View Failing Tests", "url": "/tests?status=failed"}
  ]
}
```

**Warning Alert:**
```json
{
  "level": "warning",
  "title": "Test Execution Slow",
  "message": "Test suite took 6min 30s (threshold: 5min)",
  "timestamp": "2026-01-31T14:25:00Z"
}
```

---

## Data Storage

### Time Series Data Model

```mermaid
graph TB
    subgraph "Metrics Data"
        M1[coverage_percentage]
        M2[test_pass_rate]
        M3[execution_time_ms]
        M4[failure_count]
    end

    subgraph "Dimensions"
        D1[timestamp]
        D2[module_name]
        D3[test_suite]
        D4[git_commit]
    end

    subgraph "Storage"
        TSDB[InfluxDB / Prometheus]
        Retention[30 days detailed<br/>365 days aggregated]
    end

    M1 --> TSDB
    M2 --> TSDB
    M3 --> TSDB
    M4 --> TSDB

    D1 --> TSDB
    D2 --> TSDB
    D3 --> TSDB
    D4 --> TSDB

    TSDB --> Retention

    style TSDB fill:#51cf66
```

### Data Collection Endpoints

**POST /metrics/test-results**
```json
{
  "run_id": "run-123",
  "timestamp": "2026-01-31T14:30:00Z",
  "git_commit": "abc123",
  "results": {
    "total_tests": 245,
    "passed": 245,
    "failed": 0,
    "skipped": 0,
    "pass_rate": 100,
    "execution_time_ms": 58000
  },
  "coverage": {
    "overall": 51,
    "erlmcp_core": 49,
    "erlmcp_observability": 44,
    "erlmcp_transports": 73,
    "tcps_erlmcp": 49
  }
}
```

---

## Dashboard Implementation

### Technology Stack

```mermaid
graph TB
    subgraph "Frontend"
        React[React 18]
        D3[D3.js Charts]
        WS[WebSocket Client]
        UI[Material-UI]
    end

    subgraph "Backend"
        API[Dashboard API<br/>Erlang/ Cowboy]
        WSrv[WebSocket Server<br/>Erlang/ Cowboy]
        Collector[Metrics Collector]
    end

    subgraph "Database"
        Influx[InfluxDB]
        Redis[Redis Cache]
    end

    React --> API
    D3 --> API
    WS --> WSrv

    API --> Collector
    WSrv --> Collector

    Collector --> Influx
    Collector --> Redis

    style API fill:#51cf66
    style Collector fill:#51cf66
```

### API Endpoints

**GET /api/metrics/summary**
- Returns: Overall metrics summary
- Cache: 30 seconds

**GET /api/metrics/coverage**
- Returns: Coverage by module
- Parameters: `?module=erlmcp_server`

**GET /api/metrics/trends**
- Returns: Historical trends
- Parameters: `?from=2026-01-01&to=2026-01-31`

**GET /api/metrics/failures**
- Returns: Recent test failures
- Parameters: `?limit=10`

**WebSocket /ws/metrics/live**
- Real-time test execution updates
- Events: `test_started`, `test_passed`, `test_failed`, `coverage_updated`

---

## Summary

**Dashboard Features:**

- ✅ **Real-time monitoring**: Live test execution and coverage updates
- ✅ **Comprehensive metrics**: Coverage, execution time, failure rates
- ✅ **Visualizations**: Gauges, charts, heatmaps, timelines
- ✅ **Alerting**: Critical, warning, and info notifications
- ✅ **Historical trends**: 30-day detailed, 365-day aggregated
- ✅ **Drill-down**: From summary to detailed module views
- ✅ **Quality gates**: Compliance status and history

**Data Flow:**

1. Test execution → Metrics collection → Storage
2. Storage → Processing → Dashboard API
3. Dashboard API → Frontend → Visualization
4. Threshold checks → Alerting → Notifications

**Benefits:**

- Immediate visibility into testing health
- Fast identification of failing tests
- Trend analysis for coverage improvement
- Data-driven decisions for test prioritization
- Automated alerts for quality gate violations

---

**Related Documentation:**
- [README](README.md) - Testing overview
- [Coverage Analysis](coverage-analysis.md) - Coverage metrics
- [Automated Validation](AUTOMATED_VALIDATION.md) - Quality gates

**Last Updated:** 2026-01-31
**Maintained by:** erlang-test-engineer agent
**Version:** 2.1.0
