# Testing Architecture & System Design

**Version:** 2.1.0
**Last Updated:** 2026-01-31
**Status:** Production-Ready

---

## Overview

erlmcp testing architecture is built on three interconnected frameworks (EUnit, Common Test, Proper) orchestrated through a unified quality gate system. This document provides comprehensive visualizations of the testing infrastructure, data flows, and integration points.

---

## Test Framework Architecture

### High-Level Architecture

```mermaid
graph TB
    subgraph "Developer Workflow"
        Dev[Developer] --> Write[Write Test]
        Write --> Run[Run Test]
    end

    subgraph "Test Frameworks"
        EUnit[EUnit<br/>Unit Tests]
        CT[Common Test<br/>Integration Tests]
        Proper[Proper<br/>Property Tests]
    end

    subgraph "Quality Gates"
        Compile[Compilation Gate]
        TestPass[Test Pass Gate]
        Coverage[Coverage Gate]
        Chicago[Chicago School Gate]
    end

    subgraph "Reporting"
        Results[Test Results]
        CoverageData[Coverage Data]
        Metrics[Metrics Dashboard]
    end

    subgraph "Automation"
        PreCommit[Pre-Commit Hooks]
        CI[CI/CD Pipeline]
        Release[Release Validation]
    end

    Run --> EUnit
    Run --> CT
    Run --> Proper

    EUnit --> TestPass
    CT --> TestPass
    Proper --> TestPass

    TestPass --> Coverage
    Coverage --> Chicago
    Chicago --> Results

    Results --> CoverageData
    CoverageData --> Metrics

    Chicago -.->|Block| PreCommit
    PreCommit -.->|Allow| CI
    CI -.->|Validate| Release

    Dev -.->|Trigger| PreCommit

    style Chicago fill:#51cf66
    style PreCommit fill:#ffd43b
```

### Component Relationship Map

```mermaid
graph LR
    subgraph "Test Execution Layer"
        EUnit[EUnit]
        CT[Common Test]
        Proper[Proper]
    end

    subgraph "Data Collection Layer"
        Cover[Cover Tool]
        Trace[Trace Tool]
        Log[Logger]
    end

    subgraph "Analysis Layer"
        Parse[Result Parser]
        Metrics[Metrics Calculator]
        Trend[Trend Analyzer]
    end

    subgraph "Reporting Layer"
        HTML[HTML Reports]
        JSON[JSON Export]
        Dashboard[Dashboard]
    end

    subgraph "Enforcement Layer"
        PreCommit[Pre-Commit]
        CI[CI/CD]
        Gate[Quality Gate]
    end

    EUnit --> Cover
    CT --> Cover
    Proper --> Cover

    EUnit --> Trace
    CT --> Log
    Proper --> Log

    Cover --> Parse
    Trace --> Parse
    Log --> Parse

    Parse --> Metrics
    Metrics --> Trend

    Metrics --> HTML
    Metrics --> JSON
    Trend --> Dashboard

    HTML --> Gate
    JSON --> CI
    Dashboard --> PreCommit
```

---

## Test Execution Pipeline

### Sequential Test Execution Flow

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant Make as Makefile
    participant EUnit as EUnit
    participant CT as Common Test
    participant Proper as Proper
    participant Cover as Cover Tool
    participant Gate as Quality Gate

    Dev->>Make: make test
    Make->>EUnit: rebar3 eunit
    EUnit->>EUnit: Run unit tests
    EUnit-->>Make: Test results

    Make->>CT: rebar3 ct
    CT->>CT: Run integration tests
    CT-->>Make: Test results

    Make->>Proper: rebar3 proper
    Proper->>Proper: Run property tests
    Proper-->>Make: Test results

    Make->>Cover: rebar3 cover
    Cover->>Cover: Analyze coverage
    Cover-->>Make: Coverage data

    Make->>Gate: Validate results
    Gate->>Gate: Check thresholds
    Gate-->>Make: Pass/Fail

    Make-->>Dev: Final result
```

### Parallel Test Execution (Optimized)

```mermaid
graph TB
    Start[Test Run] --> Scheduler[Test Scheduler]

    Scheduler --> Worker1[Worker 1]
    Scheduler --> Worker2[Worker 2]
    Scheduler --> Worker3[Worker 3]
    Scheduler --> Worker4[Worker 4]

    Worker1 --> E1[EUnit: Core]
    Worker2 --> E2[EUnit: Transports]
    Worker3 --> E3[EUnit: Observability]
    Worker4 --> E4[EUnit: Validation]

    E1 --> CT1[CT: Registry]
    E2 --> CT2[CT: Session]
    E3 --> CT3[CT: Supervision]
    E4 --> CT4[CT: Distributed]

    CT1 --> P1[Proper: JSON-RPC]
    CT2 --> P2[Proper: Protocol]
    CT3 --> P3[Proper: State Machines]
    CT4 --> P4[Proper: Transport]

    P1 --> Aggregate[Result Aggregator]
    P2 --> Aggregate
    P3 --> Aggregate
    P4 --> Aggregate

    Aggregate --> Report[Final Report]

    style Scheduler fill:#ffd43b
    style Aggregate fill:#51cf66
```

---

## Coverage Collection & Reporting

### Coverage Data Flow

```mermaid
flowchart TB
    subgraph "Test Execution"
        EUnitRun[EUnit Tests]
        CTRun[CT Tests]
        ProperRun[Proper Tests]
    end

    subgraph "Coverage Instrumentation"
        Instrument[Code Instrumentation]
        Execute[Execute Tests]
        Collect[Collect Coverage Data]
    end

    subgraph "Data Processing"
        Parse1[Parse Coverdata]
        Calculate[Calculate Metrics]
        Analyze[Analyze Gaps]
    end

    subgraph "Report Generation"
        HTML[HTML Report]
        JSON[JSON Export]
        Summary[Summary Stats]
    end

    subgraph "Enforcement"
        Threshold[Check Thresholds]
        Block{≥80%?}
        Allow[Pass]
        Deny[Block Commit]
    end

    EUnitRun --> Instrument
    CTRun --> Instrument
    ProperRun --> Instrument

    Instrument --> Execute
    Execute --> Collect
    Collect --> Parse1

    Parse1 --> Calculate
    Calculate --> Analyze

    Analyze --> HTML
    Analyze --> JSON
    Calculate --> Summary

    Summary --> Threshold
    Threshold --> Block

    Block -->|Yes| Allow
    Block -->|No| Deny

    Allow --> CI[CI/CD Passes]
    Deny --> Commit[Commit Blocked]

    style Allow fill:#51cf66
    style Deny fill:#ff6b6b
```

### Coverage Metrics Calculation

```mermaid
graph LR
    subgraph "Raw Data"
        CoverData[*.coverdata Files]
    end

    subgraph "Analysis"
        Line[Line Coverage]
        Branch[Branch Coverage]
        Function[Function Coverage]
        Module[Module Coverage]
    end

    subgraph "Aggregation"
        App[App Level]
        Suite[Suite Level]
        Overall[Overall Coverage]
    end

    subgraph "Visualization"
        Pie[Pie Charts]
        Heat[Heatmap]
        Trend[Trend Lines]
        Drill[Drill-Down]
    end

    CoverData --> Line
    CoverData --> Branch
    CoverData --> Function

    Line --> Module
    Branch --> Module
    Function --> Module

    Module --> App
    App --> Suite
    Suite --> Overall

    Overall --> Pie
    App --> Heat
    Suite --> Trend
    Module --> Drill

    style Overall fill:#51cf66
```

---

## Quality Gate Enforcement

### Quality Gate Architecture

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant Hook as Pre-Commit Hook
    participant Compile as Compilation Gate
    participant Test as Test Gate
    participant Cov as Coverage Gate
    participant Chicago as Chicago School Gate
    participant Bench as Benchmark Gate
    participant Git as Git

    Dev->>Hook: git commit
    Hook->>Compile: TERM=dumb rebar3 compile

    alt Compilation Fails
        Compile-->>Hook: errors > 0
        Hook-->>Dev: ❌ Block commit
    else Compilation Passes
        Compile-->>Hook: errors = 0
        Hook->>Test: make test-strict

        alt Tests Fail
            Test-->>Hook: pass rate < 90%
            Hook-->>Dev: ❌ Block commit
        else Tests Pass
            Test-->>Hook: pass rate ≥ 90%
            Hook->>Cov: make coverage-strict

            alt Coverage Low
                Cov-->>Hook: < 80%
                Hook-->>Dev: ❌ Block commit
            else Coverage OK
                Cov-->>Hook: ≥ 80%
                Hook->>Chicago: Check for mocks

                alt Mocks Found
                    Chicago-->>Hook: ❌ Mocks detected
                    Hook-->>Dev: ❌ Block commit
                else No Mocks
                    Chicago-->>Hook: ✅ Clean
                    Hook->>Bench: make benchmark-strict

                    alt Regression
                        Bench-->>Hook: ≥ 10% regression
                        Hook-->>Dev: ❌ Block commit
                    else Performance OK
                        Bench-->>Hook: < 10% regression
                        Hook->>Git: Allow commit
                        Git-->>Dev: ✅ Commit successful
                    end
                end
            end
        end
    end
```

### Gate Decision Tree

```mermaid
graph TB
    Start[Commit Triggered] --> G1{Compilation OK?}

    G1 -->|No| Fail1[❌ Block: Compile Errors]
    G1 -->|Yes| G2{Tests Pass?}

    G2 -->|No| Fail2[❌ Block: Test Failures]
    G2 -->|Yes| G3{Coverage ≥80%?}

    G3 -->|No| Fail3[❌ Block: Low Coverage]
    G3 -->|Yes| G4{Chicago School?}

    G4 -->|Mocks| Fail4[❌ Block: Mocks Detected]
    G4 -->|Clean| G5{Performance OK?}

    G5 -->|Regress| Fail5[❌ Block: Performance]
    G5 -->|OK| Success[✅ Allow Commit]

    style Success fill:#51cf66
    style Fail1 fill:#ff6b6b
    style Fail2 fill:#ff6b6b
    style Fail3 fill:#ff6b6b
    style Fail4 fill:#ff6b6b
    style Fail5 fill:#ff6b6b
```

---

## CI/CD Integration

### GitHub Actions Pipeline

```mermaid
graph TB
    subgraph "Triggers"
        Push[Push to main]
        PR[Pull Request]
        Schedule[Schedule]
    end

    subgraph "Build Environment"
        Setup[Setup Erlang/OTP]
        Deps[Fetch Dependencies]
        Compile[Compile Code]
    end

    subgraph "Test Phase"
        EUnit[Run EUnit]
        CT[Run Common Test]
        Proper[Run Proper]
    end

    subgraph "Analysis Phase"
        Cover[Check Coverage]
        Dialyzer[Dialyzer]
        Xref[Xref]
    end

    subgraph "Performance Phase"
        Bench[Run Benchmarks]
        Compare[Compare Baselines]
    end

    subgraph "Report Phase"
        Upload[Upload Artifacts]
        Comment[PR Comment]
        Status[Status Check]
    end

    Push --> Setup
    PR --> Setup
    Schedule --> Setup

    Setup --> Deps
    Deps --> Compile

    Compile --> EUnit
    EUnit --> CT
    CT --> Proper

    Proper --> Cover
    Cover --> Dialyzer
    Dialyzer --> Xref

    Xref --> Bench
    Bench --> Compare

    Compare --> Upload
    Upload --> Comment
    Comment --> Status

    Status --> Merge{Merge?}

    Merge -->|All Green| Success[✅ Merge Allowed]
    Merge -->|Any Red| Fail[❌ Merge Blocked]

    style Success fill:#51cf66
    style Fail fill:#ff6b6b
```

### CI/CD Data Flow

```mermaid
sequenceDiagram
    participant GitHub as GitHub Actions
    participant Runner as Test Runner
    participant Cover as Cover Tool
    participant S3 as Artifacts Storage
    participant Status as Status API
    participant PR as PR Comment

    GitHub->>Runner: Trigger workflow
    Runner->>Runner: Run tests

    Runner->>Cover: Generate coverage
    Cover-->>Runner: coverage.json

    Runner->>S3: Upload coverage report
    S3-->>Runner: Upload URL

    Runner->>Status: Pass/Fail status
    Status-->>GitHub: Update check

    alt Pull Request
        GitHub->>PR: Post coverage comment
        PR-->>GitHub: Comment posted
    end

    GitHub-->>Runner: Workflow complete
```

---

## Test Data Management

### Test Data Flow

```mermaid
graph LR
    subgraph "Test Inputs"
        Fixtures[Fixtures]
        Config[Config Files]
        Generators[Generators]
    end

    subgraph "Test Execution"
        Setup[Setup Phase]
        Exercise[Exercise Phase]
        Verify[Verify Phase]
        Teardown[Teardown Phase]
    end

    subgraph "Test Outputs"
        Logs[Log Files]
        CoverData[Cover Data]
        Results[Test Results]
    end

    subgraph "Analysis"
        Parse[Parse Results]
        Metrics[Calculate Metrics]
        Report[Generate Report]
    end

    Fixtures --> Setup
    Config --> Setup
    Generators --> Exercise

    Setup --> Exercise
    Exercise --> Verify
    Verify --> Teardown

    Teardown --> Logs
    Verify --> CoverData
    Verify --> Results

    Logs --> Parse
    CoverData --> Parse
    Results --> Parse

    Parse --> Metrics
    Metrics --> Report
```

### Test State Management

```mermaid
stateDiagram-v2
    [*] --> Initialized: Test starts
    Initialized --> SetupRunning: init_per_testcase
    SetupRunning --> SetupComplete: Setup done

    SetupComplete --> TestRunning: Execute test
    TestRunning --> TestPassed: Test passes
    TestRunning --> TestFailed: Test fails

    TestPassed --> TeardownRunning: end_per_testcase
    TestFailed --> TeardownRunning: Cleanup

    TeardownRunning --> TeardownComplete: Teardown done
    TeardownComplete --> [*]: Test ends

    note right of SetupRunning
        Spawn processes
        Start gen_servers
        Initialize state
    end note

    note right of TestRunning
        Exercise system
        Verify behavior
        Assert results
    end note

    note right of TeardownRunning
        Stop processes
        Clean up resources
        Verify cleanup
    end note
```

---

## Monitoring & Observability

### Test Metrics Pipeline

```mermaid
graph TB
    subgraph "Collection"
        Tests[Test Execution]
        Bench[Benchmarks]
        Cover[Coverage]
    end

    subgraph "Processing"
        Parse1[Parse Logs]
        Calc[Calculate Metrics]
        Aggregate[Aggregate Data]
    end

    subgraph "Storage"
        TSDB[Time Series DB]
        Index[Elasticsearch]
        Files[JSON Files]
    end

    subgraph "Visualization"
        Dashboard[Dashboard]
        Reports[Reports]
        Alerts[Alerts]
    end

    Tests --> Parse1
    Bench --> Parse1
    Cover --> Parse1

    Parse1 --> Calc
    Calc --> Aggregate

    Aggregate --> TSDB
    Aggregate --> Index
    Aggregate --> Files

    TSDB --> Dashboard
    Index --> Reports
    Files --> Alerts

    Dashboard --> Monitor[Monitor Trends]
    Reports --> Analyze[Analyze Gaps]
    Alerts -> Notify[Notify Team]

    style Dashboard fill:#51cf66
    style Alerts fill:#ff6b6b
```

### Real-Time Test Monitoring

```mermaid
sequenceDiagram
    participant Test as Test Runner
    participant Collector as Metrics Collector
    participant Dashboard as Dashboard Server
    participant Alert as Alert Manager
    participant Dev as Developer

    Test->>Collector: Test results
    Collector->>Collector: Calculate metrics
    Collector->>Dashboard: Update dashboard

    Dashboard->>Dashboard: Check thresholds

    alt Threshold Breach
        Dashboard->>Alert: Trigger alert
        Alert->>Dev: Notification sent
    else All Good
        Dashboard->>Dashboard: Update status
    end

    Dev->>Dashboard: View status
    Dashboard-->>Dev: Current metrics
```

---

## Integration Points

### External System Integration

```mermaid
graph TB
    subgraph "erlmcp Testing"
        TestFramework[Test Framework]
        QualityGates[Quality Gates]
    end

    subgraph "Version Control"
        Git[Git]
        GitHub[GitHub]
    end

    subgraph "CI/CD"
        Actions[GitHub Actions]
        Jenkins[Jenkins]
    end

    subgraph "Monitoring"
        Datadog[Datadog]
        Prometheus[Prometheus]
    end

    subgraph "Communication"
        Slack[Slack]
        Email[Email]
    end

    TestFramework --> QualityGates
    QualityGates --> Actions
    QualityGates --> Jenkins

    Git --> GitHub
    GitHub --> Actions

    Actions --> QualityGates
    Actions --> Datadog
    Actions --> Slack

    QualityGates --> Prometheus
    Prometheus --> Datadog

    Datadog --> Email

    style QualityGates fill:#51cf66
```

### Plugin Architecture

```mermaid
graph LR
    subgraph "Core Testing"
        TestCore[Test Core]
        CoverCore[Coverage Core]
    end

    subgraph "Plugins"
        P1[Coveralls Plugin]
        P2[GitHub Plugin]
        P3[JUnit Plugin]
        P4[HTML Plugin]
    end

    subgraph "Custom Hooks"
        H1[Pre-Test Hook]
        H2[Post-Test Hook]
        H3[Pre-Cover Hook]
        H4[Post-Cover Hook]
    end

    TestCore --> P1
    TestCore --> P2
    CoverCore --> P3
    CoverCore --> P4

    H1 --> TestCore
    TestCore --> H2
    H3 --> CoverCore
    CoverCore --> H4

    style TestCore fill:#51cf66
    style CoverCore fill:#51cf66
```

---

## Performance & Scalability

### Test Execution Optimization

```mermaid
graph TB
    subgraph "Test Execution Strategies"
        Serial[Serial Execution]
        Parallel[Parallel Execution]
        Distributed[Distributed Execution]
    end

    subgraph "Optimization Techniques"
        Cache[Result Caching]
        Shard[Test Sharding]
        Priority[Priority Queue]
    end

    subgraph "Resource Management"
        Pool[Worker Pool]
        Limit[Rate Limiting]
        Balance[Load Balancing]
    end

    Serial --> Cache
    Parallel --> Shard
    Distributed --> Priority

    Cache --> Pool
    Shard --> Balance
    Priority --> Limit

    Pool --> Fast[Faster Execution]
    Balance --> Scalable[Scalable Testing]
    Limit --> Stable[Stable System]

    style Fast fill:#51cf66
    style Scalable fill:#51cf66
    style Stable fill:#51cf66
```

### Distributed Test Execution

```mermaid
graph TB
    subgraph "Master Node"
        Scheduler[Test Scheduler]
        Coordinator[Result Coordinator]
    end

    subgraph "Worker Nodes"
        W1[Worker 1]
        W2[Worker 2]
        W3[Worker 3]
        W4[Worker 4]
    end

    subgraph "Result Aggregation"
        Aggregate[Aggregate Results]
        Report[Generate Report]
    end

    Scheduler --> W1
    Scheduler --> W2
    Scheduler --> W3
    Scheduler --> W4

    W1 --> Coordinator
    W2 --> Coordinator
    W3 --> Coordinator
    W4 --> Coordinator

    Coordinator --> Aggregate
    Aggregate --> Report

    style Scheduler fill:#ffd43b
    style Coordinator fill:#51cf66
```

---

## Summary

**Architecture Highlights:**

- **Three-Framework Integration**: EUnit, Common Test, Proper unified through quality gates
- **Sequential & Parallel Execution**: Optimized for speed and reliability
- **Comprehensive Coverage**: Line, branch, function, module coverage tracking
- **Quality Gate Enforcement**: Five mandatory gates (compile, test, coverage, Chicago School, benchmark)
- **CI/CD Integration**: GitHub Actions with artifact storage and status checks
- **Monitoring Pipeline**: Real-time metrics, dashboards, and alerts
- **Scalability**: Distributed test execution for large test suites

**Key Design Principles:**

1. **Zero-Defect Tolerance**: All gates are blocking on failure
2. **Chicago School TDD**: Real collaborators, state-based verification
3. **Automation First**: Pre-commit hooks prevent low-quality code
4. **Observability**: Comprehensive metrics and reporting
5. **Scalability**: Parallel and distributed execution support

---

**Related Documentation:**
- [README](README.md) - Testing overview
- [TDD Strategy](tdd-strategy.md) - Chicago School methodology
- [Integration Tests](integration-tests.md) - Multi-process patterns
- [Coverage Analysis](coverage-analysis.md) - Coverage metrics

**Last Updated:** 2026-01-31
**Maintained by:** erlang-test-engineer agent
**Version:** 2.1.0
