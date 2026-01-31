# ERLMCP Benchmark Consolidation & Rebuild Roadmap

**Status**: Starting from zero - comprehensive audit complete
**Total Artifacts**: 93 files, ~25,490 LOC
**Target After Consolidation**: 15-20 files, ~12,000 LOC
**Timeline**: 4 weeks

---

## EXECUTIVE SUMMARY

The erlmcp codebase contains **93 benchmark/stress/chaos test files** with significant duplication and metrology inconsistencies. This document provides a phased consolidation plan that will:

1. **Remove 15-20 redundant files** (30% LOC reduction)
2. **Consolidate 10-15 files** (20% LOC reduction)
3. **Standardize measurements** (ISO 8601-style metrology)
4. **Create unified framework** (single entry point for all tests)
5. **Maintain all unique scenarios** (no loss of testing capability)

**Key Insight**: We're not rebuilding from scratch. We're reorganizing what works and standardizing measurements.

---

## PHASE 1: CLEANUP & CONSOLIDATION (Week 1)

### 1.1 Files to Delete

These files are development artifacts, duplicates, or completely superseded:

```bash
# Development artifacts (no longer used)
rm test/erlmcp_simple_benchmark.erl          # Too basic, 50 LOC
rm test/erlmcp_simple_stress.erl             # Too basic, 60 LOC
rm test_logging_stress.erl                    # Old version, 40 LOC
rm test_quick_stress.erl                      # Development, 30 LOC
rm quick_stress_test.erl                      # Development, 40 LOC
rm run_stress_test.erl                        # Superseded by orchestrators, 80 LOC
rm test/erlmcp_master_stress_test.erl        # Old version, 150 LOC

# Non-SUITE duplicates
rm test/registry_100k_stress.erl              # Non-SUITE duplicate, 200 LOC
rm test/tcp_real_bench_tests.erl             # Duplicate with bench/transport_real/, 100 LOC

# Result: ~750 LOC removed, 9 files deleted
```

### 1.2 Files to Consolidate (Merge Source Into Target)

#### A. benchmark_100k Consolidation

**Source**: `/Users/sac/erlmcp/bench/benchmark_100k.erl` (425 LOC)
**Target**: `/Users/sac/erlmcp/bench/benchmark_100k_SUITE.erl` (~600 LOC)
**Action**:
```
1. Copy all benchmark functions from benchmark_100k.erl into SUITE
2. Add entry point: erl -noshell -run benchmark_100k_SUITE run_all
3. Keep both files for now (standalone + CT interface)
4. Later: Mark benchmark_100k.erl as @deprecated
```

#### B. Registry Stress Consolidation

**Source**: `/Users/sac/erlmcp/bench/erlmcp_registry_contention.erl` (~400 LOC)
**Target**: `/Users/sac/erlmcp/test/erlmcp_registry_100k_stress_SUITE.erl` (~600 LOC)
**Action**:
```
1. Extract contention metrics functions from contention.erl
2. Add measure_contention_metrics/3 to registry SUITE
3. Merge test cases for contention analysis
4. Update registry SUITE to include partition balance metrics
```

#### C. Queue Benchmark Consolidation

**Source**: `/Users/sac/erlmcp/src/erlmcp_queue_benchmark.erl` (~300 LOC)
**Target**: `/Users/sac/erlmcp/bench/benchmark_100k_SUITE.erl`
**Action**:
```
1. Extract queue-specific latency/throughput tests
2. Add bench_queue_detailed/1 to SUITE with:
   - Variable queue sizes (100, 1K, 10K, 100K)
   - Different operation patterns (FIFO, random, batched)
   - Memory overhead per element
3. Remove src/erlmcp_queue_benchmark.erl
```

#### D. Chaos Tests Consolidation

**Source**: `/Users/sac/erlmcp/test/erlmcp_chaos_tests.erl` (100 LOC)
**Target**: `/Users/sac/erlmcp/test/erlmcp_chaos_test_SUITE.erl`
**Action**:
```
1. Verify erlmcp_chaos module exists (skeleton in erlmcp_chaos_tests.erl)
2. Move all injection tests to SUITE
3. If module doesn't exist: Create erlmcp_chaos.erl backend first
4. Remove erlmcp_chaos_tests.erl
```

**Result after Phase 1**:
- 9 files deleted: 750 LOC
- 4 consolidations: 600 LOC merged (no loss, just reorganization)
- **Net removal**: 750 LOC, cleaner organization

---

## PHASE 2: METROLOGY STANDARDIZATION (Week 2)

### 2.1 Create Metrology Standards Module

**File**: `/Users/sac/erlmcp/src/erlmcp_benchmark_metrology.erl`

```erlang
-module(erlmcp_benchmark_metrology).

-export([
    % Definitions
    throughput_definition/0,
    latency_definition/0,
    jitter_definition/0,
    memory_definition/0,

    % Calibration
    measure_system_overhead/0,
    calculate_latency_us/2,        % {StartUs, EndUs} -> Us

    % Analysis
    analyze_latencies/1,            % [LatencyList] -> #latency_stats{}
    calculate_percentile/2,          % (Percent, SortedList) -> Value
    calculate_jitter/1,              % [Values] -> Jitter

    % Validation
    validate_percentiles/1,          % #latency_stats{} -> ok | {error, Why}
    validate_throughput/1,           % ThroughputMps -> ok | {error, Why}

    % Reporting
    format_metrics/1,                % #benchmark_result{} -> Binary
    format_latency_percentiles/1     % #latency_stats{} -> Binary
]).

%% Standard stats record
-record(latency_stats, {
    count :: integer(),
    min_us :: float(),
    max_us :: float(),
    mean_us :: float(),
    median_us :: float(),
    p50_us :: float(),
    p95_us :: float(),
    p99_us :: float(),
    p99_9_us :: float(),
    p99_99_us :: float(),
    stddev_us :: float(),
    jitter :: float()
}).

%% Measurement overhead (calibrated once, cached)
-define(SYSTEM_TIME_OVERHEAD_US, 0.5).  % erlang:system_time() cost
-define(MEASUREMENT_OVERHEAD_US, 1.0).  % Full measurement cost
```

### 2.2 Fix All Latency Measurements

**Current Issues & Fixes**:

| Issue | Current | Fix | Files Affected |
|-------|---------|-----|-----------------|
| Percentile calculation truncates p99 | `erlang:round(Count * 0.99)` for 100 elements = 1 | Use `erlang:ceil()` | All latency tests |
| Inconsistent time units | Mix of µs and ms in same file | Always use µs internally, convert to ms for display | All files |
| Memory measurement includes VM | `erlang:memory(total)` | Use `erlang:memory(processes)` | latency_SUITE, memory_profiler |
| Jitter can exceed 1000% | Capped at 9999% | Use coefficient of variation properly | benchmark_100k.erl:356 |
| No measurement overhead subtraction | Not accounted for | Add 1.0µs per latency measurement | All micro-benchmarks |

### 2.3 Add Missing Measurements

**New Percentiles to Add Everywhere**:
- p99.9 (for tail analysis)
- p99.99 (for catastrophic tail events)
- min/max bounds

**New Metrics to Add**:
- Spike detection (frequency, duration, magnitude of >3σ deviations)
- Skewness (distribution shape)
- Kurtosis (tail fatness)
- Autocorrelation (for sustained load stability)

### 2.4 Implementation Checklist

```erlang
% File: erlmcp_benchmark_metrology.erl (200 lines)
measure_system_overhead() ->
    % Measure cost of erlang:system_time() + calculation
    % Run 10,000 iterations, extract median
    % Return cached value on subsequent calls

calculate_latency_us(StartUs, EndUs) ->
    % Subtract measurement overhead
    Raw = EndUs - StartUs,
    Adjusted = max(0.1, Raw - ?MEASUREMENT_OVERHEAD_US),
    Adjusted.

calculate_percentile(Percent, SortedList) ->
    Count = length(SortedList),
    % FIXED: Use ceil instead of round
    Index = erlang:ceil(Count * Percent / 100),
    BoundIndex = erlang:max(1, erlang:min(Index, Count)),
    lists:nth(BoundIndex, SortedList).

analyze_latencies(RawLatencies) ->
    % Filter out negative latencies (measurement error)
    Latencies = lists:filter(fun(L) -> L > 0.0 end, RawLatencies),
    Sorted = lists:sort(Latencies),
    Count = length(Sorted),

    case Count of
        0 -> {error, no_data};
        1 -> {ok, single_value_stats(hd(Sorted))};
        _ -> {ok, full_stats(Sorted)}
    end.

full_stats(Sorted) ->
    Count = length(Sorted),
    Sum = lists:sum(Sorted),
    Mean = Sum / Count,

    Variance = lists:sum([(V - Mean)^2 || V <- Sorted]) / Count,
    StdDev = math:sqrt(Variance),

    Jitter = case Mean of
        0.0 -> 0.0;
        _ -> StdDev / Mean
    end,

    #latency_stats{
        count = Count,
        min_us = lists:min(Sorted),
        max_us = lists:max(Sorted),
        mean_us = Mean,
        median_us = lists:nth((Count div 2) + 1, Sorted),
        p50_us = calculate_percentile(50, Sorted),
        p95_us = calculate_percentile(95, Sorted),
        p99_us = calculate_percentile(99, Sorted),
        p99_9_us = calculate_percentile(99.9, Sorted),
        p99_99_us = calculate_percentile(99.99, Sorted),
        stddev_us = StdDev,
        jitter = Jitter
    }.
```

**Result after Phase 2**:
- 1 new metrology module (200 LOC)
- Update 15 test files to use metrology module (~100 LOC changes)
- All measurements now consistent, traceable, ISO-compliant

---

## PHASE 3: INFRASTRUCTURE & FRAMEWORK (Week 3)

### 3.1 Create Baseline Management

**File**: `/Users/sac/erlmcp/src/erlmcp_benchmark_baseline.erl`

```erlang
-module(erlmcp_benchmark_baseline).

-export([
    set_baseline/2,          % (BenchmarkId, #benchmark_result{}) -> ok | {error, Why}
    get_baseline/1,          % (BenchmarkId) -> {ok, #benchmark_result{}} | {error, not_found}
    compare_to_baseline/2,   % (BenchmarkId, CurrentResult) -> {ok, Comparison} | {error, Why}
    get_regression_report/1, % (BenchmarkId) -> Report
    clear_all_baselines/0    % For testing
]).

%% Baseline storage location
-define(BASELINE_DIR, "/tmp/erlmcp_baselines").

%% Regression severity levels
-define(REGRESSION_LEVELS, [
    {minor, 0.05},      % 5% regression - warning
    {moderate, 0.10},   % 10% regression - yellow flag
    {severe, 0.20},     % 20% regression - red flag
    {critical, 0.50}    % 50% regression - blocker
]).

set_baseline(BenchmarkId, Result) ->
    % Store baseline to /tmp/erlmcp_baselines/BenchmarkId.json
    % Include: timestamp, result data, git commit hash

    Timestamp = erlang:system_time(second),
    {ok, Commit} = shell:cmd("git rev-parse --short HEAD"),

    Baseline = Result#{
        baseline_timestamp => Timestamp,
        baseline_commit => Commit,
        baseline_version => 1
    },

    case file:write_file(baseline_path(BenchmarkId), encode_json(Baseline)) of
        ok -> ok;
        {error, _} = E -> E
    end.

compare_to_baseline(BenchmarkId, CurrentResult) ->
    case get_baseline(BenchmarkId) of
        {ok, Baseline} ->
            Comparison = #{
                baseline_throughput => maps:get(throughput_mps, Baseline),
                current_throughput => maps:get(throughput_mps, CurrentResult),
                throughput_delta_pct =>
                    calc_delta_pct(
                        maps:get(throughput_mps, Baseline),
                        maps:get(throughput_mps, CurrentResult)
                    ),

                baseline_p99_us => maps:get(latency_p99_us, Baseline),
                current_p99_us => maps:get(latency_p99_us, CurrentResult),
                p99_delta_pct =>
                    calc_delta_pct(
                        maps:get(latency_p99_us, Baseline),
                        maps:get(latency_p99_us, CurrentResult)
                    ),

                severity => assess_severity(
                    maps:get(throughput_mps, Baseline),
                    maps:get(throughput_mps, CurrentResult)
                )
            },
            {ok, Comparison};
        {error, not_found} ->
            % First run, set as baseline
            set_baseline(BenchmarkId, CurrentResult),
            {ok, #{status => first_run, baseline_set => true}}
    end.

calc_delta_pct(Baseline, Current) ->
    case Baseline of
        0 -> 0.0;
        _ -> ((Current - Baseline) / Baseline) * 100.0
    end.

assess_severity(BaselineMetric, CurrentMetric) ->
    Delta = calc_delta_pct(BaselineMetric, CurrentMetric),
    case Delta of
        D when D < -50.0 -> critical;
        D when D < -20.0 -> severe;
        D when D < -10.0 -> moderate;
        D when D < -5.0 -> minor;
        D when D >= -5.0 -> ok
    end.
```

### 3.2 Create Unified Benchmark Runner

**File**: `/Users/sac/erlmcp/src/erlmcp_benchmark.erl`

```erlang
-module(erlmcp_benchmark).

-export([
    run_all/0,              % Run all benchmarks, save results, compare baselines
    run_suite/1,            % run_suite(registry) -> run registry_100k_stress_SUITE
    run_with_config/2,      % (SuiteName, Config) -> Results
    get_latest_results/0,   % Get latest run results
    compare_to_baseline/1   % Compare latest to baseline
]).

-record(benchmark_config, {
    name :: atom(),
    module :: atom(),
    warmup_iterations = 100 :: integer(),
    measurement_duration_sec = 10 :: integer(),
    tolerance_pct = 5.0 :: float(),
    save_baseline = false :: boolean()
}).

run_all() ->
    % Run all configured benchmark suites in order:
    % 1. benchmark_100k_SUITE          (comprehensive baseline)
    % 2. throughput_SUITE               (operation-specific)
    % 3. latency_SUITE                  (stability analysis)
    % 4. erlmcp_registry_100k_stress_SUITE (registry-specific)
    % 5. erlmcp_cluster_stress_SUITE    (cluster-specific)
    % 6. erlmcp_performance_benchmark_SUITE (framework integration)

    Suites = [
        benchmark_100k_SUITE,
        throughput_SUITE,
        latency_SUITE,
        erlmcp_registry_100k_stress_SUITE,
        erlmcp_cluster_stress_SUITE
    ],

    Results = lists:map(fun run_suite/1, Suites),

    % Store results
    store_results(Results),

    % Compare each to baseline
    Comparisons = lists:map(
        fun({SuiteName, Result}) ->
            {SuiteName, erlmcp_benchmark_baseline:compare_to_baseline(SuiteName, Result)}
        end,
        Results
    ),

    % Generate report
    generate_report(Comparisons),

    {ok, Comparisons}.

run_suite(SuiteName) ->
    % Dynamically run: ct:run_test([{suite, SuiteName}])
    % Extract results
    % Return: {SuiteName, #benchmark_result{}}

    io:format("Running benchmark suite: ~w~n", [SuiteName]),

    % Use CT to run the suite
    ResultDir = "/tmp/erlmcp_benchmark_results",
    ct:run_test([
        {suite, SuiteName},
        {logdir, ResultDir},
        {label, atom_to_list(SuiteName)}
    ]).

generate_report(Comparisons) ->
    % Generate markdown report showing:
    % - Latest results for each benchmark
    % - Comparison to baseline
    % - Regression severity (ok/minor/moderate/severe/critical)
    % - Recommendations

    ReportPath = "/tmp/erlmcp_benchmark_results/LATEST_REPORT.md",

    Report = lists:map(fun({Suite, Comparison}) ->
        format_comparison_section(Suite, Comparison)
    end, Comparisons),

    file:write_file(ReportPath, Report).
```

### 3.3 Create Result Aggregator

**File**: `/Users/sac/erlmcp/src/erlmcp_benchmark_results.erl`

```erlang
-module(erlmcp_benchmark_results).

-export([
    store_result/1,         % Store single benchmark result
    get_results_since/1,    % Get results since timestamp
    get_trend/1,            % Get trend over N recent runs
    export_csv/2,           % Export results to CSV for analysis
    export_json/2           % Export results to JSON
]).

% Result storage: /tmp/erlmcp_benchmark_results/YYYY-MM-DD/HH:MM:SS.json

store_result(Result) ->
    Timestamp = erlang:system_time(second),
    DateTime = calendar:system_time_to_rfc3339(Timestamp),

    [DatePart, TimePart] = string:split(DateTime, "T"),
    DayDir = filename:join(["/tmp/erlmcp_benchmark_results", DatePart]),
    filelib:ensure_dir(DayDir ++ "/"),

    TimeFile = TimePart ++ ".json",
    FilePath = filename:join([DayDir, TimeFile]),

    file:write_file(FilePath, encode_json(Result)).

get_results_since(MinutesAgo) ->
    Cutoff = erlang:system_time(second) - (MinutesAgo * 60),
    % Read all .json files from results directory
    % Filter by timestamp
    % Return aggregated results
    ok.

get_trend(SuiteName) ->
    % Get last 10 runs of SuiteName
    % Calculate trend: improving/stable/degrading
    % Return: #trend{direction, magnitude, confidence}
    ok.
```

**Result after Phase 3**:
- Unified benchmark runner (entry point)
- Baseline comparison framework
- Result storage and trending
- Automatic regression detection
- All infrastructure ready for Phase 4

---

## PHASE 4: INTEGRATION & DOCUMENTATION (Week 4)

### 4.1 Create CI Integration

**File**: `.github/workflows/erlmcp_benchmarks.yml`

```yaml
name: ERLMCP Benchmarks

on:
  schedule:
    - cron: '0 2 * * *'  # Daily at 2 AM UTC
  workflow_dispatch:

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: 25.0
          elixir-version: 1.13.0

      - name: Build
        run: make clean compile

      - name: Run Comprehensive Benchmarks
        run: |
          rebar3 as benchmark ct --suite=benchmark_100k_SUITE
          rebar3 as benchmark ct --suite=throughput_SUITE
          rebar3 as benchmark ct --suite=latency_SUITE

      - name: Generate Report
        run: erlmcp_benchmark:run_all()

      - name: Upload Results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: /tmp/erlmcp_benchmark_results/

      - name: Comment on PR (if regression)
        if: failure()
        uses: actions/github-script@v6
        with:
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '⚠️ Performance regression detected. See benchmark results.'
            })
```

### 4.2 Documentation

**File**: `docs/BENCHMARKING_GUIDE.md`

Contents:
1. Quick start: `make benchmark`
2. Understanding metrics (ISO-style definitions)
3. Interpreting results
4. Adding new benchmarks
5. Troubleshooting common issues
6. CI integration examples

### 4.3 Make Targets

```makefile
# Add to Makefile

# Core benchmarks
.PHONY: benchmark
benchmark: compile
	rebar3 as benchmark ct --suite=benchmark_100k_SUITE

.PHONY: benchmark-throughput
benchmark-throughput: compile
	rebar3 as benchmark ct --suite=throughput_SUITE

.PHONY: benchmark-latency
benchmark-latency: compile
	rebar3 as benchmark ct --suite=latency_SUITE

.PHONY: benchmark-registry
benchmark-registry: compile
	rebar3 as benchmark ct --suite=erlmcp_registry_100k_stress_SUITE

.PHONY: benchmark-cluster
benchmark-cluster: compile
	rebar3 as benchmark ct --suite=erlmcp_cluster_stress_SUITE

# Run all benchmarks + comparison
.PHONY: benchmark-all
benchmark-all: compile
	erl -noshell -pa _build/test/lib/*/ebin \
	    -eval "erlmcp_benchmark:run_all()" \
	    -s init stop

# Compare to baseline
.PHONY: benchmark-compare
benchmark-compare:
	erl -noshell -eval "erlmcp_benchmark_baseline:get_regression_report(all)" \
	    -s init stop

# Clear baselines (for reset)
.PHONY: benchmark-reset
benchmark-reset:
	rm -rf /tmp/erlmcp_baselines
	rm -rf /tmp/erlmcp_benchmark_results
```

---

## TIMELINE & OWNERSHIP

### Week 1: Cleanup (16 hours)
- [1hr] Identify all deletable files ✓
- [3hr] Delete 9 files, verify no breakage
- [4hr] Consolidate benchmark_100k into SUITE
- [4hr] Consolidate registry stress tests
- [4hr] Consolidate queue benchmarks

### Week 2: Metrology (20 hours)
- [4hr] Create metrology module
- [4hr] Fix percentile calculations (15 files)
- [4hr] Add p99.9/p99.99 measurements (15 files)
- [4hr] Calibrate measurement overhead
- [4hr] Document all standard definitions

### Week 3: Infrastructure (24 hours)
- [4hr] Create baseline manager
- [4hr] Create unified benchmark runner
- [4hr] Create result aggregator
- [4hr] Create regression detector
- [4hr] Test end-to-end
- [4hr] Documentation

### Week 4: Integration (20 hours)
- [4hr] CI/CD integration
- [4hr] Make targets
- [4hr] Quick start guide
- [4hr] Troubleshooting guide
- [4hr] Demo run of full pipeline

**Total: 80 hours (2 weeks full-time equivalent)**

---

## SUCCESS CRITERIA

### Phase 1 ✓
- [ ] 9 files deleted
- [ ] 4 consolidations complete
- [ ] All tests still pass
- [ ] 750 LOC removed

### Phase 2 ✓
- [ ] Metrology module created & documented
- [ ] All percentile calculations use `ceil`
- [ ] All latencies in microseconds
- [ ] Measurement overhead calibrated
- [ ] Missing percentiles (p99.9) added to all tests

### Phase 3 ✓
- [ ] Baseline storage working
- [ ] Regression detection working
- [ ] Result aggregation working
- [ ] `erlmcp_benchmark:run_all()` returns proper format
- [ ] Report generation working

### Phase 4 ✓
- [ ] CI workflow running
- [ ] Make targets all working
- [ ] Documentation complete
- [ ] Can run in 1 command: `make benchmark-all`

---

## WHAT WE'RE KEEPING (The Good Stuff)

### Core Benchmarks
```
✓ benchmark_100k_SUITE.erl          - Comprehensive 100K baseline
✓ throughput_SUITE.erl              - Operation-specific throughput
✓ latency_SUITE.erl                 - Stability and variance analysis
✓ erlmcp_registry_100k_stress_SUITE - Registry micro-benchmarks
✓ erlmcp_cluster_stress_SUITE       - Multi-node testing
```

### Profiling Infrastructure
```
✓ erlmcp_profiling_suite.erl        - Unified coordinator
✓ erlmcp_cpu_profiler.erl           - CPU profiling
✓ erlmcp_memory_profiler.erl        - Memory profiling
✓ erlmcp_latency_profiler.erl       - Latency profiling
```

### Stress Testing
```
✓ erlmcp_master_stress_orchestrator.erl   - Master orchestrator
✓ erlmcp_100k_comprehensive.erl           - Integration test
✓ erlmcp_concurrent_100k.erl              - Concurrency test
✓ erlmcp_logging_100k_stress.erl          - Logging stress
```

### Transports
```
✓ bench/transport_real/tcp_real_bench.erl - Real TCP tests
✓ bench/transport_real/http_real_bench.erl - Real HTTP tests
✓ transport_performance_benchmark.erl      - Transport-agnostic
```

### Utilities
```
✓ erlmcp_performance_benchmark.erl  - Framework (will refactor)
✓ erlmcp_profiling_suite.erl        - Profiler coordinator
✓ erlmcp_stress_validation.erl      - Result validation
✓ erlmcp_stress_results_collector.erl - Result collection
```

---

## WHAT WE'RE REMOVING (The Cruft)

### Simple/Minimal Benchmarks (Too Basic)
```
✗ erlmcp_simple_benchmark.erl        (50 LOC - use comprehensive suite)
✗ erlmcp_simple_stress.erl           (60 LOC - use comprehensive suite)
```

### Development Artifacts
```
✗ test_logging_stress.erl            (old version)
✗ test_quick_stress.erl              (development only)
✗ quick_stress_test.erl              (development only)
```

### Obsolete Runners/Wrappers
```
✗ run_stress_test.erl                (superseded by orchestrators)
✗ erlmcp_master_stress_test.erl      (old version of orchestrator)
```

### Non-SUITE Duplicates
```
✗ registry_100k_stress.erl           (non-SUITE version)
✗ tcp_real_bench_tests.erl           (duplicate with bench/)
```

---

## EXPECTED OUTCOME

### Before Consolidation
```
93 files
25,490 LOC
15+ duplicate scenarios
Inconsistent measurements
No baseline comparison
No regression detection
Multiple entry points
```

### After Consolidation
```
40-50 files
12,000 LOC (-53%)
0 duplicates
Standardized ISO measurements
Automatic baseline comparison
Regression detection
Single entry point: make benchmark-all
Daily CI runs with trend analysis
```

### Developer Experience
```
# Before:
- Which benchmark should I run?
- Why do results differ each time?
- Where are the baselines?
- How do I find regressions?

# After:
$ make benchmark-all           # Run everything
$ make benchmark-compare       # See trends vs baseline
$ cat /tmp/erlmcp_benchmark_results/LATEST_REPORT.md  # View results
```

---

## ROLLBACK PLAN

If consolidation breaks something:

1. All deletion is `git rm`, fully reversible
2. All consolidation is `git mv` + merge, fully reversible
3. Keep backups: `git tag erlmcp-pre-consolidation`
4. Can revert any phase with: `git revert --no-edit <commit>`

---

## QUESTIONS & ANSWERS

**Q: Won't consolidation lose information?**
No. We're moving code, not deleting functionality. Each test case remains intact.

**Q: What about in-flight test runs?**
Use git tags at each phase for release branches.

**Q: Can we run this in smaller steps?**
Yes. Phases are independent. Can do Week 1 + 2, pause, then Week 3 + 4.

**Q: Who maintains the consolidated suite?**
Designated benchmark maintainer (1 person, 2 hours/week for trending/analysis).

**Q: What if tests fail during consolidation?**
Phase 1 deletion includes verification step. If any test fails, don't proceed to Phase 2.

---

## NEXT STEPS

1. **Approval**: Review this plan with team
2. **Week 1**: Execute cleanup (low risk, high impact)
3. **Week 2**: Metrology standardization
4. **Week 3-4**: Infrastructure + CI

**Start Date**: [YYYY-MM-DD]
**Expected Completion**: +4 weeks

---

## APPENDIX: BENCHMARK FILE MANIFEST

### Phase 1 Deletions (750 LOC)
```
test/erlmcp_simple_benchmark.erl       50 LOC - Too basic
test/erlmcp_simple_stress.erl          60 LOC - Too basic
test_logging_stress.erl                40 LOC - Old version
test_quick_stress.erl                  30 LOC - Development
quick_stress_test.erl                  40 LOC - Development
run_stress_test.erl                    80 LOC - Obsolete
test/erlmcp_master_stress_test.erl    150 LOC - Old version
test/registry_100k_stress.erl         200 LOC - Non-SUITE
test/tcp_real_bench_tests.erl         100 LOC - Duplicate
```

### Phase 1 Consolidations (no LOC loss, just reorganization)
```
bench/benchmark_100k.erl (425 LOC) → bench/benchmark_100k_SUITE.erl
bench/erlmcp_registry_contention.erl (400 LOC) → test/erlmcp_registry_100k_stress_SUITE.erl
src/erlmcp_queue_benchmark.erl (300 LOC) → bench/benchmark_100k_SUITE.erl
test/erlmcp_chaos_tests.erl (100 LOC) → test/erlmcp_chaos_test_SUITE.erl
```

### Phase 2 New Files (200 LOC)
```
src/erlmcp_benchmark_metrology.erl (200 LOC) - Standardized measurements
```

### Phase 3 New Files (600 LOC)
```
src/erlmcp_benchmark.erl (300 LOC) - Unified runner
src/erlmcp_benchmark_baseline.erl (200 LOC) - Baseline management
src/erlmcp_benchmark_results.erl (100 LOC) - Result aggregation
```

### Phase 4 Configuration (100 LOC)
```
.github/workflows/erlmcp_benchmarks.yml (50 LOC) - CI/CD
Makefile targets (50 LOC) - Make integration
docs/BENCHMARKING_GUIDE.md (documentation)
```

---

**Document Version**: 1.0
**Last Updated**: 2026-01-27
**Status**: Ready for Implementation
