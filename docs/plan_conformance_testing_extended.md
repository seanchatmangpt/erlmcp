# Plan Conformance Testing Extended Guide

## Overview

The **Extended Plan Conformance Test Suite** (`erlmcp_plan_conformance_extended_SUITE.erl`) provides comprehensive real-world validation that erlmcp's performance exactly matches the service plan envelope claims.

This document explains:
- How to run the extended test suite
- Test coverage and what each test validates
- How results are measured and exported
- Integration with the evidence bundle system
- Determinism verification approach

## Quick Start

### Run All Conformance Tests

```bash
# Run extended conformance tests only
rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE

# Run using make target
make test-plan-conformance

# Full certification (extended + original tests)
make certify-plan
```

### Results Directory

All test results are exported to `conformance_results/`:

```
conformance_results/
├── team_test_throughput_450_req_sec_<timestamp>.json
├── team_test_p99_latency_under_150ms_<timestamp>.json
├── team_test_memory_per_conn_2mb_<timestamp>.json
├── team_test_failover_under_5s_<timestamp>.json
├── team_test_queue_depth_100k_<timestamp>.json
├── team_test_refusal_behavior_deterministic_<timestamp>.json
├── enterprise_test_throughput_1500_req_sec_<timestamp>.json
├── enterprise_test_p99_latency_under_100ms_<timestamp>.json
├── enterprise_test_memory_per_conn_1_5mb_<timestamp>.json
├── enterprise_test_failover_under_2s_<timestamp>.json
├── enterprise_test_queue_depth_500k_<timestamp>.json
├── enterprise_test_refusal_behavior_deterministic_<timestamp>.json
├── gov_test_throughput_900_req_sec_<timestamp>.json
├── gov_test_p99_latency_under_80ms_<timestamp>.json
├── gov_test_memory_per_conn_1_2mb_<timestamp>.json
├── gov_test_failover_under_1s_<timestamp>.json
├── gov_test_audit_logging_enabled_<timestamp>.json
├── gov_test_fips_compliance_<timestamp>.json
├── test_upgrade_team_to_enterprise_<timestamp>.json
├── test_refusal_at_boundary_<timestamp>.json
├── test_multiple_plans_coexist_<timestamp>.json
└── summary_<run_id>.json
```

## Test Suite Architecture

### Test Organization

The suite is organized into 4 test groups with 21 total tests:

1. **Team Tier Conformance (6 tests)**
2. **Enterprise Tier Conformance (6 tests)**
3. **Government Tier Conformance (6 tests)**
4. **Cross-Plan Boundary Tests (3 tests)**

### Test Execution Flow

```
init_per_suite()
  ├─ Start erlmcp application
  ├─ Initialize benchmark system
  └─ Create conformance_results directory

Team Tier Tests (6)
  ├─ test_team_throughput_450_req_sec
  ├─ test_team_p99_latency_under_150ms
  ├─ test_team_memory_per_conn_2mb
  ├─ test_team_failover_under_5s
  ├─ test_team_queue_depth_100k
  └─ test_team_refusal_behavior_deterministic

Enterprise Tier Tests (6)
  ├─ test_enterprise_throughput_1500_req_sec
  ├─ test_enterprise_p99_latency_under_100ms
  ├─ test_enterprise_memory_per_conn_1_5mb
  ├─ test_enterprise_failover_under_2s
  ├─ test_enterprise_queue_depth_500k
  └─ test_enterprise_refusal_behavior_deterministic

Government Tier Tests (6)
  ├─ test_gov_throughput_900_req_sec
  ├─ test_gov_p99_latency_under_80ms
  ├─ test_gov_memory_per_conn_1_2mb
  ├─ test_gov_failover_under_1s
  ├─ test_gov_audit_logging_enabled
  └─ test_gov_fips_compliance

Cross-Plan Tests (3)
  ├─ test_upgrade_team_to_enterprise
  ├─ test_refusal_at_boundary
  └─ test_multiple_plans_coexist

end_per_suite()
  └─ Export suite summary JSON
```

## Detailed Test Coverage

### Team Tier Tests

#### 1. test_team_throughput_450_req_sec

**Claim:** Team tier sustains 450 req/s at 4KB payloads

**How It Works:**
1. Runs 3 separate throughput benchmarks concurrently
2. Each run measures transactions per second over 60 seconds
3. Calculates variance across runs (must be ≤ ±2%)
4. Verifies average throughput ≥ 427.5 req/s (450 * 0.95 tolerance)

**Measurement Details:**
- Concurrent connections: 25,000
- Payload size: 4,096 bytes
- Duration: 60 seconds
- Success rate tolerance: 95%+

**Expected Output:**
```json
{
  "plan": "team",
  "test": "test_team_throughput_450_req_sec",
  "status": "pass",
  "measured_value": 450.3,
  "required_value": 450,
  "unit": "req/s",
  "actual_vs_required_ratio": 1.0007,
  "runs": [449.8, 450.5, 450.1],
  "run_variance_pct": 0.15
}
```

#### 2. test_team_p99_latency_under_150ms

**Claim:** P99 latency ≤ 150ms at 25K concurrent

**How It Works:**
1. Runs 3 separate latency benchmarks
2. Collects 10,000+ transaction latencies per run
3. Calculates 99th percentile (p99)
4. Verifies p99 ≤ 157.5ms (150 * 1.05 tolerance)

**Measurement Details:**
- Concurrent connections: 25,000
- Sample count: 10,000+ transactions
- Warmup: 1,000 transactions (discarded)

**Expected Output:**
```json
{
  "plan": "team",
  "test": "test_team_p99_latency_under_150ms",
  "status": "pass",
  "measured_value": 148.2,
  "required_value": 150,
  "unit": "ms",
  "actual_vs_required_ratio": 0.988,
  "runs": [147.8, 148.5, 148.3],
  "run_variance_pct": 0.23
}
```

#### 3. test_team_memory_per_conn_2mb

**Claim:** Memory usage ≤ 2.03 MB per connection

**How It Works:**
1. Runs 3 separate memory benchmarks at 25K connections
2. Measures peak RSS (resident set size)
3. Divides peak by connection count
4. Verifies average ≤ 2.233 MB/conn (2.03 * 1.10 tolerance)

**Measurement Details:**
- Concurrent connections: 25,000
- Metric: Peak memory / connection count
- Tolerance: 10% (memory measurements are noisy)

**Expected Output:**
```json
{
  "plan": "team",
  "test": "test_team_memory_per_conn_2mb",
  "status": "pass",
  "measured_value": 2.01,
  "required_value": 2.03,
  "unit": "MB",
  "actual_vs_required_ratio": 0.990,
  "runs": [2.02, 2.00, 2.01],
  "run_variance_pct": 0.89
}
```

#### 4. test_team_failover_under_5s

**Claim:** Failover recovery < 5 seconds

**How It Works:**
1. Simulates node failure (kills connection or process)
2. Measures time from failure to system recovery
3. Verifies ≤ 5000ms across all 3 runs
4. Tests supervision tree restart mechanics

**Measurement Details:**
- Trigger: Process/connection termination
- Recovery check: Supervision tree restore
- Timeout: 5000ms (5 seconds)

**Expected Output:**
```json
{
  "plan": "team",
  "test": "test_team_failover_under_5s",
  "status": "pass",
  "measured_value": 3245,
  "required_value": 5000,
  "unit": "ms",
  "actual_vs_required_ratio": 0.649,
  "runs": [3250, 3240, 3245],
  "run_variance_pct": 0.15
}
```

#### 5. test_team_queue_depth_100k

**Claim:** Queue supports ≥ 100K messages before refusal

**How It Works:**
1. Gradually enqueue messages until refusal occurs
2. Tracks maximum accepted queue depth
3. Verifies ≥ 100,000 messages across all runs
4. Tests backpressure mechanics

**Measurement Details:**
- Increment: 10,000 messages per iteration
- Stopping condition: queue_full error
- Minimum capacity: 100,000 messages

**Expected Output:**
```json
{
  "plan": "team",
  "test": "test_team_queue_depth_100k",
  "status": "pass",
  "measured_value": 102000,
  "required_value": 100000,
  "unit": "messages",
  "actual_vs_required_ratio": 1.02,
  "runs": [101000, 102000, 103000],
  "run_variance_pct": 0.95
}
```

#### 6. test_team_refusal_behavior_deterministic

**Claim:** Refusal at boundary is deterministic (no loss, graceful degradation)

**How It Works:**
1. Tests at 427.5 req/s (below boundary) → all succeed
2. Tests at 450 req/s (at boundary) → all succeed
3. Tests at 472.5 req/s (above boundary) → graceful failure
4. Verifies success rate degrades smoothly above boundary

**Measurement Details:**
- Below boundary: ≥95% success rate
- At boundary: ≥95% success rate
- Above boundary: <95% success rate (graceful)

**Expected Output:**
```json
{
  "plan": "team",
  "test": "test_team_refusal_behavior_deterministic",
  "status": "pass",
  "measured_value": 72.5,
  "required_value": 100.0,
  "unit": "%",
  "actual_vs_required_ratio": 0.725,
  "runs": [450.2, 450.1, 427.3],
  "run_variance_pct": 1.34
}
```

### Enterprise Tier Tests

Same structure as Team tier but with Enterprise envelope claims:

- **Throughput:** 1500 req/s (8KB payloads)
- **P99 Latency:** ≤ 100ms at 100K concurrent
- **Memory:** 1.5 MB per connection at 100K scale
- **Failover:** < 2 seconds
- **Queue Depth:** ≥ 500K messages
- **Determinism:** ±2% variance across runs

### Government Tier Tests

Same structure with Government-specific claims:

- **Throughput:** 900 req/s (2KB payloads)
- **P99 Latency:** ≤ 150ms at 50K concurrent
- **Memory:** 1.2 MB per connection at 50K scale
- **Failover:** < 15 seconds
- **Audit Logging:** All refusals logged
- **FIPS-140-2:** Compliance verified if available

### Cross-Plan Boundary Tests

#### 1. test_upgrade_team_to_enterprise

**Tests:** Plan envelope expansion on upgrade

**How It Works:**
1. Load Team plan, measure baseline throughput
2. Upgrade to Enterprise plan
3. Measure throughput again
4. Verify Enterprise ≥ Team (envelope expanded, no loss)

#### 2. test_refusal_at_boundary

**Tests:** Graceful refusal at exact boundaries

**How It Works:**
1. Send load at 95% of boundary (should all succeed)
2. Send load at exactly boundary (should succeed)
3. Send load at 105% of boundary (should gracefully refuse)
4. Verify success rate degrades smoothly

#### 3. test_multiple_plans_coexist

**Tests:** Multiple plans with separate limit enforcement

**How It Works:**
1. Load Team, Enterprise, and Gov plans
2. Measure throughput for each independently
3. Verify all plans are operational
4. Verify separate limit enforcement works

## Determinism Verification

All tests verify determinism across 3 runs using variance calculation:

```erlang
Variance = ((σ / μ) × 100) %

Where:
  σ = standard deviation
  μ = mean
```

**Tolerance:** ±2% (variance_pct ≤ 2.0)

**Example:**
```
Run 1: 450.2 req/s
Run 2: 450.5 req/s
Run 3: 449.8 req/s

Mean = (450.2 + 450.5 + 449.8) / 3 = 450.17
Variance = 0.273 %  ✓ (within ±2% tolerance)
```

## JSON Export Format

Each test exports a JSON file with this structure:

```json
{
  "plan": "team|enterprise|gov",
  "test": "<test_function_name>",
  "status": "pass|fail|skip",
  "description": "What the test validates",
  "measured_value": 450.3,
  "required_value": 450,
  "unit": "req/s|ms|MB|messages|%|status",
  "tolerance_pct": 5.0,
  "actual_vs_required_ratio": 1.0007,
  "runs": [449.8, 450.5, 450.1],
  "run_variance_pct": 0.15,
  "timestamp": 1234567890123
}
```

### Suite Summary

Exported as `summary_<run_id>.json`:

```json
{
  "test_run_id": 12345,
  "start_time": 1234567890000,
  "end_time": 1234567893500,
  "duration_ms": 3500,
  "timestamp": 1234567893500,
  "results_dir": "/path/to/conformance_results"
}
```

## Integration with Evidence Bundle

The JSON results are ready for integration with the supply chain evidence system:

```bash
# Copy results to evidence bundle
cp conformance_results/*.json /path/to/evidence-bundle/

# JSON files can be parsed and included in certification reports
```

## Measurement Tolerances

### Throughput
- Tolerance: 5% below requirement
- Rationale: Natural variance in benchmarking

### Latency
- Tolerance: 5% above requirement
- Rationale: Percentile variance, measurement overhead

### Memory
- Tolerance: 10% above requirement
- Rationale: GC variance, memory fragmentation

### Failover
- Tolerance: 0% (hard deadline)
- Rationale: SLA requirement

### Queue Depth
- Tolerance: 0% (minimum capacity)
- Rationale: Envelope specification

## Running Specific Tests

```bash
# Run only Team tier tests
rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE \
          --group=team_tier_tests

# Run only throughput tests
rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE \
          --testcase=test_team_throughput_450_req_sec

# Run with verbose output
rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE -v

# Run with profiling
rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE \
          --logdir=ct_logs --keep_logs
```

## Troubleshooting

### Test Fails: "Variance exceeds tolerance"

**Cause:** System under stress or GC pauses

**Solution:**
```bash
# Run with dedicated CPU
taskset -c 0-7 rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE

# Increase test duration (wait for stable state)
# Modify test duration in envelope definition
```

### Test Fails: "Throughput below minimum"

**Cause:** System not achieving required performance

**Check:**
1. System CPU usage: `top`, `htop`
2. Memory pressure: `vm.stat` (macOS), `free` (Linux)
3. Network bandwidth: `iftop`
4. Disk I/O: `iostat`

### Results Missing

**Check:**
```bash
# Verify conformance_results directory
ls -la conformance_results/

# Check CT logs
cat _build/test/logs/ct_run_*/all.log
```

## Performance Benchmarking Details

### How Measurements Are Taken

#### Throughput Measurement
1. Submit transactions at target rate
2. Count successful submissions per second
3. Calculate average throughput
4. Record variance across runs

#### Latency Measurement
1. Measure time from submission to response
2. Collect 10,000+ samples
3. Calculate percentiles (p50, p95, p99, p99.9)
4. Report p99 (99th percentile)

#### Memory Measurement
1. Record peak RSS before test
2. Create connections and transactions
3. Record peak RSS during test
4. Divide peak memory by connection count

## FAQ

**Q: Why ±2% determinism tolerance?**
A: Normal variance in benchmarking due to OS scheduling, GC, and measurement overhead. ±2% represents "essentially identical" results.

**Q: What if FIPS-140-2 is not available?**
A: Test is marked as `skip` (not fail). Gov tier will enforce FIPS in production.

**Q: How long do tests take?**
A: ~5-10 minutes depending on system load and test parameters.

**Q: Can I modify test parameters?**
A: Yes, edit the envelope definitions in the test file (marked with `-define`).

**Q: Are these real measurements or simulations?**
A: Real measurements using `erlmcp_benchmark` module. No fudging or estimates.

## Supply Chain Evidence Integration

Results are formatted for supply chain evidence system:

1. **File naming:** Includes timestamp for traceability
2. **JSON schema:** Validated against submission requirements
3. **Determinism proof:** 3 runs demonstrate reproducibility
4. **Measurement accuracy:** Real benchmarks vs estimates

Example integration:

```bash
# Generate evidence for certification
make certify-plan

# Collect results
find conformance_results/ -name "*.json" -exec \
  jq '.measured_value, .required_value' {} \;

# Create evidence digest
sha256sum conformance_results/*.json > conformance_results/DIGEST.sha256

# Submit to evidence bundle
tar czf erlmcp_plan_conformance_evidence.tar.gz conformance_results/
```

## References

- Plan definitions: `/Users/sac/erlmcp/plans/[team|enterprise|gov].plan.json`
- Benchmark module: `/Users/sac/erlmcp/test/erlmcp_benchmark.erl`
- Original conformance suite: `/Users/sac/erlmcp/test/erlmcp_plan_conformance_SUITE.erl`
- Makefile targets: `make test-plan-conformance`, `make certify-plan`
