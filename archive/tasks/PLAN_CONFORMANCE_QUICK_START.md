# Plan Conformance Testing - Quick Start Guide

## Overview

The **Extended Plan Conformance Test Suite** validates that erlmcp meets all service plan envelope claims with real benchmarks and determinism verification.

## Quick Start

### Run All Tests (5 minutes)

```bash
# Run extended conformance tests
make test-plan-conformance

# Or directly with rebar3
rebar3 ct --suite=test/erlmcp_plan_conformance_extended_SUITE
```

### View Results

```bash
# List all result files
ls -la conformance_results/*.json

# Count results
find conformance_results -name "*.json" | wc -l

# View specific test result
cat conformance_results/team_test_throughput_*.json | jq .
```

## What Gets Tested

### Team Tier (6 tests)
- Throughput: 450 req/s sustained
- P99 Latency: ≤ 150ms at 25K concurrent
- Memory: 2.03 MB per connection
- Failover: < 5 seconds
- Queue Depth: ≥ 100K messages
- Refusal Behavior: Deterministic

### Enterprise Tier (6 tests)
- Throughput: 1500 req/s sustained
- P99 Latency: ≤ 100ms at 100K concurrent
- Memory: 1.5 MB per connection
- Failover: < 2 seconds
- Queue Depth: ≥ 500K messages
- Refusal Behavior: Deterministic

### Government Tier (6 tests)
- Throughput: 900 req/s sustained
- P99 Latency: ≤ 150ms at 50K concurrent
- Memory: 1.2 MB per connection
- Failover: < 15 seconds
- Audit Logging: All refusals logged
- FIPS-140-2: Compliance check

### Cross-Plan Tests (3 tests)
- Plan Upgrade: Team → Enterprise
- Boundary Refusals: Smooth degradation
- Multi-Plan Coexistence: Separate limits

## Key Features

### Real Measurements
- Uses actual `erlmcp_benchmark` module
- 60-second duration for each test
- Concurrent load reflects production use
- No simulations or estimates

### Determinism Verification
- Each test runs 3 times
- Variance calculated: (σ / μ) × 100%
- Tolerance: ±2% (proves reproducibility)

### JSON Export
- Every test produces JSON output
- Ready for supply chain evidence
- Includes measured vs required values
- Timestamps for traceability

## Quick Test Execution

```bash
make test-plan-conformance
```

**Expected time:** 7-10 minutes

## Expected Results

All 21 tests pass with measurements matching plan claims:
- Team: 450 req/s, p99≤150ms, 2.03MB/conn
- Enterprise: 1500 req/s, p99≤100ms, 1.5MB/conn
- Government: 900 req/s, p99≤150ms, 1.2MB/conn

## Results Directory

```
conformance_results/
├── team_test_throughput_*.json
├── enterprise_test_latency_*.json
├── gov_test_memory_*.json
└── summary_*.json
```

## Full Documentation

- **Quick Start:** This file
- **Extended Guide:** `docs/plan_conformance_testing_extended.md`
- **Summary:** `docs/PLAN_CONFORMANCE_SUMMARY.md`
- **Test Source:** `test/erlmcp_plan_conformance_extended_SUITE.erl`
