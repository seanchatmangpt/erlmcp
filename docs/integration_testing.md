# TCPS Integration Testing Guide

## Overview

This document describes the comprehensive integration test suite for the TCPS (Toyota-inspired Connected Production System). The test suite validates end-to-end functionality, quality gates, concurrency, performance, and persistence.

## Test Suites

### 1. Pipeline Integration Test Suite (`tcps_pipeline_SUITE`)

**Purpose**: Validate complete end-to-end production pipeline from work order creation through SKU publication.

**Test Coverage**:
- Complete 10-stage production pipeline
- Security patch workflow (expedited processing)
- Feature development workflow (standard processing)
- Andon stop and resume
- Quality gate enforcement
- Heijunka leveling
- Concurrent SKU processing
- Receipt chain verification
- Deterministic build verification
- Kaizen improvement cycle
- Multi-stage failure recovery
- WIP limit enforcement
- Bucket balancing
- Release rollback
- Marketplace publishing

**Key Tests**:
```erlang
test_complete_production_pipeline/1
test_security_patch_workflow/1
test_feature_development_workflow/1
test_andon_stop_and_resume/1
test_quality_gate_enforcement/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_pipeline_SUITE
```

### 2. Andon Integration Test Suite (`tcps_andon_integration_SUITE`)

**Purpose**: Validate Andon cord (stop-the-line) functionality including automatic triggering, pipeline blocking, and resolution.

**Test Coverage**:
- Automatic triggering on quality issues
- Pipeline blocking and resumption
- 5 Whys root cause analysis
- Concurrent Andon handling
- Escalation workflows
- Metrics and tracking
- Notification system
- History tracking
- Multi-failure handling
- Timeout management

**Key Tests**:
```erlang
test_andon_stops_pipeline/1
test_test_failure_triggers_andon/1
test_compilation_error_triggers_andon/1
test_low_coverage_triggers_andon/1
test_5_whys_analysis/1
test_concurrent_andons/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_andon_integration_SUITE
```

### 3. Concurrent Operations Test Suite (`tcps_concurrent_SUITE`)

**Purpose**: Validate system behavior under concurrent load and detect race conditions.

**Test Coverage**:
- 100+ concurrent work orders
- Concurrent Andon triggers
- Pipeline stage concurrency
- Race condition detection
- WIP limits under load
- Receipt integrity
- Deadlock detection
- Resource contention
- Quality gates concurrency
- Parallel releases
- Kaizen updates
- Burst load handling
- Sustained load
- Mixed bucket concurrency
- Heijunka scheduling concurrency

**Key Tests**:
```erlang
test_100_concurrent_work_orders/1
test_concurrent_andon_triggers/1
test_race_condition_detection/1
test_wip_limits_under_load/1
test_receipt_integrity/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_concurrent_SUITE
```

### 4. Quality Gates Test Suite (`tcps_quality_gates_SUITE`)

**Purpose**: Validate quality gate enforcement and zero-defect production standards.

**Test Coverage**:
- Coverage threshold enforcement (80%+)
- Compilation error detection
- Test failure blocking
- Security vulnerability detection
- SHACL validation enforcement
- Deterministic build verification
- Multi-gate failures
- Gate execution order
- Metrics tracking
- Bypass prevention
- Incremental improvement
- Zero tolerance security

**Key Tests**:
```erlang
test_coverage_below_80_triggers_andon/1
test_compilation_errors_trigger_andon/1
test_security_vulnerabilities_trigger_andon/1
test_gate_bypass_prevention/1
test_zero_tolerance_security/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_quality_gates_SUITE
```

### 5. Heijunka Test Suite (`tcps_heijunka_SUITE`)

**Purpose**: Validate production leveling and load balancing across buckets.

**Test Coverage**:
- Anti-batching verification (max 2-3 consecutive)
- WIP limit enforcement
- Bucket distribution
- Priority handling
- Leveling score calculation
- Dynamic rebalancing
- Throughput optimization
- Mixed workload handling
- Burst handling
- Sustained leveling
- Per-bucket WIP limits
- Emergency priority
- Schedule consistency
- Optimal interleaving
- Capacity planning

**Key Tests**:
```erlang
test_prevents_batching/1
test_respects_wip_limits/1
test_bucket_distribution/1
test_priority_handling/1
test_dynamic_rebalancing/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_heijunka_SUITE
```

### 6. Persistence Test Suite (`tcps_persistence_SUITE`)

**Purpose**: Validate data persistence, integrity, and recovery mechanisms.

**Test Coverage**:
- Receipt storage and retrieval (JSON + RDF)
- RDF ontology persistence
- Backup and restore (full + incremental)
- Work order persistence
- Andon persistence
- Metrics persistence
- Corruption recovery
- Concurrent writes
- Large dataset handling
- Query performance
- Cross-session persistence
- Ontology versioning
- Receipt chain integrity
- Disaster recovery

**Key Tests**:
```erlang
test_receipt_roundtrip/1
test_backup_restore/1
test_corruption_recovery/1
test_concurrent_writes/1
test_disaster_recovery/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_persistence_SUITE
```

### 7. Performance Test Suite (`tcps_performance_SUITE`)

**Purpose**: Validate performance characteristics and identify bottlenecks.

**Test Coverage**:
- Throughput measurements (1000 work orders under 10 seconds)
- Latency profiling (per-stage)
- Resource utilization (memory, CPU, disk I/O)
- Scalability testing
- Bottleneck identification
- Performance regression detection
- Sustained throughput (30+ seconds)
- Burst performance (200 instant work orders)
- Query performance
- Concurrent scalability
- Heijunka scheduling performance
- Quality gate throughput
- Receipt generation throughput

**Key Tests**:
```erlang
test_1000_work_orders_under_10_seconds/1
test_pipeline_stage_latency/1
test_concurrent_processing_scalability/1
test_memory_usage/1
test_bottleneck_identification/1
```

**Running**:
```bash
rebar3 ct --suite=test/integration/tcps_performance_SUITE
```

## Running All Integration Tests

### Run All Suites
```bash
make integration-tests
```

### Run Specific Suite
```bash
rebar3 ct --suite=test/integration/tcps_pipeline_SUITE
```

### Run Specific Test
```bash
rebar3 ct --suite=test/integration/tcps_pipeline_SUITE --case=test_complete_production_pipeline
```

### Run with Verbose Output
```bash
rebar3 ct --suite=test/integration/tcps_pipeline_SUITE --verbose
```

### Run with Coverage
```bash
rebar3 ct --suite=test/integration/tcps_pipeline_SUITE --cover
```

## Test Utilities (`tcps_test_utils`)

The `tcps_test_utils` module provides comprehensive test helpers:

### Mock Services
- `start_github_mock/0` - Mock GitHub API
- `start_marketplace_mock/0` - Mock marketplace API
- `start_otel_mock/0` - Mock OpenTelemetry collector

### Test Data Creation
- `create_test_work_order/0` - Create test work order
- `create_work_orders/2` - Create multiple work orders
- `generate_id/0` - Generate unique ID
- `generate_signature/0` - Generate mock signature

### Failure Injection
- `inject_test_failure/2` - Inject test failure
- `inject_compilation_error/2` - Inject compilation error
- `inject_low_coverage/2` - Inject low test coverage
- `inject_security_vulnerability/2` - Inject security issue

### Pipeline Execution
- `process_work_order_full/1` - Process complete pipeline
- `process_pipeline_stages/1` - Process specific stages
- `run_shacl_validation/1` - Run SHACL validation
- `run_compilation/1` - Run compilation
- `run_tests/1` - Run tests

### Async Waiting
- `wait_for_andon/1` - Wait for Andon to trigger
- `wait_for_andon_type/1` - Wait for specific Andon type
- `wait_until/2` - Wait for condition with timeout

### Verification
- `verify_no_duplicate_receipts/0` - Check receipt uniqueness
- `verify_all_receipts_valid/0` - Verify receipt signatures

## Interpreting Test Results

### Success Output
```
=== Starting test case: test_complete_production_pipeline ===
Created work order: wo-123
Work order scheduled in position: 1
SHACL validation passed: 150 rules checked
Compilation succeeded: 25 modules
Tests passed: 150 tests, 85.50% coverage
Quality gates passed: all 4 gates
=== Complete Production Pipeline: SUCCESS ===
```

### Failure Output
```
=== Starting test case: test_coverage_below_80_triggers_andon ===
Quality gate failed as expected
Low coverage Andon triggered: 75.0% < 80.0%
FAILED: Expected {ok, proceed}, got {blocked, [andon-456]}
```

### Performance Output
```
=== Testing 1000 Work Orders Under 10 Seconds ===
Created 1000 work orders in 2341 ms (427.20/s)
Processed 1000 work orders in 8945 ms (111.80/s)
Throughput: 111.80 work orders/second
=== 1000 Work Orders Under 10 Seconds: SUCCESS ===
```

## Troubleshooting

### Test Failures

#### 1. Timeouts

**Symptom**: Tests timeout after 2 minutes
```
{timeout, wo-123}
```

**Causes**:
- Slow test environment
- Resource exhaustion
- Deadlock

**Solutions**:
```bash
# Increase timeout in suite/0
suite() -> [{timetrap, {minutes, 10}}].

# Run single test to isolate
rebar3 ct --suite=tcps_pipeline_SUITE --case=test_complete_production_pipeline
```

#### 2. Race Conditions

**Symptom**: Intermittent failures
```
Expected 100 receipts, got 98
```

**Causes**:
- Async operations not completed
- Missing synchronization

**Solutions**:
```erlang
% Use wait_until/2 for async operations
ok = wait_until(fun() ->
    length(tcps_persistence:get_all_receipts()) >= 100
end, 10000).
```

#### 3. Resource Exhaustion

**Symptom**: Out of memory or file descriptors
```
{error, enomem}
{error, emfile}
```

**Solutions**:
```bash
# Increase limits
ulimit -n 10000  # File descriptors
export ERL_MAX_PORTS=10000

# Run tests in smaller batches
rebar3 ct --suite=tcps_concurrent_SUITE --case=test_100_concurrent_work_orders
```

#### 4. Data Corruption

**Symptom**: Invalid receipts or broken chains
```
{error, chain_broken}
```

**Solutions**:
```bash
# Clean test data
rm -rf /tmp/tcps_test_data

# Reinitialize
rebar3 ct --suite=tcps_persistence_SUITE --case=test_receipt_roundtrip
```

### Performance Issues

#### 1. Slow Tests

**Symptom**: Tests take longer than expected

**Diagnosis**:
```bash
# Run with profiling
rebar3 ct --suite=tcps_performance_SUITE --case=test_bottleneck_identification
```

**Solutions**:
- Optimize bottlenecks identified
- Reduce test dataset size
- Use mocks for slow operations

#### 2. High Memory Usage

**Symptom**: Memory consumption grows during tests

**Diagnosis**:
```bash
# Monitor memory
rebar3 ct --suite=tcps_performance_SUITE --case=test_memory_usage
```

**Solutions**:
- Add garbage collection between tests
- Clear test data more frequently
- Reduce concurrent test count

## Continuous Integration

### GitHub Actions Example
```yaml
name: Integration Tests

on: [push, pull_request]

jobs:
  integration:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22'
      - run: rebar3 compile
      - run: make integration-tests
      - uses: actions/upload-artifact@v2
        if: failure()
        with:
          name: test-results
          path: _build/test/logs/
```

### GitLab CI Example
```yaml
integration-tests:
  stage: test
  image: erlang:26
  script:
    - rebar3 compile
    - make integration-tests
  artifacts:
    when: always
    paths:
      - _build/test/logs/
```

## Best Practices

### 1. Test Isolation
- Each test should be independent
- Use `init_per_testcase/2` to reset state
- Clean up in `end_per_testcase/2`

### 2. Deterministic Tests
- Avoid timing dependencies
- Use `wait_until/2` instead of `timer:sleep/1`
- Control random behavior

### 3. Meaningful Assertions
```erlang
% Bad
?assert(Result).

% Good
?assertEqual(expected_result, Result),
?assertMatch({ok, _}, Result),
?assert(Coverage >= 80.0).
```

### 4. Clear Test Names
```erlang
% Bad
test_1/1

% Good
test_coverage_below_80_triggers_andon/1
```

### 5. Test Documentation
```erlang
%%--------------------------------------------------------------------
%% @doc Test that coverage below 80% triggers Andon
%%
%% This test verifies that:
%% 1. Work order with 75% coverage fails quality gate
%% 2. Andon is automatically triggered
%% 3. Pipeline is blocked from proceeding
%% @end
%%--------------------------------------------------------------------
test_coverage_below_80_triggers_andon(_Config) ->
    ...
```

## Performance Targets

| Metric | Target | Test |
|--------|--------|------|
| Throughput | 100+ work orders/sec | `test_1000_work_orders_under_10_seconds` |
| Pipeline Latency | < 100ms per stage | `test_pipeline_stage_latency` |
| Andon Trigger | < 100ms | `test_andon_trigger_latency` |
| Receipt Generation | 100+ receipts/sec | `test_receipt_generation_throughput` |
| Query Latency | < 100ms for 100 work orders | `test_query_performance` |
| Memory Usage | < 10KB per work order | `test_memory_usage` |
| Concurrent Scaling | Sub-linear with concurrency | `test_concurrent_processing_scalability` |

## Coverage Goals

- **Overall Coverage**: 80%+ (enforced by quality gates)
- **Integration Coverage**: 90%+ (comprehensive E2E scenarios)
- **Edge Cases**: 100% (all error paths tested)

## Maintenance

### Adding New Tests

1. Create test in appropriate suite:
```erlang
test_new_feature(_Config) ->
    ct:pal("~n=== Testing New Feature ===~n"),

    % Setup
    {ok, WorkOrderId} = create_test_work_order(),

    % Execute
    ok = tcps_new_feature:execute(WorkOrderId),

    % Verify
    {ok, Result} = tcps_new_feature:get_result(WorkOrderId),
    ?assertEqual(expected, Result),

    ct:pal("~n=== New Feature: SUCCESS ===~n"),
    ok.
```

2. Add to `all/0`:
```erlang
all() -> [
    test_existing_feature,
    test_new_feature  % Add here
].
```

3. Run test:
```bash
rebar3 ct --suite=tcps_pipeline_SUITE --case=test_new_feature
```

### Updating Tests

When production code changes:
1. Update test expectations
2. Add new test cases for new behavior
3. Ensure backward compatibility tests pass
4. Update documentation

## Support

For issues or questions:
- GitHub Issues: https://github.com/your-org/tcps/issues
- Internal Wiki: https://wiki.your-org.com/tcps/testing
- Slack: #tcps-testing

## References

- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)
- [EUnit Reference](https://www.erlang.org/doc/apps/eunit/chapter.html)
- [TCPS Architecture Documentation](./architecture.md)
- [TCPS Development Plan](../PLAN_v0.6.0.md)
