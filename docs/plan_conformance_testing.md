# erlmcp Plan Conformance Testing

## Overview

The plan conformance testing suite validates that erlmcp meets the service envelope claims for Team, Enterprise, and Government tier pricing plans. Each test measures real performance metrics and exports results as JSON for integration with the supply chain evidence system.

## Plan Envelope Definitions

### Team Tier
- **Throughput**: ≥450 req/s sustained
- **Latency P99**: ≤150ms
- **Memory per Connection**: ≤2.03 MB
- **Failover Time**: <5 seconds
- **Queue Depth**: ≥100,000 messages
- **Concurrent Connections**: 25,000
- **Payload Size**: 4KB default

### Enterprise Tier
- **Throughput**: ≥1500 req/s sustained
- **Latency P99**: ≤100ms
- **Memory per Connection**: ≤1.5 MB
- **Failover Time**: <2 seconds
- **Queue Depth**: ≥500,000 messages
- **Concurrent Connections**: 100,000
- **Payload Size**: 8KB default

### Government Tier
- **Throughput**: ≥900 req/s sustained
- **Latency P99**: ≤80ms
- **Memory per Connection**: ≤1.2 MB
- **Failover Time**: <1 second
- **Queue Depth**: ≥250,000 messages
- **Concurrent Connections**: 50,000
- **Payload Size**: 2KB (stricter for compliance)
- **Audit Logging**: Required for all refusals
- **FIPS-140-2**: Compliance verified (if available)

## Test Structure

### Test Suites (21 Tests Total)

#### Team Tier Conformance (6 tests)
1. `test_team_throughput_450_req_sec` - Verify ≥450 req/s sustained for 60s
2. `test_team_p99_latency_under_150ms` - Verify p99 latency ≤150ms at 25K concurrent
3. `test_team_memory_per_conn_2mb` - Verify ≤2.03MB per connection
4. `test_team_failover_under_5s` - Simulate node failure, verify recovery <5s
5. `test_team_queue_depth_100k` - Verify queue holds ≥100K messages
6. `test_team_refusal_behavior_deterministic` - Verify refusal codes at boundaries

#### Enterprise Tier Conformance (6 tests)
1. `test_enterprise_throughput_1500_req_sec` - Verify ≥1500 req/s sustained
2. `test_enterprise_p99_latency_under_100ms` - Verify p99 latency ≤100ms at 100K concurrent
3. `test_enterprise_memory_per_conn_1_5mb` - Verify ≤1.5MB per connection
4. `test_enterprise_failover_under_2s` - Verify failover recovery <2s
5. `test_enterprise_queue_depth_500k` - Verify queue holds ≥500K messages
6. `test_enterprise_refusal_behavior_deterministic` - Verify refusal behavior at boundaries

#### Government Tier Conformance (6 tests)
1. `test_gov_throughput_900_req_sec` - Verify ≥900 req/s sustained
2. `test_gov_p99_latency_under_80ms` - Verify p99 latency ≤80ms at 50K concurrent
3. `test_gov_memory_per_conn_1_2mb` - Verify ≤1.2MB per connection
4. `test_gov_failover_under_1s` - Verify failover recovery <1s
5. `test_gov_audit_logging_enabled` - Verify audit logging for all refusals
6. `test_gov_fips_compliance` - Verify FIPS-140-2 compliance (if available)

#### Cross-Plan Boundary Tests (3 tests)
1. `test_upgrade_team_to_enterprise` - Load team plan, upgrade to enterprise, verify envelope expands
2. `test_refusal_at_boundary` - Test refusal behavior exactly at plan boundaries
3. `test_multiple_plans_coexist` - Verify 3 plans can run simultaneously with separate limits

## Key Features

### Determinism Verification
- Each test runs 3 times to verify consistency
- Tolerance: ±2% variance across runs
- Reports actual measured values, not estimates
- All metrics are real throughput/latency/memory measurements

### Real Benchmark Integration
- Uses `erlmcp_benchmark` module for all measurements
- Throughput measured with concurrent client generation
- Latency measured with percentile analysis (p50, p95, p99)
- Memory measured with system resource monitoring

### JSON Export
- All results exported as JSON for evidence bundle
- Timestamp recorded for each test
- Variance statistics included
- Results stored in `conformance_results/` directory

### Resource Monitoring
- CPU utilization tracking
- Memory usage per connection
- Network I/O monitoring
- Queue depth measurement
- Connection lifecycle tracking

## Running the Tests

### Full Conformance Suite
```bash
make test-plan-conformance
```

This runs all 21 conformance tests and exports results as JSON files.

### With Certification
```bash
make certify-plan
```

This runs the full suite and generates a certification report with evidence bundle integration.

### From Rebar3
```bash
rebar3 ct --suite=test/erlmcp_plan_conformance_SUITE
```

### Individual Test Groups
```bash
# Team tier only (6 tests)
rebar3 ct --suite=test/erlmcp_plan_conformance_SUITE --group team_conformance

# Enterprise tier only (6 tests)
rebar3 ct --suite=test/erlmcp_plan_conformance_SUITE --group enterprise_conformance

# Government tier only (6 tests)
rebar3 ct --suite=test/erlmcp_plan_conformance_SUITE --group gov_conformance

# Cross-plan tests only (3 tests)
rebar3 ct --suite=test/erlmcp_plan_conformance_SUITE --group cross_plan
```

## Results Format

### JSON Result Structure
```json
{
  "plan": "team",
  "test": "test_team_throughput_450_req_sec",
  "status": "pass",
  "measured_value": 485.2,
  "required_value": 450,
  "unit": "req/s",
  "tolerance_pct": 5.0,
  "actual_vs_required_ratio": 1.078,
  "runs": [482.5, 487.1, 486.0],
  "run_variance_pct": 0.45,
  "timestamp": 1706000000000
}
```

### Result Files
Results are exported to `conformance_results/` with naming convention:
- `{plan}_{test_name}_{timestamp}.json` - Individual test results
- `summary_{run_id}.json` - Suite-level summary

## Integration with Evidence System

The conformance testing system integrates with erlmcp's supply chain evidence system:

1. **Baseline Generation**: Results become the baseline for regression detection
2. **CI/CD Pipeline**: Tests run automatically before release
3. **Evidence Bundle**: JSON results included in supply chain evidence
4. **Certification**: Proof that plan envelopes are met
5. **Regression Detection**: Future changes compared against these baselines

## Measurement Details

### Throughput Measurement
- Sustained load generation at target rate
- Measures successful requests per second
- Excludes failed/refused requests
- 60-second sustained measurement for each run

### Latency Measurement
- Generates 10,000+ sample transactions
- Calculates percentiles (50, 75, 90, 95, 99, 99.9, 99.99)
- Excludes outliers (>3σ)
- Phase-specific analysis (submission, consensus, application)

### Memory Measurement
- Process RSS memory tracking
- Per-connection memory calculation
- Heap usage monitoring
- Memory growth rate detection
- Garbage collection impact analysis

### Failover Testing
- Simulates node failure or connection loss
- Measures time from failure detection to recovery
- Verifies automatic reconnection
- Confirms message requeue

### Queue Testing
- Incremental load until queue full
- Records maximum capacity
- Tests backpressure behavior
- Verifies graceful degradation

## Determinism Analysis

Each test verifies reproducibility:

```
Run 1: 485.2 req/s
Run 2: 487.1 req/s
Run 3: 486.0 req/s

Mean: 486.1 req/s
Std Dev: 0.87
Variance: 0.45% ← Within ±2% tolerance
```

## Troubleshooting

### Test Failures

#### Throughput Below Target
- **Cause**: System under heavy load or insufficient resources
- **Solution**: Run on dedicated hardware, ensure clean system state
- **Check**: CPU usage, disk I/O, network saturation

#### P99 Latency High
- **Cause**: GC pauses, memory pressure, network jitter
- **Solution**: Tune GC parameters, increase heap, check network
- **Check**: GC logs, memory pressure, network latency

#### Memory Per Connection Exceeds Limit
- **Cause**: Memory leaks, inefficient session storage
- **Solution**: Check for unclosed connections, verify cleanup
- **Check**: Process dump, ETS table sizes, process monitors

#### Failover Time Exceeds SLA
- **Cause**: Slow failure detection or recovery process
- **Solution**: Tune timeout parameters, improve detection
- **Check**: Supervisor timeouts, health check intervals

### Determinism Issues (Variance >2%)

- **Cause**: System variability, background processes
- **Solution**:
  - Stop unnecessary services
  - Disable power saving
  - Use consistent test hardware
  - Increase warmup time

## Configuration

Conformance test envelopes are defined in the test suite header:

```erlang
-define(TEAM_ENVELOPE, #{
    min_throughput => 450,
    max_p99_latency => 150,
    memory_per_conn => 2.03,
    failover_time => 5000,
    queue_depth => 100000,
    ...
}).
```

To modify envelopes for different tier definitions, edit these maps in `test/erlmcp_plan_conformance_SUITE.erl`.

## CI/CD Integration

### GitHub Actions Example
```yaml
- name: Plan Conformance Tests
  run: make test-plan-conformance

- name: Export Evidence
  run: make certify-plan

- name: Upload Results
  uses: actions/upload-artifact@v3
  with:
    name: conformance-results
    path: conformance_results/
```

### GitLab CI Example
```yaml
plan_conformance:
  script:
    - make test-plan-conformance
    - make certify-plan
  artifacts:
    paths:
      - conformance_results/
```

## Maintenance

### Updating Envelopes
When plan definitions change:
1. Update envelope definitions in test file
2. Re-run full conformance suite
3. Review baseline changes
4. Update evidence bundle
5. Commit with clear message explaining changes

### Performance Regression Detection
The system automatically detects regressions:
- Compare new results against baseline
- Flag any variance >10% as regression
- Investigate root cause
- Optimize if performance degraded

## References

- Benchmark Module: `test/erlmcp_benchmark.erl`
- Performance Analysis: `src/erlmcp_performance_analysis.erl`
- Evidence System: `scripts/release/generate_*` scripts
- Supply Chain: `docs/supply_chain_evidence.md`

## License

Copyright © 2026 erlmcp Contributors
Licensed under Apache License 2.0
