# Nine-Nines Chaos Testing Suite

## Overview

This test suite validates erlmcp's **nine-nines (99.9999999%) availability posture** under extreme conditions using Chicago School TDD methodology (real processes, no mocks).

## Architecture

### Part 1: PropEr Model-Based FSM Tests (`prop_protocol_fsm.erl`)

Property-based state machine tests using PropEr statem framework.

**State Machines Tested:**
- **Client FSM**: `pre_initialization → initializing → initialized → error/closed`
- **Server FSM**: `initialization → initialized → error/closed`
- **Session Lifecycle**: State transitions and consistency
- **Cancellation Races**: Cancel vs result arriving simultaneously
- **Priority Messages**: Control plane under data flood

**Properties (7 total):**
1. `prop_client_fsm()` - Client state transitions are valid
2. `prop_server_fsm()` - Server maintains invariants
3. `prop_cancellation_races()` - Cancellation races handled correctly
4. `prop_priority_message_handling()` - Priority messages don't block under load
5. `prop_deadlock_freedom()` - No deadlocks with concurrent clients/servers
6. `prop_session_lifecycle()` - Session state remains consistent
7. `prop_state_transition_invariants()` - No transitions to undefined states

**Default Configuration:**
- 100 test cases per property (quick mode)
- 1000 test cases per property (full mode)

### Part 2: CT Chaos Nine-Nines Suite (`ct_chaos_nine_nines_SUITE.erl`)

Common Test suite with 8 extreme chaos scenarios.

**Scenario 1: Mailbox Saturation**
- Flood server with 100K messages/sec for 5 seconds
- Measure control plane latency (health checks, drains)
- **SLO**: Control plane p99 latency <100ms
- **Validates**: Priority message handling, queue management

**Scenario 2: Cascading Failure**
- Kill session supervisor
- Measure recovery time
- Verify clients reconnect
- **SLO**: RTO (Recovery Time Objective) <5 seconds
- **Validates**: Supervisor restart strategy, let-it-crash

**Scenario 3: Network Partition**
- Simulate TCP blackhole (packets dropped, no RST)
- Measure timeout detection speed
- Verify session state preserved
- **SLO**: Detection <30 seconds
- **Validates**: Timeout handling, state preservation

**Scenario 4: Resource Exhaustion**
- Spawn 10K concurrent sessions (1K in quick mode)
- Measure memory per session
- Verify GC pause times
- **SLO**: Memory <15 MiB/session (10 MiB heap + 5 MiB RSS)
- **SLO**: GC pause <100ms even at 40K connections
- **Validates**: Memory management, scalability

**Scenario 5: Priority Message Handling**
- Send 1K control messages during 100K msg/sec data flood
- Measure control message latency
- **SLO**: Priority message p99 latency <50ms
- **Validates**: Message prioritization, selective receive

**Scenario 6: Task Execution Under Load**
- Submit 10K tasks while flooding with data
- Measure task completion time variance
- Verify no task starvation
- **SLO**: Tasks complete within 30 seconds
- **Validates**: Queue shedding, backpressure

**Scenario 7: State Machine Consistency**
- Hammer server with 10K concurrent state requests from 100 processes
- Verify no transitions to undefined states
- **SLO**: Server maintains valid phase
- **Validates**: Concurrent access, state consistency

**Scenario 8: Recovery Discipline**
- Kill server with configured resources/tools/prompts
- Verify supervisor restart restores invariants
- **SLO**: Recovery successful
- **Validates**: Supervisor discipline, invariant restoration

## Metrics Collection (`erlmcp_chaos_metrics.erl`)

Real-time metrics collection during chaos experiments.

**Metrics Tracked:**
- **Latency**: p50, p95, p99, p999 in microseconds (per role: client, server, registry, session, transport)
- **Throughput**: msg/sec per role
- **Memory**: heap MiB, RSS MiB per process
- **GC Pauses**: average, max, p99 in microseconds
- **Queue Depths**: average, max, p95

**Storage:**
- In-memory during test run
- Export to JSON files: `_build/test/logs/metrics/scenario_*.json`
- Aggregate in HTML report

## SLO Targets

| Metric | Target | Scenario |
|--------|--------|----------|
| Control plane p99 latency | <100ms | 1, 5 |
| Priority message p99 latency | <50ms | 5 |
| RTO (Recovery Time Objective) | <5 seconds | 2 |
| Memory per session | <15 MiB | 4 |
| GC pause (at 40K connections) | <100ms | 4 |
| Network partition detection | <30 seconds | 3 |
| Task completion under load | <30 seconds | 6 |
| State consistency | No undefined states | 7 |
| Supervisor recovery | Invariants restored | 8 |

## Running the Tests

### Quick Start

```bash
# Full test suite (production-grade)
./test/run_nine_nines_tests.sh --full

# Quick validation (reduced samples)
./test/run_nine_nines_tests.sh --quick
```

### Manual Execution

```bash
# Part 1: PropEr FSM Tests
rebar3 as test proper --module=prop_protocol_fsm --numtests=1000

# Part 2: CT Chaos Suite
rebar3 ct --suite=test/ct_chaos_nine_nines_SUITE

# View coverage
rebar3 cover --verbose
```

### Individual Scenarios

```bash
# Run single chaos scenario
rebar3 ct --suite=test/ct_chaos_nine_nines_SUITE --case=scenario_1_mailbox_saturation

# Run PropEr property
rebar3 as test proper --module=prop_protocol_fsm --property=prop_client_fsm
```

## Output

### Terminal Output

```
=== Step 1: Compiling erlmcp ===
Compiled successfully

=== Step 2: Running PropEr FSM Tests ===
Testing protocol state machines with 1000 samples per property...
.....................................
OK: Passed 1000 test(s).

=== Step 3: Running Chaos Nine-Nines Test Suite ===
Testing extreme scenarios with 10000 concurrent sessions...

Scenario 1: Mailbox Saturation
  Control plane P99 latency: 87 ms
  ✅ PASS

Scenario 2: Cascading Failure
  RTO: 234 ms
  ✅ PASS

[... remaining scenarios ...]

=== Step 4: Generating HTML Report ===
Report generated: _build/test/logs/nine_nines_report.html

════════════════════════════════════════════
  NINE-NINES POSTURE: ✅ ACHIEVED
════════════════════════════════════════════

=== Step 5: Metrics Summary ===
Metrics collected:
  - scenario_1.json (2.3K)
  - scenario_2.json (1.8K)
  [...]
```

### HTML Report

Located at: `_build/test/logs/nine_nines_report.html`

**Includes:**
- Overall verdict: ✅ ACHIEVED or ⚠️ SLA VIOLATIONS
- Per-scenario breakdown with pass/fail status
- Key metrics table
- Timestamp and test configuration

### Metrics Files

JSON files in: `_build/test/logs/ct_run.*/metrics/`

**Example (`scenario_1.json`):**
```json
{
  "scenario": "mailbox_saturation",
  "baseline_latency_ms": 2,
  "p50_latency_ms": 45,
  "p95_latency_ms": 78,
  "p99_latency_ms": 87,
  "total_messages": 500000,
  "server_survived": true,
  "slo_met": true
}
```

## Chicago School TDD Compliance

This test suite follows Chicago School TDD principles:

✅ **Real Processes**: All tests use actual `erlmcp_client`, `erlmcp_server` gen_servers
✅ **No Mocks**: No meck, no fakes, no stubs - real supervision trees
✅ **State-Based Verification**: Assert on observable outputs, not internal calls
✅ **Behavior Testing**: Test what system does (results), not how it does it (implementation)
✅ **Real Collaborators**: Multiple processes coordinate via real message passing
✅ **Integration Focus**: Components tested together (client + server + registry)

## Performance Baseline (Jan 2026)

Expected baseline metrics (before chaos):

| Metric | Baseline | Source |
|--------|----------|--------|
| Registry ops | 553K msg/s | `erlmcp_bench_core_ops` |
| Queue ops | 971K msg/s | `erlmcp_bench_core_ops` |
| Session ops | 242K msg/s | `erlmcp_bench_core_ops` |
| Network I/O | 43K msg/s | `erlmcp_bench_network_real` |
| Sustained throughput | 372K msg/s | `erlmcp_bench_stress` |
| Connections/node | 40-50K | `erlmcp_bench_stress` |

**Chaos Impact Tolerance:**
- Throughput degradation: <10% under chaos
- Latency increase: <2x under chaos (control plane <100ms, priority <50ms)
- Memory increase: Linear with load, <15 MiB/session

## Interpreting Results

### ✅ PASS Criteria

All scenarios must meet SLOs:
- Mailbox saturation: P99 latency <100ms
- Cascading failure: RTO <5 seconds
- Network partition: Detection <30 seconds
- Resource exhaustion: Memory <15 MiB/session, GC <100ms
- Priority messages: P99 latency <50ms
- Task execution: Complete within 30 seconds
- State consistency: No undefined states
- Recovery: Invariants restored

**Verdict**: Nine-nines posture ✅ ACHIEVED

### ⚠️ FAIL Criteria

If any scenario fails SLO:
- Review HTML report for specific violation
- Check metrics JSON for detailed measurements
- Identify bottleneck (mailbox, memory, GC, supervision)
- Optimize and re-run

**Common Issues:**
1. **Mailbox saturation** → Add selective receive for priority messages
2. **Memory exhaustion** → Enable hibernation, reduce per-session state
3. **Slow RTO** → Tune supervisor restart strategy, max_restarts
4. **GC pauses** → Tune heap sizes, enable incremental GC

## Integration with CI/CD

### Pre-Merge Quality Gate

```yaml
# .github/workflows/nine-nines-test.yml
name: Nine-Nines Chaos Tests

on: [pull_request]

jobs:
  chaos:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Erlang/OTP 28
        uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run Nine-Nines Test Suite
        run: ./test/run_nine_nines_tests.sh --full
      - name: Upload Report
        uses: actions/upload-artifact@v2
        with:
          name: nine-nines-report
          path: _build/test/logs/nine_nines_report.html
      - name: Check Verdict
        run: |
          if grep -q "✅ ACHIEVED" _build/test/logs/nine_nines_report.html; then
            echo "Nine-nines posture verified"
            exit 0
          else
            echo "Nine-nines SLA violations detected"
            exit 1
          fi
```

### Nightly Stress Test

```bash
# cron: 0 2 * * * /path/to/nightly_chaos.sh

#!/bin/bash
# Run full chaos suite with extended scenarios
export ERLMCP_CHAOS_SESSION_COUNT=50000  # 5x production
export ERLMCP_CHAOS_DATA_RATE=500000    # 5x baseline

./test/run_nine_nines_tests.sh --full

# Email report on failure
if ! grep -q "✅ ACHIEVED" _build/test/logs/nine_nines_report.html; then
  mail -s "Nine-Nines SLA Violation" ops@example.com < _build/test/logs/nine_nines_report.html
fi
```

## Extending the Suite

### Adding a New Chaos Scenario

1. Add scenario function to `ct_chaos_nine_nines_SUITE.erl`:

```erlang
scenario_9_custom_chaos(Config) ->
    ct:pal("Scenario 9: Custom Chaos - Description~n"),
    %% Setup
    {ok, ServerPid} = start_test_server(),

    %% Execute chaos
    Result = execute_custom_chaos(ServerPid),

    %% Verify SLO
    ?assert(Result < SLO_TARGET),

    %% Store metrics
    Metrics = #{scenario => custom_chaos, result => Result, slo_met => Result < SLO_TARGET},
    store_metrics(Config, scenario_9, Metrics),
    ok.
```

2. Add to `all/0` and `groups/0`:

```erlang
groups() ->
    [{nine_nines_chaos_scenarios, [sequence], [
        scenario_1_mailbox_saturation,
        %% ...
        scenario_9_custom_chaos
    ]}].
```

3. Update HTML report generator to format new metrics.

### Adding a New PropEr Property

1. Add property function to `prop_protocol_fsm.erl`:

```erlang
prop_custom_invariant() ->
    ?FORALL(Input, input_generator(),
        begin
            {ok, Pid} = start_test_process(),
            Result = execute_with_input(Pid, Input),
            stop_test_process(Pid),
            verify_invariant(Result)
        end).
```

2. Add to `run_all_properties/0`:

```erlang
run_all_properties() ->
    Properties = [
        prop_client_fsm,
        %% ...
        prop_custom_invariant
    ],
    %% ...
```

## Troubleshooting

### Tests Hang

**Symptom**: Test suite doesn't complete
**Cause**: Deadlock or infinite wait
**Fix**: Add timeouts to all `receive` blocks, `gen_server:call/3` with timeout

### Memory Leak Detected

**Symptom**: Scenario 4 fails with memory >15 MiB/session
**Cause**: Unbounded queue growth, large message accumulation
**Fix**: Enable hibernation, implement queue shedding, use references instead of large payloads

### Flaky Tests

**Symptom**: Tests pass/fail non-deterministically
**Cause**: Race conditions, timing-dependent assertions
**Fix**: Use proper synchronization (monitor/demonitor), avoid `timer:sleep`, use message-based sync

### Erlang Not Found

**Symptom**: `make: *** [check-erlang-version] Error 1`
**Cause**: Erlang/OTP 28+ not installed or not in PATH
**Fix**: Install Erlang/OTP 28+ (see installation instructions in error message)

## References

- **CLAUDE.md**: Project specification and quality gates
- **docs/otp-patterns.md**: OTP supervision and testing patterns
- **docs/architecture.md**: System architecture
- **archive/quality-reports/**: Historical test results
- **PropEr Documentation**: https://proper-testing.github.io/
- **Common Test User Guide**: https://www.erlang.org/doc/apps/common_test/users_guide.html

## Maintenance

### When to Update

1. **New FSM added**: Add PropEr property to `prop_protocol_fsm.erl`
2. **SLO changed**: Update targets in this README and suite assertions
3. **New failure mode discovered**: Add chaos scenario to `ct_chaos_nine_nines_SUITE.erl`
4. **Performance regression**: Update baseline metrics in this README

### Versioning

- **Test Suite Version**: Matches erlmcp version (currently 2.1.0)
- **PropEr Samples**: Increase with codebase maturity (100 → 1000 → 10000)
- **Session Count**: Scale with production capacity (1K → 10K → 100K)

## Conclusion

This test suite provides **comprehensive validation of erlmcp's nine-nines availability posture** through:
- Property-based FSM testing (correctness)
- Chaos engineering scenarios (resilience)
- Real-world load patterns (scalability)
- Strict SLO enforcement (performance)

**Expected Runtime:**
- Quick mode: ~5 minutes
- Full mode: ~30 minutes
- Extended (nightly): ~2 hours

**Nine-Nines Definition**: 99.9999999% uptime = 31.5 milliseconds downtime per year

This test suite validates that erlmcp can achieve this posture through:
- Fast recovery (RTO <5s)
- Graceful degradation (priority messages always processed)
- Resilient supervision (let-it-crash, automatic restart)
- Resource discipline (bounded memory, queue shedding)
