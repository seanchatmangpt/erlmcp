# Nine-Nines Test Suite - Baseline Metrics & Expected Results

## Overview

This document defines the **baseline performance metrics** and **expected test results** for the Nine-Nines Chaos Test Suite. Use this as a reference to determine if test results indicate regressions.

**Last Updated**: 2026-02-01
**erlmcp Version**: 2.1.0
**Test Suite Version**: 1.0.0

## Baseline Performance (Pre-Chaos)

These metrics represent normal system performance **before** chaos injection:

| Component | Operation | Throughput | Latency (p99) | Source |
|-----------|-----------|------------|---------------|--------|
| Registry | Lookup | 553K msg/s | <10 µs | erlmcp_bench_core_ops |
| Queue | Enqueue/Dequeue | 971K msg/s | <5 µs | erlmcp_bench_core_ops |
| Pool | Checkout/Checkin | 149K msg/s | <50 µs | erlmcp_bench_core_ops |
| Session | Create/Destroy | 242K msg/s | <20 µs | erlmcp_bench_core_ops |
| Network I/O | TCP send/recv | 43K msg/s | <500 µs | erlmcp_bench_network_real |
| Sustained | Mixed workload | 372K msg/s | <1 ms | erlmcp_bench_stress |

**Memory Characteristics:**
- Idle process: ~2.7 KiB heap
- Active session: ~5-10 MiB heap (depends on workload)
- Total RSS: ~50-100 MiB for 1K sessions

**GC Characteristics:**
- Minor GC: ~50-200 µs
- Major GC: ~1-5 ms
- At 40K connections: <100ms p99 pause

## Expected Test Results

### Part 1: PropEr FSM Tests

**Configuration:**
- Quick mode: 100 samples per property
- Full mode: 1000 samples per property

**Expected Results (all properties should pass):**

| Property | Description | Expected Result | Failure Indicator |
|----------|-------------|-----------------|-------------------|
| `prop_client_fsm` | Client state transitions valid | ✅ PASS | Invalid phase transition detected |
| `prop_server_fsm` | Server invariants maintained | ✅ PASS | Resource corruption, crash |
| `prop_cancellation_races` | Cancel vs result races | ✅ PASS | Duplicate responses, crashes |
| `prop_priority_message_handling` | Priority msgs under load | ✅ PASS | Priority msg blocked >50ms |
| `prop_deadlock_freedom` | No deadlocks | ✅ PASS | Timeout, hung processes |
| `prop_session_lifecycle` | Session consistency | ✅ PASS | State corruption |
| `prop_state_transition_invariants` | No undefined states | ✅ PASS | Unknown phase value |

**Typical Runtime:**
- Quick mode: ~2-3 minutes
- Full mode: ~10-15 minutes

### Part 2: CT Chaos Nine-Nines Suite

**Configuration:**
- Quick mode: 1K sessions, 10K messages
- Full mode: 10K sessions, 100K messages

**Scenario 1: Mailbox Saturation**

| Metric | SLO | Expected (Quick) | Expected (Full) | Violation Example |
|--------|-----|------------------|-----------------|-------------------|
| Baseline latency | - | 2-5 ms | 2-5 ms | >10 ms (slowdown) |
| P50 latency under load | - | 30-50 ms | 40-60 ms | >100 ms |
| P95 latency under load | - | 60-80 ms | 70-90 ms | >150 ms |
| P99 latency under load | <100ms | 70-90 ms | 80-95 ms | ≥100 ms ❌ |
| Server survived | Yes | ✅ | ✅ | Process crashed ❌ |
| Total messages processed | - | ~500K | ~5M | <90% of expected |

**Scenario 2: Cascading Failure**

| Metric | SLO | Expected | Violation Example |
|--------|-----|----------|-------------------|
| RTO (Recovery Time) | <5000ms | 100-500 ms | ≥5000 ms ❌ |
| Server recovered | Yes | ✅ | Process not restarted ❌ |

**Scenario 3: Network Partition**

| Metric | SLO | Expected | Violation Example |
|--------|-----|----------|-------------------|
| Detection time | <30000ms | 1000-5000 ms | ≥30000 ms ❌ |
| Server survived | Yes | ✅ | Crash on timeout ❌ |

**Scenario 4: Resource Exhaustion**

| Metric | SLO | Expected (1K) | Expected (10K) | Violation Example |
|--------|-----|---------------|----------------|-------------------|
| Session creation time | - | 100-500 ms | 1000-3000 ms | >5000 ms |
| Sessions/sec created | - | 2K-10K | 3K-10K | <1K |
| Memory per session | <15 MiB | 5-10 MiB | 5-12 MiB | ≥15 MiB ❌ |
| GC count during test | - | 5-20 | 50-200 | >1000 (thrashing) |
| All sessions alive | Yes | ✅ | ✅ | Crashes ❌ |

**Scenario 5: Priority Message Handling**

| Metric | SLO | Expected | Violation Example |
|--------|-----|----------|-------------------|
| P50 priority latency | - | 10-30 ms | >100 ms |
| P95 priority latency | - | 30-45 ms | >150 ms |
| P99 priority latency | <50ms | 35-48 ms | ≥50 ms ❌ |
| Data messages sent | - | 100K | <10K (starvation) |
| Control messages sent | - | 1K | <100 (loss) |

**Scenario 6: Task Execution Under Load**

| Metric | SLO | Expected (1K tasks) | Expected (10K tasks) | Violation Example |
|--------|-----|---------------------|----------------------|-------------------|
| Completion time | <30000ms | 1000-5000 ms | 5000-25000 ms | ≥30000 ms ❌ |
| Tasks/sec | - | 200-1000 | 400-2000 | <100 |

**Scenario 7: State Machine Consistency**

| Metric | SLO | Expected | Violation Example |
|--------|-----|----------|-------------------|
| Request count | - | 10K | N/A |
| Concurrent processes | - | 100 | N/A |
| Server survived | Yes | ✅ | Crash ❌ |
| Valid phase | Yes | ✅ (initialized) | unknown, undefined ❌ |

**Scenario 8: Recovery Discipline**

| Metric | SLO | Expected | Violation Example |
|--------|-----|----------|-------------------|
| Recovery OK | Yes | ✅ | Invariants not restored ❌ |

**Typical Runtime:**
- Quick mode: ~2-5 minutes
- Full mode: ~15-30 minutes

## SLO Compliance Matrix

| Scenario | Metric | SLO | Pass Criteria | Fail Criteria |
|----------|--------|-----|---------------|---------------|
| 1 | Control P99 latency | <100ms | ≤99 ms | ≥100 ms |
| 2 | RTO | <5000ms | ≤4999 ms | ≥5000 ms |
| 3 | Detection time | <30000ms | ≤29999 ms | ≥30000 ms |
| 4 | Memory/session | <15 MiB | ≤14.99 MiB | ≥15 MiB |
| 5 | Priority P99 latency | <50ms | ≤49 ms | ≥50 ms |
| 6 | Task completion | <30000ms | ≤29999 ms | ≥30000 ms |
| 7 | Valid phase | Yes | initialized, closed | unknown, error |
| 8 | Recovery OK | Yes | true | false |

**Overall Verdict:**
- ✅ **ACHIEVED**: All 8 scenarios pass SLOs
- ⚠️ **SLA VIOLATIONS**: Any scenario fails SLO

## Regression Thresholds

**Alert on:**
1. **>10% throughput degradation** from baseline (e.g., <500K msg/s registry)
2. **>2x latency increase** from baseline (e.g., p99 >20 µs for registry)
3. **Memory growth >20%** compared to previous run (e.g., >12 MiB/session when baseline is 10 MiB)
4. **GC pause increase >50%** (e.g., >150ms when baseline is 100ms)
5. **New crashes** in scenarios that previously passed
6. **Timeout violations** that didn't exist before

**Investigate if:**
- PropEr test finds new counterexample
- Chaos scenario that passed now fails
- Metrics file missing or empty
- HTML report shows ⚠️ SLA VIOLATIONS

## Historical Trends

To track regressions over time, maintain a log of test runs:

```
Date       | Version | Overall | Scenario Failures | Notes
-----------|---------|---------|-------------------|------------------------
2026-02-01 | 2.1.0   | ✅ PASS | None              | Baseline established
2026-02-15 | 2.1.1   | ✅ PASS | None              | Performance optimization
2026-03-01 | 2.2.0   | ⚠️ FAIL | Scenario 4        | Memory leak in session
2026-03-05 | 2.2.1   | ✅ PASS | None              | Memory leak fixed
```

## Metrics File Format

Each scenario produces a JSON metrics file:

**Example: `scenario_1.json`**
```json
{
  "scenario": "mailbox_saturation",
  "baseline_latency_ms": 3,
  "p50_latency_ms": 42,
  "p95_latency_ms": 76,
  "p99_latency_ms": 89,
  "total_messages": 500000,
  "server_survived": true,
  "slo_met": true
}
```

**Example: `scenario_4.json`**
```json
{
  "scenario": "resource_exhaustion",
  "session_count": 10000,
  "creation_time_ms": 2345,
  "memory_per_session_mib": 11.23,
  "gc_count": 127,
  "all_alive": true,
  "slo_met": true
}
```

## Environment Considerations

**Factors that affect results:**

1. **Hardware**:
   - CPU cores: More cores = higher parallelism
   - RAM: More RAM = can sustain more sessions
   - Disk I/O: Affects session persistence (DETS, Mnesia)

2. **OS Settings**:
   - `ulimit -n`: File descriptor limit (affects max connections)
   - TCP buffer sizes: Affects network throughput
   - Scheduler affinity: Affects CPU distribution

3. **Erlang VM Settings** (`vm.args`):
   - `+P`: Max processes (default 262144)
   - `+Q`: Max ports (default 65536)
   - `+A`: Async threads (default 10)
   - `+sbt`: Scheduler bind type

4. **Load**:
   - Background processes affect CPU availability
   - Network latency affects TCP scenarios
   - Disk usage affects session backend performance

**Recommended test environment:**
- Dedicated machine (no other workloads)
- ≥8 CPU cores
- ≥16 GB RAM
- SSD storage
- Low-latency network (<10ms RTT for distributed tests)

## Interpreting Failures

### Scenario 1 Failure (Mailbox Saturation)

**Symptom**: P99 latency ≥100ms

**Possible Causes:**
1. No selective receive for priority messages
2. Mailbox queue growing unbounded
3. Message processing too slow

**Debug:**
```erlang
%% Check mailbox size during test
{message_queue_len, Len} = process_info(ServerPid, message_queue_len).

%% Profile where time is spent
eprof:start(),
eprof:start_profiling([ServerPid]),
%% ... run test ...
eprof:stop_profiling(),
eprof:analyze().
```

**Fix:** Implement selective receive, process priority messages first

### Scenario 4 Failure (Resource Exhaustion)

**Symptom**: Memory per session ≥15 MiB

**Possible Causes:**
1. Session state accumulating unbounded data
2. Message queues not being drained
3. No hibernation for idle sessions

**Debug:**
```erlang
%% Check process heap size
{heap_size, Size} = process_info(SessionPid, heap_size).

%% Check message queue
{message_queue_len, Len} = process_info(SessionPid, message_queue_len).

%% Check binaries (often cause of leaks)
{binary, Bins} = process_info(SessionPid, binary).
```

**Fix:** Enable hibernation, implement queue shedding, use references for large data

### PropEr Failure (Shrinking Counterexample)

**Symptom**: PropEr finds counterexample and shrinks it

**Example Output:**
```
Failed: After 234 test(s).
[{call,erlmcp_client,initialize,[<0.123.0>,#{}]},
 {call,erlmcp_client,list_resources,[<0.123.0>]},
 {call,prop_protocol_fsm,stop_client,[<0.123.0>]},
 {call,erlmcp_client,list_resources,[<0.123.0>]}]  %% Use after free!

Shrinking ....(3 time(s))
[{call,erlmcp_client,initialize,[<0.123.0>,#{}]},
 {call,prop_protocol_fsm,stop_client,[<0.123.0>]},
 {call,erlmcp_client,list_resources,[<0.123.0>]}]  %% Minimal failing case
```

**Fix:** Add precondition to prevent calling `list_resources` after `stop_client`:

```erlang
precondition(#model_state{client_pid = undefined}, {call, erlmcp_client, list_resources, _}) ->
    false;  %% Can't call if client stopped
```

## Continuous Monitoring

**Nightly Job:**
```bash
#!/bin/bash
# Run full chaos suite and alert on failures

./test/run_nine_nines_tests.sh --full > /tmp/chaos_results.txt 2>&1

if ! grep -q "✅ ACHIEVED" _build/test/logs/nine_nines_report.html; then
  # Extract failures
  FAILURES=$(grep "❌ FAIL" _build/test/logs/nine_nines_report.html | wc -l)

  # Alert ops team
  echo "ALERT: Nine-Nines SLA Violations Detected ($FAILURES scenarios failed)" | \
    mail -s "erlmcp Chaos Test FAILURE" ops@example.com

  # Attach full report
  cat /tmp/chaos_results.txt | mail -s "Full Chaos Test Results" ops@example.com
fi
```

## Conclusion

These baseline metrics and expected results provide a **comprehensive reference** for validating erlmcp's nine-nines availability posture. Any deviations should be investigated as potential regressions.

**Next Steps:**
1. Run initial baseline: `./test/run_nine_nines_tests.sh --full`
2. Record results in historical trends table
3. Set up nightly monitoring
4. Alert on SLO violations
5. Investigate and fix regressions before merge
