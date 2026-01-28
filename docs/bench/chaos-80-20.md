# Chaos Testing 80/20 Analysis - erlmcp

**Version:** v1.0.0
**Date:** 2026-01-27
**Methodology:** Pareto Principle applied to chaos engineering (defect class coverage)
**Goal:** Define minimum chaos scenario set that captures 80% of defect classes with 20% of scenarios

---

## Executive Summary

erlmcp chaos testing has **11 total scenarios** covering diverse failure modes. This document identifies the **7-scenario minimum set** that captures **100% of critical defect classes (6/6)** and represents the **80/20 optimization** for fast CI/CD validation.

- **Total scenarios available:** 11
- **80/20 minimum set:** 7 scenarios
- **Defect classes covered:** 6 critical classes (100%)
- **Execution time (quick mode):** ~5 minutes
- **Full suite execution time:** ~20 minutes

---

## Critical Defect Classes

Every production failure falls into one of these **6 defect classes**:

### Class 1: Retry Amplification & Message Floods
**Defect:** Legitimate traffic spike or misconfiguration causes exponential message flood, saturating queues.
**Impact:** Service degradation, memory exhaustion, cascading failures.
**Mitigation:** Rate limiting, bounded queues, backpressure mechanisms.
**80/20 Scenario:** `message_flood` (10x capacity flood detection and refusal)

---

### Class 2: Bounded Queue Overflow & Backpressure
**Defect:** Slow consumer (network latency, processing delay) causes queue buildup and timeouts.
**Impact:** Request timeouts, connection resets, partial data loss.
**Mitigation:** Timeout enforcement, queue drains, connection backpressure.
**80/20 Scenario:** `slow_consumer` (10sec delay, triggers timeout refusal)

---

### Class 3: Resource Exhaustion (Memory, Disk, CPU)
**Defect:** Unbounded growth of heap, message buffers, or file handles.
**Impact:** OOM killer, system slowdown, cascading process deaths.
**Mitigation:** Memory limits, bounded caches, disk space monitoring.
**80/20 Scenario:** `memory_exhaustion` (90%+ memory trigger, refusal before crash)

---

### Class 4: Process Crash & Supervision Recovery
**Defect:** Worker process crash, supervisor misconfiguration, or restart storm.
**Impact:** Request loss, connection drops, service outage.
**Mitigation:** Proper supervision strategy (one_for_one vs restart_intensity), monitoring.
**80/20 Scenarios:**
- `process_crash` (worker process kill → supervisor recovery)
- `supervisor_cascade` (multi-level supervisor failure → app restart)

---

### Class 5: Invalid Input & Protocol Errors
**Defect:** Malformed JSON, oversized payloads, or protocol violations.
**Impact:** Service crashes, security vulnerabilities, data corruption.
**Mitigation:** Input validation, schema enforcement, size limits.
**80/20 Scenario:** `invalid_payload` (malformed JSON → protocol error refusal)

---

### Class 6: Network Degradation & Partition Failures
**Defect:** Network splits, high latency, packet loss, or DNS failures.
**Impact:** Split-brain, consistency violations, partial service loss.
**Mitigation:** Timeout enforcement, graceful degradation, health checks.
**80/20 Scenario:** `network_partition` (simulated node split → graceful handling)

---

## 80/20 Scenario Set (7 scenarios, ~5 min execution)

| # | Scenario ID | Defect Class | Expected Behavior | Refusal Code | SLA (Recovery) |
|---|---|---|---|---|---|
| 1 | `message_flood` | Class 1 (Retry Amplification) | Rate limit refusal | 1056 (RATE_LIMIT_EXCEEDED) | <1s detection |
| 2 | `slow_consumer` | Class 2 (Queue Overflow) | Timeout refusal | 1057 (TIMEOUT) | <5s recovery |
| 3 | `memory_exhaustion` | Class 3 (Resource Exhaustion) | Resource exhaustion refusal | 1089 (RESOURCE_EXHAUSTED) | <5s refusal |
| 4 | `process_crash` | Class 4a (Worker Crash) | Supervisor recovery | - | <1s recovery |
| 5 | `supervisor_cascade` | Class 4b (Supervision Failure) | App restart | - | <5s restart |
| 6 | `invalid_payload` | Class 5 (Protocol Error) | Protocol error refusal | 1066 (PROTOCOL_ERROR) | <100ms refusal |
| 7 | `network_partition` | Class 6 (Network Degradation) | Graceful degradation | - | <500ms detection |

---

## Excluded Scenarios (Not 80/20)

These scenarios are **redundant** with the core 7 set and add minimal new defect insight:

| Scenario | Why Excluded | Covered By | Impact |
|---|---|---|---|
| `connection_leak` | Subset of Class 3 (resource exhaustion), but more specific to connection pool limits. Captured by `memory_exhaustion` + `message_flood`. | `memory_exhaustion` + load tests | High overlap, low new insight |
| `disk_full` | Specific failure (disk I/O), rarely impacts erlmcp core (stateless). Covered by operational monitoring. | Operational SLA | Low priority for core testing |
| `cpu_saturation` | Scheduler backpressure is covered by `slow_consumer` (same outcome: timeout). CPU-specific tuning is performance, not defect. | `slow_consumer` | Same outcome, different root cause |
| `large_payload` | Subset of Class 5 (input validation), caught by same mechanisms as `invalid_payload`. | `invalid_payload` | Covered by input validation layer |

---

## Mapping to Real Production Incidents

### Example 1: Message Flood (AWS Auto-Scaling Bug)
**Real Incident:** Misconfigured auto-scale launch caused 10x traffic spike.
**Detected By:** `message_flood` (rate limit refusal blocks amplification)
**Prevented:** Cascading failures to downstream services

### Example 2: Slow Consumer (Database Connection Pooling)
**Real Incident:** Database became slow (100ms → 5sec per query), client timeouts.
**Detected By:** `slow_consumer` (timeout refusal after queue fills)
**Prevented:** Connection pool exhaustion, cascading timeouts

### Example 3: Memory Exhaustion (Cache Memory Leak)
**Real Incident:** Cache entry TTL misconfiguration → unbounded heap growth.
**Detected By:** `memory_exhaustion` (bounded refusal before OOM)
**Prevented:** OOM kill, service crash

### Example 4: Worker Crash (Bad Deploy)
**Real Incident:** New code has null pointer in hot path, workers crash on first request.
**Detected By:** `process_crash` (supervisor recovery validates restart mechanism)
**Prevented:** Cascading failures, fast MTTF via automated restart

### Example 5: Protocol Error (Client Bug)
**Real Incident:** Outdated client sends invalid JSON structure.
**Detected By:** `invalid_payload` (protocol error refusal, connection reset)
**Prevented:** Server-side crash, resource leak

### Example 6: Network Partition (Datacenter Outage)
**Real Incident:** Datacenter link failure, temporary split-brain.
**Detected By:** `network_partition` (graceful degradation, no consistency violations)
**Prevented:** Data corruption, inconsistent state

---

## CI/CD Integration

### Quick Mode (Default, ~5 minutes)
```bash
make benchmark-quick
# or
./scripts/bench/run_all_benchmarks.sh quick

# Runs: message_flood, slow_consumer, memory_exhaustion,
#       process_crash, supervisor_cascade, invalid_payload,
#       network_partition
```

**Used for:** Pull request validation, pre-commit checks
**Gate:** Must pass all 7 scenarios (0 failures)

### Full Mode (Comprehensive, ~20 minutes)
```bash
./scripts/bench/run_all_benchmarks.sh full

# Runs: All 11 scenarios (includes connection_leak, disk_full,
#       cpu_saturation, large_payload redundancy)
```

**Used for:** Release validation, nightly CI, final verification before shipping
**Gate:** Must pass all 11 scenarios (0 failures)

---

## Quality Gate: Bounded Refusal Validation

For each chaos scenario, **bounded refusal** verification ensures:

1. **Correct Refusal Code:** Matches expected errno (if applicable)
2. **Fast Detection:** <1 second latency for fault detection
3. **Automatic Recovery:** <5 seconds to restore functionality
4. **No Data Loss:** All buffered messages preserved or properly failed
5. **No Cascading Failures:** Single fault doesn't propagate upstream

**Validation Criteria (from erlmcp_bench_chaos.erl):**
```erlang
validate_bounded_refusal(Result, Scenario) ->
    CodeMatches = (ExpectedCode =:= ActualCode),
    FastDetection = DetectionTime < 1000.0,      % < 1 second
    AutoRecovery = RecoveryTime < 5000.0,        % < 5 seconds
    NoDataLoss = not DataLoss,
    NoCascade = CascadingFailures =:= 0,

    CodeMatches andalso FastDetection andalso
    AutoRecovery andalso NoDataLoss andalso NoCascade.
```

---

## Execution Results Format

Each scenario produces:
```json
{
  "workload_id": "chaos_message_flood",
  "benchmark": "chaos",
  "scenario": "message_flood",
  "injection_time_s": 0.1,
  "detection_time_ms": 45.2,
  "refusal_code": 1056,
  "refusal_message": "Rate limit exceeded",
  "recovery_time_ms": 150.5,
  "data_loss": false,
  "cascading_failures": 0,
  "bounded_refusal_validated": true,
  "test_passed": true,
  "scope": "per_node",
  "timestamp": 1674345600
}
```

**Pass Criteria:**
- `test_passed: true`
- `bounded_refusal_validated: true`
- `cascading_failures: 0`
- `data_loss: false`

---

## Scaling from 80/20 to 100%

When adding new failure modes:

1. **Identify defect class:** Map to one of 6 critical classes
2. **Check 80/20 set:** Is it covered? If yes, skip.
3. **New class?** Add one scenario per new class
4. **Redundant?** Exclude and update this matrix

Example: If you add **"database connection timeout"** scenario:
- Maps to: Class 2 (Queue Overflow) → Already covered by `slow_consumer`
- Decision: Skip (no new insight)

---

## References

- **erlmcp_bench_chaos.erl** - Full 11-scenario implementation
- **scripts/bench/run_all_benchmarks.sh** - Benchmark runner (quick/full modes)
- **docs/otp-patterns.md** - Supervision and recovery patterns
- **Pareto Principle:** 80% of problems come from 20% of causes

---

**Last Updated:** 2026-01-27
**Next Review:** Post-v0.6.0 release (Feb 2026)
