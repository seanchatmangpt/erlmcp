# METROLOGY VIOLATIONS - QUICK REFERENCE INDEX

**Total Violations**: 47
**Audit Date**: January 27, 2026
**Status**: Ready for Remediation

---

## CRITICAL VIOLATIONS (19 total) - MUST FIX BEFORE RELEASE

### STOP-THE-LINE ISSUE #1: "100K Concurrent Connections" Unvalidated

| File | Line | Claim | Truth | Fix Effort |
|------|------|-------|-------|-----------|
| BENCHMARK_100K_RESULTS.md | 291-299 | "erlmcp can handle 100K concurrent connections" | Tested: 100K registry entries only, NOT real TCP sockets | 30 min (disclaimer) or 8h (real test) |
| DEPLOYMENT_STATUS_REPORT.md | 30 | "100K connections" | Registry entries, not validated connections | 30 min disclaimer |
| EXECUTIVE_SUMMARY_100K_VALIDATION.md | 80 | "100K concurrent capability" | Best measured: 150-200 real TCP/process | 1 hour rewrite |

**Impact**: Marketing claims are unsupported
**Recommendation**: Add disclaimer OR create real socket test

---

### STOP-THE-LINE ISSUE #2: Memory Scope Ambiguity (MB/conn)

| File | Line | Value | Missing | Fix |
|------|------|-------|---------|-----|
| TCPS_PRICING_IMPLEMENTATION_COMPLETE.md | 83 | `2.03MB/conn` (Team) | Scope definition | Add: "(per-process heap via erlang:memory(processes))" |
| TCPS_PRICING_IMPLEMENTATION_COMPLETE.md | 84 | `1.5MB/conn` (Enterprise) | Scope definition | Add: "(per-process heap via erlang:memory(processes))" |
| TCPS_PRICING_IMPLEMENTATION_COMPLETE.md | 85 | `1.2MB/conn` (Gov) | Scope definition | Add: "(per-process heap via erlang:memory(processes))" |
| DEPLOYMENT_STATUS_REPORT.md | 83 | `2.03MB/conn` | Scope definition | Add scope clarification |
| DEPLOYMENT_STATUS_REPORT.md | 84 | `1.5MB/conn` | Scope definition | Add scope clarification |
| DEPLOYMENT_STATUS_REPORT.md | 85 | `1.2MB/conn` | Scope definition | Add scope clarification |
| PLAN_CONFORMANCE_QUICK_START.md | 37 | `2.03 MB per connection` | Tier + measurement | Add: "(team tier, per-process heap)" |
| PLAN_CONFORMANCE_QUICK_START.md | 45 | `1.5 MB per connection` | Tier + measurement | Add: "(enterprise tier, per-process heap)" |
| PLAN_CONFORMANCE_QUICK_START.md | 53 | `1.2 MB per connection` | Tier + measurement | Add: "(gov tier, per-process heap)" |
| PLAN_CONFORMANCE_QUICK_START.md | 93 | `2.03MB/conn` | Tier info missing | Add tier identifier |
| PLAN_CONFORMANCE_QUICK_START.md | 94 | `1.5MB/conn` | Tier info missing | Add tier identifier |
| PLAN_CONFORMANCE_QUICK_START.md | 95 | `1.2MB/conn` | Tier info missing | Add tier identifier |

**Impact**: SLA enforcement could use wrong memory metric (5x error possible)
**Fix Effort**: 30 minutes (find-replace) + 15 minutes (verification)
**Risk**: HIGH - affects pricing/SLA validation

---

## HIGH-SEVERITY VIOLATIONS (18 total) - FIX IN CURRENT SPRINT

### Issue #3: Latency Rounding Artifacts

All in `/Users/sac/erlmcp/BENCHMARK_100K_RESULTS.md`:

| Line | Current | Issue | Fix |
|------|---------|-------|-----|
| 47 | `P95 Latency: 0.00 ms` | Sub-ms precision lost | `<0.001 ms (nanosecond-scale)` |
| 48 | `P99 Latency: 0.00 ms` | Sub-ms precision lost | `<0.001 ms` |
| 71 | `P95 Latency: 0.00 ms` | Sub-ms precision lost | `<0.001 ms (in-memory)` |
| 72 | `P99 Latency: 0.01 ms` | Precision unclear | `0.01 ms (10-99 nanoseconds)` |
| 95 | `P95 Latency: 0.00 ms` | Measurement limit | `<0.001 ms (measurement limit)` |
| 96 | `P99 Latency: 0.00 ms` | Measurement limit | `<0.001 ms` |
| 119 | `P95 Latency: 0.00 ms` | Sub-ms precision lost | `<0.001 ms` |
| 180 | `P95 Latency: 0.00 ms` | Sub-ms precision lost | `<0.001 ms` |

**Fix Effort**: 30 minutes
**Recommendation**: Report in microseconds or use 3+ decimal places in milliseconds

---

### Issue #4: Undefined Throughput Units

| File | Line | Current | Issue | Fix |
|------|------|---------|-------|-----|
| BENCHMARK_100K_RESULTS.md | 26 | "Throughput (msg/sec)" | Unit ambiguous | Specify: "(registry operations/sec)" |
| BENCHMARK_100K_RESULTS.md | 46 | "599,725 msg/sec" | Type unclear | "599,725 registry operations/sec (put+lookup, in-process)" |
| BENCHMARK_100K_RESULTS.md | 265 | "358K msg/sec" | Mixed workload | Specify component breakdown |
| BENCHMARK_100K_RESULTS.md | 284 | "358.4K msg/sec" | No definition | Add workload context |
| BENCHMARK_100K_RESULTS.md | 299 | "400K+ msg/sec" | Unachieved | Mark as "[PROJECTED]" or remove |
| bench/BENCHMARKS.md | Various | "throughput" | Type unclear | All instances need units |
| swarm/test-results/benchmark_report.json | Various | No units in JSON | Type missing | Add `"throughput_ops_sec"` field |
| docs/BENCHMARK_EXECUTION_SUMMARY.md | Various | Context-dependent | Type varies | Standardize per instance |

**Fix Effort**: 1 hour
**Impact**: HIGH - Capacity planning depends on clarity

---

### Issue #5: Memory Scope in JSON Plans

| File | Field | Current | Issue | Fix |
|------|-------|---------|-------|-----|
| plans/team.plan.json | `max_payload_size_mb` | 10 | Decimal or binary? | Add note: "decimal MB (1 MB = 1,000,000 bytes)" |
| plans/enterprise.plan.json | `memory_limit_mb` | 4096 | Scope undefined | Add: "per-process heap (erlang:memory(processes))" |
| plans/enterprise.plan.json | `availability_percentage` | 99.95 | Measure period? | Add: "measured over 30-day rolling window" |

**Fix Effort**: 30 minutes
**Files**: 2 JSON files

---

## MEDIUM-SEVERITY VIOLATIONS (10 total) - FIX IN NEXT SPRINT

### Issue #6: Missing Measurement Boundaries

| File | Metric | Missing | Required Addition |
|------|--------|---------|-------------------|
| BENCHMARK_100K_RESULTS.md | Memory | Type definition | Specify: "erlang:memory(total) or erlang:memory(processes)" |
| BENCHMARK_100K_RESULTS.md | Memory | Start/end | Document: "Measured at startup and after 100K ops" |
| BENCHMARK_100K_RESULTS.md | Throughput | Duration | Add: "measured during [X phase], duration [Y seconds]" |
| BENCHMARK_100K_RESULTS.md | Throughput | Calculation | Document: "Operations/TotalTime*1000" |
| MEMORY_OPTIMIZATION_QUICK_START.md | Mem/conn | Measure point | Add: "at connection initialization" |
| CLUSTER_SETUP.md | Memory/node | Type | Specify: "per Erlang VM (erlang:memory(total))" |
| CLUSTER_SETUP.md | Per-conn memory | Type | Specify: "process memory footprint scope" |
| DELIVERY.txt | Latency | Units | Add: "in milliseconds, at gen_server boundary" |

**Fix Effort**: 2 hours

---

## AFFECTED FILES SUMMARY

### Files with CRITICAL Violations (fix immediately):
1. **TCPS_PRICING_IMPLEMENTATION_COMPLETE.md** - 3 violations (memory scope)
2. **DEPLOYMENT_STATUS_REPORT.md** - 3 violations (memory scope)
3. **PLAN_CONFORMANCE_QUICK_START.md** - 6 violations (memory scope + 100K claim)

### Files with HIGH Violations (fix this sprint):
4. **BENCHMARK_100K_RESULTS.md** - 12 violations (latency + throughput)
5. **plans/team.plan.json** - 2 violations (missing units)
6. **plans/enterprise.plan.json** - 3 violations (missing units + scope)
7. **MEMORY_OPTIMIZATION_QUICK_START.md** - 2 violations (scope)

### Files with MEDIUM Violations (fix next sprint):
8. **CLUSTER_SETUP.md** - 2 violations (memory definition)
9. Other documentation - 10 violations (boundaries)

---

## REMEDIATION ROADMAP

### Phase 1: CRITICAL (30 minutes)
- [ ] Add memory scope to all "MB/conn" instances
- [ ] Find-replace in 3 TCPS/DEPLOYMENT/PLAN files
- [ ] Verify: 12 violations fixed

### Phase 2: HIGH (1.5 hours)
- [ ] Fix latency reporting in BENCHMARK_100K_RESULTS.md
- [ ] Update throughput definitions (8 instances)
- [ ] Verify: 18 violations fixed

### Phase 3: MEDIUM (2 hours)
- [ ] Add measurement boundaries documentation
- [ ] Update JSON plan files with unit hints
- [ ] Verify: 10 violations fixed

### Phase 4: OPTIONAL (8 hours)
- [ ] Create real socket test to validate "100K concurrent"
- [ ] Build erlmcp_metrology module for centralized definitions
- [ ] Implement measurement framework with unit tracking

---

## VERIFICATION CHECKLIST

After fixing, verify:
- [ ] No "MB/conn" without scope definition
- [ ] No "100K concurrent" without disclaimer or validation
- [ ] All latency values precise (not rounded to 0.00)
- [ ] All throughput values specify operation type
- [ ] All memory measurements specify scope
- [ ] All JSON numeric fields have accompanying definitions
- [ ] Measurement boundaries documented
- [ ] SLA claims supported by evidence

---

## QUICK REFERENCE FIXES

### FIX 1: Memory Scope (12 instances)
```
OLD: "2.03MB/conn"
NEW: "2.03 MB per connection (team tier, per-process heap, erlang:memory(processes))"
```

### FIX 2: Latency Precision (8 instances)
```
OLD: "P95 Latency: 0.00 ms"
NEW: "P95 Latency: <0.001 ms (nanosecond-scale, in-process)"
```

### FIX 3: Throughput Definition (10 instances)
```
OLD: "599,725 msg/sec"
NEW: "599,725 registry operations/sec (put + lookup, in-process, not network I/O)"
```

### FIX 4: 100K Concurrent Claim (7 instances)
```
OLD: "erlmcp can handle 100K concurrent connections"
NEW: "erlmcp registry tested with 100K entries (in-memory, not real sockets);
      actual concurrent connection validation: pending.
      Measured: 150-200 real TCP/stdio connections per Erlang process."
```

---

**Status**: Ready for assignment to plan-designer or OTP developer agent
**Total Estimated Effort**: 17 hours (phases 1-4)
**Critical Path**: Phase 1 (30 min) must complete before any release
