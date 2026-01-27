# TCPS Operations Runbook

**Version:** 1.0.0
**Last Updated:** 2026-01-26
**Applies To:** TCPS v1.0.0+

---

## Table of Contents

1. [System Overview](#1-system-overview)
2. [Daily Operations](#2-daily-operations)
3. [Monitoring & Alerting](#3-monitoring--alerting)
4. [Troubleshooting](#4-troubleshooting)
5. [Maintenance Procedures](#5-maintenance-procedures)
6. [Disaster Recovery](#6-disaster-recovery)
7. [Performance Tuning](#7-performance-tuning)
8. [Security Operations](#8-security-operations)
9. [Appendices](#9-appendices)

---

## 1. System Overview

### 1.1 What is TCPS?

The **Toyota Code Production System (TCPS)** is a production-grade quality control and workflow management system for software engineering, implementing Toyota Production System (TPS) principles:

- **Andon** - Stop-the-line quality control
- **Kanban** - Work-in-progress (WIP) limit management
- **Kaizen** - Continuous improvement automation
- **5 Whys** - Root cause analysis framework
- **Heijunka** - Production leveling and scheduling
- **Receipts** - Immutable audit trails with RDF ontology

### 1.2 Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      TCPS Pipeline                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Pull Signal → Work Order → Kanban → Quality Gates →       │
│  Andon (if failure) → 5 Whys → Resolution → Receipt        │
│                                                             │
└─────────────────────────────────────────────────────────────┘

Key Components:
┌────────────────────┬────────────────────────────────────────┐
│ Component          │ Description                            │
├────────────────────┼────────────────────────────────────────┤
│ tcps_work_order    │ Work order lifecycle management        │
│ tcps_andon         │ Stop-the-line events and blocking      │
│ tcps_kanban        │ WIP limits and pull signals            │
│ tcps_root_cause    │ 5 Whys analysis framework              │
│ tcps_kaizen        │ Continuous improvement metrics         │
│ tcps_dashboard     │ Real-time monitoring UI                │
│ tcps_receipt_*     │ Immutable audit trails (JSON + RDF)    │
│ tcps_health        │ Health checks and diagnostics          │
└────────────────────┴────────────────────────────────────────┘
```

### 1.3 System Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| Erlang/OTP | 25+ | Runtime platform |
| rebar3 | 3.18+ | Build tool |
| gproc | 0.9.0+ | Process registry |
| poolboy | 1.5.2+ | Connection pooling |
| gun | 2.0.1+ | HTTP client |
| ranch | 2.1.0+ | TCP listener framework |
| cowboy | 2.10.0+ | HTTP server (dashboard) |
| jsx | 3.1.0+ | JSON encoding/decoding |

### 1.4 Infrastructure Requirements

**Production Environment:**
```
CPU:     4+ cores (recommended: 8)
Memory:  8GB minimum (recommended: 16GB)
Disk:    100GB+ (receipts grow ~10KB per work order)
Network: 1Gbps LAN, <10ms latency
```

**Ports:**
```
8080 - Dashboard HTTP
4369 - EPMD (Erlang Port Mapper Daemon)
9100 - Prometheus metrics exporter (optional)
```

---

## 2. Daily Operations

### 2.1 Morning Checklist

**Every morning before starting work:**

```bash
# 1. Check system health
./tools/tcps tpm health

# Expected output:
# ✓ TCPS System Health: OK
# ✓ ETS tables: 5/5 initialized
# ✓ Gen_server processes: 6/6 running
# ✓ Disk space: 45GB available (45% free)
# ✓ Receipt storage: accessible
```

```bash
# 2. Check dashboard
open http://localhost:8080
# Or: curl -s http://localhost:8080/api/health

# Verify:
# - All panels loading
# - No stale data (timestamp < 5 seconds old)
# - SSE connection active
```

```bash
# 3. Review overnight Andon events
./tools/tcps andon list --status=open

# If ANY open Andons:
# - Review details: ./tools/tcps andon show <andon-id>
# - Check if already being investigated
# - Escalate if >8 hours old
```

```bash
# 4. Check quality gate pass rate (target >95%)
./tools/tcps quality metrics --period=daily

# Expected:
# Quality Gate Pass Rate: 96.2% (target: >95%) ✓
# Test Pass Rate: 94.1% (target: >80%) ✓
# Code Coverage: 87.3% (target: >80%) ✓
# Defect Rate: 0.004 (target: <0.01) ✓
```

```bash
# 5. Review Kanban WIP status
./tools/tcps kanban status

# Expected output:
# Bucket       Current  Limit  Status
# ─────────────────────────────────────
# security         3      5    OK
# reliability      4      5    OK
# cost             2      5    OK
# compliance       1      5    OK
# features         7     10    OK
# technical_debt   3      5    OK
```

```bash
# 6. Check lead time metrics (target <2h avg)
./tools/tcps kaizen report --daily

# Expected:
# Lead Time (P50): 1.2h ✓
# Lead Time (P90): 3.4h (target: <4h) ✓
# Lead Time (P99): 6.1h
# Throughput: 127 work orders/day
```

**Action Items Based on Morning Check:**

| Condition | Action |
|-----------|--------|
| Open Andons >5 | Emergency response - page on-call |
| Pass rate <90% | Review recent failures, check for patterns |
| WIP at limit | Check for stuck work orders |
| Lead time >4h (P90) | Investigate bottlenecks |

### 2.2 Processing Work Orders

#### 2.2.1 Sync New Work Orders from GitHub

```bash
# Pull latest GitHub issues as work orders
./tools/tcps work-order sync-github

# Output:
# Syncing GitHub issues from banyan-platform/erlmcp...
# ✓ Created work order WO-123456 from issue #456
# ✓ Created work order WO-123457 from issue #457
# ✓ 2 new work orders, 0 updated
```

**Manual Work Order Creation:**

```bash
# Create security work order (priority 10)
./tools/tcps work-order create \
  --bucket=security \
  --priority=10 \
  --title="Fix CVE-2026-1234" \
  --description="Critical security vulnerability in auth module"

# Create reliability work order
./tools/tcps work-order create \
  --bucket=reliability \
  --priority=7 \
  --title="Fix flaky test in tcps_kanban_tests"
```

#### 2.2.2 View Work Order Queue

```bash
# List pending work orders (sorted by priority)
./tools/tcps work-order list --status=pending

# Output:
# ID             Bucket      Priority  Title                        Age
# ─────────────────────────────────────────────────────────────────────
# WO-123456      security    10        Fix CVE-2026-1234            2h
# WO-123457      reliability 9         Production crash in handler  4h
# WO-123458      reliability 7         Flaky test fix               1d
# WO-123459      features    5         Add export feature           3d
```

#### 2.2.3 Process Next Work Order (Heijunka Scheduling)

TCPS uses **Heijunka leveling** to automatically select the next work order based on:
- Priority (highest first)
- Bucket distribution (prevent batching)
- WIP limits (respect Kanban constraints)
- Dependencies (check if blocked)

```bash
# Dequeue and start next work order automatically
./tools/tcps work-order process-next

# Output:
# Heijunka leveling selected: WO-123456 (security, priority 10)
# ✓ Checked WIP limit: security (3/5) - available
# ✓ Checked dependencies: none
# ✓ Started work order WO-123456
# → Receipt: receipt-start-1674755400-123456.json
```

#### 2.2.4 Monitor Work Order Progress

```bash
# View detailed work order status
./tools/tcps work-order show WO-123456

# Output:
# Work Order: WO-123456
# ═══════════════════════════════════════════════════
# Bucket:          security
# Priority:        10
# Status:          in_progress
# Created:         2026-01-26 09:15:00 (2h ago)
# Started:         2026-01-26 09:20:00 (1h55m ago)
# SLA Deadline:    2026-01-27 09:15:00 (22h remaining)
#
# Current Stage:   implementation
# Stages Completed:
#   ✓ requirements (15m)
#   ✓ design (30m)
#   → implementation (in progress - 1h10m)
#
# Dependencies:    None
# Blocking:        WO-123460, WO-123461
#
# Receipts:        3 generated
#   - receipt-creation-xxx.json
#   - receipt-start-xxx.json
#   - receipt-stage-xxx.json
```

### 2.3 Handling Andon Events

**Andon = Stop-the-Line Event**

When quality issues are detected, TCPS automatically triggers an Andon event that **blocks** the affected SKU from progressing until resolved.

#### 2.3.1 Andon Alert Notification

You'll be notified via:
- Dashboard alert (red banner)
- Email (if configured)
- Slack/Teams webhook (if configured)
- CLI output (if monitoring logs)

#### 2.3.2 View Open Andons

```bash
# List all open (unresolved) Andons
./tools/tcps andon list --status=open

# Output:
# ID              Type              SKU          Stage       Age    Status
# ─────────────────────────────────────────────────────────────────────────
# ANDON-001       test_failure      sku_123      testing     2h     OPEN
# ANDON-002       shacl_violation   sku_456      validation  45m    OPEN
# ANDON-003       non_determinism   sku_789      execution   15m    OPEN
```

#### 2.3.3 Investigate Andon Details

```bash
# View full Andon details
./tools/tcps andon show ANDON-001

# Output:
# Andon Event: ANDON-001
# ═══════════════════════════════════════════════════
# Type:            test_failure
# SKU:             sku_123
# Stage:           testing
# Triggered:       2026-01-26 09:15:00 (2h ago)
# Status:          OPEN
#
# Failure Details:
#   Test Module:   tcps_kanban_tests
#   Test Function: heijunka_leveling_test
#   Expected:      [{security, 1}, {reliability, 1}, {cost, 1}]
#   Actual:        [{security, 3}, {reliability, 0}, {cost, 0}]
#   Error:         Batching detected - Heijunka leveling not applied
#
# Impact:
#   ⚠ SKU sku_123 is BLOCKED from progressing
#   ⚠ All dependent SKUs also blocked
#
# Receipts:
#   - receipt-andon-001.json (created: 2026-01-26 09:15:00)
```

#### 2.3.4 Perform 5 Whys Root Cause Analysis

TCPS provides a structured **5 Whys framework** to identify root causes:

```bash
# Start 5 Whys analysis for Andon
./tools/tcps root-cause start ANDON-001

# Output:
# Started 5 Whys analysis: analysis_001
# Problem: Test failure - Heijunka leveling not applied
#
# Enter Why #1 (or 'skip' to add later):
```

**Interactive 5 Whys Session:**

```bash
# Why #1: Why did the test fail?
./tools/tcps root-cause add-why analysis_001 1 \
  "Heijunka leveling algorithm selected 3 security work orders consecutively"

# Why #2: Why did it select 3 consecutively?
./tools/tcps root-cause add-why analysis_001 2 \
  "Bucket distribution counter was not being updated after each selection"

# Why #3: Why wasn't the counter updated?
./tools/tcps root-cause add-why analysis_001 3 \
  "State update in heijunka_select_next/2 was missing bucket counter increment"

# Why #4: Why was the state update missing?
./tools/tcps root-cause add-why analysis_001 4 \
  "Code review didn't catch the missing state update in refactoring"

# Why #5: Why didn't code review catch it?
./tools/tcps root-cause add-why analysis_001 5 \
  "No test coverage for bucket distribution enforcement in leveling algorithm"

# Finalize analysis with root cause and prevention
./tools/tcps root-cause finalize analysis_001 \
  --root-cause="Missing test coverage for Heijunka bucket distribution" \
  --prevention="Add property-based test for bucket distribution + enforce 100% coverage"

# Output:
# ✓ Analysis finalized
# Root Cause:  Missing test coverage for Heijunka bucket distribution
# Prevention:  Add property-based test for bucket distribution + enforce 100% coverage
#
# Generated prevention actions:
#   1. Add test: test/property/tcps_heijunka_distribution_prop.erl
#   2. Update CI: require 100% coverage for tcps_kanban.erl
#   3. Update review checklist: verify Heijunka invariants
#
# → Receipt: receipt-root-cause-analysis_001.json
```

#### 2.3.5 Resolve Andon

Once the issue is fixed:

```bash
# Resolve Andon (resume pipeline)
./tools/tcps andon resolve ANDON-001 \
  --receipt-path=./priv/receipts/receipt-root-cause-analysis_001.json

# Output:
# ✓ Andon ANDON-001 resolved
# ✓ Root cause analysis linked
# ✓ Prevention actions recorded
# ✓ SKU sku_123 unblocked - can proceed
# → Receipt: receipt-andon-001-resolution.json
```

### 2.4 Receipt Verification

TCPS generates **immutable receipts** for all operations, stored as:
- JSON files (`priv/receipts/`)
- RDF triples (ontology linked)

#### 2.4.1 Verify Receipt Chain

```bash
# Verify complete receipt chain for SKU
./tools/tcps receipt verify-chain sku_123

# Output:
# Receipt Chain Verification: sku_123
# ═══════════════════════════════════════════════════
#
# Chain: work_order → compilation → testing → validation → execution → published
#
# ✓ receipt-creation-wo-123.json
#   Type: work_order_created
#   Timestamp: 2026-01-26T08:00:00Z
#   Hash: sha256:abc123...
#
# ✓ receipt-compilation-sku_123.json
#   Type: stage_completed
#   Stage: compilation
#   Timestamp: 2026-01-26T08:15:00Z
#   Previous: sha256:abc123...
#   Hash: sha256:def456...
#
# ✓ receipt-testing-sku_123.json
#   Type: stage_completed
#   Stage: testing
#   Timestamp: 2026-01-26T08:30:00Z
#   Previous: sha256:def456...
#   Hash: sha256:ghi789...
#
# ✓ All receipts valid
# ✓ Chain integrity verified
# ✓ No gaps or tampering detected
```

#### 2.4.2 Verify Deterministic Build

```bash
# Verify build is deterministic (reproducible)
./tools/tcps receipt verify-deterministic sku_123

# Output:
# Deterministic Build Verification: sku_123
# ═══════════════════════════════════════════════════
#
# Build 1: sha256:abc123def456...
# Build 2: sha256:abc123def456...
#
# ✓ Builds match - deterministic
# ✓ No timestamps embedded
# ✓ No hostname/user references
# ✓ No random values
```

---

## 3. Monitoring & Alerting

### 3.1 Key Metrics to Watch

| Metric | Target | Alert Threshold | Action |
|--------|--------|-----------------|--------|
| Quality Gate Pass Rate | >95% | <90% | Review recent failures, check for patterns |
| Lead Time (P90) | <2h | >4h | Investigate bottlenecks in pipeline |
| Andon Resolution Time | <4h avg | >8h for any | Escalate to team lead |
| Throughput | 100+ WO/day | <50 WO/day | Check pipeline health, WIP limits |
| Open Andons | 0-2 | >5 | Emergency response - page on-call |
| Test Pass Rate | >80% | <75% | Review test stability |
| Code Coverage | >80% | <75% | Enforce coverage gates |
| Defect Rate | <1% | >2% | Root cause analysis required |

### 3.2 Dashboard Monitoring

**Access:** http://localhost:8080

**Refresh Rate:** Auto-updates every 5 seconds (Server-Sent Events)

**Panels:**

1. **Overview Panel**
   - Total work orders (created, active, completed)
   - Open Andons count
   - Quality gate pass rate
   - Lead time trend (last 7 days)

2. **Quality Gates Panel**
   ```
   Gate              Pass Rate  Target  Status
   ─────────────────────────────────────────────
   Compilation       98.2%      >95%    ✓ PASS
   Testing           96.1%      >95%    ✓ PASS
   Deterministic     100%       100%    ✓ PASS
   Release           94.3%      >90%    ✓ PASS
   ```

3. **Kanban Board Panel**
   ```
   Bucket         Backlog  In Progress  Done  WIP Limit
   ───────────────────────────────────────────────────────
   security            12        3        47     5 (60%)
   reliability         28        4        89     5 (80%)
   cost                 8        2        34     5 (40%)
   compliance          15        1        23     5 (20%)
   features            42        7       156    10 (70%)
   technical_debt      19        3        67     5 (60%)
   ```

4. **Andon Alerts Panel**
   ```
   ID         Type              Age    Severity  Status
   ──────────────────────────────────────────────────────
   ANDON-001  test_failure      2h     HIGH      OPEN
   ANDON-002  shacl_violation   45m    MEDIUM    OPEN
   ```

5. **Kaizen Metrics Panel**
   ```
   Metric              Current  Previous  Change
   ────────────────────────────────────────────────
   Lead Time           1.8h     2.1h      -14% ↓
   Defect Rate         0.8%     1.2%      -33% ↓
   Throughput          127/day  115/day   +10% ↑
   First Pass Yield    96%      93%       +3%  ↑
   ```

6. **Production Flow Panel**
   - Active SKUs (with progress bars)
   - Throughput rate (SKUs/day)
   - Cycle time distribution histogram

### 3.3 Alerting Rules (Prometheus + Alertmanager)

**Configuration:** `config/prometheus_alerts.yml`

```yaml
groups:
  - name: tcps_alerts
    rules:
      - alert: HighAndonRate
        expr: tcps_andon_open_count > 5
        for: 10m
        labels:
          severity: critical
        annotations:
          summary: "More than 5 open Andons"
          description: "{{ $value }} Andons are open, blocking pipeline"

      - alert: LowQualityGatePassRate
        expr: tcps_quality_gate_pass_rate < 0.90
        for: 30m
        labels:
          severity: warning
        annotations:
          summary: "Quality gate pass rate below 90%"
          description: "Pass rate is {{ $value | humanizePercentage }}"

      - alert: HighLeadTime
        expr: tcps_lead_time_p90_hours > 4
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "P90 lead time exceeds 4 hours"
          description: "Lead time P90: {{ $value }}h (target: <4h)"

      - alert: WIPLimitExceeded
        expr: tcps_kanban_wip_utilization > 0.95
        for: 15m
        labels:
          severity: warning
        annotations:
          summary: "WIP limit near capacity"
          description: "{{ $labels.bucket }} at {{ $value | humanizePercentage }} capacity"

      - alert: LowThroughput
        expr: rate(tcps_work_orders_completed_total[1d]) < 50
        for: 2h
        labels:
          severity: warning
        annotations:
          summary: "Throughput below target"
          description: "Only {{ $value }} work orders completed per day (target: >100)"
```

### 3.4 Log Analysis

**Application Logs:**
```bash
# Tail application logs
tail -f _build/default/rel/erlmcp/log/erlang.log.1

# Filter for errors
grep -i error _build/default/rel/erlmcp/log/*.log

# Filter for Andon events
grep ANDON _build/default/rel/erlmcp/log/*.log
```

**TCPS-Specific Logs:**
```bash
# Andon event log
tail -f priv/tcps/logs/andon_events.log

# Quality gate log
tail -f priv/tcps/logs/quality_gates.log

# Work order log
tail -f priv/tcps/logs/work_orders.log
```

**Analyze Andon Patterns:**
```bash
# Identify most common Andon types
./tools/tcps root-cause analyze-patterns

# Output:
# Andon Pattern Analysis (Last 30 Days)
# ═══════════════════════════════════════════════════
#
# Top Failure Types:
#   1. test_failure (45%) - 127 events
#   2. non_determinism (28%) - 79 events
#   3. shacl_violation (18%) - 51 events
#   4. compilation_failure (9%) - 25 events
#
# Most Common Root Causes:
#   1. Missing test coverage (32%)
#   2. Race conditions (18%)
#   3. Timestamp embedded in code (15%)
#   4. SHACL constraint too strict (12%)
#
# Recommendations:
#   → Increase property-based test coverage
#   → Add race condition detection to CI
#   → Enforce deterministic build checks
#   → Review SHACL shapes for over-constraints
```

---

## 4. Troubleshooting

### 4.1 Common Issues

#### Issue: Pipeline Stuck

**Symptoms:**
- Work orders not progressing
- Dashboard shows stale data
- No new receipts generated

**Diagnosis:**
```bash
# Check 1: Are there open Andons blocking progress?
./tools/tcps andon list --status=open

# Check 2: Are WIP limits exceeded?
./tools/tcps kanban status

# Check 3: Are there dependency deadlocks?
./tools/tcps work-order show <wo-id> --dependencies

# Check 4: Are gen_server processes running?
./tools/tcps tpm health
```

**Resolution:**

If open Andons:
```bash
# Review and resolve blocking Andons
./tools/tcps andon show <andon-id>
./tools/tcps root-cause start <andon-id>
# (follow 5 Whys process)
./tools/tcps andon resolve <andon-id> --receipt-path=<receipt>
```

If WIP limits exceeded:
```bash
# Check if work orders are stuck
./tools/tcps work-order list --status=in_progress

# Temporarily increase limit (emergency only)
./tools/tcps kanban set-limit <bucket> <new-limit>
```

If dependency deadlock:
```bash
# View dependency graph
./tools/tcps work-order show <wo-id> --dependencies

# Cancel stuck work order to unblock
./tools/tcps work-order cancel <wo-id> --reason="Deadlock resolution"
```

#### Issue: Quality Gates Failing

**Symptoms:**
- High failure rate on quality gates (>10%)
- Frequent Andon events of same type
- Pass rate dropping

**Diagnosis:**
```bash
# Check which gate is failing
./tools/tcps quality gates <sku-id>

# Output:
# Quality Gates: sku_123
# ═══════════════════════════════════════════════════
# ✓ Compilation      PASS  (0.8s)
# ✗ Testing          FAIL  (5.2s)
#   → 3 tests failed: heijunka_test, wip_limit_test, pull_signal_test
# ✓ Deterministic    PASS  (1.1s)
# ✗ SHACL Validation FAIL  (0.3s)
#   → Constraint violation: tcps:workOrder must have tcps:priority
```

**Common Causes & Fixes:**

1. **Compilation Gate Failing**
   ```bash
   # Check compilation errors
   rebar3 compile

   # View error details
   cat _build/default/rel/erlmcp/log/erlang.log.1 | grep -A5 "compile error"
   ```

2. **Testing Gate Failing**
   ```bash
   # Run failing tests individually
   rebar3 eunit --module=tcps_kanban_tests

   # Check test logs
   cat _build/test/logs/eunit.log

   # For flaky tests:
   ./tools/tcps kaizen identify-flaky-tests
   ```

3. **Deterministic Gate Failing**
   ```bash
   # Check for non-deterministic sources
   ./tools/tcps receipt verify-deterministic <sku-id>

   # Common causes:
   # - Timestamps: erlang:timestamp() in code
   # - Random: rand:uniform() without seeding
   # - Hostname: $HOST or net_adm:localhost()
   # - Current directory: file:get_cwd()

   # Fix: Use deterministic alternatives
   # - Timestamps: pass as argument from caller
   # - Random: seed with deterministic value
   # - Hostname: config file or environment variable
   ```

4. **SHACL Validation Failing**
   ```bash
   # View SHACL validation report
   ./tools/tcps receipt validate <sku-id>

   # Common causes:
   # - Missing required property
   # - Invalid data type
   # - Constraint violation (min/max, pattern)

   # Fix: Update ontology or code to match shape
   ```

#### Issue: Dashboard Not Updating

**Symptoms:**
- Dashboard shows old data (timestamp >1 minute old)
- Metrics frozen
- SSE connection dropped

**Diagnosis:**
```bash
# Check 1: Is dashboard process running?
ps aux | grep tcps_dashboard

# Check 2: Test SSE connection
curl -N http://localhost:8080/api/stream

# Expected: stream of "data: {...}" messages every 5s

# Check 3: Check dashboard logs
tail -f _build/default/rel/erlmcp/log/erlang.log.1 | grep dashboard

# Check 4: Test API endpoints
curl http://localhost:8080/api/metrics/summary
```

**Resolution:**

If process crashed:
```bash
# Check crash dump
ls -lt _build/default/rel/erlmcp/log/*.dump | head -1

# Restart dashboard
./scripts/start_dashboard.sh restart

# Or restart entire application
make dev-console
# Then: application:start(erlmcp).
```

If SSE not working:
```bash
# Check firewall rules
sudo iptables -L | grep 8080

# Check browser console for errors
# (Open browser dev tools, check Network tab)

# Force reload browser (Ctrl+F5)
```

#### Issue: SPARQL Queries Slow

**Symptoms:**
- Receipt verification taking >5 seconds
- Dashboard loading slowly
- High CPU usage from SPARQL queries

**Diagnosis:**
```bash
# Check 1: How many receipts in ontology?
./tools/tcps receipt count

# Expected: <10,000 for good performance

# Check 2: Run benchmark
rebar3 eunit --module=tcps_ontology_benchmark

# Check 3: Profile slow queries
./tools/tcps tpm profile-sparql
```

**Resolution:**

If too many receipts:
```bash
# Archive old receipts (>90 days)
./tools/tcps tpm cleanup-receipts --older-than=90

# Rebuild indexes
./tools/tcps tpm rebuild-indexes
```

If indexes missing:
```bash
# Check index status
./tools/tcps tpm index-status

# Rebuild if needed
./tools/tcps tpm rebuild-indexes --force
```

### 4.2 Emergency Procedures

#### Procedure: Mass Andon Resolution

**When:** >10 Andons open, blocking entire pipeline

```bash
# 1. Identify pattern
./tools/tcps root-cause analyze-patterns

# 2. If single root cause (e.g., environment issue):
#    Create master resolution receipt
./tools/tcps root-cause start EMERGENCY-001 \
  --problem="Mass Andon events due to environment failure"

# 3. Perform 5 Whys once
./tools/tcps root-cause add-why EMERGENCY-001 1 "Test environment unavailable"
./tools/tcps root-cause add-why EMERGENCY-001 2 "Database connection pool exhausted"
# ... (continue 5 Whys)

./tools/tcps root-cause finalize EMERGENCY-001 \
  --root-cause="Database connection pool too small for load" \
  --prevention="Increase pool size to 50 connections"

# 4. Resolve all Andons with same receipt
for andon in $(./tools/tcps andon list --status=open --format=ids); do
  ./tools/tcps andon resolve $andon --receipt-path=receipt-EMERGENCY-001.json
done
```

#### Procedure: Rollback Deployment

**When:** New deployment causing mass failures

```bash
# 1. Stop current release
./scripts/deploy.sh stop

# 2. Identify last known good release
ls -lt _build/default/rel/ | head -5

# 3. Rollback to previous release
./scripts/deploy.sh rollback

# 4. Verify health
./tools/tcps tpm health

# 5. Create Andon for deployment failure
./tools/tcps andon trigger \
  --type=missing_receipt \
  --sku=deployment-$(date +%Y%m%d) \
  --stage=deployment \
  --message="Deployment failure - rolled back to previous version"
```

---

## 5. Maintenance Procedures

### 5.1 Daily Maintenance

**Automated via cron:**
```bash
# /etc/cron.d/tcps-daily
0 2 * * * /opt/erlmcp/tools/tcps tpm maintenance --daily
```

**Manual trigger:**
```bash
./tools/tcps tpm maintenance --daily

# Performs:
# ✓ Cleanup old receipts (>90 days)
# ✓ Vacuum ETS tables
# ✓ Check disk space
# ✓ Verify ontology integrity
# ✓ Generate daily report
```

### 5.2 Weekly Maintenance

```bash
./tools/tcps tpm maintenance --weekly

# Performs:
# ✓ Backup receipts and ontology
# ✓ Rebuild Dialyzer PLT
# ✓ Generate coverage report
# ✓ Review Kaizen improvements
# ✓ Check for dependency updates
# ✓ Performance benchmarks
```

**Backup Verification:**
```bash
# List recent backups
ls -lh /backups/tcps/

# Verify backup integrity
./tools/tcps tpm verify-backup /backups/tcps/tcps-20260126.tar.gz

# Expected:
# ✓ Archive integrity: OK
# ✓ Receipts: 1,247 files
# ✓ Ontology: 3.2MB
# ✓ Work orders: 892 entries
# ✓ All checksums valid
```

### 5.3 Monthly Maintenance

```bash
./tools/tcps tpm maintenance --monthly

# Performs:
# ✓ Security audit (bandit)
# ✓ Dependency updates (rebar3 upgrade --all)
# ✓ Performance benchmarks (full suite)
# ✓ Capacity planning review
# ✓ Certificate rotation (if applicable)
# ✓ Generate monthly report
```

**Security Audit:**
```bash
# Run security audit manually
rebar3 audit

# Review findings
cat _build/default/lib/*/security-audit.txt

# Address critical findings immediately
```

**Capacity Planning:**
```bash
# Generate capacity report
./tools/tcps tpm capacity-report

# Output:
# TCPS Capacity Planning Report
# ═══════════════════════════════════════════════════
#
# Current Usage (30-day average):
#   Work Orders:     127/day
#   Receipts:        1,524/day
#   Storage Growth:  15.2 MB/day
#   CPU:             35% average
#   Memory:          6.2 GB average
#
# Projected Growth (90 days):
#   Work Orders:     150/day (+18%)
#   Storage:         +1.4 GB
#   CPU:             42% average
#   Memory:          7.1 GB average
#
# Recommendations:
#   ✓ Current capacity sufficient for 6 months
#   → Plan storage expansion in 4 months
#   → Consider horizontal scaling if >200 WO/day
```

---

## 6. Disaster Recovery

### 6.1 Backup Strategy

**What to Back Up:**
- Receipts (JSON files in `priv/receipts/`)
- Ontology (RDF triples in `ontology/*.ttl`)
- Work orders (ETS dump or database export)
- Andon history (ETS dump)
- Configuration files (`config/*.config`)

**Backup Schedule:**
- **Frequency:** Daily at 2:00 AM
- **Retention:** 90 days
- **Location:** `/backups/tcps/` (local) + S3 (remote)

**Automated Backup:**
```bash
# /etc/cron.d/tcps-backup
0 2 * * * /opt/erlmcp/tools/tcps tpm backup --output=/backups/tcps/tcps-$(date +\%Y\%m\%d).tar.gz
0 3 * * * aws s3 cp /backups/tcps/tcps-$(date +\%Y\%m\%d).tar.gz s3://tcps-backups/
```

### 6.2 Restore Procedure

**Full System Restore:**

```bash
# 1. Stop TCPS
./scripts/deploy.sh stop

# 2. Restore from backup
./tools/tcps tpm restore --from=/backups/tcps/tcps-20260125.tar.gz

# Output:
# Restoring from backup: tcps-20260125.tar.gz
# ✓ Extracted 1,247 receipts
# ✓ Restored ontology (3.2 MB)
# ✓ Restored work orders (892 entries)
# ✓ Restored Andon history (45 events)
# ✓ Restored configuration

# 3. Verify integrity
./tools/tcps receipt verify-all

# Expected:
# Verifying all receipts...
# [========================================] 100%
# ✓ 1,247/1,247 receipts valid
# ✓ No gaps in receipt chain
# ✓ All checksums match

# 4. Restart TCPS
./scripts/deploy.sh start

# 5. Verify health
./tools/tcps tpm health

# Expected:
# ✓ TCPS System Health: OK
# ✓ All components operational
```

### 6.3 Data Loss Scenarios

#### Scenario 1: Receipt Storage Corruption

**Problem:** RDF ontology corrupted, receipts inaccessible

**Recovery:**
```bash
# Receipts are stored as both JSON and RDF
# Rebuild RDF ontology from JSON receipts
./tools/tcps tpm rebuild-ontology

# Output:
# Rebuilding ontology from JSON receipts...
# ✓ Found 1,247 JSON receipts
# ✓ Parsing and validating...
# ✓ Generating RDF triples...
# ✓ Writing to ontology/receipts.ttl
# ✓ Ontology rebuilt successfully (3.2 MB)
```

#### Scenario 2: Work Order Database Lost

**Problem:** ETS table crashed, all work orders lost

**Recovery:**
```bash
# Re-sync from GitHub issues (source of truth)
./tools/tcps work-order sync-github --full

# Output:
# Full sync from GitHub...
# ✓ Fetched 892 issues
# ✓ Recreated 892 work orders
# ✓ Restored priority and status from receipts
# ✓ Rebuilt dependency graph
```

#### Scenario 3: Complete System Failure

**Problem:** Server crashed, all data lost

**Recovery:**
```bash
# 1. Deploy from latest release
./scripts/deploy.sh install

# 2. Restore from last backup
./tools/tcps tpm restore --from=/backups/tcps/latest.tar.gz

# 3. Verify all services healthy
./tools/tcps tpm health

# 4. Re-sync any data created after backup
./tools/tcps work-order sync-github --since=$(date -d '1 day ago' +%Y-%m-%d)
```

---

## 7. Performance Tuning

### 7.1 Connection Pooling

**Configuration:** `config/sys.config`

```erlang
{poolboy, [
    {http_pool, [
        {size, 20},           % Worker pool size
        {max_overflow, 10}    % Extra workers under load
    ]}
]}.
```

**Tuning Guidelines:**
- **Low Load (<50 WO/day):** size=10, overflow=5
- **Medium Load (50-200 WO/day):** size=20, overflow=10 (default)
- **High Load (>200 WO/day):** size=50, overflow=20

**Monitor Pool Utilization:**
```bash
# Check pool stats
./tools/tcps tpm pool-stats

# Output:
# Pool: http_pool
# ═══════════════════════════════════════════════════
# Size:          20 workers
# Overflow:      10 (max)
# In Use:        12 workers (60%)
# Available:     8 workers
# Waiting:       0 requests
#
# Recommendation: Pool size adequate
```

### 7.2 ETS Table Tuning

**High-Concurrency Reads:**
```erlang
% For receipts cache (read-heavy)
ets:new(receipts_cache, [
    set,
    named_table,
    {read_concurrency, true},   % Enable concurrent reads
    {write_concurrency, false}  % Single writer
]).
```

**High-Concurrency Writes:**
```erlang
% For metrics collection (write-heavy)
ets:new(metrics, [
    set,
    named_table,
    {read_concurrency, false},
    {write_concurrency, true}   % Enable concurrent writes
]).
```

### 7.3 SPARQL Query Caching

**Enable Query Cache:**
```bash
# Set cache TTL (seconds)
./tools/tcps config set sparql_cache_ttl 120

# Monitor cache hit rate
./tools/tcps tpm cache-stats

# Output:
# SPARQL Query Cache
# ═══════════════════════════════════════════════════
# Hit Rate:      87.3%
# Misses:        12.7%
# Total Queries: 15,234
# Cache Size:    256 MB
# Evictions:     45
#
# Top Queries (by frequency):
#   1. receipt_chain_query (45%)
#   2. andon_history_query (23%)
#   3. work_order_status_query (18%)
```

### 7.4 Capacity Planning

**Storage Requirements:**
```
Per Work Order:
  Receipts:    ~10 KB (JSON)
  Ontology:    ~2 KB (RDF triples)
  Total:       ~12 KB

For 100 work orders/day:
  Daily:       1.2 MB
  Monthly:     36 MB
  Yearly:      438 MB
```

**Memory Requirements:**
```
Base:          100 MB (BEAM VM + libraries)
Per 1000 receipts:  1 MB (ETS tables + cache)
Per 100 active WO:  50 MB (gen_server state)
Dashboard:     50 MB (SSE connections + cache)

Total for 10,000 receipts + 500 active WO:
  ~500 MB (recommended: 2 GB for headroom)
```

**CPU Requirements:**
```
1 core per 100 work orders/hour
Recommended: 4 cores minimum, 8 cores for high load
```

---

## 8. Security Operations

### 8.1 Access Control

**CLI Tools:**
- Require sudo or group membership (`tcps` group)
- Audit log all commands to `/var/log/tcps/audit.log`

**Dashboard:**
- Basic HTTP auth (development)
- OAuth integration (production)
- TLS/SSL required for production

**API:**
- Token-based authentication
- Rate limiting (100 requests/minute per IP)

### 8.2 Secrets Management

**DO NOT:**
- ❌ Store secrets in config files
- ❌ Commit secrets to git
- ❌ Pass secrets via command line
- ❌ Log secrets

**DO:**
- ✅ Use environment variables
- ✅ Use HashiCorp Vault or AWS Secrets Manager
- ✅ Rotate credentials quarterly
- ✅ Use encrypted storage for backups

**Example:**
```bash
# Set secrets via environment
export TCPS_DB_PASSWORD=$(vault read -field=password secret/tcps/db)

# Or via config file (encrypted at rest)
./tools/tcps config set-secret db_password --vault-path=secret/tcps/db
```

### 8.3 Audit Trail

**View Andon Audit Trail:**
```bash
./tools/tcps andon audit ANDON-001

# Output:
# Audit Trail: ANDON-001
# ═══════════════════════════════════════════════════
#
# 2026-01-26 09:15:00  CREATED     by:system
#   Event triggered: test_failure in sku_123
#
# 2026-01-26 09:20:00  INVESTIGATED  by:operator1
#   Started 5 Whys analysis: analysis_001
#
# 2026-01-26 10:45:00  RESOLVED    by:operator1
#   Root cause: Missing test coverage
#   Prevention: Add property-based tests
#   Receipt: receipt-andon-001-resolution.json
```

**View Receipt Audit Trail:**
```bash
./tools/tcps receipt audit sku_123

# Shows complete chain of custody with signatures
```

**Export Audit Logs:**
```bash
# Export all audit logs for time period
./tools/tcps tpm export-audit \
  --from=2026-01-01 \
  --to=2026-01-31 \
  --output=audit-2026-01.json
```

---

## 9. Appendices

### Appendix A: CLI Command Reference

**Complete list of all TCPS CLI commands:**

#### Work Order Commands
```bash
tcps work-order create --bucket=<bucket> --priority=<1-10> --title="<title>"
tcps work-order list [--status=<status>] [--bucket=<bucket>]
tcps work-order show <work-order-id>
tcps work-order complete <work-order-id> <sku-id>
tcps work-order cancel <work-order-id> --reason="<reason>"
tcps work-order sync-github [--since=<date>]
tcps work-order process-next
```

#### Andon Commands
```bash
tcps andon trigger --type=<type> --sku=<sku-id> --stage=<stage> [--message="<msg>"]
tcps andon list [--status=<open|resolved|all>] [--sku=<sku-id>]
tcps andon show <andon-id>
tcps andon resolve <andon-id> --receipt-path=<path>
tcps andon status <sku-id>
tcps andon audit <andon-id>
```

#### Receipt Commands
```bash
tcps receipt verify <stage> <sku-id>
tcps receipt chain <sku-id> [--format=<table|json|graph>]
tcps receipt list [--recent=<n>]
tcps receipt show <receipt-id>
tcps receipt validate <receipt-id>
tcps receipt verify-chain <sku-id>
tcps receipt verify-deterministic <sku-id>
tcps receipt count
tcps receipt audit <sku-id>
```

#### Quality Commands
```bash
tcps quality gates <sku-id> [--verbose]
tcps quality metrics [--period=<daily|weekly|monthly>] [--compare]
tcps quality dashboard
tcps quality report [--output=<file>]
tcps quality list-defects [--recent=<n>]
```

#### Kanban Commands
```bash
tcps kanban status [--bucket=<bucket>]
tcps kanban schedule
tcps kanban set-limit <bucket> <limit>
tcps kanban pull
```

#### Kaizen Commands
```bash
tcps kaizen report [--daily|--weekly|--monthly] [--output=<file>]
tcps kaizen proposals [--top=<n>] [--roi-threshold=<n>]
tcps kaizen apply <improvement-id>
tcps kaizen waste
tcps kaizen trends
tcps kaizen identify-flaky-tests
```

#### Root Cause Commands
```bash
tcps root-cause start <andon-id> [--problem="<problem>"]
tcps root-cause add-why <analysis-id> <why-number> "<answer>"
tcps root-cause finalize <analysis-id> --root-cause="<cause>" --prevention="<action>"
tcps root-cause show <analysis-id>
tcps root-cause list
tcps root-cause analyze-patterns
```

#### TPM Commands
```bash
tcps tpm maintenance [--daily|--weekly|--monthly] [--force]
tcps tpm health
tcps tpm metrics
tcps tpm backup --output=<file>
tcps tpm restore --from=<file>
tcps tpm verify-backup <file>
tcps tpm cleanup-receipts --older-than=<days>
tcps tpm rebuild-ontology
tcps tpm rebuild-indexes [--force]
tcps tpm index-status
tcps tpm pool-stats
tcps tpm cache-stats
tcps tpm capacity-report
tcps tpm export-audit --from=<date> --to=<date> --output=<file>
tcps tpm profile-sparql
```

### Appendix B: Configuration Reference

**File:** `config/sys.config`

```erlang
[
    {erlmcp, [
        %% TCPS Configuration
        {tcps, [
            {receipts_dir, "priv/receipts"},
            {ontology_file, "ontology/receipts.ttl"},
            {shacl_shapes, "priv/shacl/shapes.ttl"},

            %% Kanban WIP Limits
            {wip_limits, #{
                security => 5,
                reliability => 5,
                cost => 5,
                compliance => 5,
                features => 10,
                technical_debt => 5
            }},

            %% SLA Hours by Bucket
            {sla_hours, #{
                security => 24,        % 24 hours
                reliability => 168,    % 7 days
                features => 720,       % 30 days
                cost => 720,
                compliance => 168,
                technical_debt => infinity
            }},

            %% Dashboard
            {dashboard_port, 8080},
            {dashboard_refresh_interval, 5000},  % 5 seconds

            %% Quality Gates
            {quality_gates, #{
                test_pass_rate_target => 0.80,
                code_coverage_target => 0.80,
                defect_rate_target => 0.01,
                first_pass_yield_target => 0.95
            }},

            %% Kaizen
            {kaizen_improvement_target, 0.05},  % 5% per week

            %% SPARQL Cache
            {sparql_cache_ttl, 60},  % seconds
            {sparql_cache_size, 256}  % MB
        ]}
    ]}
].
```

### Appendix C: Metrics Glossary

| Metric | Definition | Calculation | Target |
|--------|------------|-------------|--------|
| **Lead Time** | Time from work order creation to SKU published | `completed_at - created_at` | <2h (P90) |
| **Cycle Time** | Time spent actively working (excludes queue time) | Sum of stage durations | <1h (P90) |
| **Throughput** | Work orders completed per unit time | `completed_count / time_period` | >100/day |
| **Defect Rate** | Andon events per 100 SKUs | `(andon_count / sku_count) * 100` | <1% |
| **First Pass Yield** | SKUs passing without rework (no Andons) | `(no_andon_count / sku_count) * 100` | >95% |
| **Rework Rate** | SKUs requiring fixes after initial completion | `(rework_count / sku_count) * 100` | <5% |
| **Quality Gate Pass Rate** | Percentage passing all quality gates | `(passed / total) * 100` | >95% |
| **Andon Resolution Time** | Average time to resolve Andon event | `resolved_at - triggered_at` | <4h avg |
| **WIP Utilization** | Current WIP as percentage of limit | `(current_wip / limit) * 100` | 60-80% |
| **Queue Depth** | Number of work orders waiting in queue | Count of status=queued | Varies by bucket |

### Appendix D: Alert Playbooks

#### Alert: HighAndonRate

**Trigger:** >5 open Andons for >10 minutes
**Severity:** Critical

**Response:**
1. Page on-call engineer immediately
2. Check for pattern (mass failure vs individual)
   ```bash
   ./tools/tcps root-cause analyze-patterns
   ```
3. If pattern exists:
   - Create emergency root cause analysis
   - Resolve all Andons with same receipt
   - See "Emergency Procedures: Mass Andon Resolution"
4. If individual issues:
   - Prioritize by age (oldest first)
   - Assign to team members
   - Set 2-hour SLA for resolution

#### Alert: LowQualityGatePassRate

**Trigger:** Pass rate <90% for >30 minutes
**Severity:** Warning

**Response:**
1. Identify failing gate
   ```bash
   ./tools/tcps quality metrics --period=hourly
   ```
2. Review recent failures
   ```bash
   ./tools/tcps quality list-defects --recent=10
   ```
3. Check for flaky tests
   ```bash
   ./tools/tcps kaizen identify-flaky-tests
   ```
4. If systemic issue:
   - Trigger Andon to stop new work
   - Fix root cause
   - Resume work after fix verified

#### Alert: HighLeadTime

**Trigger:** P90 lead time >4h for >1 hour
**Severity:** Warning

**Response:**
1. Identify bottleneck stage
   ```bash
   ./tools/tcps kaizen waste
   ```
2. Check WIP limits (may need increase)
   ```bash
   ./tools/tcps kanban status
   ```
3. Review stuck work orders
   ```bash
   ./tools/tcps work-order list --status=in_progress --sort-by=age
   ```
4. Apply Kaizen improvements
   ```bash
   ./tools/tcps kaizen proposals --top=5
   ```

### Appendix E: Integration Guides

#### GitHub Integration

**Setup:**
```bash
# Configure GitHub API token
export GITHUB_TOKEN=<your-token>

# Or set in config
./tools/tcps config set github_token --vault-path=secret/github/token

# Configure repository
./tools/tcps config set github_repo banyan-platform/erlmcp
```

**Usage:**
```bash
# Sync issues as work orders
./tools/tcps work-order sync-github

# Auto-sync every hour (cron)
0 * * * * /opt/erlmcp/tools/tcps work-order sync-github
```

**Webhook Integration:**
```erlang
% In your web server
handle_github_webhook(IssueEvent) ->
    #{
        <<"action">> := Action,
        <<"issue">> := Issue
    } = IssueEvent,

    case Action of
        <<"opened">> ->
            create_work_order_from_issue(Issue);
        <<"closed">> ->
            complete_work_order_from_issue(Issue);
        _ ->
            ok
    end.
```

#### JIRA Integration

**Setup:**
```bash
# Configure JIRA credentials
./tools/tcps config set jira_url https://company.atlassian.net
./tools/tcps config set jira_username user@company.com
./tools/tcps config set-secret jira_api_token --vault-path=secret/jira/token
```

**Usage:**
```bash
# Sync JIRA issues
./tools/tcps work-order sync-jira --project=PROJ
```

#### Prometheus Integration

**Expose Metrics:**
```bash
# Start Prometheus exporter
./tools/tcps tpm metrics-exporter --port=9100
```

**Prometheus Config:**
```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'tcps'
    static_configs:
      - targets: ['localhost:9100']
    scrape_interval: 30s
```

**Available Metrics:**
```
# Work Orders
tcps_work_orders_total{bucket="security",status="completed"}
tcps_work_orders_in_progress{bucket="reliability"}
tcps_work_order_lead_time_seconds{bucket="features",quantile="0.9"}

# Andons
tcps_andon_open_count
tcps_andon_total{type="test_failure"}
tcps_andon_resolution_time_seconds{quantile="0.9"}

# Quality Gates
tcps_quality_gate_pass_rate{gate="testing"}
tcps_quality_gate_failures_total{gate="compilation"}

# Kanban
tcps_kanban_wip_count{bucket="security"}
tcps_kanban_wip_utilization{bucket="reliability"}

# Kaizen
tcps_kaizen_lead_time_hours{quantile="0.9"}
tcps_kaizen_defect_rate
tcps_kaizen_throughput_per_day
```

---

**Document Revision History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-01-26 | TCPS Ops Team | Initial release |

**Next Review Date:** 2026-02-26
