# TCPS New Operator Onboarding Guide

**Duration:** 2-3 days
**Prerequisites:** Basic Linux/Unix command line knowledge
**Target:** New team members operating TCPS in production

---

## Table of Contents

1. [Day 1: TCPS Fundamentals](#day-1-tcps-fundamentals)
2. [Day 2: Daily Operations](#day-2-daily-operations)
3. [Day 3: Troubleshooting & Advanced Topics](#day-3-troubleshooting--advanced-topics)
4. [Certification Checklist](#certification-checklist)

---

## Day 1: TCPS Fundamentals

### Session 1.1: Introduction to Toyota Code Production System (30 min)

**Learning Objectives:**
- Understand Toyota Production System (TPS) principles
- Learn how TCPS applies TPS to software engineering
- Understand the 4 pillars: Andon, Kanban, Kaizen, Receipts

**Key Concepts:**

1. **Toyota Production System (TPS) Origins**
   - Manufacturing system developed by Toyota
   - Focus: Eliminate waste, continuous improvement, quality
   - Principles: Just-in-time, Jidoka (automation with human touch)

2. **TCPS: TPS for Software**
   - Applies TPS principles to code production
   - Treats code as manufactured goods (SKUs)
   - Quality gates instead of assembly line checkpoints
   - Andon stops "production" when defects detected

3. **The 4 Pillars**

   **Andon (Stop-the-Line)**
   - Automatically triggers when quality issues detected
   - Blocks SKU from progressing until resolved
   - Requires root cause analysis (5 Whys)
   - Generates immutable receipts

   **Kanban (Pull-Based Flow)**
   - Work-in-progress (WIP) limits prevent overload
   - Work is "pulled" based on capacity
   - Heijunka leveling prevents batching
   - Visualized on dashboard

   **Kaizen (Continuous Improvement)**
   - Target: 5% improvement per week
   - Automated waste identification
   - Improvement proposals with ROI
   - Week-over-week trend analysis

   **Receipts (Audit Trail)**
   - Immutable records of all operations
   - JSON files + RDF triples (ontology)
   - Deterministic build verification
   - Complete chain of custody

**Hands-On Exercise 1.1:**

```bash
# 1. Check system health
./tools/tcps tpm health

# Q: What are the 5 health checks performed?
# A: ETS tables, gen_server processes, disk space, receipt storage, ontology

# 2. View dashboard
open http://localhost:8080

# Q: What are the 6 panels on the dashboard?
# A: Overview, Quality Gates, Kanban Board, Andon Alerts, Kaizen Metrics, Production Flow

# 3. List recent work orders
./tools/tcps work-order list --status=pending

# Q: What buckets do you see?
# A: security, reliability, cost, compliance, features, technical_debt
```

---

### Session 1.2: Work Order Lifecycle (45 min)

**Learning Objectives:**
- Understand work order creation and routing
- Learn pull signal sources (GitHub, CVE, marketplace)
- Practice work order management commands

**Work Order Flow:**

```
Pull Signal → Route → Create Work Order → Queue (Kanban) →
Pull (Heijunka) → Process → Quality Gates → Complete (or Andon)
→ Receipt → Kaizen Metrics
```

**Pull Signal Sources:**
1. **GitHub Issues** - Feature requests, bugs, enhancements
2. **CVE Advisories** - Security vulnerabilities (highest priority)
3. **Marketplace** - Install/refund events (demand signals)
4. **Internal** - Technical debt, refactoring

**Hands-On Exercise 1.2:**

```bash
# 1. Create a work order manually
./tools/tcps work-order create \
  --bucket=reliability \
  --priority=7 \
  --title="Fix flaky test in tcps_kanban_tests" \
  --description="Test fails intermittently due to race condition"

# Note the work order ID (e.g., WO-1234567890-123456)

# 2. View work order details
./tools/tcps work-order show WO-<your-id>

# Q: What is the SLA deadline for reliability bucket?
# A: 168 hours (7 days)

# 3. Check Kanban status
./tools/tcps kanban status

# Q: What is the WIP limit for reliability bucket?
# A: 5 (default)

# 4. Process the work order (Heijunka scheduling)
./tools/tcps work-order process-next

# Q: Was your work order selected? If not, which one was and why?
# A: Heijunka selects highest priority while maintaining bucket distribution

# 5. View receipts generated
ls -lh priv/receipts/ | tail -5

# You should see: receipt-creation-*.json
```

**Quiz 1.2:**

1. What are the 6 work order buckets?
2. What is the highest priority a work order can have?
3. What happens when a WIP limit is reached?
4. What is Heijunka leveling?
5. Where are receipts stored?

---

### Session 1.3: Andon System (60 min)

**Learning Objectives:**
- Understand Andon event types
- Learn how to investigate and resolve Andons
- Practice 5 Whys root cause analysis

**Andon Event Types:**

| Type | Trigger | Example |
|------|---------|---------|
| `test_failure` | Test fails or coverage <80% | Unit test assertion failure |
| `shacl_violation` | RDF ontology constraint violated | Missing required property |
| `compilation_failure` | Code doesn't compile | Syntax error |
| `non_determinism` | Build not reproducible | Timestamp in output |
| `missing_receipt` | Receipt not found | Chain of custody broken |

**Andon Workflow:**

```
Quality Issue Detected → Andon Triggered → SKU Blocked →
Investigation (5 Whys) → Root Cause → Fix Applied →
Prevention Measures → Andon Resolved → Receipt Generated →
SKU Unblocked
```

**Hands-On Exercise 1.3:**

Let's create a simulated Andon event:

```bash
# 1. Create a test SKU with intentional failure
mkdir -p test_sku/
cat > test_sku/example.erl << 'EOF'
-module(example).
-export([get_timestamp/0]).

get_timestamp() ->
    % INTENTIONAL BUG: Non-deterministic timestamp
    erlang:timestamp().
EOF

# 2. Trigger compilation (will detect non-determinism)
cd test_sku && erlc example.erl

# 3. Simulate Andon trigger
./tools/tcps andon trigger \
  --type=non_determinism \
  --sku=sku_test_001 \
  --stage=execution \
  --message="Timestamp embedded in code violates determinism"

# Note the Andon ID (e.g., ANDON-1234567890-123456-789012)

# 4. View Andon details
./tools/tcps andon show ANDON-<your-id>

# 5. Start 5 Whys root cause analysis
./tools/tcps root-cause start ANDON-<your-id>

# Note the analysis ID (e.g., analysis_001)

# 6. Perform 5 Whys (answer each question)
./tools/tcps root-cause add-why analysis_001 1 \
  "Code uses erlang:timestamp() directly"

./tools/tcps root-cause add-why analysis_001 2 \
  "Timestamp needed for logging but not passed as parameter"

./tools/tcps root-cause add-why analysis_001 3 \
  "Function doesn't accept timestamp parameter"

./tools/tcps root-cause add-why analysis_001 4 \
  "Initial design didn't consider deterministic build requirement"

./tools/tcps root-cause add-why analysis_001 5 \
  "No design review checklist for deterministic builds"

# 7. Finalize analysis
./tools/tcps root-cause finalize analysis_001 \
  --root-cause="No design review checklist for deterministic builds" \
  --prevention="Add deterministic build checklist to design reviews + add static analysis check"

# 8. "Fix" the code (replace with deterministic version)
cat > test_sku/example.erl << 'EOF'
-module(example).
-export([get_timestamp/1]).

% Pass timestamp as parameter for deterministic builds
get_timestamp(Timestamp) ->
    Timestamp.
EOF

# 9. Verify fix
cd test_sku && erlc example.erl

# 10. Resolve Andon
./tools/tcps andon resolve ANDON-<your-id> \
  --receipt-path=priv/receipts/receipt-root-cause-analysis_001.json

# 11. Verify SKU unblocked
./tools/tcps andon status sku_test_001
```

**5 Whys Technique:**

The goal is to drill down to the **root cause**, not just symptoms.

**Bad Example (stops too early):**
- Why did the test fail? "Because the function returned wrong value"
- Why did it return wrong value? "Because of a bug"
- *STOP* - This doesn't identify the root cause!

**Good Example (continues to root cause):**
- Why did the test fail? "Function returned different timestamps"
- Why different timestamps? "erlang:timestamp() called directly"
- Why called directly? "No timestamp parameter in function"
- Why no parameter? "Design didn't consider determinism"
- Why not considered? "No design review checklist for determinism"
- **ROOT CAUSE**: Missing design review checklist
- **PREVENTION**: Add checklist + automated checks

**Quiz 1.3:**

1. What are the 5 Andon event types?
2. What happens when an Andon is triggered?
3. What is the purpose of 5 Whys analysis?
4. What is required to resolve an Andon?
5. What is a receipt?

**End of Day 1**

---

## Day 2: Daily Operations

### Session 2.1: Morning Checklist Routine (30 min)

**Learning Objectives:**
- Learn the daily morning checklist
- Understand alert thresholds
- Practice dashboard monitoring

**The 5-Minute Morning Checklist:**

```bash
# Create morning check script
cat > ~/tcps-morning-check.sh << 'EOF'
#!/bin/bash
set -e

echo "=== TCPS Morning Health Check ==="
echo ""

echo "1. System Health:"
./tools/tcps tpm health
echo ""

echo "2. Open Andons:"
./tools/tcps andon list --status=open
echo ""

echo "3. Quality Metrics (Yesterday):"
./tools/tcps quality metrics --period=daily
echo ""

echo "4. Kanban WIP Status:"
./tools/tcps kanban status
echo ""

echo "5. Lead Time Trend:"
./tools/tcps kaizen report --daily
echo ""

echo "=== Morning Check Complete ==="
echo "Dashboard: http://localhost:8080"
EOF

chmod +x ~/tcps-morning-check.sh

# Run morning check
~/tcps-morning-check.sh
```

**Alert Thresholds (MEMORIZE THESE):**

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Open Andons | 0-2 | >3 | >5 (page on-call) |
| Quality Pass Rate | >95% | <92% | <90% |
| Lead Time (P90) | <2h | >3h | >4h |
| Throughput | >100/day | <75/day | <50/day |

**Hands-On Exercise 2.1:**

```bash
# 1. Run morning check
~/tcps-morning-check.sh > morning-check-$(date +%Y%m%d).log

# 2. Review yesterday's metrics
./tools/tcps kaizen report --daily --output=kaizen-daily.txt
cat kaizen-daily.txt

# Q: What is your current lead time P90?
# Q: Is it within target (<2h)?

# 3. Open dashboard in browser
open http://localhost:8080

# Watch the dashboard for 1 minute
# Q: What is the auto-refresh interval?
# A: 5 seconds (Server-Sent Events)

# 4. Simulate an alert condition (for training)
# This creates 6 simulated Andons to trigger alert threshold
for i in {1..6}; do
  ./tools/tcps andon trigger \
    --type=test_failure \
    --sku=sku_training_$i \
    --stage=testing \
    --message="Training Andon #$i"
  sleep 1
done

# 5. Check alert triggered
./tools/tcps andon list --status=open

# Q: How many open Andons?
# A: Should be 6 (critical threshold)

# 6. Clean up training Andons
for i in {1..6}; do
  andon_id=$(./tools/tcps andon list --status=open --format=ids | head -1)
  ./tools/tcps andon resolve $andon_id --receipt-path=/dev/null
done
```

---

### Session 2.2: Processing Work Orders (45 min)

**Learning Objectives:**
- Master work order lifecycle commands
- Understand Heijunka scheduling
- Practice receipt verification

**Hands-On Exercise 2.2:**

```bash
# 1. Sync work orders from GitHub
./tools/tcps work-order sync-github

# 2. View work order queue by bucket
for bucket in security reliability cost compliance features technical_debt; do
  echo "=== $bucket ==="
  ./tools/tcps work-order list --status=pending --bucket=$bucket | head -5
  echo ""
done

# 3. Process next 3 work orders
for i in {1..3}; do
  echo "Processing work order #$i..."
  ./tools/tcps work-order process-next
  sleep 2
done

# 4. Check which buckets were selected
./tools/tcps kanban status

# Q: Were work orders spread across buckets or batched in one?
# A: Should be spread (Heijunka leveling prevents batching)

# 5. View active work orders
./tools/tcps work-order list --status=in_progress

# 6. Complete a work order (simulated)
wo_id=$(./tools/tcps work-order list --status=in_progress --format=ids | head -1)
sku_id="sku_$(date +%s)"

./tools/tcps work-order complete $wo_id $sku_id

# 7. Verify receipt chain
./tools/tcps receipt verify-chain $sku_id

# You should see:
# ✓ receipt-creation-*.json
# ✓ receipt-start-*.json
# ✓ receipt-completion-*.json
```

**Understanding Heijunka Leveling:**

Heijunka is a Japanese word meaning "leveling" or "smoothing". In TCPS:

- Prevents **batching** (all security, then all reliability, etc.)
- Distributes work **evenly** across buckets
- Maintains **flow** instead of stop-start patterns
- Reduces **WIP** by limiting concurrent work

**Example:**

**❌ Without Heijunka (Batching):**
```
Security: [WO1, WO2, WO3, WO4, WO5] → Process all security first
Reliability: [WO6, WO7, WO8] → Then all reliability
Features: [WO9, WO10, WO11, WO12] → Then all features
```
Result: Long wait times, uneven load, high WIP

**✅ With Heijunka (Leveling):**
```
Security: [WO1] → Process one
Reliability: [WO6] → Process one
Features: [WO9] → Process one
Security: [WO2] → Back to security
Reliability: [WO7] → Back to reliability
Features: [WO10] → Back to features
```
Result: Smooth flow, even load, lower WIP

---

### Session 2.3: Quality Gates & Receipts (45 min)

**Learning Objectives:**
- Understand quality gate checks
- Practice receipt verification
- Learn deterministic build validation

**Quality Gates:**

| Gate | What It Checks | Pass Criteria |
|------|----------------|---------------|
| Compilation | Code compiles without errors | 0 errors, <5 warnings |
| Testing | Tests pass with coverage | >80% pass rate, >80% coverage |
| Deterministic | Build is reproducible | Same input → same output |
| SHACL Validation | Ontology constraints met | No constraint violations |

**Hands-On Exercise 2.3:**

```bash
# 1. Check quality gates for a SKU
sku_id="sku_test_gates"
./tools/tcps quality gates $sku_id

# Expected output:
# ✓ Compilation      PASS  (0.8s)
# ✓ Testing          PASS  (5.2s)
# ✓ Deterministic    PASS  (1.1s)
# ✓ SHACL Validation PASS  (0.3s)

# 2. View receipt for each gate
ls priv/receipts/ | grep $sku_id

# 3. Examine compilation receipt
cat priv/receipts/receipt-compilation-$sku_id.json | jq .

# Q: What information is in the receipt?
# A: timestamp, duration, errors, warnings, files compiled

# 4. Verify deterministic build
./tools/tcps receipt verify-deterministic $sku_id

# This rebuilds twice and compares hashes
# ✓ Build 1: sha256:abc123...
# ✓ Build 2: sha256:abc123...
# ✓ Builds match - deterministic

# 5. View complete receipt chain
./tools/tcps receipt chain $sku_id --format=graph

# Shows ASCII graph of receipt dependencies

# 6. Validate receipt against SHACL
./tools/tcps receipt validate receipt-compilation-$sku_id.json

# Checks receipt conforms to SHACL shapes
```

**Understanding Deterministic Builds:**

**Why Deterministic Builds Matter:**
- **Security**: Verify supply chain integrity
- **Reproducibility**: Anyone can rebuild and verify
- **Trust**: Immutable receipts prove authenticity
- **Debugging**: Exact same binary for issue reproduction

**Common Non-Deterministic Sources:**

❌ **Timestamps:**
```erlang
% BAD - Non-deterministic
get_build_time() ->
    erlang:timestamp().

% GOOD - Pass as parameter
get_build_time(Timestamp) ->
    Timestamp.
```

❌ **Random:**
```erlang
% BAD - Non-deterministic
generate_id() ->
    rand:uniform(1000000).

% GOOD - Deterministic seed
generate_id(Seed) ->
    rand:seed(exsplus, {Seed, 0, 0}),
    rand:uniform(1000000).
```

❌ **Hostname:**
```erlang
% BAD - Non-deterministic
get_server() ->
    net_adm:localhost().

% GOOD - From config
get_server(Config) ->
    maps:get(hostname, Config).
```

**End of Day 2**

---

## Day 3: Troubleshooting & Advanced Topics

### Session 3.1: Troubleshooting Common Issues (60 min)

**Learning Objectives:**
- Diagnose pipeline stuck issues
- Resolve quality gate failures
- Handle dashboard problems

**Hands-On Exercise 3.1: Pipeline Stuck**

```bash
# Scenario: Pipeline stopped progressing

# Step 1: Check for open Andons
./tools/tcps andon list --status=open

# If Andons present: Follow Andon resolution procedure (Day 1)

# Step 2: Check WIP limits
./tools/tcps kanban status

# If WIP at limit:
# - Review in-progress work orders
./tools/tcps work-order list --status=in_progress --sort-by=age

# - Check for stuck work orders (>24h in progress)
# - Complete or cancel stuck work orders

# Step 3: Check dependencies
./tools/tcps work-order show <wo-id> --dependencies

# If circular dependencies detected:
# - Visualize dependency graph
# - Cancel one work order to break cycle

# Step 4: Check system health
./tools/tcps tpm health

# If health check fails:
# - Review logs
# - Restart crashed processes
# - Check disk space
```

**Hands-On Exercise 3.2: Quality Gate Failures**

```bash
# Scenario: Test gate failing consistently

# 1. View recent failures
./tools/tcps quality list-defects --recent=10

# 2. Identify failing test
./tools/tcps quality gates <sku-id> --verbose

# Output shows:
# ✗ Testing FAIL
#   → 3 tests failed:
#     - tcps_kanban_tests:heijunka_test
#     - tcps_kanban_tests:wip_limit_test
#     - tcps_kanban_tests:pull_signal_test

# 3. Run test individually
rebar3 eunit --module=tcps_kanban_tests --verbose

# 4. Check if test is flaky
./tools/tcps kaizen identify-flaky-tests

# If test is flaky:
# - Mark as known flaky
# - Create work order to fix
# - Quarantine test temporarily

# 5. Check test coverage
rebar3 cover
rebar3 cover --verbose

# If coverage <80%:
# - Identify uncovered lines
# - Add tests for uncovered code
```

---

### Session 3.2: Advanced Operations (45 min)

**Learning Objectives:**
- Master backup and restore
- Understand performance tuning
- Learn capacity planning

**Hands-On Exercise 3.3: Backup & Restore**

```bash
# 1. Create backup
./tools/tcps tpm backup \
  --output=/tmp/tcps-backup-$(date +%Y%m%d).tar.gz

# 2. Verify backup
./tools/tcps tpm verify-backup /tmp/tcps-backup-*.tar.gz

# Expected output:
# ✓ Archive integrity: OK
# ✓ Receipts: 1,247 files
# ✓ Ontology: 3.2MB
# ✓ Work orders: 892 entries
# ✓ All checksums valid

# 3. Simulate data loss (test environment only!)
rm -rf priv/receipts/*.json

# 4. Restore from backup
./tools/tcps tpm restore \
  --from=/tmp/tcps-backup-$(date +%Y%m%d).tar.gz

# 5. Verify restore
./tools/tcps receipt verify-all

# Should show all receipts restored successfully
```

**Hands-On Exercise 3.4: Performance Tuning**

```bash
# 1. Check current pool stats
./tools/tcps tpm pool-stats

# Output:
# Pool: http_pool
# Size: 20 workers
# In Use: 18 workers (90%)  ← High utilization!
# Available: 2 workers
# Waiting: 5 requests       ← Queue building up!

# 2. Check SPARQL cache efficiency
./tools/tcps tpm cache-stats

# Output:
# Hit Rate: 65.3%          ← Low hit rate
# Misses: 34.7%
# Total Queries: 15,234
# Cache Size: 256 MB

# 3. Tune configuration
# Edit config/sys.config:
{poolboy, [
    {http_pool, [
        {size, 50},           % Increased from 20
        {max_overflow, 20}    % Increased from 10
    ]}
]}.

{tcps, [
    {sparql_cache_ttl, 120},  % Increased from 60
    {sparql_cache_size, 512}  % Increased from 256
]}.

# 4. Restart with new config
./scripts/deploy.sh restart

# 5. Verify improvements
./tools/tcps tpm pool-stats
./tools/tcps tpm cache-stats
```

---

### Session 3.3: Certification Exam (30 min)

**Practical Exam:**

You will be given a production-like scenario and must:

1. Perform morning health check
2. Process 5 work orders
3. Handle 2 Andon events (with 5 Whys)
4. Verify receipts for completed work orders
5. Identify and resolve one troubleshooting scenario

**Time Limit:** 30 minutes

**Pass Criteria:** 80% tasks completed correctly

---

## Certification Checklist

### Knowledge Checks

- [ ] Can explain the 4 pillars of TCPS
- [ ] Understands work order lifecycle
- [ ] Knows all 5 Andon event types
- [ ] Can perform 5 Whys root cause analysis
- [ ] Understands Heijunka leveling
- [ ] Knows quality gate criteria
- [ ] Understands deterministic builds
- [ ] Can interpret dashboard panels

### Operational Skills

- [ ] Perform morning health check independently
- [ ] Create and process work orders
- [ ] Investigate and resolve Andon events
- [ ] Verify receipt chains
- [ ] Use dashboard for monitoring
- [ ] Execute backup and restore
- [ ] Diagnose common troubleshooting scenarios

### Command Proficiency

- [ ] Work order: create, list, show, complete
- [ ] Andon: trigger, list, show, resolve
- [ ] Root cause: start, add-why, finalize
- [ ] Receipt: verify-chain, verify-deterministic
- [ ] Quality: gates, metrics
- [ ] Kanban: status, set-limit
- [ ] Kaizen: report, proposals
- [ ] TPM: health, maintenance, backup, restore

### Emergency Response

- [ ] Can handle mass Andon resolution
- [ ] Can troubleshoot pipeline stuck
- [ ] Can restart dashboard
- [ ] Knows escalation procedures
- [ ] Can perform emergency rollback

---

## Post-Training Resources

### Daily Practice

**Week 1-2:**
- Shadow experienced operator for morning checks
- Process 5 work orders per day
- Resolve 1-2 Andons per day (if available)

**Week 3-4:**
- Perform morning checks independently
- Take on-call shifts (with backup)
- Handle full daily operations

### Continuous Learning

- **Monthly:** Review [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md)
- **Quarterly:** Advanced troubleshooting workshop
- **Annually:** TPS principles refresher course

### Mentorship

- Assigned Mentor: [Your Team Lead]
- Weekly 1:1 for first month
- Monthly check-ins thereafter

---

## Certification

Upon completing this training and passing the practical exam, you will receive:

- ✅ **TCPS Certified Operator** badge
- ✅ Access to on-call rotation
- ✅ Full production system access
- ✅ Listed in [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md) contact list

---

**Congratulations on completing TCPS Operator Training!**

**Next Steps:**
1. Schedule practical exam with team lead
2. Shadow experienced operator for 1 week
3. Begin independent operations under supervision

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-26
**Maintained By:** TCPS Training Team

**Questions?** Contact tcps-training@company.com
