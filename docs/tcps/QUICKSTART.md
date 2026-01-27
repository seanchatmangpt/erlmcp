# TCPS Quick Start Guide

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Purpose**: Get started with TCPS in erlmcp

## Table of Contents

1. [Overview](#overview)
2. [Setup TCPS for New Project](#setup-tcps-for-new-project)
3. [Your First Work Order](#your-first-work-order)
4. [Running the Production Pipeline](#running-the-production-pipeline)
5. [Interpreting Receipts](#interpreting-receipts)
6. [Common Workflows](#common-workflows)
7. [Troubleshooting](#troubleshooting)

---

## Overview

**TCPS** turns your development process into a factory with:
- Pull-based work (JIT)
- Stop-the-line quality (Andon)
- Leveled production (Heijunka)
- Proof of work (Receipts)

**This guide gets you running in 15 minutes.**

---

## Setup TCPS for New Project

### Prerequisites

- Erlang/OTP 26+
- rebar3 3.22+
- Git
- Make

### Step 1: Initialize TCPS Directory Structure

```bash
# Navigate to erlmcp directory
cd /path/to/erlmcp

# Create TCPS directories
make tcps-setup

# This creates:
# - ontology/         (specifications)
# - shapes/           (SHACL constraints)
# - sparql/           (extraction queries)
# - templates/        (code generators)
# - receipts/         (proof artifacts)
# - tools/            (automation scripts)
```

**Expected output**:
```
Creating TCPS directory structure...
✓ Created ontology/
✓ Created shapes/
✓ Created sparql/
✓ Created templates/
✓ Created receipts/pull/
✓ Created receipts/plan/
✓ Created receipts/generate/
✓ Created receipts/build/
✓ Created receipts/test/
✓ Created receipts/release/
✓ Created tools/

TCPS setup complete!
```

---

### Step 2: Configure Heijunka Buckets

```bash
# Edit Heijunka configuration
vi config/heijunka.erl
```

```erlang
% config/heijunka.erl
-module(heijunka).
-export([daily_allocation/0, wip_limit/0]).

% Daily slot allocation per bucket
daily_allocation() ->
    #{
        reliability => 2,  % Bug fixes, stability
        security => 2,     % CVE patches, hardening
        features => 1,     % New capabilities
        compliance => 1,   % Regulatory requirements
        debt => 1          % Refactoring, cleanup
    }.

% WIP limits
wip_limit() ->
    #{
        total => 3,       % Max active work orders
        per_stage => 2    % Max per production stage
    }.
```

---

### Step 3: Initialize Base Ontology

```bash
# Create base ontology
cat > ontology/base.ttl << 'EOF'
@prefix taiea: <http://taiea.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology metadata
<http://taiea.org/ontology> a owl:Ontology ;
    owl:versionInfo "1.0.0" ;
    rdfs:label "TAIEA Production Ontology" ;
    rdfs:comment "Toyota Code Production System ontology for erlmcp" .

# Core classes
taiea:WorkOrder a owl:Class ;
    rdfs:label "Work Order" ;
    rdfs:comment "A unit of demand-driven work" .

taiea:Receipt a owl:Class ;
    rdfs:label "Receipt" ;
    rdfs:comment "Proof of stage completion" .

taiea:AndonEvent a owl:Class ;
    rdfs:label "Andon Event" ;
    rdfs:comment "Stop-the-line quality signal" .
EOF
```

---

### Step 4: Create Base SHACL Shapes

```bash
# Create work order shape
cat > shapes/work_order.ttl << 'EOF'
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix taiea: <http://taiea.org/ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

taiea:WorkOrderShape a sh:NodeShape ;
    sh:targetClass taiea:WorkOrder ;

    # Required: SKU
    sh:property [
        sh:path taiea:requestedSKU ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Work order must have exactly one requested SKU"
    ] ;

    # Required: Priority
    sh:property [
        sh:path taiea:priority ;
        sh:minCount 1 ;
        sh:in ( "critical" "high" "medium" "low" ) ;
        sh:message "Work order must have priority: critical, high, medium, or low"
    ] ;

    # Required: Bucket
    sh:property [
        sh:path taiea:bucket ;
        sh:minCount 1 ;
        sh:in ( "reliability" "security" "features" "compliance" "debt" ) ;
        sh:message "Work order must be in a valid bucket"
    ] ;

    # Required: Due date
    sh:property [
        sh:path taiea:dueDate ;
        sh:minCount 1 ;
        sh:datatype xsd:date ;
        sh:message "Work order must have a due date"
    ] .
EOF
```

---

## Your First Work Order

### Step 1: Identify Demand

```bash
# Example: GitHub issue #123 reports connection leak
# This is a demand signal → create work order

# Run demand collector (or create manually)
./tools/demand-collector analyze-github --issue 123
```

**Output**:
```
Analyzing GitHub issue #123...
Title: Connection leak in TCP transport
Labels: bug, high-priority
👍: 15
Comments: 8

Recommendation:
- Bucket: reliability
- Priority: high
- SKU: erlmcp-tcp-connection-leak-fix
```

---

### Step 2: Create Work Order

```bash
# Create work order from demand signal
./tools/work-order create \
    --sku "erlmcp-tcp-connection-leak-fix" \
    --priority high \
    --bucket reliability \
    --due-date 2026-01-28 \
    --demand-source github-issue-123

# Output: ontology/work_orders.ttl
```

**Generated work order**:
```turtle
@prefix taiea: <http://taiea.org/ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:wo-001 a taiea:WorkOrder ;
    taiea:requestedSKU "erlmcp-tcp-connection-leak-fix" ;
    taiea:priority "high" ;
    taiea:bucket "reliability" ;
    taiea:dueDate "2026-01-28"^^xsd:date ;
    taiea:demandSource :github-issue-123 ;
    taiea:created "2026-01-26T10:00:00Z"^^xsd:dateTime ;
    taiea:status "open" .
```

---

### Step 3: Validate Work Order

```bash
# Validate against SHACL shapes
./tools/shacl-validate \
    --data ontology/work_orders.ttl \
    --shapes shapes/work_order.ttl

# Expected output:
✓ SHACL validation passed
✓ Work order wo-001 is valid
✓ All constraints satisfied
```

---

## Running the Production Pipeline

### Option 1: Full Pipeline (Automatic)

```bash
# Run complete TCPS pipeline
make tcps-build

# This executes all 10 stages:
# 1. Pull (create work orders)
# 2. Plan (Heijunka scheduling)
# 3. Generate (SHACL validation)
# 4. Extract (SPARQL queries)
# 5. Render (template generation)
# 6. Build (compilation)
# 7. Test (test execution)
# 8. Release (artifact creation)
# 9. Publish (distribution)
# 10. Verify (smoke tests)
```

**Expected output**:
```
=== TCPS Production Pipeline ===

[Stage 1/10] Pull...
✓ Demand signals collected
✓ Work order wo-001 created
✓ Receipt: receipts/pull/wo-001.json

[Stage 2/10] Plan...
✓ Heijunka schedule created
✓ WIP limit: 2/3 (OK)
✓ Receipt: receipts/plan/schedule-20260126.json

[Stage 3/10] Generate...
✓ Ontology loaded (1,247 triples)
✓ SHACL validation passed
✓ Receipt: receipts/generate/validate-wo-001.json

... (continues for all 10 stages)

=== Pipeline Complete ===
Status: SUCCESS
Duration: 6.5 hours
Receipts: 10/10
SKU: erlmcp-0.6.0
Ready to ship: YES ✓
```

---

### Option 2: Stage-by-Stage (Manual)

```bash
# Run individual stages

# Stage 1: Pull
./tools/pipeline run-stage pull

# Stage 2: Plan
./tools/pipeline run-stage plan --work-order wo-001

# Stage 3: Generate
./tools/pipeline run-stage generate --work-order wo-001

# Stage 4: Extract
./tools/pipeline run-stage extract --work-order wo-001

# ... continue for all stages
```

---

## Interpreting Receipts

### View Receipt Chain

```bash
# View complete receipt chain for work order
./tools/receipt show-chain --work-order wo-001

# Output:
=== Receipt Chain for wo-001 ===

1. Pull (receipts/pull/wo-001.json)
   Status: completed
   Duration: 1.2s
   ✓ SHACL validation passed

2. Plan (receipts/plan/schedule-20260126.json)
   Status: completed
   Duration: 0.6s
   ✓ WIP limit respected (2/3)

3. Generate (receipts/generate/validate-wo-001.json)
   Status: completed
   Duration: 28.4s
   ✓ 1,247 triples loaded
   ✓ SHACL validation passed

... (continues for all 10 stages)

Chain Status: COMPLETE ✓
Chain Hash: sha256:xyz789...
All Receipts Valid: YES ✓
```

---

### Inspect Individual Receipt

```bash
# View specific receipt
cat receipts/build/compile-wo-001.json | jq

# Output (formatted):
{
  "receipt_id": "build-wo-001-20260126",
  "stage": "build",
  "timestamp": "2026-01-26T10:00:00Z",
  "work_order_id": "wo-001",
  "status": "completed",

  "outputs": {
    "beam_files": 20,
    "total_size_bytes": 456789
  },

  "validation": {
    "passed": true,
    "compilation": {
      "errors": 0,
      "warnings": 0
    }
  },

  "checksums": {
    "algorithm": "sha256",
    "files": {
      "_build/default/lib/erlmcp/ebin/erlmcp_transport_http_gun.beam": "c5d6e7f8..."
    }
  },

  "duration_ms": 12453
}
```

---

### Verify Receipt Integrity

```bash
# Verify receipt checksums
./tools/receipt verify receipts/build/compile-wo-001.json

# Output:
Verifying receipt: build-wo-001-20260126

✓ JSON schema valid
✓ Checksum algorithm: sha256
✓ File checksums verified (20/20)
✓ Chain link valid
✓ Storage location correct

Receipt verification: PASS ✓
```

---

## Common Workflows

### Workflow 1: Emergency Security Patch

```bash
# 1. Create critical security work order
./tools/work-order create \
    --sku "erlmcp-cve-2026-1234-patch" \
    --priority critical \
    --bucket security \
    --due-date 2026-01-27 \
    --slo 24h

# 2. Run expedited pipeline (skip queue)
make tcps-build-expedited

# 3. Verify all quality gates passed
./tools/receipt verify-chain --work-order wo-002

# 4. Deploy immediately
make deploy
```

---

### Workflow 2: Feature Development

```bash
# 1. Create feature work order
./tools/work-order create \
    --sku "erlmcp-websocket-transport" \
    --priority medium \
    --bucket features \
    --due-date 2026-02-15

# 2. Run normal pipeline (Heijunka scheduled)
make tcps-build

# 3. Monitor progress
./tools/wip status

# Output:
Current WIP: 2/3
Active Work Orders:
1. wo-001 (Stage: test, Age: 2.3 hours)
2. wo-003 (Stage: build, Age: 1.1 hours)

Waiting in Queue:
1. wo-004 (Priority: medium, Bucket: features)
```

---

### Workflow 3: Handle Andon Event

```bash
# Andon triggered automatically on test failure
# Output:
🚨 ANDON TRIGGERED: andon-20260126-001
Severity: CRITICAL
Reason: test_failure
Work Order: wo-001
SKU: erlmcp-0.6.0-tcp-fix (QUARANTINED)

# 1. Acknowledge Andon
./tools/andon acknowledge andon-20260126-001 \
    --assigned-to "you@example.com"

# 2. Run 5 Whys analysis
./tools/andon create-5whys andon-20260126-001
# (Opens editor with template)

# 3. Fix the bug
vi src/erlmcp_transport_tcp.erl

# 4. Add regression test
vi test/erlmcp_transport_tcp_SUITE.erl

# 5. Re-run pipeline
make tcps-build

# 6. Resolve Andon
./tools/andon resolve andon-20260126-001 \
    --5whys-id 5w-20260126-001 \
    --fix-verified true

# Output:
✓ Andon resolved
✓ Quarantine removed
✓ Production line resumed
```

---

## Troubleshooting

### Issue 1: SHACL Validation Fails

**Symptom**:
```
✗ SHACL validation failed
Violation: Work order missing required field: taiea:dueDate
```

**Solution**:
```bash
# Add missing field to work order
./tools/work-order update wo-001 --due-date 2026-01-28

# Re-run validation
./tools/shacl-validate --data ontology/work_orders.ttl
```

---

### Issue 2: WIP Limit Exceeded

**Symptom**:
```
✗ Cannot accept work order: WIP limit exceeded (3/3)
```

**Solution**:
```bash
# Check current WIP
./tools/wip status

# Wait for slot to free, or
# Complete an existing work order, or
# Increase WIP limit (not recommended)
```

---

### Issue 3: Receipt Chain Incomplete

**Symptom**:
```
✗ Receipt chain incomplete
Missing receipt: receipts/test/results-wo-001.json
```

**Solution**:
```bash
# Re-run missing stage
./tools/pipeline run-stage test --work-order wo-001

# Verify chain
./tools/receipt verify-chain --work-order wo-001
```

---

### Issue 4: Build Not Deterministic

**Symptom**:
```
✗ Build not deterministic
Checksum mismatch on build 2
```

**Solution**:
```bash
# Find source of randomness
./tools/build analyze-determinism

# Common causes:
# - Timestamps in generated files
# - Random test data
# - Dependency resolution order

# Fix and rebuild
make clean && make tcps-build
```

---

## Quick Reference

### Essential Commands

```bash
# Work Orders
./tools/work-order create --sku <name> --bucket <type> --priority <level>
./tools/work-order list
./tools/work-order update <wo-id> --field <value>

# Pipeline
make tcps-build              # Full pipeline
./tools/pipeline run-stage <stage>  # Single stage
./tools/pipeline status      # Current status

# Receipts
./tools/receipt show-chain --work-order <wo-id>
./tools/receipt verify <receipt-file>
./tools/receipt verify-chain --work-order <wo-id>

# WIP
./tools/wip status
./tools/wip count

# Andon
./tools/andon trigger --stage <stage> --reason <code>
./tools/andon acknowledge <andon-id>
./tools/andon resolve <andon-id>

# Heijunka
./tools/heijunka schedule --days <n>
./tools/heijunka balance-check
```

---

## Next Steps

Now that you've completed the quickstart:

1. **Read the full guide**: [TCPS.md](TCPS.md)
2. **Understand standard work**: [STANDARD_WORK.md](STANDARD_WORK.md)
3. **Learn quality gates**: [DEFINITION_OF_DONE.md](DEFINITION_OF_DONE.md)
4. **Master Andon response**: [ANDON_RUNBOOK.md](ANDON_RUNBOOK.md)
5. **Start improving**: [KAIZEN_GUIDE.md](KAIZEN_GUIDE.md)

---

## Conclusion

You're now running TCPS!

**Remember**:
- Pull work, don't push
- Stop the line on defects
- Prove every stage with receipts
- Improve continuously

**Welcome to the factory.**

---

**See Also**:
- [TCPS.md](TCPS.md) - Complete TCPS guide
- [STANDARD_WORK.md](STANDARD_WORK.md) - Detailed procedures
- [HEIJUNKA_SCHEDULING.md](HEIJUNKA_SCHEDULING.md) - Work leveling
- [RECEIPTS_SPEC.md](RECEIPTS_SPEC.md) - Receipt schemas
