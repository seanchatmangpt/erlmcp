# TCPS Receipt Specification

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Purpose**: Define receipt schemas and storage conventions

## Table of Contents

1. [Overview](#overview)
2. [Receipt JSON Schema](#receipt-json-schema)
3. [Required Fields Per Stage](#required-fields-per-stage)
4. [Storage Location Conventions](#storage-location-conventions)
5. [Linking to Ontology](#linking-to-ontology)
6. [Receipt Verification](#receipt-verification)

---

## Overview

**Receipts** are cryptographically-verifiable proof that a stage completed successfully.

### Core Principle

> **"No receipt = didn't happen. If you can't prove it, you can't ship it."**

### Receipt Requirements

Every receipt MUST have:
1. **Unique ID**: Globally unique identifier
2. **Timestamp**: When the stage completed
3. **Stage**: Which production stage
4. **Work Order Link**: Traceability to demand
5. **Inputs**: What was consumed
6. **Outputs**: What was produced
7. **Validation**: Proof of correctness
8. **Checksums**: Cryptographic hashes
9. **Chain Link**: Previous receipt ID

---

## Receipt JSON Schema

### Base Receipt Schema

All receipts conform to this base schema with required fields:

**Required Fields**:
- `receipt_id` (string): Unique identifier pattern: `<stage>-<wo-id>-<YYYYMMDD>`
- `receipt_version` (string): Schema version (e.g., "1.0.0")
- `stage` (enum): One of: pull, plan, generate, extract, render, build, test, release, publish, verify
- `timestamp` (string): ISO 8601 date-time
- `work_order_id` (string): Work order identifier (e.g., "wo-001")
- `status` (enum): "completed" or "failed"
- `inputs` (object): Stage-specific input data
- `outputs` (object): Stage-specific output data
- `validation` (object): Validation results with `passed` boolean
- `checksums` (object): Cryptographic hashes
- `duration_ms` (integer): Execution time in milliseconds

**Optional Fields**:
- `previous_receipt` (string or null): Previous stage receipt ID
- `next_stage` (string or null): Next stage name
- `errors` (array): Error details if status = failed
- `metadata` (object): Execution metadata

---

## Required Fields Per Stage

### Stage 1: Pull Receipt

Example receipt for work order creation:

```json
{
  "receipt_id": "pull-wo-001-20260126",
  "receipt_version": "1.0.0",
  "stage": "pull",
  "timestamp": "2026-01-26T08:00:00Z",
  "work_order_id": "wo-001",
  "status": "completed",
  "previous_receipt": null,
  "next_stage": "plan",
  "inputs": {
    "demand_signals": ["marketplace-install-spike"],
    "demand_source_file": "demand_signals.json"
  },
  "outputs": {
    "work_orders": ["ontology/work_orders.ttl#wo-001"],
    "work_order_count": 1
  },
  "validation": {
    "passed": true,
    "shacl_validation": {
      "shapes_file": "shapes/work_order.ttl",
      "violations": 0
    }
  },
  "checksums": {
    "algorithm": "sha256",
    "files": {
      "ontology/work_orders.ttl": "a3f8b9c2..."
    }
  },
  "duration_ms": 1234,
  "metadata": {
    "executor": "demand-collector",
    "environment": "production"
  }
}
```

---

### Stage 6: Build Receipt

Example receipt for compilation:

```json
{
  "receipt_id": "build-wo-001-20260126",
  "receipt_version": "1.0.0",
  "stage": "build",
  "timestamp": "2026-01-26T10:00:00Z",
  "work_order_id": "wo-001",
  "status": "completed",
  "previous_receipt": "render-wo-001-20260126",
  "next_stage": "test",
  "inputs": {
    "source_files": [
      "src/erlmcp_transport_http_gun.erl",
      "src/erlmcp_server.erl"
    ],
    "dependencies": "rebar.lock",
    "build_config": "rebar.config"
  },
  "outputs": {
    "beam_files": [
      "_build/default/lib/erlmcp/ebin/erlmcp_transport_http_gun.beam"
    ],
    "beam_count": 20,
    "total_size_bytes": 456789
  },
  "validation": {
    "passed": true,
    "compilation": {
      "errors": 0,
      "warnings": 0,
      "exit_code": 0,
      "compiler_version": "OTP 26.2"
    },
    "determinism": {
      "builds_compared": 3,
      "checksums_match": true
    }
  },
  "checksums": {
    "algorithm": "sha256",
    "files": {
      "_build/default/lib/erlmcp/ebin/erlmcp_transport_http_gun.beam": "c5d6e7f8..."
    }
  },
  "duration_ms": 12453,
  "metadata": {
    "executor": "rebar3",
    "environment": "ci"
  }
}
```

---

### Stage 7: Test Receipt

Example receipt for test execution:

```json
{
  "receipt_id": "test-wo-001-20260126",
  "receipt_version": "1.0.0",
  "stage": "test",
  "timestamp": "2026-01-26T10:30:00Z",
  "work_order_id": "wo-001",
  "status": "completed",
  "previous_receipt": "build-wo-001-20260126",
  "next_stage": "release",
  "inputs": {
    "beam_files": "_build/default/lib/erlmcp/ebin/*.beam",
    "test_suites": ["test/*_SUITE.erl"]
  },
  "outputs": {
    "test_results": {
      "unit_tests": {
        "total": 47,
        "passed": 47,
        "failed": 0
      },
      "integration_tests": {
        "total": 89,
        "passed": 89,
        "failed": 0
      }
    },
    "coverage": {
      "lines": 87.3,
      "functions": 92.1,
      "report": "_build/test/cover/index.html"
    }
  },
  "validation": {
    "passed": true,
    "all_tests_passed": true,
    "coverage_threshold_met": true
  },
  "checksums": {
    "algorithm": "sha256",
    "files": {
      "receipts/test/results-wo-001.xml": "e8f9a0b1..."
    }
  },
  "duration_ms": 49099
}
```

---

## Storage Location Conventions

### Directory Structure

```
receipts/
├── pull/          # Work order creation receipts
├── plan/          # Heijunka schedule receipts
├── generate/      # SHACL validation receipts
├── extract/       # SPARQL query receipts
├── render/        # Template rendering receipts
├── build/         # Compilation receipts
├── test/          # Test execution receipts
├── release/       # Release artifact receipts
├── publish/       # Publication receipts
├── verify/        # Verification receipts
├── andon/         # Andon event receipts
└── chain/         # Receipt chain aggregations
```

### Naming Conventions

| Stage | Pattern | Example |
|-------|---------|---------|
| Pull | `<wo-id>.json` | `wo-001.json` |
| Plan | `schedule-<YYYYMMDD>.json` | `schedule-20260126.json` |
| Build | `compile-<wo-id>.json` | `compile-wo-001.json` |
| Test | `results-<wo-id>.json` | `results-wo-001.json` |
| Release | `<sku>-<version>.json` | `erlmcp-0.6.0.json` |
| Andon | `andon-<YYYYMMDD>-<seq>.json` | `andon-20260126-001.json` |

---

## Linking to Ontology

### Receipt as RDF

Receipts can be converted to RDF for ontology storage:

```turtle
@prefix taiea: <http://taiea.org/ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:receipt-build-wo-001-20260126 a taiea:Receipt ;
    taiea:receiptId "build-wo-001-20260126" ;
    taiea:stage "build" ;
    taiea:timestamp "2026-01-26T10:00:00Z"^^xsd:dateTime ;
    taiea:workOrder :wo-001 ;
    taiea:status "completed" ;
    taiea:previousReceipt :receipt-render-wo-001-20260126 ;
    taiea:durationMs 12453 .
```

### Querying Receipts

```sparql
# Find all receipts for a work order
PREFIX taiea: <http://taiea.org/ontology#>

SELECT ?receipt ?stage ?timestamp ?status
WHERE {
  ?receipt a taiea:Receipt ;
           taiea:workOrder :wo-001 ;
           taiea:stage ?stage ;
           taiea:timestamp ?timestamp ;
           taiea:status ?status .
}
ORDER BY ?timestamp
```

---

## Receipt Verification

### Verification Steps

1. **Validate JSON schema**: Check all required fields present
2. **Verify checksums**: Recalculate and compare file hashes
3. **Verify chain link**: Ensure previous receipt exists
4. **Verify storage location**: Receipt in correct directory
5. **Verify timestamps**: Chronological order maintained

### Verification Tool

```bash
# Verify receipt integrity
./tools/receipt verify-chain --work-order wo-001

# Expected output:
Verifying receipt chain for wo-001...
✓ Stage 1 (pull): receipts/pull/wo-001.json
✓ Stage 2 (plan): receipts/plan/schedule-20260126.json
✓ Stage 3 (generate): receipts/generate/validate-wo-001.json
...
✓ Stage 10 (verify): receipts/verify/erlmcp-0.6.0.json

Chain hash: sha256:xyz789...
Chain verification: PASS
```

---

## Conclusion

Receipts are the proof that TCPS works.

Every stage. Every artifact. Every SKU.

**No receipt = No proof = No ship.**

---

**See Also**:
- [TCPS.md](TCPS.md) - Complete TCPS guide
- [STANDARD_WORK.md](STANDARD_WORK.md) - Stage procedures
- [DEFINITION_OF_DONE.md](DEFINITION_OF_DONE.md) - Quality gates
