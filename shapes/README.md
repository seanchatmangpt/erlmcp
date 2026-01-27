# TCPS SHACL Validation Shapes

## Overview

This directory contains SHACL (Shapes Constraint Language) validation shapes for the Toyota Code Production System (TCPS). These shapes enforce zero-defect quality standards across the entire production pipeline.

**Quality Standard**: Lean Six Sigma (99.99966% defect-free)
**Validation Authority**: TCPS Definition of Done (docs/TCPS.md)
**Action on Violation**: Stop-the-line (Andon event), quarantine SKU, block publish

## Files

- **`tcps_shapes.ttl`** - Complete SHACL validation shapes for TCPS quality gates

## What This Validates

### 1. SKU Completeness
- Every SKU must have: name, version, description, work_order_id
- SKU must reference at least one Receipt (proof of work)
- SKU must pass all quality gates
- Cannot ship SKU with open Andon events

### 2. WorkOrder Validation
- Must have: demand_signal, bucket (reliability/security/cost/compliance), priority (1-10)
- Status must be: pending, in_progress, completed, or failed
- Completed WorkOrders cannot have failed receipts

### 3. Receipt Requirements
- Must have: stage_name (from TCPS pipeline), timestamp, status (pass/fail), evidence_data
- Evidence data must be non-empty (proof of work)
- Timestamp must be valid ISO 8601
- Must reference exactly one SKU

### 4. Production Stage Gates

#### Compilation Stage
- Must have: build_output, error_count = 0
- Zero tolerance for compilation errors

#### Test Stage
- Must have: test_results, pass_rate >= 80%, coverage >= 80%
- Minimum quality gate: 80% pass rate and 80% coverage

#### Release Stage
- Must have: artifact_path (valid URI), deterministic_hash (SHA-256)
- Artifact path must use valid URI scheme (file://, https://, gs://, s3://)

#### Publish Stage
- Must have: marketplace_url, smoke_test_result = pass, entitlement_gate_active = true
- Cannot publish without passing smoke tests

### 5. Andon Events
- Must have: failure_reason (min 10 chars), affected_sku, root_cause_analysis (min 20 chars, 5 Whys)
- Status: open, investigating, resolved, or closed
- Resolved/closed events must have: resolved_at timestamp, preventive_action (min 10 chars)
- Cannot publish SKU with open Andon events

## Quick Start

### Validate TCPS Data

```bash
# Install dependencies
pip install pyshacl rdflib

# Validate data file
python tests/shacl/test_tcps_validation.py /path/to/your/data.ttl

# Run validation tests
pytest tests/shacl/test_tcps_validation.py -v
```

### Example Usage in Python

```python
from pyshacl import validate
from rdflib import Graph

# Load shapes
shapes_graph = Graph()
shapes_graph.parse("shapes/tcps_shapes.ttl", format="turtle")

# Load your data
data_graph = Graph()
data_graph.parse("your-data.ttl", format="turtle")

# Validate
conforms, results_graph, results_text = validate(
    data_graph=data_graph,
    shacl_graph=shapes_graph,
    inference='rdfs'
)

if conforms:
    print("✓ VALIDATION PASSED - All quality gates satisfied")
else:
    print("✗ VALIDATION FAILED - Stop the line!")
    print(results_text)
```

## Validation Categories

| Category | Shapes | Severity | Action on Violation |
|----------|--------|----------|---------------------|
| SKU Completeness | tcps:SKUShape, tcps:SKUNoOpenAndonShape | sh:Violation | Stop-the-line, quarantine SKU |
| WorkOrder | tcps:WorkOrderShape, tcps:WorkOrderCompletionShape | sh:Violation | Block WorkOrder completion |
| Receipt | tcps:ReceiptShape | sh:Violation | Reject invalid receipt |
| Compilation Gate | tcps:CompilationReceiptShape | sh:Violation | Block build |
| Test Gate | tcps:TestReceiptShape | sh:Violation | Block release (coverage < 80%) |
| Release Gate | tcps:ReleaseReceiptShape | sh:Violation | Block publish |
| Publish Gate | tcps:PublishReceiptShape | sh:Violation | Block marketplace publish |
| Andon Events | tcps:AndonEventShape, tcps:AndonResolvedRequirementsShape | sh:Violation | Block SKU shipment |
| Cross-Cutting | tcps:ReceiptTimestampOrderShape | sh:Violation | Data integrity violation |

## Quality Gates

### Zero-Defect Thresholds

| Stage | Metric | Threshold | Enforcement |
|-------|--------|-----------|-------------|
| Compilation | Error Count | 0 errors | MANDATORY |
| Test | Pass Rate | >= 80% | MANDATORY |
| Test | Coverage | >= 80% | MANDATORY |
| Release | Hash Format | SHA-256 (64 hex chars) | MANDATORY |
| Publish | Smoke Test | Pass | MANDATORY |
| Publish | Entitlement Gate | Active (true) | MANDATORY |

**NO EXCEPTIONS**: These thresholds cannot be lowered. Fix the code/tests to meet standards.

## Integration with TCPS Pipeline

### Pre-Build Validation

```bash
# Validate work orders before starting production
python tests/shacl/test_tcps_validation.py ontology/work_orders.ttl
```

### Post-Stage Validation

```bash
# After each stage (compile, test, release, publish)
python tests/shacl/test_tcps_validation.py receipts/<stage>/sku-001.ttl
```

### Pre-Publish Gate

```bash
# Final validation before marketplace publish
python tests/shacl/test_tcps_validation.py receipts/publish/sku-001.ttl

# If validation fails:
# 1. Emit Andon event
# 2. Quarantine SKU
# 3. Block publish
# 4. Perform root cause analysis (5 Whys)
# 5. Document preventive action
```

### CI/CD Integration

Add to GitHub Actions or CI pipeline:

```yaml
- name: TCPS SHACL Validation
  run: |
    pip install pyshacl rdflib pytest
    pytest tests/shacl/test_tcps_validation.py -v

- name: Validate Receipts
  run: |
    python tests/shacl/test_tcps_validation.py receipts/
```

## Test Data

See `tests/shacl/` for comprehensive test data:

- **`test_data_valid.ttl`** - Valid TCPS data (should pass all validations)
- **`test_data_invalid.ttl`** - Invalid TCPS data (should fail validations)
- **`test_tcps_validation.py`** - Complete test suite with 30+ test cases

## Common Validation Errors

| Error | Cause | Fix |
|-------|-------|-----|
| "SKU must have exactly one non-empty name" | Missing tcps:name | Add tcps:name property |
| "Test coverage must be >= 80%" | Coverage below threshold | Increase test coverage |
| "Compilation must have zero errors" | errorCount > 0 | Fix compilation errors |
| "SKU cannot be shipped with open Andon events" | Open Andon exists | Resolve Andon event first |
| "Receipt timestamp cannot be earlier than SKU creation" | Timestamp inconsistency | Fix timestamps chronologically |

## Documentation

For detailed documentation, see:
- **[SHACL Validation Guide](../docs/SHACL_VALIDATION_GUIDE.md)** - Complete guide with examples
- **[TCPS Definition](../docs/TCPS.md)** - Toyota Code Production System overview
- **[TCPS Checklist](../docs/TCPS-checklist.md)** - Validation checklist
- **[TCPS Certification](../docs/TCPS-certification.md)** - Certification program

## Quality Standard

These SHACL shapes enforce **Lean Six Sigma** standards:
- **99.99966% defect-free delivery**
- **Zero tolerance for quality gate violations**
- **Stop-the-line authority on any violation**
- **Mandatory root cause analysis for failures**
- **Preventive action required before resolution**

## License

Same as erlmcp project (see LICENSE file).

## Support

For validation issues:
1. Check error messages for specific constraint violations
2. Review test data examples in `tests/shacl/`
3. Consult SHACL Validation Guide
4. Create Andon event for systematic failures

---

**Remember**: If validation fails, STOP THE LINE. Do not ship until all quality gates pass.
