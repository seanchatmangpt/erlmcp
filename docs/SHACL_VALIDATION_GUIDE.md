# TCPS SHACL Validation Guide

## Overview

This guide documents the SHACL (Shapes Constraint Language) validation shapes for the Toyota Code Production System (TCPS). These shapes enforce zero-defect quality standards at every stage of the production pipeline.

**Authority**: TCPS Definition of Done (docs/TCPS.md, docs/TCPS-checklist.md)
**Quality Standard**: Lean Six Sigma (99.99966% defect-free)
**Action on Violation**: Stop-the-line (Andon event), quarantine SKU, block publish

## Quick Start

### Validate TCPS Data

```bash
# Validate with test data (recommended for first-time setup)
python tests/shacl/test_tcps_validation.py

# Validate specific data file
python tests/shacl/test_tcps_validation.py /path/to/your/data.ttl

# Test with valid data only
python tests/shacl/test_tcps_validation.py --test-valid

# Test with invalid data (should fail)
python tests/shacl/test_tcps_validation.py --test-invalid
```

### Run Pytest Test Suite

```bash
# Install dependencies
pip install pyshacl rdflib pytest

# Run all SHACL validation tests
pytest tests/shacl/test_tcps_validation.py -v

# Run specific test
pytest tests/shacl/test_tcps_validation.py::TestTCPSSHACLValidation::test_valid_data_passes_all_validations -v
```

## SHACL Shapes Architecture

### Files Structure

```
shapes/
  └── tcps_shapes.ttl          # Main SHACL validation shapes

tests/shacl/
  ├── test_tcps_validation.py  # Python test suite
  ├── test_data_valid.ttl      # Valid test data (should pass)
  └── test_data_invalid.ttl    # Invalid test data (should fail)

docs/
  └── SHACL_VALIDATION_GUIDE.md  # This file
```

## Validation Categories

### 1. SKU Completeness Validation

**Purpose**: Ensure every SKU is complete, traceable, and quality-gated before shipping.

**Required Properties**:
- `tcps:name` - Non-empty string (exactly one)
- `tcps:version` - Semantic version matching `X.Y.Z` or `X.Y.Z-suffix` (exactly one)
- `tcps:description` - Minimum 10 characters (exactly one)
- `tcps:workOrderId` - Reference to WorkOrder IRI (exactly one)
- `tcps:hasReceipt` - At least one Receipt (proof of work)
- `tcps:qualityGatesPassed` - Must be `true` (exactly one)
- `tcps:shippable` - Boolean status (exactly one)
- `tcps:createdAt` - ISO 8601 timestamp (exactly one)

**Special Constraints**:
- Cannot ship SKU with open Andon events (stop-the-line rule)
- Receipt timestamps must be after SKU creation time

**Example Valid SKU**:
```turtle
<http://taiea.io/sku/sku-001>
    a tcps:SKU ;
    tcps:name "erlmcp-server" ;
    tcps:version "0.6.0" ;
    tcps:description "Enterprise MCP server implementation with TCPS quality gates" ;
    tcps:workOrderId <http://taiea.io/workorder/wo-001> ;
    tcps:hasReceipt <http://taiea.io/receipt/compile-001> ;
    tcps:hasReceipt <http://taiea.io/receipt/test-001> ;
    tcps:hasReceipt <http://taiea.io/receipt/release-001> ;
    tcps:hasReceipt <http://taiea.io/receipt/publish-001> ;
    tcps:qualityGatesPassed true ;
    tcps:shippable true ;
    tcps:createdAt "2026-01-26T10:05:00Z"^^xsd:dateTime .
```

### 2. WorkOrder Validation

**Purpose**: Ensure work orders have valid demand signals, buckets, priorities, and status.

**Required Properties**:
- `tcps:demandSignal` - Non-empty string (exactly one)
- `tcps:bucket` - One of: `reliability`, `security`, `cost`, `compliance`
- `tcps:priority` - Integer 1-10 (1 = highest, 10 = lowest)
- `tcps:status` - One of: `pending`, `in_progress`, `completed`, `failed`
- `tcps:createdAt` - ISO 8601 timestamp (exactly one)
- `tcps:assignedTo` - Agent/team identifier string (optional)

**Special Constraints**:
- WorkOrder marked `completed` cannot have failed receipts

**Example Valid WorkOrder**:
```turtle
<http://taiea.io/workorder/wo-001>
    a tcps:WorkOrder ;
    tcps:demandSignal "Marketplace install spike detected for MCP server feature" ;
    tcps:bucket "reliability" ;
    tcps:priority 2 ;
    tcps:status "completed" ;
    tcps:createdAt "2026-01-26T10:00:00Z"^^xsd:dateTime ;
    tcps:assignedTo "taiea-agent-01" .
```

### 3. Receipt Requirements Validation

**Purpose**: Ensure every stage produces evidence (receipts) with proper status and data.

**Required Properties**:
- `tcps:stageName` - One of: `pull`, `plan`, `generate`, `template`, `compile`, `test`, `release`, `publish`, `verify`, `andon`
- `tcps:timestamp` - ISO 8601 timestamp (exactly one)
- `tcps:status` - One of: `pass`, `fail`
- `tcps:evidenceData` - Non-empty string (proof of work)
- `tcps:relatedSKU` - Reference to SKU (exactly one)
- `tcps:executor` - Agent/system identifier string (optional)

**Example Valid Receipt**:
```turtle
<http://taiea.io/receipt/test-001>
    a tcps:Receipt ;
    tcps:stageName "test" ;
    tcps:timestamp "2026-01-26T10:15:00Z"^^xsd:dateTime ;
    tcps:status "pass" ;
    tcps:evidenceData "All test suites passed with 85% coverage" ;
    tcps:relatedSKU <http://taiea.io/sku/sku-001> ;
    tcps:executor "ct-test-runner" .
```

### 4. Production Stage Gates Validation

**Purpose**: Enforce quality thresholds at each production stage.

#### 4.1 Compilation Stage Gate

**Required Properties**:
- `tcps:buildOutput` - Build output string (minimum 1 character)
- `tcps:errorCount` - Must be `0` (zero tolerance)
- `tcps:warningCount` - Non-negative integer (optional)

**Example**:
```turtle
<http://taiea.io/receipt/compile-001>
    a tcps:Receipt ;
    tcps:stageName "compile" ;
    tcps:buildOutput "Compiled 42 modules, 0 errors, 0 warnings" ;
    tcps:errorCount 0 ;
    tcps:warningCount 0 .
```

#### 4.2 Test Stage Gate

**Required Properties**:
- `tcps:testResults` - Test results string (minimum 1 character)
- `tcps:passRate` - Decimal 80.0-100.0 (minimum 80% quality gate)
- `tcps:coverage` - Decimal 80.0-100.0 (minimum 80% quality gate)
- `tcps:totalTests` - Integer >= 1
- `tcps:failedTests` - Non-negative integer (optional)

**Example**:
```turtle
<http://taiea.io/receipt/test-001>
    a tcps:Receipt ;
    tcps:stageName "test" ;
    tcps:testResults "156 tests passed, 0 failed, 0 skipped" ;
    tcps:passRate 100.0 ;
    tcps:coverage 85.5 ;
    tcps:totalTests 156 ;
    tcps:failedTests 0 .
```

#### 4.3 Release Stage Gate

**Required Properties**:
- `tcps:artifactPath` - Valid URI with scheme: `file://`, `http://`, `https://`, `gs://`, `s3://`
- `tcps:deterministicHash` - SHA-256 hash (64 hex characters)
- `tcps:artifactSize` - Positive integer (bytes) (optional)
- `tcps:releaseNotes` - Release notes string (optional)

**Example**:
```turtle
<http://taiea.io/receipt/release-001>
    a tcps:Receipt ;
    tcps:stageName "release" ;
    tcps:artifactPath "gs://taiea-releases/erlmcp-server-0.6.0.tar.gz" ;
    tcps:deterministicHash "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2" ;
    tcps:artifactSize 15728640 ;
    tcps:releaseNotes "Production release with full TCPS compliance" .
```

#### 4.4 Publish Stage Gate

**Required Properties**:
- `tcps:marketplaceUrl` - Valid HTTP/HTTPS URL
- `tcps:smokeTestResult` - Must be `"pass"`
- `tcps:entitlementGateActive` - Must be `true`
- `tcps:healthCheckPassed` - Boolean (optional)
- `tcps:pubsubVerified` - Boolean (optional)

**Example**:
```turtle
<http://taiea.io/receipt/publish-001>
    a tcps:Receipt ;
    tcps:stageName "publish" ;
    tcps:marketplaceUrl "https://marketplace.taiea.io/servers/erlmcp-server"^^xsd:anyURI ;
    tcps:smokeTestResult "pass" ;
    tcps:entitlementGateActive true ;
    tcps:healthCheckPassed true ;
    tcps:pubsubVerified true .
```

### 5. Andon Events Validation

**Purpose**: Document failures with actionable root cause analysis and preventive actions.

**Required Properties**:
- `tcps:failureReason` - Minimum 10 characters (exactly one)
- `tcps:affectedSKU` - At least one SKU reference
- `tcps:rootCauseAnalysis` - Minimum 20 characters (5 Whys) (exactly one)
- `tcps:status` - One of: `open`, `investigating`, `resolved`, `closed`
- `tcps:createdAt` - ISO 8601 timestamp (exactly one)
- `tcps:resolvedAt` - ISO 8601 timestamp (required if status is `resolved` or `closed`)
- `tcps:preventiveAction` - Minimum 10 characters (required if status is `resolved` or `closed`)
- `tcps:severity` - One of: `critical`, `high`, `medium`, `low`

**Special Constraints**:
- Resolved/closed Andon events MUST have `resolvedAt` timestamp
- Resolved/closed Andon events MUST document `preventiveAction`

**Example Valid Resolved Andon**:
```turtle
<http://taiea.io/andon/andon-001>
    a tcps:AndonEvent ;
    tcps:failureReason "Initial test coverage was 75%, below 80% threshold" ;
    tcps:affectedSKU <http://taiea.io/sku/sku-001> ;
    tcps:rootCauseAnalysis "Missing edge case tests for transport layer error handling. Added comprehensive failure mode tests." ;
    tcps:status "resolved" ;
    tcps:createdAt "2026-01-26T09:00:00Z"^^xsd:dateTime ;
    tcps:resolvedAt "2026-01-26T10:00:00Z"^^xsd:dateTime ;
    tcps:preventiveAction "Added test templates for transport error scenarios to prevent future coverage gaps" ;
    tcps:severity "high" .
```

## Cross-Cutting Constraints

### Timestamp Chronological Order

Receipt timestamps cannot be earlier than SKU creation time.

**SPARQL Validation**:
```sparql
PREFIX tcps: <http://taiea.io/ontology/tcps#>
SELECT $this
WHERE {
    $this a tcps:SKU .
    $this tcps:createdAt ?skuTime .
    $this tcps:hasReceipt ?receipt .
    ?receipt tcps:timestamp ?receiptTime .
    FILTER(?receiptTime < ?skuTime)
}
```

### WorkOrder Completion Integrity

WorkOrder cannot be marked `completed` if any associated receipts have status `fail`.

**SPARQL Validation**:
```sparql
PREFIX tcps: <http://taiea.io/ontology/tcps#>
SELECT $this
WHERE {
    $this a tcps:WorkOrder .
    $this tcps:status "completed" .
    ?sku tcps:workOrderId $this .
    ?sku tcps:hasReceipt ?receipt .
    ?receipt tcps:status "fail" .
}
```

## Validation Severity

All constraints use `sh:Violation` severity, which means:
- **Violations trigger stop-the-line (Andon)**
- **SKU is quarantined and cannot be published**
- **Root cause analysis required**
- **Preventive action required before resolution**

## Integration with TCPS Pipeline

### 1. Pre-Build Validation

```bash
# Validate ontology before starting build
python tests/shacl/test_tcps_validation.py ontology/work_orders.ttl
```

### 2. Post-Stage Validation

After each TCPS stage (compile, test, release, publish), validate receipts:

```bash
# Validate after test stage
python tests/shacl/test_tcps_validation.py receipts/test/sku-001.ttl
```

### 3. Pre-Publish Gate

Before publishing to marketplace:

```bash
# Final validation before publish
python tests/shacl/test_tcps_validation.py receipts/publish/sku-001.ttl

# If validation fails, emit Andon event and block publish
```

### 4. CI/CD Integration

Add to `.github/workflows/` or CI pipeline:

```yaml
- name: Validate TCPS Receipts
  run: |
    pip install pyshacl rdflib
    python tests/shacl/test_tcps_validation.py receipts/

- name: Run SHACL Tests
  run: pytest tests/shacl/test_tcps_validation.py -v --tb=short
```

## Common Validation Errors

### Error: SKU Missing Required Fields

**Message**: `SKU must have exactly one non-empty name`

**Fix**: Add required properties to SKU:
```turtle
tcps:name "your-sku-name" ;
tcps:version "1.0.0" ;
tcps:description "Description with at least 10 characters" ;
```

### Error: Test Coverage Below Threshold

**Message**: `Test coverage must be >= 80% (minimum quality gate)`

**Fix**: Increase test coverage to meet 80% threshold:
```erlang
%% Add more test cases to cover untested code paths
test_coverage_improvement_test() ->
    % Add comprehensive tests for edge cases
    ok.
```

### Error: Compilation Errors Present

**Message**: `Compilation must have zero errors (tcps:errorCount = 0)`

**Fix**: Resolve all compilation errors before creating receipt:
```bash
rebar3 compile  # Must succeed with 0 errors
```

### Error: SKU with Open Andon Event

**Message**: `SKU cannot be shipped with open Andon events - must resolve failures first`

**Fix**: Resolve Andon event and update status:
```turtle
<http://taiea.io/andon/andon-001>
    tcps:status "resolved" ;
    tcps:resolvedAt "2026-01-26T10:00:00Z"^^xsd:dateTime ;
    tcps:preventiveAction "Added preventive measures to prevent recurrence" .
```

## Best Practices

### 1. Validate Early and Often

Run SHACL validation after each production stage, not just at the end.

### 2. Use Test Data for Development

Use `test_data_valid.ttl` and `test_data_invalid.ttl` as templates for your own data.

### 3. Document Root Cause Analysis

Andon events require detailed root cause analysis (minimum 20 characters). Use 5 Whys methodology.

### 4. Maintain Chronological Consistency

Ensure timestamps are in correct order:
- WorkOrder created → SKU created → Receipts created → Andon resolved

### 5. Zero Tolerance for Quality Gates

Do not lower thresholds. Fix the code/tests to meet 80% coverage and 0 errors.

## Troubleshooting

### pyshacl Not Installed

```bash
pip install pyshacl rdflib
```

### Invalid Turtle Syntax

Use a Turtle validator:
```bash
rapper -i turtle -c your-data.ttl
```

### SPARQL Constraints Not Working

Ensure `inference='rdfs'` is enabled in validation call:
```python
validate(data_graph, shacl_graph, inference='rdfs')
```

## References

- **TCPS Definition**: [docs/TCPS.md](TCPS.md)
- **TCPS Checklist**: [docs/TCPS-checklist.md](TCPS-checklist.md)
- **TCPS Certification**: [docs/TCPS-certification.md](TCPS-certification.md)
- **SHACL Specification**: https://www.w3.org/TR/shacl/
- **pyshacl Documentation**: https://github.com/RDFLib/pySHACL

## License

Same as erlmcp project (see LICENSE file).

## Support

For issues with SHACL validation:
1. Check validation error messages for specific constraint violations
2. Review test data examples in `tests/shacl/`
3. Consult TCPS documentation for quality gate requirements
4. Create Andon event for systematic failures

---

**Quality Standard**: Lean Six Sigma (99.99966% defect-free)
**Action on Violation**: Stop-the-line, quarantine SKU, block publish
**Authority**: TCPS Definition of Done
