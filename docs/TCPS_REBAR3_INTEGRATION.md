# TCPS Rebar3 Integration Guide

## Toyota Code Production System for Erlang Builds

This document provides complete instructions for using TCPS (Toyota Code Production System) integrated into the rebar3 build system for zero-defect software delivery.

---

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Build Pipeline Integration](#build-pipeline-integration)
5. [Provider Commands](#provider-commands)
6. [Quality Gates](#quality-gates)
7. [Andon System](#andon-system)
8. [Receipt System](#receipt-system)
9. [Advanced Usage](#advanced-usage)
10. [Troubleshooting](#troubleshooting)

---

## Overview

TCPS integrates Toyota Production System principles into rebar3 builds:

### Core Principles

- **Zero Defects**: Enforce 80% test pass rate, 80% coverage minimum
- **Stop-the-Line (Andon)**: Block builds on quality gate failures
- **Receipt-Based Traceability**: Every build stage generates audit trail
- **SHACL Validation**: Ontology-driven quality enforcement
- **Continuous Improvement (Kaizen)**: Automated waste detection

### Build Pipeline Stages

```
Pull Signal → SHACL Validate → Compile → Generate Receipt →
Test → Check Quality Gates → Release → Verify Deterministic → Publish
              ↓                            ↓                    ↓
         [Andon]                      [Andon]              [Andon]
```

---

## Installation

### Prerequisites

- Erlang/OTP 24+
- Rebar3 3.18+
- Git (for version control)

### Enable TCPS Plugin

The TCPS plugin is already configured in `rebar.config`:

```erlang
{plugins, [
    rebar3_tcps_plugin  % Toyota Code Production System
]}.

{provider_hooks, [
    {pre, [{compile, {tcps, shacl_validate}}]},
    {post, [{compile, {tcps, generate_receipt}}]},
    {post, [{eunit, {tcps, check_quality_gates}}]},
    {pre, [{release, {tcps, check_quality_gates}}]}
]}.
```

### Verify Installation

```bash
rebar3 tcps --help
```

Expected output:
```
TCPS providers loaded:
  - tcps shacl_validate      SHACL ontology validation
  - tcps generate_receipt    Production receipt generation
  - tcps andon               Andon event management
  - tcps check_quality_gates Quality gates enforcement
```

---

## Quick Start

### Basic TCPS Build

```bash
# Full TCPS-validated build
rebar3 tcps_full
```

This executes:
1. SHACL validation of work orders
2. Compilation with error checking
3. Receipt generation
4. Test execution
5. Quality gates verification (80% pass, 80% coverage)

### Minimal Build (Skip TCPS)

```bash
# Standard build without TCPS enforcement
rebar3 compile
```

**Note**: TCPS hooks still run but won't block on non-critical issues.

---

## Build Pipeline Integration

### Automatic Hook Execution

TCPS providers run automatically at each stage:

| Build Stage | TCPS Provider | Action |
|-------------|---------------|--------|
| **Pre-Compile** | `tcps shacl_validate` | Validate work_orders.ttl against SHACL shapes |
| **Post-Compile** | `tcps generate_receipt` | Generate compilation receipt (errors, warnings, duration) |
| **Post-Test** | `tcps check_quality_gates` | Verify 80% pass rate, 80% coverage |
| **Pre-Release** | `tcps check_quality_gates` | Verify deterministic build hash |

### Manual Hook Invocation

Run individual providers:

```bash
# SHACL validation only
rebar3 tcps shacl_validate

# Generate receipt manually
rebar3 tcps generate_receipt --stage=compile

# Check quality gates
rebar3 tcps check_quality_gates --stage=test

# List Andon events
rebar3 tcps andon list
```

---

## Provider Commands

### 1. SHACL Validation (`tcps shacl_validate`)

Validates ontology files against TCPS SHACL shapes.

```bash
rebar3 tcps shacl_validate [OPTIONS]
```

**Options**:
- `--shapes FILE` - SHACL shapes file (default: `shapes/tcps_shapes.ttl`)
- `--data PATTERN` - Data files to validate (default: `ontology/*.ttl`)
- `--strict` - Fail on any violations (default: true)
- `--andon` - Trigger Andon on violations (default: true)

**Example**:
```bash
# Validate specific work order
rebar3 tcps shacl_validate --data=ontology/work_orders.ttl

# Non-strict mode (warnings only)
rebar3 tcps shacl_validate --strict=false --andon=false
```

**Output**:
```
=== TCPS SHACL Validation ===
Validating 3 files against shapes/tcps_shapes.ttl

  ✓ ontology/tcps_core.ttl validated successfully
  ✓ ontology/work_orders.ttl validated successfully
  ✓ ontology/tcps_quality.ttl validated successfully

Validation Summary:
  Files validated: 3
  Total violations: 0
  Critical violations: 0

✓ SHACL validation passed
```

### 2. Receipt Generation (`tcps generate_receipt`)

Generates production receipts for build stages.

```bash
rebar3 tcps generate_receipt [OPTIONS]
```

**Options**:
- `--stage STAGE` - Build stage (compile, test, release)
- `--sku-id ID` - SKU identifier (auto-generated if omitted)
- `--work-order ID` - Work order ID this build fulfills

**Example**:
```bash
# Generate compilation receipt
rebar3 tcps generate_receipt --stage=compile --sku-id=sku_12345

# Generate test receipt
rebar3 tcps generate_receipt --stage=test
```

**Output**:
```
=== TCPS Receipt Generation ===
Receipt generated: rcpt_compile_1737933600000_123456
Stage: compile, Status: pass

Receipt stored: priv/receipts/rcpt_compile_1737933600000_123456.json
```

**Receipt Structure** (`priv/receipts/rcpt_*.json`):
```json
{
  "receipt_id": "rcpt_compile_1737933600000_123456",
  "receipt_type": "compilation",
  "stage": "compile",
  "sku_id": "sku_12345",
  "timestamp_iso": "2026-01-27T00:00:00Z",
  "status": "pass",
  "metrics": {
    "error_count": 0,
    "warning_count": 2,
    "duration_ms": 1234,
    "modules_compiled": 42
  },
  "ontology_refs": [
    "tcps:Receipt",
    "tcps:CompilationStage"
  ]
}
```

### 3. Quality Gates (`tcps check_quality_gates`)

Enforces zero-defect quality standards.

```bash
rebar3 tcps check_quality_gates [OPTIONS]
```

**Options**:
- `--stage STAGE` - Stage to check (compile, test, release, all)
- `--sku-id ID` - SKU being validated
- `--strict` - Fail build on violations (default: true)
- `--min-pass-rate PERCENT` - Minimum test pass rate (default: 80.0)
- `--min-coverage PERCENT` - Minimum code coverage (default: 80.0)

**Example**:
```bash
# Check test quality gates
rebar3 tcps check_quality_gates --stage=test

# Custom thresholds
rebar3 tcps check_quality_gates --min-pass-rate=90 --min-coverage=85

# Check all gates
rebar3 tcps check_quality_gates --stage=all
```

**Output**:
```
=== TCPS Quality Gates Verification ===

Checking Compilation Quality Gate...
  ✓ compilation gate PASSED

Checking Test Quality Gate...
  ✗ test gate FAILED
    - coverage: 75.0 (expected: 80.0)
    - pass_rate: 85.5 (expected: 80.0)

Checking Release Quality Gate...
  ✓ release gate PASSED

Quality Gates Summary:
  Total Gates: 3
  Passed: 2
  Failed: 1

⚠️  TRIGGERING ANDON: Quality Gate Failure
Andon event created: ANDON-1737933600000-456789-12345

✗ Quality gates failed - build blocked
```

### 4. Andon Management (`tcps andon`)

Interactive Andon event management.

```bash
rebar3 tcps andon <COMMAND> [ARGS]
```

**Commands**:

#### List Open Andons
```bash
rebar3 tcps andon list
```

**Output**:
```
=== TCPS Open Andon Events ===
2 open Andon event(s) blocking pipeline:

Andon ID: ANDON-1737933600000-456789-12345
  Type: test_failure
  SKU: sku_12345
  Stage: testing
  Time: 2026-01-27 00:00:00 UTC
  Details: #{coverage => 75.0, pass_rate => 85.5}

To resolve: rebar3 tcps andon resolve <andon-id>
```

#### Resolve Andon
```bash
rebar3 tcps andon resolve <ANDON_ID>
```

**Interactive Prompts**:
```
=== Resolving Andon: ANDON-1737933600000-456789-12345 ===

Starting 5 Whys root cause analysis...

What was the root cause? Added new module without tests
What fix was applied? Created test suite for new module
What prevention was added? Updated CI to require 80% coverage on all new files

✓ Andon ANDON-1737933600000-456789-12345 resolved successfully!
Pipeline unblocked. You may proceed with build.
```

#### Show Andon Details
```bash
rebar3 tcps andon show <ANDON_ID>
```

#### Check Build Status
```bash
rebar3 tcps andon check
```

**Output (Blocked)**:
```
=== TCPS Build Status Check ===
⚠️  Build BLOCKED by 2 open Andon event(s):
  - ANDON-1737933600000-456789-12345
  - ANDON-1737933600000-789012-67890

Resolve Andons before proceeding with build.
```

**Output (Clear)**:
```
=== TCPS Build Status Check ===
✓ Build pipeline clear - no blocking Andon events
```

---

## Quality Gates

### Compilation Gate

**Thresholds**:
- Error count: 0 (absolute)
- Critical warnings: 0 (absolute)

**Violations Trigger**:
- Compilation errors
- Warnings categorized as critical

**Receipt Fields**:
```json
{
  "metrics": {
    "error_count": 0,
    "warning_count": 2,
    "critical_warnings": 0,
    "duration_ms": 1234
  }
}
```

### Test Gate

**Thresholds**:
- Pass rate: ≥ 80%
- Code coverage: ≥ 80%
- Total tests: ≥ 1

**Violations Trigger**:
- Test failures bringing pass rate below 80%
- Coverage below 80%
- No tests executed

**Receipt Fields**:
```json
{
  "metrics": {
    "total_tests": 50,
    "passed_tests": 48,
    "pass_rate": 96.0,
    "coverage": 85.5
  },
  "quality_gates": {
    "pass_rate_met": true,
    "coverage_met": true
  }
}
```

### Release Gate

**Thresholds**:
- Deterministic build: Required
- Artifact hash: Valid SHA-256
- Artifact size: > 0 bytes

**Violations Trigger**:
- Non-deterministic builds (hash mismatch)
- Missing or corrupted artifacts

**Receipt Fields**:
```json
{
  "metrics": {
    "artifact_hash": "a1b2c3d4...",
    "artifact_size": 12345678,
    "deterministic_build": true
  }
}
```

---

## Andon System

### What is Andon?

Andon (行灯) is a Toyota Production System concept:
- **Japanese**: "Lantern" or "stop signal"
- **Purpose**: Empower anyone to stop production when defects detected
- **TCPS**: Automatically blocks build pipeline on quality violations

### Andon Lifecycle

```
1. TRIGGER → 2. INVESTIGATE → 3. ROOT CAUSE → 4. RESOLVE → 5. PREVENT
     ↓              ↓                ↓              ↓            ↓
  (Fail)       (5 Whys)         (Analysis)      (Fix)      (SHACL/Tests)
```

### Andon Events by Type

| Event Type | Trigger | Required Resolution |
|------------|---------|-------------------|
| `shacl_violation` | SHACL validation failure | Fix ontology, add constraints |
| `compilation_failure` | Compilation errors | Fix code errors |
| `test_failure` | Test failures or coverage < 80% | Fix tests, add coverage |
| `non_determinism` | Build hash mismatch | Fix non-deterministic code |

### Resolving Andon Events

**Required Fields**:
1. **Root Cause**: Why did the failure occur? (5 Whys analysis)
2. **Fix Applied**: What immediate fix was implemented?
3. **Prevention**: What systemic change prevents recurrence?

**Example Resolution**:
```
Root Cause: "Added new HTTP client module without integration tests"
Fix Applied: "Created test suite covering all HTTP client endpoints"
Prevention: "Updated CI pipeline to reject PRs with < 80% coverage on new files"
```

**Best Practices**:
- Use 5 Whys methodology for root cause analysis
- Add SHACL constraints to prevent repeat violations
- Update templates/tests to prevent systemic issues
- Document in resolution receipt for Kaizen analysis

---

## Receipt System

### Receipt Types

1. **Compilation Receipt** (`compile`)
   - Error/warning counts
   - Build duration
   - Modules compiled

2. **Test Receipt** (`test`)
   - Test pass/fail counts
   - Coverage percentage
   - Failed test details

3. **Release Receipt** (`release`)
   - Artifact hash (SHA-256)
   - Artifact size
   - Deterministic build verification

4. **SHACL Validation Receipt** (`shacl_validation`)
   - Violations by file
   - Critical vs warning violations

5. **Quality Gates Receipt** (`quality_gates`)
   - All gate results
   - Violations summary

6. **Andon Resolution Receipt** (`andon_resolution`)
   - Root cause analysis
   - Fix and prevention actions

### Receipt Storage

All receipts stored in `priv/receipts/` as JSON:

```
priv/receipts/
  ├── rcpt_compile_1737933600000_123456.json
  ├── rcpt_test_1737933600000_234567.json
  ├── rcpt_quality_1737933600000_345678.json
  └── rcpt_shacl_1737933600000_456789.json
```

### Receipt Ontology Links

Each receipt includes ontology references:

```json
{
  "ontology_refs": [
    "tcps:Receipt",
    "tcps:TestStage",
    "tcps:QualityGate"
  ],
  "related_sku": "sku_12345",
  "work_order_id": "wo_67890"
}
```

### Using Receipts for Kaizen

Receipts feed into continuous improvement:

```bash
# Analyze receipts for waste (future feature)
rebar3 tcps kaizen analyze --period=7days

# Generate weekly Kaizen report
rebar3 tcps kaizen report --week-ending=2026-01-27
```

---

## Advanced Usage

### Custom Quality Thresholds

Override defaults in `rebar.config`:

```erlang
{tcps_config, [
    {quality_gates, [
        {min_test_pass_rate, 90.0},    % Raise to 90%
        {min_code_coverage, 85.0},     % Raise to 85%
        {max_compile_warnings, 10},    % Allow up to 10 warnings
        {strict_mode, true}            % Fail build on violations
    ]}
]}.
```

### Integrating with CI/CD

#### GitHub Actions Example

```yaml
name: TCPS Build Pipeline

on: [push, pull_request]

jobs:
  tcps-build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22'

      - name: TCPS Full Build
        run: rebar3 tcps_full

      - name: Check for Open Andons
        run: rebar3 tcps andon check

      - name: Upload Receipts
        uses: actions/upload-artifact@v3
        with:
          name: tcps-receipts
          path: priv/receipts/
```

### Custom SHACL Shapes

Add project-specific shapes to `shapes/custom_shapes.ttl`:

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix tcps: <http://taiea.io/ontology/tcps#> .

# Custom constraint: All SKUs must have release notes
tcps:SKUReleaseNotesShape
    a sh:NodeShape ;
    sh:targetClass tcps:SKU ;
    sh:property [
        sh:path tcps:releaseNotes ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 50 ;
        sh:severity sh:Violation ;
        sh:message "SKU must have release notes (minimum 50 characters)"
    ] .
```

Validate:
```bash
rebar3 tcps shacl_validate --shapes=shapes/custom_shapes.ttl
```

### Programmatic Access

Use TCPS modules directly in Erlang code:

```erlang
%% Trigger Andon from application code
Context = #{
    sku_id => <<"my-sku-123">>,
    stage => runtime,
    details => #{error => "Database connection timeout"}
},
{ok, AndonId} = tcps_andon:trigger_andon(runtime_failure, Context).

%% Check if build blocked
case tcps_andon:is_blocked(<<"my-sku-123">>) of
    true ->
        io:format("SKU blocked by open Andon~n");
    false ->
        io:format("SKU clear to proceed~n")
end.

%% Generate custom receipt
Receipt = #{
    receipt_type => <<"custom">>,
    stage => <<"deployment">>,
    sku_id => <<"my-sku-123">>,
    status => <<"pass">>
},
tcps_rebar3_receipt:store_receipt(Receipt).
```

---

## Troubleshooting

### Build Blocked by Andon

**Symptom**: Build fails with "Build blocked by open Andon events"

**Solution**:
1. List open Andons: `rebar3 tcps andon list`
2. Resolve each: `rebar3 tcps andon resolve <andon-id>`
3. Re-run build: `rebar3 tcps_full`

### SHACL Validation Failures

**Symptom**: SHACL validation fails with violations

**Solution**:
1. Review violations in output
2. Fix ontology files (`ontology/*.ttl`)
3. Ensure all required TCPS properties present
4. Re-validate: `rebar3 tcps shacl_validate`

**Common Violations**:
- Missing `tcps:createdAt` timestamps
- Invalid work order status
- Missing SKU → WorkOrder reference

### Quality Gate Failures

**Symptom**: "Quality gates failed - build blocked"

**Solution**:

**For Coverage Failures**:
```bash
# Generate coverage report
rebar3 cover

# View uncovered lines
rebar3 cover --verbose

# Add tests for uncovered code
```

**For Test Failures**:
```bash
# Run tests with detailed output
rebar3 eunit --verbose

# Fix failing tests
# Re-run: rebar3 tcps check_quality_gates --stage=test
```

### Receipts Not Generated

**Symptom**: No receipts in `priv/receipts/`

**Solution**:
1. Check directory exists: `mkdir -p priv/receipts`
2. Check file permissions: `chmod 755 priv/receipts`
3. Generate manually: `rebar3 tcps generate_receipt --stage=compile`

### Plugin Not Loading

**Symptom**: "Unknown namespace: tcps"

**Solution**:
1. Verify plugin in `rebar.config`:
   ```erlang
   {plugins, [rebar3_tcps_plugin]}.
   ```
2. Compile plugin: `rebar3 compile`
3. Verify: `rebar3 tcps --help`

---

## Reference

### Files

- **Provider Modules**:
  - `src/tcps/tcps_rebar3_shacl.erl` - SHACL validation
  - `src/tcps/tcps_rebar3_receipt.erl` - Receipt generation
  - `src/tcps/tcps_rebar3_andon.erl` - Andon management
  - `src/tcps/tcps_rebar3_quality.erl` - Quality gates
  - `src/tcps/rebar3_tcps_plugin.erl` - Plugin entry point

- **Core TCPS Modules**:
  - `src/tcps/tcps_andon.erl` - Andon system
  - `src/tcps_kaizen.erl` - Kaizen continuous improvement
  - `src/tcps_kanban.erl` - Kanban WIP management
  - `src/tcps_root_cause.erl` - 5 Whys analysis

- **Ontology & Shapes**:
  - `shapes/tcps_shapes.ttl` - SHACL quality constraints
  - `ontology/tcps_core.ttl` - Core TCPS ontology
  - `ontology/tcps_quality.ttl` - Quality definitions
  - `ontology/work_orders.ttl` - Work order instances

### Environment Variables

```bash
# Skip TCPS validation (CI override - use sparingly)
export TCPS_SKIP_VALIDATION=true

# Change receipt directory
export TCPS_RECEIPT_DIR=/custom/path/receipts

# Andon notification webhook
export TCPS_ANDON_WEBHOOK=https://hooks.slack.com/...
```

### Further Reading

- [TCPS Architecture](./TCPS.md) - Core TCPS concepts
- [TCPS Checklist](./TCPS-checklist.md) - Definition of Done
- [Toyota Production System](https://en.wikipedia.org/wiki/Toyota_Production_System) - TPS background

---

## Support

For issues or questions:
1. Check [Troubleshooting](#troubleshooting) section
2. Review receipts in `priv/receipts/` for detailed error info
3. Open GitHub issue with:
   - Andon event details (`rebar3 tcps andon show <id>`)
   - Receipts (`cat priv/receipts/rcpt_*.json`)
   - Build output

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-27
**TCPS Version**: 0.6.0
