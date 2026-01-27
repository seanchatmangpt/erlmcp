# TCPS Definition of Done (DoD)

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Purpose**: Define what makes a SKU shippable

## Table of Contents

1. [Overview](#overview)
2. [Shippability Criteria](#shippability-criteria)
3. [Quality Gates](#quality-gates)
4. [Receipt Requirements](#receipt-requirements)
5. [Acceptance Criteria](#acceptance-criteria)
6. [Verification Checklist](#verification-checklist)

---

## Overview

**Definition of Done (DoD)** is the complete list of criteria that must be satisfied before a SKU can ship.

### Core Principle

> **"If any criterion fails, stop the line. The SKU is not shippable."**

No exceptions. No "ship it anyway and fix later". TCPS enforces built-in quality.

---

## Shippability Criteria

A TAIEA SKU is shippable **ONLY** when ALL of the following are true:

### 1. Specification Complete

**Requirement**: SHACL validation passes with zero violations.

**Verification**:
```bash
./tools/shacl-validate \
  --data ontology/ \
  --shapes shapes/ \
  --report receipts/generate/shacl-report.ttl

# Expected:
✓ SHACL validation: PASS (0 violations)
```

**Failure means**:
- Specification incomplete
- Required fields missing
- Constraints violated
- **Action**: Stop the line, fix ontology

---

### 2. Build Success

**Requirement**: Compilation succeeds with zero errors.

**Verification**:
```bash
rebar3 compile 2>&1 | tee receipts/build/compile.log

# Check exit code:
echo $?  # Must be 0

# Check compile log:
grep -i error receipts/build/compile.log  # Must be empty
```

**Acceptable**: Warnings are allowed but should be minimized.

**Failure means**:
- Syntax errors
- Type errors
- Missing modules
- **Action**: Stop the line, fix code

---

### 3. All Tests Pass

**Requirement**: 100% of tests pass (zero failures, zero errors).

**Verification**:
```bash
# Unit tests
rebar3 eunit
echo $?  # Must be 0

# Integration tests
rebar3 ct
echo $?  # Must be 0

# Property tests
rebar3 proper
echo $?  # Must be 0
```

**Failure modes**:
- Unit test failure → bug in code
- Integration test failure → interaction bug
- Property test failure → specification violation
- **Action**: Stop the line, fix bug, add regression test

---

### 4. Coverage Threshold Met

**Requirement**: Test coverage ≥ 80%

**Verification**:
```bash
rebar3 cover --verbose

# Check coverage report:
./tools/test check-coverage --threshold 80

# Expected:
✓ Line coverage: 87.3% (threshold: 80%)
✓ Function coverage: 92.1% (threshold: 80%)
✓ Coverage threshold: MET
```

**Failure means**:
- Insufficient test coverage
- Untested code paths
- **Action**: Add tests to reach threshold

---

### 5. Receipts Complete

**Requirement**: Receipts exist for every stage with no gaps.

**Verification**:
```bash
./tools/receipt verify-chain --work-order wo-001

# Expected:
✓ Stage 1 (pull): receipts/pull/wo-001.json
✓ Stage 2 (plan): receipts/plan/schedule-20260126.json
✓ Stage 3 (generate): receipts/generate/validate-wo-001.json
✓ Stage 4 (extract): receipts/extract/query-wo-001.json
✓ Stage 5 (render): receipts/render/files-wo-001.json
✓ Stage 6 (build): receipts/build/compile-wo-001.json
✓ Stage 7 (test): receipts/test/results-wo-001.json
✓ Stage 8 (release): receipts/release/erlmcp-0.6.0.json
✓ Stage 9 (publish): receipts/publish/erlmcp-0.6.0.json
✓ Stage 10 (verify): receipts/verify/erlmcp-0.6.0.json

Receipt chain: COMPLETE
```

**Failure means**:
- Missing proof for a stage
- Audit trail incomplete
- **Action**: Stop the line, regenerate missing receipt

---

### 6. Functional Verification

**Requirement**: `/health`, `/pubsub`, `/marketplace` endpoints respond successfully.

**Verification**:
```bash
# Start server
./tools/start-server erlmcp-0.6.0

# Test health endpoint
curl http://localhost:5000/health
# Expected: {"status": "ok", "version": "0.6.0"}

# Test pubsub endpoint
curl http://localhost:5000/pubsub/test
# Expected: {"status": "ok", "subscribers": 0}

# Test marketplace endpoint
curl http://localhost:5000/marketplace/info
# Expected: {"sku": "erlmcp-0.6.0", "available": true}
```

**Failure means**:
- Endpoint not responding
- Incorrect response
- Runtime error
- **Action**: Stop the line, fix endpoint

---

### 7. Entitlement Gating Works

**Requirement**: Inactive users receive HTTP 403 Forbidden.

**Verification**:
```bash
# Test with active user token
curl -H "Authorization: Bearer ACTIVE_TOKEN" \
     http://localhost:5000/api/resource
# Expected: 200 OK

# Test with inactive user token
curl -H "Authorization: Bearer INACTIVE_TOKEN" \
     http://localhost:5000/api/resource
# Expected: 403 Forbidden

# Test with expired license
curl -H "Authorization: Bearer EXPIRED_TOKEN" \
     http://localhost:5000/api/resource
# Expected: 403 Forbidden

# Test with no token
curl http://localhost:5000/api/resource
# Expected: 401 Unauthorized
```

**Failure means**:
- Entitlement logic broken
- Security vulnerability
- **Action**: Stop the line, fix entitlement logic

---

### 8. Smoke Tests Pass

**Requirement**: Smoke tests against shipped artifact succeed.

**Verification**:
```bash
# Run smoke test suite
./tools/verify/smoke-test.sh erlmcp-0.6.0

# Expected output:
Running smoke tests for erlmcp 0.6.0...
✓ Server starts successfully
✓ Client connects
✓ Initialize handshake succeeds
✓ List resources succeeds
✓ Call tool succeeds
✓ Response format valid
✓ Server shutdown clean

Smoke tests: PASS (7/7)
```

**Failure means**:
- Basic functionality broken
- Release artifact corrupt
- **Action**: Stop the line, investigate

---

### 9. Deterministic Build

**Requirement**: Repeated builds produce identical checksums.

**Verification**:
```bash
# Build 3 times, compare checksums
make clean && make release
sha256sum _build/default/rel/erlmcp/erlmcp-0.6.0.tar.gz > build1.sum

make clean && make release
sha256sum _build/default/rel/erlmcp/erlmcp-0.6.0.tar.gz > build2.sum

make clean && make release
sha256sum _build/default/rel/erlmcp/erlmcp-0.6.0.tar.gz > build3.sum

# Compare
diff build1.sum build2.sum  # Must be identical
diff build2.sum build3.sum  # Must be identical

# Expected:
✓ Build 1 checksum: sha256:abc123...
✓ Build 2 checksum: sha256:abc123...
✓ Build 3 checksum: sha256:abc123...
✓ Deterministic: YES
```

**Failure means**:
- Non-deterministic build process
- Timestamps embedded
- Random data included
- **Action**: Stop the line, find and eliminate randomness

---

### 10. No Security Vulnerabilities

**Requirement**: Security scanners report zero critical/high vulnerabilities.

**Verification**:
```bash
# Scan dependencies
./tools/security/scan-deps.sh

# Expected:
Scanning dependencies for vulnerabilities...
✓ No critical vulnerabilities found
✓ No high vulnerabilities found
⚠ 2 medium vulnerabilities found (accepted)
Security scan: PASS

# Scan code
./tools/security/scan-code.sh

# Expected:
Scanning code for vulnerabilities...
✓ No hardcoded secrets
✓ No SQL injection vectors
✓ No XSS vulnerabilities
✓ No insecure crypto
Code security scan: PASS
```

**Failure means**:
- Vulnerable dependency
- Insecure code pattern
- **Action**: Stop the line, patch vulnerability

---

## Quality Gates

Quality gates are automated checks that block progression if criteria fail.

### Gate 1: Pre-Build Gate

**Location**: Before Stage 6 (Build)

**Checks**:
- [ ] SHACL validation passed
- [ ] SPARQL queries executed successfully
- [ ] Templates rendered without errors
- [ ] No manual edits detected

**Failure Action**: Block build, emit Andon

---

### Gate 2: Pre-Test Gate

**Location**: Before Stage 7 (Test)

**Checks**:
- [ ] Compilation succeeded
- [ ] All BEAM files present
- [ ] Build deterministic (checksums match)
- [ ] No compiler errors

**Failure Action**: Block tests, emit Andon

---

### Gate 3: Pre-Release Gate

**Location**: Before Stage 8 (Release)

**Checks**:
- [ ] All tests passed
- [ ] Coverage ≥ 80%
- [ ] No test failures
- [ ] No flaky tests detected

**Failure Action**: Block release, emit Andon

---

### Gate 4: Pre-Publish Gate

**Location**: Before Stage 9 (Publish)

**Checks**:
- [ ] Release artifact built
- [ ] Checksums calculated
- [ ] Smoke tests passed
- [ ] Entitlement gating works

**Failure Action**: Block publish, emit Andon

---

### Gate 5: Final Verification Gate

**Location**: After Stage 10 (Verify)

**Checks**:
- [ ] Published artifact installable
- [ ] Functional verification passed
- [ ] Security scan clean
- [ ] Receipt chain complete

**Failure Action**: Quarantine release, emit Andon

---

## Receipt Requirements

Every stage must emit a receipt with these required fields:

### Minimum Receipt Schema

```json
{
  "receipt_id": "string (unique)",
  "stage": "string (pull|plan|generate|extract|render|build|test|release|publish|verify)",
  "timestamp": "string (ISO 8601)",
  "work_order_id": "string",
  "status": "string (completed|failed)",
  "next_stage": "string (or null if final)",

  "inputs": {
    "description": "What inputs were used"
  },

  "outputs": {
    "description": "What outputs were produced"
  },

  "validation": {
    "passed": "boolean",
    "details": "object (stage-specific)"
  },

  "checksums": {
    "algorithm": "sha256",
    "values": "object (file -> checksum)"
  },

  "duration_ms": "integer",

  "errors": [
    "array of errors (if any)"
  ]
}
```

### Receipt Linking

Each receipt must link to previous stage:

```json
{
  "receipt_id": "build-wo-001-20260126",
  "previous_receipt": "render-wo-001-20260126",
  "receipt_chain_hash": "sha256:...",
  ...
}
```

**Verification**:
```bash
# Verify chain integrity
./tools/receipt verify-chain --work-order wo-001

# Check for:
# - All receipts present
# - Correct sequence
# - Valid chain hash
# - No missing links
```

---

## Acceptance Criteria

### Functional Criteria

**Must have**:
- [ ] All required features implemented
- [ ] All APIs documented
- [ ] All error cases handled
- [ ] All edge cases tested

**Must not have**:
- [ ] Known bugs
- [ ] Incomplete features
- [ ] TODO comments in shipped code
- [ ] Debug logging enabled

---

### Non-Functional Criteria

**Performance**:
- [ ] Response time < 100ms (p95)
- [ ] Throughput ≥ 1000 req/sec
- [ ] Memory usage < 500MB
- [ ] CPU usage < 50% at load

**Reliability**:
- [ ] Uptime ≥ 99.9%
- [ ] No memory leaks
- [ ] Graceful degradation
- [ ] Auto-recovery from failures

**Security**:
- [ ] Authentication enforced
- [ ] Authorization correct
- [ ] No vulnerabilities (critical/high)
- [ ] Audit trail complete

**Maintainability**:
- [ ] Code coverage ≥ 80%
- [ ] Documentation complete
- [ ] No code duplication
- [ ] Consistent style

---

## Verification Checklist

Use this checklist before marking a SKU as "shippable":

### Specification Phase
- [ ] Work order created and validated
- [ ] SHACL shapes pass
- [ ] Ontology complete
- [ ] Receipt emitted: `receipts/pull/*.json`

### Planning Phase
- [ ] Heijunka schedule created
- [ ] WIP limit respected
- [ ] Bucket allocation balanced
- [ ] Receipt emitted: `receipts/plan/*.json`

### Generation Phase
- [ ] Ontology loaded successfully
- [ ] SHACL validation passed (0 violations)
- [ ] Build deterministic
- [ ] Receipt emitted: `receipts/generate/*.json`

### Extraction Phase
- [ ] SPARQL queries executed
- [ ] Results validated
- [ ] No timeouts
- [ ] Receipt emitted: `receipts/extract/*.json`

### Rendering Phase
- [ ] Templates rendered
- [ ] No manual edits detected
- [ ] Output files validated
- [ ] Receipt emitted: `receipts/render/*.json`

### Build Phase
- [ ] Compilation succeeded
- [ ] Zero errors
- [ ] BEAM files present
- [ ] Build deterministic
- [ ] Receipt emitted: `receipts/build/*.json`

### Test Phase
- [ ] All unit tests passed
- [ ] All integration tests passed
- [ ] All property tests passed
- [ ] Coverage ≥ 80%
- [ ] Receipt emitted: `receipts/test/*.json`

### Release Phase
- [ ] Release artifact built
- [ ] Checksums calculated
- [ ] Container built (if applicable)
- [ ] Receipt emitted: `receipts/release/*.json`

### Publish Phase
- [ ] Hex.pm published
- [ ] Container pushed
- [ ] Marketplace updated
- [ ] Receipt emitted: `receipts/publish/*.json`

### Verification Phase
- [ ] Install from Hex.pm works
- [ ] Smoke tests pass
- [ ] Entitlement gating works
- [ ] Functional verification complete
- [ ] Receipt emitted: `receipts/verify/*.json`

### Final Checks
- [ ] Receipt chain complete (all 10 stages)
- [ ] No Andon events outstanding
- [ ] Security scan clean
- [ ] Performance targets met
- [ ] Documentation complete

---

## Decision Matrix

Use this matrix to determine if SKU is shippable:

| Criterion | Status | Action |
|-----------|--------|--------|
| **All criteria met** | ✓ Green | **SHIP IT** |
| **1 criterion failed** | ⚠ Yellow | Stop line, fix, re-verify |
| **2+ criteria failed** | ✗ Red | Quarantine SKU, run 5 Whys |

---

## Failure Response

### If ANY criterion fails:

1. **Immediate**: Stop the line
2. **Trigger**: Emit Andon event
3. **Quarantine**: Mark SKU as "not shippable"
4. **Analyze**: Run 5 Whys root cause analysis
5. **Fix**: Update spec/code/tests
6. **Prevent**: Add constraint/test to prevent recurrence
7. **Re-verify**: Run full DoD checklist again
8. **Only when ALL criteria pass**: Resume shipping

---

## Example: Complete DoD Verification

```bash
#!/bin/bash
# dod-verify.sh - Verify all DoD criteria

WORK_ORDER="wo-001"
SKU="erlmcp-0.6.0"

echo "=== TCPS Definition of Done Verification ==="
echo "Work Order: $WORK_ORDER"
echo "SKU: $SKU"
echo

# 1. SHACL Validation
echo "1. Verifying SHACL validation..."
./tools/shacl-validate --data ontology/ --shapes shapes/ || {
    echo "✗ SHACL validation FAILED"
    ./tools/andon trigger --stage generate --reason shacl_violation
    exit 1
}
echo "✓ SHACL validation passed"

# 2. Build Success
echo "2. Verifying build..."
rebar3 compile || {
    echo "✗ Build FAILED"
    ./tools/andon trigger --stage build --reason compilation_error
    exit 1
}
echo "✓ Build succeeded"

# 3. All Tests Pass
echo "3. Verifying tests..."
rebar3 eunit && rebar3 ct && rebar3 proper || {
    echo "✗ Tests FAILED"
    ./tools/andon trigger --stage test --reason test_failure
    exit 1
}
echo "✓ All tests passed"

# 4. Coverage Threshold
echo "4. Verifying coverage..."
./tools/test check-coverage --threshold 80 || {
    echo "✗ Coverage below threshold"
    ./tools/andon trigger --stage test --reason insufficient_coverage
    exit 1
}
echo "✓ Coverage threshold met"

# 5. Receipts Complete
echo "5. Verifying receipt chain..."
./tools/receipt verify-chain --work-order $WORK_ORDER || {
    echo "✗ Receipt chain incomplete"
    ./tools/andon trigger --stage verify --reason missing_receipt
    exit 1
}
echo "✓ Receipt chain complete"

# 6. Functional Verification
echo "6. Verifying endpoints..."
./tools/verify/test-endpoints.sh || {
    echo "✗ Endpoint verification FAILED"
    ./tools/andon trigger --stage verify --reason endpoint_failure
    exit 1
}
echo "✓ Endpoints verified"

# 7. Entitlement Gating
echo "7. Verifying entitlement gating..."
./tools/verify/test-entitlements.sh || {
    echo "✗ Entitlement gating FAILED"
    ./tools/andon trigger --stage verify --reason entitlement_failure
    exit 1
}
echo "✓ Entitlement gating works"

# 8. Smoke Tests
echo "8. Running smoke tests..."
./tools/verify/smoke-test.sh $SKU || {
    echo "✗ Smoke tests FAILED"
    ./tools/andon trigger --stage verify --reason smoke_test_failure
    exit 1
}
echo "✓ Smoke tests passed"

# 9. Deterministic Build
echo "9. Verifying deterministic build..."
./tools/build verify-determinism || {
    echo "✗ Build not deterministic"
    ./tools/andon trigger --stage build --reason non_deterministic
    exit 1
}
echo "✓ Build is deterministic"

# 10. Security Scan
echo "10. Running security scan..."
./tools/security/scan-all.sh || {
    echo "✗ Security scan FAILED"
    ./tools/andon trigger --stage verify --reason security_vulnerability
    exit 1
}
echo "✓ Security scan clean"

echo
echo "=== DEFINITION OF DONE: PASSED ==="
echo "SKU $SKU is SHIPPABLE"
echo

# Emit final DoD receipt
./tools/receipt generate \
    --stage dod-verification \
    --work-order $WORK_ORDER \
    --all-criteria-met true \
    --output receipts/dod/$SKU-verified.json

exit 0
```

---

## Conclusion

**Definition of Done is non-negotiable.**

Every criterion must pass. If any fails, stop the line.

No exceptions. No "ship it anyway". No "fix it later".

**Quality is built in, not inspected in.**

---

**See Also**:
- [TCPS.md](TCPS.md) - Complete TCPS guide
- [STANDARD_WORK.md](STANDARD_WORK.md) - Stage procedures
- [ANDON_RUNBOOK.md](ANDON_RUNBOOK.md) - Failure response
- [RECEIPTS_SPEC.md](RECEIPTS_SPEC.md) - Receipt schemas
