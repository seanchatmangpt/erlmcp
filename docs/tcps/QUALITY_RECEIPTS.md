# TCPS Quality Receipts

## Overview

Quality receipts are immutable records of quality gate execution results that form part of the TCPS receipt chain. They prevent unvalidated releases by ensuring all quality gates pass before SKU certification.

## Purpose

Quality receipts serve as:

1. **Quality Gate Evidence** - Immutable proof that compilation, tests, coverage, and other quality metrics met production standards
2. **Release Gate** - Block SKU certification if any quality gate failed
3. **Audit Trail** - Complete history of quality checks with SHA-256 integrity
4. **Compliance** - Regulatory compliance documentation (ISO 9001, Lean Six Sigma)

## Receipt Format

### Quality Gate Receipt Structure

```json
{
  "receipt_id": "RCPT-quality-1738093847123-456789",
  "receipt_type": "quality_gate",
  "sku_id": "SKU-2026-001",
  "gate": "quality",
  "timestamp": 1738093847123,
  "timestamp_iso": "2026-01-28T15:30:47.123Z",
  "status": "passed",
  "checksum": "a3f2b8c9d1e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0",
  "details": {
    "compilation_status": "passed",
    "compilation_errors": 0,
    "test_pass_rate": 0.95,
    "test_total": 100,
    "test_passed": 95,
    "coverage_percentage": 0.85,
    "dialyzer_clean": true,
    "dialyzer_warnings": 0,
    "xref_clean": true,
    "xref_issues": 0,
    "benchmark_regression": 0.03
  },
  "ontology_refs": [
    "tcps:QualityGate",
    "tcps:Receipt",
    "tcps:SKU-2026-001"
  ]
}
```

### Quality Data Fields

| Field | Type | Requirement | Description |
|-------|------|-------------|-------------|
| `compilation_status` | `passed \| failed` | MANDATORY | Compilation result (0 errors required) |
| `compilation_errors` | `integer` | MANDATORY | Number of compilation errors (must be 0) |
| `test_pass_rate` | `float` | MANDATORY | Test pass rate (≥0.95 = 95% required) |
| `test_total` | `integer` | MANDATORY | Total number of tests executed |
| `test_passed` | `integer` | MANDATORY | Number of tests passed |
| `coverage_percentage` | `float` | MANDATORY | Code coverage (≥0.80 = 80% required) |
| `dialyzer_clean` | `boolean` | MANDATORY | Dialyzer clean (true = 0 warnings) |
| `dialyzer_warnings` | `integer` | MANDATORY | Number of Dialyzer warnings |
| `xref_clean` | `boolean` | MANDATORY | Xref clean (true = 0 issues) |
| `xref_issues` | `integer` | MANDATORY | Number of undefined function calls |
| `benchmark_regression` | `float` | OPTIONAL | Performance regression (<0.10 = <10%) |

## Quality Standards

### Production Thresholds

Quality gates enforce Toyota Production System zero-defect standards:

| Gate | Threshold | Status |
|------|-----------|--------|
| **Compilation** | 0 errors | BLOCKING |
| **Test Pass Rate** | ≥95% | BLOCKING |
| **Code Coverage** | ≥80% | BLOCKING |
| **Dialyzer** | 0 warnings | BLOCKING |
| **Xref** | 0 issues | BLOCKING |
| **Benchmark Regression** | <10% | WARNING |

**BLOCKING** means release is prevented if threshold not met.

### Quality Gate Sequence

Quality gates execute in order (stop on first failure):

1. **SHACL Validation** - Ontology conformance
2. **Compilation** - Zero errors (0 tolerence)
3. **Test Execution** - 95% pass rate minimum
4. **Coverage** - 80% code coverage minimum
5. **Security Scan** - Zero critical vulnerabilities
6. **Deterministic Build** - Reproducibility verification
7. **Quality Metrics** - Production threshold enforcement
8. **Release Verification** - SBOM, licenses, receipt chain
9. **Smoke Test** - Basic functionality

## Receipt Chain Validation

### Chain Integrity

Quality receipts form part of the complete receipt chain:

```
Work Order Created → SHACL → Compilation → Test → Coverage →
Security → Deterministic Build → Quality Metrics → Release → Smoke → Deploy
```

### Verification Process

The receipt verifier checks:

1. **Completeness** - All required receipts present (no gaps)
2. **Chronological Order** - Timestamps increasing (stages in sequence)
3. **SHA-256 Checksums** - Receipt integrity (no tampering)
4. **Quality Gate Pass** - All gates passed (thresholds met)
5. **No Open Andons** - All issues resolved

### Blocking Conditions

Release is **BLOCKED** if:

- ✗ Any quality receipt missing
- ✗ Any quality gate failed (threshold not met)
- ✗ Receipt chain incomplete or out of order
- ✗ SHA-256 checksum mismatch (tampering detected)
- ✗ Open Andon events exist

## Generating Quality Receipts

### Automatic Generation

Quality receipts are generated automatically by:

```bash
# Run quality checker and generate receipt
./tools/tcps/generate-quality-receipt.sh SKU-2026-001
```

This script:
1. Runs `tools/quality/quality-checker.sh`
2. Parses quality results (compilation, tests, coverage, etc.)
3. Creates quality receipt with SHA-256 checksum
4. Stores receipt in `priv/receipts/SKU-2026-001/`
5. Commits receipt to git for immutability

### Manual Generation (Erlang)

```erlang
% Create quality data
QualityData = #{
    compilation_status => passed,
    compilation_errors => 0,
    test_pass_rate => 0.95,
    test_total => 100,
    test_passed => 95,
    coverage_percentage => 0.85,
    dialyzer_clean => true,
    dialyzer_warnings => 0,
    xref_clean => true,
    xref_issues => 0,
    benchmark_regression => 0.03
},

% Generate quality receipt
Receipt = tcps_receipt:create_quality_gate_receipt(
    <<"SKU-2026-001">>,
    quality,
    QualityData
),

% Store receipt
{ok, ReceiptPath} = tcps_receipt:store_quality_gate_receipt(
    <<"SKU-2026-001">>,
    Receipt
).
```

## Release Validation

### Validation API

```erlang
% Validate SKU for release (comprehensive check)
case tcps_release_validator:validate_release(<<"SKU-2026-001">>) of
    {ok, _Result} ->
        % Release approved - all gates passed
        tcps_sku:certify(<<"SKU-2026-001">>);
    {error, {blocked, Result}} ->
        % Release blocked - check Result for details
        BlockingIssues = maps:get(blocking_issues, Result),
        io:format("Release blocked: ~p~n", [BlockingIssues])
end.

% Check only quality gates
case tcps_quality_receipt_verifier:verify_all_gates_passed(<<"SKU-2026-001">>) of
    {ok, all_passed} ->
        io:format("All quality gates passed~n");
    {error, {gates_failed, Failed}} ->
        io:format("Quality gates failed: ~p~n", [Failed])
end.

% Check receipt chain integrity
case tcps_quality_receipt_verifier:check_receipt_chain_integrity(<<"SKU-2026-001">>) of
    {ok, intact} ->
        io:format("Receipt chain intact~n");
    {error, {integrity_failed, Details}} ->
        io:format("Receipt chain compromised: ~p~n", [Details])
end.
```

### Work Order Completion

Work orders **REQUIRE** quality receipts before completion:

```erlang
% This will FAIL if quality receipts missing or gates failed
case tcps_work_order:complete_work_order(WorkOrderId, SkuId) of
    ok ->
        io:format("Work order completed~n");
    {error, {quality_receipts_missing, Missing}} ->
        io:format("ERROR: Missing quality receipts: ~p~n", [Missing]);
    {error, {quality_gates_failed, Failed}} ->
        io:format("ERROR: Quality gates failed: ~p~n", [Failed])
end.
```

### SKU Certification

SKU certification validates complete release readiness:

```erlang
% SKU certification checks:
% 1. All quality receipts present
% 2. All quality gates passed
% 3. Receipt chain complete and intact
% 4. No open Andon events
case tcps_sku:certify(SkuId) of
    {ok, certified} ->
        % SKU certified and ready for marketplace
        io:format("SKU certified~n");
    {error, {blocked, Reason}} ->
        % Certification blocked
        io:format("SKU certification blocked: ~p~n", [Reason])
end.
```

## Implementation Details

### Module Hierarchy

```
tcps_receipt.erl
├── create_quality_gate_receipt/3    % Create receipt with quality data
├── store_quality_gate_receipt/2     % Store receipt with SHA-256
└── verify_quality_gates/1            % Verify all gates passed

tcps_quality_receipt_verifier.erl
├── verify_quality_receipts/1        % Full quality receipt verification
├── verify_all_gates_passed/1        % Check all gates passed
├── check_receipt_chain_integrity/1  % Validate SHA-256 checksums
└── validate_release_readiness/1     % Comprehensive release check

tcps_release_validator.erl
├── validate_release/1               % Complete release validation
├── check_receipt_chain_complete/1   % Verify all receipts present
├── check_all_quality_gates_passed/1 % Check quality gates
└── blocks_release_certification/1   % Determine if blocked

tcps_work_order.erl
└── complete_work_order/2            % REQUIRES quality receipts
```

### Receipt Storage

Quality receipts are stored in:
```
priv/receipts/
└── <SKU_ID>/
    ├── shacl-validation-<timestamp>.json
    ├── compilation-<timestamp>.json
    ├── test-execution-<timestamp>.json
    ├── quality-gate-<timestamp>.json      # Quality receipt
    ├── release-verification-<timestamp>.json
    └── smoke-test-<timestamp>.json
```

### SHA-256 Checksum

Each receipt includes SHA-256 checksum for integrity:

```erlang
% Compute checksum (excludes checksum field itself)
compute_checksum(Receipt) ->
    ReceiptData = maps:remove(checksum, Receipt),
    Json = jsx:encode(ReceiptData),
    Hash = crypto:hash(sha256, Json),
    base64:encode(Hash).

% Verify checksum
verify_checksum(Receipt) ->
    StoredChecksum = maps:get(checksum, Receipt),
    ComputedChecksum = compute_checksum(Receipt),
    StoredChecksum =:= ComputedChecksum.
```

## Examples

### Example 1: Successful Release

```erlang
% All quality gates passed
SkuId = <<"SKU-2026-001">>,

% Verify quality receipts
{ok, Result} = tcps_quality_receipt_verifier:verify_quality_receipts(SkuId),
true = maps:get(all_gates_passed, Result),

% Validate release readiness
{ok, _} = tcps_release_validator:validate_release(SkuId),

% Complete work order (requires quality receipts)
ok = tcps_work_order:complete_work_order(WorkOrderId, SkuId),

% Certify SKU
{ok, certified} = tcps_sku:certify(SkuId).
```

### Example 2: Blocked Release (Failed Tests)

```erlang
SkuId = <<"SKU-2026-002">>,

% Quality check fails
{error, {release_blocked, Result}} =
    tcps_quality_receipt_verifier:verify_quality_receipts(SkuId),

FailedGates = maps:get(failed_gates, Result),
% => [testing]  % Test pass rate below 95%

% Work order completion blocked
{error, {quality_gates_failed, [testing]}} =
    tcps_work_order:complete_work_order(WorkOrderId, SkuId).

% Release validation blocked
{error, {blocked, Details}} =
    tcps_release_validator:validate_release(SkuId).

% SKU certification blocked
{error, {blocked, {quality_gates_failed, [testing]}}} =
    tcps_sku:certify(SkuId).
```

### Example 3: Compromised Receipt Chain

```erlang
SkuId = <<"SKU-2026-003">>,

% Checksum mismatch detected
{error, {integrity_failed, Details}} =
    tcps_quality_receipt_verifier:check_receipt_chain_integrity(SkuId),

% Details shows which receipt was tampered
Details = #{
    quality => {checksum_mismatch,
                <<"stored_checksum">>,
                <<"computed_checksum">>}
},

% Release blocked due to integrity failure
{error, {blocked, Result}} =
    tcps_release_validator:validate_release(SkuId).
```

## Best Practices

### 1. Always Generate Quality Receipts

Generate quality receipts **after every quality check**:

```bash
# After running quality checks
rebar3 compile && rebar3 eunit && rebar3 dialyzer && rebar3 xref
./tools/tcps/generate-quality-receipt.sh $SKU_ID
```

### 2. Validate Before Release

Always validate release readiness before certification:

```erlang
case tcps_release_validator:validate_release(SkuId) of
    {ok, _} ->
        % Safe to certify
        tcps_sku:certify(SkuId);
    {error, {blocked, Reason}} ->
        % Fix issues first
        handle_blocking_issues(Reason)
end.
```

### 3. Never Skip Quality Gates

Quality gates are **MANDATORY** for production releases. Never:

- ✗ Skip quality checks
- ✗ Override failed gates
- ✗ Modify receipt checksums
- ✗ Delete failed receipts

### 4. Monitor Receipt Chain

Regularly verify receipt chain integrity:

```erlang
% Daily integrity check
CheckIntegrity = fun(SkuId) ->
    case tcps_quality_receipt_verifier:check_receipt_chain_integrity(SkuId) of
        {ok, intact} -> ok;
        {error, {integrity_failed, Details}} ->
            trigger_andon(SkuId, {tampering_detected, Details})
    end
end,

AllSkus = tcps_sku:list_all(),
lists:foreach(CheckIntegrity, AllSkus).
```

### 5. Git Commit Receipts

Always commit receipts to git for immutability:

```bash
git add priv/receipts/
git commit -m "Add quality receipt for $SKU_ID"
```

## Troubleshooting

### Problem: Quality Gates Failing

**Symptoms:**
- `{error, {gates_failed, [testing]}}` when validating release
- Work order completion blocked

**Solution:**
1. Check which gates failed: `tcps_quality_receipt_verifier:verify_quality_receipts(SkuId)`
2. Fix underlying issues (e.g., failing tests, low coverage)
3. Re-run quality checks: `rebar3 eunit`
4. Generate new quality receipt: `./tools/tcps/generate-quality-receipt.sh $SKU_ID`

### Problem: Missing Quality Receipts

**Symptoms:**
- `{error, {missing_receipts, [quality]}}` when validating release

**Solution:**
1. Run quality checks: `rebar3 compile && rebar3 eunit`
2. Generate quality receipt: `./tools/tcps/generate-quality-receipt.sh $SKU_ID`

### Problem: Checksum Mismatch

**Symptoms:**
- `{error, {integrity_failed, #{quality => {checksum_mismatch, ...}}}}`

**Solution:**
1. Receipt has been tampered with - **DO NOT USE**
2. Regenerate receipt from scratch:
   ```bash
   rm priv/receipts/$SKU_ID/quality-gate-*.json
   ./tools/tcps/generate-quality-receipt.sh $SKU_ID
   ```
3. Trigger Andon if tampering suspected

### Problem: Open Andon Events

**Symptoms:**
- `{error, {blocked, {open_andons, [AndonId1, ...]}}}` when validating release

**Solution:**
1. Resolve open Andon events: `tcps_andon:resolve(AndonId, Resolution)`
2. Re-run release validation: `tcps_release_validator:validate_release(SkuId)`

## References

- [TCPS Receipt Chain](./RECEIPTS.md)
- [TCPS Quality Gates](./QUALITY_GATES.md)
- [TCPS Release Process](./RELEASE_PROCESS.md)
- [TCPS Andon System](./ANDON.md)
- [Toyota Production System](https://en.wikipedia.org/wiki/Toyota_Production_System)
