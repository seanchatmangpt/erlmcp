# MCP Spec Compliance Workflow Guide

## Overview

The `.github/workflows/mcp-spec-compliance.yml` workflow implements a comprehensive quality gate system for erlmcp, ensuring all changes meet MCP spec compliance, quality standards, and performance requirements before merging.

## Workflow Structure

### 9 Quality Gates (All Must Pass)

The workflow runs **9 jobs** in a dependency graph with **fail-fast: true** enforcement:

```
                    ┌─────────────┐
                    │  compile   │
                    └──────┬──────┘
                           │
        ┌──────────────────┼──────────────────┐
        │                  │                  │
   ┌────▼────┐      ┌─────▼──────┐     ┌─────▼──────┐
   │  eunit  │      │     ct     │     │  dialyzer  │
   └────┬────┘      └─────┬──────┘     └─────┬──────┘
        │                  │                  │
        │                  │                  │
   ┌────▼────┐      ┌─────▼──────┐     ┌─────▼──────┐
   │   xref  │      │spec_validate│    │   perf_    │
   └────┬────┘      └─────┬──────┘     │   check    │
        │                  │          └─────┬──────┘
        └──────────────────┼────────────────┘
                           │
                  ┌────────▼─────────┐
                  │ compliance_sum-  │
                  │     mary         │
                  └──────────────────┘
```

### Job Descriptions

| Job | Purpose | Success Criteria | Evidence Artifacts |
|-----|---------|------------------|-------------------|
| **compile** | Verify all apps compile across OTP 25, 26, 27 | 0 errors, 4 apps compiled | `compile-evidence-otp{25,26,27}.zip` |
| **eunit** | Unit tests with coverage | 100% pass, ≥80% coverage | `eunit-evidence-otp{25,26,27}.zip` |
| **ct** | Integration test suites | 100% pass | `ct-evidence-otp{25,26,27}.zip` |
| **dialyzer** | Type checking | 0 warnings | `dialyzer-evidence-otp{25,26,27}.zip` |
| **xref** | Cross-reference analysis | 0 undefined functions | `xref-evidence-otp{25,26,27}.zip` |
| **spec_validate** | MCP 2025-11-25 spec compliance | All required APIs present | `spec-compliance-evidence.zip` |
| **security_scan** | Vulnerability scanning | 0 hardcoded secrets, deps secure | `security-scan-evidence.zip` |
| **perf_check** | Performance regression check | <10% regression vs baseline | `performance-check-evidence.zip` |
| **compliance_summary** | Final decision & evidence bundle | All 8 jobs passed | `compliance-bundle-{run_number}.zip` |

## Matrix Strategy: Multi-Version Erlang Testing

### Tested Erlang/OTP Versions

The workflow tests against **3 Erlang/OTP versions** using matrix strategy:

```yaml
matrix:
  otp: [25, 26, 27]
  rebar3: [3.20.0]
```

**Rationale:**
- **OTP 25**: Minimum supported version (stable LTS)
- **OTP 26**: Current stable release
- **OTP 27**: Latest release (catch breaking changes early)

**Jobs using matrix:**
- compile (3 jobs: compile-otp25, compile-otp26, compile-otp27)
- eunit (3 jobs: eunit-otp25, eunit-otp26, eunit-otp27)
- ct (3 jobs: ct-otp25, ct-otp26, ct-otp27)
- dialyzer (3 jobs: dialyzer-otp25, dialyzer-otp26, dialyzer-otp27)
- xref (3 jobs: xref-otp25, xref-otp26, xref-otp27)

**Total: 15 parallel jobs** (5 matrix jobs × 3 OTP versions)

## Gate Rules: Fail-Fast Enforcement

### Rule 1: All Jobs Must Pass

```yaml
fail-fast: true  # On all matrix jobs
```

**Behavior:**
- If ANY OTP version fails a job, the entire job fails
- Downstream jobs are blocked
- Compliance summary will report **FAIL**

**Example:**
- If `compile-otp26` fails, the entire `compile` job fails
- `eunit`, `ct`, `dialyzer`, `xref` jobs are blocked
- `compliance_summary` reports failure

### Rule 2: Dependency Chain Enforcement

```yaml
needs:
  - compile  # All other jobs depend on this
```

**Dependency Graph:**
```
compile ─┬─> eunit ─┐
         ├─> ct ────┤
         ├─> dialyzer ──> compliance_summary
         ├─> xref ──┤
         └─> (non-matrix jobs) ─┘
```

**Behavior:**
- Jobs wait for upstream dependencies to succeed
- If upstream fails, downstream jobs are skipped (not failed)
- Final `compliance_summary` runs even if dependencies fail (via `if: always()`)

### Rule 3: Coverage Threshold (80%)

```bash
if (( $(echo "$coverage_value < 80.0" | bc -l) )); then
  exit 1
fi
```

**Behavior:**
- Coverage < 80%: **FAIL** job
- Coverage ≥ 80%: **PASS** job
- Evidence includes exact coverage percentage

### Rule 4: Zero Tolerance Policies

| Gate | Zero Tolerance For | Enforcement |
|------|-------------------|-------------|
| **compile** | Compilation errors | Exit on error |
| **eunit** | Test failures | Non-zero exit code |
| **dialyzer** | Type warnings (ideal) | Logged as evidence |
| **xref** | Undefined functions | Logged as evidence |
| **security** | Hardcoded secrets | Logged as evidence |

**Note:** Dialyzer and xref warnings don't fail the job immediately but are recorded in evidence for manual review.

## Evidence Collection System

### Artifact Upload Strategy

Every job uploads evidence artifacts (even on failure):

```yaml
- name: Upload Evidence
  uses: actions/upload-artifact@v4
  if: always()  # Upload even if job fails
  with:
    name: compile-evidence-otp${{ matrix.otp }}
    path: evidence/
    retention-days: 30
```

### Evidence Types by Job

#### 1. Compile Evidence
```
compile-evidence-otp{25,26,27}.zip
├── metadata.txt              # OTP version, rebar3 version, git SHA
├── compiled_modules.txt      # List of all .beam files
└── module_counts.txt         # Module count per application
```

#### 2. EUnit Evidence
```
eunit-evidence-otp{25,26,27}.zip
├── metadata.txt              # OTP version, coverage %, test time
├── coverage.txt              # Human-readable coverage report
└── cover/                    # Raw coverdata files
    ├── cover.coverdata
    └── *.coverage.txt
```

#### 3. CT Evidence
```
ct-evidence-otp{25,26,27}.zip
├── metadata.txt              # OTP version, test time
├── summary.txt               # Suite summary (passed/failed)
└── ct_run@*/                 # Full CT logs
    ├── index.html
    ├── suite.summary.txt
    └── *.log
```

#### 4. Dialyzer Evidence
```
dialyzer-evidence-otp{25,26,27}.zip
├── metadata.txt              # OTP version, warning count, time
└── output.txt                # Full dialyzer warnings
```

#### 5. Xref Evidence
```
xref-evidence-otp{25,26,27}.zip
├── metadata.txt              # OTP version, undefined count, time
└── output.txt                # Full xref analysis
```

#### 6. Spec Compliance Evidence
```
spec-compliance-evidence.zip
└── compliance_report.txt     # MCP 2025-11-25 spec validation results
```

#### 7. Security Evidence
```
security-scan-evidence.zip
├── dependency_tree.txt       # Full dependency graph
├── secrets_scan.txt          # Potential hardcoded secrets
└── report.txt                # Security assessment
```

#### 8. Performance Evidence
```
performance-check-evidence.zip
├── baselines.json            # Historical performance baselines
├── benchmark_results.txt     # Current benchmark results
└── report.txt                # Regression analysis
```

#### 9. Compliance Bundle
```
compliance-bundle-{run_number}.zip
├── SUMMARY.md                # Final compliance decision (PASS/FAIL)
└── MANIFEST.txt              # List of all evidence artifacts
```

## Compliance Bundle Format

### SUMMARY.md Structure

```markdown
# MCP Spec Compliance Report

**Workflow Run:** 42
**Commit:** a1b2c3d...
**Branch:** main
**Triggered By:** claude
**Timestamp:** 2026-01-30T12:34:56Z

## Quality Gate Results

### Compilation
- Status: success
- Evidence: `compile-evidence-otp*.zip`

### Unit Tests (EUnit)
- Status: success
- Coverage: 82.5%
- Evidence: `eunit-evidence-otp*.zip`

[... all 8 gates ...]

## Final Decision

**Compliance Status:** ✅ PASS - All quality gates met

## Evidence Bundle Contents
- Compilation artifacts (OTP 25, 26, 27)
- Test coverage reports (OTP 25, 26, 27)
[...]

## Next Steps
If **PASS**: Merge is approved, all quality gates satisfied.
If **FAIL**: Review failed gate evidence, fix issues, re-run validation.
```

### MANIFEST.txt Structure

```
MCP Spec Compliance Evidence Manifest
=====================================

Workflow: mcp-spec-compliance.yml
Run ID: 1234567890
Run Number: 42

Evidence Artifacts:
evidence-bundle/compile-evidence-otp25/metadata.txt
evidence-bundle/compile-evidence-otp25/compiled_modules.txt
[... all files ...]

Total Artifacts: 127
```

## Merge Decision Process

### Automatic Enforcement

The workflow **automatically blocks merging** if:

1. **Any compilation error** occurs (any OTP version)
2. **Any test fails** (unit or integration)
3. **Coverage < 80%** (any OTP version)
4. **Performance regression > 10%** (vs baseline)
5. **Security vulnerabilities** found (high severity)

### Manual Review Required

The workflow **flags for manual review** if:

1. **Dialyzer warnings** > 0 (type safety issues)
2. **Xref undefined functions** > 0 (missing implementations)
3. **Potential secrets** found (needs verification)

### PR Comment Automation

On pull requests, the workflow **automatically comments** with the full compliance report:

```markdown
# MCP Spec Compliance Report

[Full SUMMARY.md content]

[Link to compliance-bundle-{run_number}.zip]
```

## Retention Policy

| Artifact Type | Retention | Rationale |
|--------------|-----------|-----------|
| Compile evidence | 30 days | Needed for debugging build failures |
| Test evidence | 30 days | Audit trail for test results |
| Dialyzer/xref | 30 days | Type safety history |
| Compliance bundle | 90 days | Long-term audit evidence |

## Usage Examples

### Trigger on Push

```bash
git push origin feature/my-feature
# Workflow runs automatically on all OTP versions
```

### Trigger on Pull Request

```bash
gh pr create --title "Add new transport" --body "Implements WebSocket"
# Workflow runs, comments on PR with compliance status
```

### Manual Trigger with Specific OTP Version

```bash
gh workflow run mcp-spec-compliance.yml \
  -f erlang_version=27
# Tests only OTP 27
```

### Download Compliance Bundle

```bash
# List recent workflow runs
gh run list --workflow=mcp-spec-compliance.yml

# Download artifacts from specific run
gh run download 1234567890

# Extract compliance bundle
unzip compliance-bundle-42.zip -d compliance-audit/
cat compliance-audit/SUMMARY.md
```

## Continuous Improvement

### Baseline Updates

When performance improves, update `perf_check` baselines:

```yaml
# In evidence/perf/baselines.json
{
  "core_ops_throughput": 3000000,  # Updated from 2.69M
  "tcp_conn_rate": 50000,          # Updated from 43K
  ...
}
```

### Adding New Quality Gates

To add a new gate (e.g., code complexity):

1. **Create new job:**
   ```yaml
   complexity_check:
     name: Code Complexity
     runs-on: ubuntu-latest
     needs: compile
     steps:
       - name: Calculate complexity
         run: ...  # Use tools like 'lizard' or 'erlc'
       - name: Upload evidence
         uses: actions/upload-artifact@v4
   ```

2. **Add to compliance_summary dependencies:**
   ```yaml
   needs:
     - compile
     - eunit
     ...
     - complexity_check  # Add here
   ```

3. **Update SUMMARY.md generation** to include new gate

## Troubleshooting

### Issue: Matrix Job Fails on Single OTP Version

**Symptom:** `compile-otp26` fails, but `compile-otp25` and `compile-otp27` pass

**Solution:**
- Check OTP 26-specific incompatibilities
- Review compile logs for that version
- Fix code to be compatible across all versions

### Issue: Coverage Drops Below 80%

**Symptom:** EUnit job fails with "Coverage 78.2% is below 80% threshold"

**Solution:**
- Download eunit-evidence artifacts
- Review coverage.txt for uncovered modules
- Add tests for uncovered code paths

### Issue: Performance Regression Detected

**Symptom:** perf_check job shows "12% regression vs baseline"

**Solution:**
- Download performance-check-evidence
- Compare benchmark_results.txt with baselines.json
- Identify regression cause (algorithm change, dependency update)
- Either fix regression or update baseline (if improvement is intentional)

## Related Documentation

- [Quality Gates Overview](../docs/QUALITY_GATES.md)
- [Testing Strategy](../docs/TESTING.md)
- [Performance Baselines](../docs/metrology/METRICS_GLOSSARY.md)
- [MCP Specification](../protocol/MCP_2025-11-25.md)

---

**Workflow File:** `.github/workflows/mcp-spec-compliance.yml`
**Maintained By:** erlmcp CI/CD team
**Last Updated:** 2026-01-30
