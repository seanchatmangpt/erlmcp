# Compliance Artifact Reference

## Quick Reference: All Artifact Types

### Evidence Bundle Structure

```
compliance-bundle-{run_number}.zip
├── SUMMARY.md                  # Final compliance decision (PASS/FAIL)
└── MANIFEST.txt                # Complete file manifest
```

### Individual Job Artifacts

| Job Name | Artifact Pattern | Retention | Contents |
|----------|-----------------|-----------|----------|
| **compile** | `compile-evidence-otp{25,26,27}.zip` | 30 days | Module lists, compile metadata |
| **eunit** | `eunit-evidence-otp{25,26,27}.zip` | 30 days | Coverage reports, test results |
| **ct** | `ct-evidence-otp{25,26,27}.zip` | 30 days | Integration test logs, suite summaries |
| **dialyzer** | `dialyzer-evidence-otp{25,26,27}.zip` | 30 days | Type warning logs, warning counts |
| **xref** | `xref-evidence-otp{25,26,27}.zip` | 30 days | Undefined function reports |
| **spec_validate** | `spec-compliance-evidence.zip` | 30 days | MCP 2025-11-25 spec compliance report |
| **security_scan** | `security-scan-evidence.zip` | 30 days | Vulnerability scans, secret detection |
| **perf_check** | `performance-check-evidence.zip` | 30 days | Benchmark results, regression analysis |
| **compliance_summary** | `compliance-bundle-{run_number}.zip` | 90 days | Aggregated summary and manifest |

## Detailed Artifact Contents

### 1. Compile Evidence

**File:** `compile-evidence-otp{25,26,27}.zip`

```
compile-evidence-otp25/
├── metadata.txt
│   ├── OTP_VERSION=25
│   ├── REBAR3_VERSION=3.20.0
│   ├── COMPILE_TIME=2026-01-30T12:34:56Z
│   ├── GIT_SHA=a1b2c3d...
│   └── GIT_REF=refs/heads/main
│
├── compiled_modules.txt
│   ├── _build/default/lib/erlmcp_core/ebin/erlmcp_client.beam
│   ├── _build/default/lib/erlmcp_core/ebin/erlmcp_server.beam
│   ├── ... (86 modules for erlmcp_core)
│   ├── _build/default/lib/erlmcp_transports/ebin/erlmcp_transport_tcp.beam
│   ├── ... (28 modules for erlmcp_transports)
│   └── ... (all 4 applications)
│
└── module_counts.txt
    ├── erlmcp_core: 86 modules
    ├── erlmcp_transports: 28 modules
    ├── erlmcp_observability: 21 modules
    └── erlmcp_validation: 5 modules
```

**Use Case:** Verify all modules compiled successfully across OTP versions.

**Audit Command:**
```bash
unzip compile-evidence-otp25.zip
cat compile-evidence-otp25/module_counts.txt
# Expected: 140 total modules (86 + 28 + 21 + 5)
```

---

### 2. EUnit Evidence

**File:** `eunit-evidence-otp{25,26,27}.zip`

```
eunit-evidence-otp25/
├── metadata.txt
│   ├── OTP_VERSION=25
│   ├── COVERAGE_RESULT=82.5%
│   ├── TEST_TIME=2026-01-30T12:35:10Z
│   └── COVERAGE_PASSED=true
│
├── coverage.txt
│   ├── erlmcp_client.erl: 85.2%
│   ├── erlmcp_server.erl: 78.9%
│   ├── ... (all modules)
│   └── Total: 82.5%
│
└── cover/
    ├── cover.coverdata           # Raw coverdata (binary)
    ├── erlmcp_client.coverage.txt
    ├── erlmcp_server.coverage.txt
    └── ... (per-module coverage)
```

**Use Case:** Verify test coverage meets 80% threshold.

**Audit Command:**
```bash
unzip eunit-evidence-otp25.zip
cat eunit-evidence-otp25/metadata.txt | grep COVERAGE_RESULT
# Expected: ≥80.0%
```

---

### 3. Common Test Evidence

**File:** `ct-evidence-otp{25,26,27}.zip`

```
ct-evidence-otp25/
├── metadata.txt
│   ├── OTP_VERSION=25
│   └── CT_TIME=2026-01-30T12:36:00Z
│
├── summary.txt
│   ├── erlmcp_transport_http_SUITE: 15/15 passed
│   ├── erlmcp_metrics_SUITE: 20/20 passed
│   └── Total: 35/35 passed
│
└── ct_run@2026-01-30_12:35:20/
    ├── index.html                   # HTML report
    ├── suite.summary.txt
    ├── erlmcp_transport_http_SUITE/
    │   ├── index.html
    │   ├── suite.log
    │   ├── test_cases/
    │   │   ├── http_get_request.test.log
    │   │   └── ...
    │   └── ...
    └── ...
```

**Use Case:** Verify integration tests pass.

**Audit Command:**
```bash
unzip ct-evidence-otp25.zip
cat ct-evidence-otp25/summary.txt
# Expected: All tests passed (0 failures)
```

---

### 4. Dialyzer Evidence

**File:** `dialyzer-evidence-otp{25,26,27}.zip`

```
dialyzer-evidence-otp25/
├── metadata.txt
│   ├── OTP_VERSION=25
│   ├── DIALYZER_WARNINGS=3
│   └── DIALYZER_TIME=2026-01-30T12:37:00Z
│
└── output.txt
    ├── erlmcp_client.erl:123: The pattern <...> can never match
    ├── erlmcp_server.erl:456: Guard test is_void(Value) has no effect
    └── ... (all warnings)
```

**Use Case:** Review type safety issues (ideally 0 warnings).

**Audit Command:**
```bash
unzip dialyzer-evidence-otp25.zip
cat dialyzer-evidence-otp25/metadata.txt | grep DIALYZER_WARNINGS
# Expected: 0 (or low number for manual review)
```

---

### 5. Xref Evidence

**File:** `xref-evidence-otp{25,26,27}.zip`

```
xref-evidence-otp25/
├── metadata.txt
│   ├── OTP_VERSION=25
│   ├── XREF_UNDEFINED=0
│   └── XREF_TIME=2026-01-30T12:38:00Z
│
└── output.txt
    ├── [xref check passed]
    └── No undefined function calls found
```

**Use Case:** Verify no undefined function calls.

**Audit Command:**
```bash
unzip xref-evidence-otp25.zip
cat xref-evidence-otp25/metadata.txt | grep XREF_UNDEFINED
# Expected: 0
```

---

### 6. Spec Compliance Evidence

**File:** `spec-compliance-evidence.zip`

```
spec-compliance-evidence/
└── compliance_report.txt
    ├── MCP Spec Compliance Validation Report
    ├── =====================================
    ├── Spec Version: 2025-11-25
    ├── Validation Time: 2026-01-30T12:39:00Z
    │
    ├── Validated Areas:
    │   ├── JSON-RPC 2.0 compliance: IMPLEMENTED
    │   ├── Refusal codes (1001-1089): DEFINED
    │   ├── Resources API: IMPLEMENTED
    │   ├── Tools API: IMPLEMENTED
    │   ├── Prompts API: IMPLEMENTED
    │   ├── Sampling API: IMPLEMENTED
    │   └── Transport behaviors: IMPLEMENTED
    │
    └── Status: COMPLIANT
```

**Use Case:** Verify MCP 2025-11-25 spec compliance.

**Audit Command:**
```bash
unzip spec-compliance-evidence.zip
cat spec-compliance-evidence/compliance_report.txt
# Expected: Status: COMPLIANT
```

---

### 7. Security Evidence

**File:** `security-scan-evidence.zip`

```
security-scan-evidence/
├── dependency_tree.txt
│   ├── erlmcp_core
│   │   ├── jsx 3.1.0
│   │   ├── jesse 1.6.0
│   │   ├── gproc 0.9.0
│   │   └── ... (all dependencies)
│   └── ... (all 4 applications)
│
├── secrets_scan.txt
│   ├── apps/erlmcp_core/src/erlmcp_auth.erl:42: password = "..."
│   └── ... (potential secrets - should be empty)
│
└── report.txt
    ├── Security Scan Report
    ├── ====================
    ├── Scan Time: 2026-01-30T12:40:00Z
    │
    ├── Checks Performed:
    │   ├── Dependency vulnerability scan: COMPLETED
    │   ├── Hardcoded secrets scan: COMPLETED
    │   ├── Input validation: COMPLETED
    │   ├── Authentication/authorization: COMPLETED
    │   ├── Rate limiting: IMPLEMENTED
    │   └── Circuit breakers: IMPLEMENTED
    │
    └── Status: SECURE
```

**Use Case:** Verify no security vulnerabilities.

**Audit Command:**
```bash
unzip security-scan-evidence.zip
wc -l security-scan-evidence/secrets_scan.txt
# Expected: 0 (no hardcoded secrets)
```

---

### 8. Performance Evidence

**File:** `performance-check-evidence.zip`

```
performance-check-evidence/
├── baselines.json
│   {
│     "core_ops_throughput": 2690000,
│     "tcp_conn_rate": 43000,
│     "memory_per_conn_mib": 0.5,
│     "latency_p50_us": 50,
│     "latency_p99_us": 200
│   }
│
├── benchmark_results.txt
│   ├── Benchmark: erlmcp_bench_core_ops
│   ├── Workload: core_ops_10k
│   ├── Results:
│   │   ├── throughput_msg_per_s: 2720000
│   │   ├── latency_p50_us: 48
│   │   ├── latency_p95_us: 85
│   │   └── latency_p99_us: 195
│   └── Comparison: +1.1% vs baseline (NO REGRESSION)
│
└── report.txt
    ├── Performance Check Report
    ├── =========================
    ├── Check Time: 2026-01-30T12:41:00Z
    │
    ├── Baselines (Jan 2026):
    │   ├── Core Ops: 2.69M ops/sec
    │   ├── TCP Conn Rate: 43K msg/sec
    │   ├── Memory/Conn: 0.5 MiB
    │   ├── Latency P50: 50us
    │   └── Latency P99: 200us
    │
    ├── Regression Threshold: 10%
    └── Status: NO REGRESSION
```

**Use Case:** Verify no performance regression >10%.

**Audit Command:**
```bash
unzip performance-check-evidence.zip
cat performance-check-evidence/report.txt | grep "Status:"
# Expected: Status: NO REGRESSION
```

---

### 9. Compliance Bundle

**File:** `compliance-bundle-{run_number}.zip`

```
compliance-bundle-42/
├── SUMMARY.md
│   ├── # MCP Spec Compliance Report
│   ├── **Workflow Run:** 42
│   ├── **Commit:** a1b2c3d...
│   │
│   ├── ## Quality Gate Results
│   │   ├── ### Compilation
│   │   │   ├── Status: success
│   │   │   └── Evidence: `compile-evidence-otp*.zip`
│   │   ├── ### Unit Tests (EUnit)
│   │   │   ├── Status: success
│   │   │   ├── Coverage: 82.5%
│   │   │   └── Evidence: `eunit-evidence-otp*.zip`
│   │   └── ... (all 8 gates)
│   │
│   ├── ## Final Decision
│   │   └── **Compliance Status:** ✅ PASS - All quality gates met
│   │
│   ├── ## Evidence Bundle Contents
│   │   ├── - Compilation artifacts (OTP 25, 26, 27)
│   │   ├── - Test coverage reports (OTP 25, 26, 27)
│   │   └── ... (all artifacts listed)
│   │
│   └── ## Next Steps
│       └── If **PASS**: Merge is approved
│
└── MANIFEST.txt
    ├── MCP Spec Compliance Evidence Manifest
    ├── =====================================
    ├── Workflow: mcp-spec-compliance.yml
    ├── Run ID: 1234567890
    ├── Run Number: 42
    │
    ├── Evidence Artifacts:
    │   ├── evidence-bundle/compile-evidence-otp25/metadata.txt
    │   ├── evidence-bundle/compile-evidence-otp25/compiled_modules.txt
    │   └── ... (127 total files)
    │
    └── Total Artifacts: 127
```

**Use Case:** Final compliance decision and complete evidence manifest.

**Audit Command:**
```bash
unzip compliance-bundle-42.zip
cat compliance-bundle-42/SUMMARY.md | grep "Compliance Status:"
# Expected: ✅ PASS - All quality gates met
```

---

## Artifact Usage Patterns

### Download All Artifacts from a Run

```bash
# Using GitHub CLI
gh run download 1234567890

# Artifacts downloaded to:
# ├── compile-evidence-otp25.zip
# ├── compile-evidence-otp26.zip
# ├── compile-evidence-otp27.zip
# ├── eunit-evidence-otp25.zip
# ├── ...
# └── compliance-bundle-42.zip
```

### Extract and Review Evidence

```bash
# Extract compliance bundle
unzip compliance-bundle-42.zip -d audit/

# Review summary
cat audit/compliance-bundle-42/SUMMARY.md

# Check specific gate evidence
unzip audit/compliance-bundle-42/compile-evidence-otp25.zip
cat audit/compliance-bundle-42/compile-evidence-otp25/module_counts.txt
```

### Automated Audit Script

```bash
#!/bin/bash
# audit_compliance.sh - Verify all gates passed

RUN_ID=$1
gh run download $RUN_ID

# Check compliance status
if grep -q "✅ PASS" compliance-bundle-*/SUMMARY.md; then
  echo "COMPLIANCE: PASS"
  exit 0
else
  echo "COMPLIANCE: FAIL"
  grep "Status:" compliance-bundle-*/SUMMARY.md
  exit 1
fi
```

---

## Artifact Retention Policy

| Artifact | Retention | Reason |
|----------|-----------|--------|
| Individual job evidence | 30 days | Short-term debugging |
| Compliance bundle | 90 days | Long-term audit trail |

**After retention period:** Artifacts are automatically deleted by GitHub.

**Permanent audit:** Store compliance bundles externally for indefinite retention:

```bash
# Archive compliance bundle to S3
aws s3 cp compliance-bundle-42.zip \
  s3://erlmcp-compliance-archive/2026/01/compliance-bundle-42.zip
```

---

## Related Documentation

- [Workflow Usage Guide](MCP_SPEC_COMPLIANCE_WORKFLOW.md)
- [Quality Gates Overview](QUALITY_GATES.md)
- [Performance Baselines](metrology/METRICS_GLOSSARY.md)

---

**Last Updated:** 2026-01-30
**Workflow Version:** 1.0.0
