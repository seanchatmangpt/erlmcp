# Regression Prevention - Quick Start Guide

Get started with erlmcp's regression prevention system in 5 minutes.

## 1. First Time Setup

The system is ready to use out-of-the-box. No installation needed!

```bash
# Verify scripts are executable
ls -l tools/regression/*.sh

# Should show: -rwxr-xr-x (executable)
```

## 2. Run Your First Check

```bash
# Detect regressions
./tools/regression/detect-regression.sh
```

**First run output:**
```
[INFO] Collecting current metrics...
[INFO] Running compilation...
[SUCCESS] Compilation passed with 0 warnings
[INFO] Running tests...
[SUCCESS] Test pass rate: 100.00%
[SUCCESS] Current metrics collected
[WARN] No baseline file found. Creating baseline from current metrics.
[SUCCESS] Regression detection complete - No issues found
```

The first run creates a baseline. Second run compares against it.

## 3. Understanding Output

### Success (No Regressions)
```
==========================================
  REGRESSION DETECTION REPORT
==========================================

[SUCCESS] No regressions detected!
```

**Exit code:** 0

### Regressions Found
```
==========================================
  REGRESSION DETECTION REPORT
==========================================

[WARN] Found 2 regression(s) - Severity: high

[ERROR] [HIGH] Compilation warnings increased by 5 (100%)
[WARN] [MEDIUM] Module complexity increased by 12%

Full report: .metrics/regression-report.json
```

**Exit codes:**
- 1 = Medium severity
- 2 = High severity  
- 3 = Critical severity

## 4. View Detailed Report

```bash
# Generate HTML report
./tools/regression/generate-html-report.sh

# Open in browser
open .metrics/regression-report.html
```

The HTML report shows:
- Color-coded severity levels
- Before/after comparisons
- Specific recommendations
- Root cause suggestions

## 5. Fix or Justify

### Option A: Fix the Regression (Recommended)

Fix the underlying issue causing the regression:

```bash
# Example: Fix failing test
vim test/my_module_tests.erl

# Re-run detection
./tools/regression/detect-regression.sh

# Verify fixed
./tools/regression/block-on-regression.sh
```

### Option B: Justify the Regression

For intentional changes, add to allowlist:

```bash
# Edit allowlist
vim tools/regression/allowlist.json

# Add entry:
{
  "metric": "performance",
  "severity": "medium",
  "justification": "Trade performance for correctness",
  "approved_by": "your-name",
  "approved_date": "2026-01-28",
  "expires": "2026-03-28",
  "ticket": "ERLMCP-456"
}

# Re-run to verify
./tools/regression/block-on-regression.sh
```

## 6. CI/CD Integration (Automatic)

The system runs automatically on every PR via GitHub Actions.

**What happens:**
1. PR opened/updated
2. System collects baseline from `main`
3. System collects current from PR branch
4. Detects regressions
5. Posts comment to PR
6. Sets status check (pass/fail)

**No manual action needed!**

## 7. Common Scenarios

### Scenario 1: Pull Request Blocked

**PR Comment:**
```
❌ Quality Gate: FAILED
This PR is BLOCKED due to critical regressions.
```

**What to do:**
1. View HTML report (artifact in Actions)
2. Review regression details
3. Fix the issues
4. Push changes
5. System re-runs automatically

### Scenario 2: Warning on PR

**PR Comment:**
```
⚠️ Quality Gate: PASSED WITH WARNINGS
Please document justification for medium severity regressions.
```

**What to do:**
1. Add justification to PR description
2. Reviewer approves reasoning
3. Merge proceeds

### Scenario 3: All Green

**PR Comment:**
```
✅ Quality Gate: PASSED
All quality checks passed!
```

**What to do:**
- Celebrate! Merge when ready.

## 8. Severity Guide

| Severity | Test Drop | Coverage Drop | Perf Loss | Warnings | Action |
|----------|-----------|---------------|-----------|----------|--------|
| Critical | ≥5% | ≥10% | ≥20% | - | BLOCK |
| High | - | - | ≥10% | +20%+ | BLOCK |
| Medium | 2-5% | 5-10% | 5-10% | +10-20% | WARN |
| Low | <2% | <5% | <5% | <10% | INFO |

## 9. Best Practices

### DO:
- Run locally before creating PR
- Fix regressions promptly
- Document intentional tradeoffs
- Keep baseline up to date
- Review regression trends

### DON'T:
- Abuse allowlist to skip fixes
- Ignore medium severity warnings
- Update baseline to hide regressions
- Disable the system
- Skip quality gates

## 10. Troubleshooting

### Problem: Flaky baseline

**Symptom:** Different results on same code

**Solution:**
```bash
# Delete and recreate baseline
rm .metrics/baseline.json
./tools/regression/detect-regression.sh
./tools/regression/detect-regression.sh  # Run twice
```

### Problem: Can't reproduce CI failure

**Symptom:** Passes locally, fails in CI

**Solution:**
```bash
# Ensure clean state
git clean -fdx
rebar3 clean

# Run exactly as CI does
TERM=dumb rebar3 compile
rebar3 eunit
./tools/regression/detect-regression.sh
```

### Problem: Script permission denied

**Symptom:** `bash: permission denied`

**Solution:**
```bash
# Make scripts executable
chmod +x tools/regression/*.sh
```

## Next Steps

- Read full documentation: `docs/regression/PREVENTION_SYSTEM.md`
- Learn about TCPS: `.claude/TCPS_SYSTEM_COMPLETE.md`
- Understand thresholds: Review `tools/regression/detect-regression.sh`
- Explore allowlist: Check `tools/regression/allowlist.json`

## Getting Help

1. Check `tools/regression/README.md`
2. Review examples in allowlist
3. Ask in #engineering channel
4. Create issue with `regression-system` label

---

**Remember:** The goal is maintaining quality, not blocking work. The system helps you ship better code!
