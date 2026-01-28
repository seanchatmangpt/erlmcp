# Regression Prevention System

**TCPS Quality Gate: 自働化 (Jidoka) - Built-in Quality with Stop-the-Line Authority**

The erlmcp regression prevention system automatically detects quality regressions and blocks merges that would degrade code quality, performance, or reliability.

## Overview

The system consists of four components:

1. **Detection** - Automated metric collection and comparison
2. **Blocking** - Quality gates that prevent regressions from merging
3. **Reporting** - Detailed analysis and recommendations
4. **Allowlist** - Controlled exceptions with justification

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Pull Request Event                        │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│  Step 1: Collect Baseline Metrics (from main branch)        │
│  - Compile and count warnings                                │
│  - Run tests and calculate pass rate                         │
│  - Measure code coverage                                     │
│  - Benchmark performance                                     │
│  - Calculate complexity metrics                              │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│  Step 2: Collect Current Metrics (from PR branch)           │
│  - Same metrics as baseline                                  │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│  Step 3: Detect Regressions                                  │
│  - Compare current vs baseline                               │
│  - Calculate severity (low/medium/high/critical)             │
│  - Generate regression report                                │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│  Step 4: Enforce Quality Gates                               │
│  - Check allowlist for exceptions                            │
│  - BLOCK if critical/high severity                           │
│  - WARN if medium severity                                   │
│  - ALLOW if low severity                                     │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│  Step 5: Report & Status                                     │
│  - Post PR comment with details                              │
│  - Set GitHub status check                                   │
│  - Upload HTML report artifact                               │
└─────────────────────────────────────────────────────────────┘
```

## Metrics Tracked

### 1. Test Pass Rate
**What:** Percentage of tests that pass
**Threshold:** 
- Critical: ≥5% drop
- Medium: ≥2% drop
- Low: <2% drop

**Example:**
```
Baseline: 100% (100/100 tests)
Current:  94% (94/100 tests)
Regression: -6% [CRITICAL] ← BLOCKS MERGE
```

### 2. Code Coverage
**What:** Percentage of code covered by tests
**Threshold:**
- Critical: ≥10% drop
- Medium: ≥5% drop
- Low: <5% drop

**Example:**
```
Baseline: 85% coverage
Current:  78% coverage
Regression: -7% [MEDIUM] ← REQUIRES JUSTIFICATION
```

### 3. Compilation Warnings
**What:** Number of compiler warnings
**Threshold:**
- High: ≥20% increase
- Medium: ≥10% increase
- Low: <10% increase

**Example:**
```
Baseline: 5 warnings
Current:  12 warnings
Regression: +140% [HIGH] ← BLOCKS MERGE
```

### 4. Performance Throughput
**What:** Operations per second (from benchmarks)
**Threshold:**
- Critical: ≥20% degradation
- High: ≥10% degradation
- Medium: ≥5% degradation

**Example:**
```
Baseline: 2,690,000 msg/s
Current:  2,100,000 msg/s
Regression: -22% [CRITICAL] ← BLOCKS MERGE
```

### 5. Code Complexity
**What:** Average lines per module
**Threshold:**
- Medium: ≥15% increase
- Low: <15% increase

**Example:**
```
Baseline: 250 lines/module
Current:  295 lines/module
Regression: +18% [MEDIUM] ← REQUIRES JUSTIFICATION
```

## Severity Levels

### Critical (BLOCKS MERGE)
- Test pass rate drops ≥5%
- Performance degrades ≥20%
- Requires VP Engineering approval to override

### High (BLOCKS MERGE)
- Warnings increase ≥20%
- Performance degrades ≥10%
- Requires Tech Lead + 2 Senior Engineers approval

### Medium (WARNING)
- Test pass rate drops 2-5%
- Coverage drops 5-10%
- Complexity increases ≥15%
- Requires justification in PR description

### Low (INFORMATIONAL)
- Minor regressions below thresholds
- Noted but doesn't block merge

## Blocking Criteria

**BLOCKS MERGE IF:**
1. Any CRITICAL regression detected
2. Any HIGH regression detected
3. Regression not in allowlist
4. No valid override approval

**ALLOWS MERGE WITH WARNING IF:**
1. Only MEDIUM or LOW regressions
2. Justification provided in PR
3. Regression documented

**ALLOWS MERGE IF:**
1. No regressions detected
2. All quality gates pass

## Using the System

### Local Testing (Before PR)

```bash
# Run regression detection locally
./tools/regression/detect-regression.sh

# Check if quality gates would pass
./tools/regression/block-on-regression.sh

# Generate HTML report
./tools/regression/generate-html-report.sh

# View report
open .metrics/regression-report.html
```

### CI/CD (Automatic)

The system runs automatically on every PR:

1. GitHub Action triggers on PR creation/update
2. Collects baseline from main branch
3. Collects current from PR branch
4. Runs regression detection
5. Posts comment to PR with results
6. Sets status check (pass/fail)
7. Uploads detailed HTML report

### PR Comments

The system posts a comment like:

```markdown
## 🔍 Regression Detection Report

**Severity:** ❌ critical

### Regressions Detected

- **test_pass_rate** [critical]: Test pass rate dropped by 6.0%
  - Baseline: 100.0
  - Current: 94.0

- **performance** [high]: Performance degraded by 22%
  - Baseline: 2690000
  - Current: 2100000

### ❌ Quality Gate: FAILED

This PR is **BLOCKED** due to critical or high severity regressions.

To proceed:
1. Fix the regressions, or
2. Add to allowlist with justification, or
3. Request override from tech lead
```

## Override Process

### Adding to Allowlist

Edit `tools/regression/allowlist.json`:

```json
{
  "allowed_regressions": [
    {
      "metric": "performance",
      "severity": "medium",
      "justification": "Intentional tradeoff for improved correctness",
      "approved_by": "tech-lead",
      "approved_date": "2026-01-28",
      "expires": "2026-02-28",
      "ticket": "ERLMCP-123"
    }
  ]
}
```

**Requirements:**
- Must have clear justification
- Must have approval from appropriate level
- Must have expiration date
- Must link to ticket/issue

### Approval Levels

| Severity | Approval Required |
|----------|-------------------|
| Critical | VP Engineering |
| High | Tech Lead + 2 Senior Engineers |
| Medium | Tech Lead |
| Low | PR Reviewer |

## Periodic Review

The allowlist is reviewed monthly:

1. Tech Lead reviews all active exceptions
2. Expired exceptions are removed
3. Long-standing exceptions are investigated
4. Improvements are planned to eliminate need

## Best Practices

### For Developers

1. **Run locally before PR** - Catch issues early
2. **Fix don't workaround** - Address root causes
3. **Document intentional tradeoffs** - Clear justification
4. **Keep PRs focused** - Easier to analyze impact

### For Reviewers

1. **Check regression report** - Part of review process
2. **Question allowlist additions** - Ensure necessary
3. **Suggest improvements** - Help eliminate regressions
4. **Enforce standards** - Don't approve questionable overrides

### For Teams

1. **Monitor trends** - Track regression frequency
2. **Improve detection** - Add more metrics over time
3. **Automate fixes** - Build self-healing capabilities
4. **Continuous improvement** - Regular retrospectives

## Troubleshooting

### False Positives

If detection reports invalid regression:

1. Check baseline metrics collection
2. Verify test stability (no flakes)
3. Ensure consistent environment
4. Consider adjusting thresholds

### Flaky Tests

If tests are flaky:

1. Fix the flaky tests (priority)
2. Temporarily skip in baseline
3. Don't add to allowlist (forces fix)

### Performance Variance

If benchmarks vary too much:

1. Use longer benchmark runs
2. Run multiple times and average
3. Consider machine-specific baselines
4. Factor in measurement uncertainty

## Integration with TCPS

This system implements **自働化 (Jidoka)** principle:

- **Built-in quality** - Automated detection, not manual
- **Stop-the-line** - Blocks bad merges immediately
- **Root cause focus** - Reports suggest fixes
- **Continuous improvement** - Metrics and trends tracked

Related TCPS commands:
- `/tcps-jidoka` - Quality checks (uses this system)
- `/poka-yoke-validate` - Error-proofing (prevention)
- `/tcps-andon` - Stop line (blocking)

## Files

```
tools/regression/
├── detect-regression.sh      # Metric collection and comparison
├── block-on-regression.sh    # Quality gate enforcement
├── generate-html-report.sh   # HTML report generation
└── allowlist.json            # Permitted exceptions

.github/workflows/
└── regression-guard.yml      # CI/CD automation

docs/regression/
└── PREVENTION_SYSTEM.md      # This file

.metrics/                     # Generated at runtime
├── baseline.json             # Baseline metrics
├── current.json              # Current metrics
├── regression-report.json    # Detected regressions
├── regression-report.html    # HTML report
└── regression-summary.txt    # Text summary
```

## Metrics

Track system effectiveness:

- **Regression detection rate** - % of regressions caught
- **False positive rate** - % of invalid detections
- **Override rate** - % of regressions allowed via allowlist
- **Fix time** - Average time to fix detected regressions
- **Trend** - Regression frequency over time

## Future Enhancements

Planned improvements:

1. **Machine learning** - Predict regressions before they happen
2. **Auto-fix suggestions** - AI-generated fix recommendations
3. **Impact analysis** - Predict downstream effects
4. **Trend analysis** - Long-term quality tracking
5. **Benchmark improvements** - More comprehensive performance tests
6. **Security metrics** - CVE scanning, vulnerability detection
7. **Dependency tracking** - Monitor third-party library changes

## Support

Questions or issues:
1. Check this documentation
2. Review allowlist examples
3. Ask in #engineering channel
4. Create issue with `regression-system` label

---

**Remember:** The goal is not to block work, but to maintain quality. If the system blocks your PR, it's protecting users from regressions. Work with it, not against it.
