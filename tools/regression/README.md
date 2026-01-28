# Regression Prevention Tools

**TCPS Quality Gate System - 自働化 (Jidoka)**

Automated detection and prevention of quality regressions in erlmcp.

## Quick Start

```bash
# Run full regression check
./detect-regression.sh

# Enforce quality gates (blocks if critical)
./block-on-regression.sh

# Generate HTML report
./generate-html-report.sh

# View report
open ../../.metrics/regression-report.html
```

## Tools

### detect-regression.sh
Collects metrics and detects regressions by comparing current code to baseline.

**Metrics tracked:**
- Test pass rate (% passing)
- Code coverage (%)
- Compilation warnings (count)
- Performance throughput (msg/s)
- Code complexity (lines/module)

**Output:** `.metrics/regression-report.json`

**Exit codes:**
- 0: No regressions
- 1: Medium severity
- 2: High severity
- 3: Critical severity

### block-on-regression.sh
Enforces quality gates and blocks merges on critical regressions.

**Actions:**
- BLOCKS: Critical/High severity (unless allowlisted)
- WARNS: Medium severity (requires justification)
- ALLOWS: Low severity or no regressions

**Output:** `.metrics/regression-summary.txt`

**Exit codes:**
- 0: Quality gate passed
- 1: Quality gate failed (merge blocked)

### generate-html-report.sh
Generates detailed HTML report with visualizations and recommendations.

**Features:**
- Color-coded severity levels
- Before/after comparisons
- Fix recommendations
- Trend visualization

**Output:** `.metrics/regression-report.html`

### allowlist.json
Controlled exceptions for permitted regressions.

**Schema:**
```json
{
  "allowed_regressions": [
    {
      "metric": "performance",
      "severity": "medium",
      "justification": "Reason for allowing",
      "approved_by": "tech-lead",
      "approved_date": "2026-01-28",
      "expires": "2026-02-28",
      "ticket": "ERLMCP-123"
    }
  ]
}
```

## Severity Levels

| Level | Description | Action | Approval |
|-------|-------------|--------|----------|
| Critical | >5% test drop, >20% perf loss | BLOCK | VP Engineering |
| High | >20% warnings, >10% perf loss | BLOCK | Tech Lead + 2 Sr Eng |
| Medium | 2-5% test drop, 5-10% coverage loss | WARN | Tech Lead |
| Low | Minor regressions | INFO | PR Reviewer |

## CI/CD Integration

The system runs automatically via GitHub Actions on every PR:

1. Collects baseline from `main` branch
2. Collects current from PR branch
3. Detects regressions
4. Posts comment to PR
5. Sets status check (pass/fail)
6. Uploads HTML report

See `.github/workflows/regression-guard.yml`

## Local Development Workflow

### Before Creating PR
```bash
# 1. Run regression detection
./tools/regression/detect-regression.sh

# 2. Check if quality gates pass
./tools/regression/block-on-regression.sh

# 3. If regressions found, view details
./tools/regression/generate-html-report.sh
open .metrics/regression-report.html

# 4. Fix regressions or justify in PR
```

### After PR Feedback
```bash
# Check what changed
git diff main

# Re-run detection
./tools/regression/detect-regression.sh

# Verify fixes
./tools/regression/block-on-regression.sh
```

## Adding Allowlist Exception

Only when regression is intentional and justified:

1. Edit `tools/regression/allowlist.json`
2. Add entry with:
   - Metric name
   - Severity level
   - Clear justification
   - Approver name
   - Expiration date
   - Ticket reference
3. Get approval from appropriate level
4. Commit with PR

**Example:**
```json
{
  "metric": "performance",
  "severity": "medium",
  "justification": "Trade performance for correctness in JSON validation",
  "approved_by": "jane-doe",
  "approved_date": "2026-01-28",
  "expires": "2026-03-28",
  "ticket": "ERLMCP-456"
}
```

## Troubleshooting

### "No baseline file found"
First run creates baseline. Subsequent runs compare against it.

**Fix:** Run detection twice on main branch to establish baseline.

### Flaky test failures
Tests failing intermittently cause false positives.

**Fix:** 
1. Fix flaky tests (priority!)
2. Don't add to allowlist (forces proper fix)

### Performance variance
Benchmarks vary due to system load.

**Fix:**
- Run on dedicated CI machines
- Use longer benchmark runs
- Average multiple runs
- Consider baseline update

### Wrong baseline
Baseline from wrong commit or branch.

**Fix:**
```bash
# Delete and regenerate
rm .metrics/baseline.json
git checkout main
./tools/regression/detect-regression.sh
git checkout your-branch
./tools/regression/detect-regression.sh
```

## Manual Baseline Update

Update baseline after intentional breaking changes:

```bash
# On main branch after merge
git checkout main
git pull

# Regenerate baseline
rm .metrics/baseline.json
./tools/regression/detect-regression.sh

# Commit new baseline
git add .metrics/baseline.json
git commit -m "Update regression baseline after performance improvements"
git push
```

**Note:** Only update after major releases or intentional breaking changes.

## Metrics Directory Structure

```
.metrics/
├── baseline.json          # Baseline metrics (from main)
├── current.json           # Current metrics (from PR)
├── regression-report.json # Detected regressions
├── regression-report.html # HTML visualization
└── regression-summary.txt # Text summary
```

**Git:** `.metrics/` is gitignored (generated at runtime)

## Integration with TCPS

Part of TCPS quality system:

- **Command:** `/tcps-jidoka` - Runs quality checks including regression detection
- **Principle:** 自働化 (Jidoka) - Built-in quality with stop-the-line authority
- **Philosophy:** Stop work when quality issues detected, fix root cause

## Further Reading

- Full documentation: `docs/regression/PREVENTION_SYSTEM.md`
- TCPS system: `.claude/TCPS_SYSTEM_COMPLETE.md`
- Quality gates: `docs/quality/QUALITY_GATES.md`

## Support

Issues or questions:
1. Check `docs/regression/PREVENTION_SYSTEM.md`
2. Review examples in allowlist
3. Ask in #engineering channel
4. Create GitHub issue with `regression-system` label
