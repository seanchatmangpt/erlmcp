# Regression Prevention System - Documentation Index

Complete documentation for erlmcp's quality regression prevention system.

## Quick Links

- **[Quick Start Guide](QUICK_START.md)** - Get started in 5 minutes
- **[Prevention System](PREVENTION_SYSTEM.md)** - Complete technical documentation
- **[Tools README](../../tools/regression/README.md)** - Tool usage reference

## What is the Regression Prevention System?

An automated quality gate that detects and blocks code changes that would degrade:
- Test pass rates
- Code coverage
- Performance
- Code complexity
- Build quality

## How It Works

```
PR Created → Metrics Collected → Regressions Detected → Quality Gate → Merge Decision
                ↓                        ↓                    ↓              ↓
         (main vs PR)            (compare & score)      (block/warn)    (pass/fail)
```

## Key Features

1. **Automatic Detection** - Runs on every PR via GitHub Actions
2. **Blocking Quality Gates** - Prevents bad code from merging
3. **Detailed Reports** - HTML reports with recommendations
4. **Controlled Exceptions** - Allowlist for justified regressions
5. **TCPS Integration** - Part of Jidoka (自働化) quality system

## Severity Levels

| Level | Impact | Action | Approval |
|-------|--------|--------|----------|
| Critical | >5% test drop, >20% perf loss | BLOCK | VP Engineering |
| High | >20% warnings, >10% perf loss | BLOCK | Tech Lead + 2 Sr |
| Medium | 2-5% test drop, 5-10% coverage loss | WARN | Tech Lead |
| Low | Minor degradation | INFO | Reviewer |

## Files & Structure

### Tools (in `tools/regression/`)
- `detect-regression.sh` - Metric collection and comparison
- `block-on-regression.sh` - Quality gate enforcement
- `generate-html-report.sh` - HTML report generator
- `allowlist.json` - Permitted exceptions
- `test-system.sh` - Validation tests
- `README.md` - Tool reference

### Documentation (in `docs/regression/`)
- `QUICK_START.md` - 5-minute getting started guide
- `PREVENTION_SYSTEM.md` - Complete technical documentation
- `INDEX.md` - This file

### CI/CD (in `.github/workflows/`)
- `regression-guard.yml` - GitHub Actions workflow

### Generated (in `.metrics/`)
- `baseline.json` - Baseline metrics from main
- `current.json` - Current PR metrics
- `regression-report.json` - Detected regressions
- `regression-report.html` - Visual report
- `regression-summary.txt` - Text summary

## Usage Examples

### Developer Workflow

```bash
# Before creating PR
./tools/regression/detect-regression.sh
./tools/regression/block-on-regression.sh

# View detailed report
./tools/regression/generate-html-report.sh
open .metrics/regression-report.html

# Fix regressions or add to allowlist
vim tools/regression/allowlist.json
```

### CI/CD Workflow

```yaml
# Automatically runs on PR
on: pull_request
  - Collect baseline from main
  - Collect current from PR
  - Detect regressions
  - Post comment
  - Set status check
```

### Allowlist Management

```json
{
  "metric": "performance",
  "severity": "medium",
  "justification": "Trade performance for correctness",
  "approved_by": "tech-lead",
  "approved_date": "2026-01-28",
  "expires": "2026-03-28",
  "ticket": "ERLMCP-456"
}
```

## Metrics Tracked

1. **Test Pass Rate** - Percentage of passing tests
2. **Code Coverage** - Lines covered by tests
3. **Compilation Warnings** - Build warning count
4. **Performance** - Throughput (ops/sec)
5. **Complexity** - Average module size

## Integration with TCPS

Part of Toyota Code Production System (TCPS):

- **Principle:** 自働化 (Jidoka) - Automation with human intelligence
- **Practice:** Stop-the-line authority on quality issues
- **Command:** `/tcps-jidoka` - Runs quality checks
- **Philosophy:** Build quality in, don't inspect it in later

## Common Scenarios

### Scenario 1: PR Blocked
```
❌ Quality Gate: FAILED
Critical regressions detected

Action: Fix issues or request override
```

### Scenario 2: PR Warning
```
⚠️ Quality Gate: PASSED WITH WARNINGS
Medium regressions require justification

Action: Document reason in PR description
```

### Scenario 3: PR Passing
```
✅ Quality Gate: PASSED
No regressions detected

Action: Merge when ready
```

## Best Practices

### DO
- Run locally before creating PR
- Fix regressions immediately
- Document intentional tradeoffs
- Keep allowlist minimal
- Review trends regularly

### DON'T
- Bypass quality gates without approval
- Add allowlist entries without justification
- Ignore medium severity warnings
- Update baseline to hide regressions
- Disable the system

## Troubleshooting

### Problem: Flaky Tests
**Solution:** Fix the flaky tests, don't allowlist

### Problem: False Positive
**Solution:** Check baseline generation, verify test stability

### Problem: Can't Reproduce
**Solution:** Run in clean environment, check dependencies

### Problem: Performance Variance
**Solution:** Use longer runs, multiple iterations, dedicated hardware

## Getting Help

1. Read [QUICK_START.md](QUICK_START.md)
2. Check [PREVENTION_SYSTEM.md](PREVENTION_SYSTEM.md)
3. Review [tools/regression/README.md](../../tools/regression/README.md)
4. Ask in #engineering channel
5. Create issue with `regression-system` label

## Related Documentation

- TCPS System: `.claude/TCPS_SYSTEM_COMPLETE.md`
- Quality Gates: `docs/quality/QUALITY_GATES.md` (if exists)
- Build System: `docs/BUILD_SYSTEM.md` (if exists)
- Testing: `docs/testing/TESTING.md` (if exists)

## Version History

- v1.0.0 (2026-01-28) - Initial release
  - Automatic regression detection
  - Quality gate enforcement
  - HTML report generation
  - GitHub Actions integration
  - Allowlist management

## Future Enhancements

Planned improvements:
1. Machine learning prediction
2. Auto-fix suggestions
3. Trend analysis
4. Security metrics
5. Dependency tracking
6. Multi-branch support
7. Custom threshold configuration

---

**Remember:** The goal is quality, not blocking. Use the system to ship better code!
