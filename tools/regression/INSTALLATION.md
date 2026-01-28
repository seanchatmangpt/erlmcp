# Regression Prevention System - Installation & Setup

Zero-configuration system for quality regression prevention in erlmcp.

## Prerequisites

The system works out-of-the-box on most Unix-like systems with:

### Required
- **Bash 4.0+** - Shell scripting
- **Python 3.6+** - JSON validation (fallback)
- **bc** - Floating point calculations
- **git** - Version control
- **rebar3** - Erlang build tool

### Optional (Enhanced Features)
- **jq** - JSON processing (recommended, Python fallback available)
- **Erlang/OTP 25+** - For running benchmarks

## Installation

### 1. System is Pre-Installed

No installation needed! All tools are included in the repository:

```bash
ls -l tools/regression/
# detect-regression.sh
# block-on-regression.sh
# generate-html-report.sh
# allowlist.json
# README.md
```

### 2. Verify Setup

```bash
# Run system tests
./tools/regression/test-system.sh
```

Expected output:
```
Testing Regression Prevention System
=====================================

Test 1: Script syntax validation... PASS
Test 2: Scripts are executable... PASS
Test 3: Allowlist is valid JSON... PASS
...
Test 10: File structure validation... PASS

All tests passed!
```

### 3. Install Dependencies (if needed)

#### macOS
```bash
# Using Homebrew
brew install bc jq

# Using MacPorts
sudo port install bc jq
```

#### Ubuntu/Debian
```bash
sudo apt-get update
sudo apt-get install bc jq
```

#### RHEL/CentOS/Fedora
```bash
sudo yum install bc jq
# or
sudo dnf install bc jq
```

#### Alpine Linux
```bash
apk add bc jq
```

### 4. Verify Dependencies

```bash
# Check all dependencies
command -v bash && echo "✓ bash"
command -v python3 && echo "✓ python3"
command -v bc && echo "✓ bc"
command -v git && echo "✓ git"
command -v rebar3 && echo "✓ rebar3"
command -v jq && echo "✓ jq (optional)"
```

## GitHub Actions Setup

### Automatic (Already Configured)

The system is pre-configured via `.github/workflows/regression-guard.yml`:

```yaml
name: Regression Guard
on: pull_request
  branches: [main, develop]
```

### Verify Workflow

```bash
# Check workflow file exists
ls -l .github/workflows/regression-guard.yml

# Validate workflow syntax
gh workflow view regression-guard
```

### Enable for Repository

1. Push the repository to GitHub
2. Workflow runs automatically on PRs
3. No additional configuration needed

### Required Permissions

Ensure GitHub Actions has:
- `contents: read` - Read repository
- `pull-requests: write` - Post comments
- `statuses: write` - Set status checks

Already configured in workflow file.

## First Run

### Create Baseline

```bash
# Switch to main branch
git checkout main

# Run detection (creates baseline)
./tools/regression/detect-regression.sh
```

Output:
```
[INFO] Collecting current metrics...
[SUCCESS] Compilation passed with 0 warnings
[SUCCESS] Test pass rate: 100.00%
[WARN] No baseline file found. Creating baseline from current metrics.
[SUCCESS] Regression detection complete - No issues found
```

### Test on Branch

```bash
# Create test branch
git checkout -b test-regression

# Make a change (e.g., add failing test)
# ...

# Run detection (compares to baseline)
./tools/regression/detect-regression.sh
```

### View Results

```bash
# Generate HTML report
./tools/regression/generate-html-report.sh

# Open in browser
open .metrics/regression-report.html
```

## Configuration

### Adjust Thresholds

Edit `tools/regression/detect-regression.sh`:

```bash
# Current thresholds
CRITICAL_TEST_PASS_DROP=5      # % drop in test pass rate
CRITICAL_COVERAGE_DROP=10      # % drop in code coverage
CRITICAL_PERF_DROP=20          # % performance degradation
HIGH_WARNING_INCREASE=20       # % increase in warnings
MEDIUM_COMPLEXITY_INCREASE=15  # % increase in complexity
```

### Customize Metrics

Add new metrics in `collect_current_metrics()`:

```bash
# Example: Add dialyzer warning count
dialyzer_warnings=$(rebar3 dialyzer 2>&1 | grep -c "Warning:" || echo "0")
```

### Configure Allowlist

Edit `tools/regression/allowlist.json`:

```json
{
  "allowed_regressions": [
    {
      "metric": "your_metric",
      "severity": "medium",
      "justification": "Why this is acceptable",
      "approved_by": "your-name",
      "approved_date": "2026-01-28",
      "expires": "2026-03-28",
      "ticket": "TICKET-123"
    }
  ]
}
```

## Integration with CI/CD

### GitHub Actions (Default)

Already configured. See `.github/workflows/regression-guard.yml`.

### GitLab CI

```yaml
# .gitlab-ci.yml
regression-check:
  stage: test
  script:
    - ./tools/regression/detect-regression.sh
    - ./tools/regression/block-on-regression.sh
  only:
    - merge_requests
```

### Jenkins

```groovy
// Jenkinsfile
stage('Regression Check') {
    steps {
        sh './tools/regression/detect-regression.sh'
        sh './tools/regression/block-on-regression.sh'
    }
}
```

### CircleCI

```yaml
# .circleci/config.yml
jobs:
  regression-check:
    steps:
      - run: ./tools/regression/detect-regression.sh
      - run: ./tools/regression/block-on-regression.sh
```

## Local Git Hooks

### Pre-Push Hook

```bash
# .git/hooks/pre-push
#!/bin/bash
./tools/regression/detect-regression.sh
exit_code=$?

if [[ $exit_code -ge 3 ]]; then
    echo "Critical regressions detected! Push blocked."
    exit 1
fi

exit 0
```

### Pre-Commit Hook

```bash
# .git/hooks/pre-commit
#!/bin/bash
# Quick syntax check
bash -n tools/regression/*.sh
```

## Troubleshooting Installation

### Problem: Scripts not executable

```bash
chmod +x tools/regression/*.sh
```

### Problem: Dependencies missing

```bash
# Install minimal set
# macOS
brew install bc

# Linux
sudo apt-get install bc

# Python is usually pre-installed
python3 --version
```

### Problem: jq not found

```bash
# jq is optional - system uses python3 fallback
python3 -m json.tool < tools/regression/allowlist.json
```

### Problem: Workflow not running

Check:
1. File exists: `.github/workflows/regression-guard.yml`
2. Branch protection enabled
3. Actions enabled in repository settings
4. Workflow syntax valid: `gh workflow view regression-guard`

## Uninstallation

To remove the system:

```bash
# Remove tools
rm -rf tools/regression/

# Remove documentation
rm -rf docs/regression/

# Remove workflow
rm .github/workflows/regression-guard.yml

# Remove metrics
rm -rf .metrics/
```

**Warning:** This removes all quality gates. Not recommended!

## Next Steps

1. Read [Quick Start Guide](../../docs/regression/QUICK_START.md)
2. Review [Prevention System](../../docs/regression/PREVENTION_SYSTEM.md)
3. Check [Tools README](README.md)
4. Run system test: `./tools/regression/test-system.sh`
5. Create baseline: `./tools/regression/detect-regression.sh`

## Support

- Documentation: `docs/regression/`
- Issues: GitHub Issues with `regression-system` label
- Community: #engineering channel

---

**Ready to use! No installation required.**
