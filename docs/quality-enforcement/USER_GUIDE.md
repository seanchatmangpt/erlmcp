# Quality Gate Enforcement System - User Guide

**Audience:** Developers using the quality gate system
**Version:** 1.0.0
**Last Updated:** 2026-01-28

## Introduction

This guide helps you use the erlmcp Quality Gate Enforcement System in your daily development workflow. Learn how to run quality checks, understand failures, fix violations, and handle emergency situations.

## Quick Start

### Running Quality Checks

**Check all gates for your current work:**
```bash
# Build and run all quality gates
make quality-check

# Or using rebar3 directly
SKU_ID="myfeature-$(git rev-parse --short HEAD)"
rebar3 tcps check-all-gates --sku=$SKU_ID
```

**Check a specific gate:**
```bash
# Check compilation only
rebar3 tcps quality-gate --gate=compilation --sku=$SKU_ID

# Check tests only
rebar3 tcps quality-gate --gate=test_execution --sku=$SKU_ID

# Check security scan
rebar3 tcps quality-gate --gate=security_scan --sku=$SKU_ID
```

**View gate status:**
```bash
# See which gates have passed/failed
rebar3 tcps gate-status --sku=$SKU_ID

# Get detailed status for specific gate
rebar3 tcps gate-status --gate=test_execution --sku=$SKU_ID
```

### Understanding Output

**Successful Gate:**
```
[Gate 2/8] Checking compilation for SKU myfeature-a3b5c7d...
  Compilation passed with 0 warnings
✅ Gate 2/8 PASSED: compilation
   Duration: 12.5s
   Receipt: RCPT-compilation-1706472000-123456
```

**Failed Gate:**
```
[Gate 3/8] Checking test execution for SKU myfeature-a3b5c7d...
  Tests failed: pass_rate=87.5% (need 95.0%), coverage=75.2% (need 80.0%)
❌ Gate 3/8 FAILED: test_execution
   Duration: 45.2s
   Violations: 2
   Andon Event: ANDON-12345

=== Quality Gate Failed: test_execution ===

Violations:
  [test_execution:pass_rate] Line 20:
    ERROR: Test pass rate 87.5% below minimum 95.0%
    HINT:  Fix failing tests or improve test reliability

  [test_execution:coverage] Line 21:
    ERROR: Code coverage 75.2% below minimum 80.0%
    HINT:  Add tests for uncovered code paths
```

## Common Workflows

### 1. Development Cycle

**Before Committing:**
```bash
# 1. Run fast gates locally
rebar3 tcps quality-gate --gate=compilation
rebar3 tcps quality-gate --gate=test_execution --fast

# 2. Fix any failures
# ... edit code, add tests ...

# 3. Re-run gates
rebar3 tcps quality-gate --gate=test_execution

# 4. Commit when all pass
git add .
git commit -m "feat: Add new feature"
```

**After Push (CI):**
```bash
# CI runs all gates automatically
# Check CI logs for detailed output
# View quality gate receipts in artifacts
```

### 2. Fixing Gate Failures

#### Compilation Failures

**Symptoms:**
```
[Gate 2/8] Checking compilation for SKU myfeature-a3b5c7d...
  Compilation failed with 3 errors

ERROR: src/mymodule.erl:42: syntax error before: ')'
ERROR: src/mymodule.erl:58: undefined function foo/1
ERROR: src/mymodule.erl:72: variable 'X' is unbound
```

**How to Fix:**
1. **View detailed errors:**
   ```bash
   rebar3 compile --verbose
   ```

2. **Fix syntax errors:** Check line numbers, fix typos, balance parens
3. **Fix undefined functions:** Import or define missing functions
4. **Fix unbound variables:** Initialize variables before use
5. **Re-run gate:**
   ```bash
   rebar3 tcps quality-gate --gate=compilation
   ```

#### Test Execution Failures

**Symptoms:**
```
[Gate 3/8] Checking test execution for SKU myfeature-a3b5c7d...
  Tests failed: pass_rate=90.0% (need 95.0%), coverage=75.0% (need 80.0%)

Failed tests:
  - mymodule_tests:test_edge_case/0
  - mymodule_tests:test_concurrent_access/0
```

**How to Fix:**

**Step 1: Fix failing tests**
```bash
# Run tests with verbose output
rebar3 eunit --module=mymodule_tests --verbose

# Debug specific test
rebar3 eunit --module=mymodule_tests --test=test_edge_case
```

**Step 2: Improve coverage**
```bash
# Check coverage report
rebar3 cover --verbose

# Identify uncovered lines
rebar3 cover --module=mymodule --verbose

# Add tests for uncovered code
# Edit test/mymodule_tests.erl
```

**Step 3: Verify fixes**
```bash
# Re-run test gate
rebar3 tcps quality-gate --gate=test_execution

# Should now see:
# ✅ Gate 3/8 PASSED: test_execution
#    Tests passed: 20/20 (100.0%), coverage: 85.3%
```

#### Security Scan Failures

**Symptoms:**
```
[Gate 4/8] Checking security scan for SKU myfeature-a3b5c7d...
  Security scan failed: 2 issues found

Issues:
  - Dependency vulnerability: jsx 3.0.0 (CVE-2023-12345)
  - Hardcoded secret: src/config.erl:15
```

**How to Fix:**

**Dependency Vulnerabilities:**
```bash
# 1. Check advisory
rebar3 deps audit

# 2. Update vulnerable dependency
# Edit rebar.config:
{jsx, "3.1.0"}  % Fixed version

# 3. Update lockfile
rebar3 upgrade jsx
rebar3 lock

# 4. Re-run security gate
rebar3 tcps quality-gate --gate=security_scan
```

**Hardcoded Secrets:**
```bash
# 1. Find hardcoded secrets
grep -r "password\|secret\|api_key" src/

# 2. Move to environment variables
% Before:
-define(API_KEY, "sk-abc123...").

% After:
get_api_key() ->
    os:getenv("API_KEY", "").

# 3. Update .env file (NOT committed)
echo "API_KEY=sk-abc123..." >> .env

# 4. Re-run security gate
rebar3 tcps quality-gate --gate=security_scan
```

#### Deterministic Build Failures

**Symptoms:**
```
[Gate 5/8] Checking deterministic build for SKU myfeature-a3b5c7d...
  Build is non-deterministic

First hash:  a3b5c7d9e1f2...
Second hash: b4c6d8e0f3g4...

Details: Timestamp embedded in beam file
```

**How to Fix:**

**Common Causes:**
1. **Timestamps in code:**
   ```erlang
   % Before (non-deterministic):
   -vsn(erlang:system_time()).

   % After (deterministic):
   -vsn("1.0.0").
   ```

2. **Random values at compile time:**
   ```erlang
   % Before:
   -define(REQUEST_ID, rand:uniform(999999)).

   % After:
   -define(REQUEST_ID_PREFIX, "REQ-").
   request_id() -> ?REQUEST_ID_PREFIX ++ integer_to_list(rand:uniform(999999)).
   ```

3. **File modification times:**
   ```bash
   # Use SOURCE_DATE_EPOCH
   export SOURCE_DATE_EPOCH=1706472000
   rebar3 compile
   ```

### 3. Improving Quality Metrics

**View current metrics:**
```bash
rebar3 tcps quality-metrics

# Output:
# Test pass rate: 92.5% (target: 95%)
# Test coverage:  78.3% (target: 80%)
# Defect rate:    3.2% (target: <5%)
# First pass yield: 88.0% (target: 90%)
```

**Improve test pass rate:**
1. Fix flaky tests (add retries, stabilize timing)
2. Remove obsolete tests
3. Add missing assertions
4. Improve test isolation

**Improve test coverage:**
1. Identify uncovered code:
   ```bash
   rebar3 cover --verbose
   ```
2. Add unit tests for uncovered functions
3. Add integration tests for uncovered scenarios
4. Remove dead code

**Improve first pass yield:**
1. Run quality gates locally before push
2. Set up pre-commit hooks
3. Fix issues early (don't wait for CI)
4. Learn from recurring failures

## Pre-Commit Hooks

### Installing Hooks

**Automatic Setup:**
```bash
# Install git hooks
make install-hooks

# Or manually:
cp scripts/hooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

**Hook Content:**
```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Running pre-commit quality checks..."

# Gate 1: Compilation
rebar3 tcps quality-gate --gate=compilation
if [ $? -ne 0 ]; then
    echo "❌ Compilation failed. Fix errors before committing."
    exit 1
fi

# Gate 2: Fast tests (unit tests only)
rebar3 tcps quality-gate --gate=test_execution --fast
if [ $? -ne 0 ]; then
    echo "❌ Fast tests failed. Fix tests before committing."
    exit 1
fi

echo "✅ Pre-commit checks passed"
exit 0
```

### Bypassing Hooks (Emergency Only)

**WARNING:** Only bypass hooks in genuine emergencies. Document reason in commit message.

```bash
# Skip hooks
git commit --no-verify -m "hotfix: Emergency security patch (skipped hooks, reason: prod incident #12345)"

# Log override to audit trail
echo "$(date): Bypassed hooks for commit $(git rev-parse HEAD) - Reason: prod incident #12345" >> .git-hook-overrides.log
```

## CI/CD Integration

### GitHub Actions

**Example workflow:**
```yaml
# .github/workflows/quality-gates.yml
name: Quality Gates

on: [push, pull_request]

jobs:
  quality-gates:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'
          rebar3-version: '3.22'

      - name: Cache Dependencies
        uses: actions/cache@v3
        with:
          path: |
            _build
            ~/.cache/rebar3
          key: ${{ runner.os }}-rebar3-${{ hashFiles('**/rebar.lock') }}

      - name: Run All Quality Gates
        run: |
          SKU_ID="${GITHUB_REF_NAME}-${GITHUB_SHA:0:8}"
          rebar3 tcps check-all-gates --sku=$SKU_ID

      - name: Upload Gate Receipts
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: quality-gate-receipts
          path: priv/receipts/*.json

      - name: Upload Coverage Report
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: coverage-report
          path: _build/test/cover/*.html

      - name: Comment on PR
        if: failure() && github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const report = fs.readFileSync('priv/receipts/latest-failure.json', 'utf8');
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: `❌ Quality gates failed:\n\n\`\`\`json\n${report}\n\`\`\``
            });
```

### GitLab CI

**Example .gitlab-ci.yml:**
```yaml
stages:
  - build
  - test
  - quality-gates

quality-gates:
  stage: quality-gates
  image: erlang:26.2
  before_script:
    - rebar3 --version
  script:
    - SKU_ID="${CI_COMMIT_REF_NAME}-${CI_COMMIT_SHORT_SHA}"
    - rebar3 tcps check-all-gates --sku=$SKU_ID
  artifacts:
    when: always
    paths:
      - priv/receipts/*.json
      - _build/test/cover/*.html
    reports:
      junit: _build/test/logs/junit.xml
```

## Emergency Override

**⚠️ USE WITH EXTREME CAUTION ⚠️**

### When to Override

**Valid Reasons:**
- Production incident requiring immediate hotfix
- Security vulnerability requiring urgent patch
- Regulatory deadline with executive approval

**Invalid Reasons:**
- "Tests are flaky" (fix the tests)
- "Deadline is tight" (poor planning)
- "Coverage is hard" (add tests)

### How to Override

**Step 1: Get approval**
```bash
# Document approval in ticket
# Example: JIRA ticket PROD-12345 approved by CTO
```

**Step 2: Override with justification**
```bash
export TCPS_OVERRIDE=true
export TCPS_OVERRIDE_REASON="Security hotfix for CVE-2026-12345 - approved by CTO in PROD-12345"
export TCPS_OVERRIDE_APPROVER="cto@company.com"
export TCPS_OVERRIDE_TICKET="PROD-12345"

rebar3 tcps quality-gate --gate=test_execution --override
```

**Step 3: Create follow-up ticket**
```bash
# Immediately create ticket to fix properly
# Example: TECH-DEBT-12346: Improve test coverage for security module
```

**Step 4: Override logged automatically**
```json
{
  "override_id": "OVR-1706472000-123456",
  "gate": "test_execution",
  "sku_id": "hotfix-cve-2026-12345",
  "reason": "Security hotfix for CVE-2026-12345 - approved by CTO in PROD-12345",
  "approver": "cto@company.com",
  "ticket": "PROD-12345",
  "timestamp": 1706472000000,
  "user": "alice@company.com"
}
```

## Tips and Best Practices

### 1. Run Gates Locally First

**Before pushing:**
```bash
# Quick checks (1-2 minutes)
rebar3 tcps quality-gate --gate=compilation
rebar3 tcps quality-gate --gate=test_execution --fast

# Full checks (5-10 minutes) - run before PR
make quality-check
```

### 2. Fix Root Causes

**Don't just fix symptoms:**
```bash
# Bad: Commenting out failing test
% test_edge_case() -> skip.

# Good: Fix the test
test_edge_case() ->
    % Add proper setup/teardown
    setup_test_env(),
    Result = mymodule:edge_case(),
    ?assertEqual(expected, Result),
    cleanup_test_env().
```

### 3. Monitor Quality Trends

**Track metrics over time:**
```bash
# Daily quality report
rebar3 tcps quality-metrics --format=json >> metrics/daily-$(date +%Y%m%d).json

# Visualize trends
python scripts/plot_quality_trends.py metrics/
```

### 4. Learn from Failures

**Analyze Andon events:**
```bash
# List all Andon events for your SKU
rebar3 tcps andon-list --sku=$SKU_ID

# View Andon details
rebar3 tcps andon-show --andon-id=ANDON-12345

# Identify patterns
rebar3 tcps andon-report --last-30-days
```

### 5. Improve Test Reliability

**Reduce flaky tests:**
```erlang
% Use proper timeouts
receive
    {response, Data} -> ok
after 5000 ->  % Don't rely on timing
    error(timeout)
end.

% Use test fixtures
setup() ->
    % Create isolated test environment
    ok.

cleanup(_) ->
    % Clean up resources
    ok.

% Use proper assertions
?assertEqual(Expected, Actual),  % Not: Expected == Actual
```

## Getting Help

### Documentation
- [Architecture](ARCHITECTURE.md) - System design
- [Admin Guide](ADMIN_GUIDE.md) - Configuration
- [FAQ](FAQ.md) - Common questions

### Troubleshooting
```bash
# Enable verbose logging
export TCPS_VERBOSE=true
rebar3 tcps quality-gate --gate=test_execution --verbose

# View gate cache
rebar3 tcps cache-dump

# View receipt chain
rebar3 tcps receipt-chain --sku=$SKU_ID
```

### Support Channels
- GitHub Issues: https://github.com/banyan-platform/erlmcp/issues
- Slack: #erlmcp-quality-gates
- Email: quality-gates@erlmcp.dev

---

**Next Steps:**
1. Run your first quality check: `make quality-check`
2. Install pre-commit hooks: `make install-hooks`
3. Review [FAQ](FAQ.md) for common questions

**Version History:**
- v1.0.0 (2026-01-28): Initial user guide
