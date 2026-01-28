# Blocking Quality Gates - CI/CD Enforcement

## Overview

erlmcp enforces **Lean Six Sigma quality standards** through automated CI/CD quality gates. These gates **BLOCK merges** when code doesn't meet production standards, ensuring zero-defect delivery.

**Philosophy:** If it doesn't pass quality gates, it doesn't get merged. Period.

---

## Quality Gate Summary

| # | Gate | Threshold | Blocking | Workflow |
|---|------|-----------|----------|----------|
| 1 | Compilation | 0 errors | ✅ YES | `ci.yml`, `quality-gate.yml` |
| 2 | Xref (Cross-Reference) | 0 undefined functions | ✅ YES | `ci.yml`, `quality-gate.yml` |
| 3 | Dialyzer (Type Checking) | 0 type errors | ✅ YES | `ci.yml`, `quality-gate.yml` |
| 4 | Unit Tests (EUnit) | ≥90% pass rate | ✅ YES | `ci.yml`, `quality-gate.yml` |
| 5 | Code Coverage | ≥80% overall | ✅ YES | `ci.yml`, `quality-gate.yml` |
| 6 | Performance Regression | <10% degradation | ✅ YES | `block-on-regression.yml` |
| 7 | Integration Tests (CT) | All suites pass | ⚠️ NO | `ci.yml`, `quality-gate.yml` |
| 8 | Documentation Lint | No legacy refs | ⚠️ NO | `ci.yml` |
| 9 | Umbrella Structure | All apps present | ✅ YES | `ci.yml` |

---

## 1. Compilation Gate

**Requirement:** Zero compilation errors

**How it works:**
```bash
TERM=dumb rebar3 compile
```

**Failure scenarios:**
- Syntax errors in Erlang code
- Missing modules or header files
- Undefined macros or records
- Compiler crashes

**How to fix:**
1. Read compilation error messages
2. Fix syntax issues in indicated files
3. Ensure all dependencies are in `rebar.config`
4. Run `make compile` locally to verify
5. Re-push

**Local testing:**
```bash
make compile
# OR
TERM=dumb rebar3 compile
```

---

## 2. Xref Gate (Cross-Reference Analysis)

**Requirement:** Zero undefined function calls

**How it works:**
```bash
rebar3 xref
```

**Checks:**
- Undefined function calls
- Deprecated function usage
- Unused local functions
- Cross-application dependencies

**Failure scenarios:**
- Calling functions that don't exist
- Typos in function names
- Missing dependencies
- Incorrect arity

**How to fix:**
1. Review xref error messages
2. Check function spelling and arity
3. Ensure modules are compiled and in path
4. Add missing dependencies to `rebar.config`
5. Update `xref_ignores` in `rebar.config` if intentional (e.g., dynamic calls)

**Local testing:**
```bash
make xref
# OR
rebar3 xref
```

---

## 3. Dialyzer Gate (Type Checking)

**Requirement:** Zero type errors

**How it works:**
```bash
rebar3 dialyzer
```

**Checks:**
- Type mismatches
- Incorrect return types
- Invalid pattern matches
- Opaque type violations
- Behavior contract violations

**Failure scenarios:**
- Return type doesn't match spec
- Function called with wrong types
- Pattern match always fails
- Behavior callback missing

**How to fix:**
1. Read dialyzer warnings carefully
2. Add/fix type specs (`-spec` declarations)
3. Fix type mismatches in code
4. Ensure behavior callbacks are implemented correctly
5. Run `make dialyzer` locally to verify

**Local testing:**
```bash
make dialyzer
# OR
rebar3 dialyzer
```

**Note:** OTP 28 (experimental) allows dialyzer warnings without blocking.

---

## 4. Unit Tests Gate (EUnit)

**Requirement:** ≥90% pass rate

**How it works:**
```bash
rebar3 as test do compile, eunit --cover
```

**Failure scenarios:**
- Test failures (assertion failures)
- Test crashes (unhandled exceptions)
- Timeout in tests
- <90% pass rate

**How to fix:**
1. Review test failure messages
2. Run failing tests locally: `rebar3 eunit --module=MODULE_tests`
3. Debug test assertions
4. Fix code or update test expectations
5. Ensure test coverage ≥90%

**Local testing:**
```bash
make test
# OR
rebar3 eunit
# OR
rebar3 eunit --module=erlmcp_client_tests
```

---

## 5. Code Coverage Gate

**Requirement:** ≥80% overall coverage

**How it works:**
```bash
rebar3 cover --verbose
./scripts/check_coverage_threshold.sh 80
```

**Critical modules (require ≥90%):**
- `tcps_andon`
- `tcps_root_cause`
- `tcps_receipt_verifier`
- `tcps_rebar3_quality`

**Failure scenarios:**
- Overall coverage <80%
- Critical module coverage <90%
- Untested code paths
- Missing test modules

**How to fix:**
1. Run coverage report locally: `make coverage`
2. Review HTML coverage reports in `_build/test/cover/`
3. Identify uncovered lines
4. Write tests for uncovered code paths
5. Verify coverage: `./scripts/check_coverage_threshold.sh 80`

**Local testing:**
```bash
make coverage
# OR
rebar3 do eunit --cover, cover --verbose
./scripts/check_coverage_threshold.sh 80
```

**Coverage report location:**
- HTML: `_build/test/cover/*.html`
- Log: `_build/test/cover/cover.log`

---

## 6. Performance Regression Gate

**Requirement:** <10% throughput degradation vs. base branch

**How it works:**
1. Run `erlmcp_bench_core_ops:run(<<"core_ops_100k">>)` on PR branch
2. Run same benchmark on base branch (main)
3. Compare throughput
4. Block if degradation >10%

**Failure scenarios:**
- Throughput drops >10% from base
- Benchmark fails to execute
- Memory usage increases significantly

**How to fix:**
1. Review PR comment with benchmark results
2. Investigate performance-critical code changes
3. Profile code: `make observer` or use `fprof`
4. Optimize hot paths
5. Run benchmarks locally: `make benchmark-quick`
6. If regression is intentional, document why in PR

**Local testing:**
```bash
make benchmark-quick
# OR
rebar3 shell --eval "
  application:ensure_all_started(erlmcp_core),
  erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>).
"
```

**Interpreting results:**
- **Green:** <10% change (acceptable)
- **Yellow:** ±10-20% change (investigate)
- **Red:** >10% regression (BLOCKED)

---

## 7. Integration Tests Gate (Non-Blocking)

**Requirement:** All CT suites pass (warning only)

**How it works:**
```bash
rebar3 ct --dir=test/integration --cover
```

**Note:** This gate is **non-blocking** because integration tests may depend on external services.

**How to fix warnings:**
1. Run CT locally: `make ct`
2. Review test logs in `_build/test/logs/`
3. Fix failing test cases
4. Ensure external dependencies are available

---

## 8. Documentation Lint Gate (Non-Blocking)

**Requirement:** No legacy file references or ambiguous metrics

**How it works:**
```bash
./tools/docs_lint.sh
```

**Common issues:**
- Legacy file references (benchmark_100k, throughput_SUITE, etc.)
- Ambiguous metric terms (req/s without context)
- Invalid workload IDs

**How to fix:**
1. Run linter locally: `./tools/docs_lint.sh`
2. Update documentation to use current file names
3. Use canonical metrics (msg/s, us, MiB)
4. Update CHANGELOG.md if needed

---

## 9. Umbrella Structure Gate

**Requirement:** All 4 applications must exist with valid structure

**How it works:**
- Checks `apps/erlmcp_core`, `apps/erlmcp_transports`, etc.
- Verifies `.app.src` files
- Confirms umbrella `rebar.config` has `project_app_dirs`

**Failure scenarios:**
- Missing application directory
- Missing `.app.src` file
- Incorrect umbrella configuration

**How to fix:**
1. Ensure all 4 apps exist in `apps/` directory
2. Verify each app has `src/<app>.app.src`
3. Check `rebar.config` has `{project_app_dirs, ["apps/*"]}`

---

## Workflow Files

### `.github/workflows/ci.yml`
**Purpose:** Main CI pipeline with blocking quality gates

**Runs on:**
- Push to main, release/**, feature/**, task/**, epic/**
- Pull requests to same branches

**Erlang versions tested:** 25, 26, 27, 28

**Jobs:**
- `test` - Compilation, xref, dialyzer, eunit, coverage, benchmark
- `docs-lint` - Documentation linting
- `umbrella-structure-check` - Umbrella verification
- `quality-gates` - Final summary

**Key feature:** Each gate sets output variables and exits on failure

### `.github/workflows/quality-gate.yml`
**Purpose:** Comprehensive single-job quality check

**Runs on:**
- Push to main, release/**, feature/**, task/**, epic/**
- Pull requests to same branches

**Erlang version:** 26 (canonical version)

**Steps:**
1. Gate 1: Compilation
2. Gate 2: Xref
3. Gate 3: Dialyzer
4. Gate 4: Unit Tests (≥90%)
5. Gate 5: Coverage (≥80%)
6. Gate 6: Integration Tests (non-blocking)
7. Generate final report with commit status

**Output:** Detailed GITHUB_STEP_SUMMARY with pass/fail status

### `.github/workflows/block-on-regression.yml`
**Purpose:** Performance regression blocker

**Runs on:**
- Pull requests to main, release/**

**Benchmark:** `core_ops_100k` (100K operations)

**Steps:**
1. Run benchmark on PR branch
2. Checkout base branch
3. Run benchmark on base branch
4. Compare throughput
5. Block if >10% regression
6. Comment on PR with results

**Output:** PR comment with benchmark comparison table

---

## How CI/CD Blocks Merges

### GitHub Branch Protection Rules

To enable blocking behavior, configure these in GitHub repository settings:

**Branch:** `main`

**Required status checks:**
- `test (25)` - CI on Erlang/OTP 25
- `test (26)` - CI on Erlang/OTP 26
- `test (27)` - CI on Erlang/OTP 27
- `test (28)` - CI on Erlang/OTP 28 (optional)
- `Quality Gates Summary` - Final summary job
- `Comprehensive Quality Gate (Blocking)` - quality-gate.yml
- `Benchmark Regression Analysis (Blocking)` - block-on-regression.yml

**Settings:**
```
☑ Require status checks to pass before merging
☑ Require branches to be up to date before merging
☑ Require conversation resolution before merging (optional)
☑ Require linear history (optional)
☐ Do not allow bypassing the above settings
```

### Configuration Steps

1. Go to repository **Settings** → **Branches**
2. Click **Add branch protection rule**
3. Branch name pattern: `main`
4. Enable "Require status checks to pass before merging"
5. Search and select required checks:
   - `test (25)`
   - `test (26)`
   - `test (27)`
   - `Quality Gates Summary`
   - `Comprehensive Quality Gate (Blocking)`
   - `Benchmark Regression Analysis (Blocking)`
6. Enable "Require branches to be up to date"
7. Save changes

**Result:** Pull requests cannot be merged until all required checks pass.

---

## Emergency Bypass Process

**Use only in production emergencies** (e.g., security hotfix).

### Option 1: Admin Override
1. Admins can click "Merge without waiting for requirements"
2. Document reason in merge commit message
3. Create follow-up issue to fix quality gates
4. Tag with `tech-debt` label

### Option 2: Temporary Bypass
1. Admin disables branch protection temporarily
2. Merge emergency fix
3. Re-enable branch protection immediately
4. Document in incident log

### Option 3: Hotfix Branch
1. Create `hotfix/*` branch (not protected)
2. Merge to main with admin override
3. Create follow-up PR to fix issues
4. Run full quality gates on follow-up

**Important:** All bypasses must be documented and followed up.

---

## Local Development Workflow

### Before pushing code

```bash
# 1. Compile
make compile

# 2. Run tests
make test

# 3. Check coverage
make coverage
./scripts/check_coverage_threshold.sh 80

# 4. Type checking
make dialyzer

# 5. Cross-reference
make xref

# 6. Full check (all gates)
make check-full

# 7. Quick benchmark (if perf-critical code changed)
make benchmark-quick
```

### If quality gates fail

1. **Compilation errors:**
   - Fix syntax issues
   - Run `make compile` until clean

2. **Test failures:**
   - Run specific test: `rebar3 eunit --module=MODULE_tests`
   - Debug with `io:format/2` or debugger
   - Fix code or test expectations

3. **Coverage <80%:**
   - Run `make coverage`
   - Open HTML reports: `_build/test/cover/*.html`
   - Write tests for uncovered lines

4. **Dialyzer warnings:**
   - Add type specs to functions
   - Fix type mismatches
   - Run `make dialyzer` until clean

5. **Xref warnings:**
   - Fix undefined function calls
   - Add missing dependencies
   - Update `xref_ignores` if needed

6. **Performance regression:**
   - Profile code: `make observer`
   - Optimize hot paths
   - Run `make benchmark-quick` to verify

---

## Interpreting CI/CD Failures

### Reading GitHub Actions Logs

1. Click **Details** next to failing check
2. Expand failed step (red X)
3. Read error messages in logs
4. Download artifacts if needed

### Common error patterns

**Compilation failure:**
```
Error: file.erl:42: undefined function foo/1
```
→ Function `foo/1` doesn't exist. Check spelling/arity.

**Xref failure:**
```
Warning: erlmcp_client.erl calls undefined function erlmcp_unknown:bar/2
```
→ `erlmcp_unknown:bar/2` doesn't exist. Fix call or add module.

**Dialyzer failure:**
```
Warning: erlmcp_server.erl:123: The pattern {'error', _} can never match
the type {'ok', term()}
```
→ Pattern match is incorrect. Fix return type.

**Test failure:**
```
erlmcp_client_tests:150: test_connection_timeout_FAILED
Expected: {error, timeout}
Actual: {ok, connected}
```
→ Test assertion failed. Fix code or test expectation.

**Coverage failure:**
```
✗ Overall coverage FAILED (75% < 80%)
```
→ Add more tests to reach 80% coverage.

**Performance regression:**
```
Performance regression detected: -15.3% (threshold: -10%)
```
→ Code is 15% slower. Optimize or document why.

---

## Best Practices

### 1. Test locally before pushing
- Run `make check-full` before every push
- Don't rely on CI to catch basic issues
- Faster feedback loop

### 2. Fix issues incrementally
- Don't try to fix all gates at once
- Focus on one gate at a time
- Commit fixes separately

### 3. Monitor CI runs
- Watch CI status in GitHub
- Fix failures immediately
- Don't merge until green

### 4. Document intentional regressions
- If performance regression is acceptable, document why
- Add comment to PR explaining tradeoff
- Update benchmarks if baseline changed

### 5. Keep coverage high
- Write tests for all new code
- Maintain ≥80% overall coverage
- Critical modules: ≥90%

### 6. Don't bypass gates
- Emergency bypass only for production hotfixes
- Document all bypasses
- Create follow-up issues

---

## Troubleshooting

### "Status check required but not running"

**Cause:** Workflow file not triggered

**Fix:**
1. Check `.github/workflows/*.yml` trigger conditions
2. Ensure branch matches `on.push.branches` or `on.pull_request.branches`
3. Push new commit to re-trigger

### "Coverage script not found"

**Cause:** `scripts/check_coverage_threshold.sh` missing

**Fix:**
1. Ensure script exists in repository
2. Check file permissions: `chmod +x scripts/check_coverage_threshold.sh`
3. Commit and push

### "Benchmark execution failed"

**Cause:** Benchmark module not compiled or crashed

**Fix:**
1. Ensure `erlmcp_bench_core_ops` is compiled
2. Check for runtime errors in benchmark code
3. Run locally: `rebar3 shell` then `erlmcp_bench_core_ops:run(<<"core_ops_1k">>).`

### "Dialyzer PLT out of date"

**Cause:** PLT cache stale

**Fix:**
1. GitHub Actions should rebuild automatically
2. Locally: `rm -rf _build/default/*_plt` then `rebar3 dialyzer`

### "Cannot merge - required checks failed"

**Cause:** One or more blocking gates failed

**Fix:**
1. Review failed checks in PR
2. Fix issues locally
3. Re-push
4. Wait for green CI

---

## Metrics and Monitoring

### Quality Gate Pass Rates

**Target:** ≥95% first-time pass rate

**Monitor:**
- Track gate failures over time
- Identify common failure patterns
- Improve developer education

### Average Time to Green

**Target:** <15 minutes per push

**Monitor:**
- CI execution time
- Time from push to green status
- Optimize slow steps

### Bypass Frequency

**Target:** <1 bypass per month

**Monitor:**
- Count emergency bypasses
- Document reasons
- Reduce bypass need over time

---

## Summary

**erlmcp's CI/CD quality gates ensure zero-defect delivery by:**

1. **Blocking merges** on compilation, xref, dialyzer, test, coverage, and performance failures
2. **Providing clear feedback** in PR comments and GitHub summaries
3. **Enforcing standards** through branch protection rules
4. **Enabling local testing** with `make check-full`
5. **Supporting emergencies** with documented bypass process

**Result:** Production-ready code, every commit.

**Questions?** See `.github/workflows/*.yml` or open an issue.
