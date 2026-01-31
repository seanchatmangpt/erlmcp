# ERLMCP Pre-Commit Hooks

## Overview

ERLMCP enforces manufacturing-grade quality standards through **mandatory pre-commit quality gates**. Every commit must pass all blocking checks before being allowed to proceed.

## Installation

```bash
./scripts/install-hooks.sh
```

## Quality Gates

### Blocking Gates (Commit will be BLOCKED if these fail)

1. **Compilation** - All Erlang modules must compile with 0 errors
2. **EUnit Tests** - All unit tests must pass (0 failures)
3. **Code Coverage** - Minimum 80% coverage required

### Non-Blocking Gates (Warnings reported but commit allowed)

4. **Dialyzer** - Type checking warnings (should be fixed but not blocking)
5. **Xref** - Undefined function warnings (should be fixed but not blocking)

## Usage

### Normal Commit (with quality gates)

```bash
git add .
git commit -m "feature: implement XYZ"
```

The pre-commit hook will automatically:
1. Compile the project
2. Run EUnit tests
3. Check code coverage
4. Run Dialyzer analysis
5. Run Xref analysis

If all blocking gates pass, the commit proceeds. Otherwise, it's blocked.

### Bypass Quality Gates (Not Recommended)

```bash
git commit --no-verify -m "feature: implement XYZ"
```

**Use sparingly** - only in emergencies or for documentation-only commits.

## Testing

Test the hook without making a commit:

```bash
./scripts/test-commit-hook.sh
```

This runs the full quality gate suite and reports results.

## Artifacts

All quality gate runs generate logs in `test_results/quality_gates/`:

- `compile_YYYYMMDD_HHMMSS.log` - Compilation output
- `eunit_YYYYMMDD_HHMMSS.log` - Test results
- `coverage_YYYYMMDD_HHMMSS.log` - Coverage analysis
- `dialyzer_YYYYMMDD_HHMMSS.log` - Type checking warnings
- `xref_YYYYMMDD_HHMMSS.log` - Cross-reference analysis
- `summary_YYYYMMDD_HHMMSS.txt` - Summary report

## Example Output

```
==========================================
  ERLMCP PRE-COMMIT QUALITY GATES
==========================================

[1/5] COMPILATION CHECK
Compiling project...
✓ PASS - Compilation succeeded (86 modules)

[2/5] EUNIT TESTS
Running EUnit tests...
✓ PASS - All EUnit tests passed (78 tests)

[3/5] CODE COVERAGE
Checking code coverage...
✓ PASS - Coverage: 87.3% (>= 80%)

[4/5] DIALYZER (Type Checking)
Running Dialyzer analysis...
✓ PASS - Dialyzer: 0 warnings

[5/5] XREF (Cross-Reference Check)
Running Xref analysis...
✓ PASS - Xref: 0 undefined functions

==========================================
  QUALITY GATES SUMMARY
==========================================
Passed:  5/5
Warnings: 0
Failed:   0

✓ ALL QUALITY GATES PASSED

Commit allowed!
Logs saved to: test_results/quality_gates
```

## Troubleshooting

### Hook Not Running

If commits proceed without quality gates:

1. Verify hook is installed:
   ```bash
   ls -la .git/hooks/pre-commit
   ls -la .git/hooks/erlmcp-quality-gates
   ```

2. Reinstall:
   ```bash
   ./scripts/install-hooks.sh
   ```

### Compilation Errors

The hook will block with compilation errors. Check:
```bash
cat test_results/quality_gates/compile_*.log
```

Fix compilation errors before committing.

### Test Failures

The hook will block with test failures. Check:
```bash
cat test_results/quality_gates/eunit_*.log
```

Fix failing tests before committing.

### Low Coverage

The hook will block if coverage < 80%. Check:
```bash
rebar3 cover
```

Add tests for uncovered code before committing.

## Lean Six Sigma Integration

These hooks implement **Jidoka (自働化)** - built-in quality:
- Automatic quality inspection on every commit
- Zero-defect delivery enforced
- Problems caught immediately (andon principle)
- Continuous improvement (kaizen) through consistent standards

## Philosophy

> "Quality is not an act, it is a habit." - Aristotle

These quality gates ensure consistent quality by making violations impossible to commit.
