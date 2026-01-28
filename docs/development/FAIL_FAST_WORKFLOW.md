# Fail-Fast Development Workflow

## Overview

The fail-fast workflow ensures quality issues are detected and fixed immediately during development, preventing defects from accumulating. This document describes the Red-Green-Refactor TDD cycle with automated validation at each step.

## Core Principle: "Fix Before You Add"

**NEVER edit code with failing tests or compilation errors.**

The fail-fast system enforces this through three validation gates:

1. **Pre-Edit Validation** - Ensures clean state before editing
2. **Post-Edit Validation** - Validates changes immediately after editing
3. **Commit Validation** - Full quality gate before committing

## Quality Gates

### Pre-Edit Validation (MANDATORY)

**When to run:** Before starting to edit any file

```bash
./tools/pre-edit-validator.sh [file-to-edit]
```

**What it checks:**
- ✅ Compilation is clean
- ✅ All tests are passing
- ⚠️ Dialyzer warnings (non-blocking)

**Enforcement:**
- BLOCKS editing if compilation fails
- BLOCKS editing if tests fail
- Ensures you start from a clean state

**Example:**
```bash
# Before editing src/erlmcp_client.erl
./tools/pre-edit-validator.sh src/erlmcp_client.erl

✅ Compilation clean
✅ All tests passing
✅ Safe to edit!
```

### Post-Edit Validation (MANDATORY)

**When to run:** Immediately after editing files

```bash
./tools/post-edit-validator.sh src/erlmcp_client.erl test/erlmcp_client_tests.erl
```

**What it checks:**
- ✅ Changed files compile
- ✅ Affected tests pass
- ⚠️ Formatting (non-blocking)
- ⚠️ Dialyzer warnings (non-blocking)

**Enforcement:**
- BLOCKS next edit if compilation fails
- BLOCKS next edit if tests fail
- Forces immediate fix

**Example:**
```bash
# After editing client code
./tools/post-edit-validator.sh src/erlmcp_client.erl

✅ Compilation successful
✅ All affected tests passed
✅ Changes are clean - safe to continue!
```

### Commit Validation (MANDATORY)

**When to run:** Before committing changes

```bash
./tools/commit-validator.sh
```

**What it checks:**
- ✅ Full compilation
- ✅ All EUnit tests
- ✅ All Common Test suites
- ✅ Code formatting
- ⚠️ Dialyzer (non-blocking)
- ⚠️ Xref analysis (non-blocking)
- ⚠️ Coverage ≥80% (non-blocking)

**Enforcement:**
- BLOCKS commit if compilation fails
- BLOCKS commit if any tests fail
- BLOCKS commit if formatting issues
- Can bypass with `git commit --no-verify` (NOT recommended)

**Example:**
```bash
# Before committing
git add src/erlmcp_client.erl test/erlmcp_client_tests.erl
./tools/commit-validator.sh

✅ Compilation clean
✅ All EUnit tests passed
✅ All CT suites passed
✅ Formatting clean
✅ Coverage: 87% (≥80% target)
✅ Safe to commit!
```

## TDD Workflow: Red-Green-Refactor

### Phase 1: RED - Write Failing Test

**Objective:** Write a test that fails because the feature doesn't exist yet

**Steps:**
1. Run pre-edit validation to ensure clean state
2. Write a failing test
3. Verify test fails for the right reason

**Example:**
```bash
# 1. Validate clean state
./tools/pre-edit-validator.sh test/erlmcp_client_tests.erl

# 2. Edit test file - add failing test
vim test/erlmcp_client_tests.erl

# 3. Run test to verify it fails
rebar3 eunit --module=erlmcp_client_tests

❌ Test failed (expected - no implementation yet)
```

**VS Code Task:** Use "RED: Failing Test Mode" task

### Phase 2: GREEN - Make Test Pass

**Objective:** Write minimal code to make the test pass

**Steps:**
1. Run pre-edit validation
2. Implement minimal code to pass test
3. Run post-edit validation
4. Verify test passes

**Example:**
```bash
# 1. Validate before editing
./tools/pre-edit-validator.sh src/erlmcp_client.erl

# 2. Implement feature
vim src/erlmcp_client.erl

# 3. Validate changes
./tools/post-edit-validator.sh src/erlmcp_client.erl

✅ Compilation successful
✅ All tests passed
```

**VS Code Task:** Use "GREEN: Make Tests Pass" task

### Phase 3: REFACTOR - Improve Code

**Objective:** Improve code quality while keeping tests green

**Steps:**
1. Run pre-edit validation (tests must be green)
2. Refactor code
3. Run post-edit validation (tests must stay green)
4. Run commit validation before committing

**Example:**
```bash
# 1. Ensure tests are green
./tools/pre-edit-validator.sh

# 2. Refactor
vim src/erlmcp_client.erl

# 3. Validate refactoring didn't break anything
./tools/post-edit-validator.sh src/erlmcp_client.erl

✅ All validations passed

# 4. Full validation before commit
./tools/commit-validator.sh

✅ Ready to commit
```

**VS Code Task:** Use "REFACTOR: Validate Changes" task

## Watch Mode: Continuous Validation

**For rapid development**, use watch mode to get immediate feedback:

```bash
./tools/watch-and-validate.sh
```

**What it does:**
- Watches `src/` and `test/` directories
- On file change:
  - Compiles changed file
  - Runs affected tests
  - Shows results in <10s
- Provides real-time feedback

**Example session:**
```bash
./tools/watch-and-validate.sh

ℹ️  Starting watch mode...
ℹ️  Watching: src/ and test/
ℹ️  Press Ctrl+C to stop

[File changed: src/erlmcp_client.erl]
ℹ️  Compiling: src/erlmcp_client.erl
✅ Compiled in 342ms
ℹ️  Running tests: erlmcp_client_tests
✅ Tests passed in 1847ms
✅ All validations passed! ✨
```

**VS Code Task:** Use "Watch and Validate" task

## IDE Integration (VS Code)

### Keyboard Shortcuts

Add to `.vscode/keybindings.json`:

```json
[
  {
    "key": "ctrl+shift+p",
    "command": "workbench.action.tasks.runTask",
    "args": "Pre-Edit Validation"
  },
  {
    "key": "ctrl+shift+o",
    "command": "workbench.action.tasks.runTask",
    "args": "Post-Edit Validation"
  },
  {
    "key": "ctrl+shift+c",
    "command": "workbench.action.tasks.runTask",
    "args": "Commit Validation"
  },
  {
    "key": "ctrl+shift+w",
    "command": "workbench.action.tasks.runTask",
    "args": "Watch and Validate"
  }
]
```

### Available Tasks

All tasks are defined in `.vscode/tasks.json`:

- **Pre-Edit Validation** - Run before editing
- **Post-Edit Validation** - Run after editing
- **Watch and Validate** - Continuous validation
- **Commit Validation** - Full validation before commit
- **RED: Failing Test Mode** - Write failing tests
- **GREEN: Make Tests Pass** - Validate implementation
- **REFACTOR: Validate Changes** - Validate refactoring

## Validation Timing Targets

All validations should complete quickly to maintain flow:

| Validation | Target Time | What It Checks |
|------------|-------------|----------------|
| Pre-Edit | <5s | Compilation + existing tests |
| Post-Edit | <10s | Changed files + affected tests |
| Watch Mode | <10s | Single file + its tests |
| Commit | <30s | Full suite + formatting + dialyzer |

## Common Scenarios

### Scenario 1: Adding a New Feature

```bash
# 1. Start from clean state
./tools/pre-edit-validator.sh

# 2. Write failing test (RED)
vim test/new_feature_tests.erl
rebar3 eunit --module=new_feature_tests
# Expected: ❌ Test fails

# 3. Implement feature (GREEN)
vim src/new_feature.erl
./tools/post-edit-validator.sh src/new_feature.erl
# Expected: ✅ Tests pass

# 4. Refactor (REFACTOR)
vim src/new_feature.erl
./tools/post-edit-validator.sh src/new_feature.erl
# Expected: ✅ Tests still pass

# 5. Commit
git add src/new_feature.erl test/new_feature_tests.erl
./tools/commit-validator.sh
git commit -m "Add new feature"
```

### Scenario 2: Fixing a Bug

```bash
# 1. Write regression test (RED)
vim test/bug_fix_tests.erl
rebar3 eunit --module=bug_fix_tests
# Expected: ❌ Test reproduces bug

# 2. Fix bug (GREEN)
vim src/buggy_module.erl
./tools/post-edit-validator.sh src/buggy_module.erl
# Expected: ✅ Regression test passes

# 3. Commit fix
./tools/commit-validator.sh
git commit -m "Fix bug XYZ"
```

### Scenario 3: Refactoring

```bash
# 1. Ensure tests are green
./tools/pre-edit-validator.sh
# MUST pass before refactoring

# 2. Refactor code
vim src/module_to_refactor.erl

# 3. Validate tests stay green
./tools/post-edit-validator.sh src/module_to_refactor.erl

# 4. Commit refactoring
./tools/commit-validator.sh
git commit -m "Refactor: improve code structure"
```

## Git Hook Integration

### Pre-Commit Hook (Automatic)

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Pre-commit hook - runs commit validation

./tools/commit-validator.sh

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Commit blocked - fix issues and try again"
    echo "To bypass (NOT recommended): git commit --no-verify"
    exit 1
fi

exit 0
```

**Make executable:**
```bash
chmod +x .git/hooks/pre-commit
```

**What it does:**
- Runs automatically on `git commit`
- Blocks commit if validation fails
- Forces fix before allowing commit

### Pre-Push Hook (Optional)

Create `.git/hooks/pre-push`:

```bash
#!/bin/bash
# Pre-push hook - full quality check

cd "$(git rev-parse --show-toplevel)"

echo "Running full quality check before push..."
make check

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Push blocked - fix quality issues"
    exit 1
fi

exit 0
```

## Quality Metrics

### Enforcement Levels

| Check | Enforcement | Can Bypass? |
|-------|-------------|-------------|
| Compilation errors | BLOCKING | No |
| Test failures | BLOCKING | No |
| Formatting issues | BLOCKING | No |
| Dialyzer warnings | WARNING | Yes |
| Coverage <80% | WARNING | Yes |
| Xref issues | WARNING | Yes |

### Success Criteria

**All commits must:**
- ✅ Compile without errors
- ✅ Pass all existing tests
- ✅ Pass all new tests
- ✅ Follow code formatting
- ✅ Have no regressions

**All releases must:**
- ✅ All of the above
- ✅ Coverage ≥80%
- ✅ No Dialyzer warnings
- ✅ No xref issues

## Troubleshooting

### Pre-Edit Validation Fails

**Problem:** Can't edit because pre-edit validation fails

**Solution:**
1. Check what's failing: `rebar3 compile && rebar3 eunit`
2. Fix the failing tests/compilation
3. Re-run pre-edit validation
4. Only then start editing

**Never bypass pre-edit validation** - it prevents editing with failing tests.

### Post-Edit Validation Fails

**Problem:** After editing, validation fails

**Solution:**
1. Review what you changed: `git diff`
2. Check compilation: `rebar3 compile`
3. Check tests: `rebar3 eunit --module=<module> --verbose`
4. Fix the issue
5. Re-run post-edit validation

**Options if stuck:**
- Revert changes: `git checkout -- <file>`
- Debug interactively: `make console`
- Check test output: `_build/test/logs/`

### Commit Validation Fails

**Problem:** Can't commit because validation fails

**Solution:**
1. Run individual checks:
   - `rebar3 compile`
   - `rebar3 eunit`
   - `rebar3 ct`
   - `rebar3 format --verify`
2. Fix issues one by one
3. Re-run commit validation

**Never use `--no-verify`** - fix the issues instead.

## Best Practices

### DO

✅ Run pre-edit validation before editing
✅ Run post-edit validation after every change
✅ Use watch mode for rapid iteration
✅ Run commit validation before committing
✅ Fix issues immediately when detected
✅ Keep test feedback cycle under 10s
✅ Write tests first (TDD)
✅ Commit often with clean code

### DON'T

❌ Edit code with failing tests
❌ Skip validation checks
❌ Use `git commit --no-verify`
❌ Ignore Dialyzer warnings
❌ Let coverage drop below 80%
❌ Commit code that doesn't compile
❌ Bypass quality gates

## Performance Tips

### Speed Up Validation

1. **Use watch mode** for rapid feedback during development
2. **Run specific tests** instead of full suite during iteration:
   ```bash
   rebar3 eunit --module=specific_module_tests
   ```
3. **Incremental compilation** - only changed files recompile
4. **Parallel test execution** - rebar3 runs tests in parallel

### Optimize Test Suite

1. **Keep tests fast** - target <100ms per test
2. **Use proper fixtures** - setup/teardown efficiently
3. **Mock external dependencies** - avoid network calls
4. **Profile slow tests** - optimize hot paths

## Summary

The fail-fast workflow ensures quality through immediate feedback:

1. **Pre-Edit** - Start clean
2. **Post-Edit** - Validate immediately
3. **Commit** - Full quality gate
4. **Never bypass** - Fix issues when detected

**Result:** Zero-defect code, fast feedback, high confidence.

---

**See Also:**
- `.vscode/tasks.json` - IDE task definitions
- `tools/watch-and-validate.sh` - Watch mode implementation
- `tools/pre-edit-validator.sh` - Pre-edit validation
- `tools/post-edit-validator.sh` - Post-edit validation
- `tools/commit-validator.sh` - Commit validation
- `docs/development/TESTING.md` - Testing best practices
- `docs/development/TDD_GUIDE.md` - TDD methodology
