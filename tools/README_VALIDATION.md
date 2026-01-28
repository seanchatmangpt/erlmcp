# Fail-Fast Validation Tools

## Overview

Comprehensive validation tools that enforce quality at every stage of development. These tools implement the "Fix Before You Add" principle by detecting and blocking defects immediately.

## Tools Inventory

### 1. watch-and-validate.sh

**Purpose:** Real-time continuous validation with <10s feedback

**Usage:**
```bash
./tools/watch-and-validate.sh
```

**What it does:**
- Watches `src/` and `test/` directories for changes
- On file change:
  - Compiles changed file immediately
  - Runs affected tests automatically
  - Shows results in terminal
  - Completes in <10 seconds
- Provides continuous Red-Green-Refactor feedback

**Dependencies:**
- `fswatch` - Install with `brew install fswatch` (macOS)

**When to use:**
- Active feature development
- TDD workflow
- Rapid iteration
- Learning/experimenting

**Example output:**
```
ℹ️  [14:23:45] File changed: src/erlmcp_client.erl
ℹ️  [14:23:45] Compiling: src/erlmcp_client.erl
✅ [14:23:46] Compiled in 342ms
ℹ️  [14:23:46] Running tests: erlmcp_client_tests
✅ [14:23:48] Tests passed in 1847ms
✅ [14:23:48] All validations passed! ✨
```

---

### 2. pre-edit-validator.sh

**Purpose:** Ensures clean state before editing (prevents editing with failing tests)

**Usage:**
```bash
./tools/pre-edit-validator.sh [file-to-edit]
./tools/pre-edit-validator.sh --skip-tests  # NOT RECOMMENDED
```

**What it checks:**
- ✅ **Compilation** (BLOCKING)
- ✅ **All existing tests** (BLOCKING)
- ⚠️ Dialyzer warnings (non-blocking)

**Enforcement:**
- BLOCKS editing if compilation fails
- BLOCKS editing if tests fail
- Ensures you start from a known-good state

**When to use:**
- Before starting any file editing session
- Before refactoring
- When switching contexts

**Example output:**
```
✅ Compilation clean
✅ All tests passing
✅ Pre-edit validation PASSED ✨
✅ Safe to edit!
```

---

### 3. post-edit-validator.sh

**Purpose:** Validates changes immediately after editing (blocks next edit if fails)

**Usage:**
```bash
./tools/post-edit-validator.sh <changed-files...>
./tools/post-edit-validator.sh src/file1.erl test/file1_tests.erl
./tools/post-edit-validator.sh  # Validates all changes
```

**What it checks:**
- ✅ **Changed files compile** (BLOCKING)
- ✅ **Affected tests pass** (BLOCKING)
- ⚠️ Code formatting (non-blocking)
- ⚠️ Dialyzer warnings (non-blocking)

**Enforcement:**
- BLOCKS next edit if compilation fails
- BLOCKS next edit if tests fail
- Forces immediate fix

**When to use:**
- After editing any source files
- After writing new tests
- Before moving to next task

**Example output:**
```
✅ Compilation successful
✅ All affected tests passed
✅ Post-edit validation PASSED ✨
✅ Changes are clean - safe to continue!
```

---

### 4. commit-validator.sh

**Purpose:** Full validation before git commit (blocks commit if fails)

**Usage:**
```bash
./tools/commit-validator.sh
```

**What it checks:**
- ✅ **Full compilation** (BLOCKING)
- ✅ **All EUnit tests** (BLOCKING)
- ✅ **All Common Test suites** (BLOCKING)
- ✅ **Code formatting** (BLOCKING)
- ⚠️ Dialyzer warnings (non-blocking)
- ⚠️ Xref analysis (non-blocking)
- ⚠️ Test coverage ≥80% (non-blocking)

**Enforcement:**
- BLOCKS commit if any critical check fails
- Can be bypassed with `git commit --no-verify` (NOT recommended)

**When to use:**
- Before every git commit
- Automatically via pre-commit hook
- Before creating pull requests

**Example output:**
```
✅ Compilation clean
✅ All EUnit tests passed
✅ All CT suites passed
✅ Formatting clean
✅ Coverage: 87% (≥80% target)
✅ Commit validation PASSED ✨
✅ Safe to commit!
```

---

## Installation

### 1. Install Dependencies

```bash
# macOS
brew install fswatch

# Linux (Debian/Ubuntu)
sudo apt-get install fswatch

# Linux (RedHat/CentOS)
sudo yum install fswatch
```

### 2. Make Scripts Executable (Already Done)

```bash
chmod +x tools/watch-and-validate.sh
chmod +x tools/pre-edit-validator.sh
chmod +x tools/post-edit-validator.sh
chmod +x tools/commit-validator.sh
```

### 3. Setup Git Pre-Commit Hook (Optional)

```bash
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
./tools/commit-validator.sh || exit 1
EOF

chmod +x .git/hooks/pre-commit
```

---

## Workflow Integration

### Method 1: Watch Mode (Recommended for Active Development)

**Best for:** TDD, rapid iteration, feature development

```bash
# Terminal 1: Start watch mode
./tools/watch-and-validate.sh

# Terminal 2: Edit files normally
vim src/erlmcp_client.erl

# Watch mode automatically validates changes in <10s
```

### Method 2: Manual Validation (Recommended for Complex Changes)

**Best for:** Refactoring, debugging, architectural changes

```bash
# 1. Before editing
./tools/pre-edit-validator.sh

# 2. Edit files
vim src/erlmcp_client.erl

# 3. After editing
./tools/post-edit-validator.sh src/erlmcp_client.erl

# 4. Before committing
./tools/commit-validator.sh
git add ...
git commit -m "..."
```

### Method 3: VS Code Integration

**Best for:** IDE-centric workflow

Tasks available in Command Palette (Cmd+Shift+P):
- **Pre-Edit Validation**
- **Post-Edit Validation**
- **Watch and Validate**
- **Commit Validation**
- **RED: Failing Test Mode**
- **GREEN: Make Tests Pass**
- **REFACTOR: Validate Changes**

See `.vscode/tasks.json` for all 15 defined tasks.

---

## TDD Workflow

### RED Phase: Write Failing Test

```bash
# 1. Ensure clean state
./tools/pre-edit-validator.sh

# 2. Write failing test
vim test/new_feature_tests.erl

# 3. Verify failure (watch mode auto-runs, or manual)
rebar3 eunit --module=new_feature_tests

Expected: ❌ Test fails
```

### GREEN Phase: Make Test Pass

```bash
# 1. Implement feature
vim src/new_feature.erl

# 2. Validate (watch mode auto-validates, or manual)
./tools/post-edit-validator.sh src/new_feature.erl

Expected: ✅ Tests pass
```

### REFACTOR Phase: Improve Code

```bash
# 1. Refactor
vim src/new_feature.erl

# 2. Ensure tests stay green
./tools/post-edit-validator.sh src/new_feature.erl

Expected: ✅ Tests still pass
```

### COMMIT Phase: Lock In Changes

```bash
# Full validation
./tools/commit-validator.sh

# Commit
git add src/new_feature.erl test/new_feature_tests.erl
git commit -m "Add new feature"
```

---

## Validation Levels

| Tool | Time | Scope | Enforcement |
|------|------|-------|-------------|
| **watch-and-validate** | <10s | Single file + its tests | Continuous feedback |
| **pre-edit-validator** | <5s | All existing code | BLOCKS editing |
| **post-edit-validator** | <10s | Changed files + affected tests | BLOCKS next edit |
| **commit-validator** | <30s | Full project | BLOCKS commit |

---

## Common Scenarios

### Scenario 1: Adding New Feature

```bash
# Start watch mode
./tools/watch-and-validate.sh

# In another terminal:
# 1. Write failing test
vim test/new_feature_tests.erl
# Watch shows: ❌ Test fails

# 2. Implement feature
vim src/new_feature.erl
# Watch shows: ✅ Tests pass

# 3. Commit
./tools/commit-validator.sh
git commit -m "Add new feature"
```

### Scenario 2: Fixing Bug

```bash
# 1. Write regression test
vim test/bug_fix_tests.erl
./tools/post-edit-validator.sh test/bug_fix_tests.erl
# Expected: ❌ Test reproduces bug

# 2. Fix bug
vim src/buggy_module.erl
./tools/post-edit-validator.sh src/buggy_module.erl
# Expected: ✅ Test passes

# 3. Commit
./tools/commit-validator.sh
git commit -m "Fix bug XYZ"
```

### Scenario 3: Refactoring

```bash
# 1. Ensure clean state
./tools/pre-edit-validator.sh
# MUST pass before refactoring

# 2. Refactor
vim src/module.erl
./tools/post-edit-validator.sh src/module.erl
# Expected: ✅ Tests stay green

# 3. Commit
./tools/commit-validator.sh
git commit -m "Refactor: improve structure"
```

---

## Troubleshooting

### Watch Mode Not Starting

**Problem:** `fswatch: command not found`

**Solution:**
```bash
brew install fswatch  # macOS
sudo apt-get install fswatch  # Linux
```

### Pre-Edit Validation Fails

**Problem:** Can't edit because validation fails

**Solution:**
```bash
# Check what's failing
rebar3 compile
rebar3 eunit

# Fix issues
vim src/failing_module.erl

# Retry validation
./tools/pre-edit-validator.sh
```

**Never bypass** - fix the issues first.

### Post-Edit Validation Fails

**Problem:** After editing, tests fail

**Solution:**
```bash
# Check specific module
rebar3 eunit --module=my_module_tests --verbose

# Review your changes
git diff src/my_module.erl

# Fix and retry
./tools/post-edit-validator.sh src/my_module.erl
```

**Options if stuck:**
- Revert changes: `git checkout -- <file>`
- Debug interactively: `make console`

### Commit Validation Fails

**Problem:** Can't commit

**Solution:**
```bash
# Run checks individually
rebar3 compile
rebar3 eunit
rebar3 ct
rebar3 format --verify

# Fix issues one by one
# Then retry
./tools/commit-validator.sh
```

**Never use `--no-verify`** - fix the issues.

---

## Performance Tips

### Speed Up Validation

1. **Use watch mode** - fastest feedback (auto-runs on save)
2. **Run specific tests** during iteration:
   ```bash
   rebar3 eunit --module=specific_module_tests
   ```
3. **Incremental compilation** - only changed files recompile
4. **Parallel test execution** - rebar3 runs tests in parallel

### Keep Tests Fast

- Target <100ms per test
- Use proper setup/teardown
- Mock external dependencies
- Profile slow tests

---

## Integration with Other Tools

### Git Hooks

**Pre-commit** (automatic validation):
```bash
#!/bin/bash
./tools/commit-validator.sh || exit 1
```

**Pre-push** (full quality check):
```bash
#!/bin/bash
make check || exit 1
```

### CI/CD Pipeline

```yaml
# .github/workflows/ci.yml
- name: Run commit validation
  run: ./tools/commit-validator.sh

- name: Check coverage
  run: rebar3 cover --verbose
```

### VS Code Tasks

Already configured in `.vscode/tasks.json`:
- 15 tasks for various validation scenarios
- Keyboard shortcuts available
- Integrated terminal output

---

## Best Practices

### DO ✅

- ✅ Use watch mode for active development
- ✅ Run pre-edit validation before editing
- ✅ Run post-edit validation after changes
- ✅ Run commit validation before committing
- ✅ Fix issues immediately when detected
- ✅ Keep test feedback cycle <10s
- ✅ Write tests first (TDD)

### DON'T ❌

- ❌ Edit code with failing tests
- ❌ Skip validation checks
- ❌ Use `git commit --no-verify`
- ❌ Ignore warnings (they become errors)
- ❌ Let test failures accumulate
- ❌ Commit code that doesn't compile

---

## Success Metrics

### Validation Timing

- **Pre-edit:** <5 seconds
- **Post-edit:** <10 seconds
- **Watch mode:** <10 seconds per change
- **Commit:** <30 seconds

### Quality Targets

- **Compilation:** 0 errors, 0 warnings
- **Tests:** 100% pass rate
- **Coverage:** ≥80%
- **Formatting:** 100% compliance
- **Type checking:** 0 Dialyzer warnings

---

## Documentation

- **Quick Start:** `docs/development/FAIL_FAST_QUICK_START.md`
- **Full Workflow:** `docs/development/FAIL_FAST_WORKFLOW.md`
- **VS Code Tasks:** `.vscode/tasks.json`
- **This README:** `tools/README_VALIDATION.md`

---

## Summary

Four tools, three principles, zero defects:

1. **watch-and-validate.sh** - Continuous validation (<10s)
2. **pre-edit-validator.sh** - Clean state before editing
3. **post-edit-validator.sh** - Validate after changes
4. **commit-validator.sh** - Full gate before commit

**Result:** Immediate feedback, no defects, high confidence.
