# Fail-Fast Quick Start Guide

## 30-Second Overview

**Fail-fast validation prevents defects by catching issues immediately during development.**

```bash
# Before editing
./tools/pre-edit-validator.sh

# After editing
./tools/post-edit-validator.sh src/my_file.erl

# Before committing
./tools/commit-validator.sh

# Watch mode (continuous)
./tools/watch-and-validate.sh
```

## Installation (One-Time Setup)

### 1. Install fswatch (for watch mode)

```bash
# macOS
brew install fswatch

# Linux
sudo apt-get install fswatch  # Debian/Ubuntu
sudo yum install fswatch      # RedHat/CentOS
```

### 2. Setup Git Pre-Commit Hook (Optional but Recommended)

```bash
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
./tools/commit-validator.sh || exit 1
EOF

chmod +x .git/hooks/pre-commit
```

### 3. VS Code Setup (Optional)

Tasks are already configured in `.vscode/tasks.json`.

Add keyboard shortcuts to `.vscode/keybindings.json`:

```json
[
  {"key": "ctrl+shift+p", "command": "workbench.action.tasks.runTask", "args": "Pre-Edit Validation"},
  {"key": "ctrl+shift+o", "command": "workbench.action.tasks.runTask", "args": "Post-Edit Validation"},
  {"key": "ctrl+shift+c", "command": "workbench.action.tasks.runTask", "args": "Commit Validation"},
  {"key": "ctrl+shift+w", "command": "workbench.action.tasks.runTask", "args": "Watch and Validate"}
]
```

## Daily Workflow

### Method 1: Watch Mode (Recommended for Active Development)

**Best for:** Rapid iteration, TDD, feature development

```bash
# Start watch mode in a terminal
./tools/watch-and-validate.sh

# In another terminal, edit files
vim src/erlmcp_client.erl

# Watch mode automatically:
# - Compiles changed file
# - Runs affected tests
# - Shows results in <10s
```

### Method 2: Manual Validation (Recommended for Complex Changes)

**Best for:** Refactoring, architectural changes, debugging

```bash
# 1. Before editing - ensure clean state
./tools/pre-edit-validator.sh src/erlmcp_client.erl

# 2. Edit the file
vim src/erlmcp_client.erl

# 3. After editing - validate changes
./tools/post-edit-validator.sh src/erlmcp_client.erl

# 4. Before committing - full validation
./tools/commit-validator.sh
git add ...
git commit -m "..."
```

### Method 3: IDE Tasks (VS Code)

**Best for:** IDE-centric workflow

1. Open Command Palette (Cmd+Shift+P / Ctrl+Shift+P)
2. Type "Tasks: Run Task"
3. Select:
   - **Pre-Edit Validation** - Before editing
   - **Post-Edit Validation** - After editing
   - **Watch and Validate** - Continuous mode
   - **Commit Validation** - Before commit

## TDD Cycle

### RED Phase (Write Failing Test)

```bash
# 1. Ensure clean state
./tools/pre-edit-validator.sh

# 2. Write failing test
vim test/new_feature_tests.erl

# 3. Verify it fails
rebar3 eunit --module=new_feature_tests

Expected: ❌ Test fails (no implementation yet)
```

### GREEN Phase (Make Test Pass)

```bash
# 1. Implement feature
vim src/new_feature.erl

# 2. Validate implementation
./tools/post-edit-validator.sh src/new_feature.erl

Expected: ✅ Tests pass
```

### REFACTOR Phase (Improve Code)

```bash
# 1. Refactor code
vim src/new_feature.erl

# 2. Ensure tests stay green
./tools/post-edit-validator.sh src/new_feature.erl

Expected: ✅ Tests still pass
```

### COMMIT Phase (Lock In Changes)

```bash
# Full validation
./tools/commit-validator.sh

# Commit
git add src/new_feature.erl test/new_feature_tests.erl
git commit -m "Add new feature"
```

## Validation Levels

| Tool | Time | Checks | Enforcement |
|------|------|--------|-------------|
| **Pre-Edit** | <5s | Compile + Existing tests | BLOCKS editing |
| **Post-Edit** | <10s | Changed files + Affected tests | BLOCKS next edit |
| **Watch Mode** | <10s | Single file + Its tests | Continuous feedback |
| **Commit** | <30s | Full suite + Formatting + Static analysis | BLOCKS commit |

## What Gets Checked

### Pre-Edit Validation ✅

- ✅ **Compilation** (BLOCKING)
- ✅ **All existing tests** (BLOCKING)
- ⚠️ Dialyzer warnings (non-blocking)

### Post-Edit Validation ✅

- ✅ **Changed file compilation** (BLOCKING)
- ✅ **Affected tests** (BLOCKING)
- ⚠️ Code formatting (non-blocking)
- ⚠️ Dialyzer warnings (non-blocking)

### Commit Validation ✅

- ✅ **Full compilation** (BLOCKING)
- ✅ **All EUnit tests** (BLOCKING)
- ✅ **All CT suites** (BLOCKING)
- ✅ **Code formatting** (BLOCKING)
- ⚠️ Dialyzer warnings (non-blocking)
- ⚠️ Xref analysis (non-blocking)
- ⚠️ Test coverage ≥80% (non-blocking)

## Common Commands

```bash
# Quick validation of current state
./tools/pre-edit-validator.sh

# Validate after editing specific files
./tools/post-edit-validator.sh src/file1.erl test/file1_tests.erl

# Full validation before commit
./tools/commit-validator.sh

# Watch mode (continuous validation)
./tools/watch-and-validate.sh

# Skip tests (NOT RECOMMENDED)
./tools/pre-edit-validator.sh --skip-tests

# Bypass commit hook (NOT RECOMMENDED)
git commit --no-verify
```

## Troubleshooting

### Issue: Pre-edit validation fails

```bash
# Check what's failing
rebar3 compile
rebar3 eunit

# Fix issues, then retry
./tools/pre-edit-validator.sh
```

### Issue: Post-edit validation fails

```bash
# Check specific module
rebar3 eunit --module=my_module_tests --verbose

# Review changes
git diff src/my_module.erl

# Fix and retry
./tools/post-edit-validator.sh src/my_module.erl
```

### Issue: Commit validation fails

```bash
# Run individual checks
rebar3 compile           # Compilation
rebar3 eunit             # Unit tests
rebar3 ct                # Integration tests
rebar3 format --verify   # Formatting

# Fix issues, then retry
./tools/commit-validator.sh
```

### Issue: Watch mode not working

```bash
# Check fswatch is installed
which fswatch

# Install if missing
brew install fswatch  # macOS

# Restart watch mode
./tools/watch-and-validate.sh
```

## Performance Tips

### Speed Up Tests

```bash
# Run specific test module instead of all tests
rebar3 eunit --module=specific_module_tests

# Run specific test function
rebar3 eunit --module=specific_module_tests --test=test_function_name
```

### Speed Up Validation

1. **Use watch mode** - fastest feedback during active development
2. **Incremental compilation** - only changed files recompile
3. **Target specific tests** - validate only affected modules
4. **Parallel execution** - rebar3 runs tests in parallel

## Best Practices

### DO ✅

- ✅ Run pre-edit validation before editing
- ✅ Use watch mode during active development
- ✅ Run post-edit validation after changes
- ✅ Run commit validation before committing
- ✅ Fix issues immediately when detected
- ✅ Write tests first (TDD)

### DON'T ❌

- ❌ Edit code with failing tests
- ❌ Skip validation checks
- ❌ Use `git commit --no-verify`
- ❌ Ignore Dialyzer warnings
- ❌ Commit code that doesn't compile
- ❌ Let test failures accumulate

## Example Session

```bash
# Terminal 1: Watch mode (continuous validation)
./tools/watch-and-validate.sh

ℹ️  Starting watch mode...
ℹ️  Watching: src/ and test/

# Terminal 2: Development workflow
cd /Users/sac/erlmcp

# RED: Write failing test
vim test/new_feature_tests.erl
# Watch mode automatically runs test → ❌ Fails (expected)

# GREEN: Implement feature
vim src/new_feature.erl
# Watch mode automatically validates → ✅ Passes

# REFACTOR: Improve code
vim src/new_feature.erl
# Watch mode automatically validates → ✅ Still passes

# COMMIT: Full validation
./tools/commit-validator.sh
git add src/new_feature.erl test/new_feature_tests.erl
git commit -m "Add new feature"
```

## Summary

**Three simple rules:**

1. **Before editing:** `./tools/pre-edit-validator.sh`
2. **After editing:** `./tools/post-edit-validator.sh <files>`
3. **Before committing:** `./tools/commit-validator.sh`

**Or use watch mode for continuous validation:**

```bash
./tools/watch-and-validate.sh
```

**Result:** Immediate feedback, zero defects, high confidence.

---

**Full Documentation:** `docs/development/FAIL_FAST_WORKFLOW.md`
