# Fail-Fast Validation System - Complete Index

## Quick Navigation

### Get Started (30 seconds)
→ [`FAIL_FAST_QUICK_START.md`](FAIL_FAST_QUICK_START.md)

### Full Documentation
→ [`FAIL_FAST_WORKFLOW.md`](FAIL_FAST_WORKFLOW.md)

### Tool Reference
→ [`../../tools/README_VALIDATION.md`](../../tools/README_VALIDATION.md)

### Implementation Details
→ [`VALIDATION_SYSTEM_SUMMARY.md`](VALIDATION_SYSTEM_SUMMARY.md)

---

## What Is This?

A comprehensive fail-fast validation system that detects and blocks quality issues at every stage of development.

**Core Principle:** Fix Before You Add - Never edit code with failing tests.

---

## Quick Commands

```bash
# Watch mode (recommended for active development)
./tools/watch-and-validate.sh

# Before editing
./tools/pre-edit-validator.sh

# After editing
./tools/post-edit-validator.sh src/my_file.erl

# Before committing
./tools/commit-validator.sh
```

---

## File Locations

### Scripts
All scripts in `/Users/sac/erlmcp/tools/`:
- `watch-and-validate.sh` - Real-time continuous validation
- `pre-edit-validator.sh` - Ensure clean state before editing
- `post-edit-validator.sh` - Validate changes immediately
- `commit-validator.sh` - Full validation before commit

### Documentation
All docs in `/Users/sac/erlmcp/docs/development/`:
- `FAIL_FAST_QUICK_START.md` - 30-second overview + common commands
- `FAIL_FAST_WORKFLOW.md` - Complete workflow documentation
- `VALIDATION_SYSTEM_SUMMARY.md` - Implementation details
- `VALIDATION_INDEX.md` - This file

### Configuration
- `.vscode/tasks.json` - 15 VS Code tasks for validation

---

## Validation Levels

| Level | Tool | Timing | Scope | Enforcement |
|-------|------|--------|-------|-------------|
| **Continuous** | watch-and-validate.sh | <10s | Single file + tests | Feedback |
| **Pre-Edit** | pre-edit-validator.sh | <5s | All existing code | BLOCKS editing |
| **Post-Edit** | post-edit-validator.sh | <10s | Changed files + tests | BLOCKS next edit |
| **Commit** | commit-validator.sh | <30s | Full project | BLOCKS commit |

---

## What Gets Checked

### ✅ BLOCKING Checks (Must Pass)
- Compilation errors
- Test failures
- Code formatting issues

### ⚠️ WARNING Checks (Non-Blocking)
- Dialyzer warnings
- Xref issues
- Coverage below 80%

---

## TDD Workflow

### RED Phase - Write Failing Test
```bash
./tools/pre-edit-validator.sh  # Ensure clean
vim test/new_feature_tests.erl  # Write test
rebar3 eunit --module=new_feature_tests  # ❌ Fails (expected)
```

### GREEN Phase - Make Test Pass
```bash
vim src/new_feature.erl  # Implement
./tools/post-edit-validator.sh src/new_feature.erl  # ✅ Passes
```

### REFACTOR Phase - Improve Code
```bash
vim src/new_feature.erl  # Refactor
./tools/post-edit-validator.sh src/new_feature.erl  # ✅ Still passes
```

### COMMIT Phase - Lock In
```bash
./tools/commit-validator.sh  # Full validation
git add ...
git commit -m "Add feature"
```

---

## Installation

### 1. Install fswatch (for watch mode)
```bash
brew install fswatch  # macOS
sudo apt-get install fswatch  # Linux
```

### 2. Setup Git Hook (optional but recommended)
```bash
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
./tools/commit-validator.sh || exit 1
EOF

chmod +x .git/hooks/pre-commit
```

### 3. VS Code (optional)
Tasks already configured in `.vscode/tasks.json`

Add keyboard shortcuts to `.vscode/keybindings.json`:
```json
[
  {"key": "ctrl+shift+p", "command": "workbench.action.tasks.runTask", "args": "Pre-Edit Validation"},
  {"key": "ctrl+shift+o", "command": "workbench.action.tasks.runTask", "args": "Post-Edit Validation"},
  {"key": "ctrl+shift+w", "command": "workbench.action.tasks.runTask", "args": "Watch and Validate"}
]
```

---

## Usage Patterns

### Pattern 1: Watch Mode (Fastest)
```bash
# Terminal 1: Watch mode
./tools/watch-and-validate.sh

# Terminal 2: Edit normally
vim src/file.erl
# Automatic validation in <10s
```

### Pattern 2: Manual Validation (Most Control)
```bash
./tools/pre-edit-validator.sh  # Before
vim src/file.erl  # Edit
./tools/post-edit-validator.sh src/file.erl  # After
./tools/commit-validator.sh  # Before commit
```

### Pattern 3: IDE Tasks (VS Code)
```
Cmd+Shift+P → "Tasks: Run Task"
Select: Pre-Edit / Post-Edit / Watch / Commit Validation
```

---

## Troubleshooting

### fswatch not found
```bash
brew install fswatch  # macOS
```

### Pre-edit validation fails
```bash
# Fix failing tests FIRST
rebar3 compile
rebar3 eunit
# Then retry
./tools/pre-edit-validator.sh
```

### Post-edit validation fails
```bash
# Check what changed
git diff src/my_file.erl
# Fix and retry
./tools/post-edit-validator.sh src/my_file.erl
```

### Commit validation fails
```bash
# Run checks individually
rebar3 compile
rebar3 eunit
rebar3 format --verify
# Fix and retry
./tools/commit-validator.sh
```

---

## Performance

### Timing Targets
- Pre-edit: <5 seconds
- Post-edit: <10 seconds
- Watch mode: <10 seconds per change
- Commit: <30 seconds

### Optimization Tips
1. Use watch mode for fastest feedback
2. Run specific tests during iteration
3. Keep tests fast (<100ms each)
4. Use mocks for external dependencies

---

## Best Practices

### DO ✅
- Use watch mode during active development
- Run pre-edit before editing
- Run post-edit after changes
- Run commit validation before commits
- Fix issues immediately
- Write tests first (TDD)

### DON'T ❌
- Edit with failing tests
- Skip validation checks
- Use `git commit --no-verify`
- Ignore Dialyzer warnings
- Let test failures accumulate

---

## VS Code Tasks (15 Available)

Access via Command Palette (Cmd+Shift+P) → "Tasks: Run Task":

**Validation Tasks:**
1. Pre-Edit Validation
2. Post-Edit Validation
3. Watch and Validate
4. Commit Validation

**Build Tasks:**
5. Quick Compile
6. Format Code

**Test Tasks:**
7. Run EUnit Tests
8. Run Module Tests
9. Run Common Test
10. Check Coverage

**Quality Tasks:**
11. Run Dialyzer
12. Full Quality Check

**TDD Tasks:**
13. RED: Failing Test Mode
14. GREEN: Make Tests Pass
15. REFACTOR: Validate Changes

---

## Documentation Map

```
docs/development/
├── VALIDATION_INDEX.md              ← You are here
├── FAIL_FAST_QUICK_START.md         ← 30-second overview
├── FAIL_FAST_WORKFLOW.md            ← Complete workflow
└── VALIDATION_SYSTEM_SUMMARY.md     ← Implementation details

tools/
├── watch-and-validate.sh            ← Continuous validation
├── pre-edit-validator.sh            ← Before editing
├── post-edit-validator.sh           ← After editing
├── commit-validator.sh              ← Before commit
└── README_VALIDATION.md             ← Tool reference

.vscode/
└── tasks.json                       ← 15 VS Code tasks
```

---

## Summary

**4 Tools:**
1. watch-and-validate.sh - Continuous (<10s)
2. pre-edit-validator.sh - Before editing (<5s)
3. post-edit-validator.sh - After editing (<10s)
4. commit-validator.sh - Before commit (<30s)

**3 Principles:**
1. Fix before you add
2. Immediate feedback
3. Zero defects

**Result:**
- Fast feedback (<10s)
- High confidence (100% test pass)
- Zero regressions (all tests run)

---

## Getting Help

1. Quick start: `docs/development/FAIL_FAST_QUICK_START.md`
2. Full workflow: `docs/development/FAIL_FAST_WORKFLOW.md`
3. Tool reference: `tools/README_VALIDATION.md`
4. Implementation: `docs/development/VALIDATION_SYSTEM_SUMMARY.md`

---

**Start here:** Run `./tools/watch-and-validate.sh` and edit files normally.
