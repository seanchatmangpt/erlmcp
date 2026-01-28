# Claude Code Integration for erlmcp

This directory contains documentation for Claude Code agent integration, including automatic quality enforcement hooks.

## Quick Start

```bash
# Install all hooks and enforcement points
./tools/claude-md-sync.sh

# Test validation manually
./tools/claude-md-enforcer.sh

# Check if git pre-commit hook is installed
ls -la .git/hooks/pre-commit
```

## What's Implemented

### 1. Automatic Quality Enforcement

All quality rules from `CLAUDE.md` are automatically enforced at multiple points:

- **Pre-commit hook:** Validates before every git commit
- **Post-task hook:** Validates after Claude Code agents complete tasks
- **CI/CD integration:** Can be added to GitHub Actions workflows
- **Manual validation:** Run anytime with `./tools/claude-md-enforcer.sh`

### 2. Quality Rules

Enforced from CLAUDE.md line 74:
> **Targets:** 0 errors, 100% test pass, ≥80% coverage, <10% perf regression.

**Blocking Rules:**
- Compilation must have 0 errors
- Tests must have 100% pass rate
- Code coverage must be ≥80%

**Reporting Rules:**
- Dialyzer warnings reported (not blocking)
- Xref issues reported (not blocking)

### 3. Hook Architecture

```
CLAUDE.md (rules)
    ↓
tools/claude-md-enforcer.sh (parser + validator)
    ↓
    ├─→ .git/hooks/pre-commit (git)
    ├─→ .claude/hooks/post-task-validate.sh (Claude Code)
    ├─→ .github/workflows/*.yml (CI/CD)
    └─→ Manual execution
```

## Documentation

- **[HOOK_INTEGRATION.md](HOOK_INTEGRATION.md)** - Complete architecture, troubleshooting, and implementation guide

## Files

### Validators
- `tools/claude-md-enforcer.sh` - Core validation engine that enforces CLAUDE.md rules
- `tools/claude-md-sync.sh` - Synchronization utility to install/update hooks

### Hooks
- `.claude/hooks/post-task-validate.sh` - Claude Code post-task validation hook
- `.claude/hooks/pre-commit-validate.sh` - Claude Code pre-commit validation hook
- `.git/hooks/pre-commit` - Git pre-commit hook (installed by sync tool)

## Usage

### Install/Update Hooks

```bash
# Run after cloning repo or updating CLAUDE.md rules
./tools/claude-md-sync.sh
```

Output:
```
[SYNC] CLAUDE.md Rules Synchronization
[INFO] ✅ Git pre-commit hook installed
[INFO] ✅ Post-task hook exists
[INFO] ✅ Pre-commit hook script exists
[SYNC] ✅ Synchronization complete
```

### Manual Validation

```bash
# Run validation anytime
./tools/claude-md-enforcer.sh

# Check exit code
echo $?
# 0 = all rules satisfied
# 1 = violations detected
```

### Git Commit (Automatic)

```bash
# Make changes
vim src/erlmcp_client.erl

# Stage changes
git add src/erlmcp_client.erl

# Commit (hook runs automatically)
git commit -m "Update client"

# If validation fails, commit is blocked
# Fix issues and try again
```

### Claude Code Task (Automatic)

When a Claude Code agent completes a task:
1. Post-task hook runs automatically
2. Validation checks CLAUDE.md rules
3. If validation passes, task marked complete
4. If validation fails, task remains incomplete with error report

## Bypassing Validation

**Not recommended, but available for emergencies:**

```bash
# Bypass git pre-commit hook
git commit --no-verify -m "Emergency fix"

# Then immediately fix and commit properly
./tools/claude-md-enforcer.sh
git add .
git commit -m "Fix quality violations"
```

## Validation Output

### Success
```
[INFO] ✅ Compilation passed (0 errors)
[INFO] ✅ Tests passed: 42/42 (100%)
[INFO] ✅ Coverage: 85% (≥80%)
[INFO] ✅ Dialyzer clean (0 warnings)
[INFO] ✅ Xref clean
[INFO] ✅ All quality rules satisfied
```

### Failure
```
[ERROR] ❌ Test pass rate: 95% (required: ≥100%)
[ERROR] Failed tests: 2 / 40
[ERROR] ❌ Coverage: 75% (required: ≥80%)
[ERROR] ❌ 2 quality rule violation(s) detected
[ERROR] Fix violations before proceeding
```

## Troubleshooting

### Hook not running

```bash
# Re-install hooks
./tools/claude-md-sync.sh

# Verify installation
ls -la .git/hooks/pre-commit

# Check hook is executable
chmod +x .git/hooks/pre-commit
chmod +x .claude/hooks/pre-commit-validate.sh
```

### Tests fail in hook but pass manually

Environment differences. Test with clean environment:

```bash
env -i HOME="$HOME" PATH="$PATH" rebar3 eunit
```

### Hook is too slow

Optimization options:
1. Skip dialyzer in hook (add flag)
2. Run partial validation in hook, full in CI
3. Use parallel validation (compile + tests)

## Philosophy

> "Quality is not an act, it is a habit." - Aristotle

This hook integration embodies "shift-left" quality:
- Catch violations immediately (local, not CI)
- Prevent bad commits from entering history
- Fast feedback (seconds, not minutes)
- Consistent rules everywhere

## Maintenance

### Updating Rules

1. Edit CLAUDE.md quality targets (line 74)
2. Run sync: `./tools/claude-md-sync.sh`
3. Test: `./tools/claude-md-enforcer.sh`
4. Commit: `git add CLAUDE.md tools/ .claude/`

### Adding New Validation

1. Update `tools/claude-md-enforcer.sh` parser
2. Add validation function
3. Call from `main()`
4. Update CLAUDE.md to document new rule
5. Run sync: `./tools/claude-md-sync.sh`
6. Update this documentation

## References

- [HOOK_INTEGRATION.md](HOOK_INTEGRATION.md) - Complete architecture guide
- [CLAUDE.md](../../CLAUDE.md) - Quality rules source of truth
- [architecture.md](../architecture.md) - Project architecture
- [otp-patterns.md](../otp-patterns.md) - OTP development patterns

## Version

- Implementation: v1.0.0
- Compatible with: erlmcp v0.5.0+
- Updated: 2026-01-28
