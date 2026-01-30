# Quality Gates Quick Start Guide
## 5-Minute Setup for erlmcp Developers

**Version:** 1.0.0 | **Updated:** 2026-01-30

---

## TL;DR - Just Get Me Started

```bash
# 1. Install hooks (one-time setup)
./tools/install-hooks.sh

# 2. Configure format plugin
# Add to rebar.config plugins section:
# {rebar3_format, "1.3.0"}
rebar3 update

# 3. Daily workflow
rebar3 format              # Format before commit
git commit -m "message"    # Auto-validates (~10s)
git push                   # Auto-validates (~2min)
```

---

## What Changed?

### Before Quality Gates
```
Developer → git commit → git push → CI fails → Fix → Repeat
                                    ↑ Slow feedback (10+ min)
```

### After Quality Gates
```
Developer → Quick check → Format → Commit (validated) → Push (validated) → CI
            ↓ Fast (10s)           ↓ Fast (10s)        ↓ Full (2min)    ↓ Confirm
```

**Result:** Catch errors in <10 seconds, not 10+ minutes.

---

## Installation (One-Time, <5 min)

### Step 1: Install Git Hooks
```bash
cd /home/user/erlmcp
./tools/install-hooks.sh
```

**What this does:**
- Creates symlinks: `.git/hooks/pre-commit` → `tools/hooks/pre-commit.sh`
- Creates symlinks: `.git/hooks/pre-push` → `tools/hooks/pre-push.sh`
- Makes hooks executable

**Verify:**
```bash
ls -la .git/hooks/pre-commit .git/hooks/pre-push
# Should show symlinks to tools/hooks/
```

### Step 2: Configure rebar3_format
Edit `rebar.config`, add to plugins section (around line 255):

```erlang
{plugins, [
    rebar3_hex,
    {rebar3_proper, "0.12.1"},
    {coveralls, "2.2.0"},
    {rebar3_format, "1.3.0"}  % ADD THIS
]}.
```

Then update:
```bash
rebar3 update
```

**Verify:**
```bash
rebar3 format --verify
# Should run without errors
```

---

## Daily Workflow

### Option A: Quick Workflow (Recommended)
```bash
# 1. Make changes
vim src/my_module.erl

# 2. Format (auto-fix)
rebar3 format

# 3. Commit (triggers hook)
git add src/my_module.erl
git commit -m "feat: Add awesome feature"
# ✅ Hook validates in ~10s

# 4. Push (triggers hook)
git push
# ✅ Hook validates in ~2min
```

### Option B: Extra Cautious Workflow
```bash
# 1. Make changes
vim src/my_module.erl

# 2. Quick check (optional)
./tools/quick-check.sh  # <30s

# 3. Format
rebar3 format

# 4. Full quality check (optional)
./tools/quality-gate.sh  # ~2min

# 5. Commit & push
git add src/my_module.erl
git commit -m "feat: Add awesome feature"
git push
```

---

## What Gets Checked?

### Pre-Commit Hook (~10 seconds)
Fast checks to catch obvious errors:

| Check | Command | Pass? |
|-------|---------|-------|
| Format | `rebar3 format --verify` | ✅ Must pass |
| Compilation | `rebar3 compile` | ✅ Must pass |

**If failed:** Fix errors, then `git commit` again.

### Pre-Push Hook (~2 minutes)
Comprehensive validation before push:

| Check | Command | Blocking? |
|-------|---------|-----------|
| Compilation | `rebar3 compile` | ✅ Yes (0 errors) |
| Format | `rebar3 format --verify` | ✅ Yes (100%) |
| Tests | `rebar3 eunit` | ✅ Yes (100% pass) |
| Coverage | `rebar3 cover` | ✅ Yes (≥80%) |
| Xref | `rebar3 xref` | ⚠️ No (warning only) |
| Dialyzer | `rebar3 dialyzer` | ⚠️ No (warning only) |

**Legend:**
- ✅ **Blocking:** Must pass or push fails
- ⚠️ **Reporting:** Logged to metrics, non-blocking

**If failed:** Fix errors, then `git push` again.

---

## Common Scenarios

### Scenario 1: "Format check failed"
```bash
# Error
❌ Format check failed
Run: rebar3 format

# Fix
rebar3 format
git add .
git commit --amend --no-edit
```

### Scenario 2: "Compilation errors"
```bash
# Error
❌ Compilation failed
src/my_module.erl:42: syntax error before: '.'

# Fix
vim src/my_module.erl  # Fix line 42
git add src/my_module.erl
git commit --amend --no-edit
```

### Scenario 3: "Test failures"
```bash
# Error
❌ EUnit failed: 155/156 tests passed (1 failure)

# Fix
rebar3 eunit --module=failing_module_tests --verbose
# Identify and fix failing test
git add test/failing_module_tests.erl
git commit --amend --no-edit
```

### Scenario 4: "Coverage below 80%"
```bash
# Error
❌ Coverage failed: 75% (required: 80%)

# Fix
rebar3 cover  # See uncovered modules
# Add tests for uncovered code
git add test/new_tests.erl
git commit --amend --no-edit
```

### Scenario 5: "Emergency bypass" (USE SPARINGLY!)
```bash
# ONLY for emergencies (hotfix, WIP on feature branch)
git commit --no-verify -m "WIP: debugging"
git push --no-verify

# NEVER bypass for:
# - Merges to main/integration
# - Production releases
```

---

## Useful Commands

### Quality Validation
```bash
# Quick check (<30s) - format + compile
./tools/quick-check.sh

# Full validation (~2min) - all gates
./tools/quality-gate.sh

# Claude.md enforcement (most comprehensive)
./tools/claude-md-enforcer.sh

# Just format check
./tools/format-checker.sh
```

### Quality Metrics
```bash
# View dashboard
./tools/quality-dashboard.sh

# Example output:
# ╔════════════════════════════════════════╗
# ║  ERLMCP QUALITY METRICS DASHBOARD      ║
# ╚════════════════════════════════════════╝
#
# BLOCKING GATES:
#   ✅ Compilation:  42 modules
#   ✅ Tests:        156 (100%)
#   ✅ Coverage:     85% (≥80%)
#
# REPORTING GATES:
#   ⚠️  Xref:         6 warnings
#
# QUALITY SCORE: 92/100 - EXCELLENT
```

### Formatting
```bash
# Format all files
rebar3 format

# Check formatting (no changes)
rebar3 format --verify

# Format specific file
rebar3 format src/my_module.erl
```

---

## Troubleshooting

### "Hook not running"
```bash
# Check hook exists
ls -la .git/hooks/pre-commit

# Reinstall hooks
./tools/install-hooks.sh

# Make executable
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/pre-push
```

### "rebar3 format not found"
```bash
# Add to rebar.config
{plugins, [
    {rebar3_format, "1.3.0"}
]}.

# Update
rebar3 update
```

### "Hook is slow"
```bash
# Pre-commit should be <15s
# Pre-push should be <3min

# If slower, check:
# 1. Dialyzer PLT built? (first run takes longer)
# 2. Many files changed? (format check scales with file count)
# 3. Slow tests? (optimize test setup/teardown)
```

### "Tests pass locally, fail in CI"
```bash
# Clean build
rebar3 clean
rebar3 compile
rebar3 eunit

# Update deps
rebar3 update
```

---

## Best Practices

### ✅ DO
- Run `./tools/quick-check.sh` before committing
- Run `rebar3 format` before every commit
- Fix quality gate failures immediately
- Monitor quality dashboard weekly
- Keep coverage ≥80%

### ❌ DON'T
- Don't bypass hooks for merges to main
- Don't commit with `--no-verify` unless emergency
- Don't ignore xref/dialyzer warnings (they accumulate)
- Don't commit untested code
- Don't push broken builds

---

## Getting Help

### Documentation
- **Full Implementation Plan:** `docs/development/quality_gates_plan.md`
- **Detailed README:** `docs/development/quality_gates_README.md`
- **CLAUDE.md:** Project quality standards

### Scripts
- **Hooks:** `tools/hooks/`
- **Quality Gates:** `tools/quality-gate.sh`
- **Dashboard:** `tools/quality-dashboard.sh`

### CI/CD
- **Workflow:** `.github/workflows/quality-gates.yml`
- **Status:** Check GitHub PR status checks

---

## FAQ

**Q: Do I have to install hooks?**
A: No, but highly recommended. Without hooks, you'll get slower feedback from CI/CD.

**Q: Can I bypass hooks?**
A: Yes with `--no-verify`, but only for emergencies. Never bypass for main branch.

**Q: Why are some gates "reporting only"?**
A: Xref/Dialyzer can have false positives. We track them but don't block on them.

**Q: What if tests take too long?**
A: Pre-commit runs minimal checks (~10s). Pre-push runs full suite (~2min). Optimize slow tests.

**Q: How do I see quality trends?**
A: Run `./tools/quality-dashboard.sh` to see trends over last 5 runs.

**Q: What's the difference between quick-check and quality-gate?**
A: `quick-check.sh` is fast (<30s), `quality-gate.sh` is comprehensive (~2min).

---

## Quick Reference Card

```
┌─────────────────────────────────────────────────────────────┐
│ ERLMCP QUALITY GATES CHEAT SHEET                            │
├─────────────────────────────────────────────────────────────┤
│ SETUP (one-time)                                            │
│   ./tools/install-hooks.sh                                  │
│   Add {rebar3_format, "1.3.0"} to rebar.config             │
│                                                             │
│ DAILY WORKFLOW                                              │
│   rebar3 format          # Format code                     │
│   git commit             # Auto-validates (~10s)           │
│   git push               # Auto-validates (~2min)          │
│                                                             │
│ MANUAL CHECKS                                               │
│   ./tools/quick-check.sh         # Fast (<30s)             │
│   ./tools/quality-gate.sh        # Full (~2min)            │
│   ./tools/quality-dashboard.sh   # View metrics            │
│                                                             │
│ BLOCKING GATES (must pass)                                  │
│   ✅ Compilation: 0 errors                                 │
│   ✅ Format: 100%                                          │
│   ✅ Tests: 100% pass rate                                 │
│   ✅ Coverage: ≥80%                                        │
│                                                             │
│ REPORTING GATES (non-blocking)                              │
│   ⚠️  Xref: warnings tracked                               │
│   ⚠️  Dialyzer: warnings tracked                           │
│                                                             │
│ BYPASS (emergencies only!)                                  │
│   git commit --no-verify                                   │
│   git push --no-verify                                     │
└─────────────────────────────────────────────────────────────┘
```

---

**Last Updated:** 2026-01-30
**Version:** 1.0.0
**Feedback:** See `docs/development/quality_gates_plan.md` for full documentation
