# Claude Code Hook Integration - Quick Start

## 30-Second Setup

```bash
# Install all hooks
./tools/claude-md-sync.sh

# Test validation
./tools/claude-md-enforcer.sh
```

Done! Quality rules from CLAUDE.md are now automatically enforced.

## What Just Happened?

1. Git pre-commit hook installed at `.git/hooks/pre-commit`
2. Claude Code post-task hook ready at `.claude/hooks/post-task-validate.sh`
3. All scripts made executable
4. Validation engine ready

## How It Works

```
┌─────────────────────────────────────────────────┐
│  Developer writes code                          │
└────────────────┬────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────┐
│  git commit (or Claude Code task completes)     │
└────────────────┬────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────┐
│  Hook triggers validation                       │
│  - Runs: ./tools/claude-md-enforcer.sh          │
│  - Checks: compile, tests, coverage, dialyzer   │
└────────────────┬────────────────────────────────┘
                 │
        ┌────────┴────────┐
        │                 │
        ▼                 ▼
    ✅ Pass          ❌ Fail
        │                 │
        ▼                 ▼
  Commit/Task      Block with
  proceeds         error report
```

## Quality Rules Enforced

From CLAUDE.md line 74:

| Rule | Threshold | Blocking |
|------|-----------|----------|
| Compilation | 0 errors | Yes |
| Tests | 100% pass rate | Yes |
| Coverage | ≥80% | Yes |
| Dialyzer | 0 warnings (goal) | No |
| Xref | Clean | No |

## Testing It

### Test 1: Manual Validation

```bash
$ ./tools/claude-md-enforcer.sh
[INFO] CLAUDE.md Quality Rules Enforcer
[INFO] ✅ Compilation passed (0 errors)
[INFO] ✅ Tests passed: 42/42 (100%)
[INFO] ✅ Coverage: 85% (≥80%)
[INFO] ✅ All quality rules satisfied
```

### Test 2: Git Commit

```bash
# Make a change
$ echo "% comment" >> src/erlmcp_client.erl

# Stage it
$ git add src/erlmcp_client.erl

# Commit (hook runs automatically)
$ git commit -m "Add comment"
[HOOK:pre-commit-validate] Pre-Commit Validation Hook
[INFO] Running CLAUDE.md quality rules validation...
[INFO] ✅ Compilation passed (0 errors)
[INFO] ✅ Tests passed: 42/42 (100%)
[HOOK:pre-commit-validate] ✅ Pre-commit validation PASSED
[main abc1234] Add comment
```

### Test 3: Force Failure

```bash
# Break a test
$ vim test/erlmcp_client_tests.erl
# (introduce a test failure)

# Try to commit
$ git commit -m "Broken test"
[HOOK:pre-commit-validate] Pre-Commit Validation Hook
[ERROR] ❌ Test pass rate: 97% (required: ≥100%)
[ERROR] Failed tests: 1 / 40
[HOOK:pre-commit-validate] ❌ Pre-commit validation FAILED
[HOOK:pre-commit-validate] Commit BLOCKED

# Fix the test, then commit succeeds
```

## Bypass (Emergency Only)

```bash
# Skip validation (not recommended)
$ git commit --no-verify -m "Emergency fix"

# Then immediately fix and commit properly
$ ./tools/claude-md-enforcer.sh
$ git commit -m "Fix quality violations"
```

## Files Created

```
erlmcp/
├── tools/
│   ├── claude-md-enforcer.sh     (6.9 KB) - Validation engine
│   └── claude-md-sync.sh          (5.9 KB) - Hook installer
├── .claude/hooks/
│   ├── post-task-validate.sh      (2.0 KB) - Claude Code hook
│   └── pre-commit-validate.sh     (2.3 KB) - Pre-commit hook
├── .git/hooks/
│   └── pre-commit                 (435 B)  - Git hook (auto-generated)
└── docs/claude-code/
    ├── README.md                  (6.5 KB) - Overview
    ├── HOOK_INTEGRATION.md        (24 KB)  - Complete guide
    └── QUICK_START.md             (This file)
```

## Troubleshooting

### Hook not running?

```bash
# Re-install
./tools/claude-md-sync.sh

# Check installation
ls -la .git/hooks/pre-commit
```

### Validation too slow?

```bash
# Run only compilation + tests (skip dialyzer)
# Edit tools/claude-md-enforcer.sh to customize
```

### Need more details?

```bash
# See full architecture
cat docs/claude-code/HOOK_INTEGRATION.md

# See all quality tools
ls -lh tools/*.sh
```

## What's Next?

The hooks are now active. Every commit and Claude Code task completion will be validated automatically.

**Normal workflow:**
1. Write code
2. Commit (validation happens automatically)
3. If validation fails, fix and commit again
4. If validation passes, commit goes through

**No manual steps required!**

## Advanced

### Customize Rules

Edit CLAUDE.md line 74:
```markdown
**Targets:** 0 errors, 100% test pass, ≥80% coverage, <10% perf regression.
```

Then sync:
```bash
./tools/claude-md-sync.sh
```

### Add to CI/CD

Add to `.github/workflows/ci.yml`:
```yaml
- name: Validate CLAUDE.md rules
  run: |
    chmod +x tools/claude-md-enforcer.sh
    ./tools/claude-md-enforcer.sh
```

### Monitor Quality Trends

```bash
# Track metrics over time
./tools/metrics/quality-snapshot.sh

# View trends
./tools/metrics/quality-trend.sh
```

## Philosophy

This hook integration embodies "shift-left" quality:

- **Prevention:** Catch violations before they enter git history
- **Fast Feedback:** Local validation in seconds (not minutes in CI)
- **Consistency:** Same rules everywhere (local, CI, production)
- **Automation:** No manual steps, no forgotten checks

Result: Higher quality code with less effort.

---

**Need help?** See [HOOK_INTEGRATION.md](HOOK_INTEGRATION.md) for complete documentation.
