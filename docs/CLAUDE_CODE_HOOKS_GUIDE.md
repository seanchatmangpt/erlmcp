# Claude Code Hooks Integration Guide for erlmcp

**Version:** 2.0.0
**Updated:** 2026-01-28
**Project:** erlmcp - Erlang/OTP MCP SDK

This guide explains how to configure and use Claude Code hooks to enforce CLAUDE.md quality rules automatically in the erlmcp project.

---

## Table of Contents

1. [Overview](#overview)
2. [Hook Architecture](#hook-architecture)
3. [Configuration Files](#configuration-files)
4. [Hook Scripts](#hook-scripts)
5. [Makefile Integration](#makefile-integration)
6. [Setup Instructions](#setup-instructions)
7. [Usage Examples](#usage-examples)
8. [Troubleshooting](#troubleshooting)

---

## Overview

erlmcp uses a comprehensive hook system to enforce quality gates defined in `CLAUDE.md`:

- **Compilation:** 0 errors (blocking)
- **Tests:** 100% pass rate (blocking)
- **Coverage:** ≥80% (blocking)
- **Dialyzer:** 0 type warnings (reported)
- **Xref:** 0 undefined calls (reported)
- **Benchmarks:** <10% performance regression (blocking if perf code changed)

### Hook Trigger Points

1. **Pre-Tool Use:** Before Claude Code executes commands or edits files
2. **Post-Tool Use:** After commands/edits complete
3. **Pre-Commit:** Before git commits (standard git hook)
4. **Post-Task:** After agent task completion
5. **Pre-Compact:** Before context window compaction
6. **Stop:** On session end

---

## Hook Architecture

```
┌─────────────────────────────────────────────────────────┐
│                  Claude Code Session                    │
└─────────────────────────────────────────────────────────┘
                        │
                        ├──► Pre-Tool Use Hooks
                        │    ├─ Bash commands → pre-command validation
                        │    └─ File edits → pre-edit context loading
                        │
                        ├──► Tool Execution
                        │
                        ├──► Post-Tool Use Hooks
                        │    ├─ Bash commands → metrics tracking
                        │    └─ File edits → auto-format + memory update
                        │
                        ├──► Pre-Commit Hook (git)
                        │    └─ CLAUDE.md enforcer (BLOCKING)
                        │
                        ├──► Post-Task Hook
                        │    └─ Task validation (BLOCKING)
                        │
                        └──► Stop Hook
                             └─ Session end (summary + metrics)
```

### Hook Execution Flow

```
┌──────────────┐
│  User Action │
└──────┬───────┘
       │
       ├─ Claude Code detects tool use
       │
       ├─ Pre-Tool Hook executes
       │  ├─ Validates safety
       │  ├─ Loads context
       │  └─ Prepares resources
       │
       ├─ Tool executes (Bash, Edit, etc.)
       │
       ├─ Post-Tool Hook executes
       │  ├─ Formats code
       │  ├─ Updates memory
       │  └─ Tracks metrics
       │
       └─ Result returned to user
```

---

## Configuration Files

### 1. `.claude/settings.json`

Primary Claude Code configuration file.

**Location:** `/Users/sac/erlmcp/.claude/settings.json`

```json
{
  "env": {
    "CLAUDE_FLOW_AUTO_COMMIT": "false",
    "CLAUDE_FLOW_AUTO_PUSH": "false",
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true",
    "CLAUDE_FLOW_REMOTE_EXECUTION": "true",
    "CLAUDE_FLOW_CHECKPOINTS_ENABLED": "true"
  },
  "permissions": {
    "allow": [
      "Bash(npx claude-flow *)",
      "Bash(rebar3 *)",
      "Bash(make *)",
      "Bash(git status)",
      "Bash(git diff *)",
      "Bash(git log *)",
      "Bash(git add *)",
      "Bash(git commit *)",
      "Bash(jq *)",
      "Bash(pwd)",
      "Bash(ls *)"
    ],
    "deny": [
      "Bash(rm -rf /)",
      "Bash(curl * | bash)",
      "Bash(wget * | sh)",
      "Bash(eval *)"
    ]
  },
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks pre-command --command '{}' --validate-safety true --prepare-resources true"
          }
        ]
      },
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // .tool_input.path // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks pre-edit --file '{}' --auto-assign-agents true --load-context true"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks post-command --command '{}' --track-metrics true --store-results true"
          }
        ]
      },
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // .tool_input.path // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks post-edit --file '{}' --format true --update-memory true"
          }
        ]
      }
    ],
    "PreCompact": [
      {
        "matcher": "manual",
        "hooks": [
          {
            "type": "command",
            "command": "/bin/bash -c 'echo \"🔄 PreCompact Guidance:\"; echo \"📋 Review CLAUDE.md for quality gates before compact\"; echo \"✅ Ready for compact operation\"'"
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "npx claude-flow@alpha hooks session-end --generate-summary true --persist-state true --export-metrics true"
          }
        ]
      }
    ]
  },
  "includeCoAuthoredBy": true,
  "enabledMcpjsonServers": ["claude-flow", "ruv-swarm"]
}
```

### Key Configuration Sections

#### Environment Variables

```json
"env": {
  "CLAUDE_FLOW_HOOKS_ENABLED": "true",        // Enable/disable all hooks
  "CLAUDE_FLOW_TELEMETRY_ENABLED": "true",    // Track metrics
  "CLAUDE_FLOW_CHECKPOINTS_ENABLED": "true"   // Enable state snapshots
}
```

#### Permissions

```json
"permissions": {
  "allow": [
    "Bash(rebar3 *)",      // Allow all rebar3 commands
    "Bash(make *)",        // Allow all make targets
    "Bash(git status)"     // Read-only git commands
  ],
  "deny": [
    "Bash(rm -rf /)",      // Block destructive commands
    "Bash(eval *)"         // Block eval for security
  ]
}
```

#### Hook Matchers

- **`Bash`** - Matches bash command execution
- **`Write|Edit|MultiEdit`** - Matches file operations
- **`manual`** - User-triggered compact
- **`auto`** - Automatic context window compact

---

## Hook Scripts

### 1. Pre-Commit Validation Hook

**Location:** `/Users/sac/erlmcp/.claude/hooks/pre-commit-validate.sh`

**Purpose:** Validates code before git commits (BLOCKING)

**Triggers:**
- Git commit operations via `.git/hooks/pre-commit`
- Manual execution: `./.claude/hooks/pre-commit-validate.sh`

**Checks:**
- ✅ Compilation (0 errors)
- ✅ Tests (100% pass rate)
- ✅ Coverage (≥80%)
- ✅ Dialyzer (type checking)
- ✅ Xref (undefined calls)

**Exit Codes:**
- `0` - All checks passed, commit allowed
- `1` - Violations detected, commit BLOCKED

**Example Output:**

```bash
[HOOK:pre-commit-validate] Pre-Commit Validation Hook
[HOOK:pre-commit-validate] Project: erlmcp

[INFO] Parsing quality rules from CLAUDE.md...
[INFO] Quality targets extracted:
  - Compilation errors: ≤0
  - Test pass rate: ≥100%
  - Coverage: ≥80%

[INFO] Validating compilation...
✅ Compilation passed (0 errors)

[INFO] Validating tests...
✅ Tests passed: 42/42 (100%)

[HOOK:pre-commit-validate] ✅ Pre-commit validation PASSED
[HOOK:pre-commit-validate] Commit approved
```

**Bypass (NOT RECOMMENDED):**

```bash
git commit --no-verify
```

---

### 2. Post-Task Validation Hook

**Location:** `/Users/sac/erlmcp/.claude/hooks/post-task-validate.sh`

**Purpose:** Validates agent task completion against CLAUDE.md rules

**Triggers:**
- After agent task completion (automatic)
- Manual execution: `./.claude/hooks/post-task-validate.sh <task-id> "<description>"`

**Usage:**

```bash
./.claude/hooks/post-task-validate.sh task-123 "Implement feature X"
```

**Checks:** Same as pre-commit (compilation, tests, coverage, etc.)

**Exit Codes:**
- `0` - Task validation passed
- `1` - Violations detected, task BLOCKED

---

### 3. CLAUDE.md Enforcer

**Location:** `/Users/sac/erlmcp/tools/claude-md-enforcer.sh`

**Purpose:** Core validation engine used by all hooks

**Direct Usage:**

```bash
./tools/claude-md-enforcer.sh
```

**Validation Flow:**

1. **Parse CLAUDE.md** - Extract quality targets (line 74):
   ```
   Targets: 0 errors, 100% test pass, ≥80% coverage, <10% perf regression
   ```

2. **Run Validations:**
   - `validate_compilation()` - Check `rebar3 compile` output
   - `validate_tests()` - Check `rebar3 eunit` results
   - `validate_coverage()` - Check `rebar3 cover` percentage
   - `validate_dialyzer()` - Check dialyzer warnings
   - `validate_xref()` - Check xref issues

3. **Report Results:**
   ```
   [INFO] ✅ All quality rules satisfied
   ```
   OR
   ```
   [ERROR] ❌ 2 quality rule violation(s) detected
   [ERROR] Fix violations before proceeding
   ```

**Logs:**
- `/tmp/erlmcp_compile.log` - Compilation output
- `/tmp/erlmcp_tests.log` - Test results
- `/tmp/erlmcp_coverage.log` - Coverage report
- `/tmp/erlmcp_dialyzer.log` - Dialyzer output
- `/tmp/erlmcp_xref.log` - Xref results

---

### 4. CLAUDE.md Sync Utility

**Location:** `/Users/sac/erlmcp/tools/claude-md-sync.sh`

**Purpose:** Synchronize hooks with CLAUDE.md updates

**Usage:**

```bash
./tools/claude-md-sync.sh
```

**Actions:**
- Re-parses CLAUDE.md quality rules
- Updates hook scripts with new thresholds
- Installs/updates git hooks
- Validates hook configuration

---

### 5. Git Hooks Installer

**Location:** `/Users/sac/erlmcp/tools/install-hooks.sh`

**Purpose:** Install git hooks for quality enforcement

**Usage:**

```bash
./tools/install-hooks.sh
```

**Installs:**
- `.git/hooks/pre-commit` → Quality gate validation
- `.git/hooks/pre-push` → Performance regression check

**Backs up existing hooks:**
```
.git/hooks/pre-commit.backup.20260128_103000
```

---

## Makefile Integration

The erlmcp Makefile provides quality gate targets that integrate with hooks.

**Location:** `/Users/sac/erlmcp/Makefile`

### Quality Gate Targets

#### 1. `make validate` (Master Quality Gate)

Runs ALL quality gates (BLOCKING):

```bash
make validate
```

**Checks:**
- ✅ Compilation (0 errors)
- ✅ Tests (0 failures)
- ✅ Coverage (≥80%)
- ✅ Quality (dialyzer + xref)
- ✅ Benchmarks (<10% regression)

**Exit:** `0` = pass, `1` = fail (blocks commit/release)

**Example Output:**

```
════════════════════════════════════════════════════════════
✅ ALL QUALITY GATES PASSED - READY FOR PRODUCTION
════════════════════════════════════════════════════════════

✓ Compilation: All modules compiled successfully (0 errors)
✓ Tests: All tests passed (0 failures)
✓ Coverage: ≥80% code coverage achieved
✓ Quality: Dialyzer + Xref passed (0 warnings)
✓ Benchmarks: No performance regression (<10%)
```

---

#### 2. Individual Quality Gates

**`make validate-compile`**

```bash
make validate-compile
```

Checks: `TERM=dumb rebar3 compile` exits 0

---

**`make validate-test`**

```bash
make validate-test
```

Checks: `rebar3 eunit` + `rebar3 ct` pass 100%

---

**`make validate-coverage`**

```bash
make validate-coverage
```

Checks: Coverage ≥80%

---

**`make validate-quality`**

```bash
make validate-quality
```

Checks: Dialyzer + Xref clean

---

**`make validate-bench`**

```bash
make validate-bench
```

Checks: No performance regression (requires `scripts/bench/check_regression.sh`)

---

#### 3. TCPS Manufacturing Targets

**`make jidoka`** (自働化 - Built-in Quality)

```bash
make jidoka
```

Runs TCPS Jidoka quality gate (stop-the-line authority).

**Script:** `tools/tcps/jidoka_quality_gate.sh`

---

**`make andon`** (行灯 - Visual Alert)

```bash
make andon
```

Shows Andon board status (production line status).

**Script:** `tools/tcps/andon_cord.sh status`

---

**`make poka-yoke`** (ポカヨケ - Error-Proofing)

```bash
make poka-yoke
```

Runs error-proofing validation checks.

**Script:** `tools/tcps/poka_yoke_validator.sh`

---

**`make release-validate`**

```bash
make release-validate
```

Full release validation with quality receipt generation.

**Checks:**
- All `validate-*` targets
- Jidoka quality gates
- Generates quality receipt (レシート)

**Script:** `tools/tcps/generate-quality-receipt.sh`

---

## Setup Instructions

### Initial Setup

1. **Clone the repository:**

   ```bash
   git clone https://github.com/yourusername/erlmcp.git
   cd erlmcp
   ```

2. **Install git hooks:**

   ```bash
   ./tools/install-hooks.sh
   ```

   **Output:**
   ```
   ═══════════════════════════════════════════════════════
      ERLMCP Git Hooks Installer
   ═══════════════════════════════════════════════════════

   ✓ Made quality-gate.sh executable
   ✓ Installed pre-commit hook
   ✓ Installed pre-push hook

   ═══════════════════════════════════════════════════════
      Installation Complete
   ═══════════════════════════════════════════════════════
   ```

3. **Verify Claude Code settings:**

   ```bash
   cat .claude/settings.json
   ```

   Ensure `CLAUDE_FLOW_HOOKS_ENABLED: "true"`

4. **Test quality gates:**

   ```bash
   make validate
   ```

---

### Claude Code Configuration

If `.claude/settings.json` doesn't exist, create it:

```bash
mkdir -p .claude
cat > .claude/settings.json << 'EOF'
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true"
  },
  "permissions": {
    "allow": [
      "Bash(rebar3 *)",
      "Bash(make *)",
      "Bash(git status)",
      "Bash(git diff *)"
    ],
    "deny": [
      "Bash(rm -rf /)",
      "Bash(eval *)"
    ]
  },
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks pre-command --command '{}' --validate-safety true"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks post-edit --file '{}' --format true"
          }
        ]
      }
    ]
  }
}
EOF
```

---

### Updating Hooks

When CLAUDE.md changes, sync hooks:

```bash
./tools/claude-md-sync.sh
```

---

## Usage Examples

### Example 1: Development Workflow with Hooks

```bash
# 1. Start feature development
git checkout -b feature/new-transport

# 2. Make code changes
vim src/erlmcp_transport_websocket.erl

# 3. Claude Code post-edit hook runs automatically:
#    - Formats code
#    - Updates memory

# 4. Compile and test
make compile
make test

# 5. Try to commit
git add src/erlmcp_transport_websocket.erl
git commit -m "Add WebSocket transport"

# Pre-commit hook runs automatically:
[HOOK:pre-commit-validate] Pre-Commit Validation Hook
[INFO] Validating compilation...
✅ Compilation passed (0 errors)

[INFO] Validating tests...
✅ Tests passed: 45/45 (100%)

[HOOK:pre-commit-validate] ✅ Pre-commit validation PASSED
[HOOK:pre-commit-validate] Commit approved

# 6. Commit succeeds
[main abc1234] Add WebSocket transport
 1 file changed, 150 insertions(+)

# 7. Before push, run full validation
make validate

# 8. Push to remote
git push origin feature/new-transport
```

---

### Example 2: Fixing Hook Violations

```bash
# Try to commit with failing tests
git commit -m "WIP: refactoring"

# Pre-commit hook BLOCKS:
[HOOK:pre-commit-validate] Pre-Commit Validation Hook

[INFO] Validating tests...
[ERROR] Test pass rate: 95% (required: ≥100%)
[ERROR] Failed tests: 2 / 40

[HOOK:pre-commit-validate] ❌ Pre-commit validation FAILED
[HOOK:pre-commit-validate] Commit BLOCKED

[ERROR] RESOLUTION REQUIRED:
[ERROR] 1. Review violations reported above
[ERROR] 2. Fix issues in code
[ERROR] 3. Re-run validation: ./tools/claude-md-enforcer.sh
[ERROR] 4. Stage fixed files: git add <files>
[ERROR] 5. Retry commit: git commit

# Fix tests
vim test/erlmcp_transport_websocket_tests.erl

# Re-run validation
./tools/claude-md-enforcer.sh

[INFO] ✅ All quality rules satisfied

# Stage and commit again
git add test/erlmcp_transport_websocket_tests.erl
git commit -m "Fix WebSocket transport tests"

# Now passes:
[HOOK:pre-commit-validate] ✅ Pre-commit validation PASSED
```

---

### Example 3: Manual Hook Execution

```bash
# Run pre-commit validation manually (without git commit)
./.claude/hooks/pre-commit-validate.sh

# Run post-task validation manually
./.claude/hooks/post-task-validate.sh task-42 "Implement caching"

# Run CLAUDE.md enforcer directly
./tools/claude-md-enforcer.sh
```

---

### Example 4: Release Workflow with TCPS

```bash
# 1. Run full quality gates
make validate

# 2. Run TCPS Jidoka quality system
make jidoka

# 3. Check Andon board status
make andon

# 4. Run error-proofing validation
make poka-yoke

# 5. Generate quality receipt for release
make release-validate

# If all pass:
════════════════════════════════════════════════════════════
🎉 RELEASE READY - 認証 (CERTIFIED)
════════════════════════════════════════════════════════════

✓ All quality gates passed
✓ Jidoka validation complete
✓ Quality receipt generated: priv/receipts/release-v2.0.0.json
✓ Ready for production deployment

# 6. Create release
git tag -a v2.0.0 -m "Release v2.0.0 - Certified quality"
git push origin v2.0.0
```

---

## Troubleshooting

### Issue 1: Hooks Not Running

**Symptoms:**
- Commits succeed without validation
- No hook output in terminal

**Diagnosis:**

```bash
# Check if hooks are installed
ls -la .git/hooks/pre-commit

# Check CLAUDE.md enforcer exists
ls -la tools/claude-md-enforcer.sh

# Check Claude Code settings
cat .claude/settings.json | jq '.env.CLAUDE_FLOW_HOOKS_ENABLED'
```

**Fix:**

```bash
# Re-install hooks
./tools/install-hooks.sh

# Make hooks executable
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/pre-push
chmod +x .claude/hooks/*.sh
chmod +x tools/*.sh
```

---

### Issue 2: Hook Failing with "CLAUDE.md not found"

**Symptoms:**

```
[ERROR] CLAUDE.md not found at /path/to/CLAUDE.md
```

**Fix:**

Ensure `CLAUDE.md` exists in project root:

```bash
ls -la CLAUDE.md
```

If missing, create from template or restore from git:

```bash
git checkout main -- CLAUDE.md
```

---

### Issue 3: Coverage Check Failing

**Symptoms:**

```
[ERROR] Coverage: 65% (required: ≥80%)
```

**Fix:**

1. Identify uncovered modules:

   ```bash
   rebar3 cover
   cat _build/test/cover/index.html
   ```

2. Add tests for uncovered code:

   ```bash
   # Create test file
   vim test/erlmcp_module_tests.erl

   # Run tests
   rebar3 eunit
   ```

3. Verify coverage:

   ```bash
   make validate-coverage
   ```

---

### Issue 4: Dialyzer PLT Not Built

**Symptoms:**

```
[WARN] Dialyzer PLT not built, skipping dialyzer validation
```

**Fix:**

```bash
# Build PLT (one-time setup, takes ~5-10 minutes)
rebar3 dialyzer

# Now validation will include dialyzer
make validate-quality
```

---

### Issue 5: Benchmark Script Missing

**Symptoms:**

```
[ERROR] BENCHMARK SCRIPT MISSING
Refusal Code: MISSING_TOOL_BENCHMARK
Remediation: Create scripts/bench/check_regression.sh
```

**Fix:**

Create benchmark script or disable benchmark validation:

**Option 1: Create script**

```bash
mkdir -p scripts/bench
cat > scripts/bench/check_regression.sh << 'EOF'
#!/usr/bin/env bash
# Quick benchmark regression check
# Exit 0: No regression
# Exit 1: Regression detected

# TODO: Implement actual benchmark comparison
exit 0
EOF
chmod +x scripts/bench/check_regression.sh
```

**Option 2: Skip benchmark validation**

Edit `Makefile` and remove `validate-bench` from `validate` target:

```makefile
validate: validate-compile validate-test validate-coverage validate-quality
```

---

### Issue 6: Bypassing Hooks (Emergency)

**When to use:** ONLY in emergencies (merge commits, hotfixes)

**Git commits:**

```bash
git commit --no-verify -m "Emergency hotfix"
```

**Git push:**

```bash
git push --no-verify
```

**Warning:** This bypasses ALL quality checks. Use sparingly and fix violations ASAP.

---

### Issue 7: Hook Logs Not Found

**Check logs:**

```bash
ls -la /tmp/erlmcp_*.log

# View specific log
cat /tmp/erlmcp_compile.log
cat /tmp/erlmcp_tests.log
cat /tmp/erlmcp_coverage.log
```

---

### Issue 8: TCPS Scripts Missing

**Symptoms:**

```
[WARN] TCPS jidoka script not found (expected: tools/tcps/jidoka_quality_gate.sh)
```

**Fix:**

Create TCPS scripts or they'll fallback to standard validation:

```bash
mkdir -p tools/tcps

# Jidoka falls back to make validate
# Andon falls back to "no issues detected"
# Poka-yoke falls back to validate-compile/test/quality
```

---

## Advanced Configuration

### Custom Hook Scripts

Add custom validation to `.claude/hooks/`:

**Example: Custom Security Check**

```bash
cat > .claude/hooks/security-check.sh << 'EOF'
#!/usr/bin/env bash
# Custom security validation

# Check for hardcoded secrets
if grep -r "password\s*=\s*\"" src/; then
    echo "ERROR: Hardcoded password detected"
    exit 1
fi

echo "✅ Security check passed"
exit 0
EOF

chmod +x .claude/hooks/security-check.sh
```

**Call from pre-commit hook:**

Edit `.git/hooks/pre-commit`:

```bash
# Run custom security check
./.claude/hooks/security-check.sh || exit 1

# Then run standard validation
./tools/claude-md-enforcer.sh || exit 1
```

---

### Environment-Specific Configuration

**Development vs CI:**

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "${CI:-true}",
    "CLAUDE_FLOW_STRICT_MODE": "${CI:-false}"
  }
}
```

In CI:

```bash
export CI=true
export CLAUDE_FLOW_STRICT_MODE=true
```

---

### Hook Telemetry

Track hook execution metrics:

```bash
# Enable telemetry in settings.json
"env": {
  "CLAUDE_FLOW_TELEMETRY_ENABLED": "true"
}

# View metrics
npx claude-flow@alpha hooks metrics
```

---

## Reference

### Hook Script Locations

| Hook Type | Script Path | Purpose |
|-----------|-------------|---------|
| Pre-Commit (git) | `.git/hooks/pre-commit` | Git commit validation |
| Pre-Commit (Claude) | `.claude/hooks/pre-commit-validate.sh` | Quality gate check |
| Post-Task | `.claude/hooks/post-task-validate.sh` | Task completion check |
| Enforcer | `tools/claude-md-enforcer.sh` | Core validation engine |
| Installer | `tools/install-hooks.sh` | Hook installation |
| Sync | `tools/claude-md-sync.sh` | Sync hooks with CLAUDE.md |

---

### Makefile Quality Targets

| Target | Checks | Blocking |
|--------|--------|----------|
| `make validate` | All gates | ✅ Yes |
| `make validate-compile` | Compilation | ✅ Yes |
| `make validate-test` | Tests | ✅ Yes |
| `make validate-coverage` | Coverage ≥80% | ✅ Yes |
| `make validate-quality` | Dialyzer + Xref | ✅ Yes |
| `make validate-bench` | Perf regression | ✅ Yes |
| `make jidoka` | TCPS quality | ✅ Yes |
| `make andon` | Status display | ❌ No |
| `make poka-yoke` | Error-proofing | ✅ Yes |
| `make release-validate` | Full release | ✅ Yes |

---

### Quality Thresholds (from CLAUDE.md)

| Metric | Threshold | Source |
|--------|-----------|--------|
| Compilation errors | 0 | Line 74 |
| Test pass rate | 100% | Line 74 |
| Code coverage | ≥80% | Line 74 |
| Performance regression | <10% | Line 74 |
| Dialyzer warnings | 0 (reported) | Lines 84-85 |
| Xref undefined calls | 0 (reported) | Lines 84-85 |

---

### Exit Codes

| Code | Meaning | Action |
|------|---------|--------|
| 0 | Success | Proceed |
| 1 | Violation | Block (fix required) |
| 2 | Warning | Proceed with caution |

---

## Best Practices

1. **Never bypass hooks** unless absolute emergency
2. **Run `make validate` before pushing** to catch issues early
3. **Keep CLAUDE.md updated** with current quality standards
4. **Sync hooks after CLAUDE.md changes** via `./tools/claude-md-sync.sh`
5. **Check hook logs** in `/tmp/erlmcp_*.log` when debugging
6. **Use TCPS targets** for production releases (`make release-validate`)
7. **Enable telemetry** to track quality trends
8. **Document custom hooks** in project wiki/docs

---

## Additional Resources

- **CLAUDE.md:** Project quality standards (root of repo)
- **Makefile:** Quality gate targets and automation
- **TCPS Guide:** `.claude/TCPS_SYSTEM_COMPLETE.md`
- **Architecture:** `docs/architecture.md`
- **OTP Patterns:** `docs/otp-patterns.md`

---

## Support

**Issues:** https://github.com/yourusername/erlmcp/issues
**Docs:** https://erlmcp.dev/docs
**Community:** #erlmcp on Slack

---

**Last Updated:** 2026-01-28
**Version:** 2.0.0
**Maintainer:** erlmcp core team
