# Claude Code Hooks - Quick Reference Card

**Project:** erlmcp v2.0.0
**Updated:** 2026-01-28

---

## 🚀 Quick Start (30 seconds)

```bash
# 1. Install hooks
./tools/install-hooks.sh

# 2. Verify settings exist
cat .claude/settings.json

# 3. Test validation
./tools/claude-md-enforcer.sh

# 4. Done! Hooks active on next commit
```

---

## 📁 File Locations

| File | Purpose |
|------|---------|
| `.claude/settings.json` | Main hook configuration |
| `.claude/hooks/pre-commit-validate.sh` | Git pre-commit validation |
| `.claude/hooks/post-task-validate.sh` | Task completion check |
| `tools/claude-md-enforcer.sh` | Quality gate engine |
| `tools/install-hooks.sh` | Hook installer |
| `.git/hooks/pre-commit` | Git hook (auto-installed) |

---

## 🎯 Quality Gates (from CLAUDE.md)

| Gate | Threshold | Blocking |
|------|-----------|----------|
| Compilation | 0 errors | ✅ Yes |
| Tests | 100% pass | ✅ Yes |
| Coverage | ≥80% | ✅ Yes |
| Dialyzer | 0 warnings | ⚠️ Reported |
| Xref | 0 undefined | ⚠️ Reported |
| Performance | <10% regression | ✅ Yes (if changed) |

---

## 🔧 Essential Commands

### Validation

```bash
# Run all quality gates (BLOCKING)
make validate

# Individual gates
make validate-compile    # Compilation check
make validate-test       # Test pass rate
make validate-coverage   # Coverage ≥80%
make validate-quality    # Dialyzer + Xref
make validate-bench      # Performance check

# Manual hook execution
./.claude/hooks/pre-commit-validate.sh
./tools/claude-md-enforcer.sh
```

### TCPS Manufacturing

```bash
make jidoka              # Stop-the-line quality (自働化)
make andon               # Status board (行灯)
make poka-yoke           # Error-proofing (ポカヨケ)
make release-validate    # Full release validation
```

### Hook Management

```bash
# Install/reinstall hooks
./tools/install-hooks.sh

# Sync hooks with CLAUDE.md
./tools/claude-md-sync.sh

# Test hooks
git commit --dry-run
```

---

## 🔌 Hook Types

| Hook | When | Purpose |
|------|------|---------|
| **PreToolUse** | Before Bash/Edit | Validate safety, load context |
| **PostToolUse** | After Bash/Edit | Format code, track metrics |
| **PreCompact** | Before context compact | Load guidance |
| **Stop** | Session end | Summary, export metrics |
| **Pre-Commit** (git) | Before commit | Quality gate (BLOCKING) |
| **Post-Task** | After task | Task validation |

---

## ⚙️ Configuration Templates

### Minimal (Quick Start)

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "true"
  },
  "permissions": {
    "allow": ["Bash(rebar3 *)", "Bash(make *)"],
    "deny": ["Bash(rm -rf /)"]
  },
  "hooks": {}
}
```

### Production (Recommended)

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true"
  },
  "hooks": {
    "PreToolUse": [...],
    "PostToolUse": [...],
    "Stop": [...]
  }
}
```

**See:** `docs/hooks/CONFIGURATION_EXAMPLES.md` for full templates

---

## 🐛 Troubleshooting (1-minute fixes)

### Hooks not running

```bash
# Check enabled
jq '.env.CLAUDE_FLOW_HOOKS_ENABLED' .claude/settings.json
# Should be: "true"

# Reinstall
./tools/install-hooks.sh
```

### Commit blocked

```bash
# Check violations
./tools/claude-md-enforcer.sh

# View logs
cat /tmp/erlmcp_*.log

# Fix and retry
make validate
git commit
```

### Coverage failing

```bash
# Check coverage
rebar3 cover

# View report
open _build/test/cover/index.html

# Add tests for uncovered code
```

### Dialyzer PLT missing

```bash
# Build PLT (one-time, ~5-10 min)
rebar3 dialyzer

# Now dialyzer runs in validate
make validate-quality
```

---

## 🚨 Emergency Bypass (USE SPARINGLY)

```bash
# Skip pre-commit hook (NOT RECOMMENDED)
git commit --no-verify -m "Emergency hotfix"

# Skip pre-push hook
git push --no-verify

# IMPORTANT: Fix violations ASAP after bypass!
```

---

## 📊 Common Workflows

### Development

```bash
# 1. Make changes
vim src/module.erl

# 2. Quick check
make compile test

# 3. Commit (hooks run automatically)
git add src/module.erl
git commit -m "Add feature"

# 4. If blocked, fix and retry
make validate
git commit --amend
```

### Release

```bash
# 1. Full validation
make validate

# 2. TCPS quality system
make jidoka

# 3. Generate receipt
make release-validate

# 4. Tag release
git tag -a v2.0.0 -m "Release v2.0.0 - Certified"
git push origin v2.0.0
```

### CI/CD

```bash
# In CI pipeline (.github/workflows/ci.yml)
- name: Quality Gates
  run: make validate

- name: TCPS Validation
  run: make jidoka
```

---

## 📈 Quality Metrics

### Check Current Status

```bash
# Compilation
TERM=dumb rebar3 compile

# Tests
rebar3 eunit
# Look for: "All X tests passed"

# Coverage
rebar3 cover
# Look for: "total: XX%"

# Dialyzer
rebar3 dialyzer
# Look for: "0 warnings"

# Xref
rebar3 xref
# Look for: "0 undefined"
```

---

## 🔑 Environment Variables

| Variable | Value | Effect |
|----------|-------|--------|
| `CLAUDE_FLOW_HOOKS_ENABLED` | `true/false` | Enable/disable hooks |
| `CLAUDE_FLOW_DEBUG` | `true/false` | Debug logging |
| `CLAUDE_FLOW_STRICT_MODE` | `true/false` | Fail-fast mode |
| `CI` | `true` | CI/CD mode |

**Set in:** `.claude/settings.json` under `"env"`

---

## 📚 Documentation Links

| Document | Purpose |
|----------|---------|
| [CLAUDE_CODE_HOOKS_GUIDE.md](./CLAUDE_CODE_HOOKS_GUIDE.md) | Complete guide (1100+ lines) |
| [CONFIGURATION_EXAMPLES.md](./CONFIGURATION_EXAMPLES.md) | Ready-to-use configs |
| [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) | This card |
| [CLAUDE.md](../../CLAUDE.md) | Quality standards |
| [Makefile](../../Makefile) | Build targets |

---

## 🎓 Key Concepts

### Quality Gates

BLOCKING validations that prevent commits/releases if violated. Based on CLAUDE.md line 74:

```
Targets: 0 errors, 100% test pass, ≥80% coverage, <10% perf regression
```

### TCPS System

Toyota Code Production System - Manufacturing principles for code:

- **自働化 (Jidoka):** Stop-the-line on defects
- **行灯 (Andon):** Visual status alerts
- **ポカヨケ (Poka-yoke):** Error-proofing
- **レシート (Receipt):** Certification evidence

### Hook Execution

1. User action (commit, edit, etc.)
2. Pre-hook validates/prepares
3. Action executes
4. Post-hook formats/tracks
5. Result returned

---

## ⚡ Pro Tips

1. **Run `make validate` before every commit** - Catch issues early
2. **Enable telemetry** - Track quality trends
3. **Never bypass hooks** without good reason
4. **Sync hooks after CLAUDE.md changes** - `./tools/claude-md-sync.sh`
5. **Check logs** when debugging - `/tmp/erlmcp_*.log`
6. **Use TCPS for releases** - `make release-validate`
7. **Keep .claude/settings.json in git** - Share config with team
8. **Test hooks manually first** - `./.claude/hooks/pre-commit-validate.sh`

---

## 📞 Help

**Issues:** https://github.com/yourusername/erlmcp/issues
**Docs:** https://erlmcp.dev/docs
**Community:** #erlmcp on Slack

---

**Need more details?** See [CLAUDE_CODE_HOOKS_GUIDE.md](./CLAUDE_CODE_HOOKS_GUIDE.md)

**Last Updated:** 2026-01-28 | **Version:** 2.0.0
