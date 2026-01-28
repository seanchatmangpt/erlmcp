# Claude Code Hooks Documentation

**Project:** erlmcp v2.0.0
**Last Updated:** 2026-01-28
**Status:** Production Ready ✅

---

## 📚 Documentation Index

This directory contains comprehensive documentation for setting up and using Claude Code hooks in the erlmcp project.

### Quick Access

| Document | Size | Lines | Purpose | Audience |
|----------|------|-------|---------|----------|
| [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) | 6.9KB | 386 | Cheat sheet, 30-sec start | Everyone |
| [CONFIGURATION_EXAMPLES.md](./CONFIGURATION_EXAMPLES.md) | 19KB | 756 | Ready-to-use configs | Developers |
| [CLAUDE_CODE_HOOKS_GUIDE.md](../CLAUDE_CODE_HOOKS_GUIDE.md) | 26KB | 1,184 | Complete reference | Architects |

**Total Documentation:** 52KB, 2,326 lines

---

## 🚀 Start Here (Choose Your Path)

### Path 1: "I need hooks working NOW" (30 seconds)

```bash
# Read this file first
cat QUICK_REFERENCE.md

# Run these commands
./tools/install-hooks.sh
./tools/claude-md-enforcer.sh

# Done! Hooks active on next commit.
```

**Next:** [QUICK_REFERENCE.md](./QUICK_REFERENCE.md)

---

### Path 2: "I need to configure hooks" (5 minutes)

```bash
# Read configuration examples
cat CONFIGURATION_EXAMPLES.md

# Copy a template to .claude/settings.json
# Choose: basic, full, dev, ci, or minimal

# Test configuration
./tools/claude-md-enforcer.sh
```

**Next:** [CONFIGURATION_EXAMPLES.md](./CONFIGURATION_EXAMPLES.md)

---

### Path 3: "I need to understand the system" (30 minutes)

```bash
# Read complete guide
cat ../CLAUDE_CODE_HOOKS_GUIDE.md

# Understand architecture, hooks, integration
# Learn troubleshooting, workflows, examples
```

**Next:** [CLAUDE_CODE_HOOKS_GUIDE.md](../CLAUDE_CODE_HOOKS_GUIDE.md)

---

## 📖 What's in Each Document?

### QUICK_REFERENCE.md (Cheat Sheet)

**When to use:** Daily reference, quick lookups

**Contents:**
- 30-second quick start
- File locations table
- Quality gates summary (6 gates)
- Essential commands (Makefile targets)
- Hook types reference (6 types)
- 1-minute troubleshooting fixes
- Common workflows (dev, release, fix)
- Pro tips (8 tips)

**Format:** Tables, bullet lists, code blocks
**Reading time:** 3-5 minutes
**Use case:** Keep open while coding

---

### CONFIGURATION_EXAMPLES.md (Templates)

**When to use:** Setting up new projects, switching environments

**Contents:**
- 5 configuration templates:
  - Basic (minimal setup)
  - Full (production, recommended)
  - Development (debug enabled)
  - CI/CD (strict mode)
  - Minimal (no hooks)
- 10 hook-specific examples
- Installation script
- Testing procedures
- Environment variables reference
- Permissions reference

**Format:** JSON configs, bash scripts
**Reading time:** 10-15 minutes
**Use case:** Copy/paste configurations

---

### CLAUDE_CODE_HOOKS_GUIDE.md (Complete Reference)

**When to use:** Deep dive, architecture understanding, troubleshooting

**Contents:**
- Hook architecture diagrams
- Full .claude/settings.json explanation
- Hook script documentation (5 scripts)
- Makefile integration guide (10+ targets)
- Step-by-step setup instructions
- Real-world usage examples (4 examples)
- Comprehensive troubleshooting (8 issues)
- Advanced configuration patterns
- Best practices (8 practices)

**Format:** Detailed guide with diagrams, examples, explanations
**Reading time:** 45-60 minutes
**Use case:** Reference manual, onboarding

---

## 🎯 What Are Claude Code Hooks?

Claude Code hooks are automated validations that run during your development workflow to enforce quality standards defined in `CLAUDE.md`.

### Hook Trigger Points

| Hook Type | Trigger | Purpose | Blocking |
|-----------|---------|---------|----------|
| **PreToolUse** | Before Bash/Edit | Validate safety, load context | ❌ No |
| **PostToolUse** | After Bash/Edit | Format code, track metrics | ❌ No |
| **PreCompact** | Before context compact | Load guidance | ❌ No |
| **Stop** | Session end | Summary, export metrics | ❌ No |
| **Pre-Commit** | Before git commit | Quality gates | ✅ Yes |
| **Post-Task** | After agent task | Task validation | ✅ Yes |

### Quality Gates (Blocking)

From `CLAUDE.md` line 74: **Targets: 0 errors, 100% test pass, ≥80% coverage, <10% perf regression**

| Gate | Threshold | Command |
|------|-----------|---------|
| Compilation | 0 errors | `make validate-compile` |
| Tests | 100% pass | `make validate-test` |
| Coverage | ≥80% | `make validate-coverage` |
| Dialyzer | 0 warnings | `make validate-quality` |
| Xref | 0 undefined | `make validate-quality` |
| Performance | <10% regression | `make validate-bench` |

**Master command:** `make validate` (runs all gates)

---

## 🏗️ System Architecture

```
┌─────────────────────────────────────────────────────────┐
│                  Claude Code Session                    │
└─────────────────────────────────────────────────────────┘
                        │
    ┌───────────────────┼───────────────────┐
    │                   │                   │
    ▼                   ▼                   ▼
┌─────────┐      ┌──────────┐      ┌──────────┐
│ Pre-Hook│      │   Tool   │      │Post-Hook │
│ Validate│─────▶│ Execute  │─────▶│ Format   │
│ Safety  │      │ (Bash/   │      │ Metrics  │
└─────────┘      │  Edit)   │      └──────────┘
                 └──────────┘
                        │
                        ▼
                 ┌──────────┐
                 │ Quality  │
                 │  Gate    │──▶ Commit/Block
                 │ (CLAUDE) │
                 └──────────┘
```

### Components

1. **Settings:** `.claude/settings.json` (configuration)
2. **Hook Scripts:** `.claude/hooks/*.sh` (validations)
3. **Tools:** `tools/*.sh` (enforcement)
4. **Makefile:** Quality gate targets
5. **Git Hooks:** `.git/hooks/pre-commit` (blocking)

---

## 🛠️ Common Tasks

### Setup New Project

```bash
# 1. Clone repo
git clone https://github.com/yourusername/erlmcp.git
cd erlmcp

# 2. Install hooks
./tools/install-hooks.sh

# 3. Verify
cat .claude/settings.json
./tools/claude-md-enforcer.sh

# 4. Start coding
# Hooks active automatically
```

---

### Daily Development

```bash
# 1. Make changes
vim src/module.erl

# 2. Quick check
make compile test

# 3. Commit (hooks run auto)
git commit -m "Add feature"

# If blocked:
./tools/claude-md-enforcer.sh  # See violations
# Fix, then retry
```

---

### Release Preparation

```bash
# 1. Full validation
make validate

# 2. TCPS quality system
make jidoka

# 3. Generate receipt
make release-validate

# 4. Tag and push
git tag -a v2.0.0 -m "Release v2.0.0"
git push origin v2.0.0
```

---

### Troubleshooting

```bash
# Hooks not running?
jq '.env.CLAUDE_FLOW_HOOKS_ENABLED' .claude/settings.json
./tools/install-hooks.sh

# Commit blocked?
./tools/claude-md-enforcer.sh
cat /tmp/erlmcp_*.log
# Fix violations, retry

# Coverage failing?
rebar3 cover
open _build/test/cover/index.html
# Add tests
```

---

## 📁 Project Structure

```
erlmcp/
├── .claude/
│   ├── settings.json                    # Hook configuration (115 lines)
│   └── hooks/
│       ├── pre-commit-validate.sh       # Pre-commit validator (98 lines)
│       └── post-task-validate.sh        # Post-task validator (86 lines)
│
├── tools/
│   ├── claude-md-enforcer.sh            # Quality gate engine (259 lines)
│   ├── claude-md-sync.sh                # Sync with CLAUDE.md (~200 lines)
│   └── install-hooks.sh                 # Hook installer (99 lines)
│
├── .git/hooks/
│   ├── pre-commit                       # Git hook (installed automatically)
│   └── pre-push                         # Git hook (installed automatically)
│
├── docs/
│   ├── CLAUDE_CODE_HOOKS_GUIDE.md       # Complete guide (26KB, 1,184 lines)
│   └── hooks/
│       ├── README.md                    # This file
│       ├── QUICK_REFERENCE.md           # Cheat sheet (6.9KB, 386 lines)
│       └── CONFIGURATION_EXAMPLES.md    # Templates (19KB, 756 lines)
│
├── Makefile                             # Quality gate targets (670 lines)
└── CLAUDE.md                            # Quality standards (project root)
```

---

## 🎓 Learning Resources

### Beginner (0-1 hour)

1. Read [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) (5 min)
2. Run `./tools/install-hooks.sh` (1 min)
3. Test `./tools/claude-md-enforcer.sh` (2 min)
4. Make test commit (5 min)

**Total:** 13 minutes to productive

---

### Intermediate (1-4 hours)

1. Read [CONFIGURATION_EXAMPLES.md](./CONFIGURATION_EXAMPLES.md) (15 min)
2. Customize `.claude/settings.json` (30 min)
3. Test different configurations (1 hour)
4. Review Makefile targets (30 min)

**Total:** 2-3 hours to expert

---

### Advanced (4+ hours)

1. Read [CLAUDE_CODE_HOOKS_GUIDE.md](../CLAUDE_CODE_HOOKS_GUIDE.md) (60 min)
2. Study hook architecture (1 hour)
3. Create custom hooks (2 hours)
4. Integrate with CI/CD (2 hours)

**Total:** 5+ hours to mastery

---

## 🔗 External Links

- **Claude Code:** https://claude.com/claude-code
- **erlmcp GitHub:** https://github.com/yourusername/erlmcp
- **TCPS System:** `.claude/TCPS_SYSTEM_COMPLETE.md`
- **Makefile Docs:** `make help`

---

## 🤝 Contributing

Found an issue? Have a suggestion?

1. Check [CLAUDE_CODE_HOOKS_GUIDE.md](../CLAUDE_CODE_HOOKS_GUIDE.md) troubleshooting section
2. Review existing hook scripts in `.claude/hooks/`
3. Test proposed changes with `./tools/claude-md-enforcer.sh`
4. Submit PR with documentation updates

---

## 📊 Quick Stats

| Metric | Value |
|--------|-------|
| Total documentation | 52KB (3 files) |
| Total lines | 2,326 |
| Hook scripts | 5 scripts |
| Makefile targets | 10+ quality gates |
| Quality gates | 6 blocking + 2 reporting |
| Configuration templates | 5 ready-to-use |
| Hook examples | 10 specific cases |
| Troubleshooting guides | 8 common issues |

---

## ✅ Checklist: Am I Ready?

Before starting development, ensure:

- [ ] `.claude/settings.json` exists and configured
- [ ] Git hooks installed (`./tools/install-hooks.sh`)
- [ ] Validation passes (`./tools/claude-md-enforcer.sh`)
- [ ] Read QUICK_REFERENCE.md (5 min)
- [ ] Understand quality gates (6 gates)
- [ ] Know how to bypass in emergency (`git commit --no-verify`)
- [ ] Know where logs are (`/tmp/erlmcp_*.log`)

**All checked?** You're ready to code with confidence! ✅

---

## 🚨 Emergency Contacts

**Hook issues:**
- Check logs: `/tmp/erlmcp_*.log`
- Re-run enforcer: `./tools/claude-md-enforcer.sh`
- Reinstall hooks: `./tools/install-hooks.sh`

**Quality gate failures:**
- Run: `make validate` (see specific failure)
- Check: Individual gates (`make validate-compile`, etc.)
- View: Coverage report (`open _build/test/cover/index.html`)

**Need help?**
- GitHub Issues: https://github.com/yourusername/erlmcp/issues
- Documentation: This directory
- Community: #erlmcp on Slack

---

## 📅 Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.0.0 | 2026-01-28 | Initial comprehensive documentation |

---

**Last Updated:** 2026-01-28
**Maintainer:** erlmcp core team
**Status:** Production Ready ✅

---

**Ready to get started?** Choose your path above and dive in! 🚀
