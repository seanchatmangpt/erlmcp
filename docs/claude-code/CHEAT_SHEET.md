# Claude Code Hook Integration - Cheat Sheet

Quick reference for erlmcp quality enforcement hooks.

## Installation

```bash
# One-time setup
./tools/claude-md-sync.sh
```

## Commands

```bash
# Manual validation
./tools/claude-md-enforcer.sh

# Install/update hooks
./tools/claude-md-sync.sh

# Test validation
./tools/claude-md-enforcer.sh && echo "✅ Pass" || echo "❌ Fail"
```

## Quality Rules

| Rule | Threshold | Blocking |
|------|-----------|----------|
| Compilation | 0 errors | Yes |
| Tests | 100% pass | Yes |
| Coverage | ≥80% | Yes |
| Dialyzer | 0 warnings | No |
| Xref | Clean | No |

## Hooks

### Git Pre-Commit

```bash
# Runs automatically
git commit -m "message"

# Bypass (not recommended)
git commit --no-verify -m "message"
```

### Claude Code Post-Task

Runs automatically after agent completes task. No manual invocation needed.

## Troubleshooting

```bash
# Re-install hooks
./tools/claude-md-sync.sh

# Check hook exists
ls -la .git/hooks/pre-commit

# Test hook manually
./.claude/hooks/pre-commit-validate.sh

# View validation details
./tools/claude-md-enforcer.sh 2>&1 | less
```

## File Locations

```
erlmcp/
├── tools/
│   ├── claude-md-enforcer.sh      Validation engine
│   └── claude-md-sync.sh          Hook installer
├── .claude/hooks/
│   ├── post-task-validate.sh      Post-task hook
│   └── pre-commit-validate.sh     Pre-commit hook
└── docs/claude-code/
    ├── README.md                  Overview
    ├── HOOK_INTEGRATION.md        Full guide
    ├── QUICK_START.md             30-sec setup
    └── CHEAT_SHEET.md             This file
```

## Exit Codes

- `0` - Validation passed
- `1` - Validation failed (violations detected)

## Common Scenarios

### Pass

```bash
$ git commit -m "Fix bug"
[HOOK] ✅ Pre-commit validation PASSED
[main abc1234] Fix bug
```

### Fail

```bash
$ git commit -m "WIP"
[HOOK] ❌ Pre-commit validation FAILED
[ERROR] Compilation errors: 1
[ERROR] Fix violations before proceeding
```

### Bypass (Emergency)

```bash
$ git commit --no-verify -m "Emergency fix"
[main abc1234] Emergency fix

# Then fix properly
$ ./tools/claude-md-enforcer.sh
$ git commit -m "Fix quality violations"
```

## Documentation

- Quick Start: `docs/claude-code/QUICK_START.md`
- Full Guide: `docs/claude-code/HOOK_INTEGRATION.md`
- Implementation: `docs/claude-code/IMPLEMENTATION_SUMMARY.md`

## Support

For issues or questions, see documentation or run:

```bash
./tools/claude-md-enforcer.sh --help
```
