---
name: agent-14-format
description: Code formatting validation - ensures consistent style
model: haiku
erlang_otp_context: true
phase: validate
depends_on: agent-02,agent-03,agent-04,agent-05
gate: blocking
---

# Agent: Format Validation (agent-14)

## Purpose

Validates that all Erlang source files follow the consistent formatting style using rebar3 format.

## Quality Gate (BLOCKING)

```bash
✅ rebar3 format --verify
✅ Result: All files formatted correctly
```

## Format Rules

- 4-space indentation
- No trailing whitespace
- 100-character line length (soft limit)
- Consistent spacing around operators
- Module header ordering: module, includes, exports, records, types

## Success Criteria

- [ ] All `.erl` files formatted
- [ ] All `.hrl` files formatted
- [ ] No formatting violations
- [ ] Auto-format available

## Commands

```bash
# Verify formatting (fails if not formatted)
rebar3 format --verify 2>&1 | tee .erlmcp/format.log

# Auto-fix formatting
rebar3 format

# Check specific app
rebar3 format --verify --apps erlmcp_core
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🎨 AGENT 14: FORMAT VALIDATION                            ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS | ❌ NEEDS FORMAT
Files Checked: 164
Files Needing Format: 0

Format Issues:
  (none) ✅

To fix:
  make format  # or: rebar3 format
```

## Pre-commit Hook

This agent integrates with the pre-commit hook to automatically check formatting:

```bash
# .git/hooks/pre-commit
rebar3 format --verify || {
    echo "⚠ Files need formatting. Run: rebar3 format"
    exit 1
}
```

## Integration

**Depends on**: Compiled apps (02-05)
**Parallel with**: agent-12 (dialyzer), agent-13 (xref)
**Blocks**: agent-20 (release)
**Auto-fix**: `rebar3 format`
