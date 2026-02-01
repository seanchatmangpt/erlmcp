---
name: agent-13-xref
description: Xref analysis - validates 0 undefined function calls
model: haiku
erlang_otp_context: true
phase: validate
depends_on: agent-02,agent-03,agent-04,agent-05
gate: advisory
---

# Agent: Xref (agent-13)

## Purpose

Runs Xref (Cross Reference) analysis to detect undefined function calls and unused functions.

## Quality Gate (ADVISORY)

```bash
✅ rebar3 xref
✅ Undefined calls: 0
```

## What Xref Checks

- Calls to undefined functions
- Calls to functions in modules that don't exist
- Unused functions (no local calls)
- Deprecated function usage

## Success Criteria

- [ ] 0 undefined function calls
- [ ] Deprecated functions documented
- [ ] Analysis completes

## Commands

```bash
# Run xref analysis
rebar3 xref 2>&1 | tee .erlmcp/xref.log

# Check for undefined calls
if grep -q "undefined function" .erlmcp/xref.log; then
    echo "⚠ Undefined function calls detected"
    grep "undefined function" .erlmcp/xref.log
fi
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🔗 AGENT 13: XREF                                         ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Undefined Calls: 0
Unused Functions: 3 (documented)

Analysis:
  ✓ No calls to undefined functions
  ✓ All module dependencies resolved
  ✓ Deprecated calls documented:
    - lists:map2/3 (use lists:map/2)
    - dict:module() (use maps:)
```

## Integration

**Depends on**: Compiled apps (02-05)
**Parallel with**: agent-12 (dialyzer), agent-14 (format)
**Gate type**: Advisory (warns, doesn't block)
