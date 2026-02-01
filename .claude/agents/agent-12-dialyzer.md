---
name: agent-12-dialyzer
description: Dialyzer type checking - validates 0 type warnings
model: sonnet
erlang_otp_context: true
phase: validate
depends_on: agent-02,agent-03,agent-04,agent-05
gate: blocking
---

# Agent: Dialyzer (agent-12)

## Purpose

Runs Dialyzer (DIscrpancy AnaLYZer for ERlang) to detect type discrepancies and potential bugs.

## Quality Gate (BLOCKING)

```bash
✅ rebar3 dialyzer
✅ Warnings: 0
```

## What Dialyzer Checks

- Type mismatches
- Guard tests that will always fail
- Pattern matches that will always fail
- Unused functions
- Functions that will never return
- Invalid calls to BIFs

## Success Criteria

- [ ] PLT built or updated
- [ ] 0 dialyzer warnings
- [ ] All type specs validated
- [ ] Analysis time < 90 seconds

## Commands

```bash
# Build/update PLT (first run takes minutes)
rebar3 dialyzer --build-plt

# Run dialyzer analysis
rebar3 dialyzer 2>&1 | tee .erlmcp/dialyzer.log

# Check for warnings
if grep -q "dialyzer:" .erlmcp/dialyzer.log; then
    echo "❌ Dialyzer warnings found"
    exit 1
fi
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🔍 AGENT 12: DIALYZER                                     ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Warnings: 0
PLT: Up to date
Duration: Xs

Files Analyzed:
  ✓ erlmcp_core (97 modules)
  ✓ erlmcp_transports (23 modules)
  ✓ erlmcp_observability (31 modules)
  ✓ erlmcp_validation (13 modules)
```

## Common Warnings

| Warning | Meaning | Fix |
|---------|---------|-----|
| `The pattern <p> can never match` | Dead code | Remove or fix pattern |
| `The call <func> will never return` | Infinite loop | Fix or document intent |
| `Invalid type specification` | Wrong spec | Correct -spec |
| `Function has no local return` | Missing return | Add return clause |

## Integration

**Depends on**: Compiled apps (02-05)
**Parallel with**: agent-13 (xref), agent-14 (format)
**Blocks**: agent-20 (release)
**PLT location**: `.erlmcp/.plt/dialyzer_plt`
