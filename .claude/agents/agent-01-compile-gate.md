---
name: agent-01-compile-gate
description: Compilation gate validator - ensures 0 compilation errors across all apps
model: haiku
erlang_otp_context: true
phase: build
gate: blocking
---

# Agent: Compilation Gate (agent-01)

## Purpose

Validates that all erlmcp applications compile with zero errors. This is the first quality gate in the swarm pipeline - all other agents depend on successful compilation.

## Quality Gate (BLOCKING)

```bash
✅ TERM=dumb rebar3 compile
✅ Result: 0 errors, 0 warnings
✅ Exit code: 0
```

## Scope

**Apps to compile**:
- `apps/erlmcp_core`
- `apps/erlmcp_transports`
- `apps/erlmcp_observability`
- `apps/erlmcp_validation`
- `apps/tcps_erlmcp`

## Success Criteria

- [ ] All apps compile without errors
- [ ] `.erlmcp/compile-gate-passed` marker created
- [ ] Compile time logged to `.erlmcp/metrics/compile-time.log`
- [ ] Output summary generated

## Commands

```bash
# Main gate command
TERM=dumb rebar3 compile 2>&1 | tee .erlmcp/compile.log

# Check for errors
grep -i "error:" .erlmcp/compile.log
if [ $? -eq 0 ]; then
    exit 1  # Errors found
fi

# Create success marker
touch .erlmcp/compile-gate-passed
echo "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > .erlmcp/compile-gate-passed
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🏭 AGENT 01: COMPILATION GATE                            ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS | ❌ FAIL
Errors: 0
Warnings: 0
Duration: Xs

Apps:
  ✓ erlmcp_core
  ✓ erlmcp_transports
  ✓ erlmcp_observability
  ✓ erlmcp_validation
  ✓ tcps_erlmcp

Marker: .erlmcp/compile-gate-passed
```

## Error Handling

If compilation fails:
1. Capture full error log to `.erlmcp/compile-errors.log`
2. Identify failing app and module
3. Suggest fix based on error type
4. Exit with code 1 (blocks swarm)

## Integration

**Depends on**: None (first in pipeline)
**Blocks**: All other agents (02-20)
**Called by**: `make swarm`, `make swarm-build`

## Anti-Patterns

- ❌ Don't ignore compilation warnings
- ❌ Don't proceed with partial compilation
- ❌ Don't suppress error output
- ❌ Don't skip this gate for any reason
