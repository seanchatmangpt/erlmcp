---
name: agent-09-test-quick
description: Quick tests - smoke + core integration (≤10 min)
model: sonnet
erlang_otp_context: true
phase: test
depends_on: agent-02,agent-03,agent-04
gate: blocking
---

# Agent: Quick Tests (agent-09)

## Purpose

Extended quick tests covering smoke tests plus core integration scenarios. Must complete in ≤10 minutes.

## Quality Gate (BLOCKING)

```bash
✅ ./scripts/test/quick.sh
✅ Duration: ≤600s
✅ Result: 0 failures
```

## Scope

**Includes**:
- All smoke tests (agent-08)
- Core integration tests
- Transport integration
- Session management
- Resource operations

## Success Criteria

- [ ] All quick tests pass
- [ ] Duration ≤ 10 minutes
- [ ] Integration scenarios validated

## Commands

```bash
./scripts/test/quick.sh 2>&1 | tee .erlmcp/quick.log
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  ⚡ AGENT 09: QUICK TESTS                                  ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Duration: 480s / 600s max

Tests:
  ✓ Smoke Tests (agent-08)
  ✓ Core Integration
  ✓ Transport Integration
  ✓ Session Management
  ✓ Resource Operations
```

## Integration

**Depends on**: agents 02, 03, 04
**Parallel with**: agents 06, 07, 08, 10
**Used in**: `make quick`
