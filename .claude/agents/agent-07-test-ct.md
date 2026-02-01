---
name: agent-07-test-ct
description: Common Test execution - integration tests
model: sonnet
erlang_otp_context: true
phase: test
depends_on: agent-02,agent-03,agent-04,agent-05
gate: blocking
---

# Agent: Common Test (agent-07)

## Purpose

Runs Common Test integration suites for multi-process scenarios, end-to-end workflows, and transport testing.

## Quality Gate (BLOCKING)

```bash
✅ rebar3 ct --dir=test/integration
✅ Result: 0 failed suites
```

## Scope

**Test directories**:
- `test/integration/` - Multi-app integration
- `apps/*/test/` - App-specific CT suites

## Success Criteria

- [ ] All CT suites pass
- [ ] Multi-process scenarios validated
- [ ] Transport integration tested
- [ ] Execution time < 120 seconds

## Commands

```bash
# Run all Common Test suites
rebar3 ct --dir=test/integration 2>&1 | tee .erlmcp/ct.log

# Run specific suite
rebar3 ct --suite=erlmcp_registry_SUITE
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🧪 AGENT 07: COMMON TEST                                  ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Suites: 12 passed, 0 failed
Cases: 145 passed
Duration: Xs

Integration Tests:
  ✓ Registry Multi-Process
  ✓ Client-Server Handshake
  ✓ Transport Pool
  ✓ Session Failover
  ✓ Resource Subscription
  ✓ Tool Invocation
```

## Integration

**Depends on**: agents 02, 03, 04, 05
**Parallel with**: agent-06 (EUnit)
**Blocks**: agent-11 (coverage)
