---
name: agent-08-test-smoke
description: Smoke tests - critical path validation (≤2 min)
model: haiku
erlang_otp_context: true
phase: test
depends_on: agent-02
gate: blocking
---

# Agent: Smoke Tests (agent-08)

## Purpose

Fast smoke tests covering critical paths. Must complete in ≤2 minutes. Used for quick pre-commit validation.

## Quality Gate (BLOCKING)

```bash
✅ ./scripts/test/smoke.sh
✅ Duration: ≤120s
✅ Result: 0 failures
```

## Scope

**Critical paths**:
- JSON-RPC codec (encode/decode)
- Server lifecycle (start/stop)
- Client connection
- Basic transport (stdio)
- Registry registration

## Success Criteria

- [ ] All smoke tests pass
- [ ] Duration ≤ 2 minutes
- [ ] No process leaks

## Commands

```bash
# Run smoke tests
./scripts/test/smoke.sh 2>&1 | tee .erlmcp/smoke.log

# Check duration
DURATION=$(grep "Duration:" .erlmcp/smoke.log | awk '{print $2}')
if [ "$DURATION" -gt 120 ]; then
    echo "⚠ Smoke tests exceeded 120s threshold"
fi
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  💨 AGENT 08: SMOKE TESTS                                  ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Duration: 95s / 120s max

Critical Paths:
  ✓ JSON-RPC Codec
  ✓ Server Lifecycle
  ✓ Client Connection
  ✓ Basic Transport
  ✓ Registry Registration
```

## Test List

1. `erlmcp_json_rpc_tests:encode_decode_test()`
2. `erlmcp_server_tests:start_stop_test()`
3. `erlmcp_client_tests:connect_test()`
4. `erlmcp_transport_stdio_tests:send_receive_test()`
5. `erlmcp_registry_tests:register_test()`

## Integration

**Depends on**: agent-02 (core compiled)
**Parallel with**: agents 06, 07, 09, 10
**Fast path**: Used in `make swarm-quick`
