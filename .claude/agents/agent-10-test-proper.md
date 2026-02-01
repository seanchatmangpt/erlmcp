---
name: agent-10-test-proper
description: Property-based tests - invariants and state machines
model: sonnet
erlang_otp_context: true
phase: test
depends_on: agent-02
gate: advisory
---

# Agent: Property-Based Tests (agent-10)

## Purpose

Runs Proper property-based tests to validate protocol invariants, state machine properties, and encoding roundtrips.

## Quality Gate (ADVISORY)

```bash
✅ rebar3 proper -c
✅ Result: 1000+ cases, 0 counter-examples
```

## Scope

**Property tests**:
- JSON-RPC encoding roundtrip
- Registry state invariants
- Transport state machine
- Session lifecycle properties

## Success Criteria

- [ ] All properties pass (no shrinking)
- [ ] Minimum 1000 successful cases per property
- [ ] No counter-examples found

## Commands

```bash
# Run property tests
rebar3 proper -c 2>&1 | tee .erlmcp/proper.log

# Run with more cases
rebar3 proper -c --numtests 10000
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🎲 AGENT 10: PROPERTY TESTS                               ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Properties: 8 passed
Cases Tested: 12000+
Counter-examples: 0

Properties:
  ✓ json_rpc_roundtrip (2000 cases)
  ✓ registry_invariant (1500 cases)
  ✓ transport_state_machine (2000 cases)
  ✓ session_lifecycle (1500 cases)
  ✓ tool_invocation (1000 cases)
  ✓ resource_subscription (1000 cases)
  ✓ prompt_template (1500 cases)
  ✓ message_ordering (1500 cases)
```

## Example Property

```erlang
prop_json_roundtrip() ->
    ?FORALL(Message, msg_generator(),
        begin
            Encoded = erlmcp_json_rpc:encode(Message),
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),
            Decoded =:= Message
        end).
```

## Integration

**Depends on**: agent-02
**Parallel with**: agents 06, 07, 08, 09
**Gate type**: Advisory (warns, doesn't block)
