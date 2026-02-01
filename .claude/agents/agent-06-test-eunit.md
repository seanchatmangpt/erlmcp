---
name: agent-06-test-eunit
description: EUnit test execution - unit tests for all apps
model: sonnet
erlang_otp_context: true
phase: test
depends_on: agent-02,agent-03,agent-04,agent-05
gate: blocking
---

# Agent: EUnit Tests (agent-06)

## Purpose

Runs all EUnit unit tests across all applications. Chicago School TDD - real processes, no mocks.

## Quality Gate (BLOCKING)

```bash
✅ rebar3 eunit
✅ Result: 0 failures, 0 errors
✅ Coverage: ≥80%
```

## Scope

**All apps**:
- `apps/erlmcp_core/test`
- `apps/erlmcp_transports/test`
- `apps/erlmcp_observability/test`
- `apps/erlmcp_validation/test`

## Success Criteria

- [ ] All EUnit tests pass (0 failures)
- [ ] Coverage ≥80% overall
- [ ] Core modules ≥85% coverage
- [ ] Test execution < 60 seconds

## Commands

```bash
# Run all EUnit tests with coverage
rebar3 eunit --cover 2>&1 | tee .erlmcp/eunit.log

# Generate coverage report
rebar3 cover --verbose > .erlmcp/coverage-report.txt

# Check coverage threshold
COVERAGE=$(grep "%*" .erlmcp/coverage-report.txt | head -1 | sed 's/.*//' | sed 's/%//')
if [ "$COVERAGE" -lt 80 ]; then
    echo "❌ Coverage ${COVERAGE}% < 80%"
    exit 1
fi
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🧪 AGENT 06: EUNIT TESTS                                  ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Tests: 84 passed, 0 failed
Duration: Xs

Coverage:
  Overall: 82% ✅
  Core: 87% ✅
  Transports: 79% ✅
  Observability: 81% ✅
  Validation: 85% ✅

Chicago School TDD: ✅ (Real processes, no mocks)
```

## Test Pattern

```erlang
%% Example EUnit test (Chicago School)
start_stop_test() ->
    {ok, Pid} = my_server:start_link(),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).
```

## Integration

**Depends on**: agents 02, 03, 04, 05 (all compiled)
**Parallel with**: agent-07 (CT)
**Blocks**: agent-11 (coverage analysis)

## Error Recovery

- Run single module: `rebar3 eunit --module=<module>`
- Verbose output: `rebar3 eunit -v`
- Check for flaky tests: run 10 times
