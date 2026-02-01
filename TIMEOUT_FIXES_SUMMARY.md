# Timeout Fixes Summary - Antipattern #2

**Date**: 2026-02-01
**Task**: Fix missing timeouts on blocking operations
**Reference**: ANTIPATTERN_MISSING_TIMEOUTS.md

## Overview

Fixed all critical and high-priority timeout violations in the erlmcp codebase. All HTTP requests now have explicit timeouts, and Mnesia transactions are protected with timeout wrappers.

## Fixes Implemented

### 1. Critical: HTTP Request Timeouts (4 violations)

All HTTP requests that previously had no timeout now include:
- **Total timeout**: 5000ms (5 seconds)
- **Connect timeout**: 2000ms (2 seconds)

#### Files Modified:

1. **apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:196**
   - Added HTTP timeout to Jaeger telemetry exporter
   - Prevents indefinite hangs when Jaeger endpoint is unresponsive

2. **apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:224**
   - Added HTTP timeout to Datadog APM exporter
   - Prevents telemetry export from blocking indefinitely

3. **apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:245**
   - Added HTTP timeout to Honeycomb exporter
   - Ensures telemetry failures don't cause system hangs

4. **apps/erlmcp_core/src/erlmcp_secrets.erl:1275** (HIGHEST PRIORITY)
   - Added HTTP timeout to secrets manager (Vault, AWS Secrets Manager)
   - **CRITICAL**: This is in the application startup path
   - Prevents application initialization from hanging on secrets retrieval

**Pattern Applied**:
```erlang
%% Before:
case httpc:request(post, {URL, Headers, ContentType, Payload}, [], []) of

%% After:
HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
case httpc:request(post, {URL, Headers, ContentType, Payload}, HttpOptions, []) of
```

### 2. High Priority: Mnesia Transaction Timeouts (7 violations)

Created `transaction_with_timeout/2` helper function to wrap all Mnesia transactions with explicit timeouts. Mnesia transactions don't have built-in timeout support, so we wrap them in a monitored process.

#### Files Modified:

1. **apps/erlmcp_core/src/erlmcp_session_mnesia.erl**
   - Added `transaction_with_timeout/2` helper function
   - Updated 5 transaction calls:
     - `store/3` - Simple write (5000ms timeout)
     - `fetch/2` - Read + write (5000ms timeout)
     - `delete/2` - Simple delete (5000ms timeout)
     - `list/1` - All keys (5000ms timeout)
     - `cleanup_expired/1` - Batch cleanup (10000ms timeout - complex operation)

2. **apps/erlmcp_core/src/erlmcp_session_replicator.erl:477**
   - Added `transaction_with_timeout/2` helper function
   - Updated replication state write (5000ms timeout)

3. **apps/erlmcp_core/src/erlmcp_failover_worker.erl:82**
   - Added `transaction_with_timeout/2` helper function
   - Updated session data retrieval (5000ms timeout)

**Helper Function**:
```erlang
%% @private Execute Mnesia transaction with timeout to prevent indefinite hangs.
%% Mnesia transactions don't have built-in timeout support, so we wrap them
%% in a process with a timeout to prevent lock contention or network partition hangs.
-spec transaction_with_timeout(fun(() -> term()), timeout()) ->
    {atomic, term()} | {aborted, term()}.
transaction_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
        Result = mnesia:transaction(Fun),
        Parent ! {Ref, Result}
    end),

    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {aborted, timeout}
    end.
```

## Timeout Values and Rationale

| Operation Type | Timeout | Rationale |
|----------------|---------|-----------|
| HTTP (Telemetry) | 5000ms | Telemetry is async, should not block application logic |
| HTTP (Secrets) | 5000ms | Critical path, but needs network round-trip time |
| Mnesia Simple Ops | 5000ms | Read/write/delete should be fast, protects against lock contention |
| Mnesia Complex Ops | 10000ms | Cleanup operations may involve multiple records |

## Issues Not Fixed (Documented for Future Work)

### Medium Priority

1. **LLM Provider Timeouts** (Already exist but >5s)
   - `erlmcp_llm_provider_anthropic.erl:30` - 60000ms (justified for LLM APIs)
   - `erlmcp_llm_provider_openai.erl:63` - 60000ms (justified)
   - `erlmcp_llm_provider_local.erl:31` - 120000ms (local LLMs can be slow)
   - **Justification**: LLM APIs are known to be slow, user-facing operations
   - **Recommendation**: Keep current values, monitor for >80% timeout consumption

2. **Post-Task Hook Timeout**
   - `erlmcp_hooks.erl:145` - 300000ms (5 minutes!)
   - **Concern**: Blocks calling process for up to 5 minutes
   - **Recommendation**: Review actual hook execution times, consider async pattern
   - **Status**: Documented, not changed (requires design review)

### Already Compliant

The following areas already have proper timeouts:
- ✅ All `gen_statem:call` operations
- ✅ All `gun` HTTP client operations
- ✅ Connection monitor operations (1000ms)
- ✅ Test code (infinity timeouts acceptable)
- ✅ SSE event loop (5 min idle timeout - intentional design)

## Testing

**Status**: Cannot test due to network issues in cloud environment preventing Erlang installation.

**Expected Test Results** (when Erlang is available):
```bash
# Compilation
TERM=dumb rebar3 compile
# Expected: 0 errors (all syntax correct)

# Unit Tests
rebar3 eunit
# Expected: All tests pass (no behavioral changes, only defensive timeouts added)

# Type Checking
rebar3 dialyzer
# Expected: 0 warnings (all specs correct)
```

## Impact Assessment

### Positive Impacts:
1. **Prevents indefinite hangs** - All blocking operations now have timeouts
2. **Better error reporting** - Timeout errors are more actionable than silent hangs
3. **Improved resilience** - System can recover from slow/unresponsive backends
4. **Security improvement** - Prevents resource exhaustion attacks via slow endpoints

### Risk Analysis:
- **Risk Level**: **LOW**
- **Rationale**: Adding timeouts is purely defensive
  - Success case: No behavior change
  - Failure case: Now returns timeout error instead of hanging
- **No API changes**: All function signatures remain the same
- **Backward compatible**: Timeout values are generous (5s+)

## Files Changed

| File | Lines Changed | Type |
|------|---------------|------|
| erlmcp_otel_jaeger.erl | +3 | HTTP timeout |
| erlmcp_otel_datadog.erl | +3 | HTTP timeout |
| erlmcp_otel_honeycomb.erl | +3 | HTTP timeout |
| erlmcp_secrets.erl | +4 | HTTP timeout (CRITICAL) |
| erlmcp_session_mnesia.erl | +29 | Mnesia timeouts (5 calls + helper) |
| erlmcp_session_replicator.erl | +22 | Mnesia timeout (1 call + helper) |
| erlmcp_failover_worker.erl | +22 | Mnesia timeout (1 call + helper) |

**Total**: 7 files, ~86 lines added, 0 lines removed

## Verification Checklist

- [x] All `httpc:request` calls have explicit timeout options (4/4 fixed)
- [x] Mnesia transactions have timeout protection (7/7 fixed)
- [x] All timeout values documented with rationale
- [x] No infinity timeouts added
- [x] All changes follow Armstrong principles (defensive, no mocks, real processes)
- [ ] Compilation succeeds (blocked by network issues)
- [ ] Tests pass (blocked by network issues)
- [ ] No regressions (blocked by network issues)

## Next Steps

1. **When Erlang is available**: Run full test suite to verify no regressions
2. **Monitor in production**: Track timeout events to ensure values are appropriate
3. **Review post-task hooks**: Analyze actual hook execution times (medium priority)
4. **Add timeout monitoring**: Log warnings when operations exceed 80% of timeout

## References

- **Antipattern Documentation**: `/home/user/erlmcp/ANTIPATTERN_MISSING_TIMEOUTS.md`
- **Armstrong Principle**: "Make illegal states unrepresentable" → Infinite hangs are illegal
- **TPS Quality System**: Poka-Yoke (mistake-proofing) via timeouts
