# Antipattern Analysis: Missing or Unbounded Timeouts

**Analysis Date**: 2026-02-01
**Antipattern**: #3 - Missing or unbounded timeouts
**Rule**: All blocking operations must have timeouts <5000ms
**Scope**: apps/erlmcp_*/src/*.erl

## Executive Summary

**Critical Violations**: 4 (HTTP requests without timeout)
**High Severity**: 6 (Operations with timeouts >5s for legitimate reasons)
**Medium Severity**: 7 (Mnesia transactions without explicit timeout)
**Low Severity**: 1 (SSE receive with 5-minute timeout - intentional design)
**Test Code**: 10 (infinity timeouts in test helpers - acceptable)

**Overall Assessment**: The codebase demonstrates **good timeout discipline** with most operations properly bounded. The critical violations are concentrated in observability/telemetry code where network failures would be gracefully handled but could cause delays.

---

## 1. Critical Violations: HTTP Requests Without Timeout

These operations can hang indefinitely if the remote endpoint is unresponsive.

### 1.1 OpenTelemetry Jaeger Exporter

**Location**: `apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:196`

```erlang
case httpc:request(post, {binary_to_list(Endpoint), AllHeaders, ContentType, Payload}, [], []) of
```

**Issue**: HTTP POST to Jaeger collector has no timeout (defaults to infinity)

**Impact**:
- If Jaeger endpoint is down or slow, the export process hangs indefinitely
- Blocks the gen_server handling telemetry export
- Could cause telemetry buffer to fill up
- Does not block application logic (telemetry is async), but prevents new telemetry from being sent

**Recommended Fix**:
```erlang
HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
case httpc:request(post, {binary_to_list(Endpoint), AllHeaders, ContentType, Payload}, HttpOptions, []) of
```

**Rationale**: 5s total timeout, 2s connect timeout gives adequate time for network round-trip while preventing indefinite hangs

---

### 1.2 OpenTelemetry Datadog Exporter

**Location**: `apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:224`

```erlang
case httpc:request(post, {binary_to_list(Endpoint), AllHeaders, "application/json", Payload}, [], []) of
```

**Issue**: HTTP POST to Datadog APM has no timeout

**Impact**: Same as Jaeger - telemetry export can hang indefinitely

**Recommended Fix**:
```erlang
HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
case httpc:request(post, {binary_to_list(Endpoint), AllHeaders, "application/json", Payload}, HttpOptions, []) of
```

---

### 1.3 OpenTelemetry Honeycomb Exporter

**Location**: `apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:245`

```erlang
case httpc:request(post, {URL, Headers, "application/json", Payload}, [], []) of
```

**Issue**: HTTP POST to Honeycomb has no timeout

**Impact**: Telemetry export can hang indefinitely

**Recommended Fix**:
```erlang
HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
case httpc:request(post, {URL, Headers, "application/json", Payload}, HttpOptions, []) of
```

---

### 1.4 Secrets Manager HTTP Requests

**Location**: `apps/erlmcp_core/src/erlmcp_secrets.erl:1275`

```erlang
case httpc:request(Method, Request, SslOpts ++ Options, []) of
```

**Issue**: HTTP requests to secrets backends (Vault, AWS Secrets Manager) have no timeout in the HTTPOptions (4th argument)

**Impact**:
- **CRITICAL** - This is in the core secrets retrieval path
- If secrets backend is slow/down, application startup or runtime secret fetches hang
- Can block gen_server:call operations that need secrets
- Much more severe than telemetry issues

**Current Mitigation**: The function is called from gen_server:call operations that have their own 10000ms timeout (line 69-99), so there's an outer timeout boundary.

**Recommended Fix**:
```erlang
HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}],
case httpc:request(Method, Request, SslOpts ++ Options, HttpOptions) of
```

**Priority**: **HIGH** - While there's an outer timeout, the lack of explicit HTTP timeout means error reporting is less precise and the full 10s could be consumed by a hung TCP connection.

---

## 2. High Severity: Operations Exceeding 5s Timeout

These operations have timeouts >5000ms, which violates the guideline. However, most are **justified** by their use case.

### 2.1 Code Reload Operations

**Location**: `apps/erlmcp_core/src/erlmcp_code_reload.erl:69,74`

```erlang
gen_server:call(?MODULE, {reload_module, Module, Opts}, 60000).
gen_server:call(?MODULE, {reload_modules, Modules, Opts}, 60000).
```

**Timeout**: 60000ms (1 minute)

**Justification**: **ACCEPTED**
- Hot code reloading can involve compilation, module loading, and state migration
- Multiple modules may need sequential reloading
- 60s is reasonable for production code reload operations

**Recommendation**: Keep current timeout, add monitoring to alert if reload takes >30s

---

### 2.2 LLM Provider API Calls

**Locations**:
- `apps/erlmcp_core/src/erlmcp_llm_provider_anthropic.erl:30` - 60000ms
- `apps/erlmcp_core/src/erlmcp_llm_provider_openai.erl:63` - 60000ms
- `apps/erlmcp_core/src/erlmcp_llm_provider_local.erl:31` - 120000ms (2 minutes!)
- `apps/erlmcp_core/src/erlmcp_sampling.erl:74` - 30000ms

**Timeouts**: 30-120 seconds

**Justification**: **PARTIALLY ACCEPTED**
- LLM APIs can be slow (especially for long prompts or large responses)
- Anthropic/OpenAI typically respond in 5-30s for normal requests
- Local LLM providers (Ollama, etc.) can be much slower

**Concerns**:
- 120s timeout for local provider is excessive - could hide performance issues
- No distinction between connection timeout and response timeout

**Recommendation**:
```erlang
% For cloud providers (Anthropic/OpenAI)
gen_server:call(?MODULE, {create_message, Messages, Params}, 30000).

% For local providers
gen_server:call(?MODULE, {create_message, Messages, Params}, 60000).

% Within the implementations, use stricter HTTP timeouts:
gun:await(ConnPid, StreamRef, 25000)  % Leave 5s buffer for processing
```

**Impact if exceeded**: User-facing request times out, but this is expected behavior for slow LLM APIs

---

### 2.3 Hook Post-Task Execution

**Location**: `apps/erlmcp_core/src/erlmcp_hooks.erl:145`

```erlang
gen_server:call(?SERVER, {post_task, TaskId, Context}, 300000).
```

**Timeout**: 300000ms (5 minutes!)

**Justification**: **QUESTIONABLE**
- Post-task hooks may include deployment, artifact upload, or validation
- 5 minutes allows for complex post-processing

**Concerns**:
- If a hook takes 5 minutes, the calling process is blocked for 5 minutes
- No way to cancel or monitor progress
- Could indicate the hook should be async

**Recommendation**:
```erlang
% Option 1: Reduce timeout and make hooks more granular
gen_server:call(?SERVER, {post_task, TaskId, Context}, 30000).

% Option 2: Make long-running hooks async with progress reporting
gen_server:cast(?SERVER, {post_task_async, TaskId, Context, ReplyTo}).
```

**Priority**: **MEDIUM** - Review actual post-task hook implementations to determine if 5min is ever needed

---

### 2.4 Plugin Manager Operations

**Location**: `apps/erlmcp_core/src/erlmcp_plugin_manager.erl:71,81,86,91,101,106,111`

```erlang
gen_server:call(?SERVER, discover_and_load_plugins, 10000).
gen_server:call(?SERVER, {load_plugin, Module, Opts}, 10000).
% ... etc
```

**Timeout**: 10000ms (10 seconds)

**Justification**: **ACCEPTED**
- Plugin loading involves file I/O, code compilation, and initialization
- Discovery may scan directories
- 10s is reasonable for plugin operations (not in hot path)

**Recommendation**: Keep current timeout, but log warning if operation takes >5s

---

### 2.5 Session Failover Operations

**Locations**:
- `apps/erlmcp_core/src/erlmcp_session_failover.erl:96` - 10000ms
- `apps/erlmcp_core/src/erlmcp_session_failover.erl:116` - 15000ms

```erlang
gen_server:call(?MODULE, {failover, SessionId}, 10000).
gen_server:call(?MODULE, {force_sync, SessionId}, 15000).
```

**Timeouts**: 10-15 seconds

**Justification**: **ACCEPTED**
- Failover involves coordinating with distributed nodes
- Session state may need to be transferred over network
- These are rare, exceptional operations (not in hot path)

**Recommendation**: Keep current timeout

---

## 3. Medium Severity: Mnesia Transactions Without Explicit Timeout

Mnesia transactions use a default timeout but don't make it explicit in the code.

### 3.1 Session Mnesia Backend

**Location**: `apps/erlmcp_core/src/erlmcp_session_mnesia.erl`

**Lines**: 122, 145, 156, 168, 201

```erlang
case mnesia:transaction(Transaction) of
```

**Issue**: No explicit timeout (uses Mnesia default: infinity for transactions)

**Impact**:
- Transactions can hang if there's lock contention
- Distributed transactions can block on network partitions
- No visibility into how long transactions should take

**Recommended Fix**:
```erlang
% For simple read/write operations
case mnesia:sync_transaction(Transaction, 5000) of

% For operations that may involve multiple records
case mnesia:sync_transaction(Transaction, 10000) of
```

**Note**: Mnesia's `transaction/1` doesn't support timeout directly. Options:
1. Use `mnesia:transaction/3` with retry count
2. Wrap in a timeout at the calling layer
3. Monitor transaction execution time

**Alternative Fix**:
```erlang
case with_timeout(5000, fun() -> mnesia:transaction(Transaction) end) of

with_timeout(Timeout, Fun) ->
    Pid = self(),
    Ref = make_ref(),
    TimerRef = erlang:send_after(Timeout, self(), {timeout, Ref}),
    try
        Fun()
    catch
        _:_ ->
            erlang:cancel_timer(TimerRef),
            error(transaction_failed)
    after
        erlang:cancel_timer(TimerRef),
        receive {timeout, Ref} -> error(timeout) after 0 -> ok end
    end.
```

**Priority**: **MEDIUM** - Mnesia's internal lock manager provides some protection, but explicit timeouts improve reliability

---

### 3.2 Other Mnesia Locations

**Locations**:
- `apps/erlmcp_core/src/erlmcp_failover_worker.erl:82`
- `apps/erlmcp_core/src/erlmcp_session_replicator.erl:469`
- `apps/erlmcp_core/test/erlmcp_session_failover_tests.erl:79,86` (test code)

**Recommendation**: Same as 3.1

---

## 4. Low Severity: Intentional Long Timeouts

### 4.1 SSE Event Loop

**Location**: `apps/erlmcp_transport_sse.erl:535`

```erlang
receive
    ping -> ...
    {send_event, EventType, Data} -> ...
    close -> ...
after 300000 ->  % 5 minute idle timeout
    cowboy_req:stream_body(format_retry_field(get_retry_timeout()), fin, Req),
    {ok, Req, State}
end.
```

**Timeout**: 300000ms (5 minutes)

**Justification**: **ACCEPTED**
- SSE connections are long-lived by design
- 5-minute idle timeout is standard for SSE (prevents zombie connections)
- Matches WebSocket idle timeout (also 5 minutes)
- Has explicit keepalive pings every 30s to detect dead connections earlier

**Recommendation**: Keep current design. This is not a blocking operation in the traditional sense - it's an event loop that should stay alive.

---

## 5. Test Code: Acceptable Infinity Timeouts

The following test files use `receive after infinity -> ok end` for helper processes that should live for the duration of the test:

1. `apps/erlmcp_core/test/erlmcp_subscription_tests.erl:68,256,322`
2. `apps/erlmcp_core/test/erlmcp_resource_subscriptions_tests.erl:68,208`
3. `apps/erlmcp_observability/test/erlmcp_debugger_dot_tests.erl:48,79,170,195,219`

**Justification**: **ACCEPTED**
- These are mock subscriber processes in tests
- Tests themselves have timeouts (controlled by test framework)
- Process lifetime is managed by test cleanup

**Recommendation**: No changes needed

---

## 6. Good Patterns Found

The codebase demonstrates many **good timeout practices**:

### 6.1 All gen_statem:call Operations Have Timeouts

**Examples**:
- `erlmcp_server_fsm.erl:80` - `gen_statem:call(Pid, drain, ?DEFAULT_DRAIN_TIMEOUT_MS + 1000)`
- `erlmcp_client_fsm.erl:79` - `gen_statem:call(Pid, connect, ?CONNECT_TIMEOUT_MS)`
- `erlmcp_session_fsm.erl:87` - `gen_statem:call(Pid, {negotiate, Params}, ?NEGOTIATION_TIMEOUT_MS)`

**Pattern**: FSM calls use named constants for timeouts, making them easy to adjust

---

### 6.2 Gun HTTP Client Operations Have Timeouts

**Examples**:
- `erlmcp_llm_provider_openai.erl:225,229,231` - All gun:await* calls include timeout parameter
- `erlmcp_secrets.erl:620,630,637` - Gun operations for Vault API have explicit timeouts
- `erlmcp_transport_health.erl:451,455` - Health checks have `?HEALTH_CHECK_TIMEOUT`

**Pattern**: Gun client is consistently used with timeouts

---

### 6.3 Connection Monitor Uses Short Timeouts

**Location**: `apps/erlmcp_core/src/erlmcp_connection_monitor.erl:138,144,157,162,167,172`

```erlang
gen_statem:call(?MODULE, {monitor_connection, ConnectionPid, Info}, 1000).
gen_statem:call(?MODULE, {unmonitor_connection, ConnectionId}, 1000).
% ... etc
```

**Timeout**: 1000ms (1 second)

**Pattern**: **EXCELLENT** - Fast operations that should never take long have aggressive timeouts

---

### 6.4 erlang:send_after for Periodic Tasks (Not Blocking)

The codebase extensively uses `erlang:send_after/3` for scheduled messages, which is **non-blocking** and therefore not subject to timeout requirements:

- Health checks (30s intervals)
- Cleanup tasks (60s intervals)
- Keepalive pings (30s intervals)
- Memory monitoring (configurable intervals)

**Pattern**: Proper use of async timers

---

## 7. Timeout Recommendations by Operation Type

| Operation Type | Recommended Timeout | Rationale |
|----------------|---------------------|-----------|
| Registry lookup | 500-1000ms | Local operation, should be near-instant |
| Session state read | 1000-2000ms | ETS/DETS read, minimal I/O |
| Session state write | 2000-5000ms | May involve DETS/Mnesia sync |
| HTTP request (internal) | 2000-5000ms | Local network, should be fast |
| HTTP request (external) | 5000-10000ms | Internet latency, retries |
| LLM API call | 30000-60000ms | Known to be slow, user-facing |
| Code reload | 30000-60000ms | Rare operation, safety critical |
| Plugin load | 5000-10000ms | File I/O, code compilation |
| Distributed operation | 5000-15000ms | Network latency, node coordination |
| Database transaction | 2000-10000ms | Depends on contention, complexity |
| Mnesia transaction | 5000ms | Lock contention possible |
| Telemetry export | 5000ms | Fire-and-forget, should not block |

---

## 8. Affected Supervision Areas

### 8.1 Observability Supervision Tree

**Location**: `apps/erlmcp_observability/src/erlmcp_observability_sup.erl`

**Affected Processes**:
- `erlmcp_otel_jaeger` - HTTP timeout issue
- `erlmcp_otel_datadog` - HTTP timeout issue
- `erlmcp_otel_honeycomb` - HTTP timeout issue

**Impact**: If telemetry exporters hang, they block within the gen_server but do not crash. The supervisor won't detect the issue unless the process eventually times out and crashes.

**Recommendation**:
1. Add HTTP timeouts to prevent hangs
2. Add health checks to detect stuck exporters
3. Consider making exports fully async (cast instead of call)

---

### 8.2 Core Supervision Tree

**Location**: `apps/erlmcp_core/src/erlmcp_sup.erl`

**Affected Processes**:
- `erlmcp_secrets` - HTTP timeout issue in secrets retrieval
- `erlmcp_session_mnesia` - Mnesia transaction timeout issue
- `erlmcp_session_failover` - Long timeouts (accepted)

**Impact**: Secrets retrieval is more critical as it can block application functionality. Mnesia transaction hangs could cause cascading failures if multiple sessions are affected.

**Recommendation**:
1. Add HTTP timeouts to secrets manager (HIGH PRIORITY)
2. Add transaction-level timeouts for Mnesia operations
3. Monitor session failover duration

---

## 9. Summary of Recommendations

### Immediate (Critical)

1. **Add HTTP timeouts to all httpc:request calls**:
   - `erlmcp_otel_jaeger.erl:196`
   - `erlmcp_otel_datadog.erl:224`
   - `erlmcp_otel_honeycomb.erl:245`
   - `erlmcp_secrets.erl:1275` (HIGHEST PRIORITY)

   ```erlang
   HttpOptions = [{timeout, 5000}, {connect_timeout, 2000}]
   ```

### High Priority

2. **Add timeout wrapper for Mnesia transactions**:
   - Implement `with_timeout/2` helper
   - Apply to all `mnesia:transaction/1` calls
   - Target timeout: 5000-10000ms depending on operation

3. **Review and reduce LLM provider timeouts**:
   - Cloud providers: 30000ms (down from 60000ms)
   - Local providers: 60000ms (down from 120000ms)
   - Add connection vs. response timeout distinction

### Medium Priority

4. **Review post-task hook timeout**:
   - Analyze actual hook execution times
   - Consider making long-running hooks async
   - Reduce timeout to 30000ms or implement async pattern

5. **Add monitoring for timeout boundary conditions**:
   - Alert when operations take >80% of timeout
   - Track timeout expiration rates
   - Identify operations that consistently approach timeout

### Low Priority

6. **Document timeout rationale**:
   - Add comments explaining why timeouts exceed 5s
   - Create timeout policy document
   - Include timeout values in API documentation

---

## 10. Verification Checklist

After implementing fixes:

- [ ] All `httpc:request` calls have explicit timeout options
- [ ] No operations have unbounded (infinity) timeouts except:
  - [ ] Test helper processes
  - [ ] Event loops with keepalive mechanisms (SSE, WebSocket)
- [ ] All timeouts >5000ms have documented justification
- [ ] Mnesia transactions have timeout protection
- [ ] Gun operations have explicit timeouts (already compliant)
- [ ] gen_server:call operations have timeouts (already compliant)
- [ ] gen_statem:call operations have timeouts (already compliant)
- [ ] Added monitoring for operations approaching timeout
- [ ] Updated supervision strategy to detect hung processes

---

## 11. Code Examples for Fixes

### Example 1: HTTP Request with Timeout

**Before**:
```erlang
case httpc:request(post, {URL, Headers, ContentType, Payload}, [], []) of
```

**After**:
```erlang
HttpOptions = [
    {timeout, 5000},           % Total request timeout
    {connect_timeout, 2000}    % TCP connection timeout
],
case httpc:request(post, {URL, Headers, ContentType, Payload}, HttpOptions, []) of
```

---

### Example 2: Mnesia Transaction with Timeout

**Before**:
```erlang
case mnesia:transaction(Transaction) of
```

**After**:
```erlang
%% Helper function in erlmcp_mnesia_utils.erl
-spec transaction_with_timeout(fun(), timeout()) -> {atomic, term()} | {aborted, term()}.
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

%% Usage
case transaction_with_timeout(Transaction, 5000) of
```

---

### Example 3: Logging Timeout Warnings

**Add to operations approaching timeout**:
```erlang
Start = erlang:monotonic_time(millisecond),
Result = gen_server:call(Server, Request, Timeout),
Duration = erlang:monotonic_time(millisecond) - Start,

%% Log if >80% of timeout consumed
case Duration > (Timeout * 0.8) of
    true ->
        logger:warning("Operation approaching timeout: ~pms / ~pms", [Duration, Timeout]);
    false ->
        ok
end,

Result
```

---

## 12. Testing Recommendations

To verify timeout behavior:

```erlang
%% Test HTTP timeout
test_http_timeout() ->
    %% Mock slow endpoint
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request, fun(_, _, _, _) -> timer:sleep(10000), {ok, response} end),

    %% Should timeout in 5s, not hang
    Start = erlang:monotonic_time(millisecond),
    Result = erlmcp_otel_jaeger:export(#{}, SpanData),
    Duration = erlang:monotonic_time(millisecond) - Start,

    ?assert(Duration < 6000),  % Should timeout before 6s
    ?assertEqual({error, timeout}, Result),

    meck:unload(httpc).

%% Test Mnesia transaction timeout
test_mnesia_timeout() ->
    %% Create lock contention
    spawn_link(fun() ->
        mnesia:transaction(fun() ->
            mnesia:write(#session{id = <<"test">>}),
            timer:sleep(10000)  % Hold lock for 10s
        end)
    end),
    timer:sleep(100),  % Let first transaction acquire lock

    %% Should timeout, not wait for lock
    Start = erlang:monotonic_time(millisecond),
    Result = erlmcp_session_mnesia:save(#session{id = <<"test">>}),
    Duration = erlang:monotonic_time(millisecond) - Start,

    ?assert(Duration < 6000),  % Should timeout in 5s
    ?assertMatch({error, timeout}, Result).
```

---

## Conclusion

The erlmcp codebase demonstrates **strong timeout discipline** overall, with most operations properly bounded. The main issues are:

1. **4 critical violations** where HTTP requests lack timeouts (concentrated in observability code)
2. **Mnesia transactions** without explicit timeout protection
3. A **few operations with long timeouts** that are mostly justified by their use case

The fixes are straightforward and low-risk. Priority should be:
1. Add HTTP timeouts (30 minutes of work, high impact)
2. Add Mnesia timeout wrapper (2 hours of work, medium impact)
3. Review and adjust long timeouts (1 hour of work, low impact)

**Estimated Effort**: 4-6 hours to address all critical and high-priority issues.

**Risk Level**: Low - adding timeouts is a defensive change that prevents hangs without changing behavior in the success case.
