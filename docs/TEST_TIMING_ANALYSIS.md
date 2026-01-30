# Test Timing Issues Analysis

**Date**: 2026-01-30
**Scope**: Comprehensive analysis of timing-related issues in erlmcp test suite
**Focus**: Flaky tests, slow tests, and proper synchronization mechanisms

---

## Executive Summary

The erlmcp test suite has **28 test files** with extensive use of `timer:sleep/1` for synchronization. This analysis identifies **4 categories of timing issues** and provides **specific recommendations** for each category.

### Key Findings

| Category | Files Affected | Issues | Severity |
|----------|---------------|--------|----------|
| **Hardcoded delays** | 28 files | Race conditions, flaky tests | HIGH |
| **TTL/cleanup timing** | 3 files | Tests wait for periodic cleanup | MEDIUM |
| **Network connection timing** | 4 files | Client-server handshake delays | MEDIUM |
| **Slow test execution** | 8 files | Excessive sleep times (2-15s) | MEDIUM |

---

## 1. Hardcoded Delays (Race Conditions)

### Problem
Tests use fixed `timer:sleep/1` delays to wait for async operations, creating race conditions:
- If system is slow: Test fails (timeout)
- If system is fast: Test wastes time

### Examples

#### erlmcp_cache_tests.erl (Lines 82, 91, 163-167, 176, 436, 586)
```erlang
%% BAD: Hardcoded delay assumes cache table creation in 100ms
timer:sleep(100),

%% BAD: Assumes cleanup completes in 50ms
timer:sleep(50);

%% BAD: Complex TTL expiration logic with multiple sleeps
timer:sleep(1500),  % Wait for expiration
timer:sleep(500),   % Wait for cleanup
```

**Issues**:
- Line 82: Assumes Mnesia table creation completes in 100ms
- Line 91: Assumes gen_server:stop completes in 50ms
- Lines 163-167: Two sequential sleeps for TTL expiration (2s total)
- Line 436: Assumes async cache warming completes in 100ms

#### erlmcp_transport_tcp_tests.erl (Lines 40, 48, 109, 129, 168, 193, 226, 234, 306, 403, 456, 524, 580, 587, 638, 666, 680)
```erlang
%% BAD: Assumes server starts in 100ms
timer:sleep(100),  % Give server time to start

%% BAD: Assumes ranch cleanup in 50-100ms
timer:sleep(50);
timer:sleep(100);

%% BAD: Connection attempt timing
timer:sleep(200),  % Wait for initial connection attempt
timer:sleep(100),  % Wait for initial connection attempt

%% BAD: Long wait for reconnection
timer:sleep(4000),  % Wait for client reconnection
```

**Issues**:
- 15 instances of hardcoded delays
- Connection timing assumes network operations complete in 100-200ms
- Reconnection test waits 4 seconds (excessive)

#### erlmcp_chaos_tests.erl (Lines 80, 104, 124, 152, 202, 225, 250, 277, 293, 309, 334)
```erlang
%% BAD: Assumes experiment starts in 500ms
timer:sleep(500),

%% BAD: Chaos experiment assumes completion in 1-2.5s
timer:sleep(1000),
timer:sleep(2500),

%% BAD: Resource exhaustion assumes completion in 1.5s
timer:sleep(1500),
```

**Issues**:
- Chaos experiments use fixed delays for async failure injection
- No verification that experiment actually started
- No verification that experiment completed

#### erlmcp_dashboard_tests.erl (Lines 53, 60, 88, 242, 265, 291)
```erlang
%% BAD: Assumes dashboard server ready in 100ms
timer:sleep(100),

%% BAD: Assumes metrics recorded in 100ms
timer:sleep(100),

%% BAD: Assumes bucket rotation in 1100ms
timer:sleep(1100),
```

**Issues**:
- WebSocket tests assume server ready in 100ms
- Bucket rotation test waits fixed 1.1s (should verify rotation occurred)

### Recommendation: Synchronization Primitives

**Pattern 1: Verify State Instead of Sleeping**
```erlang
%% BAD: Hardcoded delay
timer:sleep(100),
{ok, Value} = erlmcp_cache:get(Key),

%% GOOD: Poll with timeout
wait_for_value(Key, Timeout) ->
    wait_for_value(Key, Timeout, 100).

wait_for_value(Key, Timeout, Interval) ->
    case erlmcp_cache:get(Key) of
        {ok, _Value} -> ok;
        {error, not_found} when Timeout > 0 ->
            timer:sleep(Interval),
            wait_for_value(Key, Timeout - Interval, Interval);
        {error, not_found} ->
            {error, timeout}
    end.

%% Usage
?assertEqual(ok, wait_for_value(<<"key1">>, 2000)),
```

**Pattern 2: Use gen_server:call with Sync Responses**
```erlang
%% BAD: Assume operation completes
ok = erlmcp_cache:put(Key, Value),
timer:sleep(100),
{ok, Value} = erlmcp_cache:get(Key),

%% GOOD: Use synchronous API that returns when complete
{ok, _} = erlmcp_cache:put_sync(Key, Value),
{ok, Value} = erlmcp_cache:get(Key),
```

**Pattern 3: Monitor Process Completion**
```erlang
%% BAD: Assume process completes in 100ms
Pid = spawn(fun() -> expensive_operation() end),
timer:sleep(100),
?assertNot(is_process_alive(Pid)),

%% GOOD: Monitor process lifecycle
Pid = spawn(fun() -> expensive_operation() end),
Ref = monitor(process, Pid),
receive
    {'DOWN', Ref, process, Pid, _Reason} -> ok
after 5000 ->
    ?assert(false, "Process did not complete")
end.
```

---

## 2. TTL/Cleanup Timing Issues

### Problem
Tests for TTL expiration rely on periodic cleanup processes with hardcoded intervals.

### Examples

#### erlmcp_cache_tests.erl (Lines 153-181)
```erlang
test_ttl_expiration(_Pid) ->
    ?_test(begin
        %% Put with 1 second TTL
        ok = erlmcp_cache:put(<<"key1">>, <<"value1">>, {ttl, 1}),

        %% Immediately available
        ?assertEqual({ok, <<"value1">>}, erlmcp_cache:get(<<"key1">>)),

        %% Wait for expiration
        timer:sleep(1500),

        %% Should be expired (cleanup runs every 1s in test config)
        timer:sleep(500),  % Wait for cleanup
        ?assertEqual({error, not_found}, erlmcp_cache:get(<<"key1">>))
    end).
```

**Issues**:
- Test depends on cleanup interval (1000ms in config)
- Two sequential sleeps (2s total) create flakiness
- If cleanup runs at t=999ms, test passes
- If cleanup runs at t=1001ms, test fails (race condition)

### Recommendation: Deterministic TTL Testing

**Pattern 1: Manual Cleanup Trigger**
```erlang
%% GOOD: Add test API to trigger cleanup immediately
test_ttl_expiration(_Pid) ->
    ?_test(begin
        %% Put with 1 second TTL
        ok = erlmcp_cache:put(<<"key1">>, <<"value1">>, {ttl, 1}),

        %% Immediately available
        ?assertEqual({ok, <<"value1">>}, erlmcp_cache:get(<<"key1">>)),

        %% Advance time and trigger cleanup (test mode only)
        ok = erlmcp_cache:advance_time(1500),  % Test API
        ok = erlmcp_cache:trigger_cleanup(),  % Test API

        %% Should be expired
        ?assertEqual({error, not_found}, erlmcp_cache:get(<<"key1">>))
    end).
```

**Pattern 2: Short TTL + Fast Cleanup in Test Config**
```erlang
%% GOOD: Use test-specific config with fast cleanup
setup() ->
    Config = #{
        max_l1_size => 100,
        max_l2_size => 1000,
        default_ttl_seconds => 5,
        cleanup_interval_ms => 100  % Fast cleanup for tests
    },
    {ok, Pid} = erlmcp_cache:start_link(Config),
    Pid.

test_ttl_expiration(_Pid) ->
    ?_test(begin
        %% Put with 100ms TTL (fast for tests)
        ok = erlmcp_cache:put(<<"key1">>, <<"value1">>, {ttl, 100}),

        %% Wait for expiration + cleanup (200ms total)
        timer:sleep(200),

        %% Should be expired
        ?assertEqual({error, not_found}, erlmcp_cache:get(<<"key1">>))
    end).
```

**Pattern 3: Verify Cleanup Event**
```erlang
%% GOOD: Subscribe to cleanup event and wait
test_ttl_expiration(_Pid) ->
    ?_test(begin
        %% Subscribe to cleanup events
        {ok, _} = erlmcp_cache:subscribe(cleanup_complete),

        %% Put with 100ms TTL
        ok = erlmcp_cache:put(<<"key1">>, <<"value1">>, {ttl, 100}),

        %% Wait for cleanup event
        receive
            {cleanup_complete, _Timestamp} -> ok
        after 1000 ->
            ?assert(false, "Cleanup did not complete")
        end,

        %% Should be expired
        ?assertEqual({error, not_found}, erlmcp_cache:get(<<"key1">>))
    end).
```

---

## 3. Network Connection Timing Issues

### Problem
TCP/WebSocket tests use hardcoded delays for connection establishment.

### Examples

#### erlmcp_transport_tcp_tests.erl (Lines 270-300)
```erlang
%% BAD: Wait for connection with timeout
receive
    {transport_connected, ClientPid} ->
        ?assert(true)
after 5000 ->
    ?assert(false, "Client connection timeout")
end,
```

**Better**: This is actually GOOD pattern (uses receive...after), but other parts of the test use timer:sleep.

#### erlmcp_transport_tcp_tests.erl (Lines 129, 226)
```erlang
%% BAD: Assumes connection attempt completes in 100ms
timer:sleep(100),  % Wait for initial connection attempt

%% BAD: Assumes ranch accepts connection in 200ms
timer:sleep(200),  % Give ranch time to accept
```

### Recommendation: Verify Connection State

**Pattern 1: Query Connection State**
```erlang
%% BAD: Assume connection established
timer:sleep(200),
ok = gen_server:call(ClientPid, {send, Message}),

%% GOOD: Verify connection state before use
wait_for_connected(Pid, Timeout) ->
    wait_for_connected(Pid, Timeout, 50).

wait_for_connected(Pid, Timeout, Interval) when Timeout > 0 ->
    case gen_server:call(Pid, get_state) of
        {ok, #state{connected = true}} -> ok;
        _ ->
            timer:sleep(Interval),
            wait_for_connected(Pid, Timeout - Interval, Interval)
    end;
wait_for_connected(_, _, _) ->
    {error, timeout}.

%% Usage
?assertEqual(ok, wait_for_connected(ClientPid, 2000)),
ok = gen_server:call(ClientPid, {send, Message}),
```

**Pattern 2: Use Ranch Callbacks**
```erlang
%% GOOD: Ranch provides connection callbacks
%% In transport handler:
handle_connected(Socket, Transport) ->
    Owner = get_owner(),
    Owner ! {transport_connected, self()},
    {ok, #state{socket = Socket}}.

%% In test:
receive
    {transport_connected, Handler} -> Handler
after 3000 ->
    error("Connection timeout")
end.
```

---

## 4. Slow Test Execution

### Problem
Excessive sleep times make tests slow (2-15 seconds per test).

### Examples

#### erlmcp_chaos_tests.erl (Line 309)
```erlang
chaos_resource_test_() ->
    {timeout, 15,  % 15 second timeout!
     fun() ->
         Config = #{target_percent => 0.70, duration => 1000},
         Pid = spawn(fun() -> erlmcp_chaos_resource:exhaust_memory(Config) end),
         timer:sleep(1500),  % 1.5 second sleep
         ?assertNot(is_process_alive(Pid))
     end}.
```

#### erlmcp_transport_tcp_tests.erl (Line 524)
```erlang
timer:sleep(4000),  % 4 second wait for reconnection
```

#### erlmcp_dashboard_tests.erl (Line 265)
```erlang
test_bucket_rotation() ->
    erlmcp_metrics_aggregator:record_metric(throughput, test, 100),
    timer:sleep(1100),  % 1.1 second wait for bucket rotation
    erlmcp_metrics_aggregator:record_metric(throughput, test, 200),
    ...
```

### Performance Impact

| Test File | Sleep Time | Test Count | Total Time |
|-----------|------------|------------|------------|
| erlmcp_chaos_tests.erl | 10-15s | 10 | ~150s |
| erlmcp_transport_tcp_tests.erl | 4-5s | 15+ | ~60s |
| erlmcp_dashboard_tests.erl | 1-2s | 13 | ~20s |
| erlmcp_cache_tests.erl | 2s | 18 | ~36s |

**Total estimated time**: 266 seconds (4.4 minutes) for these 4 files alone.

### Recommendation: Reduce Sleep Times

**Pattern 1: Use Faster Test Configuration**
```erlang
%% BAD: Production-like config in tests
Config = #{
    cleanup_interval_ms => 1000,  % 1 second
    default_ttl_seconds => 5,
    ...
},

%% GOOD: Fast test config
Config = #{
    cleanup_interval_ms => 50,   % 50ms (20x faster)
    default_ttl_seconds => 1,    % 1 second (5x faster)
    ...
},
```

**Pattern 2: Use Mock Time for Time-Dependent Tests**
```erlang
%% GOOD: Use meck to mock time functions
setup() ->
    ok = meck:new(erlmcp_time, [no_link]),
    ok = meck:expect(erlmcp_time, system_time, fun(millisecond) -> 0 end),
    Config = fast_test_config(),
    {ok, Pid} = erlmcp_cache:start_link(Config),
    Pid.

test_ttl_expiration(_Pid) ->
    ?_test(begin
        ok = erlmcp_cache:put(<<"key1">>, <<"value1">>, {ttl, 1000}),

        %% Advance time by 1001ms (instant in test!)
        ok = meck:expect(erlmcp_time, system_time, fun(millisecond) -> 1001 end),
        ok = erlmcp_cache:trigger_cleanup(),

        %% Should be expired (no sleep needed!)
        ?assertEqual({error, not_found}, erlmcp_cache:get(<<"key1">>))
    end).

cleanup(_) ->
    meck:unload(erlmcp_time).
```

**Pattern 3: Reduce Timeouts for Fast Failure**
```erlang
%% BAD: 5 second timeout (slow failure)
receive
    {transport_connected, Pid} -> ok
after 5000 ->
    ?assert(false, "Timeout")
end,

%% GOOD: 500ms timeout (fast failure)
receive
    {transport_connected, Pid} -> ok
after 500 ->
    ?assert(false, "Timeout")
end.
```

---

## 5. Specific File Recommendations

### erlmcp_cache_tests.erl

**Issues**:
- 9 instances of timer:sleep
- TTL tests depend on 1000ms cleanup interval
- Mnesia table creation timing (line 82)
- Async cache warming timing (line 436)

**Recommendations**:
1. Add `erlmcp_cache:trigger_cleanup/0` for tests
2. Use fast cleanup interval (50ms) in test config
3. Poll for Mnesia table creation instead of sleep
4. Monitor async warm process instead of sleep

**Estimated time savings**: 18 tests * 1.5s = **27 seconds**

### erlmcp_transport_tcp_tests.erl

**Issues**:
- 15 instances of timer:sleep
- 4 second reconnection wait (line 524)
- Connection establishment timing (lines 129, 226)

**Recommendations**:
1. Replace all timer:sleep with state polling
2. Use verify_connected() helper function
3. Reduce reconnection test timeout to 500ms
4. Use Ranch connection callbacks

**Estimated time savings**: 15 tests * 3s = **45 seconds**

### erlmcp_chaos_tests.erl

**Issues**:
- 11 instances of timer:sleep
- 15 second timeout on resource test
- No verification that experiments started

**Recommendations**:
1. Add `erlmcp_chaos:wait_experiment_started/1` API
2. Use short-duration chaos experiments (100ms)
3. Monitor experiment lifecycle instead of sleep
4. Remove 15s timeout (use 3s)

**Estimated time savings**: 10 tests * 10s = **100 seconds**

### erlmcp_dashboard_tests.erl

**Issues**:
- 6 instances of timer:sleep
- 1.1 second bucket rotation wait
- WebSocket connection timing

**Recommendations**:
1. Verify WebSocket upgrade message instead of sleep
2. Add `erlmcp_metrics_aggregator:trigger_rotation/0` for tests
3. Poll for server readiness instead of sleep
4. Use faster bucket rotation interval (100ms)

**Estimated time savings**: 13 tests * 1s = **13 seconds**

---

## 6. Implementation Priority

### High Priority (Fix Flaky Tests)

1. **erlmcp_cache_tests.erl**: TTL expiration tests
   - Add trigger_cleanup API
   - Replace sleeps with polling
   - **Impact**: 18 tests, removes race conditions

2. **erlmcp_transport_tcp_tests.erl**: Connection tests
   - Replace all timer:sleep with state polling
   - Add verify_connected helper
   - **Impact**: 15 tests, removes flakiness

### Medium Priority (Speed Up Tests)

3. **erlmcp_chaos_tests.erl**: Reduce experiment duration
   - Short-duration experiments (100ms)
   - Monitor lifecycle instead of sleep
   - **Impact**: 100 second savings

4. **erlmcp_dashboard_tests.erl**: Bucket rotation test
   - Add trigger_rotation API
   - Verify server ready instead of sleep
   - **Impact**: 13 second savings

### Low Priority (Nice to Have)

5. All test files: Add test helpers
   - Create `erlmcp_test_helpers` module
   - Standardize polling, state verification
   - **Impact**: Consistent test patterns

---

## 7. Test Helper Module Proposal

Create `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_test_helpers.erl`:

```erlang
%%% @doc Test synchronization helpers to replace timer:sleep
-module(erlmcp_test_helpers).

%% State polling
-export([wait_for_state/3, wait_for_state/4]).
%% Process lifecycle
-export([wait_for_process_death/2, wait_for_process_death/3]).
%% Gen server calls
-export([call_with_retry/4, call_with_retry/5]).
%% Mnesia helpers
-export([wait_for_table/2, wait_for_table/3]).

%% @doc Wait for gen_server state to match predicate
-spec wait_for_state(pid(), function(), timeout()) -> ok | {error, timeout}.
wait_for_state(Pid, Predicate, Timeout) ->
    wait_for_state(Pid, Predicate, Timeout, 100).

wait_for_state(Pid, Predicate, Timeout, Interval) when Timeout > 0 ->
    case gen_server:call(Pid, get_state) of
        {ok, State} when is_function(Predicate, 1) ->
            case Predicate(State) of
                true -> ok;
                false ->
                    timer:sleep(Interval),
                    wait_for_state(Pid, Predicate, Timeout - Interval, Interval)
            end;
        _ ->
            timer:sleep(Interval),
            wait_for_state(Pid, Predicate, Timeout - Interval, Interval)
    end;
wait_for_state(_, _, _, _) ->
    {error, timeout}.

%% @doc Wait for process to terminate
-spec wait_for_process_death(pid(), timeout()) -> ok | {error, timeout}.
wait_for_process_death(Pid, Timeout) ->
    Ref = monitor(process, Pid),
    wait_for_process_death(Pid, Ref, Timeout).

wait_for_process_death(Pid, Ref, Timeout) ->
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after Timeout ->
        {error, timeout}
    end.

%% @doc Wait for Mnesia table to be available
-spec wait_for_table(atom(), timeout()) -> ok | {error, timeout}.
wait_for_table(TableName, Timeout) ->
    wait_for_table(TableName, Timeout, 100).

wait_for_table(TableName, Timeout, Interval) when Timeout > 0 ->
    case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            case mnesia:wait_for_tables([TableName], 100) of
                ok -> ok;
                {timeout, _} ->
                    timer:sleep(Interval),
                    wait_for_table(TableName, Timeout - Interval, Interval)
            end;
        false ->
            timer:sleep(Interval),
            wait_for_table(TableName, Timeout - Interval, Interval)
    end;
wait_for_table(_, _, _) ->
    {error, timeout}.

%% @doc Retry gen_server:call with backoff
-spec call_with_retry(pid(), term, non_neg_integer(), timeout()) -> {ok, term()} | {error, term()}.
call_with_retry(Pid, Request, MaxRetries, Timeout) ->
    call_with_retry(Pid, Request, MaxRetries, Timeout, 50).

call_with_retry(_Pid, _Request, 0, _Timeout, _Interval) ->
    {error, max_retries_exceeded};
call_with_retry(Pid, Request, Retries, Timeout, Interval) ->
    try gen_server:call(Pid, Request, Timeout) of
        Response -> {ok, Response}
    catch
        exit:{noproc, _} ->
            timer:sleep(Interval),
            call_with_retry(Pid, Request, Retries - 1, Timeout, Interval * 2);
        exit:{timeout, _} ->
            timer:sleep(Interval),
            call_with_retry(Pid, Request, Retries - 1, Timeout, Interval * 2)
    end.
```

---

## 8. Migration Path

### Phase 1: Add Test Helpers (Week 1)
1. Create `erlmcp_test_helpers` module
2. Add state polling, process monitoring helpers
3. Document usage patterns

### Phase 2: Fix High-Priority Tests (Week 2)
1. **erlmcp_cache_tests.erl**:
   - Add `trigger_cleanup` API to erlmcp_cache
   - Replace TTL sleeps with polling
   - Verify test passes consistently

2. **erlmcp_transport_tcp_tests.erl**:
   - Replace connection sleeps with `wait_for_state`
   - Reduce timeouts
   - Verify test passes consistently

### Phase 3: Speed Up Tests (Week 3)
1. **erlmcp_chaos_tests.erl**:
   - Shorten experiment durations
   - Add lifecycle monitoring
   - Verify time savings

2. **erlmcp_dashboard_tests.erl**:
   - Add `trigger_rotation` API
   - Replace sleeps with polling
   - Verify time savings

### Phase 4: Verification (Week 4)
1. Run full test suite 10 times to verify no flakiness
2. Measure time savings
3. Document best practices

---

## 9. Metrics and Success Criteria

### Current State
- **Flaky tests**: 8-10 tests (estimated) fail intermittently due to timing
- **Slow tests**: 4 test files take 266 seconds (4.4 minutes)
- **timer:sleep count**: 50+ instances across 28 files

### Target State
- **Flaky tests**: 0 (all timing issues fixed)
- **Slow tests**: < 60 seconds total (4x improvement)
- **timer:sleep count**: < 10 (only where absolutely necessary)

### Success Criteria
1. All tests pass 10 times in a row (no flakiness)
2. Test suite runs in < 2 minutes (currently ~5 minutes)
3. Zero timer:sleep in critical paths (connections, TTL, async ops)
4. Test helper module with comprehensive patterns

---

## 10. Conclusion

The erlmcp test suite has significant timing issues that can be categorized into:
1. **Hardcoded delays** causing race conditions (HIGH severity)
2. **TTL/cleanup timing** creating slow, flaky tests (MEDIUM severity)
3. **Network connection timing** with excessive waits (MEDIUM severity)
4. **Slow test execution** wasting developer time (MEDIUM severity)

**Recommended approach**:
- Add test helper module with synchronization primitives
- Replace timer:sleep with state polling and process monitoring
- Add test-specific APIs (trigger_cleanup, trigger_rotation)
- Use fast test configurations
- Verify no flakiness with 10 consecutive runs

**Expected impact**:
- 4x faster test execution (5 min → 2 min)
- Zero flaky tests
- Better developer experience
- More reliable CI/CD

---

**Next Steps**:
1. Review and approve this analysis
2. Prioritize implementation phases
3. Assign work to erlang-test-engineer agent
4. Track progress with GitHub project board
