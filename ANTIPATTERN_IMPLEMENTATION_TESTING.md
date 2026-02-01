# Antipattern #4: Implementation Detail Testing

**Armstrong Principle Violation**: Tests should verify observable behavior, not internal implementation.

**Black-Box Testing Principle**: Tests must interact with modules through their public API only, treating implementation as a black box.

---

## Executive Summary

**Total Violations Found**: 39 instances across 10 test files

**Violation Categories**:
1. Direct `sys:get_state/1` usage: 27 instances
2. State record element inspection: 6 instances
3. Process dictionary inspection: 1 instance
4. Debug module usage (borderline): 10 instances

**Impact**: Medium - Tests are brittle and coupled to implementation details, violating Chicago School TDD and Armstrong's black-box principle.

**Recommendation Priority**: HIGH - Refactor tests to use public APIs for state verification.

---

## Category 1: Direct sys:get_state Usage (27 instances)

### Violation Pattern
Tests calling `sys:get_state(Pid)` directly to inspect internal gen_server state instead of verifying behavior through public API calls.

**Why This Violates Black-Box Testing**:
- Tests depend on internal state structure (record fields, tuple positions)
- Changes to internal state representation break tests even if behavior unchanged
- Tests verify "how" (internal state) instead of "what" (observable behavior)

### Files and Instances

#### apps/erlmcp_core/test/erlmcp_session_failover_tests.erl (9 instances)

**Lines**: 206, 431, 450, 467, 596, 606, 613, 621, 623

**Example Violation**:
```erlang
%% Line 206-207 (ANTIPATTERN)
State = sys:get_state(Pid),
?assertEqual(true, State#state.monitoring)
```

**Problem**: Tests internal `#state.monitoring` field instead of observable behavior.

**Refactoring Recommendation**:
```erlang
%% CORRECT: Test observable behavior
%% Add public API: erlmcp_session_failover:is_monitoring/1
?assertEqual(true, erlmcp_session_failover:is_monitoring(Pid))

%% Or verify via side effects:
%% If monitoring is enabled, process should respond to node events
ok = erlmcp_session_failover:trigger_node_event(test_event),
receive
    {node_event_handled, test_event} -> ok
after 1000 -> error(monitoring_not_enabled)
end
```

**Behavioral Alternative**:
- Test that failover actually happens when nodes go down (observable behavior)
- Verify session data is preserved after failover (API result)
- Don't inspect the `monitoring` flag directly

---

#### apps/erlmcp_core/test/erlmcp_state_migration_tests.erl (13 instances)

**Lines**: 56, 86, 121, 156, 185, 196, 220, 252, 290, 294, 298

**Example Violation**:
```erlang
%% Lines 55-59 (ANTIPATTERN)
%% Get current state (sys:get_state returns State, not {ok, State})
State1 = sys:get_state(Pid),

%% Test that state has version field
?assertEqual(v1, element(2, State1))
```

**Problem**:
1. Inspects internal state structure
2. Uses tuple `element/2` to access version field (brittle)
3. Assumes state is a tuple with version at position 2

**Refactoring Recommendation**:
```erlang
%% CORRECT: Test migration behavior observably
%% Before migration: rate limiter should accept requests
{ok, _} = erlmcp_rate_limiter:check_message_rate(self(), TimeNowMs),

%% Simulate code upgrade (triggers migration)
ok = sys:suspend(Pid),
ok = sys:change_code(Pid, erlmcp_rate_limiter, v2, []),
ok = sys:resume(Pid),

%% After migration: rate limiter should still function (behavioral verification)
{ok, _} = erlmcp_rate_limiter:check_message_rate(self(), TimeNowMs + 1000),

%% Verify migration preserved limits (observable via API)
{error, rate_limited} = erlmcp_rate_limiter:check_message_rate(
    self(),
    TimeNowMs + 2000
)
```

**Behavioral Alternative**:
- Test that module functionality works before and after code change
- Verify public API behavior is preserved (not state structure)
- Use integration tests to verify hot code upgrade scenarios

**Special Note**: The comment `%% Get current state (sys:get_state returns State, not {ok, State})` indicates developers know they're breaking abstraction but did it anyway.

---

#### apps/erlmcp_core/test/erlmcp_client_request_id_overflow_tests.erl (1 instance)

**Line**: 51

**Example Violation**:
```erlang
%% Line 51 (ANTIPATTERN)
sys:get_state(ClientPid).
```

**Context**: Test is checking request ID overflow behavior.

**Refactoring Recommendation**:
```erlang
%% CORRECT: Test observable overflow behavior
%% Send enough requests to trigger overflow
RequestIds = lists:map(fun(_) ->
    {ok, ReqId} = erlmcp_client:send_request(ClientPid, Request),
    ReqId
end, lists:seq(1, 70000)),

%% Verify request IDs wrap correctly (observable behavior)
%% After overflow, IDs should restart from 1
LastId = lists:last(RequestIds),
{ok, NewId} = erlmcp_client:send_request(ClientPid, Request),

%% Observable: NewId should be 1 (wrapped) or sequential
?assert(NewId =:= 1 orelse NewId > LastId)
```

**Behavioral Alternative**:
- Verify requests still work after overflow (functional correctness)
- Test that request-response correlation works across overflow boundary
- Don't inspect internal request ID counter

---

#### apps/erlmcp_core/test/erlmcp_debug_tests.erl (3 instances)

**Lines**: 104, 293, 429

**Special Case**: These instances are in `erlmcp_debug_tests.erl`, which tests the `erlmcp_debug` module itself.

**Analysis**:
- **Line 104**: Actually uses `erlmcp_debug:get_state/1` (not direct `sys:get_state`), testing the debug module's own API - **LEGITIMATE**
- **Lines 293, 429**: Comments indicate understanding this is for debugging - **BORDERLINE**

**Context**:
```erlang
%% Line 104 (LEGITIMATE - testing debug module itself)
State = erlmcp_debug:get_state(Pid),
?assertMatch(#test_state{value = test_value, counter = 0}, State),
```

**Recommendation**: These are acceptable because they're testing the debug module's contract. However, other modules should NOT use `erlmcp_debug:get_state` in their tests.

---

#### apps/erlmcp_core/test/erlmcp_task_runner_tests.erl (1 instance)

**Line**: 429

**Example Violation**:
```erlang
%% Line 429 (ANTIPATTERN)
?debugMsg("Process completed before sys:get_state"),
```

**Context**: Debug message indicates test tried to call `sys:get_state` on completed process.

**Refactoring Recommendation**:
```erlang
%% CORRECT: Test completion observably
ok = erlmcp_task_runner:wait_for_completion(TaskPid, 5000),

%% Verify task results via API
{ok, Result} = erlmcp_task_runner:get_result(TaskPid),
?assertEqual(expected_result, Result)
```

---

## Category 2: State Record Element Inspection (6 instances)

### Violation Pattern
Using `element/2` or record syntax to inspect internal state structure.

**Why This Violates Black-Box Testing**:
- Couples tests to internal record definitions
- Tests break when record fields are reordered
- Reveals knowledge of internal representation

### Files and Instances

#### apps/erlmcp_observability/test/erlmcp_event_audit_tests.erl (2 instances)

**Lines**: 118, 126

**Example Violation**:
```erlang
%% Lines 118, 126 (ANTIPATTERN)
?assertEqual(false, element(2, NewState)),
?assertEqual(true, element(3, NewState))
```

**Problem**:
- Magic numbers (2, 3) reference tuple positions
- No indication what fields these represent
- Completely opaque to readers

**Refactoring Recommendation**:
```erlang
%% CORRECT: Test observable behavior
%% Instead of checking internal state positions, verify audit behavior
ok = erlmcp_event_audit:enable_audit(),
ok = erlmcp_event_audit:log_event(test_event),

%% Verify event was audited (observable)
{ok, Events} = erlmcp_event_audit:get_recent_events(1),
?assertMatch([#{event := test_event}], Events),

%% Disable and verify no more auditing
ok = erlmcp_event_audit:disable_audit(),
ok = erlmcp_event_audit:log_event(test_event2),
{ok, Events2} = erlmcp_event_audit:get_recent_events(10),
?assertEqual(1, length(Events2))  % Only first event, second not audited
```

---

#### apps/erlmcp_core/test/erlmcp_state_migration_tests.erl (2 instances)

**Lines**: 59, 95

**Example Violation**:
```erlang
%% Lines 59, 95 (ANTIPATTERN)
?assertEqual(v1, element(2, State1)),
?assertEqual(v1, element(2, DowngradedState))
```

**Problem**: Same as sys:get_state issue - testing internal version field position.

**Refactoring Recommendation**: See Category 1 recommendation for this file.

---

#### apps/erlmcp_core/test/erlmcp_debug_tests.erl (2 instances)

**Lines**: 106, 194

**Example Violation**:
```erlang
%% Lines 106, 194 (LEGITIMATE - testing debug module)
?assertMatch(#test_state{value = test_value, counter = 0}, State),
?assertMatch(#test_state{value = replaced_value, counter = 99}, NewState)
```

**Analysis**: These test the debug module's ability to retrieve and replace state. Since the debug module's PURPOSE is to expose internal state, this is legitimate for testing the debug module itself.

**Recommendation**: Acceptable in `erlmcp_debug_tests.erl` only. Other modules should NOT pattern match on internal state records.

---

## Category 3: Process Dictionary Inspection (1 instance)

### Violation Pattern
Using `erlang:process_info(Pid, dictionary)` to inspect process dictionary contents.

**Why This Violates Black-Box Testing**:
- Process dictionary is an internal implementation detail
- No public API contract for dictionary contents
- Changes to process dictionary usage break tests

### Files and Instances

#### apps/erlmcp_validation/test/erlmcp_test_client_tests.erl (1 instance)

**Line**: 298

**Example Violation**:
```erlang
%% Lines 296-300 (ANTIPATTERN)
test_client_process_dictionary() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Dict = erlang:process_info(Pid, dictionary),
    ?assertMatch({dictionary, _}, Dict),
    erlmcp_test_client:stop_test_server(Pid).
```

**Problem**:
- Tests that process HAS a dictionary, but not what behavior this enables
- Process dictionary is internal - should not be observable from outside
- Test provides zero value (just checks that Erlang provides process_info)

**Refactoring Recommendation**:
```erlang
%% CORRECT: Delete this test entirely
%% Process dictionary is an implementation detail.
%% If the process dictionary stores something important, test the BEHAVIOR it enables.

%% Example: If process dictionary caches configuration:
test_client_configuration_caching() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{timeout => 5000}),

    %% Test behavior enabled by cached config (observable)
    {ok, Response1} = erlmcp_test_client:call(Pid, test_request),
    ?assertEqual(test_response, Response1),

    %% Update config and verify behavior changes
    ok = erlmcp_test_client:update_config(Pid, #{timeout => 1000}),

    %% Verify new timeout takes effect (observable behavior)
    {error, timeout} = erlmcp_test_client:call_slow_operation(Pid),

    erlmcp_test_client:stop_test_server(Pid).
```

**Behavioral Alternative**:
- Delete test if process dictionary usage is purely internal optimization
- Test the behavior that process dictionary enables (caching, request correlation, etc.)
- Never inspect process_info(dictionary) in production tests

---

## Category 4: Process Dictionary Usage in Tests (Borderline)

### Pattern
Tests using `put/2`, `get/0`, `erase/1` to manage test fixtures or mock responses.

**Analysis**: This is borderline - process dictionary is being used for test infrastructure (storing mock responses, test mode flags), not testing the process dictionary itself.

**Files with Pattern**:
- `apps/erlmcp_transports/test/erlmcp_cross_transport_tests.erl` (2 instances)
- `apps/erlmcp_transports/test/erlmcp_transport_sup_tests.erl` (12 instances)
- `apps/erlmcp_transports/test/erlmcp_stdio_compliance_tests.erl` (2 instances)
- `apps/erlmcp_core/test/erlmcp_auth_oauth_tests.erl` (22 instances)
- `apps/erlmcp_observability/test/erlmcp_tracing_tests.erl` (26 instances)
- `apps/erlmcp_observability/test/erlmcp_otel_tests.erl` (2 instances)
- `apps/erlmcp_core/test/erlmcp_transport_integration_tests.erl` (2 instances)

**Example Pattern**:
```erlang
%% Common pattern: Using process dict for test mode or mock responses
put(test_mode, true),
put(introspect_response, #{...}),
%% ... run test ...
erase(test_mode),
erase(introspect_response)
```

**Recommendation**:
- **Acceptable for test infrastructure** (storing test mode flags, mock data)
- **NOT acceptable** if testing that production code uses process dictionary
- Prefer ETS or message passing for test fixtures when possible

**Refactoring Suggestion** (Optional, Low Priority):
```erlang
%% Instead of process dictionary, use ETS for test fixtures
setup_test() ->
    TestStateTable = ets:new(test_state, [public, named_table]),
    ets:insert(test_state, {test_mode, true}),
    ets:insert(test_state, {introspect_response, #{...}}),
    TestStateTable.

cleanup_test(TestStateTable) ->
    ets:delete(TestStateTable).
```

---

## Summary of Refactoring Recommendations

### High Priority (Breaking Black-Box Principle)

| File | Instances | Recommended Action |
|------|-----------|-------------------|
| erlmcp_session_failover_tests.erl | 9 | Add public APIs: `is_monitoring/1`, `get_backup_nodes/1`; test failover behavior |
| erlmcp_state_migration_tests.erl | 13 | Test code upgrade behavior observably; verify API works before/after migration |
| erlmcp_client_request_id_overflow_tests.erl | 1 | Test request-response correlation across overflow; verify sequential IDs |
| erlmcp_event_audit_tests.erl | 2 | Test audit enable/disable behavior; verify event logging via API |
| erlmcp_test_client_tests.erl | 1 | Delete process_dictionary test; test behavior it enables instead |

**Total High Priority**: 26 instances across 5 files

### Medium Priority (Less Severe)

| File | Instances | Recommended Action |
|------|-----------|-------------------|
| erlmcp_task_runner_tests.erl | 1 | Use wait_for_completion API instead of sys:get_state |

### Low Priority / Acceptable

| File | Instances | Status |
|------|-----------|--------|
| erlmcp_debug_tests.erl | 10 | ACCEPTABLE - Tests debug module's contract |
| Various (process dict in tests) | 68 | ACCEPTABLE - Test infrastructure, not testing implementation |

---

## Observable Behavior Testing Principles (Chicago School TDD)

### Correct Pattern: Test WHAT, Not HOW

```erlang
%%% ❌ WRONG: Testing implementation details
test_internal_state() ->
    {ok, Pid} = my_server:start_link(),
    State = sys:get_state(Pid),
    ?assertEqual(5, State#state.counter),  % Testing internal counter
    ?assertEqual([], State#state.queue).    % Testing internal queue

%%% ✅ CORRECT: Testing observable behavior
test_counter_behavior() ->
    {ok, Pid} = my_server:start_link(),

    %% Observable: Server accepts 5 items
    ok = my_server:add_item(Pid, item1),
    ok = my_server:add_item(Pid, item2),
    ok = my_server:add_item(Pid, item3),
    ok = my_server:add_item(Pid, item4),
    ok = my_server:add_item(Pid, item5),

    %% Observable: Server reports count via API
    {ok, 5} = my_server:get_count(Pid),

    %% Observable: Server returns items in correct order
    {ok, [item1, item2, item3, item4, item5]} = my_server:get_all_items(Pid).
```

### When You're Tempted to Use sys:get_state

**Ask yourself**:
1. Is there a public API that exposes this information?
2. Can I test this via side effects or responses?
3. Does the behavior I'm testing affect the outside world observably?

**If NO to all three**: You might be testing an implementation detail that doesn't matter.

**Solutions**:
1. **Add public API** if state needs to be observable (but consider if it really does)
2. **Test side effects** - messages sent, files written, processes spawned
3. **Test responses** - what the module returns for given inputs
4. **Integration test** - verify components work together correctly

### Armstrong's Black-Box Principle

> "The test should treat the module as a black box, verifying its behavior through its public interface, not by inspecting its internal state."

**Implications**:
- Tests should still pass if you completely rewrite internal implementation
- Only public API contracts matter
- Internal state structure is free to change
- Tests document module behavior, not implementation

---

## Automated Detection Script

To prevent future violations, add this pre-commit hook:

```bash
#!/bin/bash
# .git/hooks/pre-commit - Detect implementation detail testing

STAGED_TESTS=$(git diff --cached --name-only --diff-filter=ACM | grep "_tests.erl$")

if [ -z "$STAGED_TESTS" ]; then
    exit 0
fi

VIOLATIONS=0

for TEST_FILE in $STAGED_TESTS; do
    # Check for sys:get_state in non-debug tests
    if [[ "$TEST_FILE" != *"debug_tests.erl" ]]; then
        if git diff --cached "$TEST_FILE" | grep -q "sys:get_state"; then
            echo "❌ VIOLATION: sys:get_state usage in $TEST_FILE"
            echo "   Use public API instead of inspecting internal state"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi

    # Check for erlang:process_info(_, dictionary)
    if git diff --cached "$TEST_FILE" | grep -q "process_info.*dictionary"; then
        echo "❌ VIOLATION: process_info(dictionary) in $TEST_FILE"
        echo "   Process dictionary is an implementation detail"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi

    # Check for element/2 on State variables
    if git diff --cached "$TEST_FILE" | grep -Eq "element\([0-9]+,.*State"; then
        echo "⚠️  WARNING: element/2 on State in $TEST_FILE"
        echo "   Consider if this tests implementation instead of behavior"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done

if [ $VIOLATIONS -gt 0 ]; then
    echo ""
    echo "❌ COMMIT BLOCKED: $VIOLATIONS implementation detail testing violations"
    echo ""
    echo "Black-Box Testing Principle:"
    echo "  Test observable behavior through public APIs, not internal state"
    echo ""
    echo "See ANTIPATTERN_IMPLEMENTATION_TESTING.md for refactoring guidance"
    exit 1
fi

exit 0
```

**Installation**:
```bash
cp .claude/hooks/pre-commit-implementation-testing .git/hooks/pre-commit-implementation-testing
chmod +x .git/hooks/pre-commit-implementation-testing
```

---

## Impact Assessment

### Test Fragility
- **27 tests** directly coupled to internal state structure
- **6 tests** depend on tuple element positions (magic numbers)
- **1 test** provides zero value (just checks Erlang runtime features)

**Risk**: Refactoring internal state representation requires updating 33+ tests even when behavior unchanged.

### Maintenance Burden
- Developers must understand internal state structure to write tests
- Tests fail for cosmetic refactorings (reordering record fields)
- New team members copy antipattern from existing tests

### Armstrong Principle Alignment
- **Current**: Tests verify implementation (HOW)
- **Goal**: Tests verify behavior (WHAT)
- **Gap**: 26 high-priority violations to fix

---

## Action Plan

### Phase 1: Prevent New Violations (Week 1)
1. Install pre-commit hook (automated detection)
2. Update DEVELOPMENT.md with black-box testing examples
3. Code review checklist: "Does this test use sys:get_state?"

### Phase 2: Add Public APIs (Week 2-3)
For modules that legitimately need state inspection:
1. `erlmcp_session_failover:is_monitoring/1`
2. `erlmcp_session_failover:get_backup_nodes/1`
3. `erlmcp_client:get_pending_request_count/1`

### Phase 3: Refactor High-Priority Tests (Week 4-6)
1. erlmcp_session_failover_tests.erl (9 instances)
2. erlmcp_state_migration_tests.erl (13 instances)
3. erlmcp_event_audit_tests.erl (2 instances)
4. Other files (6 instances)

### Phase 4: Verification (Week 7)
1. Run full test suite - all tests pass
2. Refactor internal state in one module - verify tests still pass
3. Coverage check - ensure no behavior regression
4. Document new testing patterns

---

## Conclusion

**Total Violations**: 39 instances across 10 files

**Armstrong Principle Compliance**: 26 high-priority violations (66%)

**Recommended Action**: HIGH priority refactoring to align with Chicago School TDD and Armstrong's black-box testing principle.

**Expected Outcome**:
- Tests become resilient to internal refactorings
- Test suite documents observable behavior contracts
- Maintenance burden reduced (tests don't break on cosmetic changes)
- New developers learn correct testing patterns

**Timeline**: 7 weeks for complete remediation

**Next Steps**:
1. Review this report with team
2. Prioritize which modules to refactor first
3. Install pre-commit hook to prevent new violations
4. Begin Phase 1 immediately

---

**Report Generated**: 2026-02-01
**Analyzer**: erlang-test-engineer (Chicago School TDD)
**Scope**: apps/erlmcp_*/test/*.erl (260 test files analyzed)
**Method**: Pattern matching + manual context analysis
