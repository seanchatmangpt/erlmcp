# Phase 1a: Test Infrastructure Fixes - Implementation Plan

**Status:** Ready for Implementation
**Priority:** CRITICAL (Blocks all quality gates)
**Estimated Effort:** 8-12 hours
**Target Completion:** 2-3 days
**Last Updated:** 2026-01-31

---

## 1. Overview

### 1.1 Summary of Test Failures

The erlmcp test infrastructure is experiencing systematic failures across multiple test suites. Analysis of the codebase reveals four categories of failures that prevent the project from achieving the mandatory 80% pass rate and blocking quality gates:

1. **Process Registration Leaks**: Tests start singleton processes (erlmcp_connection_limiter) multiple times without proper cleanup, causing gproc registration conflicts and race conditions
2. **API Signature Mismatches**: Test code calls functions with incorrect parameter ordering, particularly in erlmcp_spec_compliance_SUITE.erl where tool/prompt/resource APIs are invoked with swapped parameters
3. **Async Timing Issues**: Heavy reliance on timer:sleep() for synchronization creates race conditions and intermittent failures
4. **Test Isolation Failures**: Tests don't clean up processes, monitors, and gproc registrations, causing cascading failures in test suites

**Current State:**
- Compilation: Blocked (rebar3 unavailable in container, but Makefile targets exist)
- EUnit Tests: ~30-40% estimated pass rate (cannot run without rebar3)
- Common Test Suites: ~50-60% estimated pass rate (cannot run without rebar3)
- Dialyzer: Unknown (blocked by compilation)
- Coverage: Unknown (blocked by test failures)

### 1.2 Impact on Quality Gates

These test failures have cascading impacts:

**Immediate Blockers:**
- ✗ `make validate-test` - BLOCKED (0% pass rate goal: 100%)
- ✗ `make validate-coverage` - BLOCKED (cannot measure coverage with failing tests)
- ✗ `make validate-compile` - BLOCKED (in container without rebar3)
- ✗ `make validate-quality` - BLOCKED (dialyzer needs clean compilation)
- ✗ `make validate` - BLOCKED (all gates must pass)

**Development Impact:**
- Cannot merge PRs (CI/CD fails)
- Cannot make commits (pre-commit hooks fail)
- Cannot release versions (release gates fail)
- Developer morale impacted (broken tests frustrate TDD workflow)

**TCPS Manufacturing Impact:**
- 自働化 (Jidoka) - Quality is NOT built-in (tests fail)
- 行灯 (Andon) - Andon cord PULLED (production stopped)
- ポカヨケ (Poka-yoke) - Error-proofing FAILED (tests don't prevent defects)

### 1.3 Success Criteria

**Phase 1a is complete when:**

1. ✅ **Compilation**: `TERM=dumb rebar3 compile` passes with 0 errors, 0 warnings
2. ✅ **EUnit Tests**: `rebar3 eunit` passes with ≥95% pass rate
3. ✅ **Common Test**: `rebar3 ct` passes with ≥90% pass rate
4. ✅ **No Race Conditions**: Tests pass consistently (10 consecutive runs)
5. ✅ **Proper Cleanup**: All tests clean up processes, monitors, gproc registrations
6. ✅ **Test Isolation**: Tests can run in any order (sequence/shuffle/parallel)
7. ✅ **Coverage Baseline**: Establish baseline coverage (≥80% target for Phase 1b)
8. ✅ **Quality Gates**: `make validate-test` passes (unblocks remaining gates)

**Acceptance Test:**
```bash
# All commands must exit with code 0
make validate-compile
make validate-test
make test-strict
rebar3 eunit --module=erlmcp_connection_limiter_tests
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE
```

---

## 2. Issue #1: Process Registration Leaks

### 2.1 Problem Description

**Root Cause:** `erlmcp_connection_limiter` is a singleton process registered with gproc. Tests start this process in `setup()` (line 40), but individual tests ALSO start/stop the process (lines 92, 104, 160, 238, 300, 316, etc.), creating race conditions where:
1. Test A starts the limiter
2. Test A's setup() tries to start it again → {error, {already_started, _}}
3. Test A stops the limiter
4. Test B's setup() tries to start it but gproc registration is still clearing
5. Test B fails with `{failed_to_register_counter, timeout}`

**File:** `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_connection_limiter_tests.erl`

**Affected Lines:**
- Line 23-54: `setup()` function
- Line 40: `{ok, _Pid} = erlmcp_connection_limiter:start_link()`
- Line 56-58: `cleanup(_)` function
- Lines 87-98: `test_multiple_starts()` - restarts limiter without coordination
- Lines 158-160, 236-238, 298-305, 314-316, etc.: Individual tests restart limiter

### 2.2 Specific Code Changes Needed

#### Change 1: Fix setup() to be idempotent

**Location:** Lines 23-54

**Current Code (WRONG):**
```erlang
setup() ->
    % Start gproc first (minimal dependency)
    case application:start(gproc) of
        ok ->
            ok;
        {error, {already_started, gproc}} ->
            ok
    end,

    % Set up default config - use erlmcp_core application name
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Start the connection limiter
    {ok, _Pid} = erlmcp_connection_limiter:start_link(),  % ← PROBLEM: Crashes if already started

    % Wait for gproc counter to be fully registered
    timer:sleep(10),  % ← PROBLEM: Race condition

    % Verify counter is accessible
    case gproc:where({c, l, erlmcp_connection_count}) of
        undefined ->
            error({failed_to_register_counter, timeout});
        _ ->
            ok
    end,

    ok.
```

**Fixed Code:**
```erlang
setup() ->
    % Start gproc first (minimal dependency)
    case application:start(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,

    % Stop any existing limiter instance (cleanup from previous test)
    case whereis(erlmcp_connection_limiter) of
        undefined -> ok;
        OldPid ->
            erlmcp_connection_limiter:stop(),
            % Wait for process to fully terminate
            wait_for_process_death(OldPid, 100)
    end,

    % Clear any lingering gproc counters
    clear_gproc_counters(),

    % Set up default config
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Start the connection limiter (now guaranteed to be clean)
    {ok, Pid} = erlmcp_connection_limiter:start_link(),

    % Synchronously wait for gproc counter registration (no race condition)
    ok = wait_for_gproc_counter({c, l, erlmcp_connection_count}, 1000),

    Pid.  % Return pid for tests to use

%% Helper: Wait for process death (synchronous)
wait_for_process_death(Pid, TimeoutMs) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} ->
            ok
    after TimeoutMs ->
        error({process_still_alive, Pid})
    end.

%% Helper: Wait for gproc counter registration (synchronous)
wait_for_gproc_counter(Key, TimeoutMs) ->
    wait_for_gproc_counter(Key, TimeoutMs, erlang:system_time(millisecond)).

wait_for_gproc_counter(Key, TimeoutMs, StartTime) ->
    case gproc:where(Key) of
        undefined ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            case Elapsed > TimeoutMs of
                true ->
                    error({failed_to_register_counter, timeout});
                false ->
                    timer:sleep(10),
                    wait_for_gproc_counter(Key, TimeoutMs, StartTime)
            end;
        _ ->
            ok
    end.

%% Helper: Clear lingering gproc counters
clear_gproc_counters() ->
    try
        gproc:unreg({c, l, erlmcp_connection_count})
    catch
        error:badarg -> ok  % Already unregistered
    end,
    % Clear per-server counters (if any)
    ok.
```

**Why This Matters:**
- **Idempotency**: setup() can now be called multiple times safely
- **No Race Conditions**: Synchronous waiting replaces timer:sleep()
- **Proper Cleanup**: Old process is guaranteed dead before new one starts
- **Predictability**: gproc counter is guaranteed ready before tests run

#### Change 2: Fix cleanup() to be robust

**Location:** Lines 56-58

**Current Code (WRONG):**
```erlang
cleanup(_) ->
    erlmcp_connection_limiter:stop(),  % ← PROBLEM: Crashes if not running
    application:unset_env(erlmcp_core, connection_limiting).
```

**Fixed Code:**
```erlang
cleanup(SetupResult) ->
    % Stop limiter only if it's running
    case whereis(erlmcp_connection_limiter) of
        undefined -> ok;
        Pid ->
            erlmcp_connection_limiter:stop(),
            wait_for_process_death(Pid, 100)
    end,

    % Clean up application config
    application:unset_env(erlmcp_core, connection_limiting),

    % Clean up gproc registrations
    clear_gproc_counters(),

    ok.
```

**Why This Matters:**
- **Robustness**: cleanup() never crashes (even if limiter already stopped)
- **Complete Cleanup**: All resources freed (process + gproc + config)

#### Change 3: Remove redundant starts in individual tests

**Affected Tests:**
- `test_multiple_starts()` (lines 86-98)
- `test_reject_over_limit()` (lines 149-174)
- `test_alert_at_70_percent()` (lines 227-251)
- All tests that do `catch erlmcp_connection_limiter:stop()` then restart

**Example Fix for `test_multiple_starts()`:**

**Current Code (WRONG):**
```erlang
test_multiple_starts() ->
    % Clean up first instance if running
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(100),  % ← PROBLEM: Race condition

    % Start new instance
    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    % Clean up for other tests
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50).
```

**Fixed Code:**
```erlang
test_multiple_starts() ->
    % Get the limiter pid from setup (already running)
    Pid = whereis(erlmcp_connection_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    % Test that we can't start a second instance
    ?assertMatch({error, {already_started, _}},
                 erlmcp_connection_limiter:start_link()),

    % Verify original instance still alive
    ?assert(erlang:is_process_alive(Pid)).
```

**Why This Matters:**
- **No Restart Chaos**: Tests use the singleton instance from setup()
- **True Test**: Actually tests "multiple start attempts fail gracefully"
- **No Cleanup Needed**: setup/cleanup handles all lifecycle

#### Change 4: Fix tests that need custom config

**Affected Tests:**
- `test_reject_over_limit()` - needs max_connections=5
- `test_alert_at_70_percent()` - needs max_connections=100
- All tests that call `application:set_env()` then restart

**Strategy:** Use separate test fixtures with custom setup/cleanup

**Example for low-limit tests:**

**Current Code (WRONG):**
```erlang
test_reject_over_limit() ->
    % Set a low limit for testing
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 5,
        alert_threshold => 0.7,
        enabled => true
    }),

    % Restart with new config
    catch erlmcp_connection_limiter:stop(),
    timer:sleep(50),  % ← RACE CONDITION
    {ok, _} = erlmcp_connection_limiter:start_link(),
    % ... rest of test
```

**Fixed Code:**
```erlang
%% New test group with custom fixture
connection_limit_low_test_() ->
    {setup,
     fun setup_low_limit/0,
     fun cleanup/1,
     [
         ?_test(test_reject_over_limit()),
         ?_test(test_refusal_before_exhaustion()),
         ?_test(test_refusal_error_message()),
         ?_test(test_recovery_after_refusal())
     ]}.

setup_low_limit() ->
    % Start gproc
    case application:start(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,

    % Stop any existing instance
    case whereis(erlmcp_connection_limiter) of
        undefined -> ok;
        OldPid ->
            erlmcp_connection_limiter:stop(),
            wait_for_process_death(OldPid, 100)
    end,
    clear_gproc_counters(),

    % Set LOW limit config
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 5,  % ← Low limit for these tests
        alert_threshold => 0.7,
        enabled => true
    }),

    % Start with low limit
    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    ok = wait_for_gproc_counter({c, l, erlmcp_connection_count}, 1000),
    Pid.

test_reject_over_limit() ->
    ServerId = test_server_3,

    % Accept 5 connections (at limit)
    AcceptResults = [erlmcp_connection_limiter:accept_connection(ServerId) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(accept) -> true; (_) -> false end, AcceptResults)),

    % 6th should be rejected
    RejectResult = erlmcp_connection_limiter:accept_connection(ServerId),
    ?assertMatch({error, too_many_connections}, RejectResult),

    % Clean up
    _ = [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, 5)],
    ok.
```

**Why This Matters:**
- **Clean Separation**: Tests with different configs use different fixtures
- **No Restart Chaos**: Each fixture starts cleanly
- **Proper Grouping**: Related tests grouped logically

### 2.3 Before/After Test File Structure

**Before (WRONG - 11 test groups, shared setup, restarts everywhere):**
```
- module_lifecycle_test_() [shared setup, 3 tests]
- connection_limit_enforcement_test_() [shared setup, 5 tests]
- alert_threshold_test_() [shared setup, 2 tests, RESTARTS]
- configuration_test_() [shared setup, 4 tests, RESTARTS]
- gproc_counter_test_() [shared setup, 3 tests]
- graceful_refusal_test_() [shared setup, 3 tests, RESTARTS]
- capacity_test_() [shared setup, 3 tests, RESTARTS]
- concurrent_access_test_() [shared setup, 2 tests]
- edge_cases_test_() [shared setup, 3 tests]
- integration_test_() [shared setup, 2 tests]
```

**After (CORRECT - Grouped by config needs, no restarts):**
```
- module_lifecycle_test_() [setup: default config, 3 tests]
- connection_limit_enforcement_test_() [setup: default config, 5 tests]
- gproc_counter_test_() [setup: default config, 3 tests]
- concurrent_access_test_() [setup: default config, 2 tests]
- edge_cases_test_() [setup: default config, 3 tests]
- integration_test_() [setup: default config, 2 tests]

- connection_limit_low_test_() [setup_low_limit: max=5, 4 tests]
- connection_limit_1k_test_() [setup_1k_limit: max=1000, 3 tests]
- alert_threshold_100_test_() [setup_100_limit: max=100, 2 tests]
- configuration_custom_test_() [setup_custom_config: various, 4 tests]
```

**Changes:**
- ✅ Default config tests share one fixture (no restarts)
- ✅ Custom config tests have dedicated fixtures
- ✅ Each fixture does proper cleanup
- ✅ No test restarts the limiter mid-test

### 2.4 Verification Steps

**After implementing changes, verify:**

```bash
# 1. Run connection_limiter tests 10 times (check consistency)
for i in {1..10}; do
    rebar3 eunit --module=erlmcp_connection_limiter_tests || exit 1
done

# 2. Run in verbose mode to see no registration errors
rebar3 eunit --module=erlmcp_connection_limiter_tests --verbose 2>&1 | grep -i "registration\|already_started\|timeout"

# 3. Check process cleanup (should be empty after tests)
erl -eval 'application:start(gproc), timer:sleep(100), io:format("~p~n", [whereis(erlmcp_connection_limiter)]), halt().'

# 4. Run with shuffle to test independence
rebar3 eunit --module=erlmcp_connection_limiter_tests --order random
```

**Success:** All commands exit 0, no registration errors, process cleanup complete.

---

## 3. Issue #2: API Signature Mismatches

### 3.1 Problem Description

**Root Cause:** The test suite `erlmcp_spec_compliance_SUITE.erl` calls server APIs with incorrect parameter ordering. The most common error pattern:

**Expected API:** `erlmcp_server:add_tool_with_description(ServerPid, Name, Description, Handler)`
**Test Calls:** `erlmcp_server:add_tool_with_description(ServerPid, Name, Handler, Description)` ← Handler/Description swapped!

This causes tests to fail with function_clause errors because the guards check `is_function(Handler, 1)` but receive a binary (Description) instead.

**File:** `/home/user/erlmcp/apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl`

### 3.2 List of Failing Tests

| Test Name | Line | Current Call (WRONG) | Correct Call | Fix |
|-----------|------|----------------------|--------------|-----|
| `list_tools_with_descriptions` | 392 | `add_tool_with_description(Pid, <<"echo">>, EchoHandler, <<"Echoes input arguments">>)` | `add_tool_with_description(Pid, <<"echo">>, <<"Echoes input arguments">>, EchoHandler)` | Swap params 3&4 |
| `tool_schema_validation` | 653 | `add_tool_with_schema(Pid, <<"validated_tool">>, Handler, Schema)` | (Already correct) | No change |
| All other tool tests | Various | Likely correct | - | Verify |

**Additional API signature issues to check:**
- `add_prompt_with_args/4` calls
- `add_resource/3` calls
- Any other multi-parameter APIs

### 3.3 Detailed Fixes

#### Fix 1: `list_tools_with_descriptions` test

**Location:** Lines 385-403

**Current Code (WRONG):**
```erlang
list_tools_with_descriptions(_Config) ->
    %% Test: List tools with descriptions
    ServerCaps = #mcp_server_capabilities{tools = #{}},
    {ok, ServerPid} = erlmcp_server:start_link(make_server_id(), ServerCaps),

    %% Add tool with description
    EchoHandler = fun(Args) -> #{<<"result">> => Args} end,
    ok = erlmcp_server:add_tool_with_description(
        ServerPid,
        <<"echo">>,
        EchoHandler,                          % ← WRONG: Handler in position 3
        <<"Echoes input arguments">>          % ← WRONG: Description in position 4
    ),
    % ... rest of test
```

**Fixed Code:**
```erlang
list_tools_with_descriptions(_Config) ->
    %% Test: List tools with descriptions
    ServerCaps = #mcp_server_capabilities{tools = #{}},
    {ok, ServerPid} = erlmcp_server:start_link(make_server_id(), ServerCaps),

    %% Add tool with description
    EchoHandler = fun(Args) -> #{<<"result">> => Args} end,
    ok = erlmcp_server:add_tool_with_description(
        ServerPid,
        <<"echo">>,
        <<"Echoes input arguments">>,         % ← CORRECT: Description in position 3
        EchoHandler                           % ← CORRECT: Handler in position 4
    ),

    %% Verify: Tool added (observable state)
    ?assert(is_process_alive(ServerPid)),

    erlmcp_server:stop(ServerPid),
    ok.
```

**Verification:**
```bash
# Compile and check function signature in BEAM
rebar3 compile
erl -pa _build/default/lib/*/ebin -eval 'io:format("~p~n", [erlmcp_server:module_info(exports)]), halt().' | grep add_tool_with_description

# Expected output: {add_tool_with_description,4}
```

#### Fix 2: Verify all other tool API calls

**Strategy:** Grep for all API calls and verify signatures

```bash
# Find all add_tool_with_description calls
grep -n "add_tool_with_description" apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl

# Find all add_tool_with_schema calls
grep -n "add_tool_with_schema" apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl

# Find all add_prompt calls
grep -n "add_prompt" apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl

# Find all add_resource calls
grep -n "add_resource" apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl
```

**For each call found:**
1. Check actual API signature in erlmcp_server.erl
2. Compare with test call
3. Fix if mismatch

### 3.4 Complete Test File Audit

**Systematic approach:**

1. **Create reference documentation**
```erlang
%% API REFERENCE (from erlmcp_server.erl)
%% Tools:
%%   add_tool(Server, Name, Handler) -> ok
%%   add_tool_with_description(Server, Name, Description, Handler) -> ok
%%   add_tool_with_schema(Server, Name, Handler, Schema) -> ok
%%   add_tool_full(Server, Name, Description, Handler, Options) -> ok
%%
%% Prompts:
%%   add_prompt(Server, Name, Handler) -> ok
%%   add_prompt_with_args(Server, Name, Handler, Arguments) -> ok
%%   add_prompt_with_args_and_schema(Server, Name, Handler, Arguments, InputSchema) -> ok
%%
%% Resources:
%%   add_resource(Server, Uri, Handler) -> ok
%%   add_resource_template(Server, UriTemplate, Name, Handler) -> ok
```

2. **Audit every test**
Create a checklist:
```
[ ] list_tools_empty (line 357) - Uses add_tool → VERIFY
[ ] list_tools_with_descriptions (line 385) - Uses add_tool_with_description → FIX (params swapped)
[ ] list_tools_pagination (line 405) - Uses add_tool → VERIFY
[ ] call_tool_success (line 423) - Uses add_tool → VERIFY
[ ] call_tool_missing_args (line 463) - Uses add_tool → VERIFY
[ ] call_tool_invalid_args (line 503) - Uses add_tool → VERIFY
[ ] tool_schema_validation (line 636) - Uses add_tool_with_schema → VERIFY
... (continue for all 63 tests)
```

3. **Automated verification script**
```bash
#!/bin/bash
# scripts/verify_api_signatures.sh

echo "Verifying API signatures in test suite..."

# Check add_tool_with_description calls
echo "Checking add_tool_with_description..."
grep -n "add_tool_with_description" apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl | \
while read line; do
    echo "  Line: $line"
    # Manual inspection needed
done

# Expected: All calls should have (Pid, Name, Description, Handler) order
```

### 3.5 Verification Steps

**After fixing all API mismatches:**

```bash
# 1. Compile to catch remaining function_clause errors
TERM=dumb rebar3 compile 2>&1 | grep -i "function_clause\|no function clause matching"

# 2. Run spec compliance suite
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE

# 3. Check for specific errors in CT logs
cat _build/test/logs/*/erlmcp_spec_compliance_SUITE.logs/*.html | grep -i "function_clause"

# 4. Run each test group individually
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=tools_api
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=resources_api
```

**Success Criteria:**
- ✅ 0 function_clause errors
- ✅ All 63 tests in erlmcp_spec_compliance_SUITE pass
- ✅ No warnings about incorrect argument types

---

## 4. Issue #3: Async Timing Issues

### 4.1 Problem Description

**Root Cause:** Tests use `timer:sleep(N)` to wait for asynchronous operations, creating race conditions:
- `timer:sleep(10)` on line 44 (erlmcp_connection_limiter_tests.erl) - waiting for gproc registration
- `timer:sleep(50)` on lines 83, 89, 158, etc. - waiting for process termination
- `timer:sleep(100)` in spec compliance tests - waiting for message processing

This is a **classic anti-pattern** in concurrent systems: sleep-based synchronization assumes operations complete within fixed time, but under load or slow systems, they may not.

### 4.2 Root Cause Analysis

**Why timer:sleep() fails:**
1. **Non-deterministic timing**: Process scheduling is non-deterministic
2. **Load sensitivity**: Under CPU load, 50ms may not be enough
3. **False confidence**: Tests pass on fast machines, fail on CI
4. **Hard to debug**: Failures are intermittent and hard to reproduce

**Proper alternatives:**
1. **Synchronous waits**: Monitor processes and receive DOWN messages
2. **Polling with timeout**: Poll for condition with exponential backoff
3. **Explicit synchronization**: Use gen_server:call() to ensure completion

### 4.3 Code Changes

#### Pattern 1: Waiting for process death

**Current (WRONG):**
```erlang
erlmcp_connection_limiter:stop(),
timer:sleep(50),  % ← Hope it's dead
{ok, _} = erlmcp_connection_limiter:start_link()  % ← May crash
```

**Fixed:**
```erlang
Pid = whereis(erlmcp_connection_limiter),
erlmcp_connection_limiter:stop(),
wait_for_process_death(Pid, 1000),  % ← Guaranteed dead
{ok, _} = erlmcp_connection_limiter:start_link()  % ← Safe

%% Helper function
wait_for_process_death(Pid, TimeoutMs) when is_pid(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after TimeoutMs ->
        error({process_still_alive, Pid, TimeoutMs})
    end;
wait_for_process_death(undefined, _TimeoutMs) ->
    ok.  % Already dead
```

#### Pattern 2: Waiting for gproc registration

**Current (WRONG):**
```erlang
{ok, _Pid} = erlmcp_connection_limiter:start_link(),
timer:sleep(10),  % ← Hope gproc registered
case gproc:where({c, l, erlmcp_connection_count}) of
    undefined -> error(timeout);  % ← Race condition
    _ -> ok
end
```

**Fixed:**
```erlang
{ok, Pid} = erlmcp_connection_limiter:start_link(),
ok = wait_for_gproc_registration({c, l, erlmcp_connection_count}, 1000),

%% Helper function
wait_for_gproc_registration(Key, TimeoutMs) ->
    StartTime = erlang:system_time(millisecond),
    wait_for_gproc_registration_loop(Key, TimeoutMs, StartTime).

wait_for_gproc_registration_loop(Key, TimeoutMs, StartTime) ->
    case gproc:where(Key) of
        undefined ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            case Elapsed >= TimeoutMs of
                true ->
                    error({gproc_registration_timeout, Key, TimeoutMs});
                false ->
                    timer:sleep(10),  % Small sleep in loop is OK
                    wait_for_gproc_registration_loop(Key, TimeoutMs, StartTime)
            end;
        Pid when is_pid(Pid) ->
            ok
    end.
```

#### Pattern 3: Waiting for message processing (spec compliance tests)

**Current (WRONG):**
```erlang
TransportPid ! {simulate_input, jsx:encode(Request)},
timer:sleep(100),  % ← Hope server processed it
?assert(is_process_alive(ServerPid))  % ← Doesn't verify processing
```

**Fixed (Option A - Synchronous):**
```erlang
% Send request and wait for response
ok = send_and_wait_for_response(TransportPid, Request, 1000),

%% Helper
send_and_wait_for_response(TransportPid, Request, TimeoutMs) ->
    % Subscribe to responses from transport
    Ref = monitor(process, TransportPid),
    TransportPid ! {simulate_input, jsx:encode(Request), self(), Ref},
    receive
        {response_sent, Ref} ->
            demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, TransportPid, Reason} ->
            error({transport_died, Reason})
    after TimeoutMs ->
        demonitor(Ref, [flush]),
        error({response_timeout, TimeoutMs})
    end.
```

**Fixed (Option B - Polling):**
```erlang
TransportPid ! {simulate_input, jsx:encode(Request)},
ok = wait_for_server_state(ServerPid, fun(State) ->
    % Check if request was processed (state changed)
    State#state.initialized =:= true
end, 1000),

%% Helper
wait_for_server_state(ServerPid, ConditionFun, TimeoutMs) ->
    StartTime = erlang:system_time(millisecond),
    wait_for_server_state_loop(ServerPid, ConditionFun, TimeoutMs, StartTime).

wait_for_server_state_loop(ServerPid, ConditionFun, TimeoutMs, StartTime) ->
    try
        {ok, State} = gen_server:call(ServerPid, get_internal_state, 100),
        case ConditionFun(State) of
            true ->
                ok;
            false ->
                check_timeout_and_retry(ServerPid, ConditionFun, TimeoutMs, StartTime)
        end
    catch
        _:_ ->
            check_timeout_and_retry(ServerPid, ConditionFun, TimeoutMs, StartTime)
    end.

check_timeout_and_retry(ServerPid, ConditionFun, TimeoutMs, StartTime) ->
    Elapsed = erlang:system_time(millisecond) - StartTime,
    case Elapsed >= TimeoutMs of
        true ->
            error({state_condition_timeout, TimeoutMs});
        false ->
            timer:sleep(10),
            wait_for_server_state_loop(ServerPid, ConditionFun, TimeoutMs, StartTime)
    end.
```

### 4.4 Affected Files and Line Counts

| File | Lines with timer:sleep | Estimated Fixes |
|------|------------------------|-----------------|
| `erlmcp_connection_limiter_tests.erl` | ~20 occurrences | Replace 20 sleeps with 6 helper functions |
| `erlmcp_spec_compliance_SUITE.erl` | ~40 occurrences | Replace 40 sleeps with sync/polling |
| Other test files | ~50 occurrences (estimated) | Audit and fix systematically |

### 4.5 Synchronization Mechanisms to Add

**New helper module:** `apps/erlmcp_core/test/erlmcp_test_helpers.erl`

```erlang
-module(erlmcp_test_helpers).
-export([
    wait_for_process_death/2,
    wait_for_process_start/2,
    wait_for_gproc_registration/2,
    wait_for_gproc_unregistration/2,
    wait_for_condition/3,
    send_and_wait_response/3,
    poll_until/3
]).

%% Wait for process to die (synchronous via monitor)
-spec wait_for_process_death(pid() | atom(), pos_integer()) -> ok | {error, timeout}.
wait_for_process_death(Pid, TimeoutMs) when is_pid(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after TimeoutMs ->
        demonitor(Ref, [flush]),
        {error, {timeout, still_alive, Pid}}
    end;
wait_for_process_death(Name, TimeoutMs) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> wait_for_process_death(Pid, TimeoutMs)
    end.

%% Wait for process to start and register
-spec wait_for_process_start(atom(), pos_integer()) -> {ok, pid()} | {error, timeout}.
wait_for_process_start(Name, TimeoutMs) ->
    poll_until(fun() ->
        case whereis(Name) of
            undefined -> false;
            Pid when is_pid(Pid) -> {true, Pid}
        end
    end, TimeoutMs, 10).

%% Wait for gproc key to be registered
-spec wait_for_gproc_registration(term(), pos_integer()) -> ok | {error, timeout}.
wait_for_gproc_registration(Key, TimeoutMs) ->
    case poll_until(fun() ->
        case gproc:where(Key) of
            undefined -> false;
            Pid when is_pid(Pid) -> true
        end
    end, TimeoutMs, 10) of
        {ok, true} -> ok;
        {error, timeout} -> {error, {gproc_timeout, Key}}
    end.

%% Generic polling with timeout
-spec poll_until(fun(() -> boolean() | {true, term()}), pos_integer(), pos_integer()) ->
    {ok, term()} | {error, timeout}.
poll_until(ConditionFun, TimeoutMs, SleepMs) ->
    StartTime = erlang:system_time(millisecond),
    poll_until_loop(ConditionFun, TimeoutMs, SleepMs, StartTime).

poll_until_loop(ConditionFun, TimeoutMs, SleepMs, StartTime) ->
    case ConditionFun() of
        true ->
            {ok, true};
        {true, Value} ->
            {ok, Value};
        false ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            case Elapsed >= TimeoutMs of
                true ->
                    {error, timeout};
                false ->
                    timer:sleep(SleepMs),
                    poll_until_loop(ConditionFun, TimeoutMs, SleepMs, StartTime)
            end
    end.
```

### 4.6 Testing Verification

**Verify timing fixes work under stress:**

```bash
#!/bin/bash
# Run tests under CPU load to trigger timing issues

# Start CPU stress (4 cores)
stress-ng --cpu 4 --timeout 60s &
STRESS_PID=$!

# Run tests multiple times
for i in {1..10}; do
    echo "Run $i under load..."
    rebar3 eunit --module=erlmcp_connection_limiter_tests || {
        kill $STRESS_PID
        echo "FAILED under load on run $i"
        exit 1
    }
done

kill $STRESS_PID
echo "SUCCESS: All runs passed under load"
```

**Success Criteria:**
- ✅ Tests pass 10/10 times under CPU load
- ✅ No intermittent failures
- ✅ Average test time <5s (no excessive polling)

---

## 5. Issue #4: Test Isolation Failures

### 5.1 Problem Description

Tests don't clean up properly, leaving behind:
1. **Processes**: Lingering gen_servers from crashed tests
2. **Monitors**: Unreleased monitor references
3. **gproc registrations**: Uncleaned gproc keys
4. **Application config**: Leftover application:set_env() state
5. **ETS tables**: Dangling ETS tables from sessions/cache

**Impact:**
- Test B fails because Test A left garbage
- Tests can't run in parallel (shared state conflicts)
- Tests can't run in random order (dependencies)
- Memory leaks in test suite

### 5.2 Exact Resources That Leak

#### Leak 1: Connection limiter process

**Test:** `erlmcp_connection_limiter_tests:test_start_stop/0`

**Lines:** 78-84
```erlang
test_start_stop() ->
    Pid = whereis(erlmcp_connection_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_connection_limiter:stop(),
    timer:sleep(50),  % ← PROBLEM: May not be dead yet
    ?assertNot(erlang:is_process_alive(Pid)).  % ← Process may still be alive
```

**Cleanup needed:**
```erlang
test_start_stop() ->
    Pid = whereis(erlmcp_connection_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    % Synchronous stop
    Ref = monitor(process, Pid),
    erlmcp_connection_limiter:stop(),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        error({process_still_alive, Pid})
    end,

    ?assertNot(erlang:is_process_alive(Pid)).
```

#### Leak 2: gproc counters

**Test:** All tests in `erlmcp_connection_limiter_tests.erl`

**Problem:**
```erlang
cleanup(_) ->
    erlmcp_connection_limiter:stop(),
    application:unset_env(erlmcp_core, connection_limiting).
    % ← MISSING: gproc counter cleanup
```

**Fixed cleanup:**
```erlang
cleanup(_) ->
    % Stop process first
    case whereis(erlmcp_connection_limiter) of
        undefined -> ok;
        Pid ->
            Ref = monitor(process, Pid),
            erlmcp_connection_limiter:stop(),
            receive {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok  % Process may have crashed
            end
    end,

    % Clean gproc registrations
    clear_gproc_counters(),

    % Clean app config
    application:unset_env(erlmcp_core, connection_limiting),
    ok.

clear_gproc_counters() ->
    % Unregister global counter
    try gproc:unreg({c, l, erlmcp_connection_count})
    catch error:badarg -> ok end,

    % Note: Per-server counters are cleaned by connection_limiter process
    % but we should verify none are left
    ok.
```

#### Leak 3: Server processes in spec compliance tests

**Test:** All lifecycle tests in `erlmcp_spec_compliance_SUITE.erl`

**Lines:** 157-173 (init_server_capabilities)
```erlang
init_server_capabilities(_Config) ->
    ServerCaps = #mcp_server_capabilities{...},
    {ok, ServerPid} = erlmcp_server:start_link(make_server_id(), ServerCaps),
    ?assert(is_process_alive(ServerPid)),
    erlmcp_server:stop(ServerPid),
    ok.  % ← PROBLEM: Doesn't wait for death
```

**Fixed:**
```erlang
init_server_capabilities(_Config) ->
    ServerCaps = #mcp_server_capabilities{...},
    {ok, ServerPid} = erlmcp_server:start_link(make_server_id(), ServerCaps),
    ?assert(is_process_alive(ServerPid)),

    % Synchronous stop
    Ref = monitor(process, ServerPid),
    erlmcp_server:stop(ServerPid),
    receive
        {'DOWN', Ref, process, ServerPid, _} -> ok
    after 1000 ->
        error({server_still_alive, ServerPid})
    end,

    ?assertNot(is_process_alive(ServerPid)),
    ok.
```

#### Leak 4: Transport processes

**Test:** All transport tests
```erlang
erlmcp_transport_stdio:close(TransportPid),
erlmcp_server:stop(ServerPid),
ok.  % ← PROBLEM: No verification both stopped
```

**Fixed:**
```erlang
% Close transport and wait
TRef = monitor(process, TransportPid),
erlmcp_transport_stdio:close(TransportPid),
receive {'DOWN', TRef, process, TransportPid, _} -> ok
after 1000 -> error({transport_still_alive, TransportPid})
end,

% Stop server and wait
SRef = monitor(process, ServerPid),
erlmcp_server:stop(ServerPid),
receive {'DOWN', SRef, process, ServerPid, _} -> ok
after 1000 -> error({server_still_alive, ServerPid})
end,

ok.
```

### 5.3 Code Changes to Add Cleanup

**Strategy:** Add comprehensive cleanup to every test fixture

#### Pattern 1: EUnit fixtures with robust cleanup

```erlang
%% BEFORE (incomplete cleanup)
module_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [...]}.

setup() ->
    application:start(gproc),
    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    Pid.

cleanup(_) ->
    erlmcp_connection_limiter:stop().

%% AFTER (complete cleanup)
module_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [...]}.

setup() ->
    % Start dependencies
    ensure_app_started(gproc),

    % Clean any previous state
    cleanup_previous_state(),

    % Start fresh limiter
    {ok, Pid} = erlmcp_connection_limiter:start_link(),
    wait_for_gproc_registration({c, l, erlmcp_connection_count}, 1000),

    #{pid => Pid}.  % Return map for easier access

cleanup(#{pid := Pid}) ->
    % Stop process
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            erlmcp_connection_limiter:stop(),
            receive {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok  % May have crashed
            end;
        false ->
            ok
    end,

    % Clean gproc
    clear_gproc_counters(),

    % Clean app config
    application:unset_env(erlmcp_core, connection_limiting),

    % Verify cleanup
    verify_cleanup(),
    ok;
cleanup(_) ->
    % Fallback for setup failures
    cleanup_previous_state(),
    ok.

cleanup_previous_state() ->
    % Kill any lingering limiter
    case whereis(erlmcp_connection_limiter) of
        undefined -> ok;
        Pid ->
            exit(Pid, kill),
            timer:sleep(10)  % Small delay for kill
    end,

    % Clear gproc
    clear_gproc_counters(),

    % Clear config
    application:unset_env(erlmcp_core, connection_limiting),
    ok.

verify_cleanup() ->
    % Verify no limiter process
    undefined = whereis(erlmcp_connection_limiter),

    % Verify no gproc counter
    undefined = gproc:where({c, l, erlmcp_connection_count}),

    ok.
```

#### Pattern 2: Common Test init/end_per_testcase

```erlang
%% BEFORE (no per-test cleanup)
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    [{test_case, TestCase} | Config].

end_per_testcase(TestCase, Config) ->
    ct:log("Completed test case: ~p", [TestCase]),
    timer:sleep(50),  % ← WRONG
    Config.

%% AFTER (proper cleanup)
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),

    % Track processes started in this test
    ProcessesBefore = processes(),

    [{test_case, TestCase}, {processes_before, ProcessesBefore} | Config].

end_per_testcase(TestCase, Config) ->
    ct:log("Completed test case: ~p", [TestCase]),

    % Clean up any servers/transports started in test
    cleanup_test_processes(Config),

    % Verify no process leaks
    ProcessesBefore = ?config(processes_before, Config),
    verify_no_process_leaks(ProcessesBefore),

    Config.

cleanup_test_processes(Config) ->
    % Find any erlmcp_server processes
    Servers = [P || P <- processes(),
                    {dictionary, Dict} <- [process_info(P, dictionary)],
                    {initial_call, {erlmcp_server, _, _}} <- Dict],

    % Stop them all
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        catch erlmcp_server:stop(Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok
        after 500 -> ok
        end
    end, Servers),

    % Find transport processes
    Transports = [P || P <- processes(),
                       {dictionary, Dict} <- [process_info(P, dictionary)],
                       lists:any(fun({initial_call, {erlmcp_transport_stdio, _, _}}) -> true;
                                    (_) -> false
                                 end, Dict)],

    % Close them all
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        catch erlmcp_transport_stdio:close(Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok
        after 500 -> ok
        end
    end, Transports),

    ok.

verify_no_process_leaks(ProcessesBefore) ->
    ProcessesAfter = processes(),
    Leaked = ProcessesAfter -- ProcessesBefore,

    % Filter out expected system processes
    RealLeaks = [P || P <- Leaked,
                      not is_system_process(P)],

    case RealLeaks of
        [] ->
            ok;
        Leaks ->
            ct:log("WARNING: Process leaks detected: ~p", [Leaks]),
            lists:foreach(fun(P) ->
                ct:log("  Leaked process: ~p~n  Info: ~p",
                       [P, process_info(P)])
            end, Leaks)
    end.

is_system_process(Pid) ->
    case process_info(Pid, [registered_name, initial_call]) of
        undefined -> true;  % Already dead
        Info ->
            RegisteredName = proplists:get_value(registered_name, Info),
            InitialCall = proplists:get_value(initial_call, Info),

            % System processes
            lists:member(RegisteredName, [init, kernel_sup, application_controller]) orelse
            % Common test processes
            InitialCall =:= {ct_util, _} orelse
            InitialCall =:= {ct_logs, _}
    end.
```

### 5.4 Verification Checklist

After implementing cleanup code, verify:

**Test 1: No lingering processes**
```bash
# Start erl shell, run tests, check processes
erl -pa _build/test/lib/*/ebin
> eunit:test(erlmcp_connection_limiter_tests, [verbose]).
> length(processes()).  % Should return to baseline (~40-50 processes)
```

**Test 2: No lingering gproc registrations**
```bash
erl -pa _build/test/lib/*/ebin
> application:ensure_all_started(gproc).
> eunit:test(erlmcp_connection_limiter_tests).
> gproc:where({c, l, erlmcp_connection_count}).  % Should be 'undefined'
```

**Test 3: Tests pass in random order**
```bash
rebar3 eunit --module=erlmcp_connection_limiter_tests --order random
# Run 5 times to verify stability
```

**Test 4: Tests pass in parallel**
```bash
# Edit test file to use parallel fixtures
rebar3 eunit --module=erlmcp_connection_limiter_tests
```

**Success Criteria:**
- ✅ No lingering processes after tests
- ✅ No lingering gproc registrations
- ✅ Tests pass in random order (10/10 runs)
- ✅ No memory growth (run 100 times, check RSS)

---

## 6. Implementation Checklist

### 6.1 File-by-File Task Breakdown

#### File 1: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_connection_limiter_tests.erl`

**Estimated Lines to Change:** ~150 lines (out of 785 total)

**Tasks:**
- [ ] **Task 1.1**: Add helper functions module (lines 23-54)
  - [ ] `wait_for_process_death/2`
  - [ ] `wait_for_gproc_registration/2`
  - [ ] `clear_gproc_counters/0`
  - [ ] `cleanup_previous_state/0`
  - Estimated: 2 hours

- [ ] **Task 1.2**: Rewrite `setup/0` to be idempotent (lines 23-54)
  - [ ] Stop any existing limiter
  - [ ] Clear gproc counters
  - [ ] Use synchronous waits instead of timer:sleep
  - Estimated: 1 hour

- [ ] **Task 1.3**: Rewrite `cleanup/1` to be robust (lines 56-58)
  - [ ] Check if process alive before stopping
  - [ ] Synchronous wait for death
  - [ ] Clear all resources
  - Estimated: 30 minutes

- [ ] **Task 1.4**: Remove redundant starts in tests (lines 86-98, 149-174, etc.)
  - [ ] Fix `test_multiple_starts/0` to test actual behavior
  - [ ] Remove restarts from 12 tests
  - [ ] Verify each test uses setup() instance
  - Estimated: 2 hours

- [ ] **Task 1.5**: Create separate fixtures for custom configs
  - [ ] `setup_low_limit/0` for max=5 tests
  - [ ] `setup_1k_limit/0` for max=1000 tests
  - [ ] `setup_100_limit/0` for max=100 tests
  - [ ] Reorganize test groups
  - Estimated: 2 hours

- [ ] **Task 1.6**: Replace all timer:sleep with sync waits
  - [ ] 20 occurrences to fix
  - Estimated: 1 hour

**Total for File 1:** ~8.5 hours

#### File 2: `/home/user/erlmcp/apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl`

**Estimated Lines to Change:** ~100 lines (out of 1515 total)

**Tasks:**
- [ ] **Task 2.1**: Fix API signature mismatches
  - [ ] Fix `add_tool_with_description` call (line 392)
  - [ ] Audit all 63 tests for API mismatches
  - [ ] Create reference documentation in comments
  - Estimated: 3 hours

- [ ] **Task 2.2**: Add proper cleanup to init_per_suite
  - [ ] Stop all supervisors synchronously
  - [ ] Clear any lingering state
  - Estimated: 1 hour

- [ ] **Task 2.3**: Add robust init/end_per_testcase
  - [ ] Track processes before test
  - [ ] Clean up servers/transports after test
  - [ ] Verify no process leaks
  - Estimated: 2 hours

- [ ] **Task 2.4**: Replace timer:sleep with synchronous waits
  - [ ] ~40 occurrences in test bodies
  - [ ] Use monitor-based waits
  - Estimated: 2 hours

**Total for File 2:** ~8 hours

#### File 3: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_test_helpers.erl` (NEW)

**Estimated Lines:** ~200 lines

**Tasks:**
- [ ] **Task 3.1**: Create helper module
  - [ ] `wait_for_process_death/2`
  - [ ] `wait_for_process_start/2`
  - [ ] `wait_for_gproc_registration/2`
  - [ ] `wait_for_gproc_unregistration/2`
  - [ ] `poll_until/3`
  - [ ] `send_and_wait_response/3`
  - [ ] `cleanup_processes/1`
  - [ ] `verify_no_leaks/1`
  - Estimated: 3 hours

- [ ] **Task 3.2**: Write EUnit tests for helpers
  - [ ] Test each helper function
  - [ ] Test timeout behavior
  - [ ] Test error cases
  - Estimated: 2 hours

**Total for File 3:** ~5 hours

#### Files 4-N: Other test files (TBD based on audit)

**Estimated:** 20+ other test files with similar issues

**Tasks:**
- [ ] **Task N.1**: Audit all test files for timer:sleep usage
  - [ ] Run `grep -r "timer:sleep" apps/*/test/*.erl`
  - [ ] Categorize by severity
  - Estimated: 2 hours

- [ ] **Task N.2**: Fix critical test files first
  - [ ] Focus on files in CI/CD workflows
  - [ ] Fix files blocking quality gates
  - Estimated: 4 hours

**Total for Other Files:** ~6 hours

### 6.2 Overall Timeline

| Phase | Tasks | Duration | Cumulative |
|-------|-------|----------|------------|
| **Phase 1**: Helper infrastructure | File 3 (helpers module) | 5 hours | 5 hours |
| **Phase 2**: Fix connection_limiter tests | File 1 | 8.5 hours | 13.5 hours |
| **Phase 3**: Fix spec compliance tests | File 2 | 8 hours | 21.5 hours |
| **Phase 4**: Audit other test files | Files 4-N | 2 hours | 23.5 hours |
| **Phase 5**: Fix other critical tests | Files 4-N | 4 hours | 27.5 hours |
| **Phase 6**: Verification & Testing | All | 4 hours | 31.5 hours |

**Total Estimated Effort:** 31.5 hours (~4 working days)

### 6.3 Dependencies Between Fixes

```
Phase 1 (Helpers) ──┬──> Phase 2 (connection_limiter)
                    ├──> Phase 3 (spec_compliance)
                    └──> Phase 5 (other tests)

Phase 4 (Audit) ────────> Phase 5 (Fix other tests)

Phase 2 ──┬──> Phase 6 (Verification)
Phase 3 ──┤
Phase 5 ──┘
```

**Critical Path:** Phase 1 → Phase 2 → Phase 6 (minimum to unblock quality gates)

### 6.4 Recommended Team Assignment

**If solo developer:**
- Day 1: Phase 1 (helpers) + Phase 2 start (connection_limiter)
- Day 2: Phase 2 finish + Phase 3 start (spec_compliance)
- Day 3: Phase 3 finish + Phase 4 (audit)
- Day 4: Phase 5 (fix other tests) + Phase 6 (verification)

**If team of 2:**
- Developer A: Phase 1 → Phase 2
- Developer B: Phase 3 (can start after Phase 1 done)
- Both: Phase 4-6 (collaborate)

**If team of 3:**
- Developer A: Phase 1 + Phase 2
- Developer B: Phase 3
- Developer C: Phase 4 + Phase 5
- All: Phase 6 (verification)

---

## 7. Verification & Testing

### 7.1 Exact Bash Commands to Run

**Step 1: Verify helpers module**
```bash
# Compile helpers
rebar3 compile

# Run helper tests
rebar3 eunit --module=erlmcp_test_helpers_tests

# Expected: All helper tests pass
```

**Step 2: Verify connection_limiter fixes**
```bash
# Run connection_limiter tests (single run)
rebar3 eunit --module=erlmcp_connection_limiter_tests --verbose

# Run 10 times for consistency
for i in {1..10}; do
    echo "Run $i..."
    rebar3 eunit --module=erlmcp_connection_limiter_tests || {
        echo "FAILED on run $i"
        exit 1
    }
done

# Run in random order
rebar3 eunit --module=erlmcp_connection_limiter_tests --order random

# Expected: 100% pass rate, consistent results
```

**Step 3: Verify spec compliance fixes**
```bash
# Run full spec compliance suite
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE

# Run each group individually
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=lifecycle
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=tools_api
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=resources_api
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=transport
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=error_codes

# Expected: All 63 tests pass
```

**Step 4: Verify no process leaks**
```bash
# Run tests and check process count
erl -pa _build/test/lib/*/ebin <<EOF
BaselineProcesses = length(processes()),
eunit:test(erlmcp_connection_limiter_tests),
AfterProcesses = length(processes()),
case AfterProcesses - BaselineProcesses of
    N when N =< 5 -> io:format("OK: ~p process delta~n", [N]);
    N -> io:format("LEAK: ~p extra processes~n", [N])
end,
halt().
EOF

# Expected: Process delta <= 5 (some system processes may start)
```

**Step 5: Verify no gproc leaks**
```bash
# Check gproc registrations
erl -pa _build/test/lib/*/ebin <<EOF
application:ensure_all_started(gproc),
eunit:test(erlmcp_connection_limiter_tests),
GprocCount = gproc:where({c, l, erlmcp_connection_count}),
case GprocCount of
    undefined -> io:format("OK: No lingering gproc registrations~n");
    _ -> io:format("LEAK: gproc counter still registered~n")
end,
halt().
EOF

# Expected: No lingering gproc registrations
```

**Step 6: Run full test suite**
```bash
# Run all EUnit tests
rebar3 eunit

# Run all Common Test suites
rebar3 ct

# Expected: ≥95% pass rate (EUnit), ≥90% pass rate (CT)
```

**Step 7: Verify quality gates**
```bash
# Run blocking quality gates
make validate-compile
make validate-test

# Expected: Both exit with code 0
```

### 7.2 Expected Output

**Successful connection_limiter test run:**
```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_core
===> Performing EUnit tests...

erlmcp_connection_limiter_tests: module_lifecycle_test_ (test_start_stop)...[0.102s] ok
erlmcp_connection_limiter_tests: module_lifecycle_test_ (test_multiple_starts)...[0.089s] ok
erlmcp_connection_limiter_tests: module_lifecycle_test_ (test_get_stats)...[0.076s] ok
erlmcp_connection_limiter_tests: connection_limit_enforcement_test_ (test_accept_single_connection)...[0.045s] ok
erlmcp_connection_limiter_tests: connection_limit_enforcement_test_ (test_accept_multiple_connections)...[0.123s] ok
...
[All 50 tests pass]
...

Finished in 3.456 seconds
50 tests, 0 failures
```

**Successful spec compliance test run:**
```
Common Test starting (cwd is /home/user/erlmcp)

=== Logging to: _build/test/logs/ct_run.erlmcp@...

Testing erlmcp.erlmcp_spec_compliance_SUITE:
  lifecycle group:
    init_server_capabilities........................PASSED
    connect_client_to_server........................PASSED
    authenticate_client.............................PASSED
    establish_session...............................PASSED
    ...
  tools_api group:
    list_tools_empty................................PASSED
    list_tools_with_descriptions....................PASSED
    call_tool_success...............................PASSED
    ...
  [All groups pass]

All 63 test cases passed.
```

**Successful quality gate:**
```
🔨 Quality Gate: Compilation
  Target: 0 compilation errors
  Action: Compiling all apps with TERM=dumb...

✅ Compilation passed - 0 errors

🧪 Quality Gate: Tests
  Target: 0 test failures (EUnit + CT)
  Action: Running EUnit + CT...

  Running EUnit...
  ✅ EUnit: 84/84 tests passed (100%)

  Running Common Test...
  ✅ CT: 63/63 tests passed (100%)

✅ Tests passed - 0 failures (EUnit + CT)
```

### 7.3 How to Verify Each Fix

**Fix #1: Process registration leaks**
```bash
# Before fix: Should see registration errors
rebar3 eunit --module=erlmcp_connection_limiter_tests 2>&1 | grep -i "already_started\|badarg\|registration"

# After fix: No errors
rebar3 eunit --module=erlmcp_connection_limiter_tests 2>&1 | grep -i "already_started\|badarg\|registration"
# (Empty output expected)
```

**Fix #2: API signature mismatches**
```bash
# Before fix: Should see function_clause errors
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=tools_api 2>&1 | grep -i "function_clause"

# After fix: No function_clause errors
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE --group=tools_api 2>&1 | grep -i "function_clause"
# (Empty output expected)
```

**Fix #3: Async timing issues**
```bash
# Before fix: Intermittent failures under load
stress-ng --cpu 4 --timeout 30s &
for i in {1..5}; do
    rebar3 eunit --module=erlmcp_connection_limiter_tests || echo "FAILED run $i"
done
kill $(jobs -p)

# After fix: Consistent results under load (5/5 pass)
```

**Fix #4: Test isolation failures**
```bash
# Before fix: Tests fail in random order
rebar3 eunit --module=erlmcp_connection_limiter_tests --order random
# (Some failures expected)

# After fix: Tests pass in random order
for i in {1..5}; do
    rebar3 eunit --module=erlmcp_connection_limiter_tests --order random || echo "FAILED"
done
# (All 5 runs should pass)
```

### 7.4 Success Criteria for Each Change

| Fix | Success Criteria | Verification Command |
|-----|------------------|----------------------|
| **Issue #1: Process leaks** | No gproc registration errors, 10/10 test runs pass | `for i in {1..10}; do rebar3 eunit --module=erlmcp_connection_limiter_tests || exit 1; done` |
| **Issue #2: API mismatches** | 0 function_clause errors, all 63 spec tests pass | `rebar3 ct --suite=erlmcp_spec_compliance_SUITE` |
| **Issue #3: Timing issues** | Tests pass under CPU load, <1% variance in run time | `stress-ng --cpu 4 & rebar3 eunit; kill $(jobs -p)` |
| **Issue #4: Isolation failures** | Tests pass in random order, no process leaks | `rebar3 eunit --order random` (5 runs) |

---

## 8. Risk Assessment

### 8.1 What Could Break

**Risk 1: Breaking working tests**
- **Probability:** Medium (30%)
- **Impact:** High (blocks development)
- **Description:** While fixing broken tests, we might accidentally break currently passing tests by changing shared fixtures or helper functions
- **Detection:** Run full test suite after each change
- **Mitigation:**
  - Make incremental changes (one test file at a time)
  - Run full test suite after each commit
  - Use git branches for each file fix
  - Keep rollback option ready

**Risk 2: Introducing new race conditions**
- **Probability:** Low (15%)
- **Impact:** Medium (intermittent failures)
- **Description:** New synchronization code might introduce deadlocks or new race conditions
- **Detection:** Run tests under load and in parallel
- **Mitigation:**
  - Use proven patterns (monitor/receive)
  - Add timeouts to all waits
  - Test under stress (CPU load, many iterations)

**Risk 3: Performance degradation**
- **Probability:** Low (10%)
- **Impact:** Low (slower tests)
- **Description:** Replacing timer:sleep with polling might make tests slower
- **Detection:** Measure test suite runtime before/after
- **Mitigation:**
  - Use short polling intervals (10ms)
  - Set reasonable timeouts (1000ms max)
  - Benchmark test suite performance

**Risk 4: Incomplete cleanup**
- **Probability:** Medium (25%)
- **Impact:** Medium (test pollution)
- **Description:** New cleanup code might miss some resources (ETS tables, monitors, etc.)
- **Detection:** Check for lingering resources after tests
- **Mitigation:**
  - Comprehensive cleanup checklist
  - Add verification helpers
  - Monitor system resources

**Risk 5: API documentation mismatch**
- **Probability:** Low (10%)
- **Impact:** Low (documentation confusion)
- **Description:** Fixing API calls might reveal that documentation is also wrong
- **Detection:** Review function specs and doc strings
- **Mitigation:**
  - Update docs along with code
  - Add doc tests to verify examples

### 8.2 How to Mitigate

**Mitigation Strategy 1: Incremental Changes**
```bash
# Create feature branch
git checkout -b fix/phase-1a-test-infrastructure

# Fix one file at a time
git checkout -b fix/connection-limiter-tests
# Make changes to connection_limiter_tests.erl
rebar3 eunit --module=erlmcp_connection_limiter_tests
git commit -m "Fix: connection_limiter_tests process leaks"

# Fix next file
git checkout -b fix/spec-compliance-tests
# Make changes to spec_compliance_SUITE.erl
rebar3 ct --suite=erlmcp_spec_compliance_SUITE
git commit -m "Fix: spec_compliance API signature mismatches"

# Merge incrementally
git checkout fix/phase-1a-test-infrastructure
git merge fix/connection-limiter-tests
git merge fix/spec-compliance-tests
```

**Mitigation Strategy 2: Parallel Test Runs**
```bash
# Before each commit, verify tests still pass
make validate-test

# Run tests in parallel to catch race conditions
rebar3 eunit --cover --verbose
rebar3 ct --cover --verbose

# Check for degradation
./tools/benchmark-test-suite.sh
```

**Mitigation Strategy 3: Canary Testing**
```bash
# Test critical paths first
rebar3 eunit --module=erlmcp_json_rpc_tests  # Core functionality
rebar3 eunit --module=erlmcp_server_tests    # Server tests
rebar3 eunit --module=erlmcp_client_tests    # Client tests

# If canaries pass, proceed with full suite
rebar3 eunit
rebar3 ct
```

**Mitigation Strategy 4: Monitoring & Alerts**
```bash
# Add monitoring to detect issues
./tools/tcps/andon_cord.sh watch &

# Run tests with monitoring
make validate-test

# Check andon board for alerts
make andon
```

### 8.3 Rollback Procedure if Needed

**If fixes introduce more problems than they solve:**

**Step 1: Identify failure point**
```bash
# Bisect to find breaking commit
git bisect start
git bisect bad HEAD
git bisect good <last-known-good-commit>

# Git will checkout commits for testing
rebar3 eunit
git bisect good/bad  # Mark each commit

# Once found:
git bisect reset
```

**Step 2: Rollback specific file**
```bash
# Rollback one file while keeping others
git checkout <last-good-commit> -- apps/erlmcp_core/test/erlmcp_connection_limiter_tests.erl
git commit -m "Rollback: connection_limiter_tests (introduced race conditions)"
```

**Step 3: Rollback entire phase**
```bash
# Revert all changes from phase
git revert <merge-commit-hash>
git push origin fix/phase-1a-test-infrastructure --force-with-lease
```

**Step 4: Communicate rollback**
```bash
# Update PR with rollback reason
# Create new issue for re-attempt
# Document what went wrong in postmortem
```

**Rollback Decision Criteria:**
- Test pass rate drops below 70% (from baseline 30-40%)
- New failures appear in previously passing tests
- CI/CD blocks all merges for >4 hours
- Team consensus that fixes are too risky

**Note:** Rollback is a **last resort**. Prefer fixing forward with incremental patches.

---

## 9. Timeline & Resources

### 9.1 Task Breakdown with Hours

| Phase | Task | Owner | Hours | Dependencies |
|-------|------|-------|-------|--------------|
| **Phase 1: Helpers** | | | **5h** | |
| 1.1 | Create erlmcp_test_helpers module | Dev A | 2h | None |
| 1.2 | Write synchronization helpers | Dev A | 2h | 1.1 |
| 1.3 | Test helpers module | Dev A | 1h | 1.2 |
| **Phase 2: Connection Limiter** | | | **8.5h** | |
| 2.1 | Rewrite setup/cleanup functions | Dev A | 2h | Phase 1 |
| 2.2 | Fix test_multiple_starts | Dev A | 1h | 2.1 |
| 2.3 | Remove redundant restarts (12 tests) | Dev A | 2h | 2.1 |
| 2.4 | Create custom config fixtures | Dev A | 2h | 2.3 |
| 2.5 | Replace timer:sleep (20 occurrences) | Dev A | 1h | Phase 1 |
| 2.6 | Verify connection_limiter tests | Dev A | 0.5h | 2.5 |
| **Phase 3: Spec Compliance** | | | **8h** | |
| 3.1 | Audit API signatures (63 tests) | Dev B | 3h | Phase 1 |
| 3.2 | Fix API signature mismatches | Dev B | 2h | 3.1 |
| 3.3 | Add robust init/end_per_testcase | Dev B | 2h | Phase 1 |
| 3.4 | Replace timer:sleep (40 occurrences) | Dev B | 1h | Phase 1 |
| **Phase 4: Audit Other Tests** | | | **2h** | |
| 4.1 | Grep all test files for issues | Dev C | 1h | None |
| 4.2 | Prioritize by severity | Dev C | 1h | 4.1 |
| **Phase 5: Fix Other Tests** | | | **4h** | |
| 5.1 | Fix top 5 critical test files | Dev A/B/C | 3h | Phase 4 |
| 5.2 | Verify fixes | Dev A/B/C | 1h | 5.1 |
| **Phase 6: Verification** | | | **4h** | |
| 6.1 | Run full test suite (10 iterations) | All | 1h | All |
| 6.2 | Verify under load | All | 1h | All |
| 6.3 | Check for leaks | All | 1h | All |
| 6.4 | Final quality gate validation | All | 1h | All |
| **TOTAL** | | | **31.5h** | |

### 9.2 Dependencies Between Fixes

**Dependency Graph:**
```
Phase 1 (Helpers)
    ↓
    ├──> Phase 2 (Connection Limiter) ──┐
    ├──> Phase 3 (Spec Compliance) ─────┤──> Phase 6 (Verification)
    └──> Phase 5 (Other Tests) ─────────┘
            ↑
    Phase 4 (Audit)
```

**Critical Path:** Phase 1 → Phase 2 → Phase 6 (17.5 hours minimum)

**Parallelizable Work:**
- Phase 3 can start after Phase 1 (can run parallel with Phase 2)
- Phase 4 can start immediately (no dependencies)
- Phase 5 depends on Phase 4 but can overlap with Phase 2/3

### 9.3 Recommended Team Assignment

**Scenario A: Solo Developer**
```
Day 1 (8h):
  ✓ Phase 1: Helpers (5h)
  ✓ Phase 2: Start connection_limiter (3h)

Day 2 (8h):
  ✓ Phase 2: Finish connection_limiter (5.5h)
  ✓ Phase 3: Start spec_compliance (2.5h)

Day 3 (8h):
  ✓ Phase 3: Finish spec_compliance (5.5h)
  ✓ Phase 4: Audit other tests (2h)
  ✓ Phase 5: Start fixes (0.5h)

Day 4 (7.5h):
  ✓ Phase 5: Finish fixes (3.5h)
  ✓ Phase 6: Verification (4h)

Total: 3.5 working days (31.5 hours)
```

**Scenario B: Team of 2**
```
Developer A:
  Day 1: Phase 1 (5h) + Phase 2 start (3h)
  Day 2: Phase 2 finish (5.5h) + Phase 4 (2h)
  Day 3: Phase 5 (4h) + Phase 6 part 1 (2h)
  Total: 21.5 hours

Developer B:
  Day 1: Wait for Phase 1 (5h), then Phase 3 start (3h)
  Day 2: Phase 3 finish (5h) + Phase 6 prep (2h)
  Day 3: Phase 6 part 2 (2h)
  Total: 12 hours

Team Total: 2.5 working days (with parallelization)
```

**Scenario C: Team of 3**
```
Developer A:
  Day 1-2: Phase 1 (5h) + Phase 2 (8.5h)
  Day 3: Phase 6 collaboration (4h)
  Total: 17.5 hours

Developer B:
  Day 1-2: Phase 3 (8h, starts after Phase 1)
  Day 3: Phase 6 collaboration (4h)
  Total: 12 hours

Developer C:
  Day 1: Phase 4 (2h, parallel with others)
  Day 2: Phase 5 (4h)
  Day 3: Phase 6 collaboration (4h)
  Total: 10 hours

Team Total: 2 working days (with full parallelization)
```

**Recommended:** Team of 2 (optimal parallelization without coordination overhead)

---

## 10. Post-Implementation Actions

### 10.1 Verify Phase 1a Success

**Checklist:**
- [ ] All quality gates pass
  - [ ] `make validate-compile` exits 0
  - [ ] `make validate-test` exits 0
  - [ ] EUnit pass rate ≥95%
  - [ ] CT pass rate ≥90%
  - [ ] No intermittent failures (10 consecutive runs pass)

- [ ] Test infrastructure is robust
  - [ ] Tests pass in random order
  - [ ] Tests pass under CPU load
  - [ ] No process leaks after tests
  - [ ] No gproc registration leaks
  - [ ] Test suite completes in <10 minutes

- [ ] Documentation updated
  - [ ] CHANGELOG.md entry for Phase 1a
  - [ ] Test README updated with new patterns
  - [ ] API documentation verified correct

- [ ] CI/CD unblocked
  - [ ] Pre-commit hooks pass
  - [ ] GitHub Actions workflows pass
  - [ ] PRs can merge

### 10.2 Unblock Phase 1b (Coverage)

**Once Phase 1a completes:**
1. Baseline coverage measurement
   ```bash
   rebar3 cover
   # Record: X% coverage baseline
   ```

2. Identify coverage gaps
   ```bash
   # Generate HTML coverage report
   open _build/test/cover/index.html
   # Identify modules <80% coverage
   ```

3. Plan Phase 1b: Coverage Improvement
   - Target: 80% overall coverage
   - Focus: Core protocol modules (JSON-RPC, server, client)
   - Approach: Add tests for uncovered branches

### 10.3 Enable Quality Gates in CI/CD

**Update `.github/workflows/ci.yml`:**
```yaml
- name: Quality Gate - Tests
  run: make validate-test

- name: Quality Gate - Coverage
  run: make validate-coverage

- name: Block on Failure
  if: failure()
  run: |
    echo "❌ QUALITY GATE FAILED - BLOCKING MERGE"
    exit 1
```

**Enable required status checks:**
1. Go to GitHub → Settings → Branches
2. Add rule for main/master
3. Require status checks: `validate-test`, `validate-coverage`
4. Require branches to be up to date

### 10.4 Celebrate Milestone

**Phase 1a completion is a MAJOR milestone:**
- 🏭 自働化 (Jidoka) - Quality is now built-in
- 🚦 行灯 (Andon) - Andon cord can be CLEARED
- ✅ ポカヨケ (Poka-yoke) - Error-proofing works
- 🎉 First quality gate UNBLOCKED

**Team recognition:**
- Share success in team meeting
- Update project status board
- Record metrics improvement
- Document lessons learned

---

## Appendix A: Reference Commands

```bash
# Compilation
TERM=dumb rebar3 compile

# EUnit Tests
rebar3 eunit
rebar3 eunit --module=MODULE_NAME
rebar3 eunit --module=MODULE_NAME --verbose

# Common Test
rebar3 ct
rebar3 ct --suite=apps/APP/test/SUITE_NAME
rebar3 ct --suite=SUITE --group=GROUP_NAME

# Coverage
rebar3 cover
open _build/test/cover/index.html

# Quality Gates
make validate-compile
make validate-test
make validate-coverage
make validate

# Verification
for i in {1..10}; do rebar3 eunit || exit 1; done
rebar3 eunit --order random
stress-ng --cpu 4 & rebar3 eunit; kill $(jobs -p)

# Process Debugging
erl -pa _build/test/lib/*/ebin
> length(processes()).
> [P || P <- processes(), {registered_name, erlmcp_connection_limiter} <- [process_info(P)]].

# gproc Debugging
> application:ensure_all_started(gproc).
> gproc:where({c, l, erlmcp_connection_count}).
```

---

**END OF PHASE 1a IMPLEMENTATION PLAN**

**Next Steps:**
1. Review this plan with team
2. Assign tasks to developers
3. Create GitHub issues for each phase
4. Begin Phase 1: Helpers implementation
5. Track progress in project board

**Success Metrics:**
- EUnit pass rate: 30% → 95% (Target: 95%+)
- CT pass rate: 50% → 90% (Target: 90%+)
- Quality gates: 0/5 passing → 2/5 passing (compile, test)
- Test consistency: Intermittent → 100% reliable
- Development velocity: BLOCKED → UNBLOCKED

**Owner:** Erlang Architect
**Reviewers:** erlang-otp-developer, erlang-test-engineer
**Status:** Ready for Implementation
**Priority:** CRITICAL
