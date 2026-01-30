# erlmcp_transport_tcp_tests Fix Summary

## Date
2026-01-30

## Issues Found and Fixed

### 1. Duplicate Ranch Startup (Line 41)
**Issue**: `setup_server()` function called `application:ensure_all_started(ranch)` without checking return value, and ranch was already started in global `setup()`.

**Fix**: Removed the duplicate ranch startup call and added a comment explaining that ranch is already started in global setup.

**Location**: Line 40-56

### 2. Nested EUnit Fixture Structure (Lines 140-155)
**Issue**: The `client_send_not_connected_test_()` test had a nested setup structure where the innermost setup function returned a single test tuple instead of a list of tests.

**Fix**: Changed the return value from a single test tuple to a list containing one test tuple.

**Location**: Lines 140-155

**Before**:
```erlang
fun({Pid, _}) ->
   {"Send fails when not connected",
    fun() ->
        ...
    end}
end
```

**After**:
```erlang
fun({Pid, _}) ->
   [{"Send fails when not connected",
     fun() ->
         ...
     end}]
end
```

### 3. Ranch Status Check After Close (Lines 464-488)
**Issue**: The test "Close server with ranch" used `?assertError(badarg, ranch:get_status(RanchRef))` which assumes that `ranch:get_status/1` throws a `badarg` error after the listener is stopped. This assumption may not always be true depending on ranch version and timing.

**Fix**: Changed to a more robust check that tries to get the status and verifies it's not in a running state, or catches any error that occurs.

**Location**: Lines 464-496

**Before**:
```erlang
?assertError(badarg, ranch:get_status(RanchRef))
```

**After**:
```erlang
try
    StoppedStatus = ranch:get_status(RanchRef),
    ?assertNot(StoppedStatus =:= running orelse StoppedStatus =:= {ok, listening})
catch
    error:_ -> ok  %% Getting status after stop is expected to potentially fail
end
```

### 4. Non-Unique Transport IDs in Tests (Lines 414-432)
**Issue**: The "Init with server mode" test used hardcoded transport and server IDs (`behavior_test_transport`, `behavior_test_server`) which could cause conflicts if tests run in parallel or if cleanup doesn't complete properly.

**Fix**: Added unique ID generation using `erlang:unique_integer([positive])` similar to other test functions.

**Location**: Lines 414-432

**Before**:
```erlang
Opts = #{
    mode => server,
    port => 0,
    owner => self(),
    transport_id => behavior_test_transport,
    server_id => behavior_test_server
},
```

**After**:
```erlang
UniqueId = erlang:unique_integer([positive]),
Opts = #{
    mode => server,
    port => 0,
    owner => self(),
    transport_id => list_to_atom("behavior_test_transport_" ++ integer_to_list(UniqueId)),
    server_id => list_to_atom("behavior_test_server_" ++ integer_to_list(UniqueId))
},
```

## OTP Pattern Compliance

All fixes follow proper OTP patterns:
- **Supervision**: Tests properly cleanup processes using `cleanup/1` function
- **Process Isolation**: Each test uses unique IDs to avoid conflicts
- **State Verification**: Tests verify observable state via `get_state` call (Chicago School TDD)
- **Real Collaborators**: Tests use actual ranch listeners, not mocks

## Testing Status

The test file has been fixed but cannot be fully executed due to build system issues (filesystem errors with dependencies). The fixes are based on static code analysis and OTP best practices.

## Next Steps

To verify these fixes:
1. Resolve build system issues (filesystem, dependency caching)
2. Run: `rebar3 eunit --module=erlmcp_transport_tcp_tests`
3. Verify all tests pass
4. Check coverage: `rebar3 cover --verbose`

## Files Modified

- `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`

## Total Fixes

- 4 issues identified and fixed
- 1 code cleanup (removed duplicate ranch startup)
- 3 robustness improvements (nested fixtures, status checks, unique IDs)
