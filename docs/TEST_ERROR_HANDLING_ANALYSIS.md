# Error Handling Analysis: Broken Test Files

**Date:** 2026-01-30
**Status:** Analysis Complete
**Decision:** REWRITE required for most tests

## Executive Summary

After analyzing 29 broken test files, I've identified **4 categories** of error handling issues:

1. **Category A: Minor Fixes (5 files)** - Can be fixed with small updates
2. **Category B: Moderate Fixes (8 files)** - Need rewrite of error assertions
3. **Category C: Major Rewrites (12 files)** - Test patterns invalid, need complete rewrite
4. **Category D: Unsalvageable (4 files)** - Tests rely on non-existent modules/concepts

## Root Cause Analysis

### 1. Missing/Changed Constants

**Problem:** Tests reference constants that don't exist in `erlmcp.hrl`

**Examples:**
- `?VALID_ERROR_CODES` - Never defined
- `?JSONRPC_MSG_*` - Message constants not defined (only error codes)
- `?MCP_MSG_*` - Only a few message constants exist
- `?ID_WARNING_THRESHOLD`, `?ID_CRITICAL_THRESHOLD` - Not defined
- `?MCP_FIELD_*` - Some field constants missing

**Impact:** 20+ test files fail compilation

**Fix Strategy:**
```erlang
%% Option 1: Add missing constants to erlmcp.hrl
-define(VALID_ERROR_CODES, [
    -32700, -32600, -32601, -32602, -32603,  %% JSON-RPC
    -32001, -32002, -32003, -32004, -32005,    %% MCP
    -32007, -32011, -32012                     %% MCP extensions
]).

%% Option 2: Remove constants from tests, hardcode values
?assertEqual(-32700, maps:get(<<"code">>, Error)).  %% Instead of ?JSONRPC_PARSE_ERROR
```

### 2. Inconsistent Error Return Formats

**Problem:** Tests expect specific error tuples that don't match implementation

**Example from `erlmcp_error_handling_tests.erl.broken`:**

```erlang
%% Test expects:
{error, {parse_error, _Reason}} = erlmcp_json_rpc:decode_message(InvalidJson),

%% But implementation returns:
{error, {parse_error, Details}} where Details is a map()
```

**Impact:** 15+ test files have mismatched error assertions

**Fix Strategy:**
```erlang
%% Rewrite tests to match actual error format
{error, {parse_error, _DetailsMap}} = erlmcp_json_rpc:decode_message(InvalidJson),
```

### 3. Missing Test Helper Modules

**Problem:** Tests rely on `test_client`, `test_cleanup_handler` modules that don't exist

**Example from `erlmcp_session_lifecycle_tests.erl.broken`:**

```erlang
{ok, ClientPid} = test_client:start_link(),  %% Module doesn't exist
```

**Impact:** 8+ test files can't compile

**Fix Strategy:**
```erlang
%% Option 1: Create test helper modules
-module(test_client).
-export([start_link/0, stop/1]).
start_link() -> {ok, spawn(fun() -> receive stop -> ok end end)}.
stop(Pid) -> Pid ! stop.

%% Option 2: Use real erlmcp_client in tests
{ok, ClientPid} = erlmcp_client:start_link({stdio, []}),
```

### 4. Incorrect API Usage Patterns

**Problem:** Tests call APIs with wrong parameters or expect non-existent functions

**Examples:**

```erlang
%% From erlmcp_cancellation_tests.erl.broken:
%% Test expects:
Token = erlmcp_cancellation:register(ClientPid, OperationPid),

%% But actual API might be:
{ok, Token} = erlmcp_cancellation:register(OperationPid, ClientPid),
```

**Impact:** 10+ test files have incorrect API calls

**Fix Strategy:**
- Check actual API in implementation files
- Rewrite tests to match actual function signatures
- Use compile-time dialyzer to catch type errors

## Category-by-Category Breakdown

### Category A: Minor Fixes (5 files)

These tests can be fixed with small edits to error constants and assertions:

1. **`erlmcp_error_handling_tests.erl.broken`**
   - **Issues:**
     - Missing `?VALID_ERROR_CODES` constant
     - Missing `?JSONRPC_MSG_*` message constants
     - Missing `?MCP_MSG_*` message constants
   - **Fix:** Add missing constants to `erlmcp.hrl` or hardcode values
   - **Estimated effort:** 30 minutes

2. **`mcp_compliance_SUITE.erl.broken`**
   - **Issues:**
     - Uses `is_record` which is deprecated (use `erlang:is_record/2`)
     - Some tests just print `ct:pal("✓ ...")` without assertions
   - **Fix:** Replace `is_record` with proper pattern matching, add real assertions
   - **Estimated effort:** 1 hour

3. **`erlmcp_jsonrpc_compliance_tests.erl.broken`**
   - **Issues:**
     - Similar to mcp_compliance_SUITE
   - **Fix:** Same pattern
   - **Estimated effort:** 1 hour

4. **`erlmcp_request_id_tests.erl.broken`**
   - **Issues:**
     - Missing `?ID_WARNING_THRESHOLD` constants
     - `get_usage_percentage/1` might not exist
   - **Fix:** Add constants or rewrite tests without them
   - **Estimated effort:** 45 minutes

5. **`erlmcp_message_parser_tests.erl.broken`**
   - **Issues:**
     - Tests parse error details format
   - **Fix:** Update error tuple matching
   - **Estimated effort:** 30 minutes

**Total Category A effort:** ~4 hours

### Category B: Moderate Fixes (8 files)

These tests need rewritten error handling assertions but test logic is sound:

1. **`erlmcp_session_lifecycle_tests.erl.broken`**
   - **Issues:**
     - Uses `test_client` helper module that doesn't exist
     - Sends messages directly to server via `!` without proper transport
     - No verification of server responses
   - **Fix:**
     - Use real `erlmcp_client` instead of `test_client`
     - Use proper transport layer for messaging
     - Add response verification via gen_server:call or message patterns
   - **Estimated effort:** 3 hours

2. **`erlmcp_cancellation_tests.erl.broken`**
   - **Issues:**
     - Uses `test_cleanup_handler` module that doesn't exist
     - Test defines module inside test file (invalid Erlang syntax)
     - API call signatures might be wrong
   - **Fix:**
     - Create `test_cleanup_handler` as separate module
     - Verify actual `erlmcp_cancellation` API
     - Update test to match actual API
   - **Estimated effort:** 3 hours

3. **`erlmcp_resources_capability_tests.erl.broken`**
   - **Issues:**
     - Tests resource operations without proper setup
     - Error assertions likely mismatched
   - **Fix:** Update error handling, use real server/client
   - **Estimated effort:** 2 hours

4. **`erlmcp_tools_capability_tests.erl.broken`**
   - **Issues:** Similar to resources tests
   - **Fix:** Similar approach
   - **Estimated effort:** 2 hours

5. **`mcp_prompts_capability_SUITE.erl.broken`**
   - **Issues:** Similar pattern
   - **Fix:** Similar approach
   - **Estimated effort:** 2 hours

6. **`erlmcp_server_capabilities_SUITE.erl.broken`**
   - **Issues:** Similar pattern
   - **Fix:** Similar approach
   - **Estimated effort:** 2 hours

7. **`erlmcp_capability_test_SUITE.erl.broken`**
   - **Issues:** Similar pattern
   - **Fix:** Similar approach
   - **Estimated effort:** 2 hours

8. **`mcp_tools_SUITE.erl.broken`**
   - **Issues:** Similar pattern
   - **Fix:** Similar approach
   - **Estimated effort:** 2 hours

**Total Category B effort:** ~18 hours

### Category C: Major Rewrites (12 files)

These tests use patterns that don't work with current architecture:

1. **`json_rpc_demo_test.erl.broken`**
   - **Issues:**
     - Demo/test hybrid unclear purpose
     - Probably integration test but structure is wrong
   - **Fix:** Rewrite as proper Common Test suite or delete
   - **Estimated effort:** 4 hours

2. **`mcp_client_server_SUITE.erl.broken`**
   - **Issues:**
     - Tests client-server interaction but lacks proper setup
     - No transport layer
   - **Fix:** Rewrite with real stdio/tcp transport
   - **Estimated effort:** 4 hours

3. **`mcp_json_rpc_SUITE.erl.broken`**
   - **Issues:**
     - Tests JSON-RPC but without proper message encoding
   - **Fix:** Rewrite using `erlmcp_json_rpc` API
   - **Estimated effort:** 3 hours

4. **`mcp_resources_SUITE.erl.broken`**
   - **Issues:**
     - Tests resources without real resource implementations
   - **Fix:** Add test resources, rewrite tests
   - **Estimated effort:** 3 hours

5. **`erlmcp_state_migration_tests.erl.broken`**
   - **Issues:**
     - Tests state migration but module doesn't exist (`erlmcp_state_migration.erl.broken`)
   - **Fix:** Depends on whether state migration is implemented
   - **Estimated effort:** 5 hours (if migration exists) or delete

6. **`erlmcp_connection_limiter_tests.erl.broken`**
   - **Issues:**
     - Tests rate limiting but unclear what's being tested
   - **Fix:** Rewrite with actual rate limiter tests
   - **Estimated effort:** 3 hours

7. **`erlmcp_progress_tests.erl.broken`**
   - **Issues:**
     - Tests progress token support but no implementation
   - **Fix:** Implement progress tracking first, then test
   - **Estimated effort:** 4 hours

8. **`erlmcp_code_reload_tests.erl.broken`**
   - **Issues:**
     - Tests code reload but uses wrong OTP patterns
   - **Fix:** Rewrite with proper `sys:change_code` and `handle_code_change`
   - **Estimated effort:** 3 hours

9. **`erlmcp_client_request_id_overflow_tests.erl.broken`**
   - **Issues:**
     - Tests request ID overflow but wrong module name
     - Should be in `erlmcp_request_id_tests`
   - **Fix:** Merge into existing tests
   - **Estimated effort:** 1 hour

10. **`erlmcp_json_rpc_proper_tests.erl.broken`**
    - **Issues:**
      - Property tests but wrong generators
    - **Fix:** Rewrite with proper Proper generators
    - **Estimated effort:** 3 hours

11. **`quality_gates_SUITE.erl.broken`**
    - **Issues:**
      - Tests quality gates but unclear what gates exist
    - **Fix:** Define quality gates, then test
    - **Estimated effort:** 3 hours

12. **`hooks_integration_SUITE.erl.broken`**
    - **Issues:**
      - Tests hooks but no hook system exists
    - **Fix:** Implement hooks or delete tests
    - **Estimated effort:** 5 hours (if hooks exist) or delete

**Total Category C effort:** ~41 hours

### Category D: Unsalvageable (4 files)

These tests rely on non-existent concepts or are completely outdated:

1. **`regression_detection_SUITE.erl.broken`**
   - **Issues:**
     - Tests "regression detection" but no such system exists
     - Unclear what it's supposed to test
   - **Recommendation:** DELETE
   - **Reason:** No implementation to test

2. **`auto_fix_SUITE.erl.broken`**
   - **Issues:**
     - Tests "auto fix" feature that doesn't exist
     - Likely speculative tests for future feature
   - **Recommendation:** DELETE or move to examples/
   - **Reason:** No implementation to test

3. **`extra_test.erl.broken`**
   - **Issues:**
     - Filename indicates "extra" or experimental
     - Probably scratchpad tests
   - **Recommendation:** DELETE
   - **Reason:** No clear purpose

4. **`erlmcp_sampling_manual_tests.erl.broken`**
   - **Issues:**
     - "Manual tests" - not automated
     - Sampling capability not fully implemented
   - **Recommendation:** Move to `examples/sampling_tests.md` as documentation
   - **Reason:** Manual tests don't belong in automated suite

**Total Category D effort:** DELETE (0 hours) or move to examples

## Recommendations

### Immediate Actions (High Value, Low Effort)

1. **Fix Category A tests** (4 hours)
   - These are closest to working
   - Add missing constants to `erlmcp.hrl`
   - Update error assertions to match implementation

2. **Delete Category D tests** (0 hours)
   - Remove unsalvageable tests
   - Reduces clutter and confusion

### Medium-Term Actions (Medium Value, Medium Effort)

3. **Fix Category B tests** (18 hours)
   - Prioritize tests for features that exist and work
   - Rewrite error handling assertions
   - Create test helper modules if needed

4. **Audit Category C tests** (variable)
   - Determine which features are actually implemented
   - Delete tests for unimplemented features
   - Rewrite tests for implemented features

### Long-Term Actions (High Value, High Effort)

5. **Improve error handling consistency**
   - Standardize error return formats across all modules
   - Document error patterns in `docs/ERROR_HANDLING.md`
   - Add error handling examples to examples/

6. **Add property-based tests**
   - Use Proper for error handling invariants
   - Test error code generation, validation
   - Test error response encoding/decoding

## Error Handling Best Practices

### For Test Writers

```erlang
%% DO: Test specific error codes
test_method_not_found_error() ->
    ErrorBin = erlmcp_json_rpc:error_method_not_found(1, <<"unknown">>),
    {ok, Response} = erlmcp_json_rpc:decode_message(ErrorBin),
    ?assertEqual(-32601, Response#json_rpc_response.error#mcp_error.code).

%% DON'T: Use undefined constants
test_undefined_constant() ->
    ?assertEqual(-32601, ?SOME_UNDEFINED_CONSTANT).  %% Won't compile

%% DO: Match error tuples with wildcards for details
test_parse_error() ->
    {error, {parse_error, _Details}} = erlmcp_json_rpc:decode_message(BadJson).

%% DON'T: Expect specific detail formats unless documented
test_parse_error_too_specific() ->
    {error, {parse_error, #{<<"line">> := 1}}} = ...  %% Brittle
```

### For Module Implementers

```erlang
%% DO: Export error construction functions
-export([
    error_method_not_found/2,
    error_invalid_params/2,
    ...
]).

%% DO: Document error formats
%% @doc Returns {error, {parse_error, DetailsMap}}
%% where DetailsMap contains #{<<"offset">> => integer(), ...}

%% DON'T: Change error formats without version bump
%% This breaks all existing tests!
```

## Implementation Checklist

### Phase 1: Quick Wins (1 week)

- [ ] Add missing constants to `apps/erlmcp_core/include/erlmcp.hrl`
- [ ] Fix Category A tests (5 files)
- [ ] Delete Category D tests (4 files)
- [ ] Run tests to verify fixes

### Phase 2: Moderate Fixes (2 weeks)

- [ ] Fix Category B tests (8 files)
- [ ] Create test helper modules (test_client.erl, etc.)
- [ ] Update error assertions to match implementation
- [ ] Document error handling patterns

### Phase 3: Major Rewrites (4 weeks)

- [ ] Audit Category C tests for implementation status
- [ ] Delete tests for unimplemented features
- [ ] Rewrite tests for implemented features
- [ ] Add integration tests with real transports

### Phase 4: Quality Improvements (ongoing)

- [ ] Add property-based tests for error handling
- [ ] Improve error documentation
- [ ] Standardize error formats across modules
- [ ] Add error handling examples

## Success Metrics

- [ ] All error tests pass (Category A + B)
- [ ] Test coverage >= 80% for error handling
- [ ] Zero undefined constants
- [ ] Error formats documented
- [ ] Property tests for critical error paths

## References

- **Current error codes:** `apps/erlmcp_core/include/erlmcp.hrl`
- **Error implementation:** `apps/erlmcp_core/src/erlmcp_json_rpc.erl`
- **Test examples:** `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Chicago School TDD:** `docs/otp-patterns.md`

---

**Document Version:** 1.0
**Last Updated:** 2026-01-30
**Author:** Error Handling Analysis (Erlang Test Engineer)
**Status:** Ready for Review
