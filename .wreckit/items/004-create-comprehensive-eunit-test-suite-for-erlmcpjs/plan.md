# Create comprehensive EUnit test suite for erlmcp_json_rpc.erl Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Create a comprehensive EUnit test suite for `erlmcp_json_rpc.erl` to achieve ≥80% code coverage and ensure JSON-RPC 2.0 specification compliance. This module is the critical protocol codec that processes EVERY message entering and leaving the MCP system - a single bug breaks the entire protocol communication layer.

### Quality Gate Requirements

**ALL GATES MANDATORY - NO EXCEPTIONS:**
- **Compilation**: 0 errors (MANDATORY) - `TERM=dumb rebar3 compile`
- **EUnit**: 100% pass rate (MANDATORY) - `rebar3 eunit --module=erlmcp_json_rpc_tests`
- **Common Test**: N/A (using EUnit only)
- **Coverage**: ≥80% (MANDATORY) - `rebar3 as test cover --verbose`
- **Dialyzer**: 0 warnings (MANDATORY) - `rebar3 dialyzer`
- **Xref**: 0 undefined function calls (MANDATORY) - `rebar3 xref`
- **Performance**: N/A (codec not performance-critical compared to core_ops)

## Current State

### What Exists Now

**Modules:**
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl` (378 lines)
  - 31 exported API functions (lines 6-31)
  - 4 exported types (lines 33-39)
  - Key functions:
    - Encoding: `encode_request/3`, `encode_response/2`, `encode_error_response/3,4`, `encode_notification/2` (lines 45-88)
    - Decoding: `decode_message/1,2`, `decode_batch/1` (lines 90-137)
    - Batch operations: `encode_batch/1`, `is_batch_request/1` (lines 139-153)
    - Error helpers: 12 functions (lines 184-256)
    - Internal: `build_message_map/1`, `build_error_object/3` (lines 299-372)

- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` (476 lines)
  - 35 existing test cases (basic happy paths)
  - Test groups: encode_request (5), encode_response (5), error_response (9), notification (3), decode (6), batch (4), error_creation (8)
  - Tests are functional but incomplete for 80% coverage target

- `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` (523 lines)
  - JSON-RPC 2.0 constants (lines 8-65)
  - Error codes and messages (lines 68-106)
  - Protocol method definitions (lines 128-184)

**Tests:**
- Current coverage: **0%** (COVERAGE_REPORT.md:74) - CRITICAL GAP
- Current test count: **35 tests** (require ≥75 total to reach 80% coverage)
- Test file exists: `erlmcp_json_rpc_tests.erl` at `/Users/sac/erlmcp/apps/erlmcp_core/test/`
- Tests run with 100% pass rate (all 35 existing tests pass)

**Quality:**
- **Compilation**: ✅ Passes (no syntax errors)
- **EUnit**: ✅ 35 tests pass (insufficient coverage)
- **Coverage**: ❌ 0% (target: ≥80%, gap: -80 percentage points)
- **Dialyzer**: ✅ Type specs present (lines 33-39 in source)
- **Xref**: ✅ No undefined function calls

### What's Missing

**Gap Quantification:**
- **Coverage Gap**: 80 percentage points (0% → 80%)
- **Test Count Gap**: 40+ additional tests needed (35 existing → 75+ total)
- **Function Coverage Gap**: Many functions have partial or no coverage

**Root Cause Analysis (5 Whys):**
1. **Why 0% coverage?** Tests focus on happy paths, missing edge cases and error paths
2. **Why missing edge cases?** Tests only validate basic encoding/decoding, not protocol compliance
3. **Why no protocol compliance tests?** No systematic approach to JSON-RPC 2.0 spec coverage
4. **Why no systematic approach?** Tests added ad-hoc without coverage metric tracking
5. **ROOT CAUSE**: No comprehensive test coverage plan - tests verify basic functionality, not spec compliance

**Impact:**
- **BLOCKS ALL PRODUCTION WORK** - Priority: CRITICAL, Urgency: P0
- A bug in this module breaks the entire MCP protocol communication layer
- Processes EVERY message in and out of the system
- No safety net for protocol spec violations

### Key Discoveries from Research

1. **Finding 1: Module Structure** (erlmcp_json_rpc.erl:1-378)
   - Pure functional codec (no gen_server, no state)
   - 31 exported functions across 4 categories: encoding, decoding, batch, errors
   - All functions have type specs (lines 45-256)
   - Depends on `erlmcp_message_parser` for parsing logic (already tested with 44 tests)

2. **Finding 2: Existing Test Coverage** (erlmcp_json_rpc_tests.erl:1-476)
   - 35 tests covering basic happy paths
   - Tests use Chicago School TDD (real encoding/decoding, no mocks)
   - Reference pattern from `erlmcp_message_parser_tests.erl` (44 comprehensive tests)
   - Tests follow EUnit conventions with setup/cleanup

3. **Finding 3: JSON-RPC 2.0 Spec Requirements** (docs/protocol.md:1-150)
   - Required fields: `jsonrpc`, `id`, `method`/`result`/`error`
   - Error codes must be from valid range (defined in erlmcp.hrl:47-65)
   - Empty batch array is invalid (spec violation)
   - Field types: id can be null/string/number, params can be object/array

4. **Finding 4: Coverage Measurement**
   - Use `rebar3 as test cover --verbose` to generate coverage report
   - HTML report at `_build/test/cover/index.html` for detailed line-by-line analysis
   - Target: ≥80% line and function coverage

5. **Finding 5: Dependencies**
   - `jsx 3.1.0` for JSON encoding/decoding (lines 101, 121, 142, 297)
   - `erlmcp_message_parser` for parsing logic (already tested)
   - `erlmcp_message_size` for size validation (Gap #45 integration)

## Desired End State

### Specification

**Module**: `erlmcp_json_rpc_tests.erl` (expanded from 476 to ~1200 lines)

**Test Coverage Target**: ≥80% of `erlmcp_json_rpc.erl` (378 lines)

**Test Count Target**: ≥75 tests (currently 35, adding 40+)

**Test Categories:**

1. **Encoding Edge Cases** (15 new tests)
   - Request: null params, missing params field, unicode method/params
   - Response: null result, large result (1MB+), unicode result
   - Error: all 12 error helper functions with various data types
   - Notification: null params, unicode params
   - Batch: empty array (should fail), mixed message types

2. **Decoding Error Paths** (15 new tests)
   - Invalid JSON: missing `jsonrpc` field, wrong version, null id in request
   - Invalid types: wrong field types, nested structure errors
   - Size validation: messages at/over limit (Gap #45 integration)
   - Transport-specific: http, tcp, stdio limits

3. **Error Helper Functions** (36 new tests)
   - 12 error functions × 3 test cases = 36 tests
   - Test cases: basic call, with unicode data, edge case (null/empty)
   - Functions: `error_method_not_found/2`, `error_invalid_params/2`, `error_resource_not_found/2`, `error_tool_not_found/2`, `error_prompt_not_found/2`, `error_capability_not_supported/2`, `error_not_initialized/1`, `error_validation_failed/2`, `error_message_too_large/2`, `error_internal/1`, `error_parse/1`, `create_error/3`, `create_error_with_data/4`

4. **Edge Cases** (10+ new tests)
   - Unicode: emoji (😀), accented chars (é), multi-byte UTF-8
   - Large payloads: 1KB, 1MB, 16MB (configurable limit)
   - Special characters: JSON escaping, null bytes, control characters
   - Type coercion: string vs binary vs atom conversion

5. **Internal Function Coverage** (8+ new tests)
   - `build_message_map/1` - all 3 record types
   - `encode_id/1` - null, string, integer
   - `maybe_add_params/2` - undefined vs defined params
   - `add_result_or_error/3` - result vs error paths
   - `build_error_object/3` - all 4 data type branches (lines 339-372)

### Verification

**Automated Verification Commands:**

```bash
# 1. Compilation (0 errors, 0 warnings)
TERM=dumb rebar3 compile

# 2. EUnit tests (100% pass rate, ≥75 tests)
rebar3 eunit --module=erlmcp_json_rpc_tests --verbose

# 3. Coverage report (≥80% coverage)
rebar3 as test cover --verbose

# 4. Check specific module coverage
rebar3 as test cover --verbose | grep -A 5 erlmcp_json_rpc

# 5. View HTML coverage report (for detailed line analysis)
open _build/test/cover/index.html

# 6. Dialyzer (0 warnings)
rebar3 dialyzer

# 7. Xref (0 undefined function calls)
rebar3 xref

# 8. Run all tests (ensure no regressions)
rebar3 check
```

**Manual Verification:**

1. **JSON-RPC 2.0 Spec Compliance**:
   - All required fields present in encoded messages
   - Error codes from valid range (erlmcp.hrl:47-65)
   - Field types correct (id: null/string/number, params: object/array)
   - Empty batch returns error (not success)

2. **Edge Cases Handling**:
   - Unicode: Emoji, accented chars, multi-byte UTF-8 encode/decode correctly
   - Large payloads: 1MB+ messages handled without errors
   - Invalid input: Missing fields, wrong types, malformed JSON return proper errors

3. **Error Paths Coverage**:
   - All 12 error helpers generate valid JSON-RPC error objects
   - Invalid error codes converted to internal error (line 69-74)
   - Error messages are binary strings (not atoms or lists)

### Manufacturing Output

**Code Created:**
- `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` (expanded: 476 → ~1200 lines)
  - Add 40+ new test functions
  - Organize into 6 test groups (encoding_extended, decoding_error, error_helpers, edge_cases, internal_functions, batch_operations)
  - Add test generators for data-driven tests

**Tests Created:**
- 75+ total EUnit tests (35 existing + 40+ new)
  - 15 encoding edge case tests
  - 15 decoding error path tests
  - 36 error helper function tests
  - 10+ edge case tests
  - 8+ internal function tests

**Documentation Updated:**
- `docs/COVERAGE_REPORT.md` - Update `erlmcp_json_rpc` coverage from 0% to ≥80%
- `apps/erlmcp_core/test/README.md` (if exists) - Document test patterns

**Receipts Generated:**
- Coverage report: `_build/test/cover/index.html`
- Test output: `_build/test/logs/erlmcp_json_rpc_tests.*.log`

## What We're NOT Doing

**Explicitly OUT OF SCOPE (to prevent scope creep):**

1. **Modifying `erlmcp_json_rpc.erl` source code** - This is a test-only task. We are NOT changing the implementation, only adding tests.

2. **Adding new API functions** - NOT adding new features to the codec module. Only testing existing functions.

3. **Performance testing** - NOT measuring encoding/decoding performance. That's a separate task (Item #006 if it exists).

4. **Integration testing with transports** - NOT testing how the codec integrates with HTTP/TCP/stdio transports. That's covered in transport tests.

5. **Fuzz testing** - NOT using property-based testing (PropEr) for fuzz testing. That's a separate task.

6. **Modifying `erlmcp_message_parser.erl`** - NOT touching the parser module (already has 44 tests). Focus is on codec-level tests.

7. **Changing JSON-RPC 2.0 spec compliance** - NOT modifying the protocol implementation. Only verifying existing compliance through tests.

8. **Adding new error codes** - NOT extending the error code definitions. Only testing existing error helpers.

9. **Documentation of protocol** - NOT writing protocol documentation. Only verifying compliance with existing spec (docs/protocol.md).

10. **Refactoring for performance** - NOT optimizing hot paths. Only adding test coverage.

**Rationale**: This task is CRITICAL (P0) and BLOCKS PRODUCTION. Scope creep will delay delivery and increase risk. Focus exclusively on achieving ≥80% test coverage with 40+ additional tests.

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** ✅ - Requirements defined (see: Desired End State)
2. **Pseudocode** - Algorithm design BEFORE coding (see: Phase 1 pseudocode)
3. **Architecture** - Integration points and dependencies (see: Dependencies)
4. **Refinement** - Chicago School TDD (tests FIRST, then implementation if needed)
5. **Completion** - All quality gates passing (compilation, EUnit, coverage, Dialyzer, Xref)

### Implementation Strategy

**High-Level Approach: Incremental Test Development**

**Strategy Rationale:**
- **Why Chicago School TDD?** Tests must validate real behavior, not mocked behavior. JSON encoding/decoding has real edge cases (Unicode, large payloads) that mocks miss.
- **Why 6 phases?** Each phase targets a specific coverage gap. Phases are ordered by dependency (encoding first, then decoding, then integration).
- **Why 75+ tests?** 31 exported functions × 2-3 test cases per function = 62-93 tests. Adding 10+ edge case tests covers all paths.
- **Why not refactor first?** Refactoring changes the code and breaks coverage baseline. Add tests FIRST, then refactor in a separate task.

**Test Development Approach:**

```
Phase 1: Encoding Extended Tests (15 tests, +15% coverage)
  ↓
Phase 2: Decoding Error Path Tests (15 tests, +20% coverage)
  ↓
Phase 3: Error Helper Function Tests (36 tests, +25% coverage)
  ↓
Phase 4: Edge Case Tests (10+ tests, +15% coverage)
  ↓
Phase 5: Internal Function Tests (8+ tests, +5% coverage)
  ↓
Phase 6: Batch Operations Extended Tests (5 tests, +5% coverage)
  ↓
Total: 89 tests, estimated 85% coverage (exceeds 80% target)
```

**Quality Integration:**

- **Pre-commit Hooks**: Use `.claude/hooks/pre-task-validate.sh` to run `rebar3 compile` and `rebar3 eunit` before allowing commits
- **CI Gates**: All quality gates must pass in CI (compilation, EUnit, coverage ≥80%, Dialyzer, Xref)
- **Receipt Generation**: Coverage report saved as artifact in CI (`_build/test/cover/`)
- **Andon Signaling**: Test failures visible in CI logs. Coverage percentage reported in PR comments.

### Dependencies

**Internal Modules:**
- `erlmcp_json_rpc` - Module under test
- `erlmcp_message_parser` - Parsing logic (already tested, 44 tests)
- `erlmcp_message_size` - Size validation (Gap #45, optional for this task)
- `erlmcp.hrl` - Protocol constants and records

**External Libraries:**
- `jsx 3.1.0` - JSON encoding/decoding
- `eunit` - Testing framework (included in OTP)
- `logger` - Warning for invalid error codes (test for warnings)

**OTP Applications:**
- `kernel` - Core OTP
- `stdlib` - Standard library
- `compiler` - Test compilation

**Integration Points:**

1. **Message Size Validation** (erlmcp_message_size:validate_message_size/2)
   - Gap #45: Tests should verify size limits are enforced
   - Mock or use real validation with configured limits
   - Test: under limit, over limit, exact limit

2. **Parsing Logic** (erlmcp_message_parser:parse_json_rpc/1)
   - Already tested in erlmcp_message_parser_tests.erl (44 tests)
   - Focus on codec-level tests, not parser internals
   - Test: codec correctly delegates to parser

3. **JSON Encoding** (jsx:encode/1, jsx:decode/2)
   - Real encoding/decoding (no mocks)
   - Test: structure validation, not exact JSON strings
   - Test: Unicode, large payloads, special characters

---

## Phases

### Phase 1: Encoding Extended Tests

**Estimated Time**: 3-4 hours

#### Overview

Add 15 tests for encoding edge cases to increase coverage from 0% to ~15%. Focus on missing edge cases in request, response, error response, and notification encoding.

#### Specification

**WHAT we're building:**

- **Test Group**: `encode_request_extended_test_()`, `encode_response_extended_test_()`, `encode_error_extended_test_()`, `encode_notification_extended_test_()`
- **Test Count**: 15 new tests
- **File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Coverage Target**: Lines 45-88 (encoding functions)

**Test Functions to Add:**

1. **Request Encoding** (4 tests):
   - `test_encode_request_null_params()` - params = null
   - `test_encode_request_missing_params_field()` - params field absent
   - `test_encode_request_unicode_method()` - method with emoji (😀)
   - `test_encode_request_unicode_params()` - params with accented chars (é)

2. **Response Encoding** (4 tests):
   - `test_encode_response_null_result()` - result = null
   - `test_encode_response_large_result()` - result with 1MB data
   - `test_encode_response_unicode_result()` - result with multi-byte UTF-8
   - `test_encode_response_nested_result()` - deeply nested object

3. **Error Response Encoding** (4 tests):
   - `test_encode_error_response_null_data()` - data = null (line 345-350)
   - `test_encode_error_response_map_data()` - data = #{} (line 351-357)
   - `test_encode_error_response_binary_data()` - data = <<"...">> (line 358-364)
   - `test_encode_error_response_term_data()` - data = complex term (line 365-372)

4. **Notification Encoding** (3 tests):
   - `test_encode_notification_null_params()` - params = null
   - `test_encode_notification_unicode_params()` - params with emoji
   - `test_encode_notification_large_params()` - params with 1MB data

#### Pseudocode

```
For each encoding test:
  1. Create input with edge case (null, unicode, large data)
  2. Call encoding function
  3. Assert result is binary
  4. Decode result with jsx:decode/2
  5. Assert decoded structure matches expected
  6. Assert required fields present (jsonrpc, id, method/result/error)
  7. Assert field types correct
```

**Example Test Pseudocode:**

```erlang
test_encode_request_unicode_method() ->
    Id = 1,
    Method = <<"test/😀method">>,  % Emoji in method name
    Params = #{},
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    % Verify encoding
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0),
    % Verify structure
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).
```

#### Architecture

**INTEGRATION - No supervision tree (stateless module)**

```
Test Module (erlmcp_json_rpc_tests)
    ↓ calls
Module Under Test (erlmcp_json_rpc)
    ↓ uses
Dependencies:
    - jsx (JSON encoding)
    - erlmcp.hrl (constants)
    - erlmcp_message_parser (for decode tests, Phase 2)
```

**Process Pattern**: None (pure functional, no processes spawned)

**Test Pattern**:
- Chicago School TDD: Real encoding/decoding, no mocks
- Setup/cleanup: Minimal (no resources to manage)
- Assertion style: `?assertMatch` for pattern matching, `?assertEqual` for exact values

#### Changes Required:

##### 1. Add Encoding Extended Test Group

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Current**: Tests end at line 476 with basic happy paths
**Changes**: Add new test group after `encode_request_test_()` (line 24)

```erlang
%%====================================================================
%% Extended Encoding Tests - Edge Cases
%%====================================================================

encode_request_extended_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_request_null_params()),
             ?_test(test_encode_request_missing_params_field()),
             ?_test(test_encode_request_unicode_method()),
             ?_test(test_encode_request_unicode_params())
         ]
     end}.

test_encode_request_null_params() ->
    Id = 1,
    Method = <<"test/method">>,
    Params = null,  % Explicit null params
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    % Verify params field is present with null value
    ?assertEqual(null, maps:get(<<"params">>, Decoded)).

test_encode_request_missing_params_field() ->
    Id = 2,
    Method = <<"test/method2">>,
    % Use undefined to skip params field
    Result = erlmcp_json_rpc:encode_request(Id, Method, undefined),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    % Verify params field is NOT present
    ?assertNot(maps:is_key(<<"params">>, Decoded)).

test_encode_request_unicode_method() ->
    Id = 3,
    Method = <<"test/😀method">>,  % Emoji in method name
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    % Verify UTF-8 encoding
    ?assert(byte_size(Result) > 0).

test_encode_request_unicode_params() ->
    Id = 4,
    Method = <<"test/unicode">>,
    Params = #{
        <<"text">> => <<"Café résumé naïve">>,  % Accented chars
        <<"emoji">> => <<"😀🎉🚀">>  % Emoji
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).
```

**Reason**: These tests cover missing edge cases in encoding functions:
- Null params vs missing params field (lines 327-331: `maybe_add_params/2`)
- Unicode encoding (UTF-8 compatibility)
- Large payloads (tests for Phase 4)

##### 2. Add Response Encoding Extended Tests

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `encode_response_test_()` (line 90)

```erlang
encode_response_extended_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_response_null_result()),
             ?_test(test_encode_response_large_result()),
             ?_test(test_encode_response_unicode_result()),
             ?_test(test_encode_response_nested_result())
         ]
     end}.

test_encode_response_null_result() ->
    Id = 1,
    Result = null,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(null, maps:get(<<"result">>, Decoded)).

test_encode_response_large_result() ->
    Id = 2,
    % Create 1MB result
    LargeData = << <<0>> || _ <- lists:seq(1, 1024 * 1024) >>,
    Result = #{<<"data">> => LargeData},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 1024 * 1024).  % Verify size

test_encode_response_unicode_result() ->
    Id = 3,
    Result = #{<<"text">> => <<"Hello 世界 🌍">>},  % Multi-byte UTF-8
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    DecodedResult = maps:get(<<"result">>, Decoded),
    ?assertEqual(Result, DecodedResult).

test_encode_response_nested_result() ->
    Id = 4,
    Result = #{
        <<"level1">> => #{
            <<"level2">> => #{
                <<"level3">> => <<"deep">>
            }
        }
    },
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)).
```

**Reason**: Test result encoding edge cases:
- Null result (line 333-337: `add_result_or_error/3`)
- Large payloads (1MB data)
- Unicode (multi-byte UTF-8)
- Nested structures

##### 3. Add Error Response Encoding Extended Tests

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `error_response_test_()` (line 145)

```erlang
encode_error_response_extended_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_error_response_null_data()),
             ?_test(test_encode_error_response_map_data()),
             ?_test(test_encode_error_response_binary_data()),
             ?_test(test_encode_error_response_term_data())
         ]
     end}.

test_encode_error_response_null_data() ->
    Id = 1,
    Code = -32600,
    Message = <<"Invalid Request">>,
    Data = null,  % Explicit null (line 345-350)
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)),
    % Verify data field is NOT present (null is omitted)
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_encode_error_response_map_data() ->
    Id = 2,
    Code = -32602,
    Message = <<"Invalid Params">>,
    Data = #{<<"field">> => <<"name">>, <<"reason">> => <<"required">>},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

test_encode_error_response_binary_data() ->
    Id = 3,
    Code = -32001,
    Message = <<"Resource not found">>,
    Data = <<"Resource details">>,  % Binary data (line 358-364)
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ExpectedData = #{<<"details">> => Data},
    ?assertEqual(ExpectedData, maps:get(<<"data">>, Error)).

test_encode_error_response_term_data() ->
    Id = 4,
    Code = -32603,
    Message = <<"Internal error">>,
    Data = {complex, term, [with, list]},  % Complex term (line 365-372)
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)),
    DataMap = maps:get(<<"data">>, Error),
    ?assert(maps:is_key(<<"details">>, DataMap)),
    % Verify term was converted to binary
    ?assert(is_binary(maps:get(<<"details">>, DataMap))).
```

**Reason**: Test all 4 branches of `build_error_object/3` (lines 339-372):
- Null data (lines 345-350): data field omitted
- Map data (lines 351-357): data field included as-is
- Binary data (lines 358-364): wrapped in `#{<<"details">> => Data}`
- Other terms (lines 365-372): converted to binary and wrapped

##### 4. Add Notification Encoding Extended Tests

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `notification_test_()` (line 252)

```erlang
encode_notification_extended_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_notification_null_params()),
             ?_test(test_encode_notification_unicode_params()),
             ?_test(test_encode_notification_large_params())
         ]
     end}.

test_encode_notification_null_params() ->
    Method = <<"test/notify">>,
    Params = null,
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(null, maps:get(<<"params">>, Decoded)).

test_encode_notification_unicode_params() ->
    Method = <<"resource/changed">>,
    Params = #{
        <<"uri">> => <<"file:///path/to/café">>,  % Accented char
        <<"emoji">> => <<"😀">>
    },
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Params, maps:get(<<"params">>, Decoded)).

test_encode_notification_large_params() ->
    Method = <<"bulk/data">>,
    LargeData = << <<0>> || _ <- lists:seq(1, 1024 * 1024) >>,
    Params = #{<<"data">> => LargeData},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 1024 * 1024).
```

**Reason**: Test notification encoding edge cases (lines 315-320):
- Null params (test `maybe_add_params/2` behavior)
- Unicode params
- Large payloads

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):

- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests` - 100% pass rate (50/50 tests pass: 35 existing + 15 new)
- [ ] **Coverage**: `rebar3 as test cover --verbose` - ≥15% coverage (up from 0%)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:

- [ ] **Code review**: Tests follow Chicago School TDD (real encoding/decoding, no mocks)
- [ ] **Integration**: Tests work with existing test suite (no conflicts)
- [ ] **Edge cases**: Unicode encodes/decodes correctly (emoji, accented chars)
- [ ] **Large payloads**: 1MB data handled without errors

**Quality Gate Checkpoint**: Before proceeding to Phase 2, verify ALL automated gates pass. If ANY gate fails, STOP and fix.

---

### Phase 2: Decoding Error Path Tests

**Estimated Time**: 3-4 hours

#### Overview

Add 15 tests for decoding error paths to increase coverage from ~15% to ~35%. Focus on invalid JSON, missing fields, wrong types, and size validation errors.

#### Specification

**WHAT we're building:**

- **Test Group**: `decode_error_path_test_()`
- **Test Count**: 15 new tests
- **File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Coverage Target**: Lines 90-137 (decoding functions)

**Test Functions to Add:**

1. **Invalid JSON Structure** (5 tests):
   - `test_decode_request_missing_jsonrpc()` - no `jsonrpc` field
   - `test_decode_request_wrong_version()` - version = "1.0"
   - `test_decode_request_null_id()` - id = null in request (invalid)
   - `test_decode_request_string_id()` - id = "str-123" (valid)
   - `test_decode_response_missing_id()` - no `id` field in response

2. **Invalid Field Types** (4 tests):
   - `test_decode_request_unicode_params()` - params with multi-byte UTF-8
   - `test_decode_notification_with_id()` - notification has `id` field (invalid)
   - `test_decode_notification_missing_method()` - no `method` field
   - `test_decode_invalid_json_syntax()` - malformed JSON string

3. **Size Validation** (4 tests):
   - `test_decode_message_too_large_default()` - exceeds default limit
   - `test_decode_message_too_large_http()` - exceeds http transport limit
   - `test_decode_message_at_limit()` - exactly at size limit
   - `test_decode_message_under_limit()` - under size limit (valid)

4. **Batch Decoding** (2 tests):
   - `test_decode_batch_empty_array()` - empty batch (should fail per spec)
   - `test_decode_batch_invalid_item()` - batch with invalid item

#### Pseudocode

```
For each decoding error test:
  1. Create invalid JSON input (missing field, wrong type, malformed)
  2. Call decode_message/1 or decode_batch/1
  3. Assert result is {error, {Reason, Details}}
  4. Assert reason matches expected error type
  5. (Optional) Assert error details provide useful info
```

**Example Test Pseudocode:**

```erlang
test_decode_request_missing_jsonrpc() ->
    % Missing jsonrpc field (spec violation)
    Json = jsx:encode(#{
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {invalid_request, missing_jsonrpc}}, Result).
```

#### Architecture

**INTEGRATION - Decoding depends on message size validation**

```
Test Module (erlmcp_json_rpc_tests)
    ↓ calls
Module Under Test (erlmcp_json_rpc:decode_message/1,2)
    ↓ validates with
erlmcp_message_size:validate_message_size/2 (Gap #45)
    ↓ parses with
erlmcp_message_parser:parse_json_rpc/1
```

**Dependencies:**
- `erlmcp_message_size` - Mock or configure limits for size validation tests
- `erlmcp_message_parser` - Already tested (44 tests), focus on codec errors

**Test Pattern:**
- Chicago School TDD: Real JSON encoding/decoding
- Error path testing: Assert `{error, {Reason, Details}}` tuples
- Size validation: Test with various payload sizes

#### Changes Required:

##### 1. Add Decoding Error Path Test Group

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `decode_test_()` (line 291)

```erlang
%%====================================================================
%% Extended Decoding Tests - Error Paths
%%====================================================================

decode_error_path_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_request_missing_jsonrpc()),
             ?_test(test_decode_request_wrong_version()),
             ?_test(test_decode_request_null_id()),
             ?_test(test_decode_request_string_id()),
             ?_test(test_decode_response_missing_id()),
             ?_test(test_decode_request_unicode_params()),
             ?_test(test_decode_notification_with_id()),
             ?_test(test_decode_notification_missing_method()),
             ?_test(test_decode_invalid_json_syntax()),
             ?_test(test_decode_message_too_large_default()),
             ?_test(test_decode_message_too_large_http()),
             ?_test(test_decode_message_at_limit()),
             ?_test(test_decode_batch_empty_array()),
             ?_test(test_decode_batch_invalid_item()),
             ?_test(test_decode_batch_single_request())
         ]
     end}.

test_decode_request_missing_jsonrpc() ->
    % Missing jsonrpc field (spec violation)
    Json = jsx:encode(#{
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {invalid_request, missing_jsonrpc}}, Result).

test_decode_request_wrong_version() ->
    % Wrong jsonrpc version (1.0 instead of 2.0)
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"1.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {invalid_request, {wrong_version, <<"1.0">>}}}, Result).

test_decode_request_null_id() ->
    % Null id is valid in JSON-RPC 2.0
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => null,
        <<"method">> => <<"initialize">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{id = null}}, Result).

test_decode_request_string_id() ->
    % String id is valid
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-123">>,
        <<"method">> => <<"initialize">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{id = <<"req-123">>}}, Result).

test_decode_response_missing_id() ->
    % Response must have id field
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => <<"ok">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {invalid_request, missing_id}}, Result).

test_decode_request_unicode_params() ->
    % Unicode in params should decode correctly
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{
            <<"text">> => <<"Café résumé naïve">>,
            <<"emoji">> => <<"😀🎉🚀">>
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{params = Params}}, Result),
    {ok, Request} = Result,
    ?assertEqual(<<"Café résumé naïve">>, maps:get(<<"text">>, Request#json_rpc_request.params)).

test_decode_notification_with_id() ->
    % Notification should NOT have id field
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,  % Invalid: notification with id
        <<"method">> => <<"initialized">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    % Parser should detect this as request, not notification
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_notification_missing_method() ->
    % Notification must have method field
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {invalid_request, missing_method}}, Result).

test_decode_invalid_json_syntax() ->
    % Malformed JSON
    InvalidJson = <<"{invalid json}">>,
    Result = erlmcp_json_rpc:decode_message(InvalidJson),
    ?assertMatch({error, {parse_error, _}}, Result).
```

**Reason**: Test decoding error paths (lines 90-117):
- Missing fields (jsonrpc, id, method)
- Wrong version (1.0 vs 2.0)
- Invalid types (notification with id)
- Malformed JSON

##### 2. Add Size Validation Tests

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: Continue in `decode_error_path_test_()`

```erlang
test_decode_message_too_large_default() ->
    % Create message larger than default limit (16MB)
    % Note: This test may need adjustment based on actual limits in sys.config
    LargeData = << <<0>> || _ <- lists:seq(1, 20 * 1024 * 1024) >>,  % 20MB
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"data">> => LargeData}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {message_too_large, _}}, Result).

test_decode_message_too_large_http() ->
    % Test HTTP transport limit (smaller than default)
    LargeData = << <<0>> || _ <- lists:seq(1, 5 * 1024 * 1024) >>,  % 5MB
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"data">> => LargeData}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, http),
    ?assertMatch({error, {message_too_large, _}}, Result).

test_decode_message_at_limit() ->
    % Create message exactly at limit (if limit is configurable)
    % This test may need mock or configured limit
    SmallData = << <<0>> || _ <- lists:seq(1, 1024) >>,  % 1KB
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"data">> => SmallData}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, _}, Result).
```

**Reason**: Test size validation (line 99: `erlmcp_message_size:validate_message_size/2`):
- Message over limit returns error
- Different transports have different limits
- Message under limit succeeds

##### 3. Add Batch Decoding Error Tests

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: Continue in `decode_error_path_test_()`

```erlang
test_decode_batch_empty_array() ->
    % Empty batch is invalid per JSON-RPC 2.0 spec (line 263-265)
    Json = <<"[]">>,
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({error, {invalid_request, empty_batch}}, Result).

test_decode_batch_invalid_item() ->
    % Batch with one invalid item
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test1">>},
        #{<<"invalid">> => <<"object">>}  % Missing required fields
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    % Per spec, batch errors return success with error responses
    % This tests the parser behavior (line 278-288)
    ?assertMatch({ok, [_]}, Result).

test_decode_batch_single_request() ->
    % Single request (not array) should be wrapped in list (line 124-129)
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_]}, Result).
```

**Reason**: Test batch decoding edge cases (lines 119-153):
- Empty array returns error (spec violation)
- Invalid items in batch handled correctly
- Single request wrapped in list

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):

- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests` - 100% pass rate (65/65 tests pass: 35 existing + 15 Phase 1 + 15 Phase 2)
- [ ] **Coverage**: `rebar3 as test cover --verbose` - ≥35% coverage (up from ~15%)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:

- [ ] **Code review**: Error paths tested (all error tuples matched)
- [ ] **Integration**: Size validation works with `erlmcp_message_size`
- [ ] **Spec compliance**: Empty batch returns error, not success
- [ ] **Edge cases**: Unicode decodes correctly

**Quality Gate Checkpoint**: Before proceeding to Phase 3, verify ALL automated gates pass.

---

### Phase 3: Error Helper Function Tests

**Estimated Time**: 3-4 hours

#### Overview

Add 36 tests for error helper functions to increase coverage from ~35% to ~60%. Focus on all 12 error helper functions with 3 test cases each (basic, unicode, edge case).

#### Specification

**WHAT we're building:**

- **Test Group**: `error_helper_extended_test_()`
- **Test Count**: 36 new tests
- **File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Coverage Target**: Lines 184-256 (error helper functions)

**Error Helper Functions to Test:**

1. **error_method_not_found/2** (3 tests)
2. **error_invalid_params/2** (3 tests)
3. **error_resource_not_found/2** (3 tests)
4. **error_tool_not_found/2** (3 tests)
5. **error_prompt_not_found/2** (3 tests)
6. **error_capability_not_supported/2** (3 tests)
7. **error_not_initialized/1** (3 tests)
8. **error_validation_failed/2** (3 tests)
9. **error_message_too_large/2** (3 tests)
10. **error_internal/1** (3 tests)
11. **error_parse/1** (3 tests)
12. **create_error/3** + **create_error_with_data/4** (3 tests)

**Test Pattern for Each Function:**
- Test 1: Basic call with valid input
- Test 2: Unicode data (emoji, accented chars)
- Test 3: Edge case (empty string, null, large data)

#### Pseudocode

```
For each error helper function:
  For each test case (basic, unicode, edge case):
    1. Create input data (Id, Method/Uri/Details/MaxSize)
    2. Call error helper function
    3. Assert result is binary
    4. Decode result with jsx:decode/2
    5. Assert decoded structure has required fields (jsonrpc, id, error)
    6. Assert error.code matches expected code
    7. Assert error.message matches expected message
    8. Assert error.data matches expected data (if present)
```

**Example Test Pseudocode:**

```erlang
test_error_method_not_found_basic() ->
    Id = 1,
    Method = <<"unknown_method">>,
    Result = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"method">> => Method}, maps:get(<<"data">>, Error)).

test_error_method_not_found_unicode() ->
    Id = 2,
    Method = <<"test/😀method">>,
    Result = erlmcp_json_rpc:error_method_not_found(Id, Method),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Method, maps:get(<<"method">>, maps:get(<<"data">>, Error))).

test_error_method_not_found_edge_case() ->
    Id = 3,
    Method = <<>>,  % Empty method name
    Result = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).
```

#### Architecture

**INTEGRATION - Error helpers are thin wrappers around encode_error_response/4**

```
Test Module (erlmcp_json_rpc_tests)
    ↓ calls
Error Helper Functions (erlmcp_json_rpc:error_*)
    ↓ calls
encode_error_response/4 (already tested in Phase 1)
```

**Dependencies:**
- `erlmcp.hrl` - Error code constants (lines 34-65)
- `encode_error_response/4` - Tested in Phase 1

**Test Pattern:**
- Chicago School TDD: Real encoding/decoding
- Error helper testing: Verify correct error code, message, and data
- Data-driven testing: Each function has 3 test cases

#### Changes Required:

##### 1. Add Error Helper Extended Test Group

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `error_creation_test_()` (line 412)

```erlang
%%====================================================================
%% Extended Error Helper Function Tests
%%====================================================================

error_helper_extended_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             %% error_method_not_found/2
             ?_test(test_error_method_not_found_basic()),
             ?_test(test_error_method_not_found_unicode()),
             ?_test(test_error_method_not_found_empty()),

             %% error_invalid_params/2
             ?_test(test_error_invalid_params_basic()),
             ?_test(test_error_invalid_params_unicode()),
             ?_test(test_error_invalid_params_list()),

             %% error_resource_not_found/2
             ?_test(test_error_resource_not_found_basic()),
             ?_test(test_error_resource_not_found_unicode()),
             ?_test(test_error_resource_not_found_empty()),

             %% error_tool_not_found/2
             ?_test(test_error_tool_not_found_basic()),
             ?_test(test_error_tool_not_found_unicode()),
             ?_test(test_error_tool_not_found_empty()),

             %% error_prompt_not_found/2
             ?_test(test_error_prompt_not_found_basic()),
             ?_test(test_error_prompt_not_found_unicode()),
             ?_test(test_error_prompt_not_found_empty()),

             %% error_capability_not_supported/2
             ?_test(test_error_capability_not_supported_basic()),
             ?_test(test_error_capability_not_supported_unicode()),
             ?_test(test_error_capability_not_supported_empty()),

             %% error_not_initialized/1
             ?_test(test_error_not_initialized_basic()),
             ?_test(test_error_not_initialized_string_id()),
             ?_test(test_error_not_initialized_null_id()),

             %% error_validation_failed/2
             ?_test(test_error_validation_failed_basic()),
             ?_test(test_error_validation_failed_unicode()),
             ?_test(test_error_validation_failed_atom()),

             %% error_message_too_large/2
             ?_test(test_error_message_too_large_basic()),
             ?_test(test_error_message_too_large_unicode()),
             ?_test(test_error_message_too_large_max_size()),

             %% error_internal/1
             ?_test(test_error_internal_basic()),
             ?_test(test_error_internal_string_id()),
             ?_test(test_error_internal_null_id()),

             %% error_parse/1
             ?_test(test_error_parse_basic()),
             ?_test(test_error_parse_string_id()),
             ?_test(test_error_parse_null_id()),

             %% create_error/3
             ?_test(test_create_error_basic()),
             ?_test(test_create_error_with_data()),
             ?_test(test_create_error_null_data()),

             %% create_error_with_data/4
             ?_test(test_create_error_with_data_basic()),
             ?_test(test_create_error_with_data_unicode()),
             ?_test(test_create_error_with_data_complex())
         ]
     end}.

%%====================================================================
%% error_method_not_found/2 Tests
%%====================================================================

test_error_method_not_found_basic() ->
    Id = 1,
    Method = <<"unknown_method">>,
    Result = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"method">> => Method}, maps:get(<<"data">>, Error)).

test_error_method_not_found_unicode() ->
    Id = 2,
    Method = <<"test/😀method">>,
    Result = erlmcp_json_rpc:error_method_not_found(Id, Method),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Method, maps:get(<<"method">>, maps:get(<<"data">>, Error))).

test_error_method_not_found_empty() ->
    Id = 3,
    Method = <<>>,  % Empty method name
    Result = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% error_invalid_params/2 Tests
%%====================================================================

test_error_invalid_params_basic() ->
    Id = 1,
    Details = <<"missing required field">>,
    Result = erlmcp_json_rpc:error_invalid_params(Id, Details),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid params">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"details">> => Details}, maps:get(<<"data">>, Error)).

test_error_invalid_params_unicode() ->
    Id = 2,
    Details = <<"Café résumé naïve">>,
    Result = erlmcp_json_rpc:error_invalid_params(Id, Details),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Details, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_invalid_params_list() ->
    Id = 3,
    Details = "missing field",  % String (should be converted to binary)
    Result = erlmcp_json_rpc:error_invalid_params(Id, Details),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<"missing field">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

%%====================================================================
%% error_resource_not_found/2 Tests
%%====================================================================

test_error_resource_not_found_basic() ->
    Id = 1,
    Uri = <<"resource://missing">>,
    Result = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32001, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Resource not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"uri">> => Uri}, maps:get(<<"data">>, Error)).

test_error_resource_not_found_unicode() ->
    Id = 2,
    Uri = <<"file:///path/to/café">>,
    Result = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Uri, maps:get(<<"uri">>, maps:get(<<"data">>, Error))).

test_error_resource_not_found_empty() ->
    Id = 3,
    Uri = <<>>,
    Result = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% error_tool_not_found/2 Tests
%%====================================================================

test_error_tool_not_found_basic() ->
    Id = 1,
    ToolName = <<"missing_tool">>,
    Result = erlmcp_json_rpc:error_tool_not_found(Id, ToolName),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32002, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Tool not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"tool">> => ToolName}, maps:get(<<"data">>, Error)).

test_error_tool_not_found_unicode() ->
    Id = 2,
    ToolName = <<"tool/😀name">>,
    Result = erlmcp_json_rpc:error_tool_not_found(Id, ToolName),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(ToolName, maps:get(<<"tool">>, maps:get(<<"data">>, Error))).

test_error_tool_not_found_empty() ->
    Id = 3,
    ToolName = <<>>,
    Result = erlmcp_json_rpc:error_tool_not_found(Id, ToolName),
    ?assert(is_binary(Result)).

%%====================================================================
%% error_prompt_not_found/2 Tests
%%====================================================================

test_error_prompt_not_found_basic() ->
    Id = 1,
    PromptName = <<"missing_prompt">>,
    Result = erlmcp_json_rpc:error_prompt_not_found(Id, PromptName),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32003, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Prompt not found">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"prompt">> => PromptName}, maps:get(<<"data">>, Error)).

test_error_prompt_not_found_unicode() ->
    Id = 2,
    PromptName = <<"prompt/café">>,
    Result = erlmcp_json_rpc:error_prompt_not_found(Id, PromptName),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(PromptName, maps:get(<<"prompt">>, maps:get(<<"data">>, Error))).

test_error_prompt_not_found_empty() ->
    Id = 3,
    PromptName = <<>>,
    Result = erlmcp_json_rpc:error_prompt_not_found(Id, PromptName),
    ?assert(is_binary(Result)).

%%====================================================================
%% error_capability_not_supported/2 Tests
%%====================================================================

test_error_capability_not_supported_basic() ->
    Id = 1,
    Capability = <<"sampling">>,
    Result = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32004, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Capability not supported">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"capability">> => Capability}, maps:get(<<"data">>, Error)).

test_error_capability_not_supported_unicode() ->
    Id = 2,
    Capability = <<"测试/功能">>,  % Chinese chars
    Result = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Capability, maps:get(<<"capability">>, maps:get(<<"data">>, Error))).

test_error_capability_not_supported_empty() ->
    Id = 3,
    Capability = <<>>,
    Result = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    ?assert(is_binary(Result)).

%%====================================================================
%% error_not_initialized/1 Tests
%%====================================================================

test_error_not_initialized_basic() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_not_initialized(Id),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32005, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Server not initialized">>, maps:get(<<"message">>, Error)).

test_error_not_initialized_string_id() ->
    Id = <<"req-123">>,
    Result = erlmcp_json_rpc:error_not_initialized(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_error_not_initialized_null_id() ->
    Id = null,
    Result = erlmcp_json_rpc:error_not_initialized(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% error_validation_failed/2 Tests
%%====================================================================

test_error_validation_failed_basic() ->
    Id = 1,
    Details = <<"invalid format">>,
    Result = erlmcp_json_rpc:error_validation_failed(Id, Details),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32007, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Validation failed">>, maps:get(<<"message">>, Error)),
    ?assertEqual(#{<<"details">> => Details}, maps:get(<<"data">>, Error)).

test_error_validation_failed_unicode() ->
    Id = 2,
    Details = <<"格式错误 😞">>,
    Result = erlmcp_json_rpc:error_validation_failed(Id, Details),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Details, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_validation_failed_atom() ->
    Id = 3,
    Details = invalid_format,  % Atom (should be converted to binary)
    Result = erlmcp_json_rpc:error_validation_failed(Id, Details),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<"invalid_format">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

%%====================================================================
%% error_message_too_large/2 Tests
%%====================================================================

test_error_message_too_large_basic() ->
    Id = 1,
    MaxSize = 1024,
    Result = erlmcp_json_rpc:error_message_too_large(Id, MaxSize),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32012, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Message too large">>, maps:get(<<"message">>, Error)),
    ExpectedData = #{<<"maxSize">> => MaxSize, <<"unit">> => <<"bytes">>},
    ?assertEqual(ExpectedData, maps:get(<<"data">>, Error)).

test_error_message_too_large_unicode() ->
    Id = 2,
    MaxSize = 2048,
    Result = erlmcp_json_rpc:error_message_too_large(Id, MaxSize),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, maps:get(<<"data">>, Error))).

test_error_message_too_large_max_size() ->
    Id = 3,
    MaxSize = 16 * 1024 * 1024,  % 16MB
    Result = erlmcp_json_rpc:error_message_too_large(Id, MaxSize),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, maps:get(<<"data">>, Error)).

%%====================================================================
%% error_internal/1 Tests
%%====================================================================

test_error_internal_basic() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_internal(Id),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Internal error">>, maps:get(<<"message">>, Error)).

test_error_internal_string_id() ->
    Id = <<"internal-123">>,
    Result = erlmcp_json_rpc:error_internal(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_error_internal_null_id() ->
    Id = null,
    Result = erlmcp_json_rpc:error_internal(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% error_parse/1 Tests
%%====================================================================

test_error_parse_basic() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_parse(Id),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, Error)).

test_error_parse_string_id() ->
    Id = <<"parse-456">>,
    Result = erlmcp_json_rpc:error_parse(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_error_parse_null_id() ->
    Id = null,
    Result = erlmcp_json_rpc:error_parse(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% create_error/3 Tests
%%====================================================================

test_create_error_basic() ->
    Code = -32600,
    Message = <<"Invalid Request">>,
    Error = erlmcp_json_rpc:create_error(Code, Message, undefined),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assertEqual(undefined, Error#mcp_error.data).

test_create_error_with_data() ->
    Code = -32602,
    Message = <<"Invalid Params">>,
    Data = #{<<"field">> => <<"name">>},
    Error = erlmcp_json_rpc:create_error(Code, Message, Data),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ?assertEqual(Data, Error#mcp_error.data).

test_create_error_null_data() ->
    Code = -32603,
    Message = <<"Internal Error">>,
    Data = null,
    Error = erlmcp_json_rpc:create_error(Code, Message, Data),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Data, Error#mcp_error.data).

%%====================================================================
%% create_error_with_data/4 Tests
%%====================================================================

test_create_error_with_data_basic() ->
    Code = -32001,
    Message = <<"Resource not found">>,
    DataKey = uri,
    DataValue = <<"resource://missing">>,
    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, DataKey, DataValue),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message),
    ExpectedData = #{<<"uri">> => DataValue},
    ?assertEqual(ExpectedData, Error#mcp_error.data).

test_create_error_with_data_unicode() ->
    Code = -32002,
    Message = <<"Tool not found">>,
    DataKey = tool,
    DataValue = <<"工具/😀">>,
    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, DataKey, DataValue),
    ?assert(is_record(Error, mcp_error)),
    ExpectedData = #{<<"tool">> => DataValue},
    ?assertEqual(ExpectedData, Error#mcp_error.data).

test_create_error_with_data_complex() ->
    Code = -32007,
    Message = <<"Validation failed">>,
    DataKey = details,
    DataValue = #{<<"nested">> => #{<<"data">> => <<"complex">>}},
    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, DataKey, DataValue),
    ?assert(is_record(Error, mcp_error)),
    ExpectedData = #{<<"details">> => DataValue},
    ?assertEqual(ExpectedData, Error#mcp_error.data).
```

**Reason**: Test all 12 error helper functions (lines 184-256) with 3 test cases each:
- Basic call with valid input
- Unicode data (emoji, accented chars, multi-byte)
- Edge case (empty string, null, atom, list)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):

- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests` - 100% pass rate (101/101 tests pass: 35 existing + 15 Phase 1 + 15 Phase 2 + 36 Phase 3)
- [ ] **Coverage**: `rebar3 as test cover --verbose` - ≥60% coverage (up from ~35%)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:

- [ ] **Code review**: All 12 error helper functions tested
- [ ] **Integration**: Error codes match erlmcp.hrl constants
- [ ] **Spec compliance**: Error objects have correct structure (code, message, data)
- [ ] **Edge cases**: Unicode, empty strings, null handled correctly

**Quality Gate Checkpoint**: Before proceeding to Phase 4, verify ALL automated gates pass.

---

### Phase 4: Edge Case Tests

**Estimated Time**: 2-3 hours

#### Overview

Add 10+ tests for edge cases to increase coverage from ~60% to ~75%. Focus on Unicode, large payloads, special characters, and type coercion.

#### Specification

**WHAT we're building:**

- **Test Group**: `edge_case_test_()`
- **Test Count**: 10+ new tests
- **File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Coverage Target**: Lines 294-372 (internal functions)

**Test Categories:**

1. **Unicode Edge Cases** (4 tests):
   - `test_unicode_emoji_in_method()` - Emoji (😀) in method name
   - `test_unicode_accented_chars_in_params()` - Accented chars (é, ñ)
   - `test_unicode_multi_byte_sequences()` - Multi-byte UTF-8 (Chinese, Japanese)
   - `test_unicode_mixed_scripts()` - Mixed scripts (Latin + Cyrillic + emoji)

2. **Large Payloads** (3 tests):
   - `test_large_payload_1mb()` - 1MB result
   - `test_large_payload_16mb_limit()` - 16MB result (at limit)
   - `test_large_payload_batch()` - Batch with multiple large items

3. **Special Characters** (3 tests):
   - `test_special_characters_json_escaping()` - JSON escaping (quotes, backslashes)
   - `test_null_bytes_in_binary()` - Null bytes in binary data
   - `test_control_characters()` - Control characters (\n, \t, \r)

4. **Type Coercion** (3 tests):
   - `test_type_coercion_string_to_binary()` - String converted to binary
   - `test_type_coercion_atom_to_binary()` - Atom converted to binary
   - `test_type_coercion_list_to_binary()` - List converted to binary

#### Pseudocode

```
For each edge case test:
  1. Create input with edge case (unicode, large data, special chars)
  2. Call encoding/decoding function
  3. Assert result is binary (encoding) or {ok, _} (decoding)
  4. Decode result with jsx:decode/2
  5. Assert decoded data matches input (no data corruption)
  6. Assert special chars handled correctly (escaped if needed)
```

**Example Test Pseudocode:**

```erlang
test_unicode_emoji_in_method() ->
    Id = 1,
    Method = <<"test/😀🎉🚀method">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)).
```

#### Architecture

**INTEGRATION - Edge cases test encoding/decoding robustness**

```
Test Module (erlmcp_json_rpc_tests)
    ↓ calls
Module Under Test (erlmcp_json_rpc)
    ↓ uses
jsx (JSON encoding/decoding - must handle UTF-8 correctly)
```

**Dependencies:**
- `jsx` - Must handle UTF-8 encoding/decoding correctly
- `erlmcp.hrl` - Constants for testing

**Test Pattern:**
- Chicago School TDD: Real encoding/decoding
- Edge case testing: Verify no data corruption
- Round-trip testing: Encode → Decode → Compare

#### Changes Required:

##### 1. Add Edge Case Test Group

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `error_helper_extended_test_()`

```erlang
%%====================================================================
%% Edge Case Tests - Unicode, Large Payloads, Special Characters
%%====================================================================

edge_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             %% Unicode edge cases
             ?_test(test_unicode_emoji_in_method()),
             ?_test(test_unicode_accented_chars_in_params()),
             ?_test(test_unicode_multi_byte_sequences()),
             ?_test(test_unicode_mixed_scripts()),

             %% Large payloads
             ?_test(test_large_payload_1mb()),
             ?_test(test_large_payload_16mb_limit()),
             ?_test(test_large_payload_batch()),

             %% Special characters
             ?_test(test_special_characters_json_escaping()),
             ?_test(test_null_bytes_in_binary()),
             ?_test(test_control_characters()),

             %% Type coercion
             ?_test(test_type_coercion_string_to_binary()),
             ?_test(test_type_coercion_atom_to_binary()),
             ?_test(test_type_coercion_list_to_binary())
         ]
     end}.

%%====================================================================
%% Unicode Edge Cases
%%====================================================================

test_unicode_emoji_in_method() ->
    Id = 1,
    Method = <<"test/😀🎉🚀method">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)).

test_unicode_accented_chars_in_params() ->
    Id = 2,
    Method = <<"test/params">>,
    Params = #{
        <<"text">> => <<"Café résumé naïve">>,
        <<"emoji">> => <<"😀🎉🚀">>
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).

test_unicode_multi_byte_sequences() ->
    Id = 3,
    Method = <<"test/chinese">>,
    Params = #{
        <<"chinese">> => <<"你好世界">>,  % Chinese
        <<"japanese">> => <<"こんにちは">>,  % Japanese
        <<"korean">> => <<"안녕하세요">>  % Korean
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).

test_unicode_mixed_scripts() ->
    Id = 4,
    Method = <<"тест/😀method">>,  % Cyrillic + emoji
    Params = #{
        <<"mixed">> => <<"Hello 世界 😀 こんにちは Привет">>
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).

%%====================================================================
%% Large Payloads
%%====================================================================

test_large_payload_1mb() ->
    Id = 1,
    Method = <<"test/large">>,
    LargeData = << <<0>> || _ <- lists:seq(1, 1024 * 1024) >>,  % 1MB
    Params = #{<<"data">> => LargeData},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 1024 * 1024),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    DecodedData = maps:get(<<"data">>, DecodedParams),
    ?assertEqual(byte_size(LargeData), byte_size(DecodedData)).

test_large_payload_16mb_limit() ->
    Id = 2,
    Method = <<"test/limit">>,
    % Create 16MB payload (at configurable limit)
    LargeData = << <<0>> || _ <- lists:seq(1, 16 * 1024 * 1024) >>,
    Result = erlmcp_json_rpc:encode_response(Id, LargeData),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 16 * 1024 * 1024).

test_large_payload_batch() ->
    % Batch with multiple large items
    LargeData1 = << <<1>> || _ <- lists:seq(1, 1024 * 1024) >>,
    LargeData2 = << <<2>> || _ <- lists:seq(1, 1024 * 1024) >>,
    Requests = [
        #json_rpc_request{id = 1, method = <<"method1">>, params = #{<<"data">> => LargeData1}},
        #json_rpc_request{id = 2, method = <<"method2">>, params = #{<<"data">> => LargeData2}}
    ],
    Result = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 2 * 1024 * 1024).

%%====================================================================
%% Special Characters
%%====================================================================

test_special_characters_json_escaping() ->
    Id = 1,
    Method = <<"test/escape">>,
    Params = #{
        <<"quote">> => <<"He said \"hello\"">>,
        <<"backslash">> => <<"path\\to\\file">>,
        <<"newline">> => <<"line1\nline2">>,
        <<"tab">> => <<"col1\tcol2">>
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).

test_null_bytes_in_binary() ->
    Id = 2,
    Method = <<"test/null">>,
    Params = #{<<"binary">> => <<0, 1, 2, 0, 3>>},  % Null bytes
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).

test_control_characters() ->
    Id = 3,
    Method = <<"test/control">>,
    Params = #{
        <<"newline">> => <<"line1\nline2">>,
        <<"carriage">> => <<"line1\rline2">>,
        <<"tab">> => <<"col1\tcol2">>,
        <<"mixed">> => <<"line1\r\n\tline2">>
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(Params, DecodedParams).

%%====================================================================
%% Type Coercion Tests
%%====================================================================

test_type_coercion_string_to_binary() ->
    % error_invalid_params/2 converts string to binary
    Id = 1,
    Details = "missing field",  % String
    Result = erlmcp_json_rpc:error_invalid_params(Id, Details),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<"missing field">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_type_coercion_atom_to_binary() ->
    % error_validation_failed/2 converts atom to binary
    Id = 2,
    Details = invalid_format,  % Atom
    Result = erlmcp_json_rpc:error_validation_failed(Id, Details),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<"invalid_format">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_type_coercion_list_to_binary() ->
    % error_invalid_params/2 converts list to binary
    Id = 3,
    Details = $m, $i, $s, $s, $i, $n, $g,  % Char list
    Result = erlmcp_json_rpc:error_invalid_params(Id, Details),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<"missing">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).
```

**Reason**: Test edge cases that could cause data corruption or encoding errors:
- Unicode: Multi-byte UTF-8 sequences must not be corrupted
- Large payloads: Must handle 1MB+ data without errors
- Special characters: JSON escaping must work correctly
- Type coercion: String/atom/list must convert to binary correctly

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):

- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests` - 100% pass rate (111+/111+ tests pass: 35 existing + 15 Phase 1 + 15 Phase 2 + 36 Phase 3 + 10+ Phase 4)
- [ ] **Coverage**: `rebar3 as test cover --verbose` - ≥75% coverage (up from ~60%)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:

- [ ] **Code review**: Unicode data not corrupted
- [ ] **Integration**: Large payloads handled correctly
- [ ] **Edge cases**: Special characters escaped properly
- [ ] **Type coercion**: All types convert to binary correctly

**Quality Gate Checkpoint**: Before proceeding to Phase 5, verify ALL automated gates pass.

---

### Phase 5: Internal Function Tests

**Estimated Time**: 2 hours

#### Overview

Add 8+ tests for internal functions to increase coverage from ~75% to ~80%. Focus on lines not covered by previous tests (build_message_map, encode_id, maybe_add_params, add_result_or_error, build_error_object).

#### Specification

**WHAT we're building:**

- **Test Group**: `internal_function_test_()`
- **Test Count**: 8+ new tests
- **File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Coverage Target**: Lines 294-372 (internal functions)

**Internal Functions to Test:**

1. **build_message_map/1** (3 tests):
   - Test with #json_rpc_request{} (lines 300-306)
   - Test with #json_rpc_response{} (lines 308-313)
   - Test with #json_rpc_notification{} (lines 315-320)

2. **encode_id/1** (3 tests):
   - Test with null (line 323)
   - Test with binary (line 324)
   - Test with integer (line 325)

3. **maybe_add_params/2** (2 tests):
   - Test with undefined (lines 328-329)
   - Test with defined params (lines 330-331)

4. **add_result_or_error/3** (2 tests):
   - Test with error (lines 334-335)
   - Test with result (lines 336-337)

5. **build_error_object/3** (4 tests):
   - Test with undefined (lines 340-344)
   - Test with null (lines 345-350)
   - Test with map data (lines 351-357)
   - Test with binary data (lines 358-364)
   - Test with other term data (lines 365-372)

#### Pseudocode

```
For each internal function test:
  1. Create input record or parameters
  2. Call internal function (via encoding API that uses it)
  3. Assert result is correct map structure
  4. Assert all required fields present
  5. Assert optional fields handled correctly
```

**Note**: Internal functions are not exported, so we test them indirectly via the encoding API that uses them.

#### Architecture

**INTEGRATION - Internal functions tested via public API**

```
Test Module (erlmcp_json_rpc_tests)
    ↓ calls
Public API (encode_request/3, encode_response/2, etc.)
    ↓ calls
Internal Functions (build_message_map/1, encode_id/1, etc.)
```

**Dependencies:**
- `erlmcp_json_rpc` - Public API that calls internal functions
- `erlmcp.hrl` - Records and constants

**Test Pattern:**
- Indirect testing: Test internal functions via public API
- Structure validation: Verify correct map structure generated
- Field presence: Check required vs optional fields

#### Changes Required:

##### 1. Add Internal Function Test Group

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `edge_case_test_()`

```erlang
%%====================================================================
%% Internal Function Tests (tested via public API)
%%====================================================================

internal_function_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             %% build_message_map/1 (via encode_request, encode_response, encode_notification)
             ?_test(test_build_message_map_request()),
             ?_test(test_build_message_map_response()),
             ?_test(test_build_message_map_notification()),

             %% encode_id/1 (via encode_request, encode_response)
             ?_test(test_encode_id_null()),
             ?_test(test_encode_id_binary()),
             ?_test(test_encode_id_integer()),

             %% maybe_add_params/2 (via encode_request, encode_notification)
             ?_test(test_maybe_add_params_undefined()),
             ?_test(test_maybe_add_params_defined()),

             %% add_result_or_error/3 (via encode_response)
             ?_test(test_add_result_or_error_result()),
             ?_test(test_add_result_or_error_error()),

             %% build_error_object/3 (via encode_error_response)
             ?_test(test_build_error_object_undefined()),
             ?_test(test_build_error_object_null()),
             ?_test(test_build_error_object_map()),
             ?_test(test_build_error_object_binary()),
             ?_test(test_build_error_object_term())
         ]
     end}.

%%====================================================================
%% build_message_map/1 Tests (via encode_request, encode_response, encode_notification)
%%====================================================================

test_build_message_map_request() ->
    % Test build_message_map for #json_rpc_request{} (lines 300-306)
    Id = 1,
    Method = <<"test/method">>,
    Params = #{<<"key">> => <<"value">>},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(Params, maps:get(<<"params">>, Decoded)).

test_build_message_map_response() ->
    % Test build_message_map for #json_rpc_response{} (lines 308-313)
    Id = 2,
    ResultData = #{<<"status">> => <<"ok">>},
    Result = erlmcp_json_rpc:encode_response(Id, ResultData),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(ResultData, maps:get(<<"result">>, Decoded)).

test_build_message_map_notification() ->
    % Test build_message_map for #json_rpc_notification{} (lines 315-320)
    Method = <<"test/notify">>,
    Params = #{<<"key">> => <<"value">>},
    Result = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(Params, maps:get(<<"params">>, Decoded)),
    ?assertNot(maps:is_key(<<"id">>, Decoded)).

%%====================================================================
%% encode_id/1 Tests (via encode_request, encode_response)
%%====================================================================

test_encode_id_null() ->
    % Test encode_id with null (line 323)
    Id = null,
    Method = <<"test/method">>,
    Result = erlmcp_json_rpc:encode_request(Id, Method, undefined),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

test_encode_id_binary() ->
    % Test encode_id with binary (line 324)
    Id = <<"req-123">>,
    Method = <<"test/method">>,
    Result = erlmcp_json_rpc:encode_request(Id, Method, undefined),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_id_integer() ->
    % Test encode_id with integer (line 325)
    Id = 42,
    Method = <<"test/method">>,
    Result = erlmcp_json_rpc:encode_request(Id, Method, undefined),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% maybe_add_params/2 Tests (via encode_request, encode_notification)
%%====================================================================

test_maybe_add_params_undefined() ->
    % Test maybe_add_params with undefined (lines 328-329)
    Id = 1,
    Method = <<"test/method">>,
    Result = erlmcp_json_rpc:encode_request(Id, Method, undefined),
    Decoded = jsx:decode(Result, [return_maps]),
    % params field should NOT be present
    ?assertNot(maps:is_key(<<"params">>, Decoded)).

test_maybe_add_params_defined() ->
    % Test maybe_add_params with defined params (lines 330-331)
    Id = 2,
    Method = <<"test/method">>,
    Params = #{<<"key">> => <<"value">>},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Decoded = jsx:decode(Result, [return_maps]),
    % params field should be present
    ?assertEqual(Params, maps:get(<<"params">>, Decoded)).

%%====================================================================
%% add_result_or_error/3 Tests (via encode_response)
%%====================================================================

test_add_result_or_error_result() ->
    % Test add_result_or_error with result (lines 336-337)
    Id = 1,
    ResultData = #{<<"status">> => <<"ok">>},
    Result = erlmcp_json_rpc:encode_response(Id, ResultData),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(ResultData, maps:get(<<"result">>, Decoded)),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

test_add_result_or_error_error() ->
    % Test add_result_or_error with error (lines 334-335)
    Id = 2,
    ErrorData = #{<<"code">> => -32600, <<"message">> => <<"Invalid">>},
    Error = #mcp_error{code = -32600, message = <<"Invalid">>, data = #{}},
    Result = erlmcp_json_rpc:encode_error_response(Id, -32600, <<"Invalid">>, #{}),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assertNot(maps:is_key(<<"result">>, Decoded)).

%%====================================================================
%% build_error_object/3 Tests (via encode_error_response)
%%====================================================================

test_build_error_object_undefined() ->
    % Test build_error_object with undefined (lines 340-344)
    Id = 1,
    Result = erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Internal error">>, undefined),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Internal error">>, maps:get(<<"message">>, Error)),
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_build_error_object_null() ->
    % Test build_error_object with null (lines 345-350)
    Id = 2,
    Result = erlmcp_json_rpc:encode_error_response(Id, -32600, <<"Invalid">>, null),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_build_error_object_map() ->
    % Test build_error_object with map data (lines 351-357)
    Id = 3,
    Data = #{<<"field">> => <<"name">>},
    Result = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Invalid params">>, Data),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

test_build_error_object_binary() ->
    % Test build_error_object with binary data (lines 358-364)
    Id = 4,
    Data = <<"Error details">>,
    Result = erlmcp_json_rpc:encode_error_response(Id, -32001, <<"Not found">>, Data),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ExpectedData = #{<<"details">> => Data},
    ?assertEqual(ExpectedData, maps:get(<<"data">>, Error)).

test_build_error_object_term() ->
    % Test build_error_object with term data (lines 365-372)
    Id = 5,
    Data = {complex, term, [with, list]},
    Result = erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Internal">>, Data),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)),
    DataMap = maps:get(<<"data">>, Error),
    ?assert(maps:is_key(<<"details">>, DataMap)),
    ?assert(is_binary(maps:get(<<"details">>, DataMap))).
```

**Reason**: Test internal functions (lines 294-372) indirectly via public API:
- `build_message_map/1`: All 3 record types (request, response, notification)
- `encode_id/1`: All 3 ID types (null, binary, integer)
- `maybe_add_params/2`: Both branches (undefined vs defined)
- `add_result_or_error/3`: Both branches (result vs error)
- `build_error_object/3`: All 5 data type branches (undefined, null, map, binary, term)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):

- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests` - 100% pass rate (119+/119+ tests pass: 35 existing + 15 Phase 1 + 15 Phase 2 + 36 Phase 3 + 10+ Phase 4 + 8+ Phase 5)
- [ ] **Coverage**: `rebar3 as test cover --verbose` - ≥80% coverage (TARGET ACHIEVED)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:

- [ ] **Code review**: All internal functions covered
- [ ] **Integration**: Public API correctly calls internal functions
- [ ] **Edge cases**: All branches of internal functions tested
- [ ] **Coverage target**: ≥80% achieved

**Quality Gate Checkpoint**: Verify ALL automated gates pass. Coverage target ≥80% ACHIEVED.

---

### Phase 6: Batch Operations Extended Tests

**Estimated Time**: 1-2 hours

#### Overview

Add 5 tests for batch operations edge cases to ensure coverage exceeds 80% target and verify JSON-RPC 2.0 spec compliance for batch requests.

#### Specification

**WHAT we're building:**

- **Test Group**: `batch_operations_extended_test_()`
- **Test Count**: 5 new tests
- **File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- **Coverage Target**: Lines 139-153, 262-288 (batch operations)

**Test Functions:**

1. **encode_batch/1** (2 tests):
   - `test_encode_batch_empty()` - Empty list (should encode as [])
   - `test_encode_batch_mixed_types()` - Mixed request/notification types

2. **decode_batch/1** (2 tests):
   - `test_decode_batch_with_notifications()` - Batch with notifications
   - `test_decode_batch_all_errors()` - Batch with all invalid items

3. **is_batch_request/1** (1 test):
   - `test_is_batch_request_not_array()` - Non-array JSON returns false

#### Pseudocode

```
For each batch operation test:
  1. Create batch input (list of messages or JSON array)
  2. Call batch function (encode_batch, decode_batch, is_batch_request)
  3. Assert result is correct (binary, {ok, _}, or boolean)
  4. Verify JSON-RPC 2.0 spec compliance
```

#### Architecture

**INTEGRATION - Batch operations tested independently**

```
Test Module (erlmcp_json_rpc_tests)
    ↓ calls
Batch Operations (encode_batch/1, decode_batch/1, is_batch_request/1)
    ↓ uses
erlmcp_message_parser:parse_json_rpc/1 (for decode_batch)
```

**Dependencies:**
- `erlmcp_message_parser` - Already tested (44 tests)
- `jsx` - JSON encoding/decoding

**Test Pattern:**
- Chicago School TDD: Real encoding/decoding
- Spec compliance: Verify batch behavior matches JSON-RPC 2.0 spec

#### Changes Required:

##### 1. Add Batch Operations Extended Test Group

**File**: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
**Location**: After `internal_function_test_()`

```erlang
%%====================================================================
%% Extended Batch Operations Tests
%%====================================================================

batch_operations_extended_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_batch_empty()),
             ?_test(test_encode_batch_mixed_types()),
             ?_test(test_decode_batch_with_notifications()),
             ?_test(test_decode_batch_all_errors()),
             ?_test(test_is_batch_request_not_array())
         ]
     end}.

%%====================================================================
%% encode_batch/1 Tests
%%====================================================================

test_encode_batch_empty() ->
    % Encode empty batch (should be "[]")
    Messages = [],
    Result = erlmcp_json_rpc:encode_batch(Messages),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([], Decoded).

test_encode_batch_mixed_types() ->
    % Encode batch with mixed request/notification types
    Messages = [
        #json_rpc_request{id = 1, method = <<"method1">>, params = #{}},
        #json_rpc_notification{method = <<"notify1">>, params = #{}},
        #json_rpc_request{id = 2, method = <<"method2">>, params = #{}}
    ],
    Result = erlmcp_json_rpc:encode_batch(Messages),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(3, length(Decoded)),
    % Verify first item has id
    ?assert(maps:is_key(<<"id">>, lists:nth(1, Decoded))),
    % Verify second item has no id (notification)
    ?assertNot(maps:is_key(<<"id">>, lists:nth(2, Decoded))),
    % Verify third item has id
    ?assert(maps:is_key(<<"id">>, lists:nth(3, Decoded))).

%%====================================================================
%% decode_batch/1 Tests
%%====================================================================

test_decode_batch_with_notifications() ->
    % Decode batch with notifications mixed with requests
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"method1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notify1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"method2">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, Messages}, Result),
    {ok, Messages} = Result,
    ?assertEqual(3, length(Messages)),
    % Verify first is request
    ?assert(is_record(lists:nth(1, Messages), json_rpc_request)),
    % Verify second is notification
    ?assert(is_record(lists:nth(2, Messages), json_rpc_notification)),
    % Verify third is request
    ?assert(is_record(lists:nth(3, Messages), json_rpc_request)).

test_decode_batch_all_errors() ->
    % Decode batch with all invalid items (per spec, return empty list)
    Json = jsx:encode([
        #{<<"invalid">> => <<"object1">>},
        #{<<"invalid">> => <<"object2">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    % Per parse_batch_requests logic (lines 278-288), invalid items are skipped
    ?assertMatch({ok, []}, Result).

%%====================================================================
%% is_batch_request/1 Tests
%%====================================================================

test_is_batch_request_not_array() ->
    % Non-array JSON should return false
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ?assertNot(erlmcp_json_rpc:is_batch_request(Json)).
```

**Reason**: Test batch operations edge cases (lines 139-153, 262-288):
- Empty batch encoding
- Mixed message types in batch
- Batch with notifications (no id field)
- Invalid items in batch
- is_batch_request with non-array JSON

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):

- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests` - 100% pass rate (124+/124+ tests pass: 35 existing + 15 Phase 1 + 15 Phase 2 + 36 Phase 3 + 10+ Phase 4 + 8+ Phase 5 + 5 Phase 6)
- [ ] **Coverage**: `rebar3 as test cover --verbose` - ≥80% coverage (TARGET EXCEEDED)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:

- [ ] **Code review**: All batch operations covered
- [ ] **Spec compliance**: Empty batch handled correctly per JSON-RPC 2.0
- [ ] **Integration**: Mixed message types handled correctly
- [ ] **Coverage target**: ≥80% achieved and exceeded

**Quality Gate Checkpoint**: Verify ALL automated gates pass. Coverage target ≥80% EXCEEDED.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

**NO MOCKS** - Use real encoding/decoding with `jsx:encode/1` and `jsx:decode/2`
**State-Based Verification** - Check decoded map structures, not exact JSON strings
**Integration Testing** - Test with real dependencies (jsx, erlmcp_message_parser)
**Round-Trip Testing** - Encode → Decode → Compare to verify no data corruption

**Reference Pattern**: `erlmcp_message_parser_tests.erl:1-351` (44 tests, good example)

### Unit Tests (EUnit)

**What to Test:**
- All 31 exported functions in `erlmcp_json_rpc.erl`
- All internal functions via public API (build_message_map, encode_id, etc.)
- All error paths (invalid JSON, missing fields, wrong types)
- All edge cases (Unicode, large payloads, special characters)

**Test Pattern:**
```erlang
test_function_name() ->
    Input = create_test_input(),
    Result = erlmcp_json_rpc:function_name(Input),
    ?assertMatch(ExpectedPattern, Result),
    % Additional assertions as needed
    ?assertEqual(ExpectedValue, ActualValue).
```

**Coverage Target:** ≥80% line and function coverage

**Pass Rate:** 100% (all 124+ tests must pass)

### Manual Testing Steps

1. **Run EUnit tests**: `rebar3 eunit --module=erlmcp_json_rpc_tests --verbose`
2. **Generate coverage report**: `rebar3 as test cover --verbose`
3. **Check specific module coverage**: `rebar3 as test cover --verbose | grep -A 5 erlmcp_json_rpc`
4. **View HTML coverage report**: `open _build/test/cover/index.html`
5. **Verify no regressions**: `rebar3 check` (compilation + all tests)
6. **Code review**: Verify OTP patterns followed, type specs correct

### Quality Gates

**EVERY PHASE MUST PASS:**

1. **Compilation**: `TERM=dumb rebar3 compile`
   - 0 errors (MANDATORY)
   - 0 warnings (MANDATORY)

2. **EUnit**: `rebar3 eunit --module=erlmcp_json_rpc_tests`
   - 100% pass rate (MANDATORY)
   - All tests must pass (no skips, no failures)

3. **Coverage**: `rebar3 as test cover --verbose`
   - ≥80% coverage (MANDATORY)
   - Measured per module (erlmcp_json_rpc)

4. **Dialyzer**: `rebar3 dialyzer`
   - 0 warnings (MANDATORY)
   - Type specs must be correct

5. **Xref**: `rebar3 xref`
   - 0 undefined function calls (MANDATORY)
   - All functions must exist

**Gate Enforcement:**
- Use pre-commit hook: `.claude/hooks/pre-task-validate.sh`
- CI must block PR if any gate fails
- Andon signaling: Failures visible in CI logs

## Manufacturing Checklist

### Before Implementation

- [x] **Research verified** (read actual source code)
  - [x] erlmcp_json_rpc.erl: All 31 functions reviewed
  - [x] erlmcp_json_rpc_tests.erl: All 35 existing tests reviewed
  - [x] erlmcp.hrl: All constants and records reviewed
  - [x] erlmcp_message_parser_tests.erl: Reference test pattern reviewed

- [x] **Scope confirmed** (IN/OUT documented)
  - [x] IN: Add 40+ tests to erlmcp_json_rpc_tests.erl
  - [x] IN: Achieve ≥80% code coverage
  - [x] OUT: Modifying erlmcp_json_rpc.erl source code
  - [x] OUT: Adding new API functions
  - [x] OUT: Performance testing
  - [x] OUT: Integration testing with transports

- [x] **No open questions** (all decisions made)
  - [x] Current coverage: 0% (verified in COVERAGE_REPORT.md)
  - [x] Target coverage: ≥80% (Lean Six Sigma standard)
  - [x] Gap: 80 percentage points (0% → 80%)
  - [x] Existing tests: 35 (counted in test file)
  - [x] Target tests: 75+ (35 existing + 40+ new)
  - [x] Test pattern: Chicago School TDD (real encoding/decoding)

- [x] **Phases broken down** (≤4 hours each)
  - [x] Phase 1: Encoding Extended Tests (3-4 hours)
  - [x] Phase 2: Decoding Error Path Tests (3-4 hours)
  - [x] Phase 3: Error Helper Function Tests (3-4 hours)
  - [x] Phase 4: Edge Case Tests (2-3 hours)
  - [x] Phase 5: Internal Function Tests (2 hours)
  - [x] Phase 6: Batch Operations Extended Tests (1-2 hours)
  - [x] Total: 14-19 hours (within 2-3 day sprint)

- [x] **Acceptance criteria defined** (measurable, specific)
  - [x] ≥80% coverage (measured by rebar3 cover)
  - [x] ≥75 total tests (35 existing + 40+ new)
  - [x] 100% pass rate (all tests pass)
  - [x] 0 compilation errors (rebar3 compile)
  - [x] 0 Dialyzer warnings (rebar3 dialyzer)
  - [x] 0 Xref errors (rebar3 xref)

### During Implementation

- [ ] **Chicago School TDD followed** (tests FIRST)
  - [ ] Real encoding/decoding (no mocks)
  - [ ] State-based verification (check decoded structures)
  - [ ] Round-trip testing (encode → decode → compare)
  - [ ] Error path testing (assert {error, {Reason, Details}})

- [ ] **OTP patterns followed** (no gen_server - pure functional module)
  - [ ] Type specs present (all functions have -spec)
  - [ ] Error handling complete (all paths return {ok, _} or {error, _})
  - [ ] Record patterns used (json_rpc_request, json_rpc_response, etc.)

- [ ] **Quality gates passing** (compilation, tests, coverage)
  - [ ] Phase 1: All gates pass (≥15% coverage)
  - [ ] Phase 2: All gates pass (≥35% coverage)
  - [ ] Phase 3: All gates pass (≥60% coverage)
  - [ ] Phase 4: All gates pass (≥75% coverage)
  - [ ] Phase 5: All gates pass (≥80% coverage)
  - [ ] Phase 6: All gates pass (≥80% coverage)

### After Implementation

- [ ] **All tests passing** (100% pass rate)
  - [ ] 124+ tests pass (35 existing + 40+ new)
  - [ ] 0 tests fail
  - [ ] 0 tests skipped

- [ ] **Coverage ≥80%** (verified)
  - [ ] rebar3 as test cover --verbose
  - [ ] Check _build/test/cover/index.html
  - [ ] Verify line coverage ≥80%
  - [ ] Verify function coverage ≥80%

- [ ] **Dialyzer 0 warnings** (verified)
  - [ ] rebar3 dialyzer
  - [ ] All type specs correct
  - [ ] No type mismatches

- [ ] **Xref 0 undefined calls** (verified)
  - [ ] rebar3 xref
  - [ ] All functions exist
  - [ ] No undefined function calls

- [ ] **Documentation updated** (README, API docs)
  - [ ] COVERAGE_REPORT.md updated (erlmcp_json_rpc: 0% → ≥80%)
  - [ ] Test comments added (explain what each test verifies)

- [ ] **Code review complete** (OTP patterns verified)
  - [ ] Chicago School TDD followed (no mocks)
  - [ ] Test structure follows EUnit conventions
  - [ ] Error paths tested (all error tuples matched)
  - [ ] Edge cases covered (Unicode, large payloads, special chars)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Insufficient test coverage after adding 40 tests** | P2 (Medium) | Low | Use `rebar3 cover --verbose` to measure coverage incrementally. Identify uncovered lines before writing tests. Add more tests if coverage <80%. |
| **Test flakiness due to JSON encoding non-determinism** | P1 (High) | Low | Use structure validation (decoded maps), not exact JSON string matching. Avoid floating point in tests. Use `?assertMatch` for pattern matching. |
| **Message size validation tests fail with default limits** | P2 (Medium) | Low | Use realistic sizes (<100MB per sys.config). Mock `erlmcp_message_size` limits if needed. Test with 1KB, 1MB, 16MB payloads. |
| **Unicode tests fail on different Erlang/OTP versions** | P3 (Low) | Low | Use standard Unicode examples (emoji, accented chars). Test UTF-8 encoding with jsx (handles UTF-8 correctly). |
| **Batch request tests don't cover empty array edge case** | P2 (Medium) | Low | Explicitly test empty batch returns `{error, {invalid_request, empty_batch}}` (line 263-265). Added in Phase 6. |
| **Error code validation tests miss edge cases** | P2 (Medium) | Low | Test all valid codes (line 47-65) and invalid codes (0, 9999). Verify invalid codes converted to internal error (line 69-74). |
| **JSON field ordering not validated in encoding tests** | P3 (Low) | Low | Use `jsx:decode/2` to verify structure, not exact string matching (field order not required by JSON-RPC 2.0 spec). Added in Phase 1. |

### Rollback Plan

**How to rollback if something goes wrong:**

1. **Git revert**:
   ```bash
   git checkout wreckit/004-create-comprehensive-eunit-test-suite-for-erlmcpjs
   git revert HEAD  # Revert if tests break production
   ```

2. **Data migration**: N/A (no data changes, test-only task)

3. **Service impact**: No service impact (test-only task, no production code changed)

4. **Recovery procedure**:
   - If tests break compilation: Delete new tests, revert to working state
   - If tests cause failures: Fix failing tests (don't delete)
   - If coverage target not met: Add more tests until ≥80% achieved

## References

- **Research**: `/Users/sac/erlmcp/.wreckit/items/004-create-comprehensive-eunit-test-suite-for-erlmcpjs/research.md`
- **CLAUDE.md**: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- **TCPS**: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **OTP Patterns**: `/Users/sac/erlmcp/docs/otp-patterns.md`
- **Test Reference**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_message_parser_tests.erl:1-351` (44 tests, good example)
- **Source Code**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl:1-378`
- **Existing Tests**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl:1-476`
- **Protocol Docs**: `/Users/sac/erlmcp/docs/protocol.md:1-150`
- **Coverage Report**: `/Users/sac/erlmcp/docs/COVERAGE_REPORT.md:74` (0% coverage)
- **Constants**: `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl:1-523`

## Appendix: Test Coverage Tracking

**Baseline (Before Implementation):**
- Coverage: 0%
- Tests: 35
- Lines: 378

**Phase 1 (Encoding Extended):**
- Target Coverage: 15%
- New Tests: 15
- Total Tests: 50

**Phase 2 (Decoding Error Paths):**
- Target Coverage: 35%
- New Tests: 15
- Total Tests: 65

**Phase 3 (Error Helper Functions):**
- Target Coverage: 60%
- New Tests: 36
- Total Tests: 101

**Phase 4 (Edge Cases):**
- Target Coverage: 75%
- New Tests: 10+
- Total Tests: 111+

**Phase 5 (Internal Functions):**
- Target Coverage: 80% ✅
- New Tests: 8+
- Total Tests: 119+

**Phase 6 (Batch Operations Extended):**
- Target Coverage: 80%+ ✅
- New Tests: 5
- Total Tests: 124+

**Final State:**
- Coverage: ≥80% (TARGET ACHIEVED)
- Tests: 124+ (35 existing + 89 new)
- Lines: 378 (no changes to source)
