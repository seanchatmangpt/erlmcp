# Research: Create comprehensive EUnit test suite for erlmcp_json_rpc.erl

**Date**: 2026-01-29
**Item**: 004-create-comprehensive-eunit-test-suite-for-erlmcpjs
**Section**: coverage
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
JSON-RPC codec has 0% test coverage - protocol implementation is completely untested

**Motivation:** Processes EVERY message in and out of the system. A bug here breaks the entire MCP protocol. JSON-RPC 2.0 spec compliance is critical.

**Success criteria:**
- ≥80% code coverage achieved
- ≥40 tests created
- All encode/decode functions tested
- JSON-RPC 2.0 spec compliance verified
- Error handling paths tested
- All tests pass: rebar3 eunit --module=erlmcp_json_rpc_tests

**Technical constraints:**
- JSON encoding tests - valid JSON output, correct field ordering
- JSON decoding tests - valid JSON input, error handling
- Schema validation tests - JSON-RPC 2.0 spec compliance
- Edge case tests - Unicode, special characters, large payloads
- Error path tests - invalid JSON, missing fields, wrong types

**Signals:** priority: critical, urgency: P0 - BLOCKS ALL PRODUCTION WORK

### Quality Gate Status
- **Gate Type**: Coverage / EUnit / Protocol Compliance
- **Current State**: 0% coverage (COVERAGE_REPORT.md:74) - CRITICAL GAP
- **Target State**: ≥80% coverage (Lean Six Sigma standard)
- **Gap**: 80 percentage points (absolute gap from 0% to 80%)

## Summary

**Manufacturing Objective:** Create a comprehensive EUnit test suite for `erlmcp_json_rpc.erl` to achieve ≥80% code coverage and ensure JSON-RPC 2.0 specification compliance. This module is the critical protocol codec that processes EVERY message entering and leaving the MCP system - a single bug breaks the entire protocol communication layer.

**Technical Approach:** The existing test file at `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` contains only 35 basic tests covering happy paths. We need to expand this to ≥40 tests with deep coverage of:
1. **Encoding functions** (lines 45-88): `encode_request/3`, `encode_response/2`, `encode_error_response/3,4`, `encode_notification/2`
2. **Decoding functions** (lines 90-137): `decode_message/1,2`, `decode_batch/1`, message size validation
3. **Error helpers** (lines 155-257): All 12 error creation functions
4. **Batch operations** (lines 119-153): `encode_batch/1`, `decode_batch/1`, `is_batch_request/1`
5. **Edge cases**: Unicode, large payloads, invalid input, transport-specific validation

**TCPS Justification:**
- **Jidoka (Built-in Quality)**: This test suite is the Poka-yoke for protocol compliance - it MUST catch spec violations before they reach production
- **Poka-yoke (Mistake-proofing)**: Type specs exist (lines 33-39) but without tests, we have no runtime verification
- **Kaizen (Continuous Improvement)**: Current coverage is 0% - measurable gap to 80% with clear metrics
- **Andon (Visible Signaling)**: Test failures must be visible and block deployment (quality gate enforcement)

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl` (378 lines)
    - 31 exported API functions
    - 3 exported types (lines 33-39)
    - Key records: `json_rpc_request`, `json_rpc_response`, `json_rpc_notification`, `mcp_error`
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` (476 lines)
    - 35 existing test cases (basic happy paths)
    - Tests are functional but incomplete for 80% coverage target
  - `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` (523 lines)
    - JSON-RPC 2.0 constants (lines 8-66)
    - Error codes and messages (lines 68-106)
    - Protocol method definitions (lines 128-184)
    - Records (lines 312-457)

- **Patterns**:
  - **No gen_server** - Pure functional codec module (stateless)
  - **Record-based data structures**: `#json_rpc_request{}`, `#json_rpc_response{}`, `#json_rpc_notification{}`
  - **Hot path optimization**: Parsing logic extracted to `erlmcp_message_parser.erl` (126 lines)
  - **Message size validation**: Integrated via `erlmcp_message_size:validate_message_size/2` (Gap #45)

- **Tests**: 0% coverage (COVERAGE_REPORT.md:74) - CRITICAL GAP
  - Current test count: 35 tests (require ≥40 more to reach 75+ total)
  - Test file exists: `erlmcp_json_rpc_tests.erl` at `/Users/sac/erlmcp/apps/erlmcp_core/test/`
  - Tests run but don't achieve sufficient coverage

- **Quality**:
  - **Compilation**: ✅ Passes (no syntax errors)
  - **EUnit**: ⚠️ 35 tests pass (insufficient coverage)
  - **Coverage**: ❌ 0% (target: ≥80%, gap: -80%)
  - **Dialyzer**: ✅ Type specs present (lines 33-39 in source)

### Key Files
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl:1-378` - Main codec module with 31 functions
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl:1-476` - Existing 35 tests (insufficient)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_message_parser.erl:1-126` - Hot path parsing (optimized)
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_message_parser_tests.erl:1-351` - 44 parser tests (good reference)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_message_size.erl:1-191` - Message size validation (Gap #45)
- `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl:1-523` - Protocol constants and records
- `/Users/sac/erlmcp/docs/protocol.md:1-150` - MCP protocol documentation

### OTP Patterns Observed
- **Behavior**: None - Pure functional module (no gen_server/gen_fsm)
- **Supervision**: Not applicable (stateless library module)
- **Process Pattern**: N/A (no processes spawned)
- **Test Pattern**: Chicago School TDD (real encoding/decoding, no mocks)
  - Reference: `erlmcp_message_parser_tests.erl:1-351` (44 tests, good example)
  - Tests use real `jsx:encode/1` and `jsx:decode/2` functions
  - No process spawning, pure function testing

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_message_parser` (lines 103, 279) - hot path parsing logic
  - `erlmcp_message_size` (line 99) - message size validation (Gap #45)
  - `erlmcp.hrl` (line 3) - protocol constants and records

- **External Libraries**:
  - `jsx 3.1.0` - JSON encoding/decoding (lines 101, 121, 142, 297)
  - `eunit` - Testing framework (included in OTP)
  - `logger` - Warning for invalid error codes (line 72)

- **OTP Applications**: kernel, stdlib (no OTP behaviors required)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (currently passing)
- [ ] **EUnit**: 100% pass rate with ≥75 total tests (currently 35, need +40)
- [ ] **Common Test**: N/A (using EUnit only)
- [ ] **Coverage**: ≥80% (currently 0%, gap: -80%)
- [ ] **Dialyzer**: 0 warnings (currently passing with type specs)
- [ ] **Xref**: 0 undefined function calls (currently passing)
- [ ] **Performance**: N/A (codec not performance-critical compared to core_ops)

### Patterns to Follow
- **Gen Server Pattern**: N/A (no gen_server)
- **Test Pattern**:
  - Reference: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_message_parser_tests.erl:1-351`
  - 44 comprehensive tests with setup/cleanup
  - Chicago School TDD: Real encoding/decoding, no mocks
  - Example test structure (lines 40-82):
    ```erlang
    parse_request_test_() ->
        {setup,
         fun setup/0,
         fun cleanup/1,
         fun(_) -> [
             ?_test(test_parse_valid_request()),
             ?_test(test_parse_request_with_params())
         ] end}.
    ```
- **Error Handling**:
  - Return `{error, {Reason, Details}}` tuples (see line 35)
  - Use pattern matching for error cases (see lines 110-117)
  - Validate error codes before encoding (lines 69-74)
- **Type Specs**:
  - Lines 33-39: Export types for external validation
  - Pattern: `-spec function_name(InputType) -> ReturnType.`

## Root Cause Analysis (5 Whys)

**Problem**: 0% test coverage on critical JSON-RPC codec module (erlmcp_json_rpc.erl)

1. **Why?** Existing test file has only 35 basic happy path tests - insufficient for 80% coverage target
2. **Why?** Tests focus on encoding/decoding valid messages, missing edge cases and error paths
3. **Why?** No comprehensive test coverage for:
   - Message size validation (Gap #45 integration)
   - Unicode and special characters
   - Large payloads (>1MB)
   - Invalid input types (wrong JSON structures)
   - All 12 error helper functions
   - Transport-specific validation
4. **Why?** Original test development focused on basic functionality, not protocol compliance completeness
5. **ROOT CAUSE**: No systematic approach to JSON-RPC 2.0 spec coverage - tests added ad-hoc without coverage metric tracking

**Solution**: Add 40+ targeted tests to achieve ≥80% coverage:
- 15 tests for encoding edge cases (Unicode, large payloads, field ordering)
- 15 tests for decoding error paths (invalid JSON, missing fields, wrong types)
- 10 tests for message size validation (Gap #45 integration)
- 5+ tests for each error helper function (12 functions × 3 cases = 36 tests)
- Total target: 75+ tests (35 existing + 40+ new)

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Insufficient test coverage after adding 40 tests** | P2 (Medium) | Waste of effort, still below 80% | Use `rebar3 cover --verbose` to measure coverage incrementally, identify uncovered lines before writing tests |
| **Test flakiness due to JSON encoding non-determinism** | P1 (High) | False positives, unreliable CI | Use exact byte comparison, avoid floating point in tests, validate structure not exact JSON string |
| **Message size validation tests fail with default limits** | P2 (Medium) | Tests timeout on large payloads | Mock `erlmcp_message_size` limits or use realistic sizes (<100MB per sys.config) |
| **Unicode tests fail on different Erlang/OTP versions** | P3 (Low) | Platform-specific test failures | Use standard Unicode examples (emoji, accented chars), test UTF-8 encoding |
| **Batch request tests don't cover empty array edge case** | P2 (Medium) | Miss spec compliance requirement (empty batch is invalid per JSON-RPC 2.0) | Explicitly test empty batch returns `{error, {invalid_request, empty_batch}}` (line 265) |
| **Error code validation tests miss edge cases** | P2 (Medium) | Invalid error codes accepted in production | Test all valid codes (line 48: -32700, -32600 to -32603, -32001 to -32012) and invalid codes (0, 9999) |
| **JSON field ordering not validated in encoding tests** | P3 (Low) | Protocol compliance issues | Use `jsx:decode/2` to verify structure, not exact string matching (field order not required by JSON-RPC 2.0 spec) |

## Recommended Manufacturing Approach

**TCPS Methodology:**

### Phase 1: Specification (Requirements with Acceptance Criteria)
**Success Criteria (from item.json):**
- [x] ≥80% code coverage achieved (measured by `rebar3 cover`)
- [ ] ≥40 tests created (currently 35, need 75+ total)
- [ ] All encode/decode functions tested (6 functions)
- [ ] JSON-RPC 2.0 spec compliance verified
- [ ] Error handling paths tested (12 error functions)
- [ ] All tests pass: `rebar3 eunit --module=erlmcp_json_rpc_tests`

### Phase 2: Pseudocode (Algorithm Design BEFORE Coding)
```
For each exported function in erlmcp_json_rpc.erl:
  1. Identify all code paths (branches, pattern matches)
  2. List edge cases (null inputs, wrong types, empty values)
  3. Design tests to cover each path
  4. Verify JSON-RPC 2.0 spec compliance

Test Groups:
1. Encoding (15 tests):
   - Request: valid params, null params, missing params
   - Response: null result, object result, array result
   - Error: valid codes, invalid codes, data variations
   - Notification: with/without params
   - Batch: empty array (should fail), mixed message types

2. Decoding (15 tests):
   - Valid: request, response, notification
   - Invalid: malformed JSON, missing fields, wrong types
   - Size validation: under limit, over limit, exact limit
   - Transport-specific: http, tcp, stdio limits

3. Error Helpers (36 tests):
   - 12 functions × 3 cases = 36 tests
   - Each: basic call, with data, edge case

4. Edge Cases (10+ tests):
   - Unicode: emoji, accented chars, multi-byte
   - Large payloads: 1KB, 1MB, 16MB (limit)
   - Special characters: JSON escaping, null bytes
   - Type coercion: string vs binary vs atom
```

### Phase 3: Architecture (Integration Points and Dependencies)
```
Test Module: erlmcp_json_rpc_tests.erl

Dependencies:
  - eunit (testing framework)
  - jsx (JSON encoding/decoding)
  - erlmcp.hrl (protocol constants)
  - erlmcp_json_rpc (module under test)
  - erlmcp_message_size (for size validation tests)
  - erlmcp_message_parser (for parsing logic tests)

Integration Points:
  1. Message size validation (erlmcp_message_size:validate_message_size/2)
     - Mock or use real validation with configured limits
     - Test Gap #45: Message size limits

  2. Parsing logic (erlmcp_message_parser:parse_json_rpc/1)
     - Already tested in erlmcp_message_parser_tests.erl (44 tests)
     - Focus on codec-level tests, not parser internals

  3. JSON encoding (jsx:encode/1, jsx:decode/2)
     - Real encoding/decoding (no mocks)
     - Chicago School TDD: test real behavior

Test Structure:
  - Setup/cleanup for resource management
  - Test groups by function category
  - Assert on structure (decoded maps), not exact JSON strings
  - Use ?assertMatch for pattern matching
  - Test error paths explicitly
```

### Phase 4: Refinement (Chicago School TDD - Tests FIRST)
**Test Implementation Order:**

**Sprint 1: Encoding Tests (15 tests, target: +15% coverage)**
```erlang
% File: apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl
% Add to existing encode_request_test_() group:
- test_encode_request_null_params()
- test_encode_request_missing_params_field()
- test_encode_request_unicode_method()
- test_encode_request_unicode_params()
- test_encode_response_null_result()
- test_encode_response_large_result()
- test_encode_response_unicode_result()
- test_encode_error_response_null_data()
- test_encode_error_response_map_data()
- test_encode_error_response_binary_data()
- test_encode_error_response_term_data()
- test_encode_notification_null_params()
- test_encode_notification_unicode_params()
- test_encode_batch_empty_array()
- test_encode_batch_mixed_types()
```

**Sprint 2: Decoding Tests (15 tests, target: +20% coverage)**
```erlang
% Add to decode_test_() group:
- test_decode_request_missing_jsonrpc()
- test_decode_request_wrong_version()
- test_decode_request_null_id()
- test_decode_request_string_id()
- test_decode_request_unicode_params()
- test_decode_response_missing_id()
- test_decode_response_null_result()
- test_decode_notification_with_id()
- test_decode_notification_missing_method()
- test_decode_invalid_json_syntax()
- test_decode_invalid_json_type()
- test_decode_message_too_large_default()
- test_decode_message_too_large_http()
- test_decode_message_at_limit()
- test_decode_batch_empty_array()
- test_decode_batch_invalid_item()
```

**Sprint 3: Error Helper Tests (36 tests, target: +25% coverage)**
```erlang
% Add error_creation_extended_test_() group:
% For each of 12 error functions, test:
- test_<function>_basic()
- test_<function>_with_unicode()
- test_<function>_edge_case()

% Functions to test:
1. error_method_not_found/2
2. error_invalid_params/2
3. error_resource_not_found/2
4. error_tool_not_found/2
5. error_prompt_not_found/2
6. error_capability_not_supported/2
7. error_not_initialized/1
8. error_validation_failed/2
9. error_message_too_large/2
10. error_internal/1
11. error_parse/1
12. create_error/3
13. create_error_with_data/4
```

**Sprint 4: Edge Cases & Integration (10+ tests, target: +20% coverage)**
```erlang
% Add edge_case_test_() group:
- test_unicode_emoji_in_method()
- test_unicode_accented_chars_in_params()
- test_unicode_multi_byte_sequences()
- test_large_payload_1mb()
- test_large_payload_16mb_limit()
- test_special_characters_json_escaping()
- test_null_bytes_in_binary()
- test_type_coercion_string_to_binary()
- test_type_coercion_atom_to_binary()
- test_type_coercion_list_to_binary()
- test_message_size_validation_integration()
- test_transport_specific_limits()
```

### Phase 5: Completion (All Quality Gates Passing)
**Quality Validation:**

**Automated:**
```bash
# 1. Run EUnit tests
rebar3 eunit --module=erlmcp_json_rpc_tests --verbose

# 2. Generate coverage report
rebar3 as test cover --verbose

# 3. Check coverage threshold (must be ≥80%)
rebar3 as test cover --verbose | grep erlmcp_json_rpc

# 4. View HTML coverage report
open _build/test/cover/index.html

# 5. Run all tests (ensure no regressions)
rebar3 check
```

**Manual:**
1. **Verify JSON-RPC 2.0 spec compliance**:
   - Check all required fields present (`jsonrpc`, `id`, `method`/`result`/`error`)
   - Verify error codes match spec (Table in erlmcp.hrl:48-66)
   - Confirm field types (id: null/string/number, params: object/array)

2. **Verify edge cases handled**:
   - Unicode: Emoji, accented chars, multi-byte UTF-8
   - Large payloads: Test with 1MB, 16MB payloads
   - Invalid input: Missing fields, wrong types, malformed JSON

3. **Verify error paths**:
   - All 12 error helpers generate valid JSON-RPC error objects
   - Error codes validated (line 69-74: invalid codes → internal error)
   - Error messages are binary strings

**Metrics to Measure:**
- **Coverage %**: Target ≥80% (currently 0%)
- **Test count**: Target ≥75 total (currently 35, need +40)
- **Test pass rate**: Target 100% (currently 100%)
- **Function coverage**: All 31 exported functions tested
- **Line coverage**: All branches covered (if/else, case, pattern matches)

## Open Questions
**NONE** - Research complete. All questions answered:

1. ✅ **What is the current coverage?** 0% (COVERAGE_REPORT.md:74)
2. ✅ **What is the target coverage?** ≥80% (Lean Six Sigma standard)
3. ✅ **What is the gap?** 80 percentage points (0% → 80%)
4. ✅ **How many tests exist?** 35 tests (erlmcp_json_rpc_tests.erl:1-476)
5. ✅ **How many tests are needed?** ≥75 total (40+ more to reach target)
6. ✅ **What functions need testing?** All 31 exported functions
7. ✅ **What edge cases are missing?** Unicode, large payloads, size validation
8. ✅ **What are the error paths?** 12 error helper functions, decode errors
9. ✅ **What is the JSON-RPC 2.0 spec?** Documented in docs/protocol.md:1-150
10. ✅ **What dependencies exist?** jsx, erlmcp_message_parser, erlmcp_message_size
11. ✅ **What is the test pattern?** Chicago School TDD (real encoding/decoding)
12. ✅ **What are the quality gates?** EUnit 100%, Cover ≥80%, Dialyzer 0 warnings

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) → Missing 40+ tests for edge cases and error paths
- [x] Quality gates defined (specific thresholds) → ≥80% coverage, ≥75 tests, 100% pass rate
- [x] OTP patterns understood (behaviors, supervision) → Pure functional module, no OTP behaviors
- [x] Test strategy clear (Chicago School TDD) → Real encoding/decoding, no mocks, structure validation
- [x] Risk assessment complete (severity P0-P3) → 7 risks identified, P1/P2/P3 with mitigations
- [x] No open questions (all research complete) → All 12 questions answered
