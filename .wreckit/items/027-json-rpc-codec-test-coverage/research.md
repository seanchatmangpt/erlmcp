# Research: JSON-RPC Codec Test Coverage

**Date**: 2025-01-29
**Item**: 027-json-rpc-codec-test-coverage
**Section**: coverage
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
erlmcp_json_rpc.erl has ZERO coverage despite processing EVERY message in and out of the system. A bug here breaks the entire protocol.

**Motivation:** This is the protocol implementation layer - all JSON-RPC encoding/decoding flows through this module. JSON-RPC 2.0 spec compliance is critical for interoperability.

**Success criteria:**
- Test file created: apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl
- All encode/decode functions tested
- JSON-RPC 2.0 spec compliance verified
- Error handling paths tested (parse error, invalid request, etc.)
- Batch operations tested
- Coverage: ≥80%
- All tests pass: rebar3 eunit --module=erlmcp_json_rpc_tests

**Technical constraints:**
- JSON Encoding Tests - Valid JSON output, correct field ordering
- JSON Decoding Tests - Valid JSON input, error handling
- Schema Validation Tests - JSON-RPC 2.0 spec compliance
- Edge Case Tests - Unicode, special characters, large payloads
- Error Path Tests - Invalid JSON, missing fields, wrong types

**Signals:** priority: critical, urgency: P0 - BLOCKS ALL PRODUCTION WORK

### Quality Gate Status
- **Gate Type**: Coverage, Test
- **Current State**: 0% coverage (tests exist but may not achieve ≥80%), unknown if all functions tested
- **Target State**: ≥80% coverage, 100% of encode/decode functions tested
- **Gap**: 80 percentage points minimum (from 0% to ≥80%)

## Summary

**Manufacturing Objective**: Complete test coverage for `erlmcp_json_rpc.erl`, the critical JSON-RPC 2.0 protocol codec layer. This module encodes and decodes EVERY message flowing through the MCP system - requests, responses, notifications, errors, and batch operations. Zero tolerance for protocol violations or encoding bugs.

**Technical Approach**: Implement comprehensive EUnit tests following Chicago School TDD principles (real message encoding/decoding, no mocks). Test all 31 exported functions covering encoding (8 functions), decoding (4 functions), batch operations (3 functions), error helpers (11 functions), and error creation (5 functions). Validate JSON-RPC 2.0 specification compliance including field ordering, data types, error codes, and edge cases (Unicode, large payloads, malformed input).

**TCPS Justification**: This is **Jidoka** (built-in quality) at the protocol layer. A bug here breaks ALL message flow. Current test file exists (`erlmcp_json_rpc_tests.erl`, 476 lines) but coverage is unknown. Must verify ≥80% line and branch coverage covering all encode/decode paths, error handling, and batch operations. Tests serve as **Poka-yoke** (mistake-proofing) by validating JSON-RPC 2.0 spec compliance automatically. **Andon** visibility: coverage failures block deployment.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_core/src/erlmcp_json_rpc.erl` (378 lines) - Main codec module
  - `apps/erlmcp_core/src/erlmcp_message_parser.erl` (126 lines) - Hot path parser
  - `apps/erlmcp_core/include/erlmcp.hrl` (523 lines) - Constants, records, type specs
  - `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` (476 lines) - Test file EXISTS
  - `apps/erlmcp_core/test/erlmcp_message_parser_tests.erl` (351 lines) - Parser tests
- **Patterns**: Pure functional codec (no gen_server), JSON-RPC 2.0 protocol implementation, jsx for JSON encoding/decoding
- **Tests**: Test file EXISTS with 476 lines covering encoding, decoding, errors, notifications, batch operations. Coverage percentage UNKNOWN.
- **Quality**: Cannot confirm if ≥80% coverage threshold is met. Test file comprehensive but needs coverage verification.

### Key Files
- `apps/erlmcp_core/src/erlmcp_json_rpc.erl:1-378` - JSON-RPC 2.0 codec with 31 exported functions
- `apps/erlmcp_core/src/erlmcp_message_parser.erl:1-126` - Hot path optimization (parse_json_rpc, validate_jsonrpc_version, parse_by_type)
- `apps/erlmcp_core/include/erlmcp.hrl:1-523` - JSON-RPC 2.0 constants, error codes, record definitions
- `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl:1-476` - Existing EUnit test suite
- `apps/erlmcp_core/test/erlmcp_message_parser_tests.erl:1-351` - Parser test reference

### OTP Patterns Observed
- **Behavior**: None (pure functional module, no gen_server)
- **Supervision**: Not applicable (stateless codec)
- **Process Pattern**: None (library module)
- **Test Pattern**: Chicago School TDD - real JSON encoding/decoding with jsx, no mocks. EUnit test generators with setup/cleanup.

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_message_parser` (parse_json_rpc, validate_jsonrpc_version) - lines 37-43, 90-117
  - `erlmcp_message_size` (validate_message_size) - line 99 (Gap #45 integration)
- **External Libraries**:
  - `jsx 3.1.0` - JSON encoding/decoding
  - `jesse 1.8.1` - JSON Schema validation (not used in codec)
  - `gproc 0.9.0` - Registry (not used in codec)
- **OTP Applications**: kernel, stdlib (for basic types)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (current: unknown)
- [ ] **EUnit**: 100% pass rate (current: unknown, test file exists)
- [ ] **Common Test**: N/A (unit tests only)
- [ ] **Coverage**: ≥80% (current: UNKNOWN - **CRITICAL GAP**)
- [ ] **Dialyzer**: 0 warnings (current: unknown)
- [ ] **Xref**: 0 undefined function calls (current: unknown)
- [ ] **Performance**: N/A (codec not performance-critical path, message_parser is optimized)

### Patterns to Follow
- **Test Pattern**: `apps/erlmcp_core/test/erlmcp_message_parser_tests.erl:1-351` - Comprehensive EUnit tests with setup/cleanup, test generators, Chicago School TDD
- **Error Handling**: Lines 109-117 in erlmcp_json_rpc.erl - Try-catch for jsx:decode errors, return {error, {parse_error, _}}
- **Type Specs**: Lines 33-38 in erlmcp_json_rpc.erl - -type() for json_rpc_message(), decode_result(), batch_request()
- **Validation Pattern**: Lines 99-117 in erlmcp_json_rpc.erl - Size validation (Gap #45) before parsing

## Root Cause Analysis (5 Whys)

**Problem**: erlmcp_json_rpc.erl has ZERO coverage according to item description, but test file EXISTS with 476 lines.

1. **Why?** Test file was created (erlmcp_json_rpc_tests.erl) but coverage measurement was never run or documented.
2. **Why?** Item description states "ZERO coverage" but this is outdated - test file exists with comprehensive tests.
3. **Why?** Item 027 was created before tests were written, but item state not updated to reflect current state.
4. **Why?** No automated coverage reporting in CI/CD to catch this discrepancy.
5. **ROOT CAUSE**: Missing **Kaizen** (continuous improvement) metrics - coverage not measured, tracked, or enforced automatically. Item state not updated after implementation.

**Solution**: Run actual coverage measurement (`rebar3 cover --module=erlmcp_json_rpc`), quantify gap, ensure ≥80% threshold met, update item state, add coverage to CI/CD as quality gate.

## Function Coverage Analysis

### erlmcp_json_rpc.erl - 31 Exported Functions

**Encoding Functions (8):**
1. `encode_request/3` (lines 45-52) - Encode JSON-RPC request
2. `encode_response/2` (lines 54-60) - Encode success response
3. `encode_error_response/3` (lines 62-64) - Encode error without data
4. `encode_error_response/4` (lines 66-80) - Encode error with data
5. `encode_notification/2` (lines 82-88) - Encode notification (no id)
6. `encode_batch/1` (lines 139-142) - Encode batch request array
7. `encode_message/1` (internal, line 294-297) - Base encoding function
8. `build_message_map/1` (lines 299-320) - Convert record to map for JSON

**Decoding Functions (4):**
1. `decode_message/1` (lines 90-92) - Decode single message (default transport)
2. `decode_message/2` (lines 94-117) - Decode with size validation
3. `decode_batch/1` (lines 119-137) - Decode batch array
4. `is_batch_request/1` (lines 144-153) - Detect batch vs single message

**Error Helper Functions (11):**
1. `error_method_not_found/2` (lines 185-188) - Method not found error (-32601)
2. `error_invalid_params/2` (lines 191-198) - Invalid params error (-32602)
3. `error_resource_not_found/2` (lines 201-204) - Resource not found (-32001)
4. `error_tool_not_found/2` (lines 207-210) - Tool not found (-32002)
5. `error_prompt_not_found/2` (lines 213-216) - Prompt not found (-32003)
6. `error_capability_not_supported/2` (lines 219-222) - Capability not supported (-32004)
7. `error_not_initialized/1` (lines 225-227) - Not initialized error (-32005)
8. `error_validation_failed/2` (lines 230-237) - Validation failed (-32007)
9. `error_internal/1` (lines 240-242) - Internal error (-32603)
10. `error_parse/1` (lines 245-247) - Parse error (-32700)
11. `error_message_too_large/2` (lines 250-256) - Message too large (-32012, Gap #45)

**Error Creation Functions (5):**
1. `create_error/3` (lines 155-161) - Create mcp_error record
2. `create_error_with_data/4` (lines 163-170) - Create error with data map
3. `validate_error_code/1` (lines 176-178) - Validate error code in list
4. `build_error_object/3` (lines 339-372) - Build error object map

**Batch Processing Functions (3):**
1. `parse_batch/1` (lines 262-271) - Parse batch array
2. `parse_batch_requests/2` (lines 273-288) - Process batch with error continuation

**Internal Helper Functions:**
1. `encode_id/1` (lines 322-325) - Encode ID (null, binary, integer)
2. `maybe_add_params/2` (lines 327-331) - Add params field if not undefined
3. `add_result_or_error/3` (lines 333-337) - Add result or error to response

### Current Test Coverage (from erlmcp_json_rpc_tests.erl)

**Tested Functions (verified):**
- ✅ `encode_request/3` - Lines 38-84 (5 tests)
- ✅ `encode_response/2` - Lines 104-139 (5 tests)
- ✅ `encode_error_response/3,4` - Lines 163-192 (3 tests)
- ✅ `encode_notification/2` - Lines 264-285 (3 tests)
- ✅ `decode_message/1,2` - Lines 306-354 (6 tests)
- ✅ `decode_batch/1` - Lines 390-406 (1 test)
- ✅ `is_batch_request/1` - Lines 373-379 (2 tests)
- ✅ `encode_batch/1` - Lines 381-388 (1 test)
- ✅ `error_method_not_found/2` - Lines 194-201 (1 test)
- ✅ `error_invalid_params/2` - Lines 203-210 (1 test)
- ✅ `error_resource_not_found/2` - Lines 212-219 (1 test)
- ✅ `error_tool_not_found/2` - Lines 221-228 (1 test)
- ✅ `error_prompt_not_found/2` - Lines 230-237 (1 test)
- ✅ `error_not_initialized/1` - Lines 444-447 (1 test)
- ✅ `error_validation_failed/2` - Lines 449-453 (1 test)
- ✅ `error_capability_not_supported/2` - Lines 455-459 (1 test)
- ✅ `error_message_too_large/2` - Lines 461-465 (1 test)
- ✅ `error_internal/1` - Lines 467-470 (1 test)
- ✅ `error_parse/1` - Lines 472-475 (1 test)
- ✅ `create_error/3` - Lines 429-435 (1 test)
- ✅ `create_error_with_data/4` - Lines 437-442 (1 test)
- ✅ `validate_error_code/1` - Lines 239-246 (1 test)

**Untested Internal Functions:**
- ❓ `encode_message/1` (internal, called by all encode functions)
- ❓ `build_message_map/1` (internal, called by encode_message)
- ❓ `encode_id/1` (internal helper)
- ❓ `maybe_add_params/2` (internal helper)
- ❓ `add_result_or_error/3` (internal helper)
- ❓ `build_error_object/3` (internal, called by encode_error_response/4)
- ❓ `parse_batch_requests/2` (internal, called by parse_batch)

**Coverage Gap Analysis:**
- All 31 exported functions have tests
- Internal helper functions may not be fully covered (need coverage report)
- Edge cases may not be covered (Unicode, large payloads, special characters)
- Error handling paths need verification (try-catch blocks in decode_message/2, decode_batch/1)

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Coverage <80% threshold** | P0 | Blocks deployment, quality gate failure | Run `rebar3 cover --module=erlmcp_json_rpc`, measure actual coverage, add tests if needed |
| **JSON-RPC 2.0 spec violations** | P0 | Protocol non-compliance, interoperability failure | Add spec compliance tests: field ordering, required fields, error codes, data types |
| **Error handling paths untested** | P1 | Silent failures, incorrect error responses | Test all try-catch blocks: invalid JSON, malformed messages, size limits (Gap #45) |
| **Internal functions uncovered** | P2 | Coverage <80% despite testing all exports | Add direct tests or verify coverage via export testing |
| **Unicode/encoding edge cases** | P2 | Binary encoding failures with non-ASCII | Test Unicode strings, special characters, emojis, large payloads |
| **Batch operations edge cases** | P2 | Batch parsing failures with empty/invalid arrays | Test empty batch, mixed valid/invalid requests, large batches |
| **Message size validation (Gap #45)** | P1 | Missing size limit enforcement | Test decode_message/2 with size validation, error_message_too_large/2 |
| **Field ordering violations** | P3 | JSON-RPC spec requires specific order | Verify JSON output field order with jsx encode tests |
| **ID type validation** | P2 | Invalid ID types break protocol | Test null, binary, integer, invalid ID types |
| **Params type validation** | P2 | Invalid params type (not map/list) | Test undefined, map, list, invalid params |

**Severity Definitions:**
- **P0 (Critical)**: BLOCKS deployment - MUST fix immediately
- **P1 (High)**: Major quality gap - MUST fix before release
- **P2 (Medium)**: Important but not blocking
- **P3 (Low)**: Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** ✅ - Requirements defined (item.json: success criteria)
2. **Pseudocode** - N/A (tests already written, need verification)
3. **Architecture** ✅ - Understood (pure functional codec, no processes)
4. **Refinement** - Verify coverage ≥80%, add missing tests if needed
5. **Completion** - All quality gates passing (compile, eunit, cover, dialyzer, xref)

**Implementation Strategy:**

**Phase 1: Coverage Measurement (Jidoka - Built-in Quality)**
```bash
# Run coverage analysis
cd /Users/sac/erlmcp
rebar3 clean
rebar3 compile
rebar3 eunit --module=erlmcp_json_rpc_tests
rebar3 cover --module=erlmcp_json_rpc
```

**Phase 2: Gap Analysis (Kaizen - Continuous Improvement)**
- Review coverage report: `_build/test/cover/erlmcp_json_rpc.coverdata`
- Identify uncovered lines/branches
- Quantify gap: current % vs 80% target
- Create test cases for uncovered code

**Phase 3: Test Enhancement (Poka-yoke - Mistake-Proofing)**
- Add JSON-RPC 2.0 spec compliance tests
- Test edge cases: Unicode, large payloads, special characters
- Test error paths: invalid JSON, missing fields, wrong types
- Test batch operations: empty batch, mixed valid/invalid
- Test message size validation (Gap #45)

**Phase 4: Quality Validation (Andon - Visible Signaling)**
- Run full test suite: `rebar3 eunit --module=erlmcp_json_rpc_tests`
- Verify 100% pass rate
- Verify ≥80% coverage
- Run dialyzer: `rebar3 dialyzer -D TEST`
- Run xref: `rebar3 xref`

**Quality Validation Commands:**
```bash
# Automated validation
rebar3 compile                          # Compilation gate
rebar3 eunit --module=erlmcp_json_rpc_tests  # Test gate
rebar3 cover --module=erlmcp_json_rpc    # Coverage gate (≥80%)
rebar3 dialyzer -D TEST                  # Type checking gate
rebar3 xref                              # Cross-reference gate
```

**Metrics to Measure:**
- **Line Coverage**: % of executable lines tested (target: ≥80%)
- **Branch Coverage**: % of conditional branches tested (target: ≥70%)
- **Function Coverage**: % of functions called (target: 100% of exports)
- **Test Pass Rate**: % of tests passing (target: 100%)
- **Spec Compliance**: JSON-RPC 2.0 requirements met (target: 100%)

## JSON-RPC 2.0 Specification Compliance

**Required Message Structure:**
```json
// Request
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {}
}

// Response (Success)
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {}
}

// Response (Error)
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32600,
    "message": "Invalid Request",
    "data": null
  }
}

// Notification (no id)
{
  "jsonrpc": "2.0",
  "method": "initialized",
  "params": {}
}

// Batch Request
[
  {"jsonrpc": "2.0", "id": 1, "method": "method1", "params": {}},
  {"jsonrpc": "2.0", "id": 2, "method": "method2", "params": {}}
]
```

**Error Codes (JSON-RPC 2.0 + MCP Extensions):**
- `-32700` - Parse error
- `-32600` - Invalid request
- `-32601` - Method not found
- `-32602` - Invalid params
- `-32603` - Internal error
- `-32001` - Resource not found (MCP)
- `-32002` - Tool not found (MCP)
- `-32003` - Prompt not found (MCP)
- `-32004` - Capability not supported (MCP)
- `-32005` - Not initialized (MCP)
- `-32011` - Tool description too long (MCP, Gap #40)
- `-32012` - Message too large (MCP, Gap #45)

**Field Validation Rules:**
1. `jsonrpc` field MUST be "2.0"
2. `id` field MUST be present in request/response (null, string, number)
3. `method` field MUST be present in request/notification (string)
4. `params` field is optional (object or array)
5. `result` OR `error` MUST be present in response (not both)
6. `error` object MUST have `code` (number) and `message` (string)
7. `error.data` is optional (any type)

## Open Questions
**NONE** - Research complete. All questions answered through code analysis.

## Manufacturing Checklist
- [x] Root cause identified (tests exist, coverage measurement missing)
- [x] Quality gates defined (≥80% coverage, 100% pass rate)
- [x] OTP patterns understood (pure functional, no processes)
- [x] Test strategy clear (Chicago School TDD, real JSON encoding/decoding)
- [x] Risk assessment complete (10 risks identified with severity P0-P3)
- [x] No open questions (all research complete)
- [ ] **NEXT STEP**: Run coverage measurement to verify ≥80% threshold
- [ ] Add missing tests if coverage <80%
- [ ] Verify all quality gates pass
- [ ] Update item state to "complete" when coverage ≥80% confirmed
