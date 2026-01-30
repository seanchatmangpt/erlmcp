# MCP 2025-11-25 Compliance Test Plan - Executive Summary

**Version:** 1.0.0
**Date:** 2026-01-30
**Status:** Ready for Implementation
**Test Philosophy:** Chicago School TDD

---

## Overview

Comprehensive test plan for achieving MCP 2025-11-25 specification compliance in erlmcp. This plan defines 300+ test cases across 15 test suites, targeting ≥80% code coverage with Chicago School TDD methodology (real processes, no mocks, state-based verification).

---

## Current State Analysis

### Existing Test Coverage

**Test Files:** 38 modules (EUnit, Common Test, Proper, Benchmarks)
```
✅ JSON-RPC encoding/decoding
✅ Client operations
✅ Registry operations
✅ Basic resource/tool tests
✅ Authentication, caching, rate limiting
✅ Circuit breaker, connection monitoring
✅ Schema validation
✅ Benchmarks (core ops, network, stress, chaos)
```

**Core Modules:** 65 source files requiring coverage
- erlmcp_core: 30+ modules
- erlmcp_transports: 15+ modules
- erlmcp_observability: 10+ modules

### Coverage Gaps (Critical P0)

**Missing Tests:**
1. ❌ Server initialization state machine (initialize → initialized → shutdown)
2. ❌ Protocol version validation (2025-11-25 enforcement)
3. ❌ Pre-initialization request rejection (?MCP_ERROR_NOT_INITIALIZED)
4. ❌ Tool metadata validation (description ≤10000 chars, deprecated flag)
5. ❌ Resource URI validation (RFC 3986, path traversal prevention)
6. ❌ Content type validation (text, image, audio, video)
7. ❌ Notification rate limiting (tools/list_changed)
8. ❌ Progress token tracking
9. ❌ Graceful shutdown with cleanup
10. ❌ MCP error code compliance (-32001 to -32099)

**Current Coverage:** ~40-50% (estimated)
**Target Coverage:** ≥80% overall, ≥85% for core modules

---

## Test Plan Overview

### 15 New/Enhanced Test Suites

| # | Test Suite | Type | Tests | Priority | Coverage Target |
|---|------------|------|-------|----------|-----------------|
| 1 | erlmcp_server_initialization_tests | EUnit | 25 | P0 | 90% |
| 2 | erlmcp_server_state_machine_tests | EUnit | 20 | P0 | 90% |
| 3 | erlmcp_json_rpc_compliance_tests | EUnit | 50 | P0 | 95% |
| 4 | erlmcp_server_tools_tests | EUnit | 40 | P1 | 85% |
| 5 | erlmcp_server_resources_tests | EUnit | 45 | P1 | 85% |
| 6 | erlmcp_server_prompts_tests | EUnit | 30 | P1 | 85% |
| 7 | erlmcp_capabilities_negotiation_tests | EUnit | 25 | P1 | 85% |
| 8 | erlmcp_notification_handler_tests | EUnit | 20 | P1 | 85% |
| 9 | erlmcp_progress_tracking_tests | EUnit | 15 | P1 | 85% |
| 10 | erlmcp_uri_validator_tests | EUnit | 25 | P0 | 95% |
| 11 | erlmcp_content_type_validation_tests | EUnit | 20 | P1 | 85% |
| 12 | erlmcp_error_code_compliance_tests | EUnit | 30 | P0 | 90% |
| 13 | erlmcp_backwards_compatibility_tests | EUnit | 15 | P2 | 80% |
| 14 | erlmcp_integration_compliance_SUITE | CT | 25 | P1 | N/A |
| 15 | erlmcp_property_tests | Proper | 20 | P2 | N/A |

**Total:** 405 test cases (300+ new, 105 enhanced)

---

## Implementation Timeline

### Phase 1: Foundation (Weeks 1-2) - P0 Tests

**Week 1:**
- Setup test infrastructure ✅
- Server initialization tests (25) - P0
- State machine tests (20) - P0
- **Target:** 60% coverage

**Week 2:**
- JSON-RPC compliance tests (50) - P0
- URI validator tests (25) - P0
- Error code compliance tests (30) - P0
- **Target:** 70% coverage

### Phase 2: Core Functionality (Weeks 3-4) - P1 Tests

**Week 3:**
- Tools tests (40) - P1
- Resources tests (45) - P1
- **Target:** 75% coverage

**Week 4:**
- Prompts tests (30) - P1
- Capabilities negotiation tests (25) - P1
- Notification handler tests (20) - P1
- **Target:** 80% coverage

### Phase 3: Advanced Features (Weeks 5-6) - P1/P2 Tests

**Week 5:**
- Progress tracking tests (15) - P1
- Content type validation tests (20) - P1
- **Target:** 82% coverage

**Week 6:**
- Integration tests (25) - P1
- Backwards compatibility tests (15) - P2
- **Target:** 85% coverage

### Phase 4: Polish (Weeks 7-8) - Property Tests

**Week 7:**
- Property tests (20) - P2
- Coverage gap analysis
- **Target:** 85% coverage

**Week 8:**
- Final polish and documentation
- Quality gate validation
- **Target:** ≥85% coverage, all tests passing

---

## Chicago School TDD Principles

### Core Principles

**DO (✅):**
- Use real gen_servers: `erlmcp_server:start_link/2`
- Use real processes for transports
- Assert on observable state (API results)
- Test behaviors and outputs
- Integrate components together

**DON'T (❌):**
- Use meck for gen_servers
- Verify internal method calls
- Stub collaborators
- Test implementation details

### Example Test Pattern

```erlang
%% GOOD: Chicago School TDD
initialize_first_request_test() ->
    %% Setup: Start real server
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Exercise: Send initialize request (real message)
    InitReq = build_initialize_request(),
    Response = send_request(Server, InitReq),

    %% Verify: Check observable state
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assert(maps:is_key(<<"result">>, Response)),

    %% Verify: Server state changed (via API)
    {ok, Initialized} = erlmcp_server:is_initialized(Server),
    ?assertEqual(true, Initialized),

    %% Cleanup
    erlmcp_server:stop(Server).
```

---

## Quality Gates (Pre-Completion)

Before marking ANY test suite as complete:

```bash
# 1. Compilation (MUST pass)
✅ make compile
# Expected: 0 errors

# 2. Tests (MUST pass 100%)
✅ rebar3 eunit --module=<module>_tests
# Expected: X/X passed (0 failures)

# 3. Coverage (MUST be ≥80%)
✅ rebar3 cover --verbose
# Expected: ≥80% overall, ≥85% for core

# 4. Dialyzer (SHOULD be 0 warnings)
✅ rebar3 dialyzer

# 5. Xref (SHOULD be 0 issues)
✅ rebar3 xref
```

**Verification Report Format:**
```
✅ Tests: X/X passed (EUnit: Y, CT: Z)
✅ Quality: Compile clean, format verified
✅ Coverage: X% overall (Core: Y%)
✅ Chicago School: Real processes ✅, No mocks ✅
✅ Edge Cases: [list]

Ready for review: <module>_tests.erl
```

---

## Key Test Scenarios by Area

### 1. Protocol Initialization (P0)

```
✅ Server starts with correct capabilities
✅ Initialize MUST be first request
✅ Double initialize returns error (not crash)
✅ Non-initialize requests before init return ?MCP_ERROR_NOT_INITIALIZED
✅ Protocol version validated (2025-11-25)
✅ Capability exchange complete
```

### 2. JSON-RPC 2.0 Compliance (P0)

```
✅ Request has jsonrpc="2.0", id, method
✅ Response has jsonrpc="2.0", id, result XOR error
✅ Notification has method, no id
✅ Error has code, message, optional data
✅ Standard error codes (-32700 to -32603)
✅ MCP error codes (-32001 to -32099)
```

### 3. Resource Management (P1)

```
✅ Static resources registered
✅ Resource templates with URI patterns
✅ URI validation (RFC 3986)
✅ Path traversal prevention (../)
✅ Resource subscriptions
✅ Change notifications
```

### 4. Tool Execution (P1)

```
✅ Tool registration with metadata
✅ Description length validation (≤10000)
✅ Deprecated flag support
✅ JSON Schema validation
✅ Progress token tracking
✅ Content type validation (text, image, audio, video)
```

### 5. Prompt Handling (P1)

```
✅ Prompt registration
✅ Required/optional arguments
✅ Argument validation
✅ Message format (role, content)
```

### 6. Error Handling (P0)

```
✅ All MCP error codes tested
✅ Error response structure validated
✅ Error messages descriptive
✅ No crashes on invalid input
```

### 7. State Machine (P0)

```
✅ Phase transitions validated
✅ Pre-init requests rejected
✅ Post-init requests accepted
✅ Graceful shutdown
```

### 8. Edge Cases (P1)

```
✅ Concurrent operations
✅ Large messages
✅ Malformed input
✅ Process crashes
✅ Timeout handling
```

---

## Success Metrics

### Quantitative

```
✅ Test Count: 300+ test cases
✅ Coverage: ≥80% overall, ≥85% core
✅ Pass Rate: 100% (0 failures)
✅ Compilation: 0 errors, 0 warnings
✅ Dialyzer: 0 type warnings
✅ Xref: 0 cross-reference issues
```

### Qualitative

```
✅ MCP 2025-11-25 compliance: 100%
✅ Chicago School TDD: All tests
✅ Security: Path traversal, injection tested
✅ Error handling: All error codes tested
✅ Documentation: All suites documented
```

---

## Quick Start

### 1. Setup

```bash
cd /home/user/erlmcp
make compile && make test-core
```

### 2. Start with P0 Tests

```bash
# Create first test file
vim apps/erlmcp_core/test/erlmcp_server_initialization_tests.erl

# Implement 25 tests from plan

# Run tests
rebar3 eunit --module=erlmcp_server_initialization_tests

# Check coverage
rebar3 cover --verbose
```

### 3. Follow the Timeline

- **Week 1:** Foundation (60% coverage)
- **Week 2-4:** Core (80% coverage)
- **Week 5-8:** Advanced (85% coverage)

---

## Documentation

**Full Test Plan:**
- [MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md](./MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md) - Comprehensive 405 test plan

**Quick Start:**
- [MCP_COMPLIANCE_TEST_QUICK_START.md](./MCP_COMPLIANCE_TEST_QUICK_START.md) - Implementation guide

**Test Helpers:**
- [erlmcp_test_helpers.erl](../../apps/erlmcp_core/test/erlmcp_test_helpers.erl) - Reusable utilities

**Supporting Docs:**
- [MCP Protocol](../protocol.md) - Protocol specification
- [Testing Strategy](./MCP_SERVER_TESTING_STRATEGY.md) - Original strategy
- [Edge Cases](../mcp-edge-case-testing-strategy.md) - Edge case testing

---

## Deliverables

### Week 1-2 (Foundation)
- [ ] erlmcp_server_initialization_tests.erl (25 tests)
- [ ] erlmcp_server_state_machine_tests.erl (20 tests)
- [ ] erlmcp_json_rpc_compliance_tests.erl (50 tests)
- [ ] erlmcp_uri_validator_tests.erl (25 tests)
- [ ] erlmcp_error_code_compliance_tests.erl (30 tests)
- [ ] Coverage: ≥70%

### Week 3-4 (Core)
- [ ] erlmcp_server_tools_tests.erl (40 tests)
- [ ] erlmcp_server_resources_tests.erl (45 tests)
- [ ] erlmcp_server_prompts_tests.erl (30 tests)
- [ ] erlmcp_capabilities_negotiation_tests.erl (25 tests)
- [ ] erlmcp_notification_handler_tests.erl (20 tests)
- [ ] Coverage: ≥80%

### Week 5-6 (Advanced)
- [ ] erlmcp_progress_tracking_tests.erl (15 tests)
- [ ] erlmcp_content_type_validation_tests.erl (20 tests)
- [ ] erlmcp_integration_compliance_SUITE.erl (25 tests)
- [ ] erlmcp_backwards_compatibility_tests.erl (15 tests)
- [ ] Coverage: ≥85%

### Week 7-8 (Polish)
- [ ] erlmcp_property_tests.erl (20 properties)
- [ ] Coverage gap analysis and fixes
- [ ] Documentation updates
- [ ] Final quality gates
- [ ] Coverage: ≥85%

---

## Resources and Support

**Code References:**
- `apps/erlmcp_core/src/erlmcp_server.erl` - Server implementation
- `apps/erlmcp_core/src/erlmcp_json_rpc.erl` - JSON-RPC encoding
- `apps/erlmcp_core/src/erlmcp_capabilities.erl` - Capability negotiation
- `include/erlmcp.hrl` - Type definitions and error codes

**Existing Tests:**
- `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` - JSON-RPC examples
- `apps/erlmcp_core/test/erlmcp_client_tests.erl` - Client test patterns
- `apps/erlmcp_core/test/erlmcp_resource_tests.erl` - Resource examples

**Testing Tools:**
- EUnit: http://erlang.org/doc/apps/eunit/chapter.html
- Common Test: http://erlang.org/doc/apps/common_test/chapter.html
- Proper: https://proper-testing.github.io/

---

## Next Steps

### Immediate (Today)

1. **Review test plan:**
   - Read [MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md](./MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md)
   - Review test scenarios for Phase 1

2. **Setup environment:**
   ```bash
   make compile && make test-core
   ```

3. **Start implementation:**
   - Create `erlmcp_server_initialization_tests.erl`
   - Implement first 5 tests
   - Run and verify

### This Week

1. Complete Phase 1 tests (150 tests)
2. Target 70% coverage
3. All P0 security tests passing

### This Month

1. Complete Phase 1-2 (250 tests)
2. Target 80% coverage
3. All core functionality tested

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-30
**Status:** Ready for Implementation

**LET'S BUILD COMPREHENSIVE TEST COVERAGE!**
