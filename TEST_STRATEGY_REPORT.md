# erlmcp v3 Test Strategy - Design Report

**Date:** 2026-01-30
**Agent:** Erlang Test Engineer
**Status:** ✅ COMPLETE - Ready for Implementation

---

## Executive Summary

Comprehensive test strategy designed for erlmcp v3 rewrite with **520+ test cases** across **5 testing layers** targeting **85%+ coverage** for core modules and **80%+ overall coverage**. Strategy focuses on **MCP 2025-11-25 specification compliance verification** through automated tests using **Chicago School TDD methodology** (state-based testing, real collaborators, no mocking).

---

## Deliverables

### 1. Primary Documents Created

**Full Strategy Document:**
- **File:** `/home/user/erlmcp/docs/TEST_STRATEGY_V3.md`
- **Size:** 1,589 lines
- **Sections:** 13 major sections + 3 appendices
- **Content:**
  - Current state analysis (broken tests, gaps)
  - 5-layer test architecture (Unit, Integration, Property, Compliance, Chaos)
  - MCP compliance test specifications (60 test cases)
  - Chicago School TDD methodology and examples
  - Module-by-module test plans (113 modules)
  - Chaos/fault injection strategy (15 scenarios)
  - Coverage targets and enforcement
  - Test organization and directory structure
  - Tooling, CI/CD, quality gates
  - 12-week implementation roadmap
  - Test templates (EUnit, CT, Proper)

**Quick Reference Summary:**
- **File:** `/home/user/erlmcp/docs/TEST_STRATEGY_SUMMARY.md`
- **Purpose:** Quick-reference guide (TL;DR version)
- **Content:**
  - Test count breakdown by layer
  - MCP compliance test overview
  - Coverage targets table
  - Chicago School DO/DON'T guide
  - Test execution commands
  - Quality gates checklist

---

## Test Strategy Overview

### Test Layer Architecture (5 Layers)

```
┌──────────────────────────────────────────────────────────────┐
│ LAYER 5: CHAOS (15 tests)                                    │
│ Fault injection, network partitions, resource exhaustion     │
│ Tools: Custom chaos modules                                  │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ LAYER 4: COMPLIANCE (60 tests)                               │
│ MCP 2025-11-25 specification verification                    │
│ Tools: Common Test suites, protocol harness                  │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ LAYER 3: PROPERTY (34 tests)                                 │
│ Invariant testing, generative testing                        │
│ Tools: Proper (property-based testing)                       │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ LAYER 2: INTEGRATION (52 tests)                              │
│ Multi-process, registry coordination, transport integration  │
│ Tools: Common Test (CT)                                      │
└──────────────────────────────────────────────────────────────┘
                           ↓
┌──────────────────────────────────────────────────────────────┐
│ LAYER 1: UNIT (434 tests)                                    │
│ Module-level, function-level, API surface testing            │
│ Tools: EUnit                                                 │
└──────────────────────────────────────────────────────────────┘
```

### Test Count Breakdown

| Category | Unit | Integration | Property | Compliance | Chaos | **Total** |
|----------|------|-------------|----------|------------|-------|-----------|
| **Core Modules** | 140 | 16 | 15 | - | - | **171** |
| **Transports** | 90 | 12 | 8 | - | - | **110** |
| **Observability** | 60 | 8 | 4 | - | - | **72** |
| **MCP Compliance** | - | - | - | 60 | - | **60** |
| **Chaos Testing** | - | - | - | - | 15 | **15** |
| **Other Modules** | 144 | 16 | 7 | - | - | **167** |
| **TOTAL** | **434** | **52** | **34** | **60** | **15** | **520+** |

---

## Key Design Decisions

### 1. Chicago School TDD Methodology

**Rationale:** Better alignment with Erlang/OTP philosophy of real processes, supervision, and message passing.

**Core Principles:**
- ✅ **State-based verification** - Assert on observable state changes
- ✅ **Real collaborators** - Spawn actual gen_servers, not mocks
- ✅ **Behavior testing** - Test what system does (outputs), not how (internals)
- ✅ **Integration by default** - Test components together

**Anti-patterns avoided:**
- ❌ No mocking gen_servers with `meck`
- ❌ No interaction verification (London School)
- ❌ No stubbing real collaborators

**Example:**
```erlang
%% Chicago School: Real gen_server, state-based assertion
test_registry() ->
    application:ensure_all_started(erlmcp_core),  % Real registry
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_server("srv1", ServerPid, #{}),
    {ok, ServerPid} = erlmcp_registry:whereis_server("srv1"),  % Real lookup
    ServerPid ! stop.
```

---

### 2. MCP Compliance as Executable Tests

**Rationale:** Protocol specification encoded directly as test cases ensures continuous compliance verification.

**Coverage:**
1. **JSON-RPC 2.0 Compliance** (15 tests)
   - Message structure, ID correlation, error codes, encoding

2. **Capability Negotiation** (10 tests)
   - Initialize handshake, version matching, capability enforcement

3. **Resources System** (12 tests)
   - Listing, reading, templates, subscriptions, notifications

4. **Tools System** (10 tests)
   - Listing, execution, schema validation, error handling

5. **Prompts System** (8 tests)
   - Listing, retrieval, argument validation, messages format

6. **Transport Compliance** (5 tests)
   - STDIO, TCP, HTTP, WebSocket protocol compliance

**Total:** 60 MCP compliance test cases

---

### 3. Layered Testing Strategy

**Rationale:** Different test types serve different purposes; layering provides comprehensive coverage.

| Layer | Tool | Purpose | Example |
|-------|------|---------|---------|
| **Unit** | EUnit | Fast feedback, module isolation | `erlmcp_json_rpc_tests.erl` |
| **Integration** | Common Test | Multi-process coordination | `mcp_client_server_SUITE.erl` |
| **Property** | Proper | Invariant verification | `erlmcp_json_rpc_prop.erl` |
| **Compliance** | Common Test | MCP spec adherence | `mcp_resources_SUITE.erl` |
| **Chaos** | Custom | Resilience testing | `chaos_network_SUITE.erl` |

---

### 4. Coverage Targets by Module Type

| Module Type | Coverage Target | Rationale |
|------------|----------------|-----------|
| **Critical Core** (json_rpc, client, server, registry) | **90-95%** | Protocol compliance essential |
| **Core Infrastructure** (supervision, session, routing) | **85-90%** | System stability critical |
| **Transports** (stdio, tcp, http, ws) | **80-85%** | Well-tested libraries (gun, ranch) |
| **Observability** (metrics, tracing, chaos) | **75-80%** | Nice-to-have features |
| **TCPS** (optional quality system) | **70-75%** | Optional component |
| **Overall Target** | **≥80%** | Industry standard for production |

---

### 5. Test-First Development Workflow

**Red → Green → Refactor Cycle:**

```
┌─────────────────────────────────────────────────────┐
│ PHASE 1: RED (Write Failing Test)                  │
│ 1. Read MCP spec requirement                       │
│ 2. Write test with real processes                  │
│ 3. Assert on observable state                      │
│ 4. Run test → FAIL (not implemented)               │
└─────────────────────────────────────────────────────┘
                       ↓
┌─────────────────────────────────────────────────────┐
│ PHASE 2: GREEN (Make Test Pass)                    │
│ 1. Implement minimum code                          │
│ 2. Use real collaborators                          │
│ 3. Run test → PASS                                 │
└─────────────────────────────────────────────────────┘
                       ↓
┌─────────────────────────────────────────────────────┐
│ PHASE 3: REFACTOR (Improve Design)                 │
│ 1. Refactor implementation                         │
│ 2. Keep tests green                                │
│ 3. Run tests → All passing                         │
└─────────────────────────────────────────────────────┘
```

---

## Critical Module Test Plans

### erlmcp_json_rpc.erl (Critical - 95%+ coverage)

**Purpose:** JSON-RPC 2.0 encoding/decoding

**Test Cases:** 25 unit tests + 3 property tests
- Encoding (10 tests): request, response, error, notification, batch
- Decoding (10 tests): validation, error handling, UTF-8, large payloads
- Roundtrip (5 tests): request, response, error, notification, batch

**Properties:**
- `prop_request_roundtrip()` - Encode/decode preserves structure
- `prop_response_roundtrip()` - Response format maintained
- `prop_error_code_in_range()` - Error codes within spec

**Acceptance Criteria:**
- ✅ 25/25 unit tests pass
- ✅ 3 properties hold (300+ generated test cases)
- ✅ 95%+ code coverage
- ✅ 0 dialyzer warnings

---

### erlmcp_client.erl (Critical - 90%+ coverage)

**Purpose:** MCP client gen_server with request correlation

**Test Cases:** 30 unit tests + 4 integration tests
- Initialization (5 tests): handshake, capabilities, timeout
- Request correlation (8 tests): ID tracking, concurrent requests, timeouts
- Resource operations (6 tests): list, read, subscribe, notifications
- Tool operations (5 tests): list, call, validation, errors
- Prompt operations (3 tests): list, get, arguments
- Error handling (3 tests): transport disconnect, invalid responses

**Integration Tests:**
- Full client-server lifecycle
- Reconnection after disconnect
- Server restart handling
- Multiple clients coordination

**Acceptance Criteria:**
- ✅ 30/30 unit tests pass
- ✅ 4/4 integration tests pass
- ✅ 90%+ code coverage
- ✅ Concurrency safety verified

---

### erlmcp_server.erl (Critical - 90%+ coverage)

**Purpose:** MCP server gen_server with resource/tool/prompt management

**Test Cases:** 35 unit tests
- Resource management (12 tests): add, list, read, subscribe, templates
- Tool management (10 tests): add with schema, call, validation, errors
- Prompt management (8 tests): add, get with arguments, templates
- Capability management (5 tests): negotiation, enforcement, changes

**Acceptance Criteria:**
- ✅ 35/35 unit tests pass
- ✅ 90%+ code coverage
- ✅ Schema validation tested
- ✅ Notification system verified

---

### erlmcp_registry.erl (Critical - 90%+ coverage)

**Purpose:** gproc-based process registration and message routing

**Test Cases:** 25 unit tests + 4 integration tests
- Server registration (8 tests): register, lookup, death cleanup
- Transport registration (8 tests): register, bind, routing
- Message routing (5 tests): delivery, error handling, load test
- Cleanup (4 tests): process death, state recovery

**Integration Tests:**
- Full routing lifecycle
- Server-transport binding
- Multiple servers/transports coordination
- Registry restart recovery

**Acceptance Criteria:**
- ✅ 25/25 unit tests pass
- ✅ 4/4 integration tests pass
- ✅ 90%+ code coverage
- ✅ 1000+ concurrent operations tested

---

## Chaos Testing Strategy

### 15 Chaos Scenarios

**Process Failures:**
1. Registry process death → Supervisor restart
2. Server crash during request → Error propagation
3. Client crash during response → Cleanup
4. Transport crash during send → Connection recovery
5. Supervisor restart cascade → System stability

**Resource Exhaustion:**
6. Memory exhaustion → Bounded refusal
7. Process limit exhaustion → Graceful degradation
8. Port limit exhaustion → Error handling
9. ETS table limit → Fail-fast

**Network Failures:**
10. Network partition → Reconnection
11. Slow network timeout → Timeout handling
12. TCP connection refused → Retry logic

**Data Corruption:**
13. Invalid JSON injection → Parse error
14. Truncated message → Error recovery

**Timing Issues:**
15. Request timeout storm → System resilience

**Bounded Refusal Philosophy:**
- System MUST refuse to degrade beyond safe limits
- Fail-fast when limits exceeded
- Recovery MUST complete within 5 seconds
- No silent corruption

---

## Test Organization

### Directory Structure

```
test/
├── unit/                         # EUnit (434 tests)
│   ├── erlmcp_json_rpc_tests.erl
│   ├── erlmcp_client_tests.erl
│   ├── erlmcp_server_tests.erl
│   ├── erlmcp_registry_tests.erl
│   └── ... (one per module)
│
├── integration/                  # Common Test (52 tests)
│   ├── mcp_client_server_SUITE.erl
│   ├── mcp_registry_coordination_SUITE.erl
│   ├── mcp_full_lifecycle_SUITE.erl
│   └── fixtures/*.json
│
├── properties/                   # Proper (34 properties)
│   ├── erlmcp_json_rpc_prop.erl
│   ├── erlmcp_transport_prop.erl
│   └── property_generators.erl
│
├── compliance/                   # MCP spec (60 tests)
│   ├── mcp_jsonrpc_compliance_SUITE.erl
│   ├── mcp_capabilities_SUITE.erl
│   ├── mcp_resources_SUITE.erl
│   ├── mcp_tools_SUITE.erl
│   ├── mcp_prompts_SUITE.erl
│   ├── mcp_transports_SUITE.erl
│   └── fixtures/*.json
│
├── chaos/                        # Chaos (15 tests)
│   ├── chaos_process_failures_SUITE.erl
│   ├── chaos_resource_exhaustion_SUITE.erl
│   ├── chaos_network_SUITE.erl
│   └── chaos_timing_SUITE.erl
│
└── helpers/
    ├── test_utils.erl
    ├── mock_transport.erl
    └── test_harness.erl
```

---

## Quality Gates

### Pre-Merge Requirements

```
┌────────────────────────────────────────────┐
│ GATE 1: Compilation                        │
│ ✅ rebar3 compile (0 errors)               │
└────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────┐
│ GATE 2: Tests                              │
│ ✅ rebar3 eunit (100% pass)                │
│ ✅ rebar3 ct (100% pass)                   │
│ ✅ rebar3 proper -c (all properties hold)  │
└────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────┐
│ GATE 3: Coverage                           │
│ ✅ Overall ≥80%                            │
│ ✅ Core modules ≥85%                       │
│ ✅ Critical modules ≥90%                   │
└────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────┐
│ GATE 4: Code Quality                       │
│ ✅ rebar3 dialyzer (0 errors)              │
│ ✅ rebar3 xref (0 undefined)               │
└────────────────────────────────────────────┘
                  ↓
┌────────────────────────────────────────────┐
│ GATE 5: MCP Compliance                     │
│ ✅ test/compliance/* (60/60 pass)          │
└────────────────────────────────────────────┘
                  ↓
              MERGE ✅
```

**Enforcement:**
- Pre-commit hooks
- CI/CD pipeline gates
- Manual script: `./tools/pre-merge-gates.sh`

---

## Implementation Roadmap (12 weeks)

| Phase | Duration | Goal | Deliverable |
|-------|----------|------|-------------|
| **Phase 1: Foundation** | Week 1-2 | Fix broken tests, setup infrastructure | All tests compile |
| **Phase 2: Core Tests** | Week 3-5 | Core module coverage | 85%+ core coverage |
| **Phase 3: Compliance** | Week 6-7 | MCP spec test suite | 60 compliance tests |
| **Phase 4: Transports** | Week 8-9 | Transport module coverage | 80%+ transport coverage |
| **Phase 5: Property/Chaos** | Week 10-11 | Advanced testing | 34 properties, 15 chaos |
| **Phase 6: Automation** | Week 12 | Quality gate enforcement | CI/CD integration |

**Total Effort:** 12-19 days (96-152 hours)

---

## Success Criteria

### Quantitative Metrics

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Total Test Count** | 0 (broken) | 520+ | ❌ → ✅ |
| **Overall Coverage** | Unknown | ≥80% | ❌ → ✅ |
| **Core Coverage** | Unknown | ≥85% | ❌ → ✅ |
| **Critical Coverage** | Unknown | ≥90% | ❌ → ✅ |
| **MCP Compliance Tests** | 0 | 60+ | ❌ → ✅ |
| **Chaos Tests** | 0 | 15+ | ❌ → ✅ |
| **Property Tests** | 0 | 34+ | ❌ → ✅ |
| **Test Pass Rate** | N/A | 100% | ❌ → ✅ |
| **CI/CD Integration** | Manual | Automated | ❌ → ✅ |

### Qualitative Success Indicators

- ✅ **Test-First Development Adopted** - All new features written TDD-style
- ✅ **MCP Compliance Verified** - Protocol adherence proven by tests
- ✅ **Chicago School TDD Used** - Real processes, state-based assertions
- ✅ **No Flaky Tests** - All tests deterministic and reliable
- ✅ **Fast Feedback Loop** - Full test suite runs in < 5 minutes
- ✅ **Comprehensive Chaos Tests** - Resilience to failures proven
- ✅ **Quality Gates Enforced** - Cannot merge without passing tests

---

## Test Execution Commands

```bash
# Full test suite (recommended for CI/CD)
rebar3 do eunit, ct, proper -c, cover --verbose

# By layer
rebar3 eunit                        # Unit tests (434)
rebar3 ct --dir=test/integration    # Integration tests (52)
rebar3 ct --dir=test/compliance     # MCP compliance tests (60)
rebar3 ct --dir=test/chaos          # Chaos tests (15)
rebar3 proper -c                    # Property tests (34)

# Coverage report
rebar3 cover --verbose
open _build/test/cover/index.html

# Individual module
rebar3 eunit --module=erlmcp_json_rpc_tests

# Quality gates (manual)
./tools/pre-merge-gates.sh
```

---

## Current State Issues (To Be Fixed in Phase 1)

### Broken Tests (20 files)

**Compilation Errors (6 files):**
1. `erlmcp_cancellation_tests.erl` - Syntax errors in spawn calls
2. `erlmcp_logging_tests.erl` - Missing Proper include
3. `erlmcp_transport_compliance_tests.erl` - Missing includes
4. `mcp_compliance/transport_compliance_tests.erl` - Invalid JSON
5. `mcp_compliance/tools_compliance_tests.erl` - Unbound variable
6. `skipped/erlmcp_vm_limits_tests.erl` - Invalid function calls

**Dependency Issues:**
- `ctx v0.6.0` - Source not recognizable
- `jobs v0.10.0` - Failed to read .app.src

**Test Duplication:**
- Tools testing: 3 overlapping suites
- JSON-RPC testing: 4 overlapping suites
- Server capabilities: 2 overlapping suites

**Fix Effort:** 8-16 hours

---

## Files Delivered

1. **Full Strategy Document** (1,589 lines)
   - `/home/user/erlmcp/docs/TEST_STRATEGY_V3.md`
   - Comprehensive test strategy with 13 sections + 3 appendices
   - Detailed test plans for all 113 modules
   - MCP compliance test specifications
   - Chicago School TDD methodology
   - 12-week implementation roadmap

2. **Quick Reference Summary** (250 lines)
   - `/home/user/erlmcp/docs/TEST_STRATEGY_SUMMARY.md`
   - TL;DR version for quick reference
   - Test count breakdowns
   - Coverage targets table
   - Quality gates checklist
   - Test execution commands

3. **This Report**
   - `/home/user/erlmcp/TEST_STRATEGY_REPORT.md`
   - Executive summary for stakeholders

---

## Next Steps

### Immediate Actions (Phase 1 - Week 1-2)

1. **Review Documents**
   - Read full strategy: `docs/TEST_STRATEGY_V3.md`
   - Read quick reference: `docs/TEST_STRATEGY_SUMMARY.md`

2. **Fix Broken Tests**
   - Fix 6 compilation errors
   - Resolve dependency issues
   - Move broken tests to `test/broken/`

3. **Setup Infrastructure**
   - Create `test_utils.erl`
   - Organize test directory structure
   - Configure rebar3 test profile
   - Set up CI/CD workflow

4. **Start TDD Workflow**
   - Begin with `erlmcp_json_rpc_tests.erl`
   - Follow Red → Green → Refactor cycle
   - Use Chicago School principles (real processes, no mocks)

### Approval Required

This test strategy is ready for:
- ✅ **Technical Review** - Erlang team validation
- ✅ **Stakeholder Approval** - Sign-off on 12-week timeline
- ✅ **Resource Allocation** - Assign test engineer(s)
- ✅ **Implementation Start** - Begin Phase 1 (Foundation)

---

## Questions & Clarifications

**Q: Why Chicago School TDD instead of London School (mocking)?**
A: Chicago School better aligns with Erlang/OTP philosophy of real processes, supervision, and message passing. Mocking gen_servers defeats the purpose of testing OTP behaviors.

**Q: Why 520+ tests for 113 modules?**
A: Average 4-5 tests per module covers happy path + edge cases + error conditions. Critical modules (json_rpc, client, server) have 25-35 tests each.

**Q: Can we reduce test count to speed up development?**
A: Not recommended. MCP compliance requires comprehensive testing. Reducing tests increases risk of protocol violations and production bugs.

**Q: What if we can't reach 80% coverage?**
A: Coverage exemptions allowed for generated code, debug-only code, and deprecated functions. 80% target is achievable with 520+ tests.

**Q: How long to run full test suite?**
A: Target: < 5 minutes. Unit tests run in parallel (EUnit), integration tests sequential (CT), property tests configurable (Proper).

---

## Conclusion

Comprehensive test strategy designed with:
- ✅ 520+ test cases across 5 layers
- ✅ 85%+ coverage targets for core modules
- ✅ 60 MCP compliance tests
- ✅ Chicago School TDD methodology
- ✅ 12-week implementation roadmap
- ✅ Automated quality gates

**Status:** ✅ READY FOR REVIEW & APPROVAL

**Documents:**
- Full Strategy: `/home/user/erlmcp/docs/TEST_STRATEGY_V3.md`
- Quick Reference: `/home/user/erlmcp/docs/TEST_STRATEGY_SUMMARY.md`
- This Report: `/home/user/erlmcp/TEST_STRATEGY_REPORT.md`

---

**Agent:** Erlang Test Engineer
**Date:** 2026-01-30
**Version:** 1.0.0
