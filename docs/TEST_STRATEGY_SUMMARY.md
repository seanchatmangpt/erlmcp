# erlmcp v3 Test Strategy - Quick Reference

**Full Strategy:** See [TEST_STRATEGY_V3.md](./TEST_STRATEGY_V3.md)

---

## TL;DR

**Goal:** Verify MCP 2025-11-25 specification compliance through automated tests using Chicago School TDD (state-based testing, real collaborators, minimal mocking).

**Test Count:** 520+ tests across 5 layers
**Coverage Target:** 85%+ (core), 80%+ (overall)
**Methodology:** Test-first development with real processes
**Quality Gates:** 100% test pass rate + coverage thresholds enforced in CI/CD

---

## Five-Layer Test Architecture

```
┌─────────────────────────────────────────────────┐
│ 5. CHAOS (15 tests)                             │
│    Fault injection, resilience testing          │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ 4. COMPLIANCE (60 tests)                        │
│    MCP 2025-11-25 specification verification    │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ 3. PROPERTY (34 tests)                          │
│    Invariant testing with Proper                │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ 2. INTEGRATION (52 tests)                       │
│    Multi-process coordination (Common Test)     │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ 1. UNIT (434 tests)                             │
│    Module-level testing (EUnit)                 │
└─────────────────────────────────────────────────┘
```

---

## Test Count Breakdown

| Category | Unit | Integration | Property | Compliance | Chaos | **Total** |
|----------|------|-------------|----------|------------|-------|-----------|
| **Core Modules** | 140 | 16 | 15 | - | - | **171** |
| **Transports** | 90 | 12 | 8 | - | - | **110** |
| **Observability** | 60 | 8 | 4 | - | - | **72** |
| **MCP Compliance** | - | - | - | 60 | - | **60** |
| **Chaos Testing** | - | - | - | - | 15 | **15** |
| **Other** | 144 | 16 | 7 | - | - | **167** |
| **TOTAL** | **434** | **52** | **34** | **60** | **15** | **520+** |

---

## Critical MCP Compliance Tests (60 total)

### JSON-RPC 2.0 Compliance (15 tests)
- Request/response structure validation
- ID correlation verification
- Error code compliance (-32700 to -32603)
- UTF-8 encoding, batch requests

### Capability Negotiation (10 tests)
- Initialize handshake flow
- Protocol version matching
- Capability enforcement (resources, tools, prompts)
- Capability change notifications

### Resources System (12 tests)
- Resource listing (uri, name, mime_type)
- Resource reading and templates
- Subscriptions and notifications
- Resource updates (resources/updated, resources/list_changed)

### Tools System (10 tests)
- Tool listing with JSON Schema
- Tool execution with validation
- Error handling (tool not found)
- Tool list change notifications

### Prompts System (8 tests)
- Prompt listing and retrieval
- Argument validation (required arguments)
- Prompt messages format
- Prompt list change notifications

### Transport Compliance (5 tests)
- STDIO line-delimited messages
- TCP framing and reconnection
- HTTP headers and connection pooling
- WebSocket binary frames
- Disconnection cleanup

---

## Coverage Targets by Module Type

| Module Type | Coverage | Rationale |
|------------|----------|-----------|
| **Critical Core** (json_rpc, client, server, registry) | **90-95%** | Protocol compliance essential |
| **Core Infrastructure** | **85-90%** | System stability critical |
| **Transports** | **80-85%** | Well-tested libraries used |
| **Observability** | **75-80%** | Nice-to-have features |
| **Overall** | **≥80%** | Industry standard |

---

## Chicago School TDD Principles

### DO ✅
- **State-based verification** - Assert on observable state changes
- **Real collaborators** - Spawn actual gen_servers, processes
- **Behavior testing** - Test what system does (outputs), not how (internals)
- **Integration by default** - Test components together

### DON'T ❌
- **Mock gen_servers** with meck
- **Verify internal calls** (London School interaction testing)
- **Stub collaborators** unnecessarily
- **Test implementation details**

### Example
```erlang
%% GOOD: Chicago School
test_registry() ->
    application:ensure_all_started(erlmcp_core),  % Real registry
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_server("srv1", ServerPid, #{}),
    {ok, ServerPid} = erlmcp_registry:whereis_server("srv1"),  % Real lookup
    ServerPid ! stop.

%% BAD: London School (DON'T DO THIS)
%% meck:new(erlmcp_registry),
%% meck:expect(erlmcp_registry, register_server, fun(...) -> ok end),
%% ?assertEqual(1, meck:num_calls(erlmcp_registry, register_server, '_')).
```

---

## Test Directory Structure

```
test/
├── unit/                         # EUnit (434 tests)
│   ├── erlmcp_json_rpc_tests.erl
│   ├── erlmcp_client_tests.erl
│   ├── erlmcp_server_tests.erl
│   └── ... (one test file per module)
│
├── integration/                  # Common Test (52 tests)
│   ├── mcp_client_server_SUITE.erl
│   ├── mcp_registry_coordination_SUITE.erl
│   └── fixtures/*.json
│
├── properties/                   # Proper (34 properties)
│   ├── erlmcp_json_rpc_prop.erl
│   └── property_generators.erl
│
├── compliance/                   # MCP spec (60 tests)
│   ├── mcp_jsonrpc_compliance_SUITE.erl
│   ├── mcp_capabilities_SUITE.erl
│   ├── mcp_resources_SUITE.erl
│   ├── mcp_tools_SUITE.erl
│   ├── mcp_prompts_SUITE.erl
│   └── fixtures/*.json
│
├── chaos/                        # Chaos (15 tests)
│   ├── chaos_process_failures_SUITE.erl
│   ├── chaos_resource_exhaustion_SUITE.erl
│   └── chaos_network_SUITE.erl
│
└── helpers/
    ├── test_utils.erl
    └── mock_transport.erl
```

---

## Quality Gates (Pre-Merge)

```
┌────────────────────────────────────┐
│ ✅ Compilation (0 errors)          │
│ ✅ Tests (100% pass)               │
│ ✅ Coverage (≥80% overall)         │
│ ✅ Dialyzer (0 errors)             │
│ ✅ MCP Compliance (60/60 pass)     │
└────────────────────────────────────┘
         ↓
    MERGE ALLOWED
```

**Enforcement:**
- Pre-commit hooks
- CI/CD pipeline gates
- Manual: `./tools/pre-merge-gates.sh`

---

## Test Execution Commands

```bash
# All tests
rebar3 do eunit, ct, proper -c, cover --verbose

# By layer
rebar3 eunit                        # Unit tests
rebar3 ct --dir=test/integration    # Integration tests
rebar3 ct --dir=test/compliance     # MCP compliance tests
rebar3 ct --dir=test/chaos          # Chaos tests
rebar3 proper -c                    # Property tests

# Coverage
rebar3 cover --verbose
open _build/test/cover/index.html

# Individual module
rebar3 eunit --module=erlmcp_json_rpc_tests
```

---

## Implementation Roadmap (12 weeks)

| Phase | Duration | Goal | Deliverable |
|-------|----------|------|-------------|
| **1. Foundation** | Week 1-2 | Fix broken tests | All tests compile |
| **2. Core Tests** | Week 3-5 | Core module coverage | 85%+ core coverage |
| **3. Compliance** | Week 6-7 | MCP spec tests | 60 compliance tests |
| **4. Transports** | Week 8-9 | Transport coverage | 80%+ transport coverage |
| **5. Property/Chaos** | Week 10-11 | Advanced testing | 34 properties, 15 chaos |
| **6. Automation** | Week 12 | Quality gates | CI/CD enforcement |

---

## Success Criteria

| Metric | Target | Status |
|--------|--------|--------|
| Total Tests | 520+ | ❌ → ✅ |
| Overall Coverage | ≥80% | ❌ → ✅ |
| Core Coverage | ≥85% | ❌ → ✅ |
| Critical Coverage | ≥90% | ❌ → ✅ |
| MCP Compliance | 60/60 | ❌ → ✅ |
| Test Pass Rate | 100% | ❌ → ✅ |
| CI/CD Integration | Automated | ❌ → ✅ |

---

## Key References

- **Full Strategy:** [TEST_STRATEGY_V3.md](./TEST_STRATEGY_V3.md)
- **MCP Specification:** MCP 2025-11-25
- **OTP Patterns:** [otp-patterns.md](./otp-patterns.md)
- **Architecture:** [architecture.md](./architecture.md)
- **CLAUDE.md:** [../CLAUDE.md](../CLAUDE.md)

---

**Last Updated:** 2026-01-30
**Version:** 1.0.0
**Status:** ✅ READY FOR IMPLEMENTATION
