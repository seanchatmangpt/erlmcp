# Test Coverage Report & Roadmap
## erlmcp - Erlang/OTP MCP SDK

**Generated:** 2026-01-30
**Current Overall Coverage:** ~10%
**Target Coverage:** 80%
**Total Source Lines:** 49,698 (87 core + 29 transport + 29 observability + 14 validation modules)

---

## Executive Summary

### Current State

The erlmcp project has **significant test coverage gaps** across all four applications. While 239 test modules exist, coverage remains at approximately 10% due to:

1. **Missing test files entirely** for 64 modules (48% of codebase)
2. **Incomplete test coverage** in existing test files (many test stubs or partial coverage)
3. **Compilation errors** blocking test execution (validation app has syntax errors)
4. **Focus on integration tests** over unit tests (CT suites vs EUnit)

### Key Findings

| Application | Modules | Test Files | Coverage | Untested Modules |
|-------------|---------|------------|----------|------------------|
| erlmcp_core | 87 | 108 | ~15% | 45 modules (52%) |
| erlmcp_transports | 29 | 29 | ~8% | 21 modules (72%) |
| erlmcp_observability | 29 | 14 | ~5% | 21 modules (72%) |
| erlmcp_validation | 14 | 13 | ~3% | 9 modules (64%) |

### Critical Risk Areas

**HIGH RISK** (Core protocol - 0% direct coverage):
- `erlmcp_client.erl` (38KB, 899 LOC) - MCP client implementation
- `erlmcp_server.erl` (90KB, 2,042 LOC) - MCP server implementation
- `erlmcp_json_rpc.erl` (46KB, 1,088 LOC) - JSON-RPC 2.0 protocol
- `erlmcp_registry.erl` (21KB, 530 LOC) - Central message routing

**MEDIUM RISK** (Transport layer):
- `erlmcp_transport_tcp.erl` - TCP transport
- `erlmcp_transport_http.erl` - HTTP transport
- `erlmcp_transport_ws.erl` - WebSocket transport
- `erlmcp_transport_sse.erl` - SSE transport

**LOW RISK** (Support modules):
- Supervision trees (`*_sup.erl`, `*_app.erl`)
- Utility modules (validators, helpers)

---

## Module-by-Module Breakdown

### Phase 1: Quick Wins (Low Effort, High Impact)

**Target:** Reach 40% coverage in 2-3 weeks

#### 1. Simple Utility Modules (1-2 hours each)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_uri_validator` | ~200 | 1h | HIGH | Low |
| `erlmcp_path_canonicalizer` | ~150 | 1h | HIGH | Low |
| `erlmcp_message_size` | ~100 | 1h | HIGH | Low |
| `erlmcp_request_id` | ~150 | 1h | HIGH | Low |
| `erlmcp_paginat` | ~200 | 2h | HIGH | Low |
| `erlmcp_logging` | ~150 | 1h | MEDIUM | Low |
| `erlmcp_test_sync` | ~100 | 0.5h | MEDIUM | Low |

**Total Effort:** 8.5 hours
**Impact:** +8% coverage
**Complexity:** Pure functions, no gen_server, easy to test

#### 2. Simple Data Structures (2-4 hours each)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_cache` | ~400 | 3h | HIGH | Low |
| `erlmcp_rate_limiter` | ~500 | 4h | HIGH | Medium |
| `erlmcp_circuit_breaker` | ~300 | 3h | HIGH | Medium |
| `erlmcp_connection_limiter` | ~400 | 3h | HIGH | Medium |
| `erlmcp_refusal` | ~600 | 4h | HIGH | Low |
| `erlmcp_capabilities` | ~300 | 2h | MEDIUM | Low |

**Total Effort:** 19 hours
**Impact:** +12% coverage
**Complexity:** gen_server with simple state, well-defined behavior

#### 3. Simple Transport Components (2-3 hours each)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_http_header_validator` | ~200 | 2h | MEDIUM | Low |
| `erlmcp_origin_validator` | ~150 | 2h | MEDIUM | Low |
| `erlmcp_tls_validation` | ~300 | 3h | MEDIUM | Medium |
| `erlmcp_security_headers` | ~150 | 1h | MEDIUM | Low |
| `erlmcp_pool_strategy` | ~250 | 2h | MEDIUM | Low |

**Total Effort:** 10 hours
**Impact:** +5% coverage
**Complexity:** Validation logic, no external dependencies

**Phase 1 Total: 37.5 hours (~5 work days) → +25% coverage**

---

### Phase 2: Core Protocol (Medium Effort, Critical Impact)

**Target:** Reach 60% coverage in 3-4 weeks

#### 4. JSON-RPC Protocol Layer (8-12 hours)

| Module | Lines | Functions | Est. Effort | Priority | Risk |
|--------|-------|-----------|-------------|----------|------|
| `erlmcp_json_rpc` | 1,088 | 50+ | 12h | CRITICAL | High |
| `erlmcp_message_parser` | ~400 | 15+ | 6h | CRITICAL | High |
| `erlmcp_schema_validator` | ~600 | 20+ | 8h | HIGH | Medium |

**Total Effort:** 26 hours
**Impact:** +8% coverage
**Complexity:** Complex parsing, error handling, edge cases
**Approach:** Property-based testing with Proper + EUnit

#### 5. Registry & Routing (6-8 hours)

| Module | Lines | API Functions | Est. Effort | Priority | Risk |
|--------|-------|---------------|-------------|----------|------|
| `erlmcp_registry` | 530 | 30+ | 8h | CRITICAL | High |
| `erlmcp_registry_utils` | ~200 | 10+ | 3h | HIGH | Low |
| `erlmcp_registry_dist` | ~400 | 15+ | 5h | HIGH | Medium |

**Total Effort:** 16 hours
**Impact:** +6% coverage
**Complexity:** gen_server with gproc, distributed systems

#### 6. Session Management (8-10 hours)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_session` | ~100 | 2h | HIGH | Low |
| `erlmcp_session_manager` | ~400 | 6h | HIGH | Medium |
| `erlmcp_subscription` | ~300 | 4h | MEDIUM | Medium |

**Total Effort:** 12 hours
**Impact:** +4% coverage
**Complexity:** gen_server with state management

**Phase 2 Total: 54 hours (~7 work days) → +18% coverage**

---

### Phase 3: Client & Server (High Effort, Maximum Impact)

**Target:** Reach 75% coverage in 4-6 weeks

#### 7. MCP Client Implementation (20-30 hours)

| Module | Lines | Functions | Clauses | Est. Effort | Priority | Risk |
|--------|-------|-----------|---------|-------------|----------|------|
| `erlmcp_client` | 899 | 52 | 42 | 30h | CRITICAL | Critical |
| `erlmcp_client_transport` | ~300 | 15+ | 8h | HIGH | High |

**Total Effort:** 38 hours
**Impact:** +10% coverage
**Complexity:** Complex gen_server, transport integration, state machine
**Testing Approach:**
- Unit tests for each API function
- State machine testing for initialization phase
- Integration tests with mock transports
- Property-based testing for request correlation

#### 8. MCP Server Implementation (40-60 hours)

| Module | Lines | Functions | Clauses | Est. Effort | Priority | Risk |
|--------|-------|-----------|---------|-------------|----------|------|
| `erlmcp_server` | 2,042 | 105 | 100+ | 60h | CRITICAL | Critical |
| `erlmcp_server_sup` | ~200 | 8h | HIGH | Medium | |
| `erlmcp_tool` | ~400 | 10h | HIGH | Medium | |
| `erlmcp_resource` | ~350 | 8h | HIGH | Medium | |
| `erlmcp_prompt_template` | ~300 | 6h | HIGH | Medium | |

**Total Effort:** 92 hours
**Impact:** +15% coverage
**Complexity:** Very large gen_server, multiple handlers, complex state
**Testing Approach:**
- Comprehensive unit tests for all 105+ functions
- Handler function testing (tools, resources, prompts)
- State machine testing for initialization phase
- Property-based testing for message handling
- Integration tests with real MCP clients

**Phase 3 Total: 130 hours (~16 work days) → +25% coverage**

---

### Phase 4: Transport Layer (Medium-High Effort)

**Target:** Reach 80% coverage in 3-4 weeks

#### 9. Transport Implementations (40-50 hours)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_transport_tcp` | ~800 | 12h | CRITICAL | High |
| `erlmcp_transport_stdio` | ~400 | 8h | HIGH | Medium |
| `erlmcp_transport_http` | ~600 | 10h | HIGH | High |
| `erlmcp_transport_ws` | ~700 | 10h | HIGH | High |
| `erlmcp_transport_sse` | ~500 | 8h | HIGH | High |
| `erlmcp_transport_pool` | ~400 | 6h | MEDIUM | Medium |
| `erlmcp_transport_registry` | ~350 | 6h | HIGH | Medium |

**Total Effort:** 60 hours
**Impact:** +12% coverage
**Complexity:** gen_server + protocol handlers + network I/O
**Testing Approach:**
- Mock ranch/gun for HTTP tests
- Real TCP sockets for integration tests
- Property-based testing for message framing

#### 10. Transport Infrastructure (10-15 hours)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_transport_behavior` | ~150 | 4h | HIGH | Low |
| `erlmcp_transport_health` | ~300 | 5h | MEDIUM | Low |
| `erlmcp_transport_validation` | ~250 | 4h | MEDIUM | Low |
| `erlmcp_pool_manager` | ~350 | 6h | MEDIUM | Medium |

**Total Effort:** 19 hours
**Impact:** +4% coverage

**Phase 4 Total: 79 hours (~10 work days) → +16% coverage**

---

### Phase 5: Observability & Validation (Optional for 80%)

**Target:** Exceed 80% coverage in 2-3 weeks

#### 11. Observability Modules (30-40 hours)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_otel` | ~400 | 8h | MEDIUM | Low |
| `erlmcp_tracing` | ~350 | 6h | MEDIUM | Low |
| `erlmcp_metrics` | ~300 | 6h | MEDIUM | Low |
| `erlmcp_health_monitor` | ~400 | 8h | MEDIUM | Low |
| `erlmcp_chaos` | ~600 | 12h | LOW | Low |
| `erlmcp_recovery_manager` | ~450 | 10h | LOW | Low |
| `erlmcp_dashboard_server` | ~350 | 8h | LOW | Low |
| `erlmcp_debugger` | ~300 | 6h | LOW | Low |
| `erlmcp_profiler` | ~250 | 4h | LOW | Low |
| `erlmcp_memory_analyzer` | ~300 | 6h | LOW | Low |

**Total Effort:** 74 hours
**Impact:** +8% coverage
**Priority:** LOW - Nice-to-have, not critical for protocol correctness

#### 12. Validation Modules (15-20 hours)

| Module | Lines | Est. Effort | Priority | Risk |
|--------|-------|-------------|----------|------|
| `erlmcp_protocol_validator` | ~500 | 8h | MEDIUM | Low |
| `erlmcp_transport_validator` | ~400 | 6h | MEDIUM | Low |
| `erlmcp_security_validator` | ~350 | 6h | MEDIUM | Low |
| `erlmcp_performance_validator` | ~300 | 4h | LOW | Low |
| `erlmcp_test_client` | ~400 | 6h | HIGH | Medium |

**Total Effort:** 30 hours
**Impact:** +4% coverage
**Priority:** MEDIUM - Important for compliance testing

**Phase 5 Total: 104 hours (~13 work days) → +12% coverage**

---

## Priority Order Summary

### Critical Path to 80% Coverage

**Total Estimated Effort: 375 hours (~47 work days)**

| Phase | Duration | Coverage | Modules | Focus |
|-------|----------|----------|---------|-------|
| **Phase 1** | 1 week | 10% → 35% | 19 | Quick wins, utilities |
| **Phase 2** | 2 weeks | 35% → 53% | 9 | JSON-RPC, registry, sessions |
| **Phase 3** | 4 weeks | 53% → 78% | 8 | Client, server, handlers |
| **Phase 4** | 3 weeks | 78% → 94% | 11 | Transports, infrastructure |
| **Phase 5** | 3 weeks | 94% → 100% | 19 | Observability, validation |

**Accelerated Path to 80%: Skip Phase 5**
- **Duration:** 10 weeks (2.5 months)
- **Coverage:** 94% (exceeds 80% target)
- **Focus:** Core protocol + transports only

---

## Testing Strategy

### Unit Testing (EUnit)

**For:** Pure functions, simple modules
- **Tools:** EUnit
- **Coverage Goal:** 90%+ line coverage
- **Pattern:** Chicago School TDD (test FIRST, no mocks)

Example structure:
```erlang
%% Test module naming: <module>_tests.erl
-module(erlmcp_uri_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test per function
validate_uri_test_() ->
    [
        ?_assertMatch({ok, _}, erlmcp_uri_validator:validate(<<"mcp://test">>)),
        ?_assertMatch({error, _}, erlmcp_uri_validator:validate(<<"invalid">>))
    ].

%% Property-based tests
prop_validate_uri() ->
    ?FORALL(Uri, binary(),
        case erlmcp_uri_validator:validate(Uri) of
            {ok, _} -> true;
            {error, _} -> true
        end).
```

### Integration Testing (Common Test)

**For:** gen_servers, transport modules, end-to-end workflows
- **Tools:** Common Test
- **Coverage Goal:** 70%+ scenario coverage
- **Pattern:** Real processes, no mocks

Example structure:
```erlang
%% Test suite naming: <module>_SUITE.erl
-module(erlmcp_client_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test suite callbacks
suite() -> [{timetrap, {seconds, 30}}].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

%% Test cases
init_phase_test(Config) ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}),
    ?assertEqual(pre_initialization, get_phase(Client)),
    ok = erlmcp_client:stop(Client).

tool_call_test(Config) ->
    {ok, Client} = setup_client(Config),
    {ok, Result} = erlmcp_client:call_tool(Client, <<"test">>, #{}),
    ?assertMatch(#{result := _}, Result),
    cleanup_client(Client).
```

### Property-Based Testing (Proper)

**For:** Complex state machines, message parsing, data structures
- **Tools:** Proper
- **Coverage Goal:** Invariants and edge cases
- **Pattern:** Generate random inputs, verify invariants

Example:
```erlang
-module(erlmcp_json_rpc_proper_tests).
-include_lib("proper/include/proper.hrl").

prop_encode_decode_roundtrip() ->
    ?FORALL(Request, json_rpc_request(),
        begin
            Encoded = erlmcp_json_rpc:encode_request(
                maps:get(id, Request),
                maps:get(method, Request),
                maps:get(params, Request)
            ),
            Decoded = erlmcp_json_rpc:decode_message(Encoded),
            Request =:= Decoded
        end).

json_rpc_request() ->
    ?LET({Id, Method, Params},
        {pos_integer(), binary(), map()},
        #{id => Id, method => Method, params => Params}).
```

---

## Risk Assessment

### High-Risk Untested Modules

| Module | Risk Level | Impact | Mitigation |
|--------|------------|--------|------------|
| `erlmcp_client` | CRITICAL | Client apps fail | Test in Phase 3, parallel development |
| `erlmcp_server` | CRITICAL | Server apps fail | Test in Phase 3, dedicate senior developer |
| `erlmcp_json_rpc` | HIGH | Protocol violations | Property-based tests, Phase 2 |
| `erlmcp_registry` | HIGH | Message routing failures | Property-based tests, Phase 2 |
| `erlmcp_transport_tcp` | MEDIUM | Network issues | Integration tests, Phase 4 |

### Coverage Gaps by Functionality

| Functionality | Coverage | Risk | Priority |
|---------------|----------|------|----------|
| JSON-RPC encoding/decoding | 20% | HIGH | Phase 2 |
| MCP client lifecycle | 5% | CRITICAL | Phase 3 |
| MCP server handlers | 3% | CRITICAL | Phase 3 |
| Transport framing | 10% | MEDIUM | Phase 4 |
| Session management | 15% | MEDIUM | Phase 2 |
| Authentication | 10% | MEDIUM | Phase 2 |
| Rate limiting | 25% | LOW | Phase 1 |
| Error handling | 5% | HIGH | All phases |

---

## Timeline Projection

### Optimistic Timeline (10 weeks to 80%)

**Assumptions:**
- 1 full-time engineer dedicated to testing
- Existing tests are updated/enhanced (not rewritten)
- No blocking bugs or refactoring needed
- Test environment already set up

| Week | Phase | Deliverables | Cumulative Coverage |
|------|-------|--------------|---------------------|
| 1 | Phase 1 | 7 utility modules | 18% |
| 2 | Phase 1 | 6 data structure modules | 30% |
| 3 | Phase 2 | JSON-RPC parser | 38% |
| 4 | Phase 2 | Registry modules | 44% |
| 5 | Phase 2 | Session management | 48% |
| 6-7 | Phase 3 | Client implementation | 58% |
| 8-9 | Phase 3 | Server implementation (Part 1) | 68% |
| 10 | Phase 3 | Server implementation (Part 2) | 78% |

**Total: 10 weeks to exceed 80% target**

### Realistic Timeline (14 weeks to 80%)

**Assumptions:**
- 1 full-time engineer with other responsibilities
- Some refactoring needed for testability
- Bug fixes and test maintenance included

| Week | Phase | Deliverables | Cumulative Coverage |
|------|-------|--------------|---------------------|
| 1-2 | Phase 1 | Utility modules | 18% |
| 3-4 | Phase 1 | Data structures | 30% |
| 5-6 | Phase 2 | JSON-RPC + registry | 42% |
| 7-8 | Phase 2 | Sessions + auth | 50% |
| 9-11 | Phase 3 | Client implementation | 65% |
| 12-14 | Phase 3 | Server implementation | 80% |

**Total: 14 weeks (3.5 months) to reach 80%**

### Conservative Timeline (20 weeks to 80%)

**Assumptions:**
- Part-time effort (50% allocation)
- Significant refactoring needed
- Multiple review cycles

| Week | Phase | Deliverables | Cumulative Coverage |
|------|-------|--------------|---------------------|
| 1-3 | Phase 1 | Utility modules | 18% |
| 4-6 | Phase 1 | Data structures | 30% |
| 7-9 | Phase 2 | JSON-RPC + registry | 42% |
| 10-12 | Phase 2 | Sessions + auth | 50% |
| 13-16 | Phase 3 | Client implementation | 65% |
| 17-20 | Phase 3 | Server implementation | 80% |

**Total: 20 weeks (5 months) to reach 80%**

---

## Quick Start Recommendations

### Week 1: Foundation (Quick Wins)

**Goal:** +8% coverage, build momentum

1. **Fix compilation errors** (validation app)
   - Fix syntax errors in test files
   - Ensure all apps compile cleanly
   - Set up CI/CD coverage reporting

2. **Test 7 simplest modules** (1-2 hours each)
   - `erlmcp_uri_validator`
   - `erlmcp_path_canonicalizer`
   - `erlmcp_message_size`
   - `erlmcp_request_id`
   - `erlmcp_pagination`
   - `erlmcp_logging`
   - `erlmcp_test_sync`

3. **Set up coverage tracking**
   - Configure rebar3 cover
   - Set minimum coverage thresholds
   - Add coverage to CI/CD

### Week 2: Data Structures

**Goal:** +12% coverage

1. **Test cache and rate limiting** (7 hours)
   - `erlmcp_cache` - ETS operations, TTL, LRU eviction
   - `erlmcp_rate_limiter` - Token bucket, leaky bucket

2. **Test circuit breaker and limits** (6 hours)
   - `erlmcp_circuit_breaker` - State transitions
   - `erlmcp_connection_limiter` - Concurrent connections

3. **Test refusal codes** (4 hours)
   - `erlmcp_refusal` - All refusal codes 1001-1089

### Week 3-4: Protocol Layer

**Goal:** +10% coverage

1. **JSON-RPC encoding/decoding** (12 hours)
   - Request/response encoding
   - Error response generation
   - Batch operations
   - Property-based tests for round-trip

2. **Message parsing** (6 hours)
   - JSON parsing validation
   - Message size limits
   - Error handling

---

## Measuring Success

### Coverage Metrics

**Minimum thresholds:**
- **Line coverage:** 80%
- **Branch coverage:** 70%
- **Function coverage:** 90%

**Tracking:**
```bash
# Generate coverage report
rebar3 cover --verbose

# View coverage in HTML
open _build/test/cover/index.html

# Check coverage per module
rebar3 cover -v | grep -E "erlmcp_.*\.erl"
```

### Quality Gates

**Before commit:**
```bash
# Run tests
rebar3 eunit

# Check coverage (fail if <80%)
./scripts/coverage_check.sh

# Run dialyzer
rebar3 dialyzer

# Run xref
rebar3 xref
```

### CI/CD Integration

**Add to GitHub Actions:**
```yaml
- name: Run tests with coverage
  run: |
    rebar3 cover
    rebar3 cover --verbose > coverage.txt

- name: Check coverage threshold
  run: |
    COVERAGE=$(grep "Percentage" coverage.txt | awk '{print $2}' | sed 's/%//')
    if (( $(echo "$COVERAGE < 80" | bc -l) )); then
      echo "Coverage $COVERAGE% is below 80% threshold"
      exit 1
    fi
```

---

## Appendix: Module Complexity Analysis

### Lines of Code per Module (Top 20)

| Rank | Module | LOC | Complexity | Est. Test Hours |
|------|--------|-----|------------|-----------------|
| 1 | `erlmcp_server` | 2,042 | Very High | 60 |
| 2 | `erlmcp_json_rpc` | 1,088 | High | 12 |
| 3 | `erlmcp_client` | 899 | High | 30 |
| 4 | `erlmcp_refusal` | 650+ | Medium | 6 |
| 5 | `erlmcp_auth` | 650+ | High | 10 |
| 6 | `erlmcp_schema_validator` | 600+ | Medium | 8 |
| 7 | `erlmcp_rate_limiter` | 550+ | Medium | 5 |
| 8 | `erlmcp_registry` | 530 | High | 8 |
| 9 | `erlmcp_registry_dist` | 450+ | Medium | 5 |
| 10 | `erlmcp_completion` | 450+ | Medium | 6 |

### Function Count per Module (Top 10)

| Rank | Module | Functions | Est. Test Cases |
|------|--------|-----------|-----------------|
| 1 | `erlmcp_server` | 105+ | 300+ |
| 2 | `erlmcp_json_rpc` | 50+ | 150+ |
| 3 | `erlmcp_client` | 52 | 150+ |
| 4 | `erlmcp_registry` | 30+ | 90+ |
| 5 | `erlmcp_auth` | 25+ | 75+ |
| 6 | `erlmcp_transport_tcp` | 20+ | 60+ |
| 7 | `erlmcp_session_manager` | 18+ | 54+ |

---

## Conclusion

The erlmcp project requires **approximately 375 hours of focused testing work** to reach 80% coverage. The most efficient path is:

1. **Start with quick wins** (Phase 1: 1 week, +25% coverage)
2. **Focus on protocol correctness** (Phase 2: 2 weeks, +18% coverage)
3. **Tackle client/server** (Phase 3: 4 weeks, +25% coverage)
4. **Complete transport layer** (Phase 4: 3 weeks, +16% coverage)

**Minimum viable timeline:** 10 weeks with dedicated resources
**Recommended timeline:** 14 weeks for sustainable pace
**Conservative timeline:** 20 weeks with part-time effort

**Critical success factors:**
- Fix compilation errors first
- Use Chicago School TDD (real processes, no mocks)
- Property-based testing for complex modules
- Continuous coverage tracking
- Regular code reviews of test quality

**Next steps:**
1. Fix validation app compilation errors
2. Set up coverage reporting in CI/CD
3. Begin Phase 1 with utility modules
4. Establish testing patterns for gen_servers
5. Create test helpers for common scenarios

---

**Report Version:** 1.0
**Last Updated:** 2026-01-30
**Maintained By:** erlmcp Development Team
