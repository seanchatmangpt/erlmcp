# MCP Test Strategy Summary

## Quick Overview

**Document**: `docs/MCP_TEST_STRATEGY.md` (comprehensive 1,500+ line strategy)
**Purpose**: Achieve full MCP specification compliance with 85%+ test coverage
**Methodology**: Chicago School TDD (real processes, no mocks, state-based verification)

---

## Current State

### Existing Tests (Strong Foundation)
- ✅ **269 EUnit test files** - Unit tests for individual modules
- ✅ **33 Common Test suites** - Integration tests
- ✅ **~10 Proper test files** - Property-based tests
- ✅ **15+ Benchmark modules** - Performance tests
- ✅ **Chaos testing framework** - `erlmcp_chaos.erl`

### Coverage Gaps (Critical)
- ❌ **Sampling feature**: <10% tested (NEW in MCP 2025-11-25)
- ⚠️ **Resource subscriptions**: Missing Proper properties, chaos tests
- ⚠️ **Tool schema validation**: Missing performance tests (jesse optimization needed)
- ⚠️ **Compliance tests**: Only ~5 suites (need 30+ for all MCP methods)

---

## Test Taxonomy (6 Categories)

### 1. EUnit (Unit Tests) - 269 → 350+ target
**Chicago School TDD**: Spawn real gen_servers, assert on observable state, no mocks

**Example**:
```erlang
test_resource_subscription(Server) ->
    %% Setup: Real server
    {ok, Server} = erlmcp_server:start_link(test_server, #{}),

    %% Exercise: Subscribe via API
    ok = erlmcp_server:subscribe_resource(Server, <<"weather://city">>, self()),

    %% Verify: Observable state (Chicago School)
    {ok, Subscribers} = erlmcp_server:get_subscribers(Server, <<"weather://city">>),
    ?assert(lists:member(self(), Subscribers)).
```

### 2. Common Test (Integration) - 33 → 60+ target
**Real OTP applications**: Multi-process, multi-transport, end-to-end workflows

### 3. Proper (Property-Based) - 10 → 60+ target
**Protocol invariants**: JSON-RPC roundtrips, FSM properties, concurrency

### 4. Compliance Tests - 5 → 30+ target
**MCP spec 2025-11-25**: Validate against official spec JSON, 100% method coverage

### 5. Performance Regression - 15 → 30+ target
**Baselines (Jan 2026)**:
- Registry: 553K msg/s ✅
- JSON-RPC encoding P95: <2ms ✅
- Tool call P95: <20ms ✅

### 6. Chaos Engineering - 5 → 20+ target
**Resilience scenarios**:
- Process crashes (20 scenarios)
- Network failures (15 scenarios)
- Resource exhaustion (10 scenarios)

---

## Critical Gaps (MCP Features)

### Sampling (CRITICAL - NEW FEATURE)
**Current**: ⚠️ <10% tested
**Need**:
- ✅ EUnit: `erlmcp_sampling_tests.erl`
- ✅ CT: `erlmcp_sampling_SUITE.erl`
- ✅ Proper: `erlmcp_sampling_proper_tests.erl`
- ✅ Compliance: `erlmcp_sampling_compliance_SUITE.erl`
- ✅ Performance: `erlmcp_bench_sampling.erl`
- ✅ Chaos: `sampling_provider_chaos_SUITE.erl`

### Resource Subscriptions
**Current**: ⚠️ Partial (missing Proper, chaos)
**Need**:
- ✅ Proper: Subscription state machine properties
- ✅ Chaos: Subscriber crash scenarios
- ✅ Performance: Subscription fan-out (1000+ subscribers)

### Tool Schema Validation
**Current**: ⚠️ Missing performance optimization
**Need**:
- ✅ Performance: Schema validation caching (ETS)
- ✅ Expected: 75% latency reduction (5-20ms → 1-5ms)

---

## Implementation Roadmap (10 Weeks)

### Phase 1: Foundation (Weeks 1-2)
**Goals**:
- Reorganize test directories (taxonomy-based)
- Create test infrastructure (helpers, fixtures)
- Document performance baselines

**Tasks**:
1. Reorganize test structure: `unit/`, `integration/`, `property/`, `compliance/`, `performance/`, `chaos/`
2. Create `test/common_test/test_helpers.erl`
3. Create `test/fixtures/mcp_spec_2025-11-25.json`
4. Run benchmarks, document baselines

**Deliverables**:
- ✅ Clean test taxonomy
- ✅ Test infrastructure
- ✅ Performance baselines JSON

---

### Phase 2: MCP Feature Coverage (Weeks 3-6)
**Goals**: 100% coverage for all MCP methods

**Week 3: Resources**
- EUnit: Subscription tests, notification tests, template tests
- CT: Multi-client subscription suite
- Proper: Subscription invariants
- Compliance: resources/list, resources/read, resources/subscribe
- Performance: Template matching, subscription fan-out
- Chaos: Subscriber crashes

**Week 4: Tools**
- EUnit: Schema validation edge cases
- CT: Concurrent tool calls
- Proper: Schema validation properties
- Compliance: tools/list, tools/call
- Performance: Schema validation latency (optimize with ETS cache)
- Chaos: Tool handler crashes, timeouts

**Week 5: Prompts**
- Similar to Tools and Resources

**Week 6: Sampling (CRITICAL)**
- EUnit: All sampling API tests
- CT: Provider integration (Anthropic, OpenAI, local)
- Proper: Temperature bounds, max tokens
- Compliance: sampling/createMessage
- Performance: Sampling latency
- Chaos: Provider failures

**Deliverables**:
- ✅ 100% MCP method coverage
- ✅ 50+ new Proper properties
- ✅ 30+ compliance suites
- ✅ 20+ benchmarks
- ✅ 15+ chaos suites

---

### Phase 3: Performance & Chaos (Weeks 7-8)
**Goals**: Performance optimization, comprehensive chaos

**Week 7: Performance**
- CI integration: `.github/workflows/performance_regression.yml`
- Optimization 1: Schema validation caching (75% reduction)
- Optimization 2: Replace jsx with jiffy (60% reduction)
- Optimization 3: Async tool execution (5-10x throughput)

**Week 8: Chaos**
- 20+ process crash scenarios
- 15+ network failure scenarios
- 10+ resource exhaustion scenarios
- Chaos monkey scheduler

**Deliverables**:
- ✅ Performance CI
- ✅ 3 major optimizations
- ✅ 50+ chaos scenarios

---

### Phase 4: Quality Gates & Reporting (Weeks 9-10)
**Goals**: 85%+ coverage, compliance reports

**Week 9: Coverage**
- Identify gaps: `rebar3 cover --verbose`
- Fill gaps: Add tests for uncovered code paths
- Coverage CI: Fail PR if <80% overall, <85% core

**Week 10: Compliance**
- Generate reports: HTML, JSON
- Documentation: TEST_COVERAGE_REPORT.md, COMPLIANCE_REPORT.md

**Deliverables**:
- ✅ 85%+ coverage
- ✅ Compliance reports
- ✅ Documentation

---

## Chicago School TDD Principles

### Core Tenets
1. **Real Collaborators** - Spawn actual gen_servers, not mocks
2. **State-Based Verification** - Assert on observable state via API calls
3. **Behavior Testing** - Verify what system does (outputs), not how (internals)
4. **Integration Over Isolation** - Test components together when practical

### Anti-Patterns (What NOT to Do)

❌ **Don't Mock gen_servers**:
```erlang
meck:new(erlmcp_server),  % BAD (London School)
```

✅ **Do Use Real gen_servers**:
```erlang
{ok, Server} = erlmcp_server:start_link(test_server, #{}),  % GOOD (Chicago School)
```

❌ **Don't Verify Internal Calls**:
```erlang
?assertMatch({call, handle_call, [...]}, meck:history(erlmcp_server)).  % BAD
```

✅ **Do Verify Observable State**:
```erlang
{ok, Resources} = erlmcp_server:list_resources(Server),  % GOOD
?assert(lists:any(fun(R) -> maps:get(uri, R) =:= <<"doc://readme">> end, Resources)).
```

---

## Claude-Flow Agent Integration

### Available Test Agents

| Agent | Purpose |
|-------|---------|
| **erlang-test-engineer** | Primary test implementation (EUnit, CT, Proper) |
| **erlang-performance** | Benchmarking, optimization |
| **code-reviewer** | Chicago School TDD compliance review |
| **verifier** | Test execution, coverage reports |
| **agent-06-test-eunit** | EUnit runner |
| **agent-07-test-ct** | Common Test runner |
| **agent-10-test-proper** | Proper runner |
| **agent-11-coverage** | Coverage analysis |

### Parallel Workflow (EPIC 9)

**Trigger**: Multi-feature testing (5+ test files)

**Spawn all agents in parallel (1 message)**:
```javascript
Task("EUnit - Resources", "Create resource EUnit tests", "erlang-test-engineer")
Task("EUnit - Tools", "Create tool EUnit tests", "erlang-test-engineer")
Task("CT - Integration", "Create integration suites", "erlang-test-engineer")
Task("Proper - Properties", "Create property tests", "erlang-test-engineer")
Task("Compliance", "Create compliance suites", "erlang-test-engineer")
Task("Performance", "Create benchmarks", "erlang-performance")
Task("Chaos", "Create chaos tests", "erlang-test-engineer")
Task("Review", "Review Chicago School compliance", "code-reviewer")
Task("Verify", "Run tests, generate reports", "verifier")
```

**Expected Speedup**: 2.8x - 4.4x

---

## Quality Gates

### Pre-Commit
1. Compile: `TERM=dumb rebar3 compile` (errors = 0)
2. Format: `rebar3 format --verify`
3. EUnit: `rebar3 eunit --module=<changed_module>_tests`

### PR Gates
1. All Tests: `rebar3 do eunit, ct, proper -c` (failures = 0)
2. Coverage: `rebar3 cover` (≥80% overall, ≥85% core)
3. Dialyzer: `rebar3 dialyzer` (warnings → 0)
4. Xref: `rebar3 xref` (undefined = ∅)
5. Performance: No regression > 10%

### Release Gates
1. Full Suite: `make check`
2. Compliance: 100% MCP method coverage
3. Performance: All baselines met
4. Chaos: All scenarios pass
5. Coverage: ≥85% core, ≥80% overall

---

## Next Steps

### Immediate Actions (Week 1)

1. **Read Full Strategy**: `docs/MCP_TEST_STRATEGY.md`

2. **Reorganize Tests** (1 day):
   ```bash
   mkdir -p apps/erlmcp_core/test/{unit,integration,property,compliance,performance,chaos}
   # Move existing tests into subdirectories
   ```

3. **Create Test Infrastructure** (1 day):
   ```bash
   # Create test helpers
   touch test/common_test/test_helpers.erl
   touch test/common_test/mcp_test_server.erl
   touch test/common_test/mcp_test_client.erl

   # Create fixtures
   mkdir -p test/fixtures/{valid_requests,invalid_requests,schemas}
   ```

4. **Document Baselines** (1 day):
   ```bash
   # Run benchmarks
   rebar3 as benchmark shell
   > erlmcp_bench_core_ops:run().
   > erlmcp_bench_mcp_features:run().

   # Save to performance_baselines_2026-02-01.json
   ```

5. **Spawn Test Agents** (parallel):
   ```javascript
   Task("Research Tests", "Analyze existing test files", "erlang-researcher")
   Task("Design Infrastructure", "Design test helpers", "erlang-architect")
   Task("Create Templates", "Create test templates", "erlang-test-engineer")
   Task("Run Benchmarks", "Document performance baselines", "erlang-performance")
   ```

### Week 2-10: Follow Roadmap

- **Weeks 3-6**: Implement tests for all MCP features (Resources, Tools, Prompts, **Sampling**)
- **Weeks 7-8**: Performance optimization, chaos engineering
- **Weeks 9-10**: Coverage improvement, compliance reporting

---

## Key Metrics (Target)

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Test Files** | 340 | 550+ | ⚠️ +210 needed |
| **Coverage (Overall)** | ~75% | 85%+ | ⚠️ +10% |
| **Coverage (Core)** | ~80% | 90%+ | ⚠️ +10% |
| **MCP Method Coverage** | ~60% | 100% | ⚠️ +40% |
| **Performance Baselines** | ✅ Met | ✅ Met | ✅ PASS |
| **Chaos Scenarios** | 5 | 50+ | ⚠️ +45 |

---

## Critical Priorities

### Priority 1: Sampling Feature Tests (Week 6)
**Why**: New MCP feature, barely tested
**Impact**: HIGH (critical for MCP compliance)
**Effort**: Medium (6 test categories)

### Priority 2: Performance Optimization (Week 7)
**Why**: Schema validation bottleneck (5-20ms)
**Impact**: HIGH (affects all tools with schemas)
**Effort**: Medium (ETS caching implementation)

### Priority 3: Compliance Tests (Weeks 3-6)
**Why**: Only ~5 suites exist, need 30+
**Impact**: HIGH (required for MCP certification)
**Effort**: High (30+ test suites)

---

## Questions?

**Detailed strategy**: `docs/MCP_TEST_STRATEGY.md`
**Contact**: erlang-test-engineer agent

**Ready to start**: Phase 1, Week 1 - Reorganize tests, create infrastructure, document baselines.

---

**Last Updated**: 2026-02-01
**Version**: 1.0.0
