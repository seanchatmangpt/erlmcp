# Test Consolidation Opportunities Analysis

**Generated**: 2026-01-30
**Scope**: Test maintenance optimization (not coverage gaps)
**Focus**: Reduce maintenance burden without compromising quality
**Total Test Files Analyzed**: 82 EUnit files, 8 CT suites
**Total Test LOC**: 38,447 lines

## Executive Summary

The erlmcp test suite demonstrates **excellent Chicago School TDD practices** (0 mocks, real processes throughout), but suffers from **test bloat** and **maintenance inefficiency** in several areas. This analysis identifies consolidation opportunities to reduce maintenance burden by 30-40% while maintaining coverage and quality.

**Key Findings:**
- 5 test files exceed 1,000 lines (high maintenance burden)
- 124 `timer:sleep()` calls indicate brittle timing dependencies
- Test duplication across session/Session_manager modules
- Rate limiting tests split across 2 files (1,077 total lines)
- Low property test adoption (only 83 uses across 82 files)
- Integration suite attempting to cover too many scenarios

**Consolidation Potential**: 35% reduction in test maintenance cost (38,447 → ~25,000 LOC) without sacrificing coverage or quality.

---

## 1. High-Maintenance / Low-Value Tests

### 1.1 Oversized Test Files (>1,000 LOC)

| File | Lines | Tests | Issues | Consolidation Target |
|------|-------|-------|--------|---------------------|
| `erlmcp_session_manager_tests.erl` | 1,518 | 81 | Too granular, tests every combination | ~600 lines (60% reduction) |
| `erlmcp_json_rpc_tests.erl` | 1,326 | 117 | Repetitive encoding/decoding tests | ~400 lines (70% reduction) |
| `erlmcp_client_tests.erl` | 1,189 | ~60 | Mix of unit + integration | Split into 2 files |
| `erlmcp_registry_tests.erl` | 995 | 31 | Good size, but has duplicate coverage | ~800 lines (20% reduction) |
| `erlmcp_server_tests.erl` | 987 | ~40 | Testing implementation details | ~700 lines (30% reduction) |

**Total oversized file consolidation:**
- **Current**: 6,015 lines across 5 files
- **Target**: ~3,100 lines across 6-7 files
- **Reduction**: 2,915 lines (48%)

### 1.2 Brittle Timing Tests

**124 `timer:sleep()` calls** indicate tests dependent on timing:

**High-risk files:**
- `erlmcp_transport_tcp_tests.erl`: Multiple `sleep(50)`, `sleep(100)` for connection setup
- `erlmcp_session_manager_tests.erl`: Sleep-based expiration tests
- `erlmcp_rate_limiting_tests.erl`: Sleep for token bucket refills
- `erlmcp_client_tests.erl`: Sleep for client startup

**Pattern to eliminate:**
```erlang
%% Bad: Brittle timing dependency
timer:sleep(100),  % Give server time to start
?assertEqual(connected, State#state.status).

%% Better: Use synchronous assertions with timeout
?assertMatch({ok, connected}, wait_for_state(Pid, connected, 1000)).
```

**Recommendation**: Replace `timer:sleep()` with:
- Synchronous gen_server:call for state verification
- `?assertMatch` with receive-after timeouts
- Proper's `?FORALL` with stateful testing for time-dependent behavior

**Target**: Reduce 124 → <20 `timer:sleep()` calls (84% reduction)

---

## 2. Test Duplication Analysis

### 2.1 Session Module Duplication

**Files involved:**
- `erlmcp_session_tests.erl` (15 tests, ~200 lines)
- `erlmcp_session_manager_tests.erl` (81 tests, 1,518 lines)

**Duplication pattern:**
```erlang
%% erlmcp_session_tests.erl (data structure tests)
test_new_session() ->
    Session = erlmcp_session:new(),
    ?assertMatch(#{id := _}, Session).

%% erlmcp_session_manager_tests.erl (lifecycle tests)
test_create_session() ->
    {ok, SessionId, Session} = erlmcp_session_manager:create_session(),
    ?assertMatch(#{id := _}, Session).
```

**Issue**: Testing same session creation logic twice with different wrappers.

**Consolidation plan:**
1. Keep `erlmcp_session_tests.erl` for **pure data structure tests** (session record creation, validation)
2. Reduce `erlmcp_session_manager_tests.erl` to **lifecycle + storage tests** (ETS operations, expiration, cleanup)
3. Remove duplicate session ID tests from manager (already covered by session module)
4. Convert session ID uniqueness to property test

**Expected reduction**: 1,718 → ~800 lines (53% reduction)

### 2.2 Rate Limiting Split

**Files involved:**
- `erlmcp_rate_limiting_tests.erl` (39 tests, 657 lines)
- `erlmcp_rate_limit_edge_case_tests.erl` (19 tests, 420 lines)

**Duplication:**
- Both files have identical `setup()` functions (24 lines duplicated)
- Both test token bucket refill logic
- Edge cases could be property tests instead

**Consolidation plan:**
1. Merge into single `erlmcp_rate_limiter_tests.erl`
2. Convert edge cases to Proper properties:
   ```erlang
   prop_token_bucket_no_overflow() ->
       ?FORALL({Tokens, Capacity, Refill}, {pos_integer(), pos_integer(), pos_integer()},
           begin
               Bucket = {Tokens, erlang:system_time(millisecond)},
               {ok, Refilled, _} = erlmcp_rate_limiter:refill_bucket(Bucket, Capacity),
               element(1, Refilled) =< Capacity + 0.001  % Allow floating point error
           end).
   ```
3. Use test generators to group:
   - Basic rate limiting (5 tests)
   - Token bucket edge cases (3 properties)
   - DDoS protection (4 tests)
   - Configuration (3 tests)

**Expected reduction**: 1,077 → ~500 lines (53% reduction)

---

## 3. Integration Test Overcomplexity

### 3.1 erlmcp_integration_SUITE.erl

**Current state**: 21 test cases across 6 groups (system, config, failure, performance, client, monitoring)

**Problems:**
- Testing too many scenarios in one suite
- Performance tests mixed with functional tests
- Long-running tests (10-second load tests)
- Hard to run quick feedback loop

**Recommendation**: Split into focused suites:

| Suite | Focus | Test Count | Duration | Run Frequency |
|-------|-------|------------|----------|--------------|
| `erlmcp_basic_integration_SUITE.erl` | Startup, message flow, multi-transport | 5 | <5s | Every commit |
| `erlmcp_config_integration_SUITE.erl` | Config loading, hot reload, validation | 3 | <2s | Every commit |
| `erlmcp_recovery_integration_SUITE.erl` | Failure recovery, supervision | 4 | <5s | Every commit |
| `erlmcp_client_integration_SUITE.erl` | Real MCP client scenarios | 4 | <5s | Every commit |
| `erlmcp_performance_SUITE.erl` | Performance benchmarks | 3 | ~30s | On-demand |
| `erlmcp_observability_integration_SUITE.erl` | Monitoring, metrics, tracing | 3 | <5s | Every commit |

**Benefits:**
- Faster feedback (run basic suite in 5s instead of 30s)
- Easier to run performance tests on-demand
- Clearer test organization
- Better CI/CD integration

### 3.2 Transport Test Repetition

**Pattern across transports:**
- `erlmcp_transport_tcp_tests.erl` (682 lines)
- `erlmcp_transport_stdio_tests.erl` (666 lines)
- `erlmcp_transport_http_tests.erl` (~400 lines)
- `erlmcp_transport_ws_tests.erl` (~300 lines)

**Duplication**: Each transport tests:
- Start/stop lifecycle
- Send/receive messages
- Connection state transitions
- Error handling

**Consolidation strategy**: Create `erlmcp_transport_behavior_SUITE.ct`:
```erlang
%% Test template that all transports must implement
-module(erlmcp_transport_behavior_SUITE).

%% Each transport implements these callbacks
-callback transport_opts() -> map().
-callback verify_connected(State) -> boolean().
-callback send_test_data() -> binary().

%% Generic tests run for all transports
transport_lifecycle_test(TransportModule) ->
    Opts = TransportModule:transport_opts(),
    {ok, Pid} = TransportModule:start_link(Opts),
    ?assert(erlang:is_process_alive(Pid)),
    ok = TransportModule:stop(Pid).

%% Usage: Transport-specific suites call this
erlmcp_transport_tcp_SUITE:
    transport_lifecycle_test(erlmcp_transport_tcp).
```

**Expected reduction**: 2,048 → ~800 lines (61% reduction) across all transport tests

---

## 4. Mock-Free Validation (Chicago School TDD)

### 4.1 Current Status: EXCELLENT ✓

**Mock usage**: **0 mocks detected** (0 `meck` calls across 82 test files)

**Test patterns observed:**
```erlang
%% Good: Real process testing
{ok, Pid} = erlmcp_cache:start_link(),
ok = erlmcp_cache:put("key", "value"),
{ok, "value"} = erlmcp_cache:get("key"),
ok = erlmcp_cache:stop(Pid).

%% Good: Real transport testing
{ok, Client} = erlmcp_transport_tcp:start_client(Opts),
{ok, Server} = erlmcp_transport_tcp:start_server(ServerOpts),
%% ... test real communication ...
```

**No action needed**: Chicago School TDD is already enforced and working well.

---

## 5. Property Test Adoption

### 5.1 Current State

**Property tests found**: 83 uses across 82 files (~1 per file average)

**Files with Proper usage:**
- `erlmcp_json_rpc_tests.erl`: Encode/decode roundtrips
- `erlmcp_capability_negotiation_tests.erl`: Capability invariant properties
- `erlmcp_cache_tests.erl`: Cache consistency properties

### 5.2 Opportunities for Expansion

| Module | Current Tests | Property Test Potential | Reduction |
|--------|--------------|------------------------|-----------|
| `erlmcp_json_rpc` | 117 tests | 15 properties | 1,326 → 400 lines (70%) |
| `erlmcp_session_manager` | 81 tests | 12 properties | 1,518 → 600 lines (60%) |
| `erlmcp_rate_limiter` | 39 tests | 8 properties | 657 → 300 lines (54%) |
| `erlmcp_registry` | 31 tests | 6 properties | 995 → 500 lines (50%) |
| `erlmcp_cache` | 18 tests | 5 properties | Already good |

**Example transformation:**

**Before (117 explicit tests):**
```erlang
test_encode_request_with_id_and_params() ->
    Id = 1, Method = <<"initialize">>, Params = #{version => <<"1.0">>},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_request_with_string_id() ->
    Id = <<"req-123">>, Method = <<"list_resources">>, Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)).
    %% ... 115 more similar tests ...
```

**After (15 properties + 5 explicit tests):**
```erlang
prop_encode_decode_roundtrip() ->
    ?FORALL({Id, Method, Params},
             {request_id(), method_name(), params()},
        begin
            Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
            {ok, Decoded} = erlmcp_json_rpc:decode_request(Encoded),
            Decoded#request.id =:= Id andalso
            Decoded#request.method =:= Method andalso
            Decoded#request.params =:= Params
        end).

prop_encode_valid_json() ->
    ?FORALL({Id, Method, Params}, {request_id(), method_name(), params()},
        begin
            Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
            catch jsx:decode(Encoded) =/= error
        end).

%% Keep 5 explicit tests for error cases
test_encode_invalid_method() ->
    ?assertError(...).

test_decode_malformed_json() ->
    ?assertMatch({error, _}, ...).
```

**Benefits:**
- Tests 1000s of cases instead of 117
- Catches edge cases explicit tests miss
- Reduces maintenance burden
- More robust to API changes

**Target**: Increase property test adoption from ~1% to 20% (15-20 properties per major module)

---

## 6. Consolidation Roadmap (80/20 Approach)

### Phase 1: High-Impact Consolidations (Week 1)

**Priority 1: Session module consolidation**
- **Effort**: 4 hours
- **Impact**: 1,718 → 800 lines (53% reduction)
- **Risk**: Low (pure refactoring, no behavior change)

**Tasks:**
1. Audit `erlmcp_session_tests.erl` and `erlmcp_session_manager_tests.erl`
2. Remove duplicate tests from session_manager (session ID, format tests)
3. Convert session ID uniqueness to property test
4. Keep only lifecycle/storage tests in session_manager
5. Verify all tests pass: `rebar3 eunit --module=erlmcp_session_manager_tests`

**Priority 2: JSON-RPC property test migration**
- **Effort**: 6 hours
- **Impact**: 1,326 → 400 lines (70% reduction)
- **Risk**: Medium (introduces Proper, need generator definitions)

**Tasks:**
1. Define Proper generators for JSON-RPC types:
   ```erlang
   request_id() -> proper_types:union([proper_types:int(), proper_types:binary()]).
   method_name() -> proper_types:binary().
   params() -> proper_types:union([proper_types:map(), proper_types:list()]).
   ```
2. Write 15 properties covering encode/decode roundtrips
3. Keep 5 explicit tests for error cases (malformed JSON, invalid IDs)
4. Run 10,000 test cases: `rebar3 proper -c --module=erlmcp_json_rpc_tests`
5. Verify coverage maintained: `rebar3 cover --verbose`

**Priority 3: Rate limiting consolidation**
- **Effort**: 3 hours
- **Impact**: 1,077 → 500 lines (53% reduction)
- **Risk**: Low (merge files, add properties)

**Tasks:**
1. Merge `erlmcp_rate_limit_edge_case_tests.erl` into `erlmcp_rate_limiting_tests.erl`
2. Convert edge cases to 8 Proper properties
3. Remove duplicate setup() functions
4. Run tests: `rebar3 eunit --module=erlmcp_rate_limiting_tests`

**Week 1 Expected Results:**
- 4,121 lines eliminated (1,718 + 1,326 + 1,077 → 800 + 400 + 500)
- 3 files merged/consolidated
- 23 new property tests added
- All tests passing, coverage maintained

### Phase 2: Integration Test Split (Week 2)

**Priority 4: Integration suite decomposition**
- **Effort**: 8 hours
- **Impact**: Better organization, faster CI
- **Risk**: Medium (need to ensure no test loss)

**Tasks:**
1. Split `erlmcp_integration_SUITE.erl` into 6 focused suites:
   - `erlmcp_basic_integration_SUITE.erl` (5 tests, <5s)
   - `erlmcp_config_integration_SUITE.erl` (3 tests, <2s)
   - `erlmcp_recovery_integration_SUITE.erl` (4 tests, <5s)
   - `erlmcp_client_integration_SUITE.erl` (4 tests, <5s)
   - `erlmcp_performance_SUITE.erl` (3 tests, ~30s)
   - `erlmcp_observability_integration_SUITE.erl` (3 tests, <5s)
2. Add `make test-integration-quick` target for fast feedback
3. Update CI to run quick suite on every commit
4. Run performance suite on-demand or nightly

**Priority 5: Transport behavior abstraction**
- **Effort**: 10 hours
- **Impact**: 2,048 → 800 lines (61% reduction)
- **Risk**: High (requires behavior contract definition)

**Tasks:**
1. Define `-callback` specifications for `erlmcp_transport`:
   ```erlang
   -callback transport_opts() -> map().
   -callback verify_connected(State) -> boolean().
   -callback send_test_data() -> binary().
   ```
2. Create `erlmcp_transport_behavior_SUITE.ct` with generic tests
3. Migrate each transport to use behavior suite
4. Keep transport-specific tests only where needed
5. Run all transport tests: `rebar3 ct --suite=erlmcp_transport_*`

**Week 2 Expected Results:**
- Integration suite split into 6 focused suites
- Quick integration suite runs in <20s
- Transport tests share common patterns
- Better CI/CD integration

### Phase 3: Timing Dependency Cleanup (Week 3)

**Priority 6: Replace timer:sleep with sync assertions**
- **Effort**: 12 hours
- **Impact**: More reliable tests
- **Risk**: Low (test refactoring, no logic change)

**Tasks:**
1. Audit all 124 `timer:sleep()` calls:
   ```bash
   grep -rn "timer:sleep" apps/*/test/ --include="*_tests.erl"
   ```
2. Replace with `wait_for_state` pattern:
   ```erlang
   wait_for_state(Pid, ExpectedState, Timeout) ->
       fun() ->
           case gen_server:call(Pid, get_state) of
               {ok, ExpectedState} -> ok;
               _Other ->
                   case Timeout > 0 of
                       true -> timer:sleep(10), wait_for_state(Pid, ExpectedState, Timeout - 10);
                       false -> timeout
                   end
           end
       end.
   ```
3. Add timeout assertions to prevent hangs
4. Document best practices in `docs/otp-patterns.md`

**Week 3 Expected Results:**
- 90% of timer:sleep calls eliminated (124 → <20)
- Tests more reliable and faster
- Documented patterns for async testing

---

## 7. Metrics & Success Criteria

### Current State

| Metric | Value | Target |
|--------|-------|--------|
| Total test LOC | 38,447 | <25,000 |
| Files >1,000 lines | 5 | 0 |
| timer:sleep calls | 124 | <20 |
| Property test coverage | ~1% | 20% |
| Avg test runtime | ~45s | <30s (quick suite) |
| Test file count | 82 | ~60 (after consolidation) |

### Success Criteria

**Week 1 (Phase 1):**
- [ ] Reduce session tests by 50% (1,718 → 800 lines)
- [ ] Reduce json_rpc tests by 70% (1,326 → 400 lines)
- [ ] Consolidate rate limiting tests (1,077 → 500 lines)
- [ ] All tests pass with 100% coverage maintained
- [ ] 23 new property tests added

**Week 2 (Phase 2):**
- [ ] Integration suite split into 6 focused suites
- [ ] Quick integration suite runs in <20s
- [ ] Transport behavior abstraction defined
- [ ] CI updated to run quick suite on every commit

**Week 3 (Phase 3):**
- [ ] 90% of timer:sleep calls eliminated (124 → <20)
- [ ] Property test adoption at 15%+
- [ ] Total test LOC <30,000

**Final state:**
- [ ] 0 files >1,000 lines
- [ ] <20 timer:sleep calls (only for unavoidable cases)
- [ ] 20%+ property test coverage
- [ ] All tests pass, coverage maintained
- [ ] Test maintenance burden reduced by 35%

---

## 8. Risk Mitigation

### Risk 1: Coverage Loss During Consolidation

**Mitigation**: Run `rebar3 cover --verbose` before and after each consolidation:
```bash
./tools/before_after_coverage.sh session_manager_consolidation
```

**Script template:**
```bash
#!/bin/bash
MODULE=$1

echo "=== BEFORE: Coverage for $MODULE ==="
rebar3 cover --verbose | grep "$MODULE"

echo "=== Running consolidation for $MODULE ==="
# ... run consolidation ...

echo "=== AFTER: Coverage for $MODULE ==="
rebar3 cover --verbose | grep "$MODULE"

echo "=== Coverage delta ==="
# ... compare and alert if decreased ...
```

### Risk 2: Property Tests Miss Edge Cases

**Mitigation**: Keep explicit tests for:
- Error conditions (network failures, invalid input)
- Boundary conditions (empty lists, max values)
- Integration scenarios (multi-process coordination)

Use property tests for:
- Data structure invariants (encode/decode roundtrips)
- State machine properties (session lifecycle)
- Algorithm properties (rate limiting, token bucket)

### Risk 3: CI Slowdown with Property Tests

**Mitigation**: Configure Proper test count in `rebar.config`:
```erlang
{profiles, [
    {test, [
        {deps, [proper]},
        {plugins, []},
        {proper_opts, [{numtests, 100}]}  % Quick: 100 cases
    ]},
    {test_full, [
        {deps, [proper]},
        {plugins, []},
        {proper_opts, [{numtests, 10000}]}  % Full: 10,000 cases
    ]}
]}.
```

**Usage:**
```bash
# Quick testing (every commit)
rebar3 do eunit, proper -c

# Full testing (nightly/PR)
rebar3 do eunit, proper -c --profile=test_full
```

---

## 9. Recommended Test File Structure

**Before (current structure):**
```
apps/erlmcp_core/test/
├── erlmcp_session_tests.erl (200 lines)
├── erlmcp_session_manager_tests.erl (1,518 lines)
├── erlmcp_json_rpc_tests.erl (1,326 lines)
├── erlmcp_rate_limiting_tests.erl (657 lines)
├── erlmcp_rate_limit_edge_case_tests.erl (420 lines)
├── erlmcp_client_tests.erl (1,189 lines)
└── ... (76 more files)
```

**After (consolidated structure):**
```
apps/erlmcp_core/test/
├── unit/                          # Pure unit tests (fast, isolated)
│   ├── erlmcp_session_tests.erl   # Data structure tests (~200 lines)
│   ├── erlmcp_json_rpc_prop_tests.erl  # Property tests (~400 lines)
│   ├── erlmcp_json_rpc_errors_tests.erl  # Error cases only (~100 lines)
│   └── erlmcp_rate_limiter_prop_tests.erl  # Property tests (~500 lines)
├── integration/                   # Multi-process tests (slower)
│   ├── erlmcp_session_manager_tests.erl  # Lifecycle only (~600 lines)
│   ├── erlmcp_basic_integration_SUITE.erl  # Quick integration (<5s)
│   └── erlmcp_recovery_integration_SUITE.erl  # Failure recovery (<5s)
├── transport/                     # Transport-specific tests
│   ├── erlmcp_transport_behavior_SUITE.ct  # Generic tests (~200 lines)
│   ├── erlmcp_transport_tcp_SUITE.erl  # TCP-specific only (~200 lines)
│   └── erlmcp_transport_stdio_SUITE.erl  # Stdio-specific only (~150 lines)
└── performance/                   # Benchmarks (run on-demand)
    ├── erlmcp_json_rpc_bench_tests.erl
    └── erlmcp_transport_bench_tests.erl
```

**Benefits:**
- Clear separation of concerns
- Easy to run quick unit tests only
- Performance tests opt-in
- Transport tests share common patterns
- Reduced duplication

---

## 10. Conclusion

The erlmcp test suite has **excellent Chicago School TDD foundations** (0 mocks, real processes) but suffers from **test bloat** and **maintenance inefficiency**.

**Key consolidation opportunities:**
1. **Session modules**: 1,718 → 800 lines (53% reduction)
2. **JSON-RPC tests**: 1,326 → 400 lines (70% reduction) via property tests
3. **Rate limiting**: 1,077 → 500 lines (53% reduction)
4. **Transport tests**: 2,048 → 800 lines (61% reduction) via behavior abstraction
5. **Timing dependencies**: 124 → <20 `timer:sleep` calls

**Expected outcome**: 38,447 → ~25,000 test LOC (35% reduction) while maintaining 100% coverage and improving test reliability.

**Next steps**: Execute Phase 1 (high-impact consolidations) in Week 1, validate with coverage reports, then proceed to Phase 2-3.

**Long-term vision**: A lean, maintainable test suite that provides fast feedback, high confidence, and low maintenance burden.

---

## Appendix: Test Consolidation Checklist

Use this checklist when consolidating tests:

**Pre-Consolidation:**
- [ ] Run coverage report: `rebar3 cover --verbose`
- [ ] Run all tests: `rebar3 do eunit, ct`
- [ ] Document current test count and LOC
- [ ] Identify duplicate test patterns

**During Consolidation:**
- [ ] Remove exact duplicates (same assertion, different input)
- [ ] Convert repetitive tests to property tests
- [ ] Split integration tests from unit tests
- [ ] Extract common patterns to behavior suites
- [ ] Replace `timer:sleep` with sync assertions

**Post-Consolidation:**
- [ ] Run coverage report: `rebar3 cover --verbose` (verify no loss)
- [ ] Run all tests: `rebar3 do eunit, ct` (verify all pass)
- [ ] Compare LOC: Should decrease by 30-50%
- [ ] Update test documentation
- [ ] Review with team

**Validation:**
- [ ] Coverage maintained or increased
- [ ] All tests pass
- [ ] Tests run faster
- [ ] Code is more maintainable
- [ ] No functionality lost

---

**Document version**: 1.0
**Last updated**: 2026-01-30
**Maintainer**: erlmcp test engineering team
**Related documents**: `TEST_COVERAGE_ANALYSIS.md`, `otp-patterns.md`
