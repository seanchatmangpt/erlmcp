# Test Coverage Summary - erlmcp apps/

**Generated:** 2026-01-28
**Status:** 76 modules identified without tests (43.4% coverage gap)

---

## Coverage Snapshot

| App | Total Modules | Tested | Untested | Coverage % | Target % |
|-----|---------------|--------|----------|------------|----------|
| **erlmcp_core** | 47 | 23 | 24 | 49% | 85% ⬆️ |
| **erlmcp_observability** | 25 | 11 | 14 | 44% | 80% ⬆️ |
| **erlmcp_transports** | 15 | 11 | 4 | 73% ✅ | 80% |
| **tcps_erlmcp** | 67 | 33 | 34 | 49% | 70% ⬆️ |
| **TOTAL** | **154** | **78** | **76** | **51%** | **80%** |

---

## Priority Matrix

### 🔴 Critical (Week 1-3) - 17 modules, ~102 hours

**Must test immediately - blocks core MCP functionality:**

| Priority | Module | App | LOC | Impact | Hours |
|----------|--------|-----|-----|--------|-------|
| P0-1 | `erlmcp_hooks` | core | 597 | Quality gates, task validation | 8h |
| P0-2 | `erlmcp_pricing_receipt` | core | 742 | Hash chain, SKU certification | 10h |
| P0-3 | `tcps_poka_yoke` | core | 528 | Error-proofing validation | 8h |
| P0-4 | `erlmcp_split_brain_detector` | core | 221 | Cluster partition detection | 6h |
| P0-5 | `erlmcp_sla_monitor` | core | 414 | SLA tracking, violations | 8h |
| P0-6 | `erlmcp_otel_middleware` | observability | 317 | Automatic tracing | 8h |
| P0-7 | `erlmcp_chaos` | observability | 762 | Failure injection | 12h |
| P0-8 | `erlmcp_subscription` | core | 51 | Subscribe/notify | 2h |
| P0-9 | `erlmcp_secrets` | core | ~100 | Secret management | 4h |
| P0-10 | `erlmcp_message_handler` | core | ~150 | Message routing | 5h |

**Plus 7 more critical modules** (see detailed plan)

---

### 🟡 Medium (Week 4-7) - 21 modules, ~80 hours

**Production operations, developer productivity:**
- erlmcp_graceful_drain, erlmcp_rate_limiter, erlmcp_schema_validator
- erlmcp_pricing_upgrade (729 LOC), erlmcp_node_monitor
- erlmcp_chaos_network, erlmcp_chaos_process, erlmcp_metrics_aggregator
- **Plus 13 more** (pricing support, observability features)

---

### 🟢 Low (Week 7-16) - 38 modules, ~190 hours

**Transports, TCPS ecosystem, CLI tools:**
- erlmcp_transport_pipeline (HTTP/2, WebSocket)
- All TCPS CLI tools (kanban, kaizen, receipt, quality)
- Documentation generators, rebar3 plugins
- **Deferrable until core hits 85%**

---

## Test Implementation Roadmap

### Sprint 1: Core Infrastructure (Week 1)
**5 modules, 25 hours, 85% coverage target**
```
✅ erlmcp_subscription_tests.erl         (2h)
✅ erlmcp_hooks_tests.erl                (8h) ← HIGH COMPLEXITY
✅ erlmcp_split_brain_detector_tests.erl (6h)
✅ erlmcp_secrets_tests.erl              (4h)
✅ erlmcp_message_handler_tests.erl      (5h)
```

### Sprint 2: Pricing & SLA (Week 2)
**6 modules, 38 hours, 85% coverage target**
```
✅ erlmcp_pricing_plan_tests.erl         (8h)
✅ erlmcp_pricing_receipt_tests.erl      (10h) ← HIGH COMPLEXITY
✅ tcps_poka_yoke_tests.erl              (8h) ← HIGH COMPLEXITY
✅ erlmcp_sla_monitor_tests.erl          (8h)
✅ erlmcp_sla_envelope_tests.erl         (2h)
✅ tcps_poka_yoke_validator_tests.erl    (2h)
```

### Sprint 3: Observability Core (Week 3)
**6 modules, 39 hours, 80% coverage target**
```
✅ erlmcp_receipt_chain_tests.erl        (3h)
✅ erlmcp_otel_middleware_tests.erl      (8h)
✅ erlmcp_otel_jaeger_tests.erl          (6h)
✅ erlmcp_otel_datadog_tests.erl         (5h)
✅ erlmcp_otel_honeycomb_tests.erl       (5h)
✅ erlmcp_chaos_tests.erl                (12h) ← HIGH COMPLEXITY
```

**Milestone 1 (Week 3):** 17 critical modules tested, core functionality at 85%+

---

### Sprint 4-7: Medium Priority (Weeks 4-7)
**21 modules, 80 hours**
- Core features: graceful_drain, node_monitor, rate_limiter, schema_validator
- Pricing support: upgrade, state, loader, CLI, HTTP
- Observability: chaos variants, metrics, dashboard

**Milestone 2 (Week 7):** 38 modules tested, 75%+ overall coverage

---

### Sprint 8: Transports (Week 7-8)
**4 modules, 19 hours**
- erlmcp_transport_pipeline (HTTP/2, WebSocket batching)
- pool_strategy, security_headers, http_server

**Milestone 3 (Week 8):** 42 modules tested, transports at 80%+

---

### Sprint 9+: TCPS (Weeks 9-16) - OPTIONAL
**34 modules, 151 hours**
- CLI tools (8 modules, 20h)
- Core TCPS (6 modules, 27h)
- Infrastructure (8 modules, 33h)
- Documentation (10 modules, 19h)
- Simulation (6 modules, 32h)
- Rebar3 plugins (5 modules, 20h)

**Milestone 4 (Week 16):** 76 modules tested, 100% coverage gap eliminated

---

## Chicago School TDD Compliance

**ALL tests MUST follow these principles:**

✅ **Real Collaborators**
```erlang
% CORRECT: Use real gen_server
{ok, Pid} = erlmcp_hooks:start_link(),
Result = erlmcp_hooks:post_task(<<"task-1">>, #{})
```

❌ **NO Mocking**
```erlang
% WRONG: Don't mock collaborators
meck:expect(tcps_quality_gates, check_all_gates, ...)
```

✅ **State-based Verification**
```erlang
% CORRECT: Assert on observable state
{ok, Status} = erlmcp_sla_monitor:get_status(),
?assertEqual(violated, maps:get(state, Status))
```

❌ **NO Interaction Verification**
```erlang
% WRONG: Don't verify method calls
?assertMatch({call, handle_call, ...}, meck:history(...))
```

---

## Detailed Test Scenarios

### Example: erlmcp_hooks (597 LOC, 8 hours)

**Test file:** `apps/erlmcp_core/test/erlmcp_hooks_tests.erl`

**Scenarios:**
1. **Pre-task validation** (1h)
   - Empty description rejected
   - Valid description accepted
   - Tool availability checked

2. **Post-task validation** (3h)
   - TCPS quality gates integration
   - All gates pass → task allowed
   - Gate fails → task blocked
   - Violations reported correctly
   - Receipt generation verified

3. **Pre/post-edit** (2h)
   - File type validation (.erl, .hrl, .rs only)
   - Auto-format Erlang (rebar3 fmt)
   - Auto-format Rust (rustfmt)
   - Protected paths rejected

4. **Session lifecycle** (1h)
   - Session start initializes TCPS
   - Session end exports metrics
   - State persistence/restoration

5. **Error handling** (1h)
   - TCPS not available fallback
   - Compilation check fallback
   - Test check fallback
   - Timeout handling

**Coverage target:** 90%+ (critical quality gate module)

---

### Example: erlmcp_subscription (51 LOC, 2 hours)

**Test file:** `apps/erlmcp_core/test/erlmcp_subscription_tests.erl`

**Scenarios:**
1. **Subscribe/unsubscribe** (0.5h)
   - Subscribe to resource
   - Unsubscribe from resource
   - Duplicate subscription handling

2. **Notification** (0.5h)
   - Notify subscribers
   - Message delivery verified
   - Options propagation

3. **Listing** (0.5h)
   - List subscribers for resource
   - Empty list when no subscribers

4. **Concurrency** (0.5h)
   - Multiple concurrent subscriptions
   - Concurrent notifications
   - Race condition handling

**Coverage target:** 85%+

---

## Quality Gates (MANDATORY)

**Before ANY test file is merged:**

✅ **Compilation:** `TERM=dumb rebar3 compile` - MUST pass with 0 errors
✅ **Tests:** `rebar3 eunit --module=<module>_tests` - MUST pass (0 failures)
✅ **Coverage:** `rebar3 cover` - MUST meet target (80-90%)
✅ **Chicago School:** Manual review confirms no mocking, real collaborators
✅ **Dialyzer:** `rebar3 dialyzer` - MUST pass (optional for tests)

**Blocking criteria:**
- ❌ Any test failure blocks merge
- ❌ Coverage below target blocks merge
- ❌ Mocking detected blocks merge
- ❌ Compilation errors block merge

---

## Resource Requirements

### Team Composition

**Phase 1 (Weeks 1-3): Critical Core**
- 2 senior test engineers (full-time)
- 1 code reviewer (part-time)

**Phase 2 (Weeks 4-7): Medium Priority**
- 1-2 test engineers (full-time)

**Phase 3 (Week 7-8): Transports**
- 1 transport specialist (full-time)

**Phase 4 (Weeks 9-16): TCPS - OPTIONAL**
- 1 test engineer (part-time)

### Estimated Calendar Time

**Fast track (2 engineers):** 8-10 weeks for Phases 1-3 (42 modules)
**Full completion:** 16 weeks for all 76 modules
**Recommended:** Execute Phases 1-3, defer Phase 4 until core hits 85%

---

## Success Metrics

### Coverage Targets by End of Phase 1 (Week 3)

| App | Current | Target | Delta |
|-----|---------|--------|-------|
| erlmcp_core | 49% | 65% | +16% ⬆️ |
| erlmcp_observability | 44% | 60% | +16% ⬆️ |
| erlmcp_transports | 73% | 73% | - |
| tcps_erlmcp | 49% | 50% | +1% |
| **OVERALL** | **51%** | **62%** | **+11%** |

### Coverage Targets by End of Phase 3 (Week 8)

| App | Target | Quality |
|-----|--------|---------|
| erlmcp_core | 85% | ✅ Production-ready |
| erlmcp_observability | 80% | ✅ Production-ready |
| erlmcp_transports | 80% | ✅ Production-ready |
| tcps_erlmcp | 50% | ⚠️ Developer tooling |
| **OVERALL** | **75%** | ✅ **Ready for v2.2.0** |

---

## Risk Analysis

### High-Risk Modules (require experienced engineers)

| Module | LOC | Risk Factors | Mitigation |
|--------|-----|--------------|------------|
| `erlmcp_hooks` | 597 | Quality gates critical, TCPS integration | Pair programming, 90% coverage |
| `erlmcp_pricing_receipt` | 742 | Hash chain immutability, revenue impact | Cryptographic validation, property tests |
| `erlmcp_pricing_upgrade` | 729 | Revenue loss if bugs, complex state transitions | State machine testing, rollback scenarios |
| `erlmcp_chaos` | 762 | Chaos must not destabilize production | Bounded failure injection, recovery validation |
| `tcps_poka_yoke` | 528 | Error-proofing ironic if untested | Comprehensive edge cases, real-world scenarios |

**Total high-risk LOC:** 3,358 (50 hours estimated)
**Mitigation effort:** +10 hours for extra review and testing

---

## Next Actions

### Immediate (Week 1, Day 1)

1. **Create test file stubs:**
   ```bash
   cd apps/erlmcp_core/test
   touch erlmcp_subscription_tests.erl
   touch erlmcp_hooks_tests.erl
   touch erlmcp_split_brain_detector_tests.erl
   touch erlmcp_secrets_tests.erl
   touch erlmcp_message_handler_tests.erl
   ```

2. **Assign engineers:**
   - Engineer 1: erlmcp_hooks, erlmcp_split_brain_detector (14h)
   - Engineer 2: erlmcp_subscription, erlmcp_secrets, erlmcp_message_handler (11h)

3. **Set up CI enforcement:**
   ```yaml
   # .github/workflows/test.yml
   - name: Run EUnit tests
     run: rebar3 eunit
   - name: Check coverage
     run: rebar3 cover --verbose
   - name: Enforce 80% minimum
     run: ./scripts/check_coverage.sh 80
   ```

4. **Establish review process:**
   - All test PRs require senior engineer approval
   - Chicago School compliance verified in review
   - Coverage reports attached to PR

---

## Conclusion

**Current state:** 51% test coverage, 76 modules untested
**Target state:** 75%+ coverage after Phase 3 (8 weeks)
**Critical path:** Phase 1 (Weeks 1-3) - 17 modules, 85%+ coverage
**Recommended approach:** Execute Phases 1-3, defer TCPS until core is production-ready

**ROI:** 42 modules tested in 8 weeks → erlmcp_core, observability, transports at 80%+ → Ready for v2.2.0 release

---

**See detailed plan:** `/Users/sac/erlmcp/docs/testing/TEST_COVERAGE_PLAN.md`
**Generated by:** erlang-test-engineer agent
**Methodology:** Chicago School TDD, criticality-complexity matrix
**Last updated:** 2026-01-28
