# Test Coverage Plan for erlmcp apps/

**Generated:** 2026-01-28
**Analysis Date:** 2026-01-28
**Target:** Create EUnit tests for 76 modules lacking test coverage
**Priority:** Criticality → Complexity → Coverage gaps

---

## Executive Summary

**Total modules analyzed:** 175
**Modules with tests:** 99 (56.6%)
**Modules without tests:** 76 (43.4%)

**Breakdown by app:**
- erlmcp_core: 24 modules missing tests (47 total, 51% coverage)
- erlmcp_observability: 14 modules missing tests (25 total, 44% coverage)
- erlmcp_transports: 4 modules missing tests (15 total, 73% coverage) ✅ BEST
- tcps_erlmcp: 34 modules missing tests (67 total, 49% coverage)

---

## Priority 1: Critical Core Modules (HIGH IMPACT)

### Tier 1A: Core Infrastructure (MUST TEST - Week 1)

**Impact:** Core MCP functionality. Failures block all operations.

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `erlmcp_subscription` | core | 51 | LOW (stubs) | Subscribe/unsubscribe, notify, list subscribers, concurrent subscriptions, process death cleanup | 2h |
| `erlmcp_hooks` | core | 597 | HIGH | Pre/post task, pre/post edit, session start/end, quality gate integration, TCPS integration, format validation | 8h |
| `erlmcp_split_brain_detector` | core | 221 | MEDIUM | Partition detection, winner_takes_all, oldest_node, configured_master, resolution strategies, periodic checks | 6h |
| `erlmcp_secrets` | core | ~100 | MEDIUM | Secret storage, encryption, retrieval, rotation, secure deletion, env var integration | 4h |
| `erlmcp_message_handler` | core | ~150 | MEDIUM | Message routing, protocol parsing, error handling, batch processing | 5h |

**Subtotal Tier 1A:** 5 modules, ~25 hours

---

### Tier 1B: Pricing & SLA (BUSINESS CRITICAL - Week 2)

**Impact:** Revenue, compliance, quality gates. Failures block releases.

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `erlmcp_pricing_plan` | core | 479 | HIGH | Plan selection, tier validation, upgrade/downgrade, feature limits, cost calculation | 8h |
| `erlmcp_pricing_receipt` | core | 742 | HIGH | Receipt generation, hash chain verification, SKU certification, evidence bundling, immutability | 10h |
| `tcps_poka_yoke` | core | 528 | HIGH | Error-proofing validation, gate enforcement, failure detection, root cause analysis | 8h |
| `erlmcp_sla_monitor` | core | 414 | HIGH | SLA tracking, violation detection, latency monitoring, availability calculation, alerting | 8h |
| `erlmcp_sla_envelope` | core | 45 | LOW | Envelope wrapping, metadata injection, trace correlation | 2h |
| `tcps_poka_yoke_validator` | core | 58 | LOW | Validation rules, constraint checking, type validation | 2h |

**Subtotal Tier 1B:** 6 modules, ~38 hours

---

### Tier 1C: Observability Core (PRODUCTION OPS - Week 3)

**Impact:** Monitoring, debugging, incident response. Failures hide production issues.

| Module | App | observability | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|---------------|-----|------------|----------------|-----------------|
| `erlmcp_receipt_chain` | observability | 92 | LOW | Event logging, chain integrity, retrieval, restoration, concurrent writes | 3h |
| `erlmcp_otel_middleware` | observability | 317 | HIGH | Automatic span injection, W3C context propagation, transport tracing, error handling | 8h |
| `erlmcp_otel_jaeger` | observability | 285 | MEDIUM | Jaeger exporter, batch export, UDP/HTTP transport, retry logic | 6h |
| `erlmcp_otel_datadog` | observability | 232 | MEDIUM | Datadog exporter, API key handling, metric aggregation, error handling | 5h |
| `erlmcp_otel_honeycomb` | observability | 252 | MEDIUM | Honeycomb exporter, dataset routing, trace forwarding, backpressure | 5h |
| `erlmcp_chaos` | observability | 762 | HIGH | Chaos engineering, failure injection, network partitions, process crashes, recovery validation | 12h |

**Subtotal Tier 1C:** 6 modules, ~39 hours

---

## Priority 2: Medium Complexity Modules (MEDIUM IMPACT)

### Tier 2A: Core Features (Week 4-5)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `erlmcp_graceful_drain` | core | ~120 | MEDIUM | Connection draining, graceful shutdown, timeout handling, in-flight request completion | 4h |
| `erlmcp_node_monitor` | core | ~150 | MEDIUM | Node health monitoring, failure detection, cluster state tracking, event notifications | 5h |
| `erlmcp_message_size` | core | ~80 | LOW | Size validation, limit enforcement, chunking, error handling | 2h |
| `erlmcp_rate_limiter` | core | ~150 | MEDIUM | Token bucket, sliding window, rate limiting strategies, burst handling | 5h |
| `erlmcp_schema_validator` | core | ~200 | MEDIUM | JSON schema validation, type checking, error reporting, caching | 5h |
| `erlmcp_task` | core | ~100 | LOW | Task management, state tracking, completion callbacks | 3h |

**Subtotal Tier 2A:** 6 modules, ~24 hours

---

### Tier 2B: Pricing Support (Week 5-6)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `erlmcp_pricing_upgrade` | core | 729 | HIGH | Upgrade paths, downgrade handling, plan transitions, prorating, rollback | 10h |
| `erlmcp_pricing_state` | core | 108 | LOW | State management, persistence, retrieval, updates | 3h |
| `erlmcp_pricing_loader` | core | 56 | LOW | Plan loading, YAML parsing, validation, caching | 2h |
| `erlmcp_pricing_cli` | core | 54 | LOW | CLI commands, plan display, upgrade triggers, status queries | 2h |
| `erlmcp_pricing_http` | core | 29 | LOW | HTTP handlers, plan API, upgrade endpoints, webhooks | 2h |
| `erlmcp_pricing_util` | core | 54 | LOW | Helper functions, formatters, converters | 2h |
| `erlmcp_pricing_validator` | core | 37 | LOW | Input validation, constraint checking, business rules | 2h |

**Subtotal Tier 2B:** 7 modules, ~23 hours

---

### Tier 2C: Observability Features (Week 6-7)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `erlmcp_chaos_network` | observability | 118 | MEDIUM | Network partition injection, latency, packet loss, DNS failures | 4h |
| `erlmcp_chaos_process` | observability | 169 | MEDIUM | Process kill, suspend, resource exhaustion, crash injection | 5h |
| `erlmcp_chaos_resource` | observability | 157 | MEDIUM | Memory exhaustion, CPU starvation, disk full, file descriptor limits | 5h |
| `erlmcp_evidence_path` | observability | ~100 | LOW | Evidence collection, path tracking, artifact bundling | 3h |
| `erlmcp_metrics_aggregator` | observability | ~150 | MEDIUM | Metric aggregation, time-series rollup, statistical summaries | 5h |
| `erlmcp_metrics_server` | observability | ~150 | MEDIUM | Metric collection, storage, querying, exposition | 5h |
| `erlmcp_dashboard_http_handler` | observability | ~100 | LOW | HTTP endpoints, JSON responses, CORS handling | 3h |
| `erlmcp_dashboard_server` | observability | ~120 | LOW | Dashboard server, WebSocket support, real-time updates | 3h |

**Subtotal Tier 2C:** 8 modules, ~33 hours

---

## Priority 3: Transports & Support (LOW-MEDIUM IMPACT)

### Tier 3A: Transport Features (Week 7-8)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `erlmcp_transport_pipeline` | transports | ~250 | HIGH | HTTP/2 multiplexing, WebSocket batching, TCP optimization, backpressure | 8h |
| `erlmcp_pool_strategy` | transports | ~100 | MEDIUM | Pool sizing, dynamic scaling, load balancing, health checks | 4h |
| `erlmcp_security_headers` | transports | ~80 | LOW | Header injection, CSP, HSTS, CORS, security validation | 2h |
| `erlmcp_transport_http_server` | transports | ~150 | MEDIUM | HTTP server, request routing, middleware, error handling | 5h |

**Subtotal Tier 3A:** 4 modules, ~19 hours

---

## Priority 4: TCPS Ecosystem (DEFERRABLE)

### Tier 4A: CLI Tools (Week 9-10)

**Impact:** Developer productivity. Failures reduce usability but don't block core functions.

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `tcps_cli_andon` | tcps | ~80 | LOW | Andon CLI, signal display, event triggers | 2h |
| `tcps_cli_config` | tcps | ~100 | LOW | Config management, YAML parsing, validation | 3h |
| `tcps_cli_kaizen` | tcps | ~80 | LOW | Kaizen CLI, improvement tracking, reporting | 2h |
| `tcps_cli_kanban` | tcps | ~100 | LOW | Kanban CLI, WIP limits, board display | 3h |
| `tcps_cli_quality` | tcps | ~80 | LOW | Quality gate CLI, status display, reports | 2h |
| `tcps_cli_receipt` | tcps | ~100 | LOW | Receipt CLI, verification, chain display | 3h |
| `tcps_cli_root_cause` | tcps | ~80 | LOW | 5 Whys CLI, analysis workflow | 2h |
| `tcps_cli_work_order` | tcps | ~100 | LOW | Work order CLI, creation, status, completion | 3h |

**Subtotal Tier 4A:** 8 modules, ~20 hours

---

### Tier 4B: TCPS Core (Week 11-12)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `tcps_receipt` | tcps | ~200 | MEDIUM | Receipt generation, verification, chain integrity | 5h |
| `tcps_quality_receipt_verifier` | tcps | ~150 | MEDIUM | Verification logic, hash checking, evidence validation | 5h |
| `tcps_receipt_verifier` | tcps | ~150 | MEDIUM | Alternative verifier, compatibility checks | 5h |
| `tcps_release_validator` | tcps | ~200 | MEDIUM | Release validation, dependency checks, version constraints | 5h |
| `tcps_mcp_prompts` | tcps | ~100 | LOW | MCP prompt generation, template rendering | 3h |
| `tcps_mcp_tools` | tcps | ~150 | MEDIUM | MCP tool definitions, capability registration | 4h |

**Subtotal Tier 4B:** 6 modules, ~27 hours

---

### Tier 4C: TCPS Infrastructure (Week 13-14)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `tcps_dashboard_handler` | tcps | ~100 | LOW | Dashboard HTTP handler, JSON API | 3h |
| `tcps_dashboard_sse_handler` | tcps | ~100 | LOW | Server-sent events, real-time updates | 3h |
| `tcps_websocket_handler` | tcps | ~150 | MEDIUM | WebSocket handler, message routing, state sync | 5h |
| `tcps_sse_manager` | tcps | ~100 | MEDIUM | SSE connection management, broadcast, cleanup | 4h |
| `tcps_metrics_aggregator` | tcps | ~150 | MEDIUM | Metric collection, rollup, storage | 5h |
| `tcps_metrics_cache` | tcps | ~100 | LOW | Metric caching, TTL, eviction | 3h |
| `tcps_query_cache` | tcps | ~150 | MEDIUM | Query result caching, invalidation, statistics | 5h |
| `tcps_persistence` | tcps | ~200 | MEDIUM | State persistence, snapshot, restoration | 5h |

**Subtotal Tier 4C:** 8 modules, ~33 hours

---

### Tier 4D: TCPS Documentation & Utilities (Week 15)

**Impact:** Documentation quality. Failures reduce discoverability but don't block functionality.

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `tcps_diataxis_explain` | tcps | ~100 | LOW | Explanation content generation, examples | 2h |
| `tcps_diataxis_howto` | tcps | ~100 | LOW | How-to guide generation, step-by-step | 2h |
| `tcps_diataxis_reference` | tcps | ~100 | LOW | Reference documentation, API listing | 2h |
| `tcps_diataxis_tutorial` | tcps | ~100 | LOW | Tutorial generation, interactive steps | 2h |
| `tcps_concepts` | tcps | ~80 | LOW | Concept definitions, glossary | 1h |
| `tcps_principles` | tcps | ~80 | LOW | TCPS principles, philosophy | 1h |
| `tcps_howto_recipes` | tcps | ~100 | LOW | Recipe collection, patterns | 2h |
| `tcps_api_reference` | tcps | ~150 | LOW | API reference generation, specs | 3h |
| `tcps_config_reference` | tcps | ~100 | LOW | Config documentation, defaults | 2h |
| `tcps_ontology_index` | tcps | ~100 | LOW | Ontology indexing, term lookup | 2h |

**Subtotal Tier 4D:** 10 modules, ~19 hours

---

### Tier 4E: TCPS Simulation & Advanced (Week 16)

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `tcps_simulator` | tcps | ~250 | HIGH | Simulation engine, scenario execution, metrics | 8h |
| `tcps_simulator_state` | tcps | ~150 | MEDIUM | State management, checkpoints, rollback | 5h |
| `tcps_simulator_telemetry` | tcps | ~100 | MEDIUM | Telemetry collection, export, visualization | 4h |
| `tcps_scenario_loader` | tcps | ~150 | MEDIUM | Scenario loading, validation, execution | 5h |
| `tcps_visualization_data` | tcps | ~100 | MEDIUM | Data transformation for visualization | 4h |
| `tcps_rdf_incremental` | tcps | ~200 | HIGH | Incremental RDF updates, diff computation | 6h |

**Subtotal Tier 4E:** 6 modules, ~32 hours

---

### Tier 4F: Rebar3 Plugins (OPTIONAL)

**Impact:** Build tooling. Failures affect development workflow but not runtime.

| Module | App | LOC | Complexity | Test Scenarios | Estimated Hours |
|--------|-----|-----|------------|----------------|-----------------|
| `rebar3_tcps_plugin` | tcps | ~100 | MEDIUM | Plugin registration, command execution | 4h |
| `tcps_rebar3_andon` | tcps | ~100 | MEDIUM | Andon rebar3 provider, hooks | 4h |
| `tcps_rebar3_quality` | tcps | ~100 | MEDIUM | Quality gate provider, validation | 4h |
| `tcps_rebar3_receipt` | tcps | ~100 | MEDIUM | Receipt generation provider | 4h |
| `tcps_rebar3_shacl` | tcps | ~100 | MEDIUM | SHACL validation provider | 4h |

**Subtotal Tier 4F:** 5 modules, ~20 hours

---

## Test Implementation Strategy

### Chicago School TDD Principles

**ALL tests MUST follow Chicago School TDD:**
- ✅ **Real collaborators** - Use actual gen_servers, real processes, no mocks
- ✅ **State-based verification** - Assert on observable state changes, not method calls
- ✅ **Behavior testing** - Test what system does (outputs), not how it does it
- ✅ **Integration focus** - Test components together whenever possible
- ❌ **NO mocking** - Spawn real processes, use real dependencies
- ❌ **NO interaction verification** - Don't test which functions were called

### Test Organization

**File naming:**
- EUnit: `<module>_tests.erl` (e.g., `erlmcp_subscription_tests.erl`)
- Common Test: `<module>_SUITE.erl` (for integration scenarios)

**Test structure per module:**
```erlang
-module(module_name_tests).
-include_lib("eunit/include/eunit.hrl").

%% Setup/Teardown (Chicago School: real gen_server)
module_test_() ->
    {setup,
     fun() ->
         % Setup: Start real gen_server
         {ok, Pid} = module_name:start_link(),
         Pid
     end,
     fun(Pid) ->
         % Teardown: Stop server
         ok = module_name:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(basic_operation(Pid)),
          ?_test(error_handling(Pid)),
          ?_test(edge_cases(Pid))
         ]
     end}.

%% Test functions (Chicago School: verify state)
basic_operation(Pid) ->
    % Exercise: Call API
    ok = module_name:do_something(Pid, Args),

    % Verify: Check observable state
    {ok, Result} = module_name:get_state(Pid),
    ?assertEqual(ExpectedState, Result).
```

### Coverage Requirements

**Mandatory for ALL modules:**
- ✅ 80%+ line coverage (85%+ for core modules)
- ✅ 100% exported function coverage
- ✅ All error paths tested
- ✅ Edge cases documented and tested
- ✅ Concurrency scenarios (if applicable)
- ✅ Process supervision (if applicable)

### Test Scenarios by Category

**Gen_server modules:**
1. Start/stop lifecycle
2. API call success paths
3. Error handling (invalid input, timeouts)
4. State transitions
5. Concurrent access
6. Process crash recovery (supervision)

**Utility modules:**
1. Function success paths
2. Invalid input handling
3. Edge cases (empty, nil, max values)
4. Type validation
5. Performance characteristics

**Protocol/parser modules:**
1. Valid input parsing
2. Invalid input rejection
3. Protocol compliance
4. Roundtrip encoding/decoding
5. Error message clarity

---

## Resource Allocation

### Phase 1: Critical Core (Weeks 1-3)

**Team:** 2 test engineers
**Modules:** 17 (Tiers 1A, 1B, 1C)
**Estimated hours:** 102 hours
**Target coverage:** 85%+ for all modules
**Deliverables:**
- ✅ erlmcp_hooks_tests.erl (quality gate integration)
- ✅ erlmcp_split_brain_detector_tests.erl (partition scenarios)
- ✅ erlmcp_pricing_receipt_tests.erl (hash chain verification)
- ✅ tcps_poka_yoke_tests.erl (error-proofing)
- ✅ erlmcp_sla_monitor_tests.erl (SLA tracking)
- ✅ erlmcp_otel_middleware_tests.erl (span injection)
- ✅ erlmcp_chaos_tests.erl (failure injection)

---

### Phase 2: Medium Priority (Weeks 4-7)

**Team:** 1-2 test engineers
**Modules:** 21 (Tiers 2A, 2B, 2C)
**Estimated hours:** 80 hours
**Target coverage:** 80%+ for all modules
**Deliverables:**
- ✅ erlmcp_graceful_drain_tests.erl
- ✅ erlmcp_rate_limiter_tests.erl
- ✅ erlmcp_schema_validator_tests.erl
- ✅ erlmcp_pricing_upgrade_tests.erl
- ✅ erlmcp_chaos_network_tests.erl
- ✅ erlmcp_metrics_aggregator_tests.erl

---

### Phase 3: Transports (Week 7-8)

**Team:** 1 transport specialist
**Modules:** 4 (Tier 3A)
**Estimated hours:** 19 hours
**Target coverage:** 80%+
**Deliverables:**
- ✅ erlmcp_transport_pipeline_tests.erl (HTTP/2, WebSocket)
- ✅ erlmcp_pool_strategy_tests.erl
- ✅ erlmcp_security_headers_tests.erl
- ✅ erlmcp_transport_http_server_tests.erl

---

### Phase 4: TCPS Ecosystem (Weeks 9-16 - OPTIONAL)

**Team:** 1 test engineer (part-time)
**Modules:** 34 (Tiers 4A-4F)
**Estimated hours:** 151 hours
**Target coverage:** 70%+ (lower priority)
**Deliverables:**
- ✅ All TCPS CLI tools tested
- ✅ TCPS core components tested
- ✅ Dashboard and infrastructure tested
- ✅ Documentation generators tested
- ⚠️ Rebar3 plugins optional (defer if low priority)

---

## Execution Timeline

### Sprint 1-3 (Weeks 1-3): Critical Core
- **Week 1:** Tier 1A (5 modules, 25h) - Core infrastructure
- **Week 2:** Tier 1B (6 modules, 38h) - Pricing & SLA
- **Week 3:** Tier 1C (6 modules, 39h) - Observability core

**Milestone 1:** 17 critical modules tested (85%+ coverage)

---

### Sprint 4-7 (Weeks 4-7): Medium Priority
- **Week 4-5:** Tier 2A (6 modules, 24h) - Core features
- **Week 5-6:** Tier 2B (7 modules, 23h) - Pricing support
- **Week 6-7:** Tier 2C (8 modules, 33h) - Observability features

**Milestone 2:** 38 modules tested (80%+ coverage)

---

### Sprint 8 (Week 7-8): Transports
- **Week 7-8:** Tier 3A (4 modules, 19h) - Transport features

**Milestone 3:** 42 modules tested

---

### Sprint 9+ (Weeks 9-16): TCPS Ecosystem (OPTIONAL)
- **Week 9-10:** Tier 4A (8 modules, 20h) - CLI tools
- **Week 11-12:** Tier 4B (6 modules, 27h) - TCPS core
- **Week 13-14:** Tier 4C (8 modules, 33h) - Infrastructure
- **Week 15:** Tier 4D (10 modules, 19h) - Documentation
- **Week 16:** Tier 4E (6 modules, 32h) - Simulation

**Milestone 4:** 76 modules tested (COMPLETE)

---

## Success Metrics

### Coverage Targets

**By app:**
- ✅ erlmcp_core: 85%+ coverage (critical)
- ✅ erlmcp_observability: 80%+ coverage (production ops)
- ✅ erlmcp_transports: 80%+ coverage (protocol compliance)
- ⚠️ tcps_erlmcp: 70%+ coverage (developer tooling)

**By module:**
- Core modules (Tier 1): 85%+ coverage
- Medium priority (Tier 2): 80%+ coverage
- Transports (Tier 3): 80%+ coverage
- TCPS (Tier 4): 70%+ coverage

### Quality Gates

**ALL modules MUST:**
- ✅ 100% exported function coverage
- ✅ All error paths tested
- ✅ Concurrency scenarios tested (if applicable)
- ✅ Chicago School TDD compliance verified
- ✅ Zero test failures (100% pass rate)
- ✅ All tests pass on clean build (`rebar3 eunit`)

---

## Risk Mitigation

### High-Risk Modules

**Modules with high complexity + no tests:**
1. `erlmcp_hooks` (597 LOC) - Quality gate integration critical
2. `erlmcp_pricing_receipt` (742 LOC) - Hash chain must be immutable
3. `erlmcp_pricing_upgrade` (729 LOC) - Revenue impact if bugs
4. `erlmcp_chaos` (762 LOC) - Chaos testing must not destabilize prod
5. `tcps_poka_yoke` (528 LOC) - Error-proofing is ironic if untested

**Mitigation:**
- Assign most experienced test engineers to these modules
- Pair programming for complex scenarios
- Code review by module author before merge
- 90%+ coverage target for these 5 modules

---

## Next Steps

### Immediate Actions (Week 1)

1. **Create test file structure:**
   ```bash
   # Tier 1A modules
   touch apps/erlmcp_core/test/erlmcp_subscription_tests.erl
   touch apps/erlmcp_core/test/erlmcp_hooks_tests.erl
   touch apps/erlmcp_core/test/erlmcp_split_brain_detector_tests.erl
   touch apps/erlmcp_core/test/erlmcp_secrets_tests.erl
   touch apps/erlmcp_core/test/erlmcp_message_handler_tests.erl
   ```

2. **Assign test engineers:**
   - Engineer 1: erlmcp_hooks, erlmcp_split_brain_detector
   - Engineer 2: erlmcp_subscription, erlmcp_secrets, erlmcp_message_handler

3. **Set up CI validation:**
   - Add `rebar3 eunit` to CI pipeline
   - Add coverage reporting (`rebar3 cover`)
   - Block merges if coverage drops below 80%

4. **Establish test review process:**
   - All tests reviewed by senior engineer
   - Chicago School TDD compliance verified
   - Coverage reports included in PR

---

## Appendix: Module Categorization

### By Criticality

**P0 (Critical - Blocks core MCP functionality):**
- erlmcp_hooks, erlmcp_split_brain_detector, erlmcp_subscription
- erlmcp_message_handler, erlmcp_secrets
- erlmcp_pricing_receipt, tcps_poka_yoke, erlmcp_sla_monitor

**P1 (High - Revenue/compliance impact):**
- erlmcp_pricing_plan, erlmcp_pricing_upgrade, erlmcp_sla_envelope
- erlmcp_receipt_chain, erlmcp_otel_middleware, erlmcp_chaos

**P2 (Medium - Production operations):**
- erlmcp_graceful_drain, erlmcp_node_monitor, erlmcp_rate_limiter
- erlmcp_chaos_network, erlmcp_chaos_process, erlmcp_metrics_aggregator

**P3 (Low - Developer tooling):**
- All TCPS CLI modules, documentation generators, rebar3 plugins

---

### By Complexity (Lines of Code)

**High Complexity (>400 LOC):**
- erlmcp_chaos (762)
- erlmcp_pricing_receipt (742)
- erlmcp_pricing_upgrade (729)
- erlmcp_hooks (597)
- tcps_poka_yoke (528)
- erlmcp_pricing_plan (479)
- erlmcp_sla_monitor (414)

**Medium Complexity (100-400 LOC):**
- erlmcp_otel_middleware (317)
- erlmcp_otel_jaeger (285)
- erlmcp_otel_honeycomb (252)
- erlmcp_otel_datadog (232)
- erlmcp_split_brain_detector (221)
- erlmcp_transport_pipeline (~250)

**Low Complexity (<100 LOC):**
- erlmcp_subscription (51)
- All pricing utilities (29-108 LOC)
- All TCPS CLI tools (80-100 LOC)

---

### By Test Approach

**Gen_server modules (supervision tests required):**
- erlmcp_hooks, erlmcp_split_brain_detector, erlmcp_sla_monitor
- erlmcp_node_monitor, erlmcp_rate_limiter, erlmcp_graceful_drain

**Stateless utility modules (unit tests only):**
- erlmcp_pricing_util, erlmcp_pricing_validator, erlmcp_message_size
- All TCPS CLI formatters

**Protocol/encoding modules (roundtrip tests):**
- erlmcp_schema_validator, erlmcp_message_handler
- erlmcp_receipt_chain, erlmcp_pricing_receipt

**Integration-heavy modules (Common Test suites):**
- erlmcp_chaos (multi-process failure injection)
- erlmcp_otel_middleware (transport integration)
- erlmcp_transport_pipeline (HTTP/2, WebSocket)

---

## Conclusion

**Total effort:** ~370 hours (9-10 weeks with 2 engineers)
**Priority 1 effort:** ~102 hours (3 weeks, critical modules)
**ROI:** 76 modules tested → 43% coverage gap eliminated

**Recommendation:** Execute Phase 1-3 immediately (Weeks 1-8, 42 modules). Defer TCPS testing (Phase 4) until after core coverage hits 85%.

---

**Generated by:** erlang-test-engineer agent
**Analysis method:** Chicago School TDD principles, criticality-complexity matrix
**Validation:** Manual review of all 76 modules without tests
**Last updated:** 2026-01-28
