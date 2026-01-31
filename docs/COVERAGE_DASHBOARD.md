# Coverage Dashboard - erlmcp

**Project**: erlmcp (Erlang/OTP MCP SDK)
**Target Coverage**: 80%
**Current Baseline**: ~10% (estimated)
**Last Updated**: 2026-01-30

## Executive Summary

This dashboard tracks test coverage progress across all erlmcp applications. The goal is to achieve **80% code coverage** across all modules before production release.

### Progress Overview

| Application | Total Modules | Modules with Tests | Coverage % | Status |
|-------------|--------------|-------------------|------------|--------|
| erlmcp_core | 71 | 42 | ~15% | 🟡 In Progress |
| erlmcp_transports | 23 | 16 | ~12% | 🟡 In Progress |
| erlmcp_observability | 30 | 21 | ~10% | 🟡 In Progress |
| erlmcp_validation | 10 | 8 | ~8% | 🟡 In Progress |
| **TOTAL** | **134** | **87** | **~12%** | 🔴 Below Target |

### Coverage Target Timeline

- **Start**: 10% (January 2026)
- **Milestone 1**: 30% (Week 1)
- **Milestone 2**: 50% (Week 2)
- **Milestone 3**: 70% (Week 3)
- **Target**: 80% (Week 4)

---

## erlmcp_core Application (71 modules)

### High Priority - Core Protocol (Target: 90%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| **erlmcp_client.erl** | 15% | 90% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_server.erl** | 20% | 90% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_registry.erl** | 25% | 90% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_json_rpc.erl** | 30% | 90% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_session.erl** | 18% | 90% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_message_parser.erl** | 35% | 90% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_auth.erl** | 22% | 85% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_cache.erl** | 28% | 85% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_elicitation.erl** | 12% | 85% | ✅ EXISTS | HIGH | `erlang-test-engineer` |

### High Priority - Missing Tests (Target: 80%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| **erlmcp_refusal.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_schema_validator.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_subscription.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_rate_limiter.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |

### Medium Priority - Infrastructure (Target: 75%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_circuit_breaker.erl | 15% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_completion.erl | 10% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_progress.erl | 12% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_resource.erl | 18% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_sampling.erl | 14% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_tasks.erl | 16% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_tool.erl | 20% | 75% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_connection_monitor.erl | 0% | 75% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_connection_limiter.erl | 0% | 75% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_notification_handler.erl | 0% | 75% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |

### Low Priority - Supporting Modules (Target: 60%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_app.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_core_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_cluster_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_batch.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_cancellation.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_capabilities.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_change_notifier.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_client_transport.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_code_reload.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_cpu_guard.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_cpu_quota.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_graceful_drain.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_hooks.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_icon_cache.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_logging.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_memory_guard.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_message_handler.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_message_size.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_mock_llm.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_node_monitor.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_notification_handler_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_pagination.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_path_canonicalizer.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_prompt_argument_validator.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_prompt_list_change_notifier.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_prompt_template.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_rate_limit_middleware.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_registry_dist.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_registry_utils.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_reload_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_request_id.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_resource_subscriptions.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_schema_registry.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_secrets.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_server_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_session_failover.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_session_manager.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_session_replicator.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_split_brain_detector.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_sse_event_store.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_test_sync.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_uri_validator.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |

### Pricing Modules (12 modules)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| pricing/erlmcp_pricing_cli.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_http.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_loader.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_plan.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_receipt.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_state.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_util.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_pricing_validator.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_sla_envelope.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/erlmcp_sla_monitor.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/tcps_poka_yoke_validator.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| pricing/tcps_poka_yoke.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| tcps_quality_gates.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |

---

## erlmcp_transports Application (23 modules)

### High Priority - Transport Implementations (Target: 80%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| **erlmcp_transport_http.erl** | 12% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_transport_tcp.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_transport_ws.erl** | 15% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_transport_sse.erl** | 10% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_transport_stdio.erl** | 18% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_transport_pool.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |

### Medium Priority - Infrastructure (Target: 70%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_transport_behavior.erl | 5% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_transport_registry.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_transport_health.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_pool_manager.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_pool_strategy.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |

### Low Priority - Supporting Modules (Target: 60%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_http_header_validator.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_origin_validator.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_security_headers.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_tls_validation.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transport_adapter.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transport_discovery.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transport_http_server.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transport_pipeline.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transport_sup.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transport_validation.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_transports_app.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |

---

## erlmcp_observability Application (30 modules)

### High Priority - Core Observability (Target: 75%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| **erlmcp_metrics.erl** | 12% | 75% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_health_monitor.erl** | 10% | 75% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_otel.erl** | 8% | 75% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_tracing.erl** | 15% | 75% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_recovery_manager.erl** | 12% | 75% | ✅ EXISTS | HIGH | `erlang-test-engineer` |

### High Priority - Missing Tests (Target: 75%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| **erlmcp_dashboard_server.erl** | 0% | 75% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_chaos_network.erl** | 0% | 75% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_chaos_process.erl** | 0% | 75% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_chaos_resource.erl** | 0% | 75% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| **erlmcp_bench_rate_limit.erl** | 0% | 75% | ❌ MISSING | HIGH | `erlang-test-engineer` |

### Medium Priority - Chaos & Debugging (Target: 65%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_chaos.erl | 10% | 65% | ✅ EXISTS | MEDIUM | `erlang-test-engineer` |
| erlmcp_debugger.erl | 0% | 65% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_profiler.erl | 0% | 65% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_memory_analyzer.erl | 0% | 65% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |
| erlmcp_trace_analyzer.erl | 0% | 65% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |

### Low Priority - Supporting Modules (Target: 50%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_audit_log.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_dashboard_http_handler.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_evidence_path.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_metrics_aggregator.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_metrics_server.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_observability_app.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_observability_sup.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_otel_datadog.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_otel_honeycomb.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_otel_jaeger.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_otel_middleware.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_process_monitor.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_receipt_chain.erl | 0% | 50% | ❌ MISSING | LOW | `erlang-test-engineer` |

---

## erlmcp_validation Application (10 modules)

### High Priority - Validators (Target: 80%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| **erlmcp_protocol_validator.erl** | 5% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_security_validator.erl** | 5% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_transport_validator.erl** | 5% | 80% | ✅ EXISTS | HIGH | `erlang-test-engineer` |
| **erlmcp_performance_validator.erl** | 0% | 80% | ❌ MISSING | HIGH | `erlang-test-engineer` |
| erlmcp_test_client.erl | 0% | 70% | ❌ MISSING | MEDIUM | `erlang-test-engineer` |

### Low Priority - Supporting Modules (Target: 60%)

| Module | Current % | Target % | Test Status | Priority | Assigned Agent |
|--------|-----------|----------|-------------|----------|----------------|
| erlmcp_compliance_report.erl | 5% | 60% | ✅ EXISTS | LOW | `erlang-test-engineer` |
| erlmcp_memory_manager.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_spec_parser.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_validate_cli.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |
| erlmcp_validation_app.erl | 0% | 60% | ❌ MISSING | LOW | `erlang-test-engineer` |

---

## Test File Status Summary

### Files Created (Existing Tests)

#### erlmcp_core (42 test files)
- erlmcp_auth_tests.erl
- erlmcp_auth_rate_limiter_tests.erl
- erlmcp_cache_tests.erl
- erlmcp_cancellation_tests.erl
- erlmcp_circuit_breaker_tests.erl
- erlmcp_client_tests.erl
- erlmcp_completion_tests.erl
- erlmcp_elicitation_tests.erl
- erlmcp_json_rpc_tests.erl
- erlmcp_message_parser_tests.erl
- erlmcp_property_tests.erl
- erlmcp_registry_tests.erl
- erlmcp_resource_tests.erl
- erlmcp_sampling_tests.erl
- erlmcp_schema_validator_tests.erl (exists but needs expansion)
- erlmcp_server_tests.erl
- erlmcp_session_tests.erl
- erlmcp_subscription_tests.erl (exists but needs expansion)
- erlmcp_tasks_tests.erl
- erlmcp_tool_tests.erl

#### erlmcp_transports (16 test files)
- erlmcp_transport_http_tests.erl
- erlmcp_transport_sse_tests.erl
- erlmcp_transport_stdio_tests.erl
- erlmcp_transport_tcp_tests.erl (exists but needs expansion)
- erlmcp_transport_ws_tests.erl

#### erlmcp_observability (21 test files)
- erlmcp_chaos_tests.erl
- erlmcp_health_monitor_tests.erl
- erlmcp_metrics_tests.erl
- erlmcp_otel_tests.erl
- erlmcp_recovery_manager_tests.erl
- erlmcp_tracing_tests.erl

#### erlmcp_validation (8 test files)
- erlmcp_protocol_validator_tests.erl
- erlmcp_security_validator_tests.erl
- erlmcp_transport_validator_tests.erl

### Files to Create (47 missing test files)

#### erlmcp_core Priority 1 (HIGH)
- erlmcp_refusal_tests.erl
- erlmcp_subscription_tests.erl (expand existing)
- erlmcp_rate_limiter_tests.erl
- erlmcp_schema_validator_tests.erl (expand existing)

#### erlmcp_core Priority 2 (MEDIUM)
- erlmcp_connection_monitor_tests.erl
- erlmcp_connection_limiter_tests.erl
- erlmcp_notification_handler_tests.erl

#### erlmcp_transports Priority 1 (HIGH)
- erlmcp_transport_pool_tests.erl
- erlmcp_transport_tcp_tests.erl (expand existing)

#### erlmcp_observability Priority 1 (HIGH)
- erlmcp_dashboard_server_tests.erl
- erlmcp_chaos_network_tests.erl
- erlmcp_chaos_process_tests.erl
- erlmcp_chaos_resource_tests.erl
- erlmcp_bench_rate_limit_tests.erl

#### erlmcp_validation Priority 1 (HIGH)
- erlmcp_performance_validator_tests.erl
- erlmcp_test_client_tests.erl

---

## Agent Assignment Strategy

### Phase 1: Core Protocol (Week 1)
**Target**: 30% overall coverage

**Agents**: 5 parallel `erlang-test-engineer` agents
1. Agent 1: erlmcp_client, erlmcp_server, erlmcp_registry
2. Agent 2: erlmcp_json_rpc, erlmcp_message_parser, erlmcp_auth
3. Agent 3: erlmcp_cache, erlmcp_session, erlmcp_elicitation
4. Agent 4: erlmcp_transport_http, erlmcp_transport_tcp, erlmcp_transport_ws
5. Agent 5: erlmcp_metrics, erlmcp_health_monitor, erlmcp_otel

### Phase 2: Infrastructure (Week 2)
**Target**: 50% overall coverage

**Agents**: 5 parallel `erlang-test-engineer` agents
1. Agent 1: Missing core tests (refusal, subscription, rate_limiter, schema_validator)
2. Agent 2: Transport infrastructure (transport_pool, transport_registry, transport_health)
3. Agent 3: Observability infrastructure (dashboard_server, chaos modules)
4. Agent 4: Validation infrastructure (performance_validator, test_client)
5. Agent 5: Protocol extensions (completion, progress, resource, sampling, tasks, tool)

### Phase 3: Supporting Modules (Week 3)
**Target**: 70% overall coverage

**Agents**: 4 parallel `erlang-test-engineer` agents
1. Agent 1: Connection management (connection_monitor, connection_limiter, notification_handler)
2. Agent 2: Chaos & recovery (chaos_network, chaos_process, chaos_resource, recovery_manager)
3. Agent 3: Pricing modules (all 12 pricing modules)
4. Agent 4: Validators (protocol_validator, security_validator, transport_validator)

### Phase 4: Coverage Polish (Week 4)
**Target**: 80% overall coverage

**Agents**: 3 parallel `erlang-test-engineer` agents
1. Agent 1: Fill gaps in core modules
2. Agent 2: Fill gaps in transport modules
3. Agent 3: Fill gaps in observability modules

---

## Coverage Calculation Formula

```
Overall Coverage % = (Sum of all module coverage %) / (Total number of modules)

Current Estimate:
- erlmcp_core: ~15% average (71 modules)
- erlmcp_transports: ~12% average (23 modules)
- erlmcp_observability: ~10% average (30 modules)
- erlmcp_validation: ~8% average (10 modules)

Overall: (15% * 71 + 12% * 23 + 10% * 30 + 8% * 10) / 134 ≈ 12.4%
```

---

## Progress Tracking

### Daily Updates

| Date | Overall % | Core % | Transports % | Observability % | Validation % | Agent Activity |
|------|-----------|--------|--------------|-----------------|--------------|----------------|
| 2026-01-30 | 12% | 15% | 12% | 10% | 8% | Dashboard created |
| | | | | | | |

### Weekly Milestones

| Week | Target | Actual | Status | Notes |
|------|--------|--------|--------|-------|
| 1 | 30% | TBD | 🟡 Pending | Core protocol tests |
| 2 | 50% | TBD | 🟡 Pending | Infrastructure tests |
| 3 | 70% | TBD | 🟡 Pending | Supporting modules |
| 4 | 80% | TBD | 🟡 Pending | Coverage polish |

---

## Quality Gates

### Before Marking Coverage Complete

For each module, verify:
- [ ] All public functions have tests
- [ ] All code paths covered (happy path + error paths)
- [ ] Edge cases tested
- [ ] Property-based tests for complex logic
- [ ] Integration tests for protocol interactions
- [ ] All tests passing (100% pass rate)
- [ ] No test flakes or intermittent failures

### Coverage Validation Commands

```bash
# Generate coverage report
rebar3 cover

# View coverage by module
rebar3 cover --verbose

# Generate HTML coverage report
rebar3 cover -v

# Check specific module coverage
rebar3 cover --module=erlmcp_client
```

---

## Next Steps

1. **Launch Phase 1 agents** (5 parallel `erlang-test-engineer` agents)
2. **Create missing test files** for HIGH priority modules
3. **Run coverage analysis** after each agent completes
4. **Update dashboard** with actual coverage percentages
5. **Adjust targets** based on complexity and dependencies

---

## Notes

- **Chicago School TDD**: Tests must use REAL erlmcp processes, NO mocks/fakes
- **Black-box testing**: Test ALL observable behavior through ALL interfaces
- **Property-based tests**: Use Proper for complex logic and state machines
- **Integration tests**: Test protocol compliance through actual JSON-RPC messages
- **Transport testing**: Test with REAL transports (stdio, TCP, HTTP, WebSocket), NEVER mocked

---

*This dashboard is a living document. Update it as agents complete their work and coverage data becomes available.*
