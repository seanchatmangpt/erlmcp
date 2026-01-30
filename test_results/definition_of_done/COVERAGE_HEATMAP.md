# Coverage Heatmap Report

**Generated**: 2026-01-30
**Threshold**: ≥80% coverage required
**Overall Project Coverage**: 3.00%

---

## Visual Legend

| Coverage Range | Indicator | Status | Action Required |
|---------------|-----------|--------|-----------------|
| 90-100% | 🟢 | Excellent | Maintain coverage |
| 80-89% | 🟡 | Good | Monitor for regression |
| 50-79% | 🟠 | Fair | Needs improvement |
| 1-49% | 🔴 | Poor | Critical - needs tests |
| 0% | ⚫ | Critical | URGENT - no coverage |

---

## Overall Heatmap

```
Coverage Distribution:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

0%       ████████████████████████████████████████████████████ 130 modules (93.5%)
1-20%    ███ 5 modules (3.6%)
21-50%   ███ 6 modules (4.3%)
51-79%   █ 1 module (0.7%)
80-100%  █ 1 module (0.7%)

Overall: 3.00% coverage [77 percentage points below threshold]
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## Module Coverage Heatmap

### PASSING Modules (≥80%)

| Module | Coverage | Lines | Indicator | Status |
|--------|----------|-------|-----------|--------|
| erlmcp_pool_manager | 83.00% | N/A | 🟢 | ✅ PASS |

**Summary**: 1 of 139 modules (0.72%) passing

---

### FAILING Modules (<80%)

#### GOOD COVERAGE (50-79%)

| Module | Coverage | Gap | Lines | Indicator | Priority |
|--------|----------|-----|-------|-----------|----------|
| erlmcp_transport_discovery | 58.00% | -22% | N/A | 🟠 | MEDIUM |

**Summary**: 1 module needs improvement to reach 80%

#### FAIR COVERAGE (21-50%)

| Module | Coverage | Gap | Lines | Indicator | Priority |
|--------|----------|-----|-------|-----------|----------|
| erlmcp_pool_strategy | 41.00% | -39% | N/A | 🔴 | HIGH |
| erlmcp_json_rpc | 35.00% | -45% | N/A | 🔴 | HIGH |
| erlmcp_compliance_report | 31.00% | -49% | N/A | 🔴 | CRITICAL |
| erlmcp_message_parser | 26.00% | -54% | N/A | 🔴 | CRITICAL |
| erlmcp_transport_http | 25.00% | -55% | N/A | 🔴 | CRITICAL |
| erlmcp_message_size | 23.00% | -57% | N/A | 🔴 | CRITICAL |
| erlmcp_transport_http_server | 20.00% | -60% | N/A | 🔴 | CRITICAL |

**Summary**: 7 modules need significant test expansion

#### LOW COVERAGE (1-20%)

| Module | Coverage | Gap | Lines | Indicator | Priority |
|--------|----------|-----|-------|-----------|----------|
| erlmcp_memory_manager | 3.00% | -77% | N/A | 🔴 | CRITICAL |
| erlmcp_transport_registry | 3.00% | -77% | N/A | 🔴 | CRITICAL |

**Summary**: 2 modules have minimal coverage

#### ZERO COVERAGE (0%) - CRITICAL PRIORITY

**Core Protocol Modules (⚫ = URGENT)**

| Module | Coverage | Priority | Category |
|--------|----------|----------|----------|
| erlmcp | 0.00% | CRITICAL | Core |
| erlmcp_app | 0.00% | CRITICAL | Core |
| erlmcp_audit_log | 0.00% | CRITICAL | Observability |
| erlmcp_auth | 0.00% | CRITICAL | Security |
| erlmcp_auth_rate_limiter | 0.00% | CRITICAL | Security |
| erlmcp_batch | 0.00% | HIGH | Core |
| erlmcp_bench_rate_limit | 0.00% | LOW | Benchmark |
| erlmcp_cache | 0.00% | HIGH | Core |
| erlmcp_cancellation | 0.00% | CRITICAL | Core |
| erlmcp_capabilities | 0.00% | CRITICAL | Core |
| erlmcp_change_notifier | 0.00% | MEDIUM | Observability |
| erlmcp_chaos | 0.00% | HIGH | Observability |
| erlmcp_chaos_network | 0.00% | HIGH | Observability |
| erlmcp_chaos_process | 0.00% | HIGH | Observability |
| erlmcp_chaos_resource | 0.00% | HIGH | Observability |
| erlmcp_circuit_breaker | 0.00% | CRITICAL | Core |
| erlmcp_client | 0.00% | CRITICAL | Core Protocol |
| erlmcp_client_transport | 0.00% | CRITICAL | Transport |
| erlmcp_cluster_sup | 0.00% | MEDIUM | Cluster |
| erlmcp_code_reload | 0.00% | MEDIUM | Utilities |
| erlmcp_completion | 0.00% | CRITICAL | Core |
| erlmcp_connection_limiter | 0.00% | CRITICAL | Core |
| erlmcp_connection_monitor | 0.00% | HIGH | Observability |
| erlmcp_core_sup | 0.00% | HIGH | Supervision |
| erlmcp_cpu_guard | 0.00% | MEDIUM | Resource |
| erlmcp_cpu_quota | 0.00% | MEDIUM | Resource |
| erlmcp_dashboard_http_handler | 0.00% | MEDIUM | Observability |
| erlmcp_dashboard_server | 0.00% | MEDIUM | Observability |
| erlmcp_debugger | 0.00% | MEDIUM | Observability |
| erlmcp_evidence_path | 0.00% | MEDIUM | Observability |
| erlmcp_graceful_drain | 0.00% | MEDIUM | Shutdown |
| erlmcp_health_monitor | 0.00% | CRITICAL | Observability |
| erlmcp_hooks | 0.00% | MEDIUM | Utilities |
| erlmcp_http_header_validator | 0.00% | MEDIUM | Validation |
| erlmcp_icon_cache | 0.00% | LOW | UI |
| erlmcp_logging | 0.00% | MEDIUM | Logging |
| erlmcp_memory_analyzer | 0.00% | MEDIUM | Observability |
| erlmcp_memory_guard | 0.00% | MEDIUM | Resource |
| erlmcp_message_handler | 0.00% | CRITICAL | Core |
| erlmcp_metrics | 0.00% | CRITICAL | Observability |
| erlmcp_metrics_aggregator | 0.00% | HIGH | Observability |
| erlmcp_metrics_server | 0.00% | HIGH | Observability |
| erlmcp_mock_llm | 0.00% | LOW | Testing |
| erlmcp_node_monitor | 0.00% | MEDIUM | Cluster |
| erlmcp_notification_handler | 0.00% | MEDIUM | Notification |
| erlmcp_notification_handler_sup | 0.00% | MEDIUM | Supervision |
| erlmcp_observability_app | 0.00% | MEDIUM | Application |
| erlmcp_observability_sup | 0.00% | MEDIUM | Supervision |
| erlmcp_origin_validator | 0.00% | MEDIUM | Validation |
| erlmcp_otel | 0.00% | HIGH | Telemetry |
| erlmcp_otel_datadog | 0.00% | MEDIUM | Telemetry |
| erlmcp_otel_honeycomb | 0.00% | MEDIUM | Telemetry |
| erlmcp_otel_jaeger | 0.00% | MEDIUM | Telemetry |
| erlmcp_otel_middleware | 0.00% | MEDIUM | Telemetry |
| erlmcp_pagination | 0.00% | LOW | Utilities |
| erlmcp_path_canonicalizer | 0.00% | MEDIUM | Validation |
| erlmcp_pricing_cli | 0.00% | LOW | Pricing |
| erlmcp_pricing_http | 0.00% | LOW | Pricing |
| erlmcp_pricing_loader | 0.00% | LOW | Pricing |
| erlmcp_pricing_plan | 0.00% | LOW | Pricing |
| erlmcp_pricing_receipt | 0.00% | LOW | Pricing |
| erlmcp_pricing_state | 0.00% | LOW | Pricing |
| erlmcp_pricing_util | 0.00% | LOW | Pricing |
| erlmcp_pricing_validator | 0.00% | LOW | Pricing |
| erlmcp_process_monitor | 0.00% | MEDIUM | Observability |
| erlmcp_profiler | 0.00% | MEDIUM | Observability |
| erlmcp_progress | 0.00% | CRITICAL | Core |
| erlmcp_prompt_list_change_notifier | 0.00% | MEDIUM | Prompts |
| erlmcp_prompt_template | 0.00% | CRITICAL | Prompts |
| erlmcp_protocol_validator | 0.00% | HIGH | Validation |
| erlmcp_rate_limit_middleware | 0.00% | MEDIUM | Rate Limiting |
| erlmcp_rate_limiter | 0.00% | CRITICAL | Core |
| erlmcp_receipt_chain | 0.00% | MEDIUM | Observability |
| erlmcp_recovery_manager | 0.00% | HIGH | Observability |
| erlmcp_registry | 0.00% | CRITICAL | Core Protocol |
| erlmcp_registry_dist | 0.00% | HIGH | Registry |
| erlmcp_registry_utils | 0.00% | HIGH | Registry |
| erlmcp_reload_sup | 0.00% | MEDIUM | Supervision |
| erlmcp_request_id | 0.00% | MEDIUM | Utilities |
| erlmcp_resource | 0.00% | CRITICAL | Resources |
| erlmcp_resource_subscriptions | 0.00% | CRITICAL | Resources |
| erlmcp_sampling | 0.00% | CRITICAL | Core |
| erlmcp_schema_registry | 0.00% | MEDIUM | Schema |
| erlmcp_secrets | 0.00% | HIGH | Security |
| erlmcp_security_headers | 0.00% | MEDIUM | Security |
| erlmcp_security_validator | 0.00% | HIGH | Security |
| erlmcp_server | 0.00% | CRITICAL | Core Protocol |
| erlmcp_server_sup | 0.00% | HIGH | Supervision |
| erlmcp_session | 0.00% | CRITICAL | Session |
| erlmcp_session_failover | 0.00% | MEDIUM | Session |
| erlmcp_session_manager | 0.00% | HIGH | Session |
| erlmcp_session_replicator | 0.00% | MEDIUM | Session |
| erlmcp_sla_envelope | 0.00% | LOW | SLA |
| erlmcp_sla_monitor | 0.00% | LOW | SLA |
| erlmcp_split_brain_detector | 0.00% | MEDIUM | Cluster |
| erlmcp_sse_event_store | 0.00% | MEDIUM | SSE |
| erlmcp_subscription | 0.00% | CRITICAL | Resources |
| erlmcp_sup | 0.00% | HIGH | Supervision |
| erlmcp_tasks | 0.00% | CRITICAL | Tasks |
| erlmcp_test_sync | 0.00% | LOW | Testing |
| erlmcp_tool | 0.00% | CRITICAL | Tools |
| erlmcp_trace_analyzer | 0.00% | MEDIUM | Observability |
| erlmcp_tracing | 0.00% | CRITICAL | Observability |
| erlmcp_transport_adapter | 0.00% | MEDIUM | Transport |
| erlmcp_transport_behavior | 0.00% | HIGH | Transport |
| erlmcp_transport_health | 0.00% | MEDIUM | Transport |
| erlmcp_transport_pipeline | 0.00% | MEDIUM | Transport |
| erlmcp_transport_pool | 0.00% | CRITICAL | Transport |
| erlmcp_transport_sse | 0.00% | CRITICAL | Transport |
| erlmcp_transport_stdio | 0.00% | CRITICAL | Transport |
| erlmcp_transport_sup | 0.00% | MEDIUM | Supervision |
| erlmcp_transport_tcp | 0.00% | CRITICAL | Transport |
| erlmcp_transport_validation | 0.00% | HIGH | Transport |
| erlmcp_transport_ws | 0.00% | CRITICAL | Transport |
| erlmcp_transports_app | 0.00% | MEDIUM | Application |
| erlmcp_uri_validator | 0.00% | MEDIUM | Validation |
| erlmcp_validate_cli | 0.00% | LOW | Validation |
| erlmcp_validation_app | 0.00% | MEDIUM | Application |
| tcps_poka_yoke | 0.00% | MEDIUM | TCPS |
| tcps_poka_yoke_validator | 0.00% | MEDIUM | TCPS |

**Summary**: 130 modules have ZERO coverage (93.5% of all modules)

---

## Critical Path Heatmap

### Tier 1: Core Protocol (CRITICAL)

```
Module                      Coverage    Gap    Indicator
──────────────────────────────────────────────────────────
erlmcp_client                  0.00%  -80%   ⚫ URGENT
erlmcp_server                  0.00%  -80%   ⚫ URGENT
erlmcp_registry                0.00%  -80%   ⚫ URGENT
erlmcp_json_rpc               35.00%  -45%   🔴 CRITICAL
erlmcp_session                 0.00%  -80%   ⚫ URGENT
erlmcp_message_handler         0.00%  -80%   ⚫ URGENT
erlmcp_message_parser         26.00%  -54%   🔴 CRITICAL
```

**Tier 1 Average**: 8.71% coverage (71.29 points below threshold)

### Tier 2: Transport Layer (CRITICAL)

```
Module                      Coverage    Gap    Indicator
──────────────────────────────────────────────────────────
erlmcp_transport_stdio         0.00%  -80%   ⚫ URGENT
erlmcp_transport_tcp           0.00%  -80%   ⚫ URGENT
erlmcp_transport_ws            0.00%  -80%   ⚫ URGENT
erlmcp_transport_sse           0.00%  -80%   ⚫ URGENT
erlmcp_transport_http         25.00%  -55%   🔴 CRITICAL
erlmcp_transport_pool          0.00%  -80%   ⚫ URGENT
erlmcp_transport_registry      3.00%  -77%   🔴 CRITICAL
erlmcp_transport_behavior      0.00%  -80%   ⚫ URGENT
```

**Tier 2 Average**: 3.50% coverage (76.50 points below threshold)

### Tier 3: Observability (HIGH)

```
Module                      Coverage    Gap    Indicator
──────────────────────────────────────────────────────────
erlmcp_metrics                 0.00%  -80%   ⚫ URGENT
erlmcp_tracing                 0.00%  -80%   ⚫ URGENT
erlmcp_health_monitor          0.00%  -80%   ⚫ URGENT
erlmcp_dashboard_server        0.00%  -80%   ⚫ URGENT
erlmcp_chaos                   0.00%  -80%   ⚫ URGENT
erlmcp_recovery_manager        0.00%  -80%   ⚫ URGENT
erlmcp_audit_log               0.00%  -80%   ⚫ URGENT
erlmcp_receipt_chain           0.00%  -80%   ⚫ URGENT
```

**Tier 3 Average**: 0.00% coverage (80 points below threshold)

### Tier 4: Resources & Tools (CRITICAL)

```
Module                      Coverage    Gap    Indicator
──────────────────────────────────────────────────────────
erlmcp_resource                0.00%  -80%   ⚫ URGENT
erlmcp_subscription            0.00%  -80%   ⚫ URGENT
erlmcp_tool                    0.00%  -80%   ⚫ URGENT
erlmcp_prompt_template         0.00%  -80%   ⚫ URGENT
erlmcp_tasks                   0.00%  -80%   ⚫ URGENT
erlmcp_completion              0.00%  -80%   ⚫ URGENT
erlmcp_progress                0.00%  -80%   ⚫ URGENT
```

**Tier 4 Average**: 0.00% coverage (80 points below threshold)

---

## Priority Heatmap Matrix

| Priority | Count | Modules | Example Modules |
|----------|-------|---------|-----------------|
| 🔴 CRITICAL | 47 | Core protocol, transports, resources | erlmcp_client, erlmcp_server, erlmcp_transport_tcp |
| 🟠 HIGH | 28 | Observability, security, validation | erlmcp_metrics, erlmcp_auth, erlmcp_protocol_validator |
| 🟡 MEDIUM | 48 | Utilities, helpers, supervisors | erlmcp_logging, erlmcp_dashboard_http_handler |
| ⚪ LOW | 16 | Benchmarks, pricing, testing | erlmcp_bench_rate_limit, erlmcp_pricing_* |

---

## Visual Impact Summary

```
CRITICAL COVERAGE GAPS BY CATEGORY:

Core Protocol     ░░░░░░░░░░░ 8.71% (71.29 points below)
Transports        ░░░░░░░░░░░ 3.50% (76.50 points below)
Observability     ░░░░░░░░░░░ 0.00% (80.00 points below)
Resources/Tools   ░░░░░░░░░░░ 0.00% (80.00 points below)
Security          ░░░░░░░░░░░ 0.00% (80.00 points below)

Overall Project   ░░░░░░░░░░░ 3.00% (77.00 points below)

Legend: ████ 100% coverage
        ░░░░ 0% coverage
```

---

## Heatmap Statistics

### By Coverage Range

| Range | Count | Percentage | Color |
|-------|-------|------------|-------|
| 90-100% | 0 | 0.00% | 🟢 |
| 80-89% | 1 | 0.72% | 🟡 |
| 50-79% | 1 | 0.72% | 🟠 |
| 21-50% | 7 | 5.04% | 🔴 |
| 1-20% | 2 | 1.44% | 🔴 |
| 0% | 130 | 93.53% | ⚫ |

### By Priority

| Priority | Count | Percentage |
|----------|-------|------------|
| CRITICAL | 47 | 33.81% |
| HIGH | 28 | 20.14% |
| MEDIUM | 48 | 34.53% |
| LOW | 16 | 11.51% |

### By Application

| Application | Modules | Avg Coverage | Status |
|-------------|---------|--------------|--------|
| erlmcp_core | 86 | 2.89% | 🔴 FAIL |
| erlmcp_transports | 28 | 5.36% | 🔴 FAIL |
| erlmcp_observability | 21 | 0.00% | ⚫ FAIL |
| erlmcp_validation | 5 | 11.80% | 🔴 FAIL |

---

## Action Heatmap

### Immediate Actions (Week 1-2)

- [ ] **Tier 1 Core Protocol**: Add tests for erlmcp_client, erlmcp_server, erlmcp_registry
- [ ] **JSON-RPC**: Expand erlmcp_json_rpc tests from 35% to 80%
- [ ] **Message Handling**: Add tests for erlmcp_message_handler, erlmcp_message_parser
- [ ] **Session Management**: Add tests for erlmcp_session

### Short-term Actions (Week 3-4)

- [ ] **Tier 2 Transports**: Add tests for all transport modules
- [ ] **Transport HTTP**: Expand from 25% to 80% coverage
- [ ] **Transport Pool**: Add connection pooling tests
- [ ] **Transport Behavior**: Implement behavior compliance tests

### Medium-term Actions (Week 5-6)

- [ ] **Tier 3 Observability**: Add metrics and tracing tests
- [ ] **Health Monitoring**: Implement health check tests
- [ ] **Chaos Engineering**: Add chaos testing framework
- [ ] **Dashboard**: Add dashboard server tests

### Long-term Actions (Week 7-8)

- [ ] **Tier 4 Resources**: Add resource and subscription tests
- [ ] **Tools**: Add tool execution tests
- [ ] **Prompts**: Add prompt template tests
- [ ] **Tasks**: Add background task tests

---

## ❌ HEATMAP VERDICT: CRITICAL FAILURE

**Overall Coverage**: 3.00%
**Required Threshold**: 80%
**Gap**: 77 percentage points

### Heatmap Analysis

The coverage heatmap reveals a **critical testing crisis**:

- **93.5% of modules** (130/139) have **ZERO** test coverage
- **99.3% of modules** (138/139) are **below** the 80% threshold
- **Only 1 module** (0.72%) meets the minimum coverage requirement
- **Core protocol modules** have **0% coverage** (client, server, registry)
- **Transport modules** average **3.5% coverage**
- **Observability modules** have **0% coverage**

### Severity Assessment

🔴 **SEVERITY: CRITICAL**

This level of test coverage is **unacceptable for any production system** and represents:

- **Zero safety net** for production deployments
- **High risk** of undetected bugs and regressions
- **No confidence** in system reliability
- **Impossible** to safely refactor or optimize code

### Recommendation

**HALT all non-critical development** until coverage reaches 80%.

**Focus exclusively on testing** using Chicago School TDD methodology.

**Estimated timeline**: 8 weeks of focused testing effort.

---

**Heatmap Generated**: 2026-01-30
**Status**: ❌ CRITICAL FAILURE
**Action Required**: Immediate test implementation
