# erlmcp v2.0.0 Deletions

**What v2 Intentionally Drops - Complete Deletion List**

Version: v2.0.0-draft
Status: CANONICAL REFERENCE
Date: 2026-01-27

---

## Purpose

This document lists **all modules to be deleted or separated** in erlmcp v2.0.0, organized by category with rationale.

**Total Deletions**: 177 modules (72% of v1 codebase)
- **Separated**: 85+ TCPS modules → `apps/tcps_erlmcp/`
- **Deleted**: 92+ legacy/experimental modules

---

## Separation (Not Deletion): TCPS Modules (85+ modules)

**Action**: Move to separate OTP application (`apps/tcps_erlmcp/`)
**Rationale**: TCPS is a complete manufacturing workflow system orthogonal to MCP protocol.

### TCPS CLI (10 modules)
- `tcps_cli_andon.erl`
- `tcps_cli_config.erl`
- `tcps_cli_examples.erl`
- `tcps_cli_format.erl`
- `tcps_cli_kaizen.erl`
- `tcps_cli_kanban.erl`
- `tcps_cli_quality.erl`
- `tcps_cli_receipt.erl`
- `tcps_cli_root_cause.erl`
- `tcps_cli_work_order.erl`

### TCPS Dashboard (8 modules)
- `tcps_dashboard.erl`
- `tcps_dashboard_handler.erl`
- `tcps_dashboard_sse_handler.erl`
- `tcps_websocket_handler.erl`
- `tcps_web_server.erl`
- `tcps_sse_manager.erl`
- `tcps_visualization_data.erl`
- `tcps_api_handler.erl`

### TCPS Core (15 modules)
- `tcps_andon.erl`
- `tcps_heijunka.erl`
- `tcps_kanban.erl`
- `tcps_kaizen.erl`
- `tcps_quality_gates.erl`
- `tcps_work_order.erl`
- `tcps_root_cause.erl`
- `tcps_health.erl`
- `tcps_deterministic.erl`
- `tcps_concepts.erl`
- `tcps_principles.erl`
- `tcps_config_reference.erl`
- `tcps_api_reference.erl`
- `tcps_howto_recipes.erl`
- `tcps_sku.erl`

### TCPS MCP Integration (5 modules)
- `tcps_mcp_prompts.erl`
- `tcps_mcp_server.erl`
- `tcps_mcp_tools.erl`

### TCPS Documentation (12 modules)
- `tcps_diataxis_explain.erl`
- `tcps_diataxis_howto.erl`
- `tcps_diataxis_reference.erl`
- `tcps_diataxis_tutorial.erl` (2 modules)
- `tcps_tutorial_steps.erl`
- `tcps_tutorial_validation.erl`

### TCPS Persistence (8 modules)
- `tcps_receipt.erl`
- `tcps_receipt_verifier.erl`
- `tcps_persistence.erl`
- `tcps_ontology_index.erl`
- `tcps_rdf_incremental.erl`

### TCPS Quality (10 modules)
- `tcps_rebar3_andon.erl`
- `tcps_rebar3_quality.erl`
- `tcps_rebar3_receipt.erl`
- `tcps_rebar3_shacl.erl`
- `rebar3_tcps_plugin.erl`

### TCPS Simulator (7 modules)
- `tcps_simulator.erl` (2 modules)
- `tcps_simulator_state.erl`
- `tcps_simulator_telemetry.erl`
- `tcps_scenario_loader.erl`

### TCPS Metrics (5 modules)
- `tcps_metrics_aggregator.erl`
- `tcps_metrics_cache.erl`
- `tcps_metrics_collector.erl`

### TCPS Utilities (5 modules)
- `tcps_query_cache.erl`
- `rdf_utils.erl`

**Total**: 85+ modules

**Post-Separation**:
- ✅ erlmcp remains 100% MCP-focused
- ✅ TCPS becomes optional dependency
- ✅ TCPS can be used with non-MCP projects
- ✅ Independent release cycles

---

## Deletion: Experimental Features (12 modules)

**Rationale**: Not in MCP spec, no critical dependencies, experimental/niche features.

### Routing (2 modules)
- ✅ `erlmcp_complex_routing.erl` - Use `erlmcp_registry.erl` instead
- ✅ `erlmcp_router.erl` - Redundant with registry

### Audio (1 module)
- ✅ `erlmcp_audio.erl` - Audio not in MCP spec 2024-11-05

### Icon Caching (2 modules)
- ✅ `erlmcp_icon_cache.erl` - Niche feature, not widely used
- ✅ `erlmcp_icon_validator.erl` - Niche feature

### Sampling (2 modules)
- ✅ `erlmcp_sampling.erl` - Experimental LLM sampling
- ✅ `erlmcp_sampling_strategy.erl` - Experimental

### Miscellaneous (5 modules)
- ✅ `erlmcp_elicitation.erl` - Experimental, unclear purpose
- ✅ `erlmcp_coordination.erl` - Unclear purpose
- ✅ `erlmcp_message_size.erl` - Single-purpose utility
- ✅ `erlmcp_pagination.erl` - Not widely used
- ✅ `erlmcp_binding.erl` - Niche binding feature
- ✅ `erlmcp_localhost_binding.erl` - Niche

**Total**: 12 modules

---

## Deletion: Legacy CLI (8 modules)

**Rationale**: Replaced by rebar3 tasks, TCPS CLI, or no longer needed.

### Benchmarking CLI (2 modules)
- ✅ `erlmcp_cli_bench.erl` - Use `rebar3 bench` instead
- ✅ `erlmcp_cli_chaos.erl` - Use `bench/erlmcp_bench_chaos.erl`

### Utilities (3 modules)
- ✅ `erlmcp_cli_doctor.erl` - Use `rebar3 doctor` or health checks
- ✅ `erlmcp_cli_marketplace.erl` - Marketplace feature removed
- ✅ `erlmcp_cli_upgrade.erl` - Use `rebar3 upgrade` or `erlmcp_upgrade.erl`

### Receipts/Plans (3 modules)
- ✅ `erlmcp_receipt_cli.erl` - Moved to TCPS
- ✅ `erlmcp_plan_cli.erl` - Moved to TCPS
- ✅ `erlmcp_marketplace_copy.erl` - Legacy marketplace

**Total**: 8 modules

---

## Deletion: Legacy Dashboard (5 modules)

**Rationale**: Replaced by TCPS dashboard or external monitoring (Grafana).

### Dashboard Backends (3 modules)
- ✅ `erlmcp_dashboard_http.erl` - Replaced by TCPS dashboard
- ✅ `erlmcp_monitor_dashboard.erl` - Replaced by TCPS dashboard
- ✅ `erlmcp_dashboard_stress_test.erl` - Use `bench/erlmcp_bench_stress.erl`

### SLA Dashboards (2 modules)
- ✅ `erlmcp_regression_dashboard.erl` - Experimental
- ✅ `erlmcp_sla_dashboard_handler.erl` - Moved to TCPS
- ✅ `erlmcp_sla_http_handler.erl` - Moved to TCPS

**Total**: 5 modules (listed 6, 1 duplicate)

---

## Deletion: Legacy Profiling (4 modules)

**Rationale**: Use `recon` library or existing profiling modules.

### Profilers (4 modules)
- ✅ `erlmcp_cpu_profiler.erl` - Use `recon:proc_count/2`, `recon:proc_window/3`
- ✅ `erlmcp_latency_profiler.erl` - Use `erlmcp_bench_*.erl` benchmarks
- ✅ `erlmcp_profiling_suite.erl` - Experimental
- ✅ `erlmcp_performance_benchmark.erl` - Use `bench/` directory

**Total**: 4 modules

---

## Deletion: Legacy Planning (8 modules)

**Rationale**: Moved to TCPS or deprecated.

### Planning Modules (8 modules)
- ✅ `erlmcp_plan.erl` - Moved to TCPS
- ✅ `erlmcp_plan_loader.erl` - Moved to TCPS
- ✅ `erlmcp_plan_sla_monitor.erl` - Moved to TCPS
- ✅ `erlmcp_plan_sla_monitor_extended.erl` - Moved to TCPS
- ✅ `erlmcp_plan_docs_generator.erl` - Moved to TCPS
- ✅ `erlmcp_pricing_plan.erl` - Moved to TCPS
- ✅ `erlmcp_pricing_poka_yoke.erl` - Moved to TCPS
- ✅ `erlmcp_pricing_receipt.erl` - Moved to TCPS

**Total**: 8 modules

---

## Deletion: Legacy Benchmarks (9 modules)

**Rationale**: Replaced by 5 canonical benchmarks.

### Legacy Benchmark Modules (9 modules)
- ✅ `benchmark_100k.erl` - Use `erlmcp_bench_core_ops.erl`
- ✅ `benchmark_100k_SUITE.erl` - Common Test suite (deprecated)
- ✅ `latency_SUITE.erl` - Use `erlmcp_bench_network_real.erl`
- ✅ `throughput_SUITE.erl` - Use `erlmcp_bench_core_ops.erl`
- ✅ `erlmcp_registry_contention.erl` - Covered by `erlmcp_bench_core_ops.erl`
- ✅ `erlmcp_transport_tcp_4kb.erl` - Covered by `erlmcp_bench_network_real.erl`
- ✅ `test_network_real_bench.erl` - Test module (deprecated)
- ✅ `erlmcp_queue_benchmark.erl` - Covered by `erlmcp_bench_core_ops.erl`

**Total**: 9 modules

---

## Deletion: Duplicate Modules (6 modules)

**Rationale**: Merge "simple" modules into canonical counterparts.

### Simple Variants (3 modules)
- ✅ `erlmcp_simple_metrics.erl` - Merge into `erlmcp_metrics.erl`
- ✅ `erlmcp_simple_monitor.erl` - Merge into `erlmcp_health_monitor.erl`
- ✅ `erlmcp_simple_trace.erl` - Merge into `erlmcp_tracing.erl`

### HTTP Transport Variants (3 modules)
- ✅ `erlmcp_transport_http_new.erl` - Merge into `erlmcp_transport_http.erl`
- ✅ `erlmcp_transport_http_adapter.erl` - Merge into `erlmcp_transport_http.erl`
- ✅ `erlmcp_transport_sse.erl` - Use HTTP long-polling or WebSocket

**Total**: 6 modules

---

## Deletion: Experimental Infrastructure (10 modules)

**Rationale**: Experimental features with no critical dependencies.

### Connection Management (3 modules)
- ✅ `erlmcp_connection_pool.erl` - Migrate to poolboy
- ✅ `erlmcp_connection_pool_sup.erl` - Migrate to poolboy
- ✅ `erlmcp_connection_optimizer.erl` - Experimental

### Memory Management (3 modules)
- ✅ `erlmcp_memory_pool.erl` - Experimental
- ✅ `erlmcp_memory_optimization.erl` - Experimental

### Upgrades (3 modules)
- ✅ `erlmcp_hot_reload.erl` - Experimental
- ✅ `erlmcp_hot_reload_example.erl` - Example
- ✅ `erlmcp_zero_downtime_upgrade.erl` - Experimental

### Queues (1 module)
- ✅ `erlmcp_queue_optimized.erl` - Use `erlmcp_queue_bounded.erl`

**Total**: 10 modules

---

## Deletion: Experimental Registry (2 modules)

**Rationale**: Migrate to gproc (production library).

### Sharded Registry (1 module)
- ✅ `erlmcp_registry_sharded.erl` - Experimental sharding (migrate to gproc)

### Config Modules (1 module)
- ✅ `erlmcp_config_profiles.erl` - Not widely used

**Total**: 2 modules

---

## Deletion: Experimental HTTP/Security (10 modules)

**Rationale**: Not core to MCP protocol, niche features.

### HTTP Security (5 modules)
- ✅ `erlmcp_http_auth.erl` - Use external auth proxy (nginx, envoy)
- ✅ `erlmcp_http_header_validator.erl` - Niche
- ✅ `erlmcp_https_enforcer.erl` - Use TLS termination at load balancer
- ✅ `erlmcp_oauth_security.erl` - Use external OAuth provider
- ✅ `erlmcp_origin_validator.erl` - Niche CORS validation

### HTTP Headers/Validation (2 modules)
- ✅ `erlmcp_http_headers.erl` - Merge into transport_http
- ✅ `erlmcp_http_middleware.erl` - Experimental

### TLS (1 module)
- ✅ `erlmcp_tls_validation.erl` - Use standard Erlang ssl module

### Misc HTTP (2 modules)
- ✅ `erlmcp_http_delete_handler.erl` - Not needed (REST API removed)
- ✅ `erlmcp_http_security.erl` - Merge into transport_http_server

**Total**: 10 modules

---

## Deletion: Experimental Monitoring (8 modules)

**Rationale**: Not core, experimental, or redundant.

### Continuous Monitoring (2 modules)
- ✅ `erlmcp_sla_continuous_monitor.erl` - Moved to TCPS
- ✅ `erlmcp_cluster_monitor.erl` - Experimental

### Evidence/Receipts (3 modules)
- ✅ `erlmcp_receipt_chain.erl` - Moved to TCPS
- ✅ `erlmcp_evidence_path.erl` - Moved to TCPS
- ✅ `erlmcp_pricing_state.erl` - Moved to TCPS
- ✅ `erlmcp_pricing_upgrade.erl` - Moved to TCPS

### Portal/Reporting (3 modules)
- ✅ `erlmcp_portal_generator.erl` - Experimental
- ✅ `erlmcp_report_generator.erl` - Use external reporting (Grafana)
- ✅ `erlmcp_report_visualizer.erl` - Use external reporting
- ✅ `erlmcp_report_metrics.erl` - Merge into erlmcp_metrics

**Total**: 8 modules (listed 10, some duplicates)

---

## Deletion: Miscellaneous (10 modules)

**Rationale**: Niche features, test utilities, or deprecated.

### Test Utilities (2 modules)
- ✅ `erlmcp_test_dummy_worker.erl` - Test helper (move to test/)
- ✅ `erlmcp_poolboy_worker_adapter.erl` - Experimental

### Utilities (5 modules)
- ✅ `erlmcp_apps_util.erl` - Niche
- ✅ `erlmcp_path_canonicalizer.erl` - Single-purpose utility
- ✅ `erlmcp_uri_validator.erl` - Merge into validation.erl
- ✅ `erlmcp_prompt_argument_validator.erl` - Merge into validation.erl
- ✅ `erlmcp_prompt_list_change_notifier.erl` - Merge into server

### Profiling/Benchmarking (3 modules)
- ✅ `erlmcp_bench_plan_validator.erl` - Moved to TCPS
- ✅ `erlmcp_chaos_plan_validator.erl` - Moved to TCPS
- ✅ `erlmcp_refusal_plan_validator.erl` - Moved to TCPS
- ✅ `erlmcp_refusal.erl` - Moved to TCPS

**Total**: 10 modules

---

## Summary

| Category | Modules | Action | Destination |
|----------|---------|--------|-------------|
| **TCPS** | 85+ | SEPARATE | `apps/tcps_erlmcp/` |
| **Experimental** | 12 | DELETE | N/A |
| **Legacy CLI** | 8 | DELETE | N/A |
| **Legacy Dashboard** | 5 | DELETE | N/A |
| **Legacy Profiling** | 4 | DELETE | N/A |
| **Legacy Planning** | 8 | DELETE | N/A |
| **Legacy Benchmarks** | 9 | DELETE | N/A |
| **Duplicates** | 6 | MERGE | Into canonical modules |
| **Experimental Infra** | 10 | DELETE | N/A |
| **Experimental Registry** | 2 | MIGRATE | gproc |
| **Experimental HTTP** | 10 | DELETE | N/A |
| **Experimental Monitoring** | 8 | DELETE or MOVE | TCPS or delete |
| **Miscellaneous** | 10 | DELETE or MOVE | test/ or delete |
| **TOTAL** | **177** | | |

**Breakdown**:
- **Separated**: 85+ TCPS modules
- **Deleted**: 92+ modules

---

## Verification Checklist

Before deleting each module:
- [ ] Run: `grep -r "module_name" src/` to check dependencies
- [ ] Verify no critical imports
- [ ] Check test suite: `grep -r "module_name" test/`
- [ ] Run: `rebar3 compile` after deletion
- [ ] Run: `rebar3 eunit` and `rebar3 ct` to verify tests pass

---

## References

### Source Documents
- **Principles**: [v2_principles.md](./v2_principles.md)
- **Required Modules**: [v2_required_modules.md](./v2_required_modules.md)
- **Code Map**: [L4-code-map.md](../C4/L4-code-map.md)
- **Glossary**: [GLOSSARY.md](../GLOSSARY.md)

### Source Code
- **Current Modules**: `find src -name "*.erl"`
- **Module Count**: 247 modules (before v2)

---

**Document Status**: CANONICAL (v2.0.0-draft)
**Last Updated**: 2026-01-27
**Purpose**: Complete deletion list for erlmcp v2.0.0 (177 modules)
