# Module Inventory - erlmcp v2

**Source:** `inventory.json` (auto-generated from src/ scan)
**Generated:** 2026-01-27
**Total Modules:** 244 active modules across 29 families

## Executive Summary

| Category | Count |
|----------|-------|
| **Active Modules** | 244 |
| **Families** | 29 |
| **Broken Files** | 6 |
| **Duplicate Files** | 6 |

## Module Families


### Benchmarking (1 modules)

- **erlmcp_bench_plan_validator** (13,899 bytes, 4 exports)
  - `run_benchmark/2, run_benchmark_with_duration/3, generate_bench_report/3, validate_against_envelope/2`

### Cli (5 modules)

- **erlmcp_cli_bench** (17,828 bytes, 4 exports)
  - `run/1, format_json/1, format_csv/1, format_text/1`
- **erlmcp_cli_chaos** (18,954 bytes, 4 exports)
  - `run/1, format_json/1, format_csv/1, format_text/1`
- **erlmcp_cli_doctor** (31,226 bytes, 6 exports)
  - `run/1, check_all/0, check_all/1, get_exit_code/1, output_text_report/1` + 1 more
- **erlmcp_cli_marketplace** (5,396 bytes, 2 exports)
  - `list/1, generate_and_write/2`
- **erlmcp_cli_upgrade** (17,806 bytes, 1 exports)
  - `command/2`

### Client (1 modules)

- **erlmcp_client** (29,143 bytes, 29 exports)
  - `start_link/1, start_link/2, initialize/2, initialize/3, list_roots/1` + 24 more

### Configuration (7 modules)

- **erlmcp_config** (24,791 bytes, 34 exports)
  - `start_link/0, stop/0, get/1, get/2, set_config/1` + 29 more
- **erlmcp_config_loader** (11,025 bytes, 10 exports)
  - `load_profile/1, load_profile_with_overrides/2, load_from_file/1, validate_and_load/1, get_loaded_config/0` + 5 more
- **erlmcp_config_profiles** (17,872 bytes, 9 exports)
  - `get_profile/1, list_profiles/0, apply_profile/1, apply_profile/2, describe_profile/1` + 4 more
- **erlmcp_config_schema** (16,944 bytes, 6 exports)
  - `validate/1, get_schema/0, get_defaults/0, migrate_schema/2, validate_type/3` + 1 more
- **erlmcp_config_sup** (1,917 bytes, 2 exports)
  - `start_link/0, init/1`
- **erlmcp_config_validation** (30,311 bytes, 14 exports)
  - `validate_transport_config/2, validate_transport_config/3, validate_field/3, validate_config_map/2, get_transport_schema/1` + 9 more
- **erlmcp_config_validator_strict** (16,118 bytes, 6 exports)
  - `validate/1, validate/2, validate_and_report/1, validate_and_report/2, get_validation_rules/0` + 1 more

### Core (1 modules)

- **erlmcp** (44,702 bytes, 38 exports)
  - `start_server/1, start_server/2, stop_server/1, list_servers/0, start_transport/2` + 33 more

### Health (2 modules)

- **erlmcp_health** (14,995 bytes, 5 exports)
  - `check/0, readiness/0, liveness/0, startup/0, detailed_check/0`
- **erlmcp_health_monitor** (27,155 bytes, 21 exports)
  - `start_link/0, start_link/1, register_component/2, register_component/3, unregister_component/1` + 16 more

### Http (7 modules)

- **erlmcp_http_auth** (10,918 bytes, 11 exports)
  - `start_link/1, get_token/1, refresh_token/1, validate_token/1, inject_headers/2` + 6 more
- **erlmcp_http_delete_handler** (13,017 bytes, 11 exports)
  - `handle_delete/3, handle_delete_by_path/5, handle_delete_session_termination/4, handle_delete_resource/5, handle_delete_tool/5` + 6 more
- **erlmcp_http_header_validator** (31,877 bytes, 12 exports)
  - `validate_request_headers/2, validate_protocol_version/1, validate_content_type/2, validate_accept/1, validate_session_id/1` + 7 more
- **erlmcp_http_headers** (12,019 bytes, 7 exports)
  - `validate_protocol_version/1, validate_accept_header/1, get_response_content_type/1, get_http_status_code/1, validate_method/1` + 2 more
- **erlmcp_http_middleware** (4,985 bytes, 3 exports)
  - `validate_request/2, extract_session_header/1, inject_session_header/2`
- **erlmcp_http_security** (4,934 bytes, 4 exports)
  - `validate_origin/2, validate_session/1, require_https/1, is_localhost/1`
- **erlmcp_https_enforcer** (18,483 bytes, 20 exports)
  - `%% Configuration and initialization
    get_config/0, get_config/1, validate_config/0, load_certificates/0, load_certificates/1` + 15 more

### Json_Rpc (1 modules)

- **erlmcp_json_rpc** (14,403 bytes, 24 exports)
  - `encode_request/3, encode_response/2, encode_error_response/3, encode_error_response/4, encode_notification/2` + 19 more

### Logging (1 modules)

- **erlmcp_logging** (5,949 bytes, 8 exports)
  - `init_session_levels/0, validate_log_level/1, normalize_log_level/1, set_global_level/1, get_global_level/0` + 3 more

### Memory (4 modules)

- **erlmcp_memory_accounting** (19,021 bytes, 12 exports)
  - `% Core measurement functions
    measure_per_connection_heap/1, measure_per_connection_state/1, measure_per_node_rss/0, measure_per_node_base_overhead/1, measure_cluster_total/1` + 7 more
- **erlmcp_memory_optimization** (2,664 bytes, 3 exports)
  - `get_memory_stats/0, estimate_connection_memory/1, validate_memory_constraints/1`
- **erlmcp_memory_pool** (11,742 bytes, 14 exports)
  - `start_link/0, acquire_connection_state/1, release_connection_state/1, acquire_message_buffer/1, release_message_buffer/1` + 9 more
- **erlmcp_memory_profiler** (14,048 bytes, 11 exports)
  - `start_profiling/0, stop_profiling/0, create_test_connections/2, create_test_connections/3, measure_memory_snapshot/0` + 6 more

### Metrics (6 modules)

- **erlmcp_metrics** (12,088 bytes, 15 exports)
  - `start_link/0, record_transport_operation/4, record_server_operation/4, record_registry_operation/3, get_metrics/0` + 10 more
- **erlmcp_metrics_http** (1,743 bytes, 4 exports)
  - `init/2, allowed_methods/2, content_types_provided/2, to_json/2`
- **erlmcp_metrics_http_handler** (6,949 bytes, 1 exports)
  - `do/1`
- **erlmcp_metrics_http_sup** (1,380 bytes, 3 exports)
  - `start_link/0, start_link/1, init/1`
- **erlmcp_metrics_http_worker** (2,843 bytes, 7 exports)
  - `start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2` + 2 more
- **erlmcp_metrics_server** (9,657 bytes, 15 exports)
  - `start_link/0, get_metrics/0, record_message/1, record_error/0, record_latency/1` + 10 more

### Monitoring (4 modules)

- **erlmcp_monitor_config** (15,204 bytes, 10 exports)
  - `load_config/0, load_config/1, save_config/1, validate_config/1, get_alert_rules/0` + 5 more
- **erlmcp_monitor_dashboard** (11,796 bytes, 12 exports)
  - `start_link/1, start_dashboard/0, stop_dashboard/0, update_metrics/2, get_dashboard_data/0` + 7 more
- **erlmcp_monitor_sup** (4,294 bytes, 6 exports)
  - `start_link/0, start_link/1, stop_monitoring/0, restart_component/1, get_component_status/0` + 1 more
- **erlmcp_monitoring_sup** (2,334 bytes, 2 exports)
  - `start_link/0, init/1`

### Planning (6 modules)

- **erlmcp_plan** (5,321 bytes, 10 exports)
  - `current_plan/0, show/1, list_plans/0, get_envelope/1, get_limits/1` + 5 more
- **erlmcp_plan_cli** (8,204 bytes, 5 exports)
  - `list_plans/0, show_plan/1, current_plan/0, test_refusal/1, upgrade_path/2`
- **erlmcp_plan_docs_generator** (17,834 bytes, 7 exports)
  - `generate_all_docs/0, generate_tier_doc/1, load_plan_spec/1, format_envelope_table/1, format_limits_table/1` + 2 more
- **erlmcp_plan_loader** (1,225 bytes, 2 exports)
  - `load_plan/1, load_plan_file/1`
- **erlmcp_plan_sla_monitor** (14,478 bytes, 17 exports)
  - `start_link/0, start_link/1, monitor_envelope/2, check_throughput/1, check_latency/1` + 12 more
- **erlmcp_plan_sla_monitor_extended** (22,717 bytes, 20 exports)
  - `start_link/0, start_link/1, monitor_envelope/2, check_throughput/1, check_latency/1` + 15 more

### Pooling (2 modules)

- **erlmcp_buffer_pool** (12,774 bytes, 16 exports)
  - `start_link/1, get_buffer/2, get_buffer/1, return_buffer/2, stats/1` + 11 more
- **erlmcp_poolboy_worker_adapter** (464 bytes, 1 exports)
  - `start_link/1`

### Pricing (5 modules)

- **erlmcp_pricing_plan** (16,400 bytes, 6 exports)
  - `load_plan/1, validate_plan/1, get_envelope/1, check_refusal/2, list_available_plans/0` + 1 more
- **erlmcp_pricing_poka_yoke** (19,536 bytes, 9 exports)
  - `validate_plan/1, validate_plan_file/1, validate_all_plans/1, validate_plan_schema/1, validate_envelope_consistency/1` + 4 more
- **erlmcp_pricing_receipt** (24,718 bytes, 15 exports)
  - `% Receipt creation and retrieval
    create_receipt/2, create_receipt/3, get_receipt/1, list_receipts/1, list_receipts/2` + 10 more
- **erlmcp_pricing_state** (3,157 bytes, 9 exports)
  - `get_current_plan/0, set_current_plan/1, get_last_upgrade_time/1, set_last_upgrade_time/2, get_certification_valid/1` + 4 more
- **erlmcp_pricing_upgrade** (27,649 bytes, 25 exports)
  - `can_upgrade/2, can_downgrade/2, get_upgrade_path/2, list_possible_upgrades/1, simulate_upgrade/2` + 20 more

### Profiling (1 modules)

- **erlmcp_profile_manager** (11,633 bytes, 8 exports)
  - `list_profiles/0, show_profile/1, apply_profile/1, apply_profile/2, validate_profile/1` + 3 more

### Queuing (4 modules)

- **erlmcp_queue_benchmark** (11,833 bytes, 6 exports)
  - `benchmark_10k/0, benchmark_50k/0, benchmark_100k/0, benchmark_sustained_throughput/1, benchmark_latency/1` + 1 more
- **erlmcp_queue_bounded** (7,396 bytes, 13 exports)
  - `new/1, new/2, enqueue/2, dequeue/1, depth/1` + 8 more
- **erlmcp_queue_limits** (21,716 bytes, 17 exports)
  - `start_link/0, stop/0, check_queue_limit/2, record_message/3, remove_message/2` + 12 more
- **erlmcp_queue_optimized** (14,639 bytes, 15 exports)
  - `start_link/0, start_link/1, enqueue/2, enqueue/3, dequeue_batch/1` + 10 more

### Receipts (2 modules)

- **erlmcp_receipt_chain** (2,444 bytes, 5 exports)
  - `add_event/1, get_events_by_type/1, get_event_by_id/1, get_all_events/0, restore_state/1`
- **erlmcp_receipt_cli** (17,416 bytes, 16 exports)
  - `tail/1, tail/2, show/1, verify/1, export/2` + 11 more

### Registry (4 modules)

- **erlmcp_registry** (13,980 bytes, 20 exports)
  - `start_link/0, register_server/3, register_transport/3, unregister_server/1, unregister_transport/1` + 15 more
- **erlmcp_registry_health_check** (3,116 bytes, 3 exports)
  - `health_check/0, is_healthy/0, get_status/0`
- **erlmcp_registry_sharded** (19,530 bytes, 25 exports)
  - `start_link/0, start_link/1, register_server/3, register_transport/3, unregister_server/1` + 20 more
- **erlmcp_registry_sup** (1,562 bytes, 2 exports)
  - `start_link/0, init/1`

### Reporting (3 modules)

- **erlmcp_report_generator** (18,148 bytes, 3 exports)
  - `generate_report/2, generate_report/3, generate_summary/1`
- **erlmcp_report_metrics** (36,042 bytes, 8 exports)
  - `collect_system_metrics/0, analyze_performance_trends/2, calculate_regression_impact/2, generate_benchmark_comparison/2, assess_resource_utilization/1` + 3 more
- **erlmcp_report_visualizer** (48,054 bytes, 8 exports)
  - `generate_performance_charts/1, create_trace_heatmap/1, build_dependency_graph/1, create_regression_timeline/2, generate_security_dashboard/1` + 3 more

### Resilience (6 modules)

- **erlmcp_backpressure** (11,571 bytes, 15 exports)
  - `start_link/0, stop/0, check_rate_limit/2, update_latency/2, check_handler_queue/2` + 10 more
- **erlmcp_backpressure_signal** (4,563 bytes, 6 exports)
  - `new/2, check_and_signal/2, set_warning_threshold/2, set_critical_threshold/2, get_status/1` + 1 more
- **erlmcp_circuit_breaker** (19,159 bytes, 17 exports)
  - `start_link/0, stop/0, get_status/0, is_open/0, can_execute/0` + 12 more
- **erlmcp_graceful_degradation** (10,179 bytes, 9 exports)
  - `init/0, register_service/3, unregister_service/1, degrade_service/2, restore_service/1` + 4 more
- **erlmcp_graceful_drain** (10,732 bytes, 15 exports)
  - `start_link/0, register_connection/2, unregister_connection/1, request_drain/2, drain_started/0` + 10 more
- **erlmcp_rate_limiter** (23,449 bytes, 22 exports)
  - `start_link/0, stop/0, check_message_rate/2, check_connection_rate/2, check_tool_call_rate/2` + 17 more

### Routing (1 modules)

- **erlmcp_routing_metrics** (22,263 bytes, 22 exports)
  - `start_link/0, init/0, reset/0, % Counter operations
    increment_counter/1, increment_counter/2` + 17 more

### Server (4 modules)

- **erlmcp_server** (67,387 bytes, 26 exports)
  - `start_link/2, add_resource/3, add_resource_template/4, add_tool/3, add_tool_with_schema/4` + 21 more
- **erlmcp_server_handlers** (9,052 bytes, 19 exports)
  - `handle_add_resource/3, handle_add_resource_template/4, handle_add_tool/3, handle_add_tool_with_schema/4, handle_add_prompt/3` + 14 more
- **erlmcp_server_pool_sup** (950 bytes, 2 exports)
  - `start_link/1, init/1`
- **erlmcp_server_sup** (1,403 bytes, 3 exports)
  - `start_link/0, start_child/2, init/1`

### Sessions (3 modules)

- **erlmcp_session_failover** (14,697 bytes, 12 exports)
  - `start_link/1, get_failover_status/0, trigger_manual_failover/1, monitor_node/1, unmonitor_node/1` + 7 more
- **erlmcp_session_manager** (14,652 bytes, 14 exports)
  - `start_link/0, create_session/0, create_session/1, validate_session/1, touch_session/1` + 9 more
- **erlmcp_session_replicator** (18,322 bytes, 17 exports)
  - `start_link/0, start_cluster/1, store_session/3, get_session/1, delete_session/1` + 12 more

### Supervision (1 modules)

- **erlmcp_sup** (8,552 bytes, 9 exports)
  - `start_link/0, start_server/2, stop_server/1, start_transport/3, stop_transport/1` + 4 more

### Tcps (63 modules)

- **tcps_andon** (24,762 bytes, 21 exports)
  - `% Event triggering
    trigger_andon/2, get_andon_event/1, get_andon_history/1, % Stop-the-line enforcement
    is_blocked/1, can_proceed_to_stage/2` + 16 more
- **tcps_api_handler** (19,533 bytes, 7 exports)
  - `init/2, terminate/3, allowed_methods/2, content_types_provided/2, content_types_accepted/2` + 2 more
- **tcps_api_reference** (37,552 bytes, 11 exports)
  - `generate_full_reference/0, generate_module_api/1, get_quality_gates_api/0, get_kanban_api/0, get_andon_api/0` + 6 more
- **tcps_cli_andon** (11,403 bytes, 1 exports)
  - `run/1`
- **tcps_cli_config** (6,533 bytes, 7 exports)
  - `load/0, load/1, get/1, get/2, set/2` + 2 more
- **tcps_cli_examples** (10,091 bytes, 1 exports)
  - `run/1`
- **tcps_cli_format** (9,013 bytes, 17 exports)
  - `output/2, output/3, table/2, json/1, markdown/2` + 12 more
- **tcps_cli_kaizen** (9,424 bytes, 1 exports)
  - `run/1`
- **tcps_cli_kanban** (5,710 bytes, 1 exports)
  - `run/1`
- **tcps_cli_quality** (9,677 bytes, 1 exports)
  - `run/1`
- **tcps_cli_receipt** (9,828 bytes, 1 exports)
  - `run/1`
- **tcps_cli_root_cause** (9,810 bytes, 1 exports)
  - `run/1`
- **tcps_cli_tpm** (10,050 bytes, 1 exports)
  - `run/1`
- **tcps_cli_work_order** (9,214 bytes, 1 exports)
  - `run/1`
- **tcps_concepts** (53,111 bytes, 5 exports)
  - `why_tcps/0, jidoka_philosophy/0, pull_vs_push/0, andon_thinking/0, heijunka_leveling/0`
- **tcps_config_reference** (31,535 bytes, 10 exports)
  - `generate_full_config_reference/0, get_quality_gates_config/0, get_kanban_config/0, get_andon_config/0, get_receipt_config/0` + 5 more
- **tcps_dashboard** (20,399 bytes, 17 exports)
  - `start_link/1, start_dashboard/1, stop_dashboard/0, get_metrics_summary/0, get_real_time_metrics/0` + 12 more
- **tcps_dashboard_handler** (10,559 bytes, 8 exports)
  - `init/2, terminate/3, allowed_methods/2, content_types_provided/2, content_types_accepted/2` + 3 more
- **tcps_dashboard_sse_handler** (4,104 bytes, 3 exports)
  - `init/2, info/3, terminate/3`
- **tcps_deterministic** (40,887 bytes, 35 exports)
  - `% Deterministic build verification
    verify_deterministic_build/1, verify_artifact_hash/2, calculate_artifact_hash/1, % Build environment
    capture_build_env/0, verify_build_env/1` + 30 more
- **tcps_diataxis_explain** (7,984 bytes, 7 exports)
  - `get_explanation/1, list_explanations/0, list_by_category/1, search_explanations/1, get_related_explanations/1` + 2 more
- **tcps_diataxis_howto** (12,364 bytes, 16 exports)
  - `start_link/0, get_guide/1, list_guides/0, search_guides/1, get_guide_by_category/1` + 11 more
- **tcps_diataxis_reference** (28,046 bytes, 12 exports)
  - `generate_api_reference/0, generate_api_reference/1, generate_config_reference/0, generate_module_reference/1, generate_cli_reference/0` + 7 more
- **tcps_diataxis_tutorial** (16,806 bytes, 10 exports)
  - `get_tutorial/1, list_tutorials/0, list_by_level/1, get_next_step/2, get_previous_step/2` + 5 more
- **tcps_diataxis_tutorial** (25,399 bytes, 22 exports)
  - `start_link/0, list_tutorials/0, get_tutorial_info/1, start_tutorial/2, get_tutorial_progress/1` + 17 more
- **tcps_health** (53,874 bytes, 42 exports)
  - `% System lifecycle
    start_link/0, start_link/1, stop/0, % Health checks
    health_check/0, component_health/1` + 37 more
- **tcps_heijunka** (15,813 bytes, 8 exports)
  - `schedule_next_batch/0, schedule_next_batch/1, level_work_orders/1, calculate_bucket_allocation/1, get_leveling_score/1` + 3 more
- **tcps_howto_recipes** (111,410 bytes, 13 exports)
  - `load_all_guides/0, get_quality_gates_guide/0, get_kanban_config_guide/0, get_andon_response_guide/0, get_five_whys_guide/0` + 8 more
- **tcps_kaizen** (47,340 bytes, 10 exports)
  - `collect_metrics/1, identify_waste_points/0, identify_waste_points/1, propose_improvements/1, apply_improvement/1` + 5 more
- **tcps_kanban** (21,924 bytes, 19 exports)
  - `start_link/0, start_link/1, stop/0, check_wip_limit/1, set_wip_limit/2` + 14 more
- **tcps_mcp_prompts** (23,780 bytes, 7 exports)
  - `get_all_prompts/0, handle_tutorial_completion/1, handle_howto_recipe/1, handle_explanation_clarify/1, handle_reference_lookup/1` + 2 more
- **tcps_mcp_server** (9,649 bytes, 13 exports)
  - `start_link/0, start_link/1, stop/0, get_server_pid/0, get_server_info/0` + 8 more
- **tcps_mcp_tools** (33,458 bytes, 9 exports)
  - `get_all_tools/0, handle_simulator_start/1, handle_simulator_step/1, handle_simulator_query/1, handle_diataxis_navigate/1` + 4 more
- **tcps_metrics_aggregator** (23,720 bytes, 16 exports)
  - `start_link/0, stop/0, get_overview_metrics/0, get_quality_metrics/0, get_kanban_metrics/0` + 11 more
- **tcps_metrics_cache** (4,146 bytes, 7 exports)
  - `init_cache/0, destroy_cache/0, get_cached/1, update_cache/2, invalidate/1` + 2 more
- **tcps_metrics_collector** (22,070 bytes, 31 exports)
  - `start_link/0, start_link/1, stop/0, %% Work order metrics
    record_work_order_created/1, record_work_order_completed/1` + 26 more
- **tcps_ontology_index** (9,648 bytes, 20 exports)
  - `start_link/0, stop/0, % Index management
    create_indexes/0, rebuild_indexes/0, clear_indexes/0` + 15 more
- **tcps_persistence** (52,962 bytes, 44 exports)
  - `% Receipt Storage
    store_receipt/1, load_receipt/1, get_all_receipts/1, get_receipt/1, list_receipts_by_sku/1` + 39 more
- **tcps_principles** (121,332 bytes, 10 exports)
  - `% Design Decisions
    receipts_not_commits/0, quality_gates_vs_ci/0, wip_limits_matter/0, dual_storage/0, mcp_integration/0` + 5 more
- **tcps_quality_gates** (51,435 bytes, 15 exports)
  - `start_link/0, check_gate/2, check_all_gates/1, get_gate_status/2, get_quality_metrics/0` + 10 more
- **tcps_query_cache** (7,793 bytes, 13 exports)
  - `start_link/0, stop/0, % Cache operations
    cached_sparql_query/2, cached_sparql_query/3, invalidate/1` + 8 more
- **tcps_rdf_incremental** (10,432 bytes, 9 exports)
  - `% Incremental updates
    add_receipt_to_ontology/1, add_work_order_to_ontology/1, add_andon_to_ontology/1, add_sku_to_ontology/1, % Batch operations
    add_batch/1` + 4 more
- **tcps_rebar3_andon** (11,134 bytes, 3 exports)
  - `init/1, do/1, format_error/1`
- **tcps_rebar3_quality** (8,384 bytes, 7 exports)
  - `init/1, do/1, format_error/1, check_compilation_gate/2, check_test_gate/2` + 2 more
- **tcps_rebar3_receipt** (13,665 bytes, 6 exports)
  - `init/1, do/1, format_error/1, generate_compilation_receipt/3, generate_test_receipt/3` + 1 more
- **tcps_rebar3_shacl** (13,441 bytes, 5 exports)
  - `init/1, do/1, format_error/1, perform_shacl_validation/2, generate_validation_receipt/2`
- **tcps_receipt** (7,545 bytes, 6 exports)
  - `% Receipt storage
    store_receipt/1, % Chain verification
    verify_chain/1, verify_deterministic/1, verify_chronological/1, % Audit operations
    generate_audit_trail/1` + 1 more
- **tcps_receipt_verifier** (58,564 bytes, 19 exports)
  - `verify_receipt/1, verify_receipt_chain/1, verify_deterministic_build/1, verify_complete_chain/1, verify_quality_gates_passed/1` + 14 more
- **tcps_root_cause** (19,450 bytes, 16 exports)
  - `start_link/0, start_analysis/2, add_why/3, finalize_analysis/3, generate_prevention_actions/1` + 11 more
- **tcps_scenario_loader** (23,906 bytes, 4 exports)
  - `load_scenario/1, list_scenarios/0, validate_scenario/1, get_scenario_metadata/1`
- **tcps_simulator** (41,277 bytes, 31 exports)
  - `start_link/0, start_link/1, stop/0, load_scenario/1, start_simulation/0` + 26 more
- **tcps_simulator** (15,800 bytes, 15 exports)
  - `start_link/0, start_scenario/1, start_scenario/2, stop_scenario/1, get_scenario_status/1` + 10 more
- **tcps_simulator_state** (8,959 bytes, 15 exports)
  - `new/1, snapshot/1, restore/2, add_event/2, get_events/1` + 10 more
- **tcps_simulator_telemetry** (30,994 bytes, 42 exports)
  - `start_link/0, start_link/1, stop/0, %% Span management
    start_session/1, end_session/2` + 37 more
- **tcps_sku** (52,046 bytes, 36 exports)
  - `start_link/0, start_link/1, stop/0, create_sku/1, get_sku/1` + 31 more
- **tcps_sse_manager** (9,453 bytes, 14 exports)
  - `start_link/0, stop/0, register_client/1, unregister_client/1, broadcast_update/1` + 9 more
- **tcps_tutorial_steps** (43,618 bytes, 7 exports)
  - `get_first_step/1, get_next_step/2, get_step_info/2, execute_step/3, get_hint/2` + 2 more
- **tcps_tutorial_validation** (18,608 bytes, 6 exports)
  - `validate_step/3, validate_prerequisites/2, verify_checkpoint/2, assess_comprehension/2, validate_code_quality/1` + 1 more
- **tcps_visualization_data** (29,179 bytes, 21 exports)
  - `%% Kanban visualizations
    kanban_heatmap/1, kanban_time_series/1, kanban_utilization/1, %% Quality gate visualizations
    quality_gate_funnel/1, quality_gate_scores/1` + 16 more
- **tcps_web_server** (7,784 bytes, 10 exports)
  - `start_link/1, stop/0, get_server_info/0, broadcast_event/2, init/1` + 5 more
- **tcps_websocket_handler** (17,943 bytes, 5 exports)
  - `init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3`
- **tcps_websocket_handler** (13,309 bytes, 5 exports)
  - `init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3`
- **tcps_work_order** (78,706 bytes, 41 exports)
  - `create_work_order/1, create_from_github/1, create_from_security_advisory/1, create_from_marketplace/1, start_work_order/1` + 36 more

### Tracing (1 modules)

- **erlmcp_trace_propagation** (8,334 bytes, 13 exports)
  - `spawn_traced/1, spawn_traced/2, spawn_traced/3, with_trace_context/2, with_trace_context/3` + 8 more

### Transport (11 modules)

- **erlmcp_transport_api** (927 bytes, 5 exports)
  - `start_transport/2, start_transport/3, stop_transport/1, list_transports/0, ensure_transport_supervisor/0`
- **erlmcp_transport_behavior** (18,301 bytes, 21 exports)
  - `init/1, send/2, close/1, get_info/1, init/1` + 16 more
- **erlmcp_transport_http** (1,609 bytes, 4 exports)
  - `send/2, close/1, init/1, start_link/1`
- **erlmcp_transport_http_adapter** (1,501 bytes, 5 exports)
  - `init/1, send/2, close/1, get_info/1, handle_transport_call/2`
- **erlmcp_transport_http_server** (23,023 bytes, 7 exports)
  - `start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2` + 2 more
- **erlmcp_transport_sse** (18,839 bytes, 6 exports)
  - `init/2, send/2, close/1, init/3, handle/2` + 1 more
- **erlmcp_transport_stdio** (9,975 bytes, 11 exports)
  - `send/2, start_link/1, close/1, validate_message_size/2, get_max_message_size/0` + 6 more
- **erlmcp_transport_sup** (4,516 bytes, 3 exports)
  - `start_link/0, start_child/3, init/1`
- **erlmcp_transport_tcp** (20,022 bytes, 14 exports)
  - `send/2, close/1, transport_init/1, start_link/1, start_server/1` + 9 more
- **erlmcp_transport_validation** (18,730 bytes, 7 exports)
  - `validate_transport_config/2, validate_transport_config/1, validate_field/3, get_validation_errors/1, is_valid_transport_type/1` + 2 more
- **erlmcp_transport_ws** (19,718 bytes, 12 exports)
  - `init/2, send/2, close/1, websocket_handle/2, websocket_info/2` + 7 more

### Utilities (87 modules)

- **erlmcp_app** (371 bytes, 2 exports)
  - `start/2, stop/1`
- **erlmcp_apps_util** (6,516 bytes, 9 exports)
  - `generate_app_id/1, validate_app_manifest/1, validate_app_name/1, validate_permission/1, validate_uri/1` + 4 more
- **erlmcp_audio** (7,228 bytes, 7 exports)
  - `encode_audio_content/2, encode_audio_content_with_metadata/3, validate_audio_mime_type/1, encode_audio_base64/1, decode_audio_base64/1` + 2 more
- **erlmcp_binding** (1,382 bytes, 7 exports)
  - `bind_transport_to_server/2, unbind_transport/1, get_transport_binding_info/1, list_transport_bindings/0, validate_transport_binding/2` + 2 more
- **erlmcp_bottleneck_detector** (13,430 bytes, 9 exports)
  - `start_detection/0, start_detection/1, stop_detection/0, check_bottlenecks/0, get_alerts/0` + 4 more
- **erlmcp_capabilities** (14,628 bytes, 11 exports)
  - `build_server_capabilities/0, build_server_capabilities/1, extract_client_capabilities/1, validate_protocol_version/1, validate_capability/2` + 6 more
- **erlmcp_capability_cache** (5,642 bytes, 9 exports)
  - `new/1, check_capability/2, check_capabilities/3, has_resource_support/1, has_tool_support/1` + 4 more
- **erlmcp_chaos_plan_validator** (17,881 bytes, 8 exports)
  - `run_chaos_suite/2, run_chaos_scenario/3, generate_chaos_report/3, simulate_connection_failure/1, simulate_message_loss/2` + 3 more
- **erlmcp_cluster_monitor** (11,005 bytes, 16 exports)
  - `start_link/0, stop/0, get_cluster_status/0, get_node_connections/1, get_global_connections/0` + 11 more
- **erlmcp_completion_context** (16,621 bytes, 5 exports)
  - `complete/3, resolve_context/2, resolve_argument_reference/3, filter_completions/2, generate_completions_from_schema/3`
- **erlmcp_complex_routing** (14,983 bytes, 16 exports)
  - `start_link/0, add_route/1, update_route/2, delete_route/1, match_request/5` + 11 more
- **erlmcp_connection_optimizer** (8,722 bytes, 9 exports)
  - `create_optimized_state/1, update_state_field/3, get_state_field/2, get_field_safe/3, compress_state/1` + 4 more
- **erlmcp_connection_pool** (20,646 bytes, 23 exports)
  - `start_link/0, start_pool/2, stop_pool/1, checkout/2, checkout/3` + 18 more
- **erlmcp_connection_pool_sup** (2,045 bytes, 4 exports)
  - `start_link/0, start_child/3, get_pool_for_connection/1, init/1`
- **erlmcp_coordination** (1,258 bytes, 4 exports)
  - `store_supervisor_decision/1, notify_coordination_hooks/3, check_transport_dependencies/2, validate_transport_health/2`
- **erlmcp_cpu_profiler** (10,825 bytes, 10 exports)
  - `start_profiling/0, start_profiling/1, stop_profiling/0, get_cpu_stats/0, get_top_functions/1` + 5 more
- **erlmcp_dashboard_http** (25,600 bytes, 1 exports)
  - `init/2`
- **erlmcp_dashboard_stress_test** (6,401 bytes, 6 exports)
  - `start/0, start/1, start_workers/2, stop/0, run_benchmark/1` + 1 more
- **erlmcp_elicitation** (15,417 bytes, 17 exports)
  - `start_link/0, create_form/3, create_form/4, create_url_elicitation/3, create_url_elicitation/4` + 12 more
- **erlmcp_error** (21,302 bytes, 37 exports)
  - `% Context creation and management
    new_context/1, new_context/2, add_context/3, get_context/2, context_to_map/1` + 32 more
- **erlmcp_evidence_path** (17,204 bytes, 8 exports)
  - `create_evidence_path/2, list_evidence_artifacts/2, verify_artifact_completeness/2, generate_conformance_report/3, mark_certified/2` + 3 more
- **erlmcp_hot_reload** (19,574 bytes, 23 exports)
  - `start_link/0, reload_module/1, reload_modules/1, reload_all_modules/0, get_reload_status/0` + 18 more
- **erlmcp_hot_reload_example** (11,123 bytes, 6 exports)
  - `example_simple_module_reload/0, example_multi_module_reload/0, example_config_reload/0, example_graceful_drain/0, example_zero_downtime_upgrade/0` + 1 more
- **erlmcp_icon_cache** (6,444 bytes, 14 exports)
  - `start_link/0, cache_icon/3, cache_icon/4, get_cached_icon/1, invalidate_icon/1` + 9 more
- **erlmcp_icon_validator** (6,309 bytes, 4 exports)
  - `validate_icon/1, store_icon/2, get_icon/1, is_valid_mime_type/1`
- **erlmcp_infrastructure_sup** (3,960 bytes, 2 exports)
  - `start_link/0, init/1`
- **erlmcp_inter_node_comm** (19,478 bytes, 16 exports)
  - `start_link/0, send_message/3, batch_send/3, send_async/3, get_stats/1` + 11 more
- **erlmcp_io_worker** (7,663 bytes, 10 exports)
  - `async_read_file/3, async_write_file/3, async_execute_handler/2, start_link/0, init/1` + 5 more
- **erlmcp_latency_profiler** (12,621 bytes, 10 exports)
  - `start_profiling/0, stop_profiling/0, measure_operation/2, measure_operation/3, get_latency_stats/0` + 5 more
- **erlmcp_lifecycle_manager** (17,402 bytes, 16 exports)
  - `start_link/1, register_subscription/4, unregister_subscription/3, register_task/4, unregister_task/3` + 11 more
- **erlmcp_localhost_binding** (6,099 bytes, 7 exports)
  - `validate_bind_address/2, validate_bind_address/1, is_localhost/1, get_localhost_binding/0, get_localhost_binding/1` + 2 more
- **erlmcp_log_helpers** (16,570 bytes, 17 exports)
  - `filter_by_component/2, filter_by_level/2, filter_by_time_range/3, filter_by_pattern/2, combine_filters/2` + 12 more
- **erlmcp_message_handler** (5,206 bytes, 9 exports)
  - `process_message/3, handle_initialize/2, handle_initialized/2, handle_resources_list/2, handle_tools_list/2` + 4 more
- **erlmcp_message_parser** (4,817 bytes, 8 exports)
  - `parse_json_rpc/1, validate_jsonrpc_version/1, parse_by_type/1, parse_request/3, parse_response/3` + 3 more
- **erlmcp_message_size** (7,356 bytes, 11 exports)
  - `get_limit/1, validate_message_size/2, validate_message_size/3, validate_http_body_size/1, validate_sse_event_size/1` + 6 more
- **erlmcp_metrology_validator** (28,744 bytes, 8 exports)
  - `validate_report/1, validate_plan/1, validate_metric/1, canonical_unit/1, decompose_memory/1` + 3 more
- **erlmcp_network_optimizer** (5,512 bytes, 13 exports)
  - `start_link/0, stop/0, get_inter_node_stats/0, wait_for_convergence/1, ping/0` + 8 more
- **erlmcp_oauth_security** (8,610 bytes, 13 exports)
  - `start_link/0, get_client_secret/0, get_client_id/0, get_oauth_config/0, validate_oauth_config/0` + 8 more
- **erlmcp_origin_validator** (8,320 bytes, 5 exports)
  - `validate_origin/2, validate_origin/3, matches_origin_pattern/2, get_default_allowed_origins/0, is_origin_allowed/2`
- **erlmcp_otel** (23,482 bytes, 24 exports)
  - `init/1, start_span/2, start_span/3, end_span/1, with_span/3` + 19 more
- **erlmcp_pagination** (9,862 bytes, 6 exports)
  - `encode_cursor/2, decode_cursor/1, validate_cursor/1, generate_next_cursor/3, paginate_list/4` + 1 more
- **erlmcp_path_canonicalizer** (13,545 bytes, 6 exports)
  - `canonicalize_path/1, is_within_allowed_directory/2, validate_resource_path/3, validate_resource_path/2, resolve_symlinks/1` + 1 more
- **erlmcp_performance_benchmark** (34,008 bytes, 17 exports)
  - `start_link/0, start_link/1, run_benchmark/2, run_baseline/1, monitor_performance/2` + 12 more
- **erlmcp_portal_generator** (14,883 bytes, 2 exports)
  - `generate/0, generate/1`
- **erlmcp_profiling_suite** (14,411 bytes, 9 exports)
  - `start_full_profiling/0, start_full_profiling/1, stop_and_generate_report/0, get_comprehensive_analysis/0, get_executive_summary/0` + 4 more
- **erlmcp_progress** (16,662 bytes, 15 exports)
  - `start_link/0, generate_token/0, track_tool_call/3, send_progress/4, get_progress/1` + 10 more
- **erlmcp_prompt_argument_validator** (17,393 bytes, 12 exports)
  - `validate_prompt_arguments/2, validate_prompt_arguments/3, get_argument_schema/1, build_validation_error/2, validate_against_schema/2` + 7 more
- **erlmcp_prompt_list_change_notifier** (7,571 bytes, 5 exports)
  - `notify_prompt_added/4, notify_prompt_removed/2, notify_prompt_updated/4, broadcast_to_subscribers/3, send_notification_to_client/3`
- **erlmcp_recovery_manager** (22,415 bytes, 20 exports)
  - `start_link/0, start_link/1, register_component/3, unregister_component/1, trigger_recovery/2` + 15 more
- **erlmcp_refusal** (9,821 bytes, 20 exports)
  - `get_http_status/1, get_message/1, get_hint/1, get_severity/1, get_metadata/1` + 15 more
- **erlmcp_refusal_plan_validator** (17,514 bytes, 8 exports)
  - `audit_refusal_codes/2, trigger_throughput_exceeded/1, trigger_queue_depth_exceeded/1, trigger_connection_limit_exceeded/1, trigger_message_size_exceeded/1` + 3 more
- **erlmcp_regression_dashboard** (21,928 bytes, 15 exports)
  - `start_dashboard/1, stop_dashboard/0, get_dashboard_data/0, update_dashboard/1, render_dashboard/0` + 10 more
- **erlmcp_regression_detector** (22,429 bytes, 15 exports)
  - `detect_regression/2, update_baseline/1, start/0, stop/0, get_regression_report/1` + 10 more
- **erlmcp_request_id** (2,123 bytes, 4 exports)
  - `safe_increment/1, safe_increment/2, validate_id/1, is_valid_id/1`
- **erlmcp_resource_list_changed** (4,459 bytes, 4 exports)
  - `notify_added/3, notify_removed/3, notify_updated/3, build_notification/3`
- **erlmcp_resource_subscriptions** (13,261 bytes, 14 exports)
  - `start_link/0, subscribe/2, unsubscribe/2, get_subscribers/1, notify_updated/2` + 9 more
- **erlmcp_roots** (10,578 bytes, 13 exports)
  - `start_link/1, add_root/1, remove_root/1, list_roots/0, validate_path/1` + 8 more
- **erlmcp_router** (31,357 bytes, 14 exports)
  - `start_link/0, route_message/3, setup_load_balancer/2, setup_circuit_breaker/2, get_routing_metrics/0` + 9 more
- **erlmcp_sampling** (7,711 bytes, 4 exports)
  - `extract_model_preferences/1, validate_model_preferences/1, apply_preferences_to_handler/2, get_default_preferences/0`
- **erlmcp_sampling_strategy** (1,919 bytes, 3 exports)
  - `validate_strategy/1, is_valid_strategy/1, get_valid_strategies/0`
- **erlmcp_setup** (1,179 bytes, 5 exports)
  - `start_stdio_setup/2, start_tcp_setup/3, start_http_setup/3, start_transport_enhanced/3, start_transport_with_retry/4`
- **erlmcp_simple_metrics** (7,503 bytes, 9 exports)
  - `start/0, stop/0, increment/2, record_latency/2, get_stats/0` + 4 more
- **erlmcp_simple_monitor** (5,819 bytes, 10 exports)
  - `start_link/0, get_status/0, set_threshold/2, stop/0, init/1` + 5 more
- **erlmcp_simple_trace** (16,203 bytes, 12 exports)
  - `start_trace/1, start_trace/2, add_span/2, add_span/3, add_span/4` + 7 more
- **erlmcp_sla_continuous_monitor** (18,112 bytes, 13 exports)
  - `start_link/0, start_link/1, start_monitoring/1, stop_monitoring/1, get_status/0` + 8 more
- **erlmcp_sla_dashboard_handler** (6,329 bytes, 1 exports)
  - `do/1`
- **erlmcp_sla_http_handler** (5,802 bytes, 4 exports)
  - `init/2, allowed_methods/2, content_types_provided/2, to_json/2`
- **erlmcp_sse_event_store** (14,294 bytes, 14 exports)
  - `start_link/0, add_event/3, get_events_since/2, cleanup_expired/0, clear_session/1` + 9 more
- **erlmcp_stdio** (3,144 bytes, 10 exports)
  - `start/0, start/1, stop/0, add_tool/3, add_tool/4` + 5 more
- **erlmcp_stdio_server** (21,860 bytes, 14 exports)
  - `start_link/1, add_tool/3, add_tool/4, add_resource/3, add_resource/4` + 9 more
- **erlmcp_structured_logging** (19,065 bytes, 25 exports)
  - `init/1, debug/2, info/2, notice/2, warning/2` + 20 more
- **erlmcp_subscription_handlers** (1,670 bytes, 2 exports)
  - `validate_resource_uri/1, format_error/1`
- **erlmcp_task_manager** (9,934 bytes, 16 exports)
  - `start_link/0, stop/0, register_server/2, unregister_server/1, create_tool_task/5` + 11 more
- **erlmcp_templates** (9,333 bytes, 8 exports)
  - `render_receipt/1, render_work_order/1, render_andon_event/1, render_sku_listing/1, render_standard_work/1` + 3 more
- **erlmcp_test_dummy_worker** (2,301 bytes, 7 exports)
  - `start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2` + 2 more
- **erlmcp_tls_validation** (15,562 bytes, 20 exports)
  - `start_link/0, build_tls_options/2, validate_tls_options/1, validate_peer_verification/1, validate_hostname_verification/1` + 15 more
- **erlmcp_tool_change_notifier** (10,144 bytes, 14 exports)
  - `start_link/0, notify_tool_added/2, notify_tool_removed/2, notify_tool_updated/2, subscribe_to_changes/1` + 9 more
- **erlmcp_tracing** (10,659 bytes, 23 exports)
  - `start_span/1, start_span/2, start_span/3, end_span/1, set_attributes/2` + 18 more
- **erlmcp_transport** (2,012 bytes, 2 exports)
  - `validate_config/2, merge_defaults/2`
- **erlmcp_upgrade** (16,543 bytes, 4 exports)
  - `upgrade_plan/2, upgrade_verify/0, format_plan/1, format_verify/1`
- **erlmcp_uri_validator** (17,786 bytes, 14 exports)
  - `validate_uri/1, validate_uri_scheme/1, validate_uri_template/1, is_valid_uri_format/1, validate_resource_uri_on_registration/1` + 9 more
- **erlmcp_util** (677 bytes, 4 exports)
  - `create_transport_id/2, get_process_status/1, list_supported_transport_types/0, get_config_examples/0`
- **erlmcp_validation** (666 bytes, 4 exports)
  - `validate_transport_config/1, validate_transport_config/2, get_config_schema/1, initialize/0`
- **erlmcp_version** (9,545 bytes, 9 exports)
  - `version/0, version_string/0, major/0, minor/0, patch/0` + 4 more
- **erlmcp_zero_downtime_upgrade** (10,499 bytes, 6 exports)
  - `prepare_upgrade/1, execute_upgrade/2, execute_upgrade/3, rollback_upgrade/0, get_upgrade_status/0` + 1 more
- **rdf_utils** (7,577 bytes, 12 exports)
  - `% RDF Generation
    format_triple/3, format_triples/1, uri/1, literal/1, typed_literal/2` + 7 more
- **rebar3_tcps_plugin** (2,352 bytes, 1 exports)
  - `init/1`


## Broken Files (Compilation Failures)

These files have `.broken` extension and should be reviewed or deleted:

- `src/erlmcp_alert_manager.erl.broken`
- `src/erlmcp_metrics_aggregator.erl.broken`
- `src/erlmcp_metrics_collector.erl.broken`
- `src/erlmcp_monitor.erl.broken`
- `src/erlmcp_trace_analyzer.erl.broken`
- `src/erlmcp_transport_tcp.erl.broken`


## Duplicate Files (Backups & Variants)

These files are marked as duplicates (`.backup`, `.bak`, or variant names like `_new`, `_refactored`):

- `src/erlmcp_marketplace_copy.erl`
- `src/erlmcp_server.erl.backup`
- `src/erlmcp_server_refactored.erl`
- `src/erlmcp_transport_http.erl.backup`
- `src/erlmcp_transport_http_new.erl`
- `src/tcps/tcps_rebar3_quality.erl.bak`
