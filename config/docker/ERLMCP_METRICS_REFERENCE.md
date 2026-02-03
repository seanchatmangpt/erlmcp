# erlmcp v3 Metrics Reference
# ============================
# Complete list of all metrics exposed by erlmcp
#
# Metrics are available at: http://erlmcp:9100/metrics
#
# Categories:
# 1. Standard Erlang VM Metrics
# 2. Application-Specific Metrics
# 3. Transport Metrics
# 4. Session Metrics
# 5. Cluster Metrics

# =================================================================
# 1. STANDARD ERLANG VM METRICS
# =================================================================

# ------------------------------------------------------------------------------
# Memory Metrics (erlmcp_memory_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_memory_total_bytes Total memory allocated by the Erlang VM
# TYPE erlmcp_memory_total_bytes gauge
erlmcp_memory_total_bytes{instance="erlmcp@node1"} 1.073741824e+09

# HELP erlmcp_memory_available_bytes Total available memory for the VM
# TYPE erlmcp_memory_available_bytes gauge
erlmcp_memory_available_bytes{instance="erlmcp@node1"} 2.147483648e+09

# HELP erlmcp_memory_system_bytes Memory used for system/emulator
# TYPE erlmcp_memory_system_bytes gauge
erlmcp_memory_system_bytes{instance="erlmcp@node1"} 2.68435456e+08

# HELP erlmcp_memory_processes_bytes Memory used by processes
# TYPE erlmcp_memory_processes_bytes gauge
erlmcp_memory_processes_bytes{instance="erlmcp@node1"} 5.36870912e+08

# HELP erlmcp_memory_ets_bytes Memory used by ETS tables
# TYPE erlmcp_memory_ets_bytes gauge
erlmcp_memory_ets_bytes{instance="erlmcp@node1"} 1.34217728e+08

# HELP erlmcp_memory_binary_bytes Memory used by binaries
# TYPE erlmcp_memory_binary_bytes{instance="erlmcp@node1"} 6.7108864e+07

# HELP erlmcp_memory_atom_bytes Memory used by atoms
# TYPE erlmcp_memory_atom_bytes gauge
erlmcp_memory_atom_bytes{instance="erlmcp@node1"} 1.048576e+06

# HELP erlmcp_memory_code_bytes Memory used for code
# TYPE erlmcp_memory_code_bytes gauge
erlmcp_memory_code_bytes{instance="erlmcp@node1"} 2.097152e+07

# ------------------------------------------------------------------------------
# Process Metrics (erlmcp_process_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_process_count Current number of processes
# TYPE erlmcp_process_count gauge
erlmcp_process_count{instance="erlmcp@node1"} 15234

# HELP erlmcp_process_limit Maximum number of processes allowed
# TYPE erlmcp_process_limit gauge
erlmcp_process_limit{instance="erlmcp@node1"} 262144

# HELP erlmcp_processes_spawned_total Total processes spawned since start
# TYPE erlmcp_processes_spawned_total counter
erlmcp_processes_spawned_total{instance="erlmcp@node1"} 1.234567e+06

# ------------------------------------------------------------------------------
# Reduction Metrics (erlmcp_reductions_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_reductions_total Total reductions since VM start
# TYPE erlmcp_reductions_total counter
erlmcp_reductions_total{instance="erlmcp@node1"} 9.87654321e+11

# HELP erlmcp_reductions_per_second Reductions per second (instantaneous)
# TYPE erlmcp_reductions_per_second gauge
erlmcp_reductions_per_second{instance="erlmcp@node1"} 5.4321e+06

# ------------------------------------------------------------------------------
# Garbage Collection Metrics (erlmcp_gc_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_gc_count_total Total garbage collections since start
# TYPE erlmcp_gc_count_total counter
erlmcp_gc_count_total{instance="erlmcp@node1", kind="generational"} 123456
erlmcp_gc_count_total{instance="erlmcp@node1", kind="fullsweep"} 789

# HELP erlmcp_gc_ms_total Total time spent in GC (milliseconds)
# TYPE erlmcp_gc_ms_total counter
erlmcp_gc_ms_total{instance="erlmcp@node1", kind="generational"} 1.234567e+06
erlmcp_gc_ms_total{instance="erlmcp@node1", kind="fullsweep"} 123456

# HELP erlmcp_gc_words_reclaimed_total Total words reclaimed by GC
# TYPE erlmcp_gc_words_reclaimed_total counter
erlmcp_gc_words_reclaimed_total{instance="erlmcp@node1"} 9.87654321e+10

# ------------------------------------------------------------------------------
# Scheduler Metrics (erlmcp_scheduler_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_scheduler_utilization Scheduler utilization (0-1)
# TYPE erlmcp_scheduler_utilization gauge
erlmcp_scheduler_utilization{instance="erlmcp@node1", scheduler_id="1"} 0.85
erlmcp_scheduler_utilization{instance="erlmcp@node1", scheduler_id="2"} 0.82

# HELP erlmcp_scheduler_count Total number of schedulers
# TYPE erlmcp_scheduler_count gauge
erlmcp_scheduler_count{instance="erlmcp@node1"} 16

# ------------------------------------------------------------------------------
# I/O Metrics (erlmcp_io_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_io_input_bytes Total bytes read
# TYPE erlmcp_io_input_bytes counter
erlmcp_io_input_bytes{instance="erlmcp@node1"} 1.23456789e+10

# HELP erlmcp_io_output_bytes Total bytes written
# TYPE erlmcp_io_output_bytes counter
erlmcp_io_output_bytes{instance="erlmcp@node1"} 2.3456789e+10

# =================================================================
# 2. APPLICATION-SPECIFIC METRICS
# =================================================================

# ------------------------------------------------------------------------------
# Request Metrics (erlmcp_request_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_requests_total Total requests processed
# TYPE erlmcp_requests_total counter
erlmcp_requests_total{instance="erlmcp@node1", method="initialize", status="success"} 123456
erlmcp_requests_total{instance="erlmcp@node1", method="tools/call", status="success"} 234567
erlmcp_requests_total{instance="erlmcp@node1", method="tools/list", status="error"} 12

# HELP erlmcp_request_duration_seconds Request duration histogram
# TYPE erlmcp_request_duration_seconds histogram
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="0.001"} 1000
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="0.005"} 5000
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="0.01"} 10000
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="0.05"} 50000
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="0.1"} 99000
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="0.5"} 100000
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="1"} 100500
erlmcp_request_duration_seconds_bucket{instance="erlmcp@node1", method="initialize", le="+Inf"} 100500
erlmcp_request_duration_seconds_sum{instance="erlmcp@node1", method="initialize"} 5025.5
erlmcp_request_duration_seconds_count{instance="erlmcp@node1", method="initialize"} 100500

# HELP erlmcp_request_active Currently active requests
# TYPE erlmcp_request_active gauge
erlmcp_request_active{instance="erlmcp@node1"} 42

# ------------------------------------------------------------------------------
# Tool Metrics (erlmcp_tool_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_tool_calls_total Total tool invocations
# TYPE erlmcp_tool_calls_total counter
erlmcp_tool_calls_total{instance="erlmcp@node1", tool_name="read_file", status="success"} 54321
erlmcp_tool_calls_total{instance="erlmcp@node1", tool_name="write_file", status="success"} 12345
erlmcp_tool_calls_total{instance="erlmcp@node1", tool_name="execute_command", status="error"} 67

# HELP erlmcp_tool_duration_seconds Tool execution duration histogram
# TYPE erlmcp_tool_duration_seconds histogram
erlmcp_tool_duration_seconds_bucket{instance="erlmcp@node1", tool_name="read_file", le="0.01"} 50000
erlmcp_tool_duration_seconds_bucket{instance="erlmcp@node1", tool_name="read_file", le="0.1"} 54000
erlmcp_tool_duration_seconds_bucket{instance="erlmcp@node1", tool_name="read_file", le="+Inf"} 54321

# ------------------------------------------------------------------------------
# Resource Metrics (erlmcp_resource_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_resource_messages_total Total JSON-RPC messages processed
# TYPE erlmcp_resource_messages_total counter
erlmcp_resource_messages_total{instance="erlmcp@node1", direction="incoming"} 987654
erlmcp_resource_messages_total{instance="erlmcp@node1", direction="outgoing"} 876543

# HELP erlmcp_resource_bytes_total Total bytes transferred
# TYPE erlmcp_resource_bytes_total counter
erlmcp_resource_bytes_total{instance="erlmcp@node1", direction="incoming"} 1.23456789e+10
erlmcp_resource_bytes_total{instance="erlmcp@node1", direction="outgoing"} 2.3456789e+10

# ------------------------------------------------------------------------------
# Registry Metrics (erlmcp_registry_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_registry_tools_registered Number of registered tools
# TYPE erlmcp_registry_tools_registered gauge
erlmcp_registry_tools_registered{instance="erlmcp@node1"} 42

# HELP erlmcp_registry_resources_registered Number of registered resources
# TYPE erlmcp_registry_resources_registered gauge
erlmcp_registry_resources_registered{instance="erlmcp@node1"} 15

# HELP erlmcp_registry_prompts_registered Number of registered prompts
# TYPE erlmcp_registry_prompts_registered gauge
erlmcp_registry_prompts_registered{instance="erlmcp@node1"} 8

# =================================================================
# 3. TRANSPORT METRICS
# =================================================================

# ------------------------------------------------------------------------------
# Transport Connection Metrics (erlmcp_transport_connection_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_transport_connections_active Active transport connections
# TYPE erlmcp_transport_connections_active gauge
erlmcp_transport_connections_active{instance="erlmcp@node1", transport_type="stdio"} 1
erlmcp_transport_connections_active{instance="erlmcp@node1", transport_type="tcp"} 127
erlmcp_transport_connections_active{instance="erlmcp@node1", transport_type="http"} 256
erlmcp_transport_connections_active{instance="erlmcp@node1", transport_type="websocket"} 64

# HELP erlmcp_transport_connections_total Total connections established
# TYPE erlmcp_transport_connections_total counter
erlmcp_transport_connections_total{instance="erlmcp@node1", transport_type="stdio"} 1
erlmcp_transport_connections_total{instance="erlmcp@node1", transport_type="tcp"} 12345
erlmcp_transport_connections_total{instance="erlmcp@node1", transport_type="http"} 23456
erlmcp_transport_connections_total{instance="erlmcp@node1", transport_type="websocket"} 7890

# HELP erlmcp_transport_connections_closed_total Total connections closed
# TYPE erlmcp_transport_connections_closed_total counter
erlmcp_transport_connections_closed_total{instance="erlmcp@node1", transport_type="tcp", reason="error"} 12
erlmcp_transport_connections_closed_total{instance="erlmcp@node1", transport_type="tcp", reason="eof"} 123
erlmcp_transport_connections_closed_total{instance="erlmcp@node1", transport_type="tcp", reason="timeout"} 5

# ------------------------------------------------------------------------------
# Transport Message Metrics (erlmcp_transport_messages_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_transport_messages_total Total messages processed by transport
# TYPE erlmcp_transport_messages_total counter
erlmcp_transport_messages_total{instance="erlmcp@node1", transport_type="stdio", direction="receive"} 123456
erlmcp_transport_messages_total{instance="erlmcp@node1", transport_type="stdio", direction="send"} 123450
erlmcp_transport_messages_total{instance="erlmcp@node1", transport_type="tcp", direction="receive"} 234567
erlmcp_transport_messages_total{instance="erlmcp@node1", transport_type="tcp", direction="send"} 234560

# HELP erlmcp_transport_message_errors_total Total message errors
# TYPE erlmcp_transport_message_errors_total counter
erlmcp_transport_message_errors_total{instance="erlmcp@node1", transport_type="stdio", error_type="parse"} 3
erlmcp_transport_message_errors_total{instance="erlmcp@node1", transport_type="tcp", error_type="parse"} 12
erlmcp_transport_message_errors_total{instance="erlmcp@node1", transport_type="tcp", error_type="validation"} 5

# ------------------------------------------------------------------------------
# Transport Byte Metrics (erlmcp_transport_bytes_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_transport_bytes_total Total bytes transferred
# TYPE erlmcp_transport_bytes_total counter
erlmcp_transport_bytes_total{instance="erlmcp@node1", transport_type="stdio", direction="receive"} 1.23456789e+09
erlmcp_transport_bytes_total{instance="erlmcp@node1", transport_type="tcp", direction="receive"} 2.3456789e+09

# HELP erlmcp_transport_backlog_bytes Current message backlog
# TYPE erlmcp_transport_backlog_bytes gauge
erlmcp_transport_backlog_bytes{instance="erlmcp@node1", transport_type="tcp"} 524288

# =================================================================
# 4. SESSION METRICS
# =================================================================

# ------------------------------------------------------------------------------
# Session Lifecycle Metrics (erlmcp_sessions_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_sessions_total Total sessions created
# TYPE erlmcp_sessions_total counter
erlmcp_sessions_total{instance="erlmcp@node1"} 54321

# HELP erlmcp_sessions_active Currently active sessions
# TYPE erlmcp_sessions_active gauge
erlmcp_sessions_active{instance="erlmcp@node1"} 1024

# HELP erlmcp_sessions_closed_total Total sessions closed
# TYPE erlmcp_sessions_closed_total counter
erlmcp_sessions_closed_total{instance="erlmcp@node1", reason="normal"} 53000
erlmcp_sessions_closed_total{instance="erlmcp@node1", reason="error"} 100
erlmcp_sessions_closed_total{instance="erlmcp@node1", reason="timeout"} 200

# HELP erlmcp_sessions_timed_out_total Total sessions timed out
# TYPE erlmcp_sessions_timed_out_total counter
erlmcp_sessions_timed_out_total{instance="erlmcp@node1"} 200

# HELP erlmcp_sessions_errored_total Total sessions with errors
# TYPE erlmcp_sessions_errored_total counter
erlmcp_sessions_errored_total{instance="erlmcp@node1"} 100

# ------------------------------------------------------------------------------
# Session Duration Metrics (erlmcp_session_duration_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_session_duration_seconds Session duration histogram
# TYPE erlmcp_session_duration_seconds histogram
erlmcp_session_duration_seconds_bucket{instance="erlmcp@node1", le="10"} 10000
erlmcp_session_duration_seconds_bucket{instance="erlmcp@node1", le="60"} 40000
erlmcp_session_duration_seconds_bucket{instance="erlmcp@node1", le="300"} 50000
erlmcp_session_duration_seconds_bucket{instance="erlmcp@node1", le="+Inf"} 53000
erlmcp_session_duration_seconds_sum{instance="erlmcp@node1"} 2.65e+06
erlmcp_session_duration_seconds_count{instance="erlmcp@node1"} 53000

# ------------------------------------------------------------------------------
# Session Request Metrics (erlmcp_session_requests_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_session_requests_per_session Average requests per session
# TYPE erlmcp_session_requests_per_session gauge
erlmcp_session_requests_per_session{instance="erlmcp@node1"} 42.5

# =================================================================
# 5. CLUSTER METRICS
# =================================================================

# ------------------------------------------------------------------------------
# Cluster Membership Metrics (erlmcp_cluster_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_cluster_nodes Number of nodes in cluster
# TYPE erlmcp_cluster_nodes gauge
erlmcp_cluster_nodes{instance="erlmcp@node1"} 3

# HELP erlmcp_cluster_nodes_up Number of up nodes in cluster
# TYPE erlmcp_cluster_nodes_up gauge
erlmcp_cluster_nodes_up{instance="erlmcp@node1"} 3

# HELP erlmcp_cluster_nodes_down Number of down nodes in cluster
# TYPE erlmcp_cluster_nodes_down gauge
erlmcp_cluster_nodes_down{instance="erlmcp@node1"} 0

# HELP erlmcp_cluster_partitions_detected Number of partitions detected
# TYPE erlmcp_cluster_partitions_detected gauge
erlmcp_cluster_partitions_detected{instance="erlmcp@node1"} 0

# ------------------------------------------------------------------------------
# Cluster Communication Metrics (erlmcp_cluster_comm_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_cluster_messages_total Total inter-cluster messages
# TYPE erlmcp_cluster_messages_total counter
erlmcp_cluster_messages_total{instance="erlmcp@node1", direction="sent"} 123456
erlmcp_cluster_messages_total{instance="erlmcp@node1", direction="received"} 123450

# HELP erlmcp_cluster_message_errors_total Total cluster message errors
# TYPE erlmcp_cluster_message_errors_total counter
erlmcp_cluster_message_errors_total{instance="erlmcp@node1", error_type="nodedown"} 5
erlmcp_cluster_message_errors_total{instance="erlmcp@node1", error_type="timeout"} 2

# ------------------------------------------------------------------------------
# Cluster Sync Metrics (erlmcp_cluster_sync_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_cluster_sync_lag_seconds Replication lag
# TYPE erlmcp_cluster_sync_lag_seconds gauge
erlmcp_cluster_sync_lag_seconds{instance="erlmcp@node1"} 0.05

# HELP erlmcp_cluster_sync_bytes_total Total bytes synced
# TYPE erlmcp_cluster_sync_bytes_total counter
erlmcp_cluster_sync_bytes_total{instance="erlmcp@node1"} 1.23456789e+11

# =================================================================
# 6. HEALTH CHECK METRICS
# =================================================================

# ------------------------------------------------------------------------------
# Health Status (erlmcp_health_*)
# ------------------------------------------------------------------------------
# HELP erlmcp_health_status Overall health status (1=healthy, 0=unhealthy)
# TYPE erlmcp_health_status gauge
erlmcp_health_status{instance="erlmcp@node1"} 1

# HELP erlmcp_health_check_duration_seconds Health check duration
# TYPE erlmcp_health_check_duration_seconds gauge
erlmcp_health_check_duration_seconds{instance="erlmcp@node1", check="memory"} 0.001
erlmcp_health_check_duration_seconds{instance="erlmcp@node1", check="disk"} 0.002
erlmcp_health_check_duration_seconds{instance="erlmcp@node1", check="processes"} 0.001

# =================================================================
# OPEN TELEMETRY METRICS (Semantic Conventions)
# =================================================================

# ------------------------------------------------------------------------------
# OTEL Process Metrics (process_*)
# ------------------------------------------------------------------------------
# HELP process_runtime_uptime_seconds Process uptime in seconds
# TYPE process_runtime_uptime_seconds gauge
process_runtime_uptime_seconds{service_name="erlmcp"} 86400

# HELP process_cpu_seconds_total Total CPU seconds
# TYPE process_cpu_seconds_total counter
process_cpu_seconds_total{service_name="erlmcp"} 12345.67

# HELP process_open_fds Number of open file descriptors
# TYPE process_open_fds gauge
process_open_fds{service_name="erlmcp"} 512

# HELP process_max_fds Maximum file descriptors
# TYPE process_max_fds gauge
process_max_fds{service_name="erlmcp"} 65536

# ------------------------------------------------------------------------------
# OTEL Runtime Metrics (runtime_*)
# ------------------------------------------------------------------------------
# HELP runtime_erlang_erts_version Erlang/OTP version
# TYPE runtime_erlang_erts_version gauge
runtime_erlang_erts_version{service_name="erlmcp", version="26.2"} 1

# HELP runtime_erlang_scheduler_utilization Scheduler utilization
# TYPE runtime_erlang_scheduler_utilization gauge
runtime_erlang_scheduler_utilization{service_name="erlmcp"} 0.85
