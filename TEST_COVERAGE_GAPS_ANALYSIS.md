# Test Coverage Gap Analysis - erlmcp

**Generated:** 2026-01-31
**Analysis Scope:** apps/erlmcp_core, apps/erlmcp_transports

---

## Executive Summary

**Total Coverage Status:**
- **erlmcp_core:** 41/77 modules have direct test files (53% module coverage)
- **erlmcp_transports:** 9/22 modules have direct test files (41% module coverage)
- **Total:** 50/99 modules have direct test files (51% module coverage)

**Test Quality Issues:**
- 63 test files with potential issues
- 56 test files with minimal tests (<3 test cases)
- 7 test files with TODO/SKIP/PLACEHOLDER annotations
- 4 Common Test suites with incomplete test group implementations

---

## 1. Modules Without Direct Test Files

### 1.1 erlmcp_core (36 modules missing tests)

#### High Priority - Core Infrastructure
These modules are critical to the system but lack dedicated test files:

- **erlmcp_sup** - Main supervisor (critical)
- **erlmcp_core_sup** - Core supervisor (critical)
- **erlmcp_app** - Application callback (critical)
- **erlmcp_server_sup** - Server supervisor (critical)
- **erlmcp_cluster_sup** - Cluster supervisor (critical)
- **erlmcp_reload_sup** - Reload supervisor

#### High Priority - Session Backends
Session persistence backends lack individual test files:

- **erlmcp_session_ets** - ETS backend (covered by integration tests?)
- **erlmcp_session_dets** - DETS backend (covered by integration tests?)
- **erlmcp_session_mnesia** - Mnesia backend (covered by integration tests?)

#### High Priority - Validation & Security
- **erlmcp_protocol_validator** - Protocol validation (NEW module for MCP spec compliance)
- **erlmcp_transport_validator** - Transport validation (NEW module)
- **erlmcp_performance_validator** - Performance validation (NEW module)
- **erlmcp_mtls_validator** - mTLS validation
- **erlmcp_uri_validator** - URI validation
- **erlmcp_schema_validator** - Schema validation

#### Medium Priority - Infrastructure
- **erlmcp** - Main API module
- **erlmcp_client_transport** - Client transport abstraction
- **erlmcp_message_handler** - Message handling
- **erlmcp_registry_utils** - Registry utilities
- **erlmcp_hooks** - Hook system
- **erlmcp_node_monitor** - Node monitoring
- **erlmcp_split_brain_detector** - Split-brain detection

#### Medium Priority - Features
- **erlmcp_capabilities** - Capability negotiation (has erlmcp_capabilities_*_tests.erl but not erlmcp_capabilities_tests.erl)
- **erlmcp_refusal** - Refusal code handling (has erlmcp_refusal_*_tests.erl)
- **erlmcp_rate_limiter** - Rate limiting (has erlmcp_rate_limiter_*_tests.erl)
- **erlmcp_change_notifier** - Change notifications
- **erlmcp_prompt_list_change_notifier** - Prompt change notifications
- **erlmcp_prompt_argument_validator** - Prompt argument validation

#### Low Priority - Utilities & Guards
- **erlmcp_message_size** - Message size validation
- **erlmcp_path_canonicalizer** - Path canonicalization
- **erlmcp_cpu_guard** - CPU guard
- **erlmcp_cpu_quota** - CPU quota
- **erlmcp_graceful_drain** - Graceful drain
- **erlmcp_mock_llm** - Mock LLM (test utility)
- **erlmcp_test_sync** - Test synchronization
- **tcps_quality_gates** - TCPS quality gates

---

### 1.2 erlmcp_transports (13 modules missing tests)

#### High Priority - Core Transports
- **erlmcp_transport_tcp** - TCP transport (has erlmcp_tcp_*_tests.erl but not erlmcp_transport_tcp_tests.erl)
- **erlmcp_transport_http_server** - HTTP server

#### High Priority - Validation & Security
- **erlmcp_http_header_validator** - HTTP header validation
- **erlmcp_origin_validator** - Origin validation (CORS)
- **erlmcp_tls_validation** - TLS validation
- **erlmcp_security_headers** - Security headers
- **erlmcp_transport_validation** - Transport validation

#### Medium Priority - Infrastructure
- **erlmcp_transport_health** - Transport health (has erlmcp_transport_health_*_tests.erl)
- **erlmcp_transport_pool** - Transport pooling
- **erlmcp_transport_adapter** - Transport adapter
- **erlmcp_transport_pipeline** - Transport pipeline
- **erlmcp_pool_strategy** - Pool strategy
- **erlmcp_transports_app** - Application callback

---

## 2. Test Files with Minimal Coverage

### 2.1 Files with <3 Test Cases (56 files)

Many test files contain only 1-2 test cases despite the modules having complex functionality:

#### Property-Based Tests (Expected to have 1 test, OK)
These files use Proper and contain a single property test (acceptable):
- erlmcp_cache_basic_proper_tests.erl (1 test)
- erlmcp_cache_lru_proper_tests.erl (1 test)
- erlmcp_cache_proper_tests.erl (1 test)
- erlmcp_cache_ttl_proper_tests.erl (1 test)
- erlmcp_json_rpc_proper_tests.erl (1 test)
- erlmcp_rate_limiter_proper_tests.erl (1 test)
- erlmcp_registry_proper_tests.erl (1 test)
- erlmcp_registry_server_proper_tests.erl (1 test)
- erlmcp_registry_transport_proper_tests.erl (1 test)
- erlmcp_session_proper_tests.erl (1 test)

#### Fixture-Based Tests (May need expansion)
These use EUnit fixtures but have minimal test cases:
- erlmcp_cache_tests.erl (1 test, 593 lines) - Large file, may contain nested tests
- erlmcp_session_manager_tests.erl (1 test, 1519 lines) - Very large, likely contains nested tests
- erlmcp_connection_monitor_tests.erl (1 test, 315 lines)
- erlmcp_icon_cache_tests.erl (1 test, 480 lines)
- erlmcp_progress_tests.erl (1 test, 358 lines)
- erlmcp_resource_subscriptions_tests.erl (1 test, 350 lines)
- erlmcp_subscription_tests.erl (1 test, 349 lines)

#### Split Test Suites (May be intentional)
These modules have multiple specialized test files:
- erlmcp_auth_api_tests.erl (2 tests)
- erlmcp_auth_mtls_tests.erl (1 test)
- erlmcp_auth_rate_limiter_tests.erl (2 tests)
- erlmcp_sampling_tests.erl (2 tests)
- erlmcp_schema_validation_tests.erl (2 tests)
- erlmcp_session_manager_basic_tests.erl (1 test)
- erlmcp_session_manager_client_tests.erl (1 test)
- erlmcp_session_manager_error_tests.erl (1 test)
- erlmcp_session_manager_state_tests.erl (1 test)

#### Potentially Incomplete
These should be reviewed for completeness:
- erlmcp_code_reload_tests.erl (1 test, 227 lines)
- erlmcp_initialized_notification_tests.erl (1 test, 118 lines)
- erlmcp_registry_basic_tests.erl (1 test, 230 lines)
- erlmcp_registry_error_tests.erl (1 test, 319 lines)
- erlmcp_registry_transport_tests.erl (1 test, 391 lines)
- erlmcp_resource_integration_tests.erl (2 tests, 124 lines)
- erlmcp_schema_defaults_tests.erl (1 test, 153 lines)
- erlmcp_schema_registry_tests.erl (1 test, 260 lines)
- erlmcp_session_failover_tests.erl (1 test, 600 lines)
- erlmcp_test_helpers_tests.erl (1 test, 94 lines)
- erlmcp_tool_integration_tests.erl (2 tests, 165 lines)

#### Transport Tests (Mostly 1 test per file, may be intentional)
- erlmcp_cross_transport_tests.erl (1 test)
- erlmcp_http_compliance_tests.erl (1 test)
- erlmcp_stdio_compliance_tests.erl (1 test)
- erlmcp_tcp_compliance_tests.erl (1 test)
- erlmcp_websocket_compliance_tests.erl (1 test)
- erlmcp_transport_health_circuit_tests.erl (1 test)
- erlmcp_transport_health_monitoring_tests.erl (1 test)
- erlmcp_transport_health_recovery_tests.erl (1 test)
- erlmcp_transport_registry_health_tests.erl (1 test)
- erlmcp_transport_registry_lifecycle_tests.erl (1 test)
- erlmcp_transport_registry_selection_tests.erl (1 test)
- erlmcp_transport_sse_connection_tests.erl (1 test)
- erlmcp_transport_sse_tests.erl (1 test)
- erlmcp_transport_sse_validation_tests.erl (1 test)
- erlmcp_transport_sup_tests.erl (1 test)
- erlmcp_transport_ws_connection_tests.erl (1 test)
- erlmcp_transport_ws_message_tests.erl (1 test)
- erlmcp_transport_ws_tests.erl (1 test)
- erlmcp_transport_ws_validation_tests.erl (1 test)

---

## 3. Test Files with TODO/SKIP/PLACEHOLDER Annotations

### 3.1 High Priority Issues

**erlmcp_cancellation_tests.erl** (SKIP: 4)
- Contains 2 skipped tests due to monitor bug
- Tests: `monitor_sends_down_to_correct_process_test()` and `cancellation_with_monitor_test()`
- Reason: "Monitor bug: DOWN messages sent to wrong process"
- **Action Required:** Fix monitor bug or file issue

**erlmcp_state_migration_tests.erl** (PLACEHOLDER: 5)
- Contains 5 commented-out not_implemented assertions
- Module appears incomplete
- **Action Required:** Implement state migration tests or remove placeholders

### 3.2 Medium Priority Issues

**erlmcp_transport_integration_tests.erl** (SKIP: 4)
- Skips tests when transports/registry not available
- Conditionally skips tests (acceptable for integration tests)
- **Action Required:** Ensure CI/CD properly sets up test environment

**erlmcp_session_tests.erl** (TODO: 1)
- Comment: "Currently returns empty list (TODO: persistent storage)"
- Related to `list_sessions()` functionality
- **Action Required:** Implement session listing persistence

### 3.3 Low Priority Issues

**erlmcp_auth_tests.erl** (PLACEHOLDER: 1)
- Contains placeholder signature for testing (acceptable)

**erlmcp_transport_http_tests.erl** (PLACEHOLDER: 1)
- Comment: "Integration Test Placeholder"
- **Action Required:** Add HTTP integration tests

**erlmcp_transport_integration_SUITE.erl** (PLACEHOLDER: 1)
- Placeholder for transport mechanism
- **Action Required:** Complete transport integration mechanisms

---

## 4. Common Test Suites with Incomplete Groups

### 4.1 erlmcp_integration_SUITE.erl

**Declared Groups (6):**
- system_integration
- configuration_management
- failure_recovery
- performance_integration
- client_interaction
- monitoring_integration

**Problem:** All 6 groups are declared but not implemented. Instead, 25 individual test functions exist without group structure.

**Recommendation:** Either:
1. Organize the 25 test functions into the 6 declared groups, OR
2. Remove group declarations and use flat test structure

### 4.2 erlmcp_registry_dist_SUITE.erl

**Declared Groups (2):**
- single_node
- multi_node

**Problem:** Groups declared but not used. 8 test functions exist without group structure.

**Recommendation:** Organize tests into single_node and multi_node groups.

### 4.3 erlmcp_transport_behavior_SUITE.erl

**Declared Groups (7):**
- behavior_validation
- message_validation
- transport_options
- message_formats
- behavior_compliance
- validation_functions
- integration

**Problem:** All 7 groups declared but not implemented. 25 individual test functions exist.

**Recommendation:** Organize tests into declared groups or remove group declarations.

### 4.4 erlmcp_transport_http_SUITE.erl

**Declared Groups (8):**
- initialization
- connection
- requests
- responses
- retry_logic
- error_handling
- pool_management
- cleanup

**Problem:** All 8 groups declared but not implemented. 24 individual test functions exist.

**Recommendation:** Organize tests into declared groups (proper HTTP test flow).

### 4.5 erlmcp_transport_integration_SUITE.erl

**Status:** ✅ GOOD - No groups declared, 7 tests implemented and declared in all()

---

## 5. Recommendations

### 5.1 Immediate Actions (High Priority)

1. **Create tests for new validator modules:**
   - erlmcp_protocol_validator_tests.erl
   - erlmcp_transport_validator_tests.erl
   - erlmcp_performance_validator_tests.erl

2. **Fix monitor bug in erlmcp_cancellation_tests.erl:**
   - Unskip the 2 failing tests
   - File GitHub issue if this is an OTP bug

3. **Complete erlmcp_state_migration_tests.erl:**
   - Implement the 5 commented-out tests
   - Or remove module if migration not needed

4. **Organize Common Test suite groups:**
   - erlmcp_integration_SUITE.erl - organize 25 tests into 6 groups
   - erlmcp_registry_dist_SUITE.erl - organize 8 tests into 2 groups
   - erlmcp_transport_behavior_SUITE.erl - organize 25 tests into 7 groups
   - erlmcp_transport_http_SUITE.erl - organize 24 tests into 8 groups

### 5.2 Short-term Actions (Medium Priority)

5. **Add tests for supervisor modules:**
   - erlmcp_sup_tests.erl
   - erlmcp_core_sup_tests.erl
   - erlmcp_server_sup_tests.erl
   - erlmcp_cluster_sup_tests.erl

6. **Add tests for session backend implementations:**
   - erlmcp_session_ets_tests.erl
   - erlmcp_session_dets_tests.erl
   - erlmcp_session_mnesia_tests.erl

7. **Add tests for security/validation modules:**
   - erlmcp_uri_validator_tests.erl
   - erlmcp_mtls_validator_tests.erl
   - erlmcp_schema_validator_tests.erl
   - erlmcp_http_header_validator_tests.erl
   - erlmcp_origin_validator_tests.erl
   - erlmcp_tls_validation_tests.erl

8. **Add tests for transport modules:**
   - erlmcp_transport_tcp_tests.erl (has integration tests only)
   - erlmcp_transport_http_server_tests.erl
   - erlmcp_transport_pool_tests.erl
   - erlmcp_transport_adapter_tests.erl

### 5.3 Long-term Actions (Lower Priority)

9. **Review and expand minimal test files:**
   - Focus on files with <3 tests but >200 lines of code
   - Ensure edge cases are covered

10. **Add tests for utility modules:**
    - erlmcp_message_size_tests.erl
    - erlmcp_path_canonicalizer_tests.erl
    - erlmcp_registry_utils_tests.erl
    - erlmcp_hooks_tests.erl

11. **Add tests for infrastructure modules:**
    - erlmcp_tests.erl (main API)
    - erlmcp_app_tests.erl
    - erlmcp_transports_app_tests.erl
    - erlmcp_client_transport_tests.erl
    - erlmcp_message_handler_tests.erl

12. **Complete placeholder tests:**
    - Add HTTP integration tests
    - Complete transport integration mechanisms

### 5.4 Methodology Compliance

**Chicago School TDD Violations to Check:**
- Ensure NO mocks/fakes/stubs are used (some test files mention this)
- Verify ALL tests use real erlmcp processes
- Confirm tests verify observable behavior, not implementation details
- Test through ALL interfaces (JSON-RPC, stdio, HTTP, WebSocket, TCP)

---

## 6. Coverage Metrics Needed

**To complete this analysis, run:**

```bash
# Generate coverage report
rebar3 cover --verbose

# View HTML report
open _build/test/cover/index.html

# Check per-module coverage
rebar3 cover --verbose | grep -E "erlmcp_.*\s+[0-9]+%"
```

**Target Coverage:**
- Minimum: 80% for all modules
- Core modules: 85%+ (server, client, registry, transport)
- Public APIs: 100% (all exported functions)

**Priority Modules for Coverage:**
1. erlmcp_server
2. erlmcp_client
3. erlmcp_registry
4. erlmcp_json_rpc
5. erlmcp_session_manager
6. Transport implementations (stdio, tcp, http, ws, sse)
7. Validation modules (protocol, transport, security, performance)

---

## 7. Summary Statistics

**Module Coverage:**
- erlmcp_core: 41/77 (53%) have direct tests, 36 missing
- erlmcp_transports: 9/22 (41%) have direct tests, 13 missing
- **Total: 50/99 (51%) have direct tests**

**Test Quality:**
- 56 test files with <3 test cases
- 7 test files with TODO/SKIP/PLACEHOLDER
- 4 Common Test suites with incomplete groups
- 0 completely empty test files

**Chicago School TDD Compliance:**
- Most test files explicitly state "NO MOCKS" (good)
- Need to verify actual test implementations follow this

**Next Steps:**
1. Run coverage analysis: `rebar3 cover --verbose`
2. Prioritize tests for new validator modules
3. Fix cancellation test monitor bug
4. Organize Common Test suite groups
5. Add tests for supervisors and session backends

---

**End of Analysis**
