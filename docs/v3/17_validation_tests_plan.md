# erlmcp v3 Validation Tests - Complete Test Plan

**Document Version:** 1.0.0
**Date:** 2026-01-31
**Status:** Draft - Ready for Implementation
**Author:** Erlang Test Engineer Agent

---

## Executive Summary

This document provides a **comprehensive test plan** for the erlmcp v3 validation application (`apps/erlmcp_validation`), targeting **100% MCP specification compliance validation**.

### Current State Analysis

**Validation App Module Count:** 21 modules
- **Currently Tested:** 16 modules (76%)
- **Untested:** 5 modules (24%):
  1. `erlmcp_validation_app.erl` - Application callback
  2. `erlmcp_validation_sup.erl` - Top-level supervisor
  3. `erlmcp_validate_cli.erl` - CLI interface (partial tests exist)
  4. `erlmcp_security_validator.erl` - Security validation (partial tests exist)
  5. `erlmcp_performance_validator.erl` - Performance validation (partial tests exist)
  6. `erlmcp_uri_validator.erl` - URI validation (partial tests exist)

**Existing Test Count:** 38 test files
- **EUnit tests:** 26 modules
- **Common Test suites:** 12 suites

### Test Plan Goals

1. **Complete coverage** of all 21 validation modules (100%)
2. **CLI testing** - comprehensive argument parsing and command execution
3. **Security boundary validation** - authentication, JWT, secrets, rate limiting, CORS
4. **Performance validation testing** - latency, throughput, memory, concurrency
5. **URI validation testing** - RFC 3986 compliance, SSRF prevention, injection prevention
6. **Application/supervisor testing** - lifecycle, supervision tree, restart strategies

---

## Table of Contents

1. [Test Strategy](#test-strategy)
2. [Untested Modules Analysis](#untested-modules-analysis)
3. [CLI Testing Plan](#cli-testing-plan)
4. [Security Validator Testing Plan](#security-validator-testing-plan)
5. [Performance Validator Testing Plan](#performance-validator-testing-plan)
6. [URI Validator Testing Plan](#uri-validator-testing-plan)
7. [Application/Supervisor Testing Plan](#applicationsupervisor-testing-plan)
8. [Test Implementation Roadmap](#test-implementation-roadmap)
9. [Quality Gates](#quality-gates)
10. [Execution Plan](#execution-plan)

---

## Test Strategy

### Chicago School TDD Principles

**All tests MUST follow:**

1. **Real Processes Only** - No mocks, fakes, or stubs
   - Spawn actual gen_servers for testing
   - Use real transport implementations
   - Test actual observable behavior

2. **State-Based Verification** - Assert on state, not interactions
   - Test API results and return values
   - Verify gen_server state through API calls
   - No internal state inspection or mocking

3. **Observable Behavior Testing** - Black-box testing only
   - Test through public API interfaces
   - Verify outputs, not internal calls
   - No white-box testing of implementation details

4. **Test Isolation** - Each test independent
   - Proper setup/teardown lifecycle
   - No shared state between tests
   - Clean resource management

### Test Organization

**EUnit Tests** (unit and integration):
- Location: `apps/erlmcp_validation/test/<module>_tests.erl`
- Fast execution (< 1 second per test)
- Real processes, no mocks
- State-based verification

**Common Test Suites** (integration and system):
- Location: `apps/erlmcp_validation/test/<module>_SUITE.erl`
- Slower execution (transport-level tests)
- Multi-process coordination
- Full protocol validation

---

## Untested Modules Analysis

### Module 1: erlmcp_validation_app.erl

**Type:** Application callback module
**Lines of Code:** 16
**Complexity:** Low

**Exported Functions:**
- `start/2` - Application startup
- `stop/1` - Application shutdown

**Test Requirements:**
- Application starts successfully
- Application stops cleanly
- Supervisor is started correctly
- Dependencies are available

**Test Count Target:** 3 tests

### Module 2: erlmcp_validation_sup.erl

**Type:** Supervisor module
**Lines of Code:** 87
**Complexity:** Low-Medium

**Child Specifications:**
1. `erlmcp_compliance_report` - Report gen_server
2. `erlmcp_memory_manager` - Memory management gen_server

**Supervision Strategy:** `one_for_one`
- Intensity: 10
- Period: 60 seconds

**Test Requirements:**
- Supervisor starts correctly
- Child specifications are valid
- Children start under supervision
- Child restart works (one_for_one)
- Max restart intensity enforced
- Supervisor can be stopped

**Test Count Target:** 8 tests

### Module 3: erlmcp_validate_cli.erl

**Type:** CLI interface + programmatic API
**Lines of Code:** 2,023
**Complexity:** High

**Exported Functions:**
- `run/1, run/2` - Programmatic API
- `validate_spec/0` - Spec validation
- `validate_protocol_message/1` - Protocol message validation
- `validate_transport/1` - Transport validation
- `validate_compliance/0` - Full compliance validation
- `validate_all/0` - All validators
- `generate_compliance_report/1` - Report generation
- `main/1` - Escript entry point

**CLI Commands:**
- `erlmcp-validate validate <url>` - Validate running server
- `erlmcp-validate spec-check` - Check spec compliance
- `erlmcp-validate report` - Generate compliance report
- `erlmcp-validate transport-check <name>` - Verify transport behavior
- Legacy commands: `spec`, `protocol`, `transport`, `compliance`, `all`

**Current Test Coverage:**
- **Existing:** 92 tests in `erlmcp_validate_cli_tests.erl`
- **Gaps:**
  - CLI argument parsing edge cases
  - Error handling for invalid CLI syntax
  - Format conversion edge cases (json, text, markdown, html)
  - File I/O for report generation
  - Exit code validation
  - Integration with real validators

**Additional Tests Needed:** 48 tests
- CLI parsing: 15 tests
- Command execution: 12 tests
- Format handling: 8 tests
- Error handling: 8 tests
- Integration: 5 tests

**Total Target:** 140 tests

### Module 4: erlmcp_security_validator.erl

**Type:** gen_server - Security validation
**Lines of Code:** 1,140
**Complexity:** High

**Security Categories:**
1. **Authentication (4 checks):**
   - Auth mechanism
   - Token handling
   - Session management
   - Authorization

2. **Input Validation (5 checks):**
   - JSON schema validation
   - Parameter sanitization
   - SQL injection prevention
   - XSS prevention
   - Path traversal prevention

3. **Secret Management (4 checks):**
   - No hardcoded secrets
   - Environment variable usage
   - Secret file permissions
   - Secret rotation support

4. **JWT Validation (4 checks):**
   - JWT structure
   - Signature validation
   - Expiration checking
   - Claims validation

5. **Rate Limiting (3 checks):**
   - Rate limit mechanism
   - Distributed rate limiting
   - Rate limit bypass prevention

6. **CORS (3 checks):**
   - CORS headers
   - Origin validation
   - Preflight handling

**Current Test Coverage:**
- **Existing:** Basic tests in `erlmcp_security_validator_tests.erl`
- **Gaps:**
  - Full security boundary testing
  - Attack vector simulation
  - Compliance with OWASP Top 10
  - JWT edge cases
  - Rate limiting accuracy
  - CORS bypass attempts

**Additional Tests Needed:** 72 tests
- Authentication: 12 tests
- Input validation: 18 tests
- Secret management: 12 tests
- JWT: 12 tests
- Rate limiting: 9 tests
- CORS: 9 tests

**Total Target:** 90 tests

### Module 5: erlmcp_performance_validator.erl

**Type:** Performance validation (not gen_server)
**Lines of Code:** 1,213
**Complexity:** High

**Performance Metrics:**
1. **Latency:**
   - P50: < 5ms
   - P95: < 20ms
   - P99: < 50ms

2. **Throughput:**
   - Target: > 1000 req/s

3. **Memory:**
   - Per connection: < 100KB
   - Memory leak detection

4. **Connection Setup:**
   - Target: < 100ms

5. **Concurrency:**
   - Target: 10K connections support

**Current Test Coverage:**
- **Existing:** Basic unit tests in `erlmcp_performance_validator_tests.erl`
- **Gaps:**
  - Real transport performance measurement
  - Benchmark comparison against baselines
  - Performance regression detection
  - Memory leak detection
  - Concurrent connection stress testing
  - Sustained load testing

**Additional Tests Needed:** 65 tests
- Latency measurement: 15 tests
- Throughput measurement: 15 tests
- Memory measurement: 12 tests
- Connection setup: 8 tests
- Concurrency: 10 tests
- Report generation: 5 tests

**Total Target:** 85 tests

### Module 6: erlmcp_uri_validator.erl

**Type:** URI validation (not gen_server)
**Lines of Code:** 362
**Complexity:** Medium

**Validation Features:**
1. **RFC 3986 Compliance:**
   - Scheme validation
   - Host parsing
   - Port validation
   - Path/query/fragment parsing

2. **Security Features:**
   - Injection prevention (SQL, command, path traversal)
   - SSRF prevention (private IP detection)
   - Dangerous character filtering
   - DNS rebinding detection

3. **URI Templates:**
   - Variable substitution syntax
   - Template validation

**Current Test Coverage:**
- **Existing:** 28 tests in `erlmcp_uri_validator_tests.erl`
- **Gaps:**
  - IPv6 address validation
  - Internationalized domain names (IDN)
  - Edge case URIs
  - DNS rebinding patterns
  - URI template edge cases
  - Performance with large URIs

**Additional Tests Needed:** 22 tests
- IPv6 validation: 5 tests
- IDN validation: 3 tests
- Edge cases: 6 tests
- DNS rebinding: 4 tests
- URI templates: 4 tests

**Total Target:** 50 tests

---

## CLI Testing Plan

### Test Suite: erlmcp_validate_cli_tests.erl (Enhanced)

**Target:** 140 tests total (48 new tests)

### Test Categories

#### 1. CLI Argument Parsing (15 tests)

**Test Group 1.1: Basic Command Parsing (5 tests)**
```erlang
test_parse_help_command()
test_parse_version_command()
test_parse_validate_spec_command()
test_parse_validate_protocol_command()
test_parse_validate_transport_command()
```

**Validation:**
- Correct command identification
- Argument count validation
- Command type mapping

**Test Group 1.2: Option Parsing (5 tests)**
```erlang
test_parse_validate_format_json()
test_parse_validate_format_text()
test_parse_validate_transport_option()
test_parse_report_format_markdown()
test_parse_report_output_file()
```

**Validation:**
- Option key extraction
- Option value parsing
- Default value handling
- Option type validation

**Test Group 1.3: Error Cases (5 tests)**
```erlang
test_parse_invalid_command()
test_parse_invalid_validate_type()
test_parse_invalid_format()
test_parse_invalid_transport()
test_parse_missing_required_argument()
```

**Validation:**
- Error detection
- Error message generation
- Help text display
- Exit code validation

#### 2. Command Execution (12 tests)

**Test Group 2.1: Validation Commands (6 tests)**
```erlang
test_run_command_validate_spec()
test_run_command_validate_protocol()
test_run_command_validate_transport()
test_run_command_validate_security()
test_run_command_validate_performance()
test_run_command_validate_all()
```

**Validation:**
- Command dispatch
- Validator invocation
- Result collection
- State verification

**Test Group 2.2: Report Commands (3 tests)**
```erlang
test_run_command_report_generate()
test_run_command_report_with_format()
test_run_command_report_with_output()
```

**Validation:**
- Report generation
- Format conversion
- File I/O operations

**Test Group 2.3: Utility Commands (3 tests)**
```erlang
test_run_command_version()
test_run_command_help()
test_run_command_unknown()
```

**Validation:**
- Version display
- Help text formatting
- Unknown command handling

#### 3. Format Handling (8 tests)

**Test Group 3.1: Output Formats (4 tests)**
```erlang
test_format_to_json()
test_format_to_text()
test_format_to_markdown()
test_format_to_html()
```

**Validation:**
- Format conversion correctness
- Schema validation for JSON
- Syntax validation for Markdown/HTML
- Character encoding

**Test Group 3.2: Format Edge Cases (4 tests)**
```erlang
test_format_invalid_format()
test_format_empty_results()
test_format_large_results()
test_format_special_characters()
```

**Validation:**
- Error handling
- Empty state handling
- Large data handling
- Unicode/special character handling

#### 4. Error Handling (8 tests)

**Test Group 4.1: Validation Errors (4 tests)**
```erlang
test_error_invalid_spec_version()
test_error_missing_transport()
test_error_invalid_uri()
test_error_timeout()
```

**Validation:**
- Error detection
- Error message quality
- Error recovery
- Error propagation

**Test Group 4.2: System Errors (4 tests)**
```erlang
test_error_application_not_started()
test_error_validator_crash()
test_error_file_permission_denied()
test_error_disk_full()
```

**Validation:**
- System error handling
- Graceful degradation
- Resource cleanup
- Logging verification

#### 5. Integration Testing (5 tests)

**Test Group 5.1: End-to-End Workflows (5 tests)**
```erlang
test_integration_full_validation_workflow()
test_integration_report_generation_workflow()
test_integration_multi_transport_validation()
test_integration_error_recovery_workflow()
test_integration_concurrent_execution()
```

**Validation:**
- Complete workflow execution
- Multi-component coordination
- State consistency
- Resource management
- Concurrency safety

---

## Security Validator Testing Plan

### Test Suite: erlmcp_security_validator_SUITE.erl (Enhanced)

**Target:** 90 tests total (72 new tests)

### Test Categories

#### 1. Authentication Testing (12 tests)

**Test Group 1.1: Auth Mechanisms (3 tests)**
```erlang
test_auth_mechanism_present()
test_auth_mechanism_type()
test_auth_mechanism_configuration()
```

**Validation:**
- Auth mechanism exists
- Auth type (token, cert, API key)
- Configurable auth providers

**Test Group 1.2: Token Handling (3 tests)**
```erlang
test_token_validation()
test_token_expiration()
test_token_revocation()
```

**Validation:**
- Token format validation
- Expiration enforcement
- Revocation checking

**Test Group 1.3: Session Management (3 tests)**
```erlang
test_session_creation()
test_session_timeout()
test_session_cleanup()
```

**Validation:**
- Session establishment
- Timeout enforcement
- Cleanup on expiration

**Test Group 1.4: Authorization (3 tests)**
```erlang
test_authorization_checks()
test_role_based_access()
test_permission_denied()
```

**Validation:**
- Permission verification
- Role-based enforcement
- Denial handling

#### 2. Input Validation Testing (18 tests)

**Test Group 2.1: JSON Schema Validation (4 tests)**
```erlang
test_schema_validation_valid()
test_schema_validation_invalid_type()
test_schema_validation_missing_field()
test_schema_validation_extra_field()
```

**Validation:**
- Schema enforcement
- Type checking
- Required fields
- Rejection of unknown fields

**Test Group 2.2: Parameter Sanitization (4 tests)**
```erlang
test_sanitize_sql_injection()
test_sanitize_command_injection()
test_sanitize_xss()
test_sanitize_path_traversal()
```

**Validation:**
- SQL injection prevention
- Command injection prevention
- XSS prevention
- Path traversal prevention

**Test Group 2.3: SQL Injection Prevention (4 tests)**
```erlang
test_sql_injection_union_select()
test_sql_injection_comment()
test_sql_injection_stacked()
test_sql_injection_time_based()
```

**Validation:**
- UNION SELECT attacks
- Comment-based attacks
- Stacked queries
- Time-based blind attacks

**Test Group 2.4: XSS Prevention (3 tests)**
```erlang
test_xss_script_tag()
test_xss_event_handler()
test_xss_javascript_protocol()
```

**Validation:**
- Script tag filtering
- Event handler filtering
- Dangerous protocol blocking

**Test Group 2.5: Path Traversal Prevention (3 tests)**
```erlang
test_path_traversal_dotdot()
test_path_traversal_encoded()
test_path_traversal_unicode()
```

**Validation:**
- `../` pattern blocking
- URL-encoded variants
- Unicode evasion attempts

#### 3. Secret Management Testing (12 tests)

**Test Group 3.1: Hardcoded Secrets (3 tests)**
```erlang
test_no_hardcoded_passwords()
test_no_hardcoded_api_keys()
test_no_hardcoded_tokens()
```

**Validation:**
- Source code scanning
- Pattern detection
- Secret leakage prevention

**Test Group 3.2: Environment Variables (3 tests)**
```erlang
test_secret_from_env()
test_env_var_missing()
test_env_var_invalid()
```

**Validation:**
- Environment variable loading
- Missing variable handling
- Invalid value handling

**Test Group 3.3: Secret File Permissions (3 tests)**
```erlang
test_secret_file_readable()
test_secret_file_not_world_readable()
test_secret_file_permissions()
```

**Validation:**
- File accessibility
- Permission checks
- Secure defaults

**Test Group 3.4: Secret Rotation (3 tests)**
```erlang
test_secret_rotation_supported()
test_secret_rotation_timing()
test_secret_rotation_grace_period()
```

**Validation:**
- Rotation capability
- Rotation triggers
- Grace period handling

#### 4. JWT Validation Testing (12 tests)

**Test Group 4.1: JWT Structure (3 tests)**
```erlang
test_jwt_header_valid()
test_jwt_payload_valid()
test_jwt_signature_valid()
```

**Validation:**
- Header structure
- Payload claims
- Signature presence

**Test Group 4.2: Signature Validation (3 tests)**
```erlang
test_jwt_signature_valid_key()
test_jwt_signature_invalid_key()
test_jwt_signature_algorithm()
```

**Validation:**
- Valid signature verification
- Invalid signature rejection
- Algorithm whitelist enforcement

**Test Group 4.3: Expiration Checking (3 tests)**
```erlang
test_jwt_not_expired()
test_jwt_expired()
test_jwt_expiration_skew()
```

**Validation:**
- Valid token acceptance
- Expired token rejection
- Clock skew tolerance

**Test Group 4.4: Claims Validation (3 tests)**
```erlang
test_jwt_issuer_valid()
test_jwt_audience_valid()
test_jwt_claims_required()
```

**Validation:**
- Issuer (iss) validation
- Audience (aud) validation
- Required claims presence

#### 5. Rate Limiting Testing (9 tests)

**Test Group 5.1: Rate Limit Mechanism (3 tests)**
```erlang
test_rate_limit_enforced()
test_rate_limit_burst_allowed()
test_rate_limit_exceeded()
```

**Validation:**
- Rate enforcement
- Burst allowance
- Limit rejection

**Test Group 5.2: Distributed Rate Limiting (3 tests)**
```erlang
test_distributed_rate_limit_sync()
test_distributed_rate_limit_node_failure()
test_distributed_rate_limit_recovery()
```

**Validation:**
- Cross-node synchronization
- Node failure handling
- Recovery after failure

**Test Group 5.3: Rate Limit Bypass Prevention (3 tests)**
```erlang
test_rate_limit_bypass_ip_rotation()
test_rate_limit_bypass_user_agent()
test_rate_limit_bypass_header_manipulation()
```

**Validation:**
- IP rotation detection
- User-agent tracking
- Header manipulation prevention

#### 6. CORS Testing (9 tests)

**Test Group 6.1: CORS Headers (3 tests)**
```erlang
test_cors_allow_origin()
test_cors_allow_methods()
test_cors_allow_headers()
```

**Validation:**
- Origin header validation
- Methods header validation
- Headers validation

**Test Group 6.2: Origin Validation (3 tests)**
```erlang
test_cors_valid_origin()
test_cors_invalid_origin()
test_cors_null_origin()
```

**Validation:**
- Whitelist enforcement
- Invalid origin rejection
- Null origin handling

**Test Group 6.3: Preflight Handling (3 tests)**
```erlang
test_cors_preflight_options()
test_cors_preflight_cache()
test_cors_preflight_max_age()
```

**Validation:**
- OPTIONS method handling
- Cache headers
- Max-age validation

---

## Performance Validator Testing Plan

### Test Suite: erlmcp_performance_validator_SUITE.erl (Enhanced)

**Target:** 85 tests total (65 new tests)

### Test Categories

#### 1. Latency Measurement (15 tests)

**Test Group 1.1: P50 Latency (5 tests)**
```erlang
test_p50_latency_pass_stdio()
test_p50_latency_pass_tcp()
test_p50_latency_fail_slow()
test_p50_latency_regression()
test_p50_latency_improvement()
```

**Validation:**
- P50 < 5ms target
- Pass/fail determination
- Regression detection (>10% degradation)
- Improvement tracking

**Test Group 1.2: P95 Latency (5 tests)**
```erlang
test_p95_latency_pass()
test_p95_latency_fail()
test_p95_variance()
test_p95_outliers()
test_p95_percentile_calculation()
```

**Validation:**
- P95 < 20ms target
- Variance analysis
- Outlier detection
- Accurate percentile calculation

**Test Group 1.3: P99 Latency (5 tests)**
```erlang
test_p99_latency_pass()
test_p99_latency_fail()
test_p99_tail_latency()
test_p99_worst_case()
test_p99_stability()
```

**Validation:**
- P99 < 50ms target
- Tail latency analysis
- Worst-case scenario
- Stability over time

#### 2. Throughput Measurement (15 tests)

**Test Group 2.1: Request Rate (5 tests)**
```erlang
test_throughput_pass_1000()
test_throughput_fail_below_1000()
test_throughput_burst()
test_throughput_sustained()
test_throughput_scaling()
```

**Validation:**
- >1000 req/s target
- Burst capability
- Sustained load handling
- Scaling characteristics

**Test Group 2.2: Connection Efficiency (5 tests)**
```erlang
test_throughput_per_connection()
test_throughput_concurrent_requests()
test_throughput_pipeline()
test_throughput_batch()
test_throughput_multiplexing()
```

**Validation:**
- Per-connection rate
- Concurrent request handling
- Pipelining efficiency
- Batch processing
- Multiplexing support

**Test Group 2.3: Resource Utilization (5 tests)**
```erlang
test_throughput_cpu_usage()
test_throughput_memory_usage()
test_throughput_network_bandwidth()
test_throughput_file_descriptors()
test_throughput_context_switches()
```

**Validation:**
- CPU efficiency
- Memory efficiency
- Network usage
- File descriptor usage
- Context switch rate

#### 3. Memory Measurement (12 tests)

**Test Group 3.1: Per-Connection Memory (4 tests)**
```erlang
test_memory_per_connection_pass()
test_memory_per_connection_fail()
test_memory_per_connection_leak()
test_memory_per_connection_gc()
```

**Validation:**
- <100KB per connection target
- Memory leak detection
- GC effectiveness

**Test Group 3.2: Total Memory (4 tests)**
```erlang
test_memory_total_baseline()
test_memory_total_growth()
test_memory_total_fragmentation()
test_memory_total_heap()
```

**Validation:**
- Baseline measurement
- Growth rate
- Fragmentation analysis
- Heap usage

**Test Group 3.3: Memory Leak Detection (4 tests)**
```erlang
test_memory_leak_short_term()
test_memory_leak_long_term()
test_memory_leak_cycles()
test_memory_leak_references()
```

**Validation:**
- Short-term leak detection (1 minute)
- Long-term leak detection (1 hour)
- GC cycle analysis
- Reference counting

#### 4. Connection Setup Testing (8 tests)

**Test Group 4.1: Setup Time (4 tests)**
```erlang
test_connection_setup_pass()
test_connection_setup_fail()
test_connection_setup_variance()
test_connection_setup_worst_case()
```

**Validation:**
- <100ms target
- Variance analysis
- Worst-case scenario

**Test Group 4.2: Setup Scalability (4 tests)**
```erlang
test_connection_setup_concurrent()
test_connection_setup_rate_limit()
test_connection_setup_backlog()
test_connection_setup_rejection()
```

**Validation:**
- Concurrent setup capability
- Rate limiting
- Backlog handling
- Rejection under load

#### 5. Concurrent Connections Testing (10 tests)

**Test Group 5.1: Connection Capacity (5 tests)**
```erlang
test_concurrent_1k()
test_concurrent_5k()
test_concurrent_10k()
test_concurrent_20k()
test_concurrent_50k()
```

**Validation:**
- 1K connection target
- 5K connection stretch
- 10K connection target
- 20K connection stress
- 50K connection extreme

**Test Group 5.2: Connection Stability (5 tests)**
```erlang
test_concurrent_stability_1min()
test_concurrent_stability_5min()
test_concurrent_churn()
test_concurrent_drop()
test_concurrent_recovery()
```

**Validation:**
- 1-minute stability
- 5-minute stability
- Connection churn handling
- Drop detection
- Recovery after drops

#### 6. Report Generation (5 tests)

**Test Group 6.1: Report Content (5 tests)**
```erlang
test_report_completeness()
test_report_accuracy()
test_report_format()
test_report_aggregation()
test_report_trends()
```

**Validation:**
- All metrics included
- Accurate calculations
- Correct format
- Proper aggregation
- Trend analysis

---

## URI Validator Testing Plan

### Test Suite: erlmcp_uri_validator_tests.erl (Enhanced)

**Target:** 50 tests total (22 new tests)

### Test Categories

#### 1. IPv6 Address Validation (5 tests)

**Test Group 1.1: IPv6 Formats (5 tests)**
```erlang
test_ipv6_full_notation()
test_ipv6_compressed()
test_ipv6_mixed_ipv4()
test_ipv6_loopback()
test_ipv6_private()
```

**Validation:**
- Full notation (`2001:0db8:85a3:0000:0000:8a2e:0370:7334`)
- Compressed notation (`2001:db8:85a3::8a2e:370:7334`)
- Mixed IPv4 (`::ffff:192.0.2.1`)
- Loopback (`::1`)
- Private ranges (`fc00::/7`, `fe80::/10`)

#### 2. Internationalized Domain Names (3 tests)

**Test Group 2.1: IDN Support (3 tests)**
```erlang
test_idn_punycode_encoded()
test_idn_unicode()
test_idn_mixed_case()
```

**Validation:**
- Punycode encoding (`xn--`)
- Unicode domain names
- Case sensitivity

#### 3. Edge Cases (6 tests)

**Test Group 3.1: Unusual URIs (6 tests)**
```erlang
test_uri_empty_scheme()
test_uri_empty_host()
test_uri_very_long()
test_uri_special_chars()
test_uri_unicode_path()
test_uri_fragment_query_order()
```

**Validation:**
- Empty components
- Maximum length (2048 bytes)
- Special character handling
- Unicode in path
- Fragment vs query order

#### 4. DNS Rebinding Patterns (4 tests)

**Test Group 4.1: Rebinding Attacks (4 tests)**
```erlang
test_dns_rebinding_127()
test_dns_rebinding_localhost()
test_dns_rebinding_subdomain()
test_dns_rebinding_long_chain()
```

**Validation:**
- `127.0.0.1.example.com` pattern
- `localhost.example.com` pattern
- Subdomain detection
- Long domain chain handling

#### 5. URI Templates (4 tests)

**Test Group 5.1: Template Syntax (4 tests)**
```erlang
test_template_single_variable()
test_template_multiple_variables()
test_template_nested_braces()
test_template_invalid_syntax()
```

**Validation:**
- Single variable `{var}`
- Multiple variables `{var1}/{var2}`
- Nested brace handling
- Invalid syntax rejection

---

## Application/Supervisor Testing Plan

### Test Suite: erlmcp_validation_app_SUITE.erl

**Target:** 11 tests total

### Test Categories

#### 1. Application Lifecycle (3 tests)

**Test Group 1.1: Application Start/Stop (3 tests)**
```erlang
test_app_starts_successfully()
test_app_stops_cleanly()
test_app_dependencies_available()
```

**Validation:**
- Application starts without errors
- All dependencies available
- Clean shutdown

#### 2. Supervisor Behavior (8 tests)

**Test Group 2.1: Child Management (5 tests)**
```erlang
test_supervisor_starts_children()
test_supervisor_child_count()
test_supervisor_child_specs()
test_supervisor_one_for_one_strategy()
test_supervisor_restart_intensity()
```

**Validation:**
- All children start
- Correct child count (2)
- Valid child specifications
- one_for_one strategy
- Intensity: 10, period: 60

**Test Group 2.2: Child Restart (3 tests)**
```erlang
test_child_restart_on_crash()
test_child_max_restart_intensity()
test_supervisor_terminates_on_failure()
```

**Validation:**
- Individual child restart
- Max restart enforcement
- Supervisor termination after max restarts

---

## Test Implementation Roadmap

### Phase 1: CLI Testing (Week 1)

**Deliverables:**
- Enhanced `erlmcp_validate_cli_tests.erl`
- 48 new tests added
- Total: 140 tests for CLI

**Tasks:**
1. Implement CLI argument parsing tests (15 tests)
2. Implement command execution tests (12 tests)
3. Implement format handling tests (8 tests)
4. Implement error handling tests (8 tests)
5. Implement integration tests (5 tests)

**Acceptance Criteria:**
- All 140 tests pass
- Code coverage ≥ 85%
- Chicago School TDD compliance verified

### Phase 2: Security Testing (Week 2)

**Deliverables:**
- Enhanced `erlmcp_security_validator_SUITE.erl`
- 72 new tests added
- Total: 90 tests for security

**Tasks:**
1. Implement authentication tests (12 tests)
2. Implement input validation tests (18 tests)
3. Implement secret management tests (12 tests)
4. Implement JWT validation tests (12 tests)
5. Implement rate limiting tests (9 tests)
6. Implement CORS tests (9 tests)

**Acceptance Criteria:**
- All 90 tests pass
- OWASP Top 10 coverage verified
- All security boundaries tested

### Phase 3: Performance Testing (Week 3)

**Deliverables:**
- Enhanced `erlmcp_performance_validator_SUITE.erl`
- 65 new tests added
- Total: 85 tests for performance

**Tasks:**
1. Implement latency measurement tests (15 tests)
2. Implement throughput measurement tests (15 tests)
3. Implement memory measurement tests (12 tests)
4. Implement connection setup tests (8 tests)
5. Implement concurrent connection tests (10 tests)
6. Implement report generation tests (5 tests)

**Acceptance Criteria:**
- All 85 tests pass
- Baseline metrics established
- Regression detection working

### Phase 4: URI Validation Testing (Week 3)

**Deliverables:**
- Enhanced `erlmcp_uri_validator_tests.erl`
- 22 new tests added
- Total: 50 tests for URI validation

**Tasks:**
1. Implement IPv6 validation tests (5 tests)
2. Implement IDN validation tests (3 tests)
3. Implement edge case tests (6 tests)
4. Implement DNS rebinding tests (4 tests)
5. Implement URI template tests (4 tests)

**Acceptance Criteria:**
- All 50 tests pass
- RFC 3986 compliance verified
- SSRF prevention tested

### Phase 5: Application/Supervisor Testing (Week 4)

**Deliverables:**
- New `erlmcp_validation_app_SUITE.erl`
- New `erlmcp_validation_sup_SUITE.erl`
- 11 tests total

**Tasks:**
1. Implement application lifecycle tests (3 tests)
2. Implement supervisor behavior tests (8 tests)

**Acceptance Criteria:**
- All 11 tests pass
- Supervision tree verified
- Restart strategies validated

### Phase 6: Integration and Documentation (Week 4)

**Deliverables:**
- Test execution documentation
- Coverage reports
- Quality gate verification

**Tasks:**
1. Run all test suites
2. Generate coverage reports
3. Verify Chicago School TDD compliance
4. Document test execution procedures

---

## Quality Gates

### Pre-Test Gates

**Code Quality:**
- [ ] All modules compile without errors
- [ ] Zero compiler warnings
- [ ] Dialyzer warnings reviewed and addressed
- [ ] Code formatted (rebar3 format)

**Test Readiness:**
- [ ] Test file naming convention followed
- [ ] EUnit or CT properly configured
- [ ] Setup/teardown fixtures defined
- [ ] Test data generators prepared

### Post-Test Gates

**Test Execution:**
- [ ] All tests pass (0 failures)
- [ ] No flaky tests (10 consecutive runs all pass)
- [ ] Test execution time acceptable (< 5 minutes per suite)

**Coverage Requirements:**
- [ ] Overall coverage ≥ 80%
- [ ] Core modules coverage ≥ 85%
- [ ] Public API coverage = 100%
- [ ] Coverage report generated

**Chicago School TDD Compliance:**
- [ ] No mock objects used
- [ ] All tests use real processes
- [ ] State-based verification only
- [ ] No internal state inspection
- [ ] Observable behavior testing only

### Documentation Gates

- [ ] All tests have descriptive names
- [ ] Test categories documented
- [ ] Edge cases documented
- [ ] Test execution procedures documented

---

## Execution Plan

### Test Execution Commands

**Run All Validation Tests:**
```bash
# EUnit tests
rebar3 eunit --app=erlmcp_validation

# Common Test suites
rebar3 ct --dir=apps/erlmcp_validation/test

# With coverage
rebar3 do eunit --app=erlmcp_validation, cover --verbose
```

**Run Specific Test Suites:**
```bash
# CLI tests
rebar3 eunit --module=erlmcp_validate_cli_tests

# Security validator
rebar3 ct --suite=erlmcp_security_validator_SUITE

# Performance validator
rebar3 ct --suite=erlmcp_performance_validator_SUITE

# URI validator
rebar3 eunit --module=erlmcp_uri_validator_tests

# Application/Supervisor
rebar3 ct --suite=erlmcp_validation_app_SUITE
rebar3 ct --suite=erlmcp_validation_sup_SUITE
```

### Continuous Integration

**GitHub Actions Workflow:**
```yaml
name: Validation Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "28.3.1"
      - run: rebar3 compile
      - run: rebar3 do eunit --app=erlmcp_validation, cover
      - run: rebar3 ct --dir=apps/erlmcp_validation/test
      - uses: codecov/codecov-action@v3
```

### Test Reports

**Coverage Reports:**
```bash
# Generate HTML coverage report
rebar3 cover --verbose

# View report
open _build/test/cover/index.html
```

**Test Results:**
- Location: `_build/test/logs/`
- Format: CT logs (HTML), EUnit output (console)

---

## Summary

### Test Coverage Targets

| Module | Current Tests | Target Tests | Additional Tests | Coverage Target |
|--------|---------------|--------------|------------------|-----------------|
| erlmcp_validation_app | 0 | 3 | 3 | 85% |
| erlmcp_validation_sup | 0 | 8 | 8 | 85% |
| erlmcp_validate_cli | 92 | 140 | 48 | 85% |
| erlmcp_security_validator | 18 | 90 | 72 | 85% |
| erlmcp_performance_validator | 20 | 85 | 65 | 85% |
| erlmcp_uri_validator | 28 | 50 | 22 | 85% |
| **Total** | **158** | **376** | **218** | **85%** |

### Deliverables

**Test Suites:**
1. `erlmcp_validate_cli_tests.erl` (enhanced) - 140 tests
2. `erlmcp_security_validator_SUITE.erl` (enhanced) - 90 tests
3. `erlmcp_performance_validator_SUITE.erl` (enhanced) - 85 tests
4. `erlmcp_uri_validator_tests.erl` (enhanced) - 50 tests
5. `erlmcp_validation_app_SUITE.erl` (new) - 3 tests
6. `erlmcp_validation_sup_SUITE.erl` (new) - 8 tests

**Total:** 376 tests across 6 test suites

### Success Criteria

**Quantitative:**
- 376 tests implemented
- 100% of validation modules tested
- 85%+ code coverage achieved
- 0 test failures
- Chicago School TDD compliance verified

**Qualitative:**
- All security boundaries tested
- Performance baselines established
- RFC 3986 compliance verified
- CLI usability validated
- Application lifecycle tested

---

**Document Status:** Draft - Ready for Implementation
**Next Steps:** Begin Phase 1 (CLI Testing)
**Contact:** Erlang Test Engineer Agent
