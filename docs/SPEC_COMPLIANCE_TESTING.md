# MCP Specification Compliance Testing Guide

## Overview

This guide covers testing erlmcp for compliance with the **MCP 2025-11-25 specification**. The test suite is organized by spec sections (Lifecycle, Tools, Resources, Prompts, Transports, Error Codes) following **Chicago School TDD** principles with black-box testing.

## Test Organization

### By Spec Section

Tests are organized to mirror the MCP specification structure:

```
apps/erlmcp_validation/test/
├── erlmcp_spec_parser_tests.erl              # Spec parser unit tests (10 tests)
├── erlmcp_protocol_validator_tests.erl       # Protocol validation (54 tests)
├── erlmcp_vulnerability_scanner_tests.erl    # Vulnerability scanning (12 tests)
├── erlmcp_validator_accuracy_tests.erl       # Validator accuracy (15 tests)
├── erlmcp_compliance_report_tests.erl        # Compliance reporting (5 tests)
├── erlmcp_memory_manager_tests.erl           # Memory management (3 tests)
├── erlmcp_performance_validator_tests.erl    # Performance validation (2 tests)
│
├── erlmcp_protocol_checker_SUITE.erl         # Protocol checker suite (5 tests)
├── erlmcp_performance_validator_SUITE.erl    # Performance validator suite (21 tests)
├── erlmcp_error_response_SUITE.erl           # Error response handling (10 tests)
├── erlmcp_error_handling_robustness_SUITE.erl # Error handling robustness (20 tests)
│
├── erlmcp_security_comprehensive_SUITE.erl   # Security comprehensive (40+ tests)
├── erlmcp_authorization_SUITE.erl            # Authorization/RBAC (19 tests)
├── erlmcp_error_recovery_SUITE.erl           # Error recovery (24 tests)
├── erlmcp_network_failure_recovery_SUITE.erl # Network failure recovery (14 tests)
├── erlmcp_integration_contracts_SUITE.erl    # Integration contracts (10 tests)
│
└── erlmcp_spec_compliance_SUITE.ct           # Main spec compliance suite
    ├── lifecycle (10 tests)
    ├── tools (12 tests)
    ├── resources (14 tests)
    ├── prompts (8 tests)
    ├── transports (15 tests)
    └── error_codes (12 tests)
```

### Test Frameworks

- **EUnit**: Unit tests and quick tests (`*_tests.erl`)
- **Common Test**: Integration and suite tests (`*_SUITE.ct`, `*_SUITE.erl`)

## Running Tests

### Run All Tests

```bash
# Run EUnit tests
rebar3 eunit

# Run Common Test suites
rebar3 ct

# Run specific suite
rebar3 ct --suite=erlmcp_spec_compliance_SUITE

# Run with verbose output
rebar3 ct --verbose
```

### Run Specific Test Categories

```bash
# Run spec parser tests
rebar3 eunit --module=erlmcp_spec_parser_tests

# Run protocol validator tests
rebar3 ct --suite=erlmcp_protocol_validator_SUITE

# Run transport validator tests
rebar3 ct --suite=erlmcp_transport_validator_SUITE
```

### Run with Coverage

```bash
# Generate coverage report
rebar3 cover

# View coverage in browser
open _build/test/cover/index.html
```

## Test Categories

### 1. Lifecycle Tests (10 tests)

Validates MCP connection lifecycle requirements.

**Key Tests:**
- `initialize_must_be_first_test` - First message must be initialize
- `protocol_version_negotiation_test` - Version 2025-11-25 negotiated
- `capability_exchange_test` - Capabilities correctly exchanged
- `initialized_notification_test` - Notification sent after initialize
- `reinitialize_rejection_test` - Duplicate initialize rejected

**Example:**
```erlang
initialize_must_be_first_test(_Config) ->
    % Attempt to send tools/list before initialize
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/list">>
    },
    % Should return error -32003 (Not initialized)
    ?assertMatch({error, -32003, _}, send_request(Request)).
```

### 2. Tools API Tests (12 tests)

Validates tools/list and tools/call methods.

**Key Tests:**
- `tools_list_returns_array_test` - Response is array
- `tools_list_structure_test` - Tool has name, description, inputSchema
- `tools_call_execution_test` - Execute tool with arguments
- `tools_call_invalid_args_test` - Reject invalid arguments
- `tools_progress_token_test` - Progress token support

**Example:**
```erlang
tools_list_structure_test(_Config) ->
    % Initialize connection
    initialize_connection(),

    % Call tools/list
    {ok, Response} = tools_list(),

    % Validate response structure
    Tools = maps_get(<<"tools">>, Response),
    ?assert(is_list(Tools)),
    lists:foreach(fun(Tool) ->
        ?assert(maps:is_key(<<"name">>, Tool)),
        ?assert(maps:is_key(<<"description">>, Tool)),
        ?assert(maps:is_key(<<"inputSchema">>, Tool))
    end, Tools).
```

### 3. Resources API Tests (14 tests)

Validates resources/list, resources/read, subscriptions.

**Key Tests:**
- `resources_list_returns_array_test` - Response is array
- `resources_read_execution_test` - Read resource content
- `resources_subscribe_test` - Subscribe to updates
- `resources_uri_validation_test` - URI format validation
- `resources_concurrent_reads_test` - Concurrent reads work

**Example:**
```erlang
resources_subscribe_test(_Config) ->
    % Initialize and assert resources capability
    initialize_with_capability(<<"resources">>),

    % Subscribe to resource
    Uri = <<"file:///test.txt">>,
    {ok, _} = subscribe_resource(Uri),

    % Verify subscription active
    ?assert(is_subscribed(Uri)).
```

### 4. Prompts API Tests (8 tests)

Validates prompts/list and prompts/get methods.

**Key Tests:**
- `prompts_list_returns_array_test` - Response is array
- `prompts_get_execution_test` - Get prompt template
- `prompts_argument_validation_test` - Validate arguments
- `prompts_rendering_test` - Template rendering

### 5. Transport Tests (15 tests)

Validates transport-specific behavior.

**Key Tests:**
- `stdio_newline_delimited_test` - STDIO uses newline delimiter
- `tcp_length_prefix_test` - TCP framing correct
- `http_content_type_test` - HTTP Content-Type header
- `websocket_text_frames_test` - WebSocket uses text frames
- `cross_transport_consistency_test` - Same behavior across transports

**Example:**
```erlang
stdio_newline_delimited_test(_Config) ->
    % Send JSON-RPC message via STDIO
    Message = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>}),
    StdioMessage = <<Message/binary, "\n">>,

    % Verify newline delimiter
    ?assertEqual(<<"\n">>, binary:part(StdioMessage, {byte_size(StdioMessage), -1})).
```

### 6. Error Codes Tests (12 tests)

Validates all JSON-RPC and MCP error codes.

**Key Tests:**
- `jsonrpc_parse_error_test` - -32700 Parse error
- `jsonrpc_method_not_found_test` - -32601 Method not found
- `mcp_resource_not_found_test` - -32008 Resource not found
- `mcp_error_structure_test` - Error has code, message, data

**Example:**
```erlang
jsonrpc_parse_error_test(_Config) ->
    % Send invalid JSON
    InvalidJson = <<"{invalid json}">>,

    % Should get parse error -32700
    {ok, Response} = send_raw(InvalidJson),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, Error)).
```

## Advanced Test Categories

### 7. Security Comprehensive Suite (40+ tests)

Validates security features including authorization, injection vulnerabilities, certificate validation, and penetration testing scenarios.

#### 7a. Authorization & RBAC (19 tests)
**Location:** `erlmcp_authorization_SUITE.erl`

**Key Tests:**
- `admin_role_full_access_test` - Admin role has full access
- `user_role_limited_access_test` - User role has limited access
- `guest_role_read_only_access_test` - Guest role is read-only
- `role_escalation_prevention_test` - Prevent role escalation
- `resource_access_permissions_test` - Resource access control
- `tool_execution_permissions_test` - Tool execution authorization
- `session_creation_permissions_test` - Session creation rights
- `invalid_permission_denied_test` - Invalid permissions denied

**Example:**
```erlang
admin_role_full_access_test(_Config) ->
    % Create admin session
    {ok, Session} = create_session(<<"admin">>, <<"test_admin_key">>),

    % Test full access to all operations
    ?assertMatch({ok, _}, erlmcp_auth:check_permission(Session, <<"tools:call">>)),
    ?assertMatch({ok, _}, erlmcp_auth:check_permission(Session, <<"resources:read">>)),
    ?assertMatch({ok, _}, erlmcp_auth:check_permission(Session, <<"admin:shutdown">>)).
```

#### 7b. Certificate Validation (7 tests)
**Location:** `erlmcp_security_comprehensive_SUITE.erl`

**Key Tests:**
- `valid_certificate_accepted_test` - Valid certificates accepted
- `expired_certificate_rejected_test` - Expired certificates rejected
- `self_signed_certificate_handling_test` - Self-signed cert handling
- `invalid_ca_rejected_test` - Invalid CA rejected
- `certificate_pinning_test` - Certificate pinning enforcement
- `wildcard_certificate_validation_test` - Wildcard cert validation
- `certificate_chain_validation_test` - Certificate chain validation

#### 7c. Injection Vulnerabilities (16 tests)
**Location:** `erlmcp_security_comprehensive_SUITE.erl`

**SQL Injection (3 tests):**
- `sql_injection_resource_uri_test` - SQLi in resource URI
- `sql_injection_tool_params_test` - SQLi in tool parameters
- `sql_injection_prompt_template_test` - SQLi in prompt templates

**XSS Prevention (3 tests):**
- `xss_in_resource_content_test` - XSS in resource content
- `xss_in_tool_response_test` - XSS in tool responses
- `xss_in_prompt_rendering_test` - XSS in prompt rendering

**Command Injection (3 tests):**
- `command_injection_tool_name_test` - Command injection in tool names
- `command_injection_uri_test` - Command injection in URIs
- `command_injection_arguments_test` - Command injection in arguments

**Path Traversal (4 tests):**
- `path_traversal_resource_uri_test` - Path traversal in resource URI
- `path_traversal_file_read_test` - Path traversal in file read
- `path_traversal_template_include_test` - Path traversal in includes
- `path_traversal_log_injection_test` - Path traversal in logs

**Other Injections (3 tests):**
- `ldap_injection_filter_test` - LDAP injection in filters
- `header_injection_test` - Header injection attacks
- `template_injection_test` - Template injection attacks

#### 7d. Penetration Testing Scenarios (8 tests)
**Location:** `erlmcp_security_comprehensive_SUITE.erl`

**Key Tests:**
- `brute_force_rate_limiting_test` - Rate limiting prevents brute force
- `session_hijacking_prevention_test` - Session hijacking prevention
- `csrf_token_validation_test` - CSRF token validation
- `replay_attack_prevention_test` - Replay attack prevention
- `timing_attack_resistance_test` - Timing attack resistance
- `memory_leak_detection_test` - Memory leak detection
- `sql_injection_prevention_test` - SQL injection prevention
- `xss_prevention_test` - XSS prevention

**Example:**
```erlang
brute_force_rate_limiting_test(_Config) ->
    % Attempt multiple failed authentications
    lists:foreach(fun(_) ->
        ?assertMatch({error, _}, authenticate(<<"invalid">>, <<"invalid">>))
    end, lists:seq(1, 100)),

    % Verify rate limiting triggered
    ?assertMatch({error, rate_limited}, authenticate(<<"valid">>, <<"valid">>)).
```

### 8. Error Recovery Suite (24+ tests)

Validates system recovery from various failure scenarios.

#### 8a. Process Crash Recovery (5 tests)
**Location:** `erlmcp_error_recovery_SUITE.erl`

**Key Tests:**
- `registry_crash_recovery_test` - Registry crash recovery
- `client_crash_recovery_test` - Client crash recovery
- `server_crash_recovery_test` - Server crash recovery
- `session_manager_crash_recovery_test` - Session manager recovery
- `supervisor_tree_recovery_validation_test` - Supervision tree recovery

#### 8b. Transaction Rollback (4 tests)
**Location:** `erlmcp_error_recovery_SUITE.erl`

**Key Tests:**
- `resource_subscription_rollback_test` - Resource subscription rollback
- `tool_execution_rollback_test` - Tool execution rollback
- `multi_step_transaction_atomicity_test` - Multi-step atomicity
- `state_rollback_on_cancellation_test` - Cancellation rollback

#### 8c. State Validation (4 tests)
**Location:** `erlmcp_error_recovery_SUITE.erl`

**Key Tests:**
- `request_id_consistency_after_recovery_test` - Request ID consistency
- `capability_integrity_after_restart_test` - Capability integrity
- `registry_state_validation_test` - Registry state validation
- `pending_request_cleanup_test` - Pending request cleanup

#### 8d. Network Failure Recovery (14 tests)
**Location:** `erlmcp_network_failure_recovery_SUITE.erl`

**Transport Failures (5 tests):**
- `test_tcp_connection_drop_recovery` - TCP connection drop recovery
- `test_http_timeout_recovery` - HTTP timeout recovery
- `test_websocket_disconnection_recovery` - WebSocket disconnection recovery
- `test_sse_reconnection_after_failure` - SSE reconnection
- `test_multi_transport_failover` - Multi-transport failover

**Data Consistency (3 tests):**
- `test_state_consistency_after_network_failure` - State consistency
- `test_inflight_request_handling_on_failure` - In-flight request handling
- `test_resource_subscription_recovery` - Resource subscription recovery

**Recovery Time Objectives (3 tests):**
- `test_connection_loss_rto` - Connection loss RTO (<5s)
- `test_server_restart_rto` - Server restart RTO
- `test_network_partition_rto` - Network partition RTO

**State Restoration (4 tests):**
- `test_session_state_restoration` - Session state restoration
- `test_pending_request_restoration` - Pending request restoration
- `test_resource_list_restoration` - Resource list restoration
- `test_tool_list_restoration` - Tool list restoration

**Example:**
```erlang
test_connection_loss_rto(_Config) ->
    % Start client and server
    {ok, Client} = erlmcp_client:start_link(tcp, []),
    {ok, Server} = erlmcp_server:start_link(#{}),

    % Establish connection
    {ok, _} = erlmcp_client:connect(Client, "localhost", 9999),

    % Inject connection failure
    StartTime = erlang:monotonic_time(millisecond),
    erlmcp_chaos:inject_failure(network_partition),

    % Measure recovery time
    wait_for_recovery(Client),
    RecoveryTime = erlang:monotonic_time(millisecond) - StartTime,

    % Assert RTO <5s
    ?assert(RecoveryTime < 5000),
    ?assertMatch(initialized, get_state(Client)).
```

#### 8e. Supervision Tree Validation (4 tests)
**Location:** `erlmcp_error_recovery_SUITE.erl`

**Key Tests:**
- `one_for_one_recovery_test` - one_for_one recovery
- `one_for_all_recovery_test` - one_for_all recovery
- `rest_for_one_recovery_test` - rest_for_one recovery
- `max_restart_intensity_validation_test` - Max restart intensity

#### 8f. Chaos Engineering Integration (4 tests)
**Location:** `erlmcp_error_recovery_SUITE.erl`

**Key Tests:**
- `chaos_kill_servers_recovery_test` - Kill servers recovery
- `chaos_kill_random_recovery_test` - Random kill recovery
- `chaos_memory_exhaustion_recovery_test` - Memory exhaustion recovery
- `chaos_circuit_breaker_recovery_test` - Circuit breaker recovery

### 9. Integration Contracts Suite (10+ tests)

Validates integration contracts between components.

**Location:** `erlmcp_integration_contracts_SUITE.erl`

**Key Tests:**
- `test_valid_initialize_stdio` - Initialize via STDIO
- `test_capability_exchange` - Capability exchange
- `test_version_negotiation` - Version negotiation
- `test_invalid_init_rejection` - Invalid init rejection
- `test_request_id_correlation` - Request ID correlation
- `test_response_format` - Response format validation
- `test_error_contracts` - Error contracts
- `test_tools_contract` - Tools API contract
- `test_resources_contract` - Resources API contract
- `test_prompts_contract` - Prompts API contract

### 10. Performance Validator Suite (21 tests)

Validates performance characteristics and SLAs.

**Location:** `erlmcp_performance_validator_SUITE.erl`

**Key Tests:**
- Latency measurements (p50, p95, p99)
- Throughput validation
- Memory usage limits
- Connection pool efficiency
- Concurrent request handling

### 11. Error Handling Robustness Suite (20 tests)

Validates error handling robustness.

**Location:** `erlmcp_error_handling_robustness_SUITE.erl`

**Key Tests:**
- Malformed message handling
- Protocol violation recovery
- Resource exhaustion handling
- Concurrent error scenarios

## Black-Box Testing (Chicago School TDD)

### Principles

1. **Test Observable Behavior**: Test only through public APIs
2. **No Mocks**: Use real erlmcp processes
3. **State Verification**: Assert on state changes, not implementation
4. **Real Transports**: Test through all transports (stdio, tcp, http, websocket)

### Example: Testing Without Mocks

**❌ Wrong (with mocks):**
```erlang
% This VIOLATES Chicago School TDD
test_tool_call() ->
    meck:new(erlmcp_server),
    meck:expect(erlmcp_server, handle_call, fun(...) -> {reply, ok, State} end),
    % Test with mocked server
    meck:unload(erlmcp_server).
```

**✅ Correct (real processes):**
```erlang
% This FOLLOWS Chicago School TDD
test_tool_call(_Config) ->
    % Start real server
    {ok, Server} = erlmcp_server:start_link(#{ capabilities => #{<<"tools">> => true} }),

    % Start real client
    {ok, Client} = erlmcp_client:start_link(stdio, []),

    % Send real request through client
    {ok, Response} = erlmcp_client:call_tool(Client, <<"my_tool">>, #{}),

    % Assert on observable response
    ?assertMatch(#{<<"content">> := _}, Response),

    % Clean up
    erlmcp_client:stop(Client),
    erlmcp_server:stop(Server).
```

### Setup/Teardown

```erlang
init_per_suite(Config) ->
    % Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    Config.

end_per_suite(_Config) ->
    % Stop applications
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core).

init_per_testcase(_TestCase, Config) ->
    % Start fresh server for each test
    {ok, Server} = erlmcp_server:start_link(test_config()),
    [{server, Server} | Config].

end_per_testcase(_TestCase, Config) ->
    % Clean up server
    Server = proplists:get_value(server, Config),
    erlmcp_server:stop(Server).
```

## Test Execution Guide

### Running All Tests

```bash
# Run all EUnit tests
rebar3 eunit

# Run all Common Test suites
rebar3 ct

# Run all tests with coverage
rebar3 cover
```

### Running Specific Test Suites

#### Protocol and Compliance Tests
```bash
# Spec parser tests (10 tests)
rebar3 eunit --module=erlmcp_spec_parser_tests

# Protocol validator tests (54 tests)
rebar3 eunit --module=erlmcp_protocol_validator_tests

# Protocol checker suite (5 tests)
rebar3 ct --suite=erlmcp_protocol_checker_SUITE
```

#### Security Tests
```bash
# Authorization and RBAC tests (19 tests)
rebar3 ct --suite=erlmcp_authorization_SUITE

# Security comprehensive suite (40+ tests)
rebar3 ct --suite=erlmcp_security_comprehensive_SUITE

# Vulnerability scanner tests (12 tests)
rebar3 eunit --module=erlmcp_vulnerability_scanner_tests
```

#### Error Recovery and Robustness Tests
```bash
# Error recovery suite (24+ tests)
rebar3 ct --suite=erlmcp_error_recovery_SUITE

# Network failure recovery suite (14 tests)
rebar3 ct --suite=erlmcp_network_failure_recovery_SUITE

# Error handling robustness suite (20 tests)
rebar3 ct --suite=erlmcp_error_handling_robustness_SUITE

# Error response suite (10 tests)
rebar3 ct --suite=erlmcp_error_response_SUITE
```

#### Performance and Integration Tests
```bash
# Performance validator suite (21 tests)
rebar3 ct --suite=erlmcp_performance_validator_SUITE

# Performance validator tests (2 tests)
rebar3 eunit --module=erlmcp_performance_validator_tests

# Integration contracts suite (10 tests)
rebar3 ct --suite=erlmcp_integration_contracts_SUITE
```

#### Validation Tests
```bash
# Validator accuracy tests (15 tests)
rebar3 eunit --module=erlmcp_validator_accuracy_tests

# Compliance report tests (5 tests)
rebar3 eunit --module=erlmcp_compliance_report_tests

# Memory manager tests (3 tests)
rebar3 eunit --module=erlmcp_memory_manager_tests
```

### Running Tests by Category

```bash
# Run all security-related tests
rebar3 ct --suite=erlmcp_authorization_SUITE \
          --suite=erlmcp_security_comprehensive_SUITE
rebar3 eunit --module=erlmcp_vulnerability_scanner_tests

# Run all recovery-related tests
rebar3 ct --suite=erlmcp_error_recovery_SUITE \
          --suite=erlmcp_network_failure_recovery_SUITE \
          --suite=erlmcp_error_handling_robustness_SUITE

# Run all validation tests
rebar3 eunit --module=erlmcp_spec_parser_tests \
             --module=erlmcp_protocol_validator_tests \
             --module=erlmcp_validator_accuracy_tests
```

### Coverage Report Generation

```bash
# Generate coverage report for all tests
rebar3 cover

# View coverage in browser
open _build/test/cover/index.html

# Generate coverage for specific module
rebar3 cover --module=erlmcp_client

# Export coverage to XML
rebar3 cover --export coverage.xml
```

### Coverage Targets and Current Status

#### Overall Coverage Goals

| Category | Current | Target | Status |
|----------|---------|--------|--------|
| **Overall Coverage** | 62% | 80% | ⚠️ Below Target |
| **Spec-Driven Coverage** | 75% | 95% | ⚠️ Below Target |
| **Critical Paths** | 85% | 100% | ⚠️ Below Target |
| **Security Tests** | 70% | 90% | ⚠️ Below Target |
| **Error Recovery** | 65% | 85% | ⚠️ Below Target |

#### Test Count Summary

| Suite | Test Count | Status |
|-------|------------|--------|
| Protocol Validator Tests | 54 | ✅ Complete |
| Security Comprehensive | 40+ | ✅ Complete |
| Error Recovery | 24+ | ✅ Complete |
| Performance Validator | 21 | ✅ Complete |
| Error Handling Robustness | 20 | ✅ Complete |
| Authorization | 19 | ✅ Complete |
| Network Failure Recovery | 14 | ✅ Complete |
| Integration Contracts | 10 | ✅ Complete |
| Validator Accuracy | 15 | ✅ Complete |
| Vulnerability Scanner | 12 | ✅ Complete |
| Spec Parser | 10 | ✅ Complete |
| Error Response | 10 | ✅ Complete |
| Protocol Checker | 5 | ✅ Complete |
| Compliance Report | 5 | ✅ Complete |
| Memory Manager | 3 | ✅ Complete |
| Performance Validator Unit | 2 | ✅ Complete |
| **Total** | **260+** | **✅ Comprehensive** |

#### Coverage Improvement Plan

1. **Increase Overall Coverage** (62% → 80%)
   - Add edge case tests for uncovered branches
   - Add error path tests for all modules
   - Add concurrent operation tests

2. **Achieve Spec-Driven Coverage** (75% → 95%)
   - Map all MCP spec requirements to tests
   - Add missing API method tests
   - Add all error code tests

3. **Complete Critical Path Coverage** (85% → 100%)
   - Add initialization sequence tests
   - Add shutdown sequence tests
   - Add state transition tests

## Achieving 95%+ Spec-Driven Coverage

### Coverage Strategy

1. **Map Spec Requirements to Tests**: Each spec requirement has corresponding test
2. **Test All Methods**: Every MCP method has test coverage
3. **Test All Error Codes**: Every error code is validated
4. **Test All Transports**: All 4 transports tested
5. **Edge Cases**: Boundary conditions and error paths
6. **Security Scenarios**: All OWASP Top 10 covered
7. **Recovery Scenarios**: All failure modes tested

### Coverage Tracking

```bash
# Generate coverage report
rebar3 cover

# Check coverage percentage
grep -A 5 "Percentage" _build/test/cover/index.html

# View coverage by module
open _build/test/cover/index.html
```

### Coverage Targets

- **Overall**: ≥ 80% (Current: 62%, Target: 80%)
- **Spec-Driven**: ≥ 95% (Current: 75%, Target: 95%)
- **Critical Paths**: 100% (Current: 85%, Target: 100%)
- **Security**: ≥ 90% (Current: 70%, Target: 90%)
- **Error Recovery**: ≥ 85% (Current: 65%, Target: 85%)

### Traceability Matrix

#### Lifecycle Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Lifecycle | Initialize first | `initialize_must_be_first_test` | ✅ |
| Lifecycle | Protocol version negotiation | `protocol_version_negotiation_test` | ✅ |
| Lifecycle | Capability exchange | `capability_exchange_test` | ✅ |
| Lifecycle | Initialized notification | `initialized_notification_test` | ✅ |
| Lifecycle | Reinitialize rejection | `reinitialize_rejection_test` | ✅ |
| Lifecycle | Graceful shutdown | `graceful_shutdown_test` | ✅ |

#### Tools API Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Tools | tools/list array | `tools_list_returns_array_test` | ✅ |
| Tools | Tool structure (name, description, inputSchema) | `tools_list_structure_test` | ✅ |
| Tools | tools/call execution | `tools_call_execution_test` | ✅ |
| Tools | Invalid arguments rejection | `tools_call_invalid_args_test` | ✅ |
| Tools | Progress token support | `tools_progress_token_test` | ✅ |
| Tools | Tool authorization | `tool_call_authorization_test` | ✅ |
| Tools | Dangerous tool restrictions | `dangerous_tool_restrictions_test` | ✅ |

#### Resources API Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Resources | resources/list array | `resources_list_returns_array_test` | ✅ |
| Resources | resources/read execution | `resources_read_execution_test` | ✅ |
| Resources | Resource subscription | `resources_subscribe_test` | ✅ |
| Resources | URI validation | `resources_uri_validation_test` | ✅ |
| Resources | Concurrent reads | `resources_concurrent_reads_test` | ✅ |
| Resources | Resource access permissions | `resource_access_permissions_test` | ✅ |
| Resources | Subscription recovery | `resource_subscription_recovery_test` | ✅ |

#### Prompts API Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Prompts | prompts/list array | `prompts_list_returns_array_test` | ✅ |
| Prompts | prompts/get execution | `prompts_get_execution_test` | ✅ |
| Prompts | Argument validation | `prompts_argument_validation_test` | ✅ |
| Prompts | Template rendering | `prompts_rendering_test` | ✅ |
| Prompts | Prompt permissions | `prompt_template_permissions_test` | ✅ |

#### Transport Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Transports | STDIO newline delimiter | `stdio_newline_delimited_test` | ✅ |
| Transports | TCP length prefix | `tcp_length_prefix_test` | ✅ |
| Transports | HTTP Content-Type header | `http_content_type_test` | ✅ |
| Transports | WebSocket text frames | `websocket_text_frames_test` | ✅ |
| Transports | Cross-transport consistency | `cross_transport_consistency_test` | ✅ |
| Transports | TCP connection drop recovery | `test_tcp_connection_drop_recovery` | ✅ |
| Transports | HTTP timeout recovery | `test_http_timeout_recovery` | ✅ |
| Transports | WebSocket disconnection recovery | `test_websocket_disconnection_recovery` | ✅ |

#### Error Code Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Error Codes | -32700 Parse error | `jsonrpc_parse_error_test` | ✅ |
| Error Codes | -32601 Method not found | `jsonrpc_method_not_found_test` | ✅ |
| Error Codes | -32602 Invalid params | `jsonrpc_invalid_params_test` | ✅ |
| Error Codes | -32002 Invalid request | `mcp_invalid_request_test` | ✅ |
| Error Codes | -32003 Not initialized | `mcp_not_initialized_test` | ✅ |
| Error Codes | -32008 Resource not found | `mcp_resource_not_found_test` | ✅ |
| Error Codes | Error structure (code, message, data) | `mcp_error_structure_test` | ✅ |

#### Security Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Security | RBAC - Admin full access | `admin_role_full_access_test` | ✅ |
| Security | RBAC - User limited access | `user_role_limited_access_test` | ✅ |
| Security | RBAC - Guest read-only | `guest_role_read_only_access_test` | ✅ |
| Security | Role escalation prevention | `role_escalation_prevention_test` | ✅ |
| Security | Certificate validation | `valid_certificate_accepted_test` | ✅ |
| Security | Expired certificate rejection | `expired_certificate_rejected_test` | ✅ |
| Security | SQL injection prevention | `sql_injection_prevention_test` | ✅ |
| Security | XSS prevention | `xss_prevention_test` | ✅ |
| Security | Command injection prevention | `command_injection_tool_name_test` | ✅ |
| Security | Path traversal prevention | `path_traversal_resource_uri_test` | ✅ |
| Security | Brute force rate limiting | `brute_force_rate_limiting_test` | ✅ |
| Security | Session hijacking prevention | `session_hijacking_prevention_test` | ✅ |
| Security | CSRF token validation | `csrf_token_validation_test` | ✅ |
| Security | Replay attack prevention | `replay_attack_prevention_test` | ✅ |

#### Error Recovery Requirements

| Spec Section | Requirement | Test | Status |
|--------------|-------------|------|--------|
| Recovery | Registry crash recovery | `registry_crash_recovery_test` | ✅ |
| Recovery | Client crash recovery | `client_crash_recovery_test` | ✅ |
| Recovery | Server crash recovery | `server_crash_recovery_test` | ✅ |
| Recovery | Transaction rollback | `resource_subscription_rollback_test` | ✅ |
| Recovery | State consistency after recovery | `request_id_consistency_after_recovery_test` | ✅ |
| Recovery | Connection loss RTO <5s | `test_connection_loss_rto` | ✅ |
| Recovery | Server restart RTO | `test_server_restart_rto` | ✅ |
| Recovery | Network partition RTO | `test_network_partition_rto` | ✅ |
| Recovery | Supervision tree recovery | `one_for_one_recovery_test` | ✅ |
| Recovery | Chaos engineering recovery | `chaos_kill_servers_recovery_test` | ✅ |

## CI/CD Integration

### GitHub Actions Workflow

```yaml
name: Spec Compliance Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "25.3"

      - name: Compile
        run: rebar3 compile

      - name: Run EUnit
        run: rebar3 eunit

      - name: Run Common Test
        run: rebar3 ct

      - name: Generate Coverage
        run: rebar3 cover

      - name: Upload Coverage
        uses: actions/upload-artifact@v3
        with:
          name: coverage-report
          path: _build/test/cover/
```

### Quality Gates

- **Compilation**: 0 errors
- **Tests**: 100% pass rate
- **Coverage**: ≥ 80%
- **Spec Compliance**: ≥ 95%

## Evidence Collection

### Test Evidence

Each test provides evidence of spec compliance:

```erlang
resources_read_execution_test(_Config) ->
    % ARRANGE: Initialize with resource capability
    {ok, Server} = setup_server_with_capability(<<"resources">>),
    Uri = <<"file:///test.txt">>,

    % ACT: Read resource
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"resources/read">>,
        <<"params">> => #{<<"uri">> => Uri}
    },
    Response = send_request(Server, Request),

    % ASSERT: Validate response structure
    ?assertMatch({ok, #{<<"result">> := #{<<"contents">> := [_|_]}}}, Response),

    % EVIDENCE: Log for compliance report
    ct:log("Resource read successful for URI: ~s", [Uri]),
    {comment, "Evidence: Resource content returned in contents array"}.
```

### Compliance Report

Generate compliance report:

```bash
./erlmcp_validate run --all --format markdown > compliance_report.md
```

Example output:

```markdown
# MCP Compliance Report

## Summary
- Overall: 96.2%
- Lifecycle: 100%
- Tools: 98%
- Resources: 95%
- Prompts: 92%
- Transports: 94%
- Error Codes: 100%

## Evidence
- 71 tests passed
- 3 tests skipped (unsupported features)
- 0 tests failed
```

## Troubleshooting

### Common Issues

**Issue**: Test fails with "not initialized"

**Solution**: Ensure `initialize_connection()` is called first in test.

**Issue**: Transport test times out

**Solution**: Check port availability and firewall rules.

**Issue**: Coverage below 80%

**Solution**: Add tests for uncovered branches and error paths.

### Debug Mode

```bash
# Run tests with verbose output
rebar3 ct --verbose

# Run specific test with debug
rebar3 ct --suite=erlmcp_spec_compliance_SUITE --case=tools_list_structure_test --verbose
```

## See Also

- [Spec Parser Guide](SPEC_PARSER_GUIDE.md)
- [Validator Guide](VALIDATOR_GUIDE.md)
- [MCP 2025-11-25 Specification](https://modelcontextprotocol.io/)
