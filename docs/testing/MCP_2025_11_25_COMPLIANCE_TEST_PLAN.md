# MCP 2025-11-25 Specification Compliance Test Plan

**Version:** 1.0.0
**Date:** 2026-01-30
**Status:** Strategic Planning
**Test Philosophy:** Chicago School TDD (real processes, no mocks, state-based verification)
**Coverage Target:** ≥80% (minimum), ≥85% for core modules

---

## Executive Summary

This test plan provides comprehensive coverage for MCP 2025-11-25 specification compliance in erlmcp. It identifies gaps in current test coverage (38 existing test files), defines new test suites following Chicago School TDD principles, and establishes quality gates for production readiness.

**Key Metrics:**
- **Current Test Files:** 38 test modules
- **Core Modules:** 65 source files requiring coverage
- **Planned Test Suites:** 15 new/enhanced test modules
- **Total Test Scenarios:** 300+ test cases
- **Estimated Implementation:** 8-12 weeks

---

## 1. Current Test Coverage Analysis

### 1.1 Existing Test Files (38 modules)

**Unit Tests (EUnit):**
```
✅ erlmcp_json_rpc_tests.erl           - JSON-RPC 2.0 encoding/decoding
✅ erlmcp_client_tests.erl             - Client lifecycle and operations
✅ erlmcp_registry_tests.erl           - Process registry
✅ erlmcp_resource_tests.erl           - Resource operations
✅ erlmcp_tool_tests.erl               - Tool execution
✅ erlmcp_auth_tests.erl               - Authentication
✅ erlmcp_cache_tests.erl              - Caching layer
✅ erlmcp_circuit_breaker_tests.erl    - Circuit breaker
✅ erlmcp_connection_monitor_tests.erl - Connection monitoring
✅ erlmcp_rate_limiting_tests.erl      - Rate limiting
✅ erlmcp_schema_validator_tests.erl   - JSON Schema validation
✅ erlmcp_capability_negotiation_tests.erl - Basic capability tests
✅ erlmcp_pagination_tests.erl         - Pagination
✅ erlmcp_batch_tests.erl              - Batch operations
✅ erlmcp_logging_tests.erl            - Logging
✅ erlmcp_notification_handler_tests.erl - Notification handlers
✅ erlmcp_request_id_tests.erl         - Request ID correlation
✅ erlmcp_memory_guard_tests.erl       - Memory protection
✅ erlmcp_ets_race_condition_tests.erl - ETS race conditions
✅ erlmcp_rate_limit_edge_case_tests.erl - Rate limit edge cases
✅ erlmcp_rate_limit_middleware_tests.erl - Rate limit middleware
✅ erlmcp_sse_event_store_tests.erl    - SSE event storage
✅ erlmcp_supervisor_collapse_tests.erl - Supervisor resilience
```

**Integration Tests (Common Test):**
```
✅ erlmcp_integration_SUITE.erl        - End-to-end integration
✅ erlmcp_registry_dist_SUITE.erl      - Distributed registry
```

**Property Tests (Proper):**
```
✅ erlmcp_json_rpc_proper_tests.erl    - JSON-RPC properties
```

**Benchmarks:**
```
✅ erlmcp_bench_core_ops.erl           - Core operations
✅ erlmcp_bench_network_real.erl       - Network I/O
✅ erlmcp_bench_stress.erl             - Stress testing
✅ erlmcp_bench_chaos.erl              - Chaos engineering
✅ erlmcp_bench_integration.erl        - Integration benchmarks
✅ erlmcp_bench_cache.erl              - Cache performance
✅ erlmcp_bench_helpers.erl            - Benchmark utilities
```

### 1.2 Coverage Gaps Identified

**Critical Gaps (P0):**
1. ❌ **Server initialization state machine** - No tests for initialize → initialized → shutdown flow
2. ❌ **Protocol version validation** - No tests for 2025-11-25 version enforcement
3. ❌ **Capability negotiation compliance** - Limited tests for full capability exchange
4. ❌ **Tool metadata validation** - No tests for description length (≤10000 chars), deprecated flag
5. ❌ **Resource URI validation** - No tests for RFC 3986 compliance, path traversal attacks
6. ❌ **Content type validation** - No tests for text/image/audio content types
7. ❌ **Notification rate limiting** - No tests for tools/list_changed rate limiting
8. ❌ **Progress token tracking** - No tests for progress reporting
9. ❌ **Graceful shutdown** - No tests for cleanup and shutdown flow
10. ❌ **Error code compliance** - Limited tests for MCP-specific error codes (-32001 to -32099)

**Important Gaps (P1):**
11. ❌ **Prompt argument validation** - Limited tests for required/optional arguments
12. ❌ **Resource subscription lifecycle** - No tests for subscribe → notify → unsubscribe
13. ❌ **Batch request handling** - Limited tests for batch operations
14. ❌ **Transport layer compliance** - No tests for transport-specific behaviors
15. ❌ **State machine edge cases** - No tests for invalid state transitions
16. ❌ **Concurrent capability changes** - No tests for race conditions in capability updates
17. ❌ **Message size limits** - No tests for oversized messages
18. ❌ **Backwards compatibility** - No tests for older protocol versions

**Nice-to-Have Gaps (P2):**
19. ⚠️ **Sampling capability** - No tests for LLM sampling requests
20. ⚠️ **Roots capability** - No tests for filesystem roots
21. ⚠️ **Logging levels** - Limited tests for log level filtering
22. ⚠️ **Experimental capabilities** - No tests for custom capability passthrough

---

## 2. MCP 2025-11-25 Specification Requirements

### 2.1 Core Protocol Requirements

**Initialize Handshake:**
- MUST support initialize request as first message
- MUST validate protocolVersion = "2025-11-25"
- MUST return server capabilities in response
- MUST capture client capabilities from request
- MUST transition to initialized state after initialize
- MUST reject non-initialize requests before initialization

**Capability Negotiation:**
- MUST advertise server capabilities (resources, tools, prompts, logging)
- MUST support optional capabilities (sampling, roots)
- MUST support experimental capabilities (custom fields)
- MUST validate client capabilities structure
- MUST respect capability flags (enabled = true/false)

**State Machine:**
```
initialization → initialized → ready → shutdown
     ↓              ↓          ↓          ↓
  (reject)      (accept)   (operate)  (cleanup)
```

### 2.2 Resource System Requirements

**Resource Management:**
- MUST support static resources (fixed URIs)
- MUST support resource templates (URI patterns)
- MUST validate URI format (RFC 3986)
- MUST prevent path traversal attacks (../)
- MUST support resource metadata (name, description, mime_type)
- MUST support content types (text, blob)

**Resource Operations:**
- resources/list - List available resources
- resources/read - Read resource content
- resources/subscribe - Subscribe to resource changes
- resources/unsubscribe - Unsubscribe from resource
- resources/updated - Notification when resource changes
- resources/list_changed - Notification when list changes

### 2.3 Tool System Requirements

**Tool Management:**
- MUST support tool registration with handler functions
- MUST support tool metadata (name, description, schema)
- MUST validate description length (≤10000 characters)
- MUST support deprecated flag
- MUST support JSON Schema for argument validation
- MUST support progress tokens

**Tool Operations:**
- tools/list - List available tools
- tools/call - Execute tool with arguments
- tools/list_changed - Notification when list changes (rate limited)

**Tool Result Content:**
- MUST support content types: text, image, audio, video
- MUST validate content structure (type, text/data, mimeType)
- MUST support embedded resources

### 2.4 Prompt System Requirements

**Prompt Management:**
- MUST support prompt registration with handler functions
- MUST support prompt arguments (name, description, required flag)
- MUST validate required arguments
- MUST support optional arguments with defaults
- MUST support JSON Schema for argument validation

**Prompt Operations:**
- prompts/list - List available prompts
- prompts/get - Get prompt with arguments
- prompts/list_changed - Notification when list changes

### 2.5 JSON-RPC 2.0 Requirements

**Message Format:**
- MUST include jsonrpc = "2.0"
- MUST include id for requests (string or integer)
- MUST include method for requests/notifications
- MUST include params (object or array, optional)
- MUST include result XOR error for responses

**Error Codes:**
- Standard: -32700 (parse), -32600 (invalid request), -32601 (method not found), -32602 (invalid params), -32603 (internal error)
- MCP-specific: -32001 to -32099 (see erlmcp.hrl for full list)

### 2.6 Error Handling Requirements

**Error Response Structure:**
```erlang
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => RequestId,
    <<"error">> => #{
        <<"code">> => ErrorCode,
        <<"message">> => ErrorMessage,
        <<"data">> => OptionalData  % Optional
    }
}
```

**Required Error Codes:**
- ?MCP_ERROR_NOT_INITIALIZED (-32005) - Request before initialize
- ?MCP_ERROR_RESOURCE_NOT_FOUND (-32001) - Resource URI not found
- ?MCP_ERROR_TOOL_NOT_FOUND (-32002) - Tool name not found
- ?MCP_ERROR_PROMPT_NOT_FOUND (-32003) - Prompt name not found
- ?MCP_ERROR_VALIDATION_FAILED (-32007) - Schema validation failed
- ?MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG (-32011) - Description >10000 chars

---

## 3. Test Suite Specifications

### 3.1 Test Suite: erlmcp_server_initialization_tests.erl (EUnit)

**Objective:** Validate server initialization state machine and protocol handshake.

**Test Scenarios (25 tests):**

```erlang
%% Lifecycle Tests
✅ server_start_link_success_test/0
✅ server_start_link_with_map_config_test/0
✅ server_stop_normal_test/0
✅ server_stop_with_active_resources_test/0
✅ server_stop_with_active_subscribers_test/0
✅ server_trap_exit_flag_test/0
✅ multiple_servers_independent_state_test/0

%% Initialize Handshake Tests (P0 Security)
✅ initialize_first_request_success_test/0
✅ initialize_response_structure_test/0
✅ initialize_protocol_version_validation_test/0
✅ initialize_capability_negotiation_test/0
✅ reject_double_initialize_test/0               % MUST return error
✅ reject_non_initialize_before_init_test/0      % MUST return ?MCP_ERROR_NOT_INITIALIZED
✅ initialized_flag_set_correctly_test/0
✅ phase_transition_init_to_ready_test/0

%% Capability Advertising Tests
✅ advertise_resources_capability_test/0
✅ advertise_tools_capability_test/0
✅ advertise_prompts_capability_test/0
✅ advertise_logging_capability_test/0
✅ advertise_sampling_capability_test/0          % Optional
✅ advertise_roots_capability_test/0             % Optional
✅ experimental_capability_passthrough_test/0

%% Client Capability Extraction
✅ extract_client_capabilities_full_test/0
✅ extract_client_capabilities_minimal_test/0
✅ store_client_capabilities_in_state_test/0
```

**Quality Gates:**
- Coverage: ≥90% for erlmcp_server:init/1, handle_call(initialize)
- All tests MUST pass
- No mocks - use real gen_server

---

### 3.2 Test Suite: erlmcp_server_tools_tests.erl (EUnit)

**Objective:** Validate tool execution, schema validation, and metadata handling.

**Test Scenarios (40 tests):**

```erlang
%% Tool Registration Tests
✅ add_tool_simple_test/0
✅ add_tool_with_description_test/0
✅ add_tool_with_schema_test/0
✅ add_tool_full_metadata_test/0
✅ add_tool_deprecated_flag_test/0
✅ add_tool_description_length_validation_test/0  % MUST reject >10000 chars
✅ add_tool_overwrite_existing_test/0
✅ add_tool_list_changed_notification_test/0
✅ add_tool_invalid_name_rejected_test/0
✅ add_tool_handler_not_function_test/0

%% Tool Invocation Tests
✅ tools_call_success_test/0
✅ tools_call_with_arguments_test/0
✅ tools_call_missing_name_error_test/0          % Return ?MCP_ERROR_TOOL_NOT_FOUND
✅ tools_call_tool_not_found_error_test/0
✅ tools_call_handler_crash_caught_test/0
✅ tools_call_progress_token_included_test/0
✅ tools_call_timeout_enforced_test/0
✅ tools_call_returns_content_types_test/0       % text, image, audio, video

%% Tool Schema Validation Tests
✅ tool_validate_arguments_valid_test/0
✅ tool_validate_arguments_missing_required_test/0
✅ tool_validate_arguments_wrong_type_test/0
✅ tool_validate_arguments_nested_schema_test/0
✅ tool_validate_arguments_array_schema_test/0
✅ tool_validate_arguments_enum_schema_test/0
✅ tool_validate_no_schema_passes_test/0

%% Tool Error Handling Tests
✅ tool_error_response_internal_error_test/0     % -32603
✅ tool_error_response_validation_error_test/0   % -32007
✅ tool_error_handler_exception_logged_test/0
✅ tool_error_timeout_error_test/0               % -32032

%% Tool Content Type Tests (NEW for 2025-11-25)
✅ tool_result_text_content_test/0
✅ tool_result_image_content_test/0
✅ tool_result_audio_content_test/0
✅ tool_result_embedded_resource_test/0
✅ tool_result_multiple_content_test/0

%% Tool List Changed Notification Tests (NEW)
✅ tools_list_changed_notification_sent_test/0
✅ tools_list_changed_rate_limiting_test/0       % Rate limited notifications
✅ tools_list_changed_multiple_subscribers_test/0

%% Tool Metadata Tests (NEW)
✅ tools_list_includes_description_test/0
✅ tools_list_includes_deprecated_flag_test/0
✅ tools_list_includes_schema_test/0
```

**Quality Gates:**
- Coverage: ≥85% for erlmcp_server:add_tool*, handle_call(tools/call)
- All tests MUST pass
- Schema validation MUST reject invalid inputs
- Description length MUST be enforced (≤10000)

---

### 3.3 Test Suite: erlmcp_server_resources_tests.erl (EUnit)

**Objective:** Validate resource management, subscriptions, and URI validation.

**Test Scenarios (45 tests):**

```erlang
%% Resource Registration Tests
✅ add_resource_simple_test/0
✅ add_resource_template_test/0
✅ add_resource_uri_validation_test/0            % RFC 3986
✅ add_resource_rejects_traversal_attacks_test/0 % ../ attacks
✅ add_resource_canonicalization_test/0
✅ add_resource_overwrite_existing_test/0
✅ add_resource_list_changed_notification_test/0
✅ add_resource_metadata_validation_test/0       % name, description, mime_type

%% Resource Reading Tests
✅ resources_read_success_test/0
✅ resources_read_uri_not_found_error_test/0     % ?MCP_ERROR_RESOURCE_NOT_FOUND
✅ resources_read_missing_uri_parameter_test/0   % ?JSONRPC_INVALID_PARAMS
✅ resources_read_binary_content_test/0
✅ resources_read_text_content_test/0
✅ resources_read_mcp_content_structure_test/0
✅ resources_read_annotations_test/0
✅ resources_read_resource_link_test/0
✅ resources_read_handler_crash_caught_test/0

%% Resource Subscription Tests (P0)
✅ resources_subscribe_success_test/0
✅ resources_subscribe_multiple_test/0
✅ resources_unsubscribe_success_test/0
✅ resources_unsubscribe_not_subscribed_test/0
✅ resources_updated_notification_sent_test/0
✅ resources_updated_all_subscribers_test/0
✅ resources_subscribe_process_death_cleanup_test/0  % Monitor subscriber

%% Resource Template Tests
✅ resource_template_uri_pattern_matching_test/0
✅ resource_template_variable_extraction_test/0
✅ resource_template_handler_receives_uri_test/0
✅ resource_template_not_found_error_test/0

%% Resource List Tests
✅ resources_list_returns_all_test/0
✅ resources_list_includes_templates_test/0
✅ resources_list_includes_metadata_test/0
✅ resources_list_pagination_test/0              % cursor-based

%% Resource URI Validation Tests (NEW for 2025-11-25)
✅ uri_validator_rfc3986_compliance_test/0
✅ uri_validator_rejects_invalid_scheme_test/0
✅ uri_validator_rejects_empty_uri_test/0
✅ uri_validator_accepts_valid_uris_test/0

%% Path Canonicalization Tests (NEW)
✅ path_canonicalizer_removes_dot_segments_test/0
✅ path_canonicalizer_rejects_traversal_test/0   % ../../../etc/passwd
✅ path_canonicalizer_normalizes_slashes_test/0
✅ path_canonicalizer_preserves_valid_paths_test/0

%% Resource Change Notification Tests
✅ notify_resource_updated_sends_notification_test/0
✅ notify_resources_changed_sends_notification_test/0
✅ resource_notification_multiple_subscribers_test/0
✅ resource_notification_subscriber_death_test/0
```

**Quality Gates:**
- Coverage: ≥85% for erlmcp_server:add_resource*, handle_call(resources/*)
- All tests MUST pass
- URI validation MUST reject invalid URIs
- Path traversal MUST be prevented

---

### 3.4 Test Suite: erlmcp_server_prompts_tests.erl (EUnit)

**Objective:** Validate prompt management and argument validation.

**Test Scenarios (30 tests):**

```erlang
%% Prompt Registration Tests
✅ add_prompt_simple_test/0
✅ add_prompt_with_args_test/0
✅ add_prompt_with_args_and_schema_test/0
✅ add_prompt_overwrite_existing_test/0
✅ add_prompt_list_changed_notification_test/0

%% Prompt Retrieval Tests
✅ prompts_get_success_test/0
✅ prompts_get_not_found_error_test/0            % ?MCP_ERROR_PROMPT_NOT_FOUND
✅ prompts_get_with_arguments_test/0
✅ prompts_get_missing_required_argument_test/0  % ?MCP_ERROR_PROMPT_ARGUMENT_MISSING
✅ prompts_get_handler_crash_caught_test/0

%% Prompt Argument Validation Tests
✅ prompt_validate_required_arguments_test/0
✅ prompt_validate_optional_arguments_test/0
✅ prompt_validate_argument_types_test/0
✅ prompt_validate_schema_compliance_test/0
✅ prompt_validate_missing_required_error_test/0

%% Prompt List Tests
✅ prompts_list_returns_all_test/0
✅ prompts_list_includes_arguments_test/0
✅ prompts_list_includes_metadata_test/0

%% Prompt Message Format Tests
✅ prompt_returns_message_array_test/0
✅ prompt_message_has_role_field_test/0          % system, user, assistant
✅ prompt_message_has_content_field_test/0
✅ prompt_multiple_messages_test/0

%% Prompt Error Handling Tests
✅ prompt_error_response_internal_error_test/0
✅ prompt_error_response_validation_error_test/0
✅ prompt_error_handler_exception_logged_test/0

%% Prompt Notification Tests
✅ prompts_list_changed_notification_sent_test/0
✅ prompts_list_changed_multiple_subscribers_test/0

%% Prompt Schema Validation Tests (NEW)
✅ prompt_schema_validation_enabled_test/0
✅ prompt_schema_validation_rejects_invalid_test/0
✅ prompt_schema_validation_optional_test/0
```

**Quality Gates:**
- Coverage: ≥85% for erlmcp_server:add_prompt*, handle_call(prompts/*)
- All tests MUST pass
- Required arguments MUST be enforced
- Schema validation MUST reject invalid arguments

---

### 3.5 Test Suite: erlmcp_server_state_machine_tests.erl (EUnit)

**Objective:** Validate protocol state machine and phase transitions.

**Test Scenarios (20 tests):**

```erlang
%% State Machine Phase Tests
✅ phase_initialization_initial_state_test/0
✅ phase_transition_init_to_initialized_test/0
✅ phase_transition_initialized_to_ready_test/0
✅ phase_transition_ready_to_shutdown_test/0
✅ phase_invalid_transition_rejected_test/0

%% Pre-Initialization Tests (P0 Security)
✅ reject_resources_list_before_init_test/0      % ?MCP_ERROR_NOT_INITIALIZED
✅ reject_tools_call_before_init_test/0
✅ reject_prompts_get_before_init_test/0
✅ reject_resources_read_before_init_test/0
✅ reject_any_non_init_method_test/0

%% Post-Initialization Tests
✅ accept_resources_list_after_init_test/0
✅ accept_tools_call_after_init_test/0
✅ accept_prompts_get_after_init_test/0

%% Double Initialize Tests
✅ reject_double_initialize_test/0               % Return error, don't crash
✅ double_initialize_returns_error_code_test/0

%% Shutdown Tests
✅ graceful_shutdown_cleanup_test/0
✅ graceful_shutdown_notifies_subscribers_test/0
✅ graceful_shutdown_stops_handlers_test/0
✅ graceful_shutdown_releases_resources_test/0

%% Timeout Tests (NEW)
✅ initialize_timeout_enforced_test/0            % Default 10s timeout
```

**Quality Gates:**
- Coverage: ≥90% for state machine logic
- All tests MUST pass
- Pre-init requests MUST be rejected
- State transitions MUST be validated

---

### 3.6 Test Suite: erlmcp_json_rpc_compliance_tests.erl (EUnit)

**Objective:** Validate JSON-RPC 2.0 compliance and message formatting.

**Test Scenarios (50 tests):**

```erlang
%% Request Format Tests
✅ request_has_jsonrpc_2_0_test/0
✅ request_has_id_field_test/0
✅ request_has_method_field_test/0
✅ request_params_optional_test/0
✅ request_id_can_be_string_test/0
✅ request_id_can_be_integer_test/0
✅ request_id_can_be_null_test/0

%% Response Format Tests
✅ response_has_jsonrpc_2_0_test/0
✅ response_has_id_field_test/0
✅ response_has_result_or_error_test/0           % MUST have exactly one
✅ response_result_and_error_mutually_exclusive_test/0

%% Notification Format Tests
✅ notification_has_jsonrpc_2_0_test/0
✅ notification_has_method_field_test/0
✅ notification_has_no_id_field_test/0
✅ notification_params_optional_test/0

%% Error Response Format Tests
✅ error_has_code_field_test/0
✅ error_has_message_field_test/0
✅ error_data_field_optional_test/0
✅ error_code_is_integer_test/0
✅ error_message_is_string_test/0

%% Standard Error Code Tests
✅ error_code_parse_error_test/0                 % -32700
✅ error_code_invalid_request_test/0             % -32600
✅ error_code_method_not_found_test/0            % -32601
✅ error_code_invalid_params_test/0              % -32602
✅ error_code_internal_error_test/0              % -32603

%% MCP Error Code Tests (NEW for 2025-11-25)
✅ mcp_error_not_initialized_test/0              % -32005
✅ mcp_error_resource_not_found_test/0           % -32001
✅ mcp_error_tool_not_found_test/0               % -32002
✅ mcp_error_prompt_not_found_test/0             % -32003
✅ mcp_error_validation_failed_test/0            % -32007
✅ mcp_error_tool_description_too_long_test/0    % -32011

%% Batch Request Tests
✅ batch_request_is_array_test/0
✅ batch_request_multiple_requests_test/0
✅ batch_response_is_array_test/0
✅ batch_response_preserves_order_test/0
✅ batch_response_includes_all_ids_test/0

%% Invalid Message Tests
✅ invalid_json_returns_parse_error_test/0
✅ missing_jsonrpc_field_returns_error_test/0
✅ wrong_jsonrpc_version_returns_error_test/0
✅ missing_method_field_returns_error_test/0
✅ invalid_id_type_returns_error_test/0

%% Encoding Tests
✅ encode_request_utf8_test/0
✅ encode_response_utf8_test/0
✅ encode_notification_utf8_test/0
✅ encode_error_utf8_test/0

%% Decoding Tests
✅ decode_valid_request_test/0
✅ decode_valid_response_test/0
✅ decode_valid_notification_test/0
✅ decode_invalid_json_test/0
✅ decode_malformed_utf8_test/0
```

**Quality Gates:**
- Coverage: ≥95% for erlmcp_json_rpc module
- All tests MUST pass
- Error codes MUST match JSON-RPC 2.0 and MCP spec
- Message structure MUST be validated

---

### 3.7 Test Suite: erlmcp_capabilities_negotiation_tests.erl (EUnit)

**Objective:** Validate capability negotiation and feature flag handling.

**Test Scenarios (25 tests):**

```erlang
%% Server Capability Advertising Tests
✅ server_capabilities_resources_enabled_test/0
✅ server_capabilities_tools_enabled_test/0
✅ server_capabilities_prompts_enabled_test/0
✅ server_capabilities_logging_enabled_test/0
✅ server_capabilities_all_disabled_test/0       % Minimal server
✅ server_capabilities_selective_enabled_test/0

%% Client Capability Extraction Tests
✅ client_capabilities_roots_enabled_test/0
✅ client_capabilities_sampling_enabled_test/0
✅ client_capabilities_all_disabled_test/0
✅ client_capabilities_missing_fields_test/0     % Default to disabled

%% Capability Negotiation Tests
✅ negotiate_capabilities_full_test/0
✅ negotiate_capabilities_partial_test/0
✅ negotiate_capabilities_mismatch_test/0
✅ negotiate_capabilities_store_in_state_test/0

%% Experimental Capability Tests
✅ experimental_capability_passthrough_test/0
✅ experimental_capability_ignored_test/0
✅ experimental_capability_in_response_test/0

%% Capability Change Notification Tests (NEW)
✅ capability_list_changed_notification_test/0
✅ tools_list_changed_notification_test/0
✅ resources_list_changed_notification_test/0
✅ prompts_list_changed_notification_test/0

%% Capability Validation Tests
✅ validate_capability_structure_test/0
✅ validate_capability_enabled_flag_test/0
✅ validate_capability_optional_fields_test/0
✅ reject_invalid_capability_format_test/0
```

**Quality Gates:**
- Coverage: ≥85% for erlmcp_capabilities module
- All tests MUST pass
- Capability flags MUST be validated
- Experimental capabilities MUST be preserved

---

### 3.8 Test Suite: erlmcp_notification_handler_integration_tests.erl (EUnit)

**Objective:** Validate notification handler registration and lifecycle.

**Test Scenarios (20 tests):**

```erlang
%% Notification Handler Registration Tests (NEW for 2025-11-25)
✅ register_notification_handler_success_test/0
✅ register_notification_handler_replaces_existing_test/0
✅ unregister_notification_handler_success_test/0
✅ unregister_all_handlers_test/0

%% Notification Handler Invocation Tests
✅ handler_receives_tools_list_changed_test/0
✅ handler_receives_resources_list_changed_test/0
✅ handler_receives_prompts_list_changed_test/0
✅ handler_receives_resources_updated_test/0

%% Notification Handler Monitoring Tests
✅ handler_death_cleanup_test/0                  % Handler crashes, auto-cleanup
✅ handler_monitor_ref_stored_test/0

%% Notification Handler Error Tests
✅ handler_crash_doesnt_crash_server_test/0
✅ handler_timeout_handled_test/0
✅ handler_invalid_response_logged_test/0

%% Notification Rate Limiting Tests (NEW)
✅ tools_list_changed_rate_limited_test/0        % Rate limited to prevent spam
✅ rate_limit_respects_interval_test/0
✅ rate_limit_resets_after_interval_test/0

%% Multiple Handler Tests
✅ multiple_handlers_all_receive_test/0
✅ multiple_handlers_independent_test/0
✅ multiple_handlers_selective_test/0           % Different methods

%% Handler Lifecycle Tests
✅ handler_cleanup_on_server_shutdown_test/0
```

**Quality Gates:**
- Coverage: ≥85% for notification handler logic
- All tests MUST pass
- Handler crashes MUST NOT crash server
- Rate limiting MUST be enforced

---

### 3.9 Test Suite: erlmcp_progress_tracking_tests.erl (EUnit)

**Objective:** Validate progress token generation and tracking.

**Test Scenarios (15 tests):**

```erlang
%% Progress Token Generation Tests (NEW)
✅ generate_progress_token_unique_test/0
✅ generate_progress_token_format_test/0         % String or integer
✅ progress_token_stored_in_state_test/0

%% Progress Reporting Tests
✅ report_progress_sends_notification_test/0
✅ report_progress_includes_token_test/0
✅ report_progress_includes_progress_value_test/0 % 0-100
✅ report_progress_includes_total_test/0

%% Progress Token Lifecycle Tests
✅ progress_token_cleanup_after_completion_test/0
✅ progress_token_cleanup_on_error_test/0
✅ progress_token_multiple_active_test/0

%% Progress Notification Format Tests
✅ progress_notification_structure_test/0
✅ progress_notification_method_test/0           % notifications/progress
✅ progress_notification_params_test/0

%% Progress Error Handling Tests
✅ progress_invalid_token_error_test/0
✅ progress_out_of_range_value_error_test/0     % <0 or >100
```

**Quality Gates:**
- Coverage: ≥85% for progress tracking logic
- All tests MUST pass
- Progress tokens MUST be unique
- Cleanup MUST occur after completion

---

### 3.10 Test Suite: erlmcp_uri_validator_tests.erl (EUnit)

**Objective:** Validate URI validation (RFC 3986) and security.

**Test Scenarios (25 tests):**

```erlang
%% URI Validation Tests (NEW for 2025-11-25)
✅ uri_valid_http_test/0
✅ uri_valid_https_test/0
✅ uri_valid_file_test/0
✅ uri_valid_custom_scheme_test/0               % weather://, doc://
✅ uri_invalid_empty_test/0
✅ uri_invalid_no_scheme_test/0
✅ uri_invalid_malformed_test/0

%% URI Component Tests
✅ uri_parse_scheme_test/0
✅ uri_parse_authority_test/0
✅ uri_parse_path_test/0
✅ uri_parse_query_test/0
✅ uri_parse_fragment_test/0

%% URI Security Tests
✅ uri_rejects_file_traversal_test/0            % file://../../../etc/passwd
✅ uri_rejects_null_bytes_test/0
✅ uri_rejects_command_injection_test/0
✅ uri_rejects_oversized_uri_test/0             % >2048 chars

%% URI Normalization Tests
✅ uri_normalize_percent_encoding_test/0
✅ uri_normalize_case_test/0
✅ uri_normalize_empty_path_test/0

%% URI Template Tests
✅ uri_template_match_test/0                     % user://{username}/profile
✅ uri_template_extract_variables_test/0
✅ uri_template_invalid_pattern_test/0

%% Path Canonicalization Tests
✅ path_remove_dot_segments_test/0              % /a/./b -> /a/b
✅ path_remove_double_dot_segments_test/0       % /a/../b -> /b
✅ path_reject_traversal_test/0                 % /../../../etc/passwd
```

**Quality Gates:**
- Coverage: ≥95% for erlmcp_uri_validator module
- All tests MUST pass
- Security vulnerabilities MUST be rejected
- RFC 3986 compliance MUST be validated

---

### 3.11 Test Suite: erlmcp_content_type_validation_tests.erl (EUnit)

**Objective:** Validate content type validation for tool results.

**Test Scenarios (20 tests):**

```erlang
%% Content Type Validation Tests (NEW for 2025-11-25)
✅ content_type_text_valid_test/0
✅ content_type_image_valid_test/0
✅ content_type_audio_valid_test/0
✅ content_type_video_valid_test/0
✅ content_type_invalid_type_test/0

%% Text Content Tests
✅ text_content_has_text_field_test/0
✅ text_content_has_type_field_test/0
✅ text_content_optional_mime_type_test/0
✅ text_content_utf8_encoding_test/0

%% Image Content Tests
✅ image_content_has_data_field_test/0
✅ image_content_has_mime_type_test/0            % image/png, image/jpeg
✅ image_content_base64_encoded_test/0

%% Audio Content Tests
✅ audio_content_has_data_field_test/0
✅ audio_content_has_mime_type_test/0            % audio/wav, audio/mp3

%% Embedded Resource Tests
✅ embedded_resource_has_resource_field_test/0
✅ embedded_resource_has_uri_test/0
✅ embedded_resource_valid_structure_test/0

%% Multiple Content Tests
✅ multiple_content_array_test/0
✅ multiple_content_mixed_types_test/0
✅ multiple_content_preserves_order_test/0
```

**Quality Gates:**
- Coverage: ≥85% for content type validation logic
- All tests MUST pass
- Content structure MUST be validated
- MIME types MUST be validated

---

### 3.12 Test Suite: erlmcp_error_code_compliance_tests.erl (EUnit)

**Objective:** Validate error code compliance with JSON-RPC 2.0 and MCP spec.

**Test Scenarios (30 tests):**

```erlang
%% JSON-RPC Standard Error Codes
✅ error_parse_error_code_test/0                 % -32700
✅ error_invalid_request_code_test/0             % -32600
✅ error_method_not_found_code_test/0            % -32601
✅ error_invalid_params_code_test/0              % -32602
✅ error_internal_error_code_test/0              % -32603

%% MCP Core Error Codes
✅ mcp_error_resource_not_found_test/0           % -32001
✅ mcp_error_tool_not_found_test/0               % -32002
✅ mcp_error_prompt_not_found_test/0             % -32003
✅ mcp_error_not_initialized_test/0              % -32005
✅ mcp_error_validation_failed_test/0            % -32007

%% MCP Content Error Codes
✅ mcp_error_tool_description_too_long_test/0    % -32011
✅ mcp_error_message_too_large_test/0            % -32012
✅ mcp_error_invalid_content_type_test/0         % -32013

%% MCP Resource Error Codes
✅ mcp_error_invalid_uri_test/0                  % -32022
✅ mcp_error_resource_access_denied_test/0       % -32025

%% MCP Tool Error Codes
✅ mcp_error_tool_execution_failed_test/0        % -32031
✅ mcp_error_tool_timeout_test/0                 % -32032
✅ mcp_error_invalid_tool_arguments_test/0       % -32034

%% MCP Prompt Error Codes
✅ mcp_error_prompt_argument_missing_test/0      % -32043
✅ mcp_error_invalid_prompt_arguments_test/0     % -32045

%% Error Message Format Tests
✅ error_message_not_empty_test/0
✅ error_message_descriptive_test/0
✅ error_data_field_optional_test/0
✅ error_data_includes_context_test/0            % Stack trace, details

%% Error Response Structure Tests
✅ error_response_has_error_object_test/0
✅ error_response_no_result_field_test/0         % Mutually exclusive
✅ error_response_preserves_id_test/0
✅ error_response_valid_json_rpc_test/0

%% Error Code Range Tests
✅ error_code_in_valid_range_test/0              % -32768 to -32000
✅ error_code_not_reserved_test/0                % Avoid -32768 to -32000 for custom
```

**Quality Gates:**
- Coverage: ≥90% for error handling logic
- All tests MUST pass
- Error codes MUST match specification
- Error messages MUST be descriptive

---

### 3.13 Test Suite: erlmcp_backwards_compatibility_tests.erl (EUnit)

**Objective:** Validate backwards compatibility with older MCP versions.

**Test Scenarios (15 tests):**

```erlang
%% Protocol Version Tests
✅ protocol_version_2025_11_25_test/0
✅ protocol_version_2024_11_05_test/0            % Older version
✅ protocol_version_unknown_rejected_test/0
✅ protocol_version_negotiation_test/0

%% Deprecated Feature Tests
✅ deprecated_tool_marked_test/0
✅ deprecated_tool_still_callable_test/0
✅ deprecated_tool_warning_logged_test/0

%% Feature Flag Tests
✅ feature_flag_enabled_test/0
✅ feature_flag_disabled_test/0
✅ feature_flag_unknown_ignored_test/0

%% Legacy Message Format Tests
✅ legacy_message_format_accepted_test/0
✅ legacy_message_format_converted_test/0
✅ legacy_message_format_warned_test/0

%% Migration Path Tests
✅ migration_from_old_version_test/0
✅ migration_preserves_state_test/0
```

**Quality Gates:**
- Coverage: ≥80% for backwards compatibility logic
- All tests MUST pass
- Older protocol versions MUST be handled gracefully
- Deprecated features MUST be marked

---

### 3.14 Test Suite: erlmcp_integration_compliance_SUITE.erl (Common Test)

**Objective:** End-to-end integration testing for MCP 2025-11-25 compliance.

**Test Scenarios (25 tests):**

```erlang
%% Full Protocol Flow Tests (Chicago School: real client + server)
✅ full_initialize_handshake_test/1
✅ full_capability_negotiation_test/1
✅ full_resource_lifecycle_test/1
✅ full_tool_execution_test/1
✅ full_prompt_retrieval_test/1

%% Multi-Process Coordination Tests
✅ multiple_clients_concurrent_test/1            % 100 clients
✅ multiple_servers_independent_test/1
✅ client_server_bidirectional_test/1

%% Subscription and Notification Tests
✅ resource_subscribe_notify_test/1
✅ tools_list_changed_notify_test/1
✅ multiple_subscribers_test/1

%% Error Recovery Tests
✅ client_crash_recovery_test/1
✅ server_crash_recovery_test/1
✅ network_partition_recovery_test/1

%% Performance Tests
✅ throughput_100k_requests_test/1
✅ latency_p99_under_100ms_test/1
✅ concurrent_1000_clients_test/1

%% State Consistency Tests
✅ state_consistency_after_error_test/1
✅ state_consistency_after_restart_test/1
✅ state_consistency_distributed_test/1

%% Compliance Tests
✅ mcp_spec_compliance_full_test/1
✅ json_rpc_compliance_full_test/1
✅ capability_negotiation_compliance_test/1

%% Security Tests
✅ reject_unauthorized_access_test/1
✅ reject_path_traversal_test/1
```

**Quality Gates:**
- All tests MUST pass
- P99 latency: <100ms
- Throughput: >10K req/s
- No crashes under normal load
- No memory leaks

---

### 3.15 Test Suite: erlmcp_property_tests.erl (Proper)

**Objective:** Property-based testing for protocol invariants.

**Test Scenarios (20 properties):**

```erlang
%% JSON-RPC Encoding/Decoding Properties
✅ prop_encode_decode_roundtrip/0
✅ prop_request_always_has_id/0
✅ prop_notification_never_has_id/0
✅ prop_error_code_in_range/0

%% Capability Negotiation Properties
✅ prop_capabilities_always_valid_structure/0
✅ prop_capability_flags_boolean/0

%% Resource URI Properties
✅ prop_uri_validation_idempotent/0
✅ prop_uri_canonicalization_stable/0
✅ prop_path_traversal_always_rejected/0

%% Tool Validation Properties
✅ prop_tool_description_length_enforced/0
✅ prop_tool_schema_validation_consistent/0

%% Progress Token Properties
✅ prop_progress_token_unique/0
✅ prop_progress_value_in_range/0               % 0-100

%% Error Code Properties
✅ prop_error_code_valid_range/0
✅ prop_error_message_not_empty/0

%% State Machine Properties
✅ prop_initialize_always_first/0
✅ prop_pre_init_requests_rejected/0

%% Content Type Properties
✅ prop_content_type_valid/0
✅ prop_mime_type_format_valid/0
```

**Quality Gates:**
- All properties MUST pass 1000 cases
- Shrinking MUST find minimal failing case
- No false positives

---

## 4. Test Execution Strategy

### 4.1 Test Phases

**Phase 1: Foundation (Weeks 1-2)**
- Priority: P0 tests (critical security and correctness)
- Modules:
  - erlmcp_server_initialization_tests.erl
  - erlmcp_server_state_machine_tests.erl
  - erlmcp_json_rpc_compliance_tests.erl
- Target Coverage: ≥60%

**Phase 2: Core Functionality (Weeks 3-4)**
- Priority: P0 + P1 tests (core features)
- Modules:
  - erlmcp_server_tools_tests.erl
  - erlmcp_server_resources_tests.erl
  - erlmcp_server_prompts_tests.erl
  - erlmcp_capabilities_negotiation_tests.erl
- Target Coverage: ≥75%

**Phase 3: Advanced Features (Weeks 5-6)**
- Priority: P1 + P2 tests (advanced features)
- Modules:
  - erlmcp_notification_handler_integration_tests.erl
  - erlmcp_progress_tracking_tests.erl
  - erlmcp_uri_validator_tests.erl
  - erlmcp_content_type_validation_tests.erl
  - erlmcp_error_code_compliance_tests.erl
- Target Coverage: ≥80%

**Phase 4: Integration and Polish (Weeks 7-8)**
- Priority: Integration and property tests
- Modules:
  - erlmcp_integration_compliance_SUITE.erl
  - erlmcp_property_tests.erl
  - erlmcp_backwards_compatibility_tests.erl
- Target Coverage: ≥85%

### 4.2 Test Execution Commands

**Run All EUnit Tests:**
```bash
make test-core
# OR
rebar3 eunit --app=erlmcp_core
```

**Run Specific Test Module:**
```bash
rebar3 eunit --module=erlmcp_server_initialization_tests
```

**Run Common Test Suites:**
```bash
rebar3 ct --suite=erlmcp_integration_compliance_SUITE
```

**Run Property Tests:**
```bash
rebar3 proper --module=erlmcp_property_tests --numtests=1000
```

**Run with Coverage:**
```bash
rebar3 eunit --cover
rebar3 cover --verbose
```

**Generate Coverage Report:**
```bash
rebar3 cover
open _build/test/cover/index.html
```

### 4.3 Quality Gates (Pre-Completion Verification)

**Before marking ANY test suite as complete, MUST execute:**

1. **Compilation:**
   ```bash
   TERM=dumb make compile
   # Expected: 0 errors, 0 warnings
   ```

2. **Tests:**
   ```bash
   rebar3 eunit --module=<module>_tests
   # Expected: 100% pass rate (0 failures)
   ```

3. **Coverage:**
   ```bash
   rebar3 cover --verbose
   # Expected: ≥80% overall, ≥85% for core modules
   ```

4. **Dialyzer:**
   ```bash
   rebar3 dialyzer
   # Expected: 0 type warnings
   ```

5. **Xref:**
   ```bash
   rebar3 xref
   # Expected: 0 cross-reference issues
   ```

6. **Format:**
   ```bash
   rebar3 format --verify
   # Expected: All files formatted correctly
   ```

**Verification Report Format:**
```
✅ Tests: X/X passed (EUnit: Y, CT: Z, Proper: W)
✅ Quality: Compile clean, tests compile clean, format verified
✅ Coverage: X% overall (Core modules: Y%)
  - erlmcp_server: A%
  - erlmcp_json_rpc: B%
  - erlmcp_capabilities: C%
✅ Chicago School TDD: Real collaborators ✅, State-based assertions ✅, No mocks ✅
✅ Edge Cases: [list of edge cases covered]

Ready for review: [brief summary]
```

---

## 5. Coverage Targets by Module

### 5.1 Critical Modules (≥90% coverage)

```
erlmcp_json_rpc.erl         - 95% (JSON-RPC encoding/decoding)
erlmcp_uri_validator.erl    - 95% (URI validation, security)
erlmcp_path_canonicalizer.erl - 95% (Path security)
```

### 5.2 Core Modules (≥85% coverage)

```
erlmcp_server.erl           - 85% (Server state machine, handlers)
erlmcp_client.erl           - 85% (Client operations)
erlmcp_capabilities.erl     - 85% (Capability negotiation)
erlmcp_schema_validator.erl - 85% (Schema validation)
```

### 5.3 Supporting Modules (≥80% coverage)

```
erlmcp_registry.erl         - 80% (Process registry)
erlmcp_cache.erl            - 80% (Caching)
erlmcp_auth.erl             - 80% (Authentication)
erlmcp_rate_limiting.erl    - 80% (Rate limiting)
erlmcp_circuit_breaker.erl  - 80% (Circuit breaker)
```

### 5.4 Observability Modules (≥70% coverage)

```
erlmcp_logging.erl          - 70% (Logging)
erlmcp_connection_monitor.erl - 70% (Monitoring)
```

---

## 6. Test Implementation Guidelines

### 6.1 Chicago School TDD Principles

**DO:**
- ✅ Use real gen_servers (erlmcp_server:start_link/2)
- ✅ Use real processes for transports
- ✅ Assert on observable state (API results, message receipts)
- ✅ Test behaviors and outputs
- ✅ Integrate components together

**DON'T:**
- ❌ Use meck or other mocking frameworks
- ❌ Verify internal method calls
- ❌ Stub collaborators (use real implementations)
- ❌ Test implementation details

**Example (Good):**
```erlang
server_initialization_test() ->
    %% Setup: Start real server
    {ok, Server} = erlmcp_server:start_link(server1, #mcp_server_capabilities{}),

    %% Exercise: Send initialize request
    InitReq = build_initialize_request(),
    {ok, Response} = erlmcp_server:handle_request(Server, InitReq),

    %% Verify: Check observable state (Chicago School)
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assert(maps:is_key(<<"result">>, Response)),
    Result = maps:get(<<"result">>, Response),
    ?assert(maps:is_key(<<"capabilities">>, Result)),

    %% Verify server state changed (via API, not internal inspection)
    {ok, Initialized} = erlmcp_server:is_initialized(Server),
    ?assertEqual(true, Initialized),

    %% Cleanup
    erlmcp_server:stop(Server).
```

**Example (Bad - London School, NOT ALLOWED):**
```erlang
%% BAD: Uses meck, verifies internal calls
server_initialization_test() ->
    meck:new(erlmcp_capabilities),
    meck:expect(erlmcp_capabilities, negotiate, fun(...) -> ... end),

    erlmcp_server:initialize(...),

    %% BAD: Verifying internal method calls
    ?assert(meck:called(erlmcp_capabilities, negotiate, ['_', '_'])),

    meck:unload(erlmcp_capabilities).
```

### 6.2 Test Structure Pattern

```erlang
-module(erlmcp_<feature>_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Suite for <Feature>
%%%====================================================================
%%% Chicago School TDD:
%%% - Real processes (no mocks)
%%% - State-based verification
%%% - Behavior verification

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Start real gen_server or process
    {ok, Pid} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    Pid.

cleanup(Pid) ->
    %% Stop process
    erlmcp_server:stop(Pid),
    timer:sleep(50).  % Allow cleanup to complete

%%====================================================================
%% Test Cases
%%====================================================================

feature_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Server) ->
         [
          ?_test(test_scenario_1(Server)),
          ?_test(test_scenario_2(Server))
         ]
     end}.

test_scenario_1(Server) ->
    %% Exercise: Call API
    Result = erlmcp_server:some_operation(Server, Args),

    %% Verify: Check observable state (Chicago School)
    ?assertEqual(ExpectedState, Result),

    %% Verify: Check server state via API (not internal inspection)
    {ok, State} = erlmcp_server:get_state(Server),
    ?assertEqual(ExpectedValue, State).
```

### 6.3 Test Naming Conventions

```erlang
%% Pattern: <feature>_<scenario>_<expectation>_test/0

initialize_first_request_success_test/0
initialize_double_initialize_error_test/0
tools_call_missing_name_error_test/0
resources_read_uri_not_found_error_test/0
```

### 6.4 Assertion Patterns

```erlang
%% Equality
?assertEqual(Expected, Actual)

%% Pattern matching
?assertMatch({ok, _}, Result)

%% Boolean
?assert(Condition)
?assertNot(Condition)

%% Exceptions
?assertError(badarg, Function())
?assertThrow(Term, Function())

%% Custom assertions (in test helpers)
assert_server_initialized(Server)
assert_resource_exists(Server, Uri)
assert_error_code(Response, ExpectedCode)
```

---

## 7. Success Criteria

### 7.1 Quantitative Metrics

```
✅ Test Count: 300+ test cases implemented
✅ Coverage: ≥80% overall, ≥85% for core modules
✅ Pass Rate: 100% (0 failures)
✅ Quality: 0 compilation errors, 0 dialyzer warnings
✅ Performance: No regression (<10% degradation)
```

### 7.2 Qualitative Metrics

```
✅ Chicago School TDD: All tests use real processes, no mocks
✅ MCP Compliance: All 2025-11-25 features tested
✅ Error Handling: All error codes tested and validated
✅ Security: Path traversal, injection attacks tested
✅ Documentation: All test suites documented
```

### 7.3 Readiness Gates

**Gate 1: Foundation (Week 2)**
- Initialization tests pass: 25/25
- State machine tests pass: 20/20
- JSON-RPC tests pass: 50/50
- Coverage: ≥60%

**Gate 2: Core (Week 4)**
- Tools tests pass: 40/40
- Resources tests pass: 45/45
- Prompts tests pass: 30/30
- Capabilities tests pass: 25/25
- Coverage: ≥75%

**Gate 3: Advanced (Week 6)**
- Notification tests pass: 20/20
- Progress tests pass: 15/15
- URI validation tests pass: 25/25
- Content type tests pass: 20/20
- Error code tests pass: 30/30
- Coverage: ≥80%

**Gate 4: Production (Week 8)**
- Integration tests pass: 25/25
- Property tests pass: 20/20 (1000 cases each)
- Backwards compatibility tests pass: 15/15
- Coverage: ≥85%
- All quality gates pass

---

## 8. Next Steps

### 8.1 Immediate Actions (Week 1)

1. **Setup Test Infrastructure:**
   ```bash
   # Install dependencies
   rebar3 deps

   # Verify build
   make compile

   # Verify existing tests
   make test-core
   ```

2. **Create Test Helpers:**
   - Create `erlmcp_test_helpers.erl` with:
     - Server setup/teardown
     - Sample data builders
     - Custom assertions
     - Transport simulation

3. **Implement Priority 1 (P0) Tests:**
   - erlmcp_server_initialization_tests.erl (25 tests)
   - erlmcp_server_state_machine_tests.erl (20 tests)
   - Run and verify: `rebar3 eunit --module=erlmcp_server_initialization_tests`

### 8.2 Ongoing Actions (Weeks 2-8)

**Week 2:** Foundation
- Implement JSON-RPC compliance tests (50 tests)
- Target: 60% coverage

**Week 3-4:** Core Functionality
- Implement tools, resources, prompts tests (115 tests)
- Target: 75% coverage

**Week 5-6:** Advanced Features
- Implement notification, progress, URI, content type, error code tests (110 tests)
- Target: 80% coverage

**Week 7-8:** Integration and Polish
- Implement integration, property, backwards compatibility tests (60 tests)
- Target: 85% coverage
- Final quality gates

### 8.3 Continuous Actions

- Run tests after every code change
- Monitor coverage trends
- Update test plan as needed
- Document test failures and resolutions

---

## 9. Resources

**Documentation:**
- MCP Specification: docs/protocol.md
- Testing Strategy: docs/testing/MCP_SERVER_TESTING_STRATEGY.md
- Edge Cases: docs/mcp-edge-case-testing-strategy.md
- OTP Patterns: docs/otp-patterns.md

**Reference Implementation:**
- Server: apps/erlmcp_core/src/erlmcp_server.erl
- Client: apps/erlmcp_core/src/erlmcp_client.erl
- JSON-RPC: apps/erlmcp_core/src/erlmcp_json_rpc.erl
- Capabilities: apps/erlmcp_core/src/erlmcp_capabilities.erl

**Existing Tests:**
- apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl
- apps/erlmcp_core/test/erlmcp_client_tests.erl
- apps/erlmcp_core/test/erlmcp_resource_tests.erl

**Tools:**
- EUnit: http://erlang.org/doc/apps/eunit/chapter.html
- Common Test: http://erlang.org/doc/apps/common_test/chapter.html
- Proper: https://proper-testing.github.io/

---

## 10. Appendix: Test Case Index

### Index by Priority

**P0 (Critical Security and Correctness):**
- Server initialization state machine (25 tests)
- Protocol version validation (5 tests)
- Pre-init request rejection (5 tests)
- Tool description length validation (3 tests)
- URI validation and security (25 tests)
- Error code compliance (30 tests)

**P1 (Core Functionality):**
- Tool execution and validation (40 tests)
- Resource management (45 tests)
- Prompt handling (30 tests)
- Capability negotiation (25 tests)
- JSON-RPC compliance (50 tests)

**P2 (Advanced Features):**
- Notification handlers (20 tests)
- Progress tracking (15 tests)
- Content type validation (20 tests)
- Backwards compatibility (15 tests)

### Index by Module Under Test

```
erlmcp_server.erl:
  - erlmcp_server_initialization_tests.erl (25 tests)
  - erlmcp_server_state_machine_tests.erl (20 tests)
  - erlmcp_server_tools_tests.erl (40 tests)
  - erlmcp_server_resources_tests.erl (45 tests)
  - erlmcp_server_prompts_tests.erl (30 tests)

erlmcp_json_rpc.erl:
  - erlmcp_json_rpc_compliance_tests.erl (50 tests)

erlmcp_capabilities.erl:
  - erlmcp_capabilities_negotiation_tests.erl (25 tests)

erlmcp_uri_validator.erl:
  - erlmcp_uri_validator_tests.erl (25 tests)

...
```

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-30
**Status:** Strategic Planning
**Next Review:** After Phase 1 completion

---

**END OF TEST PLAN**
