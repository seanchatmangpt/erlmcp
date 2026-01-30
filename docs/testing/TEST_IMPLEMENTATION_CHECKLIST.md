# MCP 2025-11-25 Compliance Test Implementation Checklist

**Version:** 1.0.0
**Date:** 2026-01-30
**Status:** Active Tracking
**Target:** 300+ tests, ≥80% coverage

---

## Quick Status Dashboard

**Overall Progress:**
- [ ] Phase 1: Foundation (150 tests) - Target: Week 2
- [ ] Phase 2: Core (115 tests) - Target: Week 4
- [ ] Phase 3: Advanced (80 tests) - Target: Week 6
- [ ] Phase 4: Polish (60 tests) - Target: Week 8

**Coverage Progress:**
```
Current:  [          ] 0% → Target: 85%
Week 1:   [█         ] 10%
Week 2:   [████      ] 40%
Week 4:   [███████   ] 70%
Week 6:   [████████  ] 80%
Week 8:   [█████████ ] 85%
```

**Quality Gates:**
- [ ] 0 compilation errors
- [ ] 100% test pass rate
- [ ] ≥80% code coverage
- [ ] 0 dialyzer warnings
- [ ] 0 xref issues

---

## Phase 1: Foundation (Weeks 1-2) - P0 Security Tests

### Week 1: Server Initialization and State Machine

#### Test Suite 1: erlmcp_server_initialization_tests.erl (25 tests)

**File:** `apps/erlmcp_core/test/erlmcp_server_initialization_tests.erl`

**Lifecycle Tests (7):**
- [ ] server_start_link_success_test/0
- [ ] server_start_link_with_map_config_test/0
- [ ] server_stop_normal_test/0
- [ ] server_stop_with_active_resources_test/0
- [ ] server_stop_with_active_subscribers_test/0
- [ ] server_trap_exit_flag_test/0
- [ ] multiple_servers_independent_state_test/0

**Initialize Handshake Tests (8):**
- [ ] initialize_first_request_success_test/0
- [ ] initialize_response_structure_test/0
- [ ] initialize_protocol_version_validation_test/0
- [ ] initialize_capability_negotiation_test/0
- [ ] reject_double_initialize_test/0 **(P0 Critical)**
- [ ] reject_non_initialize_before_init_test/0 **(P0 Critical)**
- [ ] initialized_flag_set_correctly_test/0
- [ ] phase_transition_init_to_ready_test/0

**Capability Advertising Tests (7):**
- [ ] advertise_resources_capability_test/0
- [ ] advertise_tools_capability_test/0
- [ ] advertise_prompts_capability_test/0
- [ ] advertise_logging_capability_test/0
- [ ] advertise_sampling_capability_test/0
- [ ] advertise_roots_capability_test/0
- [ ] experimental_capability_passthrough_test/0

**Client Capability Tests (3):**
- [ ] extract_client_capabilities_full_test/0
- [ ] extract_client_capabilities_minimal_test/0
- [ ] store_client_capabilities_in_state_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_server_initialization_tests
rebar3 cover --verbose
# Target: 25/25 passed, ≥90% coverage for init logic
```

---

#### Test Suite 2: erlmcp_server_state_machine_tests.erl (20 tests)

**File:** `apps/erlmcp_core/test/erlmcp_server_state_machine_tests.erl`

**State Machine Phase Tests (5):**
- [ ] phase_initialization_initial_state_test/0
- [ ] phase_transition_init_to_initialized_test/0
- [ ] phase_transition_initialized_to_ready_test/0
- [ ] phase_transition_ready_to_shutdown_test/0
- [ ] phase_invalid_transition_rejected_test/0

**Pre-Initialization Tests (5) - P0 Critical:**
- [ ] reject_resources_list_before_init_test/0 **(P0)**
- [ ] reject_tools_call_before_init_test/0 **(P0)**
- [ ] reject_prompts_get_before_init_test/0 **(P0)**
- [ ] reject_resources_read_before_init_test/0 **(P0)**
- [ ] reject_any_non_init_method_test/0 **(P0)**

**Post-Initialization Tests (3):**
- [ ] accept_resources_list_after_init_test/0
- [ ] accept_tools_call_after_init_test/0
- [ ] accept_prompts_get_after_init_test/0

**Double Initialize Tests (2):**
- [ ] reject_double_initialize_test/0
- [ ] double_initialize_returns_error_code_test/0

**Shutdown Tests (4):**
- [ ] graceful_shutdown_cleanup_test/0
- [ ] graceful_shutdown_notifies_subscribers_test/0
- [ ] graceful_shutdown_stops_handlers_test/0
- [ ] graceful_shutdown_releases_resources_test/0

**Timeout Tests (1):**
- [ ] initialize_timeout_enforced_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_server_state_machine_tests
# Target: 20/20 passed, ≥90% coverage for state machine
```

**Week 1 Deliverable:**
- [ ] 45 tests implemented and passing
- [ ] Coverage: ≥60%
- [ ] All P0 security tests passing

---

### Week 2: JSON-RPC, URI Validation, Error Codes

#### Test Suite 3: erlmcp_json_rpc_compliance_tests.erl (50 tests)

**File:** `apps/erlmcp_core/test/erlmcp_json_rpc_compliance_tests.erl`

**Request Format Tests (7):**
- [ ] request_has_jsonrpc_2_0_test/0
- [ ] request_has_id_field_test/0
- [ ] request_has_method_field_test/0
- [ ] request_params_optional_test/0
- [ ] request_id_can_be_string_test/0
- [ ] request_id_can_be_integer_test/0
- [ ] request_id_can_be_null_test/0

**Response Format Tests (4):**
- [ ] response_has_jsonrpc_2_0_test/0
- [ ] response_has_id_field_test/0
- [ ] response_has_result_or_error_test/0
- [ ] response_result_and_error_mutually_exclusive_test/0

**Notification Format Tests (4):**
- [ ] notification_has_jsonrpc_2_0_test/0
- [ ] notification_has_method_field_test/0
- [ ] notification_has_no_id_field_test/0
- [ ] notification_params_optional_test/0

**Error Response Format Tests (5):**
- [ ] error_has_code_field_test/0
- [ ] error_has_message_field_test/0
- [ ] error_data_field_optional_test/0
- [ ] error_code_is_integer_test/0
- [ ] error_message_is_string_test/0

**Standard Error Code Tests (5):**
- [ ] error_code_parse_error_test/0 (-32700)
- [ ] error_code_invalid_request_test/0 (-32600)
- [ ] error_code_method_not_found_test/0 (-32601)
- [ ] error_code_invalid_params_test/0 (-32602)
- [ ] error_code_internal_error_test/0 (-32603)

**MCP Error Code Tests (6):**
- [ ] mcp_error_not_initialized_test/0 (-32005)
- [ ] mcp_error_resource_not_found_test/0 (-32001)
- [ ] mcp_error_tool_not_found_test/0 (-32002)
- [ ] mcp_error_prompt_not_found_test/0 (-32003)
- [ ] mcp_error_validation_failed_test/0 (-32007)
- [ ] mcp_error_tool_description_too_long_test/0 (-32011)

**Batch Request Tests (5):**
- [ ] batch_request_is_array_test/0
- [ ] batch_request_multiple_requests_test/0
- [ ] batch_response_is_array_test/0
- [ ] batch_response_preserves_order_test/0
- [ ] batch_response_includes_all_ids_test/0

**Invalid Message Tests (5):**
- [ ] invalid_json_returns_parse_error_test/0
- [ ] missing_jsonrpc_field_returns_error_test/0
- [ ] wrong_jsonrpc_version_returns_error_test/0
- [ ] missing_method_field_returns_error_test/0
- [ ] invalid_id_type_returns_error_test/0

**Encoding/Decoding Tests (9):**
- [ ] encode_request_utf8_test/0
- [ ] encode_response_utf8_test/0
- [ ] encode_notification_utf8_test/0
- [ ] encode_error_utf8_test/0
- [ ] decode_valid_request_test/0
- [ ] decode_valid_response_test/0
- [ ] decode_valid_notification_test/0
- [ ] decode_invalid_json_test/0
- [ ] decode_malformed_utf8_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_json_rpc_compliance_tests
# Target: 50/50 passed, ≥95% coverage for JSON-RPC
```

---

#### Test Suite 4: erlmcp_uri_validator_tests.erl (25 tests)

**File:** `apps/erlmcp_core/test/erlmcp_uri_validator_tests.erl`

**URI Validation Tests (7):**
- [ ] uri_valid_http_test/0
- [ ] uri_valid_https_test/0
- [ ] uri_valid_file_test/0
- [ ] uri_valid_custom_scheme_test/0
- [ ] uri_invalid_empty_test/0
- [ ] uri_invalid_no_scheme_test/0
- [ ] uri_invalid_malformed_test/0

**URI Component Tests (5):**
- [ ] uri_parse_scheme_test/0
- [ ] uri_parse_authority_test/0
- [ ] uri_parse_path_test/0
- [ ] uri_parse_query_test/0
- [ ] uri_parse_fragment_test/0

**URI Security Tests (6) - P0 Critical:**
- [ ] uri_rejects_file_traversal_test/0 **(P0)**
- [ ] uri_rejects_null_bytes_test/0 **(P0)**
- [ ] uri_rejects_command_injection_test/0 **(P0)**
- [ ] uri_rejects_oversized_uri_test/0 **(P0)**
- [ ] path_remove_dot_segments_test/0
- [ ] path_reject_traversal_test/0 **(P0)**

**URI Normalization Tests (3):**
- [ ] uri_normalize_percent_encoding_test/0
- [ ] uri_normalize_case_test/0
- [ ] uri_normalize_empty_path_test/0

**URI Template Tests (3):**
- [ ] uri_template_match_test/0
- [ ] uri_template_extract_variables_test/0
- [ ] uri_template_invalid_pattern_test/0

**Path Canonicalization Tests (1):**
- [ ] path_remove_double_dot_segments_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_uri_validator_tests
# Target: 25/25 passed, ≥95% coverage for URI validation
```

---

#### Test Suite 5: erlmcp_error_code_compliance_tests.erl (30 tests)

**File:** `apps/erlmcp_core/test/erlmcp_error_code_compliance_tests.erl`

**JSON-RPC Standard Error Codes (5):**
- [ ] error_parse_error_code_test/0
- [ ] error_invalid_request_code_test/0
- [ ] error_method_not_found_code_test/0
- [ ] error_invalid_params_code_test/0
- [ ] error_internal_error_code_test/0

**MCP Core Error Codes (5):**
- [ ] mcp_error_resource_not_found_test/0
- [ ] mcp_error_tool_not_found_test/0
- [ ] mcp_error_prompt_not_found_test/0
- [ ] mcp_error_not_initialized_test/0
- [ ] mcp_error_validation_failed_test/0

**MCP Content Error Codes (3):**
- [ ] mcp_error_tool_description_too_long_test/0
- [ ] mcp_error_message_too_large_test/0
- [ ] mcp_error_invalid_content_type_test/0

**MCP Resource Error Codes (2):**
- [ ] mcp_error_invalid_uri_test/0
- [ ] mcp_error_resource_access_denied_test/0

**MCP Tool Error Codes (4):**
- [ ] mcp_error_tool_execution_failed_test/0
- [ ] mcp_error_tool_timeout_test/0
- [ ] mcp_error_invalid_tool_arguments_test/0
- [ ] mcp_error_tool_schema_invalid_test/0

**MCP Prompt Error Codes (2):**
- [ ] mcp_error_prompt_argument_missing_test/0
- [ ] mcp_error_invalid_prompt_arguments_test/0

**Error Message Format Tests (4):**
- [ ] error_message_not_empty_test/0
- [ ] error_message_descriptive_test/0
- [ ] error_data_field_optional_test/0
- [ ] error_data_includes_context_test/0

**Error Response Structure Tests (5):**
- [ ] error_response_has_error_object_test/0
- [ ] error_response_no_result_field_test/0
- [ ] error_response_preserves_id_test/0
- [ ] error_response_valid_json_rpc_test/0
- [ ] error_code_in_valid_range_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_error_code_compliance_tests
# Target: 30/30 passed, ≥90% coverage for error handling
```

**Week 2 Deliverable:**
- [ ] 105 tests implemented and passing (cumulative: 150)
- [ ] Coverage: ≥70%
- [ ] All P0 security and compliance tests passing

**Phase 1 Complete:**
- [ ] 150 tests total
- [ ] Coverage: ≥70%
- [ ] All quality gates passing

---

## Phase 2: Core Functionality (Weeks 3-4) - P1 Tests

### Week 3: Tools and Resources

#### Test Suite 6: erlmcp_server_tools_tests.erl (40 tests)

**File:** `apps/erlmcp_core/test/erlmcp_server_tools_tests.erl`

**Tool Registration Tests (10):**
- [ ] add_tool_simple_test/0
- [ ] add_tool_with_description_test/0
- [ ] add_tool_with_schema_test/0
- [ ] add_tool_full_metadata_test/0
- [ ] add_tool_deprecated_flag_test/0
- [ ] add_tool_description_length_validation_test/0 **(P0)**
- [ ] add_tool_overwrite_existing_test/0
- [ ] add_tool_list_changed_notification_test/0
- [ ] add_tool_invalid_name_rejected_test/0
- [ ] add_tool_handler_not_function_test/0

**Tool Invocation Tests (8):**
- [ ] tools_call_success_test/0
- [ ] tools_call_with_arguments_test/0
- [ ] tools_call_missing_name_error_test/0
- [ ] tools_call_tool_not_found_error_test/0
- [ ] tools_call_handler_crash_caught_test/0
- [ ] tools_call_progress_token_included_test/0
- [ ] tools_call_timeout_enforced_test/0
- [ ] tools_call_returns_content_types_test/0

**Tool Schema Validation Tests (7):**
- [ ] tool_validate_arguments_valid_test/0
- [ ] tool_validate_arguments_missing_required_test/0
- [ ] tool_validate_arguments_wrong_type_test/0
- [ ] tool_validate_arguments_nested_schema_test/0
- [ ] tool_validate_arguments_array_schema_test/0
- [ ] tool_validate_arguments_enum_schema_test/0
- [ ] tool_validate_no_schema_passes_test/0

**Tool Error Handling Tests (4):**
- [ ] tool_error_response_internal_error_test/0
- [ ] tool_error_response_validation_error_test/0
- [ ] tool_error_handler_exception_logged_test/0
- [ ] tool_error_timeout_error_test/0

**Tool Content Type Tests (5):**
- [ ] tool_result_text_content_test/0
- [ ] tool_result_image_content_test/0
- [ ] tool_result_audio_content_test/0
- [ ] tool_result_embedded_resource_test/0
- [ ] tool_result_multiple_content_test/0

**Tool Notification Tests (3):**
- [ ] tools_list_changed_notification_sent_test/0
- [ ] tools_list_changed_rate_limiting_test/0
- [ ] tools_list_changed_multiple_subscribers_test/0

**Tool Metadata Tests (3):**
- [ ] tools_list_includes_description_test/0
- [ ] tools_list_includes_deprecated_flag_test/0
- [ ] tools_list_includes_schema_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_server_tools_tests
# Target: 40/40 passed, ≥85% coverage for tool logic
```

---

#### Test Suite 7: erlmcp_server_resources_tests.erl (45 tests)

**File:** `apps/erlmcp_core/test/erlmcp_server_resources_tests.erl`

**Resource Registration Tests (8):**
- [ ] add_resource_simple_test/0
- [ ] add_resource_template_test/0
- [ ] add_resource_uri_validation_test/0
- [ ] add_resource_rejects_traversal_attacks_test/0 **(P0)**
- [ ] add_resource_canonicalization_test/0
- [ ] add_resource_overwrite_existing_test/0
- [ ] add_resource_list_changed_notification_test/0
- [ ] add_resource_metadata_validation_test/0

**Resource Reading Tests (8):**
- [ ] resources_read_success_test/0
- [ ] resources_read_uri_not_found_error_test/0
- [ ] resources_read_missing_uri_parameter_test/0
- [ ] resources_read_binary_content_test/0
- [ ] resources_read_text_content_test/0
- [ ] resources_read_mcp_content_structure_test/0
- [ ] resources_read_annotations_test/0
- [ ] resources_read_resource_link_test/0

**Resource Subscription Tests (6):**
- [ ] resources_subscribe_success_test/0
- [ ] resources_subscribe_multiple_test/0
- [ ] resources_unsubscribe_success_test/0
- [ ] resources_unsubscribe_not_subscribed_test/0
- [ ] resources_updated_notification_sent_test/0
- [ ] resources_updated_all_subscribers_test/0

**Resource Template Tests (4):**
- [ ] resource_template_uri_pattern_matching_test/0
- [ ] resource_template_variable_extraction_test/0
- [ ] resource_template_handler_receives_uri_test/0
- [ ] resource_template_not_found_error_test/0

**Resource List Tests (4):**
- [ ] resources_list_returns_all_test/0
- [ ] resources_list_includes_templates_test/0
- [ ] resources_list_includes_metadata_test/0
- [ ] resources_list_pagination_test/0

**URI Validation Tests (4):**
- [ ] uri_validator_rfc3986_compliance_test/0
- [ ] uri_validator_rejects_invalid_scheme_test/0
- [ ] uri_validator_rejects_empty_uri_test/0
- [ ] uri_validator_accepts_valid_uris_test/0

**Path Canonicalization Tests (4):**
- [ ] path_canonicalizer_removes_dot_segments_test/0
- [ ] path_canonicalizer_rejects_traversal_test/0
- [ ] path_canonicalizer_normalizes_slashes_test/0
- [ ] path_canonicalizer_preserves_valid_paths_test/0

**Notification Tests (3):**
- [ ] notify_resource_updated_sends_notification_test/0
- [ ] notify_resources_changed_sends_notification_test/0
- [ ] resource_notification_multiple_subscribers_test/0

**Cleanup Tests (4):**
- [ ] resources_subscribe_process_death_cleanup_test/0
- [ ] resource_handler_crash_caught_test/0
- [ ] resource_notification_subscriber_death_test/0
- [ ] delete_resource_success_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_server_resources_tests
# Target: 45/45 passed, ≥85% coverage for resource logic
```

**Week 3 Deliverable:**
- [ ] 85 tests implemented and passing (cumulative: 235)
- [ ] Coverage: ≥75%

---

### Week 4: Prompts, Capabilities, Notifications

#### Test Suite 8: erlmcp_server_prompts_tests.erl (30 tests)

**File:** `apps/erlmcp_core/test/erlmcp_server_prompts_tests.erl`

**Prompt Registration Tests (5):**
- [ ] add_prompt_simple_test/0
- [ ] add_prompt_with_args_test/0
- [ ] add_prompt_with_args_and_schema_test/0
- [ ] add_prompt_overwrite_existing_test/0
- [ ] add_prompt_list_changed_notification_test/0

**Prompt Retrieval Tests (5):**
- [ ] prompts_get_success_test/0
- [ ] prompts_get_not_found_error_test/0
- [ ] prompts_get_with_arguments_test/0
- [ ] prompts_get_missing_required_argument_test/0
- [ ] prompts_get_handler_crash_caught_test/0

**Argument Validation Tests (5):**
- [ ] prompt_validate_required_arguments_test/0
- [ ] prompt_validate_optional_arguments_test/0
- [ ] prompt_validate_argument_types_test/0
- [ ] prompt_validate_schema_compliance_test/0
- [ ] prompt_validate_missing_required_error_test/0

**Prompt List Tests (3):**
- [ ] prompts_list_returns_all_test/0
- [ ] prompts_list_includes_arguments_test/0
- [ ] prompts_list_includes_metadata_test/0

**Message Format Tests (4):**
- [ ] prompt_returns_message_array_test/0
- [ ] prompt_message_has_role_field_test/0
- [ ] prompt_message_has_content_field_test/0
- [ ] prompt_multiple_messages_test/0

**Error Handling Tests (3):**
- [ ] prompt_error_response_internal_error_test/0
- [ ] prompt_error_response_validation_error_test/0
- [ ] prompt_error_handler_exception_logged_test/0

**Notification Tests (2):**
- [ ] prompts_list_changed_notification_sent_test/0
- [ ] prompts_list_changed_multiple_subscribers_test/0

**Schema Validation Tests (3):**
- [ ] prompt_schema_validation_enabled_test/0
- [ ] prompt_schema_validation_rejects_invalid_test/0
- [ ] prompt_schema_validation_optional_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_server_prompts_tests
# Target: 30/30 passed, ≥85% coverage for prompt logic
```

---

#### Test Suite 9: erlmcp_capabilities_negotiation_tests.erl (25 tests)

**File:** `apps/erlmcp_core/test/erlmcp_capabilities_negotiation_tests.erl`

**Server Capability Tests (6):**
- [ ] server_capabilities_resources_enabled_test/0
- [ ] server_capabilities_tools_enabled_test/0
- [ ] server_capabilities_prompts_enabled_test/0
- [ ] server_capabilities_logging_enabled_test/0
- [ ] server_capabilities_all_disabled_test/0
- [ ] server_capabilities_selective_enabled_test/0

**Client Capability Tests (4):**
- [ ] client_capabilities_roots_enabled_test/0
- [ ] client_capabilities_sampling_enabled_test/0
- [ ] client_capabilities_all_disabled_test/0
- [ ] client_capabilities_missing_fields_test/0

**Capability Negotiation Tests (4):**
- [ ] negotiate_capabilities_full_test/0
- [ ] negotiate_capabilities_partial_test/0
- [ ] negotiate_capabilities_mismatch_test/0
- [ ] negotiate_capabilities_store_in_state_test/0

**Experimental Capability Tests (3):**
- [ ] experimental_capability_passthrough_test/0
- [ ] experimental_capability_ignored_test/0
- [ ] experimental_capability_in_response_test/0

**Notification Tests (4):**
- [ ] capability_list_changed_notification_test/0
- [ ] tools_list_changed_notification_test/0
- [ ] resources_list_changed_notification_test/0
- [ ] prompts_list_changed_notification_test/0

**Validation Tests (4):**
- [ ] validate_capability_structure_test/0
- [ ] validate_capability_enabled_flag_test/0
- [ ] validate_capability_optional_fields_test/0
- [ ] reject_invalid_capability_format_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_capabilities_negotiation_tests
# Target: 25/25 passed, ≥85% coverage for capability logic
```

---

#### Test Suite 10: erlmcp_notification_handler_tests.erl (20 tests)

**File:** `apps/erlmcp_core/test/erlmcp_notification_handler_tests.erl`

**Handler Registration Tests (4):**
- [ ] register_notification_handler_success_test/0
- [ ] register_notification_handler_replaces_existing_test/0
- [ ] unregister_notification_handler_success_test/0
- [ ] unregister_all_handlers_test/0

**Handler Invocation Tests (4):**
- [ ] handler_receives_tools_list_changed_test/0
- [ ] handler_receives_resources_list_changed_test/0
- [ ] handler_receives_prompts_list_changed_test/0
- [ ] handler_receives_resources_updated_test/0

**Handler Monitoring Tests (2):**
- [ ] handler_death_cleanup_test/0
- [ ] handler_monitor_ref_stored_test/0

**Handler Error Tests (3):**
- [ ] handler_crash_doesnt_crash_server_test/0
- [ ] handler_timeout_handled_test/0
- [ ] handler_invalid_response_logged_test/0

**Rate Limiting Tests (3):**
- [ ] tools_list_changed_rate_limited_test/0
- [ ] rate_limit_respects_interval_test/0
- [ ] rate_limit_resets_after_interval_test/0

**Multiple Handler Tests (3):**
- [ ] multiple_handlers_all_receive_test/0
- [ ] multiple_handlers_independent_test/0
- [ ] multiple_handlers_selective_test/0

**Lifecycle Tests (1):**
- [ ] handler_cleanup_on_server_shutdown_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_notification_handler_tests
# Target: 20/20 passed, ≥85% coverage for notification logic
```

**Week 4 Deliverable:**
- [ ] 75 tests implemented and passing (cumulative: 310)
- [ ] Coverage: ≥80%

**Phase 2 Complete:**
- [ ] 160 tests total (cumulative: 310)
- [ ] Coverage: ≥80%
- [ ] All core functionality tested

---

## Phase 3: Advanced Features (Weeks 5-6) - P1/P2 Tests

### Week 5: Progress, Content Types

#### Test Suite 11: erlmcp_progress_tracking_tests.erl (15 tests)

**File:** `apps/erlmcp_core/test/erlmcp_progress_tracking_tests.erl`

**Progress Token Tests (3):**
- [ ] generate_progress_token_unique_test/0
- [ ] generate_progress_token_format_test/0
- [ ] progress_token_stored_in_state_test/0

**Progress Reporting Tests (4):**
- [ ] report_progress_sends_notification_test/0
- [ ] report_progress_includes_token_test/0
- [ ] report_progress_includes_progress_value_test/0
- [ ] report_progress_includes_total_test/0

**Lifecycle Tests (3):**
- [ ] progress_token_cleanup_after_completion_test/0
- [ ] progress_token_cleanup_on_error_test/0
- [ ] progress_token_multiple_active_test/0

**Format Tests (3):**
- [ ] progress_notification_structure_test/0
- [ ] progress_notification_method_test/0
- [ ] progress_notification_params_test/0

**Error Handling Tests (2):**
- [ ] progress_invalid_token_error_test/0
- [ ] progress_out_of_range_value_error_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_progress_tracking_tests
# Target: 15/15 passed, ≥85% coverage
```

---

#### Test Suite 12: erlmcp_content_type_validation_tests.erl (20 tests)

**File:** `apps/erlmcp_core/test/erlmcp_content_type_validation_tests.erl`

**Content Type Tests (5):**
- [ ] content_type_text_valid_test/0
- [ ] content_type_image_valid_test/0
- [ ] content_type_audio_valid_test/0
- [ ] content_type_video_valid_test/0
- [ ] content_type_invalid_type_test/0

**Text Content Tests (4):**
- [ ] text_content_has_text_field_test/0
- [ ] text_content_has_type_field_test/0
- [ ] text_content_optional_mime_type_test/0
- [ ] text_content_utf8_encoding_test/0

**Image Content Tests (3):**
- [ ] image_content_has_data_field_test/0
- [ ] image_content_has_mime_type_test/0
- [ ] image_content_base64_encoded_test/0

**Audio Content Tests (2):**
- [ ] audio_content_has_data_field_test/0
- [ ] audio_content_has_mime_type_test/0

**Embedded Resource Tests (3):**
- [ ] embedded_resource_has_resource_field_test/0
- [ ] embedded_resource_has_uri_test/0
- [ ] embedded_resource_valid_structure_test/0

**Multiple Content Tests (3):**
- [ ] multiple_content_array_test/0
- [ ] multiple_content_mixed_types_test/0
- [ ] multiple_content_preserves_order_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_content_type_validation_tests
# Target: 20/20 passed, ≥85% coverage
```

**Week 5 Deliverable:**
- [ ] 35 tests implemented and passing (cumulative: 345)
- [ ] Coverage: ≥82%

---

### Week 6: Integration, Backwards Compatibility

#### Test Suite 13: erlmcp_integration_compliance_SUITE.erl (25 tests)

**File:** `apps/erlmcp_core/test/erlmcp_integration_compliance_SUITE.erl`

**Full Protocol Flow Tests (5):**
- [ ] full_initialize_handshake_test/1
- [ ] full_capability_negotiation_test/1
- [ ] full_resource_lifecycle_test/1
- [ ] full_tool_execution_test/1
- [ ] full_prompt_retrieval_test/1

**Multi-Process Tests (3):**
- [ ] multiple_clients_concurrent_test/1
- [ ] multiple_servers_independent_test/1
- [ ] client_server_bidirectional_test/1

**Subscription Tests (3):**
- [ ] resource_subscribe_notify_test/1
- [ ] tools_list_changed_notify_test/1
- [ ] multiple_subscribers_test/1

**Error Recovery Tests (3):**
- [ ] client_crash_recovery_test/1
- [ ] server_crash_recovery_test/1
- [ ] network_partition_recovery_test/1

**Performance Tests (3):**
- [ ] throughput_100k_requests_test/1
- [ ] latency_p99_under_100ms_test/1
- [ ] concurrent_1000_clients_test/1

**State Consistency Tests (3):**
- [ ] state_consistency_after_error_test/1
- [ ] state_consistency_after_restart_test/1
- [ ] state_consistency_distributed_test/1

**Compliance Tests (3):**
- [ ] mcp_spec_compliance_full_test/1
- [ ] json_rpc_compliance_full_test/1
- [ ] capability_negotiation_compliance_test/1

**Security Tests (2):**
- [ ] reject_unauthorized_access_test/1
- [ ] reject_path_traversal_test/1

**Quality Gate:**
```bash
rebar3 ct --suite=erlmcp_integration_compliance_SUITE
# Target: 25/25 passed, P99 latency <100ms
```

---

#### Test Suite 14: erlmcp_backwards_compatibility_tests.erl (15 tests)

**File:** `apps/erlmcp_core/test/erlmcp_backwards_compatibility_tests.erl`

**Protocol Version Tests (4):**
- [ ] protocol_version_2025_11_25_test/0
- [ ] protocol_version_2024_11_05_test/0
- [ ] protocol_version_unknown_rejected_test/0
- [ ] protocol_version_negotiation_test/0

**Deprecated Feature Tests (3):**
- [ ] deprecated_tool_marked_test/0
- [ ] deprecated_tool_still_callable_test/0
- [ ] deprecated_tool_warning_logged_test/0

**Feature Flag Tests (3):**
- [ ] feature_flag_enabled_test/0
- [ ] feature_flag_disabled_test/0
- [ ] feature_flag_unknown_ignored_test/0

**Legacy Message Tests (3):**
- [ ] legacy_message_format_accepted_test/0
- [ ] legacy_message_format_converted_test/0
- [ ] legacy_message_format_warned_test/0

**Migration Tests (2):**
- [ ] migration_from_old_version_test/0
- [ ] migration_preserves_state_test/0

**Quality Gate:**
```bash
rebar3 eunit --module=erlmcp_backwards_compatibility_tests
# Target: 15/15 passed, ≥80% coverage
```

**Week 6 Deliverable:**
- [ ] 40 tests implemented and passing (cumulative: 385)
- [ ] Coverage: ≥85%

**Phase 3 Complete:**
- [ ] 75 tests total (cumulative: 385)
- [ ] Coverage: ≥85%
- [ ] All advanced features tested

---

## Phase 4: Polish (Weeks 7-8) - Property Tests

### Week 7-8: Property Tests and Final Polish

#### Test Suite 15: erlmcp_property_tests.erl (20 properties)

**File:** `apps/erlmcp_core/test/erlmcp_property_tests.erl`

**JSON-RPC Properties (4):**
- [ ] prop_encode_decode_roundtrip/0
- [ ] prop_request_always_has_id/0
- [ ] prop_notification_never_has_id/0
- [ ] prop_error_code_in_range/0

**Capability Properties (2):**
- [ ] prop_capabilities_always_valid_structure/0
- [ ] prop_capability_flags_boolean/0

**URI Properties (3):**
- [ ] prop_uri_validation_idempotent/0
- [ ] prop_uri_canonicalization_stable/0
- [ ] prop_path_traversal_always_rejected/0

**Tool Properties (2):**
- [ ] prop_tool_description_length_enforced/0
- [ ] prop_tool_schema_validation_consistent/0

**Progress Properties (2):**
- [ ] prop_progress_token_unique/0
- [ ] prop_progress_value_in_range/0

**Error Properties (2):**
- [ ] prop_error_code_valid_range/0
- [ ] prop_error_message_not_empty/0

**State Machine Properties (2):**
- [ ] prop_initialize_always_first/0
- [ ] prop_pre_init_requests_rejected/0

**Content Properties (2):**
- [ ] prop_content_type_valid/0
- [ ] prop_mime_type_format_valid/0

**Quality Gate:**
```bash
rebar3 proper --module=erlmcp_property_tests --numtests=1000
# Target: 20/20 properties pass 1000 cases each
```

**Week 7-8 Deliverable:**
- [ ] 20 properties implemented and passing (cumulative: 405)
- [ ] Coverage: ≥85%
- [ ] All quality gates passing

**Phase 4 Complete:**
- [ ] 20 properties total (cumulative: 405)
- [ ] Coverage: ≥85%
- [ ] Production ready

---

## Final Quality Gates

### Compilation

```bash
✅ make compile
# Expected: 0 errors, 0 warnings
```

### Tests

```bash
✅ make test-core
# Expected: 405/405 passed (100% pass rate)
```

### Coverage

```bash
✅ rebar3 cover --verbose
# Expected: ≥85% overall
#   - erlmcp_server: ≥85%
#   - erlmcp_json_rpc: ≥95%
#   - erlmcp_capabilities: ≥85%
#   - erlmcp_uri_validator: ≥95%
```

### Dialyzer

```bash
✅ rebar3 dialyzer
# Expected: 0 warnings
```

### Xref

```bash
✅ rebar3 xref
# Expected: 0 issues
```

### Format

```bash
✅ rebar3 format --verify
# Expected: All files formatted
```

---

## Production Readiness Checklist

### Code Quality

- [ ] All 405 tests passing
- [ ] ≥85% code coverage
- [ ] 0 compilation errors/warnings
- [ ] 0 dialyzer warnings
- [ ] 0 xref issues
- [ ] All files formatted

### Documentation

- [ ] All test suites documented
- [ ] Test plan complete
- [ ] Quick start guide available
- [ ] Test helpers documented
- [ ] Quality gates documented

### MCP 2025-11-25 Compliance

- [ ] Initialize handshake validated
- [ ] State machine enforced
- [ ] Protocol version validated
- [ ] All error codes tested
- [ ] Capability negotiation complete
- [ ] Tool metadata validated
- [ ] Resource URI security enforced
- [ ] Content type validation complete
- [ ] Notification system tested
- [ ] Progress tracking validated

### Security

- [ ] Path traversal prevention tested
- [ ] URI validation enforced
- [ ] Pre-init rejection tested
- [ ] Command injection prevented
- [ ] All P0 security tests passing

### Performance

- [ ] Throughput: >10K req/s
- [ ] P99 latency: <100ms
- [ ] 1000+ concurrent clients supported
- [ ] No memory leaks
- [ ] No degradation under load

---

## Summary

**Total Test Count:** 405 tests
- EUnit: 360 tests
- Common Test: 25 tests
- Proper: 20 properties

**Coverage Target:** ≥85%
**Implementation Timeline:** 8 weeks
**Test Philosophy:** Chicago School TDD
**Status:** Ready for Implementation

**LET'S ACHIEVE 85% COVERAGE WITH 405 TESTS!**

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-30
**Status:** Active Tracking
