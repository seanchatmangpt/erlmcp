# Xref Analysis Report for erlmcp

## Executive Summary

**Status**: ❌ FAIL
- **Undefined Function Calls**: 40
- **Unused Functions**: 42
- **Dead Code Paths**: Multiple

The project has significant issues with undefined function calls and dead code that need to be addressed before production deployment.

## 1. Undefined Function Calls

### Missing/Unimplemented Modules (5)
1. **`erlmcp_mtls_validator`** - `validate_certificate/2` is undefined
2. **`erlmcp_prompt_argument_validator`** - `validate_prompt_arguments/3` is undefined
3. **`erlmcp_protocol_validator`** - `run/1` and `validate_error_codes/1` are undefined
4. **`erlmcp_resources`** - `list_roots/1` is undefined
5. **`erlmcp_schema_validator`** - `validate/3` is undefined
6. **`erlmcp_sse_event_store`** - `delete_session/1` is undefined
7. **`erlmcp_transport_ws`** - `init/2` is undefined
8. **`erlmcp_uri_validator`** - `validate_resource_uri_on_registration/1` and `validate_uri_template/1` are undefined

### Missing External Functions (7)
1. **`ets:fold/3`** - Missing ETS table folding function
2. **`ssl:peercert/2`** - Missing SSL certificate extraction function
3. **`opentelemetry:get_tracer_provider/0`** - Missing OTEL API function
4. **`otel_batch_processor:force_flush/0`** - Missing OTEL batch processor function
5. **`otel_batch_processor:shutdown/0`** - Missing OTEL batch processor function
6. **`otel_sampler:always_off/0`** - Missing OTEL sampler function
7. **`otel_sampler:always_on/0`** - Missing OTEL sampler function
8. **`otel_sampler:parent_based/1`** - Missing OTEL sampler function
9. **`otel_sampler:trace_id_ratio_based/1`** - Missing OTEL sampler function
10. **`otel_span:add_event/4`** - Missing OTEL span function

## 2. Unused Functions (42)

### erlmcp_core (18 unused functions)
1. `erlmcp_client:ping/1`
2. `erlmcp_code_reload:get_all_modules/0`
3. `erlmcp_code_reload:get_module_info/1`
4. `erlmcp_code_reload:is_valid_module/1`
5. `erlmcp_code_reload:registered_modules/0`
6. `erlmcp_json_rpc:error_access_denied/2`
7. `erlmcp_json_rpc:error_authentication_failed/2`
8. `erlmcp_json_rpc:error_authorization_failed/2`
9. `erlmcp_json_rpc:error_capability_negotiation_failed/2`
10. `erlmcp_json_rpc:error_completion_failed/3`
11. `erlmcp_json_rpc:error_completion_not_found/2`
12. `erlmcp_json_rpc:error_content_too_large/3`
13. `erlmcp_json_rpc:error_cursor_expired/2`
14. `erlmcp_json_rpc:error_invalid_completion_argument/3`
15. `erlmcp_json_rpc:error_invalid_completion_reference/2`
16. `erlmcp_json_rpc:error_invalid_content_type/2`
17. `erlmcp_json_rpc:error_invalid_credentials/1`
18. `erlmcp_json_rpc:error_invalid_cursor/2`
19. `erlmcp_json_rpc:error_invalid_encoding/2`
20. `erlmcp_json_rpc:error_invalid_progress_token/2`
21. `erlmcp_json_rpc:error_invalid_prompt_arguments/3`
22. `erlmcp_json_rpc:error_invalid_tool_arguments/3`
23. `erlmcp_json_rpc:error_invalid_uri/2`
24. `erlmcp_json_rpc:error_method_not_supported/2`
25. `erlmcp_json_rpc:error_notification_failed/3`
26. `erlmcp_json_rpc:error_notification_queue_full/2`
27. `erlmcp_json_rpc:error_page_size_too_large/3`
28. `erlmcp_json_rpc:error_pagination_not_supported/2`
29. `erlmcp_json_rpc:error_progress_token_expired/2`
30. `erlmcp_json_rpc:error_progress_update_failed/3`
31. `erlmcp_json_rpc:error_prompt_argument_missing/3`
32. `erlmcp_json_rpc:error_prompt_render_failed/3`
33. `erlmcp_json_rpc:error_protocol_version_mismatch/3`
34. `erlmcp_json_rpc:error_resource_access_denied/2`
35. `erlmcp_json_rpc:error_resource_template_not_found/2`
36. `erlmcp_json_rpc:error_sampling_failed/2`
37. `erlmcp_json_rpc:error_task_already_exists/2`
38. `erlmcp_json_rpc:error_task_cancelled/2`
39. `erlmcp_json_rpc:error_task_failed/3`
40. `erlmcp_json_rpc:error_task_not_found/2`
41. `erlmcp_json_rpc:error_task_timeout/3`
42. `erlmcp_json_rpc:error_template_render_failed/3`
43. `erlmcp_json_rpc:error_token_expired/1`
44. `erlmcp_json_rpc:error_tool_cancelled/2`
45. `erlmcp_json_rpc:error_tool_description_too_large/3`
46. `erlmcp_json_rpc:error_tool_execution_failed/3`
47. `erlmcp_json_rpc:error_tool_timeout/3`
48. `erlmcp_json_rpc:error_unsupported_protocol_version/2`
49. `erlmcp_json_rpc:error_uri_syntax_error/3`

### erlmcp_core (4 additional unused functions)
1. `erlmcp_resources:list_roots_with_state/1`
2. `erlmcp_resources:read_resource_with_roots/2`
3. `erlmcp_server:create_elicitations/2`
4. `erlmcp_server:create_elicitations/3`
5. `erlmcp_server:create_single_elicitation/3`

### erlmcp_core (2 more unused functions)
1. `erlmcp_session_failover:notify_failover_local/2`
2. `erlmcp_session_replicator:is_fully_replicated/2`

### erlmcp_core (1 more unused function)
1. `erlmcp_tasks:list_tasks/1`

### erlmcp_observability (2 unused functions)
1. `erlmcp_chaos_resource:allocate_chunks/4`
2. `erlmcp_chaos_resource:allocate_memory_gradually/2`

### erlmcp_validation (2 unused functions)
1. `erlmcp_compliance_report_json:validate_report_structure/1`
2. `erlmcp_validate_cli:run_command/2`

## 3. Files with Most Issues

### Most Critical Files (High Impact)
1. **`apps/erlmcp_core/src/erlmcp_json_rpc.erl`** - 30 unused error functions
2. **`apps/erlmcp_validation/src/erlmcp_validate_cli.erl`** - 3 undefined function calls
3. **`apps/erlmcp_core/src/erlmcp_server.erl`** - 3 undefined function calls
4. **`apps/erlmcp_observability/src/erlmcp_otel.erl`** - 8 undefined function calls

### Most Common Issues
1. **Missing validator modules** - Several validator modules are referenced but not implemented
2. **Dead error handling functions** - Large number of unused error handling functions in `erlmcp_json_rpc`
3. **Missing OTEL API functions** - OpenTelemetry integration has missing functions
4. **Missing transport implementations** - WebSocket transport initialization not implemented

## 4. Recommendations

### Immediate Actions (Critical)
1. **Implement missing validator modules**:
   - Create `erlmcp_mtls_validator.erl`
   - Create `erlmcp_prompt_argument_validator.erl`
   - Create `erlmcp_protocol_validator.erl`
   - Create `erlmcp_schema_validator.erl`
   - Create `erlmcp_uri_validator.erl`

2. **Fix WebSocket transport**:
   - Implement `erlmcp_transport_ws:init/2`
   - Or remove the call to this function

3. **Clean up unused functions**:
   - Remove or properly implement the 42 unused functions
   - Focus on `erlmcp_json_rpc` error functions first

4. **Fix OTEL integration**:
   - Update OpenTelemetry dependency versions
   - Fix API function calls

### Medium Priority
1. **Fix ETS usage**:
   - Implement `ets:fold/3` or replace with alternative
   - Review ETS usage patterns

2. **Fix SSL integration**:
   - Update SSL API calls
   - Review MTLS validation logic

### Long Term
1. **Refactor error handling**:
   - Consolidate error functions
   - Implement proper error handling strategy
   - Remove unused error functions

2. **Module organization**:
   - Review module responsibilities
   - Consolidate related functions

## 5. Compliance with Quality Gates

This analysis shows that the project currently fails the xref quality gate:

- ❌ **Undefined function calls**: 40 (should be 0)
- ❌ **Unused functions**: 42 (should be ideally 0, at least < 10)
- ❌ **Dead code paths**: Multiple

The project needs significant refactoring before it can meet the quality standards required for production deployment.

---
*Generated by Agent 13: Xref Analysis*
*Date: 2026-02-01*