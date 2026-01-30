# MCP Server Testing Strategy - Comprehensive Plan

**Version:** 1.0.0
**Date:** 2025-01-29
**Status:** Planning Phase
**Scope:** erlmcp_server (1587 lines) and related server-side modules

## Executive Summary

This document outlines a comprehensive testing strategy for the erlmcp_server module implementing the Model Context Protocol (MCP) server. The strategy covers all 8 critical areas identified, with emphasis on Chicago School TDD (real processes, no mocks), OTP compliance, and production-grade reliability.

**Key Metrics:**
- **Target Coverage:** ≥80% (mandatory)
- **Test Framework:** EUnit + Common Test
- **Test Philosophy:** Chicago School TDD (state-based, no mocks)
- **Estimated Test Count:** 200+ test cases across 15 test modules

---

## 1. Server Initialization and Capability Advertising

### Scope
- Server startup and registration
- Capability negotiation (MCP 2025-11-25 spec)
- Protocol version validation
- Initialization state machine (Gap #4 enforcement)

### Test Scenarios

#### 1.1 Basic Lifecycle Tests
```
✅ test_server_start_link_success/0
✅ test_server_start_link_with_map_config/0
✅ test_server_stop_normal/0
✅ test_server_stop_with_active_resources/0
✅ test_server_stop_with_active_subscribers/0
✅ test_server_trap_exit_flag/0
✅ test_supervisor_restart_on_crash/0
✅ test_multiple_servers_independent_state/0
```

#### 1.2 Initialization Phase Tests (P0 Security)
```
✅ test_initialize_first_request_success/0
✅ test_initialize_response_structure/0
✅ test_initialize_capability_negotiation/0
✅ test_initialize_protocol_version_validation/0
✅ test_reject_double_initialize/0
✅ test_reject_non_initialize_before_init/0
✅ test_initialized_flag_set_correctly/0
✅ test_phase_transition_init_to_ready/0
```

#### 1.3 Capability Advertising Tests
```
✅ test_advertise_resources_capability/0
✅ test_advertise_tools_capability/0
✅ test_advertise_prompts_capability/0
✅ test_advertise_logging_capability/0
✅ test_advertise_sampling_capability/0
✅ test_advertise_roots_capability/0
✅ test_capability_list_changed_notification/0
✅ test_experimental_capability_passthrough/0
```

#### 1.4 Client Capability Handling
```
✅ test_extract_client_capabilities_full/0
✅ test_extract_client_capabilities_minimal/0
✅ test_store_client_capabilities_in_state/0
✅ test_client_roots_capability_detection/0
✅ test_client_sampling_capability_detection/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_init_tests.erl`

### Key Assertions
- Server starts with correct capabilities record
- Initialize MUST be first request (P0 security)
- Double initialize returns error (protocol violation)
- Non-initialize requests before init return NOT_INITIALIZED error
- Capability response matches MCP spec structure
- Server info includes name and version

---

## 2. Tool Execution with Validation and Error Handling

### Scope
- Tool registration (with/without JSON Schema)
- Tool invocation via `tools/call`
- Argument validation against schemas
- CPU quota protection (Gap #10)
- Progress token tracking
- Error responses and exceptions

### Test Scenarios

#### 2.1 Tool Registration Tests
```
✅ test_add_tool_simple/0
✅ test_add_tool_with_json_schema/0
✅ test_add_tool_schema_validation_required_fields/0
✅ test_add_tool_schema_validation_types/0
✅ test_add_tool_overwrite_existing/0
✅ test_add_tool_list_changed_notification/0
✅ test_add_tool_invalid_name_rejected/0
✅ test_add_tool_handler_not_function_crashes/0
```

#### 2.2 Tool Invocation Tests
```
✅ test_tools_call_success/0
✅ test_tools_call_with_arguments/0
✅ test_tools_call_missing_name_error/0
✅ test_tools_call_tool_not_found_error/0
✅ test_tools_call_handler_crash_caught/0
✅ test_tools_call_progress_token_included/0
✅ test_tools_call_timeout_enforced/0
✅ test_tools_call_cpu_quota_protection/0
```

#### 2.3 Tool Schema Validation Tests
```
✅ test_tool_validate_arguments_valid/0
✅ test_tool_validate_arguments_missing_required/0
✅ test_tool_validate_arguments_wrong_type/0
✅ test_tool_validate_arguments_nested_schema/0
✅ test_tool_validate_arguments_array_schema/0
✅ test_tool_validate_arguments_enum_schema/0
✅ test_tool_validate_no_schema_passes/0
```

#### 2.4 Tool Error Handling Tests
```
✅ test_tool_error_response_internal_error/0
✅ test_tool_error_response_custom_error/0
✅ test_tool_error_response_validation_error/0
✅ test_tool_error_handler_exception_logged/0
✅ test_tool_error_timeout_error/0
✅ test_tool_error_cpu_quota_exceeded/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_tools_tests.erl`

### Key Assertions
- Tools list returns registered tools with schemas
- Tool call invokes handler with arguments
- Schema validation rejects invalid arguments
- CPU quota prevents runaway handlers
- Progress tokens are unique and tracked
- Errors are returned as JSON-RPC error responses
- Handler crashes don't crash server

---

## 3. Resource Management with Subscriptions and Notifications

### Scope
- Resource registration (URIs, templates)
- Resource reading via `resources/read`
- Resource subscriptions (`resources/subscribe`)
- Unsubscription (`resources/unsubscribe`)
- Change notifications (`notifications/resources/list_changed`)
- Path validation and security (Gap #36)

### Test Scenarios

#### 3.1 Resource Registration Tests
```
✅ test_add_resource_simple/0
✅ test_add_resource_template/0
✅ test_add_resource_uri_validation/0
✅ test_add_resource_rejects_traversal_attacks/0
✅ test_add_resource_canonicalization/0
✅ test_add_resource_overwrite_existing/0
✅ test_add_resource_list_changed_notification/0
✅ test_add_resource_multiple_subscribers/0
```

#### 3.2 Resource Reading Tests
```
✅ test_resources_read_success/0
✅ test_resources_read_uri_not_found_error/0
✅ test_resources_read_missing_uri_parameter/0
✅ test_resources_read_binary_content/0
✅ test_resources_read_mcp_content_structure/0
✅ test_resources_read_annotations/0
✅ test_resources_read_resource_link/0
✅ test_resources_read_handler_crash_caught/0
```

#### 3.3 Resource Subscription Tests
```
✅ test_resources_subscribe_success/0
✅ test_resources_subscribe_multiple/0
✅ test_resources_unsubscribe_success/0
✅ test_resources_unsubscribe_not_subscribed/0
✅ test_resources_updated_notification_sent/0
✅ test_resources_updated_metadata_included/0
✅ test_resources_subscribe_persists_across_calls/0
✅ test_resources_cleanup_on_server_stop/0
```

#### 3.4 Resource Template Tests
```
✅ test_resources_templates_list/0
✅ test_resources_templates_uri_template_valid/0
✅ test_resources_templates_list_changed_notification/0
✅ test_resources_templates_expand_uri/0
✅ test_resources_templates_validation/0
```

#### 3.5 Resource Security Tests (Gap #36)
```
✅ test_resource_path_traversal_prevented/0
✅ test_resource_symlink_attack_prevented/0
✅ test_resource_absolute_path_rejected/0
✅ test_resource_relative_path_validation/0
✅ test_resource_allowed_directories_enforced/0
✅ test_resource_canonical_uri_maintained/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_resources_tests.erl`

### Key Assertions
- Resources list returns all registered resources
- Resource read invokes handler with URI
- Subscribers receive update notifications
- Path traversal attacks are rejected (security)
- URI canonicalization prevents symlink attacks
- Templates are valid URI templates
- Missing resources return 404 error

---

## 4. Prompt Handling with Template Expansion

### Scope
- Prompt registration (with/without arguments)
- Prompt retrieval via `prompts/get`
- Argument validation against schemas (Gap #42)
- Template variable expansion
- Multi-message prompt responses

### Test Scenarios

#### 4.1 Prompt Registration Tests
```
✅ test_add_prompt_simple/0
✅ test_add_prompt_with_arguments/0
✅ test_add_prompt_with_json_schema/0
✅ test_add_prompt_argument_required/0
✅ test_add_prompt_argument_optional/0
✅ test_add_prompt_list_changed_notification/0
✅ test_add_prompt_overwrite_existing/0
✅ test_add_prompt_invalid_name_rejected/0
```

#### 4.2 Prompt Retrieval Tests
```
✅ test_prompts_get_success/0
✅ test_prompts_get_with_arguments/0
✅ test_prompts_get_missing_name_error/0
✅ test_prompts_get_prompt_not_found_error/0
✅ test_prompts_get_template_expansion/0
✅ test_prompts_get_multi_message_response/0
✅ test_prompts_get_handler_crash_caught/0
✅ test_prompts_get_metadata_included/0
```

#### 4.3 Prompt Argument Validation Tests (Gap #42)
```
✅ test_prompt_validate_arguments_valid/0
✅ test_prompt_validate_arguments_missing_required/0
✅ test_prompt_validate_arguments_wrong_type/0
✅ test_prompt_validate_arguments_no_schema_passes/0
✅ test_prompt_validate_arguments_nested_schema/0
✅ test_prompt_validate_arguments_with_defaults/0
```

#### 4.4 Prompt Template Tests
```
✅ test_prompt_template_variable_substitution/0
✅ test_prompt_template_missing_variable_error/0
✅ test_prompt_template_complex_expansion/0
✅ test_prompt_template_nested_variables/0
✅ test_prompt_template_conditionals/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_prompts_tests.erl`

### Key Assertions
- Prompts list returns registered prompts with arguments
- Prompt get invokes handler with arguments
- Argument validation rejects invalid inputs
- Template variables are expanded correctly
- Multi-message responses are properly formatted
- Missing prompts return 404 error

---

## 5. Batch Request Processing and Partial Failures

### Scope
- Batch request handling (JSON-RPC batch)
- Partial success/failure scenarios
- Error isolation between requests
- Ordered response handling
- Concurrent request processing

### Test Scenarios

#### 5.1 Batch Request Tests
```
✅ test_batch_request_all_success/0
✅ test_batch_request_all_failure/0
✅ test_batch_request_partial_success/0
✅ test_batch_request_empty_array/0
✅ test_batch_request_single_request/0
✅ test_batch_request_order_preserved/0
✅ test_batch_request_isolation/0
✅ test_batch_request_concurrent/0
```

#### 5.2 Batch Error Handling Tests
```
✅ test_batch_error_one_fails_other_succeeds/0
✅ test_batch_error_invalid_json_rpc/0
✅ test_batch_error_missing_id/0
✅ test_batch_error_notification_only/0
✅ test_batch_error_mixed_requests_notifications/0
✅ test_batch_error_response_format/0
```

#### 5.3 Batch Performance Tests
```
✅ test_batch_throughput_100_requests/0
✅ test_batch_latency_p50_p99/0
✅ test_batch_memory_leak_check/0
✅ test_batch_concurrent_clients/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_batch_tests.erl`

### Key Assertions
- Batch requests are processed independently
- Partial failures don't abort entire batch
- Response order matches request order
- Errors are isolated to individual requests
- Notifications in batch don't generate responses
- Batch processing doesn't leak memory

---

## 6. State Management and Phase Tracking

### Scope
- Initialization phase tracking (Gap #4)
- Phase transitions (init → ready)
- State persistence across requests
- Client capability storage
- Protocol version storage

### Test Scenarios

#### 6.1 Phase Tracking Tests
```
✅ test_phase_initialization_on_start/0
✅ test_phase_ready_after_initialize/0
✅ test_phase_reject_wrong_phase_operations/0
✅ test_phase_transition_single_direction/0
✅ test_phase_timeout_enforced/0
✅ test_phase_reinitialization_blocked/0
```

#### 6.2 State Persistence Tests
```
✅ test_state_capabilities_persist/0
✅ test_state_client_capabilities_stored/0
✅ test_state_protocol_version_stored/0
✅ test_state_resources_persist/0
✅ test_state_tools_persist/0
✅ test_state_prompts_persist/0
✅ test_state_subscriptions_persist/0
```

#### 6.3 State Cleanup Tests
```
✅ test_state_cleanup_on_terminate/0
✅ test_state_cleanup_progress_tokens/0
✅ test_state_cleanup_subscriptions/0
✅ test_state_cleanup_handler_references/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_state_tests.erl`

### Key Assertions
- Phase transitions are strictly enforced
- State persists across gen_server callbacks
- Client capabilities are stored correctly
- Protocol version is validated and stored
- Cleanup happens on terminate

---

## 7. Resource Cleanup and Memory Management

### Scope
- Periodic garbage collection (Gap #10)
- Binary heap cleanup
- Process monitoring and cleanup
- Subscription cleanup on disconnect
- Progress token cleanup

### Test Scenarios

#### 7.1 Memory Management Tests
```
✅ test_periodic_gc_triggered/0
✅ test_periodic_gc_frees_binary_heap/0
✅ test_periodic_gc_reschedules/0
✅ test_binary_leak_prevention/0
✅ test_memory_growth_under_load/0
```

#### 7.2 Resource Cleanup Tests
```
✅ test_cleanup_resources_on_stop/0
✅ test_cleanup_tools_on_stop/0
✅ test_cleanup_prompts_on_stop/0
✅ test_cleanup_subscriptions_on_disconnect/0
✅ test_cleanup_progress_tokens_on_complete/0
```

#### 7.3 Process Cleanup Tests
```
✅ test_cleanup_handler_processes/0
✅ test_cleanup_monitor_references/0
✅ test_cleanup_stale_connections/0
✅ test_cleanup_orphaned_subscriptions/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_cleanup_tests.erl`

### Key Assertions
- Periodic GC runs every 60 seconds
- Binary heap is freed after GC
- Resources are cleaned on server stop
- Subscriptions are cleaned on disconnect
- Progress tokens are cleaned on completion
- No memory leaks under sustained load

---

## 8. Performance and Load Testing Approaches

### Scope
- Throughput benchmarks (requests/second)
- Latency measurements (p50, p95, p99)
- Concurrent connection handling
- Memory usage under load
- Stress testing (100K+ requests)

### Test Scenarios

#### 8.1 Throughput Tests
```
✅ test_throughput_tools_call_1000_ops/0
✅ test_throughput_resources_read_1000_ops/0
✅ test_throughput_prompts_get_1000_ops/0
✅ test_throughput_mixed_workload_10000_ops/0
```

#### 8.2 Latency Tests
```
✅ test_latency_p50_tools_call/0
✅ test_latency_p95_tools_call/0
✅ test_latency_p99_tools_call/0
✅ test_latency_under_concurrent_load/0
```

#### 8.3 Concurrency Tests
```
✅ test_concurrent_10_clients/0
✅ test_concurrent_100_clients/0
✅ test_concurrent_1000_clients/0
✅ test_concurrent_request_isolation/0
```

#### 8.4 Stress Tests
```
✅ test_stress_100k_requests/0
✅ test_stress_sustained_load_1hr/0
✅ test_stress_memory_exhaustion/0
✅ test_stress_process_explosion/0
✅ test_stress_port_exhaustion/0
```

### Test Module
**File:** `apps/erlmcp_core/test/erlmcp_server_performance_tests.erl`

### Key Assertions
- Baseline throughput ≥1000 ops/sec (in-memory)
- p99 latency ≤100ms (in-memory)
- Handles 100 concurrent clients without degradation
- No memory leaks over 1 hour sustained load
- Graceful degradation under stress

---

## Test Organization

### Test Module Structure

```
apps/erlmcp_core/test/
├── erlmcp_server_init_tests.erl           (25 tests)
├── erlmcp_server_tools_tests.erl          (35 tests)
├── erlmcp_server_resources_tests.erl      (40 tests)
├── erlmcp_server_prompts_tests.erl        (30 tests)
├── erlmcp_server_batch_tests.erl          (20 tests)
├── erlmcp_server_state_tests.erl          (20 tests)
├── erlmcp_server_cleanup_tests.erl        (20 tests)
└── erlmcp_server_performance_tests.erl    (10 tests)
```

### Common Test Suite Structure

```
test/
├── erlmcp_server_init_SUITE.erl           (CT integration tests)
├── erlmcp_server_tools_SUITE.erl          (CT integration tests)
├── erlmcp_server_resources_SUITE.erl      (CT integration tests)
└── erlmcp_server_full_protocol_SUITE.erl  (End-to-end tests)
```

### Test Helper Modules

```
apps/erlmcp_core/test/
├── erlmcp_server_test_helpers.erl         (Common fixtures, assertions)
├── erlmcp_server_mocks.erl                (Mock handlers, transport)
└── erlmcp_serverAssertions.erl            (Custom assertions)
```

---

## Test Execution Strategy

### Phase 1: Unit Tests (EUnit)
- Run per-module: `rebar3 eunit --module=erlmcp_server_init_tests`
- Run all: `rebar3 eunit`
- Coverage: `rebar3 cover`

### Phase 2: Integration Tests (CT)
- Run specific suite: `rebar3 ct --suite=erlmcp_server_init_SUITE`
- Run all: `rebar3 ct`
- Coverage: Combined with EUnit

### Phase 3: Performance Tests (Benchmarks)
- Run quick: `make benchmark-quick`
- Run full: `./scripts/bench/run_all_benchmarks.sh`
- Baselines: Compare against v1.5.0 metrics

### Phase 4: Stress Tests (Chaos)
- Run chaos: `rebar3 eunit --module=erlmcp_bench_chaos`
- Run stress: `rebar3 eunit --module=erlmcp_bench_stress`
- Duration: 30s to 24hr tests

---

## Quality Gates

### Mandatory Checks (Before "Done")

1. **Compilation:**
   ```
   ✅ TERM=dumb rebar3 compile (0 errors)
   ⚠️ Warnings: [list if any]
   ```

2. **Tests:**
   ```
   ✅ rebar3 eunit (100% pass rate, 0 failures)
   ✅ rebar3 ct (100% pass rate, 0 failures)
   ⚠️ Skipped: [reasons]
   ```

3. **Coverage:**
   ```
   ✅ rebar3 cover (≥80% coverage)
   ❌ Modules below 80%: [list]
   ```

4. **Dialyzer:**
   ```
   ✅ rebar3 dialyzer (0 type warnings)
   ⚠️ Warnings: [list if any]
   ```

5. **Xref:**
   ```
   ✅ rebar3 xref (0 cross-reference issues)
   ```

6. **Benchmarks (if perf code changed):**
   ```
   ✅ No regression vs baseline (<10% degradation)
   ```

---

## Testing Philosophy

### Chicago School TDD
- **Real processes:** Use actual gen_servers, no mocks
- **State-based:** Verify actual state changes, not behavior
- **No mocks:** Avoid meck, use real dependencies
- **Async verification:** Use timers and assertions for async ops

### OTP Compliance
- **Supervision trees:** Test supervisor restarts
- **gen_server callbacks:** Test all 6 callbacks
- **Trapping exits:** Verify trap_exit flag
- **Monitoring:** Test process monitor cleanup

### Production Readiness
- **Security:** Test path traversal, symlink attacks
- **Performance:** Measure throughput, latency
- **Reliability:** Test crashes, timeouts, errors
- **Scalability:** Test 100K+ connections

---

## Test Data Management

### Fixtures
- **Default capabilities:** Standard server capabilities
- **Sample resources:** Test URIs, handlers
- **Sample tools:** Tool names, schemas, handlers
- **Sample prompts:** Prompt names, arguments, handlers

### Test Data Generation
- Use `proper` for property-based testing
- Generate random valid inputs
- Generate random invalid inputs
- Test edge cases (empty, max size, special chars)

---

## Continuous Integration

### Pre-Commit Hooks
```bash
#!/bin/bash
# .git/hooks/pre-commit

# Compile
rebar3 compile || exit 1

# Run unit tests
rebar3 eunit || exit 1

# Check coverage
rebar3 cover || exit 1

# Dialyzer
rebar3 dialyzer || exit 1

# Xref
rebar3 xref || exit 1
```

### CI Pipeline (GitHub Actions)
```yaml
name: Test erlmcp_server

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Erlang
        uses: erlang-solutions/setup-beam@v1
        with:
          otp-version: '25'
      - name: Install dependencies
        run: rebar3 get-deps
      - name: Compile
        run: rebar3 compile
      - name: Run tests
        run: rebar3 ct
      - name: Check coverage
        run: rebar3 cover
```

---

## Documentation

### Test Documentation
- Each test module has header documentation
- Complex tests have inline comments
- Test names are self-documenting
- Assertions explain what is being tested

### Coverage Reports
- Generate HTML coverage reports: `rebar3 cover`
- Upload to CI artifacts
- Track coverage trends over time
- Set minimum coverage thresholds

---

## Success Criteria

### Functional Requirements
- ✅ All 8 areas tested with ≥80% coverage
- ✅ All tests passing (0 failures)
- ✅ 0 compilation errors
- ✅ 0 Dialyzer warnings (exceptions documented)
- ✅ 0 Xref issues

### Non-Functional Requirements
- ✅ Performance benchmarks meet baselines
- ✅ No memory leaks detected
- ✅ Handles 100+ concurrent connections
- ✅ Graceful error handling verified
- ✅ Security tests pass (path traversal, etc.)

### Documentation Requirements
- ✅ Test strategy document complete
- ✅ Test modules documented
- ✅ Coverage reports generated
- ✅ Benchmark baselines documented

---

## Next Steps

### Immediate Actions (Week 1)
1. Create test module stubs for all 8 areas
2. Implement common test helpers
3. Set up CI pipeline
4. Establish baseline metrics

### Short Term (Weeks 2-4)
1. Implement initialization tests (Area 1)
2. Implement tool execution tests (Area 2)
3. Implement resource management tests (Area 3)
4. Implement prompt handling tests (Area 4)

### Medium Term (Weeks 5-8)
1. Implement batch request tests (Area 5)
2. Implement state management tests (Area 6)
3. Implement cleanup tests (Area 7)
4. Implement performance tests (Area 8)

### Long Term (Weeks 9-12)
1. Run full test suite
2. Generate coverage reports
3. Fix any coverage gaps
4. Document results

---

## Appendix A: Test Case Templates

### EUnit Test Template
```erlang
%% @doc Test description
test_feature_scenario() ->
    %% Setup
    Server = setup_server(),

    %% Exercise
    Result = erlmcp_server:api_call(Server, Args),

    %% Verify
    ?assertMatch(expected, Result),

    %% Cleanup
    erlmcp_server:stop(Server).
```

### Common Test Suite Template
```erlang
%% @doc Test suite for feature
-module(erlmcp_server_feature_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%% Suite callbacks
all() -> [test_scenario1, test_scenario2].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Server} = erlmcp_server:start_link(test, #mcp_server_capabilities{}),
    [{server, Server} | Config].

end_per_testcase(_TestCase, Config) ->
    Server = ?config(server, Config),
    erlmcp_server:stop(Server),
    ok.

%% Test cases
test_scenario1(Config) ->
    Server = ?config(server, Config),
    %% Test logic
    ok.
```

---

## Appendix B: Assertion Library

### Custom Assertions
```erlang
%% @doc Assert server is in expected phase
-define(assertPhase(Server, ExpectedPhase),
    ?assertEqual(ExpectedPhase, get_server_phase(Server))).

%% @doc assert resource exists
-define(assertResourceExists(Server, Uri),
    ?assertMatch({ok, _}, lookup_resource(Server, Uri))).

%% @doc Assert tool returns expected result
-define(assertToolResult(Server, ToolName, Args, ExpectedResult),
    ?assertEqual(ExpectedResult, call_tool_sync(Server, ToolName, Args))).
```

---

**Document Status:** ✅ Complete
**Next Review:** After test implementation
**Owner:** erlang-otp-developer agent
**Reviewers:** erlang-test-engineer, erlang-researcher
