# erlmcp v3 Rewrite - Comprehensive Test Strategy

**Version:** 1.0.0
**Date:** 2026-01-30
**Status:** Planning Phase
**Target Coverage:** 85%+ (Core modules), 80%+ (All modules)

---

## Executive Summary

This document defines the comprehensive testing strategy for the erlmcp v3 rewrite, focusing on **MCP protocol compliance verification through automated tests** using **Chicago School TDD methodology** (state-based testing, real collaborators, minimal mocking).

**Key Principles:**
- **Test-First Development** - Write failing tests before implementation
- **Chicago School TDD** - Real processes, state-based assertions, no mocks
- **MCP Compliance as Tests** - Protocol specification encoded as executable tests
- **Layered Testing** - Unit → Integration → Property → Compliance → Chaos
- **Automated Quality Gates** - Tests must pass before merge

**Deliverables:**
- 300+ test cases covering 113 modules
- 85%+ coverage for core modules (erlmcp_core, erlmcp_json_rpc, erlmcp_client, erlmcp_server, erlmcp_registry)
- 80%+ coverage for all modules
- MCP protocol compliance test suite (50+ test cases)
- Chaos engineering test suite (15+ failure scenarios)
- Automated test execution in CI/CD

---

## 1. Current State Analysis

### 1.1 Existing Test Infrastructure

**Test Files Inventory:**
- **Broken:** 20 test files (`.erl.broken`) - cannot compile
- **Working:** 4 files (gcp_simulator_server, tcps_test_helper, destructive_memory tests)
- **Total Lines:** ~1,611 lines of test code (mostly broken)
- **Coverage:** Unknown (tests don't compile)

**Root Causes of Breakage:**
1. **Compilation Errors** (6 files):
   - `erlmcp_cancellation_tests.erl` - Syntax errors in spawn calls
   - `erlmcp_logging_tests.erl` - Missing Proper include, macro conflicts
   - `erlmcp_transport_compliance_tests.erl` - Missing includes
   - `mcp_compliance/transport_compliance_tests.erl` - Invalid JSON syntax
   - `mcp_compliance/tools_compliance_tests.erl` - Unbound variable
   - `skipped/erlmcp_vm_limits_tests.erl` - Invalid function calls

2. **Dependency Issues:**
   - `ctx v0.6.0` - Source not recognizable (blocks chatterbox, grpcbox)
   - `jobs v0.10.0` - Failed to read .app.src file

3. **Test Duplication:**
   - Tools testing: 3 overlapping suites
   - JSON-RPC testing: 4 overlapping suites
   - Server capabilities: 2 overlapping suites
   - Resources: 2 overlapping suites

**Module Inventory (113 modules):**
- **erlmcp_core:** 52 modules (including client, server, registry, JSON-RPC)
- **erlmcp_transports:** 13 modules (stdio, tcp, http, ws)
- **erlmcp_observability:** 29 modules (metrics, tracing, chaos, receipts)
- **tcps_erlmcp:** 63 modules (optional TCPS quality system)

### 1.2 Gap Analysis

**Critical Gaps:**
1. **No MCP protocol compliance tests** - Cannot verify specification adherence
2. **No property-based tests** - Missing invariant testing for JSON-RPC encoding
3. **No integration tests** - Multi-process scenarios untested
4. **No chaos tests** - Fault injection and resilience untested
5. **Coverage unknown** - Tests can't compile, no baseline metrics

**Testing Debt Estimation:**
- **Fixing broken tests:** 8-16 hours
- **Writing new tests:** 80-120 hours (300+ test cases)
- **Infrastructure setup:** 8-16 hours
- **Total:** 96-152 hours (12-19 days)

---

## 2. Test Layer Architecture

### 2.1 Five-Layer Testing Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                    5. CHAOS LAYER                           │
│  Fault injection, network partitions, resource exhaustion   │
│  Tools: Custom chaos modules, destructive tests             │
│  Coverage: 15+ failure scenarios                            │
└─────────────────────────────────────────────────────────────┘
                             ▲
┌─────────────────────────────────────────────────────────────┐
│                 4. COMPLIANCE LAYER                         │
│  MCP 2025-11-25 specification compliance verification       │
│  Tools: Common Test suites, protocol test harness          │
│  Coverage: 50+ MCP protocol test cases                      │
└─────────────────────────────────────────────────────────────┘
                             ▲
┌─────────────────────────────────────────────────────────────┐
│                  3. PROPERTY LAYER                          │
│  Invariant testing, generative testing, QuickCheck-style    │
│  Tools: Proper (property-based testing)                     │
│  Coverage: 20+ properties (JSON-RPC, state machines)        │
└─────────────────────────────────────────────────────────────┘
                             ▲
┌─────────────────────────────────────────────────────────────┐
│                2. INTEGRATION LAYER                         │
│  Multi-process, multi-transport, registry coordination      │
│  Tools: Common Test (CT)                                    │
│  Coverage: 40+ integration scenarios                        │
└─────────────────────────────────────────────────────────────┘
                             ▲
┌─────────────────────────────────────────────────────────────┐
│                    1. UNIT LAYER                            │
│  Module-level, function-level, API surface testing          │
│  Tools: EUnit                                               │
│  Coverage: 200+ test cases                                  │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Chicago School TDD Principles

**Core Tenets:**
1. **State-Based Verification** - Assert on observable state changes, not method calls
2. **Real Collaborators** - Use actual gen_servers, processes, transports (no mocks)
3. **Behavior Testing** - Test what system does (outputs), not how it does it (internals)
4. **Integration by Default** - Test components together whenever practical

**Anti-Patterns to Avoid (London School):**
- ❌ Mocking gen_server calls with `meck`
- ❌ Verifying internal method call counts
- ❌ Interaction testing (which functions were called)
- ❌ Stubbing collaborators unnecessarily

**Chicago School Example:**
```erlang
%% GOOD: Chicago School (state-based, real gen_server)
basic_registry_test() ->
    %% Setup: Start real registry
    application:ensure_all_started(erlmcp_core),

    %% Exercise: Register real server process
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_server("server1", ServerPid, #{}),

    %% Verify: Observable state (registry lookup returns Pid)
    {ok, ServerPid} = erlmcp_registry:whereis_server("server1"),

    %% Cleanup
    ServerPid ! stop.

%% BAD: London School (interaction verification, mocking)
%% DON'T DO THIS:
%%   meck:new(erlmcp_registry),
%%   meck:expect(erlmcp_registry, register_server, fun(...) -> ok end),
%%   ?assertEqual(1, meck:num_calls(erlmcp_registry, register_server, '_')).
```

### 2.3 Test Layer Mapping

| Layer | Tool | File Pattern | Scope | Example |
|-------|------|--------------|-------|---------|
| **Unit** | EUnit | `*_tests.erl` | Single module | `erlmcp_json_rpc_tests.erl` |
| **Integration** | Common Test | `*_SUITE.erl` | Multi-module | `mcp_client_server_SUITE.erl` |
| **Property** | Proper | `*_prop.erl` | Invariants | `erlmcp_json_rpc_prop.erl` |
| **Compliance** | Common Test | `mcp_compliance_*.erl` | MCP spec | `mcp_compliance_resources_SUITE.erl` |
| **Chaos** | Custom | `chaos_*.erl` | Fault injection | `chaos_network_partition_test.erl` |

---

## 3. MCP Protocol Compliance Tests

### 3.1 Critical MCP Test Scenarios

**MCP 2025-11-25 Specification Coverage:**

#### 3.1.1 JSON-RPC 2.0 Compliance (15 tests)
```erlang
%% test/compliance/mcp_jsonrpc_compliance_SUITE.erl
all() -> [
    %% Message Structure
    request_must_have_jsonrpc_field_test,
    request_must_have_method_field_test,
    request_must_have_id_field_test,
    response_must_have_result_or_error_test,
    notification_must_not_have_id_test,

    %% ID Correlation
    response_id_matches_request_id_test,
    error_response_id_matches_request_id_test,
    batch_request_responses_match_ids_test,

    %% Error Codes
    parse_error_minus_32700_test,
    invalid_request_minus_32600_test,
    method_not_found_minus_32601_test,
    invalid_params_minus_32602_test,
    internal_error_minus_32603_test,

    %% Encoding
    utf8_encoding_test,
    binary_data_encoding_test
].
```

#### 3.1.2 Capability Negotiation (10 tests)
```erlang
%% test/compliance/mcp_capabilities_SUITE.erl
all() -> [
    %% Initialize Handshake
    initialize_must_be_first_message_test,
    initialize_sends_protocol_version_test,
    initialize_sends_capabilities_test,
    initialize_returns_server_capabilities_test,
    initialize_fails_if_version_mismatch_test,

    %% Capability Enforcement
    resources_list_requires_resources_capability_test,
    tools_call_requires_tools_capability_test,
    prompts_get_requires_prompts_capability_test,

    %% Capability Changes
    capability_change_notification_test,
    disabled_capability_returns_error_test
].
```

#### 3.1.3 Resources System (12 tests)
```erlang
%% test/compliance/mcp_resources_SUITE.erl
all() -> [
    %% Resource Listing
    resources_list_returns_array_test,
    resources_list_includes_uri_test,
    resources_list_includes_name_test,
    resources_list_includes_mime_type_test,

    %% Resource Reading
    resources_read_returns_content_test,
    resources_read_supports_templates_test,
    resources_read_uri_not_found_returns_error_test,

    %% Resource Subscriptions
    resources_subscribe_enables_updates_test,
    resources_unsubscribe_disables_updates_test,
    resources_updated_notification_sent_on_change_test,
    resources_list_changed_notification_test,

    %% Resource Templates
    resource_template_uri_pattern_matching_test
].
```

#### 3.1.4 Tools System (10 tests)
```erlang
%% test/compliance/mcp_tools_SUITE.erl
all() -> [
    %% Tool Listing
    tools_list_returns_array_test,
    tools_list_includes_name_test,
    tools_list_includes_description_test,
    tools_list_includes_schema_test,

    %% Tool Execution
    tools_call_executes_handler_test,
    tools_call_validates_schema_test,
    tools_call_returns_result_test,
    tools_call_handles_errors_test,

    %% Tool Changes
    tools_list_changed_notification_test,
    tool_not_found_returns_error_test
].
```

#### 3.1.5 Prompts System (8 tests)
```erlang
%% test/compliance/mcp_prompts_SUITE.erl
all() -> [
    %% Prompt Listing
    prompts_list_returns_array_test,
    prompts_list_includes_name_test,
    prompts_list_includes_description_test,

    %% Prompt Retrieval
    prompts_get_returns_messages_test,
    prompts_get_supports_arguments_test,
    prompts_get_validates_required_arguments_test,

    %% Prompt Changes
    prompts_list_changed_notification_test,
    prompt_not_found_returns_error_test
].
```

#### 3.1.6 Transport Compliance (5 tests)
```erlang
%% test/compliance/mcp_transports_SUITE.erl
all() -> [
    stdio_transport_line_delimited_test,
    tcp_transport_framing_test,
    http_transport_headers_test,
    ws_transport_binary_frames_test,
    transport_disconnection_cleanup_test
].
```

**Total MCP Compliance Tests:** 60 test cases

### 3.2 Compliance Test Organization

```
test/
├── compliance/
│   ├── mcp_compliance_harness.erl        # Shared test harness
│   ├── mcp_jsonrpc_compliance_SUITE.erl  # JSON-RPC 2.0 (15 tests)
│   ├── mcp_capabilities_SUITE.erl        # Capability negotiation (10 tests)
│   ├── mcp_resources_SUITE.erl           # Resources system (12 tests)
│   ├── mcp_tools_SUITE.erl               # Tools system (10 tests)
│   ├── mcp_prompts_SUITE.erl             # Prompts system (8 tests)
│   └── mcp_transports_SUITE.erl          # Transport compliance (5 tests)
└── compliance/fixtures/
    ├── valid_initialize_request.json
    ├── invalid_method_request.json
    ├── batch_requests.json
    └── ... (20+ JSON fixtures)
```

---

## 4. Test-First Development Approach (Chicago School TDD)

### 4.1 TDD Workflow for v3 Rewrite

**Red → Green → Refactor Cycle:**

```
┌─────────────────────────────────────────────────────────────┐
│ PHASE 1: RED (Write Failing Test)                          │
├─────────────────────────────────────────────────────────────┤
│ 1. Read MCP spec section (e.g., "initialize handshake")    │
│ 2. Write test that verifies spec requirement               │
│ 3. Use REAL processes (spawn gen_servers, not mocks)       │
│ 4. Assert on OBSERVABLE STATE (API results, not internals) │
│ 5. Run test → Expect FAILURE (module not implemented yet)  │
└─────────────────────────────────────────────────────────────┘
                             ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 2: GREEN (Make Test Pass)                            │
├─────────────────────────────────────────────────────────────┤
│ 1. Implement MINIMUM code to make test pass                │
│ 2. Use REAL collaborators (call erlmcp_registry, not mock) │
│ 3. Run test → Expect SUCCESS                               │
│ 4. No mocks unless testing external I/O                    │
└─────────────────────────────────────────────────────────────┘
                             ↓
┌─────────────────────────────────────────────────────────────┐
│ PHASE 3: REFACTOR (Improve Design)                         │
├─────────────────────────────────────────────────────────────┤
│ 1. Refactor implementation (extract functions, improve)    │
│ 2. Keep tests GREEN (no behavior changes)                  │
│ 3. Tests use real system, so refactoring is safe           │
│ 4. Run tests → All still passing                           │
└─────────────────────────────────────────────────────────────┘
                             ↓
                        (Repeat)
```

### 4.2 Example: TDD for erlmcp_json_rpc Module

**Step 1: RED - Write failing test**
```erlang
-module(erlmcp_json_rpc_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: JSON-RPC request encoding must include jsonrpc field
encode_request_includes_jsonrpc_field_test() ->
    %% Exercise: Encode request
    Request = #{
        id => 1,
        method => <<"initialize">>,
        params => #{}
    },
    Encoded = erlmcp_json_rpc:encode_request(Request),

    %% Verify: Decoded message has jsonrpc field (observable state)
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>}, Decoded).

%% Run: rebar3 eunit --module=erlmcp_json_rpc_tests
%% Expected: FAIL (module erlmcp_json_rpc doesn't exist yet)
```

**Step 2: GREEN - Implement minimum code**
```erlang
-module(erlmcp_json_rpc).
-export([encode_request/1]).

encode_request(Request) ->
    Message = Request#{
        <<"jsonrpc">> => <<"2.0">>
    },
    jsx:encode(Message).

%% Run: rebar3 eunit --module=erlmcp_json_rpc_tests
%% Expected: PASS ✅
```

**Step 3: REFACTOR - Improve design**
```erlang
-module(erlmcp_json_rpc).
-export([encode_request/1]).

%% Refactor: Add validation helper
encode_request(Request) ->
    validate_request(Request),
    Message = add_jsonrpc_version(Request),
    jsx:encode(Message).

validate_request(#{id := _, method := _}) -> ok;
validate_request(_) -> error(invalid_request).

add_jsonrpc_version(Req) ->
    Req#{<<"jsonrpc">> => <<"2.0">>}.

%% Run: rebar3 eunit --module=erlmcp_json_rpc_tests
%% Expected: PASS ✅ (tests still green after refactor)
```

### 4.3 Chicago School Test Patterns for erlmcp

#### Pattern 1: Testing gen_server with Real Processes
```erlang
%% DON'T: Mock gen_server (London School)
%%   meck:expect(erlmcp_server, add_tool, fun(...) -> ok end).

%% DO: Spawn real gen_server (Chicago School)
test_add_tool() ->
    {ok, Pid} = erlmcp_server:start_link([]),
    ok = erlmcp_server:add_tool(Pid, "calculator", fun add_handler/1, #{}),

    %% Verify observable state (tools list includes new tool)
    {ok, Tools} = erlmcp_server:list_tools(Pid),
    ?assert(lists:member("calculator", [Name || #{name := Name} <- Tools])),

    erlmcp_server:stop(Pid).
```

#### Pattern 2: Testing Registry with Real Coordination
```erlang
%% DON'T: Mock registry lookups
%%   meck:expect(erlmcp_registry, whereis_server, fun(_) -> {ok, fake_pid} end).

%% DO: Use real registry with real processes
test_registry_coordination() ->
    application:ensure_all_started(erlmcp_core),  % Real registry

    %% Register real server
    ServerPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_server("srv1", ServerPid, #{}),

    %% Verify real lookup (observable behavior)
    {ok, ServerPid} = erlmcp_registry:whereis_server("srv1"),

    %% Cleanup
    ServerPid ! stop.
```

#### Pattern 3: Testing Transport with Real I/O
```erlang
%% DO: Use real TCP sockets for integration tests
test_tcp_transport_integration() ->
    %% Start real TCP listener
    {ok, _} = ranch:start_listener(test_tcp, ranch_tcp,
                                    #{port => 0},
                                    erlmcp_transport_tcp, []),
    {ok, Port} = ranch:get_port(test_tcp),

    %% Connect real client
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary]),

    %% Send real message
    Message = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>}),
    ok = gen_tcp:send(Socket, [Message, $\n]),

    %% Verify real response (observable behavior)
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>}, jsx:decode(Response)),

    %% Cleanup
    gen_tcp:close(Socket),
    ranch:stop_listener(test_tcp).
```

---

## 5. Module-by-Module Test Plan

### 5.1 Core Modules (erlmcp_core) - 85%+ Coverage Target

#### 5.1.1 erlmcp_json_rpc.erl (Critical - 95%+ coverage)
**Purpose:** JSON-RPC 2.0 encoding/decoding

**Test File:** `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`

**Test Cases (25 tests):**
1. **Encoding (10 tests)**:
   - `encode_request_includes_jsonrpc_field_test`
   - `encode_request_includes_id_test`
   - `encode_request_includes_method_test`
   - `encode_request_includes_params_test`
   - `encode_response_includes_result_test`
   - `encode_error_includes_code_and_message_test`
   - `encode_notification_omits_id_test`
   - `encode_batch_request_test`
   - `encode_handles_binary_ids_test`
   - `encode_handles_null_ids_test`

2. **Decoding (10 tests)**:
   - `decode_valid_request_test`
   - `decode_valid_response_test`
   - `decode_error_response_test`
   - `decode_notification_test`
   - `decode_batch_test`
   - `decode_invalid_json_returns_error_test`
   - `decode_missing_jsonrpc_field_returns_error_test`
   - `decode_invalid_id_type_returns_error_test`
   - `decode_handles_utf8_test`
   - `decode_large_payload_test`

3. **Roundtrip (5 tests)**:
   - `request_roundtrip_test`
   - `response_roundtrip_test`
   - `error_roundtrip_test`
   - `notification_roundtrip_test`
   - `batch_roundtrip_test`

**Property Tests (3 properties):**
```erlang
%% apps/erlmcp_core/test/erlmcp_json_rpc_prop.erl
prop_request_roundtrip() ->
    ?FORALL({Id, Method, Params}, {json_rpc_id(), method(), params()},
        begin
            Request = #{id => Id, method => Method, params => Params},
            Encoded = erlmcp_json_rpc:encode_request(Request),
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),
            Decoded =:= Request
        end).

prop_response_roundtrip() -> ...
prop_error_code_in_range() -> ...
```

**Acceptance Criteria:**
- ✅ All 25 tests pass
- ✅ 3 properties hold (100 test cases each via Proper)
- ✅ 95%+ code coverage
- ✅ No dialyzer warnings

---

#### 5.1.2 erlmcp_client.erl (Critical - 90%+ coverage)
**Purpose:** MCP client gen_server with request correlation

**Test File:** `apps/erlmcp_core/test/erlmcp_client_tests.erl`

**Test Cases (30 tests):**
1. **Initialization (5 tests)**:
   - `start_link_spawns_process_test`
   - `initialize_handshake_sends_version_test`
   - `initialize_handshake_sends_capabilities_test`
   - `initialize_handshake_receives_server_capabilities_test`
   - `initialize_handshake_timeout_returns_error_test`

2. **Request Correlation (8 tests)**:
   - `request_id_increments_test`
   - `response_matches_request_id_test`
   - `pending_requests_tracked_test`
   - `response_completes_pending_request_test`
   - `timeout_clears_pending_request_test`
   - `multiple_concurrent_requests_test`
   - `out_of_order_responses_handled_test`
   - `duplicate_response_id_ignored_test`

3. **Resource Operations (6 tests)**:
   - `list_resources_sends_request_test`
   - `read_resource_sends_uri_test`
   - `subscribe_resource_enables_notifications_test`
   - `unsubscribe_resource_disables_notifications_test`
   - `resource_updated_notification_handled_test`
   - `resource_not_found_returns_error_test`

4. **Tool Operations (5 tests)**:
   - `list_tools_returns_array_test`
   - `call_tool_sends_arguments_test`
   - `call_tool_returns_result_test`
   - `call_tool_invalid_params_returns_error_test`
   - `call_tool_timeout_returns_error_test`

5. **Prompt Operations (3 tests)**:
   - `list_prompts_returns_array_test`
   - `get_prompt_sends_arguments_test`
   - `get_prompt_returns_messages_test`

6. **Error Handling (3 tests)**:
   - `transport_disconnect_returns_error_test`
   - `invalid_json_response_handled_test`
   - `server_error_propagated_to_caller_test`

**Integration Tests (Common Test):**
```erlang
%% apps/erlmcp_core/test/erlmcp_client_integration_SUITE.erl
all() -> [
    client_server_full_lifecycle_test,
    client_reconnection_after_disconnect_test,
    client_handles_server_restart_test,
    multiple_clients_same_server_test
].
```

**Acceptance Criteria:**
- ✅ All 30 unit tests pass
- ✅ 4 integration tests pass
- ✅ 90%+ code coverage
- ✅ Request correlation tested under concurrency

---

#### 5.1.3 erlmcp_server.erl (Critical - 90%+ coverage)
**Purpose:** MCP server gen_server with resource/tool/prompt management

**Test File:** `apps/erlmcp_core/test/erlmcp_server_tests.erl`

**Test Cases (35 tests):**
1. **Resource Management (12 tests)**:
   - `add_resource_stores_handler_test`
   - `add_resource_duplicate_uri_returns_error_test`
   - `list_resources_returns_all_test`
   - `read_resource_calls_handler_test`
   - `read_resource_nonexistent_returns_error_test`
   - `remove_resource_deletes_handler_test`
   - `subscribe_resource_tracks_subscriber_test`
   - `unsubscribe_resource_removes_subscriber_test`
   - `notify_resource_update_sends_notification_test`
   - `resource_template_uri_matching_test`
   - `resource_handler_exception_returns_error_test`
   - `resource_change_notification_multiple_subscribers_test`

2. **Tool Management (10 tests)**:
   - `add_tool_stores_handler_test`
   - `add_tool_with_schema_validates_test`
   - `list_tools_returns_all_test`
   - `call_tool_executes_handler_test`
   - `call_tool_validates_schema_test`
   - `call_tool_nonexistent_returns_error_test`
   - `call_tool_invalid_args_returns_error_test`
   - `remove_tool_deletes_handler_test`
   - `tool_handler_exception_returns_error_test`
   - `tool_list_changed_notification_test`

3. **Prompt Management (8 tests)**:
   - `add_prompt_stores_template_test`
   - `list_prompts_returns_all_test`
   - `get_prompt_calls_template_test`
   - `get_prompt_with_arguments_test`
   - `get_prompt_required_argument_missing_returns_error_test`
   - `get_prompt_nonexistent_returns_error_test`
   - `remove_prompt_deletes_template_test`
   - `prompt_list_changed_notification_test`

4. **Capability Management (5 tests)**:
   - `get_capabilities_returns_enabled_features_test`
   - `update_capabilities_changes_features_test`
   - `disabled_capability_rejects_requests_test`
   - `capability_negotiation_in_initialize_test`
   - `capability_change_notification_test`

**Acceptance Criteria:**
- ✅ All 35 unit tests pass
- ✅ 90%+ code coverage
- ✅ All resource/tool/prompt operations tested
- ✅ Schema validation tested

---

#### 5.1.4 erlmcp_registry.erl (Critical - 90%+ coverage)
**Purpose:** gproc-based process registration and message routing

**Test File:** `apps/erlmcp_core/test/erlmcp_registry_tests.erl`

**Test Cases (25 tests):**
1. **Server Registration (8 tests)**:
   - `register_server_stores_pid_test`
   - `register_server_duplicate_id_returns_error_test`
   - `whereis_server_returns_pid_test`
   - `whereis_server_nonexistent_returns_error_test`
   - `unregister_server_removes_registration_test`
   - `server_death_auto_unregisters_test`
   - `list_servers_returns_all_test`
   - `concurrent_server_registration_test`

2. **Transport Registration (8 tests)**:
   - `register_transport_stores_pid_test`
   - `register_transport_duplicate_id_returns_error_test`
   - `whereis_transport_returns_pid_test`
   - `bind_server_transport_links_test`
   - `route_message_to_server_test`
   - `route_message_to_transport_test`
   - `transport_death_cleans_up_bindings_test`
   - `concurrent_transport_registration_test`

3. **Message Routing (5 tests)**:
   - `route_to_server_delivers_message_test`
   - `route_to_nonexistent_server_returns_error_test`
   - `route_to_transport_delivers_message_test`
   - `routing_handles_process_death_test`
   - `routing_load_test_1000_messages_test`

4. **Cleanup (4 tests)**:
   - `process_death_triggers_cleanup_test`
   - `cleanup_removes_all_bindings_test`
   - `cleanup_notifies_connected_processes_test`
   - `registry_restart_clears_state_test`

**Integration Tests:**
```erlang
%% apps/erlmcp_core/test/erlmcp_registry_integration_SUITE.erl
all() -> [
    full_routing_lifecycle_test,
    server_transport_binding_test,
    multiple_servers_multiple_transports_test,
    registry_restart_recovery_test
].
```

**Acceptance Criteria:**
- ✅ All 25 unit tests pass
- ✅ 4 integration tests pass
- ✅ 90%+ code coverage
- ✅ Concurrency safety verified (1000+ concurrent registrations)

---

### 5.2 Transport Modules (erlmcp_transports) - 80%+ Coverage Target

#### 5.2.1 erlmcp_transport_stdio.erl (85%+ coverage)
**Test File:** `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Test Cases:** 20 tests (line-based framing, stdin/stdout handling, test mode)

#### 5.2.2 erlmcp_transport_tcp.erl (85%+ coverage)
**Test File:** `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`
**Test Cases:** 25 tests (ranch integration, socket management, reconnection)

#### 5.2.3 erlmcp_transport_http.erl (85%+ coverage)
**Test File:** `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
**Test Cases:** 25 tests (gun integration, HTTP/2, connection pooling)

#### 5.2.4 erlmcp_transport_ws.erl (80%+ coverage)
**Test File:** `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Test Cases:** 20 tests (WebSocket frames, binary/text mode, ping/pong)

---

### 5.3 Observability Modules (erlmcp_observability) - 75%+ Coverage Target

#### 5.3.1 erlmcp_metrics.erl (80%+ coverage)
**Test File:** `apps/erlmcp_observability/test/erlmcp_metrics_tests.erl`
**Test Cases:** 15 tests (counter, gauge, histogram, metric collection)

#### 5.3.2 erlmcp_otel.erl (75%+ coverage)
**Test File:** `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`
**Test Cases:** 12 tests (span creation, trace propagation, exporter integration)

#### 5.3.3 erlmcp_chaos.erl (80%+ coverage)
**Test File:** `apps/erlmcp_observability/test/erlmcp_chaos_tests.erl`
**Test Cases:** 18 tests (fault injection, network chaos, resource exhaustion)

---

### 5.4 Test Count Summary

| Module Category | Modules | Unit Tests | Integration Tests | Property Tests | Total |
|----------------|---------|-----------|------------------|----------------|-------|
| **Core (Critical)** | 5 | 140 | 16 | 15 | 171 |
| **Core (Other)** | 47 | 94 | 10 | 5 | 109 |
| **Transports** | 13 | 90 | 12 | 8 | 110 |
| **Observability** | 29 | 60 | 8 | 4 | 72 |
| **TCPS (Optional)** | 63 | 50 | 6 | 2 | 58 |
| **TOTAL** | **157** | **434** | **52** | **34** | **520** |

---

## 6. Chaos/Fault Injection Testing Strategy

### 6.1 Chaos Engineering Principles

**Goal:** Verify system resilience under failure conditions

**Bounded Refusal Philosophy:**
- System MUST refuse to degrade beyond safe limits
- Fail-fast when limits exceeded
- Recovery MUST complete within 5 seconds
- No silent corruption (errors are explicit)

### 6.2 Chaos Test Scenarios (15 scenarios)

```erlang
%% test/chaos/erlmcp_chaos_SUITE.erl
all() -> [
    %% Process Failures
    chaos_registry_process_death_test,
    chaos_server_crash_during_request_test,
    chaos_client_crash_during_response_test,
    chaos_transport_crash_during_send_test,
    chaos_supervisor_restart_cascade_test,

    %% Resource Exhaustion
    chaos_memory_exhaustion_test,
    chaos_process_limit_exhaustion_test,
    chaos_port_limit_exhaustion_test,
    chaos_ets_table_limit_test,

    %% Network Failures
    chaos_network_partition_test,
    chaos_slow_network_timeout_test,
    chaos_tcp_connection_refused_test,

    %% Data Corruption
    chaos_invalid_json_injection_test,
    chaos_truncated_message_test,

    %% Timing Issues
    chaos_request_timeout_storm_test
].
```

### 6.3 Example Chaos Test

```erlang
%% Test: System recovers when registry crashes
chaos_registry_process_death_test(_Config) ->
    %% Setup: Start system
    application:ensure_all_started(erlmcp_core),
    {ok, Server} = erlmcp_server:start_link([]),
    ok = erlmcp_registry:register_server("srv1", Server, #{}),

    %% Chaos: Kill registry process
    RegistryPid = whereis(erlmcp_registry),
    exit(RegistryPid, kill),

    %% Wait for supervisor restart
    timer:sleep(100),

    %% Verify: Registry restarted and operational
    NewRegistryPid = whereis(erlmcp_registry),
    ?assert(is_pid(NewRegistryPid)),
    ?assertNotEqual(RegistryPid, NewRegistryPid),

    %% Verify: Can register again (state clean)
    ok = erlmcp_registry:register_server("srv2", Server, #{}),
    {ok, Server} = erlmcp_registry:whereis_server("srv2").
```

### 6.4 Chaos Test Organization

```
test/chaos/
├── erlmcp_chaos_SUITE.erl              # Main chaos test suite
├── chaos_process_failures_SUITE.erl    # Process death scenarios
├── chaos_resource_exhaustion_SUITE.erl # Memory/process limits
├── chaos_network_SUITE.erl             # Network partition tests
└── chaos_timing_SUITE.erl              # Timeout/race conditions
```

---

## 7. Test Coverage Targets (Refined for v3)

### 7.1 Coverage Requirements by Module Type

| Module Type | Coverage Target | Rationale |
|------------|----------------|-----------|
| **Critical Core** (json_rpc, client, server, registry) | **90-95%** | Protocol compliance essential |
| **Core Infrastructure** (supervision, session, routing) | **85-90%** | System stability critical |
| **Transports** (stdio, tcp, http, ws) | **80-85%** | Well-tested libraries used |
| **Observability** (metrics, tracing, chaos) | **75-80%** | Nice-to-have features |
| **TCPS** (optional quality system) | **70-75%** | Optional component |
| **Overall Target** | **≥80%** | Industry standard for production |

### 7.2 Coverage Exemptions

**Exclude from coverage:**
- Generated code (e.g., protobuf bindings)
- Debug-only code (`ifdef(DEBUG)`)
- Example/demo modules
- Deprecated functions marked for removal

**Example:**
```erlang
-ifndef(COVERAGE).
debug_print(Msg) ->
    io:format("[DEBUG] ~p~n", [Msg]).
-endif.
```

### 7.3 Coverage Enforcement

**Pre-Commit Hook:**
```bash
#!/bin/bash
# .git/hooks/pre-commit
rebar3 eunit
rebar3 cover --verbose

# Parse coverage report
COVERAGE=$(grep -oP 'Total coverage: \K[0-9]+' _build/test/cover/index.txt)

if [ "$COVERAGE" -lt 80 ]; then
    echo "❌ Coverage $COVERAGE% < 80% (BLOCKED)"
    exit 1
else
    echo "✅ Coverage $COVERAGE% ≥ 80%"
fi
```

**CI/CD Gate:**
```yaml
# .github/workflows/test.yml
- name: Run tests with coverage
  run: rebar3 do eunit, ct, proper -c, cover --verbose

- name: Check coverage threshold
  run: |
    COVERAGE=$(grep -oP 'Total: \K[0-9]+' _build/test/cover/index.html)
    if [ "$COVERAGE" -lt 80 ]; then
      echo "Coverage $COVERAGE% below 80% threshold"
      exit 1
    fi
```

---

## 8. Test Organization and Directory Structure

### 8.1 Proposed Test Directory Structure

```
test/
├── README.md                          # Test suite overview
├── test_utils.erl                     # Shared test utilities
│
├── unit/                              # EUnit tests (200+ tests)
│   ├── erlmcp_json_rpc_tests.erl
│   ├── erlmcp_client_tests.erl
│   ├── erlmcp_server_tests.erl
│   ├── erlmcp_registry_tests.erl
│   └── ... (one test file per module)
│
├── integration/                       # Common Test (40+ tests)
│   ├── mcp_client_server_SUITE.erl
│   ├── mcp_transport_integration_SUITE.erl
│   ├── mcp_registry_coordination_SUITE.erl
│   ├── mcp_full_lifecycle_SUITE.erl
│   └── fixtures/
│       ├── sample_requests.json
│       └── sample_responses.json
│
├── properties/                        # Proper tests (20+ properties)
│   ├── erlmcp_json_rpc_prop.erl
│   ├── erlmcp_transport_prop.erl
│   ├── erlmcp_state_machine_prop.erl
│   └── property_generators.erl
│
├── compliance/                        # MCP spec tests (60+ tests)
│   ├── README.md                      # MCP 2025-11-25 spec mapping
│   ├── mcp_compliance_harness.erl     # Shared test infrastructure
│   ├── mcp_jsonrpc_compliance_SUITE.erl
│   ├── mcp_capabilities_SUITE.erl
│   ├── mcp_resources_SUITE.erl
│   ├── mcp_tools_SUITE.erl
│   ├── mcp_prompts_SUITE.erl
│   ├── mcp_transports_SUITE.erl
│   └── fixtures/
│       ├── valid_initialize.json
│       ├── invalid_method.json
│       └── ... (30+ JSON fixtures)
│
├── chaos/                             # Chaos tests (15+ scenarios)
│   ├── README.md                      # Chaos engineering methodology
│   ├── erlmcp_chaos_SUITE.erl
│   ├── chaos_process_failures_SUITE.erl
│   ├── chaos_resource_exhaustion_SUITE.erl
│   ├── chaos_network_SUITE.erl
│   └── chaos_timing_SUITE.erl
│
├── helpers/                           # Test helpers (not tests)
│   ├── mock_transport.erl             # Lightweight transport mock
│   ├── mock_server.erl                # Test server implementation
│   ├── test_harness.erl               # Setup/teardown helpers
│   └── assertion_helpers.erl          # Custom assertions
│
└── broken/                            # Archive broken tests (20 files)
    └── ... (move .erl.broken files here for reference)
```

### 8.2 Test File Naming Conventions

| Test Type | Pattern | Example |
|-----------|---------|---------|
| **Unit** | `<module>_tests.erl` | `erlmcp_client_tests.erl` |
| **Integration** | `<feature>_SUITE.erl` | `mcp_client_server_SUITE.erl` |
| **Property** | `<module>_prop.erl` | `erlmcp_json_rpc_prop.erl` |
| **Compliance** | `mcp_<feature>_compliance_SUITE.erl` | `mcp_resources_compliance_SUITE.erl` |
| **Chaos** | `chaos_<category>_SUITE.erl` | `chaos_network_SUITE.erl` |

### 8.3 Test Function Naming Conventions

```erlang
%% Pattern: <what>_<condition>_<expected_result>_test
subscribe_to_nonexistent_resource_returns_error_test() -> ...

%% Examples:
initialize_handshake_sends_protocol_version_test() -> ...
call_tool_with_invalid_params_returns_error_test() -> ...
registry_crash_triggers_supervisor_restart_test() -> ...
concurrent_requests_return_correct_responses_test() -> ...
```

---

## 9. Tooling and Infrastructure

### 9.1 Test Execution Commands

```bash
# Individual test types
rebar3 eunit                          # All EUnit tests
rebar3 eunit --module=<module>_tests  # Single module
rebar3 ct                             # All Common Test suites
rebar3 ct --suite=<suite>             # Single CT suite
rebar3 proper -c                      # Property-based tests
rebar3 cover --verbose                # Coverage report

# Full test suite (recommended for CI/CD)
rebar3 do eunit, ct, proper -c, cover --verbose

# Specific test layers
rebar3 eunit --dir=test/unit
rebar3 ct --dir=test/integration
rebar3 ct --dir=test/compliance
rebar3 ct --dir=test/chaos

# Coverage HTML report
rebar3 cover
open _build/test/cover/index.html
```

### 9.2 rebar.config Test Profile

```erlang
%% rebar.config
{profiles, [
    {test, [
        {deps, [
            {proper, "1.4.0"},      % Property-based testing
            {meck, "0.9.2"},        % Mocking (use sparingly)
            {coveralls, "2.2.0"}    % Coverage reporting
        ]},
        {erl_opts, [
            debug_info,
            export_all,             % Export all functions for testing
            nowarn_export_all,
            {d, 'TEST'}             % Define TEST macro
        ]},
        {cover_enabled, true},
        {cover_export_enabled, true},
        {cover_opts, [verbose]},
        {eunit_opts, [
            verbose,
            {report, {eunit_progress, [colored]}}
        ]},
        {ct_opts, [
            {sys_config, "config/test.config"},
            {logdir, "_build/test/logs"}
        ]}
    ]}
]}.
```

### 9.3 Test Utilities Module

```erlang
-module(test_utils).
-export([
    %% Setup/teardown
    start_test_env/0,
    stop_test_env/1,
    setup_registry/0,

    %% Mock processes
    spawn_mock_server/0,
    spawn_mock_transport/0,
    message_collector/0,

    %% Assertions
    assert_message_received/2,
    assert_process_alive/1,
    assert_ets_entry_exists/2,

    %% Fixtures
    load_json_fixture/1,
    create_test_server/0,
    create_test_client/0
]).

%% Chicago School: Setup real system
start_test_env() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports).

%% Chicago School: Spawn real mock server (not meck)
spawn_mock_server() ->
    spawn(fun() ->
        receive
            {call, From, Request} ->
                Response = handle_request(Request),
                From ! {response, Response},
                mock_server_loop()
        end
    end).
```

### 9.4 CI/CD Integration

```yaml
# .github/workflows/test.yml
name: Test Suite

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25'
          rebar3-version: '3.22'

      - name: Compile
        run: rebar3 compile

      - name: Run EUnit tests
        run: rebar3 eunit

      - name: Run Common Test suites
        run: rebar3 ct

      - name: Run Property tests
        run: rebar3 proper -c

      - name: Generate coverage report
        run: rebar3 cover --verbose

      - name: Check coverage threshold
        run: |
          COVERAGE=$(grep -oP 'Total: \K[0-9]+' _build/test/cover/index.html)
          if [ "$COVERAGE" -lt 80 ]; then
            echo "❌ Coverage $COVERAGE% < 80%"
            exit 1
          fi
          echo "✅ Coverage $COVERAGE% ≥ 80%"

      - name: Upload coverage to Coveralls
        run: rebar3 as test coveralls send
        env:
          COVERALLS_REPO_TOKEN: ${{ secrets.COVERALLS_TOKEN }}

      - name: Run chaos tests (nightly only)
        if: github.event_name == 'schedule'
        run: rebar3 ct --dir=test/chaos
```

---

## 10. Quality Gates and Enforcement

### 10.1 Pre-Merge Quality Gates

**MANDATORY gates before PR merge:**

```
┌─────────────────────────────────────────────────┐
│ GATE 1: Compilation                             │
│ ✅ rebar3 compile (0 errors)                    │
│ ⚠️  Warnings allowed (but reviewed)             │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ GATE 2: Tests                                   │
│ ✅ rebar3 eunit (100% pass)                     │
│ ✅ rebar3 ct (100% pass)                        │
│ ✅ rebar3 proper -c (all properties hold)       │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ GATE 3: Coverage                                │
│ ✅ Overall ≥80%                                 │
│ ✅ Core modules ≥85%                            │
│ ✅ Critical modules ≥90%                        │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ GATE 4: Code Quality                            │
│ ✅ rebar3 dialyzer (0 errors)                   │
│ ✅ rebar3 xref (0 undefined calls)              │
│ ⚠️  rebar3 format --verify (reviewed)           │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ GATE 5: MCP Compliance (v3 specific)            │
│ ✅ test/compliance/* (all pass)                 │
│ ✅ No spec violations                           │
└─────────────────────────────────────────────────┘
                    ↓
                 MERGE ✅
```

### 10.2 Automated Enforcement Script

```bash
#!/bin/bash
# tools/pre-merge-gates.sh

set -e  # Exit on any error

echo "======================================"
echo "ERLMCP V3 PRE-MERGE QUALITY GATES"
echo "======================================"

# GATE 1: Compilation
echo ""
echo "[GATE 1] Compiling..."
rebar3 compile
echo "✅ Compilation passed"

# GATE 2: Tests
echo ""
echo "[GATE 2] Running tests..."
rebar3 do eunit, ct, proper -c
echo "✅ All tests passed"

# GATE 3: Coverage
echo ""
echo "[GATE 3] Checking coverage..."
rebar3 cover --verbose
COVERAGE=$(grep -oP 'Total: \K[0-9]+' _build/test/cover/index.html)
if [ "$COVERAGE" -lt 80 ]; then
    echo "❌ Coverage $COVERAGE% < 80% (GATE FAILED)"
    exit 1
fi
echo "✅ Coverage $COVERAGE% ≥ 80%"

# GATE 4: Code Quality
echo ""
echo "[GATE 4] Running dialyzer..."
rebar3 dialyzer
echo "✅ Dialyzer passed"

echo ""
echo "[GATE 4] Running xref..."
rebar3 xref
echo "✅ Xref passed"

# GATE 5: MCP Compliance
echo ""
echo "[GATE 5] Running MCP compliance tests..."
rebar3 ct --dir=test/compliance
echo "✅ MCP compliance verified"

echo ""
echo "======================================"
echo "✅ ALL QUALITY GATES PASSED"
echo "======================================"
echo "Ready to merge!"
```

### 10.3 Test Metrics Dashboard

**Track over time:**
- Test count (unit, integration, property, compliance, chaos)
- Code coverage % (overall, by module)
- Test execution time
- Flaky test count (tests that fail intermittently)
- MCP compliance test pass rate

**Example metrics:**
```
┌───────────────────────────────────────────────────┐
│ ERLMCP V3 TEST METRICS (2026-01-30)               │
├───────────────────────────────────────────────────┤
│ Total Tests:        520                           │
│   - Unit:           434                           │
│   - Integration:     52                           │
│   - Property:        34                           │
│   - Compliance:      60                           │
│   - Chaos:           15                           │
├───────────────────────────────────────────────────┤
│ Test Pass Rate:     100% ✅                       │
│ Coverage:            87% ✅                       │
│   - Core:            92% ✅                       │
│   - Transports:      84% ✅                       │
│   - Observability:   78% ⚠️ (target: 80%)         │
├───────────────────────────────────────────────────┤
│ Execution Time:     3m 45s                        │
│ Flaky Tests:        0 ✅                          │
│ MCP Compliance:     60/60 ✅                      │
└───────────────────────────────────────────────────┘
```

---

## 11. Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
**Goal:** Fix broken tests, establish test infrastructure

**Tasks:**
1. ✅ Fix 6 broken test files (syntax errors, missing includes)
2. ✅ Resolve dependency issues (ctx, jobs)
3. ✅ Create `test_utils.erl` shared utilities
4. ✅ Set up test directory structure
5. ✅ Configure rebar3 test profile
6. ✅ Write CI/CD test workflow

**Deliverable:** All existing tests compile and run

---

### Phase 2: Core Module Tests (Week 3-5)
**Goal:** Achieve 85%+ coverage on critical core modules

**Tasks:**
1. ✅ Write `erlmcp_json_rpc_tests.erl` (25 tests + 3 properties)
2. ✅ Write `erlmcp_client_tests.erl` (30 tests + 4 integration)
3. ✅ Write `erlmcp_server_tests.erl` (35 tests)
4. ✅ Write `erlmcp_registry_tests.erl` (25 tests + 4 integration)
5. ✅ Run coverage reports, identify gaps
6. ✅ Add missing tests to reach 85%+ coverage

**Deliverable:** Core modules at 85%+ coverage

---

### Phase 3: MCP Compliance Tests (Week 6-7)
**Goal:** Encode MCP specification as executable tests

**Tasks:**
1. ✅ Create `test/compliance/` structure
2. ✅ Write `mcp_jsonrpc_compliance_SUITE.erl` (15 tests)
3. ✅ Write `mcp_capabilities_SUITE.erl` (10 tests)
4. ✅ Write `mcp_resources_SUITE.erl` (12 tests)
5. ✅ Write `mcp_tools_SUITE.erl` (10 tests)
6. ✅ Write `mcp_prompts_SUITE.erl` (8 tests)
7. ✅ Write `mcp_transports_SUITE.erl` (5 tests)

**Deliverable:** 60 MCP compliance tests passing

---

### Phase 4: Transport & Observability Tests (Week 8-9)
**Goal:** Achieve 80%+ coverage on transports and observability

**Tasks:**
1. ✅ Write transport tests (stdio, tcp, http, ws) - 90 tests
2. ✅ Write observability tests (metrics, otel, chaos) - 60 tests
3. ✅ Run coverage reports, identify gaps
4. ✅ Add missing tests to reach 80%+ coverage

**Deliverable:** Transports and observability at 80%+ coverage

---

### Phase 5: Property & Chaos Tests (Week 10-11)
**Goal:** Add property-based and chaos testing

**Tasks:**
1. ✅ Write property tests for JSON-RPC, transports, state machines - 34 properties
2. ✅ Write chaos tests (process failures, resource exhaustion, network) - 15 scenarios
3. ✅ Run long-duration chaos tests (24hr stability)
4. ✅ Document chaos testing methodology

**Deliverable:** Property tests (34) and chaos tests (15) passing

---

### Phase 6: Quality Gates & Automation (Week 12)
**Goal:** Automate test enforcement

**Tasks:**
1. ✅ Set up pre-commit hooks for test execution
2. ✅ Configure CI/CD pipeline with quality gates
3. ✅ Set up coverage reporting (Coveralls)
4. ✅ Create test metrics dashboard
5. ✅ Document testing strategy (this document)

**Deliverable:** Automated quality gates enforced in CI/CD

---

## 12. Success Criteria

### 12.1 Quantitative Metrics

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| **Total Test Count** | 520+ | 0 (broken) | ❌ → ✅ |
| **Overall Coverage** | ≥80% | Unknown | ❌ → ✅ |
| **Core Module Coverage** | ≥85% | Unknown | ❌ → ✅ |
| **Critical Module Coverage** | ≥90% | Unknown | ❌ → ✅ |
| **MCP Compliance Tests** | 60+ | 0 | ❌ → ✅ |
| **Chaos Tests** | 15+ | 0 | ❌ → ✅ |
| **Property Tests** | 34+ | 0 | ❌ → ✅ |
| **Test Pass Rate** | 100% | N/A | ❌ → ✅ |
| **CI/CD Integration** | Automated | Manual | ❌ → ✅ |

### 12.2 Qualitative Success Indicators

✅ **Test-First Development Adopted** - All new features written TDD-style
✅ **MCP Compliance Verified** - Protocol adherence proven by tests
✅ **Chicago School TDD Used** - Real processes, state-based assertions
✅ **No Flaky Tests** - All tests deterministic and reliable
✅ **Fast Feedback Loop** - Full test suite runs in < 5 minutes
✅ **Comprehensive Chaos Tests** - Resilience to failures proven
✅ **Quality Gates Enforced** - Cannot merge without passing tests

---

## 13. Appendix

### A. Test Template: EUnit Test File

```erlang
-module(erlmcp_example_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Setup/Teardown (Chicago School: real processes)

example_test_() ->
    {setup,
     fun() ->
         %% Setup: Start real system
         application:ensure_all_started(erlmcp_core),
         {ok, Pid} = erlmcp_example:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop real system
         erlmcp_example:stop(Pid),
         application:stop(erlmcp_core)
     end,
     fun(Pid) ->
         [
          ?_test(basic_operation(Pid)),
          ?_test(error_handling(Pid)),
          ?_test(concurrent_access(Pid))
         ]
     end}.

%%% Test Functions (Chicago School: state-based verification)

basic_operation(Pid) ->
    %% Exercise: Call real API
    ok = erlmcp_example:do_something(Pid, "input"),

    %% Verify: Observable state
    {ok, Result} = erlmcp_example:get_result(Pid),
    ?assertEqual("expected_output", Result).

error_handling(Pid) ->
    %% Exercise: Trigger error
    Result = erlmcp_example:do_something(Pid, invalid_input),

    %% Verify: Error returned
    ?assertMatch({error, invalid_input}, Result).

concurrent_access(Pid) ->
    %% Exercise: 100 concurrent calls
    Pids = [spawn(fun() ->
        ok = erlmcp_example:do_something(Pid, "input")
    end) || _ <- lists:seq(1, 100)],

    %% Wait for completion
    [begin
        Ref = monitor(process, P),
        receive {'DOWN', Ref, process, P, _} -> ok end
    end || P <- Pids],

    %% Verify: All succeeded (state verification)
    {ok, Count} = erlmcp_example:get_call_count(Pid),
    ?assertEqual(100, Count).
```

### B. Test Template: Common Test Suite

```erlang
-module(erlmcp_example_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%% Common Test Callbacks

all() -> [
    basic_integration_test,
    multi_process_test,
    failure_recovery_test
].

init_per_suite(Config) ->
    %% Start real application
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%% Test Cases (Chicago School: real processes, real coordination)

basic_integration_test(_Config) ->
    %% Setup: Start real processes
    {ok, Server} = erlmcp_server:start_link([]),
    {ok, Client} = erlmcp_client:start_link([]),

    %% Exercise: Real interaction
    ok = erlmcp_client:call_tool(Client, "tool1", #{}),

    %% Verify: Observable behavior
    {ok, History} = erlmcp_server:get_call_history(Server),
    [#{tool := "tool1"}] = History.

multi_process_test(_Config) ->
    %% Test coordination between multiple real processes
    ...
```

### C. Property Test Template

```erlang
-module(erlmcp_example_prop).
-include_lib("proper/include/proper.hrl").

prop_roundtrip() ->
    ?FORALL(Input, input_generator(),
        begin
            Encoded = erlmcp_example:encode(Input),
            {ok, Decoded} = erlmcp_example:decode(Encoded),
            Decoded =:= Input
        end).

input_generator() ->
    oneof([
        binary(),
        integer(),
        list(binary())
    ]).
```

---

**End of Test Strategy Document**

**Document Version:** 1.0.0
**Last Updated:** 2026-01-30
**Authors:** Erlang Test Engineer Agent
**Status:** ✅ READY FOR REVIEW
