# erlmcp v2.1.0 Production Readiness Backlog

**Status:** ❌ NOT PRODUCTION READY
**Date:** 2026-01-28
**Version:** 2.1.0
**Estimated Effort:** 80-150 hours (10-19 days)

---

## Critical Context

This backlog is organized by **blocking priority**. Items MUST be completed in order. Each item maps to a specific production quality gate that has FAILED.

**Quality Gate Status:**
- ❌ Test Coverage: 1% (REQUIRED: ≥80%) - **79% gap**
- ❌ EUnit Tests: 80.8% pass rate (REQUIRED: 100%)
- ❌ Common Test: 5 suite failures, 33 tests skipped
- ❌ Dialyzer: 526 type warnings (REQUIRED: 0)
- ❌ Xref: 250 warnings (REQUIRED: 0)

**Passed Gates:**
- ✅ Compilation: 0 errors
- ✅ Performance: No regression (-1.99% throughput, -12.79% memory)
- ✅ Version Consistency: All 2.1.0
- ✅ Dependencies: All locked
- ✅ V1 Legacy: No v1 code found

---

## SECTION 1: CRITICAL - Test Coverage Foundation (BLOCKING)

**Priority:** P0 - BLOCKS ALL PRODUCTION WORK
**Section:** `coverage/`
**Estimated:** 40-80 hours
**Goal:** Achieve ≥80% test coverage baseline

---

### Item 001: Core Client Module Test Coverage

**Type:** `coverage/`
**ID:** `coverage/001-core-client-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_client.erl
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
`erlmcp_client.erl` is the CORE CLIENT for the entire MCP protocol implementation. It has **ZERO** test coverage. This is the single most critical module in the entire codebase - every MCP client interaction flows through this module. Deploying without tests is unacceptable.

**Module Location:** `apps/erlmcp_core/src/erlmcp_client.erl`
**Test File Location:** `apps/erlmcp_core/test/erlmcp_client_tests.erl`

**Functions Requiring Tests (ALL OF THEM - 0% COVERAGE):**

**Client Lifecycle:**
- `start_link/0` - gen_server initialization
- `init/1` - State initialization with request correlation map
- `terminate/2` - Cleanup and pending request handling

**Connection Management:**
- `connect/1` - Transport connection establishment
- `connect/2` - Connection with options (timeout, capabilities)
- `disconnect/1` - Graceful shutdown
- `reconnect/1` - Reconnection logic with backoff

**Request-Response Correlation:**
- `send_request/2` - Send JSON-RPC request, track pending
- `send_request/3` - Send with timeout override
- `handle_response/2` - Correlate response to pending request
- `cleanup_expired_requests/1` - Remove stale pending requests

**Capability Negotiation:**
- `initialize/2` - Protocol initialization handshake
- `list_resources/1` - Resource enumeration
- `list_tools/1` - Tool enumeration
- `list_prompts/1` - Prompt enumeration
- `call_tool/3` - Tool execution with arguments
- `get_prompt/2` - Prompt template retrieval

**Error Handling:**
- `handle_transport_error/2` - Transport failure recovery
- `handle_timeout/2` - Request timeout handling
- `handle_parse_error/2` - JSON decode error handling

**State Management:**
- `handle_info/2` - Timeout messages, transport signals
- `handle_call/3` - Synchronous API calls
- `handle_cast/2` - Asynchronous messages

**Test Requirements:**

1. **Chicago School TDD** - Real processes, real gen_server, no mocks
2. **State-Based Verification** - Check #state{} record contents after each operation
3. **Race Condition Testing** - Concurrent requests, timeouts, re-entrant calls
4. **Error Path Coverage** - Every error clause, every exit path
5. **Integration Points** - Test with real transports (stdio, tcp)

**Test Structure:**
```erlang
-module(erlmcp_client_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixtures
client_setup() ->
    {ok, Pid} = erlmcp_client:start_link(),
    Pid.

client_cleanup(Pid) ->
    erlmcp_client:disconnect(Pid).

%% Tests (ALL functions, ALL paths)

% Example:
send_request_test_() ->
    {setup,
     fun client_setup/0,
     fun client_cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                   % Test basic request sending
                   % Test request tracking in pending map
                   % Test timeout registration
                   % Verify state updates
               end),
          ?_test(begin
                   % Test concurrent requests
                   % Test request ID generation
                   % Test pending request correlation
               end)
         ]
     end}.
```

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
- [ ] All public functions have tests
- [ ] All handle_info/handle_call/handle_cast clauses tested
- [ ] All error paths tested
- [ ] Coverage: ≥80%
- [ ] All tests pass: `rebar3 eunit --module=erlmcp_client_tests`
- [ ] No race conditions in concurrent request testing
- [ ] Integration tests with real stdio transport pass

**Dependencies:**
- None (this is foundation work)

**Success Metrics:**
- Test count: ≥50 tests
- Coverage: ≥80%
- Pass rate: 100%
- Execution time: <5 seconds

**Notes:**
- This is the MOST CRITICAL test suite in the entire backlog
- All other client tests depend on patterns established here
- Use erlmcp_session_manager_tests.erl as a reference for gen_server testing patterns
- Location: `apps/erlmcp_core/test/erlmcp_session_manager_tests.erl` (527 lines, 25 tests, all passing)

---

### Item 002: Core Server Module Test Coverage

**Type:** `coverage/`
**ID:** `coverage/002-core-server-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_server.erl
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
`erlmcp_server.erl` is the CORE SERVER for MCP protocol. It handles incoming requests, manages resources, tools, and prompts. **ZERO** test coverage. This module must be bulletproof - any server crash takes down ALL MCP connections.

**Module Location:** `apps/erlmcp_core/src/erlmcp_server.erl`
**Test File Location:** `apps/erlmcp_core/test/erlmcp_server_tests.erl`

**Functions Requiring Tests:**

**Server Lifecycle:**
- `start_link/0` - gen_server initialization
- `init/1` - Resource/tool/prompt registry initialization
- `terminate/2` - Cleanup of all registered handlers

**Resource Management:**
- `add_resource/2` - Register resource handler
- `remove_resource/1` - Unregister resource
- `list_resources/0` - Enumerate all resources
- `get_resource/1` - Retrieve specific resource
- `resource_handler/1` - Execute resource read operation

**Tool Management:**
- `add_tool/2` - Register tool handler
- `remove_tool/1` - Unregister tool
- `list_tools/0` - Enumerate all tools
- `get_tool/1` - Retrieve tool definition
- `call_tool/2` - Execute tool with arguments

**Prompt Management:**
- `add_prompt/2` - Register prompt template
- `remove_prompt/1` - Unregister prompt
- `list_prompts/0` - Enumerate prompts
- `get_prompt/1` - Retrieve prompt template

**Request Handling:**
- `handle_call/3` - Synchronous MCP method calls
- `handle_cast/2` - Asynchronous notifications
- `process_request/2` - JSON-RPC request routing
- `process_notification/2` - Notification handling

**Error Handling:**
- `invalid_request/2` - JSON-RPC error responses
- `method_not_found/1` - Unknown method errors
- `invalid_params/2` - Parameter validation failures

**Test Requirements:**

1. **Handler Registration Testing** - Add/remove/list operations for resources, tools, prompts
2. **Request Routing Testing** - JSON-RPC method calls to correct handlers
3. **Argument Validation** - Invalid arguments, missing required params
4. **Concurrent Access** - Multiple clients registering/calling simultaneously
5. **Handler Execution** - Mock handlers that return test data, verify execution

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_core/test/erlmcp_server_tests.erl`
- [ ] All resource management functions tested
- [ ] All tool management functions tested
- [ ] All prompt management functions tested
- [ ] JSON-RPC request routing tested
- [ ] Error handling paths tested
- [ ] Coverage: ≥80%
- [ ] All tests pass: `rebar3 eunit --module=erlmcp_server_tests`

**Dependencies:**
- None

**Success Metrics:**
- Test count: ≥60 tests
- Coverage: ≥80%
- Pass rate: 100%

---

### Item 003: JSON-RPC Codec Test Coverage

**Type:** `coverage/`
**ID:** `coverage/003-json-rpc-codec-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_json_rpc.erl
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
`erlmcp_json_rpc.erl` implements JSON-RPC 2.0 protocol encoding/decoding. **ZERO** coverage. This module processes EVERY message in and out of the system. A bug here breaks the entire protocol.

**Module Location:** `apps/erlmcp_core/src/erlmcp_json_rpc.erl`
**Test File Location:** `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`

**Functions Requiring Tests:**

**Request Encoding:**
- `encode_request/3` - Create JSON-RPC request (method, params, id)
- `encode_request/4` - Request with custom ID
- `encode_notification/2` - Notification (no response expected)

**Response Decoding:**
- `decode_response/1` - Parse JSON-RPC response
- `decode_error/1` - Parse error response
- `is_response/1` - Response type guard

**Error Encoding:**
- `encode_error/3` - Create error response (code, message, id)
- `standard_errors/0` - Predefined error objects (parse error, invalid request, etc.)

**Batch Operations:**
- `encode_batch/1` - Batch request encoding
- `decode_batch/1` - Batch response decoding

**Validation:**
- `validate_request/1` - Request schema validation
- `validate_response/1` - Response schema validation
- `validate_error/1` - Error schema validation

**Test Requirements:**

1. **JSON Encoding Tests** - Valid JSON output, correct field ordering
2. **JSON Decoding Tests** - Valid JSON input, error handling
3. **Schema Validation Tests** - JSON-RPC 2.0 spec compliance
4. **Edge Case Tests** - Unicode, special characters, large payloads
5. **Error Path Tests** - Invalid JSON, missing fields, wrong types

**Test Data:**
```erlang
% Valid request
#{
  <<"jsonrpc">> => <<"2.0">>,
  <<"method">> => <<"tools/list">>,
  <<"id">> => 1
}

% Valid response
#{
  <<"jsonrpc">> => <<"2.0">>,
  <<"result">> => #{<<"tools">> => []},
  <<"id">> => 1
}

% Error response
#{
  <<"jsonrpc">> => <<"2.0">>,
  <<"error">> => #{
    <<"code">> => -32700,
    <<"message">> => <<"Parse error">>
  },
  <<"id">> => null
}
```

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- [ ] All encode/decode functions tested
- [ ] JSON-RPC 2.0 spec compliance verified
- [ ] Error handling paths tested
- [ ] Coverage: ≥80%
- [ ] All tests pass: `rebar3 eunit --module=erlmcp_json_rpc_tests`

**Dependencies:**
- None

**Success Metrics:**
- Test count: ≥40 tests
- Coverage: ≥80%
- Pass rate: 100%

---

### Item 004: Registry Module Test Coverage

**Type:** `coverage/`
**ID:** `coverage/004-registry-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_registry.erl
**Current Coverage:** 4%
**Target Coverage:** ≥80%

**Context:**
`erlmcp_registry.erl` is the CENTRAL MESSAGE ROUTING system. It currently has 4% coverage - completely inadequate. Registry performance is critical: baseline is 553K msg/sec. Tests must verify performance does not regress.

**Module Location:** `apps/erlmcp_core/src/erlmcp_registry.erl`
**Test File Location:** `apps/erlmcp_core/test/erlmcp_registry_tests.erl`

**Current Issues:**
- 5/26 tests failing (80.8% pass rate)
- Failing tests need to be fixed BEFORE adding new tests

**Functions Requiring Tests:**

**Registry Operations:**
- `register_name/2` - Register process name
- `register_name/3` - Register with options
- `unregister_name/1` - Unregister process name
- `whereis_name/1` - Find process by name
- `send_name/2` - Send message to named process

**Distributed Registry:**
- `register_name_dist/3` - Distributed registration
- `unregister_name_dist/1` - Distributed unregistration
- `whereis_name_dist/1` - Find distributed process

**Performance Critical:**
- `lookup/1` - Fast name lookup (must maintain 553K msg/sec baseline)
- `route/2` - Message routing (performance critical)

**Test Requirements:**

1. **Fix Existing Failing Tests** - 5 tests currently failing, fix these first
2. **Registration Testing** - Register/unregister/whereis operations
3. **Performance Testing** - Verify 553K msg/sec baseline maintained
4. **Concurrency Testing** - Multiple processes registering/routing simultaneously
5. **Distributed Testing** - Multi-node registry operations (requires CT suites)

**Acceptance Criteria:**
- [ ] Fix 5 failing tests in existing suite
- [ ] Add tests for uncovered functions
- [ ] Performance tests verify ≥553K msg/sec
- [ ] Coverage: ≥80%
- [ ] All tests pass: `rebar3 eunit --module=erlmcp_registry_tests`

**Dependencies:**
- Item 005 (EUnit configuration fix) - EUnit currently misconfigured

**Success Metrics:**
- Test count: ≥30 tests (existing) + ≥20 new tests = ≥50 total
- Coverage: ≥80% (from 4%)
- Pass rate: 100% (from 80.8%)
- Performance: ≥553K msg/sec (no regression)

---

### Item 005: EUnit Configuration Fix

**Type:** `infra/`
**ID:** `infra/001-eunit-config-fix`
**Title:** Fix EUnit configuration - exclude CT SUITE files
**Current State:** BROKEN - EUnit attempting to run CT-only SUITE modules

**Context:**
EUnit is misconfigured in `rebar.config`. It's trying to run Common Test SUITE modules as EUnit tests, causing errors. This MUST be fixed before any EUnit test development.

**Problem:**
```
Module `tcps_andon_integration_SUITE' not found
Module `tcps_concurrent_SUITE' not found
Module `tcps_heijunka_SUITE' not found
... (13 total SUITE modules)
```

**Root Cause:**
`rebar.config` line 38-41:
```erlang
{eunit_opts, [
    {exclude, ".*_SUITE$"},  <-- NOT WORKING
    verbose
]}.
```

**Solution:**

Option 1: Fix the exclude pattern (recommended)
```erlang
{eunit_opts, [
    {exclude, ['_SUITE$']},  %% Use regex pattern, not string
    verbose
]}.
```

Option 2: Move CT suites to separate directory
```bash
mkdir -p test/ct
mv test/*_SUITE.erl test/ct/
```

Option 3: Explicitly list test modules
```erlang
{eunit_opts, [
    {test_dirs, ["test"]},
    {test_suffix, "_tests.erl"},  %% Only run *_tests.erl files
    verbose
]}.
```

**Test Requirements:**

1. Verify EUnit only runs `*_tests.erl` modules
2. Verify EUnit does NOT run `*_SUITE.erl` modules
3. Run `rebar3 eunit` - should complete without "module not found" errors
4. Verify existing tests still run

**Acceptance Criteria:**
- [ ] EUnit configuration fixed in `rebar.config`
- [ ] `rebar3 eunit` runs without "module not found" errors
- [ ] EUnit only processes `*_tests.erl` files
- [ ] EUnit ignores `*_SUITE.erl` files
- [ ] All existing EUnit tests still pass

**Dependencies:**
- None

**Success Metrics:**
- EUnit runs cleanly
- Zero "module not found" errors
- Test execution time: <30 seconds

---

## SECTION 2: HIGH PRIORITY - Transport Test Coverage

**Priority:** P1 - BLOCKS PRODUCTION DEPLOYMENT
**Section:** `transports/`
**Estimated:** 15-25 hours
**Goal:** All transport modules tested

---

### Item 006: STDIO Transport Test Coverage

**Type:** `transports/`
**ID:** `transports/001-stdio-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_transport_stdio
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
STDIO transport is the DEFAULT transport for MCP. Every CLI tool uses this. **ZERO** coverage. Critical for local development workflows.

**Module Location:** `apps/erlmcp_transports/src/erlmcp_transport_stdio.erl`
**Test File Location:** `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`

**Functions Requiring Tests:**

**Transport Behavior Implementation:**
- `init/2` - Initialize stdin/stdout streams
- `send/2` - Write to stdout
- `close/1` - Close streams
- `setopts/2` - Configure stream options

**Message Processing:**
- `read_loop/1` - Continuous stdin reading
- `handle_data/2` - Parse incoming JSON-RPC
- `handle_eof/1` - Graceful shutdown on EOF

**Test Requirements:**

1. **Process Testing** - Real stdin/stdout process spawning
2. **I/O Testing** - Read/write to streams
3. **Protocol Testing** - JSON-RPC message framing
4. **Error Handling** - Broken pipe, invalid JSON, EOF

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
- [ ] All transport behavior callbacks tested
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

### Item 007: TCP Transport Test Coverage

**Type:** `transports/`
**ID:** `transports/002-tcp-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_transport_tcp
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
TCP transport is for long-lived connections. **ZERO** coverage. Critical for production deployments. Uses ranch for connection management.

**Module Location:** `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`
**Test File Location:** `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`

**Functions Requiring Tests:**

**Transport Behavior Implementation:**
- `init/2` - Start ranch listener
- `send/2` - Send via TCP socket
- `close/1` - Close connection
- `setopts/2` - Socket options

**Connection Management:**
- `start_listener/3` - Ranch listener startup
- `stop_listener/1` - Ranch listener shutdown
- `handle_connection/1` - New connection handler

**Test Requirements:**

1. **Socket Testing** - Real TCP sockets on localhost
2. **Ranch Integration** - Listener lifecycle
3. **Concurrent Connections** - Multiple simultaneous TCP connections
4. **Error Handling** - Connection refused, timeout, socket close

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`
- [ ] Ranch integration tested
- [ ] Concurrent connection handling tested
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

### Item 008: HTTP Transport Test Coverage

**Type:** `transports/`
**ID:** `transports/003-http-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_transport_http
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
HTTP transport for web-based MCP servers. **ZERO** coverage. Uses gun HTTP client.

**Module Location:** `apps/erlmcp_transports/src/erlmcp_transport_http.erl`
**Test File Location:** `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`

**Functions Requiring Tests:**

**Transport Behavior Implementation:**
- `init/2` - Start HTTP connection via gun
- `send/2` - Send HTTP POST with JSON-RPC
- `close/1` - Close gun connection
- `setopts/2` - HTTP headers/options

**Connection Management:**
- `connect_http/1` - gun connection startup
- `disconnect/1` - gun shutdown

**Test Requirements:**

1. **Gun Integration** - Real HTTP client testing
2. **HTTP Protocol** - POST requests, headers, JSON body
3. **Error Handling** - Connection refused, timeout, HTTP errors
4. **Keep-Alive** - Connection reuse

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
- [ ] Gun client integration tested
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

### Item 009: WebSocket Transport Test Coverage

**Type:** `transports/`
**ID:** `transports/004-websocket-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_transport_ws
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
WebSocket transport for real-time bidirectional messaging. **ZERO** coverage. Critical for browser-based MCP clients.

**Module Location:** `apps/erlmcp_transports/src/erlmcp_transport_ws.erl`
**Test File Location:** `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`

**Functions Requiring Tests:**

**Transport Behavior Implementation:**
- `init/2` - WebSocket upgrade via gun
- `send/2` - Send WebSocket frame
- `close/1` - Close WebSocket connection
- `setopts/2` - WebSocket options

**Frame Handling:**
- `handle_ws_frame/2` - Incoming WebSocket messages
- `encode_frame/1` - Frame encoding

**Test Requirements:**

1. **WebSocket Protocol** - Real WebSocket connections
2. **Frame Testing** - Text/binary frames
3. **Bidirectional Messaging** - Simultaneous send/receive
4. **Error Handling** - Connection drop, invalid frames

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
- [ ] WebSocket protocol tested
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

### Item 010: SSE Transport Test Coverage

**Type:** `transports/`
**ID:** `transports/005-sse-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_transport_sse
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
Server-Sent Events transport for server-to-client streaming. **ZERO** coverage. Critical for real-time event streaming.

**Module Location:** `apps/erlmcp_transports/src/erlmcp_transport_sse.erl`
**Test File Location:** `apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl`

**Functions Requiring Tests:**

**Transport Behavior Implementation:**
- `init/2` - SSE stream initialization
- `send/2` - Send SSE event
- `close/1` - Close SSE stream
- `setopts/2` - SSE options

**Event Handling:**
- `encode_event/1` - SSE event formatting
- `handle_keepalive/1` - Keep-alive messages

**Test Requirements:**

1. **SSE Protocol** - Correct event formatting
2. **Stream Testing** - Continuous event streaming
3. **Keep-Alive** - Automatic keep-alive messages
4. **Error Handling** - Stream interruption, reconnect

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl`
- [ ] SSE protocol compliance tested
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

## SECTION 3: HIGH PRIORITY - Observability Test Coverage

**Priority:** P1 - BLOCKS PRODUCTION MONITORING
**Section:** `observability/`
**Estimated:** 20-30 hours
**Goal:** All OTEL modules tested

---

### Item 011: OpenTelemetry Core Test Coverage

**Type:** `observability/`
**ID:** `observability/001-otel-core-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_otel.erl
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
OpenTelemetry integration for distributed tracing. **ZERO** coverage. Critical for production observability.

**Module Location:** `apps/erlmcp_observability/src/erlmcp_otel.erl`
**Test File Location:** `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

**Functions Requiring Tests:**

**Trace Context:**
- `start_span/1` - Create new trace span
- `end_span/1` - Close span
- `inject_context/1` - W3C trace context injection
- `extract_context/1` - W3C trace context extraction

**Span Attributes:**
- `set_attribute/3` - Add span attribute
- `add_event/2` - Add span event
- `set_status/2` - Set span status

**Test Requirements:**

1. **W3C Trace Context** - traceparent header format
2. **Span Lifecycle** - Start/end span hierarchy
3. **Attribute Testing** - Key-value pairs
4. **Context Propagation** - In-process and cross-process

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`
- [ ] W3C trace context compliance verified
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

### Item 012: OTEL Middleware Test Coverage

**Type:** `observability/`
**ID:** `observability/002-otel-middleware-tests`
**Title:** Create comprehensive EUnit test suite for erlmcp_otel_middleware.erl
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
Automatic tracing middleware for transports. **ZERO** coverage. All MCP calls should be traced automatically.

**Module Location:** `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl`
**Test File Location:** `apps/erlmcp_observability/test/erlmcp_otel_middleware_tests.erl`

**Functions Requiring Tests:**

**Middleware:**
- `wrap_transport/2` - Wrap transport calls with tracing
- `wrap_send/3` - Trace send operations
- `wrap_receive/2` - Trace receive operations

**Span Creation:**
- `create_span_from_request/1` - Extract span from JSON-RPC request
- `inject_span_headers/2` - Inject trace context into transport

**Test Requirements:**

1. **Automatic Tracing** - Verify all operations create spans
2. **Context Propagation** - Trace context through transport layers
3. **Performance** - Minimal overhead from middleware
4. **Error Handling** - Middleware failures don't break transport

**Acceptance Criteria:**
- [ ] Test file created: `apps/erlmcp_observability/test/erlmcp_otel_middleware_tests.erl`
- [ ] Automatic tracing verified
- [ ] Coverage: ≥80%
- [ ] All tests pass

---

### Item 013: OTEL Exporters Test Coverage

**Type:** `observability/`
**ID:** `observability/003-otel-exporters-tests`
**Title:** Create comprehensive EUnit test suite for all OTEL exporters
**Current Coverage:** 0%
**Target Coverage:** ≥80%

**Context:**
OpenTelemetry exporters (Jaeger, Datadog, Honeycomb). **ZERO** coverage. Production telemetry depends on these.

**Modules:**
- `apps/erlmcp_observability/src/erlmcp_otel_exporter_jaeger.erl`
- `apps/erlmcp_observability/src/erlmcp_otel_exporter_datadog.erl`
- `apps/erlmcp_observability/src/erlmcp_otel_exporter_honeycomb.erl`

**Test Files:**
- `apps/erlmcp_observability/test/erlmcp_otel_exporter_jaeger_tests.erl`
- `apps/erlmcp_observability/test/erlmcp_otel_exporter_datadog_tests.erl`
- `apps/erlmcp_observability/test/erlmcp_otel_exporter_honeycomb_tests.erl`

**Functions Requiring Tests:**

**Exporter Interface:**
- `export_spans/1` - Send spans to backend
- `shutdown/0` - Flush and close connection

**Protocol Encoding:**
- `encode_span/1` - Convert span to backend format
- `batch_spans/1` - Batch multiple spans

**Test Requirements:**

1. **Protocol Compliance** - Correct backend format
2. **Batch Testing** - Multiple spans in one export
3. **Error Handling** - Backend unavailable, retry logic
4. **Performance** - Export overhead minimal

**Acceptance Criteria:**
- [ ] 3 test files created (one per exporter)
- [ ] Protocol compliance verified for each backend
- [ ] Coverage: ≥80% per exporter
- [ ] All tests pass

---

## SECTION 4: CRITICAL - Common Test Failures

**Priority:** P0 - BLOCKS INTEGRATION TESTING
**Section:** `ct-fixes/`
**Estimated:** 20-30 hours
**Goal:** All CT suites passing

---

### Item 014: Fix Integration Suite Init Failure

**Type:** `ct-fixes/`
**ID:** `ct-fixes/001-integration-suite-init`
**Title:** Fix erlmcp_integration_SUITE application startup failure
**Current State:** init_per_suite failing, blocking all tests

**Context:**
`erlmcp_integration_SUITE` fails during `init_per_suite` - application won't start. This blocks ALL integration testing.

**Suite Location:** `test/erlmcp_integration_SUITE.erl`

**Error:**
```
init_per_suite failed - application start error
```

**Root Cause Analysis Required:**
1. Check `init_per_suite/1` - what applications are being started?
2. Check application dependencies - are all required apps available?
3. Check supervisor start - is erlmcp_core_sup starting correctly?
4. Check missing supervisor children - are referenced modules available?

**Required Actions:**

1. **Investigation Phase:**
   - Read `test/erlmcp_integration_SUITE.erl`
   - Identify `init_per_suite/1` logic
   - Check which applications are started
   - Verify all dependencies in rebar.config

2. **Fix Phase:**
   - Fix application startup order
   - Ensure all supervisor children exist
   - Add missing modules if needed
   - Update init_per_suite/1 to handle dependencies

3. **Verification Phase:**
   - Run: `rebar3 ct --suite=erlmcp_integration_SUITE`
   - Verify init_per_suite passes
   - Verify all tests in suite can execute

**Acceptance Criteria:**
- [ ] Root cause identified and documented
- [ ] init_per_suite/1 fixed
- [ ] Application starts successfully
- [ ] All tests in suite can run (may still fail, but not blocked by init)
- [ ] `rebar3 ct --suite=erlmcp_integration_SUITE` executes

**Dependencies:**
- None

**Success Metrics:**
- init_per_suite: PASS
- Tests execute: ≥0 tests run (not blocked by init)

---

### Item 015: Fix Observability Suite Dashboard Failure

**Type:** `ct-fixes/`
**ID:** `ct-fixes/002-observability-dashboard-init`
**Title:** Fix erlmcp_observability_SUITE Ranch/Cowboy dashboard startup failure
**Current State:** init_per_suite failing, dashboard won't start

**Context:**
`erlmcp_observability_SUITE` fails because the dashboard server (Cowboy) can't start - Ranch supervisor not available.

**Suite Location:** `test/erlmcp_observability_SUITE.erl`

**Error:**
```
{noproc,{gen_server,call,[ranch_sup,{start_child,...}]}}
```

**Root Cause:**
Ranch supervision tree not initialized before Cowboy listener start. The dashboard tries to start a Cowboy listener that depends on Ranch, but Ranch isn't started.

**Required Actions:**

1. **Investigation Phase:**
   - Read `test/erlmcp_observability_SUITE.erl`
   - Find init_per_suite/1 - what apps are started?
   - Find dashboard startup code
   - Check if ranch is in application dependencies

2. **Fix Phase (choose one):**

   **Option A:** Start ranch in init_per_suite
   ```erlang
   init_per_suite(Config) ->
       {ok, _} = application:ensure_all_started(ranch),
       {ok, _} = application:ensure_all_started(cowboy),
       %% Now start dashboard
       Config.
   ```

   **Option B:** Make dashboard optional
   ```erlang
   init_per_suite(Config) ->
       case application:get_env(dashboard, enabled) of
           {ok, true} -> start_dashboard();
           _ -> ok
       end,
       Config.
   ```

   **Option C:** Start full erlmcp_observability app
   ```erlang
   init_per_suite(Config) ->
       {ok, _} = application:ensure_all_started(erlmcp_observability),
       Config.
   ```

3. **Verification Phase:**
   - Run: `rebar3 ct --suite=erlmcp_observability_SUITE`
   - Verify init_per_suite passes
   - Verify dashboard starts
   - Verify observability tests can run

**Acceptance Criteria:**
- [ ] Root cause documented (Ranch not started)
- [ ] init_per_suite/1 fixed to start ranch/cowboy
- [ ] Dashboard server starts successfully
- [ ] Observability tests execute
- [ ] `rebar3 ct --suite=erlmcp_observability_SUITE` runs

**Dependencies:**
- None

**Success Metrics:**
- init_per_suite: PASS
- Dashboard startup: PASS
- Tests execute: ≥0 tests run

---

### Item 016: Fix Registry Distributed Suite Failure

**Type:** `ct-fixes/`
**ID:** `ct-fixes/003-registry-dist-init`
**Title:** Fix erlmcp_registry_dist_SUITE distributed node initialization failure
**Current State:** single_node test fails, multi_node group init fails

**Context:**
`erlmcp_registry_dist_SUITE` tests distributed registry functionality. Tests fail because distributed nodes can't initialize properly.

**Suite Location:** `test/erlmcp_registry_dist_SUITE.erl`

**Errors:**
```
single_node test failed
multi_node group init failed
```

**Root Cause Analysis Required:**
1. Check distributed node setup in init_per_suite/1
2. Check if slave nodes are being spawned
3. Check network connectivity between nodes
4. Check epmd (Erlang Port Mapper Daemon) availability
5. Check cookie authentication

**Required Actions:**

1. **Investigation Phase:**
   - Read `test/erlmcp_registry_dist_SUITE.erl`
   - Identify distributed node setup logic
   - Check node names and cookies
   - Verify epmd is running: `epmd -names`

2. **Fix Phase:**
   - Fix node spawn logic
   - Ensure proper cookie distribution
   - Add node startup health checks
   - Handle node crashes gracefully

3. **Verification Phase:**
   - Run: `rebar3 ct --suite=erlmcp_registry_dist_SUITE`
   - Verify distributed nodes start
   - Verify registry operations across nodes
   - Verify cleanup (nodes stopped)

**Acceptance Criteria:**
- [ ] Root cause identified and documented
- [ ] Distributed node setup fixed
- [ ] single_node test passes
- [ ] multi_node group init passes
- [ ] `rebar3 ct --suite=erlmcp_registry_dist_SUITE` executes

**Dependencies:**
- None

**Success Metrics:**
- Distributed nodes: START SUCCESSFULLY
- Registry operations across nodes: PASS
- Cleanup: PASS (nodes stopped)

---

### Item 017: Fix Transport Behavior Suite Validation Failure

**Type:** `ct-fixes/`
**ID:** `ct-fixes/004-transport-behavior-validation`
**Title:** Fix erlmcp_transport_behavior_SUITE stdio_opts_validation failure
**Current State:** Test expects error, gets ok

**Context:**
`erlmcp_transport_behavior_SUITE` tests transport behavior compliance. The `stdio_opts_validation` test is failing - expected error but got ok.

**Suite Location:** `test/erlmcp_transport_behavior_SUITE.erl`

**Error:**
```
stdio_opts_validation expected error but got ok
```

**Root Cause Analysis Required:**
1. What options are being passed to stdio transport?
2. What validation SHOULD fail?
3. Why is validation passing when it shouldn't?
4. Is the test wrong or is the validation wrong?

**Required Actions:**

1. **Investigation Phase:**
   - Read `test/erlmcp_transport_behavior_SUITE.erl`
   - Find `stdio_opts_validation` test
   - Identify what invalid options are being tested
   - Check `erlmcp_transport_stdio:setopts/2` validation logic

2. **Fix Phase (choose one):**

   **Option A:** Fix the test (validation is correct)
   ```erlang
   % Change test to expect ok instead of error
   ?assertEqual(ok, erlmcp_transport_stdio:setopts(Pid, Options))
   ```

   **Option B:** Fix the validation (test is correct)
   ```erlang
   % Add validation to reject invalid options
   setopts(_Pid, Options) when is_map(Options) ->
       case validate_options(Options) of
           ok -> ok;
           {error, Reason} -> error({invalid_options, Reason})
       end.
   ```

3. **Verification Phase:**
   - Run: `rebar3 ct --suite=erlmcp_transport_behavior_SUITE`
   - Verify stdio_opts_validation passes
   - Verify all transport behavior tests pass

**Acceptance Criteria:**
- [ ] Root cause documented (validation logic or test expectation)
- [ ] Fix applied (validation or test)
- [ ] stdio_opts_validation test passes
- [ ] All transport behavior tests pass
- [ ] `rebar3 ct --suite=erlmcp_transport_behavior_SUITE` executes

**Dependencies:**
- None

**Success Metrics:**
- stdio_opts_validation: PASS
- All transport behavior tests: PASS

---

## SECTION 5: MEDIUM PRIORITY - Quality Gate Fixes

**Priority:** P2 - REQUIRED FOR PRODUCTION
**Section:** `quality-gates/`
**Estimated:** 30-50 hours
**Goal:** Dialyzer and Xref clean

---

### Item 018: Implement Missing Modules

**Type:** `quality-gates/`
**ID:** `quality-gates/001-missing-modules`
**Title:** Implement or remove 15+ missing modules referenced by Xref
**Current State:** 60+ undefined function calls to non-existent modules

**Context:**
Xref analysis found 60+ undefined function calls to modules that don't exist. These must be implemented or the references removed.

**Missing Modules:**
1. `erlmcp_request_id` - Safe increment function
2. `erlmcp_tracing` - 9 functions for trace management
3. `erlmcp_task_manager` - 8 functions for task tracking
4. `tcps_persistence` - SKU/receipt persistence
5. `tcps_work_order` - Work order management

**Decision Matrix:**

For each missing module, decide:

| Module | Implement? | Remove References? | Reason |
|--------|-----------|-------------------|---------|
| erlmcp_request_id | TBD | TBD | Need input - is this needed? |
| erlmcp_tracing | TBD | TBD | Need input - is this needed? |
| erlmcp_task_manager | NO | YES | Replaced by erlmcp_hooks |
| tcps_persistence | TBD | TBD | Need input - is this needed? |
| tcps_work_order | TBD | TBD | Need input - is this needed? |

**Required Actions:**

1. **Audit Phase:**
   - For each missing module, search codebase for references
   - Determine if referenced by active code or dead code
   - Document why module was referenced but never implemented

2. **Decision Phase:**
   - For modules needed: Create stub or full implementation
   - For modules not needed: Remove all references
   - Update dependencies if needed

3. **Implementation Phase:**
   - Implement needed modules (minimal gen_servers or utilities)
   - Remove references to unneeded modules
   - Test that references are resolved

**Acceptance Criteria:**
- [ ] All missing modules audited
- [ ] Decision made for each (implement or remove)
- [ ] Needed modules implemented (stubs ok if not critical path)
- [ ] References to unneeded modules removed
- [ ] `rebar3 xref` shows 0 undefined function calls to missing modules

**Dependencies:**
- None

**Success Metrics:**
- Xref undefined calls: 0 (from 60+)
- Missing modules: 0 (all implemented or references removed)

---

### Item 019: Fix Dialyzer Type Warnings

**Type:** `quality-gates/`
**ID:** `quality-gates/002-dialyzer-warnings`
**Title:** Fix 526 Dialyzer type warnings
**Current State:** 526 type warnings blocking production

**Context:**
Dialyzer found 526 type safety issues. These must be resolved for production deployment.

**Warning Categories:**

1. **Unknown Function Calls (HIGH PRIORITY):**
   - `mnesia:*` functions - Mnesia not in dependencies
   - `otel:*` functions - OpenTelemetry API functions
   - `erlmcp_request_id:safe_increment/1` - Missing module

2. **Unmatched Return Values:**
   - `timer:send_after/3` - Return value not checked
   - `ets:insert/2` - Return value not checked

3. **Missing Type Exports:**
   - `erlmcp_rate_limiter:priority/0` - Type not exported

4. **Functions With No Local Return:**
   - Infinite loops causing potential crashes

**Required Actions:**

1. **Categorization Phase:**
   - Run: `rebar3 dialyzer > dialyzer_output.txt`
   - Categorize warnings by type and severity
   - Identify top 10 offending modules

2. **Fix Phase:**

   **Fix Strategy A: Add Type Specs**
   ```erlang
   % Before (no spec)
   safe_increment(Id) -> Id + 1.

   % After (with spec)
   -spec safe_increment(integer()) -> integer().
   safe_increment(Id) -> Id + 1.
   ```

   **Fix Strategy B: Handle Return Values**
   ```erlang
   % Before (return ignored)
   timer:send_after(1000, self(), timeout).

   % After (return checked)
   {ok, TRef} = timer:send_after(1000, self(), timeout),
   % Store TRef for cancellation if needed
   ```

   **Fix Strategy C: Fix Infinite Loops**
   ```erlang
   % Before (no local return - Dialyzer warning)
   loop() -> loop().

   % After (add timeout clause or spec)
   -spec loop() -> no_return().
   loop() -> loop().
   ```

3. **Verification Phase:**
   - Run: `rebar3 dialyzer`
   - Verify warnings reduced to 0
   - Ensure no new warnings introduced

**Acceptance Criteria:**
- [ ] All 526 warnings categorized
- [ ] Type specs added for missing functions
- [ ] Return values handled properly
- [ ] Infinite loops annotated with -spec or fixed
- [ ] `rebar3 dialyzer` shows 0 warnings

**Dependencies:**
- Item 018 (missing modules) - Must implement or remove first

**Success Metrics:**
- Dialyzer warnings: 0 (from 526)

---

### Item 020: Fix Xref Warnings

**Type:** `quality-gates/`
**ID:** `quality-gates/003-xref-warnings`
**Title:** Fix 250 Xref warnings (undefined calls + unused functions)
**Current State:** 250 warnings - 60+ undefined calls, 27+ unused functions

**Context:**
Xref found code quality issues - undefined function calls and unused functions.

**Warning Categories:**

1. **Undefined Function Calls (60+):**
   - Calls to missing modules (see Item 018)
   - Calls to functions that don't exist in existing modules

2. **Unused Local Functions (27+):**
   - Dead code - functions defined but never called
   - Helper functions that are obsolete

3. **Deprecated Functions:**
   - Calls to deprecated OTP functions

**Required Actions:**

1. **Categorization Phase:**
   - Run: `rebar3 xref > xref_output.txt`
   - Separate undefined calls from unused functions
   - Identify top 10 offending modules

2. **Fix Phase (Undefined Calls):**
   - Implement missing functions or remove calls
   - Fix function name typos
   - Update calls to match actual API

3. **Fix Phase (Unused Functions):**
   - Remove dead code
   - Or document why function is kept (export for public API)

**Acceptance Criteria:**
- [ ] All 250 warnings categorized
- [ ] Undefined function calls resolved
- [ ] Unused functions removed or documented
- [ ] `rebar3 xref` shows 0 warnings

**Dependencies:**
- Item 018 (missing modules) - Must resolve first
- Item 019 (Dialyzer) - Can work in parallel

**Success Metrics:**
- Xref warnings: 0 (from 250)

---

### Item 021: Remove Dead Code

**Type:** `quality-gates/`
**ID:** `quality-gates/004-dead-code-removal`
**Title:** Remove 27+ unused local functions
**Current State:** Dead code cluttering codebase

**Context:**
Xref identified 27+ functions that are defined but never called. This is dead code that should be removed.

**Required Actions:**

1. **Identification Phase:**
   - Run: `rebar3 xref` to get list of unused functions
   - For each unused function, check if:
     - Truly unused (internal helper not called)
     - Part of public API (exported but not used internally)
     - Testing stub (used only in tests)

2. **Cleanup Phase:**
   - Remove truly unused functions
   - Document public API functions (why exported?)
   - Keep test-only functions (move to test file if needed)

3. **Verification Phase:**
   - Run: `rebar3 xref` - verify 0 unused functions
   - Run: `rebar3 compile` - verify no references to removed functions
   - Run: `rebar3 eunit` - verify tests still pass

**Acceptance Criteria:**
- [ ] All 27+ unused functions audited
- [ ] Truly unused functions removed
- [ ] Public API functions documented
- [ ] Test-only functions moved to test files
- [ ] `rebar3 xref` shows 0 unused function warnings

**Dependencies:**
- None

**Success Metrics:**
- Xref unused functions: 0 (from 27+)
- Lines of code removed: ≥500 (estimated)

---

## SECTION 6: LOW PRIORITY - Documentation & Polish

**Priority:** P3 - NICE TO HAVE
**Section:** `docs/`
**Estimated:** 10-20 hours
**Goal:** Complete documentation

---

### Item 022: Update README with v2.1.0 Changes

**Type:** `docs/`
**ID:** `docs/001-readme-update`
**Title:** Update main README.md with v2.1.0 features and production readiness status

**Context:**
README still references v1.x features. Needs update for v2.1.0 architecture.

**Required Updates:**

1. **Architecture Section:**
   - Update for 4-app umbrella structure
   - Document new modules (session_manager, hooks, etc.)
   - Update transport layer documentation

2. **Quick Start Section:**
   - Update for rebar3 (vs rebar)
   - Add production readiness warning
   - Document current limitations

3. **Performance Section:**
   - Update with v2.1.0 benchmarks
   - Document 2.52M msg/sec baseline

4. **Testing Section:**
   - Document test coverage status
   - Link to coverage reports

**Acceptance Criteria:**
- [ ] README.md updated
- [ ] v2.1.0 features documented
- [ ] Production readiness warnings added
- [ ] Performance benchmarks updated
- [ ] Test coverage status documented

---

### Item 023: Create Migration Guide from v1 to v2

**Type:** `docs/`
**ID:** `docs/002-migration-guide`
**Title:** Create MIGRATION.md for v1 to v2 upgrade path

**Context:**
Users need guidance upgrading from v1.x to v2.1.0.

**Required Content:**

1. **Breaking Changes:**
   - List all API changes
   - List config file changes
   - List dependency changes

2. **Upgrade Steps:**
   - Step-by-step migration process
   - Code examples
   - Testing checklist

3. **Rollback Plan:**
   - How to rollback if needed
   - Data migration considerations

**Acceptance Criteria:**
- [ ] MIGRATION.md created
- [ ] Breaking changes documented
- [ ] Upgrade steps provided
- [ ] Rollback plan documented

---

### Item 024: Archive v1 Documentation

**Type:** `docs/`
**ID:** `docs/003-archive-v1-docs`
**Title:** Archive 90-95 v1 markdown files to docs/archive/

**Context:**
Root directory has 171 markdown files (should be 5). 90-95 v1 docs need archiving.

**Required Actions:**

1. **Create Archive Structure:**
   ```bash
   mkdir -p docs/archive/v1-agent-deliverables
   mkdir -p docs/archive/v1-phase-deliverables
   mkdir -p docs/archive/v1-version-releases
   ```

2. **Move Files:**
   - Agent deliverables → `docs/archive/v1-agent-deliverables/`
   - Phase deliverables → `docs/archive/v1-phase-deliverables/`
   - Version releases → `docs/archive/v1-version-releases/`

3. **Delete Duplicates:**
   - Remove duplicate files
   - Remove transient docs (scratch pads, etc.)

**Acceptance Criteria:**
- [ ] Archive directories created
- [ ] 90-95 files moved to archive
- [ ] Root directory has ≤5 markdown files
- [ ] README, CHANGELOG, CONTRIBUTING, DEVELOPMENT, CLAUDE remain in root

---

## Execution Notes for Wreckit

### Recommended Execution Order

**Phase 1: Foundation (MUST DO FIRST)**
1. Item 005: Fix EUnit configuration (INFRA - blocks all EUnit work)
2. Item 001: Core client tests (COVERAGE - most critical module)
3. Item 002: Core server tests (COVERAGE - second most critical)
4. Item 003: JSON-RPC codec tests (COVERAGE - protocol implementation)

**Phase 2: Transport & Observability (HIGH PRIORITY)**
5. Item 006-010: All transport tests (5 items, can run in parallel)
6. Item 011-013: All OTEL tests (3 items, can run in parallel)

**Phase 3: CT Fixes (INTEGRATION)**
7. Item 014-017: Fix all CT suite failures (4 items, sequential)

**Phase 4: Quality Gates (PRODUCTION)**
8. Item 018: Implement missing modules (blocks Dialyzer/Xref fixes)
9. Item 019: Fix Dialyzer warnings
10. Item 020: Fix Xref warnings
11. Item 021: Remove dead code

**Phase 5: Polish (OPTIONAL)**
12. Item 022-024: Documentation updates (3 items, can run in parallel)

### Wreckit Configuration

```json
{
  "agent": {
    "mode": "sdk",
    "sdk_model": "claude-sonnet-4-20250514",
    "sdk_max_tokens": 8192,
    "sdk_tools": ["Read", "Edit", "Bash", "Glob", "Grep"]
  },
  "base_branch": "main",
  "branch_prefix": "coverage/",
  "max_iterations": 100
}
```

### Running the Backlog

```bash
# Initialize Wreckit
wreckit init

# Ingest this backlog
wreckit ideas < BACKLOG.md

# Run first item (EUnit config fix - blocks all other work)
wreckit run infra/001-eunit-config-fix

# Run Phase 1 in sequence
wreckit run coverage/001-core-client-tests
wreckit run coverage/002-core-server-tests
wreckit run coverage/003-json-rpc-codec-tests

# Run Phase 2 in parallel (spawn multiple sandboxes)
wreckit run transports/001-stdio-tests    # Sandbox 1
wreckit run transports/002-tcp-tests      # Sandbox 2
wreckit run transports/003-http-tests     # Sandbox 3
wreckit run transports/004-websocket-tests # Sandbox 4
wreckit run transports/005-sse-tests      # Sandbox 5

# Continue...
```

### Expected Timeline

- **Phase 1 (Foundation):** 15-25 hours
- **Phase 2 (Transports/OTEL):** 35-55 hours
- **Phase 3 (CT Fixes):** 20-30 hours
- **Phase 4 (Quality Gates):** 30-50 hours
- **Phase 5 (Docs):** 10-20 hours

**Total: 110-180 hours (14-23 days)**

---

## Success Criteria

**When is erlmcp v2.1.0 PRODUCTION READY?**

All of the following MUST pass:

- ✅ **Compilation:** 0 errors (CURRENT: PASS)
- ✅ **EUnit:** 100% pass rate (CURRENT: 80.8%)
- ✅ **Common Test:** 100% pass rate (CURRENT: FAIL)
- ✅ **Coverage:** ≥80% (CURRENT: 1%)
- ✅ **Dialyzer:** 0 warnings (CURRENT: 526)
- ✅ **Xref:** 0 warnings (CURRENT: 250)
- ✅ **Performance:** No regression >10% (CURRENT: PASS)

---

*"I'm in danger!"* — erlmcp v2.1.0, nervously
*"I'm gonna wreck it!"* — Wreckit, enthusiastically
