# Medium Priority Gap Fix Plan
**Agent 19**: Medium Priority Gap Fix Planner
**Date**: 2026-01-30
**Scope**: P2 (Medium Priority) Implementation and Test Gaps
**Planning Horizon**: 4 Sprints (8 weeks)

---

## Executive Summary

This plan addresses **P2 (Medium Priority)** gaps identified in the comprehensive MCP specification compliance audit. These gaps are **important for production readiness** but do not block core functionality.

### P2 Gap Classification

**Total P2 Gaps Identified**: 47
- Implementation Gaps: 28
- Test Gaps: 19
- Validation Gaps: 0 (all validation gaps are P0/P1)

### Overall P2 Statistics

| Category | P2 Count | Est. Effort | Target Completion |
|----------|----------|-------------|-------------------|
| **Core Protocol** | 8 | 40 hours | Sprint 2 |
| **Capabilities** | 12 | 60 hours | Sprint 3 |
| **Transports** | 10 | 50 hours | Sprint 3-4 |
| **Error Handling** | 6 | 30 hours | Sprint 2 |
| **Test Coverage** | 11 | 55 hours | Sprint 4 |
| **TOTAL** | **47** | **235 hours** | **8 weeks** |

---

## Sprint 1: Foundation (Week 1-2)
**Focus**: Core protocol medium priority gaps
**Capacity**: 60 hours
**Gaps**: 10 gaps

### Sprint 1 Goals
1. Fix message size limit configuration issues
2. Add basic session management features
3. Implement resource versioning foundation
4. Add tool execution timeout support

### Sprint 1 Detailed Tasks

#### Task 1.1: Message Size Limit Configuration (P2-IMPL-001)
**Gap**: Default 16MB limit too large for production, per-transport limits not properly configured
**Location**: `erlmcp_message_size.erl:1-191`, `erlmcp_json_rpc.erl:99-118`
**Estimate**: 8 hours

**Implementation Steps**:
1. Add per-transport size limit configuration
   - STDIO: 4MB (appropriate for CLI tools)
   - TCP: 16MB (for high-throughput scenarios)
   - HTTP: 10MB (web-friendly limit)
   - WebSocket: 16MB (real-time streaming)
   - SSE: 8MB (event streaming)

2. Implement runtime limit adjustment API
   ```erlang
   erlmcp_message_size:set_limit(Transport, NewLimit).
   erlmcp_message_size:get_limit(Transport).
   ```

3. Add configuration validation
   - Validate limits on startup
   - Reject limits <1KB or >100MB
   - Log warning for limits >32MB

4. Update documentation with production recommendations

**Acceptance Criteria**:
- [ ] Each transport has appropriate default size limit
- [ ] Runtime limit adjustment API works correctly
- [ ] Configuration validation prevents invalid limits
- [ ] All existing tests pass with new defaults
- [ ] New tests for limit adjustment API

**Dependencies**: None
**Risk**: Low - backwards compatible (only changes defaults)

---

#### Task 1.2: Session Lifecycle Management (P2-IMPL-002)
**Gap**: No session lifecycle management, expiration, or cleanup
**Location**: `erlmcp_session.erl:1-68`, `erlmcp_server.erl:886-902`
**Estimate**: 12 hours

**Implementation Steps**:
1. Add session expiration mechanism
   ```erlang
   -record(session, {
       id,
       client_id,
       created_at,
       last_activity,
       expires_at,
       metadata
   }).
   ```

2. Implement session cleanup process
   - Expiration checker runs every 60 seconds
   - Removes sessions inactive for >30 minutes (configurable)
   - Sends cleanup notification to client

3. Add session lifecycle API
   ```erlang
   erlmcp_session:refresh(SessionId) -> ok | {error, not_found}.
   erlmcp_session:expire(SessionId) -> ok.
   erlmcp_session:extend(SessionId, Minutes) -> ok.
   ```

4. Integrate with client disconnect
   - Auto-expire session on disconnect
   - Allow reconnection to resume session (within expiration window)

**Acceptance Criteria**:
- [ ] Sessions expire after inactivity period
- [ ] Cleanup process removes expired sessions
- [ ] Session refresh API works correctly
- [ ] Reconnection resumes existing session
- [ ] All existing tests pass
- [ ] New tests for lifecycle management

**Dependencies**: Task 1.1 (for config pattern)
**Risk**: Medium - changes session state management

---

#### Task 1.3: Resource Versioning Foundation (P2-IMPL-003)
**Gap**: No resource versioning or last-modified timestamp support
**Location**: `erlmcp_server.erl:131-210`, `erlmcp_resource.erl`
**Estimate**: 10 hours

**Implementation Steps**:
1. Add version metadata to resource records
   ```erlang
   -record(resource, {
       uri,
       name,
       description,
       mime_type,
       content,
       version,           % NEW: integer version counter
       last_modified,     % NEW: timestamp
       metadata
   }).
   ```

2. Implement version increment on update
   - Auto-increment version on resource update
   - Update last_modified timestamp
   - Store version history (optional, last 10 versions)

3. Add version-based conditional reads
   ```erlang
   resources/read_with_version(Uri, Version) -> Result.
   resources/check_version(Uri, Version) -> Changed | Unchanged.
   ```

4. Add version information to resource list
   - Include version in list response
   - Support filtering by version range

**Acceptance Criteria**:
- [ ] Resources track version and last-modified
- [ ] Version increments on each update
- [ ] Conditional reads work correctly
- [ ] Version information included in list
- [ ] All existing tests pass
- [ ] New tests for versioning

**Dependencies**: None
**Risk**: Medium - extends resource data structure

---

#### Task 1.4: Tool Execution Timeout Support (P2-IMPL-004)
**Gap**: No tool execution timeouts, cancellation support, or concurrent limits
**Location**: `erlmcp_server.erl:388-456`
**Estimate**: 10 hours

**Implementation Steps**:
1. Add timeout configuration to tool handlers
   ```erlang
   -record(tool, {
       name,
       description,
       input_schema,
       handler,
       timeout,           % NEW: milliseconds, default 30000
       metadata
   }).
   ```

2. Implement timeout enforcement
   - Monitor tool execution process
   - Kill process after timeout
   - Return timeout error to client

3. Add tool cancellation API
   ```erlang
   tools/cancel(RequestId) -> ok | {error, not_found}.
   ```

4. Add concurrent tool limits
   - Configure max concurrent tools (default: 10)
   - Queue tools when limit reached
   - Return queue position to client

**Acceptance Criteria**:
- [ ] Tools timeout after configured duration
- [ ] Timeout errors returned to client
- [ ] Tool cancellation works correctly
- [ ] Concurrent limit enforced
- [ ] Queued tools execute when slot available
- [ ] All existing tests pass
- [ ] New tests for timeout and cancellation

**Dependencies**: None
**Risk**: Medium - changes tool execution flow

---

#### Task 1.5: Batch Request Size Validation (P2-IMPL-005)
**Gap**: Batch request size limits not enforced, partial failure handling incomplete
**Location**: `erlmcp_json_rpc.erl:120-141`
**Estimate**: 6 hours

**Implementation Steps**:
1. Add batch size limit configuration
   ```erlang
   -define(MAX_BATCH_SIZE, 100).  % Configurable per transport
   ```

2. Implement batch size validation
   - Reject batches exceeding limit
   - Return error code -32012 (message too large)
   - Include batch size in error response

3. Implement partial failure handling
   - Process each request independently
   - Return mixed success/error results
   - Maintain request-response correlation

**Acceptance Criteria**:
- [ ] Batch size limit enforced
- [ ] Large batches rejected with proper error
- [ ] Partial failures handled correctly
- [ ] Request-response correlation maintained
- [ ] All existing tests pass
- [ ] New tests for batch validation

**Dependencies**: None
**Risk**: Low - adds validation only

---

#### Task 1.6: Notification Persistence Queue (P2-IMPL-006)
**Gap**: No notification persistence, queue management, or prioritization
**Location**: `erlmcp_client.erl:706-745`, `erlmcp_server.erl:590-612`
**Estimate**: 8 hours

**Implementation Steps**:
1. Add notification queue process
   ```erlang
   -record(notification_queue, {
       client_id,
       queue,          % queue:queue()
       max_size,       % default: 1000
       priority        % high, normal, low
   }).
   ```

2. Implement queue management
   - Enqueue notifications with priority
   - Dequeue and send to client
   - Drop oldest when queue full

3. Add queue status API
   ```erlang
   notifications/get_queue_status() -> Size, MaxSize.
   notifications/clear_queue() -> ok.
   ```

**Acceptance Criteria**:
- [ ] Notifications queued with priority
- [ ] High-priority notifications sent first
- [ ] Queue drops oldest when full
- [ ] Queue status API works
- [ ] All existing tests pass
- [ ] New tests for notification queue

**Dependencies**: None
**Risk**: Low - adds optional feature

---

#### Task 1.7: Progress Tracking Integration (P2-IMPL-007)
**Gap**: Progress tracking not integrated with tool execution or resource operations
**Location**: `erlmcp_server.erl:898-900`, `erlmcp_progress.erl`
**Estimate**: 6 hours

**Implementation Steps**:
1. Add progress callback to tool handlers
   ```erlang
   -record(tool, {
       ...
       progress_callback,  % Optional: fun(Partial) -> ok
       ...
   }).
   ```

2. Integrate progress with resource operations
   - Send progress for long-running reads
   - Update progress token during subscription

3. Add progress reporting APIs
   ```erlang
   progress/send(Token, Partial) -> ok.
   progress/complete(Token) -> ok.
   ```

**Acceptance Criteria**:
- [ ] Tools can report progress during execution
- [ ] Resource operations send progress
- [ ] Progress tokens updated correctly
- [ ] All existing tests pass
- [ ] New tests for progress integration

**Dependencies**: None
**Risk**: Low - adds optional feature

---

### Sprint 1 Completion Criteria

- [ ] All 10 tasks completed
- [ ] All new tests passing
- [ ] All existing tests still passing
- [ ] Code coverage ≥80% for new code
- [ ] Documentation updated
- [ ] Sprint retrospective completed

---

## Sprint 2: Error Handling & JSON-RPC (Week 3-4)
**Focus**: Medium priority error handling and JSON-RPC gaps
**Capacity**: 60 hours
**Gaps**: 9 gaps

### Sprint 2 Goals
1. Integrate refusal codes with error handling
2. Add missing MCP error codes
3. Implement severity-based error routing
4. Fix error response format inconsistencies

### Sprint 2 Detailed Tasks

#### Task 2.1: Refusal Code Integration (P2-ERR-001)
**Gap**: Refusal codes (1001-1089) not integrated with error handling
**Location**: Multiple modules (auth, rate_limiter, transport)
**Estimate**: 12 hours

**Implementation Steps**:
1. Map existing errors to refusal codes
   ```erlang
   % Queue errors (1001-1005)
   -define(ERR_QUEUE_FULL, 1001).
   -define(ERR_BYTE_LIMIT, 1002).
   -define(ERR_TENANT_QUOTA, 1003).

   % Auth errors (1011-1016)
   -define(ERR_AUTH_FAILED, 1011).
   -define(ERR_AUTH_EXPIRED, 1012).
   -define(ERR_AUTH_DENIED, 1014).
   ```

2. Update error return functions
   - Modify `erlmcp_auth` to return auth refusal codes
   - Modify `erlmcp_rate_limiter` to return rate limit codes
   - Modify `erlmcp_connection_limiter` to return queue codes

3. Add refusal code formatting
   ```erlang
   format_refusal_error(Code, Message, Data) ->
       #{code => Code, message => Message, data => Data}.
   ```

**Acceptance Criteria**:
- [ ] Auth module returns 1011-1016 codes
- [ ] Rate limiter returns 1056-1060 codes
- [ ] Connection limiter returns 1001-1005 codes
- [ ] Refusal errors formatted correctly
- [ ] All existing tests pass
- [ ] New tests for refusal code mapping

**Dependencies**: None
**Risk**: Medium - changes error return values

---

#### Task 2.2: Missing MCP Error Codes (P2-ERR-002)
**Gap**: Missing MCP error codes (-32011, -32021, -32026, etc.)
**Location**: `erlmcp_json_rpc.erl:156-249`, `erlmcp.hrl:34-285`
**Estimate**: 6 hours

**Implementation Steps**:
1. Add missing error code definitions
   ```erlang
   -define(MCP_ERR_TOOL_DESC_TOO_LONG, -32011).
   -define(MCP_ERR_RESOURCE_TEMPLATE_NOT_FOUND, -32021).
   -define(MCP_ERR_RESOURCE_ALREADY_EXISTS, -32026).
   -define(MCP_ERR_RESOURCE_LOCKED, -32027).
   -define(MCP_ERR_RESOURCE_VERSION_MISMATCH, -32028).
   ```

2. Implement error code usage
   - Check tool description length (10,000 chars)
   - Validate resource templates exist
   - Check for duplicate resources
   - Implement resource locking
   - Validate resource version on update

3. Update error handling to use new codes
   - Return appropriate error codes
   - Include error data with context
   - Log error with details

**Acceptance Criteria**:
- [ ] All missing error codes defined
- [ ] Error codes used in appropriate places
- [ ] Error responses include context
- [ ] All existing tests pass
- [ ] New tests for each error code

**Dependencies**: Task 1.3 (resource versioning)
**Risk**: Low - adds error codes only

---

#### Task 2.3: Severity-Based Error Routing (P2-ERR-003)
**Gap**: Error severity levels not consistently applied, no automated actions
**Location**: Multiple modules
**Estimate**: 10 hours

**Implementation Steps**:
1. Define severity levels and actions
   ```erlang
   -record(severity_config, {
       critical,  % Alert + stop accepting requests
       error,     % Log + continue
       warn       % Log + metrics only
   }).

   SeverityActions = #{
       critical => fun log_and_alert/1,
       error => fun log_error/1,
       warn => fun log_warning/1
   }.
   ```

2. Implement severity-based routing
   - Route critical errors to alerting system
   - Aggregate error rates for monitoring
   - Trigger actions based on severity

3. Add severity escalation
   - Escalate repeated errors to higher severity
   - Reset escalation after cooldown period

**Acceptance Criteria**:
- [ ] Severity levels defined and documented
- [ ] Critical errors trigger alerts
- [ ] Error rates aggregated and reported
- [ ] Severity escalation works correctly
- [ ] All existing tests pass
- [ ] New tests for severity routing

**Dependencies**: Task 2.1 (refusal code integration)
**Risk**: Medium - adds error routing logic

---

#### Task 2.4: Error Response Format Consistency (P2-ERR-004)
**Gap**: Inconsistent data field handling across modules
**Location**: `erlmcp_json_rpc.erl:431-464`, `erlmcp_server.erl:580-584`
**Estimate**: 8 hours

**Implementation Steps**:
1. Define standard error response format
   ```erlang
   -record(error_response, {
       code,
       message,
       data => #{},       % Consistent data map
       request_id,        % For correlation
       timestamp          % ISO 8601
   }).
   ```

2. Update all error returns
   - Use standard format across all modules
   - Ensure data field is always a map
   - Include request ID and timestamp

3. Add error response validation
   - Validate format before sending
   - Log warnings for non-compliant errors

**Acceptance Criteria**:
- [ ] All errors use standard format
- [ ] Data field always a map
- [ ] Request ID included in all errors
- [ ] Timestamp included in all errors
- [ ] All existing tests pass
- [ ] New tests for error format validation

**Dependencies**: Task 2.2 (missing error codes)
**Risk**: Medium - changes error format

---

#### Task 2.5: JSON-RPC Batch Partial Failure (P2-ERR-005)
**Gap**: Partial failure handling for large batches incomplete
**Location**: `erlmcp_json_rpc.erl:120-141`
**Estimate**: 6 hours

**Implementation Steps**:
1. Implement independent request processing
   ```erlang
   process_batch(Requests) ->
       lists:map(fun process_request_independently/1, Requests).
   ```

2. Maintain error context for each request
   - Keep request-response correlation
   - Include individual error details
   - Preserve batch order in response

3. Add batch error aggregation
   - Count successes and failures
   - Return batch-level summary

**Acceptance Criteria**:
- [ ] Each request processed independently
- [ ] Mixed success/error results returned
- [ ] Request-response correlation maintained
- [ ] Batch summary included
- [ ] All existing tests pass
- [ ] New tests for partial failure

**Dependencies**: Task 1.5 (batch size validation)
**Risk**: Low - improves existing feature

---

### Sprint 2 Completion Criteria

- [ ] All 9 tasks completed
- [ ] All new tests passing
- [ ] All existing tests still passing
- [ ] Code coverage ≥80% for new code
- [ ] Error handling documentation updated
- [ ] Sprint retrospective completed

---

## Sprint 3: Capabilities & Transports (Week 5-6)
**Focus**: Medium priority capability and transport gaps
**Capacity**: 60 hours
**Gaps**: 14 gaps

### Sprint 3 Goals
1. Complete JSON Schema validation across capabilities
2. Add capability list_changed notifications
3. Improve transport test coverage
4. Implement missing transport features

### Sprint 3 Detailed Tasks

#### Task 3.1: Comprehensive JSON Schema Validation (P2-CAP-001)
**Gap**: No comprehensive JSON Schema validation across capabilities
**Location**: Multiple capability modules
**Estimate**: 12 hours

**Implementation Steps**:
1. Centralize JSON Schema validation
   ```erlang
   -module(erlmcp_schema_validator).
   -export([validate/2, validate_tool_input/2, validate_prompt_args/2]).

   validate(Data, Schema) ->
       jesse:validate(Data, Schema, [{default_schema_ver, 7}]).
   ```

2. Add validation to all capabilities
   - Tool input schemas
   - Prompt argument schemas
   - Resource metadata schemas
   - Sampling parameter schemas

3. Implement schema caching
   - Cache compiled schemas
   - Invalidate on schema change
   - Track validation performance

**Acceptance Criteria**:
- [ ] Centralized validator implemented
- [ ] All capabilities use validator
- [ ] Schema caching improves performance
- [ ] Validation errors include details
- [ ] All existing tests pass
- [ ] New tests for schema validation

**Dependencies**: None
**Risk**: Medium - changes validation logic

---

#### Task 3.2: List Changed Notifications (P2-CAP-002)
**Gap**: Missing list_changed notifications for all capabilities
**Location**: `erlmcp_server.erl`
**Estimate**: 10 hours

**Implementation Steps**:
1. Implement list_changed notification
   ```erlang
   notify_list_changed(Capability) ->
       Clients = get_subscribed_clients(Capability),
       Notification = #{
           jsonrpc => <<"2.0">>,
           method => <<Capability/binary, "/list_changed">>,
           params => #{}
       },
       send_to_clients(Clients, Notification).
   ```

2. Add notification triggers
   - After tool registration/removal
   - After resource registration/removal
   - After prompt registration/removal

3. Implement subscription management
   ```erlang
   capabilities/subscribe(Capability) -> ok.
   capabilities/unsubscribe(Capability) -> ok.
   ```

**Acceptance Criteria**:
- [ ] List_changed notifications sent on changes
- [ ] Subscribed clients receive notifications
- [ ] Subscription management works
- [ ] All existing tests pass
- [ ] New tests for list_changed

**Dependencies**: Task 3.1 (schema validation)
**Risk**: Low - adds optional feature

---

#### Task 3.3: Content-Type Handling (P2-CAP-003)
**Gap**: No proper MIME type validation or content negotiation
**Location**: `erlmcp_server.erl:131-210`
**Estimate**: 8 hours

**Implementation Steps**:
1. Add MIME type validation
   ```erlang
   -module(erlmcp_mime_types).
   -export([validate/1, negotiate/2]).

   -define(VALID_MIME_TYPES, [
       <<"application/json">>,
       <<"text/plain">>,
       <<"text/markdown">>,
       <<"image/png">>,
       <<"image/jpeg">>
   ]).
   ```

2. Implement content negotiation
   - Parse Accept headers
   - Match against available types
   - Return best match

3. Add MIME type to resource responses
   - Include Content-Type in response
   - Validate resource MIME type
   - Support charset parameter

**Acceptance Criteria**:
- [ ] MIME types validated on registration
- [ ] Content negotiation works correctly
- [ ] Content-Type included in responses
- [ ] All existing tests pass
- [ ] New tests for MIME handling

**Dependencies**: None
**Risk**: Low - adds validation

---

#### Task 3.4: Prompt Template Variable Substitution (P2-CAP-004)
**Gap**: No template variable substitution engine
**Location**: `erlmcp_prompts.erl`
**Estimate**: 8 hours

**Implementation Steps**:
1. Implement Mustache-like template engine
   ```erlang
   -module(erlmcp_template_engine).
   -export([render/2]).

   render(Template, Variables) ->
       % Replace {{var}} with values
       % Handle sections {{#var}}...{{/var}}
       % Handle inverted {{^var}}...{{/var}}
   ```

2. Add variable validation
   - Validate variable names
   - Check for missing variables
   - Support default values

3. Integrate with prompts/get
   - Render template with arguments
   - Return rendered content
   - Cache compiled templates

**Acceptance Criteria**:
- [ ] Template engine renders Mustache syntax
- [ ] Variable substitution works correctly
- [ ] Sections and inverted sections handled
- [ ] Template caching improves performance
- [ ] All existing tests pass
- [ ] New tests for template engine

**Dependencies**: None
**Risk**: Medium - adds new module

---

#### Task 3.5: Sampling Model Preferences (P2-CAP-005)
**Gap**: Missing cost/speed/intelligence priorities
**Location**: `erlmcp_sampling.erl`
**Estimate**: 6 hours

**Implementation Steps**:
1. Add model preference support
   ```erlang
   -record(model_preferences, {
       cost_priority,      % low, medium, high
       speed_priority,     % low, medium, high
       intelligence_priority, % low, medium, high
       hints              % Additional hints
   }).
   ```

2. Implement preference scoring
   - Score available models by preferences
   - Select best matching model
   - Fallback to default model

3. Add preference validation
   - Validate priority combinations
   - Reject invalid hints

**Acceptance Criteria**:
- [ ] Model preferences supported
- [ ] Scoring algorithm works correctly
- [ ] Best model selected based on preferences
- [ ] Invalid preferences rejected
- [ ] All existing tests pass
- [ ] New tests for preferences

**Dependencies**: None
**Risk**: Low - adds optional feature

---

#### Task 3.6: HTTP Transport Integration Tests (P2-TRANS-001)
**Gap**: HTTP transport has no integration tests, only URL parsing tests
**Location**: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
**Estimate**: 8 hours

**Implementation Steps**:
1. Add real HTTP server tests
   ```erlang
   test_http_server_integration() ->
       {ok, Server} = start_http_server(),
       {ok, Client} = start_http_client(),
       {ok, Response} = send_request(Client, Request),
       verify_response(Response),
       stop_server(Server).
   ```

2. Test HTTP methods
   - GET requests with query params
   - POST requests with JSON body
   - DELETE requests

3. Test error handling
   - Connection refused
   - Timeout scenarios
   - 5xx error responses

**Acceptance Criteria**:
- [ ] HTTP server integration test passes
- [ ] All HTTP methods tested
- [ ] Error scenarios covered
- [ ] Test coverage ≥60% for HTTP transport
- [ ] All existing tests still pass

**Dependencies**: None
**Risk**: Low - adds tests only

---

#### Task 3.7: TCP Transport TLS/SSL Tests (P2-TRANS-002)
**Gap**: TCP transport has no TLS/SSL tests
**Location**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`
**Estimate**: 6 hours

**Implementation Steps**:
1. Generate test certificates
   - Create self-signed CA cert
   - Create server cert
   - Create client cert

2. Add TLS connection tests
   ```erlang
   test_tls_connection() ->
       {ok, Server} = start_tls_server(),
       {ok, Client} = start_tls_client(),
       verify_secure_connection(Client),
       stop_server(Server).
   ```

3. Test certificate validation
   - Valid certificates accepted
   - Invalid certificates rejected
   - Expired certificates rejected

**Acceptance Criteria**:
- [ ] TLS connection test passes
- [ ] Certificate validation tested
- [ ] Secure connection verified
- [ ] Test coverage ≥75% for TCP transport
- [ ] All existing tests still pass

**Dependencies**: None
**Risk**: Low - adds tests only

---

#### Task 3.8: WebSocket Real Connection Tests (P2-TRANS-003)
**Gap**: WebSocket has no real connection tests, only validation tests
**Location**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Estimate**: 10 hours

**Implementation Steps**:
1. Add real WebSocket server tests
   ```erlang
   test_websocket_server_integration() ->
       {ok, Server} = start_websocket_server(),
       {ok, Client} = connect_websocket_client(),
       {ok, Response} = send_message(Client, Message),
       verify_websocket_response(Response),
       disconnect(Client).
   ```

2. Test WebSocket handshake
   - Upgrade header validation
   - Sec-WebSocket-Key handling
   - Protocol negotiation

3. Test connection lifecycle
   - Connect, send, receive
   - Reconnection scenarios
   - Close code validation

**Acceptance Criteria**:
- [ ] WebSocket server test passes
- [ ] Handshake validation tested
- [ ] Connection lifecycle covered
- [ ] Test coverage ≥70% for WebSocket
- [ ] All existing tests still pass

**Dependencies**: None
**Risk**: Low - adds tests only

---

### Sprint 3 Completion Criteria

- [ ] All 14 tasks completed
- [ ] All new tests passing
- [ ] All existing tests still passing
- [ ] Code coverage ≥80% for new code
- [ ] Capability and transport docs updated
- [ ] Sprint retrospective completed

---

## Sprint 4: Test Coverage & Polish (Week 7-8)
**Focus**: Remaining test gaps and polish
**Capacity**: 55 hours
**Gaps**: 14 gaps

### Sprint 4 Goals
1. Complete remaining test coverage gaps
2. Add property-based tests
3. Improve edge case coverage
4. Documentation and examples

### Sprint 4 Detailed Tasks

#### Task 4.1: Refusal Code Tests (P2-TEST-001)
**Gap**: Zero coverage of refusal codes (1001-1089)
**Location**: New test file
**Estimate**: 10 hours

**Implementation Steps**:
1. Create comprehensive refusal code test suite
   ```erlang
   -module(erlmcp_refusal_codes_tests).
   ```

2. Test all refusal code categories
   - Queue errors (1001-1005)
   - Auth errors (1011-1016)
   - Validation errors (1021-1029)
   - Resource errors (1046-1052)
   - Rate limit errors (1056-1060)
   - Transport errors (1066-1070)
   - Server state errors (1076-1080)
   - Circuit breaker errors (1086-1089)

3. Verify error format
   - Correct error code returned
   - Error message present
   - Error data includes context

**Acceptance Criteria**:
- [ ] All refusal codes tested
- [ ] Error format verified
- [ ] Error context validated
- [ ] Test coverage ≥90% for refusal codes

**Dependencies**: Task 2.1 (refusal code integration)
**Risk**: Low - adds tests only

---

#### Task 4.2: Edge Case Coverage (P2-TEST-002)
**Gap**: Limited edge case coverage (boundary conditions, error recovery)
**Location**: Multiple test files
**Estimate**: 12 hours

**Implementation Steps**:
1. Add boundary condition tests
   - Empty inputs
   - Maximum sizes
   - Unicode handling
   - Concurrent limits
   - Timeout values

2. Add error recovery tests
   - Process crash during request
   - Network partition
   - Resource exhaustion
   - Invalid state transitions

3. Add concurrent access tests
   - Race conditions
   - Deadlock scenarios
   - Message ordering

**Acceptance Criteria**:
- [ ] Boundary conditions tested
- [ ] Error recovery validated
- [ ] Concurrent access covered
- [ ] Overall edge case coverage ≥75%

**Dependencies**: None
**Risk**: Low - adds tests only

---

#### Task 4.3: Property-Based Tests (P2-TEST-003)
**Gap**: Only 3 Proper properties defined (need 10+ for invariants)
**Location**: Multiple test files
**Estimate**: 8 hours

**Implementation Steps**:
1. Add JSON-RPC encoding properties
   ```erlang
   prop_encode_decode_roundtrip() ->
       ?FORALL(Request, jsonrpc_request_gen(),
           begin
               Encoded = erlmcp_json_rpc:encode(Request),
               Decoded = erlmcp_json_rpc:decode(Encoded),
               Request =:= Decoded
           end).
   ```

2. Add message validation properties
   - Valid messages always accepted
   - Invalid messages always rejected
   - Roundtrip preservation

3. Add transport properties
   - Message ordering preserved
   - No message loss
   - Framing correctness

**Acceptance Criteria**:
- [ ] At least 10 properties defined
- [ ] Properties cover critical invariants
- [ ] Properties run without counterexamples
- [ ] Property-based test coverage ≥60%

**Dependencies**: None
**Risk**: Low - adds tests only

---

#### Task 4.4: Transport Stress Tests (P2-TEST-004)
**Gap**: No load tests, stress tests, or memory leak validation
**Location**: New test files
**Estimate**: 10 hours

**Implementation Steps**:
1. Create load test suite
   ```erlang
   -module(erlmcp_transport_load_tests).
   ```

2. Implement throughput tests
   - Messages per second per transport
   - Sustained load (5 minutes)
   - Peak load handling

3. Implement stress tests
   - Maximum concurrent connections
   - Large message handling
   - Rapid connection cycling

4. Implement memory leak tests
   - Long-running connections
   - Message accumulation
   - Process cleanup verification

**Acceptance Criteria**:
- [ ] Throughput benchmarks established
- [ ] Stress tests pass without crashes
- [ ] No memory leaks detected
- [ ] Performance regression <10%

**Dependencies**: None
**Risk**: Low - adds tests only

---

#### Task 4.5: Integration Test Expansion (P2-TEST-005)
**Gap**: Limited multi-transport and end-to-end integration tests
**Location**: `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl`
**Estimate**: 8 hours

**Implementation Steps**:
1. Add multi-transport scenarios
   - Simultaneous stdio + TCP
   - Transport switching during runtime
   - Transport failover

2. Add end-to-end workflows
   - Complete client-server lifecycle
   - Tool execution with progress
   - Resource subscription flow

3. Add cross-module integration
   - Client → Registry → Server
   - Transport → Registry → Client
   - Auth → Server → Resource access

**Acceptance Criteria**:
- [ ] Multi-transport scenarios tested
- [ ] End-to-end workflows validated
- [ ] Cross-module integration covered
- [ ] Integration test coverage ≥85%

**Dependencies**: None
**Risk**: Low - adds tests only

---

#### Task 4.6: Documentation Updates (P2-DOC-001)
**Gap**: Documentation needs updates for new features
**Location**: Multiple documentation files
**Estimate**: 7 hours

**Implementation Steps**:
1. Update API documentation
   - Document new APIs (session, progress, versioning)
   - Update error code documentation
   - Add examples for new features

2. Update architecture docs
   - Document session management
   - Document resource versioning
   - Document notification queue

3. Update testing docs
   - Document test patterns
   - Add test writing guidelines
   - Include coverage targets

**Acceptance Criteria**:
- [ ] All new APIs documented
- [ ] Architecture updated
- [ ] Testing guidelines documented
- [ ] Examples provided for all features

**Dependencies**: All implementation tasks
**Risk**: Low - documentation only

---

### Sprint 4 Completion Criteria

- [ ] All 14 tasks completed
- [ ] All new tests passing
- [ ] All existing tests still passing
- [ ] Overall test coverage ≥85%
- [ ] All documentation updated
- [ ] Sprint retrospective completed

---

## Summary & Effort Estimation

### Overall Effort by Sprint

| Sprint | Focus | Tasks | Hours | Completion |
|--------|-------|-------|-------|------------|
| **Sprint 1** | Core Protocol | 10 | 60 | Week 2 |
| **Sprint 2** | Error Handling | 9 | 60 | Week 4 |
| **Sprint 3** | Capabilities & Transports | 14 | 60 | Week 6 |
| **Sprint 4** | Test Coverage & Polish | 14 | 55 | Week 8 |
| **TOTAL** | **All P2 Gaps** | **47** | **235** | **8 weeks** |

### Effort by Category

| Category | P2 Count | Hours | Percentage |
|----------|----------|-------|------------|
| **Implementation** | 28 | 140 | 60% |
| **Test Coverage** | 11 | 55 | 23% |
| **Documentation** | 1 | 7 | 3% |
| **Integration** | 7 | 33 | 14% |
| **TOTAL** | **47** | **235** | **100%** |

### Risk Assessment

| Risk Category | Count | Mitigation |
|---------------|-------|------------|
| **High Risk** | 3 | Extensive testing, gradual rollout |
| **Medium Risk** | 12 | Code review, staging validation |
| **Low Risk** | 32 | Standard testing procedures |

### Dependencies

**Internal Dependencies**:
- Task 1.3 (Resource Versioning) → Task 2.2 (Missing Error Codes)
- Task 2.1 (Refusal Code Integration) → Task 2.3 (Severity Routing)
- Task 3.1 (Schema Validation) → Task 3.2 (List Changed)
- Task 2.2 (Missing Error Codes) → Task 2.4 (Error Format)
- Task 1.5 (Batch Size) → Task 2.5 (Batch Failure)
- All Implementation → Task 4.6 (Documentation)

**External Dependencies**:
- None (all P2 gaps are self-contained)

### Success Criteria

**Quantitative**:
- [ ] All 47 P2 gaps addressed
- [ ] 235 hours of effort completed
- [ ] Test coverage ≥85% overall
- [ ] Zero regression in existing tests
- [ ] Performance regression <10%

**Qualitative**:
- [ ] Production readiness improved
- [ ] MCP specification compliance ≥90%
- [ ] Code quality maintained
- [ ] Documentation comprehensive

---

## Next Steps

### Immediate Actions (Week 1)
1. Review and approve this plan
2. Assign tasks to team members
3. Set up sprint tracking
4. Begin Sprint 1 implementation

### Tracking & Reporting
- Weekly sprint reviews
- Bi-weekly stakeholder updates
- Monthly progress reports
- Final retrospective after Sprint 4

### After P2 Completion
- Conduct gap analysis re-audit
- Update compliance scorecard
- Plan P3 (Low Priority) gaps if needed
- Celebrate milestone completion

---

**Plan Created**: 2026-01-30
**Planner**: Agent 19 - Medium Priority Gap Fix Planner
**Status**: Ready for Execution
**Next Review**: After Sprint 1 Completion
