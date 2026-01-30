# MCP 2025-11-25 Compliance Gap Analysis

**Analysis Date:** 2025-01-30
**Current erlmcp Version:** 2.1.0
**Target MCP Specification:** 2025-11-25
**Analyst:** SPARC Orchestrator

---

## Executive Summary

The erlmcp SDK implements **core MCP 2025-11-25 features** but has **significant gaps** in new experimental features introduced in the latest specification. This analysis identifies **23 high-priority gaps** across experimental features, authorization, metadata support, and testing infrastructure.

**Compliance Status:** ~65% compliant with MCP 2025-11-25 specification
- ✅ **Fully Implemented:** Core protocol, resources, tools, prompts, logging
- ⚠️ **Partially Implemented:** Sampling, authorization (OAuth incomplete)
- ❌ **Not Implemented:** Tasks (experimental), Completion (experimental), Elicitation (experimental), Icons, SSE improvements

---

## 1. MISSING EXPERIMENTAL FEATURES (CRITICAL)

### 1.1 Tasks API (EXPERIMENTAL) - HIGHEST PRIORITY

**Status:** ❌ NOT IMPLEMENTED
**Spec Reference:** [Tasks - MCP 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25/basic/utilities/tasks)
**Priority:** P0 - Critical for long-running workflows

**Missing Components:**
- `tasks/create` - Create async task from any request
- `tasks/list` - List all tasks
- `tasks/get` - Get task status and metadata
- `tasks/result` - Retrieve completed task result
- `tasks/cancel` - Cancel in-flight task
- `notifications/tasks/status` - Task status updates

**Implementation Requirements:**
```erlang
-record(mcp_task, {
    id :: binary(),                           % Unique task ID
    status :: pending | processing | completed | failed | cancelled,
    request_id :: json_rpc_id(),              % Original request ID
    method :: binary(),                       % Original method
    params :: map(),                          % Original params
    result :: term() | undefined,             % Result when completed
    error :: #mcp_error{} | undefined,        % Error if failed
    created_at :: integer(),                  % Creation timestamp
    updated_at :: integer(),                  % Last update timestamp
    expires_at :: integer() | undefined,      % Optional expiration
    metadata :: map() | undefined             % Task metadata
}).

%% Task Manager gen_server
-module(erlmcp_task_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_task/3,              % Create task from request
    list_tasks/0,                % List all tasks
    get_task/1,                  % Get task by ID
    get_task_result/1,           % Get task result
    cancel_task/1,               % Cancel task
    cleanup_expired_tasks/0      % Cleanup expired tasks
]).
```

**Integration Points:**
- Modify `erlmcp_server` to check if request should be task-ified
- Add `experimental` capability flag: `#{<<"tasks">> => #{}}`
- Implement task persistence (ETS or database)
- Add task status notification support
- Implement polling and deferred result retrieval

**Estimated Effort:** 3-5 days (design + implementation + testing)

---

### 1.2 Completion Capability (EXPERIMENTAL)

**Status:** ❌ NOT IMPLEMENTED
**Spec Reference:** MCP 2025-11-25 Completion API
**Priority:** P1 - High value for IDE integration

**Missing Components:**
- `completion/complete` - Complete text prompt
- Completion capability negotiation
- Context-aware completion support

**Implementation Requirements:**
```erlang
-record(mcp_completion_capability, {
    enabled = false :: boolean()
}).

%% Completion handler
-type completion_handler() :: fun((
    binary(),    % Prompt text
    map()        % Context options
) -> {
    ok,
    binary(),    % Completion text
    map()        % Metadata
} | {error, term()}).
```

**Estimated Effort:** 2-3 days

---

### 1.3 Elicitation Capability (EXPERIMENTAL)

**Status:** ⚠️ PARTIALLY IMPLEMENTED (error codes exist)
**Spec Reference:** SEP-1036, SEP-1330
**Priority:** P1 - User interaction enhancement

**Missing Components:**
- `elicitation/create` - Create elicitation request
- `notifications/elicitation/complete` - Complete elicitation
- URL mode elicitation support (SEP-1036)
- Enhanced enum schema support (SEP-1330) - titled/untitled, single/multi-select
- Default values for all primitive types (SEP-1034)

**Existing Implementation:**
- ✅ Error code defined: `?MCP_ERROR_URL_ELICITATION_REQUIRED (-32042)`
- ✅ Capability constants defined in header

**Required Enhancements:**
```erlang
-record(mcp_elicitation_capability, {
    enabled = false :: boolean()
}).

-record(mcp_elicitation_request, {
    id :: binary(),
    schema :: map(),                        % JSON Schema for elicitation
    url_mode = false :: boolean(),          % URL mode support
    default_values :: map() | undefined     % Default values
}).

%% Enum schema enhancements (SEP-1330)
-record(mcp_enum_schema, {
    title :: binary() | undefined,          % Titled enum
    multiple = false :: boolean(),          % Multi-select support
    values :: [term()],                     % Enum values
    default :: term() | undefined           % Default value
}).
```

**Estimated Effort:** 2-3 days

---

## 2. AUTHORIZATION & SECURITY GAPS

### 2.1 OAuth 2.0 Enhancements

**Status:** ⚠️ PARTIALLY IMPLEMENTED (basic auth exists)
**Spec References:** PR #797, SEP-835, SEP-985, SEP-991
**Priority:** P0 - Security critical

**Existing Implementation:**
- ✅ `erlmcp_auth.erl` module exists
- ✅ Basic authentication error codes defined

**Missing Components:**
1. **OpenID Connect Discovery 1.0** (PR #797)
   - Automatic authorization server discovery
   - `/.well-known/openid-configuration` endpoint

2. **Incremental Scope Consent via WWW-Authenticate** (SEP-835)
   - Dynamic scope request/response
   - WWW-Authenticate header parsing

3. **OAuth Client ID Metadata Documents** (SEP-991, PR #1296)
   - Client registration mechanism
   - Metadata document validation

4. **RFC 9728 Protected Resource Metadata** (SEP-985)
   - Authorization metadata endpoint
   - WWW-Authenticate optional (fallback to `.well-known`)

**Implementation Requirements:**
```erlang
-record(mcp_oauth_config, {
    discovery_url :: binary() | undefined,     % OpenID Connect discovery URL
    client_id :: binary(),                      % OAuth client ID
    client_secret :: binary() | undefined,      % OAuth client secret
    scopes :: [binary()],                       % Requested scopes
    authorization_endpoint :: binary() | undefined,
    token_endpoint :: binary() | undefined,
    metadata_endpoint :: binary() | undefined   % RFC 9728 metadata endpoint
}).

-record(mcp_auth_challenge, {
    realm :: binary(),
    scope :: [binary()],                        % Requested scopes
    error :: binary() | undefined,              % Error code
    error_description :: binary() | undefined   % Error details
}).

%% Enhanced auth module
-module(erlmcp_auth_oauth).
-export([
    discover_endpoints/1,              % OpenID Connect discovery
    request_incremental_scope/2,       % Request additional scopes
    validate_client_metadata/1,        % Validate client metadata
    fetch_resource_metadata/1          % RFC 9728 metadata
]).
```

**Estimated Effort:** 5-7 days (full OAuth 2.0 compliance)

---

### 2.2 Security Best Practices Updates

**Status:** ⚠️ NEEDS REVIEW
**Priority:** P0 - Security compliance

**Action Items:**
- Update security documentation per 2025-11-25 best practices
- Implement tool description validation (annotations considered untrusted)
- Enhance user consent flows for LLM sampling
- Add server visibility limits for prompts

**Estimated Effort:** 2 days (documentation + validation)

---

## 3. METADATA & ICONS SUPPORT

### 3.1 Icons for Tools, Resources, Prompts

**Status:** ⚠️ PARTIALLY IMPLEMENTED (icon cache exists)
**Spec Reference:** SEP-973
**Priority:** P2 - UI enhancement

**Existing Implementation:**
- ✅ `erlmcp_icon_cache.erl` module exists
- ✅ Basic icon caching infrastructure

**Missing Components:**
- Icon field in tool metadata
- Icon field in resource metadata
- Icon field in resource template metadata
- Icon field in prompt metadata
- Icon URL validation and fetching

**Implementation Requirements:**
```erlang
%% Extend existing records
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map() | undefined,
    metadata :: map() | undefined,
    experimental = undefined :: map() | undefined,
    version :: binary() | undefined,
    deprecated = false :: boolean(),
    icon :: binary() | undefined            % NEW: Icon URL
}).

-record(mcp_resource, {
    uri :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    mime_type :: binary() | undefined,
    metadata :: map() | undefined,
    audience :: binary() | undefined,
    priority :: integer() | undefined,
    last_modified :: integer() | undefined,
    annotations :: map() | undefined,
    size :: integer() | undefined,
    icon :: binary() | undefined            % NEW: Icon URL
}).

-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined,
    input_schema :: map() | undefined,
    icon :: binary() | undefined            % NEW: Icon URL
}).

%% Enhanced icon cache
-module(erlmcp_icon_cache).
-export([
    fetch_icon/1,                         % Fetch and cache icon
    validate_icon_url/1,                  % Validate icon URL
    get_icon_cached/1,                    % Get cached icon
    clear_cache/0                         % Clear icon cache
]).
```

**Estimated Effort:** 2 days

---

### 3.2 Server Implementation Description Field

**Status:** ❌ NOT IMPLEMENTED
**Spec Reference:** Minor change - optional `description` field
**Priority:** P3 - Low priority

**Implementation:**
```erlang
-record(mcp_implementation, {
    name :: binary(),
    version :: binary(),
    description :: binary() | undefined   % NEW: Optional description
}).

%% Update initialize response
%% Server info should include description if available
```

**Estimated Effort:** 0.5 days

---

## 4. SSE & POLLING IMPROVEMENTS

### 4.1 SSE Stream Polling Support

**Status:** ❌ NOT IMPLEMENTED
**Spec Reference:** SEP-1699
**Priority:** P1 - Important for HTTP transports

**Missing Components:**
- Server-initiated SSE disconnection support
- Polling GET stream support
- Stream resumption via GET (regardless of stream origin)
- Event ID encoding for stream identity
- Disconnection including server-initiated closure

**Implementation Requirements:**
```erlang
-record(mcp_sse_stream, {
    stream_id :: binary(),
    last_event_id :: binary() | undefined,
    polling_mode = false :: boolean(),      % Enable polling
    resumable = true :: boolean(),          % Support resumption
    expires_at :: integer() | undefined     % Stream expiration
}).

%% SSE event store enhancement
-module(erlmcp_sse_event_store).
-export([
    create_stream/1,                       % Create new SSE stream
    add_event/3,                           % Add event to stream
    get_events_since/2,                    % Poll events since ID
    disconnect_stream/1,                   % Server-initiated disconnect
    cleanup_expired_streams/0              % Cleanup
]).
```

**Estimated Effort:** 3 days

---

## 4.2 HTTP Origin Header Validation

**Status:** ⚠️ PARTIALLY IMPLEMENTED (HTTP transport exists)
**Spec Reference:** PR #1439
**Priority:** P0 - Security critical

**Missing Component:**
- HTTP 403 Forbidden response for invalid Origin headers in Streamable HTTP transport

**Implementation:**
```erlang
%% In HTTP transport module
validate_origin_header(Origin, AllowedOrigins) ->
    case AllowedOrigins of
        any -> ok;
        List when is_list(List) ->
            case lists:member(Origin, List) of
                true -> ok;
                false -> {error, forbidden}
            end;
        _ -> {error, forbidden}
    end.

%% Return 403 for invalid origins
handle_request(Req, State) ->
    Origin = get_origin_header(Req),
    case validate_origin_header(Origin, State#state.allowed_origins) of
        ok -> handle_request_authenticated(Req, State);
        {error, forbidden} -> respond_403_forbidden(Req)
    end.
```

**Estimated Effort:** 1 day

---

## 5. SCHEMA VALIDATION ENHANCEMENTS

### 5.1 JSON Schema 2020-12 Default

**Status:** ⚠️ PARTIALLY IMPLEMENTED (using jesse)
**Spec Reference:** SEP-1613
**Priority:** P2 - Standards compliance

**Current Implementation:**
- ✅ Using `jesse` library for JSON Schema validation
- ✅ Schema validation for tools and prompts

**Required Enhancement:**
- Ensure JSON Schema 2020-12 is the default dialect
- Update validation error messages to match 2020-12 spec
- Add schema version detection and validation

**Implementation:**
```erlang
%% Schema registry enhancement
-module(erlmcp_schema_registry).
-export([
    register_schema/3,                    % Register schema with version
    validate_with_dialect/3,              % Validate with specific dialect
    get_default_dialect/0                 % Get default (should be 2020-12)
]).

-define(JSON_SCHEMA_2020_12, <<"https://json-schema.org/draft/2020-12/schema">>).
-define(DEFAULT_SCHEMA_DIALECT, ?JSON_SCHEMA_2020_12).
```

**Estimated Effort:** 1-2 days

---

### 5.2 Tool Name Guidance Compliance

**Status:** ⚠️ NEEDS VALIDATION
**Spec Reference:** SEP-986
**Priority:** P3 - Documentation

**Action Items:**
- Document tool naming conventions per SEP-986
- Add tool name validation (optional)
- Update examples to follow naming guidance

**Estimated Effort:** 0.5 days

---

## 6. ERROR HANDLING IMPROVEMENTS

### 6.1 Input Validation vs Protocol Errors

**Status:** ⚠️ PARTIALLY COMPLIANT
**Spec Reference:** SEP-1303
**Priority:** P2 - Error handling improvement

**Required Change:**
- Input validation errors should return **Tool Execution Errors** (not Protocol Errors)
- This enables model self-correction

**Current Implementation:**
```erlang
%% Current: May return protocol errors for validation
case validate_tool_input(Args, Schema) of
    {error, Reason} ->
        {error, #mcp_error{
            code = ?JSONRPC_INVALID_PARAMS,  % Protocol error - WRONG
            message = <<"Validation failed">>
        }}
end.
```

**Required Implementation:**
```erlang
%% Correct: Return tool execution error for input validation
case validate_tool_input(Args, Schema) of
    {error, Reason} ->
        {error, #mcp_error{
            code = ?MCP_ERROR_INVALID_TOOL_ARGUMENTS,  % Tool error - CORRECT
            message = <<"Invalid tool arguments">>,
            data = #{<<"validation">> => Reason}
        }}
end.
```

**Estimated Effort:** 1 day (audit + fix all validation paths)

---

## 7. TESTING & VALIDATION GAPS

### 7.1 Compliance Test Suite

**Status:** ❌ NOT IMPLEMENTED
**Priority:** P0 - Quality assurance

**Missing Components:**
- MCP 2025-11-25 compliance test suite
- Protocol version negotiation tests
- Capability negotiation tests
- Error code validation tests
- Experimental feature tests (tasks, completion, elicitation)

**Implementation Requirements:**
```erlang
%% Compliance test suite
-module(erlmcp_compliance_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test categories
-export([
    test_core_protocol/0,
    test_resources_capability/0,
    test_tools_capability/0,
    test_prompts_capability/0,
    test_logging_capability/0,
    test_sampling_capability/0,
    test_tasks_experimental/0,            % NEW
    test_completion_experimental/0,       % NEW
    test_elicitation_experimental/0,      % NEW
    test_oauth_flow/0,                    % NEW
    test_sse_polling/0                    % NEW
]).
```

**Estimated Effort:** 5-7 days (comprehensive suite)

---

### 7.2 SDK Tiering Compliance (SEP-1730)

**Status:** ❌ NOT DOCUMENTED
**Spec Reference:** SEP-1730
**Priority:** P2 - Documentation

**Required Documentation:**
- Define erlmcp SDK tier (Basic, Standard, Advanced)
- Document feature support matrix
- Document maintenance commitments per tier

**Implementation:**
```markdown
# erlmcp SDK Tier Classification

## Current Tier: Standard
- Core protocol: ✅ Full support
- Resources: ✅ Full support
- Tools: ✅ Full support
- Prompts: ✅ Full support
- Logging: ✅ Full support
- Sampling: ⚠️ Basic support
- Roots: ⚠️ Basic support
- Tasks (experimental): ❌ Not supported
- Completion (experimental): ❌ Not supported
- Elicitation (experimental): ❌ Not supported

## Target Tier: Advanced (Future)
- All Standard features
- Tasks: ✅ Full support
- Completion: ✅ Full support
- Elicitation: ✅ Full support
- OAuth 2.0: ✅ Full support
```

**Estimated Effort:** 1 day

---

## 8. MINOR GAPS & CLEANUP

### 8.1 Stdio Transport stderr Logging

**Status:** ⚠️ ALREADY COMPLIANT
**Spec Reference:** PR #670
**Priority:** P4 - Documentation only

**Action:**
- Document that stdio transport uses stderr for all logging (not just errors)

**Estimated Effort:** 0.5 days

---

### 8.2 Deprecated Feature Removal

**Status:** ⚠️ NEEDS AUDIT
**Priority:** P3 - Housekeeping

**Action Items:**
- Audit code for deprecated features from 2024-11-05 spec
- Remove or mark deprecated APIs
- Update documentation

**Estimated Effort:** 1-2 days

---

## 9. PRIORITIZED IMPLEMENTATION ROADMAP

### Phase 1: Critical Compliance (2-3 weeks)
**Target:** Achieve 80% compliance with MCP 2025-11-25

1. **Tasks API Implementation** (5 days)
   - Design task manager architecture
   - Implement task CRUD operations
   - Add task status notifications
   - Write comprehensive tests

2. **OAuth 2.0 Enhancements** (5 days)
   - OpenID Connect discovery
   - Incremental scope consent
   - Client metadata validation
   - Security documentation updates

3. **SSE Polling Support** (3 days)
   - Server-initiated disconnect
   - GET stream polling
   - Stream resumption

4. **HTTP Origin Validation** (1 day)
   - Origin header validation
   - 403 Forbidden response

**Deliverables:**
- Tasks API fully functional
- OAuth 2.0 compliance
- SSE polling support
- HTTP security hardening

---

### Phase 2: Experimental Features (1-2 weeks)
**Target:** Add experimental capabilities

1. **Completion API** (3 days)
   - Completion handler interface
   - Context-aware completion
   - Integration with tools

2. **Elicitation API** (3 days)
   - Elicitation request/response
   - URL mode support
   - Enhanced enum schemas
   - Default values support

3. **Icons Support** (2 days)
   - Icon metadata in resources/tools/prompts
   - Icon fetching and caching
   - URL validation

**Deliverables:**
- Completion capability functional
- Elicitation capability functional
- Icons fully supported

---

### Phase 3: Testing & Documentation (1 week)
**Target:** Production-ready compliance

1. **Compliance Test Suite** (5 days)
   - Core protocol tests
   - Capability tests
   - Experimental feature tests
   - OAuth flow tests

2. **Documentation Updates** (2 days)
   - SDK tier classification
   - Security best practices
   - API documentation updates
   - Migration guide

**Deliverables:**
- 90%+ test coverage
- Complete documentation
- Compliance certification

---

## 10. COMPLIANCE TESTING STRATEGY

### 10.1 Automated Compliance Testing

```erlang
%% Compliance test runner
-module(erlmcp_compliance_runner).
-export([
    run_all_compliance_tests/0,
    run_category_tests/1,
    generate_compliance_report/0
]).

%% Test categories
-define(COMPLIANCE_CATEGORIES, [
    {core, [
        protocol_version_negotiation,
        initialize_handshake,
        json_rpc_2_0_compliance,
        error_code_validity
    ]},
    {capabilities, [
        resources_capability,
        tools_capability,
        prompts_capability,
        logging_capability
    ]},
    {experimental, [
        tasks_api,
        completion_api,
        elicitation_api
    ]},
    {security, [
        oauth_flow,
        origin_validation,
        input_separation
    ]}
]).
```

### 10.2 Manual Compliance Testing

**Test Scenarios:**
1. Interoperability with official MCP clients (Claude Desktop, etc.)
2. OAuth 2.0 flow with real authorization servers
3. Long-running task workflows
4. SSE polling under various network conditions
5. Error recovery and graceful degradation

### 10.3 Continuous Compliance Monitoring

```erlang
%% Compliance checker
-module(erlmcp_compliance_checker).
-export([
    check_compliance/0,
    check_capability_support/1,
    check_experimental_features/0,
    generate_compliance_badge/0
]).
```

---

## 11. RISK ASSESSMENT

### High-Risk Gaps (P0)
1. **Tasks API** - Blocks long-running workflow support
2. **OAuth 2.0 Enhancements** - Security compliance gap
3. **HTTP Origin Validation** - Security vulnerability
4. **Compliance Test Suite** - No validation of compliance claims

### Medium-Risk Gaps (P1)
1. **Completion API** - Missing IDE integration feature
2. **Elicitation API** - Missing user interaction feature
3. **SSE Polling** - Reliability issue for HTTP transports

### Low-Risk Gaps (P2-P3)
1. **Icons Support** - UI enhancement only
2. **Schema 2020-12** - Already mostly compliant
3. **Documentation** - Non-blocking

---

## 12. SUCCESS METRICS

### Compliance Metrics
- **Current:** 65% compliant with MCP 2025-11-25
- **Phase 1 Target:** 80% compliant
- **Phase 2 Target:** 90% compliant
- **Phase 3 Target:** 95%+ compliant

### Quality Metrics
- **Test Coverage:** 80%+ (currently ~75%)
- **Documentation Coverage:** 100% of public APIs
- **Security Audit:** 0 critical vulnerabilities

### Performance Metrics
- **No regression** in existing benchmarks
- **Task overhead:** <5% for async operations
- **SSE latency:** <100ms for polling responses

---

## 13. RECOMMENDATIONS

### Immediate Actions (Week 1)
1. Prioritize Tasks API implementation (highest value)
2. Fix HTTP Origin validation (security critical)
3. Begin OAuth 2.0 enhancement design

### Short-term Actions (Month 1)
1. Complete Phase 1 implementation (Critical Compliance)
2. Establish compliance testing framework
3. Update security documentation

### Long-term Actions (Quarter 1)
1. Complete Phase 2 (Experimental Features)
2. Achieve 95%+ compliance with MCP 2025-11-25
3. Publish compliance certification

### Ongoing Maintenance
1. Track MCP specification updates
2. Participate in MCP working groups
3. Maintain compliance test suite
4. Regular security audits

---

## APPENDICES

### Appendix A: MCP 2025-11-25 Change Summary

**Major Changes:**
1. Tasks API (experimental) - Async request handling
2. Completion API (experimental) - Text completion
3. Elicitation API (experimental) - User interaction
4. OAuth 2.0 enhancements - Security improvements
5. Icons support - UI metadata
6. SSE polling improvements - Reliability
7. JSON Schema 2020-12 - Validation standard

**Minor Changes:**
1. Server description field
2. Stdio stderr logging clarification
3. Tool naming guidance
4. Error handling improvements
5. SDK tiering system

### Appendix B: Current Implementation Status

| Feature | Status | Completion | Priority |
|---------|--------|------------|----------|
| Core Protocol | ✅ Complete | 100% | - |
| Resources | ✅ Complete | 100% | - |
| Tools | ✅ Complete | 100% | - |
| Prompts | ✅ Complete | 100% | - |
| Logging | ✅ Complete | 100% | - |
| Sampling | ⚠️ Partial | 70% | P2 |
| Roots | ⚠️ Basic | 60% | P2 |
| Tasks | ❌ Missing | 0% | P0 |
| Completion | ❌ Missing | 0% | P1 |
| Elicitation | ❌ Missing | 10% | P1 |
| OAuth 2.0 | ⚠️ Partial | 40% | P0 |
| Icons | ⚠️ Partial | 30% | P2 |
| SSE Polling | ❌ Missing | 0% | P1 |
| Compliance Tests | ❌ Missing | 0% | P0 |

### Appendix C: References

- [MCP 2025-11-25 Specification](https://modelcontextprotocol.io/specification/2025-11-25)
- [Key Changes](https://modelcontextprotocol.io/specification/2025-11-25/changelog)
- [Tasks API](https://modelcontextprotocol.io/specification/2025-11-25/basic/utilities/tasks)
- [MCP GitHub Schema](https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2025-11-25/schema.ts)
- [SEP-973: Icons for Tools/Resources/Prompts](https://github.com/modelcontextprotocol/modelcontextprotocol/pull/973)
- [SEP-835: Incremental Scope Consent](https://github.com/modelcontextprotocol/modelcontextprotocol/pull/835)
- [SEP-1303: Input Validation Errors](https://github.com/modelcontextprotocol/modelcontextprotocol/pull/1303)
- [SEP-1613: JSON Schema 2020-12](https://github.com/modelcontextprotocol/modelcontextprotocol/pull/1613)
- [SEP-1699: SSE Polling Streams](https://github.com/modelcontextprotocol/modelcontextprotocol/pull/1699)
- [SEP-1730: SDK Tiering System](https://github.com/modelcontextprotocol/modelcontextprotocol/pull/1730)

---

**Document Version:** 1.0
**Last Updated:** 2025-01-30
**Next Review:** After Phase 1 completion
