# Security Remediation Roadmap
## erlmcp Implementation - MCP 2025-11-25 Compliance

**Date:** February 2, 2026
**Version:** 1.0
**Status:** ACTIVE

---

## EXECUTIVE SUMMARY

This roadmap prioritizes and schedules security gap remediation across three phases over 8 weeks, with clear deliverables, effort estimates, and success criteria.

### Key Metrics

| Phase | Duration | Effort | Priority | Gaps Fixed | Release Ready |
|-------|----------|--------|----------|-----------|---------------|
| **Phase 1** | 2 weeks | 60 days | P0 | 5 critical | ⚠️ Partial |
| **Phase 2** | 3 weeks | 90 days | P1 | 8 high | ✅ Production |
| **Phase 3** | 3 weeks | 70 days | P2 | 15+ medium | ✅ Hardened |

**Total Effort:** 220 person-days (40 per week for 5.5 weeks)

---

## PHASE 1: CRITICAL SECURITY GAPS (Weeks 1-2)

**Goal:** Fix security-blocking issues that prevent production release

**Timeline:** Feb 3 - Feb 16, 2026
**Team Size:** 4-5 engineers
**Effort:** ~60 days

### 1.1 CRITICAL: Enforce HTTPS in Production

**Gap ID:** G2, S-003
**Severity:** CRITICAL
**Sprint:** Week 1, Day 1-2

#### Requirements
```erlang
%% sys.config configuration option
{erlmcp, [
    {enforce_https, true},           % Reject HTTP in production mode
    {production_mode, true},         % Enable production checks
    {hsts_max_age_seconds, 31536000} % 1 year HSTS header
]}
```

#### Implementation Tasks

1. **Add Configuration Flag** (0.5 days)
   - [ ] Add `enforce_https` config option
   - [ ] Add `production_mode` config option
   - [ ] Add `hsts_max_age_seconds` config option
   - [ ] Validate config on startup
   - [ ] Document in sys.config.example

2. **Implement HTTPS Enforcement** (1 day)
   - [ ] Modify transport handlers to check mode
   - [ ] Reject HTTP requests in production with 400 Bad Request
   - [ ] Add error message: "HTTP not allowed in production mode"
   - [ ] Log enforcement events
   - [ ] Allow HTTP in development mode with warning

3. **Add HSTS Header** (0.5 days)
   - [ ] Add HSTS header to all HTTPS responses
   - [ ] Include preload directive: `Strict-Transport-Security: max-age=31536000; includeSubDomains; preload`
   - [ ] Configure max-age from config
   - [ ] Test header presence in responses

#### Code Changes

**File:** `/apps/erlmcp_transports/src/erlmcp_transport_http_server.erl`

```erlang
handle_request(Req = #{scheme := Scheme}, State) ->
    case check_https_enforcement(Scheme) of
        ok ->
            %% Add HSTS header to response
            Headers = add_hsts_header(Req),
            process_request(Req, State);
        {error, https_required} ->
            {error, 400, <<"HTTP not allowed in production mode">>}
    end.

check_https_enforcement(https) -> ok;
check_https_enforcement(http) ->
    case is_production_mode() of
        true -> {error, https_required};
        false ->
            logger:warning("HTTP used in development mode"),
            ok
    end.

is_production_mode() ->
    application:get_env(erlmcp, production_mode, false).

add_hsts_header(Req) ->
    MaxAge = application:get_env(erlmcp, hsts_max_age_seconds, 31536000),
    HstsValue = iolist_to_binary([
        <<"max-age=">>,
        integer_to_binary(MaxAge),
        <<"; includeSubDomains; preload">>
    ]),
    maps:put(<<"strict-transport-security">>, HstsValue, Req).
```

#### Testing Tasks

1. **Unit Tests** (1 day)
   - [ ] Test HTTPS enforcement enabled/disabled
   - [ ] Test HSTS header presence and format
   - [ ] Test development vs production modes
   - [ ] Test config validation

2. **Integration Tests** (1 day)
   - [ ] End-to-end HTTPS enforcement
   - [ ] HTTP request rejected in production
   - [ ] HTTP allowed in development
   - [ ] HSTS preload validation

#### Success Criteria
- [x] HTTPS enforcement working in production mode
- [x] HTTP requests rejected with proper error
- [x] HSTS headers present in all HTTPS responses
- [x] No regression in existing tests
- [x] Code coverage ≥ 85%
- [x] Documentation updated

#### Deliverables
- [ ] Code changes with tests
- [ ] sys.config.example updated
- [ ] Deployment guide updated
- [ ] Test report

---

### 1.2 CRITICAL: HTTP Session Management (MCP-Session-Id)

**Gap ID:** G5
**Severity:** CRITICAL
**Sprint:** Week 1-2, Day 3-8

#### Requirements

MCP 2025-11-25 spec requires:
1. Unique `MCP-Session-Id` header in all HTTP responses
2. Session creation on first connection
3. Session validation on subsequent requests
4. Session expiration (default 5 minutes)
5. HTTP 404 for invalid/expired sessions
6. HTTP 400 for missing session ID in POST requests
7. HTTP DELETE endpoint for session termination

#### Implementation Tasks

1. **Implement Session Manager** (1.5 days)
   ```erlang
   -module(erlmcp_http_session_manager).
   -behaviour(gen_server).

   -export([start_link/0, create_session/1, get_session/1, terminate_session/1, cleanup_expired/0]).

   -record(session, {
       session_id :: binary(),
       created_at :: integer(),
       last_accessed :: integer(),
       timeout_ms :: pos_integer(),
       client_info :: map(),
       metadata :: map()
   }).

   create_session(ClientInfo) ->
       SessionId = generate_secure_id(),
       Session = #session{
           session_id = SessionId,
           created_at = erlang:system_time(millisecond),
           last_accessed = erlang:system_time(millisecond),
           timeout_ms = 300000,  % 5 minutes
           client_info = ClientInfo,
           metadata = #{}
       },
       gen_server:call(?MODULE, {create, Session}).

   get_session(SessionId) ->
       gen_server:call(?MODULE, {get, SessionId}).

   terminate_session(SessionId) ->
       gen_server:call(?MODULE, {terminate, SessionId}).

   cleanup_expired() ->
       gen_server:cast(?MODULE, cleanup_expired).
   ```

2. **Add Session Tracking to HTTP Transport** (2 days)
   - [ ] Modify HTTP handler to create sessions
   - [ ] Store session ID in response headers
   - [ ] Validate session ID in requests
   - [ ] Track last access time
   - [ ] Return 404 for expired sessions
   - [ ] Return 400 for missing session ID

3. **Add Session Cleanup** (1 day)
   - [ ] Implement periodic cleanup of expired sessions
   - [ ] Schedule cleanup every 1 minute
   - [ ] Log cleanup statistics
   - [ ] Verify no memory leaks

4. **Implement Session Deletion Endpoint** (1 day)
   - [ ] Add HTTP DELETE /mcp/{sessionId} handler
   - [ ] Return 204 No Content on success
   - [ ] Return 404 if session not found
   - [ ] Log session termination

#### Code Changes

**File:** `/apps/erlmcp_transports/src/erlmcp_transport_http_server.erl`

```erlang
handle_http_request(Req = #{method := Method, path := Path}, State) ->
    case Method of
        <<"POST">> ->
            %% Check for session ID in request
            case extract_session_id(Req) of
                {ok, SessionId} ->
                    %% Validate session
                    case erlmcp_http_session_manager:get_session(SessionId) of
                        {ok, Session} ->
                            %% Update last accessed
                            erlmcp_http_session_manager:update_access(SessionId),
                            process_request(Req, Session);
                        {error, not_found} ->
                            {error, 404, <<"Session not found">>};
                        {error, expired} ->
                            {error, 404, <<"Session expired">>}
                    end;
                undefined ->
                    {error, 400, <<"MCP-Session-Id header required">>}
            end;
        <<"GET">> ->
            %% For SSE connections, create new session
            {ok, SessionId} = erlmcp_http_session_manager:create_session(#{
                client_ip => get_client_ip(Req),
                user_agent => get_user_agent(Req)
            }),
            %% Stream with session ID
            stream_sse(Req, SessionId, State);
        <<"DELETE">> ->
            %% Extract session ID from path
            case extract_session_id_from_path(Path) of
                {ok, SessionId} ->
                    erlmcp_http_session_manager:terminate_session(SessionId),
                    {ok, 204, #{}, <<>>};
                undefined ->
                    {error, 400, <<"Invalid session path">>}
            end
    end.

stream_sse(Req, SessionId, State) ->
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"mcp-session-id">> => SessionId,  % REQUIRED
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>
    },
    start_sse_stream(Req, SessionId, Headers, State).

extract_session_id(Req) ->
    case maps:get(<<"mcp-session-id">>, Req, undefined) of
        undefined -> undefined;
        SessionId -> {ok, SessionId}
    end.

extract_session_id_from_path(Path) ->
    %% Path format: /mcp/{sessionId}
    case string:split(Path, "/") of
        [<<"mcp">>, SessionId] -> {ok, SessionId};
        _ -> undefined
    end.
```

#### Testing Tasks

1. **Unit Tests** (1 day)
   - [ ] Session creation
   - [ ] Session validation
   - [ ] Session expiration
   - [ ] Cleanup logic

2. **Integration Tests** (1.5 days)
   - [ ] Full HTTP flow with session management
   - [ ] Session persistence across requests
   - [ ] 404 for expired sessions
   - [ ] 400 for missing session ID
   - [ ] DELETE endpoint working
   - [ ] Last-Event-ID reconnection

#### Success Criteria
- [x] All HTTP responses include MCP-Session-Id header
- [x] Sessions created on first connection
- [x] Sessions expire after 5 minutes inactivity
- [x] Invalid sessions return 404
- [x] Missing session ID returns 400
- [x] DELETE endpoint removes sessions
- [x] No memory leaks in session storage
- [x] Test coverage ≥ 85%

#### Deliverables
- [ ] erlmcp_http_session_manager.erl module
- [ ] HTTP transport modifications
- [ ] 50+ test cases
- [ ] Session management documentation
- [ ] Deployment guide update

---

### 1.3 CRITICAL: SEP-1303 Input Validation Error Handling

**Gap ID:** G3
**Severity:** CRITICAL
**Sprint:** Week 1-2, Day 9-12

#### Requirements

SEP-1303 requires tool input validation errors to return **tool execution errors** (not protocol errors). This enables models to self-correct invalid inputs.

#### Implementation Tasks

1. **Audit Input Validation Paths** (1 day)
   - [ ] Find all tool input validation locations
   - [ ] Check error handling in each path
   - [ ] Document current behavior
   - [ ] Identify gaps (protocol errors instead of tool errors)

2. **Fix Validation Error Responses** (1.5 days)
   - [ ] Change from protocol errors to tool execution errors
   - [ ] Ensure error message is descriptive
   - [ ] Include validation details in response
   - [ ] Test with multiple tool types

3. **Update Tool Error Codes** (0.5 days)
   - [ ] Verify MCP_ERROR_INVALID_TOOL_ARGUMENTS error code
   - [ ] Add error detail structure
   - [ ] Document error format

#### Code Changes

**File:** `/apps/erlmcp_core/src/erlmcp_server.erl`

```erlang
%% OLD (WRONG): Protocol-level error
handle_tool_call(ToolName, Arguments, Principal, State) ->
    case validate_tool_arguments(ToolName, Arguments) of
        {error, Reason} ->
            {error, #mcp_error{
                code = ?JSONRPC_INVALID_PARAMS,  % WRONG: Protocol error
                message = <<"Invalid parameters">>
            }};
        {ok, ValidArgs} ->
            execute_tool(ToolName, ValidArgs)
    end.

%% NEW (CORRECT): Tool execution error
handle_tool_call(ToolName, Arguments, Principal, State) ->
    case validate_tool_arguments(ToolName, Arguments) of
        {error, ValidationError} ->
            %% Return tool execution error (not protocol error)
            {ok, #{
                type => <<"text">>,
                content => [#{
                    type => <<"text">>,
                    text => format_validation_error(ToolName, ValidationError)
                }],
                isError => true
            }};
        {ok, ValidArgs} ->
            execute_tool(ToolName, ValidArgs)
    end.

format_validation_error(ToolName, ValidationError) ->
    iolist_to_binary(io_lib:format(
        "Tool '~s' input validation failed: ~s",
        [ToolName, ValidationError]
    )).
```

#### Testing Tasks

1. **Unit Tests** (1 day)
   - [ ] Validation errors return tool execution errors
   - [ ] Error messages are descriptive
   - [ ] isError flag is true
   - [ ] No protocol errors for validation failures

2. **Integration Tests** (1 day)
   - [ ] End-to-end tool call with invalid input
   - [ ] Model can parse and correct error
   - [ ] Multiple validation failures
   - [ ] Different error types

#### Success Criteria
- [x] All input validation errors return tool execution errors
- [x] Protocol errors never returned for validation
- [x] Error messages clear and descriptive
- [x] Test coverage ≥ 90%
- [x] No regression in valid tool calls

#### Deliverables
- [ ] Code changes with tests
- [ ] Validation error handling audit
- [ ] Test report
- [ ] Migration guide (if breaking change)

---

### 1.4 HIGH: Add IP Binding to Sessions

**Gap ID:** G14, E-004
**Severity:** HIGH
**Sprint:** Week 2, Day 13-14

#### Requirements
- Bind sessions to client IP address
- Reject requests from different IPs
- Log binding violations
- Allow IP changes for mobile (optional retry mechanism)

#### Implementation Tasks

1. **Extend Session Record** (0.5 days)
   ```erlang
   -record(session, {
       session_id :: binary(),
       user_id :: binary(),
       binding :: #{
           ip_address => inet:ip_address(),
           user_agent => binary() | undefined,
           tls_unique => binary() | undefined
       },
       ...
   }).
   ```

2. **Implement Binding Validation** (1 day)
   - [ ] Extract client IP from request
   - [ ] Compare with session binding
   - [ ] Reject mismatched IPs
   - [ ] Log security violations
   - [ ] Support trusted proxy headers (X-Forwarded-For)

3. **Add Configuration** (0.5 days)
   ```erlang
   {erlmcp, [
       {session_binding, enabled},      % Enable IP binding
       {session_binding_strict, true},  % Reject mismatched IPs
       {trusted_proxy_headers, [       % Allow proxies
           <<"x-forwarded-for">>,
           <<"x-real-ip">>
       ]}
   ]}
   ```

#### Testing Tasks
- [ ] Session binding on creation
- [ ] Same IP allowed
- [ ] Different IP rejected
- [ ] Proxy headers handled
- [ ] Log entries created

#### Success Criteria
- [x] Sessions bound to IP on creation
- [x] Mismatched IPs rejected
- [x] Security violations logged
- [x] Backward compatible (configurable)
- [x] Test coverage ≥ 80%

#### Deliverables
- [ ] Session binding implementation
- [ ] 20+ test cases
- [ ] Configuration documentation
- [ ] Security event logging

---

## PHASE 2: HIGH-PRIORITY SECURITY GAPS (Weeks 3-5)

**Goal:** Improve security posture for enterprise deployment and MCP compliance

**Timeline:** Feb 17 - Mar 9, 2026
**Team Size:** 4-5 engineers
**Effort:** ~90 days

### 2.1 HIGH: OIDC Discovery Support

**Gap ID:** G1
**Severity:** CRITICAL
**Sprint:** Week 3-4, Day 15-28

#### Requirements
- OpenID Connect Discovery 1.0 support
- Automatic authorization server discovery
- JWT validation using discovered keys
- Caching of discovery results

#### Implementation Tasks

1. **Create OIDC Discovery Module** (2 days)
   ```erlang
   -module(erlmcp_auth_oidc_discovery).

   -export([discover_provider/1, get_authorization_endpoint/1]).

   -spec discover_provider(binary()) ->
       {ok, oidc_provider_config()} | {error, term()}.
   discover_provider(IssuerUrl) ->
       %% Fetch /.well-known/openid-configuration
       %% Parse response
       %% Cache for 24 hours
       %% Validate structure
       ok.
   ```

2. **Implement JWK Set Caching** (1.5 days)
   - [ ] Fetch jwks_uri from discovery
   - [ ] Cache JWK set
   - [ ] Implement key rotation
   - [ ] Handle key rotation during use

3. **Update JWT Validation** (1 day)
   - [ ] Use discovered keys for validation
   - [ ] Validate issuer claim
   - [ ] Validate audience claim
   - [ ] Check token expiration

4. **Add Configuration** (0.5 days)
   ```erlang
   {erlmcp, [
       {oauth2, #{
           oidc_discovery_enabled => true,
           oidc_issuers => [
               #{
                   name => <<"auth0">>,
                   issuer_url => <<"https://example.auth0.com/">>
               }
           ],
           jwk_cache_ttl_seconds => 86400  % 24 hours
       }}
   ]}
   ```

#### Testing Tasks
- [ ] Discovery endpoint called correctly
- [ ] Discovery results cached
- [ ] Key rotation handled
- [ ] JWT validation with discovered keys
- [ ] Error handling for discovery failures

#### Success Criteria
- [x] OIDC discovery working
- [x] JWK sets cached and rotated
- [x] JWT validation using discovered keys
- [x] Production-ready caching
- [x] Test coverage ≥ 85%

---

### 2.2 HIGH: Comprehensive Audit Logging

**Gap ID:** G10
**Severity:** HIGH
**Sprint:** Week 3-4, Day 15-28

#### Requirements
- Log all significant security events
- Include context (actor, action, resource, result)
- Send to remote syslog (immutable storage)
- Implement log rotation with archival

#### Implementation Tasks

1. **Create Audit Log Module** (1.5 days)
   ```erlang
   -module(erlmcp_audit_log).

   -export([log_authentication/3, log_authorization/4, log_tool_execution/4]).

   log_authentication(Method, Principal, Result) ->
       Entry = #{
           timestamp => erlang:system_time(millisecond),
           event_type => <<"authentication">>,
           method => Method,
           principal => Principal,
           result => Result,
           ip_address => get_client_ip(),
           session_id => get_session_id()
       },
       write_audit_log(Entry).

   log_authorization(Principal, Resource, Action, Result) ->
       Entry = #{
           timestamp => erlang:system_time(millisecond),
           event_type => <<"authorization">>,
           principal => Principal,
           resource => Resource,
           action => Action,
           result => Result
       },
       write_audit_log(Entry).

   log_tool_execution(Principal, ToolName, Arguments, Result) ->
       Entry = #{
           timestamp => erlang:system_time(millisecond),
           event_type => <<"tool_execution">>,
           principal => Principal,
           tool => ToolName,
           arguments => sanitize_arguments(Arguments),
           result => Result
       },
       write_audit_log(Entry).
   ```

2. **Integrate Audit Logging** (2 days)
   - [ ] Add logging at authentication points
   - [ ] Add logging at authorization checks
   - [ ] Add logging for tool execution
   - [ ] Add logging for resource access

3. **Implement Remote Syslog** (1.5 days)
   - [ ] Send logs to remote syslog server
   - [ ] Use TLS for transport security
   - [ ] Implement retry logic
   - [ ] Log failures locally

4. **Add Log Rotation** (1 day)
   - [ ] Rotate logs daily/weekly
   - [ ] Archive old logs (gzip)
   - [ ] Delete logs after retention period
   - [ ] Monitor rotation for failures

#### Testing Tasks
- [ ] All events logged correctly
- [ ] Remote syslog working
- [ ] Log rotation functioning
- [ ] No sensitive data in logs

#### Success Criteria
- [x] All security events logged
- [x] Remote syslog working
- [x] Log rotation in place
- [x] No sensitive data leakage
- [x] Test coverage ≥ 80%

---

### 2.3 HIGH: Per-Method Rate Limiting

**Gap ID:** G9
**Severity:** HIGH
**Sprint:** Week 5, Day 29-35

#### Requirements
- Different rate limits per MCP method
- Stricter limits for expensive operations
- Configuration-driven limits
- Default limits for all methods

#### Implementation Tasks

1. **Extend Rate Limiter** (1.5 days)
   ```erlang
   {erlmcp, [
       {rate_limiting, #{
           per_method => #{
               <<"initialize">> => #{max_per_second => 5},
               <<"resources/list">> => #{max_per_second => 50},
               <<"resources/read">> => #{max_per_second => 100},
               <<"tools/list">> => #{max_per_second => 50},
               <<"tools/call">> => #{max_per_second => 20},
               <<"tools/call/expensive">> => #{max_per_second => 5},
               <<"prompts/list">> => #{max_per_second => 50},
               <<"default">> => #{max_per_second => 100}
           }
       }}
   ]}
   ```

2. **Implement Method-Specific Limits** (1.5 days)
   - [ ] Check method in rate limiting
   - [ ] Apply method-specific limit
   - [ ] Fall back to default limit
   - [ ] Return 429 Too Many Requests

3. **Add Metrics** (1 day)
   - [ ] Track per-method rate limit hits
   - [ ] Export metrics for monitoring
   - [ ] Alert on method-specific anomalies

#### Testing Tasks
- [ ] Different limits per method
- [ ] Default limit applied
- [ ] 429 returned when exceeded
- [ ] Limits reset correctly

#### Success Criteria
- [x] Per-method limits working
- [x] Configuration driven
- [x] Metrics exported
- [x] Test coverage ≥ 80%

---

## PHASE 3: MEDIUM-PRIORITY SECURITY GAPS (Weeks 6-8)

**Goal:** Achieve defense-in-depth and hardening

**Timeline:** Mar 10 - Mar 30, 2026
**Team Size:** 3-4 engineers
**Effort:** ~70 days

### 3.1 MEDIUM: Add Security Headers

**Gap ID:** G7
**Severity:** MEDIUM
**Sprint:** Week 6, Day 36-37

#### Requirements
- X-Content-Type-Options: nosniff
- X-Frame-Options: DENY
- Strict-Transport-Security: (already done in Phase 1)
- X-XSS-Protection: 1; mode=block

#### Implementation
```erlang
%% Add to HTTP response headers
add_security_headers(Headers) ->
    Headers#{
        <<"x-content-type-options">> => <<"nosniff">>,
        <<"x-frame-options">> => <<"DENY">>,
        <<"x-xss-protection">> => <<"1; mode=block">>
    }.
```

#### Testing
- [ ] Headers present in all responses
- [ ] Correct values
- [ ] No override possible

---

### 3.2 MEDIUM: Token Binding Validation

**Gap ID:** G12, S-012
**Severity:** MEDIUM
**Sprint:** Week 6-7, Day 38-42

#### Requirements
- Bind OAuth tokens to TLS connection
- Prevent token substitution
- Validate binding on token use

#### Implementation
```erlang
%% Add token binding
bind_oauth_token(Token, TlsBinding) ->
    Token#{
        binding => #{
            tls_unique => TlsBinding
        }
    }.

%% Validate binding
validate_token_binding(Token, CurrentTlsBinding) ->
    case maps:get(binding, Token) of
        undefined -> ok;  % Backward compat
        #{tls_unique := TokenBinding} ->
            case TokenBinding =:= CurrentTlsBinding of
                true -> ok;
                false -> {error, binding_mismatch}
            end
    end.
```

---

### 3.3 MEDIUM: API Key Rotation

**Gap ID:** G8
**Severity:** MEDIUM
**Sprint:** Week 7, Day 43-46

#### Requirements
- API key expiration dates
- Rotation before expiration
- Deprecation period for old keys
- Audit rotation events

#### Implementation
```erlang
-record(api_key, {
    key_id :: binary(),
    hash :: binary(),  % bcrypt hash
    created_at :: integer(),
    expires_at :: integer(),
    status :: active | deprecated | revoked
}).

check_key_expiration(ApiKey) ->
    case erlang:system_time(second) > ApiKey#api_key.expires_at of
        true -> {error, key_expired};
        false -> ok
    end.
```

---

### 3.4 MEDIUM: Log Signing with HMAC

**Gap ID:** G11
**Severity:** MEDIUM
**Sprint:** Week 7-8, Day 47-50

#### Requirements
- Sign audit log entries with HMAC-SHA256
- Verify integrity on log review
- Detect tampering

#### Implementation
```erlang
sign_log_entry(Entry, Secret) ->
    Json = jsx:encode(maps:without([signature], Entry)),
    Signature = crypto:mac(hmac, sha256, Secret, Json),
    Entry#{signature => base64:encode(Signature)}.

verify_log_entry(Entry, Secret) ->
    StoredSig = maps:get(signature, Entry),
    Json = jsx:encode(maps:without([signature], Entry)),
    ComputedSig = crypto:mac(hmac, sha256, Secret, Json),
    constant_time_compare(StoredSig, ComputedSig).
```

---

### 3.5 MEDIUM: Root Directory Validation

**Gap ID:** G6
**Severity:** MEDIUM
**Sprint:** Week 8, Day 51-52

#### Requirements
- Validate root directory permissions
- Prevent root directory traversal
- Audit root access

#### Implementation
```erlang
validate_root_access(RootUri, Principal) ->
    case erlmcp_auth:check_permission(Principal, RootUri, read) of
        {ok, _} -> ok;
        {error, Reason} -> {error, {forbidden, Reason}}
    end.
```

---

## IMPLEMENTATION SCHEDULE

### Week-by-Week Breakdown

```
WEEK 1 (Feb 3-9)
├─ Mon-Tue: Enforce HTTPS (1.1)
├─ Wed-Thu: Begin HTTP Session Manager (1.2)
└─ Fri: Code review + testing

WEEK 2 (Feb 10-16)
├─ Mon-Tue: Complete HTTP Session Manager (1.2)
├─ Wed-Thu: SEP-1303 Input Validation (1.3)
├─ Fri: IP Binding to Sessions (1.4)
└─ Fri: Sprint review + retrospective

WEEK 3 (Feb 17-23)
├─ Mon-Wed: OIDC Discovery (2.1)
├─ Wed-Fri: Audit Logging (2.2)
└─ Fri: Integration testing

WEEK 4 (Feb 24-Mar 2)
├─ Mon-Tue: Complete OIDC (2.1)
├─ Wed-Fri: Complete Audit Logging (2.2)
└─ Fri: Sprint review

WEEK 5 (Mar 3-9)
├─ Mon-Fri: Per-Method Rate Limiting (2.3)
└─ Fri: Sprint review + release prep

WEEK 6 (Mar 10-16)
├─ Mon: Security Headers (3.1)
├─ Tue-Fri: Token Binding (3.2)
└─ Fri: Sprint review

WEEK 7 (Mar 17-23)
├─ Mon-Tue: API Key Rotation (3.3)
├─ Wed-Thu: Log Signing (3.4)
├─ Fri: Root Directory Validation (3.5)
└─ Fri: Sprint review

WEEK 8 (Mar 24-30)
├─ Mon-Thu: Final testing + fixes
├─ Thu: Security validation + sign-off
├─ Fri: Final release prep
└─ Fri: Version 2.2.0 Release
```

---

## RESOURCE ALLOCATION

### Team Composition

**Phase 1** (2 weeks, 60 days)
- Lead Security Engineer: 100% (10 days)
- Senior Backend Engineer: 100% (10 days)
- QA Engineer: 100% (5 days)
- DevOps Engineer: 50% (2.5 days)

**Phase 2** (3 weeks, 90 days)
- Lead Security Engineer: 75% (15 days)
- Senior Backend Engineer: 75% (15 days)
- Backend Engineer: 100% (10 days)
- QA Engineer: 100% (7.5 days)

**Phase 3** (3 weeks, 70 days)
- Senior Backend Engineer: 50% (7.5 days)
- Backend Engineer: 100% (10 days)
- QA Engineer: 75% (5.625 days)

---

## TESTING STRATEGY

### Test Coverage Requirements

| Phase | Unit Tests | Integration Tests | E2E Tests | Coverage Target |
|-------|-----------|------------------|-----------|-----------------|
| **Phase 1** | 40+ | 30+ | 10+ | ≥85% |
| **Phase 2** | 60+ | 40+ | 15+ | ≥87% |
| **Phase 3** | 50+ | 30+ | 10+ | ≥90% |

### Automated Testing

```bash
# Pre-commit
rebar3 fmt check
rebar3 dialyzer

# CI Pipeline
rebar3 compile
rebar3 eunit
rebar3 ct --suite=security_tests
rebar3 cover --min=85

# Security Tests
./scripts/security_tests.sh
./scripts/pen_test.sh
./scripts/threat_modeling_tests.sh
```

---

## SUCCESS CRITERIA & VALIDATION

### Phase 1 Sign-Off

- [x] All critical gaps remediated
- [x] Test coverage ≥ 85%
- [x] Security review passed
- [x] Documentation updated
- [x] No high-severity findings

### Phase 2 Sign-Off

- [x] All high gaps remediated
- [x] Test coverage ≥ 87%
- [x] Enterprise features working
- [x] Performance validated
- [x] No open findings

### Phase 3 Sign-Off

- [x] All medium gaps remediated
- [x] Test coverage ≥ 90%
- [x] Defense-in-depth achieved
- [x] Zero-trust compliance verified
- [x] Ready for production GA

---

## RELEASE SCHEDULE

### Version 2.2.0 (End of Phase 2) - Production Release
- Target: March 9, 2026
- Security critical + high gaps fixed
- MCP 2025-11-25 compliance: ≥85%
- Release notes: Security hardening

### Version 2.3.0 (End of Phase 3) - Hardened Release
- Target: March 30, 2026
- All security gaps closed
- Defense-in-depth complete
- MCP 2025-11-25 compliance: ≥95%
- Release notes: Complete security suite

---

## RISK MITIGATION

### Schedule Risks

**Risk:** Timeline compression
**Mitigation:**
- Buffer included (10% time reserve)
- Parallel workstreams
- Prioritize MVP features

**Risk:** Resource unavailability
**Mitigation:**
- Cross-train team
- Document handoffs
- Use contractors if needed

### Quality Risks

**Risk:** Regression in existing features
**Mitigation:**
- Full regression test suite
- Pre-merge testing
- 1-week stabilization period

---

## COMMUNICATION PLAN

### Weekly Updates
- Tuesday: Progress report to stakeholders
- Friday: Sprint review with team
- EOW: Release notes draft

### Milestone Announcements
- Phase 1 completion: 2/17/2026
- Phase 2 completion: 3/9/2026
- Phase 3 completion: 3/30/2026

---

**END OF REMEDIATION ROADMAP**
