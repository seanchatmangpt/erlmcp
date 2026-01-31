# erlmcp WIP Report - Mock Violations & Spec Compliance

**Generated:** 2026-01-30
**Analysis Scope:** All apps (core, transports, observability, validation)
**Chicago School TDD Compliance:** NO MOCKS, FAKE, OR PLACEHOLDER IMPLEMENTATIONS

---

## CRITICAL (Mock Violations - MUST FIX)

### 1. HTTP Client Mocking in AWS Secrets Tests
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_secrets_aws_tests.erl`
**Lines:** 30-31, 39, 65, 90, 121, 152, 181, 317, 343, 372, 401, 444, 484, 517
**Issue:** Uses `meck:new(httpc)` and `meck:expect(httpc, request, ...)` to mock HTTP client
**Violation:** Chicago School TDD requires REAL HTTP connections, not mocked httpc

**Impact:**
- 14 mock violations across test suite
- Tests don't validate actual AWS Secrets Manager integration
- Mocked responses don't test real network behavior, retries, timeouts

**Fix Required:**
```erlang
% VIOLATION (current):
meck:new(httpc, [unstick]),
meck:expect(httpc, request, fun(_Method, _Request, _Options, []) ->
    {ok, {{<<"HTTP/1.1">>, 200, <<"OK">>}, [], Response}}
end),

% REQUIRED (Chicago School):
% Use REAL httpc with local AWS LocalStack or test container
% OR use real AWS test environment with proper credentials
```

**Effort:** 4-6 hours (setup LocalStack, rewrite 14 test cases, integration testing)
**Dependencies:** LocalStack Docker container or AWS test account

---

### 2. Mock LLM Provider for Testing
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_mock_llm.erl`
**Lines:** 1-145 (entire module)
**Issue:** Module name contains "mock" - provides fake LLM responses
**Violation:** PLACEHOLDER IMPLEMENTATION - tests should use REAL LLM providers

**Impact:**
- `create_message/2` returns fake echo responses
- `set_response_mode/1` simulates error/timeout modes artificially
- Sampling tests don't validate actual OpenAI/Anthropic/etc. integration
- Token estimation is fake (1 token ≈ 4 characters)

**Fix Required:**
```erlang
% VIOLATION (current):
create_message(Messages, _Params) ->
    {ok, #{
        <<"role">> => <<"assistant">>,
        <<"content">> => <<"Echo: ", LastUserMessage/binary>>,
        <<"model">> => <<"mock-model-echo">>
    }}.

% REQUIRED (Chicago School):
% Use REAL LLM provider (OpenAI, Anthropic, Ollama, etc.)
% Provide test credentials via environment variables
% Tests should make actual API calls and validate real responses
```

**Effort:** 6-8 hours (integrate real LLM provider, handle API keys, rewrite tests)
**Dependencies:** LLM provider API keys, test account setup

---

### 3. Mock HTTP MCP Handler for Transport Testing
**File:** `/Users/sac/erlmcp/apps/erlmcp_transports/test/mock_http_mcp_handler.erl`
**Lines:** 1-133 (entire module)
**Issue:** Mock Cowboy handler simulates HTTP responses
**Violation:** FAKE IMPLEMENTATION - tests should use REAL HTTP servers

**Impact:**
- HTTP transport tests use fake handler instead of real MCP server
- `/mcp`, `/mcp/error`, `/mcp/retry` endpoints are simulated
- No validation of real JSON-RPC protocol over HTTP
- Doesn't test real network failures, timeouts, chunked responses

**Fix Required:**
```erlang
% VIOLATION (current):
handle_mcp_request(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Response = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{<<"echo">> => Body}
    }),
    cowboy_req:reply(200, #{}, Response, Req1).

% REQUIRED (Chicago School):
% Use REAL erlmcp_server instance with HTTP transport
% Tests should start actual server process and send real JSON-RPC
% Validate full MCP protocol implementation, not echo responses
```

**Effort:** 8-10 hours (refactor tests to use real server, full protocol validation)
**Dependencies:** None (use existing erlmcp_server + HTTP transport)

---

## HIGH (Missing Spec Features)

### 4. Roots URI Scheme Not Implemented
**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_validation/src/erlmcp_test_client.erl.broken:189,843-844`
- Protocol docs reference `roots://` scheme

**Issue:** MCP 2024-11-05 spec requires roots support for file system access
**Status:** Only type spec in broken test client file, no implementation

**Missing Features:**
- `roots/list` method (client → server)
- Root URI validation (`roots://` scheme)
- Root metadata (name, uri, optional dedicated file I/O)
- Root change notifications

**Impact:**
- Spec compliance failure (roots is REQUIRED capability)
- Cannot use file system resources through MCP
- Test client has stub types but no implementation

**Effort:** 12-15 hours (implement roots module, client/server methods, tests)
**Dependencies:** None (pure Erlang implementation)

---

### 5. Resource URI Scheme Incomplete
**Files:** 8 test files use `resource://`, but limited implementation

**Issue:** Resource URIs (`resource://scheme/id`) need full validation
**Status:** Basic string matching, no proper URI parsing/verification

**Missing Features:**
- URI scheme validation per RFC 3986
- Resource ID encoding/decoding
- URI template support
- Cross-resource references

**Impact:**
- Resource operations may fail on malformed URIs
- No guarantee of URI uniqueness
- Cannot enforce URI format standards

**Effort:** 6-8 hours (implement URI validator, refactor resource operations)
**Dependencies:** None (can use uri_string module from OTP)

---

### 6. LLM Sampling Integration Incomplete
**Files:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sampling.erl`, capability extraction

**Issue:** Sampling capability defined but not integrated with real LLM providers
**Status:** Capability negotiation works, but actual sampling uses mock_llm

**Missing Features:**
- Real OpenAI API integration for sampling
- Anthropic Claude integration
- Ollama/local model support
- Model preference negotiation
- Token counting and usage tracking
- Sampling parameters (temperature, max_tokens, etc.)

**Impact:**
- Server advertises sampling capability but cannot actually sample
- Clients requesting sampling will get fake responses
- No real LLM integration for server-side operations

**Effort:** 16-20 hours (implement 3-4 LLM providers, comprehensive tests)
**Dependencies:** LLM provider API keys, HTTP client configuration

---

### 7. TODO: SSE DELETE Handler
**File:** `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_sse.erl:153`
**Issue:** DELETE handler commented as TODO

**Impact:**
- SSE transport cannot handle DELETE requests
- Subscription cancellation via HTTP DELETE not implemented
- May violate MCP spec if DELETE is required method

**Effort:** 2 hours (implement DELETE handler, add tests)
**Dependencies:** None

---

## MEDIUM (Incomplete Implementations)

### 8. TODO: Audit Log Export Tests (5 stub functions)
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_audit_log_tests.erl:237-254`
**Lines:** 237, 241, 245, 249, 253

**Missing Tests:**
- `test_export_json/0` - Only TODO comment
- `test_export_csv/0` - Only TODO comment
- `test_user_logs_query/0` - Only TODO comment
- `test_search_logs/0` - Only TODO comment
- `test_tamper_detection/0` - Only TODO comment

**Note:** Functions 115-233 ARE implemented (with fixture args), but duplicate stubs exist at 237-254

**Impact:**
- Code coverage gaps
- Duplicate function definitions (compilation warning?)
- Export functionality untested

**Effort:** 2 hours (remove duplicate stubs, verify existing tests work)
**Dependencies:** None (tests already written, just cleanup)

---

### 9. TODO: Dashboard Metrics Filtering
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:279`
**Issue:** Metrics endpoint doesn't filter based on subscription

**Impact:**
- All clients receive all metrics (performance overhead)
- No way to subscribe to specific metric types
- May violate least-privilege principle

**Effort:** 4 hours (implement metric filtering, subscription management)
**Dependencies:** None

---

### 10. TODO: Code Reload State Versioning
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_code_reload.erl:279, 330`

**Missing Features:**
- Line 279: State record version check
- Line 330: Query actual in-flight request count from registry

**Impact:**
- Hot code upgrade may corrupt state if record format changes
- In-flight request counting is estimated, not accurate
- May drop requests during upgrade if count is wrong

**Effort:** 6 hours (implement state versioning, accurate request tracking)
**Dependencies:** Registry introspection API

---

### 11. TODO: Audit Log Chain Verification Ranges
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_audit_log.erl:389`
**Issue:** Hash chain verification doesn't support range-based verification

**Impact:**
- Cannot verify partial log segments
- Full log verification required (expensive for large logs)
- Cannot verify imported log segments

**Effort:** 4 hours (implement range verification, optimize chain traversal)
**Dependencies:** None

---

### 12. TODO: Debugger DOT Format Export
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_debugger.erl:320`
**Issue:** Call graph cannot be exported to DOT format for visualization

**Impact:**
- Cannot visualize call graphs with Graphviz
- Debugger output is text-only
- Hard to analyze complex call hierarchies

**Effort:** 3 hours (implement DOT export, add tests)
**Dependencies:** None (use existing graph data structures)

---

### 13. TODO: Session Persistent Storage
**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_tests.erl:126`
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_proper_tests.erl:228`

**Issue:** `list_sessions/0` returns empty list with comment "TODO: persistent storage"

**Impact:**
- Session listings don't work across restarts
- Session data lost on node crash
- Cannot query active sessions cluster-wide

**Effort:** 8-10 hours (implement ETS/DETS/Mnesia backend, migration)
**Dependencies:** None (use existing backend interface)

---

### 14. TODO: Subscription Rate Limiting
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_subscription.erl:344`
**Issue:** Comment "TODO: Implement proper rate limiting with token bucket or sliding window"

**Impact:**
- Subscription notifications are not rate-limited
- Client spam could overwhelm server
- No protection against notification floods

**Effort:** 6 hours (implement token bucket or sliding window rate limiter)
**Dependencies:** None (can use existing rate_limiter module)

---

### 15. TODO: Session Failover & Replication
**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_session_failover.erl:5`
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_session_replicator.erl:5`

**Issue:** Modules have only header comments, no implementation

**Impact:**
- No session failover on node failure
- Sessions not replicated across cluster
- High availability not possible

**Effort:** 20-24 hours (design replication protocol, implement Mnesia backend, testing)
**Dependencies:** Mnesia schema, cluster setup

---

### 16. TODO: Icon Cache Implementation
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_icon_cache.erl:5`
**Issue:** Module has only header comment, no implementation

**Impact:**
- Icon URLs not cached
- Repeated icon downloads
- Wasted bandwidth

**Effort:** 4-6 hours (implement ETS cache with TTL, HTTP download)
**Dependencies:** HTTP client, icon URL sources

---

### 17. TODO: Auth OAuth2 & mTLS
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_auth.erl:579, 592`

**Missing Features:**
- Line 579: "TODO: Implement OAuth2 token introspection"
- Line 592: "TODO: Implement mTLS certificate validation"

**Impact:**
- OAuth2 tokens not validated against issuer
- mTLS certificates not checked
- Security gap for enterprise authentication

**Effort:** 10-12 hours (implement OAuth2 introspection, mTLS validation)
**Dependencies:** OAuth2 introspection endpoint, CA certificates

---

### 18. TODO: Pricing Upgrade Logic
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/pricing/erlmcp_pricing_cli.erl:47`
**Issue:** "TODO: Implement pricing upgrade logic"

**Impact:**
- Cannot upgrade pricing tiers
- Manual intervention required
- No automated billing

**Effort:** 6-8 hours (implement upgrade logic, integration with billing)
**Dependencies:** Billing system API

---

## LOW (Nice to Have)

### 19. TODO: Security Headers Wrapped Handler
**File:** `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_security_headers.erl:69`
**Issue:** "TODO: Return wrapped handler module"

**Impact:**
- Security headers not easily composable
- Manual header management required
- Minor developer experience issue

**Effort:** 2 hours (implement handler wrapper)
**Dependencies:** None

---

### 20. TODO: Health Check Module
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_core_sup.erl:46`
**Issue:** Comment "TODO: Implement health check module or remove from architecture"

**Impact:**
- Health check referenced but not implemented
- Architecture unclear
- Minor consistency issue

**Effort:** 1 hour (implement or remove reference)
**Dependencies:** None

---

### 21. 27 Backup Files Cleanup
**Files:** 27 `.bak`, `.backup`, `.orig` files across codebase

**Impact:**
- Repository clutter
- Confusion about which file is current
- Git bloat

**Effort:** 30 minutes (delete all backup files)
**Dependencies:** None

---

### 22. 19 Broken/Skipped Test Files
**Files:** `.broken`, `.skip`, `.tmp` files (19 in core + transports)

**Examples:**
- `erlmcp_progress_tests.erl.broken`
- `erlmcp_cache_tests.erl.broken`
- `erlmcp_tool_execution_SUITE.erl.skip`
- `erlmcp_integration_SUITE.erl.broken`

**Impact:**
- Test coverage gaps
- Unknown broken functionality
- CI/CD incomplete

**Effort:** 40-60 hours (fix all broken/skipped tests, varies by complexity)
**Dependencies:** Varies per test

---

## Summary Statistics

| Category | Count | Total Effort (hours) |
|----------|-------|---------------------|
| CRITICAL (mock violations) | 3 | 18-24 |
| HIGH (missing spec features) | 4 | 36-43 |
| MEDIUM (incomplete implementations) | 16 | 94-130 |
| LOW (nice to have) | 4 | 41.5-61.5 |
| **TOTAL** | **27** | **189.5-258.5** |

**Test Coverage:**
- Source modules: 288
- Test modules: 375
- Test-to-source ratio: 1.30 (good)
- BUT: 19 tests broken/skipped, 3 with mock violations

**Compilation Status:**
- ✅ All apps compile successfully (4/4)
- ✅ 0 compilation errors

**Documentation:**
- 629 `%% @doc` comments (good coverage)
- Need to verify all public APIs documented

---

## Priority Order for Fixes

### Phase 1: Critical Mock Violations (Week 1)
1. Remove httpc mocking in AWS secrets tests (setup LocalStack)
2. Replace mock_llm with real LLM provider integration
3. Replace mock_http_mcp_handler with real server tests

### Phase 2: Spec Compliance (Week 2-3)
4. Implement roots URI scheme and methods
5. Complete resource URI validation
6. Integrate real LLM sampling
7. Implement SSE DELETE handler

### Phase 3: Production Readiness (Week 4-6)
8. Fix all broken/skipped tests (19 files)
9. Complete session persistent storage
10. Implement subscription rate limiting
11. Add OAuth2/mTLS auth
12. Complete audit log test coverage

### Phase 4: Polish & Optimization (Week 7-8)
13. Implement session failover/replication
14. Complete icon cache
15. Implement code reload state versioning
16. Add dashboard metric filtering
17. Cleanup backup files

---

## Testing Strategy

### For Mock Violations:
- **AWS Secrets:** Use LocalStack Docker container for real-ish AWS API
- **LLM Sampling:** Use Ollama (free local) or test API keys for OpenAI/Anthropic
- **HTTP Transport:** Start real erlmcp_server with HTTP transport, test JSON-RPC protocol

### For Spec Features:
- **Roots:** Follow MCP 2024-11-05 spec exactly, add compliance tests
- **Resources:** Implement URI validator per RFC 3986, add property tests
- **Sampling:** Test with 2-3 real LLM providers, validate responses

### For Incomplete Features:
- **Sessions:** Implement Mnesia backend for cluster-wide persistence
- **Subscriptions:** Add rate limiter using existing module or token bucket
- **Auth:** Integrate with OAuth2 introspection endpoint, add mTLS

---

## Compliance Checklist

**Chicago School TDD Compliance:**
- ❌ NO MECK MOCKS (14 violations in aws_tests)
- ❌ NO FAKE IMPLEMENTATIONS (mock_llm, mock_http_mcp_handler)
- ❌ NO PLACEHOLDERS (erlmcp_icon_cache, failover, replicator)
- ✅ Test observable behavior (most tests do this correctly)
- ✅ Use real processes (most tests use real gen_servers)
- ⚠️ Test all interfaces (incomplete: roots, sampling, DELETE)

**MCP Spec Compliance (2024-11-05):**
- ❌ Roots URI scheme (not implemented)
- ⚠️ Resource URIs (basic support, incomplete validation)
- ❌ LLM Sampling (advertised but uses fake responses)
- ✅ JSON-RPC 2.0 (implemented and tested)
- ✅ Tools API (implemented)
- ✅ Resources API (implemented)
- ✅ Prompts API (implemented)
- ⚠️ Notifications (some missing, e.g., SSE DELETE)

**Production Readiness:**
- ✅ Compilation (0 errors)
- ⚠️ Tests (19 broken/skipped files)
- ❌ Mock violations (3 critical issues)
- ⚠️ Documentation (good coverage, some gaps)
- ⚠️ Observability (comprehensive but incomplete)
- ❌ High availability (no failover/replication)
- ⚠️ Security (OAuth2/mTLS missing)

---

**Report End**
