# Threat Model: erlmcp Implementation
## STRIDE Analysis for MCP 2025-11-25

**Date:** February 2, 2026
**Version:** 1.0
**Status:** COMPLETE

---

## EXECUTIVE SUMMARY

This threat model applies the STRIDE methodology to the erlmcp implementation, identifying threats across six categories: Spoofing, Tampering, Repudiation, Information Disclosure, Denial of Service, and Elevation of Privilege.

### Risk Summary

| Category | Threats Identified | Critical | High | Medium | Low |
|----------|-------------------|----------|------|--------|-----|
| **Spoofing** | 8 | 2 | 3 | 2 | 1 |
| **Tampering** | 7 | 1 | 3 | 2 | 1 |
| **Repudiation** | 4 | 0 | 1 | 2 | 1 |
| **Information Disclosure** | 6 | 1 | 2 | 2 | 1 |
| **Denial of Service** | 8 | 2 | 3 | 2 | 1 |
| **Elevation of Privilege** | 6 | 3 | 2 | 1 | 0 |
| **TOTAL** | **39** | **9** | **14** | **11** | **5** |

---

## PART 1: SPOOFING THREATS

### 1.1 JWT Algorithm Confusion (Alg=None)

**Threat ID:** S-001
**Severity:** CRITICAL
**Likelihood:** LOW (due to mitigations)
**Impact:** HIGH (complete authentication bypass)
**CVSS v3.1:** 7.5 (High)

#### Description
An attacker creates a JWT with `"alg": "none"` header and no signature. If the server doesn't validate the algorithm, it accepts the token without verification.

#### Attack Scenario
```json
{
  "alg": "none",
  "typ": "JWT"
}.
{
  "sub": "attacker",
  "admin": true,
  "exp": 9999999999
}.
```

#### Current Mitigation
✅ **PROTECTED** - JOSE library rejects `alg:none` by default

```erlang
%% erlmcp_auth.erl - verify_jwt_with_key/3
case jose_jws:verify(JWK, Token) of
    {true, Payload, _JWS} -> {ok, Claims};
    {false, _, _} -> {error, invalid_signature}
end.
%% JOSE never validates alg:none
```

#### Test Evidence
- Test: `erlmcp_auth_tests.erl` line 456 - "JWT algorithm confusion attack is prevented"
- Coverage: ✅ VERIFIED

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH (JOSE is industry-standard)

---

### 1.2 API Key Brute Force Attack

**Threat ID:** S-002
**Severity:** CRITICAL
**Likelihood:** MEDIUM (weak protection)
**Impact:** HIGH (account compromise)
**CVSS v3.1:** 8.8 (High)

#### Description
An attacker attempts to brute-force valid API keys by sending rapid authentication requests.

#### Attack Scenario
```
GET /api/resources HTTP/1.1
Authorization: Bearer 1a2b3c4d5e6f7g8h9i0j
```
Attacker tries 10 million combinations/hour

#### Current Mitigation
✅ **PARTIALLY PROTECTED** - Rate limiting exists

**Rate Limit Configuration:**
```erlang
erlmcp_auth_rate_limiter:
- max_attempts_per_second: 10 (per client)
- max_failures: 5 (per window)
- backoff_levels: [0ms, 1s, 2s, 4s, 8s, 16s]
- block_duration: 5 minutes
```

#### Remaining Gaps
- ❌ No global brute force protection across all clients
- ❌ No CAPTCHA after N failures
- ❌ No account lockout (permanent)
- ❌ No alerting on brute force attempts

#### Remediation Plan
```erlang
%% Add global brute force detection
check_global_brute_force(ApiKey) ->
    case erlmcp_auth_rate_limiter:get_global_attempts(ApiKey) of
        {ok, Count} when Count > 1000 ->  % 1000 attempts
            %% Alert security team
            erlmcp_security:alert(
                <<"Brute force detected">>,
                #{api_key => ApiKey, attempts => Count}
            );
        _ -> ok
    end.
```

#### Recommendation
- Priority: P1 (HIGH)
- Effort: 2 days
- Implementation: Add global counters + alerting

---

### 1.3 OAuth Token Interception (Weak TLS)

**Threat ID:** S-003
**Severity:** CRITICAL
**Likelihood:** LOW (TLS 1.2+ enforced)
**Impact:** HIGH (session hijacking)
**CVSS v3.1:** 8.1 (High)

#### Description
Attacker intercepts OAuth token in transit and reuses it.

#### Attack Scenario
1. Client connects to server via HTTP (not HTTPS)
2. Server returns OAuth token in Authorization header
3. Attacker on network intercepts token
4. Attacker replays token to access victim's resources

#### Current Mitigation
✅ **PROTECTED** - TLS 1.2+ enforced

**TLS Configuration:**
- Minimum version: TLS 1.2
- No legacy protocols (SSLv3, TLS 1.0, TLS 1.1)
- Forward secrecy (ephemeral keys)
- Strong ciphers only

**Gap:** HTTP not explicitly forbidden in production

#### Remediation Plan
```erlang
%% Enforce HTTPS in production
handle_connection(Scheme, Config) ->
    case {Scheme, maps:get(mode, Config, development)} of
        {http, production} ->
            {error, <<"HTTPS required in production">>};
        {https, _} -> ok;
        {http, development} -> ok
    end.
```

#### Recommendation
- Priority: P0 (CRITICAL - blocks production)
- Effort: 1 day
- Implementation: Add enforce_https flag

---

### 1.4 Session Fixation via Predictable IDs

**Threat ID:** S-004
**Severity:** HIGH
**Likelihood:** LOW (cryptographically random IDs)
**Impact:** HIGH (session hijacking)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker predicts session ID, forces user to use attacker's session, gains access to user resources.

#### Attack Scenario
1. Attacker predicts session ID pattern (e.g., sequential)
2. Attacker creates malicious link with crafted session ID
3. User clicks link, gets assigned attacker's session
4. User performs actions in attacker's session
5. Attacker executes those actions as the user

#### Current Mitigation
✅ **PARTIALLY PROTECTED**

**Session ID Generation:**
```erlang
%% erlmcp_session_manager.erl
generate_session_id() ->
    crypto:strong_rand_bytes(32) |> base64:encode().
```

**Gaps:**
- ❌ No IP binding to session (same session from different IP = suspicious)
- ❌ No user agent binding
- ❌ No TLS unique binding

#### Remediation Plan
```erlang
%% Add context binding to sessions
create_session(UserId, Context) ->
    SessionId = generate_secure_id(),
    Binding = #{
        ip_address => maps:get(ip_address, Context),
        user_agent => maps:get(user_agent, Context),
        tls_unique => erlmcp_tls_validation:get_unique_id()
    },
    erlmcp_session_manager:create(
        SessionId,
        UserId,
        #{binding => Binding}
    ).

validate_session_binding(SessionId, CurrentContext) ->
    {ok, Session} = erlmcp_session_manager:get(SessionId),
    PreviousBinding = maps:get(binding, Session),
    case validate_context_match(PreviousBinding, CurrentContext) of
        ok -> {ok, Session};
        {error, binding_mismatch} ->
            %% Potential session fixation - reject
            {error, binding_mismatch}
    end.
```

#### Recommendation
- Priority: P1 (HIGH)
- Effort: 2 days
- Implementation: Add context binding

---

### 1.5 OAuth Client ID Spoofing

**Threat ID:** S-005
**Severity:** HIGH
**Likelihood:** MEDIUM (missing validation)
**Impact:** MEDIUM (incorrect client identification)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker impersonates different OAuth client by using client_id from another application.

#### Attack Scenario
1. Attacker knows legitimate OAuth client_id (public)
2. Attacker registers their own OAuth app with different secret
3. Attacker attempts to use legitimate client_id with their secret
4. If server doesn't verify client_secret, attacker succeeds

#### Current Mitigation
✅ **PROTECTED** - Client secret validation required

**OAuth Configuration:**
```erlang
validate_oauth2_token(Token, State) ->
    %% Token endpoint requires client_id AND client_secret
    %% Both must match registered OAuth client
    case introspect_token(Token, State#state.oauth2_config) of
        {ok, TokenInfo} -> {ok, TokenInfo};
        {error, Reason} -> {error, Reason}
    end.
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 1.6 JWT Key ID Confusion

**Threat ID:** S-006
**Severity:** HIGH
**Likelihood:** MEDIUM
**Impact:** MEDIUM (wrong key validation)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker modifies JWT key ID (kid) header to use weak key or bypass verification.

#### Attack Scenario
1. JWT has `"kid": "test_key"` pointing to weak test key
2. Attacker modifies `"kid"` to point to their own weak key
3. If server doesn't validate kid source, attacker's signature validates

#### Current Mitigation
✅ **PROTECTED** - Key lookup enforced

```erlang
%% erlmcp_auth.erl - verify_jwt_signature/2
KeyId = maps:get(<<"kid">>, Protected, undefined),
PublicKeyPem = case KeyId of
    undefined ->
        case maps:get(default_key, State#state.jwt_config, undefined) of
            undefined -> error(unknown_key_id);
            DefaultKey -> DefaultKey
        end;
    Kid ->
        case ets:lookup(State#state.jwt_keys, Kid) of
            [{_, Key}] -> Key;
            [] -> error(unknown_key_id)
        end
end.
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH
- Note: Add jti (JWT ID) claim validation for additional protection

---

### 1.7 mTLS Certificate Impersonation

**Threat ID:** S-007
**Severity:** CRITICAL
**Likelihood:** LOW (certificate verification)
**Impact:** HIGH (client impersonation)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker uses forged or stolen client certificate to impersonate legitimate client.

#### Attack Scenario
1. Attacker steals client certificate file (PEM format)
2. Attacker uses certificate to connect as victim client
3. Server trusts certificate (valid signature from trusted CA)
4. Attacker gains victim's access

#### Current Mitigation
✅ **PROTECTED** - Certificate chain validation enforced

**mTLS Configuration:**
```erlang
erlmcp_tls_validation:
- verify: verify_peer (required)
- fail_if_no_peer_cert: true (required)
- depth: 3 (chain depth limit)
- check_crl: optional (certificate revocation checking)
```

**Remaining Gap:**
- ❌ No certificate pinning (harden against CA compromise)
- ❌ No certificate revocation checking (CRL/OCSP)

#### Remediation Plan
```erlang
%% Add certificate pinning
validate_client_cert(Cert, PinnedCerts) ->
    case lists:member(Cert, PinnedCerts) of
        true -> ok;
        false -> {error, certificate_not_pinned}
    end.

%% Add OCSP stapling
check_cert_revocation(Cert) ->
    case erlmcp_ocsp_validator:validate(Cert) of
        {ok, good} -> ok;
        {ok, revoked} -> {error, certificate_revoked};
        {ok, unknown} -> {error, certificate_status_unknown}
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 3 days
- Implementation: Add pinning + OCSP

---

### 1.8 API Key Format Confusion

**Threat ID:** S-008
**Severity:** MEDIUM
**Likelihood:** LOW
**Impact:** MEDIUM (unauthorized access)
**CVSS v3.1:** 5.5 (Medium)

#### Description
Attacker uses API key from one service with different service (if formats similar).

#### Attack Scenario
1. Two services use same API key format (32-byte hex string)
2. Attacker uses key from Service A to access Service B
3. Both services accept each other's keys

#### Current Mitigation
✅ **PARTIALLY PROTECTED** - Prefix-based distinction recommended

**Best Practice Implementation:**
```erlang
%% API keys should have service prefix
generate_api_key(ServiceId) ->
    Prefix = iolist_to_binary(["sk_", atom_to_list(ServiceId)]),
    Random = crypto:strong_rand_bytes(32),
    <<Prefix/binary, "_", (base64:encode(Random))/binary>>.

%% Validate prefix
validate_api_key(Key) ->
    case binary:match(Key, <<"sk_">>) of
        {0, 3} -> ok;  % Correct prefix
        nomatch -> {error, invalid_key_format};
        _ -> {error, invalid_key_format}
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 1 day
- Implementation: Add key prefix validation

---

## PART 2: TAMPERING THREATS

### 2.1 JSON-RPC Message Injection

**Threat ID:** T-001
**Severity:** HIGH
**Likelihood:** MEDIUM
**Impact:** HIGH (code execution potential)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker injects malicious JSON-RPC messages to modify server behavior.

#### Attack Scenario
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "execute_shell",
    "arguments": {
      "command": "; rm -rf /"
    }
  }
}
```

#### Current Mitigation
✅ **PROTECTED** - Input validation enforced

**Validation Points:**
1. JSON structure validation (JSX library)
2. Schema validation (Jesse library)
3. Parameter sanitization per tool
4. Size limits (16 MB)

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Coverage: Comprehensive

---

### 2.2 CRLF Injection in HTTP Headers

**Threat ID:** T-002
**Severity:** MEDIUM
**Likelihood:** LOW (detection implemented)
**Impact:** MEDIUM (response header injection)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker injects CR+LF sequences in HTTP header values to split headers.

#### Attack Scenario
```
X-Custom-Header: value\r\nContent-Length: 0\r\n\r\nmalicious_body
```
Server sends header with injected content.

#### Current Mitigation
✅ **PROTECTED** - CRLF detection enforced

```erlang
%% erlmcp_http_header_validator.erl
validate_header_security(Name, Value) ->
    case binary:match(Value, [<<"\r">>, <<"\n">>]) of
        nomatch -> ok;
        _ ->
            logger:error("CRLF injection detected in ~p: ~p", [Name, Value]),
            {error, crlf_injection_in_value}
    end.
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 2.3 Tool Result Tampering

**Threat ID:** T-003
**Severity:** HIGH
**Likelihood:** MEDIUM (no signature)
**Impact:** HIGH (incorrect results)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker intercepts tool execution result and modifies it before returning to client.

#### Attack Scenario
1. Client calls `tools/call` (e.g., database query tool)
2. Tool returns result: `[{"id": 1, "amount": 100}]`
3. Attacker modifies result: `[{"id": 1, "amount": 1000000}]`
4. Client trusts modified result

#### Current Mitigation
✅ **PROTECTED** - TLS encryption in transit

**Gap:** No integrity verification of tool results after TLS decryption

#### Remediation Plan
```erlang
%% Add message authentication code to tool results
sign_tool_result(ToolName, Result, Secret) ->
    Payload = jsx:encode(#{tool => ToolName, result => Result}),
    MAC = crypto:mac(hmac, sha256, Secret, Payload),
    #{
        result => Result,
        signature => base64:encode(MAC),
        timestamp => erlang:system_time(millisecond)
    }.

%% Verify tool result signature
verify_tool_result(SignedResult, Secret) ->
    #{result := Result, signature := Sig, timestamp := Ts} = SignedResult,
    Payload = jsx:encode(#{tool => Tool, result => Result}),
    ExpectedSig = crypto:mac(hmac, sha256, Secret, Payload),
    case constant_time_compare(Sig, ExpectedSig) of
        true -> {ok, Result};
        false -> {error, signature_invalid}
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 1 day
- Implementation: Add HMAC-SHA256 signing

---

### 2.4 Rate Limit Counter Tampering

**Threat ID:** T-004
**Severity:** MEDIUM
**Likelihood:** MEDIUM (ETS access)
**Impact:** MEDIUM (bypass rate limiting)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker with process access modifies ETS rate limit counters to bypass rate limiting.

#### Attack Scenario
1. Attacker gains Erlang process access (compromised node)
2. Attacker directly modifies ETS tables:
   `ets:delete_all_objects(erlmcp_rate_limits)`
3. All rate limit counters reset
4. Attacker can now send unlimited requests

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - ETS access restricted

**Gap:** No integrity verification of ETS state

#### Remediation Plan
```erlang
%% Add checksums to rate limit state
-record(rate_limit_state, {
    client_id :: binary(),
    tokens :: float(),
    last_refill :: integer(),
    checksum :: binary()  % HMAC of state
}).

%% Verify state integrity
verify_rate_limit_state(State, Secret) ->
    {Client, Tokens, Refill} = State,
    Payload = <<Client/binary, Tokens/binary, Refill/binary>>,
    ExpectedChecksum = crypto:mac(hmac, sha256, Secret, Payload),
    case State#rate_limit_state.checksum =:= ExpectedChecksum of
        true -> ok;
        false -> {error, state_tampering}
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 2 days
- Implementation: Add state checksums
- Note: Only applies if node compromise is in threat model

---

### 2.5 Path Canonicalization Bypass

**Threat ID:** T-005
**Severity:** HIGH
**Likelihood:** MEDIUM
**Impact:** HIGH (path traversal)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker uses alternate path representation to bypass canonicalization.

#### Attack Scenario
1. Allowed path: `/home/user/data/`
2. Attacker tries: `/home/user/./../../etc/`
3. Canonicalization converts to: `/etc/`
4. Path validation fails: not under allowed dir
5. But if there's a window between canonicalization and access check...

#### Current Mitigation
✅ **PROTECTED** - Atomic validation

```erlang
%% erlmcp_path_canonicalizer.erl
validate_resource_path(Uri, AllowedDirs) ->
    Path = extract_path_from_uri(Uri),
    CanonicalPath = canonicalize_path(Path),
    %% Atomic check - no window for TOCTOU
    case is_path_safe(CanonicalPath, AllowedDirs) of
        true -> {ok, rebuild_uri(Uri, CanonicalPath)};
        false -> {error, path_outside_allowed_dirs}
    end.
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 2.6 Message Size Limit Bypass

**Threat ID:** T-006
**Severity:** MEDIUM
**Likelihood:** LOW (enforced across transports)
**Impact:** MEDIUM (memory exhaustion)
**CVSS v3.1:** 5.5 (Medium)

#### Description
Attacker bypasses 16 MB message size limit.

#### Attack Scenario
1. Attacker tries to send 17 MB message
2. Size validation should reject it
3. If there's a transport that doesn't validate...

#### Current Mitigation
✅ **PROTECTED** - All transports validate

**Coverage:**
- ✅ HTTP/SSE transport
- ✅ WebSocket transport
- ✅ TCP transport
- ✅ Stdio transport (recently fixed)

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Coverage: COMPLETE

---

### 2.7 OAuth Token Modification

**Threat ID:** T-007
**Severity:** MEDIUM
**Likelihood:** LOW (token signature)
**Impact:** MEDIUM (privilege escalation)
**CVSS v3.1:** 5.5 (Medium)

#### Description
Attacker modifies OAuth token claims without invalidating signature.

#### Attack Scenario
1. Attacker obtains valid OAuth token: `{user_id: "123", scope: "read"}`
2. Attacker modifies to: `{user_id: "456", scope: "admin"}`
3. If server doesn't verify token signature, attack succeeds

#### Current Mitigation
✅ **PROTECTED** - OAuth tokens must have valid signature

**Gap:** No token binding to prevent substitution (see S-012)

#### Recommendation
- Status: ✅ NO ACTION NEEDED (for tampering)
- Note: See S-012 for token binding improvements

---

## PART 3: REPUDIATION THREATS

### 3.1 Unlogged Administrative Actions

**Threat ID:** R-001
**Severity:** HIGH
**Likelihood:** MEDIUM (no comprehensive audit)
**Impact:** MEDIUM (evidence destruction)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Administrator performs malicious action with no audit trail, later denies it.

#### Attack Scenario
1. Administrator modifies resource access permissions
2. No audit log created
3. User files compliance complaint
4. Administrator denies making changes (no evidence)

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Authentication logging exists

**Coverage:**
- ✅ Authentication attempts logged
- ✅ Authorization failures logged
- ❌ Successful authorization not logged
- ❌ Tool execution not logged
- ❌ Resource modifications not logged

#### Remediation Plan
```erlang
%% Add comprehensive audit logging
-module(erlmcp_audit_log).

-export([log_operation/4]).

log_operation(Actor, Operation, Resource, Result) ->
    Entry = #{
        timestamp => erlang:system_time(millisecond),
        actor => Actor,
        operation => Operation,
        resource => Resource,
        result => Result,
        ip_address => get_client_ip(),
        session_id => get_session_id()
    },
    write_audit_log(Entry),
    %% Also send to remote syslog for immutability
    send_to_syslog(Entry).
```

#### Recommendation
- Priority: P1 (HIGH)
- Effort: 3 days
- Implementation: Add audit logging framework

---

### 3.2 Log Tampering

**Threat ID:** R-002
**Severity:** MEDIUM
**Likelihood:** MEDIUM (file system access)
**Impact:** MEDIUM (evidence destruction)
**CVSS v3.1:** 5.5 (Medium)

#### Description
Attacker gains file system access and modifies audit logs.

#### Attack Scenario
1. Attacker compromises server (gets OS-level access)
2. Attacker modifies log files to remove evidence
3. No one knows attacker was there

#### Current Mitigation
⚠️ **NOT PROTECTED** - No log integrity verification

#### Remediation Plan
```erlang
%% Add log integrity verification with HMAC-SHA256
sign_log_entry(Entry, Secret) ->
    Json = jsx:encode(Entry),
    Signature = crypto:mac(hmac, sha256, Secret, Json),
    Entry#{signature => base64:encode(Signature)}.

%% Verify log integrity
verify_log_entry(Entry, Secret) ->
    Json = jsx:encode(maps:without([signature], Entry)),
    StoredSig = maps:get(signature, Entry),
    ComputedSig = crypto:mac(hmac, sha256, Secret, Json),
    case constant_time_compare(StoredSig, ComputedSig) of
        true -> ok;
        false -> {error, log_tampering_detected}
    end.

%% Append-only log file
append_signed_log(Filename, Entry, Secret) ->
    SignedEntry = sign_log_entry(Entry, Secret),
    {ok, File} = file:open(Filename, [append, binary]),
    file:write(File, jsx:encode(SignedEntry) ++ "\n"),
    file:close(File).
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 2 days
- Implementation: Add HMAC-SHA256 signing + append-only mode

---

### 3.3 Session Termination without Notification

**Threat ID:** R-003
**Severity:** LOW
**Likelihood:** LOW
**Impact:** LOW (session denial)
**CVSS v3.1:** 3.7 (Low)

#### Description
Administrator terminates session without logging action.

#### Attack Scenario
1. Administrator terminates user's session
2. No log entry created
3. User cannot prove admin terminated session

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Session timeout logged, termination not

#### Remediation
Add to audit logging framework (R-001)

---

### 3.4 Tool Execution Without Tracing

**Threat ID:** R-004
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM
**CVSS v3.1:** 5.5 (Medium)

#### Description
Tool execution occurs without audit trail.

#### Impact
- Cannot trace which user executed tool
- Cannot prove tool was called correctly
- Cannot replay execution for debugging

#### Current Mitigation
⚠️ **NOT PROTECTED** - Tool execution not logged

#### Remediation
Add tool execution logging to audit framework (R-001)

---

## PART 4: INFORMATION DISCLOSURE THREATS

### 4.1 Error Message Information Leakage

**Threat ID:** I-001
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM (reconnaissance)
**CVSS v3.1:** 5.5 (Medium)

#### Description
Error messages reveal internal system details.

#### Attack Scenario
1. Attacker sends malformed request
2. Server returns detailed error: `"ETS table erlmcp_auth_tokens crashed"`
3. Attacker learns about internal architecture
4. Attacker crafts targeted attacks

#### Current Mitigation
✅ **PROTECTED** - Error messages sanitized

**Implementation:**
```erlang
%% erlmcp_errors.erl
error_to_json_rpc(Error) ->
    case Error of
        invalid_request ->
            #{code => -32600, message => <<"Invalid Request">>};
        invalid_params ->
            #{code => -32602, message => <<"Invalid Params">>};
        internal_error ->
            %% Never reveal internal details
            #{code => -32603, message => <<"Internal error">>};
        % ... more cases
    end.
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 4.2 Rate Limit Information Disclosure

**Threat ID:** I-002
**Severity:** LOW
**Likelihood:** MEDIUM
**Impact:** LOW (enumeration)
**CVSS v3.1:** 3.7 (Low)

#### Description
Rate limit response headers reveal rate limit thresholds.

#### Attack Scenario
1. Attacker observes X-RateLimit-Remaining header
2. Attacker determines threshold: 100 requests/minute
3. Attacker sends 90 requests, waits for reset
4. Attacker sends 100 requests in next window

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Headers included by default

**Gap:** No way to disable rate limit header disclosure

#### Remediation Plan
```erlang
%% Configuration to disable rate limit headers
{erlmcp, [
    {rate_limit_header_disclosure, false}  % Hide from responses
]}
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 1 day
- Implementation: Add config flag

---

### 4.3 Timing Attack on Token Validation

**Threat ID:** I-003
**Severity:** LOW
**Likelihood:** VERY LOW (constant-time comparison)
**Impact:** LOW (enumeration)
**CVSS v3.1:** 3.7 (Low)

#### Description
Token validation timing reveals whether token is partially valid.

#### Attack Scenario
1. Attacker measures response time for token validation
2. Valid token signature check takes 10ms
3. Invalid token check takes 5ms
4. Attacker uses timing to enumerate valid tokens

#### Current Mitigation
✅ **PROTECTED** - Constant-time comparison

**Implementation:**
```erlang
%% JOSE library uses constant-time comparison
jose_jws:verify(JWK, Token) ->
    %% Internally uses constant_time_compare
    ...
end.
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 4.4 Resource Metadata Leakage

**Threat ID:** I-004
**Severity:** HIGH
**Likelihood:** MEDIUM
**Impact:** MEDIUM (reconnaissance)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Resource metadata reveals system configuration and sensitive information.

#### Attack Scenario
1. Attacker lists resources
2. Response includes: `{"name": "db_backup.sql", "size": 5GB, "modified": "2024-01-01"}`
3. Attacker learns:
   - System uses SQL databases
   - Backup size (data volume)
   - Last backup time
4. Attacker crafts targeted backup extraction attacks

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Path access control enforced

**Gap:** No metadata field filtering for sensitive information

#### Remediation Plan
```erlang
%% Filter sensitive metadata fields
filter_resource_metadata(Metadata, Principal) ->
    case erlmcp_auth:check_permission(Principal, resource, admin) of
        {ok, _} ->
            %% Admin sees all metadata
            Metadata;
        {error, _} ->
            %% Regular user sees filtered metadata
            Metadata#{
                size => undefined,     % Hide file size
                modifiedTime => undefined,  % Hide modification time
                mimeType => <<"application/octet-stream">>  % Generic type
            }
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 2 days
- Implementation: Add metadata filtering

---

### 4.5 Authorization Header Leakage in Logs

**Threat ID:** I-005
**Severity:** HIGH
**Likelihood:** MEDIUM
**Impact:** HIGH (credential exposure)
**CVSS v3.1:** 7.5 (High)

#### Description
Authorization headers (API keys, tokens) logged in plain text.

#### Attack Scenario
1. Server logs full HTTP request: `Authorization: Bearer abc123def456`
2. Attacker accesses log files
3. Attacker extracts tokens/API keys
4. Attacker uses credentials for unauthorized access

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Should redact credentials

**Gap:** Automatic credential redaction not implemented

#### Remediation Plan
```erlang
%% Redact sensitive headers in logs
sanitize_headers_for_log(Headers) ->
    SensitiveHeaders = [
        <<"authorization">>,
        <<"x-api-key">>,
        <<"cookie">>,
        <<"set-cookie">>
    ],
    maps:map(fun(Key, Value) ->
        case lists:member(Key, SensitiveHeaders) of
            true -> <<"[REDACTED]">>;
            false -> Value
        end
    end, Headers).

%% Log with redacted headers
log_request(Method, Path, Headers) ->
    RedactedHeaders = sanitize_headers_for_log(Headers),
    logger:info("~s ~s ~p", [Method, Path, RedactedHeaders]).
```

#### Recommendation
- Priority: P1 (HIGH)
- Effort: 1 day
- Implementation: Add header redaction

---

### 4.6 Session ID Leakage in Referer Headers

**Threat ID:** I-006
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM (session hijacking)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Session IDs leak in Referer headers when user clicks external link.

#### Attack Scenario
1. User has active session with MCP server
2. Session ID in URL: `/api/resource?session=abc123def456`
3. User clicks external link
4. Browser sends Referer header: `Referer: https://mcp-server/api/resource?session=abc123def456`
5. External site receives session ID

#### Current Mitigation
⚠️ **NOT PROTECTED** - No referrer policy configured

#### Remediation Plan
```erlang
%% Add Referrer-Policy header
add_security_headers(Headers) ->
    Headers#{
        <<"referrer-policy">> => <<"no-referrer">>,
        <<"x-content-type-options">> => <<"nosniff">>
    }.

%% Don't include session IDs in URLs
generate_session_url(SessionId) ->
    %% Wrong: https://server/api?session=abc123
    %% Correct: Use cookie or header instead
    ok.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 1 day
- Implementation: Use cookies, add referrer policy

---

## PART 5: DENIAL OF SERVICE THREATS

### 5.1 Connection Pool Exhaustion

**Threat ID:** D-001
**Severity:** CRITICAL
**Likelihood:** MEDIUM
**Impact:** HIGH (service unavailable)
**CVSS v3.1:** 8.6 (High)

#### Description
Attacker opens many connections without sending data, exhausting connection pool.

#### Attack Scenario
1. Attacker opens 1000 TCP connections
2. Attacker doesn't send any data (slow/incomplete requests)
3. Server holds connections open waiting for requests
4. Connection pool exhausted
5. Legitimate users cannot connect

#### Current Mitigation
✅ **PROTECTED** - Rate limiting enforced

**Protections:**
- Per-client connection rate limit: 10 connections/sec
- Connection timeout: 30 seconds (default)
- Pool size limit: configurable

**Gap:** No connection idle timeout (<30 sec)

#### Remediation Plan
```erlang
%% Add per-connection idle timeout
{erlmcp, [
    {connection_idle_timeout_ms, 300000}  % 5 minutes idle
]}

%% Monitor and close idle connections
monitor_idle_connections() ->
    IdleThreshold = application:get_env(erlmcp, connection_idle_timeout_ms, 300000),
    erlmcp_connection_monitor:cleanup_idle_connections(IdleThreshold).
```

#### Recommendation
- Priority: P1 (HIGH)
- Effort: 1 day
- Implementation: Add idle timeout

---

### 5.2 Message Flooding Attack

**Threat ID:** D-002
**Severity:** HIGH
**Likelihood:** MEDIUM (rate limiting)
**Impact:** HIGH (service degradation)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker sends many small messages to consume server resources.

#### Attack Scenario
1. Attacker sends 1000 empty JSON-RPC messages/sec
2. Each message consumes CPU for parsing and validation
3. Server CPU usage spikes
4. Legitimate requests get delayed

#### Current Mitigation
✅ **PROTECTED** - Global rate limiting

**Configuration:**
```erlang
{erlmcp, [
    {rate_limiting, #{
        global_max_messages_per_sec => 10000,
        max_messages_per_sec => 100  % Per client
    }}
]}
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 5.3 Large Message Amplification Attack

**Threat ID:** D-003
**Severity:** HIGH
**Likelihood:** LOW (size limits)
**Impact:** MEDIUM (memory exhaustion)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker sends large messages to exhaust memory.

#### Attack Scenario
1. Attacker sends 16 MB message (at size limit)
2. Server allocates memory for parsing
3. JSON parsing uses 2x memory (parsing overhead)
4. Server allocates 32 MB for single message
5. Attacker sends 100 messages in parallel
6. Server exhausts memory (3.2 GB)

#### Current Mitigation
✅ **PARTIALLY PROTECTED** - Size limits and circuit breakers

**Protections:**
- 16 MB message size limit
- Per-client rate limiting
- Memory monitoring + circuit breaker
- Graceful degradation under load

#### Recommendation
- Status: ✅ NO ACTION NEEDED (good coverage)
- Note: Monitor memory usage in production

---

### 5.4 Tool Execution Timeout Attack

**Threat ID:** D-004
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM (resource exhaustion)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker calls tools with parameters that cause long execution time.

#### Attack Scenario
1. Attacker calls `tools/call` with expensive operation
2. Example: database query that locks table
3. Tool execution takes 1 minute
4. Attacker sends 100 concurrent tool calls
5. Server spawns 100 processes, all waiting
6. System resources exhausted

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Per-client tool call limits

**Configuration:**
```erlang
{erlmcp, [
    {rate_limiting, #{
        max_tool_calls_per_sec => 50  % Per client
    }}
]}
```

**Gap:** No per-tool timeout or execution limits

#### Remediation Plan
```erlang
%% Add per-tool execution timeout
-record(tool_config, {
    name :: binary(),
    timeout_ms :: pos_integer(),  % Maximum execution time
    max_concurrent :: pos_integer()  % Max parallel executions
}).

execute_tool(ToolName, Args) ->
    case get_tool_config(ToolName) of
        {ok, Config} ->
            Timeout = Config#tool_config.timeout_ms,
            case tool_execute_with_timeout(ToolName, Args, Timeout) of
                {ok, Result} -> Result;
                {error, timeout} ->
                    {ok, #{
                        type => <<"text">>,
                        text => <<"Tool execution timed out">>
                    }};
                {error, Error} -> {error, Error}
            end;
        {error, _} -> {error, tool_not_found}
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 2 days
- Implementation: Add tool execution timeout

---

### 5.5 Regex Denial of Service (ReDoS)

**Threat ID:** D-005
**Severity:** MEDIUM
**Likelihood:** LOW (no regex used)
**Impact:** HIGH (CPU exhaustion)
**CVSS v3.1:** 7.5 (High)

#### Description
Attacker inputs crafted string that causes regex engine to hang.

#### Attack Scenario
1. If path validation used regex: `^(/[a-z0-9]*)*$`
2. Attacker sends path: `/` repeated 1000 times
3. Regex engine tries all combinations
4. CPU usage spikes to 100%

#### Current Mitigation
✅ **PROTECTED** - No regex used for path validation

**Implementation:**
```erlang
%% erlmcp_path_canonicalizer.erl uses string:split/3, not regex
process_path_components([Component | Rest], Acc) ->
    process_path_components(Rest, [Component | Acc]).
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 5.6 Slowloris Attack

**Threat ID:** D-006
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM (connection exhaustion)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker sends HTTP requests with incomplete headers very slowly.

#### Attack Scenario
1. Attacker connects to HTTP server
2. Sends headers one byte at a time every 10 seconds
3. Server keeps connection open waiting for complete request
4. After 1 minute, request still incomplete
5. Attacker opens 1000 connections this way
6. Server exhausts connection pool

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Request timeout exists (30 sec default)

**Gap:** No per-header line timeout

#### Remediation Plan
```erlang
%% Add per-line read timeout
{erlmcp, [
    {http_line_timeout_ms, 5000}  % 5 seconds per header line
]}

%% Cowboy configuration
CowboyOpts = #{
    inactivity_timeout => 5000,  % 5 seconds between packets
    request_timeout => 30000     % 30 seconds for full request
}.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 1 day
- Implementation: Configure Cowboy timeout settings

---

### 5.7 Subscription Flood Attack

**Threat ID:** D-007
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM (memory exhaustion)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker creates many resource subscriptions to exhaust server memory.

#### Attack Scenario
1. Attacker sends `resources/subscribe` for 1 million resources
2. Server allocates memory for each subscription
3. Memory usage spikes
4. Server becomes unresponsive

#### Current Mitigation
✅ **PROTECTED** - Per-client subscription rate limit

**Configuration:**
```erlang
{erlmcp, [
    {rate_limiting, #{
        max_subscriptions_per_sec => 20  % Per client
    }}
]}
```

#### Recommendation
- Status: ✅ NO ACTION NEEDED (rate limiting effective)
- Note: Add per-client subscription count limit for additional protection

---

### 5.8 Cache Poisoning via Large Payloads

**Threat ID:** D-008
**Severity:** MEDIUM
**Likelihood:** MEDIUM
**Impact:** MEDIUM (cache exhaustion)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker sends requests with large payloads to poison cache.

#### Attack Scenario
1. Attacker sends tool call with 1 MB input
2. Server caches response (1 MB)
3. Attacker sends 1000 different tool calls
4. Cache grows to 1 GB
5. Server memory exhausted

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - Size limits and rate limiting

**Gap:** No cache size limit or LRU eviction

#### Remediation Plan
```erlang
%% Add cache size limiting
-module(erlmcp_cache).

-export([put/2, get/1, limit_size/1]).

-define(MAX_CACHE_SIZE_BYTES, 100 * 1024 * 1024).  % 100 MB

put(Key, Value) ->
    case check_cache_size(byte_size(Value)) of
        ok -> ets:insert(erlmcp_cache, {Key, Value});
        {error, cache_full} ->
            %% Evict least recently used
            evict_lru_entry(),
            ets:insert(erlmcp_cache, {Key, Value})
    end.
```

#### Recommendation
- Priority: P2 (MEDIUM)
- Effort: 2 days
- Implementation: Add cache size limits + LRU eviction

---

## PART 6: ELEVATION OF PRIVILEGE THREATS

### 6.1 Missing Authorization Check

**Threat ID:** E-001
**Severity:** CRITICAL
**Likelihood:** LOW (checks implemented)
**Impact:** CRITICAL (complete access)
**CVSS v3.1:** 9.8 (Critical)

#### Description
Code path exists without authorization check, allowing unauthorized access.

#### Attack Scenario
1. Attacker discovers endpoint that accesses sensitive resource
2. Endpoint missing `erlmcp_auth:check_permission/3` call
3. Attacker directly accesses resource without credentials
4. Attacker gains full access

#### Current Mitigation
✅ **PROTECTED** - Authorization checks required

**Implementation:**
```erlang
handle_resource_read(ResourceUri, Principal, State) ->
    case erlmcp_auth:check_permission(Principal, ResourceUri, read) of
        {ok, _} -> get_resource(ResourceUri);
        {error, forbidden} -> {error, permission_denied}
    end.
```

**Test Coverage:**
- 50+ authorization tests
- Code review process enforces checks

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 6.2 Role-Based Access Control (RBAC) Bypass

**Threat ID:** E-002
**Severity:** CRITICAL
**Likelihood:** MEDIUM (RBAC complexity)
**Impact:** CRITICAL (privilege escalation)
**CVSS v3.1:** 9.8 (Critical)

#### Description
Attacker bypasses RBAC system by exploiting logic errors.

#### Attack Scenario
1. Attacker has role "user" with permissions: [read, write]
2. Attacker tries to execute action that requires "admin" role: [delete, admin]
3. Permission check logic bug: `[read] ⊆ [delete]` returns true (wrong)
4. Attacker executes admin action

#### Current Mitigation
✅ **PROTECTED** - Correct permission checking

**Implementation:**
```erlang
check_permission(Principal, Resource, Action) ->
    UserId = Principal#principal.user_id,
    UserRoles = get_user_roles(UserId),
    RequiredRoles = get_required_roles(Resource, Action),
    %% Check if user has any required role
    case lists:any(fun(Role) -> lists:member(Role, UserRoles) end, RequiredRoles) of
        true -> {ok, Principal};
        false -> {error, forbidden}
    end.
```

**Test Coverage:**
- 30+ RBAC tests
- Permission matrix validation

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: HIGH

---

### 6.3 Privilege Escalation via JWT Claims Modification

**Threat ID:** E-003
**Severity:** CRITICAL
**Likelihood:** LOW (signature verification)
**Impact:** CRITICAL (privilege escalation)
**CVSS v3.1:** 9.8 (Critical)

#### Description
Attacker modifies JWT claims to escalate privileges.

#### Attack Scenario
1. Attacker receives valid JWT: `{sub: "user123", roles: ["user"]}`
2. Attacker modifies: `{sub: "user123", roles: ["admin"]}`
3. If signature not verified, server accepts modified token
4. Attacker becomes admin

#### Current Mitigation
✅ **PROTECTED** - Signature verification mandatory

**Test Evidence:**
- Test: `erlmcp_auth_tests.erl` line 489 - "Algorithm confusion prevents privilege escalation"
- Coverage: ✅ VERIFIED

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Confidence: VERY HIGH

---

### 6.4 Session Hijacking via Weak Binding

**Threat ID:** E-004
**Severity:** CRITICAL
**Likelihood:** MEDIUM
**Impact:** CRITICAL (account takeover)
**CVSS v3.1:** 9.8 (Critical)

#### Description
Session ID stolen and used from different context (IP, device).

#### Attack Scenario
1. Attacker steals session ID (from logs, Referer header, etc.)
2. Attacker connects from different IP address
3. Server accepts session ID without context verification
4. Attacker gains access as victim

#### Current Mitigation
⚠️ **PARTIALLY PROTECTED** - No IP binding

**Gap:** Session ID not bound to client context

#### Remediation Plan
(See S-004 Session Fixation threat for details)

**Priority:** P1 (HIGH)
**Effort:** 2 days

---

### 6.5 Authentication Bypass via Default Credentials

**Threat ID:** E-005
**Severity:** CRITICAL
**Likelihood:** MEDIUM (config-based)
**Impact:** CRITICAL (account takeover)
**CVSS v3.1:** 9.8 (Critical)

#### Description
Default credentials left in production configuration.

#### Attack Scenario
1. Administrator deploys with default config
2. Config includes: `{default_admin_key => "admin123"}`
3. Attacker uses known default key
4. Attacker logs in as admin

#### Current Mitigation
✅ **PROTECTED** - No default credentials in code

**Configuration Requirements:**
```erlang
%% sys.config must provide actual credentials
{erlmcp, [
    {api_keys, #{
        <<"default">> => <<"YOUR_SECRET_KEY_HERE">>  % No default
    }}
]}
```

**Deployment Best Practices:**
- Require explicit configuration (no defaults)
- Validate configuration on startup
- Alert on missing required credentials

#### Recommendation
- Status: ✅ NO ACTION NEEDED
- Implementation: Good (no default credentials)

---

### 6.6 OAuth Scope Escalation

**Threat ID:** E-006
**Severity:** HIGH
**Likelihood:** MEDIUM
**Impact:** MEDIUM (privilege escalation)
**CVSS v3.1:** 6.5 (Medium)

#### Description
Attacker requests more scopes than authorized during OAuth flow.

#### Attack Scenario
1. OAuth consent shows scopes: [read, write]
2. Attacker modifies request to add: [delete, admin]
3. If server doesn't validate scopes, attack succeeds

#### Current Mitigation
✅ **PARTIALLY PROTECTED** - Scope validation exists

**Gap:** No incremental scope consent support (SEP-835)

#### Remediation Plan
```erlang
%% Add OAuth scope validation
validate_oauth2_scopes(RequestedScopes, AllowedScopes) ->
    case lists:all(fun(Scope) -> lists:member(Scope, AllowedScopes) end, RequestedScopes) of
        true -> {ok, RequestedScopes};
        false ->
            %% Scope requested that wasn't authorized
            {error, {scope_not_authorized, RequestedScopes}}
    end.

%% Support incremental consent (SEP-835)
request_incremental_consent(Scope) ->
    %% Use WWW-Authenticate header for dynamic scope request
    erlmcp_oauth2:request_scope_via_authenticate_header(Scope).
```

#### Recommendation
- Priority: P1 (HIGH)
- Effort: 3 days
- Implementation: Add scope validation + incremental consent

---

## SUMMARY & RECOMMENDATIONS

### Risk Ranking

**CRITICAL (Fix Immediately)**
1. S-002: API Key Brute Force (Global limits)
2. S-003: OAuth Token Interception (Enforce HTTPS)
3. S-007: mTLS Certificate Impersonation (Add pinning)
4. T-003: Tool Result Tampering (Add signatures)
5. E-001: Missing Authorization Checks (Audit code)
6. E-003: JWT Claims Modification (Signature verification)
7. D-001: Connection Pool Exhaustion (Add timeouts)
8. E-004: Session Hijacking (Add IP binding)
9. E-006: OAuth Scope Escalation (Validate scopes)
10. I-005: Authorization Header Leakage (Redact logs)

### Implementation Roadmap

**Week 1 (Critical)**
- [ ] Enforce HTTPS in production (S-003)
- [ ] Redact sensitive headers in logs (I-005)
- [ ] Add connection idle timeout (D-001)
- [ ] Add IP binding to sessions (E-004)

**Week 2-3 (High)**
- [ ] Add global brute force detection (S-002)
- [ ] Implement audit logging (R-001)
- [ ] Add tool execution timeout (D-004)
- [ ] Validate OAuth scopes (E-006)

**Week 4+ (Medium)**
- [ ] Add certificate pinning (S-007)
- [ ] Add log signing (R-002)
- [ ] Add tool result signatures (T-003)
- [ ] Add cache size limits (D-008)

---

**END OF THREAT MODEL**
