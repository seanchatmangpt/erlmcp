# erlmcp v3 Security Hardening Guide

**Version:** 3.0.0-oss
**Target Audience:** DevOps Engineers, Security Professionals
**Purpose:** Production security hardening steps

---

## Quick Start: 5-Minute Security Checklist

### Minimal Security (Development)

```erlang
{erlmcp, [
    {transports, [stdio]},  % Local only, no network
    {validation, #{
        json_schema => true,
        max_message_size => 10485760
    }}
]}.
```

### Basic Security (Staging)

```erlang
{erlmcp, [
    {transports, [tcp, http]},
    {tls, #{
        enabled => true,
        verify => verify_peer,
        cacertfile => "/etc/ssl/certs/ca-certificates.crt"
    }},
    {authentication, #{
        enabled => true,
        methods => [api_key],
        api_keys => #{
            {os_env, "ERLMCP_API_KEY"} => <<"staging_user">>
        }
    }},
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 100
    }}
]}.
```

### Production Security

```erlang
{erlmcp, [
    {transports, [tcp, http, websocket]},
    {tls, #{
        enabled => true,
        verify => verify_peer,
        versions => ['tlsv1.2', 'tlsv1.3'],
        certfile => {os_env, "ERLMCP_TLS_CERT"},
        keyfile => {os_env, "ERLMCP_TLS_KEY"},
        cacertfile => {os_env, "ERLMCP_TLS_CA"}
    }},
    {authentication, #{
        enabled => true,
        methods => [jwt, oauth2],
        jwt => #{
            required_issuer => {os_env, "JWT_ISSUER"},
            required_audience => {os_env, "JWT_AUDIENCE"}
        },
        oauth2 => #{
            introspection_url => {os_env, "OAUTH2_INTROSPECTION_URL"},
            client_id => {os_env, "OAUTH2_CLIENT_ID"},
            client_secret => {os_env, "OAUTH2_CLIENT_SECRET"}
        }
    }},
    {authorization, #{
        enabled => true,
        default_role => <<"guest">>
    }},
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 10,
        max_failures => 5,
        block_duration_ms => 300000
    }},
    {secrets, [
        {backend, vault},
        {backend_config, #{
            address => {os_env, "VAULT_ADDR"},
            auth_method => approle,
            role_id => {os_env, "VAULT_ROLE_ID"},
            secret_id => {os_env, "VAULT_SECRET_ID"}
        }}
    ]}
]}.
```

---

## Hardening Levels

### Level 1: Baseline Security (Mandatory)

**Objective:** Meet minimum security standards for any deployment.

**Steps:**

1. **Enable TLS for Network Transports**
   ```bash
   export ERLMCP_TLS_CERT="/etc/erlmcp/tls/server.crt"
   export ERLMCP_TLS_KEY="/etc/erlmcp/tls/server.key"
   export ERLMCP_TLS_CA="/etc/erlmcp/tls/ca.crt"
   ```

2. **Verify Certificate Configuration**
   ```erlang
   %% In erl shell
   {ok, CertInfo} = file:read_file(os:getenv("ERLMCP_TLS_CERT")),
   io:format("Certificate loaded: ~p bytes~n", [byte_size(CertInfo)]).
   ```

3. **Run Security Validator**
   ```bash
   rebar3 compile
   erl -pa _build/default/lib/*/ebin -noshell -eval "
       {ok, _} = application:ensure_all_started(erlmcp),
       {ok, Report} = erlmcp_security_validator:run(erlmcp_transport_tcp),
       io:format('~p~n', [Report]),
       halt().
   "
   ```

4. **Validate Input Processing**
   ```bash
   %% Test with oversized message (should reject)
   echo '{"jsonrpc":"2.0","method":"test","params":"'$(python3 -c 'print("A"*11000000')}'","id":1}' | \
       erlmcp validate stdin
   %% Expected: {"jsonrpc":"2.0","error":{"code":-32700,"message":"Message too large"},"id":1}
   ```

**Validation Checklist:**
- [ ] TLS enabled for TCP/HTTP/WS
- [ ] Certificate validation active
- [ ] Message size limits enforced
- [ ] Security validator passes
- [ ] No hardcoded secrets detected

### Level 2: Authentication Hardening

**Objective:** Implement robust authentication and authorization.

**Steps:**

1. **Configure JWT Authentication**
   ```erlang
   %% Generate JWT keys (using openssl)
   %% Private key (for identity provider, not erlmcp):
   openssl ecparam -genkey -name prime256v1 -noout -out jwt_private.pem

   %% Public key (for erlmcp validation):
   openssl ec -in jwt_private.pem -pubout -out jwt_public.pem

   %% Convert to PEM string for config:
   base64 -i jwt_public.pem
   ```

   ```erlang
   {authentication, #{
       methods => [jwt],
       jwt => #{
           required_issuer => <<"https://auth.yourdomain.com">>,
           required_audience => <<"erlmcp-production">>,
           public_keys => #{
               <<"key-2024-01">> => <<"
                   -----BEGIN PUBLIC KEY-----
   MFkwEQYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEEVs/o5+uQbTjL3jynYl8kF0...
                   -----END PUBLIC KEY-----
               ">>
           }
       }
   }}
   ```

2. **Test JWT Validation**
   ```erlang
   %% Valid JWT test
   ValidJWT = <<"eyJhbGciOiJFUzI1NiIsImtpZCI6ImtleS0yMDI0LTAxIn0.eyJzdWIiOiJ1c2VyXzEyMyIsImF1ZCI6ImVybG1jcC1wcm9kdWN0aW9uIiwiaXNzIjoiaHR0cHM6Ly9hdXRoLnlvdXJkb21haW4uY29tIiwiZXhwIjoxNzYxMjM0NTY3fQ.SIGNATURE">>,
   {ok, Claims} = erlmcp_auth:validate_jwt(ValidJWT),
   io:format("Validated user: ~p~n", [maps:get(<<"sub">>, Claims)]).

   %% Expired JWT test (should fail)
   ExpiredJWT = <<"eyJhbGciOiJFUzI1NiIsImtpZCI6ImtleS0yMDI0LTAxIn0.eyJzdWIiOiJ1c2VyXzEyMyIsImV4cCI6MTYxMjM0NTY3fQ.SIGNATURE">>,
   {error, token_expired} = erlmcp_auth:validate_jwt(ExpiredJWT).
   ```

3. **Configure Rate Limiting**
   ```erlang
   {rate_limiting, #{
       enabled => true,
       max_attempts_per_second => 10,
       max_failures => 5,
       block_duration_ms => 300000,
       backoff_levels => [0, 1000, 2000, 4000, 8000, 16000]
   }}
   ```

4. **Set Up RBAC**
   ```erlang
   %% Define roles and permissions
   {authorization, #{
       enabled => true,
       default_role => <<"guest">>,
       roles => #{
           <<"guest">> => [<<"read">>],
           <<"user">> => [<<"read">>, <<"write">>],
           <<"admin">> => [<<"read">>, <<"write">>, <<"execute">>, <<"delete">>]
       },
       user_roles => #{
           <<"user_123">> => [<<"user">>],
           <<"service_456">> => [<<"admin">>]
       }
   }}

   %% Test permission check
   {ok, SessionId} = erlmcp_auth:create_session(<<"user_123">>, #{}),
   ok = erlmcp_auth:check_permission(SessionId, <<"resources">>, <<"read">>),
   {error, forbidden} = erlmcp_auth:check_permission(SessionId, <<"resources">>, <<"delete">>).
   ```

**Validation Checklist:**
- [ ] JWT validation working with real tokens
- [ ] Expired tokens rejected
- [ ] Invalid signatures rejected
- [ ] Rate limiting enforces limits
- [ ] Permission checks work correctly

### Level 3: Network Hardening

**Objective:** Secure network communications and prevent attacks.

**Steps:**

1. **Configure mTLS (Zero Trust)**

   **Note:** mTLS stub exists but needs completion. Current workaround:

   ```erlang
   %% Use TLS client certificate validation
   {tls, #{
       verify => verify_peer,
       fail_if_no_peer_cert => true,
       verify_fun => {fun ssl_peercert_validator:validate_peer_cert/3, #{}
   }}}
   ```

2. **Configure CORS for HTTP**
   ```erlang
   {transports, [
       {http, #{
           port => 8080,
           tls => true,
           allowed_origins => [
               <<"https://app.yourdomain.com">>,
               <<"https://*.yourdomain.com">>
           ],
           allowed_methods => [<<"POST">>, <<"GET">>, <<"OPTIONS">>],
           allowed_headers => [<<"authorization">>, <<"content-type">>],
           max_age => 86400
       }}
   ]}
   ```

3. **Test Origin Validation**
   ```bash
   %% Valid origin (should pass)
   curl -X POST https://erlmcp.example.com:8080/mcp \
     -H "Origin: https://app.yourdomain.com" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"ping","id":1}'

   %% Invalid origin (should be rejected)
   curl -X POST https://erlmcp.example.com:8080/mcp \
     -H "Origin: https://evil.com" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","method":"ping","id":1}'
   %% Expected: 403 Forbidden
   ```

4. **Configure Security Headers**
   ```erlang
   {security_headers, #{
       enabled => true,
       content_security_policy => "default-src 'self'; script-src 'self'; object-src 'none'",
       strict_transport_security => "max-age=31536000; includeSubDomains; preload",
       x_frame_options => "DENY",
       x_content_type_options => "nosniff",
       referrer_policy => "strict-origin-when-cross-origin",
       permissions_policy => "geolocation=(), microphone=(), camera=()"
   }}
   ```

**Validation Checklist:**
- [ ] TLS 1.2+ only (no SSLv3/TLS 1.0/1.1)
- [ ] Strong cipher suites only
- [ ] Certificate chain validation working
- [ ] CORS rejecting invalid origins
- [ ] Security headers present on responses

### Level 4: Secrets Management

**Objective:** Secure secret storage and rotation.

**Steps:**

1. **Configure Vault Integration**
   ```bash
   # Install Vault
   vault server -dev

   # Enable KV secrets engine
   vault secrets enable -path=erlmcp kv

   # Store JWT public keys
   vault kv put erlmcp/jwt/keys \
     key-2024-01="$(cat jwt_public.pem)"

   # Configure Vault auth
   vault auth enable approle
   vault write auth/approle/role/erlmcp \
     token_policies="erlmcp-policy" \
     token_ttl=1h

   # Get role-id and secret-id
   vault read auth/approle/role/erlmcp/role-id
   vault write -f auth/approle/role/erlmcp/secret-id
   ```

   ```erlang
   {secrets, [
       {backend, vault},
       {backend_config, #{
           address => "https://vault.internal:8200",
           auth_method => approle,
           role_id => {os_env, "VAULT_ROLE_ID"},
           secret_id => {os_env, "VAULT_SECRET_ID"},
           engine => "kv",
           mount => "erlmcp"
       }},
       {ttl_seconds => 300}
   ]}
   ```

2. **Implement Key Rotation**
   ```erlang
   %% Rotate JWT public key
   {ok, NewKeyPEM} = file:read_file("jwt_public_new.pem"),
   ok = erlmcp_auth:rotate_public_key(<<"key-2024-02">>, NewKeyPEM).

   %% Verify rotation
   {ok, Claims} = erlmcp_auth:validate_jwt(NewJWT),
   io:format("New key validated: ~p~n", [maps:get(<<"sub">>, Claims)]).
   ```

3. **Set Up Audit Logging**
   ```erlang
   {audit, [
       {backend, file},
       {backend_config, #{
         log_path => "/var/log/erlmcp/audit.log",
         rotation => daily,
         retention_days => 90
       }},
       {events, [
         authentication_success,
         authentication_failure,
         authorization_denied,
         rate_limit_exceeded,
         secret_accessed
       ]}
   ]}
   ```

**Validation Checklist:**
- [ ] Vault integration working
- [ ] Secrets loaded from Vault
- [ ] Key rotation successful
- [ ] Audit logs capturing security events
- [ ] No secrets in logs/environment

### Level 5: Monitoring & Incident Response

**Objective:** Detect and respond to security incidents.

**Steps:**

1. **Configure Security Metrics**
   ```erlang
   {observability, [
       {metrics, [
           {security, [
               {authentication_failures, counter},
               {authorization_denials, counter},
               {rate_limit_breaches, counter},
               {invalid_input_errors, counter},
               {tls_handshake_failures, counter}
           ]}
       ]},
       {alerts, [
           {{security, authentication_failures}, [
               {threshold, 10},
               {window, 60},
               {action, alert_security_team}
           ]},
           {{security, rate_limit_breaches}, [
               {threshold, 5},
               {window, 60},
               {action, block_ip}
           ]}
       ]}
   ]}
   ```

2. **Set Up Intrusion Detection**
   ```bash
   # Use osquery or similar
   osqueryi --pack "erlmcp-security-monitoring"

   # Monitor for suspicious patterns:
   # - Excessive authentication failures
   # - Unusual access patterns
   # - Invalid input attempts
   # - TLS handshake failures
   ```

3. **Create Incident Response Runbook**
   ```markdown
   ## Security Incident Response

   ### Phase 1: Detection (0-15 min)
   - [ ] Alert received from monitoring
   - [ ] Verify incident (false positive?)
   - [ ] Classify severity (P1-P4)

   ### Phase 2: Containment (15-60 min)
   - [ ] Isolate affected systems
   - [ ] Block attacker IPs
   - [ ] Revoke compromised credentials
   - [ ] Enable enhanced logging

   ### Phase 3: Eradication (1-4 hours)
   - [ ] Identify root cause
   - [ ] Patch vulnerability
   - [ ] Scan for persistence mechanisms
   - [ ] Verify no backdoors

   ### Phase 4: Recovery (4-24 hours)
   - [ ] Restore from clean backups
   - [ ] Validate system integrity
   - [ ] Monitor for recurrence
   - [ ] Document lessons learned

   ### Phase 5: Post-Incident (24-72 hours)
   - [ ] Conduct retrospective
   - [ ] Update security policies
   - [ ] Improve detection rules
   - [ ] Share threat intelligence
   ```

**Validation Checklist:**
- [ ] Security metrics collected
- [ ] Alert thresholds configured
- [ ] Incident response runbook created
- [ ] Team trained on procedures
- [ ] Post-mortem process defined

---

## Testing Security Controls

### Security Validation Suite

```bash
# Run full security validation
rebar3 compile
erlmcp_validate run --security --verbose

# Expected output:
# ✓ Authentication mechanism configured
# ✓ Token handling secure
# ✓ Session management secure
# ✓ Authorization checks in place
# ✓ JSON Schema validation enabled
# ✓ No hardcoded secrets found
# ✓ JWT signature validation active
# ✓ Rate limiting configured
# ✓ CORS headers configured
# ✓ Origin validation enabled
#
# Security Score: 100% (10/10 checks passed)
```

### Penetration Testing

```bash
# Test authentication bypass
for i in {1..20}; do
  curl -X POST https://erlmcp.test/mcp \
    -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","method":"tools/call","id":'"$i"'}'
done
# Expected: 401 Unauthorized after rate limit

# Test input validation
curl -X POST https://erlmcp.test/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0":"method":"../../etc/passwd","id":1}'
# Expected: 400 Bad Request (path traversal rejected)

# Test TLS configuration
nmap --script ssl-enum-ciphers -p 9000 erlmcp.test
# Expected: Only TLS 1.2+ and strong ciphers
```

---

## Environment-Specific Hardening

### Development Environment

```erlang
{erlmcp, [
    {transports, [stdio]},  % No network
    {authentication, #{
        enabled => false
    }},
    {rate_limiting, #{
        enabled => false
    }},
    {validation, #{
        json_schema => true
    }}
]}.
```

### Staging Environment

```erlang
{erlmcp, [
    {transports, [tcp, http]},
    {tls, #{
        enabled => true,
        verify => verify_peer
    }},
    {authentication, #{
        enabled => true,
        methods => [api_key],
        api_keys => #{
            <<"staging_key">> => <<"staging_user">>
        }
    }},
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 100  % Relaxed for testing
    }}
]}.
```

### Production Environment

```erlang
{erlmcp, [
    {transports, [tcp, http, websocket]},
    {tls, #{
        enabled => true,
        verify => verify_peer,
        versions => ['tlsv1.2', 'tlsv1.3']
    }},
    {authentication, #{
        enabled => true,
        methods => [jwt, oauth2],
        jwt => #{
            required_issuer => {os_env, "JWT_ISSUER"},
            required_audience => {os_env, "JWT_AUDIENCE"}
        }
    }},
    {authorization, #{
        enabled => true
    }},
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 10,
        max_failures => 5
    }},
    {secrets, [
        {backend, vault}
    ]}
]}.
```

---

## Troubleshooting Security Issues

### Problem: Certificate Validation Fails

**Symptoms:**
```
[error] TLS handshake failed: {tls_alert,"certificate unknown"}
```

**Diagnosis:**
```erlang
%% Check certificate
ssl:peercert(Socket).

%% Verify certificate chain
public_key:pkix_path_validation(TrustStore, Cert, []).
```

**Solutions:**
1. Verify CA certificate is correct
2. Check certificate chain is complete
3. Ensure hostname matches CN/SAN
4. Verify certificate not expired

### Problem: JWT Validation Fails

**Symptoms:**
```
[warning] JWT signature verification failed
```

**Diagnosis:**
```erlang
%% Extract JWT header
Protected = jose_jws:peek_protected(Token).
Header = jsx:decode(Protected, [return_maps]).
io:format("Header: ~p~n", [Header]).

%% Check if key ID exists
KeyId = maps:get(<<"kid">>, Header),
ets:lookup(auth_jwt_keys, KeyId).
```

**Solutions:**
1. Verify public key is loaded
2. Check key ID matches
3. Ensure algorithm supported (ES256, RS256, etc.)
4. Verify token not expired

### Problem: Rate Limiting Too Aggressive

**Symptoms:**
```
[warning] Rate limit exceeded for client: ...
```

**Diagnosis:**
```erlang
%% Check client stats
{ok, Stats} = erlmcp_auth_rate_limiter:get_client_stats(ClientId).
io:format("Stats: ~p~n", [Stats]).

%% Reset client (admin)
erlmcp_auth_rate_limiter:reset_client(ClientId).
```

**Solutions:**
1. Adjust `max_attempts_per_second`
2. Increase `max_failures` threshold
3. Implement IP whitelisting for trusted clients
4. Use separate rate limiters per user type

---

## References

- **Main Security Plan:** `docs/v3/11_security_architecture_plan.md`
- **TLS Best Practices:** https://wiki.mozilla.org/Security/Server_Side_TLS
- **OWASP Cheat Sheets:** https://cheatsheetseries.owasp.org/
- **NIST Cybersecurity Framework:** https://www.nist.gov/cyberframework

---

**Document Status:** Production-ready
**Last Updated:** 2026-01-31
**Maintainer:** Security Team
