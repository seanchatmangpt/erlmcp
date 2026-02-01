# Security Quick Reference - erlmcp

## TL;DR - Essential Security Commands

### Authentication

```erlang
% Start auth server with multiple methods
{ok, AuthPid} = erlmcp_auth:start_link(#{
    api_keys => #{<<"key_abc123">> => <<"user_alice">>},
    jwt_keys => #{<<"kid_1">> => PublicKey},
    oauth2 => #{enabled => true, introspection_url => <<"https://oauth.example.com/introspect">>},
    mtls => #{enabled => true, ca_cert_path => "/path/to/ca.pem"}
}).

% Authenticate with API key
{ok, SessionId} = erlmcp_auth:authenticate(api_key, #{api_key => <<"key_abc123">>}).

% Check permission
ok = erlmcp_auth:check_permission(SessionId, <<"/api/admin">>, <<"write">>).

% Revoke token
ok = erlmcp_auth:revoke_token(<<"key_abc123">>).
```

### Secrets Management

```erlang
% Start secrets manager with Vault
{ok, SecretsPid} = erlmcp_secrets:start_link(#{
    backend => vault,
    backend_config => #{
        address => "https://vault.example.com:8200",
        token => "s.vault_token_abc123",
        engine => "kv",
        mount => "secret"
    },
    ttl_seconds => 300  % 5 minutes cache
}).

% Store secret
ok = erlmcp_secrets:set_secret(<<"database/password">>, <<"SuperSecret123">>).

% Retrieve secret (cached)
{ok, Password} = erlmcp_secrets:get_secret(<<"database/password">>).

% Rotate secret
{ok, NewPassword} = erlmcp_secrets:rotate_secret(<<"database/password">>).

% Delete secret
ok = erlmcp_secrets:delete_secret(<<"database/password">>).
```

### Audit Logging

```erlang
% Start audit log with hash chain
{ok, AuditPid} = erlmcp_audit_log:start_link(#{
    log_path => "priv/audit/audit.log",
    hash_chain => true,
    digital_signature => #{
        enabled => true,
        algorithm => ecdsa,
        private_key_path => "/etc/erlmcp/keys/audit_private.pem"
    }
}).

% Log authentication success
ok = erlmcp_audit_log:log_auth_success(<<"user_alice">>, #{method => jwt, ip => <<"192.168.1.100">>}).

% Log operation
ok = erlmcp_audit_log:log_operation(<<"user_alice">>, <<"/api/tools/calculator">>, <<"execute">>, #{}).

% Verify chain integrity
ok = erlmcp_audit_log:verify_chain().

% Export logs
ok = erlmcp_audit_log:export_logs(json, "audit_export.json").

% Query user logs
{ok, Logs} = erlmcp_audit_log:get_user_logs(<<"user_alice">>, {StartTime, EndTime}).
```

### Rate Limiting

```erlang
% Configure multi-layer rate limiting
{erlmcp_rate_limiter, [
    {strategy, token_bucket},
    {ip_limits, #{rate => 100, period => 60000}},  % 100 req/min per IP
    {session_limits, #{rate => 1000, period => 60000}},  % 1000 req/min per session
    {user_limits, #{rate => 5000, period => 60000}},  % 5000 req/min per user
    {global_limits, #{rate => 100000, period => 60000}}  % 100K req/min global
]}.
```

### TLS Configuration

```erlang
% TCP transport with TLS 1.3
{erlmcp_transport_tcp, [
    {tls, #{
        enabled => true,
        certfile => "/etc/erlmcp/tls/server.crt",
        keyfile => "/etc/erlmcp/tls/server.key",
        cacertfile => "/etc/erlmcp/tls/ca.crt",
        verify => verify_peer,
        versions => ['tlsv1.3', 'tlsv1.2'],
        ciphers => [
            "TLS_AES_256_GCM_SHA384",
            "TLS_CHACHA20_POLY1305_SHA256",
            "TLS_AES_128_GCM_SHA256"
        ],
        hsts => true
    }}
]}.
```

## Security Checklist

### Deployment Security

- [ ] **TLS 1.3 enabled** on all transports
- [ ] **Strong cipher suites** only (AES-256-GCM, ChaCha20-Poly1305)
- [ ] **Certificate validation** enabled (verify_peer)
- [ ] **mTLS configured** for service-to-service communication
- [ ] **API keys rotated** (every 90 days maximum)
- [ ] **Secrets in Vault/AWS** (not hardcoded)
- [ ] **Audit logging enabled** with hash chains
- [ ] **Rate limiting enabled** (multi-layer)
- [ ] **Security headers configured** (CSP, HSTS, etc.)
- [ ] **Firewall rules** restrictive (allow-list only)

### Runtime Security

- [ ] **Session TTLs configured** (1 hour default)
- [ ] **Token expiration checks** enabled
- [ ] **Failed login monitoring** (>10/min = alert)
- [ ] **Secret access logging** all retrievals
- [ ] **Hash chain verification** every 5 minutes
- [ ] **Circuit breakers active** for cascading failures
- [ ] **Connection limits** enforced per IP
- [ ] **Input validation** on all endpoints
- [ ] **Output encoding** prevent XSS
- [ ] **SQL injection protection** (if using DB)

### Compliance Verification

- [ ] **GDPR right to access** working (user data export)
- [ ] **GDPR right to erasure** working (user deletion)
- [ ] **SOC2 audit trail** tamper-proof
- [ ] **HIPAA encryption** AES-256 minimum
- [ ] **Data retention** 7 years maximum
- [ ] **Breach detection** automated alerts
- [ ] **Incident response** procedures documented
- [ ] **Penetration testing** quarterly
- [ ] **Security training** annual for developers

## Common Security Patterns

### Pattern 1: Secure API Key Generation

```erlang
generate_secure_api_key() ->
    % 256-bit random key, base64-encoded
    Key = crypto:strong_rand_bytes(32),
    base64:encode(Key).
```

### Pattern 2: Session Validation Middleware

```erlang
validate_session(SessionId) ->
    case erlmcp_auth:validate_session(SessionId) of
        {ok, UserId} ->
            {ok, UserId};
        {error, session_expired} ->
            % Redirect to login
            {redirect, "/login"};
        {error, invalid_session} ->
            % Log potential attack
            erlmcp_audit_log:log_auth_failure(unknown, #{
                reason => invalid_session_id,
                session_id => SessionId
            }),
            {redirect, "/login"}
    end.
```

### Pattern 3: Permission Check Wrapper

```erlang
require_permission(SessionId, Resource, Action) ->
    case erlmcp_auth:check_permission(SessionId, Resource, Action) of
        ok ->
            % Permission granted, execute handler
            ok;
        {error, forbidden} ->
            % Log unauthorized access attempt
            {ok, UserId} = erlmcp_auth:get_user_from_session(SessionId),
            erlmcp_audit_log:log_permission_denied(UserId, Resource, Action),
            {error, forbidden}
    end.
```

### Pattern 4: Secret Retrieval with Fallback

```erlang
get_secret_with_fallback(Key) ->
    case erlmcp_secrets:get_secret(Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            % Try fallback
            case application:get_env(erlmcp, fallback_secrets) of
                {ok, FallbackMap} ->
                    case maps:get(Key, FallbackMap, undefined) of
                        undefined -> {error, not_found};
                        Value -> {ok, Value}
                    end;
                undefined -> {error, not_found}
            end
    end.
```

### Pattern 5: Audit All Critical Operations

```erlang
execute_critical_operation(UserId, OperationType, Fun) ->
    % Log before execution
    ok = erlmcp_audit_log:log_sensitive_operation(
        UserId,
        OperationType,
        #{status => initiated}
    ),

    try
        Result = Fun(),

        % Log success
        ok = erlmcp_audit_log:log_sensitive_operation(
            UserId,
            OperationType,
            #{status => success, result => Result}
        ),

        {ok, Result}
    catch
        Type:Reason:Stacktrace ->
            % Log failure
            ok = erlmcp_audit_log:log_sensitive_operation(
                UserId,
                OperationType,
                #{status => failed, error => Reason, type => Type}
            ),
            {error, {Type, Reason}}
    end.
```

## Security Metrics Dashboard

### Key Metrics to Monitor

| Metric | Formula | Alert Threshold |
|--------|---------|-----------------|
| Failed Login Rate | failed_logins / total_logins | > 5% |
| Unauthorized Access Attempts | count(permission_denied) | > 10/min |
| Secret Access Volume | count(secret_access) | Spike > 3x baseline |
| Hash Chain Verification | verify_chain() | Any failure |
| Certificate Expiry | cert_expiry_date | < 30 days |
| TLS Version | count(tls_1_2) / total | < 95% TLS 1.3 |
| Rate Limit Hits | count(429_responses) | > 100/min |
| Audit Log Size | log_file_size | > 10GB/day |

### Grafana Dashboard Queries

```promql
# Failed login rate (last 5 minutes)
rate(failed_logins_total[5m]) / rate(login_attempts_total[5m]) * 100

# Unauthorized access attempts
rate(permission_denied_total[1m])

# Secret access anomaly
abs(rate(secret_access_total[5m]) - avg_over_time(rate(secret_access_total[1h])[5m:5m])) / avg_over_time(rate(secret_access_total[1h])[5m:5m]) * 100

# Hash chain verification status
hash_chain_verification_status{status="failed"}

# Certificate expiry
cert_expiry_time_hours_remaining

# Rate limit hits
rate(rate_limit_exceeded_total[1m])
```

## Incident Response Commands

### Immediate Response

```erlang
% Block IP address
ok = erlmcp_rate_limiter:block_ip(<<"192.168.1.100">>, 3600000).  % 1 hour

% Revoke all user sessions
ok = erlmcp_auth:revoke_all_user_sessions(<<"user_alice">>).

% Rotate leaked secret
{ok, NewSecret} = erlmcp_secrets:rotate_secret(<<"leaked_secret">>).

% Verify audit log integrity
ok = erlmcp_audit_log:verify_chain().

% Export audit logs for investigation
ok = erlmcp_audit_log:export_logs(json, "incident_export.json", #{
    start_time => <<"2025-01-31T00:00:00Z">>,
    end_time => <<"2025-01-31T23:59:59Z">>
}).
```

### Investigation Queries

```erlang
% Get all failed login attempts in last hour
Now = erlang:system_time(microsecond),
HourAgo = Now - 3600000000,
{ok, FailedLogins} = erlmcp_audit_log:search_logs(#{
    event_type => auth_failure,
    start_time => HourAgo,
    end_time => Now
}).

% Get all secret access by user
{ok, SecretAccess} = erlmcp_audit_log:get_secret_access_logs(
    <<"user_alice">>,
    {StartTime, EndTime}
).

% Check for unusual patterns (e.g., 100+ failures from same IP)
{ok, SuspiciousIPs} = erlmcp_audit_log:aggregate_logs(#{
    group_by => [ip_address],
    event_type => auth_failure,
    aggregation => count,
    having => #{count => {gt, 100}}
}).
```

## Configuration Templates

### Development Environment

```erlang
%% config/dev.config
{erlmcp_auth, [
    {session_ttl, 3600},  % 1 hour
    {api_keys => #{
        <<"dev_key_123">> => <<"developer">>
    }}
]}.

{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key, {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path => "priv/secrets/dev.enc"},
    {ttl_seconds, 60}  % Short TTL for dev
]}.

{erlmcp_audit_log, [
    {log_path => "priv/audit/dev-audit.log"},
    {hash_chain, true},
    {digital_signature, #{enabled => false}}  % Disable signing in dev
]}.

{erlmcp_rate_limiter, [
    {strategy, token_bucket},
    {ip_limits, #{rate => 1000, period => 60000}}  % Relaxed limits
]}.
```

### Production Environment

```erlang
%% config/prod.config
{erlmcp_auth, [
    {session_ttl, 3600},  % 1 hour
    {mtls, #{
        enabled => true,
        ca_cert_path => "/etc/erlmcp/tls/ca.crt"
    }}
]}.

{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault.prod.example.com:8200",
        auth_method => approle,
        role_id => {env_var, "VAULT_ROLE_ID"},
        secret_id => {env_var, "VAULT_SECRET_ID"},
        engine => "kv-v2",
        mount => "secret"
    }},
    {ttl_seconds, 600},  % 10 minutes
    {audit_enabled, true}
]}.

{erlmcp_audit_log, [
    {log_path => "/var/log/erlmcp/audit.log"},
    {hash_chain, true},
    {digital_signature, #{
        enabled => true,
        algorithm => ecdsa,
        private_key_path => "/etc/erlmcp/keys/audit_private.pem",
        sign_interval => 60000
    }},
    {retention_days, 2555}  % 7 years (GDPR)
]}.

{erlmcp_rate_limiter, [
    {strategy, token_bucket},
    {ip_limits, #{rate => 100, period => 60000, block_duration => 300000}},
    {session_limits, #{rate => 1000, period => 60000}},
    {user_limits, #{rate => 5000, period => 60000}},
    {global_limits, #{rate => 100000, period => 60000}}
]}.

{erlmcp_transport_tcp, [
    {tls, #{
        enabled => true,
        certfile => "/etc/erlmcp/tls/server.crt",
        keyfile => "/etc/erlmcp/tls/server.key",
        cacertfile => "/etc/erlmcp/tls/ca.crt",
        verify => verify_peer,
        versions => ['tlsv1.3', 'tlsv1.2'],
        hsts => true
    }}
]}.
```

## Common Security Issues & Solutions

### Issue 1: "Too many failed logins"

**Symptom**: Spike in auth_failure events

**Solution**:
```erlang
% Check rate limiting
erlmcp_rate_limiter:block_ip(AttackerIP, 3600000).

% Check for compromised user
{ok, Logs} = erlmcp_audit_log:get_user_logs(UserId, {StartTime, Now}),

% If brute force detected
ok = erlmcp_auth:lock_user_account(UserId, 3600000).  % Lock for 1 hour
```

### Issue 2: "Hash chain verification failed"

**Symptom**: `{error, {tampered, Sequence}}`

**Solution**:
```erlang
% Immediate halt
halt_system_for_investigation(),

% Preserve evidence
file:copy("priv/audit/current.log", "priv/audit/evidence_" ++ iso8601_now() ++ ".log"),

% Restore from backup
{ok, Backup} = read_backup_from_s3(),
ok = file:write_file("priv/audit/current.log", Backup),

% Verify restored chain
ok = erlmcp_audit_log:verify_chain().
```

### Issue 3: "Certificate expiring soon"

**Symptom**: Certificate expires in < 30 days

**Solution**:
```erlang
% Check certificate expiry
{ok, Cert} = file:read_file("/etc/erlmcp/tls/server.crt"),
NotAfter = extract_expiry_date(Cert),
DaysRemaining = calculate_days_until(NotAfter),

if
    DaysRemaining < 30 ->
        % Alert security team
        security_alert:send(cert_expiry_warning, #{
            certificate => server,
            days_remaining => DaysRemaining
        });
    true -> ok
end.
```

### Issue 4: "High secret access volume"

**Symptom**: Unusual spike in secret_access events

**Solution**:
```erlang
% Check who accessed secrets
{ok, AccessLogs} = erlmcp_audit_log:get_secret_access_logs(SecretKey, {StartTime, Now}),

% Identify patterns
UsersAccessing = lists:usum([maps:get(<<"user_id">>, L) || L <- AccessLogs]),

% If unauthorized access detected
lists:foreach(fun(UserId) ->
    case is_authorized(UserId, SecretKey) of
        false -> erlmcp_auth:revoke_all_user_sessions(UserId);
        true -> ok
    end
end, UsersAccessing).

% Rotate secret
{ok, NewSecret} = erlmcp_secrets:rotate_secret(SecretKey).
```

## References

- **Full Security Documentation**: `/Users/sac/erlmcp/docs/security/README.md`
- **Transport Security**: `/Users/sac/erlmcp/docs/security/transport-security.md`
- **Secrets Management**: `/Users/sac/erlmcp/docs/secrets/README.md`
- **Audit Logging**: `/Users/sac/erlmcp/docs/security/audit-logging.md`
- **Configuration Guide**: `/Users/sac/erlmcp/docs/SECRETS_MANAGEMENT.md`
