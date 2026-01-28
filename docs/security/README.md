# Security Documentation - erlmcp

## Overview

erlmcp provides comprehensive security features for authentication, authorization, secrets management, and audit logging. This document covers all security components and best practices.

## Security Components

### 1. Authentication (`erlmcp_auth`)

Multi-method authentication system with support for:

- **API Keys** - Simple token-based authentication
- **JWT** - JSON Web Token validation with expiration checking
- **OAuth2** - OAuth 2.0 token introspection (client credentials flow)
- **mTLS** - Mutual TLS certificate-based authentication

#### Usage

```erlang
% Start auth server
{ok, AuthPid} = erlmcp_auth:start_link(#{
    api_keys => #{
        <<"key_abc123">> => <<"user_alice">>
    },
    jwt_keys => #{
        <<"kid_1">> => PublicKey
    },
    oauth2 => #{
        enabled => true,
        introspection_url => <<"https://oauth.example.com/introspect">>
    },
    mtls => #{
        enabled => true,
        ca_cert_path => "/path/to/ca.pem"
    }
}).

% Authenticate with API key
{ok, SessionId} = erlmcp_auth:authenticate(api_key, #{
    api_key => <<"key_abc123">>
}).

% Authenticate with JWT
{ok, SessionId} = erlmcp_auth:authenticate(jwt, #{
    token => <<"eyJhbGci...header.payload.signature">>
}).

% Check permission
ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>).
```

### 2. Role-Based Access Control (RBAC)

Fine-grained permission system with roles and resource-level ACLs.

#### Default Roles

- **admin** - Full permissions (read, write, execute, delete)
- **user** - Read and write permissions
- **guest** - Read-only permissions

#### Custom Roles

```erlang
% Add custom role
ok = erlmcp_auth:add_role(<<"user_bob">>, <<"developer">>).

% Define role permissions
ok = erlmcp_auth:add_permission(<<"/api/debug">>, <<"execute">>, [<<"developer">>, <<"admin">>]).

% Check if user has permission
ok = erlmcp_auth:check_permission(SessionId, <<"/api/debug">>, <<"execute">>).
```

### 3. Secrets Management (`erlmcp_secrets`)

Secure storage and retrieval of sensitive data with multiple backend support.

#### Backends

- **HashiCorp Vault** - Enterprise-grade secret storage
- **AWS Secrets Manager** - Cloud-native secrets
- **Local Encrypted** - AES-256-GCM encrypted local storage (fallback)

#### Usage

```erlang
% Start secrets manager with Vault
{ok, SecretsPid} = erlmcp_secrets:start_link(#{
    backend => vault,
    backend_config => #{
        enabled => true,
        url => <<"https://vault.example.com">>,
        token => <<"s.vault_token_abc123">>
    },
    ttl_seconds => 300  % 5 minutes cache
}).

% Store secret
ok = erlmcp_secrets:set_secret(<<"database_password">>, <<"super_secret_123">>).

% Retrieve secret (cached for TTL)
{ok, Password} = erlmcp_secrets:get_secret(<<"database_password">>).

% Rotate secret
{ok, NewPassword} = erlmcp_secrets:rotate_secret(<<"database_password">>).

% Delete secret
ok = erlmcp_secrets:delete_secret(<<"database_password">>).
```

#### Local Encrypted Storage

When Vault/AWS are not available, secrets are stored locally with AES-256-GCM:

```erlang
{ok, SecretsPid} = erlmcp_secrets:start_link(#{
    backend => local_encrypted,
    storage_path => "priv/secrets/secrets.enc",
    encryption_key_path => "priv/secrets/master.key"
}).
```

**Security:** Master key is auto-generated with 600 permissions. Store in secure location.

### 4. Audit Logging (`erlmcp_audit_log`)

Tamper-proof audit trail with SHA-256 hash chains for compliance.

#### Features

- **Hash Chain** - Each entry links to previous hash (tamper detection)
- **Structured Logging** - JSON format with rich metadata
- **Export Formats** - JSON, CSV, Syslog (RFC 5424)
- **Compliance** - GDPR, SOC2, HIPAA compatible
- **Search** - Query logs by user, resource, time range

#### Usage

```erlang
% Start audit log
{ok, AuditPid} = erlmcp_audit_log:start_link(#{
    log_path => "priv/audit/audit.log",
    buffer_size => 100,
    flush_interval_ms => 5000
}).

% Log authentication success
ok = erlmcp_audit_log:log_auth_success(<<"user_alice">>, #{
    method => jwt,
    ip => <<"192.168.1.100">>
}).

% Log failed auth
ok = erlmcp_audit_log:log_auth_failure(<<"user_bob">>, #{
    method => api_key,
    reason => invalid_key,
    ip => <<"192.168.1.101">>
}).

% Log operation
ok = erlmcp_audit_log:log_operation(
    <<"user_alice">>,
    <<"/api/tools/calculator">>,
    <<"execute">>,
    #{input => <<"2+2">>, output => <<"4">>}
).

% Log sensitive operation (PII access, key rotation)
ok = erlmcp_audit_log:log_sensitive_operation(
    <<"user_alice">>,
    <<"pii_access">>,
    #{resource => <<"customer_data">>, reason => <<"support_ticket_123">>}
).

% Verify hash chain integrity
ok = erlmcp_audit_log:verify_chain().

% Export logs
ok = erlmcp_audit_log:export_logs(json, "audit_export.json").
ok = erlmcp_audit_log:export_logs(csv, "audit_export.csv").
ok = erlmcp_audit_log:export_logs(syslog, "audit_export.log").

% Query user logs
Now = erlang:system_time(microsecond),
Start = Now - (24 * 3600 * 1000000),  % 24 hours ago
{ok, Logs} = erlmcp_audit_log:get_user_logs(<<"user_alice">>, {Start, Now}).

% Search logs
{ok, SensitiveLogs} = erlmcp_audit_log:search_logs(#{
    <<"event_type">> => sensitive_op,
    <<"user_id">> => <<"user_alice">>
}).
```

#### Hash Chain Verification

```erlang
% Verify entire chain
ok = erlmcp_audit_log:verify_chain().

% Verify range
ok = erlmcp_audit_log:verify_chain(1000, 2000).

% Tamper detection
{error, {tampered, 1523}} = erlmcp_audit_log:verify_chain().
```

### 5. HTTP Security Headers (`erlmcp_security_headers`)

Automatic security headers for HTTP transports to prevent common web attacks.

#### Headers Added

- **X-Content-Type-Options**: nosniff (prevent MIME sniffing)
- **X-Frame-Options**: DENY (prevent clickjacking)
- **X-XSS-Protection**: 1; mode=block (legacy XSS protection)
- **Content-Security-Policy**: default-src 'self' (CSP)
- **Strict-Transport-Security**: max-age=31536000 (HSTS)
- **Referrer-Policy**: strict-origin-when-cross-origin
- **Permissions-Policy**: geolocation=(), camera=() (feature restrictions)
- **Cross-Origin-***: CORP, COEP, COOP policies

#### Usage

```erlang
% Configure globally
erlmcp_security_headers:configure(#{
    csp => <<"default-src 'self'; script-src 'self' 'unsafe-inline'">>,
    hsts => true,
    hsts_max_age => 31536000,
    frame_options => deny,
    referrer_policy => <<"strict-origin-when-cross-origin">>
}).

% Add to response headers
Headers = [{<<"content-type">>, <<"application/json">>}],
SecureHeaders = erlmcp_security_headers:add_headers(Headers).

% Get default headers
DefaultHeaders = erlmcp_security_headers:get_default_headers().

% Audit response headers
{ok, []} = erlmcp_security_headers:audit_headers(SecureHeaders).
{error, MissingHeaders} = erlmcp_security_headers:audit_headers(InsecureHeaders).

% Generate security report
Report = erlmcp_security_headers:security_report(Headers).
%% #{
%%   has_csp => true,
%%   has_hsts => true,
%%   has_frame_options => true,
%%   missing_headers => []
%% }
```

#### Cowboy Middleware Integration

```erlang
% In Cowboy configuration
{ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{
        env => #{dispatch => Dispatch},
        middlewares => [
            erlmcp_security_headers,  % Add security headers
            cowboy_router,
            cowboy_handler
        ]
    }
).
```

## Security Best Practices

### 1. Authentication

- **Always use HTTPS** - Encrypt all traffic with TLS 1.2+
- **Short session TTLs** - Default 1 hour, adjust based on risk
- **Token rotation** - Rotate tokens regularly (weekly/monthly)
- **Rate limiting** - Prevent brute force attacks
- **Multi-factor auth** - Consider adding MFA for sensitive operations

### 2. Authorization

- **Principle of least privilege** - Grant minimum required permissions
- **Regular audits** - Review role assignments quarterly
- **Resource-level ACLs** - Fine-grained control per endpoint
- **Deny by default** - Explicit allow lists, not deny lists

### 3. Secrets Management

- **Never hardcode secrets** - Use environment variables or secret stores
- **Rotate regularly** - Automate secret rotation (90 days max)
- **Encrypt at rest** - Use AES-256-GCM minimum
- **Audit access** - Log all secret retrievals
- **Separate secrets** - Different secrets per environment (dev/staging/prod)

### 4. Audit Logging

- **Log all auth events** - Success and failure
- **Log sensitive operations** - PII access, admin actions
- **Immutable logs** - Use hash chains, append-only storage
- **Retention policy** - Keep logs for compliance period (7 years GDPR)
- **Regular verification** - Verify hash chain integrity weekly
- **Secure storage** - Separate audit log storage from application

### 5. HTTP Security

- **HSTS preload** - Add domain to HSTS preload list
- **Strong CSP** - Restrictive Content-Security-Policy
- **HTTPS only** - Redirect HTTP to HTTPS
- **Security.txt** - Publish security contact info
- **Subresource Integrity** - SRI for external resources

## Compliance

### GDPR (EU General Data Protection Regulation)

- **Right to audit** - Tamper-proof audit logs with search
- **Data retention** - Configurable log retention policies
- **Right to erasure** - Delete user data from secrets/logs
- **Breach notification** - Audit logs provide evidence

### SOC2 (System and Organization Controls 2)

- **Access control** - RBAC with audit trail
- **Change management** - All changes logged
- **Monitoring** - Real-time audit logging
- **Data protection** - Encrypted secrets, secure transmission

### HIPAA (Health Insurance Portability and Accountability Act)

- **Access logging** - All PHI access audited
- **Encryption** - AES-256 for data at rest
- **Authentication** - Multi-method auth with MFA support
- **Integrity controls** - Hash chain verification

## Performance Considerations

### Authentication

- **Session caching** - Sessions stored in ETS (O(1) lookups)
- **API key cache** - Avoid DB lookups
- **JWT validation** - Cache public keys
- **Connection pooling** - Reuse OAuth2/Vault connections

### Secrets Management

- **TTL caching** - Default 5 minutes (configurable)
- **Batch operations** - Fetch multiple secrets at once
- **Async rotation** - Background secret rotation
- **Local fallback** - Encrypted local cache for offline mode

### Audit Logging

- **Buffered writes** - Batch log writes (default 100 entries)
- **Async I/O** - Non-blocking file writes
- **Compression** - Gzip old logs for storage
- **Partitioning** - Daily log files for fast queries

## Security Incidents

### Compromised API Key

1. Revoke token immediately: `erlmcp_auth:revoke_token(Key)`
2. Query audit logs: `erlmcp_audit_log:get_user_logs(UserId, TimeRange)`
3. Assess impact: Check accessed resources
4. Generate new key: Issue replacement
5. Notify user: Send security alert

### Tampered Audit Log

1. Verify chain: `erlmcp_audit_log:verify_chain()`
2. Identify tampered entry: Returns `{error, {tampered, Sequence}}`
3. Restore from backup: Use offsite backup
4. Investigate: Review file access logs
5. Harden storage: Move to append-only filesystem

### Secret Leak

1. Rotate immediately: `erlmcp_secrets:rotate_secret(Key)`
2. Check audit trail: Who accessed the secret?
3. Revoke sessions: Invalidate all sessions using the secret
4. Update applications: Deploy with new secret
5. Post-incident review: How did leak occur?

## Testing

### Unit Tests

```bash
# Run auth tests
rebar3 eunit --module=erlmcp_auth_tests

# Run audit log tests
rebar3 eunit --module=erlmcp_audit_log_tests

# All security tests
rebar3 eunit --dir=apps/erlmcp_core/test
rebar3 eunit --dir=apps/erlmcp_observability/test
```

### Integration Tests

```erlang
% Test full authentication flow
{ok, Session} = erlmcp_auth:authenticate(jwt, #{token => JWT}),
ok = erlmcp_auth:check_permission(Session, <<"/api/admin">>, <<"write">>),
ok = erlmcp_audit_log:log_operation(User, Resource, Action, #{}),
ok = erlmcp_audit_log:verify_chain().
```

### Security Scanning

```bash
# Check for hardcoded secrets
rebar3 as test do compile, dialyzer, xref

# Run security audit
make security-audit

# Verify dependencies
rebar3 tree | grep -i security
```

## References

- **JWT RFC 7519**: https://tools.ietf.org/html/rfc7519
- **OAuth2 RFC 6749**: https://tools.ietf.org/html/rfc6749
- **OWASP Security Headers**: https://owasp.org/www-project-secure-headers/
- **HashiCorp Vault**: https://www.vaultproject.io/docs
- **AWS Secrets Manager**: https://docs.aws.amazon.com/secretsmanager/

## Support

For security issues:
- Email: security@erlmcp.dev
- PGP Key: [Publish PGP key for encrypted reports]
- Responsible disclosure: 90-day disclosure timeline

For questions:
- GitHub Discussions: https://github.com/erlmcp/erlmcp/discussions
- Slack: #security channel
