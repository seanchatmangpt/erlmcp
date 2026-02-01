# erlmcp v3 OSS Release - Security Architecture Plan

**Version:** 3.0.0-oss
**Status:** Draft
**Author:** Erlang Architect Agent
**Date:** 2026-01-31

---

## Executive Summary

This document defines the security architecture for erlmcp v3 OSS release, establishing **secure-by-default** principles while maintaining flexibility for diverse deployment scenarios. The architecture balances security best practices with OSS usability, providing clear guidance on mandatory vs optional security features.

**Core Security Posture:**
- **Authentication:** Multi-method support (API key, JWT, OAuth2, mTLS) - disabled by default
- **TLS:** Mandatory for network transports (TCP, HTTP, WebSocket)
- **Input Validation:** Always enabled (JSON Schema)
- **Rate Limiting:** Optional, disabled by default
- **Authorization:** Role-based (RBAC) - disabled by default
- **Secrets Management:** Environment-based, no hardcoded secrets

---

## Table of Contents

1. [Security Principles](#security-principles)
2. [Security Features Matrix](#security-features-matrix)
3. [Secure Defaults](#secure-defaults)
4. [TLS Configuration](#tls-configuration)
5. [Authentication Options](#authentication-options)
6. [Authorization Model](#authorization-model)
7. [Rate Limiting Strategy](#rate-limiting-strategy)
8. [Input Validation](#input-validation)
9. [Secrets Management](#secrets-management)
10. [Transport Security](#transport-security)
11. [Security Hardening Guide](#security-hardening-guide)
12. [OSS Deployment Security Checklist](#oss-deployment-security-checklist)

---

## Security Principles

### 1. Secure by Default (OSS Philosophy)

**Principle:** Default configuration should be secure for development and production use.

**Implementation:**
- TLS enabled for all network transports
- Input validation always active
- No hardcoded secrets or credentials
- Safe cipher suites only (TLS 1.2+)
- Authentication features available but opt-in (respect OSS usability)

### 2. Fail Securely

**Principle:** Security controls fail to a safe state.

**Implementation:**
- Certificate validation failures → connection rejected
- JWT validation failures → 401 Unauthorized
- Rate limit exceeded → 429 Too Many Requests
- Invalid input → 400 Bad Request (never silently accepted)

### 3. Defense in Depth

**Principle:** Multiple layered security controls.

**Implementation:**
- Network: TLS + certificate validation
- Application: Auth + authorization + rate limiting
- Input: JSON Schema validation + size limits + URI sanitization
- Infrastructure: Process isolation via supervision trees

### 4. Minimal Attack Surface

**Principle:** Only expose necessary functionality.

**Implementation:**
- Transport polymorphism - enable only required transports
- Disabled by default: auth, rate limiting, mTLS (opt-in security)
- No unnecessary debugging in production builds
- Security headers on all HTTP responses

---

## Security Features Matrix

### Categorization: MANDATORY vs OPTIONAL

| Feature | Category | Default | Rationale |
|---------|----------|---------|-----------|
| **TLS (TCP/HTTP/WS)** | MANDATORY | Enabled | Network security baseline |
| **JSON Schema Validation** | MANDATORY | Enabled | Input integrity (MCP spec) |
| **Message Size Limits** | MANDATORY | Enabled | DoS protection (10MB default) |
| **Security Headers (HTTP)** | MANDATORY | Enabled | Web security baseline |
| **Origin Validation** | MANDATORY | Enabled | DNS rebinding protection |
| **URI Sanitization** | MANDATORY | Enabled | Path traversal prevention |
| **API Key Auth** | OPTIONAL | Disabled | Simple auth for OSS users |
| **JWT Auth** | OPTIONAL | Disabled | Standard token-based auth |
| **OAuth2 Introspection** | OPTIONAL | Disabled | Enterprise SSO integration |
| **mTLS** | OPTIONAL | Disabled | Mutual TLS (zero trust) |
| **Rate Limiting** | OPTIONAL | Disabled | Brute force protection |
| **RBAC Authorization** | OPTIONAL | Disabled | Fine-grained permissions |
| **Secret Encryption at Rest** | OPTIONAL | Disabled | Requires Vault/external |
| **Session Persistence** | OPTIONAL | ETS (in-memory) | Durability vs performance choice |

---

## Secure Defaults

### Default Configuration (OSS Release)

```erlang
{erlmcp, [
    %% Transport defaults
    {transports, [stdio]},  % Local development: stdio only (no network)

    %% TLS configuration (for network transports)
    {tls, #{
        enabled => true,              % MANDATORY for TCP/HTTP/WS
        verify => verify_peer,        % MANDATORY
        versions => ['tlsv1.2', 'tlsv1.3'],
        ciphers => [
            "TLS_AES_128_GCM_SHA256",
            "TLS_AES_256_GCM_SHA384",
            "TLS_CHACHA20_POLY1305_SHA256",
            "ECDHE-ECDSA-AES128-GCM-SHA256",
            "ECDHE-RSA-AES128-GCM-SHA256"
        ],
        depth => 5,
        secure_renegotiate => true
    ]},

    %% Security features (all opt-in for OSS)
    {authentication, #{
        enabled => false,             % DISABLED by default
        methods => []                 % No auth methods configured
    }},

    {authorization, #{
        enabled => false              % DISABLED by default
    }},

    {rate_limiting, #{
        enabled => false              % DISABLED by default
    }},

    %% Input validation (always enabled)
    {validation, #{
        json_schema => true,          % MANDATORY
        max_message_size => 10485760, % 10MB (MCP spec)
        uri_validation => true        % MANDATORY
    }},

    %% Security headers (HTTP)
    {security_headers, #{
        enabled => true,              % MANDATORY for HTTP
        content_security_policy => "default-src 'self'",
        strict_transport_security => "max-age=31536000; includeSubDomains",
        x_frame_options => "DENY",
        x_content_type_options => "nosniff"
    }}
]}.
```

### Production Configuration (Recommended)

```erlang
{erlmcp, [
    %% Enable network transports with TLS
    {transports, [tcp, http, websocket]},

    %% Authentication (ENABLED for production)
    {authentication, #{
        enabled => true,
        methods => [jwt, api_key],     % At least one method
        jwt => #{
            required_issuer => <<"https://your-auth.com">>,
            required_audience => <<"erlmcp">>,
            public_keys => #{
                <<"key1">> => <<"PEM_ENCODED_PUBLIC_KEY">>
            }
        },
        api_keys => #{
            % Loaded from environment, never hardcoded
            {os_env, "ERLMCP_API_KEYS"}
        }
    }},

    %% Authorization (ENABLED for production)
    {authorization, #{
        enabled => true,
        default_role => <<"guest">>,
        role_permissions => #{
            <<"guest">> => [<<"read">>],
            <<"user">> => [<<"read">>, <<"write">>],
            <<"admin">> => [<<"read">>, <<"write">>, <<"execute">>, <<"delete">>]
        }
    }},

    %% Rate limiting (ENABLED for production)
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 10,
        max_failures => 5,
        block_duration_ms => 300000  % 5 minutes
    }}
]}.
```

---

## TLS Configuration

### Security Requirements

**Mandatory for:** TCP, HTTP, WebSocket transports

**Versions:**
- **Minimum:** TLS 1.2
- **Recommended:** TLS 1.3

**Cipher Suites:**

**TLS 1.3 (Preferred):**
```
TLS_AES_128_GCM_SHA256
TLS_AES_256_GCM_SHA384
TLS_CHACHA20_POLY1305_SHA256
```

**TLS 1.2 (Forward Secrecy Only):**
```
ECDHE-ECDSA-AES128-GCM-SHA256
ECDHE-RSA-AES128-GCM-SHA256
ECDHE-ECDSA-AES256-GCM-SHA384
ECDHE-RSA-AES256-GCM-SHA384
ECDHE-ECDSA-CHACHA20-POLY1305
ECDHE-RSA-CHACHA20-POLY1305
```

**Explicitly Forbidden:**
- SSLv3, TLS 1.0, TLS 1.1 (legacy)
- RC4, DES, 3DES, MD5 (weak ciphers)
- CBC mode without authenticated encryption
- Anonymous cipher suites (no authentication)

### Certificate Validation

**Mode:** `verify_peer` (MANDATORY for production)

**Client (Outbound Connections):**
```erlang
{tls, [
    {verify, verify_peer},
    {cacertfile, "/path/to/ca.crt"},
    {server_name_indication, "example.com"},  % SNI
    {depth, 5},
    {secure_renegotiate, true}
]}
```

**Server (Inbound Connections):**
```erlang
{tls, [
    {verify, verify_peer},
    {certfile, "/path/to/server.crt"},
    {keyfile, "/path/to/server.key"},
    {cacertfile, "/path/to/ca.crt"},
    {fail_if_no_peer_cert, true},  % For mTLS
    {depth, 5}
]}
```

### Module: `erlmcp_tls_validation`

**Features:**
- Secure default TLS options
- Client and server TLS option builders
- Certificate validation helpers
- Configuration validation

**Usage:**
```erlang
%% Build secure TLS options
{ok, ClientOpts} = erlmcp_tls_validation:build_tls_options(client, #{
    certfile => "/path/to/client.crt",
    keyfile => "/path/to/client.key",
    cacertfile => "/path/to/ca.crt",
    server_name_indication => "example.com"
}).
```

---

## Authentication Options

### Supported Methods (All Optional)

| Method | Use Case | Security Level | Status |
|--------|----------|----------------|--------|
| **API Key** | Simple service-to-service | Low-Medium | Implemented |
| **JWT** | Standard token-based auth | High | Implemented |
| **OAuth2** | Enterprise SSO integration | High | Implemented |
| **mTLS** | Zero-trust mutual authentication | Very High | Stub (needs completion) |

### 1. API Key Authentication

**Configuration:**
```erlang
{authentication, #{
    methods => [api_key],
    api_keys => #{
        %% NEVER hardcode in production - use environment
        <<"sk_test_12345">> => <<"service_user_1">>,
        {os_env, "ERLMCP_API_KEY"} => <<"production_user">>
    }
}}
```

**Implementation:** `erlmcp_auth.erl`

**Security Notes:**
- Keys stored in ETS table (protected)
- Rate limiting recommended
- Key rotation supported
- Audit logging available

### 2. JWT Authentication

**Configuration:**
```erlang
{authentication, #{
    methods => [jwt],
    jwt => #{
        required_issuer => <<"https://auth.example.com">>,
        required_audience => <<"erlmcp-production">>,
        public_keys => #{
            <<"key1">> => <<"PEM_ENCODED_PUBLIC_KEY">>,
            <<"key2">> => <<"ANOTHER_PUBLIC_KEY">>
        },
        clock_skew_seconds => 30
    }
}}
```

**Validation:**
- Cryptographic signature verification (jose library)
- Expiration (`exp`) claim - MANDATORY
- Not before (`nbf`) claim - checked if present
- Issuer (`iss`) claim - validated if required
- Audience (`aud`) claim - validated if required
- Subject (`sub`) claim - MANDATORY

**Revocation:** Token blacklist in ETS with TTL

### 3. OAuth2 Introspection (RFC 7662)

**Configuration:**
```erlang
{authentication, #{
    methods => [oauth2],
    oauth2 => #{
        enabled => true,
        introspection_url => <<"https://auth.example.com/introspect">>,
        client_id => <<"erlmcp-client">>,
        client_secret => {os_env, "OAUTH2_CLIENT_SECRET"},
        timeout_ms => 5000,
        cache_ttl_seconds => 300
    }
}}
```

**Flow:**
1. Client presents OAuth2 access token
2. erlmcp calls introspection endpoint (RFC 7662)
3. Validates `active` claim
4. Caches result for 5 minutes

**Implementation:** `erlmcp_auth.erl` (gun HTTP client)

### 4. mTLS Authentication (Stub - Needs Completion)

**Current Status:** Stub implementation exists

**Required for Production:**
```erlang
%% TODO: Complete mTLS implementation
-module(erlmcp_auth_mtls).

%% Required functions (stubs):
validate(CertInfo, Config) ->
    %% Extract certificate from socket
    %% Validate certificate chain
    %% Check expiration
    %% Extract CN or SAN for user_id
    %% Check revocation (OCSP/CRL)
    %% Return {ok, User} or {error, Reason}
```

**Recommendation:** Prioritize mTLS completion for zero-trust deployments

---

## Authorization Model

### Role-Based Access Control (RBAC)

**Implementation:** `erlmcp_auth.erl`

**Default Roles:**
| Role | Permissions |
|------|-------------|
| `guest` | `read` |
| `user` | `read`, `write` |
| `admin` | `read`, `write`, `execute`, `delete` |

**Configuration:**
```erlang
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
    },
    resource_permissions => #{
        {<<"tools">>, <<"execute">>} => [<<"user">>, <<"admin">>],
        {<<"resources">>, <<"delete">>} => [<<"admin">>]
    }
}}
```

**Permission Check:**
```erlang
%% API
case erlmcp_auth:check_permission(SessionId, Resource, Permission) of
    ok -> granted;
    {error, forbidden} -> denied
end
```

---

## Rate Limiting Strategy

### Module: `erlmcp_auth_rate_limiter`

**Algorithm:** Sliding window (1-second window)

**Configuration:**
```erlang
{rate_limiting, #{
    enabled => true,
    max_attempts_per_second => 10,
    max_failures => 5,
    block_duration_ms => 300000,      % 5 minutes
    backoff_levels => [0, 1000, 2000, 4000, 8000, 16000],
    cleanup_interval_ms => 60000      % 1 minute
}}
```

**Behavior:**
1. **Rate Limit:** 10 requests/second per client
2. **Exponential Backoff:** After 5 failures
   - Failure 5-9: 1s block
   - Failure 10-14: 2s block
   - Failure 15-19: 4s block
   - Failure 20-24: 8s block
   - Failure 25-29: 16s block
   - Failure 30+: Permanent 5-minute block
3. **IP Blocking:** Client IP also blocked on excessive failures
4. **Reset:** Successful auth resets backoff level

**Recommendation:** Enable for production deployments

---

## Input Validation

### JSON Schema Validation (MANDATORY)

**Library:** jesse (JSON Schema validator)

**MCP Schema:** `priv/schema/mcp.json`

**Configuration:**
```erlang
{validation, #{
    json_schema => true,              % MANDATORY - cannot disable
    max_message_size => 10485760,      % 10MB (MCP spec limit)
    uri_validation => true,            % MANDATORY - path traversal prevention
    xss_prevention => true,            % For HTML-rendered content
    sql_injection_prevention => true   % No SQL in erlmcp, but best practice
}}
```

### Module: `erlmcp_uri_validator`

**Validates:**
- URI structure (RFC 3986)
- Path traversal (`../`) - REJECTED
- Null bytes - REJECTED
- Excessive length (>4096 chars) - REJECTED

---

## Secrets Management

### Principles

1. **No Hardcoded Secrets** - Enforced by security validator
2. **Environment-Based** - Load from `os:getenv/1`
3. **Optional Encryption at Rest** - Vault integration available

### Configuration

```erlang
{secrets, [
    {backend, local_encrypted},  % Default: env vars
    {encryption_key, {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path, "priv/secrets/secrets.enc"},
    {ttl_seconds => 300}
]}.

%% Alternative: HashiCorp Vault
{secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault:8200",
        auth_method => approle,
        role_id => {env_var, "VAULT_ROLE_ID"},
        secret_id => {env_var, "VAULT_SECRET_ID"},
        engine => "kv",
        mount => "secret"
    }}
]}.
```

### Module: `erlmcp_secrets`

**Features:**
- Multiple backends (env, Vault, AWS Secrets Manager)
- TTL-based caching (5 minutes default)
- Automatic key rotation support

---

## Transport Security

### STDIO (Local Development)

**Security:** Process-level isolation only
**Use Case:** Local development, testing
**Network Exposure:** None
**Auth Required:** No (local only)

### TCP

**TLS:** MANDATORY for non-localhost
**Default:** TLS enabled
**Config:**
```erlang
{transports, [
    {tcp, #{
        port => 9000,
        tls => true,
        tls_opts => #{
            certfile => "/path/to/server.crt",
            keyfile => "/path/to/server.key",
            cacertfile => "/path/to/ca.crt",
            verify => verify_peer
        }
    }}
]}
```

### HTTP

**TLS:** MANDATORY
**Security Headers:** MANDATORY
**Config:**
```erlang
{transports, [
    {http, #{
        port => 8080,
        tls => true,
        security_headers => true,
        origin_validation => true
    }}
]}
```

### WebSocket

**TLS:** MANDATORY (WSS only)
**Origin Check:** MANDATORY
**Config:**
```erlang
{transports, [
    {websocket, #{
        port => 9090,
        tls => true,
        allowed_origins => [
            <<"https://example.com">>,
            <<"https://*.example.com">>
        ]
    }}
]}
```

---

## Security Hardening Guide

### Phase 1: Baseline (All Deployments)

**Mandatory Steps:**
1. Enable TLS for all network transports
2. Configure certificate validation (`verify_peer`)
3. Set max message size (10MB recommended)
4. Enable security headers (HTTP)
5. Enable origin validation

**Validation:**
```bash
erlmcp_validate_cli run security
```

### Phase 2: Authentication (Production)

**Recommended Steps:**
1. Choose authentication method (JWT recommended)
2. Configure JWT public keys
3. Set issuer/audience validation
4. Enable rate limiting
5. Configure RBAC roles

**Validation:**
```erlang
%% Test authentication
{ok, SessionId} = erlmcp_auth:authenticate(jwt, #{
    token => <<"eyJhbGciOi...">>
}).
```

### Phase 3: Hardening (High Security)

**Advanced Steps:**
1. Enable mTLS (complete stub implementation)
2. Configure secrets backend (Vault)
3. Enable audit logging
4. Set up monitoring/alerting
5. Implement intrusion detection

### Phase 4: Compliance (Regulated Industries)

**Additional Controls:**
1. Enable session persistence (Mnesia)
2. Configure immutable audit logs
3. Implement key rotation policy
4. Set up SIEM integration
5. Conduct regular security audits

---

## OSS Deployment Security Checklist

### Pre-Deployment

- [ ] TLS certificates obtained and configured
- [ ] Cipher suites verified (TLS 1.2+ only)
- [ ] Certificate validation enabled (`verify_peer`)
- [ ] Security headers configured
- [ ] Message size limits set
- [ ] Input validation enabled (JSON Schema)
- [ ] Origin validation configured

### Authentication Setup

- [ ] Authentication method chosen (JWT/API key/OAuth2)
- [ ] Public keys configured (JWT) or API keys loaded
- [ ] Rate limiting enabled and configured
- [ ] RBAC roles defined (if authorization enabled)
- [ ] Session TTL configured

### Secrets Management

- [ ] No hardcoded secrets in source code
- [ ] Secrets loaded from environment
- [ ] Encryption at rest configured (optional)
- [ ] Key rotation policy defined

### Monitoring

- [ ] Security event logging enabled
- [ ] Failed authentication alerts configured
- [ ] Rate limit breach monitoring
- [ ] Certificate expiration alerts
- [ ] Intrusion detection setup

### Testing

- [ ] Security validator passes all checks
- [ ] Penetration testing completed
- [ ] Authentication flow tested
- [ ] TLS configuration verified
- [ ] Rate limiting tested
- [ ] Input validation tested

### Documentation

- [ ] Security architecture documented
- [ ] Incident response plan created
- [ ] Security contact information available
- [ ] Vulnerability disclosure process defined

---

## Appendix: Security Module Reference

### Core Security Modules

| Module | Purpose | Status |
|--------|---------|--------|
| `erlmcp_auth` | Authentication/authorization gen_server | Production |
| `erlmcp_auth_mtls` | mTLS certificate validation | Stub (needs completion) |
| `erlmcp_auth_rate_limiter` | Rate limiting with backoff | Production |
| `erlmcp_tls_validation` | TLS configuration builder | Production |
| `erlmcp_security_headers` | HTTP security headers middleware | Production |
| `erlmcp_origin_validator` | CORS/origin validation | Production |
| `erlmcp_uri_validator` | URI sanitization | Production |
| `erlmcp_secrets` | Secrets management backend | Production |
| `erlmcp_security_validator` | Security compliance validation | Production |

### Security Checks (22 Total)

**Authentication (4):**
- Auth mechanism available
- Token handling secure
- Session management
- Authorization checks

**Input Validation (5):**
- JSON schema validation
- Parameter sanitization
- SQL injection prevention
- XSS prevention
- Path traversal prevention

**Secret Management (4):**
- No hardcoded secrets
- Environment variable usage
- Secret encryption at rest
- Key rotation

**JWT (4):**
- JWT structure valid
- JWT signature verified
- JWT validation enabled
- JWT expiration checked

**Rate Limiting (3):**
- Rate limiting configured
- Rate limits enforced
- Rate limit bypass prevention

**CORS (2):**
- CORS headers configured
- Origin validation enabled

---

## References

- **MCP Specification:** https://modelcontextprotocol.io
- **TLS Best Practices:** https://wiki.mozilla.org/Security/Server_Side_TLS
- **OAuth2 RFC 7662:** https://datatracker.ietf.org/doc/html/rfc7662
- **OWASP ASVS:** https://owasp.org/www-project-application-security-verification-standard
- **JWT Best Practices:** https://tools.ietf.org/html/rfc8725

---

**Document Status:** Draft for erlmcp v3 OSS release
**Next Review:** Pre-release security audit
**Maintainer:** Erlang Architect Agent
