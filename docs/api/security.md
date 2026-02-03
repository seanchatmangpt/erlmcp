# erlmcp Security Documentation

## Overview

erlmcp provides multiple layers of security for MCP client-server communication. This document covers authentication, authorization, secrets management, transport security, and security best practices.

---

## Authentication Methods

### Token-Based Authentication

#### JWT (JSON Web Tokens)

```erlang
%% Server configuration
{erlmcp_core, [
    {auth, #{
        method => jwt,
        secret => <<"your-256-bit-secret">>,
        algorithm => hs256,
        token_ttl => 3600  %% 1 hour
    }}
]}
```

**Client Usage:**

```bash
# Generate JWT token
TOKEN=$(jwt encode --secret your-secret-key --alg HS256 '{"sub":"user123","exp":1735689600}')

# Send with request
curl -X POST http://localhost:8765/mcp \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

#### API Keys

```erlang
%% Server configuration with API key validation
{erlmcp_core, [
    {auth, #{
        method => api_key,
        header => <<"X-API-Key">>,
        keys => #{
            <<"client-1">> => #{permissions => [tools, resources]},
            <<"client-2">> => #{permissions => [tools]}
        }
    }}
]}
```

**Client Usage:**

```bash
curl -X POST http://localhost:8765/mcp \
  -H "X-API-Key: client-1" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

### Mutual TLS (mTLS)

mTLS provides the strongest security by requiring both client and server to present certificates.

#### Server Configuration

```erlang
%% Generate CA certificate
%% openssl genrsa -out ca.key 4096
%% openssl req -new -x509 -days 365 -key ca.key -out ca.crt

%% Generate server certificate
%% openssl genrsa -out server.key 4096
%% openssl req -new -key server.key -out server.csr
%% openssl x509 -req -days 365 -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt

%% Generate client certificate
%% openssl genrsa -out client.key 4096
%% openssl req -new -key client.key -out client.csr
%% openssl x509 -req -days 365 -in client.csr -CA ca.crt -CAkey ca.key -out client.crt

{erlmcp_core, [
    {auth, #{
        method => mtl s,
        cafile => "/path/to/ca.crt",
        certfile => "/path/to/server.crt",
        keyfile => "/path/to/server.key",
        verify => verify_peer,
        fail_if_no_peer_cert => true,
        depth => 3
    }}
]}
```

#### Client Configuration (cURL)

```bash
curl -X POST https://localhost:8765/mcp \
  --cert client.crt \
  --key client.key \
  --cacert ca.crt \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

### OAuth 2.0

```erlang
{erlmcp_core, [
    {auth, #{
        method => oauth2,
        issuer => <<"https://auth.example.com">>,
        audience => <<"erlmcp-api">>,
        scopes => [<<"mcp:tools">>, <<"mcp:resources">>],
        jwks_url => <<"https://auth.example.com/.well-known/jwks.json">>
    }}
]}
```

---

## Authorization

### Role-Based Access Control (RBAC)

```erlang
%% Define roles and permissions
{erlmcp_core, [
    {authorization, #{
        enabled => true,
        roles => #{
            admin => #{
                permissions => all
            },
            user => #{
                permissions => [
                    <<"tools:read">>,
                    <<"tools:execute:public">>,
                    <<"resources:read:public">>
                ]
            },
            readonly => #{
                permissions => [
                    <<"tools:read">>,
                    <<"resources:read">>
                ]
            }
        },
        default_role => readonly
    }}
]}
```

### Resource-Level Authorization

```erlang
%% Add resource with access control
erlmcp_server:add_resource(Server, #{
    uri => <<"mcp://admin/config">>,
    name => <<"Admin Config">>,
    handler => fun(Uri) -> get_admin_config(Uri) end,
    access => #{
        required_permission => <<"resources:read:admin">>,
        allowed_roles => [admin]
    }
}).
```

### Tool-Level Authorization

```erlang
%% Add tool with access control
erlmcp_server:add_tool(Server, #{
    name => <<"restart_server">>,
    description => <<"Restart the MCP server">>,
    handler => fun(Args) -> restart_server() end,
    access => #{
        required_permission => <<"tools:execute:admin">>,
        allowed_roles => [admin],
        audit_log => true
    }
}).
```

---

## Secrets Management

### Local Encrypted Storage

erlmcp provides AES-256-GCM encrypted local storage for secrets.

```erlang
%% Start secrets manager with local encrypted backend
{ok, Secrets} = erlmcp_secrets:start_link(#{
    backend => local_encrypted,
    storage_path => "priv/secrets.secrets.enc",
    encryption_key_path => "priv/secrets/master.key"
}).

%% Store a secret
ok = erlmcp_secrets:set_secret(<<"api_key">>, <<"sk-1234567890">>).

%% Retrieve a secret
{ok, Key} = erlmcp_secrets:get_secret(<<"api_key">>).

%% Inject secret into tool call
%% The secret is automatically masked in logs
```

### HashiCorp Vault Integration

```erlang
%% Configure Vault backend
{ok, Secrets} = erlmcp_secrets:start_link(#{
    backend => vault,
    vault_config => #{
        url => <<"https://vault.example.com:8200">>,
        auth_method => approle,
        role_id => os:getenv("VAULT_ROLE_ID"),
        secret_id => os:getenv("VAULT_SECRET_ID"),
        mount => <<"secret">>,
        namespace => <<"production">>
    }
}).

%% Secrets are automatically fetched from Vault
{ok, DbPassword} = erlmcp_secrets:get_secret(<<"database/password">>).
```

### AWS Secrets Manager Integration

```erlang
%% Configure AWS backend
{ok, Secrets} = erlmcp_secrets:start_link(#{
    backend => aws_secrets_manager,
    aws_config => #{
        region => <<"us-west-2">>,
        auth_method => iam_role
    }
}).

%% Retrieve secret from AWS
{ok, ApiKey} = erlmcp_secrets:get_secret(<<"prod/api-key">>).
```

### Secret Rotation

```erlang
%% Rotate a secret automatically
{ok, NewValue} = erlmcp_secrets:rotate_secret(<<"api_key">>).

%% Manual rotation with notification
{ok, NewValue} = erlmcp_secrets:rotate_secret(<<"db_password">>, #{
    notify_subscribers => true,
    ttl => 3600
}).
```

---

## Transport Security

### TLS Configuration

#### Minimum TLS Version

```erlang
{erlmcp_transports, [
    {http, [
        {enable_tls, true},
        {min_tls_version, 'tlsv1.3'},
        {ciphers, [
            %{'AES_256_GCM', 'SHA384'},
            %{'CHACHA20_POLY1305', 'SHA256'}
        ]},
        {honor_cipher_order, true},
        {secure_renegotiate, true}
    ]}
]}
```

#### Certificate Pinning

```erlang
{erlmcp_core, [
    {tls_pinning, #{
        enabled => true,
        pins => [
            %% SHA-256 hash of expected certificates
            <<"sha256/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=">>,
            <<"sha256/BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=">>
        ]
    }}
]}
```

### HTTP Security Headers

```erlang
{erlmcp_transports, [
    {http, [
        {security_headers, #{
            strict_transport_security => "max-age=31536000; includeSubDomains",
            content_security_policy => "default-src 'self'",
            x_frame_options => "DENY",
            x_content_type_options => "nosniff",
            x_xss_protection => "1; mode=block",
            referrer_policy => "no-referrer"
        }}
    ]}
]}
```

---

## Input Validation

### JSON Schema Validation

All tool inputs are validated against JSON Schema:

```erlang
%% Tool with strict input validation
erlmcp_server:add_tool(Server, #{
    name => <<"calculate">>,
    description => <<"Perform arithmetic">>,
    input_schema => #{
        type => object,
        properties => #{
            a => #{type => number, minimum => -1000, maximum => 1000},
            b => #{type => number, minimum => -1000, maximum => 1000},
            op => #{
                type => string,
                enum => [add, subtract, multiply, divide]
            }
        },
        required => [a, b, op],
        additionalProperties => false
    }
}).
```

### Content Type Validation

```erlang
%% Validate content types
{erlmcp_core, [
    {content_validation, #{
        enabled => true,
        max_text_length => 1048576,  %% 1MB
        max_binary_size => 10485760, %% 10MB
        allowed_mime_types => [
            <<"text/plain">>,
            <<"text/markdown">>,
            <<"application/json">>,
            <<"image/png">>,
            <<"image/jpeg">>
        ]
    }}
]}
```

### Prompt Injection Prevention

```erlang
{erlmcp_core, [
    {prompt_security, #{
        enabled => true,
        max_prompt_size => 1048576,  %% 1MB
        max_template_depth => 10,
        allowed_var_pattern => "^[a-zA-Z_][a-zA-Z0-9_]*$",
        blocked_patterns => [
            <<"<ignore">>,
            <<"SYSTEM:">>,
            <<"HUMAN:">>
        ]
    }}
]}
```

---

## Rate Limiting

### Per-Client Rate Limits

```erlang
{erlmcp_core, [
    {rate_limiting, #{
        enabled => true,
        storage => ets,
        limits => [
            #{
                key => {client, all},
                max_requests => 100,
                window_ms => 60000  %% 100 req/min
            },
            #{
                key => {client, tools},
                max_requests => 50,
                window_ms => 60000  %% 50 tool calls/min
            }
        ]
    }}
]}
```

### Token Bucket Rate Limiting

```erlang
%% Configure token bucket for smooth rate limiting
{ok, Limiter} = erlmcp_rate_limiter:start_link(#{
    mode => token_bucket,
    rate => 10,  %% tokens per second
    burst => 100, %% maximum burst size
    storage => ets
}).

%% Check rate limit
case erlmcp_rate_limiter:check(Limiter, ClientId) of
    {ok, State} -> process_request();
    {rate_limited, RetryAfter} -> send_rate_limit_error(RetryAfter)
end.
```

---

## Audit Logging

### Enable Audit Logging

```erlang
{erlmcp_observability, [
    {audit_log, #{
        enabled => true,
        backend => file,
        file_path => "/var/log/erlmcp/audit.log",
        format => json,
        events => [
            tool_execute,
            resource_read,
            authenticate,
            authorize_fail,
            security_event
        ],
        include => [
            timestamp,
            client_id,
            method,
            params_hash,  %% SHA-256 hash, not actual params
            result,
            error_code
        ]
    }}
]}
```

### OTEL Security Tracing

```erlang
%% Security events are traced with OTEL
erlmcp_otel:span(
    security_event,
    #{
        event => authenticate,
        client_id => <<"client-123">>,
        result => success,
        auth_method => jwt
    },
    #{}
).
```

---

## Security Checklist

### Deployment Checklist

- [ ] TLS enabled on all transports
- [ ] Strong cipher suites configured
- [ ] Certificate validation enabled
- [ ] Authentication required
- [ ] Authorization rules configured
- [ ] Secrets encrypted at rest
- [ ] Secrets not in logs
- [ ] Rate limiting enabled
- [ ] Input validation enabled
- [ ] Audit logging enabled
- [ ] Security headers configured
- [ ] CORS properly configured
- [ ] Dependencies updated
- [ ] Error messages don't leak sensitive data

### Development Checklist

- [ ] No hardcoded secrets
- [ ] Secrets in environment variables
- [ ] Input validation on all endpoints
- [ ] Output encoding to prevent XSS
- [ ] SQL injection prevention
- [ ] Command injection prevention
- [ ] Path traversal prevention
- [ ] Resource exhaustion prevention
- [ ] Timeout on all external calls
- [ ] Principle of least privilege

---

## Common Security Vulnerabilities

### Preventing Secret Leakage

```erlang
%% BAD: Logging raw arguments
logger:info("Tool called with args: ~p", [Arguments]).

%% GOOD: Log hash instead
logger:info("Tool called with args hash: ~p",
    [crypto:hash(sha256, term_to_binary(Arguments))]).

%% GOOD: Use structured logging with sanitization
logger:info("Tool called",
    #{tool => ToolName,
      args_sanitized => sanitize_args(Arguments)}).
```

### Preventing Timing Attacks

```erlang
%% Use constant-time comparison for secrets
secure_compare(<<>>, <<>>) -> true;
secure_compare(<<A, RestA/binary>>, <<B, RestB/binary>>) when A =:= B ->
    secure_compare(RestA, RestB);
secure_compare(Bin1, Bin2) when byte_size(Bin1) =/= byte_size(Bin2) ->
    false;
secure_compare(_, _) ->
    %% Add random delay to prevent timing attacks
    timer:sleep(rand:uniform(10)),
    false.
```

### Preventing DoS

```erlang
%% Limit message size
{erlmcp_transports, [
    {max_message_size, 16777216},  %% 16MB
    {max_concurrent_connections, 1000},
    {connection_timeout, 30000}
]}.
```

---

## Security Monitoring

### Real-time Security Alerts

```erlang
%% Set up security event handlers
erlmcp_security:subscribe(fun(Event) ->
    case Event of
        #{type := authorize_fail, client_id := ClientId} ->
            logger:warning("Authorization failed for ~p", [ClientId]),
            %% Trigger alert after 5 failed attempts
            case erlmcp_security:failed_attempts(ClientId) of
                N when N >= 5 -> block_client(ClientId);
                _ -> ok
            end;
        #{type := rate_limit_exceeded, client_id := ClientId} ->
            logger:alert("Rate limit exceeded for ~p", [ClientId]);
        _ -> ok
    end
end).
```

### Security Metrics

| Metric | Description | Threshold |
|--------|-------------|-----------|
| Failed Auth Rate | Failed authentication attempts | > 10/min |
| Failed Authz Rate | Failed authorization attempts | > 100/min |
| Rate Limit Hits | Rate limit triggered | > 50/min |
| Large Messages | Messages > 1MB | > 10/min |
| Unusual Patterns | Anomalous behavior | ML-based |

---

## Compliance

### SOC 2 Type II

erlmcp provides features for SOC 2 compliance:

1. **Access Control**: RBAC with audit logging
2. **Encryption**: AES-256-GCM for secrets, TLS for transport
3. **Change Management**: Git-tracked configuration
4. **Monitoring**: OTEL traces and metrics
5. **Incident Response**: Automated alerting

### GDPR Compliance

```erlang
%% Personal data handling
{erlmcp_core, [
    {gdpr, #{
        enabled => true,
        data_retention_days => 365,
        right_to_deletion => true,
        consent_tracking => true,
        data_portability => true
    }}
]}
```

### HIPAA Compliance

```erlang
%% PHI (Protected Health Information) handling
{erlmcp_core, [
    {hipaa, #{
        enabled => true,
        encryption => required,
        audit_trail => required,
        access_log => required,
        breach_notification => required
    }}
]}
```
