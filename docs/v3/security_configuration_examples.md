# erlmcp v3 Security Configuration Examples

**Quick reference for common security scenarios**

---

## Table of Contents

1. [Local Development](#local-development)
2. [Internal Service](#internal-service)
3. [Public API (Basic Auth)](#public-api-basic-auth)
4. [Enterprise SSO]((#enterprise-sso)
5. [Zero Trust mTLS](#zero-trust-mtls)
6. [High-Security Compliance](#high-security-compliance)

---

## Local Development

**Use Case:** Local development, no network exposure

**Security Profile:** Minimal (process isolation only)

```erlang
%% config/dev.config
{erlmcp, [
    %% STDIO only - no network
    {transports, [stdio]},

    %% No authentication needed (local only)
    {authentication, #{
        enabled => false
    }},

    %% Input validation still active
    {validation, #{
        json_schema => true,
        max_message_size => 10485760
    }}
]}.
```

**Commands:**
```bash
# Start erlmcp
erl -pa _build/default/lib/*/ebin -s erlmcp

# Connect via STDIO
echo '{"jsonrpc":"2.0":"method":"ping","id":1}' | erlmcp
```

---

## Internal Service

**Use Case:** Internal microservice, trusted network

**Security Profile:** Network-level security + basic auth

```erlang
%% config/internal.config
{erlmcp, [
    %% TCP transport within VPC
    {transports, [
        {tcp, #{
            port => 9000,
            listen_opts => [
                {ip, {10,0,0,1}},  % Internal IP only
                {backlog, 1024}
            ]
        }}
    ]},

    %% TLS (self-signed cert acceptable internally)
    {tls, #{
        enabled => true,
        verify => verify_peer,  % Still verify certs
        certfile => "/etc/erlmcp/internal/server.crt",
        keyfile => "/etc/erlmcp/internal/server.key",
        cacertfile => "/etc/erlmcp/internal/ca.crt",
        server_name_indication => disable
    }},

    %% Simple API key authentication
    {authentication, #{
        enabled => true,
        methods => [api_key],
        api_keys => #{
            {os_env, "ERLMCP_INTERNAL_KEY"} => <<"internal_service">>
        }
    }},

    %% Minimal rate limiting
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 1000  % High limit for trusted network
    }}
]}.
```

**Deployment:**
```bash
# Generate self-signed cert
openssl req -x509 -newkey rsa:4096 -keyout server.key -out server.crt -days 365 -nodes

# Set API key
export ERLMCP_INTERNAL_KEY="internal-key-$(openssl rand -hex 16)"

# Start service
erlmcp start --config=config/internal.config
```

---

## Public API (Basic Auth)

**Use Case:** Public-facing API for web/mobile apps

**Security Profile:** JWT auth + rate limiting + TLS

```erlang
%% config/public_api.config
{erlmcp, [
    %% HTTP transport with TLS
    {transports, [
        {http, #{
            port => 443,
            tls => true,
            tls_opts => #{
                certfile => {os_env, "ERLMCP_TLS_CERT"},
                keyfile => {os_env, "ERLMCP_TLS_KEY"},
                cacertfile => "/etc/ssl/certs/ca-certificates.crt",
                verify => verify_peer,
                depth => 5,
                versions => ['tlsv1.2', 'tlsv1.3'],
                ciphers => [
                    "TLS_AES_128_GCM_SHA256",
                    "TLS_AES_256_GCM_SHA384",
                    "ECDHE-ECDSA-AES128-GCM-SHA256"
                ]
            },
            %% CORS for web apps
            allowed_origins => [
                <<"https://app.example.com">>,
                <<"https://*.example.com">>
            ]
        }}
    ]},

    %% JWT authentication
    {authentication, #{
        enabled => true,
        methods => [jwt],
        jwt => #{
            required_issuer => {os_env, "JWT_ISSUER"},
            required_audience => <<"erlmcp-public-api">>,
            clock_skew_seconds => 30,
            public_keys => #{
                %% Load from environment or Vault
                {os_env, "JWT_PUBLIC_KEY_ID"} => {os_env, "JWT_PUBLIC_KEY_PEM"}
            }
        }
    }},

    %% RBAC authorization
    {authorization, #{
        enabled => true,
        default_role => <<"guest">>,
        roles => #{
            <<"guest">> => [<<"read">>],
            <<"user">> => [<<"read">>, <<"write">>],
            <<"premium">> => [<<"read">>, <<"write">>, <<"execute">>]
        },
        user_roles => #{
            %% Loaded from database at runtime
            %% Maps user_id -> [roles]
        }
    }},

    %% Aggressive rate limiting
    {rate_limiting, #{
        enabled => true,
        max_attempts_per_second => 10,
        max_failures => 5,
        block_duration_ms => 300000,  % 5 minutes
        backoff_levels => [0, 1000, 2000, 4000, 8000, 16000]
    }},

    %% Security headers
    {security_headers, #{
        enabled => true,
        content_security_policy => "default-src 'self'; script-src 'self' 'unsafe-inline'; connect-src 'self'",
        strict_transport_security => "max-age=31536000; includeSubDomains",
        x_frame_options => "DENY",
        x_content_type_options => "nosniff"
    }}
]}.
```

**Environment Setup:**
```bash
# JWT Configuration
export JWT_ISSUER="https://auth.example.com"
export JWT_PUBLIC_KEY_ID="key-2024-01"
export JWT_PUBLIC_KEY_PEM="$(cat /path/to/jwt_public.pem)"

# TLS Certificates (Let's Encrypt)
export ERLMCP_TLS_CERT="/etc/letsencrypt/live/erlmcp.example.com/fullchain.pem"
export ERLMCP_TLS_KEY="/etc/letsencrypt/live/erlmcp.example.com/privkey.pem"

# Start service
erlmcp start --config=config/public_api.config
```

---

## Enterprise SSO

**Use Case:** Corporate environment with Okta/Azure AD/etc.

**Security Profile:** OAuth2 introspection + RBAC + audit

```erlang
%% config/enterprise_sso.config
{erlmcp, [
    {transports, [
        {https, #{port => 443}}
    ]},

    %% OAuth2 introspection (RFC 7662)
    {authentication, #{
        enabled => true,
        methods => [oauth2],
        oauth2 => #{
            enabled => true,
            introspection_url => {os_env, "OAUTH2_INTROSPECTION_URL"},
            client_id => {os_env, "OAUTH2_CLIENT_ID"},
            client_secret => {os_env, "OAUTH2_CLIENT_SECRET"},
            timeout_ms => 5000,
            cache_ttl_seconds => 300,  % Cache for 5 minutes
            scope => <<"erlmcp.read erlmcp.write">>
        }
    }},

    %% Enterprise RBAC
    {authorization, #{
        enabled => true,
        default_role => <<"employee">>,
        roles => #{
            <<"employee">> => [<<"read">>],
            <<"manager">> => [<<"read">>, <<"write">>],
            <<"admin">> => [<<"read">>, <<"write">>, <<"execute">>, <<"delete">>],
            <<"auditor">> => [<<"read">>, <<"audit">>]
        },
        %% Sync roles from corporate directory
        role_sync => #{
            enabled => true,
            backend => ldap,
            ldap_config => #{
                server => {os_env, "LDAP_SERVER"},
                base_dn => {os_env, "LDAP_BASE_DN"},
                bind_dn => {os_env, "LDAP_BIND_DN"},
                bind_password => {os_env, "LDAP_BIND_PASSWORD"}
            }
        }
    }},

    %% Enterprise rate limiting
    {rate_limiting, #{
        enabled => true,
        %% Per-user limits
        strategy => user_based,
        limits => #{
            <<"employee">> => #{rate => 100, period => 60},
            <<"manager">> => #{rate => 500, period => 60},
            <<"admin">> => #{rate => 1000, period => 60}
        }
    }},

    %% Audit logging (compliance)
    {audit, [
        {backend, file},
        {backend_config, #{
            log_path => "/var/log/erlmcp/audit.log",
            format => json,
            rotation => daily,
            retention_days => 2555  % 7 years for compliance
        }},
        {events, [
            authentication_success,
            authentication_failure,
            authorization_denied,
            data_access,
            configuration_change
        ]}
    ]}
]}.
```

**Environment Setup:**
```bash
# OAuth2 Configuration (Okta example)
export OAUTH2_INTROSPECTION_URL="https://yourcompany.okta.com/oauth2/default/v1/introspect"
export OAUTH2_CLIENT_ID="erlmcp-production"
export OAUTH2_CLIENT_SECRET="your-client-secret"

# LDAP Configuration
export LDAP_SERVER="ldap://ldap.yourcompany.com"
export LDAP_BASE_DN="dc=yourcompany,dc=com"
export LDAP_BIND_DN="cn=erlmcp,ou=services,dc=yourcompany,dc=com"
export LDAP_BIND_PASSWORD="service-account-password"

# Start service
erlmcp start --config=config/enterprise_sso.config
```

---

## Zero Trust mTLS

**Use Case:** High-security environment, zero trust network

**Security Profile:** mTLS + least privilege + continuous validation

**NOTE:** mTLS stub exists but needs completion. This is a design example:

```erlang
%% config/zero_trust.config
{erlmcp, [
    {transports, [
        {https, #{
            port => 443,
            %% mTLS required
            tls => #{
                verify => verify_peer,
                fail_if_no_peer_cert => true,  % REQUIRE client cert
                certfile => "/etc/erlmcp/server.crt",
                keyfile => "/etc/erlmcp/server.key",
                cacertfile => "/etc/erlmcp/ca.crt",
                depth => 2,
                %% Custom certificate validation
                verify_fun => {fun verify_client_cert/3, #{
                    %% Extract user_id from client certificate CN
                    user_id_source => cn,
                    %% Check certificate against internal CA
                    allowed_cas => [
                        <<"CN=Internal CA,DC=yourcompany,DC=com">>
                    ],
                    %% Check revocation via OCSP
                    ocsp_check => true,
                    ocsp_responder => <<"https://ocsp.yourcompany.com">>
                }}
            }
        }}
    ]},

    %% mTLS authentication
    {authentication, #{
        enabled => true,
        methods => [mtls],
        mtls => #{
            %% Extract identity from client certificate
            identity_source => cn,  % Common Name
            %% Validate certificate attributes
            require_san => true,  % Subject Alternative Name
            allowed_ous => [  % Organizational Units
                <<"Engineering">>,
                <<"Production Services">>
            ],
            %% Certificate policy
            min_key_strength => 2048,
            allowed_signatures => [sha256, sha384]
        }
    }},

    %% Fine-grained RBAC
    {authorization, #{
        enabled => true,
        %% No default role - must be explicitly granted
        default_role => none,
        roles => #{
            <<"service_readonly">> => [<<"read">>],
            <<"service_readwrite">> => [<<"read">>, <<"write">>],
            <<"service_admin">> => [<<"read">>, <<"write">>, <<"execute">>, <<"delete">>]
        },
        %% Permissions based on certificate attributes
        cert_based_roles => #{
            %% Services in Engineering OU get readwrite
            {cert_ou, <<"Engineering">>} => [<<"service_readwrite">>],
            %% Services with specific CN get admin
            {cert_cn, <<"erlmcp-admin">>} => [<<"service_admin">>]
        }
    }},

    %% Continuous validation
    {continuous_validation, #{
        enabled => true,
        %% Re-validate certificate periodically
        cert_revalidation_interval => 300000,  % 5 minutes
        %% Check certificate revocation
        revocation_check => ocsp,
        %% Revoke session if certificate no longer valid
        auto_revoke_on_cert_invalid => true
    }}
]}.
```

**Certificate Setup:**
```bash
# Generate CA certificate
openssl ecparam -genkey -name prime256v1 -out ca.key
openssl req -new -x509 -days 3650 -key ca.key -out ca.crt \
  -subj "/CN=Internal CA/DC=yourcompany/DC=com"

# Generate server certificate
openssl ecparam -genkey -name prime256v1 -out server.key
openssl req -new -key server.key -out server.csr \
  -subj "/CN=erlmcp.yourcompany.com/O=Your Company/OU=Production Services"
openssl x509 -req -days 365 -in server.csr -CA ca.crt -CAkey ca.key \
  -CAcreateserial -out server.crt

# Generate client certificate
openssl ecparam -genkey -name prime256v1 -out client.key
openssl req -new -key client.key -out client.csr \
  -subj "/CN=service-123/O=Your Company/OU=Engineering"
openssl x509 -req -days 365 -in client.csr -CA ca.crt -CAkey ca.key \
  -CAcreateserial -out client.crt

# Start service
erlmcp start --config=config/zero_trust.config
```

---

## High-Security Compliance

**Use Case:** Regulated industries (finance, healthcare, government)

**Security Profile:** All controls + compliance reporting + tamper-evidence

```erlang
%% config/compliance.config
{erlmcp, [
    %% All transports with maximum security
    {transports, [
        {https, #{
            port => 443,
            tls => #{
                verify => verify_peer,
                fail_if_no_peer_cert => true,
                certfile => {os_env, "COMPLIANCE_TLS_CERT"},
                keyfile => {os_env, "COMPLIANCE_TLS_KEY"},
                cacertfile => "/etc/erlmcp/compliance/ca.crt",
                %% FIPS 140-2 approved ciphers only
                versions => ['tlsv1.2'],  % No TLS 1.3 until FIPS certified
                ciphers => [
                    "ECDHE-ECDSA-AES256-GCM-SHA384",
                    "ECDHE-RSA-AES256-GCM-SHA384"
                ],
                %% Certificate transparency
                require_ct => true,
                ct_logs => [<<"https://ct.googleapis.com/logs/argon2024h1">>]
            }
        }}
    ]},

    %% Multi-factor authentication
    {authentication, #{
        enabled => true,
        methods => [jwt, mtls],
        mfa_required => true,
        mfa_methods => [totp, hardware_key],
        jwt => #{
            required_issuer => {os_env, "COMPLIANCE_JWT_ISSUER"},
            required_audience => <<"erlmcp-compliance">>,
            %% Require specific claims for compliance
            required_claims => [
                <<"sub">>, <<"iss">>, <<"aud">>, <<"exp">>, <<"nbf">>,
                <<"auth_time">>, <<"amr">>  % Authentication methods referenced
            ],
            %% Maximum token lifetime
            max_lifetime_seconds => 3600,  % 1 hour
            public_keys => #{
                %% Keys from HSM-backed CA
                {os_env, "HSM_PUBLIC_KEY_ID"} => {os_env, "HSM_PUBLIC_KEY_PEM"}
            }
        },
        mtls => #{
            identity_source => san,
            %% Require PIV certificate (government/healthcare)
            require_piv => true,
            allowed_oids => [
                <<"2.16.840.1.101.3.6.6">>  % PIV OID
            ]
        }
    }},

    %% Compliance-grade RBAC
    {authorization, #{
        enabled => true,
        default_role => none,
        roles => #{
            %% Principle of least privilege
            <<"auditor_readonly">> => [<<"read">>, <<"audit_read">>],
            <<"operator_standard">> => [<<"read">>, <<"write">>],
            <<"admin_full">> => [<<"read">>, <<"write">>, <<"execute">>, <<"delete">>]
        },
        %% Require explicit authorization for privileged actions
        privileged_actions => [<<"delete">>, <<"execute">>],
        privileged_auth => #{
            require_approval => true,
            approval_workflow => <<"https://approval.yourcompany.com">>,
            approval_timeout_seconds => 300
        }
    }},

    %% Secrets from HSM-backed Vault
    {secrets, [
        {backend, vault},
        {backend_config, #{
            address => {os_env, "COMPLIANCE_VAULT_ADDR"},
            auth_method => approle,
            role_id => {os_env, "COMPLIANCE_VAULT_ROLE_ID"},
            secret_id => {os_env, "COMPLIANCE_VAULT_SECRET_ID"},
            %% Use HSM for unseal
            recovery_key_shares => 5,
            recovery_key_threshold => 3
        }},
        {ttl_seconds => 300}
    ]},

    %% Immutable audit logs (blockchain/WORM)
    {audit, [
        {backend, blockchain},
        {backend_config, #{
            blockchain_type => private,
            nodes => [
                "node1.audit.yourcompany.com",
                "node2.audit.yourcompany.com",
                "node3.audit.yourcompany.com"
            ],
            consensus_algorithm => pbft,
            %% WORM storage (Write Once Read Many)
            worm_storage => true,
            retention_years => 10
        }},
        {events, [
            %% All security-relevant events
            authentication_attempt,
            authentication_success,
            authentication_failure,
            authorization_check,
            authorization_granted,
            authorization_denied,
            data_read,
            data_written,
            data_deleted,
            configuration_change,
            privileged_action,
            system_access,
            network_connection
        ]},
        %% Tamper-evidence
        {integrity, [
            {hash_algorithm, sha512},
            {signature_algorithm, ecdsa},
            {signature_key, {hsm, "audit-signature-key"}},
            {chain_of_trust, true}
        ]}
    ]},

    %% Session recording
    {session_recording, [
        {enabled => true},
        {record_content => true},
        {storage_backend => s3_compliant},
        {storage_config => #{
            bucket => "erlmcp-session-records",
            retention_days => 2555,  % 7 years
        }},
        %% Immutable storage
        {immutable_storage => true},
        {encryption_at_rest => true},
        {encryption_key => {hsm, "session-encryption-key"}}
    ]},

    %% Compliance monitoring
    {compliance_monitoring, [
        {standards, [hipaa, pci_dss, soc2]},
        {controls, [
            {access_control, #{
                requirement => "All access must be authenticated and authorized",
                enforcement => real_time,
                evidence_generation => true
            }},
            {audit_logging, #{
                requirement => "All actions must be logged immutably",
                enforcement => synchronous,
                retention_years => 7
            }},
            {data_encryption, #{
                requirement => "All data encrypted at rest and in transit",
                encryption_level => aes256,
                key_management => hsm
            }}
        ]},
        {continuous_monitoring => true},
        {alert_on_violation => true},
        {generate_evidence_reports => true}
    ]}
]}.
```

---

## Quick Configuration Builder

Use this interactive builder to generate your configuration:

```bash
#!/bin/bash
# erlmcp-security-config-builder.sh

echo "=== erlmcp Security Configuration Builder ==="
echo ""
echo "Select deployment type:"
echo "1) Local Development"
echo "2) Internal Service"
echo "3) Public API (Basic Auth)"
echo "4) Enterprise SSO"
echo "5) Zero Trust mTLS"
echo "6) High-Security Compliance"
read -p "Enter choice [1-6]: " choice

case $choice in
    1)
        cp templates/dev.config sys.config
        ;;
    2)
        cp templates/internal.config sys.config
        read -p "Internal API key: " api_key
        export ERLMCP_INTERNAL_KEY="$api_key"
        ;;
    3)
        cp templates/public_api.config sys.config
        read -p "JWT issuer URL: " issuer
        read -p "JWT public key path: " pubkey
        export JWT_ISSUER="$issuer"
        export JWT_PUBLIC_KEY_PEM="$(cat $pubkey)"
        ;;
    # ... more cases
    *)
        echo "Invalid choice"
        exit 1
esac

echo "Configuration generated in sys.config"
echo "Environment variables set. Run 'erlmcp start' to begin."
```

---

## Validation Scripts

### Security Configuration Validator

```bash
#!/bin/bash
# validate-security-config.sh

echo "=== erlmcp Security Configuration Validator ==="

# Check TLS configuration
echo "[1/6] Checking TLS configuration..."
if grep -q "{tls, #{enabled => true}" sys.config; then
    echo "✓ TLS enabled"
else
    echo "✗ TLS disabled - SECURITY RISK"
    exit 1
fi

# Check authentication
echo "[2/6] Checking authentication configuration..."
if grep -q "enabled => true" sys.config | grep -q "authentication"; then
    echo "✓ Authentication enabled"
else
    echo "⚠ Authentication disabled - OK for local dev only"
fi

# Check rate limiting
echo "[3/6] Checking rate limiting..."
if grep -q "{rate_limiting, #{enabled => true}" sys.config; then
    echo "✓ Rate limiting enabled"
else
    echo "⚠ Rate limiting disabled - RECOMMENDED for production"
fi

# Check security headers
echo "[4/6] Checking security headers..."
if grep -q "{security_headers, #{enabled => true}" sys.config; then
    echo "✓ Security headers enabled"
else
    echo "✗ Security headers disabled - SECURITY RISK"
fi

# Check for hardcoded secrets
echo "[5/6] Checking for hardcoded secrets..."
if grep -E "(password|api_key|secret)\s*=>\s*\"[^\"]+\"" sys.config; then
    echo "✗ Hardcoded secrets detected - SECURITY RISK"
    exit 1
else
    echo "✓ No hardcoded secrets"
fi

# Validate with erlmcp
echo "[6/6] Running erlmcp security validator..."
erlmcp_validate run security

echo "=== Validation Complete ==="
```

---

**Document Status:** Quick reference
**Related:** [Security Architecture Plan](11_security_architecture_plan.md) | [Security Hardening Guide](security_hardening_guide.md)
