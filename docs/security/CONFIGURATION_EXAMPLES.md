# Security Configuration Examples - erlmcp

## Overview

This document provides comprehensive security configuration examples for different deployment scenarios, environments, and compliance requirements.

## Table of Contents

- [Development Configuration](#development-configuration)
- [Staging Configuration](#staging-configuration)
- [Production Configuration](#production-configuration)
- [High-Security Configuration](#high-security-configuration)
- [Compliance-Specific Configurations](#compliance-specific-configurations)

## Development Configuration

### Minimal Setup (Local Development)

```erlang
%% config/dev.config
[
    % Authentication: API keys only
    {erlmcp_auth, [
        {methods, [api_key]},
        {api_keys, #{
            <<"dev_key_123">> => #{user_id => <<"developer">>, role => admin},
            <<"dev_key_456">> => #{user_id => <<"tester">>, role => user}
        }},
        {session_ttl, 7200},  % 2 hours
        {max_sessions_per_user, 10}
    ]},

    % Secrets: Local encrypted storage
    {erlmcp_secrets, [
        {backend, local_encrypted},
        {encryption_key, {env_var, "ERLMCP_DEV_SECRET_KEY"}},
        {storage_path => "priv/secrets/dev.enc"},
        {ttl_seconds, 60},  % 1 minute cache
        {audit_enabled, false}  % Disabled in dev
    ]},

    % Audit logging: Basic logging
    {erlmcp_audit_log, [
        {log_path, "priv/audit/dev-audit.log"},
        {hash_chain, false},  % Disabled for performance
        {buffer_size, 50},
        {flush_interval_ms, 10000}  % 10 seconds
    ]},

    % Rate limiting: Relaxed limits
    {erlmcp_rate_limiter, [
        {strategy, token_bucket},
        {enabled, false}  % Disabled in dev
    ]},

    % Transport: STDIO (no network)
    {erlmcp_transport, [
        {default, stdio}
    ]},

    % TLS: Disabled (local only)
    {erlmcp_tls, [
        {enabled, false}
    ]}
].
```

### Docker Development Environment

```erlang
%% config/docker-dev.config
[
    % Authentication: Multiple methods for testing
    {erlmcp_auth, [
        {methods, [api_key, jwt, mtls]},
        {api_keys, #{
            <<"docker_dev_key">> => #{user_id => <<"docker_dev">>, role => admin}
        }},
        {jwt_keys, #{
            <<"dev_key">> => dev_public_key
        }},
        {mtls, #{
            enabled => true,
            ca_cert_path => "/etc/erlmcp/tls/dev-ca.crt"
        }},
        {session_ttl, 3600}
    ]},

    % Secrets: Vault (dev instance)
    {erlmcp_secrets, [
        {backend, vault},
        {backend_config, #{
            address => "http://vault:8200",
            token => "s.dev-token",
            engine => "kv",
            mount => "secret"
        }},
        {ttl_seconds, 300},
        {fallback_to_local, true},
        {storage_path, "priv/secrets/fallback.enc"}
    ]},

    % Audit logging: Full logging with hash chain
    {erlmcp_audit_log, [
        {log_path, "/var/log/erlmcp/dev-audit.log"},
        {hash_chain, true},
        {digital_signature, #{enabled, false}},
        {buffer_size, 100},
        {flush_interval_ms, 5000}
    ]},

    % Rate limiting: Enabled but relaxed
    {erlmcp_rate_limiter, [
        {strategy, token_bucket},
        {ip_limits, #{rate => 1000, period => 60000}},  % 1000 req/min
        {session_limits, #{rate => 5000, period => 60000}},
        {whitelisted_ips => [<<"172.16.0.0/12">>]}  % Docker network
    ]},

    % Transport: All transports for testing
    {erlmcp_transports, [
        {tcp, #{
            port => 9000,
            tls => true,
            certfile => "/etc/erlmcp/tls/dev-server.crt",
            keyfile => "/etc/erlmcp/tls/dev-server.key"
        }},
        {http, #{
            port => 8080,
            tls => true
        }},
        {stdio, #{
            enabled => true
        }}
    ]}
].
```

## Staging Configuration

### Pre-Production Testing

```erlang
%% config/staging.config
[
    % Authentication: Production-like auth
    {erlmcp_auth, [
        {methods, [jwt, api_key, oauth2, mtls]},
        {jwt_config, #{
            issuer => "https://auth.staging.example.com",
            audience => "erlmcp-staging",
            clock_skew_seconds => 30
        }},
        {oauth2, #{
            enabled => true,
            introspection_url => "https://oauth.staging.example.com/introspect",
            client_id => "erlmcp-staging",
            client_secret => {env_var, "OAUTH2_CLIENT_SECRET"}
        }},
        {mtls, #{
            enabled => true,
            ca_cert_path => "/etc/erlmcp/tls/staging-ca.crt",
            verify_client => true
        }},
        {session_ttl, 3600},  % 1 hour
        {max_sessions_per_user, 5}
    ]},

    % Secrets: Vault (staging)
    {erlmcp_secrets, [
        {backend, vault},
        {backend_config, #{
            address => "https://vault.staging.example.com:8200",
            auth_method => approle,
            role_id => {env_var, "VAULT_ROLE_ID"},
            secret_id => {env_var, "VAULT_SECRET_ID"},
            engine => "kv-v2",
            mount => "secret",
            namespace => "staging"
        }},
        {ttl_seconds, 300},  % 5 minutes
        {audit_enabled, true}
    ]},

    % Audit logging: Full compliance logging
    {erlmcp_audit_log, [
        {log_path, "/var/log/erlmcp/staging-audit.log"},
        {hash_chain, true},
        {digital_signature, #{
            enabled => true,
            algorithm => ecdsa,
            private_key_path => "/etc/erlmcp/keys/staging-audit-private.pem",
            sign_interval => 60000
        }},
        {buffer_size, 100},
        {flush_interval_ms, 5000},
        {retention_days, 90}
    ]},

    % Rate limiting: Production-like limits
    {erlmcp_rate_limiter, [
        {strategy, token_bucket},
        {ip_limits, #{
            rate => 100,
            period => 60000,
            burst => 20,
            block_duration => 300000
        }},
        {session_limits, #{
            rate => 1000,
            period => 60000,
            burst => 100
        }},
        {user_limits, #{
            rate => 5000,
            period => 60000,
            burst => 500
        }},
        {global_limits, #{
            rate => 50000,  % Lower than prod
            period => 60000
        }}
    ]},

    % Transport: HTTPS with strict security
    {erlmcp_transports, [
        {tcp, #{
            port => 9000,
            tls => true,
            certfile => "/etc/erlmcp/tls/staging-server.crt",
            keyfile => "/etc/erlmcp/tls/staging-server.key",
            cacertfile => "/etc/erlmcp/tls/staging-ca.crt",
            verify => verify_peer,
            hsts => true
        }},
        {http, #{
            port => 8080,
            tls => true,
            certfile => "/etc/erlmcp/tls/staging-server.crt",
            keyfile => "/etc/erlmcp/tls/staging-server.key",
            security_headers => #{
                content_security_policy => "default-src 'self'; script-src 'self' 'unsafe-inline'",
                strict_transport_security => "max-age=31536000; includeSubDomains",
                x_frame_options => "DENY"
            }
        }}
    ]}
].
```

## Production Configuration

### Standard Production Setup

```erlang
%% config/prod.config
[
    % Authentication: Enterprise-grade auth
    {erlmcp_auth, [
        {methods, [jwt, oauth2, mtls, api_key]},
        {jwt_config, #{
            issuer => "https://auth.production.example.com",
            audience => "erlmcp-production",
            clock_skew_seconds => 10,  % Tighter clock skew
            require_exp => true,
            require_nbf => true,
            require_iat => true
        }},
        {oauth2, #{
            enabled => true,
            introspection_url => "https://oauth.production.example.com/introspect",
            client_id => "erlmcp-production",
            client_secret => {env_var, "OAUTH2_CLIENT_SECRET"},
            cache_ttl => 300
        }},
        {mtls, #{
            enabled => true,
            ca_cert_path => "/etc/erlmcp/tls/prod-ca.crt",
            verify_client => true,
            check_crl => true,
            ocsp_stapling => true
        }},
        {api_keys, #{
            rotation_days => 90,
            min_entropy_bits => 256
        }},
        {session_ttl, 3600},  % 1 hour
        {max_sessions_per_user, 3},
        {lockout_threshold, 5},  % Lock after 5 failed attempts
        {lockout_duration, 900}  % 15 minutes
    ]},

    % Secrets: Vault with AppRole (production)
    {erlmcp_secrets, [
        {backend, vault},
        {backend_config, #{
            address => "https://vault.production.example.com:8200",
            auth_method => approle,
            role_id => {env_var, "VAULT_ROLE_ID"},
            secret_id => {env_var, "VAULT_SECRET_ID"},
            engine => "kv-v2",
            mount => "secret",
            namespace => "production",
            timeout => 5000,
            connection_pool_size => 10
        }},
        {ttl_seconds, 600},  % 10 minutes
        {max_cache_size, 10000},
        {audit_enabled, true},
        {encrypt_in_memory, true},  % Encrypt cache in memory
        {fallback_to_local, true},
        {storage_path, "/var/lib/erlmcp/secrets-fallback.enc"}
    ]},

    % Audit logging: Full compliance with digital signatures
    {erlmcp_audit_log, [
        {log_path, "/var/log/erlmcp/production-audit.log"},
        {archive_path, "/var/log/erlmcp/archive/"},
        {hash_chain, true},
        {digital_signature, #{
            enabled => true,
            algorithm => rsa,  % RSA-4096 for maximum security
            key_size => 4096,
            private_key_path => "/etc/erlmcp/keys/production-audit-private.pem",
            public_key_path => "/etc/erlmcp/keys/production-audit-public.pem",
            sign_interval => 60000,
            verify_on_write => true
        }},
        {buffer_size, 100},
        {flush_interval_ms, 5000},
        {retention_days, 2555},  % 7 years (GDPR)
        {compression, gzip},
        {indexing, true},
        {real_time_alerts, true}
    ]},

    % Rate limiting: Strict production limits
    {erlmcp_rate_limiter, [
        {strategy, token_bucket},
        {ip_limits, #{
            rate => 100,
            period => 60000,
            burst => 10,
            block_duration => 3600000  % 1 hour block
        }},
        {session_limits, #{
            rate => 1000,
            period => 60000,
            burst => 50
        }},
        {user_limits, #{
            rate => 5000,
            period => 60000,
            burst => 250
        }},
        {global_limits, #{
            rate => 100000,
            period => 60000,
            burst => 10000
        }},
        {whitelisted_ips => []},  % No whitelist in prod
        {blacklisted_ips, [
            <<"0.0.0.0/8">>,  % Reserved
            <<"127.0.0.0/8">>  % Loopback (shouldn't come from internet)
        ]},
        {alert_on_exceed, true}
    ]},

    % Transport: TLS 1.3 only
    {erlmcp_transports, [
        {tcp, #{
            port => 9000,
            tls => true,
            certfile => "/etc/erlmcp/tls/production-server.crt",
            keyfile => "/etc/erlmcp/tls/production-server.key",
            cacertfile => "/etc/erlmcp/tls/production-ca.crt",
            verify => verify_peer,
            fail_if_no_peer_cert => true,
            versions => ['tlsv1.3'],  % TLS 1.3 only
            ciphers => [
                "TLS_AES_256_GCM_SHA384",
                "TLS_CHACHA20_POLY1305_SHA256"
            ],
            hsts => true,
            hsts_max_age => 31536000,
            hsts_include_subdomains => true,
            hsts_preload => true,
            secure_renegotiate => true,
            reuse_sessions => true
        }},
        {http, #{
            port => 8080,
            tls => true,
            certfile => "/etc/erlmcp/tls/production-server.crt",
            keyfile => "/etc/erlmcp/tls/production-server.key",
            versions => ['tlsv1.3'],
            security_headers => #{
                content_security_policy =>
                    "default-src 'self'; "
                    "script-src 'self' 'unsafe-inline' 'unsafe-eval'; "
                    "style-src 'self' 'unsafe-inline'; "
                    "img-src 'self' data: https:; "
                    "connect-src 'self' wss: ws:; "
                    "object-src 'none'; "
                    "frame-ancestors 'none'; "
                    "base-uri 'self'; "
                    "upgrade-insecure-requests",
                strict_transport_security =>
                    "max-age=31536000; includeSubDomains; preload",
                x_content_type_options => "nosniff",
                x_frame_options => "DENY",
                x_xss_protection => "1; mode=block",
                referrer_policy => "strict-origin-when-cross-origin",
                permissions_policy =>
                    "geolocation=(), microphone=(), camera=(), payment=(), usb=()",
                cross_origin_resource_policy => "same-origin",
                cross_origin_opener_policy => "same-origin",
                cross_origin_embedder_policy => "require-corp"
            },
            cors => #{
                enabled => true,
                allowed_origins => ["https://app.production.example.com"],
                allowed_methods => ["GET", "POST", "OPTIONS"],
                allowed_headers => ["Content-Type", "Authorization"],
                max_age => 86400,
                credentials => true
            }
        }}
    ]}
].
```

## High-Security Configuration

### Financial/Healthcare Grade Security

```erlang
%% config/high-security.config
[
    % Authentication: Multi-factor authentication
    {erlmcp_auth, [
        {methods, [jwt, mtls, totp]},  % TOTP = Time-based OTP
        {jwt_config, #{
            issuer => "https://auth.high-security.example.com",
            audience => "erlmcp-high-security",
            require_scopes => ["profile", "email"],
            max_token_age => 300  % 5 minutes max token age
        }},
        {mtls, #{
            enabled => true,
            ca_cert_path => "/etc/erlmcp/tls/hs-ca.crt",
            verify_client => true,
            check_crl => true,
            ocsp_stapling => true,
            cert_expiry_warning_days => 30,
            min_key_strength => 3072  % RSA-3072 minimum
        }},
        {totp, #{
            enabled => true,
            issuer => "erlmcp-hs",
            time_step => 30,  % 30-second TOTP window
            skew => 1  % Allow 1 time step skew
        }},
        {session_ttl, 900},  % 15 minutes
        {max_sessions_per_user, 1},  % Single session only
        {require_mfa_for_admin, true},
        {ip_binding, true},  % Bind sessions to IP addresses
        {user_agent_binding, true},  % Bind to user agent
        {geo_verification, true}  % Verify geolocation consistency
    ]},

    % Secrets: Vault with HSM backing
    {erlmcp_secrets, [
        {backend, vault_hsm},
        {backend_config, #{
            address => "https://vault.high-security.example.com:8200",
            auth_method => approle,
            role_id => {env_var, "VAULT_ROLE_ID"},
            secret_id => {env_var, "VAULT_SECRET_ID"},
            engine => "kv-v2",
            mount => "secret",
            namespace => "high-security",
            hsm_slot => 0,
            hsm_pin => {env_var, "HSM_PIN"}
        }},
        {ttl_seconds, 60},  % 1 minute cache
        {encrypt_in_memory, true},
        {zero_buffer_on_free, true},
        {audit_all_access, true}
    ]},

    % Audit logging: WORM storage (Write Once, Read Many)
    {erlmcp_audit_log, [
        {log_path, "/mnt/worm/audit.log"},  % WORM filesystem
        {hash_chain, true},
        {digital_signature, #{
            enabled => true,
            algorithm => ecdsa,
            curve => secp521r1,  % Highest security curve
            private_key_path => "/etc/erlmcp/keys/hs-audit-private.pem",
            sign_interval => 1000  % Sign every second
        }},
        {buffer_size, 10},  % Flush frequently
        {flush_interval_ms, 1000},
        {retention_days, 7300},  % 20 years (financial)
        {replication, #{
            enabled => true,
            destinations => [
                #{
                    type => s3,
                    bucket => "erlmcp-audit-backup",
                    prefix => "worm/",
                    storage_class => "GLACIER"
                }
            ]
        }},
        {immutable_storage, true},
        {real_time_verification, true}
    ]},

    % Rate limiting: Very strict limits
    {erlmcp_rate_limiter, [
        {strategy, sliding_window},  % More accurate
        {ip_limits, #{
            rate => 10,
            period => 60000,
            burst => 2,
            block_duration => 86400000  % 24 hours
        }},
        {session_limits, #{
            rate => 100,
            period => 60000,
            burst => 10
        }},
        {user_limits, #{
            rate => 500,
            period => 60000,
            burst => 50
        }},
        {global_limits, #{
            rate => 10000,
            period => 60000
        }},
        {anomaly_detection, true},  % Detect unusual patterns
        {auto_block_on_anomaly, true}
    ]},

    % Transport: TLS 1.3 + mTLS + Certificate pinning
    {erlmcp_transports, [
        {tcp, #{
            port => 9000,
            tls => true,
            certfile => "/etc/erlmcp/tls/hs-server.crt",
            keyfile => "/etc/erlmcp/tls/hs-server.key",
            cacertfile => "/etc/erlmcp/tls/hs-ca.crt",
            verify => verify_peer,
            fail_if_no_peer_cert => true,
            versions => ['tlsv1.3'],
            ciphers => ["TLS_AES_256_GCM_SHA384"],  % Single cipher only
            certificate_pinning => true,
            pinned_certs => [
                "/etc/erlmcp/tls/pinned-client-cert-1.pem",
                "/etc/erlmcp/tls/pinned-client-cert-2.pem"
            ]
        }}
    ]}
].
```

## Compliance-Specific Configurations

### GDPR Compliance (EU Data Protection)

```erlang
%% config/gdpr.config
[
    {erlmcp_auth, [
        {consent_tracking, true},  % Track user consent
        {data_minimization, true},  % Only collect necessary data
        {right_to_access, #{
            enabled => true,
            response_format => json,
            max_processing_time => 30  % 30 days maximum
        }},
        {right_to_erasure, #{
            enabled => true,
            retention_override => true,  % Allow deletion before retention period
            anonymization_method => hash  % Hash instead of delete
        }},
        {right_to_portability, #{
            enabled => true,
            formats => [json, csv]
        }},
        {breach_notification, #{
            enabled => true,
            notification_threshold => 72,  % 72 hours
            recipients => ["dpo@example.com", "gdpr@example.com"]
        }}
    ]},

    {erlmcp_audit_log, [
        {retention_days, 2555},  % 7 years maximum
        {user_data_export, #{
            enabled => true,
            include_logs => true,
            include_sessions => true,
            include_resources => true
        }},
        {anonymization, #{
            enabled => true,
            method => crypto_hash,
            algorithm => sha256,
            salt => {env_var, "ANONYMIZATION_SALT"}
        }}
    ]}
].
```

### SOC2 Compliance (Service Organizations)

```erlang
%% config/soc2.config
[
    {erlmcp_auth, [
        {access_control, #{
            principle => least_privilege,
            role_based => true,
            separation_of_duties => true
        }},
        {change_management, #{
            enabled => true,
            require_approval => true,
            audit_all_changes => true,
            rollback_on_failure => true
        }}
    ]},

    {erlmcp_secrets, [
        {encryption_at_rest, #{
            enabled => true,
            algorithm => aes_256_gcm,
            key_rotation_days => 90
        }},
        {encryption_in_transit, #{
            enabled => true,
            min_tls_version => tlsv1_2
        }}
    ]},

    {erlmcp_audit_log, [
        {audit_trail, #{
            enabled => true,
            tamper_proof => true,
            immutable => true,
            all_events => true
        }},
        {monitoring, #{
            enabled => true,
            real_time_alerts => true,
            performance_metrics => true,
            availability_tracking => true
        }},
        {incident_response, #{
            enabled => true,
            automated_response => true,
            escalation_procedures => true,
            postmortem_required => true
        }}
    ]}
].
```

### HIPAA Compliance (Healthcare)

```erlang
%% config/hipaa.config
[
    {erlmcp_auth, [
        {phi_access_control, #{
            enabled => true,
            minimum_necessary => true,  % Minimum necessary access rule
            role_based => true,
            emergency_access => #{
                enabled => true,
                require_break_glass => true,
                audit_emergency_access => true,
                approval_required => true
            }
        }}
    ]},

    {erlmcp_secrets, [
        {phi_encryption, #{
            enabled => true,
            algorithm => aes_256_gcm,
            key_management => hsm,  % HSM for PHI encryption keys
            key_rotation_days => 90,
            encrypt_backups => true
        }}
    ]},

    {erlmcp_audit_log, [
        {phi_audit_trail, #{
            enabled => true,
            all_phi_access => true,  % Log ALL PHI access
            tamper_proof => true,
            retention_years => 6
        }},
        { PHI_access_logging, #{
            user_id => true,
            timestamp => true,
            resource_accessed => true,
            justification_required => true,
            supervisor_approval_for_high_sensitivity => true
        }}
    ]}
].
```

### PCI DSS Compliance (Payment Cards)

```erlang
%% config/pci_dss.config
[
    {erlmcp_auth, [
        {strong_auth, #{
            enabled => true,
            min_password_length => 12,
            require_mfa => true,
            password_complexity => true,
            session_timeout => 900
        }},
        {access_control, #{
            need_to_know => true,
            role_based => true,
            default_deny => true
        }}
    ]},

    {erlmcp_secrets, [
        {card_data_protection, #{
            enabled => true,
            encryption_at_rest => aes_256_gcm,
            encryption_in_transit => tls_1_3,
            key_management => hsm,
            key_rotation_days => 90,
            no_card_data_storage => true  % Don't store full PAN
        }}
    ]},

    {erlmcp_audit_log, [
        {pci_audit_trail, #{
            enabled => true,
            all_access => true,
            tamper_proof => true,
            retention_years => 1
        }},
        {logging, #{
            log_all_card_data_access => true,
            log_all_administrative_access => true,
            log_all_security_events => true,
            daily_review => true
        }}
    ]}
].
```

## Environment-Specific Overrides

### Kubernetes Deployment

```yaml
# config/kubernetes-values.yaml
auth:
  methods:
    - jwt
    - mtls
  sessionTtl: 3600
  maxSessionsPerUser: 5

secrets:
  backend: vault
  vault:
    address: "https://vault.example.com:8200"
    authMethod: approle
    roleId:
      secretKeyRef:
        name: vault-role-id
        key: role-id
    secretId:
      secretKeyRef:
        name: vault-secret-id
        key: secret-id
  ttlSeconds: 600

auditLog:
  enabled: true
  hashChain: true
  digitalSignature:
    enabled: true
    privateKeyPath: /etc/erlmcp/keys/audit-private.pem
  retentionDays: 2555
  persistence:
    enabled: true
    storageClass: "standard"
    size: 100Gi

rateLimiter:
  enabled: true
  ipLimits:
    rate: 100
    period: 60000
  sessionLimits:
    rate: 1000
    period: 60000

transports:
  tcp:
    port: 9000
    tls: true
    certFile: /etc/erlmcp/tls/server.crt
    keyFile: /etc/erlmcp/tls/server.key
    caCertFile: /etc/erlmcp/tls/ca.crt
```

### Terraform Deployment

```hcl
# modules/erlmcp-security/main.tf
variable "auth_methods" {
  type    = list(string)
  default = ["jwt", "mtls"]
}

variable "session_ttl" {
  type    = number
  default = 3600
}

variable "vault_address" {
  type    = string
  default = "https://vault.example.com:8200"
}

variable "audit_retention_days" {
  type    = number
  default = 2555  # 7 years
}

variable "rate_limit_per_minute" {
  type    = number
  default = 100
}

# Generate erlmcp config
resource "local_file" "erlmcp_config" {
  content = templatefile("${path.module}/config.tmpl", {
    auth_methods          = var.auth_methods
    session_ttl           = var.session_ttl
    vault_address         = var.vault_address
    audit_retention_days  = var.audit_retention_days
    rate_limit_per_minute = var.rate_limit_per_minute
  })
  filename = "${path.module}/generated/sys.config"
}
```

## References

- **Security Architecture**: `/Users/sac/erlmcp/docs/security/README.md`
- **Transport Security**: `/Users/sac/erlmcp/docs/security/transport-security.md`
- **Secrets Management**: `/Users/sac/erlmcp/docs/secrets/README.md`
- **Audit Logging**: `/Users/sac/erlmcp/docs/security/audit-logging.md`
- **Quick Reference**: `/Users/sac/erlmcp/docs/security/QUICK_REFERENCE.md`
