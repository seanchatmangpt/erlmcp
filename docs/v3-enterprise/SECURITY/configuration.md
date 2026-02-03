# Security Configuration Guide

## Overview

This document provides comprehensive security configuration guidelines for erlmcp v3 deployments in enterprise environments. It covers authentication, authorization, encryption, network security, and compliance requirements.

## Security Architecture

### Defense in Depth

erlmcp v3 implements multiple layers of security:

1. **Network Security**: Firewalls, load balancers, VPNs
2. **Transport Security**: TLS 1.3, certificate validation
3. **Application Security**: Authentication, authorization, input validation
4. **Data Security**: Encryption at rest and in transit
5. **Host Security**: Container security, process isolation

### Key Security Features

- **JWT-based authentication**
- **Role-based access control (RBAC)**
- **Transport layer encryption (TLS 1.3)**
- **Input validation and sanitization**
- **Audit logging**
- **Circuit breaker patterns**
- **Rate limiting**
- **Certificate pinning**

## Authentication & Authorization

### 1. Authentication Methods

#### JWT Token Authentication

```yaml
# jwt_authentication.conf
{erlmcp, [
    {auth, [
        {method, jwt},
        {jwt, [
            {issuer, "https://auth.company.com"},
            {audience, "erlmcp-api"},
            {secret, <<"$JWT_SECRET_KEY">>},
            {algorithm, "HS256"},
            {expiry, 3600}  % 1 hour
        ]}
    ]}
]}.
```

#### OAuth 2.0 Integration

```yaml
# oauth2.conf
{erlmcp, [
    {auth, [
        {method, oauth2},
        {oauth2, [
            {provider, azure_ad},
            {client_id, "$CLIENT_ID"},
            {client_secret, "$CLIENT_SECRET"},
            {tenant_id, "$TENANT_ID"},
            {scopes, ["api://erlmcp-api/.default"]}
        ]}
    ]}
]}.
```

#### API Key Authentication

```yaml
# api_key.conf
{erlmcp, [
    {auth, [
        {method, api_key},
        {api_key, [
            {header, "X-API-Key"},
            {validation, fun(Key) -> validate_api_key(Key) end}
        ]}
    ]}
]}.
```

### 2. Authorization Configuration

#### Role-Based Access Control (RBAC)

```yaml
# rbac.conf
{erlmcp, [
    {auth, [
        {rbac, [
            {roles, [
                {admin, [
                    {permissions, [
                        "resources:*",
                        "tools:*",
                        "sessions:*",
                        "system:*"
                    ]}
                ]},
                {developer, [
                    {permissions, [
                        "resources:read",
                        "tools:read",
                        "tools:execute",
                        "sessions:read",
                        "sessions:create"
                    ]}
                ]},
                {viewer, [
                    {permissions, [
                        "resources:read",
                        "tools:read"
                    ]}
                ]}
            ]},
            {policy, [
                {allow, "admin", "*"},
                {allow, "developer", ["resources:read", "tools:read", "tools:execute"]},
                {allow, "viewer", ["resources:read", "tools:read"]}
            ]}
        ]}
    ]}
]}.
```

#### Attribute-Based Access Control (ABAC)

```yaml
# abac.conf
{erlmcp, [
    {auth, [
        {abac, [
            {attributes, [
                {department, ["engineering", "product", "marketing"]},
                {security_level, ["low", "medium", "high"]},
                {region, ["us-east", "us-west", "eu-central"]}
            ]},
            {policies, [
                {allow, [
                    {eq, "${user.department}", "engineering"},
                    {gte, "${user.security_level}", "medium"}
                ], ["tools:*"]},
                {allow, [
                    {in, "${user.region}", ["us-east", "us-west"]}
                ], ["resources:read"]}
            ]}
        ]}
    ]}
]}.
```

### 3. LDAP/Active Directory Integration

```yaml
# ldap.conf
{erlmcp, [
    {auth, [
        {method, ldap},
        {ldap, [
            {host, "ldap.company.com"},
            {port, 636},
            {ssl, true},
            {base_dn, "dc=company,dc=com"},
            {bind_dn, "cn=admin,dc=company,dc=com"},
            {bind_password, "$LDAP_PASSWORD"},
            {user_search, "(uid=${username})"},
            {role_mapping, fun(User) -> ldap_to_rbac_roles(User) end},
            {group_search, "(member=${dn})"},
            {cache_ttl, 300}
        ]}
    ]}
]}.
```

## Transport Security

### 1. SSL/TLS Configuration

#### Server Configuration

```yaml
# ssl.conf
{erlmcp, [
    {ssl, [
        {enabled, true},
        {port, 8443},
        {certfile, "/etc/erlmcp/ssl/server.crt"},
        {keyfile, "/etc/erlmcp/ssl/server.key"},
        {cafile, "/etc/erlmcp/ssl/ca.crt"},
        {verify, verify_none},
        {versions, [tlsv1.2, tlsv1.3]},
        {ciphers, [
            "ECDHE-ECDSA-AES256-GCM-SHA384",
            "ECDHE-RSA-AES256-GCM-SHA384",
            "ECDHE-ECDSA-CHACHA20-POLY1305",
            "ECDHE-RSA-CHACHA20-POLY1305",
            "ECDHE-ECDSA-AES128-GCM-SHA256",
            "ECDHE-RSA-AES128-GCM-SHA256"
        ]},
        {honor_cipher_order, true},
        {client_renegotiation, false},
        {secure_renegotiation, true}
    ]}
]}.
```

#### Certificate Management

```bash
# Generate CSR
openssl req -new -newkey rsa:2048 -nodes \
    -keyout server.key -out server.csr \
    -subj "/CN=erlmcp.company.com/O=Company/C=US"

# Generate self-signed certificate (for testing)
openssl req -x509 -new -nodes -key server.key -sha256 -days 365 \
    -out server.crt -subj "/CN=erlmcp.company.com/O=Company/C=US"
```

### 2. Certificate Pinning

```yaml
{erlmcp, [
    {security, [
        {certificate_pinning, [
            {pin_sha256, "abc123..."},
            {pin_algorithm, "sha256"},
            {strict_mode, true}
        ]}
    ]}
]}.
```

### 3. HSTS Configuration

```yaml
# haproxy.cfg (for HTTP layer)
frontend https-in
    bind *:443 ssl crt /etc/haproxy/ssl/
    http-response set-header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload"
```

## Network Security

### 1. Firewall Rules

```bash
# iptables rules
# Allow HTTP/HTTPS
iptables -A INPUT -p tcp --dport 80 -j ACCEPT
iptables -A INPUT -p tcp --dport 443 -j ACCEPT

# Allow internal traffic
iptables -A INPUT -s 10.0.0.0/8 -j ACCEPT
iptables -A INPUT -s 172.16.0.0/12 -j ACCEPT
iptables -A INPUT -s 192.168.0.0/16 -j ACCEPT

# Allow loopback
iptables -A INPUT -i lo -j ACCEPT

# Deny all other traffic
iptables -A INPUT -j DROP
```

### 2. IP Whitelisting

```yaml
{erlmcp, [
    {network, [
        {allowed_ips, [
            "10.0.0.0/24",
            "172.16.0.0/12",
            "192.168.0.0/16"
        ]},
        {allowed_cidrs, [
            {"production", "10.1.0.0/16"},
            {"staging", "10.2.0.0/16"}
        ]}
    ]}
]}.
```

### 3. VPN Configuration

```yaml
# Site-to-Site VPN configuration
{erlmcp, [
    {vpn, [
        {enabled, true},
        {type, "ipsec"},
        {peer, "vpn.company.com"},
        {local_subnet, "10.0.0.0/24"},
        {remote_subnet, "10.1.0.0/24"},
        {psk, "$VPN_PSK"},
        {ike_version, 2}
    ]}
]}.
```

## Data Security

### 1. Encryption at Rest

```yaml
{erlmcp, [
    {encryption, [
        {at_rest, [
            {algorithm, "aes-256-gcm"},
            {key_rotation, 90},  % days
            {storage, "vault"},
            {vault, [
                {endpoint, "https://vault.company.com:8200"},
                {auth_method, "kubernetes"},
                {secret_path, "secret/erlmcp"}
            ]}
        ]}
    ]}
]}.
```

### 2. Encryption in Transit

```yaml
{erlmcp, [
    {encryption, [
        {in_transit, [
            {tls, true},
            {minimum_protocol, "tlsv1.2"},
            {cipher_suites, "secure"},
            {certificate, "/etc/erlmcp/ssl/server.crt"},
            {private_key, "/etc/erlmcp/ssl/server.key"}
        ]}
    ]}
]}.
```

### 3. Data Masking

```yaml
{erlmcp, [
    {data_masking, [
        {enabled, true},
        {patterns, [
            {credit_card, "\\b\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}\\b"},
            {ssn, "\\b\\d{3}-\\d{2}-\\d{4}\\b"},
            {email, "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b"}
        ]},
        {replacement, "***MASKED***"}
    ]}
]}.
```

## Input Validation

### 1. Request Validation

```yaml
{erlmcp, [
    {validation, [
        {requests, [
            {max_size, 1048576},  % 1MB
            {allowed_methods, ["get", "post", "put", "delete"]},
            {content_types, ["application/json", "application/json-rpc"]},
            {rate_limit, [
                {per_second, 100},
                {burst, 1000}
            ]}
        ]},
        {inputs, [
            {file_paths, [
                {allow_absolute, false},
                {allowed_extensions, [".txt", ".md", ".json", ".yaml"]},
                {max_depth, 10}
            ]},
            {uri_schemes, ["http", "https", "file", "mcp"]},
            {command_args, [
                {allowed_commands, ["ls", "find", "grep"]},
                {max_args, 10}
            ]}
        ]}
    ]}
]}.
```

### 2. Sanitization Rules

```yaml
{erlmcp, [
    {sanitization, [
        {html, [
            {enabled, true},
            {allowed_tags, ["p", "b", "i", "u", "br"]},
            {allowed_attributes, []}
        ]},
        {sql, [
            {enabled, true},
            {parameterized_queries, true}
        ]},
        {xss, [
            {enabled, true},
            {strip_tags, true}
        ]}
    ]}
]}.
```

## Security Monitoring

### 1. Audit Logging

```yaml
{erlmcp, [
    {audit, [
        {enabled, true},
        {level, "info"},
        {format, "json"},
        {outputs, [
            {file, "/var/log/erlmcp/audit.log"},
            {syslog, [
                {host, "log.company.com"},
                {port, 514}
            ]}
        ]},
        {events, [
            "auth.success",
            "auth.failure",
            "resource.access",
            "tool.execute",
            "session.create",
            "session.delete",
            "system.admin"
        ]},
        {retention, 90}  % days
    ]}
]}.
```

### 2. Intrusion Detection

```yaml
{erlmcp, [
    {intrusion_detection, [
        {enabled, true},
        {rules, [
            {max_failed_auth, 5},
            {suspicious_ip_rate, 100},
            {large_download, 10485760},  % 10MB
            {command_injection, true}
        ]},
        {actions, [
            {log, true},
            {block, true},
            {notify, "security@company.com"}
        ]}
    ]}
]}.
```

### 3. Security Metrics

```yaml
{erlmcp, [
    {metrics, [
        {security, [
            {auth_attempts, true},
            {failed_auth, true},
            {blocked_requests, true},
            {suspicious_activities, true}
        ]}
    ]}
]}.
```

## Compliance Requirements

### 1. GDPR Compliance

```yaml
{erlmcp, [
    {compliance, [
        {gdpr, [
            {data_subject_rights, [
                "right_to_access",
                "right_to_rectification",
                "right_to_erasure",
                "right_to_restrict_processing"
            ]},
            {data_retention, [
                {personal_data, 30},
                {non_personal_data, 365}
            ]}
        ]}
    ]}
]}.
```

### 2. HIPAA Compliance

```yaml
{erlmcp, [
    {compliance, [
        {hipaa, [
            {encryption_required, true},
            {audit_required, true},
            {access_controls, true},
            {data_breach_notification, 72},  % hours
            {business_associate_agreements, true}
        ]}
    ]}
]}.
```

### 3. PCI DSS Compliance

```yaml
{erlmcp, [
    {compliance, [
        {pci_dss, [
            {card_data_handling, "prohibited"},
            {network_segmentation, true},
            {access_monitoring, true},
            {penetration_testing, quarterly}
        ]}
    ]}
]}.
```

## Configuration Management

### 1. Secure Configuration Management

```bash
# Environment variables for sensitive data
export ERLMCP_JWT_SECRET_KEY="$JWT_SECRET"
export ERLMCP_LDAP_PASSWORD="$LDAP_PASSWORD"
export ERLMCP_VAULT_TOKEN="$VAULT_TOKEN"

# Secure file permissions
chmod 600 /etc/erlmcp/ssl/server.key
chmod 600 /etc/erlmcp/secrets.conf
chown erlmcp:erlmcp /etc/erlmcp/*
```

### 2. Configuration Validation

```erlang
% Validate security configuration
validate_security_config() ->
    case application:get_env(erlmcp, ssl) of
        {ok, SslConfig} ->
            validate_ssl_config(SslConfig);
        undefined ->
            {error, ssl_not_configured}
    end.

validate_ssl_config(Config) ->
    %% Check required fields
    Required = [enabled, certfile, keyfile],
    case check_required_fields(Config, Required) of
        ok ->
            validate_cipher_suites(Config);
        {error, Missing} ->
            {error, {missing_fields, Missing}}
    end.
```

## Testing Security Configuration

### 1. Security Test Suite

```erlang
-include_lib("eunit/include/eunit.hrl").

security_config_test() ->
    %% Test JWT configuration
    jwt_config_test(),
    %% Test SSL configuration
    ssl_config_test(),
    %% Test authentication
    auth_config_test(),
    ok.

jwt_config_test() ->
    Config = application:get_env(erlmcp, jwt, []),
    ?assert(proplists:get_value(issuer, Config) /= undefined),
    ?assert(proplists:get_value(secret, Config) /= undefined),
    ok.
```

### 2. Penetration Testing

```bash
# Test SSL/TLS configuration
nmap --script ssl-enum-ciphers -p 443 erlmcp.company.com

# Test for common vulnerabilities
nikto -h erlmcp.company.com

# SQL injection test
curl -X POST "https://erlmcp.company.com/rpc" \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"sql_injection","arguments":"test\'"},"id":1}'
```

## Security Checklist

### Pre-Deployment Checklist

- [ ] JWT secret is 256+ bits
- [ ] SSL certificates are valid and not expired
- [ ] All ports are properly firewalled
- [ ] Network segmentation is in place
- [ ] RBAC roles are defined and tested
- [ ] Audit logging is enabled
- [ ] Data encryption is configured
- [ ] Input validation is implemented
- [ ] Rate limiting is configured
- [ ] Security headers are set

### Post-Deployment Checklist

- [ ] Run penetration testing
- [ ] Monitor security metrics
- [ ] Review audit logs
- [ ] Test backup and recovery
- [ ] Verify compliance requirements
- [ ] Update security policies
- [ ] Train security staff
- [ ] Document security procedures

## Troubleshooting Common Security Issues

### 1. Authentication Failures

```bash
# Check authentication logs
tail -f /var/log/erlmcp/auth.log

# Verify JWT configuration
erl -eval "application:start(erlmcp), io:format(\"JWT Config: ~p~n\", [application:get_env(erlmcp, jwt)]), halt()."
```

### 2. SSL Certificate Issues

```bash
# Check certificate expiration
openssl x509 -in /etc/erlmcp/ssl/server.crt -text -noout | grep "Not After"

# Test cipher suites
openssl s_client -connect erlmcp.company.com:443 -cipher AES256
```

### 3. Security Events

```bash
# Monitor security events
journalctl -u erlmcp -f | grep security

# Review audit log
grep "security" /var/log/erlmcp/audit.log
```

## Support

For security-related issues, please contact:
- Security Team: security@erlmcp.com
- Emergency Support: +1-800-SECURITY
- Documentation: https://docs.erlmcp.com/security

## References

- OWASP Top 10: https://owasp.org/Top10/
- NIST Cybersecurity Framework: https://www.nist.gov/cyberframework
- PCI DSS Requirements: https://www.pcisecuritystandards.org/security_standards/