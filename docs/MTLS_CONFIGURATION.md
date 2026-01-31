# mTLS Certificate Validation Configuration Guide

## Overview

The `erlmcp_auth` module provides production-grade mTLS (mutual TLS) certificate validation with comprehensive security features:

- **Peer Certificate Extraction**: Automatic extraction from SSL/TLS sockets
- **X.509 Chain Validation**: Verification against trusted CAs
- **Expiration Checking**: Validates notBefore and notAfter timestamps
- **Revocation Checking**: OCSP (RFC 6960) and CRL (RFC 5280) support
- **Subject DN Matching**: Pattern-based CN validation with wildcards
- **Depth Limit**: Configurable certificate chain depth limits

## Quick Start

### Basic Configuration

```erlang
%% Minimal mTLS configuration - just enable
MtlsConfig = #{
    enabled => true
}.

erlmcp_auth:start_link(#{
    mtls => MtlsConfig
}).
```

### Production Configuration

```erlang
%% Full mTLS configuration with all features
MtlsConfig = #{
    %% Enable mTLS authentication
    enabled => true,

    %% Trusted CA certificates (DER format)
    trusted_cas => [
        read_file("priv/certs/ca-cert.der"),
        read_file("priv/certs/intermediate-ca.der")
    ],

    %% Allowed CN patterns (supports wildcards)
    allowed_cn_patterns => [
        <<"*.example.com">>,
        <<"client-*.production.example.com">>,
        <<"specific-client.example.com">>
    ],

    %% Certificate chain depth limit (default: 10)
    depth_limit => 5,

    %% OCSP (Online Certificate Status Protocol) configuration
    ocsp_enabled => true,
    ocsp_url => <<"http://ocsp.example.com">>,
    ocsp_timeout => 5000,  % milliseconds
    ocsp_fail_open => true,  % Allow if OCSP unavailable

    %% CRL (Certificate Revocation List) configuration
    crl_enabled => false,  % Disable if using OCSP
    crl_url => <<"http://crl.example.com/cert.crl">>,
    crl_timeout => 10000,  % milliseconds
    crl_fail_open => true  % Allow if CRL unavailable
}.

erlmcp_auth:start_link(#{
    mtls => MtlsConfig
}).
```

## Certificate Formats

### DER Format (Recommended)

DER (Distinguished Encoding Rules) is the binary X.509 certificate format:

```erlang
%% Load DER certificate from file
{ok, CertDer} = file:read_file("priv/certs/ca-cert.der").

%% Use in configuration
trusted_cas => [CertDer]
```

### PEM Format (Convert to DER)

PEM (Privacy Enhanced Mail) is base64-encoded DER with headers:

```erlang
%% Convert PEM to DER
{ok, PemData} = file:read_file("priv/certs/ca-cert.pem"),
[{'Certificate', DerCert, not_encrypted}] = public_key:pem_decode(PemData),

%% Use in configuration
trusted_cas => [DerCert]
```

### Extract from SSL Connection

The system automatically extracts peer certificates from SSL sockets:

```erlang
%% Server-side: Configure SSL to request client cert
SslOpts = [
    {verify, verify_peer},
    {fail_if_no_peer_cert, true},
    {cacertfile, "priv/certs/ca-cert.pem"},
    {certfile, "priv/certs/server-cert.pem"},
    {keyfile, "priv/certs/server-key.pem"}
],

{ok, ListenSocket} = ssl:listen(8443, SslOpts),
{ok, SslSocket} = ssl:transport_accept(ListenSocket),

%% Authenticate with mTLS
erlmcp_auth:authenticate(mtls, #{
    ssl_socket => SslSocket
}).
```

## CN Pattern Matching

Common Name (CN) patterns support wildcards for flexible matching:

### Wildcard Patterns

```erlang
allowed_cn_patterns => [
    %% Exact match
    <<"client.example.com">>,

    %% Prefix wildcard: matches any.example.com
    <<"*.example.com">>,

    %% Suffix wildcard: matches client-anything
    <<"client-*">>,

    %% Infix wildcard: matches client-foo-prod
    <<"client-*-prod">>
]
```

### Pattern Examples

| Pattern                  | Matches                                 | Doesn't Match            |
|--------------------------|----------------------------------------|--------------------------|
| `*.example.com`          | `foo.example.com`, `bar.example.com`   | `example.com`, `foo.org` |
| `client-*`               | `client-123`, `client-prod`            | `server-123`             |
| `client-*-prod`          | `client-123-prod`, `client-foo-prod`   | `client-prod`, `prod`    |
| `specific.example.com`   | `specific.example.com`                 | `other.example.com`      |

## Certificate Chain Validation

### Chain Depth Limit

Limit certificate chain depth to prevent abuse:

```erlang
%% Maximum chain depth (default: 10)
depth_limit => 5
```

Chain depth includes:
1. End-entity (peer) certificate
2. Intermediate CA certificate(s)
3. Root CA certificate

Example chain (depth = 3):
```
Peer Cert → Intermediate CA → Root CA
```

### Trusted CAs

Provide trusted CA certificates in DER format:

```erlang
trusted_cas => [
    RootCADer,
    IntermediateCA1Der,
    IntermediateCA2Der
]
```

If no trusted CAs are configured, chain validation is skipped (not recommended for production).

## Revocation Checking

### OCSP (Online Certificate Status Protocol)

Real-time revocation checking via OCSP responder:

```erlang
ocsp_enabled => true,
ocsp_url => <<"http://ocsp.example.com">>,
ocsp_timeout => 5000,  % 5 seconds
ocsp_fail_open => true  % Allow if OCSP unavailable
```

**Fail-Open vs Fail-Closed**:
- `ocsp_fail_open => true`: Allow authentication if OCSP responder unreachable (default)
- `ocsp_fail_open => false`: Reject authentication if OCSP responder unreachable (stricter)

### CRL (Certificate Revocation List)

Download and check certificate against CRL:

```erlang
crl_enabled => true,
crl_url => <<"http://crl.example.com/cert.crl">>,
crl_timeout => 10000,  % 10 seconds
crl_fail_open => true  % Allow if CRL unavailable
```

**When to use**:
- Use OCSP for real-time checks (preferred)
- Use CRL for offline/batch validation
- Don't enable both (OCSP takes precedence)

### Implementation Status

**Current**: OCSP and CRL checking infrastructure is in place with soft-fail support.

**Future**: Full OCSP/CRL implementation requires:
1. OCSP request/response encoding/decoding (`public_key:ocsp_*`)
2. CRL download and parsing (`public_key:pkix_crl_*`)
3. Signature validation
4. Production HTTP client integration

See `erlmcp_auth_mtls.erl` for implementation placeholders.

## Authentication Flow

### 1. API-Based Authentication

```erlang
%% Pre-extracted certificate DER
CertInfo = #{
    certificate => CertDer,
    cert_chain => [CertDer, IntermediateDer, RootDer]
},

%% Authenticate
{ok, SessionId} = erlmcp_auth:authenticate(mtls, CertInfo).
```

### 2. SSL Socket-Based Authentication

```erlang
%% SSL socket with peer certificate
CertInfo = #{
    ssl_socket => SslSocket
},

%% Authenticate (extracts cert automatically)
{ok, SessionId} = erlmcp_auth:authenticate(mtls, CertInfo).
```

### 3. Legacy Format (Backwards Compatibility)

```erlang
%% Legacy format (no X.509 validation)
CertInfo = #{
    subject => #{
        cn => <<"client.example.com">>
    }
},

%% Authenticate (skips chain validation)
{ok, SessionId} = erlmcp_auth:authenticate(mtls, CertInfo).
```

## Error Handling

### Common Errors

| Error                            | Cause                                      | Solution                              |
|----------------------------------|--------------------------------------------|---------------------------------------|
| `mtls_not_configured`            | mTLS not enabled in config                 | Set `enabled => true`                 |
| `no_certificate_provided`        | No certificate in CertInfo                 | Provide `certificate` or `ssl_socket` |
| `invalid_ssl_socket`             | Invalid SSL socket                         | Verify socket is valid SSL connection |
| `no_peer_certificate`            | SSL socket has no peer cert                | Configure SSL with `verify_peer`      |
| `certificate_expired`            | Certificate expired                        | Renew certificate                     |
| `certificate_not_yet_valid`      | Certificate not yet valid (future notBefore) | Check system clock                  |
| `untrusted_certificate_chain`    | Certificate not signed by trusted CA       | Add CA to `trusted_cas`               |
| `certificate_chain_too_deep`     | Chain depth exceeds limit                  | Increase `depth_limit` or fix chain   |
| `cn_not_allowed`                 | CN doesn't match allowed patterns          | Update `allowed_cn_patterns`          |
| `certificate_revoked`            | Certificate revoked (OCSP/CRL)             | Revoke client access                  |

### Error Logging

All validation failures are logged with detailed context:

```erlang
%% Enable debug logging
logger:set_module_level(erlmcp_auth_mtls, debug),
logger:set_module_level(erlmcp_mtls_validator, debug).

%% Logs include:
%% - Certificate extraction details
%% - Chain depth and validation steps
%% - OCSP/CRL check results
%% - Error reasons and stack traces
```

## Security Best Practices

### 1. Always Verify Peer Certificates

```erlang
%% SSL server configuration
SslOpts = [
    {verify, verify_peer},
    {fail_if_no_peer_cert, true},  % Require client cert
    {depth, 5},                     % Chain depth limit
    {cacertfile, "priv/certs/ca-cert.pem"}
]
```

### 2. Use Strict CN Patterns

```erlang
%% Good: Specific patterns
allowed_cn_patterns => [
    <<"client-prod-123.example.com">>,
    <<"client-prod-*.example.com">>
]

%% Bad: Too permissive
allowed_cn_patterns => [
    <<"*">>  % Matches anything!
]
```

### 3. Enable Revocation Checking

```erlang
%% Enable OCSP for real-time revocation
ocsp_enabled => true,
ocsp_url => <<"http://ocsp.example.com">>,
ocsp_fail_open => false  % Strict: reject if OCSP unavailable
```

### 4. Set Appropriate Depth Limits

```erlang
%% Limit chain depth to prevent abuse
depth_limit => 5  % Typical: peer + 1-2 intermediates + root
```

### 5. Use Separate CAs for mTLS

```erlang
%% Dedicated CA for client certificates
trusted_cas => [
    ClientCADer  % Not the same as server CA
]
```

## Testing

### Generate Test Certificates

```bash
# Generate CA key and certificate
openssl genrsa -out ca-key.pem 2048
openssl req -new -x509 -key ca-key.pem -out ca-cert.pem -days 365 \
    -subj "/CN=Test CA"

# Generate client key and CSR
openssl genrsa -out client-key.pem 2048
openssl req -new -key client-key.pem -out client.csr \
    -subj "/CN=client.example.com"

# Sign client certificate with CA
openssl x509 -req -in client.csr -CA ca-cert.pem -CAkey ca-key.pem \
    -CAcreateserial -out client-cert.pem -days 365

# Convert to DER
openssl x509 -in ca-cert.pem -outform DER -out ca-cert.der
openssl x509 -in client-cert.pem -outform DER -out client-cert.der
```

### Test Authentication

```erlang
%% Load test certificates
{ok, CADer} = file:read_file("priv/certs/ca-cert.der"),
{ok, ClientDer} = file:read_file("priv/certs/client-cert.der"),

%% Configure mTLS
erlmcp_auth:start_link(#{
    mtls => #{
        enabled => true,
        trusted_cas => [CADer],
        allowed_cn_patterns => [<<"client.example.com">>]
    }
}),

%% Test authentication
CertInfo = #{certificate => ClientDer},
{ok, SessionId} = erlmcp_auth:authenticate(mtls, CertInfo).
```

## Performance Considerations

### Certificate Caching

Certificates are validated on every authentication. For high-traffic systems:

```erlang
%% Consider session caching
SessionTTL = 3600,  % 1 hour

%% Reuse sessions instead of re-authenticating
case erlmcp_auth:validate_session(SessionId) of
    {ok, _Session} ->
        %% Session valid, no need to re-authenticate
        ok;
    {error, _} ->
        %% Session expired, re-authenticate
        {ok, NewSessionId} = erlmcp_auth:authenticate(mtls, CertInfo)
end
```

### OCSP/CRL Performance

- **OCSP**: Adds ~50-500ms per authentication (network latency)
- **CRL**: Adds ~100-1000ms for download + parsing

**Optimization**:
- Cache OCSP responses (TTL from response)
- Cache CRLs (TTL from nextUpdate field)
- Use `fail_open => true` for graceful degradation

## Troubleshooting

### Enable Debug Logging

```erlang
logger:set_module_level(erlmcp_auth, debug),
logger:set_module_level(erlmcp_auth_mtls, debug),
logger:set_module_level(erlmcp_mtls_validator, debug).
```

### Verify Certificate Chain

```bash
# Verify client cert against CA
openssl verify -CAfile ca-cert.pem client-cert.pem

# View certificate details
openssl x509 -in client-cert.pem -text -noout
```

### Check SSL Connection

```erlang
%% Verify SSL socket has peer certificate
{ok, CertDer} = ssl:peercert(SslSocket),

%% Verify certificate chain available
{ok, CertChain} = ssl:peercert(SslSocket, []),
length(CertChain).  % Should be > 1
```

## References

- **RFC 5280**: X.509 Public Key Infrastructure Certificate and CRL Profile
- **RFC 6960**: Online Certificate Status Protocol (OCSP)
- **Erlang/OTP ssl**: https://www.erlang.org/doc/man/ssl.html
- **Erlang/OTP public_key**: https://www.erlang.org/doc/man/public_key.html
- **erlmcp_mtls_validator**: X.509 certificate validation module
- **erlmcp_auth_mtls**: Comprehensive mTLS authentication module

## Support

For issues or questions:
1. Check error logs with debug logging enabled
2. Verify certificate chain with OpenSSL
3. Test with simple configuration first (no OCSP/CRL)
4. Review `erlmcp_auth_mtls.erl` implementation
5. File issue with full config and error logs
