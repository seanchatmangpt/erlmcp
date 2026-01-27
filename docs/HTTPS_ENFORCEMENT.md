# HTTPS Enforcement Configuration

**Gap #31: HTTPS Enforcement from MCP 2025-11-25 Compliance Review**

This document describes the HTTPS enforcement functionality in erlmcp, which implements MCP 2025-11-25 specification requirements for secure HTTP transport.

## Overview

The HTTPS enforcement module (`erlmcp_https_enforcer`) provides:

- **Configurable HTTPS enforcement** - Enable/disable HTTPS requirements
- **SSL/TLS certificate management** - Load and validate certificates
- **HTTP to HTTPS redirect** - Automatic redirect with HTTP 301 responses
- **HSTS header support** - HTTP Strict-Transport-Security headers
- **Certificate validation** - Verify certificate files exist and are readable
- **Mixed HTTP/HTTPS support** - Handle both protocols simultaneously

## Configuration

HTTPS enforcement is configured in `config/sys.config` via two configuration sections:

### HTTP Security Configuration

```erlang
{erlmcp, [
    {http_security, [
        %% Require HTTPS for all connections (default: false for development)
        {require_https, false},

        %% Redirect HTTP to HTTPS when require_https is true (default: true)
        {http_redirect_to_https, true},

        %% HTTP server bind address (default: 127.0.0.1)
        {http_bind_address, "127.0.0.1"},

        %% HTTPS server bind address (default: 0.0.0.0)
        {https_bind_address, "0.0.0.0"},

        %% Allowed origins for CORS validation
        {allowed_origins, [
            "https://example.com",
            "https://localhost:8443"
        ]}
    ]}
]}
```

### HTTPS/TLS Configuration

```erlang
{erlmcp, [
    {https_config, [
        %% Enable HTTPS support (default: false for development)
        {enabled, false},

        %% Path to SSL certificate file (PEM format)
        {certfile, "priv/cert.pem"},

        %% Path to SSL private key file (PEM format)
        {keyfile, "priv/key.pem"},

        %% Optional: Path to CA certificate chain file
        {cacertfile, undefined},

        %% Minimum TLS version (default: tlsv1.2)
        {min_tls_version, 'tlsv1.2'},

        %% Cipher suites (modern and secure)
        {ciphers, [
            "ECDHE-RSA-AES256-GCM-SHA384",
            "ECDHE-RSA-AES128-GCM-SHA256",
            "ECDHE-RSA-CHACHA20-POLY1305",
            "DHE-RSA-AES256-GCM-SHA384",
            "DHE-RSA-AES128-GCM-SHA256"
        ]},

        %% Enable HSTS (HTTP Strict-Transport-Security) header
        {enable_hsts, true},

        %% HSTS max age in seconds (default: 1 year = 31536000)
        {hsts_max_age, 31536000},

        %% Include subdomains in HSTS policy
        {hsts_include_subdomains, false},

        %% Client certificate verification mode
        {verify_mode, 'verify_none'},

        %% Enable SNI (Server Name Indication)
        {sni_enabled, true}
    ]}
]}
```

## Production Configuration

For production deployments, use this configuration:

```erlang
{erlmcp, [
    {http_security, [
        {require_https, true},           % ENFORCE HTTPS
        {http_redirect_to_https, true},  % Redirect HTTP to HTTPS
        {http_bind_address, "127.0.0.1"},% Bind HTTP to localhost only
        {https_bind_address, "0.0.0.0"}, % HTTPS on all interfaces
        {allowed_origins, [
            "https://your-domain.com",
            "https://api.your-domain.com"
        ]}
    ]},
    {https_config, [
        {enabled, true},                % ENABLE HTTPS
        {certfile, "/etc/erlmcp/certs/cert.pem"},
        {keyfile, "/etc/erlmcp/certs/key.pem"},
        {cacertfile, "/etc/erlmcp/certs/chain.pem"},
        {min_tls_version, 'tlsv1.2'},
        {enable_hsts, true},
        {hsts_max_age, 31536000},       % 1 year
        {hsts_include_subdomains, true},
        {sni_enabled, true}
    ]}
]}
```

## Development Configuration

For local development without HTTPS:

```erlang
{erlmcp, [
    {http_security, [
        {require_https, false},          % Allow HTTP in development
        {http_redirect_to_https, false}, % No redirect
        {http_bind_address, "127.0.0.1"},
        {https_bind_address, "127.0.0.1"}
    ]},
    {https_config, [
        {enabled, false}                 % HTTPS disabled
    ]}
]}
```

## API Usage

### Configuration Queries

```erlang
%% Get complete HTTPS configuration
Config = erlmcp_https_enforcer:get_config(),

%% Get specific configuration value
CertFile = erlmcp_https_enforcer:get_config(certfile),

%% Check if HTTPS enforcement is enabled
case erlmcp_https_enforcer:is_https_required() of
    true  -> io:format("HTTPS is required~n");
    false -> io:format("HTTP is allowed~n")
end,

%% Validate configuration
case erlmcp_https_enforcer:validate_config() of
    {ok, Config} -> io:format("Config valid: ~p~n", [Config]);
    {error, Reason} -> io:format("Config invalid: ~s~n", [Reason])
end
```

### Request Handling

```erlang
%% Determine if HTTP request should redirect to HTTPS
case erlmcp_https_enforcer:should_redirect_to_https(<<"http">>) of
    true ->
        %% Build redirect response
        {Status, Headers, Body} = erlmcp_https_enforcer:build_redirect_response(
            http, <<"example.com:8080">>),
        cowboy_req:reply(Status, Headers, Body, Req);
    false ->
        %% Handle normally
        handle_request(Req)
end,

%% Get HTTPS-related response headers
Headers = erlmcp_https_enforcer:get_https_headers(),
Response = maps:merge(BaseHeaders, Headers),

%% Extract host from request for redirect
case erlmcp_https_enforcer:extract_host_from_request(Req) of
    {ok, Host} ->
        Redirect = <<"https://", Host/binary, "/api">>,
        io:format("Redirecting to: ~s~n", [Redirect]);
    {error, Reason} ->
        io:format("Could not extract host: ~s~n", [Reason])
end
```

### Certificate Management

```erlang
%% Load SSL certificates for gun/cowboy
case erlmcp_https_enforcer:load_certificates() of
    {ok, SslOptions} ->
        io:format("SSL options loaded: ~p~n", [SslOptions]);
    {error, Reason} ->
        io:format("Failed to load certificates: ~s~n", [Reason])
end,

%% Check if certificate files are valid
case erlmcp_https_enforcer:is_certificate_valid() of
    true  -> io:format("Certificates valid~n");
    false -> io:format("Certificates invalid~n")
end,

%% Get certificate information
case erlmcp_https_enforcer:get_certificate_info() of
    {ok, Info} ->
        io:format("Certificate info: ~p~n", [Info]);
    {error, Reason} ->
        io:format("Could not get certificate info: ~s~n", [Reason])
end
```

### Protocol Detection

```erlang
%% Check if protocol is secure
case erlmcp_https_enforcer:is_secure_protocol(<<"https">>) of
    true  -> io:format("HTTPS is secure~n");
    false -> io:format("Not a secure protocol~n")
end,

%% Support various input formats
erlmcp_https_enforcer:is_secure_protocol(https),     % atom
erlmcp_https_enforcer:is_secure_protocol("https"),   % string
erlmcp_https_enforcer:is_secure_protocol(<<"https">>), % binary
```

## HTTP Redirect Flow

When HTTPS enforcement is enabled and HTTP is requested:

1. Client makes HTTP request to server
2. Server receives request with scheme = http
3. `should_redirect_to_https(<<"http">>)` returns true
4. Server builds 301 redirect response with:
   - Status: 301 (Moved Permanently)
   - Location header: `https://host/path`
   - HSTS header: `max-age=31536000; [includeSubDomains]`
5. Client follows redirect to HTTPS
6. Subsequent requests use HTTPS

### HTTP Response Example

```
HTTP/1.1 301 Moved Permanently
Location: https://example.com/
Strict-Transport-Security: max-age=31536000
Content-Type: text/plain
Content-Length: 82

301 Moved Permanently: This server requires HTTPS. Please use: https://example.com/
```

## HSTS (HTTP Strict-Transport-Security)

HSTS policy tells browsers to:
- Always use HTTPS for future requests
- Prevent downgrade attacks (HTTP forcing)
- Include subdomains if configured

### Configuration Notes

- **hsts_max_age**: Tells browsers how long to enforce HTTPS
  - Development: 300 (5 minutes)
  - Production: 31536000 (1 year)
- **hsts_include_subdomains**: Include all subdomains in policy
  - Only enable if ALL subdomains support HTTPS
  - Carefully consider before enabling in production

### HSTS Examples

```erlang
%% Short duration (development)
{hsts_max_age, 300}
% Header: Strict-Transport-Security: max-age=300

%% Long duration (production)
{hsts_max_age, 31536000}
% Header: Strict-Transport-Security: max-age=31536000; includeSubDomains

%% With subdomains
{hsts_include_subdomains, true}
% Header: Strict-Transport-Security: max-age=31536000; includeSubDomains
```

## Certificate Management

### Generating Self-Signed Certificates (Development Only)

```bash
# Generate private key
openssl genrsa -out priv/key.pem 2048

# Generate certificate (valid for 365 days)
openssl req -new -x509 -key priv/key.pem -out priv/cert.pem -days 365 \
    -subj "/C=US/ST=State/L=City/O=Org/CN=localhost"
```

### Production Certificates

For production, use certificates from trusted Certificate Authorities:

1. **Let's Encrypt** (free, automatic renewal)
   ```bash
   certbot certonly --standalone -d example.com
   # Certificate: /etc/letsencrypt/live/example.com/fullchain.pem
   # Key: /etc/letsencrypt/live/example.com/privkey.pem
   ```

2. **Commercial CA** (DigiCert, Sectigo, etc.)
   - Follow CA's certificate generation instructions
   - Ensure certificate includes full chain
   - Store key file securely with restricted permissions

### Certificate Permissions

Ensure proper file permissions:

```bash
# Certificate should be readable by erlmcp process
chmod 644 /etc/erlmcp/certs/cert.pem
chmod 644 /etc/erlmcp/certs/chain.pem

# Private key should be readable ONLY by erlmcp user
chmod 600 /etc/erlmcp/certs/key.pem
chown erlmcp:erlmcp /etc/erlmcp/certs/key.pem
```

## Security Best Practices

### 1. Enforce HTTPS in Production

```erlang
%% NEVER use this in production:
{require_https, false}

%% ALWAYS use this:
{require_https, true}
```

### 2. Use Strong TLS Versions

```erlang
%% Good: TLS 1.2 or higher
{min_tls_version, 'tlsv1.2'}

%% Better: TLS 1.3 only (if all clients support it)
{min_tls_version, 'tlsv1.3'}

%% DO NOT use:
{min_tls_version, 'tlsv1'}      % Deprecated
{min_tls_version, 'tlsv1.1'}     % Weak
```

### 3. Use Modern Cipher Suites

```erlang
%% Good cipher suites (from default config)
{ciphers, [
    "ECDHE-RSA-AES256-GCM-SHA384",      % Best
    "ECDHE-RSA-AES128-GCM-SHA256",      % Good
    "ECDHE-RSA-CHACHA20-POLY1305",      % Modern
    "DHE-RSA-AES256-GCM-SHA384",        % Fallback
    "DHE-RSA-AES128-GCM-SHA256"         % Fallback
]}

%% Avoid weak ciphers:
%% - DES, 3DES
%% - RC4
%% - MD5
%% - NULL
```

### 4. Enable HSTS in Production

```erlang
%% ALWAYS enable in production
{enable_hsts, true}
{hsts_max_age, 31536000}  % 1 year

%% Gradually increase:
%% Week 1: 300 seconds (test)
%% Week 2: 3600 seconds (1 hour)
%% Week 3: 86400 seconds (1 day)
%% Production: 31536000 seconds (1 year)
```

### 5. Bind HTTP to Localhost Only

```erlang
%% Development/Local only:
{http_bind_address, "127.0.0.1"}    % Only localhost

%% NEVER use in production:
{http_bind_address, "0.0.0.0"}      % Accessible from anywhere
```

### 6. Monitor Certificate Expiration

```bash
# Check certificate expiration
openssl x509 -in /etc/erlmcp/certs/cert.pem -text -noout | grep -A2 "Validity"

# Set up monitoring/alerts
# Check expiration at least monthly
# Plan renewal 30+ days before expiration
```

## Testing

### Unit Tests

```bash
# Run erlmcp_https_enforcer tests
rebar3 eunit -m erlmcp_https_enforcer_tests
```

### Common Test Suite

```bash
# Run full CT suite
rebar3 ct --suite=erlmcp_https_enforcer_SUITE

# Run specific test group
rebar3 ct --suite=erlmcp_https_enforcer_SUITE --group https_enforcement
```

### Manual Testing

```erlang
%% 1. Start erlmcp with HTTPS enforcement
application:ensure_all_started(erlmcp),

%% 2. Check configuration
erlmcp_https_enforcer:get_config(),

%% 3. Verify enforcement
erlmcp_https_enforcer:is_https_required(),
erlmcp_https_enforcer:is_https_enabled(),

%% 4. Test redirect logic
erlmcp_https_enforcer:should_redirect_to_https(<<"http">>),
erlmcp_https_enforcer:should_redirect_to_https(<<"https">>),

%% 5. Get redirect response
{Status, Headers, Body} = erlmcp_https_enforcer:build_redirect_response(
    http, <<"example.com">>),
io:format("Status: ~p~nHeaders: ~p~nBody: ~s~n", [Status, Headers, Body]).
```

### Integration with Gun/Cowboy

When using gun or cowboy for HTTP handling:

```erlang
%% In gun-based HTTP client
case erlmcp_https_enforcer:should_redirect_to_https(<<"http">>) of
    true ->
        %% Switch to HTTPS
        {ok, HttpsPid} = gun:open("example.com", 443, #{
            transport => ssl,
            tls_opts => erlmcp_https_enforcer:get_ssl_options()
        });
    false ->
        %% Continue with HTTP
        {ok, HttpPid} = gun:open("example.com", 80, #{transport => tcp})
end.
```

## Troubleshooting

### Certificate Loading Fails

**Problem**: "Cannot read cert file" error

**Solution**:
1. Verify file path is correct
2. Check file permissions (must be readable by erlmcp process)
3. Check file format (must be PEM)
4. Verify file is not corrupted

```bash
# Check certificate format
openssl x509 -in /path/to/cert.pem -text -noout

# Verify key matches certificate
openssl x509 -noout -modulus -in cert.pem | openssl md5
openssl rsa -noout -modulus -in key.pem | openssl md5
# Both should produce the same hash
```

### HTTPS Enforcement Not Working

**Problem**: HTTP requests are not being redirected

**Solution**:
1. Verify both `require_https` and `enabled` are `true`
2. Check that `should_redirect_to_https/1` returns true
3. Ensure HTTP handler calls `build_redirect_response/2`
4. Verify browser follows 301 redirects (check F12 network tab)

### HSTS Not Being Applied

**Problem**: HSTS header is not present

**Solution**:
1. Verify `enable_hsts` is `true` in https_config
2. Ensure HTTPS is enabled (`enabled: true`)
3. Check response headers contain `strict-transport-security`
4. Browser HSTS max age: Check Chrome DevTools > Application > Cookies > HSTS

### Mixed Content Warnings in Browser

**Problem**: Browser shows "Mixed Content" warnings

**Solution**:
1. Ensure ALL resources (scripts, CSS, images) use HTTPS
2. Update any hardcoded `http://` URLs to `https://`
3. Use relative URLs (e.g., `/api/resource` instead of `http://host/api/resource`)
4. Enable HSTS to prevent future mixed content

## Performance Considerations

1. **Certificate Loading**: Done at initialization, not per-request
2. **HSTS Header Generation**: Efficient binary operations, < 1ms
3. **Redirect Logic**: Simple boolean checks, negligible overhead
4. **SSL/TLS Handshake**: Added per-HTTPS-connection (one-time cost)

## Compliance

This implementation satisfies:

- **MCP 2025-11-25 Specification**: Gap #31 (HTTPS Enforcement)
- **RFC 6797**: HTTP Strict-Transport-Security (HSTS)
- **TLS 1.2+**: Minimum TLS version security
- **OWASP**: Security best practices for HTTPS

## See Also

- [MCP 2025-11-25 Specification](https://spec.modelcontextprotocol.io)
- [RFC 6797 - HTTP Strict-Transport-Security](https://tools.ietf.org/html/rfc6797)
- [OWASP Transport Layer Protection](https://owasp.org/www-community/attacks/Man-in-the-middle_attack)
- [Erlang SSL/TLS Documentation](https://www.erlang.org/doc/man/ssl.html)
