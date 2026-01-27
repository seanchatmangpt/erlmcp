# erlmcp Production Deployment Security Checklist

**Document Type:** Deployment Guide
**Version:** 1.0
**Date:** January 27, 2026
**Scope:** MCP 2025-11-25 Compliance

---

## Pre-Deployment Phase

### Certificate & Cryptography Setup

- [ ] **Generate SSL/TLS Certificates**
  - [ ] Use OpenSSL 3.x or newer
  - [ ] Minimum 2048-bit RSA or P-256 ECDSA
  - [ ] Certificate CN matches deployment hostname
  - [ ] Certificate not self-signed in production (use trusted CA)
  - [ ] Certificate valid for at least 1 year
  - [ ] Private key generated with strong entropy

  ```bash
  # Generate Certificate Signing Request (CSR)
  openssl req -new -newkey rsa:2048 -keyout /etc/erlmcp/certs/key.pem \
    -out /etc/erlmcp/certs/cert.csr \
    -subj "/CN=erlmcp.example.com/O=YourOrg/C=US"

  # Submit CSR to trusted CA, receive signed cert.pem
  # Verify certificate:
  openssl x509 -in /etc/erlmcp/certs/cert.pem -text -noout
  ```

- [ ] **Certificate File Permissions**
  - [ ] Certificate file: 644 (readable by all, writable by owner)
  - [ ] Private key file: 600 (readable/writable by owner only)
  - [ ] Owner: erlmcp service user (not root)

  ```bash
  sudo chown erlmcp:erlmcp /etc/erlmcp/certs/*
  sudo chmod 644 /etc/erlmcp/certs/cert.pem
  sudo chmod 600 /etc/erlmcp/certs/key.pem
  ```

- [ ] **CA Bundle Configuration**
  - [ ] If using self-signed CA internally, provide CA bundle
  - [ ] Path configured in `cacertfile` in sys.config
  - [ ] CA certificate is valid and trusted

  ```erlang
  {https_config, [
      {cacertfile, "/etc/erlmcp/certs/ca-bundle.pem"}
  ]}
  ```

### Network Configuration

- [ ] **Localhost Binding Verification**
  - [ ] For local-only deployments: `enforce_localhost_only = true`
  - [ ] HTTP bind address: `"127.0.0.1"` (not `0.0.0.0`)
  - [ ] IPv6 bind address: `"::1"` (not `::`)
  - [ ] Test binding with: `netstat -tlnp | grep 8080`

  ```bash
  # Verify localhost-only binding
  $ netstat -tlnp | grep erlmcp
  tcp    0  0 127.0.0.1:8080    0.0.0.0:*    LISTEN    12345/erlmcp
  tcp6   0  0 ::1:8080          :::*         LISTEN    12345/erlmcp

  # Verify NOT listening on 0.0.0.0
  $ netstat -tlnp | grep 0.0.0.0:8080
  [no output - good!]
  ```

- [ ] **Firewall Configuration**
  - [ ] If binding to non-localhost, implement firewall rules
  - [ ] Allow only from authorized IP ranges
  - [ ] Block all other inbound connections

  ```bash
  # Example iptables rules
  sudo iptables -A INPUT -p tcp --dport 8080 -s 192.168.1.0/24 -j ACCEPT
  sudo iptables -A INPUT -p tcp --dport 8080 -j DROP
  ```

- [ ] **DNS Configuration**
  - [ ] If using DNS names, ensure proper resolution
  - [ ] Test: `nslookup erlmcp.example.com`
  - [ ] Verify A and AAAA records if supporting IPv6

### HTTPS/TLS Configuration

- [ ] **Minimum TLS Version**
  - [ ] Set `min_tls_version` to `'tlsv1.2'` minimum
  - [ ] Prefer `'tlsv1.3'` if supported by clients

  ```erlang
  {https_config, [
      {min_tls_version, 'tlsv1.3'}  % TLS 1.3 preferred
  ]}
  ```

- [ ] **Cipher Suite Verification**
  - [ ] Only AEAD ciphers enabled (GCM, Poly1305)
  - [ ] Forward secrecy required (ECDHE, DHE)
  - [ ] Weak ciphers explicitly disabled

  ```erlang
  {https_config, [
      {ciphers, [
          "ECDHE-ECDSA-AES256-GCM-SHA384",
          "ECDHE-RSA-AES256-GCM-SHA384",
          "ECDHE-ECDSA-CHACHA20-POLY1305",
          "ECDHE-RSA-CHACHA20-POLY1305",
          "ECDHE-ECDSA-AES128-GCM-SHA256",
          "ECDHE-RSA-AES128-GCM-SHA256"
      ]}
  ]}
  ```

- [ ] **HTTPS Enforcement**
  - [ ] Set `require_https = true` in http_security config
  - [ ] HTTP requests redirect to HTTPS (301 Moved Permanently)
  - [ ] HSTS header enabled with 1-year max-age

  ```erlang
  {http_security, [
      {require_https, true},
      {http_redirect_to_https, true}
  ]},
  {https_config, [
      {enable_hsts, true},
      {hsts_max_age, 31536000},  % 1 year in seconds
      {hsts_include_subdomains, true}
  ]}
  ```

### Origin Whitelist Configuration

- [ ] **Review Default Origins**
  - [ ] Default includes: `http://127.0.0.1:*`, `http://localhost:*`, `http://[::1]:*`
  - [ ] Remove HTTPS versions if not needed
  - [ ] Add HTTPS versions if required

  ```erlang
  {http_security, [
      {allowed_origins, [
          "https://app.example.com",        % Production client
          "https://app-staging.example.com", % Staging client
          "https://127.0.0.1:3000",         % Local development
          "https://localhost:3000"          % Local development
      ]}
  ]}
  ```

- [ ] **Test Origin Validation**
  - [ ] Request from whitelisted origin: Should succeed (200)
  - [ ] Request from non-whitelisted origin: Should fail (403 Forbidden)

  ```bash
  # Test whitelisted origin
  curl -H "Origin: https://app.example.com" \
    https://erlmcp.example.com/mcp
  # Expected: 200 OK

  # Test non-whitelisted origin
  curl -H "Origin: https://attacker.com" \
    https://erlmcp.example.com/mcp
  # Expected: 403 Forbidden
  ```

### Session Configuration

- [ ] **Session Timeout**
  - [ ] Set appropriate timeout for use case
  - [ ] Default: 1800 seconds (30 minutes)
  - [ ] Shorter for high-security scenarios (300s = 5 min)
  - [ ] Longer for background tasks (3600s = 1 hour)

  ```erlang
  {session_manager, [
      {timeout, 1800},  % 30 minutes for interactive users
      {cleanup_interval, 300000}  % Cleanup every 5 minutes
  ]}
  ```

- [ ] **Cleanup Configuration**
  - [ ] Cleanup interval: default 300000ms (5 minutes)
  - [ ] Ensure not too frequent (performance) or too infrequent (memory)

### Message Size Configuration

- [ ] **Set Appropriate Limits**
  - [ ] Default: 16 MB for all transports
  - [ ] Review for your use case:
    - [ ] Small API calls: 1-4 MB
    - [ ] Document uploads: 10-50 MB
    - [ ] Multimedia streams: 50-100+ MB

  ```erlang
  {message_size_limits, #{
      http_body => 16777216,    % 16 MB
      websocket => 8388608,     % 8 MB (can be smaller)
      tcp => 16777216,
      stdio => 16777216,
      sse => 16777216
  }}
  ```

- [ ] **Test Size Limits**
  - [ ] Send message at limit: Should succeed
  - [ ] Send message exceeding limit: Should fail with 413

  ```bash
  # Create 16 MB file (at limit)
  dd if=/dev/zero bs=1M count=16 of=/tmp/test_16mb.bin

  # Send (should succeed)
  curl -d @/tmp/test_16mb.bin \
    https://erlmcp.example.com/mcp

  # Create 17 MB file (over limit)
  dd if=/dev/zero bs=1M count=17 of=/tmp/test_17mb.bin

  # Send (should fail with 413)
  curl -d @/tmp/test_17mb.bin \
    https://erlmcp.example.com/mcp
  # Expected: 413 Payload Too Large
  ```

---

## Deployment Phase

### Application Startup

- [ ] **Start erlmcp with Production Config**
  - [ ] Use separate `sys.config` for production
  - [ ] Verify no development/debug settings

  ```bash
  # Start with production configuration
  erl -config /etc/erlmcp/sys.config \
      -boot /opt/erlmcp/releases/erlmcp/erlmcp \
      -noinput -detached
  ```

- [ ] **Verify Application Started Successfully**
  - [ ] Check process running: `ps aux | grep erl`
  - [ ] Check listening ports: `netstat -tlnp | grep erlmcp`
  - [ ] Check logs: `tail -f /var/log/erlmcp/erlmcp.log`

  ```bash
  # Verify startup
  $ ps aux | grep erl
  erlmcp   12345  1.5  0.8 1234567 123456 ?  Ssl  10:00 0:05 erlmcp

  $ netstat -tlnp | grep erlmcp
  tcp    0  0 127.0.0.1:8080    0.0.0.0:*    LISTEN    12345/erlmcp
  tcp6   0  0 ::1:8080          :::*         LISTEN    12345/erlmcp
  ```

- [ ] **Test Basic Connectivity**
  ```bash
  # Test HTTP redirect to HTTPS
  curl -i http://127.0.0.1:8080/
  # Expected: 301 Moved Permanently to https://127.0.0.1/

  # Test HTTPS endpoint
  curl -i https://127.0.0.1:8080/ --insecure
  # Expected: 200 or appropriate MCP response (--insecure for self-signed cert)
  ```

### Security Verification Tests

- [ ] **Test Origin Validation**
  ```bash
  # Whitelisted origin should work
  curl -H "Origin: https://app.example.com" \
    https://erlmcp.example.com/mcp \
    --insecure -i
  # Expected: 200 or MCP protocol response

  # Non-whitelisted origin should fail
  curl -H "Origin: https://attacker.com" \
    https://erlmcp.example.com/mcp \
    --insecure -i
  # Expected: 403 Forbidden
  ```

- [ ] **Test HTTPS Enforcement**
  ```bash
  # HTTP request should redirect
  curl -i http://erlmcp.example.com/mcp
  # Expected: 301 Moved Permanently with HSTS header

  # Verify HSTS header present
  curl -i https://erlmcp.example.com/mcp --insecure | grep -i "strict-transport"
  # Expected: strict-transport-security: max-age=31536000
  ```

- [ ] **Test Path Canonicalization**
  ```bash
  # Test traversal prevention
  curl "https://erlmcp.example.com/resources?path=../../../../etc/passwd" \
    --insecure
  # Expected: 403 Forbidden or "path outside allowed directories"

  # Test symlink handling
  # (requires creating test symlink in resources directory)
  curl "https://erlmcp.example.com/resources?path=symlink_to_secret" \
    --insecure
  # Expected: 403 Forbidden if pointing outside boundary
  ```

- [ ] **Test Message Size Limits**
  ```bash
  # Create large file
  dd if=/dev/zero bs=1M count=20 of=/tmp/large.bin

  # Send oversized message
  curl -d @/tmp/large.bin \
    https://erlmcp.example.com/mcp \
    --insecure -i
  # Expected: 413 Payload Too Large
  ```

- [ ] **Test TLS Configuration**
  ```bash
  # Check TLS version and ciphers
  openssl s_client -connect erlmcp.example.com:8080 \
    -tls1_2 -tls1_3
  # Expected: TLS 1.2 or 1.3 connection established

  # Verify weak ciphers are rejected
  openssl s_client -connect erlmcp.example.com:8080 \
    -cipher 'DES-CBC3-SHA'
  # Expected: No ciphers available to connect
  ```

### Monitoring & Logging Setup

- [ ] **Configure Logging**
  - [ ] Set log level: info (not debug in production)
  - [ ] Log to file with rotation
  - [ ] Include security events (origin validation, path errors, size rejections)

  ```erlang
  {kernel, [
      {logger, [
          {default_level, info},
          {handlers, [
              {file, file, #{
                  config => #{
                      file => "/var/log/erlmcp/erlmcp.log",
                      max_size => 52428800,  % 50 MB
                      max_num => 10           % Keep 10 rotated files
                  }
              }}
          ]}
      ]}
  ]}
  ```

- [ ] **Monitor Key Metrics**
  - [ ] Track 403 responses (blocked origins)
  - [ ] Track 413 responses (oversized messages)
  - [ ] Track failed path validation errors
  - [ ] Track session creation rate
  - [ ] Track error rates

  ```bash
  # Example: Monitor for attacks
  tail -f /var/log/erlmcp/erlmcp.log | grep -E "403|413|traversal|origin.*rejected"
  ```

- [ ] **Set Up Alerting**
  - [ ] Alert on spike in 403 Forbidden (potential attack)
  - [ ] Alert on spike in 413 Payload Too Large (potential DoS)
  - [ ] Alert on application startup failures
  - [ ] Alert on TLS certificate expiration (30 days before)

---

## Post-Deployment Operations

### Daily Operations

- [ ] **Daily Log Review**
  - [ ] Check for security-related errors
  - [ ] Look for patterns in rejected origins or requests
  - [ ] Check for unusual session creation patterns

  ```bash
  # Review security events
  grep -E "forbidden|rejected|401|403" /var/log/erlmcp/erlmcp.log | tail -100
  ```

- [ ] **Monitor Resource Usage**
  - [ ] Check memory usage (sessions grow linearly)
  - [ ] Check CPU usage (should be idle when no requests)
  - [ ] Check disk space (log rotation)

  ```bash
  # Check process resource usage
  ps aux | grep erlmcp | head -1
  # Look for: reasonable memory, low CPU
  ```

- [ ] **Verify Service Health**
  - [ ] Test basic connectivity: `curl https://erlmcp.example.com/ --insecure`
  - [ ] Verify TLS is working
  - [ ] Check no error spikes in logs

### Weekly Operations

- [ ] **Security Log Analysis**
  - [ ] Analyze failed origin validation patterns
  - [ ] Look for brute force attempts (repeated failures)
  - [ ] Check for unusual request patterns

  ```bash
  # Analyze failed origins (potential attacks)
  grep "origin.*rejected\|forbidden" /var/log/erlmcp/erlmcp.log | \
    cut -d' ' -f5 | sort | uniq -c | sort -rn
  ```

- [ ] **Performance Review**
  - [ ] Session cleanup happening regularly
  - [ ] No memory leaks (memory stable)
  - [ ] Response times acceptable

  ```erlang
  % Check session counts
  erlmcp_session_manager:cleanup_expired_sessions().
  % Returns {ok, DeletedCount}
  ```

- [ ] **Certificate Expiration Check**
  - [ ] Check days until certificate expires
  - [ ] Plan renewal if < 30 days remaining

  ```bash
  # Check certificate expiration
  openssl x509 -in /etc/erlmcp/certs/cert.pem -noout -dates
  # Output: notBefore=... notAfter=...

  # Days remaining
  echo $(($(date -d "$(openssl x509 -in /etc/erlmcp/certs/cert.pem \
    -noout -enddate | cut -d= -f2)" +%s) - $(date +%s))) / 86400 | bc
  ```

### Monthly Operations

- [ ] **Security Audit**
  - [ ] Review all configuration changes
  - [ ] Verify no new vulnerabilities in dependencies
  - [ ] Test recovery procedures

- [ ] **Dependency Updates**
  - [ ] Check for Erlang/OTP updates
  - [ ] Check for OpenSSL updates
  - [ ] Test in staging before applying to production

  ```bash
  # Check Erlang version
  erl -eval 'erlang:system_info(otp_release)' -noshell -s init stop

  # Check for vulnerabilities
  # Subscribe to: erlang-security mailing list, erlang.org advisories
  ```

- [ ] **Backup Verification**
  - [ ] Verify certificate backups in secure location
  - [ ] Verify configuration backups
  - [ ] Test restore procedure

  ```bash
  # Backup sensitive files
  tar -czf erlmcp_backup_$(date +%Y%m%d).tar.gz \
    /etc/erlmcp/certs/ \
    /etc/erlmcp/sys.config
  ```

### Incident Response

- [ ] **DDoS Attack Response**
  - [ ] Monitor for spike in requests
  - [ ] Check message size rejections (413 errors)
  - [ ] Implement rate limiting if needed
  - [ ] Contact infrastructure team for network-level mitigation

  ```bash
  # Check for DoS patterns
  tail -f /var/log/erlmcp/erlmcp.log | grep "413\|message_too_large"

  # Check request rate
  grep "$(date '+%Y-%m-%d %H:%M')" /var/log/erlmcp/erlmcp.log | wc -l
  ```

- [ ] **Security Breach Response**
  - [ ] Immediately audit access logs for unauthorized activity
  - [ ] Rotate session tokens/IDs
  - [ ] Review and update origin whitelist
  - [ ] Check for certificate misuse
  - [ ] Notify security team and customers

  ```bash
  # Audit for suspicious activity
  grep -E "invalid.*origin|path.*escape|traversal" \
    /var/log/erlmcp/erlmcp.log | head -20
  ```

---

## Configuration Reference

### Complete Production sys.config

```erlang
[
  {erlmcp, [
      %% HTTPS/TLS Configuration (Gap #31)
      {https_config, [
          {enabled, true},
          {certfile, "/etc/erlmcp/certs/cert.pem"},
          {keyfile, "/etc/erlmcp/certs/key.pem"},
          {cacertfile, "/etc/erlmcp/certs/ca-bundle.pem"},
          {min_tls_version, 'tlsv1.3'},
          {ciphers, [
              "ECDHE-ECDSA-AES256-GCM-SHA384",
              "ECDHE-RSA-AES256-GCM-SHA384",
              "ECDHE-ECDSA-CHACHA20-POLY1305",
              "ECDHE-RSA-CHACHA20-POLY1305",
              "ECDHE-ECDSA-AES128-GCM-SHA256",
              "ECDHE-RSA-AES128-GCM-SHA256"
          ]},
          {enable_hsts, true},
          {hsts_max_age, 31536000},
          {hsts_include_subdomains, true},
          {verify_mode, verify_none}  % or verify_peer for mTLS
      ]},

      %% HTTP Security (Gap #3, Gap #32)
      {http_security, [
          {require_https, true},
          {http_redirect_to_https, true},
          {http_bind_address, "127.0.0.1"},
          {https_bind_address, "127.0.0.1"},  % or remote IP for non-localhost
          {allowed_origins, [
              "https://app.example.com",
              "https://app-staging.example.com",
              "https://127.0.0.1:3000",
              "https://localhost:3000"
          ]}
      ]},

      %% Localhost Binding (Gap #32)
      {enforce_localhost_only, true},
      {http_bind_address, "127.0.0.1"},
      {http_bind_ipv6, "::1"},

      %% Session Management (Gap #2)
      {session_manager, [
          {timeout, 1800},  % 30 minutes
          {cleanup_interval, 300000}  % 5 minutes
      ]},

      %% Message Size Limits (Gap #45)
      {message_size_limits, #{
          default => 16777216,         % 16 MB
          http_body => 16777216,
          sse_event => 16777216,
          websocket => 8388608,        % 8 MB for WebSocket
          tcp => 16777216,
          stdio => 16777216
      }}
  ]},

  %% Kernel/Logger Configuration
  {kernel, [
      {logger, [
          {default_level, info},  % NOT debug in production
          {handlers, [
              {default, logger_standard_h, #{
                  level => info,
                  config => #{
                      type => standard_io
                  }
              }},
              {file_handler, logger_disk_log_h, #{
                  level => info,
                  config => #{
                      file => "/var/log/erlmcp/erlmcp.log",
                      type => wrap,
                      max_no_files => 10,
                      max_no_bytes => 52428800,  % 50 MB
                      sync_on => error,
                      sync_interval => 5000
                  }
              }}
          ]},
          {formatter, {logger_formatter, #{
              template => [time, " ", level, " ", pid, " ", msg, "\n"],
              time_offset => "+00:00"
          }}}
      ]}
  ]},

  %% SASL Configuration
  {sasl, [
      {sasl_error_logger, {file, "/var/log/erlmcp/sasl.log"}},
      {errlog_type, error},  % Only log errors, not info
      {error_logger_mf_dir, "/var/log/erlmcp"},
      {error_logger_mf_maxbytes, 52428800},
      {error_logger_mf_maxfiles, 10}
  ]}
].
```

### Environment Variables (Optional)

```bash
# Set in systemd service or shell before starting
export ERL_CRASH_DUMP="/var/log/erlmcp/erl_crash.dump"
export ERL_MAX_PORTS=65536
export ERL_MAX_ETS_TABLES=100000
```

---

## Verification Checklist Summary

### Critical Security Items (MUST complete)
- [ ] SSL/TLS certificate valid and installed
- [ ] HTTPS enabled and enforced (require_https = true)
- [ ] Localhost-only binding verified (not 0.0.0.0)
- [ ] Origin whitelist configured (not using defaults)
- [ ] Session timeout configured appropriately
- [ ] Message size limits set for your use case
- [ ] HSTS header enabled
- [ ] Firewall rules in place if needed
- [ ] Logging configured and monitoring enabled

### Important Security Items (SHOULD complete)
- [ ] Certificate auto-renewal configured
- [ ] Log rotation configured
- [ ] Security monitoring/alerting set up
- [ ] Incident response plan documented
- [ ] Key personnel trained on security procedures
- [ ] Regular security review schedule established

### Optional Security Hardening
- [ ] Implement rate limiting
- [ ] Enable mutual TLS (mTLS) if needed
- [ ] Configure IP whitelisting
- [ ] Set up intrusion detection
- [ ] Implement security scanning in CI/CD

---

## Testing Procedures

### Security Test Plan

Run these tests before and after deployment:

```bash
#!/bin/bash
# security_test.sh - erlmcp security validation

ERLMCP_URL="https://erlmcp.example.com:8080"
CURL_OPTS="--insecure -i"  # --insecure for self-signed certs

echo "=== Security Test Suite ==="

echo "1. Test HTTPS Enforcement"
curl -i http://erlmcp.example.com:8080/ | grep -q "301\|403"
echo "   HTTP redirect: $([ $? -eq 0 ] && echo 'PASS' || echo 'FAIL')"

echo "2. Test Origin Validation"
curl -H "Origin: https://app.example.com" $CURL_OPTS $ERLMCP_URL | grep -q "200\|403"
echo "   Valid origin: $([ $? -eq 0 ] && echo 'PASS' || echo 'FAIL')"

curl -H "Origin: https://attacker.com" $CURL_OPTS $ERLMCP_URL | grep -q "403"
echo "   Invalid origin: $([ $? -eq 0 ] && echo 'PASS' || echo 'FAIL')"

echo "3. Test Message Size Limits"
dd if=/dev/zero bs=1M count=20 2>/dev/null | \
  curl -d @- $CURL_OPTS $ERLMCP_URL | grep -q "413"
echo "   Oversized message rejected: $([ $? -eq 0 ] && echo 'PASS' || echo 'FAIL')"

echo "4. Test TLS Configuration"
openssl s_client -connect erlmcp.example.com:8080 -tls1_2 </dev/null 2>/dev/null | \
  grep -q "Server certificate"
echo "   TLS 1.2+ available: $([ $? -eq 0 ] && echo 'PASS' || echo 'FAIL')"

echo "5. Test Localhost-Only Binding"
netstat -tlnp | grep erlmcp | grep -q "127.0.0.1"
echo "   Localhost binding: $([ $? -eq 0 ] && echo 'PASS' || echo 'FAIL')"

netstat -tlnp | grep erlmcp | grep -q "0.0.0.0"
echo "   Not 0.0.0.0: $([ $? -ne 0 ] && echo 'PASS' || echo 'FAIL')"

echo "=== Test Complete ==="
```

---

## Troubleshooting Guide

### Issue: Certificate Not Valid

```
Error: "certificate verify failed" or "bad certificate"

Solution:
1. Verify certificate file readable:
   ls -la /etc/erlmcp/certs/cert.pem

2. Verify certificate valid:
   openssl x509 -in /etc/erlmcp/certs/cert.pem -text -noout

3. Check certificate expiration:
   openssl x509 -in /etc/erlmcp/certs/cert.pem -noout -dates

4. Verify key matches certificate:
   openssl x509 -in /etc/erlmcp/certs/cert.pem -noout -modulus | openssl md5
   openssl rsa -in /etc/erlmcp/certs/key.pem -noout -modulus | openssl md5
   (Both md5 hashes should match)
```

### Issue: HTTPS Not Working

```
Error: Connection refused or timeout on HTTPS port

Solution:
1. Verify HTTPS enabled in config:
   grep "enabled.*true" /etc/erlmcp/sys.config | grep https_config

2. Check if application started:
   ps aux | grep erlmcp

3. Check logs for errors:
   tail -f /var/log/erlmcp/erlmcp.log | grep -i error

4. Verify port listening:
   netstat -tlnp | grep erlmcp

5. Verify certificate path in config matches actual files:
   ls -l /etc/erlmcp/certs/cert.pem /etc/erlmcp/certs/key.pem
```

### Issue: Origins Rejected (All Requests Return 403)

```
Problem: All requests return 403 Forbidden with "origin rejected"

Solution:
1. Check allowed_origins configuration:
   grep -A 10 "allowed_origins" /etc/erlmcp/sys.config

2. Verify client origin matches whitelist:
   curl -H "Origin: https://your-app.com" ...
   Check that "https://your-app.com" is in whitelist

3. Default origins (should not use in production):
   "http://127.0.0.1:*"    (HTTP, not HTTPS)
   "http://localhost:*"
   "http://[::1]:*"

4. Fix: Add client origin to whitelist:
   {allowed_origins, [
       "https://your-app.com",  % Your production client
       "https://127.0.0.1:3000" % Your local development
   ]}
```

### Issue: Memory Growing (Potential Memory Leak)

```
Problem: Memory usage increasing over time

Likely Cause: Sessions not expiring properly

Solution:
1. Check session cleanup running:
   erlmcp_session_manager:cleanup_expired_sessions().

2. Verify cleanup interval:
   grep "cleanup_interval" /etc/erlmcp/sys.config
   Default: 300000ms (5 minutes)

3. Check session timeout:
   grep "timeout" /etc/erlmcp/sys.config
   Default: 1800 seconds (30 minutes)

4. Monitor session count:
   # Add to logs or monitoring
   erlmcp_session_manager:get_session_info(SessionId).

   If count keeps growing, check for stuck sessions.
```

---

**Document Status:** READY FOR USE
**Last Updated:** January 27, 2026
**Responsibility:** DevOps/SRE Team
**Review Cycle:** Quarterly or after security incidents
