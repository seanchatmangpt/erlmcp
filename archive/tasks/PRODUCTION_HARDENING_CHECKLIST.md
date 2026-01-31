# erlmcp Production Hardening Checklist

**Status**: Development → Staging → Production
**Created**: 2026-01-27
**Version**: 1.0

---

## Pre-Deployment Security Gate

### Phase 0: Code Review & Analysis

- [ ] **Security Code Review**
  - [ ] No hardcoded secrets in code
  - [ ] All sensitive data uses environment variables
  - [ ] No debug logging of credentials
  - [ ] No raw SQL/commands (not applicable, but check for injection patterns)
  - Reviewer: _________________ Date: _______

- [ ] **Dependency Audit**
  ```bash
  rebar3 hex audit
  # Result: ___________________
  ```
  - [ ] No known vulnerabilities
  - [ ] All dependencies up-to-date
  - [ ] License compliance checked
  - Reviewer: _________________ Date: _______

- [ ] **Xref & Dialyzer Check**
  ```bash
  rebar3 do xref, dialyzer
  # All warnings addressed: YES / NO
  ```
  - [ ] No undefined function calls
  - [ ] No type errors
  - [ ] Debug info present on all modules
  - Reviewer: _________________ Date: _______

---

## Cryptography & Secrets Management

### TLS/HTTPS Configuration

- [ ] **Certificate Validation**
  - [ ] `verify_mode` set to `verify_peer` (NOT `verify_none`)
  - [ ] Certificate chain validation enabled
  - [ ] `verify_depth` set appropriately (recommend: 10)
  - [ ] CA certificate bundle loaded (`cacertfile` present)
  - [ ] Test: `openssl s_client -connect localhost:8443 -showcerts`

- [ ] **TLS Protocol Version**
  - [ ] Minimum version: TLS 1.3 (NOT 1.2)
  - [ ] Maximum version: TLS 1.3
  - [ ] Test: `nmap -sV --script ssl-enum-ciphers localhost:8443`

- [ ] **Cipher Suites**
  - [ ] Only strong ciphers (AEAD ciphers: GCM, ChaCha20-Poly1305)
  - [ ] No weak ciphers (DES, RC4, MD5)
  - [ ] Perfect Forward Secrecy (PFS) enabled
  - [ ] Test: Verify with `nmap` above

- [ ] **Session Configuration**
  - [ ] Session lifetime: 1 hour maximum
  - [ ] Session timeout enforcement
  - [ ] Session resumption configured
  - [ ] Test: Verify session expires after 1 hour

- [ ] **Additional TLS Hardening**
  - [ ] HSTS enabled with appropriate max-age (min 1 year)
  - [ ] HSTS includeSubdomains: true (if applicable)
  - [ ] HSTS preload: disabled (only if HSTS is stable)
  - [ ] OCSP stapling enabled (if certificates support it)
  - [ ] SNI enabled
  - [ ] Certificate pinning implemented (for high-security deployments)

### Secrets Management

- [ ] **Credentials Not in Code**
  - [ ] No hardcoded passwords
  - [ ] No hardcoded API keys
  - [ ] No hardcoded certificates
  - [ ] All secrets in: `{env, "ENV_VAR"}` or Vault
  - Audit: `grep -r "password\|secret\|api_key" src/`

- [ ] **Environment Variables Secured**
  - [ ] OAuth client secret from: `{env, "OAUTH_CLIENT_SECRET"}`
  - [ ] Database credentials from environment
  - [ ] JWT signing key from environment
  - [ ] Session encryption key from environment
  - [ ] Secrets are NOT logged
  - Test: Check logs for any secret exposure

- [ ] **Secrets Vault Integration (RECOMMENDED)**
  - [ ] HashiCorp Vault configured
  - [ ] OR Kubernetes Sealed Secrets configured
  - [ ] OR AWS Secrets Manager configured
  - [ ] Service account auth enabled
  - [ ] Secret rotation policy: Every 90 days
  - [ ] Secrets stored: Encrypted at rest
  - [ ] Secrets in transit: TLS encrypted

- [ ] **Secrets Rotation**
  - [ ] Automatic rotation mechanism implemented
  - [ ] Rotation doesn't cause service downtime
  - [ ] Old secrets supported briefly during rotation
  - [ ] Audit log of all secret rotations
  - [ ] Tested: Verified rotation under load

---

## Authentication & Authorization

### Session Management

- [ ] **Session Configuration**
  - [ ] Session timeout: ≤ 30 minutes (config: session_timeout)
  - [ ] Session cleanup interval: 5-10 minutes (config: cleanup_interval)
  - [ ] Tested: Session expires after timeout period

- [ ] **Session Security**
  - [ ] Session tokens: 32-byte random (crypto:strong_rand_bytes)
  - [ ] Session tokens: NOT predictable
  - [ ] Session storage: Encrypted in memory (recommended)
  - [ ] Session binding to IP address implemented
  - [ ] Session binding to User-Agent implemented
  - [ ] Test: Verify IP/User-Agent binding prevents reuse

- [ ] **Session Cookies**
  - [ ] Secure flag: Enabled (HTTPS only)
  - [ ] HttpOnly flag: Enabled (no JavaScript access)
  - [ ] SameSite attribute: `Strict` (CSRF protection)
  - [ ] Domain restriction: Set appropriately
  - [ ] Path restriction: `/` or specific endpoint
  - [ ] Expires/Max-Age: Session timeout value
  - Test: `curl -v https://localhost:8443 | grep -i cookie`

### OAuth 2.0 Implementation

- [ ] **OAuth Configuration**
  - [ ] OAuth enabled: true
  - [ ] Client ID from: `{env, "OAUTH_CLIENT_ID"}`
  - [ ] Client secret from: `{env, "OAUTH_CLIENT_SECRET"}` (NOT config)
  - [ ] Token endpoint: HTTPS only
  - [ ] Resource indicator: Configured
  - [ ] Scopes: Properly defined and enforced

- [ ] **PKCE Support (RFC 7636)**
  - [ ] PKCE enabled for public clients
  - [ ] Code challenge method: SHA-256
  - [ ] Code verifier: 128-char random (recommended: 128)
  - [ ] Test: Verify PKCE flow works

- [ ] **Token Management**
  - [ ] Access token lifetime: ≤ 1 hour
  - [ ] Refresh token lifetime: ≤ 30 days
  - [ ] Token refresh mechanism implemented
  - [ ] Expired tokens rejected immediately
  - [ ] Token revocation implemented
  - [ ] Test: Verify expired tokens rejected

- [ ] **OAuth Security**
  - [ ] HTTPS required for all OAuth endpoints
  - [ ] State parameter validated on callback
  - [ ] Nonce parameter validated (if OIDC)
  - [ ] Redirect URI whitelist enforced
  - [ ] JWT signature validation (if using JWT tokens)

### Authorization & Access Control

- [ ] **API Authorization**
  - [ ] All endpoints require authentication (except /health, /ready)
  - [ ] Authorization tokens validated before execution
  - [ ] Expired tokens rejected with 401
  - [ ] Missing tokens rejected with 401
  - [ ] Invalid tokens rejected with 403

- [ ] **Scope/Permission Enforcement (IF IMPLEMENTED)**
  - [ ] Scopes extracted from token
  - [ ] Required scope checked before operation
  - [ ] Insufficient scope returns 403
  - [ ] Scope hierarchy enforced

- [ ] **Resource-Level Authorization**
  - [ ] User can only access own resources
  - [ ] User cannot escalate to admin
  - [ ] User cannot access other users' data
  - [ ] Test: Attempt to access another user's resource

---

## Input Validation & Sanitization

### JSON-RPC Message Validation

- [ ] **Message Size Limits**
  - [ ] Default message limit: 1 MB (not 16 MB)
  - [ ] HTTP body limit: 10 MB
  - [ ] SSE event limit: 64 KB
  - [ ] WebSocket limit: 1 MB
  - [ ] Per-connection rate limit: 100 msg/sec
  - [ ] Oversized messages: Rejected with error
  - Test: Send 10MB message, verify rejection

- [ ] **JSON Parsing**
  - [ ] Max nesting depth: 50 (prevent stack exhaustion)
  - [ ] Malformed JSON: Rejected with parse error
  - [ ] Non-JSON: Rejected with parse error
  - [ ] Empty payload: Rejected appropriately
  - Test: Send deeply nested JSON (51+ levels)

- [ ] **JSON-RPC Fields**
  - [ ] `jsonrpc` field: Must be "2.0"
  - [ ] `method` field: Required, must be string
  - [ ] `params` field: Optional, validated against schema
  - [ ] `id` field: Required for requests, string or number
  - [ ] Unknown fields: Allowed (per spec) but logged

### URI & Path Validation

- [ ] **Resource URI Validation**
  - [ ] URIs must be binary (not strings)
  - [ ] URIs must not be empty
  - [ ] Scheme validation: Only allowed schemes
  - [ ] Relative paths: Prevented from escaping
  - [ ] Absolute URIs: Require valid scheme
  - Test: `erlmcp_uri_validator:validate_uri(<<"../../etc/passwd">>)` returns error

- [ ] **Path Canonicalization**
  - [ ] Symlinks resolved safely
  - [ ] Max symlink depth: 40 (prevent loops)
  - [ ] Race conditions prevented
  - [ ] Canonicalized paths validated against whitelist
  - [ ] Path length limit: 4096 characters
  - Test: Try symlink escape, verify rejection

- [ ] **Directory Traversal Prevention**
  - [ ] `../` sequences normalized before use
  - [ ] Resolved path verified within allowed directories
  - [ ] Each path hop validated (not just final)
  - [ ] Symlink targets verified within allowed directories
  - Test: Attempt various traversal patterns

### Tool & Prompt Parameter Validation

- [ ] **Tool Input Validation**
  - [ ] Tool parameters validated against JSON Schema
  - [ ] Schema validation performed by `jesse`
  - [ ] Invalid parameters: Rejected with validation error
  - [ ] Extra parameters: Handled appropriately (allowed or rejected)
  - [ ] Required parameters: Enforced
  - Test: Call tool with invalid params, verify rejection

- [ ] **Prompt Argument Validation**
  - [ ] Prompt arguments validated against schema (Gap #42)
  - [ ] Argument names must match defined schema
  - [ ] Argument types must match schema
  - [ ] Required arguments enforced
  - Test: Call prompt with missing required argument

---

## Network Security & Isolation

### HTTP/HTTPS Enforcement

- [ ] **HTTPS Requirement**
  - [ ] Production: HTTPS required (require_https: true)
  - [ ] HTTP requests: Redirected to HTTPS (308 Permanent Redirect)
  - [ ] HTTP/2 support: Enabled (for performance)
  - [ ] Clear-text HTTP: Disabled entirely
  - Test: Attempt HTTP connection, verify redirect

- [ ] **Port Configuration**
  - [ ] HTTP port (if used): 8080 only for development
  - [ ] HTTPS port: 8443 (or standard 443)
  - [ ] Localhost binding: 127.0.0.1 (not 0.0.0.0)
  - [ ] IPv6 binding: ::1 (not ::)
  - Verify config: Check enforce_localhost_only = true

- [ ] **Reverse Proxy Integration**
  - [ ] Behind reverse proxy (nginx, Envoy, etc.)
  - [ ] X-Forwarded-Proto header validated
  - [ ] X-Real-IP / X-Forwarded-For logged safely
  - [ ] Trust proxy headers only from trusted IPs
  - Config check: Proxy whitelist configured

### Origin & CORS Validation

- [ ] **Origin Validation**
  - [ ] Allowed origins configured (whitelist only)
  - [ ] Wildcard origins: NOT allowed
  - [ ] Localhost origins: Allowed in development only
  - [ ] Scheme validation: http vs https distinguished
  - [ ] Subdomain validation: Proper matching (no prefix matching)
  - [ ] IPv6 addresses: Properly normalized
  - Test: Attempt request from disallowed origin

- [ ] **DNS Rebinding Protection**
  - [ ] Origin validation prevents rebinding
  - [ ] Localhost-only binding enforces isolation
  - [ ] Reverse DNS validation: Optional but recommended
  - Test: Attempt DNS rebinding attack

- [ ] **CORS Headers (if CORS needed)**
  - [ ] Access-Control-Allow-Origin: Specific origins only
  - [ ] Access-Control-Allow-Methods: Explicitly listed
  - [ ] Access-Control-Allow-Headers: Explicitly listed
  - [ ] Access-Control-Allow-Credentials: true (if needed)
  - [ ] Access-Control-Max-Age: 3600 (1 hour)
  - [ ] Preflight validation: Properly implemented

### Security Headers

- [ ] **HTTP Security Headers**
  - [ ] Strict-Transport-Security: max-age=31536000 (1 year)
  - [ ] X-Content-Type-Options: nosniff
  - [ ] X-Frame-Options: DENY or SAMEORIGIN
  - [ ] X-XSS-Protection: 1; mode=block
  - [ ] Content-Security-Policy: Restrictive policy
  - [ ] Referrer-Policy: strict-origin-when-cross-origin
  - [ ] Permissions-Policy: Appropriate restrictions
  - [ ] Cache-Control: no-store (for sensitive responses)
  - [ ] Pragma: no-cache (for older clients)
  - Test: curl -I https://localhost:8443 | grep -i security

### Rate Limiting & DDoS Protection

- [ ] **Rate Limiting Implemented**
  - [ ] Per-connection rate limit: 100 msg/sec
  - [ ] Per-user rate limit: 1000 req/min
  - [ ] Per-API-key rate limit: Configured appropriately
  - [ ] Global rate limit: 10000 req/sec
  - [ ] Burst allowance: 5 MB/sec
  - [ ] Test: Send 101 messages in 1 second, verify rate limit

- [ ] **Connection Limits**
  - [ ] Max concurrent connections: Configured
  - [ ] Max connections per IP: 100
  - [ ] Max connections per user: 50
  - [ ] Connection timeout: 30 seconds (idle)
  - [ ] Keep-alive timeout: 5 minutes
  - Test: Verify connection limits enforced under load

- [ ] **DoS Mitigation**
  - [ ] Message flood protection: Rate limits
  - [ ] Slowloris protection: Request timeout
  - [ ] Recursive expansion protection: JSON depth limit
  - [ ] Regex DoS protection: No dangerous regex
  - [ ] Amplification protection: Response size limited

---

## Logging, Monitoring & Observability

### Security Logging

- [ ] **Authentication Events Logged**
  - [ ] Session creation logged with: timestamp, user, IP, User-Agent
  - [ ] Session destruction logged
  - [ ] Authentication failures logged
  - [ ] Failed token validation logged
  - [ ] Invalid credentials logged (without password/token value)
  - Format: JSON structured logging for parsing

- [ ] **Authorization Events Logged**
  - [ ] Scope/permission checks logged
  - [ ] Insufficient permissions logged
  - [ ] Resource access logged (read/write)
  - [ ] Admin operations logged with actor
  - [ ] Privilege escalation attempts logged

- [ ] **Security Events Logged**
  - [ ] Origin validation failures logged
  - [ ] HTTPS enforcement violations logged
  - [ ] Certificate validation failures logged
  - [ ] Rate limit violations logged
  - [ ] Path traversal attempts logged
  - [ ] Invalid input detected logged
  - [ ] Dependency check failures logged

- [ ] **PII & Sensitive Data**
  - [ ] No passwords in logs
  - [ ] No API keys in logs
  - [ ] No session tokens in logs
  - [ ] No credit card numbers in logs
  - [ ] Secrets: Masked in logs (SHA hash only)
  - [ ] Test: Grep logs for common secret patterns

### Monitoring & Alerting

- [ ] **Metrics Collection**
  - [ ] OpenTelemetry exporter configured
  - [ ] Request latency metrics: P50, P95, P99
  - [ ] Request throughput: req/sec
  - [ ] Error rates: errors/sec
  - [ ] Authentication failures: count/min
  - [ ] Rate limit violations: count/min
  - [ ] Connection count: current active
  - [ ] Memory usage: bytes
  - [ ] GC pause time: milliseconds

- [ ] **Alerting Configured**
  - [ ] Authentication failure spike: Alert if > 10/min
  - [ ] Rate limit violations spike: Alert if > 100/min
  - [ ] Error rate spike: Alert if > 1%
  - [ ] Latency spike: Alert if P99 > 1 sec
  - [ ] Memory usage: Alert if > 80%
  - [ ] CPU usage: Alert if > 80%
  - [ ] Disk usage: Alert if > 85%
  - [ ] Service down: Alert immediately

- [ ] **Log Aggregation**
  - [ ] Logs shipped to central repository
  - [ ] Retention policy: Min 30 days
  - [ ] Tamper-proof logging: Cryptographically signed
  - [ ] Searchable: Full-text indexing
  - [ ] Correlated: Trace IDs across requests
  - [ ] Test: Verify logs appear in aggregation system

---

## Data Protection

### Data in Transit

- [ ] **TLS/HTTPS (covered above)**
  - [ ] All data encrypted in transit
  - [ ] No plaintext protocols (HTTP, FTP, etc.)
  - [ ] Certificate validation enabled
  - [ ] Perfect Forward Secrecy (PFS)

### Data at Rest

- [ ] **Sensitive Data Encryption**
  - [ ] Session data: Encrypted with AES-256-GCM
  - [ ] Configuration: Encrypted at rest (optional)
  - [ ] Audit logs: Encrypted (optional)
  - [ ] Encryption keys: Managed securely
  - [ ] Test: Verify encrypted data cannot be read without key

- [ ] **PII Handling**
  - [ ] No unnecessary PII collection
  - [ ] PII minimization: Only needed data stored
  - [ ] Data retention: Clear policy, enforced
  - [ ] Data deletion: Secure erasure (NIST guidelines)
  - [ ] Right to erasure: Implemented (if GDPR applies)
  - [ ] Data export: Implemented (if GDPR applies)

---

## Compliance & Policies

### Security Policies

- [ ] **Password Policy (if applicable)**
  - [ ] Minimum length: 12 characters
  - [ ] Complexity: Mix of upper, lower, numbers, symbols
  - [ ] History: Cannot reuse last 5 passwords
  - [ ] Expiration: 90 days (optional for high-security)
  - [ ] Lockout: 5 failed attempts, 30-min lockout

- [ ] **MFA Policy (if applicable)**
  - [ ] MFA enabled for all admin accounts
  - [ ] MFA methods: TOTP, WebAuthn recommended
  - [ ] Recovery codes: Generated and securely stored
  - [ ] Backup MFA method: Enforced

- [ ] **Access Control Policy**
  - [ ] Least privilege enforced
  - [ ] Role-based access control (RBAC)
  - [ ] Regular access reviews: Quarterly
  - [ ] Offboarding: Immediate access revocation

### Audit & Compliance

- [ ] **Audit Trail**
  - [ ] All privileged actions logged
  - [ ] Configuration changes logged
  - [ ] User actions logged (if applicable)
  - [ ] Audit logs: Immutable
  - [ ] Retention: Min 1 year
  - [ ] Regular review: Monthly

- [ ] **Compliance Requirements**
  - [ ] GDPR compliance: (if handling EU data)
    - [ ] Data Processing Agreement with vendors
    - [ ] Data retention policy documented
    - [ ] Right to erasure implemented
    - [ ] Data breach notification process

  - [ ] PCI-DSS: (if handling payment data)
    - [ ] Cryptographic key management
    - [ ] Secure configuration
    - [ ] Access controls
    - [ ] Regular security testing

  - [ ] HIPAA: (if handling health data)
    - [ ] Encryption at rest & in transit
    - [ ] Access controls
    - [ ] Audit logging
    - [ ] Business Associate Agreements

- [ ] **Security Testing**
  - [ ] Annual penetration test: Scheduled
  - [ ] Vulnerability scanning: Automated weekly
  - [ ] Dependency audit: Automated daily
  - [ ] Code review: Before merge
  - [ ] Static analysis: Automated (rebar3 lint)

---

## Deployment & Operations

### Pre-Deployment

- [ ] **Build Verification**
  ```bash
  rebar3 as prod compile
  # Build successful: YES / NO
  # Warnings addressed: YES / NO
  ```
  - [ ] No compilation errors
  - [ ] No dialyzer warnings
  - [ ] No xref warnings
  - [ ] All tests passing

- [ ] **Release Build**
  ```bash
  rebar3 as prod release
  # Release created: YES / NO
  # Size: _____________ MB
  ```
  - [ ] Release tarball created
  - [ ] Release verified with: `tar -tzf _build/prod/rel/erlmcp/*.tar.gz`
  - [ ] Source code excluded (include_src: false)
  - [ ] ERTS included (include_erts: true)

- [ ] **Configuration Preparation**
  - [ ] Production sys.config created
  - [ ] Production vm.args created
  - [ ] All secrets populated (from Vault/env)
  - [ ] HTTPS certificates in place
  - [ ] Certificate chain validated
  - [ ] Certificate expiration: > 30 days

### Deployment

- [ ] **Staging Deployment**
  - [ ] Deployed to staging environment
  - [ ] Configuration verified
  - [ ] Smoke tests passed
  - [ ] Security checks passed
  - [ ] Performance tests passed

- [ ] **Production Deployment**
  - [ ] Deployment procedure: Documented & tested
  - [ ] Rollback procedure: Documented & tested
  - [ ] Zero-downtime deployment: Tested
  - [ ] Service health checked post-deploy
  - [ ] No error spike in first 5 minutes
  - [ ] Monitoring/alerting verified

### Post-Deployment

- [ ] **Service Health**
  - [ ] Health check endpoint: Responding 200
  - [ ] Readiness check: All dependencies healthy
  - [ ] Liveness check: Service responding to requests
  - [ ] No unusual errors in logs
  - [ ] Latency: Within acceptable range
  - [ ] CPU usage: < 50% baseline
  - [ ] Memory usage: Stable, no leaks
  - [ ] Connections: Expected count

- [ ] **Security Verification**
  - [ ] HTTPS working: `openssl s_client -connect prod:8443`
  - [ ] Certificate valid: `openssl x509 -text -noout < cert.pem`
  - [ ] Origin validation working: Test from disallowed origin
  - [ ] Rate limiting working: Send 1000 msg/sec
  - [ ] Logging working: Check logs for security events
  - [ ] Secrets not logged: Grep logs for credentials

---

## Monitoring & Maintenance Schedule

### Daily

- [ ] Review security alerts (10 min)
- [ ] Check error logs (10 min)
- [ ] Verify backup status (5 min)

### Weekly

- [ ] Security metrics review (30 min)
- [ ] Dependency vulnerability check (20 min)
- [ ] Log analysis for anomalies (30 min)

### Monthly

- [ ] Full security audit (2 hours)
- [ ] Performance review (1 hour)
- [ ] Access control review (1 hour)
- [ ] Incident post-mortems (if any) (1 hour)

### Quarterly

- [ ] Penetration test (if not annual)
- [ ] Compliance audit
- [ ] Security training
- [ ] Disaster recovery drill

### Annually

- [ ] Full penetration test
- [ ] Security assessment
- [ ] Dependency audit
- [ ] Compliance review

---

## Sign-Off

### Security Lead Approval

- **Name**: ________________________
- **Date**: ________________________
- **Signature**: ________________________
- **Notes**:

### Operations Lead Approval

- **Name**: ________________________
- **Date**: ________________________
- **Signature**: ________________________
- **Notes**:

### Executive Approval

- **Name**: ________________________
- **Date**: ________________________
- **Signature**: ________________________
- **Notes**:

---

## Deployment Record

- **Deployment Date**: ________________________
- **Environment**: ☐ Dev | ☐ Staging | ☐ Production
- **Version**: ________________________
- **Git Commit**: ________________________
- **Release Officer**: ________________________
- **Approved By**: ________________________
- **Deployment Time**: ________________________ to ________________________
- **Issues During Deployment**: ☐ None | ☐ Yes (describe):

### Rollback History (if needed)

| Date | Version | Reason | Duration |
|------|---------|--------|----------|
|      |         |        |          |
|      |         |        |          |

---

Document Version: 1.0
Last Updated: 2026-01-27
Review Schedule: Monthly
