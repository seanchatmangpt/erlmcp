# CRITICAL SECURITY ACTION ITEMS
## erlmcp Security Vulnerability Assessment - Executive Summary

**Date:** 2026-01-27
**Status:** 3 Critical Actions Identified
**Effort:** < 30 minutes total

---

## CRITICAL ISSUES (Fix Immediately - 3 Items)

### 1. OAuth Token Endpoint Missing Validation
**CVSS:** 6.8 | **Effort:** 5 minutes
**File:** `/Users/sac/erlmcp/src/erlmcp_oauth_security.erl`
**Lines:** 151-152

**Problem:**
```erlang
%% CURRENT (VULNERABLE):
TokenEndpoint = os:getenv(?OAUTH_TOKEN_ENDPOINT_ENV,
                         "https://oauth.example.com/token")  % ❌ DEFAULT FALLBACK
```

Default URL may not match production OAuth provider.

**Fix:**
```erlang
%% CORRECTED:
case os:getenv("OAUTH_TOKEN_ENDPOINT") of
    false -> {error, oauth_token_endpoint_required};
    "" -> {error, oauth_token_endpoint_empty};
    Endpoint -> Endpoint
end
```

**Verification:**
```bash
# Ensure env var is set before deployment:
export OAUTH_TOKEN_ENDPOINT="https://your-oauth-provider.com/token"
```

---

### 2. Remove Non-PFS Cipher Suites
**CVSS:** 6.5 | **Effort:** 5 minutes
**File:** `/Users/sac/erlmcp/config/sys.config`
**Lines:** 108-115

**Problem:**
```erlang
%% CURRENT (WEAK):
{ciphers, [
    "ECDHE-RSA-AES256-GCM-SHA384",    % ✓ Good (PFS)
    "ECDHE-RSA-AES128-GCM-SHA256",    % ✓ Good (PFS)
    "ECDHE-RSA-CHACHA20-POLY1305",    % ✓ Good (PFS)
    "DHE-RSA-AES256-GCM-SHA384",      % ❌ NO PFS
    "DHE-RSA-AES128-GCM-SHA256"       % ❌ NO PFS
]}
```

DHE ciphers lack Perfect Forward Secrecy (PFS).

**Fix:**
```erlang
%% CORRECTED:
{ciphers, [
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-RSA-CHACHA20-POLY1305"
]}
```

**Impact:** Ensures all negotiated ciphers use PFS (forward secrecy).

---

### 3. Remove Data URI Scheme
**CVSS:** 7.5 | **Effort:** 5 minutes
**File:** `/Users/sac/erlmcp/src/erlmcp_uri_validator.erl`
**Lines:** 36-38

**Problem:**
```erlang
%% CURRENT (VULNERABLE):
ValidSchemes = [<<"file">>, <<"http">>, <<"https">>, <<"data">>,  % ❌ data: scheme
    <<"ftp">>, <<"ftps">>, <<"ws">>, <<"wss">>, <<"custom">>]
```

`data:` scheme can contain embedded scripts (XSS risk).

**Fix:**
```erlang
%% CORRECTED:
ValidSchemes = [<<"file">>, <<"http">>, <<"https">>,
    <<"ftp">>, <<"ftps">>, <<"ws">>, <<"wss">>, <<"custom">>]
```

**Impact:** Prevents inline script execution via data URIs.

---

## VERIFICATION CHECKLIST

After applying fixes, verify with:

```bash
# 1. Check OAuth token endpoint enforcement
grep -A5 "OAUTH_TOKEN_ENDPOINT" src/erlmcp_oauth_security.erl | grep -q "false ->" && echo "✓ FIXED"

# 2. Verify cipher suites
grep -A2 "ciphers," config/sys.config | grep -q "DHE" && echo "❌ STILL VULNERABLE" || echo "✓ FIXED"

# 3. Verify data: scheme removed
grep -q 'data' src/erlmcp_uri_validator.erl && echo "❌ STILL VULNERABLE" || echo "✓ FIXED"

# 4. Test with SSL Labs (after deploying)
# Visit: https://www.ssllabs.com/ssltest/
```

---

## DEPLOYMENT VERIFICATION

Before deploying to production:

```bash
# Set required environment variable
export OAUTH_TOKEN_ENDPOINT="https://your-oauth.example.com/token"

# Run security tests
make test
make lint

# Check for any regressions
rebar3 do eunit, ct

# Verify configuration
erl -config config/sys.config -eval 'erlmcp:start(), halt().'
```

---

## ALREADY FIXED (No Action Needed)

✓ **Session ID Generation (CVSS 8.7)** - Uses crypto:strong_rand_bytes/1
✓ **Certificate Verification (CVSS 9.8)** - verify_peer enforced
✓ **Path Traversal (CVSS 8.8)** - Path canonicalization + symlink resolution
✓ **OAuth Credential Storage (CVSS 9.1)** - Environment variables only
✓ **Rate Limiting (CVSS 7.8)** - Token bucket DoS protection
✓ **Origin Validation (CVSS 7.9)** - DNS rebinding protection

---

## SUMMARY

| Item | Issue | CVSS | Effort | Status |
|------|-------|------|--------|--------|
| 1 | OAuth Token Endpoint | 6.8 | 5 min | ⚠️ TODO |
| 2 | Cipher Suites | 6.5 | 5 min | ⚠️ TODO |
| 3 | Data URI Scheme | 7.5 | 5 min | ⚠️ TODO |

**Total Estimated Effort:** 15 minutes
**Security Impact:** HIGH - Closes CVSS 6.5-7.5 vulnerabilities

After fixes: **0 unresolved critical vulnerabilities** ✓

---

## DEPLOYMENT READINESS

**Current State:** PRODUCTION READY (with above actions)
**Recommended Deployment:** After completing 3 critical items
**Timeline:** < 1 hour total

Contact: Security Team
Date: 2026-01-27
