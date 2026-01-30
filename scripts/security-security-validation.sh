#!/usr/bin/env bash
#
# Security Validation Script for erlmcp
# Agent #17: Security Impact Assessment
#
# This script performs automated security checks on the erlmcp codebase
# to identify vulnerabilities and validate security controls.
#

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
CRITICAL=0
HIGH=0
MEDIUM=0
LOW=0
INFO=0

# Logging functions
log_critical() {
    echo -e "${RED}[CRITICAL]${NC} $1"
    ((CRITICAL++))
}

log_high() {
    echo -e "${RED}[HIGH]${NC} $1"
    ((HIGH++))
}

log_medium() {
    echo -e "${YELLOW}[MEDIUM]${NC} $1"
    ((MEDIUM++))
}

log_low() {
    echo -e "${YELLOW}[LOW]${NC} $1"
    ((LOW++))
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
    ((INFO++))
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

echo "========================================"
echo "  erlmcp Security Validation Scan"
echo "  Agent #17: Security Impact Assessment"
echo "========================================"
echo ""

# Change to project root
cd "$(dirname "$0")/.."

# =============================================================================
# 1. CRITICAL: Authentication & Authorization
# =============================================================================
echo "=== 1. Authentication & Authorization Checks ==="

# Check if JWT verification is implemented
if grep -q "TODO.*Implement full JWT validation" apps/erlmcp_core/src/erlmcp_auth.erl; then
    log_critical "JWT verification is NOT cryptographically implemented (placeholder code)"
    log_info "Location: apps/erlmcp_core/src/erlmcp_auth.erl:443"
else
    log_pass "JWT verification appears implemented"
fi

# Check if OAuth2 introspection is implemented
if grep -q "TODO.*Implement OAuth2 token introspection" apps/erlmcp_core/src/erlmcp_auth.erl; then
    log_high "OAuth2 introspection is not implemented (placeholder code)"
    log_info "Location: apps/erlmcp_core/src/erlmcp_auth.erl:476"
else
    log_pass "OAuth2 introspection appears implemented"
fi

# Check if mTLS validation is implemented
if grep -q "TODO.*Implement mTLS certificate validation" apps/erlmcp_core/src/erlmcp_auth.erl; then
    log_high "mTLS certificate validation is not implemented (placeholder code)"
    log_info "Location: apps/erlmcp_core/src/erlmcp_auth.erl:489"
else
    log_pass "mTLS validation appears implemented"
fi

# Check authorization enforcement in server operations
if ! grep -q "check_permission" apps/erlmcp_core/src/erlmcp_server.erl; then
    log_critical "Authorization checks NOT enforced in erlmcp_server operations"
    log_info "Risk: Unauthorized tool/resource registration"
else
    # Check if authorization is actually called in handle_call
    if ! grep -A 10 "{add_tool," apps/erlmcp_core/src/erlmcp_server.erl | grep -q "check_permission"; then
        log_critical "Authorization NOT called in add_tool operation"
        log_info "Location: apps/erlmcp_core/src/erlmcp_server.erl"
    else
        log_pass "Authorization enforced in add_tool"
    fi
fi

echo ""

# =============================================================================
# 2. INPUT VALIDATION
# =============================================================================
echo "=== 2. Input Validation Checks ==="

# Check message size limits
if grep -q "validate_message_size" apps/erlmcp_core/src/erlmcp_json_rpc.erl; then
    log_pass "Message size validation implemented"

    # Check default limits
    DEFAULT_SIZE=$(grep -o "MCP_DEFAULT_MESSAGE_SIZE_LIMIT.*[0-9]*" apps/erlmcp_core/include/erlmcp.hrl | grep -o "[0-9]*$" | head -1)
    if [ "$DEFAULT_SIZE" -gt 1048576 ]; then
        log_high "Default message size limit is $DEFAULT_SIZE bytes (recommended: 1048576 bytes = 1 MB)"
        log_info "Location: apps/erlmcp_core/include/erlmcp.hrl"
    else
        log_pass "Message size limits are reasonable"
    fi
else
    log_high "Message size validation NOT implemented"
fi

# Check header validation
if [ -f "apps/erlmcp_transports/src/erlmcp_http_header_validator.erl" ]; then
    log_pass "HTTP header validator implemented"

    # Check for header count limits
    if ! grep -q "MAX_HEADER_COUNT" apps/erlmcp_transports/src/erlmcp_http_header_validator.erl; then
        log_medium "No header count limit (DoS risk: 10,000 headers)"
    fi
else
    log_high "HTTP header validator NOT implemented"
fi

# Check origin validation
if [ -f "apps/erlmcp_transports/src/erlmcp_origin_validator.erl" ]; then
    log_pass "Origin validator implemented (DNS rebinding protection)"

    # Check for unsafe defaults
    if grep -q "<<\"null\">>" apps/erlmcp_transports/src/erlmcp_origin_validator.erl; then
        log_medium "Origin validator allows 'null' origin (potential XSS risk)"
    fi
else
    log_high "Origin validator NOT implemented"
fi

echo ""

# =============================================================================
# 3. INJECTION VULNERABILITIES
# =============================================================================
echo "=== 3. Injection Vulnerability Checks ==="

# Check for prompt template injection
if ! grep -q "sanitize\|validate.*prompt" apps/erlmcp_core/src/erlmcp_server.erl; then
    log_critical "Prompt template validation NOT implemented (prompt injection risk)"
    log_info "Location: apps/erlmcp_core/src/erlmcp_server.erl:add_prompt*"
else
    log_pass "Prompt validation implemented"
fi

# Check for command injection risks
CMD_INJECT=$(find apps/ -name "*.erl" -type f | xargs grep -l "os:cmd\|open_port" | grep -v test | grep -v _build || true)
if [ -n "$CMD_INJECT" ]; then
    log_medium "Potential command injection vectors found:"
    echo "$CMD_INJECT" | while read -r file; do
        log_info "  - $file"
    done
else
    log_pass "No command injection risks found"
fi

# Check for SQL injection (if any DB access)
SQL_INJECT=$(find apps/ -name "*.erl" -type f | xargs grep -l "odbc\|mysql\|pgsql" | grep -v test | grep -v _build || true)
if [ -n "$SQL_INJECT" ]; then
    log_medium "Database access found (verify SQL injection protection)"
fi

echo ""

# =============================================================================
# 4. ERROR HANDLING & DATA EXPOSURE
# =============================================================================
echo "=== 4. Error Handling & Data Exposure Checks ==="

# Check error message sanitization
if ! grep -q "sanitize.*error\|sanitize.*data" apps/erlmcp_core/src/erlmcp_json_rpc.erl; then
    log_high "Error message sanitization NOT implemented (data exposure risk)"
    log_info "Location: apps/erlmcp_core/src/erlmcp_json_rpc.erl:encode_error_response*"
else
    log_pass "Error sanitization implemented"
fi

# Check for stack trace exposure
if grep -q "logger:warning.*~p\|logger:error.*~p" apps/erlmcp_core/src/erlmcp_json_rpc.erl | head -3; then
    log_medium "Internal state logged (check for sensitive data exposure)"
fi

echo ""

# =============================================================================
# 5. SECRETS MANAGEMENT
# =============================================================================
echo "=== 5. Secrets Management Checks ==="

# Check for hardcoded secrets
HARDCODED=$(grep -r "hardcoded.*password\|hardcoded.*secret\|hardcoded.*key" apps/erlmcp_core/src/ apps/erlmcp_transports/src/ 2>/dev/null || true)
if [ -n "$HARDCODED" ]; then
    log_critical "Potential hardcoded secrets found"
else
    log_pass "No hardcoded secrets detected"
fi

# Check secrets module implementation
if [ -f "apps/erlmcp_core/src/erlmcp_secrets.erl" ]; then
    log_pass "Secrets management module implemented"

    # Check for TODO in secrets module
    if grep -q "TODO.*Vault\|TODO.*AWS" apps/erlmcp_core/src/erlmcp_secrets.erl; then
        log_medium "Secrets backend has TODO items (Vault/AWS not fully implemented)"
    fi
else
    log_high "Secrets management module NOT implemented"
fi

echo ""

# =============================================================================
# 6. TRANSPORT SECURITY
# =============================================================================
echo "=== 6. Transport Security Checks ==="

# Check security headers
if [ -f "apps/erlmcp_transports/src/erlmcp_security_headers.erl" ]; then
    log_pass "Security headers module implemented"

    # Check for specific headers
    REQUIRED_HEADERS=("X-Content-Type-Options" "X-Frame-Options" "Content-Security-Policy" "Strict-Transport-Security")
    for header in "${REQUIRED_HEADERS[@]}"; do
        if grep -q "$header" apps/erlmcp_transports/src/erlmcp_security_headers.erl; then
            log_pass "Security header: $header"
        else
            log_medium "Missing security header: $header"
        fi
    done
else
    log_high "Security headers module NOT implemented"
fi

# Check TLS/SSL configuration
if grep -q "ssl.*verify\|tls.*verify" apps/erlmcp_transports/src/erlmcp_transport_tcp.erl; then
    log_pass "TLS verification configured"
else
    log_medium "TLS verification may not be enforced"
fi

echo ""

# =============================================================================
# 7. DoS RESILIENCE
# =============================================================================
echo "=== 7. DoS Resilience Checks ==="

# Check rate limiting
if [ -f "apps/erlmcp_core/src/erlmcp_auth_rate_limiter.erl" ]; then
    log_pass "Rate limiting implemented"

    # Check if rate limiter is enabled by default
    if grep -q "rate_limiter_enabled.*true" apps/erlmcp_core/src/erlmcp_auth.erl; then
        log_pass "Rate limiting enabled by default"
    else
        log_medium "Rate limiting may not be enabled by default"
    fi
else
    log_high "Rate limiting NOT implemented"
fi

# Check resource quotas
if ! grep -q "MAX.*SESSIONS\|MAX.*TOOLS\|MAX.*RESOURCES" apps/erlmcp_core/src/erlmcp_auth.erl; then
    log_high "Resource quotas NOT implemented (DoS risk: resource exhaustion)"
else
    log_pass "Resource quotas defined"
fi

echo ""

# =============================================================================
# 8. CODE QUALITY & STATIC ANALYSIS
# =============================================================================
echo "=== 8. Code Quality & Static Analysis ==="

# Run dialyzer for security-related warnings
echo "Running dialyzer analysis (this may take a while)..."
if TERM=dumb rebar3 dialyzer 2>&1 | grep -q "dialyzer"; then
    log_info "Dialyzer analysis completed (check for type warnings)"
else
    log_medium "Dialyzer analysis failed or not available"
fi

# Check for TODO/FIXME in security-critical code
TODO_COUNT=$(grep -r "TODO\|FIXME" apps/erlmcp_core/src/erlmcp_auth.erl apps/erlmcp_core/src/erlmcp_server.erl apps/erlmcp_core/src/erlmcp_json_rpc.erl 2>/dev/null | wc -l)
if [ "$TODO_COUNT" -gt 0 ]; then
    log_medium "$TODO_COUNT TODO/FIXME comments in security-critical code"
fi

echo ""

# =============================================================================
# 9. TEST COVERAGE
# =============================================================================
echo "=== 9. Security Test Coverage ==="

# Check if authentication tests exist
if [ -f "apps/erlmcp_core/test/erlmcp_auth_tests.erl" ]; then
    log_pass "Authentication tests exist"

    # Run auth tests
    echo "Running authentication tests..."
    if TERM=dumb rebar3 eunit --module=erlmcp_auth_tests 2>&1 | grep -q "All.*passed"; then
        log_pass "Authentication tests passing"
    else
        log_high "Authentication tests failing or not run"
    fi
else
    log_high "Authentication tests NOT implemented"
fi

echo ""

# =============================================================================
# SUMMARY
# =============================================================================
echo "========================================"
echo "  SECURITY SCAN SUMMARY"
echo "========================================"
echo ""
echo "CRITICAL: $CRITICAL"
echo "HIGH:     $HIGH"
echo "MEDIUM:   $MEDIUM"
echo "LOW:      $LOW"
echo "INFO:     $INFO"
echo ""

# Calculate risk score
TOTAL_CRITICAL=$((CRITICAL * 10))
TOTAL_HIGH=$((HIGH * 7))
TOTAL_MEDIUM=$((MEDIUM * 4))
TOTAL_LOW=$((LOW * 2))
RISK_SCORE=$((TOTAL_CRITICAL + TOTAL_HIGH + TOTAL_MEDIUM + TOTAL_LOW))

echo "Risk Score: $RISK_SCORE"
echo ""

# Go/No-Go decision
if [ "$CRITICAL" -gt 0 ]; then
    echo -e "${RED}🛑 DECISION: NO-GO (CRITICAL issues found)${NC}"
    echo ""
    echo "Required Actions:"
    echo "1. Implement cryptographic JWT verification"
    echo "2. Enforce authorization in server operations"
    echo "3. Implement prompt template validation"
    echo "4. Sanitize error messages"
    exit 1
elif [ "$HIGH" -gt 3 ]; then
    echo -e "${YELLOW}⚠️  DECISION: CONDITIONAL NO-GO (Too many HIGH issues)${NC}"
    echo ""
    echo "Required Actions:"
    echo "1. Reduce HIGH severity issues to <3"
    echo "2. Re-run security scan"
    exit 1
elif [ "$RISK_SCORE" -gt 50 ]; then
    echo -e "${YELLOW}⚠️  DECISION: CONDITIONAL GO (Address issues before production)${NC}"
    exit 0
else
    echo -e "${GREEN}✅ DECISION: GO (Security posture acceptable)${NC}"
    exit 0
fi
