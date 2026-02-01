#!/usr/bin/env bash
# scripts/test/quick.sh - Quick test tier (target: ≤10 min)
# Smoke + important integration tests, excluding chaos/performance

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

START_TIME=$(date +%s)

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}⚡ QUICK TEST TIER (Target: ≤10 min)${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${BLUE}Scope:${NC} Smoke + core integration tests"
echo -e "${BLUE}  - All smoke tests${NC}"
echo -e "${BLUE}  - HTTP/WebSocket/SSE transports${NC}"
echo -e "${BLUE}  - Tools/Resources/Prompts APIs${NC}"
echo -e "${BLUE}  - Transport validators${NC}"
echo -e "${BLUE}  - Auth & security${NC}"
echo -e "${BLUE}  - Core integration suites${NC}"
echo ""
echo -e "${YELLOW}Excluded:${NC} Chaos, performance, property-based tests"
echo ""

# Track failures
FAILED_TESTS=()
FAILED_SUITES=()

# Test function
run_test_module() {
    local module=$1
    local description=$2

    echo -e "${BLUE}Testing:${NC} ${description} (${module})"

    if rebar3 eunit --module="${module}" > /tmp/quick_${module}.log 2>&1; then
        echo -e "${GREEN}  ✓ PASS${NC}"
        return 0
    else
        echo -e "${RED}  ✗ FAIL${NC}"
        FAILED_TESTS+=("${module}")
        return 1
    fi
}

# CT suite function
run_ct_suite() {
    local suite=$1
    local description=$2

    echo -e "${BLUE}CT Suite:${NC} ${description} (${suite})"

    if rebar3 ct --suite="${suite}" > /tmp/quick_ct_${suite##*/}.log 2>&1; then
        echo -e "${GREEN}  ✓ PASS${NC}"
        return 0
    else
        echo -e "${RED}  ✗ FAIL${NC}"
        FAILED_SUITES+=("${suite}")
        return 1
    fi
}

# ============================================================================
# TIER 1: RUN SMOKE TESTS
# ============================================================================

echo -e "${BLUE}[1/10] Running Smoke Tests${NC}"
echo ""

if bash scripts/test/smoke.sh; then
    echo -e "${GREEN}  ✓ Smoke tests passed${NC}"
else
    echo -e "${RED}  ✗ Smoke tests failed${NC}"
    echo -e "${RED}     Aborting quick tests (fix smoke tests first)${NC}"
    exit 1
fi

echo ""

# ============================================================================
# TIER 2: TRANSPORT TESTS (HTTP, WS, SSE)
# ============================================================================

echo -e "${BLUE}[2/10] Transport Tests${NC}"
echo ""

run_test_module "erlmcp_transport_http_tests" "HTTP transport" || true
run_test_module "erlmcp_transport_ws_tests" "WebSocket transport" || true
run_test_module "erlmcp_transport_sse_tests" "SSE transport" || true
run_test_module "erlmcp_transport_sse_connection_tests" "SSE connections" || true
run_test_module "erlmcp_transport_ws_connection_tests" "WS connections" || true
run_test_module "erlmcp_transport_ws_message_tests" "WS messages" || true
run_test_module "erlmcp_tcp_client_tests" "TCP client" || true
run_test_module "erlmcp_tcp_server_tests" "TCP server" || true

echo ""

# ============================================================================
# TIER 3: VALIDATION & SECURITY
# ============================================================================

echo -e "${BLUE}[3/10] Validation & Security Tests${NC}"
echo ""

run_test_module "erlmcp_origin_validator_tests" "Origin validation" || true
run_test_module "erlmcp_http_header_validator_tests" "HTTP header validation" || true
run_test_module "erlmcp_security_headers_tests" "Security headers" || true
run_test_module "erlmcp_transport_validator_tests" "Transport validator" || true
run_test_module "erlmcp_protocol_validator_tests" "Protocol validator" || true
run_test_module "erlmcp_security_validator_tests" "Security validator" || true

echo ""

# ============================================================================
# TIER 4: TOOLS API
# ============================================================================

echo -e "${BLUE}[4/10] Tools API Tests${NC}"
echo ""

run_test_module "erlmcp_server_tools_tests" "Server tools" || true
run_test_module "erlmcp_client_tool_calls_tests" "Client tool calls" || true
run_test_module "erlmcp_tool_validation_tests" "Tool validation" || true

echo ""

# ============================================================================
# TIER 5: RESOURCES API
# ============================================================================

echo -e "${BLUE}[5/10] Resources API Tests${NC}"
echo ""

run_test_module "erlmcp_resource_tests" "Resource core" || true
run_test_module "erlmcp_resource_encoding_tests" "Resource encoding" || true
run_test_module "erlmcp_resource_validation_tests" "Resource validation" || true
run_test_module "erlmcp_resource_subscriptions_tests" "Resource subscriptions" || true
run_test_module "erlmcp_resource_integration_tests" "Resource integration" || true

echo ""

# ============================================================================
# TIER 6: PROMPTS API
# ============================================================================

echo -e "${BLUE}[6/10] Prompts API Tests${NC}"
echo ""

run_test_module "erlmcp_server_prompts_tests" "Server prompts" || true
run_test_module "erlmcp_prompt_template_tests" "Prompt templates" || true
run_test_module "erlmcp_prompt_rendering_tests" "Prompt rendering" || true
run_test_module "erlmcp_prompt_validation_tests" "Prompt validation" || true
run_test_module "erlmcp_prompt_integration_tests" "Prompt integration" || true

echo ""

# ============================================================================
# TIER 7: CAPABILITIES & AUTH
# ============================================================================

echo -e "${BLUE}[7/10] Capabilities & Auth Tests${NC}"
echo ""

run_test_module "erlmcp_capability_negotiation_tests" "Capability negotiation" || true
run_test_module "erlmcp_capabilities_negotiation_tests" "Capabilities negotiation" || true
run_test_module "erlmcp_capabilities_query_tests" "Capabilities query" || true
run_test_module "erlmcp_auth_api_tests" "Auth API" || true
run_test_module "erlmcp_auth_jwt_tests" "JWT auth" || true
run_test_module "erlmcp_auth_oauth_tests" "OAuth auth" || true
run_test_module "erlmcp_auth_integration_tests" "Auth integration" || true

echo ""

# ============================================================================
# TIER 8: SESSION & CACHING
# ============================================================================

echo -e "${BLUE}[8/10] Session & Caching Tests${NC}"
echo ""

run_test_module "erlmcp_session_tests" "Session core" || true
run_test_module "erlmcp_session_manager_basic_tests" "Session manager basics" || true
run_test_module "erlmcp_session_manager_tests" "Session manager" || true
run_test_module "erlmcp_cache_tests" "Cache core" || true
run_test_module "erlmcp_icon_cache_tests" "Icon cache" || true

echo ""

# ============================================================================
# TIER 9: SCHEMA & VALIDATION
# ============================================================================

echo -e "${BLUE}[9/10] Schema & Validation Tests${NC}"
echo ""

run_test_module "erlmcp_schema_validation_tests" "Schema validation" || true
run_test_module "erlmcp_schema_registry_tests" "Schema registry" || true
run_test_module "erlmcp_schema_registry_validation_tests" "Schema registry validation" || true
run_test_module "erlmcp_schema_type_validation_tests" "Schema type validation" || true
run_test_module "erlmcp_schema_defaults_tests" "Schema defaults" || true
run_test_module "erlmcp_sampling_validation_tests" "Sampling validation" || true

echo ""

# ============================================================================
# TIER 10: CORE INTEGRATION SUITES (CT)
# ============================================================================

echo -e "${BLUE}[10/10] Integration Test Suites (CT)${NC}"
echo ""

run_ct_suite "apps/erlmcp_core/test/erlmcp_integration_SUITE" "Core integration" || true
run_ct_suite "apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE" "Transport integration" || true
run_ct_suite "apps/erlmcp_validation/test/erlmcp_integration_contracts_SUITE" "Integration contracts" || true
run_ct_suite "apps/erlmcp_validation/test/erlmcp_error_response_SUITE" "Error responses" || true

echo ""

# ============================================================================
# RESULTS
# ============================================================================

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"

TOTAL_FAILURES=$((${#FAILED_TESTS[@]} + ${#FAILED_SUITES[@]}))

if [ $TOTAL_FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ QUICK TESTS PASSED${NC}"
    echo ""
    echo -e "${GREEN}  All core tests passed${NC}"
    echo -e "${GREEN}  Elapsed time: ${ELAPSED}s ($(($ELAPSED / 60))m $(($ELAPSED % 60))s)${NC}"
    if [ $ELAPSED -le 600 ]; then
        echo -e "${GREEN}  Performance: ✓ (target: ≤600s)${NC}"
    else
        echo -e "${YELLOW}  Performance: ⚠ (${ELAPSED}s > 600s target)${NC}"
    fi
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ QUICK TESTS FAILED${NC}"
    echo ""
    echo -e "${RED}  Failed tests: ${#FAILED_TESTS[@]}${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo -e "${RED}    - ${test}${NC}"
        echo -e "${YELLOW}      Log: /tmp/quick_${test}.log${NC}"
    done

    if [ ${#FAILED_SUITES[@]} -gt 0 ]; then
        echo ""
        echo -e "${RED}  Failed CT suites: ${#FAILED_SUITES[@]}${NC}"
        for suite in "${FAILED_SUITES[@]}"; do
            echo -e "${RED}    - ${suite}${NC}"
            echo -e "${YELLOW}      Log: /tmp/quick_ct_${suite##*/}.log${NC}"
        done
    fi

    echo ""
    echo -e "${RED}  Elapsed time: ${ELAPSED}s ($(($ELAPSED / 60))m $(($ELAPSED % 60))s)${NC}"
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi
