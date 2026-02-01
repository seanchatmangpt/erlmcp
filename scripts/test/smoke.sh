#!/usr/bin/env bash
# scripts/test/smoke.sh - Smoke test tier (target: ≤2 min)
# Fast, critical tests: codec, lifecycle, basic transport, validators

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
echo -e "${BLUE}🔥 SMOKE TEST TIER (Target: ≤2 min)${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${BLUE}Scope:${NC} Critical path tests only"
echo -e "${BLUE}  - JSON-RPC codec (encode/decode)${NC}"
echo -e "${BLUE}  - Message parsing${NC}"
echo -e "${BLUE}  - Basic lifecycle (init/terminate)${NC}"
echo -e "${BLUE}  - stdio transport${NC}"
echo -e "${BLUE}  - Registry basics${NC}"
echo -e "${BLUE}  - Validator CLI${NC}"
echo ""

# Track failures
FAILED_TESTS=()

# Test function
run_test_module() {
    local module=$1
    local description=$2

    echo -e "${BLUE}Testing:${NC} ${description} (${module})"

    if rebar3 eunit --module="${module}" > /tmp/smoke_${module}.log 2>&1; then
        echo -e "${GREEN}  ✓ PASS${NC}"
        return 0
    else
        echo -e "${RED}  ✗ FAIL${NC}"
        FAILED_TESTS+=("${module}")
        return 1
    fi
}

# ============================================================================
# TIER 1: CODEC & PROTOCOL (most critical)
# ============================================================================

echo -e "${BLUE}[1/6] Codec & Protocol Tests${NC}"
echo ""

run_test_module "erlmcp_json_rpc_tests" "JSON-RPC core" || true
run_test_module "erlmcp_json_rpc_encoding_tests" "JSON-RPC encoding" || true
run_test_module "erlmcp_json_rpc_request_tests" "JSON-RPC requests" || true
run_test_module "erlmcp_json_rpc_response_tests" "JSON-RPC responses" || true
run_test_module "erlmcp_message_parser_tests" "Message parser" || true
run_test_module "erlmcp_request_id_tests" "Request ID correlation" || true

echo ""

# ============================================================================
# TIER 2: LIFECYCLE
# ============================================================================

echo -e "${BLUE}[2/6] Lifecycle Tests${NC}"
echo ""

run_test_module "erlmcp_client_basic_tests" "Client lifecycle" || true
run_test_module "erlmcp_server_basic_tests" "Server lifecycle" || true
run_test_module "erlmcp_ping_tests" "Ping/heartbeat" || true

echo ""

# ============================================================================
# TIER 3: REGISTRY
# ============================================================================

echo -e "${BLUE}[3/6] Registry Tests${NC}"
echo ""

run_test_module "erlmcp_registry_basic_tests" "Registry basics" || true
run_test_module "erlmcp_registry_tests" "Registry core" || true

echo ""

# ============================================================================
# TIER 4: TRANSPORT BASICS
# ============================================================================

echo -e "${BLUE}[4/6] Transport Tests${NC}"
echo ""

run_test_module "erlmcp_transport_stdio_tests" "stdio transport" || true

echo ""

# ============================================================================
# TIER 5: VALIDATION CLI
# ============================================================================

echo -e "${BLUE}[5/6] Validation Tests${NC}"
echo ""

run_test_module "erlmcp_validate_cli_tests" "Validator CLI" || true
run_test_module "erlmcp_uri_validator_tests" "URI validator" || true

echo ""

# ============================================================================
# TIER 6: SUPERVISOR
# ============================================================================

echo -e "${BLUE}[6/6] Supervision Tests${NC}"
echo ""

run_test_module "erlmcp_supervisor_tests" "Supervisor tree" || true

echo ""

# ============================================================================
# RESULTS
# ============================================================================

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"

if [ ${#FAILED_TESTS[@]} -eq 0 ]; then
    echo -e "${GREEN}✅ SMOKE TESTS PASSED${NC}"
    echo ""
    echo -e "${GREEN}  All critical tests passed${NC}"
    echo -e "${GREEN}  Elapsed time: ${ELAPSED}s${NC}"
    if [ $ELAPSED -le 120 ]; then
        echo -e "${GREEN}  Performance: ✓ (target: ≤120s)${NC}"
    else
        echo -e "${YELLOW}  Performance: ⚠ (${ELAPSED}s > 120s target)${NC}"
    fi
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ SMOKE TESTS FAILED${NC}"
    echo ""
    echo -e "${RED}  Failed tests: ${#FAILED_TESTS[@]}${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo -e "${RED}    - ${test}${NC}"
        echo -e "${YELLOW}      Log: /tmp/smoke_${test}.log${NC}"
    done
    echo ""
    echo -e "${RED}  Elapsed time: ${ELAPSED}s${NC}"
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi
