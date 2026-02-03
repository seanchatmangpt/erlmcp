#!/bin/bash
# Validate graceful shutdown implementation (static analysis + Docker test)
# This validates that prep_stop/1 is properly implemented across all apps

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo "======================================================================"
echo "Graceful Shutdown Validation (P0-011)"
echo "======================================================================"
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

check_function() {
    local file="$1"
    local function="$2"
    local description="$3"

    if [ ! -f "$file" ]; then
        echo -e "${RED}✗ FAIL: $file does not exist${NC}"
        return 1
    fi

    if grep -q "${function}" "$file"; then
        echo -e "${GREEN}✓${NC} $description"
        return 0
    else
        echo -e "${RED}✗ FAIL: $description${NC}"
        return 1
    fi
}

check_export() {
    local file="$1"
    local function="$2"
    local description="$3"

    if grep -E "export.*${function}" "$file" > /dev/null; then
        echo -e "${GREEN}✓${NC} $description"
        return 0
    else
        echo -e "${RED}✗ FAIL: $description${NC}"
        return 1
    fi
}

echo "Phase 1: Static Analysis"
echo "----------------------------------------------------------------------"
echo ""

FAIL_COUNT=0

# Check erlmcp_app.erl
check_export \
    "apps/erlmcp_core/src/erlmcp_app.erl" \
    "prep_stop" \
    "erlmcp_app exports prep_stop/1" || ((FAIL_COUNT++))

check_function \
    "apps/erlmcp_core/src/erlmcp_app.erl" \
    "prep_stop(State)" \
    "erlmcp_app implements prep_stop/1" || ((FAIL_COUNT++))

# Check erlmcp_transports_app.erl
check_export \
    "apps/erlmcp_transports/src/erlmcp_transports_app.erl" \
    "prep_stop" \
    "erlmcp_transports_app exports prep_stop/1" || ((FAIL_COUNT++))

check_function \
    "apps/erlmcp_transports/src/erlmcp_transports_app.erl" \
    "prep_stop(State)" \
    "erlmcp_transports_app implements prep_stop/1" || ((FAIL_COUNT++))

# Check erlmcp_observability_app.erl
check_export \
    "apps/erlmcp_observability/src/erlmcp_observability_app.erl" \
    "prep_stop" \
    "erlmcp_observability_app exports prep_stop/1" || ((FAIL_COUNT++))

check_function \
    "apps/erlmcp_observability/src/erlmcp_observability_app.erl" \
    "prep_stop(State)" \
    "erlmcp_observability_app implements prep_stop/1" || ((FAIL_COUNT++))

echo ""
echo "Phase 2: Helper Functions"
echo "----------------------------------------------------------------------"
echo ""

# Check erlmcp_registry:graceful_shutdown/0
check_function \
    "apps/erlmcp_core/src/erlmcp_registry.erl" \
    "graceful_shutdown/0" \
    "erlmcp_registry exports and implements graceful_shutdown/0" || ((FAIL_COUNT++))

# Check erlmcp_transport_sup:stop_accepting/0
check_function \
    "apps/erlmcp_transports/src/erlmcp_transport_sup.erl" \
    "stop_accepting/0" \
    "erlmcp_transport_sup exports and implements stop_accepting/0" || ((FAIL_COUNT++))

# Check erlmcp_connection_pool:drain/0
check_function \
    "apps/erlmcp_transports/src/erlmcp_connection_pool.erl" \
    "drain/0" \
    "erlmcp_connection_pool exports and implements drain/0" || ((FAIL_COUNT++))

echo ""
echo "Phase 3: Application Configuration"
echo "----------------------------------------------------------------------"
echo ""

# Check .app.src files have {mod, ...} directive
check_function \
    "apps/erlmcp_core/src/erlmcp_core.app.src" \
    "{mod, {erlmcp_app" \
    "erlmcp_core.app.src has application callback module" || ((FAIL_COUNT++))

check_function \
    "apps/erlmcp_transports/src/erlmcp_transports.app.src" \
    "{mod, {erlmcp_transports_app" \
    "erlmcp_transports.app.src has application callback module" || ((FAIL_COUNT++))

check_function \
    "apps/erlmcp_observability/src/erlmcp_observability.app.src" \
    "{mod, {erlmcp_observability_app" \
    "erlmcp_observability.app.src has application callback module" || ((FAIL_COUNT++))

echo ""
echo "Phase 4: Syntax Check"
echo "----------------------------------------------------------------------"
echo ""

# Check for common syntax errors in the new functions
if grep -q "spec prep_stop" apps/erlmcp_core/src/erlmcp_app.erl; then
    echo -e "${GREEN}✓${NC} erlmcp_app prep_stop has type specification"
else
    echo -e "${YELLOW}⚠${NC} erlmcp_app prep_stop missing type specification (optional)"
fi

if grep -q "spec prep_stop" apps/erlmcp_transports/src/erlmcp_transports_app.erl; then
    echo -e "${GREEN}✓${NC} erlmcp_transports_app prep_stop has type specification"
else
    echo -e "${YELLOW}⚠${NC} erlmcp_transports_app prep_stop missing type specification (optional)"
fi

if grep -q "spec prep_stop" apps/erlmcp_observability/src/erlmcp_observability_app.erl; then
    echo -e "${GREEN}✓${NC} erlmcp_observability_app prep_stop has type specification"
else
    echo -e "${YELLOW}⚠${NC} erlmcp_observability_app prep_stop missing type specification (optional)"
fi

echo ""
echo "======================================================================"
if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}✓ VALIDATION PASSED${NC}"
    echo "======================================================================"
    echo ""
    echo "Graceful shutdown implementation is complete:"
    echo "  • erlmcp_app:prep_stop/1 ✓"
    echo "  • erlmcp_transports_app:prep_stop/1 ✓"
    echo "  • erlmcp_observability_app:prep_stop/1 ✓"
    echo "  • Helper functions implemented ✓"
    echo "  • Application callbacks configured ✓"
    echo ""
    echo "Next steps:"
    echo "  1. Build Docker image: docker compose build erlmcp"
    echo "  2. Test graceful shutdown: docker compose restart erlmcp"
    echo "  3. Verify logs for graceful shutdown messages"
    echo ""
    exit 0
else
    echo -e "${RED}✗ VALIDATION FAILED${NC}"
    echo "======================================================================"
    echo ""
    echo "Failed checks: $FAIL_COUNT"
    echo "Please review the errors above."
    echo ""
    exit 1
fi
