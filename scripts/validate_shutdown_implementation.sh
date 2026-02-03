#!/usr/bin/env bash
# Validation script for erlmcp_shutdown implementation
# Checks code quality, type specs, and implementation completeness

set -eo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

# Helper functions
check_passed() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASSED_CHECKS++))
    ((TOTAL_CHECKS++))
}

check_failed() {
    echo -e "${RED}✗${NC} $1"
    ((FAILED_CHECKS++))
    ((TOTAL_CHECKS++))
}

check_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

echo "======================================"
echo "erlmcp_shutdown Implementation Validation"
echo "======================================"
echo ""

# Check 1: Source file exists
echo "1. Checking source file..."
if [ -f "apps/erlmcp_core/src/erlmcp_shutdown.erl" ]; then
    check_passed "Source file exists: apps/erlmcp_core/src/erlmcp_shutdown.erl"
else
    check_failed "Source file not found"
    exit 1
fi

# Check 2: Test file exists
echo ""
echo "2. Checking test file..."
if [ -f "apps/erlmcp_core/test/erlmcp_shutdown_tests.erl" ]; then
    check_passed "Test file exists: apps/erlmcp_core/test/erlmcp_shutdown_tests.erl"
else
    check_failed "Test file not found"
fi

# Check 3: Module exports
echo ""
echo "3. Checking module exports..."
EXPORTS=$(grep -E "^-export\(" apps/erlmcp_core/src/erlmcp_shutdown.erl | wc -l)

REQUIRED_EXPORTS=(
    "start_link/0"
    "shutdown/1"
    "shutdown/2"
    "shutdown_now/0"
    "get_status/0"
    "cancel_shutdown/0"
    "register_cleanup_handler/2"
    "unregister_cleanup_handler/1"
    "await_shutdown/0"
    "await_shutdown/1"
)

for export in "${REQUIRED_EXPORTS[@]}"; do
    if grep -q "$export" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Export found: $export"
    else
        check_failed "Export missing: $export"
    fi
done

# Check 4: gen_server callbacks
echo ""
echo "4. Checking gen_server callbacks..."
CALLBACKS=(
    "init/1"
    "handle_call/3"
    "handle_cast/2"
    "handle_info/2"
    "terminate/2"
    "code_change/3"
)

for callback in "${CALLBACKS[@]}"; do
    if grep -q "^[[:space:]]*$callback" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Callback found: $callback"
    else
        check_failed "Callback missing: $callback"
    fi
done

# Check 5: Type specifications
echo ""
echo "5. Checking type specifications..."
TYPE_SPECS=(
    "start_link()"
    "shutdown(term())"
    "shutdown(term(), pos_integer())"
    "shutdown_now()"
    "get_status()"
    "cancel_shutdown()"
    "register_cleanup_handler(term(), cleanup_handler())"
    "unregister_cleanup_handler(term())"
    "await_shutdown()"
    "await_shutdown(timeout())"
)

for spec in "${TYPE_SPECS[@]}"; do
    if grep -q "-spec $spec" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Type spec found: $spec"
    else
        check_failed "Type spec missing: $spec"
    fi
done

# Check 6: Shutdown phases implementation
echo ""
echo "6. Checking shutdown phases..."
PHASES=(
    "phase_initiation"
    "phase_drain"
    "proceed_to_cleanup"
    "phase_state_persistence"
)

for phase in "${PHASES[@]}"; do
    if grep -q "^[[:space:]]*$phase(" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Phase function found: $phase"
    else
        check_failed "Phase function missing: $phase"
    fi
done

# Check 7: Helper functions
echo ""
echo "7. Checking helper functions..."
HELPERS=(
    "notify_processes"
    "stop_accepting_connections"
    "get_connection_counts"
    "run_cleanup_handlers"
    "close_connection_pools"
    "stop_transports"
    "stop_servers"
    "save_registry_state"
    "persist_metrics"
    "flush_telemetry"
)

for helper in "${HELPERS[@]}"; do
    if grep -q "^[[:space:]]*$helper(" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Helper function found: $helper"
    else
        check_failed "Helper function missing: $helper"
    fi
done

# Check 8: Documentation
echo ""
echo "8. Checking documentation..."
if grep -q "@doc" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
    check_passed "Module documentation present"
else
    check_failed "Module documentation missing"
fi

# Count number of @doc tags
DOC_COUNT=$(grep -c "@doc" apps/erlmcp_core/src/erlmcp_shutdown.erl)
if [ "$DOC_COUNT" -gt 10 ]; then
    check_passed "Function documentation adequate ($DOC_COUNT @doc tags)"
else
    check_warning "Low function documentation count ($DOC_COUNT @doc tags)"
fi

# Check 9: Error handling
echo ""
echo "9. Checking error handling..."
ERROR_PATTERNS=(
    "try"
    "catch"
    "logger:error"
    "logger:warning"
)

for pattern in "${ERROR_PATTERNS[@]}"; do
    if grep -q "$pattern" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Error handling pattern found: $pattern"
    else
        check_warning "Error handling pattern missing: $pattern"
    fi
done

# Check 10: Constants defined
echo ""
echo "10. Checking constants..."
CONSTANTS=(
    "DEFAULT_DRAIN_TIMEOUT"
    "MIN_DRAIN_TIMEOUT"
    "MAX_DRAIN_TIMEOUT"
    "SHUTDOWN_STATUS_TABLE"
    "CLEANUP_HANDLERS_TABLE"
)

for const in "${CONSTANTS[@]}"; do
    if grep -q "define($const" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Constant defined: $const"
    else
        check_failed "Constant missing: $const"
    fi
done

# Check 11: Records defined
echo ""
echo "11. Checking records..."
RECORDS=(
    "shutdown_phase"
    "shutdown_status"
    "cleanup_handler"
)

for record in "${RECORDS[@]}"; do
    if grep -q "record($record" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Record defined: $record"
    else
        check_failed "Record missing: $record"
    fi
done

# Check 12: Integration with existing modules
echo ""
echo "12. Checking integration..."
INTEGRATIONS=(
    "erlmcp_registry"
    "erlmcp_transport_sup"
    "erlmcp_connection_pool"
    "erlmcp_otel"
)

for module in "${INTEGRATIONS[@]}"; do
    if grep -q "$module:" apps/erlmcp_core/src/erlmcp_shutdown.erl; then
        check_passed "Integration found: $module"
    else
        check_warning "Integration missing: $module"
    fi
done

# Check 13: Test coverage
echo ""
echo "13. Checking test coverage..."
TEST_FUNCTIONS=(
    "startup_test"
    "get_status_not_shutting_down_test"
    "initiate_shutdown_test"
    "shutdown_custom_timeout_test"
    "duplicate_shutdown_test"
    "register_cleanup_handler_test"
    "unregister_cleanup_handler_test"
    "cleanup_handler_priority_test"
    "shutdown_phases_test"
    "connection_counts_test"
)

for test in "${TEST_FUNCTIONS[@]}"; do
    if grep -q "^[[:space:]]*$test()" apps/erlmcp_core/test/erlmcp_shutdown_tests.erl; then
        check_passed "Test function found: $test"
    else
        check_failed "Test function missing: $test"
    fi
done

# Summary
echo ""
echo "======================================"
echo "Validation Summary"
echo "======================================"
echo "Total checks: $TOTAL_CHECKS"
echo -e "${GREEN}Passed:${NC} $PASSED_CHECKS"
echo -e "${RED}Failed:${NC} $FAILED_CHECKS"

if [ $FAILED_CHECKS -eq 0 ]; then
    echo ""
    echo -e "${GREEN}All checks passed!${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}Some checks failed. Please review.${NC}"
    exit 1
fi
