#!/usr/bin/env bash
# Run Common Test suites bypassing rebar3 format issues

set -euo pipefail

echo "======================================================================"
echo "Running Common Test Suites (bypassing rebar3 format)"
echo "======================================================================"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Clean old results
echo "Cleaning old test results..."
rm -rf _build/test/lib/erlmcp_*/test/*.beam
rm -rf test_results/*_run.*

# Compile tests manually using erlc
echo "Compiling test suites..."
compile_suite() {
    local app=$1
    local test_dir="apps/${app}/test"

    if [ ! -d "$test_dir" ]; then
        return 0
    fi

    echo "  Compiling ${app} tests..."
    for suite_file in ${test_dir}/*_SUITE.erl; do
        if [ -f "$suite_file" ]; then
            local suite_name=$(basename "$suite_file" .erl)
            echo "    - $suite_name"

            # Compile with proper include paths
            erlc \
                -I apps/${app}/include \
                -I _build/default/lib/*/include \
                -pa _build/default/lib/*/ebin \
                -o _build/test/lib/${app}/test/ \
                "$suite_file" 2>&1 | grep -v "Warning:" || true
        fi
    done
}

# Compile each app's test suites
for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
    compile_suite "$app"
done

echo ""
echo "Running Common Test suites..."
echo ""

# Track results
TOTAL=0
PASSED=0
FAILED=0

run_suite() {
    local suite=$1
    TOTAL=$((TOTAL + 1))

    echo "----------------------------------------------------------------------"
    echo "Running: $suite"
    echo "----------------------------------------------------------------------"

    if erl -pa _build/test/lib/*/ebin -pa _build/default/lib/*/ebin \
        -eval "ct:run_test([{suite, '$suite'}, {logdir, \"test_results/ct_run\"}])" \
        -s init stop -noshell 2>&1 | tee "test_results/ct_${suite}.log" | grep -q "All tests passed"; then
        echo "${GREEN}✓ $suite PASSED${NC}"
        PASSED=$((PASSED + 1))
    else
        echo "${RED}✗ $suite FAILED${NC}"
        FAILED=$((FAILED + 1))
    fi
    echo ""
}

# Run all test suites
run_suite "erlmcp_authorization_SUITE"
run_suite "erlmcp_error_handling_robustness_SUITE"
run_suite "erlmcp_error_recovery_SUITE"
run_suite "erlmcp_integration_contracts_SUITE"
run_suite "erlmcp_lifecycle_advanced_SUITE"
run_suite "erlmcp_network_failure_recovery_SUITE"
run_suite "erlmcp_performance_validator_SUITE"
run_suite "erlmcp_security_comprehensive_SUITE"
run_suite "erlmcp_spec_compliance_SUITE"
run_suite "erlmcp_validation_SUITE"
run_suite "erlmcp_error_response_SUITE"
run_suite "erlmcp_observability_SUITE"
run_suite "erlmcp_performance_regression_SUITE"
run_suite "erlmcp_transport_behavior_SUITE"
run_suite "erlmcp_transport_http_SUITE"
run_suite "erlmcp_transport_integration_SUITE"

# Summary
echo "======================================================================"
echo "Common Test Summary"
echo "======================================================================"
echo "Total:   $TOTAL"
echo "${GREEN}Passed:  $PASSED${NC}"
if [ $FAILED -gt 0 ]; then
    echo "${RED}Failed:  $FAILED${NC}"
else
    echo "Failed:  $FAILED"
fi
echo "======================================================================"

if [ $FAILED -eq 0 ]; then
    echo "${GREEN}✓ All Common Test suites PASSED${NC}"
    exit 0
else
    echo "${RED}✗ Some Common Test suites FAILED${NC}"
    exit 1
fi
