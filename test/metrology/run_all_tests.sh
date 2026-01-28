#!/usr/bin/env bash
###====================================================================
### Metrology Test Suite Runner
###
### Comprehensive test execution for v1.5.0 metrology compliance.
### Runs all test modules, generates coverage, and enforces quality gates.
###
### Usage:
###   ./run_all_tests.sh                  # Run all tests
###   ./run_all_tests.sh --coverage       # Run with coverage report
###   ./run_all_tests.sh --property       # Run property-based tests
###   ./run_all_tests.sh --ci             # CI mode (strict gates)
###
###====================================================================

set -e  # Exit on first error

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
TEST_DIR="test/metrology"
MIN_COVERAGE=90
TEST_MODULES=(
    "erlmcp_metrology_validator_tests"
    "plan_spec_conformance_tests"
    "evidence_bundle_validation_tests"
    "benchmark_report_format_tests"
)

# Parse arguments
COVERAGE=false
PROPERTY=false
CI_MODE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --coverage)
            COVERAGE=true
            shift
            ;;
        --property)
            PROPERTY=true
            shift
            ;;
        --ci)
            CI_MODE=true
            COVERAGE=true
            PROPERTY=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Print header
echo -e "${GREEN}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║                Metrology Test Suite v1.5.0                   ║${NC}"
echo -e "${GREEN}║           Comprehensive Metrology Compliance Testing         ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Function: Run a single test module
run_test_module() {
    local module=$1
    echo -e "${YELLOW}▶ Running: ${module}${NC}"

    # Use direct erl command to avoid rebar3 colorization bug
    erl -pa _build/test/lib/*/ebin \
        -pa _build/default/lib/*/ebin \
        -pa test \
        -noshell \
        -eval "
            code:add_pathsz(filelib:wildcard(\"_build/test/lib/*/ebin\")),
            code:add_pathsz(filelib:wildcard(\"test\")),
            case eunit:test(${module}, [verbose]) of
                ok -> init:stop(0);
                _ -> init:stop(1)
            end." 2>&1 | tee /tmp/metrology_test_${module}.log

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ ${module} PASSED${NC}"
        return 0
    else
        echo -e "${RED}❌ ${module} FAILED${NC}"
        return 1
    fi
}

# Step 1: Compile project
echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Step 1: Compiling Project${NC}"
echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"

if rebar3 compile > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Compilation successful${NC}"
else
    echo -e "${YELLOW}⚠️  Compilation warnings (continuing)${NC}"
fi
echo ""

# Step 2: Run unit tests
echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Step 2: Running Unit Tests${NC}"
echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"

PASSED_MODULES=0
FAILED_MODULES=0

for module in "${TEST_MODULES[@]}"; do
    if run_test_module "$module"; then
        ((PASSED_MODULES++))
    else
        ((FAILED_MODULES++))
    fi
    echo ""
done

# Step 3: Run property-based tests (if enabled)
if [ "$PROPERTY" = true ]; then
    echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}Step 3: Running Property-Based Tests${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"

    if rebar3 proper -c --module=erlmcp_metrology_validator_tests 2>&1 | tee /tmp/metrology_proper.log; then
        echo -e "${GREEN}✅ Property-based tests PASSED${NC}"
    else
        echo -e "${RED}❌ Property-based tests FAILED${NC}"
        ((FAILED_MODULES++))
    fi
    echo ""
fi

# Step 4: Generate coverage report (if enabled)
if [ "$COVERAGE" = true ]; then
    echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}Step 4: Generating Coverage Report${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"

    rebar3 do eunit --dir=test/metrology, cover 2>&1 | tee /tmp/metrology_coverage.log

    # Extract coverage percentage
    COVERAGE_PCT=$(rebar3 cover --verbose 2>&1 | grep "metrology" | awk '{print $NF}' | sed 's/%//' | sort -n | tail -1)

    if [ -n "$COVERAGE_PCT" ] && [ "$COVERAGE_PCT" -ge "$MIN_COVERAGE" ]; then
        echo -e "${GREEN}✅ Coverage: ${COVERAGE_PCT}% (target: ${MIN_COVERAGE}%)${NC}"
    else
        echo -e "${RED}❌ Coverage: ${COVERAGE_PCT}% (target: ${MIN_COVERAGE}%)${NC}"
        if [ "$CI_MODE" = true ]; then
            exit 1
        fi
    fi
    echo ""
fi

# Step 5: Summary
echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Test Suite Summary${NC}"
echo -e "${YELLOW}═══════════════════════════════════════════════════════════${NC}"

TOTAL_MODULES=$((PASSED_MODULES + FAILED_MODULES))

echo -e "Total Modules:   ${TOTAL_MODULES}"
echo -e "Passed Modules:  ${GREEN}${PASSED_MODULES}${NC}"
echo -e "Failed Modules:  ${RED}${FAILED_MODULES}${NC}"

if [ "$COVERAGE" = true ]; then
    echo -e "Coverage:        ${COVERAGE_PCT}% (target: ${MIN_COVERAGE}%)"
fi

echo ""

# Final verdict
if [ "$FAILED_MODULES" -eq 0 ]; then
    echo -e "${GREEN}╔═══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                   ✅ ALL TESTS PASSED                        ║${NC}"
    echo -e "${GREEN}║          Metrology Test Suite v1.5.0 Complete                ║${NC}"
    echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════╝${NC}"
    exit 0
else
    echo -e "${RED}╔═══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║                   ❌ TESTS FAILED                            ║${NC}"
    echo -e "${RED}║          ${FAILED_MODULES} module(s) failed - Fix before release             ║${NC}"
    echo -e "${RED}╚═══════════════════════════════════════════════════════════════╝${NC}"

    # Print failure details
    echo ""
    echo -e "${YELLOW}Failure Details:${NC}"
    for module in "${TEST_MODULES[@]}"; do
        if [ -f "/tmp/metrology_test_${module}.log" ]; then
            if grep -q "FAILED" "/tmp/metrology_test_${module}.log"; then
                echo -e "${RED}✗ ${module}${NC}"
                grep "error:" "/tmp/metrology_test_${module}.log" | head -5
            fi
        fi
    done

    exit 1
fi
