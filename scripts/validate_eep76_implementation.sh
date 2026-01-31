#!/usr/bin/env bash
# EEP 76 Priority Messages Implementation Validation Script
# Purpose: Verify priority message implementation across all affected modules
# Author: Claude Code Agent
# Date: 2026-01-31

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counter for test results
PASS=0
FAIL=0

# Print section header
print_section() {
    echo -e "\n${BLUE}===================================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}===================================================================${NC}\n"
}

# Print test result
print_result() {
    if [ "$1" -eq 0 ]; then
        echo -e "${GREEN}✅ PASS${NC}: $2"
        ((PASS++))
    else
        echo -e "${RED}❌ FAIL${NC}: $2"
        ((FAIL++))
    fi
}

# Check if OTP version is available
check_otp_version() {
    print_section "OTP Version Detection"

    if command -v erl &> /dev/null; then
        OTP_VERSION=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1)
        echo "OTP Version: $OTP_VERSION"

        if [[ "$OTP_VERSION" == *"28"* ]]; then
            echo -e "${GREEN}OTP 28 detected - Priority messages available${NC}"
            return 0
        else
            echo -e "${YELLOW}OTP $OTP_VERSION detected - Priority messages NOT available (fallback mode)${NC}"
            return 1
        fi
    else
        echo -e "${RED}Erlang not found${NC}"
        return 2
    fi
}

# Validate compilation
validate_compilation() {
    print_section "Compilation Validation"

    echo "Compiling erlmcp..."
    if TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_compile.log; then
        print_result 0 "Compilation successful"

        # Check for warnings
        WARNING_COUNT=$(grep -c "Warning:" /tmp/erlmcp_compile.log || true)
        if [ "$WARNING_COUNT" -eq 0 ]; then
            print_result 0 "No compilation warnings"
        else
            print_result 1 "Found $WARNING_COUNT compilation warnings"
        fi
    else
        print_result 1 "Compilation failed"
        return 1
    fi
}

# Validate type checking
validate_dialyzer() {
    print_section "Dialyzer Type Checking"

    echo "Running dialyzer..."
    if rebar3 dialyzer 2>&1 | tee /tmp/erlmcp_dialyzer.log; then
        # Check for warnings
        WARNING_COUNT=$(grep -c "Warning:" /tmp/erlmcp_dialyzer.log || true)
        if [ "$WARNING_COUNT" -eq 0 ]; then
            print_result 0 "Dialyzer: No type warnings"
        else
            print_result 1 "Dialyzer found $WARNING_COUNT warnings"
        fi
    else
        print_result 1 "Dialyzer failed"
    fi
}

# Validate cross-references
validate_xref() {
    print_section "Cross-Reference Validation"

    echo "Running xref..."
    if rebar3 xref 2>&1 | tee /tmp/erlmcp_xref.log; then
        # Check for undefined functions
        UNDEFINED_COUNT=$(grep -c "undefined" /tmp/erlmcp_xref.log || true)
        if [ "$UNDEFINED_COUNT" -eq 0 ]; then
            print_result 0 "Xref: No undefined functions"
        else
            print_result 1 "Xref found $UNDEFINED_COUNT undefined functions"
        fi
    else
        print_result 1 "Xref failed"
    fi
}

# Validate health monitor tests
validate_health_monitor_tests() {
    print_section "Health Monitor Priority Tests"

    echo "Running erlmcp_health_monitor_priority_tests..."
    if rebar3 eunit --module=erlmcp_health_monitor_priority_tests 2>&1 | tee /tmp/health_monitor_tests.log; then
        # Extract test results
        PASSED=$(grep -oP '\d+(?= passed)' /tmp/health_monitor_tests.log | tail -1 || echo "0")
        FAILED=$(grep -oP '\d+(?= failed)' /tmp/health_monitor_tests.log | tail -1 || echo "0")

        if [ "$FAILED" -eq 0 ]; then
            print_result 0 "Health monitor tests: $PASSED passed, $FAILED failed"
        else
            print_result 1 "Health monitor tests: $PASSED passed, $FAILED failed"
        fi

        # Check latency metrics (OTP 28 only)
        if check_otp_version; then
            if grep -q "OTP 28 priority health check latency" /tmp/health_monitor_tests.log; then
                LATENCIES=$(grep -oP 'OTP 28 priority health check latency: \K\d+' /tmp/health_monitor_tests.log)
                for LATENCY in $LATENCIES; do
                    if [ "$LATENCY" -lt 1000 ]; then
                        print_result 0 "Health check latency: ${LATENCY}μs (<1ms target)"
                    else
                        print_result 1 "Health check latency: ${LATENCY}μs (>=1ms, exceeds target)"
                    fi
                done
            fi
        fi
    else
        print_result 1 "Health monitor tests failed to run"
    fi
}

# Validate circuit breaker tests
validate_circuit_breaker_tests() {
    print_section "Circuit Breaker Priority Tests"

    echo "Running erlmcp_circuit_breaker_priority_tests..."
    if rebar3 eunit --module=erlmcp_circuit_breaker_priority_tests 2>&1 | tee /tmp/circuit_breaker_tests.log; then
        PASSED=$(grep -oP '\d+(?= passed)' /tmp/circuit_breaker_tests.log | tail -1 || echo "0")
        FAILED=$(grep -oP '\d+(?= failed)' /tmp/circuit_breaker_tests.log | tail -1 || echo "0")

        if [ "$FAILED" -eq 0 ]; then
            print_result 0 "Circuit breaker tests: $PASSED passed, $FAILED failed"
        else
            print_result 1 "Circuit breaker tests: $PASSED passed, $FAILED failed"
        fi

        # Check transition latencies (OTP 28 only)
        if check_otp_version; then
            if grep -q "OTP 28 CLOSED->OPEN transition" /tmp/circuit_breaker_tests.log; then
                LATENCIES=$(grep -oP 'OTP 28 CLOSED->OPEN transition: \K\d+' /tmp/circuit_breaker_tests.log)
                for LATENCY in $LATENCIES; do
                    if [ "$LATENCY" -lt 1000 ]; then
                        print_result 0 "State transition latency: ${LATENCY}μs (<1ms target)"
                    else
                        print_result 1 "State transition latency: ${LATENCY}μs (>=1ms, exceeds target)"
                    fi
                done
            fi
        fi
    else
        print_result 1 "Circuit breaker tests failed to run"
    fi
}

# Validate graceful drain tests
validate_graceful_drain_tests() {
    print_section "Graceful Drain Priority Tests"

    echo "Running erlmcp_graceful_drain_priority_tests..."
    if rebar3 eunit --module=erlmcp_graceful_drain_priority_tests 2>&1 | tee /tmp/graceful_drain_tests.log; then
        PASSED=$(grep -oP '\d+(?= passed)' /tmp/graceful_drain_tests.log | tail -1 || echo "0")
        FAILED=$(grep -oP '\d+(?= failed)' /tmp/graceful_drain_tests.log | tail -1 || echo "0")

        if [ "$FAILED" -eq 0 ]; then
            print_result 0 "Graceful drain tests: $PASSED passed, $FAILED failed"
        else
            print_result 1 "Graceful drain tests: $PASSED passed, $FAILED failed"
        fi

        # Check shutdown latencies (OTP 28 only)
        if check_otp_version; then
            if grep -q "OTP 28 priority shutdown signal" /tmp/graceful_drain_tests.log; then
                LATENCIES=$(grep -oP 'OTP 28 priority shutdown signal: \K\d+' /tmp/graceful_drain_tests.log)
                for LATENCY in $LATENCIES; do
                    if [ "$LATENCY" -lt 1000 ]; then
                        print_result 0 "Shutdown signal latency: ${LATENCY}μs (<1ms target)"
                    else
                        print_result 1 "Shutdown signal latency: ${LATENCY}μs (>=1ms, exceeds target)"
                    fi
                done
            fi
        fi
    else
        print_result 1 "Graceful drain tests failed to run"
    fi
}

# Validate file existence
validate_files() {
    print_section "File Existence Validation"

    FILES=(
        "apps/erlmcp_observability/src/erlmcp_health_monitor.erl"
        "apps/erlmcp_core/src/erlmcp_circuit_breaker.erl"
        "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"
        "apps/erlmcp_observability/test/erlmcp_health_monitor_priority_tests.erl"
        "apps/erlmcp_core/test/erlmcp_circuit_breaker_priority_tests.erl"
        "apps/erlmcp_core/test/erlmcp_graceful_drain_priority_tests.erl"
        "include/otp_compat.hrl"
        "docs/EEP76_PRIORITY_MESSAGES.md"
    )

    for FILE in "${FILES[@]}"; do
        if [ -f "$FILE" ]; then
            print_result 0 "File exists: $FILE"
        else
            print_result 1 "File missing: $FILE"
        fi
    done
}

# Validate OTP compatibility macros
validate_otp_compat() {
    print_section "OTP Compatibility Macro Validation"

    FILES=(
        "apps/erlmcp_observability/src/erlmcp_health_monitor.erl"
        "apps/erlmcp_core/src/erlmcp_circuit_breaker.erl"
        "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"
    )

    for FILE in "${FILES[@]}"; do
        # Check for otp_compat.hrl include
        if grep -q 'include("otp_compat.hrl")' "$FILE"; then
            print_result 0 "OTP compat included: $FILE"
        else
            print_result 1 "OTP compat missing: $FILE"
        fi

        # Check for OTP_28 guards
        if grep -q '\-ifdef(OTP_28)' "$FILE"; then
            print_result 0 "OTP_28 guards found: $FILE"
        else
            print_result 1 "OTP_28 guards missing: $FILE"
        fi
    done
}

# Main validation flow
main() {
    echo -e "${BLUE}╔═══════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║        EEP 76 Priority Messages Implementation Validation        ║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════════════════════════════╝${NC}"

    # Validate files first (no compilation needed)
    validate_files
    validate_otp_compat

    # Check if compilation tools are available
    if ! command -v rebar3 &> /dev/null; then
        echo -e "\n${RED}rebar3 not found - skipping compilation and test validation${NC}"
        echo -e "${YELLOW}Please install rebar3 and Erlang/OTP to run full validation${NC}"
    else
        # Run compilation and tests
        validate_compilation
        validate_dialyzer
        validate_xref
        validate_health_monitor_tests
        validate_circuit_breaker_tests
        validate_graceful_drain_tests
    fi

    # Print summary
    print_section "Validation Summary"
    TOTAL=$((PASS + FAIL))
    PASS_RATE=$(awk "BEGIN {printf \"%.1f\", ($PASS/$TOTAL)*100}")

    echo -e "Total Tests: $TOTAL"
    echo -e "${GREEN}Passed: $PASS${NC}"
    echo -e "${RED}Failed: $FAIL${NC}"
    echo -e "Pass Rate: ${PASS_RATE}%"

    if [ "$FAIL" -eq 0 ]; then
        echo -e "\n${GREEN}╔═══════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║                  ✅ ALL VALIDATIONS PASSED ✅                     ║${NC}"
        echo -e "${GREEN}╚═══════════════════════════════════════════════════════════════════╝${NC}"
        exit 0
    else
        echo -e "\n${RED}╔═══════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║                  ❌ SOME VALIDATIONS FAILED ❌                    ║${NC}"
        echo -e "${RED}╚═══════════════════════════════════════════════════════════════════╝${NC}"
        exit 1
    fi
}

# Run main
main "$@"
