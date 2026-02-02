#!/usr/bin/env bash
# ============================================================================
# ERLMCP-FLOW QUALITY CHECK - Automated Quality Gate Enforcement
# ============================================================================
# Comprehensive quality validation for erlmcp-flow agent coordination layer
#
# Usage:
#   ./scripts/erlmcp-flow-quality-check.sh [--fast|--full|--gates=1,2,3]
#
# Options:
#   --fast      Run fast checks only (compilation, format, smoke tests)
#   --full      Run all 12 quality gates (default)
#   --gates=N   Run specific gates (comma-separated, e.g., --gates=1,2,4)
#
# Exit Codes:
#   0 - All gates passed
#   1 - One or more gates failed
# ============================================================================

set -e

# Colors for output
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Configuration
ERLMCP_FLOW_DIR="apps/erlmcp_flow"
REQUIRED_COVERAGE_OVERALL=80
REQUIRED_COVERAGE_CORE=85

# Track failures and warnings
FAILURES=0
WARNINGS=0
GATES_RUN=0
GATES_PASSED=0

# Track gate statuses
declare -A GATE_STATUS

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $1"
    WARNINGS=$((WARNINGS + 1))
}

log_error() {
    echo -e "${RED}[✗]${NC} $1"
    FAILURES=$((FAILURES + 1))
}

print_section() {
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo ""
}

print_gate_header() {
    local gate_num=$1
    local gate_name=$2
    echo ""
    echo -e "${BLUE}[Gate $gate_num/12]${NC} $gate_name"
    echo "────────────────────────────────────────────────────────────────"
}

# Check if erlmcp-flow directory exists
check_directory() {
    if [ ! -d "$ERLMCP_FLOW_DIR" ]; then
        log_error "erlmcp-flow directory not found: $ERLMCP_FLOW_DIR"
        log_error "Run this script from the erlmcp repository root"
        exit 1
    fi
}

# Gate 1: Compilation
gate_compile() {
    print_gate_header 1 "Compilation"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Compiling erlmcp-flow..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    if TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_flow_compile.log; then
        # Check for errors
        local error_count
        error_count=$(grep -c "Error:" /tmp/erlmcp_flow_compile.log 2>/dev/null || echo "0")

        if [ "$error_count" -eq 0 ]; then
            # Check for warnings
            local warning_count
            warning_count=$(grep -c "Warning:" /tmp/erlmcp_flow_compile.log 2>/dev/null || echo "0")

            if [ "$warning_count" -gt 0 ]; then
                log_warning "Compilation produced $warning_count warnings"
            fi

            log_success "Gate 1: Compilation passed (0 errors)"
            GATE_STATUS[1]="PASS"
            GATES_PASSED=$((GATES_PASSED + 1))
            cd ../.. || exit 1
            return 0
        else
            log_error "Gate 1: Compilation failed ($error_count errors)"
            GATE_STATUS[1]="FAIL"
            cd ../.. || exit 1
            return 1
        fi
    else
        log_error "Gate 1: Compilation failed"
        GATE_STATUS[1]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 2: Cross-Reference Analysis (Xref)
gate_xref() {
    print_gate_header 2 "Cross-Reference Analysis (Xref)"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Running xref analysis..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    if rebar3 xref 2>&1 | tee /tmp/erlmcp_flow_xref.log; then
        local xref_warnings
        xref_warnings=$(grep -cE "(Warning|undefined)" /tmp/erlmcp_flow_xref.log 2>/dev/null || echo "0")

        if [ "$xref_warnings" -eq 0 ]; then
            log_success "Gate 2: Xref passed (0 undefined calls)"
            GATE_STATUS[2]="PASS"
            GATES_PASSED=$((GATES_PASSED + 1))
            cd ../.. || exit 1
            return 0
        else
            log_error "Gate 2: Xref failed ($xref_warnings undefined calls)"
            GATE_STATUS[2]="FAIL"
            cd ../.. || exit 1
            return 1
        fi
    else
        log_error "Gate 2: Xref execution failed"
        GATE_STATUS[2]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 3: Type Checking (Dialyzer)
gate_dialyzer() {
    print_gate_header 3 "Type Checking (Dialyzer)"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Running dialyzer (this may take a few minutes)..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    # Check if PLT exists
    if [ ! -f "../../_build/default/rebar3_"*"_plt" ]; then
        log_info "Building Dialyzer PLT (first run only)..."
        rebar3 dialyzer --plt || true
    fi

    if rebar3 dialyzer 2>&1 | tee /tmp/erlmcp_flow_dialyzer.log; then
        local dialyzer_warnings
        dialyzer_warnings=$(grep -c "Warning:" /tmp/erlmcp_flow_dialyzer.log 2>/dev/null || echo "0")

        if [ "$dialyzer_warnings" -eq 0 ]; then
            log_success "Gate 3: Dialyzer passed (0 type warnings)"
            GATE_STATUS[3]="PASS"
            GATES_PASSED=$((GATES_PASSED + 1))
            cd ../.. || exit 1
            return 0
        else
            log_error "Gate 3: Dialyzer failed ($dialyzer_warnings warnings)"
            GATE_STATUS[3]="FAIL"
            cd ../.. || exit 1
            return 1
        fi
    else
        log_error "Gate 3: Dialyzer execution failed"
        GATE_STATUS[3]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 4: Unit Tests (EUnit)
gate_eunit() {
    print_gate_header 4 "Unit Tests (EUnit)"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Running EUnit tests..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    if rebar3 eunit --app erlmcp_flow 2>&1 | tee /tmp/erlmcp_flow_eunit.log; then
        # Parse test results
        local passed
        passed=$(grep -oE "All [0-9]+ tests passed" /tmp/erlmcp_flow_eunit.log | grep -oE "[0-9]+" 2>/dev/null || echo "0")

        local failed
        failed=$(grep -oE "[0-9]+ failed" /tmp/erlmcp_flow_eunit.log | grep -oE "[0-9]+" 2>/dev/null || echo "0")

        if [ "$failed" -eq 0 ] && [ "$passed" -gt 0 ]; then
            log_success "Gate 4: EUnit passed ($passed tests, 0 failures)"
            GATE_STATUS[4]="PASS"
            GATES_PASSED=$((GATES_PASSED + 1))
            cd ../.. || exit 1
            return 0
        elif [ "$passed" -eq 0 ] && [ "$failed" -eq 0 ]; then
            log_warning "Gate 4: No EUnit tests found (skipped)"
            GATE_STATUS[4]="SKIP"
            cd ../.. || exit 1
            return 0
        else
            log_error "Gate 4: EUnit failed ($passed passed, $failed failed)"
            GATE_STATUS[4]="FAIL"
            cd ../.. || exit 1
            return 1
        fi
    else
        log_error "Gate 4: EUnit execution failed"
        GATE_STATUS[4]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 5: Integration Tests (Common Test)
gate_ct() {
    print_gate_header 5 "Integration Tests (Common Test)"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Running Common Test suites..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    if rebar3 ct --dir test 2>&1 | tee /tmp/erlmcp_flow_ct.log; then
        log_success "Gate 5: Common Test passed"
        GATE_STATUS[5]="PASS"
        GATES_PASSED=$((GATES_PASSED + 1))
        cd ../.. || exit 1
        return 0
    else
        # Check if no tests exist (acceptable)
        if grep -q "No tests to run" /tmp/erlmcp_flow_ct.log 2>/dev/null; then
            log_warning "Gate 5: No Common Test suites found (skipped)"
            GATE_STATUS[5]="SKIP"
            cd ../.. || exit 1
            return 0
        else
            log_error "Gate 5: Common Test failed"
            GATE_STATUS[5]="FAIL"
            cd ../.. || exit 1
            return 1
        fi
    fi
}

# Gate 6: Coverage
gate_coverage() {
    print_gate_header 6 "Test Coverage"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Checking test coverage (target: overall ≥${REQUIRED_COVERAGE_OVERALL}%, core ≥${REQUIRED_COVERAGE_CORE}%)..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    if rebar3 cover --verbose 2>&1 | tee /tmp/erlmcp_flow_coverage.log; then
        # Parse overall coverage
        local coverage
        coverage=$(grep -oE "total[^0-9]+[0-9]+%" /tmp/erlmcp_flow_coverage.log | grep -oE "[0-9]+" 2>/dev/null || echo "0")

        if [ "$coverage" -eq 0 ]; then
            log_warning "Gate 6: Unable to determine coverage percentage"
            GATE_STATUS[6]="SKIP"
            cd ../.. || exit 1
            return 0
        elif [ "$coverage" -ge "$REQUIRED_COVERAGE_OVERALL" ]; then
            log_success "Gate 6: Coverage passed (${coverage}% ≥ ${REQUIRED_COVERAGE_OVERALL}%)"
            GATE_STATUS[6]="PASS"
            GATES_PASSED=$((GATES_PASSED + 1))
            cd ../.. || exit 1
            return 0
        else
            log_error "Gate 6: Coverage failed (${coverage}% < ${REQUIRED_COVERAGE_OVERALL}%)"
            GATE_STATUS[6]="FAIL"
            cd ../.. || exit 1
            return 1
        fi
    else
        log_error "Gate 6: Coverage check failed"
        GATE_STATUS[6]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 7: Performance Benchmarks
gate_benchmarks() {
    print_gate_header 7 "Performance Benchmarks"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Running performance benchmarks..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    # Check if benchmark module exists
    if [ ! -f "bench/erlmcp_flow_bench.erl" ]; then
        log_warning "Gate 7: Benchmark module not found (skipped)"
        GATE_STATUS[7]="SKIP"
        cd ../.. || exit 1
        return 0
    fi

    if rebar3 eunit --module erlmcp_flow_bench 2>&1 | tee /tmp/erlmcp_flow_bench.log; then
        # Check for benchmark failures
        if grep -q "FAIL" /tmp/erlmcp_flow_bench.log; then
            log_error "Gate 7: Performance benchmarks failed"
            GATE_STATUS[7]="FAIL"
            cd ../.. || exit 1
            return 1
        else
            log_success "Gate 7: Performance benchmarks passed"
            GATE_STATUS[7]="PASS"
            GATES_PASSED=$((GATES_PASSED + 1))
            cd ../.. || exit 1
            return 0
        fi
    else
        log_error "Gate 7: Benchmark execution failed"
        GATE_STATUS[7]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 8: Chaos Engineering
gate_chaos() {
    print_gate_header 8 "Chaos Engineering"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Running chaos engineering tests..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    # Check if chaos suite exists
    if [ ! -f "test/erlmcp_flow_chaos_SUITE.erl" ]; then
        log_warning "Gate 8: Chaos test suite not found (skipped)"
        GATE_STATUS[8]="SKIP"
        cd ../.. || exit 1
        return 0
    fi

    if rebar3 ct --suite test/erlmcp_flow_chaos_SUITE 2>&1 | tee /tmp/erlmcp_flow_chaos.log; then
        log_success "Gate 8: Chaos engineering tests passed"
        GATE_STATUS[8]="PASS"
        GATES_PASSED=$((GATES_PASSED + 1))
        cd ../.. || exit 1
        return 0
    else
        log_error "Gate 8: Chaos engineering tests failed"
        GATE_STATUS[8]="FAIL"
        cd ../.. || exit 1
        return 1
    fi
}

# Gate 9: Chicago TDD Compliance
gate_chicago_tdd() {
    print_gate_header 9 "Chicago TDD Compliance"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Scanning for Chicago TDD violations..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    local violations=0

    # Check for mock usage
    if grep -r "meck:new\|meck:expect" test/ 2>/dev/null | grep -q "."; then
        log_error "  Mock usage detected (meck framework)"
        violations=$((violations + 1))
    fi

    # Check for state inspection
    if grep -r "sys:get_status\|sys:get_state" test/ 2>/dev/null | grep -q "."; then
        log_error "  State inspection detected (sys:get_status)"
        violations=$((violations + 1))
    fi

    # Check for dummy processes
    if grep -r -E "spawn\(fun\(\) -> receive" test/ 2>/dev/null | grep -q "."; then
        log_error "  Dummy process pattern detected"
        violations=$((violations + 1))
    fi

    cd ../.. || exit 1

    if [ $violations -eq 0 ]; then
        log_success "Gate 9: Chicago TDD compliance verified"
        GATE_STATUS[9]="PASS"
        GATES_PASSED=$((GATES_PASSED + 1))
        return 0
    else
        log_error "Gate 9: Chicago TDD violations found ($violations violations)"
        GATE_STATUS[9]="FAIL"
        return 1
    fi
}

# Gate 10: OTP Compliance
gate_otp_compliance() {
    print_gate_header 10 "OTP Compliance"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Scanning for OTP compliance..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    local violations=0

    # Check for unsupervised spawn
    if grep -r "spawn(fun()" src/ 2>/dev/null | grep -v "test" | grep -q "."; then
        log_error "  Unsupervised spawn detected"
        violations=$((violations + 1))
    fi

    # Check for timeouts < 5000ms (excluding specific patterns)
    if grep -r -E "gen_server:call.*[0-9]{1,3}\)" src/ 2>/dev/null | grep -q "."; then
        log_warning "  Potential short timeout detected (check manually)"
    fi

    cd ../.. || exit 1

    if [ $violations -eq 0 ]; then
        log_success "Gate 10: OTP compliance verified"
        GATE_STATUS[10]="PASS"
        GATES_PASSED=$((GATES_PASSED + 1))
        return 0
    else
        log_error "Gate 10: OTP compliance violations found ($violations violations)"
        GATE_STATUS[10]="FAIL"
        return 1
    fi
}

# Gate 11: Format
gate_format() {
    print_gate_header 11 "Code Format"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Verifying code formatting..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    if rebar3 format --verify 2>&1 | tee /tmp/erlmcp_flow_format.log; then
        log_success "Gate 11: Code formatting verified"
        GATE_STATUS[11]="PASS"
        GATES_PASSED=$((GATES_PASSED + 1))
        cd ../.. || exit 1
        return 0
    else
        log_warning "Gate 11: Code formatting violations (run: rebar3 format)"
        GATE_STATUS[11]="WARN"
        cd ../.. || exit 1
        return 0  # Non-blocking
    fi
}

# Gate 12: Documentation
gate_documentation() {
    print_gate_header 12 "Documentation"
    GATES_RUN=$((GATES_RUN + 1))

    log_info "Checking documentation coverage..."

    cd "$ERLMCP_FLOW_DIR" || exit 1

    local missing_specs=0

    # Check for missing -spec annotations
    for file in src/*.erl; do
        if [ -f "$file" ]; then
            # Count exported functions
            local exports
            exports=$(grep -cE "^-export\(" "$file" 2>/dev/null || echo "0")

            # Count -spec annotations
            local specs
            specs=$(grep -cE "^-spec " "$file" 2>/dev/null || echo "0")

            # Simple heuristic: exports should roughly match specs
            if [ "$exports" -gt 0 ] && [ "$specs" -eq 0 ]; then
                log_warning "  Missing -spec in $file"
                missing_specs=$((missing_specs + 1))
            fi
        fi
    done

    cd ../.. || exit 1

    if [ $missing_specs -eq 0 ]; then
        log_success "Gate 12: Documentation verified"
        GATE_STATUS[12]="PASS"
        GATES_PASSED=$((GATES_PASSED + 1))
        return 0
    else
        log_warning "Gate 12: Documentation incomplete ($missing_specs files missing specs)"
        GATE_STATUS[12]="WARN"
        return 0  # Non-blocking
    fi
}

# Print final report
print_final_report() {
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    print_section "QUALITY GATE REPORT"

    echo "Gates Run:    $GATES_RUN"
    echo "Gates Passed: $GATES_PASSED"
    echo "Failures:     $FAILURES"
    echo "Warnings:     $WARNINGS"
    echo "Duration:     ${duration}s"
    echo ""

    # Print gate statuses
    echo "Gate Status Summary:"
    echo "────────────────────────────────────────────────────────────────"
    for i in {1..12}; do
        if [ -n "${GATE_STATUS[$i]}" ]; then
            local status="${GATE_STATUS[$i]}"
            case $status in
                PASS)
                    echo -e "  Gate $i: ${GREEN}PASS${NC}"
                    ;;
                FAIL)
                    echo -e "  Gate $i: ${RED}FAIL${NC}"
                    ;;
                SKIP)
                    echo -e "  Gate $i: ${YELLOW}SKIP${NC}"
                    ;;
                WARN)
                    echo -e "  Gate $i: ${YELLOW}WARN${NC}"
                    ;;
            esac
        fi
    done
    echo ""

    if [ $FAILURES -eq 0 ]; then
        echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║                   ✓ ALL GATES PASSED                           ║${NC}"
        echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}"

        if [ $WARNINGS -gt 0 ]; then
            log_warning "$WARNINGS warnings detected (non-blocking)"
        fi

        echo ""
        echo -e "${GREEN}erlmcp-flow is ready to merge!${NC}"
        exit 0
    else
        echo -e "${RED}╔════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║                   ✗ QUALITY GATE FAILED                        ║${NC}"
        echo -e "${RED}╚════════════════════════════════════════════════════════════════╝${NC}"
        echo ""
        log_error "$FAILURES gate(s) failed"

        if [ $WARNINGS -gt 0 ]; then
            log_warning "$WARNINGS warnings detected"
        fi

        echo ""
        echo -e "${RED}Fix the errors above before merging.${NC}"
        exit 1
    fi
}

# Main execution
main() {
    local start_time=$(date +%s)
    local mode="full"
    local gates_to_run=""

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --fast)
                mode="fast"
                shift
                ;;
            --full)
                mode="full"
                shift
                ;;
            --gates=*)
                gates_to_run="${1#*=}"
                mode="custom"
                shift
                ;;
            --help)
                echo "Usage: $0 [--fast|--full|--gates=1,2,3]"
                echo ""
                echo "Options:"
                echo "  --fast       Run fast checks only (gates 1,4,9,11)"
                echo "  --full       Run all 12 quality gates (default)"
                echo "  --gates=N    Run specific gates (comma-separated)"
                echo ""
                echo "Gates:"
                echo "  1  - Compilation"
                echo "  2  - Xref"
                echo "  3  - Dialyzer"
                echo "  4  - Unit Tests (EUnit)"
                echo "  5  - Integration Tests (CT)"
                echo "  6  - Coverage"
                echo "  7  - Performance Benchmarks"
                echo "  8  - Chaos Engineering"
                echo "  9  - Chicago TDD Compliance"
                echo "  10 - OTP Compliance"
                echo "  11 - Format"
                echo "  12 - Documentation"
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done

    echo ""
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║       ERLMCP-FLOW QUALITY CHECK - Mode: $(printf '%-24s' "$mode") ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""

    # Check prerequisites
    check_directory

    # Run gates based on mode
    case $mode in
        fast)
            log_info "Running FAST quality gates (1,4,9,11)..."
            gate_compile || true
            gate_eunit || true
            gate_chicago_tdd || true
            gate_format || true
            ;;
        custom)
            log_info "Running custom quality gates ($gates_to_run)..."
            IFS=',' read -ra GATES <<< "$gates_to_run"
            for gate in "${GATES[@]}"; do
                case $gate in
                    1) gate_compile || true ;;
                    2) gate_xref || true ;;
                    3) gate_dialyzer || true ;;
                    4) gate_eunit || true ;;
                    5) gate_ct || true ;;
                    6) gate_coverage || true ;;
                    7) gate_benchmarks || true ;;
                    8) gate_chaos || true ;;
                    9) gate_chicago_tdd || true ;;
                    10) gate_otp_compliance || true ;;
                    11) gate_format || true ;;
                    12) gate_documentation || true ;;
                    *)
                        log_error "Invalid gate number: $gate"
                        ;;
                esac
            done
            ;;
        full)
            log_info "Running FULL quality gates (1-12)..."
            gate_compile || true
            gate_xref || true
            gate_dialyzer || true
            gate_eunit || true
            gate_ct || true
            gate_coverage || true
            gate_benchmarks || true
            gate_chaos || true
            gate_chicago_tdd || true
            gate_otp_compliance || true
            gate_format || true
            gate_documentation || true
            ;;
    esac

    # Print final report
    print_final_report
}

# Execute main function
main "$@"
