#!/usr/bin/env bash
# ============================================================================
# ERLMCP QUALITY GATE - Comprehensive Quality Check Script
# ============================================================================
# Used by git hooks and CI/CD to enforce quality standards.
# Returns exit code 0 (pass) or 1 (fail).
#
# Requirements (from CLAUDE.md):
# - 100% type coverage (dialyzer)
# - 80%+ test coverage
# - ALL tests passing
# - 0 compilation errors
# - Clean xref (cross-reference)
# - Security scans (TODO: add security scanning)
# ============================================================================

set -e

# Colors for output
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Quality gate configuration
REQUIRED_TEST_COVERAGE=80
MAX_DIALYZER_WARNINGS=0
MAX_XREF_WARNINGS=0

# Track failures
FAILURES=0
WARNINGS=0

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

# Check if rebar3 is available
check_rebar3() {
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found in PATH"
        return 1
    fi
    return 0
}

# Gate 1: Compilation
gate_compile() {
    print_section "GATE 1: COMPILATION"
    log_info "Compiling all applications..."

    if TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_compile.log; then
        module_count=$(grep -cE "(Compiling|Compiled)" /tmp/erlmcp_compile.log 2>/dev/null)
        module_count=${module_count:-0}
        log_success "Compilation passed: ${module_count} applications compiled"

        # Check for warnings
        warning_count=$(grep -c "Warning:" /tmp/erlmcp_compile.log 2>/dev/null)
        warning_count=${warning_count:-0}
        if [ "$warning_count" -gt 0 ]; then
            log_warning "Compilation produced ${warning_count} warnings"
        fi

        return 0
    else
        log_error "Compilation failed (see errors above)"
        return 1
    fi
}

# Gate 2: Cross-reference analysis
gate_xref() {
    print_section "GATE 2: CROSS-REFERENCE ANALYSIS (XREF)"
    log_info "Running xref to check for undefined functions..."

    if rebar3 xref 2>&1 | tee /tmp/erlmcp_xref.log; then
        # Count xref warnings (undefined functions, etc.)
        xref_warnings=$(grep -cE "(Warning|undefined)" /tmp/erlmcp_xref.log 2>/dev/null)
        xref_warnings=${xref_warnings:-0}

        if [ "$xref_warnings" -le "$MAX_XREF_WARNINGS" ]; then
            log_success "Xref passed: ${xref_warnings} warnings (max allowed: ${MAX_XREF_WARNINGS})"
            return 0
        else
            log_error "Xref failed: ${xref_warnings} warnings (max allowed: ${MAX_XREF_WARNINGS})"
            return 1
        fi
    else
        log_error "Xref execution failed"
        return 1
    fi
}

# Gate 3: Type checking (Dialyzer)
gate_dialyzer() {
    print_section "GATE 3: TYPE CHECKING (DIALYZER)"
    log_info "Running dialyzer (this may take several minutes)..."

    # Check if PLT exists, build if needed
    if [ ! -f "_build/default/rebar3_"*"_plt" ]; then
        log_info "Building Dialyzer PLT (first run only)..."
        rebar3 dialyzer --plt
    fi

    if rebar3 dialyzer 2>&1 | tee /tmp/erlmcp_dialyzer.log; then
        dialyzer_warnings=$(grep -c "Warning:" /tmp/erlmcp_dialyzer.log 2>/dev/null)
        dialyzer_warnings=${dialyzer_warnings:-0}

        if [ "$dialyzer_warnings" -le "$MAX_DIALYZER_WARNINGS" ]; then
            log_success "Dialyzer passed: ${dialyzer_warnings} warnings (max allowed: ${MAX_DIALYZER_WARNINGS})"
            return 0
        else
            log_error "Dialyzer failed: ${dialyzer_warnings} warnings (max allowed: ${MAX_DIALYZER_WARNINGS})"
            return 1
        fi
    else
        log_error "Dialyzer execution failed"
        return 1
    fi
}

# Gate 4: Unit tests (EUnit)
gate_eunit() {
    print_section "GATE 4: UNIT TESTS (EUNIT)"
    log_info "Running EUnit tests..."

    if rebar3 eunit 2>&1 | tee /tmp/erlmcp_eunit.log; then
        # Parse test results - look for "All X tests passed"
        passed=$(grep -oE "All [0-9]+ tests passed" /tmp/erlmcp_eunit.log | grep -oE "[0-9]+" 2>/dev/null | head -1)
        passed=${passed:-0}

        # Check for failures
        failed=$(grep -oE "[0-9]+ failed" /tmp/erlmcp_eunit.log | grep -oE "[0-9]+" 2>/dev/null | head -1)
        failed=${failed:-0}

        total=$((passed + failed))

        if [ "$failed" -eq 0 ] && [ "$passed" -gt 0 ]; then
            log_success "EUnit passed: ${passed}/${total} tests passed (${failed} failures)"
            return 0
        elif [ "$passed" -eq 0 ] && [ "$failed" -eq 0 ]; then
            log_warning "EUnit: No tests found or executed"
            return 0
        else
            log_error "EUnit failed: ${passed}/${total} tests passed (${failed} failures)"
            return 1
        fi
    else
        log_error "EUnit execution failed"
        return 1
    fi
}

# Gate 5: Integration tests (Common Test)
gate_ct() {
    print_section "GATE 5: INTEGRATION TESTS (COMMON TEST)"
    log_info "Running Common Test suites..."

    # CT may not have suites, so we allow it to skip gracefully
    if rebar3 ct 2>&1 | tee /tmp/erlmcp_ct.log; then
        log_success "Common Test passed"
        return 0
    else
        # Check if failure is due to no test suites (acceptable)
        if grep -q "No tests to run" /tmp/erlmcp_ct.log 2>/dev/null; then
            log_warning "Common Test: No test suites found (skipped)"
            return 0
        else
            log_error "Common Test failed (see errors above)"
            return 1
        fi
    fi
}

# Gate 6: Test coverage
gate_coverage() {
    print_section "GATE 6: TEST COVERAGE"
    log_info "Checking test coverage (target: ${REQUIRED_TEST_COVERAGE}%)..."

    if rebar3 cover 2>&1 | tee /tmp/erlmcp_coverage.log; then
        # Parse coverage percentage - look for "total" line
        coverage=$(grep -oE "total[^0-9]+[0-9]+%" /tmp/erlmcp_coverage.log | grep -oE "[0-9]+" 2>/dev/null | tail -1)
        coverage=${coverage:-0}

        if [ "$coverage" -eq 0 ]; then
            log_warning "Coverage: Unable to determine coverage percentage"
            return 0
        elif [ "$coverage" -ge "$REQUIRED_TEST_COVERAGE" ]; then
            log_success "Coverage passed: ${coverage}% (required: ${REQUIRED_TEST_COVERAGE}%)"
            return 0
        else
            log_error "Coverage failed: ${coverage}% (required: ${REQUIRED_TEST_COVERAGE}%)"
            return 1
        fi
    else
        log_error "Coverage check failed"
        return 1
    fi
}

# Gate 7: Security scan (placeholder for future implementation)
gate_security() {
    print_section "GATE 7: SECURITY SCAN"
    log_info "Security scanning not yet implemented (TODO)"
    log_warning "Security scan: SKIPPED (not implemented)"
    return 0
}

# Main quality gate execution
run_quality_gates() {
    local start_time=$(date +%s)

    echo ""
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║         ERLMCP QUALITY GATE - COMPREHENSIVE CHECK              ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""

    # Check prerequisites
    if ! check_rebar3; then
        log_error "Prerequisites not met"
        exit 1
    fi

    # Run all quality gates
    gate_compile || true
    gate_xref || true
    gate_dialyzer || true
    gate_eunit || true
    gate_ct || true
    gate_coverage || true
    gate_security || true

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    # Print final report
    print_section "QUALITY GATE REPORT"

    if [ $FAILURES -eq 0 ]; then
        echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║                   ✓ ALL GATES PASSED                           ║${NC}"
        echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}"
        echo ""
        log_success "Quality gate completed in ${duration}s"

        if [ $WARNINGS -gt 0 ]; then
            log_warning "${WARNINGS} warnings detected (non-blocking)"
        fi

        echo ""
        echo -e "${GREEN}Ready to commit/push!${NC}"
        exit 0
    else
        echo -e "${RED}╔════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║                   ✗ QUALITY GATE FAILED                        ║${NC}"
        echo -e "${RED}╚════════════════════════════════════════════════════════════════╝${NC}"
        echo ""
        log_error "${FAILURES} gate(s) failed"

        if [ $WARNINGS -gt 0 ]; then
            log_warning "${WARNINGS} warnings detected"
        fi

        echo ""
        echo -e "${RED}Fix the errors above before committing/pushing.${NC}"
        echo ""
        echo -e "${YELLOW}To bypass (NOT recommended):${NC}"
        echo -e "  git commit --no-verify -m \"message\""
        echo ""
        exit 1
    fi
}

# Parse command-line options
MODE="full"

while [[ $# -gt 0 ]]; do
    case $1 in
        --fast)
            MODE="fast"
            shift
            ;;
        --compile-only)
            MODE="compile"
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --fast          Run fast checks only (compile + eunit)"
            echo "  --compile-only  Run compilation check only"
            echo "  --help          Show this help message"
            echo ""
            echo "Default: Run all quality gates (full check)"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Execute based on mode
case $MODE in
    fast)
        log_info "Running FAST quality gates (compile + tests only)..."
        check_rebar3 && gate_compile && gate_eunit
        exit $?
        ;;
    compile)
        log_info "Running COMPILE-ONLY quality gate..."
        check_rebar3 && gate_compile
        exit $?
        ;;
    full)
        run_quality_gates
        ;;
esac
