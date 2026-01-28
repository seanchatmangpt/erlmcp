#!/usr/bin/env bash
# ============================================================================
# ERLMCP QUALITY DASHBOARD
# ============================================================================
# Real-time quality gate status visualization
# Shows: compilation, tests, coverage, dialyzer, xref, benchmarks
# Color-coded output with overall PASS/FAIL status
# ============================================================================

set -euo pipefail

# ============================================================================
# ANSI COLOR CODES
# ============================================================================
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# ============================================================================
# SYMBOLS
# ============================================================================
CHECK="${GREEN}✅${NC}"
CROSS="${RED}❌${NC}"
WARN="${YELLOW}⚠️${NC}"
INFO="${BLUE}ℹ️${NC}"
ROCKET="${CYAN}🚀${NC}"
GAUGE="${MAGENTA}📊${NC}"

# ============================================================================
# CONFIGURATION
# ============================================================================
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
BUILD_DIR="${PROJECT_ROOT}/_build"
BENCHMARK_THRESHOLD="-10"  # -10% regression allowed
COVERAGE_THRESHOLD="80"     # 80% minimum
TEST_PASS_THRESHOLD="100"   # 100% pass rate required

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

print_header() {
    local title="$1"
    echo ""
    echo -e "${BOLD}${CYAN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${WHITE}  $title${NC}"
    echo -e "${BOLD}${CYAN}═══════════════════════════════════════════════════════════════${NC}"
}

print_section() {
    local title="$1"
    echo ""
    echo -e "${BOLD}${BLUE}▶ $title${NC}"
    echo -e "${BLUE}───────────────────────────────────────────────────────────────${NC}"
}

print_result() {
    local status="$1"
    local message="$2"
    local details="${3:-}"

    if [[ "$status" == "pass" ]]; then
        echo -e "  $CHECK ${GREEN}PASS${NC} - $message"
    elif [[ "$status" == "fail" ]]; then
        echo -e "  $CROSS ${RED}FAIL${NC} - $message"
    elif [[ "$status" == "warn" ]]; then
        echo -e "  $WARN ${YELLOW}WARN${NC} - $message"
    else
        echo -e "  $INFO ${BLUE}INFO${NC} - $message"
    fi

    if [[ -n "$details" ]]; then
        echo -e "        ${details}"
    fi
}

# ============================================================================
# QUALITY GATE CHECKS
# ============================================================================

check_compilation() {
    print_section "Compilation Status"

    cd "$PROJECT_ROOT"

    # Run compilation and capture output
    local compile_output
    compile_output=$(TERM=dumb rebar3 compile 2>&1 || true)

    # Check for errors
    local error_count
    error_count=$(echo "$compile_output" | grep "ERROR:" 2>/dev/null | wc -l | tr -d ' \n\r' || echo "0")
    error_count=${error_count:-0}
    error_count=$(echo "$error_count" | tr -d ' \n\r')  # Strip all whitespace

    # Check for warnings
    local warning_count
    warning_count=$(echo "$compile_output" | grep "Warning:" 2>/dev/null | wc -l | tr -d ' \n\r' || echo "0")
    warning_count=${warning_count:-0}
    warning_count=$(echo "$warning_count" | tr -d ' \n\r')  # Strip all whitespace

    # Check for success
    if echo "$compile_output" | grep -q "Compiling"; then
        local module_count
        module_count=$(echo "$compile_output" | grep "Compiling" 2>/dev/null | wc -l | tr -d ' \n\r' || echo "0")
        module_count=${module_count:-0}
        module_count=$(echo "$module_count" | tr -d ' \n\r')  # Strip all whitespace

        if [[ ${error_count} -eq 0 ]]; then
            print_result "pass" "Compiled ${module_count} modules"
            if [[ ${warning_count} -gt 0 ]]; then
                print_result "warn" "${warning_count} warnings found"
            fi
            echo "pass"
            return 0
        else
            print_result "fail" "${error_count} errors, ${warning_count} warnings"
            echo "fail"
            return 1
        fi
    else
        print_result "fail" "Compilation failed"
        echo "fail"
        return 1
    fi
}

check_tests() {
    print_section "Test Status"

    cd "$PROJECT_ROOT"

    # Run EUnit tests and capture output
    local test_output
    test_output=$(rebar3 eunit 2>&1 || true)

    # Parse test results
    local passed=0
    local failed=0
    local total=0

    if echo "$test_output" | grep -q "All .* tests passed"; then
        total=$(echo "$test_output" | grep -o "All [0-9]* tests passed" | grep -o "[0-9]*" | head -1 || echo "0")
        total=${total:-0}
        passed=$total
        failed=0
    elif echo "$test_output" | grep -q "Failed:"; then
        failed=$(echo "$test_output" | grep -o "Failed: [0-9]*" | grep -o "[0-9]*" | tail -1 || echo "0")
        failed=${failed:-0}
        passed=$(echo "$test_output" | grep -o "Passed: [0-9]*" | grep -o "[0-9]*" | tail -1 || echo "0")
        passed=${passed:-0}
        total=$((passed + failed))
    fi

    # Calculate pass rate
    local pass_rate=0
    if [[ ${total} -gt 0 ]]; then
        pass_rate=$((passed * 100 / total))
    fi

    # Determine status
    if [[ ${failed} -eq 0 ]] && [[ ${total} -gt 0 ]]; then
        print_result "pass" "All ${total} tests passed (100%)"
        echo "pass,${pass_rate}"
        return 0
    elif [[ ${pass_rate} -ge ${TEST_PASS_THRESHOLD} ]]; then
        print_result "warn" "${passed}/${total} tests passed (${pass_rate}%)"
        echo "warn,${pass_rate}"
        return 0
    else
        print_result "fail" "${passed}/${total} tests passed (${pass_rate}%) - ${failed} failures"
        echo "fail,${pass_rate}"
        return 1
    fi
}

check_coverage() {
    print_section "Code Coverage"

    cd "$PROJECT_ROOT"

    # Check if coverage data exists
    if [[ ! -d "$BUILD_DIR/test/cover" ]]; then
        print_result "warn" "No coverage data found - run 'rebar3 cover' first"
        echo "warn,0"
        return 0
    fi

    # Run coverage analysis
    local cover_output
    cover_output=$(rebar3 cover 2>&1 || true)

    # Parse coverage percentage
    local coverage=0
    if echo "$cover_output" | grep -q "total"; then
        coverage=$(echo "$cover_output" | grep "total" | grep -o "[0-9]*%" | tr -d '%' | head -1 || echo "0")
        coverage=${coverage:-0}
    fi

    # Determine status
    if [[ ${coverage} -ge ${COVERAGE_THRESHOLD} ]]; then
        print_result "pass" "Coverage: ${coverage}% (≥${COVERAGE_THRESHOLD}% required)"
        echo "pass,${coverage}"
        return 0
    elif [[ ${coverage} -gt 0 ]]; then
        print_result "warn" "Coverage: ${coverage}% (<${COVERAGE_THRESHOLD}% required)"
        echo "warn,${coverage}"
        return 0
    else
        print_result "fail" "Coverage data unavailable"
        echo "fail,0"
        return 1
    fi
}

check_dialyzer() {
    print_section "Dialyzer Type Checking"

    cd "$PROJECT_ROOT"

    # Check if PLT exists
    if [[ ! -f "$BUILD_DIR/default/*_plt" ]]; then
        print_result "info" "Building PLT (first run may take 5-10 minutes)..."
    fi

    # Run Dialyzer
    local dialyzer_output
    dialyzer_output=$(rebar3 dialyzer 2>&1 || true)

    # Check for warnings
    local warning_count
    warning_count=$(echo "$dialyzer_output" | grep "Warning:" 2>/dev/null | wc -l | tr -d ' \n\r' || echo "0")
    warning_count=${warning_count:-0}
    warning_count=$(echo "$warning_count" | tr -d ' \n\r')  # Strip all whitespace

    # Check for success
    if echo "$dialyzer_output" | grep -q "done (passed successfully)"; then
        print_result "pass" "Type checking passed (0 warnings)"
        echo "pass,0"
        return 0
    elif echo "$dialyzer_output" | grep -q "done"; then
        print_result "warn" "${warning_count} type warnings found"
        echo "warn,${warning_count}"
        return 0
    else
        print_result "fail" "Dialyzer failed"
        echo "fail,${warning_count}"
        return 1
    fi
}

check_xref() {
    print_section "Cross-Reference Analysis"

    cd "$PROJECT_ROOT"

    # Run Xref
    local xref_output
    xref_output=$(rebar3 xref 2>&1 || true)

    # Check for warnings
    local warning_count
    warning_count=$(echo "$xref_output" | grep "Warning:" 2>/dev/null | wc -l | tr -d ' \n\r' || echo "0")
    warning_count=${warning_count:-0}
    warning_count=$(echo "$warning_count" | tr -d ' \n\r')  # Strip all whitespace

    # Check for undefined calls
    local undefined_count
    undefined_count=$(echo "$xref_output" | grep "undefined" 2>/dev/null | wc -l | tr -d ' \n\r' || echo "0")
    undefined_count=${undefined_count:-0}
    undefined_count=$(echo "$undefined_count" | tr -d ' \n\r')  # Strip all whitespace

    # Determine status
    if echo "$xref_output" | grep -q "Xref completed"; then
        if [[ ${warning_count} -eq 0 ]] && [[ ${undefined_count} -eq 0 ]]; then
            print_result "pass" "No cross-reference issues found"
            echo "pass,0"
            return 0
        else
            print_result "warn" "${warning_count} warnings, ${undefined_count} undefined calls"
            echo "warn,$((warning_count + undefined_count))"
            return 0
        fi
    else
        print_result "fail" "Xref analysis failed"
        echo "fail,0"
        return 1
    fi
}

check_benchmarks() {
    print_section "Benchmark Status"

    cd "$PROJECT_ROOT"

    # Check if benchmark data exists
    local latest_bench
    latest_bench=$(find bench/results -type f -name "*.json" 2>/dev/null | sort -r | head -1 || echo "")

    if [[ -z "$latest_bench" ]]; then
        print_result "info" "No benchmark data found - run 'make benchmark-quick' first"
        echo "info,0"
        return 0
    fi

    # Check if jq is available
    if ! command -v jq &> /dev/null; then
        print_result "info" "jq not found - cannot parse benchmark data"
        echo "info,0"
        return 0
    fi

    # Parse benchmark results
    local throughput
    throughput=$(jq -r '.results[0].throughput_msg_per_s // 0' "$latest_bench" 2>/dev/null || echo "0")

    # Check for regression (compare with baseline if exists)
    local baseline_file="${PROJECT_ROOT}/bench/baseline.json"
    if [[ -f "$baseline_file" ]]; then
        local baseline_throughput
        baseline_throughput=$(jq -r '.results[0].throughput_msg_per_s // 0' "$baseline_file" 2>/dev/null || echo "0")

        if [[ "$baseline_throughput" != "0" ]]; then
            local regression
            regression=$(awk "BEGIN {printf \"%.1f\", (($throughput - $baseline_throughput) / $baseline_throughput) * 100}")

            if (( $(echo "$regression >= 0" | bc -l) )); then
                print_result "pass" "No regression - ${regression}% improvement"
                echo "pass,$regression"
                return 0
            elif (( $(echo "$regression >= $BENCHMARK_THRESHOLD" | bc -l) )); then
                print_result "warn" "${regression}% regression (within ${BENCHMARK_THRESHOLD}% threshold)"
                echo "warn,$regression"
                return 0
            else
                print_result "fail" "${regression}% regression (exceeds ${BENCHMARK_THRESHOLD}% threshold)"
                echo "fail,$regression"
                return 1
            fi
        fi
    fi

    # No baseline - just show current throughput
    print_result "info" "Throughput: ${throughput} msg/sec (no baseline for comparison)"
    echo "info,0"
    return 0
}

# ============================================================================
# MAIN DASHBOARD
# ============================================================================

main() {
    local timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    print_header "ERLMCP QUALITY DASHBOARD"
    echo -e "${CYAN}Generated: $timestamp${NC}"
    echo -e "${CYAN}Project: $PROJECT_ROOT${NC}"

    # Track overall status
    local overall_status="pass"
    local gate_results=()

    # Run all quality gates
    local compilation_status
    compilation_status=$(check_compilation)
    gate_results+=("compilation:$compilation_status")
    [[ "$compilation_status" == "fail" ]] && overall_status="fail"

    local test_status
    test_status=$(check_tests)
    gate_results+=("tests:$test_status")
    [[ "$test_status" =~ ^fail ]] && overall_status="fail"

    local coverage_status
    coverage_status=$(check_coverage)
    gate_results+=("coverage:$coverage_status")
    [[ "$coverage_status" =~ ^fail ]] && overall_status="fail"

    local dialyzer_status
    dialyzer_status=$(check_dialyzer)
    gate_results+=("dialyzer:$dialyzer_status")
    [[ "$dialyzer_status" == "fail" ]] && overall_status="fail"

    local xref_status
    xref_status=$(check_xref)
    gate_results+=("xref:$xref_status")
    [[ "$xref_status" == "fail" ]] && overall_status="fail"

    local benchmark_status
    benchmark_status=$(check_benchmarks)
    gate_results+=("benchmarks:$benchmark_status")
    [[ "$benchmark_status" == "fail" ]] && overall_status="fail"

    # Display overall status
    print_header "OVERALL STATUS"

    if [[ "$overall_status" == "pass" ]]; then
        echo -e "${GREEN}${BOLD}${CHECK} ALL QUALITY GATES PASSED${NC}"
        echo -e "${GREEN}Project is ready for deployment${NC}"
        return 0
    else
        echo -e "${RED}${BOLD}${CROSS} QUALITY GATES FAILED${NC}"
        echo -e "${RED}Please fix failing gates before deployment${NC}"
        return 1
    fi
}

# ============================================================================
# SCRIPT EXECUTION
# ============================================================================

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
