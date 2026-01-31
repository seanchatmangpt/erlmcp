#!/bin/bash
# ERLMCP Performance Validation Benchmark Script

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

WORKLOAD_ID="core_ops_10k"
BASELINE_FILE="${PROJECT_ROOT}/bench/baselines/2026-01-28_v2.0.0.json"
MAX_REGRESSION=10
VERBOSE=false
TEMP_RESULT_FILE="/tmp/erlmcp_bench_result_$$.json"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

PASS="✓"
FAIL="✗"

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_verbose() {
    if [[ "${VERBOSE}" == "true" ]]; then
        echo -e "${CYAN}[VERBOSE]${NC} $*"
    fi
}

print_header() {
    local title="$1"
    local width=60
    local padding=$(( (width - ${#title}) / 2 ))
    
    echo ""
    echo -e "${CYAN}╔$(printf '═%.0s' $(seq 1 $width))╗${NC}"
    echo -e "${CYAN}║${NC}$(printf ' %.0s' $(seq 1 $padding))${title}$(printf ' %.0s' $(seq 1 $padding))${CYAN}║${NC}"
    echo -e "${CYAN}╚$(printf '═%.0s' $(seq 1 $width))╝${NC}"
    echo ""
}

usage() {
    cat << EOF
Usage: $(basename "$0") [options]

Validate erlmcp performance against established baselines.

Options:
  -w, --workload ID       Workload ID (default: core_ops_10k)
  -b, --baseline FILE     Baseline JSON file
  -t, --threshold N       Maximum regression threshold in % (default: 10)
  -v, --verbose           Enable verbose output
  -h, --help              Show this help message

Examples:
  $(basename "$0")
  $(basename "$0") --workload core_ops_100k
  $(basename "$0") --threshold 5

Exit codes:
  0 - All tests passed
  1 - Performance regression detected
  2 - Benchmark execution failed
  3 - Missing dependencies
EOF
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -w|--workload)
                WORKLOAD_ID="$2"
                shift 2
                ;;
            -b|--baseline)
                BASELINE_FILE="$2"
                shift 2
                ;;
            -t|--threshold)
                MAX_REGRESSION="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                usage
                exit 3
                ;;
        esac
    done
}

check_dependencies() {
    log_verbose "Checking dependencies..."
    
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found in PATH"
        exit 3
    fi
    
    if [[ ! -f "${BASELINE_FILE}" ]]; then
        log_error "Baseline file not found: ${BASELINE_FILE}"
        exit 3
    fi
    
    if ! command -v jq &> /dev/null; then
        log_error "jq not found in PATH. Install: brew install jq"
        exit 3
    fi
    
    log_verbose "Dependencies OK"
}

extract_metric() {
    local json_file="$1"
    local metric_path="$2"
    local default_value="${3:-0}"
    
    jq -r "${metric_path} // ${default_value}" "${json_file}" 2>/dev/null || echo "${default_value}"
}

run_benchmark() {
    log_info "Running benchmark: erlmcp_bench_core_ops:run(<<\"${WORKLOAD_ID}\">>)"
    
    cd "${PROJECT_ROOT}"
    
    erl -noshell -eval "Result = erlmcp_bench_core_ops:run(<<\"${WORKLOAD_ID}\">>), io:format(\"~s\", [jsx:encode(Result)]), halt(0)." -s init stop 2>"${TEMP_RESULT_FILE}.err" > "${TEMP_RESULT_FILE}"
    
    if [[ ! -s "${TEMP_RESULT_FILE}" ]]; then
        log_error "Benchmark produced no output"
        cat "${TEMP_RESULT_FILE}.err" 2>/dev/null || true
        rm -f "${TEMP_RESULT_FILE}" "${TEMP_RESULT_FILE}.err"
        exit 2
    fi
    
    if ! jq empty "${TEMP_RESULT_FILE}" 2>/dev/null; then
        log_error "Invalid JSON output"
        cat "${TEMP_RESULT_FILE}"
        rm -f "${TEMP_RESULT_FILE}" "${TEMP_RESULT_FILE}.err"
        exit 2
    fi
    
    rm -f "${TEMP_RESULT_FILE}.err"
    log_verbose "Benchmark completed"
}

compare_metric() {
    local metric_name="$1"
    local current_value="$2"
    local baseline_value="$3"
    local threshold="$4"
    local unit="$5"
    local higher_is_better="${6:-false}"
    
    local current_num=$(printf "%.0f" "${current_value}")
    local baseline_num=$(printf "%.0f" "${baseline_value}")
    
    local diff_percent
    if [[ "${higher_is_better}" == "true" ]]; then
        if [[ "${baseline_num}" -eq 0 ]]; then
            diff_percent="0.00"
        else
            diff_percent=$(awk "BEGIN {printf \"%.2f\", ((${current_num} - ${baseline_num}) / ${baseline_num}) * 100}")
        fi
        local is_regression=$(awk "BEGIN {print (${diff_percent} < -${threshold}) ? 1 : 0}")
    else
        if [[ "${baseline_num}" -eq 0 ]]; then
            diff_percent="0.00"
        else
            diff_percent=$(awk "BEGIN {printf \"%.2f\", ((${current_num} - ${baseline_num}) / ${baseline_num}) * 100}")
        fi
        local is_regression=$(awk "BEGIN {print (${diff_percent} > ${threshold}) ? 1 : 0}")
    fi
    
    local status_symbol
    local status_color
    if [[ "${is_regression}" -eq 1 ]]; then
        status_symbol="${FAIL}"
        status_color="${RED}"
    else
        status_symbol="${PASS}"
        status_color="${GREEN}"
    fi
    
    printf "  %-20s ${status_color}%s${NC} | Current: %s %s | Baseline: %s %s | Delta: %s%%\n" \
        "${metric_name}:" \
        "${status_symbol}" \
        "${current_num}" \
        "${unit}" \
        "${baseline_num}" \
        "${unit}" \
        "${diff_percent}"
    
    return "${is_regression}"
}

validate_performance() {
    log_info "Loading baseline: ${BASELINE_FILE}"
    
    local baseline_throughput baseline_p50 baseline_p95 baseline_p99 baseline_memory
    baseline_throughput=$(extract_metric "${BASELINE_FILE}" ".metrics.throughput_msg_per_s")
    baseline_p50=$(extract_metric "${BASELINE_FILE}" ".metrics.latency_p50_us")
    baseline_p95=$(extract_metric "${BASELINE_FILE}" ".metrics.latency_p95_us")
    baseline_p99=$(extract_metric "${BASELINE_FILE}" ".metrics.latency_p99_us")
    baseline_memory=$(extract_metric "${BASELINE_FILE}" ".metrics.memory_delta_mib")
    
    local current_throughput current_p50 current_p95 current_p99 current_memory
    current_throughput=$(extract_metric "${TEMP_RESULT_FILE}" ".throughput_msg_per_s")
    current_p50=$(extract_metric "${TEMP_RESULT_FILE}" ".latency_p50_us")
    current_p95=$(extract_metric "${TEMP_RESULT_FILE}" ".latency_p95_us")
    current_p99=$(extract_metric "${TEMP_RESULT_FILE}" ".latency_p99_us")
    current_memory=$(extract_metric "${TEMP_RESULT_FILE}" ".memory_delta_mib")
    
    print_header "Performance Validation Results"
    
    local failures=0
    
    echo -e "\n${CYAN}Throughput (msg/s):${NC}"
    compare_metric "Throughput" "${current_throughput}" "${baseline_throughput}" "${MAX_REGRESSION}" "msg/s" "true" || ((failures++))
    
    echo -e "\n${CYAN}Latency P50 (µs):${NC}"
    compare_metric "Latency P50" "${current_p50}" "${baseline_p50}" "${MAX_REGRESSION}" "µs" "false" || ((failures++))
    
    echo -e "\n${CYAN}Latency P95 (µs):${NC}"
    compare_metric "Latency P95" "${current_p95}" "${baseline_p95}" "${MAX_REGRESSION}" "µs" "false" || ((failures++))
    
    echo -e "\n${CYAN}Latency P99 (µs):${NC}"
    compare_metric "Latency P99" "${current_p99}" "${baseline_p99}" "${MAX_REGRESSION}" "µs" "false" || ((failures++))
    
    echo -e "\n${CYAN}Memory (MiB):${NC}"
    compare_metric "Memory Delta" "${current_memory}" "${baseline_memory}" "${MAX_REGRESSION}" "MiB" "false" || ((failures++))
    
    echo ""
    
    if [[ ${failures} -eq 0 ]]; then
        print_header "Overall Status: ${GREEN}ALL TESTS PASSED${NC}"
        log_success "No performance regression detected"
        return 0
    else
        print_header "Overall Status: ${RED}PERFORMANCE REGRESSION${NC}"
        log_error "${failures} metric(s) exceeded threshold of ${MAX_REGRESSION}%"
        return 1
    fi
}

cleanup() {
    rm -f "${TEMP_RESULT_FILE}" "${TEMP_RESULT_FILE}.err" 2>/dev/null || true
}

main() {
    trap cleanup EXIT
    
    parse_args "$@"
    
    print_header "ERLMCP Performance Validation Benchmark"
    
    log_info "Configuration:"
    log_info "  Workload: ${WORKLOAD_ID}"
    log_info "  Baseline: ${BASELINE_FILE}"
    log_info "  Threshold: ${MAX_REGRESSION}%"
    
    check_dependencies
    run_benchmark
    
    if validate_performance; then
        exit 0
    else
        exit 1
    fi
}

main "$@"
