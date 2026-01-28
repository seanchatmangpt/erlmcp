#!/usr/bin/env bash
# detect-regression.sh - Detects quality regressions in erlmcp
# Part of TCPS Quality Gate System (自働化 - Jidoka)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
METRICS_DIR="${PROJECT_ROOT}/.metrics"
BASELINE_FILE="${METRICS_DIR}/baseline.json"
CURRENT_FILE="${METRICS_DIR}/current.json"
REGRESSION_REPORT="${METRICS_DIR}/regression-report.json"

# Severity thresholds
CRITICAL_TEST_PASS_DROP=5      # % drop in test pass rate
CRITICAL_COVERAGE_DROP=10      # % drop in code coverage
CRITICAL_PERF_DROP=20          # % performance degradation
HIGH_WARNING_INCREASE=20       # % increase in warnings
MEDIUM_COMPLEXITY_INCREASE=15  # % increase in cyclomatic complexity

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

# Collect current metrics
collect_current_metrics() {
    log_info "Collecting current metrics..."
    
    mkdir -p "${METRICS_DIR}"
    
    local test_pass_rate=0
    local coverage=0
    local warnings=0
    local perf_throughput=0
    local complexity=0
    
    # Run compilation and count warnings
    log_info "Running compilation..."
    if compile_output=$(TERM=dumb rebar3 compile 2>&1); then
        warnings=$(echo "$compile_output" | grep -c "Warning:" || true)
        log_success "Compilation passed with ${warnings} warnings"
    else
        log_error "Compilation failed"
        warnings=9999
    fi
    
    # Run tests and calculate pass rate
    log_info "Running tests..."
    if test_output=$(rebar3 eunit 2>&1); then
        local total_tests=$(echo "$test_output" | grep -oP '\d+(?= tests)' | head -1 || echo "0")
        local failed_tests=$(echo "$test_output" | grep -oP '\d+(?= failed)' | head -1 || echo "0")
        
        if [[ $total_tests -gt 0 ]]; then
            local passed_tests=$((total_tests - failed_tests))
            test_pass_rate=$(awk "BEGIN {printf \"%.2f\", ($passed_tests / $total_tests) * 100}")
        fi
        log_success "Test pass rate: ${test_pass_rate}%"
    else
        log_error "Tests failed"
        test_pass_rate=0
    fi
    
    # Get coverage if available
    if [[ -f "${PROJECT_ROOT}/_build/test/cover/index.html" ]]; then
        coverage=$(grep -oP '\d+(?=%)' "${PROJECT_ROOT}/_build/test/cover/index.html" | head -1 || echo "0")
        log_info "Code coverage: ${coverage}%"
    fi
    
    # Run quick benchmark if available
    log_info "Running performance benchmark..."
    if [[ -f "${PROJECT_ROOT}/bench/erlmcp_bench_core_ops.erl" ]]; then
        perf_output=$(erl -noshell -pa _build/default/lib/*/ebin -eval \
            "erlmcp_bench_core_ops:run(<<\"core_ops_1k\">>), halt()." 2>&1 || true)
        perf_throughput=$(echo "$perf_output" | grep -oP 'throughput_msg_per_s":\s*\K\d+' | head -1 || echo "0")
        log_info "Performance throughput: ${perf_throughput} msg/s"
    fi
    
    # Calculate complexity (lines per module as simple metric)
    complexity=$(find "${PROJECT_ROOT}/src" -name "*.erl" -exec wc -l {} + | \
        awk '{sum+=$1; count++} END {if(count>0) printf "%.0f", sum/count; else print "0"}')
    log_info "Average module complexity: ${complexity} lines"
    
    # Write current metrics
    cat > "${CURRENT_FILE}" << JSON
{
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
    "git_commit": "$(git rev-parse HEAD 2>/dev/null || echo 'unknown')",
    "git_branch": "$(git branch --show-current 2>/dev/null || echo 'unknown')",
    "metrics": {
        "test_pass_rate": ${test_pass_rate},
        "coverage": ${coverage},
        "warnings": ${warnings},
        "perf_throughput_msg_per_s": ${perf_throughput},
        "avg_module_complexity": ${complexity}
    }
}
JSON
    
    log_success "Current metrics collected"
}

# Compare metrics and detect regressions
detect_regressions() {
    log_info "Comparing metrics to baseline..."
    
    if [[ ! -f "${BASELINE_FILE}" ]]; then
        log_warn "No baseline file found. Creating baseline from current metrics."
        cp "${CURRENT_FILE}" "${BASELINE_FILE}"
        echo '{"regressions": [], "severity": "none"}' > "${REGRESSION_REPORT}"
        return 0
    fi
    
    # Parse JSON files
    local baseline_test_pass=$(jq -r '.metrics.test_pass_rate' "${BASELINE_FILE}")
    local baseline_coverage=$(jq -r '.metrics.coverage' "${BASELINE_FILE}")
    local baseline_warnings=$(jq -r '.metrics.warnings' "${BASELINE_FILE}")
    local baseline_perf=$(jq -r '.metrics.perf_throughput_msg_per_s' "${BASELINE_FILE}")
    local baseline_complexity=$(jq -r '.metrics.avg_module_complexity' "${BASELINE_FILE}")
    
    local current_test_pass=$(jq -r '.metrics.test_pass_rate' "${CURRENT_FILE}")
    local current_coverage=$(jq -r '.metrics.coverage' "${CURRENT_FILE}")
    local current_warnings=$(jq -r '.metrics.warnings' "${CURRENT_FILE}")
    local current_perf=$(jq -r '.metrics.perf_throughput_msg_per_s' "${CURRENT_FILE}")
    local current_complexity=$(jq -r '.metrics.avg_module_complexity' "${CURRENT_FILE}")
    
    local regressions=()
    local max_severity="none"
    
    # Check test pass rate regression
    if [[ $(echo "$current_test_pass < $baseline_test_pass" | bc -l) -eq 1 ]]; then
        local drop=$(echo "$baseline_test_pass - $current_test_pass" | bc -l)
        local severity="low"
        
        if [[ $(echo "$drop >= $CRITICAL_TEST_PASS_DROP" | bc -l) -eq 1 ]]; then
            severity="critical"
            max_severity="critical"
        elif [[ $(echo "$drop >= 2" | bc -l) -eq 1 ]]; then
            severity="medium"
            [[ "$max_severity" != "critical" ]] && max_severity="medium"
        fi
        
        regressions+=("$(cat <<REGRESSION
{
    "metric": "test_pass_rate",
    "baseline": ${baseline_test_pass},
    "current": ${current_test_pass},
    "change": -${drop},
    "severity": "${severity}",
    "message": "Test pass rate dropped by ${drop}%"
}
REGRESSION
)")
    fi
    
    # Check coverage regression
    if [[ $(echo "$current_coverage < $baseline_coverage" | bc -l) -eq 1 ]]; then
        local drop=$(echo "$baseline_coverage - $current_coverage" | bc -l)
        local severity="low"
        
        if [[ $(echo "$drop >= $CRITICAL_COVERAGE_DROP" | bc -l) -eq 1 ]]; then
            severity="critical"
            max_severity="critical"
        elif [[ $(echo "$drop >= 5" | bc -l) -eq 1 ]]; then
            severity="medium"
            [[ "$max_severity" != "critical" ]] && max_severity="medium"
        fi
        
        regressions+=("$(cat <<REGRESSION
{
    "metric": "coverage",
    "baseline": ${baseline_coverage},
    "current": ${current_coverage},
    "change": -${drop},
    "severity": "${severity}",
    "message": "Code coverage dropped by ${drop}%"
}
REGRESSION
)")
    fi
    
    # Check warnings increase
    if [[ $current_warnings -gt $baseline_warnings ]]; then
        local increase=$((current_warnings - baseline_warnings))
        local pct_increase=$(echo "scale=2; ($increase / ($baseline_warnings + 1)) * 100" | bc -l)
        local severity="low"
        
        if [[ $(echo "$pct_increase >= $HIGH_WARNING_INCREASE" | bc -l) -eq 1 ]]; then
            severity="high"
            [[ "$max_severity" == "none" ]] || [[ "$max_severity" == "low" ]] && max_severity="high"
        fi
        
        regressions+=("$(cat <<REGRESSION
{
    "metric": "warnings",
    "baseline": ${baseline_warnings},
    "current": ${current_warnings},
    "change": ${increase},
    "severity": "${severity}",
    "message": "Compilation warnings increased by ${increase} (${pct_increase}%)"
}
REGRESSION
)")
    fi
    
    # Check performance regression
    if [[ $baseline_perf -gt 0 ]] && [[ $(echo "$current_perf < $baseline_perf" | bc -l) -eq 1 ]]; then
        local drop_pct=$(echo "scale=2; (($baseline_perf - $current_perf) / $baseline_perf) * 100" | bc -l)
        local severity="low"
        
        if [[ $(echo "$drop_pct >= $CRITICAL_PERF_DROP" | bc -l) -eq 1 ]]; then
            severity="critical"
            max_severity="critical"
        elif [[ $(echo "$drop_pct >= 10" | bc -l) -eq 1 ]]; then
            severity="high"
            [[ "$max_severity" != "critical" ]] && max_severity="high"
        fi
        
        regressions+=("$(cat <<REGRESSION
{
    "metric": "performance",
    "baseline": ${baseline_perf},
    "current": ${current_perf},
    "change": -${drop_pct},
    "severity": "${severity}",
    "message": "Performance degraded by ${drop_pct}%"
}
REGRESSION
)")
    fi
    
    # Check complexity increase
    if [[ $(echo "$current_complexity > $baseline_complexity" | bc -l) -eq 1 ]]; then
        local increase_pct=$(echo "scale=2; (($current_complexity - $baseline_complexity) / $baseline_complexity) * 100" | bc -l)
        local severity="low"
        
        if [[ $(echo "$increase_pct >= $MEDIUM_COMPLEXITY_INCREASE" | bc -l) -eq 1 ]]; then
            severity="medium"
            [[ "$max_severity" == "none" ]] || [[ "$max_severity" == "low" ]] && max_severity="medium"
        fi
        
        regressions+=("$(cat <<REGRESSION
{
    "metric": "complexity",
    "baseline": ${baseline_complexity},
    "current": ${current_complexity},
    "change": ${increase_pct},
    "severity": "${severity}",
    "message": "Module complexity increased by ${increase_pct}%"
}
REGRESSION
)")
    fi
    
    # Generate regression report
    local regression_json=$(printf '%s\n' "${regressions[@]}" | paste -sd,)
    
    cat > "${REGRESSION_REPORT}" << JSON
{
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
    "baseline_commit": "$(jq -r '.git_commit' "${BASELINE_FILE}")",
    "current_commit": "$(jq -r '.git_commit' "${CURRENT_FILE}")",
    "severity": "${max_severity}",
    "regressions": [${regression_json}]
}
JSON
    
    # Print summary
    echo ""
    log_info "=========================================="
    log_info "  REGRESSION DETECTION REPORT"
    log_info "=========================================="
    echo ""
    
    if [[ ${#regressions[@]} -eq 0 ]]; then
        log_success "No regressions detected!"
        return 0
    fi
    
    log_warn "Found ${#regressions[@]} regression(s) - Severity: ${max_severity}"
    echo ""
    
    for regression in "${regressions[@]}"; do
        local metric=$(echo "$regression" | jq -r '.metric')
        local message=$(echo "$regression" | jq -r '.message')
        local severity=$(echo "$regression" | jq -r '.severity')
        
        case "$severity" in
            critical)
                log_error "[CRITICAL] ${message}"
                ;;
            high)
                log_error "[HIGH] ${message}"
                ;;
            medium)
                log_warn "[MEDIUM] ${message}"
                ;;
            low)
                log_warn "[LOW] ${message}"
                ;;
        esac
    done
    
    echo ""
    log_info "Full report: ${REGRESSION_REPORT}"
    
    # Return exit code based on severity
    case "$max_severity" in
        critical)
            return 3
            ;;
        high)
            return 2
            ;;
        medium)
            return 1
            ;;
        *)
            return 0
            ;;
    esac
}

# Main execution
main() {
    log_info "Starting regression detection for erlmcp..."
    
    cd "${PROJECT_ROOT}"
    
    collect_current_metrics
    detect_regressions
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        log_success "Regression detection complete - No issues found"
    else
        log_warn "Regression detection complete - Issues found (exit code: ${exit_code})"
    fi
    
    return $exit_code
}

main "$@"
