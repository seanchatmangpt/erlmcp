#!/usr/bin/env bash
# ============================================================================
# ERLMCP QUALITY GATE JSON REPORTER
# ============================================================================
# Machine-readable quality gate status in JSON format
# Used for CI/CD integration and programmatic analysis
# ============================================================================

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
BUILD_DIR="${PROJECT_ROOT}/_build"
BENCHMARK_THRESHOLD="-10"
COVERAGE_THRESHOLD="80"
TEST_PASS_THRESHOLD="100"

# ============================================================================
# JSON OUTPUT FUNCTIONS
# ============================================================================

json_escape() {
    local input="$1"
    echo "$input" | jq -Rs .
}

# ============================================================================
# QUALITY GATE CHECKS (JSON FORMAT)
# ============================================================================

check_compilation_json() {
    cd "$PROJECT_ROOT"

    local compile_output
    compile_output=$(TERM=dumb rebar3 compile 2>&1 || true)

    local error_count
    error_count=$(echo "$compile_output" | grep "ERROR:" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    error_count=${error_count:-0}

    local warning_count
    warning_count=$(echo "$compile_output" | grep "Warning:" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    warning_count=${warning_count:-0}

    local status="pass"
    [[ ${error_count} -gt 0 ]] && status="fail"
    [[ ${warning_count} -gt 0 ]] && status="warn"

    local module_count
    module_count=$(echo "$compile_output" | grep "Compiling" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    module_count=${module_count:-0}

    cat <<EOF
{
  "status": "$status",
  "errors": $error_count,
  "warnings": $warning_count,
  "modules_compiled": $module_count,
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

check_tests_json() {
    cd "$PROJECT_ROOT"

    local test_output
    test_output=$(rebar3 eunit 2>&1 || true)

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

    local pass_rate=0
    [[ ${total} -gt 0 ]] && pass_rate=$((passed * 100 / total))

    local status="pass"
    [[ ${failed} -gt 0 ]] && status="fail"
    [[ ${pass_rate} -lt ${TEST_PASS_THRESHOLD} ]] && status="fail"

    cat <<EOF
{
  "status": "$status",
  "passed": $passed,
  "failed": $failed,
  "total": $total,
  "pass_rate": $(printf "%.2f" "$(echo "scale=4; $pass_rate / 100" | bc)"),
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

check_coverage_json() {
    cd "$PROJECT_ROOT"

    if [[ ! -d "$BUILD_DIR/test/cover" ]]; then
        cat <<EOF
{
  "status": "warn",
  "percentage": 0,
  "message": "No coverage data found",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
        return
    fi

    local cover_output
    cover_output=$(rebar3 cover 2>&1 || true)

    local coverage=0
    if echo "$cover_output" | grep -q "total"; then
        coverage=$(echo "$cover_output" | grep "total" | grep -o "[0-9]*%" | tr -d '%' | head -1 || echo "0")
        coverage=${coverage:-0}
    fi

    local status="pass"
    [[ ${coverage} -lt ${COVERAGE_THRESHOLD} ]] && status="warn"
    [[ ${coverage} -eq 0 ]] && status="fail"

    cat <<EOF
{
  "status": "$status",
  "percentage": $(printf "%.2f" "$(echo "scale=4; $coverage / 100" | bc)"),
  "threshold": $(printf "%.2f" "$(echo "scale=4; $COVERAGE_THRESHOLD / 100" | bc)"),
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

check_dialyzer_json() {
    cd "$PROJECT_ROOT"

    local dialyzer_output
    dialyzer_output=$(rebar3 dialyzer 2>&1 || true)

    local warning_count
    warning_count=$(echo "$dialyzer_output" | grep "Warning:" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    warning_count=${warning_count:-0}

    local status="pass"
    if echo "$dialyzer_output" | grep -q "done (passed successfully)"; then
        status="pass"
    elif echo "$dialyzer_output" | grep -q "done"; then
        status="warn"
    else
        status="fail"
    fi

    cat <<EOF
{
  "status": "$status",
  "warnings": $warning_count,
  "errors": $(if [[ "$status" == "fail" ]]; then echo "1"; else echo "0"; fi),
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

check_xref_json() {
    cd "$PROJECT_ROOT"

    local xref_output
    xref_output=$(rebar3 xref 2>&1 || true)

    local warning_count
    warning_count=$(echo "$xref_output" | grep "Warning:" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    warning_count=${warning_count:-0}

    local undefined_count
    undefined_count=$(echo "$xref_output" | grep "undefined" 2>/dev/null | wc -l | tr -d ' ' || echo "0")
    undefined_count=${undefined_count:-0}

    local status="pass"
    if echo "$xref_output" | grep -q "Xref completed"; then
        [[ ${warning_count} -gt 0 ]] || [[ ${undefined_count} -gt 0 ]] && status="warn"
    else
        status="fail"
    fi

    cat <<EOF
{
  "status": "$status",
  "warnings": $warning_count,
  "undefined_calls": $undefined_count,
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

check_benchmarks_json() {
    cd "$PROJECT_ROOT"

    local latest_bench
    latest_bench=$(find bench/results -type f -name "*.json" 2>/dev/null | sort -r | head -1 || echo "")

    if [[ -z "$latest_bench" ]]; then
        cat <<EOF
{
  "status": "info",
  "regression": 0,
  "message": "No benchmark data found",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
        return
    fi

    # Check if jq is available
    if ! command -v jq &> /dev/null; then
        cat <<EOF
{
  "status": "info",
  "regression": 0,
  "message": "jq not found - cannot parse benchmark data",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
        return
    fi

    local throughput
    throughput=$(jq -r '.results[0].throughput_msg_per_s // 0' "$latest_bench" 2>/dev/null || echo "0")

    local baseline_file="${PROJECT_ROOT}/bench/baseline.json"
    if [[ -f "$baseline_file" ]]; then
        local baseline_throughput
        baseline_throughput=$(jq -r '.results[0].throughput_msg_per_s // 0' "$baseline_file" 2>/dev/null || echo "0")

        if [[ "$baseline_throughput" != "0" ]]; then
            local regression
            regression=$(awk "BEGIN {printf \"%.4f\", (($throughput - $baseline_throughput) / $baseline_throughput)}")

            local status="pass"
            local regression_pct
            regression_pct=$(awk "BEGIN {printf \"%.1f\", $regression * 100}")

            if (( $(echo "$regression < 0" | bc -l) )); then
                if (( $(echo "$regression * 100 < $BENCHMARK_THRESHOLD" | bc -l) )); then
                    status="fail"
                else
                    status="warn"
                fi
            fi

            cat <<EOF
{
  "status": "$status",
  "regression": $regression,
  "regression_percentage": $regression_pct,
  "current_throughput": $throughput,
  "baseline_throughput": $baseline_throughput,
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
            return
        fi
    fi

    cat <<EOF
{
  "status": "info",
  "regression": 0,
  "current_throughput": $throughput,
  "message": "No baseline for comparison",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

# ============================================================================
# MAIN JSON OUTPUT
# ============================================================================

main() {
    local compilation_json
    compilation_json=$(check_compilation_json)

    local tests_json
    tests_json=$(check_tests_json)

    local coverage_json
    coverage_json=$(check_coverage_json)

    local dialyzer_json
    dialyzer_json=$(check_dialyzer_json)

    local xref_json
    xref_json=$(check_xref_json)

    local benchmarks_json
    benchmarks_json=$(check_benchmarks_json)

    # Determine overall status
    local overall_status="pass"

    for json in "$compilation_json" "$tests_json" "$coverage_json" "$dialyzer_json" "$xref_json" "$benchmarks_json"; do
        local gate_status
        gate_status=$(echo "$json" | jq -r '.status')
        if [[ "$gate_status" == "fail" ]]; then
            overall_status="fail"
            break
        fi
    done

    # Output complete JSON
    cat <<EOF
{
  "overall_status": "$overall_status",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "project_root": "$PROJECT_ROOT",
  "gates": {
    "compilation": $compilation_json,
    "tests": $tests_json,
    "coverage": $coverage_json,
    "dialyzer": $dialyzer_json,
    "xref": $xref_json,
    "benchmarks": $benchmarks_json
  }
}
EOF
}

# ============================================================================
# SCRIPT EXECUTION
# ============================================================================

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
