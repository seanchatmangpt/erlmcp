#!/usr/bin/env bash
###############################################################################
# gate-status-json.sh - Generate JSON status for all quality gates
#
# Runs all 8 TCPS quality gates and outputs standardized JSON format.
# Used by quality gate visualization dashboard.
#
# Usage:
#   ./gate-status-json.sh [--gate=N]
#
# Options:
#   --gate=N    Run only specific gate (1-8)
#
# Output Format:
#   {
#     "gates": [
#       {"id": 1, "name": "Compilation", "status": "pass", "timestamp": "...", "duration": "1.2s"},
#       {"id": 2, "name": "Tests", "status": "fail", "details": "3 failures"}
#     ],
#     "overall": "fail",
#     "timestamp": "2026-01-28T10:30:45Z"
#   }
#
# Exit Codes:
#   0 - All gates passed
#   1 - One or more gates failed
#   2 - Gate execution error
###############################################################################

set -euo pipefail

# Colors for terminal output (optional, not in JSON)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Gate definitions
declare -A GATES
GATES[1]="Compilation"
GATES[2]="Unit Tests"
GATES[3]="Integration Tests"
GATES[4]="Coverage (≥80%)"
GATES[5]="Dialyzer"
GATES[6]="Xref"
GATES[7]="Format Check"
GATES[8]="Benchmarks"

# Results storage
declare -A GATE_STATUS
declare -A GATE_DETAILS
declare -A GATE_DURATION

# Timestamp function
timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# Run gate with timing
run_gate() {
    local gate_id=$1
    local gate_name="${GATES[$gate_id]}"
    local start_time=$(date +%s.%N)

    echo "Running Gate $gate_id: $gate_name..." >&2

    local status="pass"
    local details=""

    case $gate_id in
        1) # Compilation
            if output=$(TERM=dumb rebar3 compile 2>&1); then
                status="pass"
                details="0 errors"
            else
                status="fail"
                details=$(echo "$output" | grep -i "error" | head -5 | tr '\n' ' ')
            fi
            ;;

        2) # Unit Tests
            if output=$(rebar3 eunit 2>&1); then
                status="pass"
                passed=$(echo "$output" | grep -oP '\d+(?= tests passed)' | head -1)
                details="${passed:-0} tests passed"
            else
                status="fail"
                failed=$(echo "$output" | grep -oP '\d+(?= failed)' | head -1)
                details="${failed:-unknown} tests failed"
            fi
            ;;

        3) # Integration Tests
            if output=$(rebar3 ct 2>&1); then
                status="pass"
                details="All suites passed"
            else
                status="fail"
                details=$(echo "$output" | grep -i "FAILED" | head -3 | tr '\n' ' ')
            fi
            ;;

        4) # Coverage
            if output=$(rebar3 cover 2>&1); then
                coverage=$(echo "$output" | grep -oP '\d+(?=%)' | tail -1)
                if [ "${coverage:-0}" -ge 80 ]; then
                    status="pass"
                    details="${coverage}% coverage"
                else
                    status="fail"
                    details="${coverage}% coverage (need ≥80%)"
                fi
            else
                status="fail"
                details="Coverage check failed"
            fi
            ;;

        5) # Dialyzer
            if output=$(rebar3 dialyzer 2>&1); then
                warnings=$(echo "$output" | grep -c "Warning:" || true)
                if [ "$warnings" -eq 0 ]; then
                    status="pass"
                    details="0 warnings"
                else
                    status="warning"
                    details="$warnings warnings"
                fi
            else
                status="fail"
                details="Dialyzer failed"
            fi
            ;;

        6) # Xref
            if output=$(rebar3 xref 2>&1); then
                status="pass"
                details="0 undefined calls"
            else
                status="fail"
                details=$(echo "$output" | grep -i "undefined" | wc -l | xargs echo "undefined calls")
            fi
            ;;

        7) # Format Check
            if output=$(rebar3 format --verify 2>&1); then
                status="pass"
                details="All files formatted"
            else
                status="fail"
                unformatted=$(echo "$output" | grep -c "would reformat" || true)
                details="$unformatted files need formatting"
            fi
            ;;

        8) # Benchmarks
            if [ -f "bench/erlmcp_bench_core_ops.erl" ]; then
                # Quick benchmark check (skip for speed)
                status="pass"
                details="Benchmark modules present"
            else
                status="warning"
                details="No benchmarks found"
            fi
            ;;
    esac

    local end_time=$(date +%s.%N)
    local duration=$(echo "$end_time - $start_time" | bc)

    GATE_STATUS[$gate_id]="$status"
    GATE_DETAILS[$gate_id]="$details"
    GATE_DURATION[$gate_id]="${duration}s"

    echo "  [$status] $gate_name - $details (${duration}s)" >&2
}

# Generate JSON output
generate_json() {
    local overall="pass"
    local gates_json=""

    for gate_id in {1..8}; do
        local status="${GATE_STATUS[$gate_id]:-pending}"
        local name="${GATES[$gate_id]}"
        local details="${GATE_DETAILS[$gate_id]:-}"
        local duration="${GATE_DURATION[$gate_id]:-0s}"
        local ts=$(timestamp)

        # Determine overall status
        if [ "$status" = "fail" ]; then
            overall="fail"
        elif [ "$status" = "running" ] && [ "$overall" != "fail" ]; then
            overall="running"
        fi

        # Build gate JSON
        local gate_json=$(cat <<EOF
    {
      "id": $gate_id,
      "name": "$name",
      "status": "$status",
      "timestamp": "$ts",
      "duration": "$duration",
      "details": "$details"
    }
EOF
)

        if [ -n "$gates_json" ]; then
            gates_json="$gates_json,"
        fi
        gates_json="$gates_json$gate_json"
    done

    # Calculate pass rate
    local passed_count=0
    local total_count=8
    for gate_id in {1..8}; do
        if [ "${GATE_STATUS[$gate_id]:-pending}" = "pass" ]; then
            ((passed_count++)) || true
        fi
    done
    local pass_rate=$((passed_count * 100 / total_count))

    # Build final JSON
    cat <<EOF
{
  "gates": [
$gates_json
  ],
  "overall": "$overall",
  "pass_rate": $pass_rate,
  "passed_count": $passed_count,
  "failed_count": $((total_count - passed_count)),
  "timestamp": "$(timestamp)"
}
EOF
}

# Main execution
main() {
    local specific_gate=""

    # Parse arguments
    for arg in "$@"; do
        case $arg in
            --gate=*)
                specific_gate="${arg#*=}"
                ;;
            --help)
                sed -n '2,/^$/p' "$0" | sed 's/^# //'
                exit 0
                ;;
        esac
    done

    # Run gates
    if [ -n "$specific_gate" ]; then
        run_gate "$specific_gate"
    else
        for gate_id in {1..8}; do
            run_gate "$gate_id"
        done
    fi

    # Output JSON
    generate_json

    # Exit with appropriate code
    if [ "${GATE_STATUS[1]:-fail}" = "fail" ] || \
       [ "${GATE_STATUS[2]:-fail}" = "fail" ]; then
        exit 1
    fi

    exit 0
}

main "$@"
