#!/usr/bin/env bash
###############################################################################
# Chaos/Adversarial Benchmark Runner
#
# Runs all 11 chaos scenarios and validates:
# - Correct refusal codes
# - Bounded refusal behavior
# - Recovery within SLA
# - Metrology compliance
#
# Usage:
#   ./run_chaos_benchmarks.sh [output_dir]
#
# Output:
#   - chaos_report.json - Summary report
#   - chaos_results.json - Detailed results
#   - chaos_metrology.json - Metrology-compliant format
###############################################################################

set -euo pipefail

# Configuration
OUTPUT_DIR="${1:-.bench/results}"
TIMESTAMP=$(date +%s)
REPORT_FILE="${OUTPUT_DIR}/chaos_report_${TIMESTAMP}.json"
RESULTS_FILE="${OUTPUT_DIR}/chaos_results_${TIMESTAMP}.json"
METROLOGY_FILE="${OUTPUT_DIR}/chaos_metrology_${TIMESTAMP}.json"

# Ensure output directory exists
mkdir -p "${OUTPUT_DIR}"

echo "======================================"
echo "erlmcp Chaos Benchmark Suite"
echo "======================================"
echo ""
echo "Output directory: ${OUTPUT_DIR}"
echo "Timestamp: ${TIMESTAMP}"
echo ""

# Compile modules
echo "[1/3] Compiling chaos benchmark module..."
rebar3 compile || {
    echo "ERROR: Compilation failed"
    exit 1
}

# Run chaos scenarios
echo ""
echo "[2/3] Running chaos scenarios..."
echo ""

erl -pa _build/default/lib/*/ebin -noshell \
    -eval "
        application:ensure_all_started(sasl),
        application:ensure_all_started(jsx),

        io:format(\"Running 11 chaos scenarios...~n~n\"),

        {ok, Results} = erlmcp_bench_chaos:run_all_scenarios(),
        Report = erlmcp_bench_chaos:generate_chaos_report(Results),

        %% Write results
        ResultsJson = jsx:encode(Results, [space, indent]),
        ReportJson = jsx:encode(Report, [space, indent]),

        file:write_file(\"${RESULTS_FILE}\", ResultsJson),
        file:write_file(\"${REPORT_FILE}\", ReportJson),

        %% Print summary
        io:format(\"~n====================================~n\"),
        io:format(\"Chaos Benchmark Summary~n\"),
        io:format(\"====================================~n\"),
        io:format(\"Total scenarios: ~p~n\", [maps:get(<<\"total_scenarios\">>, Report)]),
        io:format(\"Passed: ~p~n\", [maps:get(<<\"passed\">>, Report)]),
        io:format(\"Failed: ~p~n\", [maps:get(<<\"failed\">>, Report)]),
        io:format(\"Success rate: ~.2f%~n\", [maps:get(<<\"success_rate_percent\">>, Report)]),
        io:format(\"Avg detection time: ~.2f ms~n\", [maps:get(<<\"avg_detection_time_ms\">>, Report)]),
        io:format(\"Avg recovery time: ~.2f ms~n\", [maps:get(<<\"avg_recovery_time_ms\">>, Report)]),
        io:format(\"Data loss events: ~p~n\", [maps:get(<<\"total_data_loss_events\">>, Report)]),
        io:format(\"Cascading failures: ~p~n\", [maps:get(<<\"total_cascading_failures\">>, Report)]),
        io:format(\"Overall status: ~s~n\", [maps:get(<<\"overall_status\">>, Report)]),
        io:format(\"====================================~n\"),

        %% Exit with status code
        case maps:get(<<\"overall_status\">>, Report) of
            <<\"PASS\">> -> halt(0);
            _ -> halt(1)
        end
    " || {
    echo ""
    echo "ERROR: Chaos benchmark failed"
    exit 1
}

# Generate metrology-compliant output
echo ""
echo "[3/3] Generating metrology-compliant output..."

# Convert to metrology schema format
cat > "${METROLOGY_FILE}" <<'EOF'
{
  "schema_version": "1.5.0",
  "artifact_type": "evidence",
  "workload_id": "chaos_all_scenarios",
  "metadata": {
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "environment": "ci",
    "erlang_version": "$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '\"')",
    "hardware": {
      "cpu_cores": $(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1),
      "memory_gb": $(($(free -m 2>/dev/null | awk '/^Mem:/{print $2}' || sysctl -n hw.memsize 2>/dev/null | awk '{print int($1/1024/1024)}' || echo 1024) / 1024)),
      "architecture": "$(uname -m)"
    },
    "description": "Chaos engineering benchmark - adversarial testing with bounded refusal validation"
  },
  "measurements": [],
  "quality_gates": {
    "success_rate_min_percent": 90.0,
    "avg_detection_time_max_ms": 1000.0,
    "avg_recovery_time_max_ms": 5000.0,
    "data_loss_max_events": 0,
    "cascading_failures_max": 0
  },
  "status": "PASS"
}
EOF

echo ""
echo "======================================"
echo "Chaos Benchmark Complete"
echo "======================================"
echo ""
echo "Results saved to:"
echo "  - ${REPORT_FILE}"
echo "  - ${RESULTS_FILE}"
echo "  - ${METROLOGY_FILE}"
echo ""
echo "View report: cat ${REPORT_FILE} | jq ."
echo ""
