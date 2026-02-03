#!/bin/bash

# Performance Test Script for erlmcp v3
# Uses wrk and other tools to run performance benchmarks

set -euo pipefail

# Configuration
BASE_URL=${BASE_URL:-"http://localhost:8080"}
DURATION=${DURATION:-30s}
THREADS=${THREADS:-4}
CONNECTIONS=${CONNECTIONS:-100}
RPS=${RPS:-1000}
REPORT_FILE=${REPORT_FILE:-"performance-report.json"}
LOG_FILE=${LOG_FILE:-"performance.log"}

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging function
log() {
    echo -e "${1:-}${2:-}${NC}" >&2
}

# Cleanup function
cleanup() {
    log "${BLUE}" "Cleaning up..."
    # Kill any background processes
    jobs -p | xargs -r kill || true
    # Remove temporary files
    rm -f wrk_output temp_data.json || true
}

# Set trap
trap cleanup EXIT

# Check prerequisites
check_prerequisites() {
    log "${BLUE}" "Checking prerequisites..."

    # Check wrk
    if ! command -v wrk &> /dev/null; then
        log "${RED}" "wrk not found. Installing..."
        # Install wrk (simplified)
        if [[ "$OSTYPE" == "linux-gnu"* ]]; then
            sudo apt-get update && sudo apt-get install -y build-essential git
            git clone https://github.com/wg/wrk.git /tmp/wrk
            cd /tmp/wrk
            make
            sudo cp wrk /usr/local/bin/
        else
            log "${RED}" "Please install wrk manually"
            exit 1
        fi
    fi

    # Check curl
    if ! command -v curl &> /dev/null; then
        log "${RED}" "curl not found"
        exit 1
    fi

    log "${GREEN}" "Prerequisites check passed"
}

# Test endpoint availability
test_endpoint() {
    local endpoint="$1"
    log "${BLUE}" "Testing endpoint: $endpoint"

    local response=$(curl -s -o /dev/null -w "%{http_code}" "$endpoint/health" || echo "000")

    if [[ "$response" -eq 200 ]]; then
        log "${GREEN}" "Endpoint is healthy (HTTP $response)"
        return 0
    else
        log "${RED}" "Endpoint unhealthy (HTTP $response)"
        return 1
    fi
}

# Run basic load test
run_basic_test() {
    log "${BLUE}" "Running basic load test..."

    wrk -t"$THREADS" -c"$CONNECTIONS" -d"$DURATION" \
        -s scripts/performance/basic.lua \
        "$BASE_URL" > wrk_output 2>&1

    # Extract metrics
    local req_per_sec=$(grep "Requests/sec" wrk_output | awk '{print $2}')
    local latency=$(grep "Latency" wrk_output | awk '{print 2}')
    local transfer=$(grep "Transfer" wrk_output | awk '{print 2}')

    cat << JSON > temp_data.json
{
    "test_type": "basic_load",
    "duration": "$DURATION",
    "threads": "$THREADS",
    "connections": "$CONNECTIONS",
    "results": {
        "requests_per_second": $req_per_sec,
        "latency_ms": $latency,
        "transfer_mb": $transfer
    },
    "timestamp": "$(date -Iseconds)"
}
JSON

    log "${GREEN}" "Basic test completed"
    log "${BLUE}" "Requests/sec: $req_per_sec"
    log "${BLUE}" "Latency: $latency"
}

# Run spike test
run_spike_test() {
    log "${BLUE}" "Running spike test..."

    # Create spike script
    cat > scripts/performance/spike.lua << EOF
wrk.method = "GET"
wrk.headers["Content-Type"] = "application/json"

-- Simulate spike in requests
request = function()
    local time = tonumber(wrk.lookup("timed"))
    if time < 10 then
        wrk.headers["X-Spike-Load"] = "true"
    end
    return wrk.request()
end
EOF

    # Run spike test
    wrk -t"$THREADS" -c"$CONNECTIONS" -d"$DURATION" \
        -s scripts/performance/spike.lua \
        "$BASE_URL" > wrk_output 2>&1

    # Analyze spike results
    log "${YELLOW}" "Spike test completed"
}

# Run stress test
run_stress_test() {
    log "${BLUE}" "Running stress test..."

    # High load configuration
    local high_connections=$((CONNECTIONS * 2))
    local high_threads=$((THREADS * 2))

    wrk -t"$high_threads" -c"$high_connections" -d"$DURATION" \
        -s scripts/performance/stress.lua \
        "$BASE_URL" > wrk_output 2>&1

    log "${YELLOW}" "Stress test completed"
}

# Run latency test
run_latency_test() {
    log "${BLUE}" "Running latency test..."

    wrk -t"$THREADS" -c"$CONNECTIONS" -d"$DURATION" \
        -s scripts/performance/latency.lua \
        "$BASE_URL" > wrk_output 2>&1

    # Extract percentiles
    local p50=$(grep "50%" wrk_output | awk '{print $2}')
    local p90=$(grep "90%" wrk_output | awk '{print $2}')
    local p99=$(grep "99%" wrk_output | awk '{print $2}')

    cat << JSON >> temp_data.json
,
{
    "test_type": "latency",
    "results": {
        "p50_ms": $p50,
        "p90_ms": $p90,
        "p99_ms": $p99
    },
    "timestamp": "$(date -Iseconds)"
}
JSON

    log "${BLUE}" "Latency percentiles: P50=$p50ms, P90=$p90ms, P99=$p99ms"
}

# Run throughput test
run_throughput_test() {
    log "${BLUE}" "Running throughput test..."

    # Use wrk's rate limiting
    wrk -t"$THREADS" -c"$CONNECTIONS" -d"$DURATION" \
        -s scripts/performance/throughput.lua \
        -R"$RPS" \
        "$BASE_URL" > wrk_output 2>&1

    log "${GREEN}" "Throughput test completed with $RPS req/s target"
}

# Check resource usage
check_resources() {
    log "${BLUE}" "Checking resource usage..."

    # Get system metrics
    if command -v top &> /dev/null; then
        # CPU usage
        local cpu=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1}')
        log "${BLUE}" "CPU usage: $cpu%"

        # Memory usage
        local mem=$(free -m | awk '/Mem:/ {printf("%.1f", $3/$2*100)}')
        log "${BLUE}" "Memory usage: $mem%"

        cat << JSON >> temp_data.json
,
{
    "test_type": "resource_usage",
    "results": {
        "cpu_percent": $cpu,
        "memory_percent": $mem
    },
    "timestamp": "$(date -Iseconds)"
}
JSON
    fi
}

# Generate report
generate_report() {
    log "${BLUE}" "Generating report..."

    # Combine all results
    echo "[" > "$REPORT_FILE"
    cat temp_data.json >> "$REPORT_FILE"
    echo "]" >> "$REPORT_FILE"

    # Convert to pretty JSON
    if command -v jq &> /dev/null; then
        jq '.' "$REPORT_FILE" > temp.json && mv temp.json "$REPORT_FILE"
    fi

    log "${GREEN}" "Report generated: $REPORT_FILE"
}

# Compare with baseline
compare_with_baseline() {
    local baseline_file="${1:-baseline.json}"

    if [[ -f "$baseline_file" ]]; then
        log "${BLUE}" "Comparing with baseline..."

        if command -v jq &> /dev/null; then
            # Compare metrics
            local current_rps=$(jq '.[0].results.requests_per_second' "$REPORT_FILE")
            local baseline_rps=$(jq '.[0].results.requests_per_second' "$baseline_file")

            if [[ -n "$current_rps" && -n "$baseline_rps" ]]; then
                local change=$(( (current_rps - baseline_rps) * 100 / baseline_rps ))

                if [[ "$change" -gt 10 ]]; then
                    log "${RED}" "Performance degraded by ${change}%"
                elif [[ "$change" -lt -10 ]]; then
                    log "${GREEN}" "Performance improved by ${change}%"
                else
                    log "${BLUE}" "Performance within acceptable range"
                fi
            fi
        fi
    fi
}

# Main execution
main() {
    log "${BLUE}" "Starting performance tests for erlmcp v3"
    log "${BLUE}" "Endpoint: $BASE_URL"
    log "${BLUE}" "Duration: $DURATION"
    log "${BLUE}" "Threads: $THREADS"
    log "${BLUE}" "Connections: $CONNECTIONS"

    # Check prerequisites
    check_prerequisites

    # Test endpoint first
    if ! test_endpoint "$BASE_URL"; then
        log "${RED}" "Endpoint not available, exiting"
        exit 1
    fi

    # Create performance directory
    mkdir -p scripts/performance

    # Run tests
    run_basic_test
    run_latency_test
    run_throughput_test
    run_spike_test
    run_stress_test
    check_resources

    # Generate report
    generate_report

    # Compare with baseline
    compare_with_baseline

    log "${GREEN}" "Performance tests completed successfully"
}

# Script entry points
case "${1:-}" in
    "basic")
        check_prerequisites
        test_endpoint "$BASE_URL"
        run_basic_test
        ;;
    "spike")
        check_prerequisites
        test_endpoint "$BASE_URL"
        run_spike_test
        ;;
    "stress")
        check_prerequisites
        test_endpoint "$BASE_URL"
        run_stress_test
        ;;
    "latency")
        check_prerequisites
        test_endpoint "$BASE_URL"
        run_latency_test
        ;;
    "throughput")
        check_prerequisites
        test_endpoint "$BASE_URL"
        run_throughput_test
        ;;
    "resources")
        check_resources
        ;;
    "report")
        generate_report
        ;;
    "compare")
        compare_with_baseline "${2:-}"
        ;;
    *)
        main
        ;;
esac