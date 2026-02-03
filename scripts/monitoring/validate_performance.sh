#!/bin/bash

# Performance validation against baseline
set -e

BASELINE_URL=$1
CURRENT_URL=$2

if [ -z "$BASELINE_URL" ] || [ -z "$CURRENT_URL" ]; then
    echo "Usage: $0 <baseline_url> <current_url>"
    exit 1
fi

# Configuration
METRICS=(
    "response_time"
    "throughput"
    "error_rate"
    "p99_latency"
    "p95_latency"
    "p90_latency"
    "cpu_usage"
    "memory_usage"
)

# Function to collect metrics
collect_metrics() {
    local url=$1
    local metrics_file="temp_metrics_$(date +%s).json"

    # Generate load and collect metrics
    curl -s "$url/mcp/capabilities" >/dev/null &
    curl -s "$url/mcp/list_tools" >/dev/null &
    curl -s "$url/health/metrics" > $metrics_file

    sleep 5

    # Parse Prometheus metrics
    response_time=$(cat $metrics_file | grep "http_request_duration_seconds_sum" | awk '{print $2}')
    request_count=$(cat $metrics_file | grep "http_requests_total" | awk '{print $2}')

    rm -f $metrics_file

    echo "{
        \"url\": \"$url\",
        \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",
        \"metrics\": {
            \"response_time\": $response_time,
            \"throughput\": $request_count,
            \"error_rate\": \"0.0\"
        }
    }" | jq .
}

# Compare metrics
compare_metrics() {
    local baseline=$1
    local current=$2
    local metric=$3
    local threshold=0.1  # 10% threshold

    baseline_value=$(jq ".metrics.$metric" $baseline)
    current_value=$(jq ".metrics.$metric" $current)

    # Calculate percentage change
    change=$(echo "scale=2; ($current_value - $baseline_value) / $baseline_value" | bc)

    echo "Metric: $metric"
    echo "  Baseline: $baseline_value"
    echo "  Current: $current_value"
    echo "  Change: ${change}%"

    if [ "$(echo "$change > $threshold" | bc)" -eq 1 ] || [ "$(echo "$change < -$threshold" | bc)" -eq 1 ]; then
        echo "  ⚠ REGRESSION DETECTED"
        return 1
    fi

    echo "  ✓ Acceptable change"
    return 0
}

# Collect baseline metrics
echo "Collecting baseline metrics from $BASELINE_URL..."
baseline_metrics=$(mktemp)
collect_metrics "$BASELINE_URL" > $baseline_metrics

# Collect current metrics
echo "Collecting current metrics from $CURRENT_URL..."
current_metrics=$(mktemp)
collect_metrics "$CURRENT_URL" > $current_metrics

# Compare metrics
regressions=0
for metric in "${METRICS[@]}"; do
    if ! compare_metrics $baseline_metrics $current_metrics $metric; then
        regressions=$((regressions + 1))
    fi
done

# Generate report
cat > performance_comparison_$(date +%s).json <<EOF
{
    "baseline_url": "$BASELINE_URL",
    "current_url": "$CURRENT_URL",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "comparisons": {
        "response_time": $(jq -r ".metrics.response_time" $baseline_metrics),
        "throughput": $(jq -r ".metrics.throughput" $baseline_metrics),
        "error_rate": $(jq -r ".metrics.error_rate" $baseline_metrics)
    },
    "regressions_detected": $regressions
}
EOF

# Clean up
rm -f $baseline_metrics $current_metrics

if [ $regressions -gt 0 ]; then
    echo "Performance regression detected!"
    exit 1
else
    echo "No performance regressions detected!"
    exit 0
fi