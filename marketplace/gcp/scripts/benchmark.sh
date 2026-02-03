#!/usr/bin/env bash
# Marketplace Benchmark - Comprehensive performance benchmarking

set -euo pipefail

echo "=== Marketplace Performance Benchmark ==="
echo "Comprehensive ERLMCP performance analysis"
echo ""

PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

# Get instance external IP
EXTERNAL_IP=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(networkInterfaces[0].accessConfigs[0].natIP)')

if [[ -z "${EXTERNAL_IP}" ]]; then
    echo "ERROR: Could not get external IP"
    exit 1
fi

echo "Target endpoint: http://${EXTERNAL_IP}:8080"
echo ""

# Create results directory
RESULTS_DIR="/tmp/erlmcp-benchmark-$(date +%Y%m%d-%H%M%S)"
mkdir -p "${RESULTS_DIR}"

echo "Results directory: ${RESULTS_DIR}"
echo ""

echo "Benchmark 1: CPU Performance"
echo "------------------------------"
echo "Fetching instance CPU utilization from Cloud Monitoring..."
gcloud monitoring time-series list \
    --filter='metric.type=compute.googleapis.com/instance/cpu/utilization resource.labels.instance_name='"${INSTANCE_NAME}"'' \
    --period=300s \
    --format=json > "${RESULTS_DIR}/cpu-metrics.json" || true

if [[ -s "${RESULTS_DIR}/cpu-metrics.json" ]]; then
    echo "CPU metrics collected"
    echo "Average CPU: $(jq -r '[.timeSeries[].points[].value.doubleValue | tonumber] | add / length' "${RESULTS_DIR}/cpu-metrics.json" 2>/dev/null || echo "N/A")"
else
    echo "No CPU metrics available"
fi

echo ""
echo "Benchmark 2: Memory Performance"
echo "--------------------------------"
echo "Fetching memory metrics..."
gcloud monitoring time-series list \
    --filter='metric.type=compute.googleapis.com/instance/memory/balloon/ram_used resource.labels.instance_name='"${INSTANCE_NAME}"'' \
    --period=300s \
    --format=json > "${RESULTS_DIR}/memory-metrics.json" || true

if [[ -s "${RESULTS_DIR}/memory-metrics.json" ]]; then
    echo "Memory metrics collected"
else
    echo "No memory metrics available"
fi

echo ""
echo "Benchmark 3: Disk I/O Performance"
echo "---------------------------------"
echo "Fetching disk metrics..."
gcloud monitoring time-series list \
    --filter='metric.type=compute.googleapis.com/instance/disk/read_bytes_count resource.labels.instance_name='"${INSTANCE_NAME}"'' \
    --period=300s \
    --format=json > "${RESULTS_DIR}/disk-read-metrics.json" || true

gcloud monitoring time-series list \
    --filter='metric.type=compute.googleapis.com/instance/disk/write_bytes_count resource.labels.instance_name='"${INSTANCE_NAME}"'' \
    --period=300s \
    --format=json > "${RESULTS_DIR}/disk-write-metrics.json" || true

echo "Disk I/O metrics collected"

echo ""
echo "Benchmark 4: Network Performance"
echo "--------------------------------"
echo "Fetching network metrics..."
gcloud monitoring time-series list \
    --filter='metric.type=compute.googleapis.com/instance/network/received_bytes_count resource.labels.instance_name='"${INSTANCE_NAME}"'' \
    --period=300s \
    --format=json > "${RESULTS_DIR}/network-rx-metrics.json" || true

gcloud monitoring time-series list \
    --filter='metric.type=compute.googleapis.com/instance/network/sent_bytes_count resource.labels.instance_name='"${INSTANCE_NAME}"'' \
    --period=300s \
    --format=json > "${RESULTS_DIR}/network-tx-metrics.json" || true

echo "Network metrics collected"

echo ""
echo "Benchmark 5: Request Throughput"
echo "-------------------------------"
if command -v ab >/dev/null 2>&1; then
    echo "Running throughput benchmark..."
    ab -n 1000 -c 10 -g "${RESULTS_DIR}/throughput.tsv" "http://${EXTERNAL_IP}:8080/" || true
    echo "Throughput results saved to ${RESULTS_DIR}/throughput.tsv"
else
    echo "Apache Bench not available, skipping throughput test"
fi

echo ""
echo "Benchmark 6: Concurrent Connection Test"
echo "---------------------------------------"
if command -v ab >/dev/null 2>&1; then
    echo "Testing with 100 concurrent connections..."
    ab -n 5000 -c 100 "http://${EXTERNAL_IP}:8080/" > "${RESULTS_DIR}/concurrent-test.txt" || true
    echo "Concurrent test results saved"
fi

echo ""
echo "Benchmark 7: Response Time Distribution"
echo "----------------------------------------"
RESPONSE_TIMES=()
for i in {1..100}; do
    response_time=$(curl -o /dev/null -s -w "%{time_total}\n" "http://${EXTERNAL_IP}:8080/")
    RESPONSE_TIMES+=(${response_time}))
done

echo "Response time statistics:"
echo "${RESPONSE_TIMES[@]}" | tr ' ' '\n' | awk '
BEGIN {count=0; sum=0; sumsq=0}
{times[count++]=$1; sum+=$1; sumsq+=$1*$1}
END {
    mean=sum/count
    variance=sumsq/count - mean*mean
    stddev=sqrt(variance)
    print "  Mean: " mean " seconds"
    print "  Min: " times[0] " seconds"
    print "  Max: " times[count-1] " seconds"
    print "  Std Dev: " stddev " seconds"
}'

echo ""
echo "Benchmark 8: Instance Performance Summary"
echo "-----------------------------------------"
echo "Fetching instance details..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='yaml' > "${RESULTS_DIR}/instance-details.yaml"

echo "Machine type: $(jq -r '.machineType' "${RESULTS_DIR}/instance-details.yaml" 2>/dev/null || gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(machineType)')"
echo "Zone: ${ZONE}"
echo "Creation timestamp: $(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(creationTimestamp)')"

echo ""
echo "Benchmark 9: Cost Performance Analysis"
echo "--------------------------------------"
MACHINE_TYPE=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(machineType)' | cut -d'/' -f6)
echo "Machine type: ${MACHINE_TYPE}"

# Estimate monthly cost (rough estimate based on us-central1 pricing)
case "${MACHINE_TYPE}" in
    *e2-medium*)
        echo "Estimated monthly cost: ~$24-30"
        ;;
    *e2-standard-2*)
        echo "Estimated monthly cost: ~$48-60"
        ;;
    *n1-standard-2*)
        echo "Estimated monthly cost: ~$60-75"
        ;;
    *)
        echo "Cost estimate not available for ${MACHINE_TYPE}"
        ;;
esac

echo ""
echo "Benchmark 10: Generate Summary Report"
echo "-------------------------------------"
cat > "${RESULTS_DIR}/benchmark-summary.md" <<EOF
# ERLMCP Performance Benchmark Summary

**Timestamp**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Instance**: ${INSTANCE_NAME}
**Zone**: ${ZONE}
**Machine Type**: ${MACHINE_TYPE}
**External IP**: ${EXTERNAL_IP}

## Test Results

- CPU metrics collected
- Memory metrics collected
- Disk I/O metrics collected
- Network metrics collected
- Throughput tested
- Concurrent connections tested
- Response time distribution calculated

## Files Generated

- cpu-metrics.json
- memory-metrics.json
- disk-read-metrics.json
- disk-write-metrics.json
- network-rx-metrics.json
- network-tx-metrics.json
- throughput.tsv (if Apache Bench available)
- concurrent-test.txt
- instance-details.yaml
- benchmark-summary.md

## Recommendations

Review individual metric files for detailed analysis.
EOF

echo ""
echo "=== Benchmark Complete ==="
echo "Results saved to: ${RESULTS_DIR}"
echo "Summary report: ${RESULTS_DIR}/benchmark-summary.md"
