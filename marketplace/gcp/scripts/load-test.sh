#!/usr/bin/env bash
# Marketplace Load Test - Verify performance under load

set -euo pipefail

echo "=== Marketplace Load Test ==="
echo "Testing ERLMCP performance under concurrent load"
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

# Check if ab (Apache Bench) is available
if ! command -v ab >/dev/null 2>&1; then
    echo "WARNING: Apache Bench (ab) not installed"
    echo "Install with: sudo apt-get install apache2-utils"
    echo "Skipping load test..."
    exit 0
fi

echo "Test 1: Baseline Performance (10 concurrent, 100 requests)"
echo "----------------------------------------------------------"
ab -n 100 -c 10 -q "http://${EXTERNAL_IP}:8080/" || true

echo ""
echo "Test 2: High Concurrency (50 concurrent, 500 requests)"
echo "------------------------------------------------------"
ab -n 500 -c 50 -q "http://${EXTERNAL_IP}:8080/" || true

echo ""
echo "Test 3: Sustained Load (20 concurrent, 1000 requests)"
echo "-----------------------------------------------------"
ab -n 1000 -c 20 -q "http://${EXTERNAL_IP}:8080/" || true

echo ""
echo "Test 4: Connection Time Analysis"
echo "--------------------------------"
echo "Performing connection time test..."
for i in {1..10}; do
    curl -o /dev/null -s -w "%{time_connect}\n" "http://${EXTERNAL_IP}:8080/" || echo "FAILED"
done | awk '{sum+=$1; count++} END {print "Average connect time:", sum/count, "seconds"}'

echo ""
echo "Test 5: Throughput Analysis"
echo "---------------------------"
echo "Running 60-second sustained throughput test..."
ab -n 10000 -c 20 -t 60 "http://${EXTERNAL_IP}:8080/" || true

echo ""
echo "=== Load Test Complete ==="
echo "Review the output above for:"
echo "  - Requests per second"
echo "  - Time per request"
echo "  - Failed requests"
echo "  - Transfer rate"
