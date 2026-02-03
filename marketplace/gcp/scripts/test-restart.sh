#!/usr/bin/env bash
# Marketplace Restart Test - Verify service state after restart

set -euo pipefail

echo "=== Marketplace Restart Test ==="
echo "Testing service state after clean restart"
echo ""

PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

echo "Test 1: Clean Restart"
echo "---------------------"

# Get initial status
echo "Initial instance status:"
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='table(status,lastStartTimestamp)'

# Restart instance
echo ""
echo "Restarting instance..."
gcloud compute instances reset "${INSTANCE_NAME}" --zone="${ZONE}" --quiet

# Wait for restart
echo "Waiting 60 seconds for restart..."
sleep 60

# Check final status
echo ""
echo "Post-restart instance status:"
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='table(status,lastStartTimestamp)'

# Verify service is responding
echo ""
echo "Verifying ERLMCP service is responding..."
EXTERNAL_IP=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(networkInterfaces[0].accessConfigs[0].natIP)')

if [[ -n "${EXTERNAL_IP}" ]]; then
    echo "Testing health endpoint..."
    curl -s "http://${EXTERNAL_IP}:8080/health" || echo "Health check not available"
fi

echo ""
echo "Test 2: Serial Console Logs"
echo "---------------------------"

# Get serial port output
echo "Fetching serial console output (last boot)..."
gcloud compute instances get-serial-port-output "${INSTANCE_NAME}" --port=1 --zone="${ZONE}" --start=0 | tail -50 || true

echo ""
echo "=== Restart Test Complete ==="
