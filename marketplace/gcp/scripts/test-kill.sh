#!/usr/bin/env bash
# Marketplace Kill Test - Verify service recovery after termination

set -euo pipefail

echo "=== Marketplace Kill Test ==="
echo "Testing service recovery after VM/instance termination"
echo ""

# Get project ID
PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

echo "Test 1: VM Instance Kill"
echo "------------------------"

# Stop instance
echo "Stopping instance..."
gcloud compute instances stop "${INSTANCE_NAME}" --zone="${ZONE}" --quiet

# Wait 10 seconds
echo "Waiting 10 seconds..."
sleep 10

# Start instance
echo "Starting instance..."
gcloud compute instances start "${INSTANCE_NAME}" --zone="${ZONE}" --quiet

# Wait for instance to be ready
echo "Waiting for instance to be READY..."
sleep 30

# Check instance status
echo "Checking instance status..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='table(status,machineType,statusMessage)'

# Test SSH connectivity
echo "Testing SSH connectivity..."
gcloud compute ssh "${INSTANCE_NAME}" --zone="${ZONE}" --command="echo 'SSH connection successful'" || true

echo ""
echo "Test 2: Forceful Termination"
echo "----------------------------"

# Reset instance (simulates crash)
echo "Resetting instance (simulates OS crash)..."
gcloud compute instances reset "${INSTANCE_NAME}" --zone="${ZONE}" --quiet

# Wait for recovery
echo "Waiting for recovery..."
sleep 45

# Check status again
echo "Checking post-recovery status..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='table(status,statusMessage)'

echo ""
echo "=== Kill Test Complete ==="
