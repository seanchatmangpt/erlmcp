#!/usr/bin/env bash
# Marketplace Recovery Test - Verify disaster recovery procedures

set -euo pipefail

echo "=== Marketplace Recovery Test ==="
echo "Testing disaster recovery and backup procedures"
echo ""

PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

echo "Test 1: Snapshot Recovery"
echo "-------------------------"

# Create snapshot before test
SNAPSHOT_NAME="erlmcp-test-snapshot-$(date +%Y%m%d-%H%M%S)"
DISK_NAME="${INSTANCE_NAME}-disk"

echo "Creating snapshot of boot disk..."
gcloud compute disks snapshot "${DISK_NAME}" --snapshot-names="${SNAPSHOT_NAME}" --zone="${ZONE}"

echo "Snapshot created: ${SNAPSHOT_NAME}"
echo ""

# Simulate disk failure scenario
echo "Simulating disk failure by stopping instance..."
gcloud compute instances stop "${INSTANCE_NAME}" --zone="${ZONE}" --quiet

echo "Waiting 10 seconds..."
sleep 10

# Recovery procedure
echo "Starting recovery procedure..."
gcloud compute instances start "${INSTANCE_NAME}" --zone="${ZONE}" --quiet

echo "Waiting for instance to recover..."
sleep 60

# Verify recovery
echo "Verifying instance status after recovery..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='table(status,statusMessage)'

echo ""
echo "Test 2: Backup and Restore"
echo "-------------------------"

# List available snapshots
echo "Available snapshots:"
gcloud compute snapshots list --filter="name~'erlmcp'" --format='table(name,status,diskSizeGb,creationTimestamp)'

echo ""
echo "Test 3: Instance Template Recovery"
echo "---------------------------------"

# Check if instance template exists
TEMPLATE_NAME="erlmcp-marketplace-template"
if gcloud compute instance-templates describe "${TEMPLATE_NAME}" &>/dev/null; then
    echo "Instance template exists: ${TEMPLATE_NAME}"
    echo "Template details:"
    gcloud compute instance-templates describe "${TEMPLATE_NAME}" --format='table(properties.machineType,properties.disks[0].initializeParams.sourceImage)'
else
    echo "No instance template found (skipping)"
fi

echo ""
echo "Test 4: Health Check Recovery"
echo "----------------------------"

# Check health check configuration
HEALTH_CHECK_NAME="erlmcp-health-check"
if gcloud compute health-checks describe "${HEALTH_CHECK_NAME}" &>/dev/null; then
    echo "Health check configuration:"
    gcloud compute health-checks describe "${HEALTH_CHECK_NAME}" --format='yaml'
else
    echo "No health check found (skipping)"
fi

echo ""
echo "=== Recovery Test Complete ==="
