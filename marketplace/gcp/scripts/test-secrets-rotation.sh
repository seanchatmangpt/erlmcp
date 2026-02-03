#!/usr/bin/env bash
# Marketplace Secrets Rotation Test - Verify secret rotation procedures

set -euo pipefail

echo "=== Marketplace Secrets Rotation Test ==="
echo "Testing secret rotation and update procedures"
echo ""

PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

echo "Test 1: Secret Manager Configuration"
echo "------------------------------------"

# Check if Secret Manager API is enabled
if gcloud services list --enabled --filter='name:secretmanager.googleapis.com' --format='get(name)' | grep -q secretmanager; then
    echo "Secret Manager API: ENABLED"
else
    echo "Secret Manager API: DISABLED (not tested)"
    exit 0
fi

echo ""
echo "Listing secrets in project..."
gcloud secrets list --format='table(name,createTime,replication.automatic.locations)'

echo ""
echo "Test 2: Create Test Secret"
echo "-------------------------"
TEST_SECRET_NAME="erlmcp-test-secret-$(date +%s)"

echo "Creating test secret: ${TEST_SECRET_NAME}"
echo "erlmcp-test-value-$(date +%s)" | gcloud secrets create "${TEST_SECRET_NAME}" --data-file=-

echo "Secret created successfully"
gcloud secrets describe "${TEST_SECRET_NAME}" --format='table(name,createTime,state)'

echo ""
echo "Test 3: Access Secret Version"
echo "----------------------------"
echo "Accessing secret..."
SECRET_VALUE=$(gcloud secrets versions access latest --secret="${TEST_SECRET_NAME}")
echo "Secret value: ${SECRET_VALUE}"

echo ""
echo "Test 4: Rotate Secret"
echo "---------------------"
echo "Creating new secret version..."
echo "erlmcp-rotated-value-$(date +%s)" | gcloud secrets versions add "${TEST_SECRET_NAME}" --data-file=-

echo "New version created"
gcloud secrets versions list "${TEST_SECRET_NAME}" --format='table(version,state,createTime)'

echo ""
echo "Test 5: Verify Rotation"
echo "-----------------------"
NEW_SECRET_VALUE=$(gcloud secrets versions access latest --secret="${TEST_SECRET_NAME}")
echo "New secret value: ${NEW_SECRET_VALUE}"

if [[ "${SECRET_VALUE}" != "${NEW_SECRET_VALUE}" ]]; then
    echo "SUCCESS: Secret rotation verified"
else
    echo "FAILED: Secret value did not change"
fi

echo ""
echo "Test 6: IAM Permissions for Secret"
echo "----------------------------------"
echo "Checking secret IAM policy..."
gcloud secrets get-iam-policy "${TEST_SECRET_NAME}" --format=json

echo ""
echo "Test 7: Disable Old Secret Version"
echo "----------------------------------"
LATEST_VERSION=$(gcloud secrets versions list "${TEST_SECRET_NAME}" --format='get(version)' --limit=1)
echo "Disabling version: ${LATEST_VERSION}"
gcloud secrets versions disable "${LATEST_VERSION}" --secret="${TEST_SECRET_NAME}"

echo "Version disabled"
gcloud secrets versions list "${TEST_SECRET_NAME}" --format='table(version,state,createTime)'

echo ""
echo "Test 8: Destroy Secret Version"
echo "------------------------------"
echo "Enabling version for destruction..."
gcloud secrets versions enable "${LATEST_VERSION}" --secret="${TEST_SECRET_NAME}"

echo "Destroying version..."
gcloud secrets versions destroy "${LATEST_VERSION}" --secret="${TEST_SECRET_NAME}" --quiet

echo "Version destroyed"
gcloud secrets versions list "${TEST_SECRET_NAME}" --format='table(version,state,createTime)'

echo ""
echo "Test 9: Delete Test Secret"
echo "-------------------------"
echo "Deleting test secret..."
gcloud secrets delete "${TEST_SECRET_NAME}" --quiet

echo "Secret deleted"

echo ""
echo "Test 10: Instance Access to Secrets"
echo "-----------------------------------"
echo "Checking instance service account..."
INSTANCE_SA=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(serviceAccounts[0].email)')
echo "Instance service account: ${INSTANCE_SA}"

echo ""
echo "Verifying Secret Accessor role..."
if gcloud projects get-iam-policy "${PROJECT_ID}" --format=json | jq -r '.bindings[] | select(.role=="roles/secretmanager.secretAccessor") | .members[]' | grep -q "${INSTANCE_SA}"; then
    echo "Instance has secretAccessor role"
else
    echo "WARNING: Instance may not have secretAccessor role"
fi

echo ""
echo "=== Secrets Rotation Test Complete ==="
