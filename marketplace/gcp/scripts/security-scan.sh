#!/usr/bin/env bash
# Marketplace Security Scan - Comprehensive security audit

set -euo pipefail

echo "=== Marketplace Security Audit ==="
echo "Comprehensive GCP security scan"
echo ""

PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

echo "Project: ${PROJECT_ID}"
echo "Instance: ${INSTANCE_NAME}"
echo ""

echo "Security Check 1: IAM Policy Analysis"
echo "--------------------------------------"
echo "Fetching IAM policies..."
gcloud projects get-iam-policy "${PROJECT_ID}" --format=json > /tmp/iam-policy.json

echo ""
echo "Analyzing IAM roles..."
echo "Users with Owner role:"
jq -r '.bindings[] | select(.role=="roles/owner") | .members[]' /tmp/iam-policy.json || echo "None found"

echo ""
echo "Users with Editor role:"
jq -r '.bindings[] | select(.role=="roles/editor") | .members[]' /tmp/iam-policy.json || echo "None found"

echo ""
echo "Service accounts with Editor+ roles:"
jq -r '.bindings[] | select(.role=="roles/editor" or .role=="roles/owner") | .members[] | select(startswith("serviceAccount:"))' /tmp/iam-policy.json || echo "None found"

echo ""
echo "Security Check 2: Service Account Analysis"
echo "-------------------------------------------"
echo "Listing service accounts..."
gcloud iam service-accounts list --format='table(email,disabled,title)'

echo ""
echo "Checking service account keys..."
for sa in $(gcloud iam service-accounts list --format='get(email)' --filter="disabled:false"); do
    echo "Service Account: ${sa}"
    gcloud iam service-accounts keys list --iam-account="${sa}" --format='table(name,status,keyType)' || echo "  No keys or permission denied"
    echo ""
done

echo ""
echo "Security Check 3: Firewall Rules Analysis"
echo "------------------------------------------"
echo "Listing firewall rules..."
gcloud compute firewall-rules list --format='table(name,network,direction,sourceRanges.list(),allowed[].map().firewall_rule(),disabled)'

echo ""
echo "Checking for overly permissive rules..."
echo "Rules allowing 0.0.0.0/0:"
gcloud compute firewall-rules list --format='table(name,sourceRanges.list())' --filter="sourceRanges:0.0.0.0/0" || echo "None found"

echo ""
echo "Rules allowing all ports:"
gcloud compute firewall-rules list --format='json' | jq -r '.[] | select(.allowed[].ports[] == "0-65535" or .allowed[].ports == null) | .name' || echo "None found"

echo ""
echo "Security Check 4: Network Configuration"
echo "---------------------------------------"
echo "Network details:"
gcloud compute networks list --format='table(name,subnettingMode,IPv4Range)'

echo ""
echo "Subnet details:"
gcloud compute networks subnets list --format='table(name,region,ipCidrRange,privateIpGoogleAccess)'

echo ""
echo "Security Check 5: Instance Metadata"
echo "-----------------------------------"
echo "Checking instance metadata for sensitive data..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='json' | jq '.metadata.items[]? | select(.key | test("password|secret|key|token"; "i")) | {key: .key, value: .value}' || echo "No sensitive metadata keys found"

echo ""
echo "Security Check 6: Disk Encryption"
echo "---------------------------------"
echo "Checking disk encryption status..."
DISK_NAME="${INSTANCE_NAME}"
gcloud compute disks describe "${DISK_NAME}" --zone="${ZONE}" --format='table(name,diskEncryptionKey.type,encryptionKey)' || echo "Could not fetch disk details"

echo ""
echo "Security Check 7: OS Login"
echo "-------------------------"
OS_LOGIN_STATUS=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(metadata.items.enable-oslogin)')
if [[ "${OS_LOGIN_STATUS}" == "TRUE" ]]; then
    echo "OS Login: ENABLED (recommended)"
else
    echo "OS Login: DISABLED (consider enabling)"
fi

echo ""
echo "Security Check 8: Shielded VM Configuration"
echo "--------------------------------------------"
echo "Checking Shielded VM features..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='yaml' | grep -A5 "shieldedInstanceConfig:" || echo "Shielded VM not configured"

echo ""
echo "Security Check 9: Serial Port Access"
echo "-------------------------------------"
SERIAL_PORT_STATUS=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(metadata.items.serial-port-enable)')
if [[ "${SERIAL_PORT_STATUS}" == "1" ]] || [[ "${SERIAL_PORT_STATUS}" == "TRUE" ]]; then
    echo "Serial port access: ENABLED (consider disabling)"
else
    echo "Serial port access: DISABLED (recommended)"
fi

echo ""
echo "Security Check 10: Access Configuration"
echo "----------------------------------------"
echo "Checking SSH access..."
gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(metadata.items.block-project-ssh-keys)'

echo ""
echo "Security Check 11: Vulnerability Scanning"
echo "-----------------------------------------"
echo "Checking for Container Analysis findings..."
gcloud container analysis occurrences list --filter='resourceUrl~"'${INSTANCE_NAME}'"' --format='table(noteKind,kind,severity)' || echo "No vulnerability scan results"

echo ""
echo "Security Check 12: Security Command Center"
echo "-------------------------------------------"
if gcloud scc assets list --project="${PROJECT_ID}" --format='table(name,securityCenterProperties.resourceType,securityCenterProperties.resourceDisplayName)' &>/dev/null; then
    echo "Security Command Center findings:"
    gcloud scc assets list --project="${PROJECT_ID}" --format='table(name,securityCenterProperties.resourceType,securityCenterProperties.resourceDisplayName)' || echo "No findings"
else
    echo "Security Command Center not enabled"
fi

echo ""
echo "Security Check 13: Policy Scanner"
echo "---------------------------------"
echo "Running policy constraint checks..."
gcloud asset analyze-org-policies --scope=projects/"${PROJECT_ID}" --format='table(resource,constraint,organized)' 2>/dev/null || echo "Policy analysis not available"

echo ""
echo "Security Check 14: Cloud SQL Security"
echo "-------------------------------------"
if gcloud sql instances list --format='table(name,databaseVersion,settings.tier)' 2>/dev/null; then
    echo "Cloud SQL instances found"
    echo "Checking SSL requirement..."
    for instance in $(gcloud sql instances list --format='get(name)'); do
        echo "Instance: ${instance}"
        gcloud sql instances describe "${instance}" --format='get(settings.ipConfiguration.requireSsl)' || echo "  Could not check SSL requirement"
    done
else
    echo "No Cloud SQL instances"
fi

echo ""
echo "Security Check 15: Storage Bucket Security"
echo "------------------------------------------"
echo "Checking for public buckets..."
for bucket in $(gsutil ls); do
    echo "Bucket: ${bucket}"
    gsutil iam get "${bucket}" | grep -E "allUsers|allAuthenticatedUsers" && echo "  WARNING: Public access!" || echo "  No public access"
    echo ""
done

echo ""
echo "=== Security Audit Complete ==="
echo "Review findings above and address any issues marked WARNING"
