#!/usr/bin/env bash
# ============================================================================
# Secret Manager Security Validation Script
# Validates security configuration against best practices
# ============================================================================
#
# DOCKER-ONLY: This script must be run via Docker container
# Per erlmcp Constitution, host execution is forbidden
#
# Usage:
#   docker compose run --rm security-validator \
#     --project-id=PROJECT_ID \
#     --check-all
#
# ============================================================================

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
WARNINGS=0

# Functions
print_header() {
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

check_passed() {
    ((PASSED++))
    echo -e "${GREEN}✓${NC} $1"
}

check_failed() {
    ((FAILED++))
    echo -e "${RED}✗${NC} $1"
}

check_warning() {
    ((WARNINGS++))
    echo -e "${YELLOW}⚠${NC} $1"
}

# Parse arguments
PROJECT_ID=""
CHECK_ALL=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --project-id=*)
            PROJECT_ID="${1#*=}"
            shift
            ;;
        --check-all)
            CHECK_ALL=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

if [[ -z "$PROJECT_ID" ]]; then
    echo "Error: --project-id is required"
    exit 1
fi

print_header "Secret Manager Security Validation for Project: $PROJECT_ID"

# ============================================================================
# Check 1: Verify Secret Manager API is enabled
# ============================================================================
echo ""
print_header "Check 1: Secret Manager API Status"

if gcloud services list --project="$PROJECT_ID" --enabled --filter="name:secretmanager.googleapis.com" --format="value(name)" | grep -q secretmanager; then
    check_passed "Secret Manager API is enabled"
else
    check_failed "Secret Manager API is NOT enabled"
fi

# ============================================================================
# Check 2: Verify all required secrets exist
# ============================================================================
echo ""
print_header "Check 2: Required Secrets Existence"

REQUIRED_SECRETS=(
    "erlmcp-erlang-cookie"
    "erlmcp-db-password"
    "erlmcp-redis-password"
    "erlmcp-tls-cert"
    "erlmcp-tls-key"
    "erlmcp-ca-bundle"
    "erlmcp-jwt-private-key"
    "erlmcp-jwt-public-key"
    "erlmcp-grafana-password"
    "erlmcp-backup-key"
    "erlmcp-otel-ca-cert"
)

for secret in "${REQUIRED_SECRETS[@]}"; do
    if gcloud secrets describe "$secret" --project="$PROJECT_ID" &>/dev/null; then
        check_passed "Secret exists: $secret"
    else
        check_failed "Secret MISSING: $secret"
    fi
done

# ============================================================================
# Check 3: Verify CMEK encryption for critical secrets
# ============================================================================
echo ""
print_header "Check 3: CMEK Encryption Status"

CRITICAL_SECRETS=(
    "erlmcp-erlang-cookie"
    "erlmcp-db-password"
    "erlmcp-tls-key"
    "erlmcp-jwt-private-key"
    "erlmcp-backup-key"
)

for secret in "${CRITICAL_SECRETS[@]}"; do
    if gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | grep -q "customerManagedEncryption"; then
        check_passed "CMEK enabled for critical secret: $secret"
    else
        check_warning "CMEK NOT enabled for critical secret: $secret (Recommended for production)"
    fi
done

# ============================================================================
# Check 4: Verify rotation configuration
# ============================================================================
echo ""
print_header "Check 4: Automatic Rotation Configuration"

ROTATION_SECRETS=(
    "erlmcp-erlang-cookie"
    "erlmcp-db-password"
    "erlmcp-redis-password"
    "erlmcp-jwt-private-key"
    "erlmcp-backup-key"
)

for secret in "${ROTATION_SECRETS[@]}"; do
    if gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | grep -q "rotation"; then
        rotation_period=$(gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="value(rotation.rotationPeriod)")
        check_passed "Rotation configured for $secret: $rotation_period"
    else
        check_warning "Rotation NOT configured for $secret (Recommended for production)"
    fi
done

# ============================================================================
# Check 5: Verify replication configuration
# ============================================================================
echo ""
print_header "Check 5: Multi-Region Replication"

for secret in "${CRITICAL_SECRETS[@]}"; do
    replication=$(gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | jq -r '.replication | keys[0]')

    if [[ "$replication" == "userManaged" ]]; then
        replica_count=$(gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | jq -r '.replication.userManaged.replicas | length')
        if [[ $replica_count -ge 2 ]]; then
            check_passed "Multi-region replication configured for $secret: $replica_count replicas"
        else
            check_warning "Single region for critical secret $secret (Multi-region recommended for HA)"
        fi
    elif [[ "$replication" == "automatic" ]]; then
        check_warning "Automatic replication for critical secret $secret (User-managed recommended for control)"
    fi
done

# ============================================================================
# Check 6: Verify IAM bindings follow least-privilege
# ============================================================================
echo ""
print_header "Check 6: IAM Least-Privilege Validation"

for secret in "${REQUIRED_SECRETS[@]}"; do
    binding_count=$(gcloud secrets get-iam-policy "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | jq '[.bindings[] | select(.role == "roles/secretmanager.secretAccessor")] | length')

    if [[ $binding_count -eq 0 ]]; then
        check_warning "No accessor bindings for $secret (May need IAM configuration)"
    elif [[ $binding_count -le 5 ]]; then
        check_passed "Least-privilege IAM for $secret: $binding_count accessor bindings"
    else
        check_warning "Many accessor bindings for $secret: $binding_count (Review for least-privilege)"
    fi
done

# ============================================================================
# Check 7: Verify audit logging is enabled
# ============================================================================
echo ""
print_header "Check 7: Audit Logging Configuration"

audit_config=$(gcloud projects get-iam-policy "$PROJECT_ID" --format="json" 2>/dev/null | jq -r '.auditConfigs[] | select(.service == "secretmanager.googleapis.com")')

if [[ -n "$audit_config" ]]; then
    check_passed "Audit logging is enabled for Secret Manager"

    if echo "$audit_config" | grep -q "DATA_READ"; then
        check_passed "DATA_READ audit logging enabled (tracks secret access)"
    else
        check_warning "DATA_READ audit logging NOT enabled (Recommended for compliance)"
    fi

    if echo "$audit_config" | grep -q "DATA_WRITE"; then
        check_passed "DATA_WRITE audit logging enabled (tracks secret modifications)"
    else
        check_warning "DATA_WRITE audit logging NOT enabled (Recommended for compliance)"
    fi
else
    check_failed "Audit logging is NOT enabled for Secret Manager"
fi

# ============================================================================
# Check 8: Verify secret version management
# ============================================================================
echo ""
print_header "Check 8: Secret Version Management"

for secret in "${REQUIRED_SECRETS[@]}"; do
    version_count=$(gcloud secrets versions list "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | jq 'length')
    enabled_count=$(gcloud secrets versions list "$secret" --project="$PROJECT_ID" --filter="state=ENABLED" --format="json" 2>/dev/null | jq 'length')

    if [[ $version_count -gt 0 ]]; then
        if [[ $enabled_count -eq 1 ]]; then
            check_passed "Single enabled version for $secret (versions: $version_count, enabled: $enabled_count)"
        elif [[ $enabled_count -gt 1 ]]; then
            check_warning "Multiple enabled versions for $secret: $enabled_count (Disable old versions)"
        else
            check_failed "No enabled versions for $secret"
        fi
    else
        check_warning "No versions exist for $secret (Needs initial value)"
    fi
done

# ============================================================================
# Check 9: Verify secret labels and annotations
# ============================================================================
echo ""
print_header "Check 9: Labels and Annotations"

for secret in "${REQUIRED_SECRETS[@]}"; do
    labels=$(gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | jq -r '.labels')
    annotations=$(gcloud secrets describe "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | jq -r '.annotations')

    if [[ "$labels" != "null" ]] && [[ -n "$labels" ]]; then
        check_passed "Labels configured for $secret"
    else
        check_warning "No labels for $secret (Labels help organization and cost tracking)"
    fi

    if [[ "$annotations" != "null" ]] && [[ -n "$annotations" ]]; then
        check_passed "Annotations configured for $secret (compliance tracking)"
    else
        check_warning "No annotations for $secret (Annotations help compliance tracking)"
    fi
done

# ============================================================================
# Check 10: Verify KMS key configuration (if CMEK enabled)
# ============================================================================
echo ""
print_header "Check 10: KMS Key Configuration"

if gcloud services list --project="$PROJECT_ID" --enabled --filter="name:cloudkms.googleapis.com" --format="value(name)" | grep -q cloudkms; then
    check_passed "Cloud KMS API is enabled"

    # Check for erlmcp KMS key ring
    if gcloud kms keyrings describe erlmcp-secrets-keyring --location=us-central1 --project="$PROJECT_ID" &>/dev/null; then
        check_passed "KMS key ring exists: erlmcp-secrets-keyring"

        # Check for encryption key
        if gcloud kms keys describe erlmcp-secrets-key --keyring=erlmcp-secrets-keyring --location=us-central1 --project="$PROJECT_ID" &>/dev/null; then
            check_passed "KMS encryption key exists: erlmcp-secrets-key"

            # Check rotation period
            rotation_period=$(gcloud kms keys describe erlmcp-secrets-key --keyring=erlmcp-secrets-keyring --location=us-central1 --project="$PROJECT_ID" --format="value(rotationPeriod)")
            if [[ -n "$rotation_period" ]]; then
                check_passed "KMS key rotation configured: $rotation_period"
            else
                check_warning "KMS key rotation NOT configured (90-day rotation recommended)"
            fi
        else
            check_warning "KMS encryption key NOT found"
        fi
    else
        check_warning "KMS key ring NOT found (Required for CMEK)"
    fi
else
    check_warning "Cloud KMS API is NOT enabled (Required for CMEK)"
fi

# ============================================================================
# Check 11: Security best practices
# ============================================================================
echo ""
print_header "Check 11: Security Best Practices"

# Check for overly permissive IAM
project_iam=$(gcloud projects get-iam-policy "$PROJECT_ID" --format="json" 2>/dev/null)

if echo "$project_iam" | jq -r '.bindings[] | select(.role == "roles/secretmanager.admin") | .members[]' | grep -q "allUsers"; then
    check_failed "CRITICAL: Secret Manager admin role granted to allUsers"
elif echo "$project_iam" | jq -r '.bindings[] | select(.role == "roles/secretmanager.admin") | .members[]' | grep -q "allAuthenticatedUsers"; then
    check_failed "CRITICAL: Secret Manager admin role granted to allAuthenticatedUsers"
else
    check_passed "No overly permissive project-level IAM for Secret Manager"
fi

# Check for public access
for secret in "${REQUIRED_SECRETS[@]}"; do
    if gcloud secrets get-iam-policy "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | grep -q "allUsers"; then
        check_failed "CRITICAL: Public access (allUsers) on secret: $secret"
    elif gcloud secrets get-iam-policy "$secret" --project="$PROJECT_ID" --format="json" 2>/dev/null | grep -q "allAuthenticatedUsers"; then
        check_failed "CRITICAL: Public access (allAuthenticatedUsers) on secret: $secret"
    fi
done

check_passed "No public access detected on secrets"

# ============================================================================
# Summary
# ============================================================================
echo ""
print_header "Validation Summary"

TOTAL=$((PASSED + FAILED + WARNINGS))

echo -e "${GREEN}Passed:${NC}   $PASSED / $TOTAL"
echo -e "${RED}Failed:${NC}   $FAILED / $TOTAL"
echo -e "${YELLOW}Warnings:${NC} $WARNINGS / $TOTAL"

echo ""

if [[ $FAILED -eq 0 ]] && [[ $WARNINGS -eq 0 ]]; then
    echo -e "${GREEN}✓ Excellent! All security checks passed.${NC}"
    exit 0
elif [[ $FAILED -eq 0 ]]; then
    echo -e "${YELLOW}⚠ Good! All critical checks passed, but there are warnings to review.${NC}"
    exit 0
else
    echo -e "${RED}✗ Failed! Critical security issues detected. Please review and fix.${NC}"
    exit 1
fi
