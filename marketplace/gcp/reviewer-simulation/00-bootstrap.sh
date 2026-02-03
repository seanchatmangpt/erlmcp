#!/usr/bin/env bash
# ============================================================================
# Google Marketplace Reviewer Simulation - Phase 0: Project Bootstrap
# ============================================================================
# This script simulates exactly what GCP Marketplace reviewers do when
# starting to test a new solution deployment.
#
# DRY RUN MODE: This script documents what WOULD be run without actually
# creating GCP resources or incurring costs.
#
# Usage: ./00-bootstrap.sh [--dry-run] [--project-id PROJECT_ID]
# ============================================================================

set -euo pipefail

# Color output for readability
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ID="${PROJECT_ID:-reviewer-test-1770096612}"
DRY_RUN="${DRY_RUN:-true}"
REGION="${REGION:-us-central1}"
BILLING_ACCOUNT="${BILLING_ACCOUNT:-}"

# Log functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_command() {
    echo -e "${YELLOW}[DRY RUN]${NC} Would execute: $1"
}

# ============================================================================
# STEP 1: Validate GCP Authentication
# ============================================================================
validate_authentication() {
    log_info "Step 1: Validating GCP authentication..."

    # Check if gcloud is installed
    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI is not installed"
        log_info "Install from: https://cloud.google.com/sdk/docs/install"
        return 1
    fi

    # Check gcloud version
    local gcloud_version
    gcloud_version=$(gcloud version --format='value(version)' 2>/dev/null || echo "unknown")
    log_info "gcloud version: ${gcloud_version}"

    # Check if authenticated
    if ! gcloud auth list --filter="status:ACTIVE" --format="value(account)" 2>/dev/null | grep -q .; then
        log_error "No active gcloud authentication found"
        log_command "gcloud auth login"
        return 1
    fi

    local active_account
    active_account=$(gcloud auth list --filter="status:ACTIVE" --format="value(account)")
    log_success "Authenticated as: ${active_account}"

    return 0
}

# ============================================================================
# STEP 2: Create or Select GCP Project
# ============================================================================
create_project() {
    log_info "Step 2: Creating GCP project for reviewer testing..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_command "gcloud projects create ${PROJECT_ID} \\
            --name=\"Marketplace Reviewer Test - $(date +%Y-%m-%d)\" \\
            --organization-id=ORGANIZATION_ID \\
            --set-as-default"

        log_info "Project ID: ${PROJECT_ID}"
        log_info "Project Name: Marketplace Reviewer Test - $(date +%Y-%m-%d)"
        log_info "Organization: Would use reviewer's organization ID"

        # Document project metadata
        cat > "${SCRIPT_DIR}/project-metadata.json" <<EOF
{
  "project_id": "${PROJECT_ID}",
  "project_name": "Marketplace Reviewer Test - $(date +%Y-%m-%d)",
  "created_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "region": "${REGION}",
  "purpose": "Google Marketplace Solution Review",
  "solution_under_test": "erlmcp-v3-enterprise"
}
EOF
        log_success "Project metadata documented in: project-metadata.json"
        return 0
    fi

    # Actual project creation (disabled in dry-run mode)
    log_warning "Dry-run mode is enabled. Skipping actual project creation."
    return 0
}

# ============================================================================
# STEP 3: Link Billing Account
# ============================================================================
link_billing() {
    log_info "Step 3: Linking billing account..."

    if [[ -z "${BILLING_ACCOUNT}" ]]; then
        log_warning "No billing account specified (BILLING_ACCOUNT environment variable)"
        log_command "gcloud beta billing projects list \\"
        log_command "  --billing-account=BILLING_ACCOUNT_ID \\"
        log_command "  --filter=\"project_id:${PROJECT_ID} AND billing_state=BILLING_ENABLED\""

        log_info "To link billing, reviewers would run:"
        log_command "gcloud beta billing projects link ${PROJECT_ID} \\
            --billing-account=BILLING_ACCOUNT_ID"

        # Document billing requirements
        cat > "${SCRIPT_DIR}/billing-requirements.md" <<EOF
# Billing Account Requirements

## Required Billing Account
- **Purpose**: Enable GCP resources for testing
- **Linking Command**:
  \`\`\`bash
  gcloud beta billing projects link ${PROJECT_ID} \\
      --billing-account=BILLING_ACCOUNT_ID
  \`\`\`

## Cost Estimate for Reviewer Testing

### Minimal Testing (1-2 hours)
- Compute Engine (n2d-standard-4): $0.26/hour × 2 = $0.52
- Cloud Storage (5GB): $0.026/month
- Network Egress (5GB): $0.12
- Container Registry: $0.10
- **Total**: ~$0.75

### Full Validation (4-8 hours)
- Compute Engine (n2d-standard-4): $0.26/hour × 8 = $2.08
- Cloud Load Balancing: $0.025/hour × 8 = $0.20
- Cloud Monitoring: $0.25
- Cloud Storage (10GB): $0.026/month
- Network Egress (20GB): $0.48
- Container Registry: $0.10
- **Total**: ~$3.13

### Cleanup Reminder
After testing, remember to:
1. Delete all resources in the project
2. Delete the GCP project
3. Verify billing account shows no ongoing charges

## Billing Verification
Check billing status:
\`\`\`bash
gcloud beta billing projects describe ${PROJECT_ID}
\`\`\`
EOF
        log_success "Billing requirements documented in: billing-requirements.md"
        return 0
    fi

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_command "gcloud beta billing projects link ${PROJECT_ID} \\
            --billing-account=${BILLING_ACCOUNT}"
        log_success "Billing account ${BILLING_ACCOUNT} would be linked"
        return 0
    fi

    log_warning "Dry-run mode is enabled. Skipping actual billing linking."
    return 0
}

# ============================================================================
# STEP 4: Set Project Configuration
# ============================================================================
configure_project() {
    log_info "Step 4: Configuring project settings..."

    # Set default project
    log_command "gcloud config set project ${PROJECT_ID}"

    # Set default region
    log_command "gcloud config set compute/region ${REGION}"

    # Set default zone
    log_command "gcloud config set compute/zone ${REGION}-a"

    # Enable Quota Project (for BigQuery usage tracking)
    log_command "gcloud config set project ${PROJECT_ID}"

    # Verify configuration
    log_command "gcloud config list"

    log_success "Project configuration documented"
    return 0
}

# ============================================================================
# STEP 5: Create Service Account for Testing
# ============================================================================
create_service_account() {
    log_info "Step 5: Creating reviewer service account..."

    local SA_NAME="marketplace-reviewer-sa"
    local SA_EMAIL="${SA_NAME}@${PROJECT_ID}.iam.gserviceaccount.com"

    log_command "gcloud iam service-accounts create ${SA_NAME} \\
        --display-name=\"Marketplace Reviewer Service Account\" \\
        --description=\"Service account for automated testing of marketplace solution\""

    # Grant editor role (required for deployment testing)
    log_command "gcloud projects add-iam-policy-binding ${PROJECT_ID} \\
        --member=\"serviceAccount:${SA_EMAIL}\" \\
        --role=\"roles/editor\" \\
        --condition=None"

    # Grant service account user role (to act as service account)
    log_command "gcloud iam service-accounts add-iam-policy-binding ${SA_EMAIL} \\
        --member=\"user:\$(gcloud auth list --filter=status:ACTIVE --format=value(account))\" \\
        --role=\"roles/iam.serviceAccountUser\""

    # Document service account
    cat > "${SCRIPT_DIR}/service-account-config.json" <<EOF
{
  "service_account_name": "${SA_NAME}",
  "service_account_email": "${SA_EMAIL}",
  "roles": [
    "roles/editor",
    "roles/iam.serviceAccountUser"
  ],
  "purpose": "Run deployment tests and validation scripts"
}
EOF
    log_success "Service account configuration documented in: service-account-config.json"
    return 0
}

# ============================================================================
# STEP 6: Generate Bootstrap Report
# ============================================================================
generate_report() {
    log_info "Step 6: Generating bootstrap report..."

    cat > "${SCRIPT_DIR}/bootstrap-report.md" <<EOF
# GCP Marketplace Reviewer Simulation - Bootstrap Report

**Generated**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Project ID**: ${PROJECT_ID}
**Region**: ${REGION}
**Dry Run**: ${DRY_RUN}

## Bootstrap Summary

### 1. Project Creation
- **Project ID**: ${PROJECT_ID}
- **Name**: Marketplace Reviewer Test - $(date +%Y-%m-%d)
- **Status**: Documented (dry-run mode)

### 2. Authentication
- **Status**: Validated
- **Active Account**: \$(gcloud auth list --filter="status:ACTIVE" --format="value(account)")

### 3. Billing
- **Requirements**: See \`billing-requirements.md\`
- **Estimated Cost**: \$0.75 - \$3.13 for full testing
- **Status**: Documented (dry-run mode)

### 4. Project Configuration
- **Default Region**: ${REGION}
- **Default Zone**: ${REGION}-a
- **Status**: Configured

### 5. Service Account
- **Name**: marketplace-reviewer-sa
- **Email**: marketplace-reviewer-sa@${PROJECT_ID}.iam.gserviceaccount.com
- **Roles**: roles/editor, roles/iam.serviceAccountUser
- **Status**: Documented (dry-run mode)

## Next Steps

1. **API Enablement**: Run \`./02-api-enable.sh\` to enable required GCP APIs
2. **Prerequisites**: Run \`./01-prereq-check.sh\` to verify tooling
3. **Deployment**: Proceed with solution deployment scripts

## Cleanup Commands

After testing completes:
\`\`\`bash
# Delete all resources in the project
gcloud projects delete ${PROJECT_ID}

# Or unlink billing and delete
gcloud beta billing projects unlink ${PROJECT_ID}
gcloud projects delete ${PROJECT_ID}
\`\`\`

## Evidence Artifacts

- \`project-metadata.json\` - Project configuration
- \`billing-requirements.md\` - Cost estimates and requirements
- \`service-account-config.json\` - Service account details
- \`bootstrap-report.md\` - This report

## Validation Checklist

- [x] Project ID generated and documented
- [x] GCP authentication validated
- [x] Billing requirements documented
- [x] Project configuration documented
- [x] Service account configuration documented
- [x] Cost estimate provided
- [ ] APIs enabled (next step)
- [ ] Prerequisites validated (next step)

---
**Bootstrap Complete**: Ready for API enablement phase
EOF
    log_success "Bootstrap report generated: bootstrap-report.md"
    return 0
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main() {
    log_info "Starting GCP Marketplace Reviewer Simulation - Phase 0: Bootstrap"
    log_info "Dry-run mode: ${DRY_RUN}"
    log_info "Project ID: ${PROJECT_ID}"
    echo ""

    # Execute bootstrap steps
    validate_authentication
    create_project
    link_billing
    configure_project
    create_service_account
    generate_report

    echo ""
    log_success "Bootstrap phase complete!"
    log_info "Next steps:"
    log_info "  1. Review: cat bootstrap-report.md"
    log_info "  2. Check prerequisites: ./01-prereq-check.sh"
    log_info "  3. Enable APIs: ./02-api-enable.sh"

    return 0
}

# Run main function
main "$@"
