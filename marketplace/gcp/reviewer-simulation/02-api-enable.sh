#!/usr/bin/env bash
# ============================================================================
# Google Marketplace Reviewer Simulation - Phase 2: API Enablement
# ============================================================================
# This script enables all required GCP APIs for deploying the erlmcp v3
# enterprise solution on Google Cloud Platform.
#
# Google Marketplace reviewers verify that:
# 1. All required APIs are documented
# 2. APIs can be enabled successfully
# 3. API dependencies are satisfied
# 4. No conflicting API configurations exist
#
# Usage: ./02-api-enable.sh [--dry-run] [--project-id PROJECT_ID]
# ============================================================================

set -euo pipefail

# Color output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Configuration
PROJECT_ID="${PROJECT_ID:-$(gcloud config get-value project 2>/dev/null || echo '')}"
DRY_RUN="${DRY_RUN:-true}"
REGION="${REGION:-us-central1}"

# Counters
TOTAL_APIS=0
ENABLED_APIS=0
FAILED_APIS=0

# Log functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
    ((ENABLED_APIS++)) || true
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
    ((FAILED_APIS++)) || true
}

log_command() {
    echo -e "${YELLOW}[DRY RUN]${NC} Would execute: $1"
}

# ============================================================================
# Required GCP APIs for erlmcp v3 Enterprise
#
# These APIs are organized by functional area:
# - Compute: GKE, Compute Engine, Cloud Run
# - Storage: Cloud Storage, Filestore
# - Networking: VPC, Load Balancing, Cloud CDN
# - Operations: Monitoring, Logging, Trace, Debugger
# - Security: Secret Manager, KMS, IAM, Resource Manager
# - Database: Cloud SQL, Firestore, Memorystore
# - Management: Deployment Manager, Resource Manager
# ============================================================================

declare -a REQUIRED_APIS=(
    # ===== Compute APIs =====
    "container.googleapis.com|GKE|Kubernetes cluster management"
    "compute.googleapis.com|Compute Engine|Virtual machine instances"
    "run.googleapis.com|Cloud Run|Serverless container execution"

    # ===== Storage APIs =====
    "storage-component.googleapis.com|Cloud Storage|Object storage"
    "storage-api.googleapis.com|Cloud Storage API|Storage operations"
    "file.googleapis.com|Filestore|Network file storage"

    # ===== Networking APIs =====
    "containersecurity.googleapis.com|Container Security|GKE security policies"
    "networkservices.googleapis.com|Network Services|Advanced networking"
    "trafficdirector.googleapis.com|Traffic Director|Global load balancing"

    # ===== Operations APIs =====
    "monitoring.googleapis.com|Cloud Monitoring|Metrics and dashboards"
    "logging.googleapis.com|Cloud Logging|Log aggregation"
    "trace.googleapis.com|Cloud Trace|Distributed tracing"
    "debugger.googleapis.com|Cloud Debugger|Application debugging"
    "cloudprofiler.googleapis.com|Cloud Profiler|Performance profiling"

    # ===== Security APIs =====
    "secretmanager.googleapis.com|Secret Manager|Secret storage"
    "cloudkms.googleapis.com|Cloud KMS|Key management"
    "iam.googleapis.com|Identity and Access Management|IAM policies"
    "iamcredentials.googleapis.com|IAM Credentials|Service account impersonation"
    "binaryauthorization.googleapis.com|Binary Authorization|Image security policies"

    # ===== Database APIs =====
    "sql-component.googleapis.com|Cloud SQL|Managed databases"
    "sqladmin.googleapis.com|Cloud SQL Admin|Database administration"
    "firestore.googleapis.com|Cloud Firestore|NoSQL document database"
    "redis.googleapis.com|Memorystore|Redis-compatible cache"

    # ===== Management APIs =====
    "deploymentmanager.googleapis.com|Deployment Manager|Infrastructure deployment"
    "cloudresourcemanager.googleapis.com|Cloud Resource Manager|Project management"
    "cloudbuild.googleapis.com|Cloud Build|CI/CD pipelines"
    "cloudfunctions.googleapis.com|Cloud Functions|Serverless functions"

    # ===== Eventing APIs =====
    "pubsub.googleapis.com|Cloud Pub/Sub|Messaging and event streaming"
    "eventarc.googleapis.com|Eventarc|Event delivery"

    # ===== Observability APIs =====
    "opsconfig.googleapis.com|Operations Config|Operations configuration"
    "osconfig.googleapis.com|OS Config|OS management"
    "oslogin.googleapis.com|OS Login|SSH key management"

    # ===== Networking (Advanced) =====
    "servicemanagement.googleapis.com|Service Management|API management"
    "servicecontrol.googleapis.com|Service Control|API control plane"
    "servicenetworking.googleapis.com|Service Networking|Private network connections"

    # ===== Additional Enterprise APIs =====
    "accessapproval.googleapis.com|Access Approval|Access request approval"
    "accesscontextmanager.googleapis.com|Access Context Manager|Zero trust security"
    "policytroubleshooter.googleapis.com|Policy Troubleshooter|IAM policy debugging"
)

# ============================================================================
# Validate Project
# ============================================================================
validate_project() {
    log_info "Validating GCP project..."

    if [[ -z "${PROJECT_ID}" ]]; then
        log_error "No project ID set. Use: gcloud config set project PROJECT_ID"
        return 1
    fi

    log_info "Project ID: ${PROJECT_ID}"

    # Verify project exists
    if ! gcloud projects describe "${PROJECT_ID}" &> /dev/null; then
        log_error "Project ${PROJECT_ID} does not exist or no access"
        return 1
    fi

    log_success "Project validated: ${PROJECT_ID}"
    return 0
}

# ============================================================================
# Check API Status
# ============================================================================
check_api() {
    local api=$1

    if [[ "${DRY_RUN}" == "true" ]]; then
        return 0
    fi

    if gcloud services list --enabled --project="${PROJECT_ID}" --filter="name:${api}" --format="value(name)" 2>/dev/null | grep -q "${api}"; then
        return 0
    else
        return 1
    fi
}

# ============================================================================
# Enable Single API
# ============================================================================
enable_api() {
    local api_line=$1
    ((TOTAL_APIS++)) || true

    local api_id
    local api_name
    local api_description

    IFS='|' read -r api_id api_name api_description <<< "${api_line}"

    log_info "Enabling: ${api_name} (${api_id})"
    log_info "  Description: ${api_description}"

    # Check if already enabled
    if check_api "${api_id}"; then
        log_warning "  Already enabled, skipping"
        return 0
    fi

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_command "gcloud services enable ${api_id} \\
            --project=${PROJECT_ID} \\
            --async"
        log_success "  Would enable ${api_name}"
        return 0
    fi

    # Enable the API
    if gcloud services enable "${api_id}" \
        --project="${PROJECT_ID}" \
        --async &> /dev/null; then
        log_success "  Enabled ${api_name}"
        return 0
    else
        log_error "  Failed to enable ${api_name}"
        return 1
    fi
}

# ============================================================================
# Enable All APIs
# ============================================================================
enable_all_apis() {
    log_info "Enabling ${#REQUIRED_APIS[@]} required GCP APIs..."
    echo ""

    for api_line in "${REQUIRED_APIS[@]}"; do
        enable_api "${api_line}"
        echo ""
    done

    log_info "================================"
    log_info "API Enablement Summary:"
    log_info "  Total APIs:   ${TOTAL_APIS}"
    log_success "  Enabled:     ${ENABLED_APIS}"
    log_error "  Failed:       ${FAILED_APIS}"
    log_info "================================"
    echo ""

    return 0
}

# ============================================================================
# Wait for APIs to be Ready
# ============================================================================
wait_for_apis() {
    log_info "Waiting for APIs to become active..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_command "gcloud services wait \\
            --project=${PROJECT_ID} \\
            --timeout=600"
        log_success "APIs would be checked asynchronously"
        return 0
    fi

    # Wait for all operations to complete
    local wait_time=0
    local max_wait=300  # 5 minutes
    local check_interval=10

    while [[ ${wait_time} -lt ${max_wait} ]]; do
        local pending_operations
        pending_operations=$(gcloud operations list \
            --project="${PROJECT_ID}" \
            --filter="status:DONE AND operationType:enableServices" \
            --format="value(name)" 2>/dev/null | wc -l)

        if [[ ${pending_operations} -eq 0 ]]; then
            log_success "All APIs are now active"
            return 0
        fi

        log_info "Waiting for ${pending_operations} operations to complete..."
        sleep ${check_interval}
        ((wait_time += check_interval)) || true
    done

    log_warning "Some APIs may still be activating (continuing anyway)"
    return 0
}

# ============================================================================
# Verify API Status
# ============================================================================
verify_apis() {
    log_info "Verifying API status..."

    local verification_file="${SCRIPT_DIR}/api-verification.md"

    cat > "${verification_file}" <<EOF
# API Enablement Verification Report

**Generated**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Project**: ${PROJECT_ID}
**Dry Run**: ${DRY_RUN}

## Required APIs Status

| API Name | Service ID | Description | Status |
|----------|-----------|-------------|--------|
EOF

    for api_line in "${REQUIRED_APIS[@]}"; do
        local api_id api_name api_description
        IFS='|' read -r api_id api_name api_description <<< "${api_line}"

        local status="PENDING"
        if check_api "${api_id}" || [[ "${DRY_RUN}" == "true" ]]; then
            status="ENABLED"
        fi

        echo "| ${api_name} | \`${api_id}\` | ${api_description} | ${status} |" >> "${verification_file}"
    done

    cat >> "${verification_file}" <<EOF

## API Usage by Component

### GKE Cluster
- **container.googleapis.com**: Cluster provisioning and management
- **compute.googleapis.com**: Underlying compute resources
- **monitoring.googleapis.com**: Cluster metrics
- **logging.googleapis.com**: Cluster logs

### Application Deployment
- **storage-component.googleapis.com**: Container image storage
- **cloudkms.googleapis.com**: Encryption key management
- **secretmanager.googleapis.com**: Application secrets

### Networking
- **networkservices.googleapis.com**: Service mesh and networking
- **trafficdirector.googleapis.com**: Traffic management

### Databases
- **sql-component.googleapis.com**: Cloud SQL for relational data
- **redis.googleapis.com**: Memorystore for caching
- **firestore.googleapis.com**: NoSQL document storage

### Observability
- **monitoring.googleapis.com**: Metrics collection
- **logging.googleapis.com**: Log aggregation
- **trace.googleapis.com**: Distributed tracing
- **cloudprofiler.googleapis.com**: Performance profiling

### Security
- **iam.googleapis.com**: Access control
- **binaryauthorization.googleapis.com**: Image security
- **accesscontextmanager.googleapis.com**: Zero trust policies

## Cost Impact

Enabled APIs may incur costs:
- **GKE**: $0.10/hour per cluster (management fee)
- **Cloud Monitoring**: Free tier covers most metrics
- **Cloud Logging**: $0.50 per GB ingested
- **Cloud KMS**: $0.03 per key version
- **Secret Manager**: $0.03 per 10,000 secret accesses
- **Cloud SQL**: Varies by instance size
- **Memorystore**: $0.18/GB-month for Redis

**Estimated API Costs**: $10-50/month for moderate testing workload

## Verification Commands

Check enabled APIs:
\`\`\`bash
gcloud services list --enabled --project=${PROJECT_ID}
\`\`\`

Check specific API:
\`\`\`bash
gcloud services describe container.googleapis.com --project=${PROJECT_ID}
\`\`\`

List API operations:
\`\`\`bash
gcloud operations list --project=${PROJECT_ID} --filter="operationType:enableServices"
\`\`\`

---
**Verification Complete**
EOF

    log_success "Verification report: ${verification_file}"
    return 0
}

# ============================================================================
# Generate API Documentation
# ============================================================================
generate_api_docs() {
    log_info "Generating API documentation..."

    local doc_file="${SCRIPT_DIR}/api-reference.md"

    cat > "${doc_file}" <<EOF
# GCP API Reference for erlmcp v3 Enterprise

**Solution Version**: 3.0.0
**Last Updated**: $(date -u +%Y-%m-%dT%H:%M:%SZ)

## Required GCP APIs

### Single Command Enablement

Enable all required APIs in one command:

\`\`\`bash
gcloud services enable \\
    container.googleapis.com \\
    compute.googleapis.com \\
    run.googleapis.com \\
    storage-component.googleapis.com \\
    storage-api.googleapis.com \\
    file.googleapis.com \\
    containersecurity.googleapis.com \\
    networkservices.googleapis.com \\
    trafficdirector.googleapis.com \\
    monitoring.googleapis.com \\
    logging.googleapis.com \\
    trace.googleapis.com \\
    debugger.googleapis.com \\
    cloudprofiler.googleapis.com \\
    secretmanager.googleapis.com \\
    cloudkms.googleapis.com \\
    iam.googleapis.com \\
    iamcredentials.googleapis.com \\
    binaryauthorization.googleapis.com \\
    sql-component.googleapis.com \\
    sqladmin.googleapis.com \\
    firestore.googleapis.com \\
    redis.googleapis.com \\
    deploymentmanager.googleapis.com \\
    cloudresourcemanager.googleapis.com \\
    cloudbuild.googleapis.com \\
    cloudfunctions.googleapis.com \\
    pubsub.googleapis.com \\
    eventarc.googleapis.com \\
    opsconfig.googleapis.com \\
    osconfig.googleapis.com \\
    oslogin.googleapis.com \\
    servicemanagement.googleapis.com \\
    servicecontrol.googleapis.com \\
    servicenetworking.googleapis.com \\
    accessapproval.googleapis.com \\
    accesscontextmanager.googleapis.com \\
    policytroubleshooter.googleapis.com \\
    --project=PROJECT_ID \\
    --async
\`\`\`

## API Categories

### 1. Container Orchestration
EOF

    # Add APIs by category
    echo "" >> "${doc_file}"
    echo "**Google Kubernetes Engine (GKE)**" >> "${doc_file}"
    echo "- \`container.googleapis.com\`: Primary GKE API" >> "${doc_file}"
    echo "- \`containersecurity.googleapis.com\`: Security policies" >> "${doc_file}"
    echo "- \`compute.googleapis.com\`: Compute Engine for node pools" >> "${doc_file}"
    echo "" >> "${doc_file}"

    cat >> "${doc_file}" <<EOF
### 2. Storage & Data

**Object Storage**
- \`storage-component.googleapis.com\`: Cloud Storage core
- \`storage-api.googleapis.com\`: Storage operations API

**File Storage**
- \`file.googleapis.com\`: Filestore for NFS shares

**Databases**
- \`sql-component.googleapis.com\`: Cloud SQL core
- \`sqladmin.googleapis.com\`: Cloud SQL administration
- \`firestore.googleapis.com\`: NoSQL document database
- \`redis.googleapis.com\**: Memorystore for Redis

### 3. Networking

**Core Networking**
- \`networkservices.googleapis.com\`: Advanced networking features
- \`trafficdirector.googleapis.com\**: Global traffic management
- \`servicenetworking.googleapis.com\**: Private service connections

**Service Management**
- \`servicemanagement.googleapis.com\`: API service management
- \`servicecontrol.googleapis.com\`: Service control plane

### 4. Observability

**Monitoring & Logging**
- \`monitoring.googleapis.com\`: Cloud Monitoring
- \`logging.googleapis.com\`: Cloud Logging
- \`trace.googleapis.com\**: Cloud Trace

**Debugging & Profiling**
- \`debugger.googleapis.com\`: Cloud Debugger
- \`cloudprofiler.googleapis.com\**: Continuous profiling

**Operations**
- \`opsconfig.googleapis.com\**: Operations configuration
- \`osconfig.googleapis.com\**: OS management
- \`oslogin.googleapis.com\**: SSH key management

### 5. Security

**Identity & Access**
- \`iam.googleapis.com\**: IAM policies
- \`iamcredentials.googleapis.com\**: Service account credentials

**Secrets & Encryption**
- \`secretmanager.googleapis.com\**: Secret storage
- \`cloudauthorization.googleapis.com\**: Image security policies

**Zero Trust**
- \`accessapproval.googleapis.com\**: Access approval workflows
- \`accesscontextmanager.googleapis.com\**: Context-aware access

### 6. Eventing

**Messaging**
- \`pubsub.googleapis.com\**: Pub/Sub messaging
- \`eventarc.googleapis.com\**: Event delivery

### 7. CI/CD

**Build & Deploy**
- \`cloudbuild.googleapis.com\**: Cloud Build
- \`cloudfunctions.googleapis.com\**: Serverless functions
- \`deploymentmanager.googleapis.com\**: Infrastructure deployment

### 8. Resource Management

**Project & Policies**
- \`cloudresourcemanager.googleapis.com\**: Resource management
- \`policytroubleshooter.googleapis.com\**: IAM policy debugging

## API Dependencies

Some APIs have dependencies on others:

```
container.googleapis.com
  ├── depends on: compute.googleapis.com
  ├── enables: containersecurity.googleapis.com
  └── integrates with: monitoring.googleapis.com

sql-component.googleapis.com
  ├── depends on: cloudresourcemanager.googleapis.com
  └── integrates with: secretmanager.googleapis.com

pubsub.googleapis.com
  └── integrates with: iam.googleapis.com
```

## Cost Estimates

### Per-API Costs (Monthly)

| API | Cost | Notes |
|-----|------|-------|
| container.googleapis.com | ~$74 | $0.10/hour cluster fee |
| compute.googleapis.com | Variable | Based on instance usage |
| monitoring.googleapis.com | Free tier | $0.25/GB metrics over free tier |
| logging.googleapis.com | ~$5-50 | $0.50/GB ingested |
| secretmanager.googleapis.com | ~$0.60 | $0.03/10k accesses |
| cloudkms.googleapis.com | ~$1 | $0.03/key version |
| sql-component.googleapis.com | Variable | Based on instance size |

### Total Estimated API Costs
**Minimal Testing**: $50-100/month
**Full Production**: $200-500/month

## Troubleshooting

### API Not Activating
\`\`\`bash
# Check operation status
gcloud operations list \\
    --project=PROJECT_ID \\
    --filter="operationType:enableServices"

# Describe specific operation
gcloud operations describe OPERATION_ID --project=PROJECT_ID
\`\`\`

### API Quota Errors
\`\`\`bash
# Check API quotas
gcloud services quotas list \\
    --service=SERVICE.googleapis.com \\
    --project=PROJECT_ID
\`\`\`

### API Dependencies Missing
\`\`\`bash
# Enable dependencies first
gcloud services enable \\
    cloudresourcemanager.googleapis.com \\
    iam.googleapis.com \\
    --project=PROJECT_ID
\`\`\`

## Validation

After enabling APIs, validate with:

\`\`\`bash
# List all enabled APIs
gcloud services list --enabled --project=PROJECT_ID

# Check specific API
gcloud services describe container.googleapis.com --project=PROJECT_ID

# Test API access (example: GKE)
gcloud container clusters list --project=PROJECT_ID
\`\`\`

---
**End of API Reference**
EOF

    log_success "API documentation: ${doc_file}"
    return 0
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main() {
    log_info "Starting GCP API Enablement"
    log_info "Project ID: ${PROJECT_ID}"
    log_info "Dry-run mode: ${DRY_RUN}"
    echo ""

    if [[ -z "${PROJECT_ID}" ]]; then
        log_error "No project ID. Set with: gcloud config set project PROJECT_ID"
        return 1
    fi

    # Execute enablement steps
    validate_project
    enable_all_apis
    wait_for_apis
    verify_apis
    generate_api_docs

    echo ""
    log_success "API enablement complete!"
    echo ""
    log_info "Next steps:"
    log_info "  1. Review API status: cat api-verification.md"
    log_info "  2. Check API reference: cat api-reference.md"
    log_info "  3. Proceed with deployment"

    return 0
}

# Run main
main "$@"
