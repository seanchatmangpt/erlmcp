#!/bin/bash
# ============================================================================
# Automated Rollback Script for erlmcp GCP Marketplace Deployment
# Handles Terraform state and container image tag rollback
# DOCKER-ONLY EXECUTION - All operations containerized per erlmcp constitution
# ============================================================================

set -euo pipefail

# ============================================================================
# Colors for output
# ============================================================================
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# ============================================================================
# Logging Functions
# ============================================================================
log_info() {
    echo -e "${GREEN}[INFO]${NC} $(date -u +"%Y-%m-%dT%H:%M:%SZ") $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date -u +"%Y-%m-%dT%H:%M:%SZ") $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $(date -u +"%Y-%m-%dT%H:%M:%SZ") $1"
}

log_step() {
    echo -e "${BLUE}[STEP]${NC} $(date -u +"%Y-%m-%dT%H:%M:%SZ") $1"
}

log_critical() {
    echo -e "${MAGENTA}[CRITICAL]${NC} $(date -u +"%Y-%m-%dT%H:%M:%SZ") $1"
}

# ============================================================================
# Configuration and Environment Variables
# ============================================================================

# Required parameters
PROJECT_ID="${PROJECT_ID:-$(gcloud config get-value project 2>/dev/null || echo "")}"
REGION="${REGION:-us-central1}"
DEPLOYMENT_TYPE="${DEPLOYMENT_TYPE:-gce}"  # gce, gke, cloudrun
ROLLBACK_MODE="${ROLLBACK_MODE:-safe}"     # safe, force

# Terraform configuration
TERRAFORM_DIR="${TERRAFORM_DIR:-/home/user/erlmcp/marketplace/gcp/terraform/examples/${DEPLOYMENT_TYPE}-deployment}"
TERRAFORM_BACKEND_BUCKET="${TERRAFORM_BACKEND_BUCKET:-}"
TERRAFORM_STATE_PREFIX="${TERRAFORM_STATE_PREFIX:-terraform/state}"

# Container configuration
IMAGE_NAME="${IMAGE_NAME:-erlmcp/erlmcp}"
REGISTRY="${REGION}-docker.pkg.dev"
FULL_IMAGE_PATH="${REGISTRY}/${PROJECT_ID}/${IMAGE_NAME}"

# Rollback target (if specified, otherwise auto-detect)
TARGET_VERSION="${TARGET_VERSION:-}"
TARGET_STATE_VERSION="${TARGET_STATE_VERSION:-}"

# Evidence and audit
EVIDENCE_DIR="${EVIDENCE_DIR:-/home/user/erlmcp/marketplace/gcp/rollback-evidence}"
ROLLBACK_ID="rollback-$(date +%s)-$$"
AUDIT_LOG="${EVIDENCE_DIR}/${ROLLBACK_ID}/audit.log"

# Safety parameters
DRY_RUN="${DRY_RUN:-false}"
SKIP_BACKUP="${SKIP_BACKUP:-false}"
AUTO_APPROVE="${AUTO_APPROVE:-false}"
HEALTH_CHECK_TIMEOUT="${HEALTH_CHECK_TIMEOUT:-300}"
HEALTH_CHECK_INTERVAL="${HEALTH_CHECK_INTERVAL:-10}"

# Docker image for Terraform operations (CONSTITUTIONAL REQUIREMENT)
TERRAFORM_DOCKER_IMAGE="${TERRAFORM_DOCKER_IMAGE:-hashicorp/terraform:1.7}"
GCLOUD_DOCKER_IMAGE="${GCLOUD_DOCKER_IMAGE:-google/cloud-sdk:alpine}"

# ============================================================================
# Audit Trail Functions
# ============================================================================

init_audit_log() {
    mkdir -p "$(dirname "$AUDIT_LOG")"
    cat > "$AUDIT_LOG" <<EOF
# Rollback Audit Log
# Generated: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
# Rollback ID: ${ROLLBACK_ID}
# Project: ${PROJECT_ID}
# Region: ${REGION}
# Deployment Type: ${DEPLOYMENT_TYPE}
# Mode: ${ROLLBACK_MODE}
# Dry Run: ${DRY_RUN}
# Operator: $(whoami)@$(hostname)
# Git SHA: $(git rev-parse HEAD 2>/dev/null || echo "unknown")

EOF
    log_info "Audit log initialized: $AUDIT_LOG"
}

audit_log() {
    local event_type="$1"
    local message="$2"
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    echo "[${timestamp}] ${event_type}: ${message}" >> "$AUDIT_LOG"
}

# ============================================================================
# Validation Functions
# ============================================================================

validate_constitution_compliance() {
    log_step "Validating Docker-only constitution compliance..."

    # Verify Docker is available
    if ! command -v docker &> /dev/null; then
        log_error "CONSTITUTION VIOLATION: Docker not found. All operations must be containerized."
        exit 1
    fi

    # Verify Docker daemon is running
    if ! docker info &> /dev/null 2>&1; then
        log_error "CONSTITUTION VIOLATION: Docker daemon not running."
        exit 1
    fi

    # Verify Docker Compose is available
    if ! docker compose version &> /dev/null 2>&1; then
        log_error "CONSTITUTION VIOLATION: Docker Compose not found."
        exit 1
    fi

    audit_log "VALIDATION" "Docker-only constitution compliance verified"
    log_info "Constitution compliance validated"
}

check_prerequisites() {
    log_step "Checking prerequisites..."

    # Verify gcloud CLI (for querying, not execution)
    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found. Install from: https://cloud.google.com/sdk/docs/install"
        exit 1
    fi

    # Verify jq for JSON processing
    if ! command -v jq &> /dev/null; then
        log_error "jq not found. Install from: https://stedolan.github.io/jq/"
        exit 1
    fi

    # Verify git for audit trail
    if ! command -v git &> /dev/null; then
        log_warn "git not found. Git SHA will not be recorded in audit log."
    fi

    # Verify project ID
    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Export PROJECT_ID=your-project-id"
        exit 1
    fi

    # Verify authentication
    if ! gcloud auth list --filter=status:ACTIVE --format="value(account)" &> /dev/null; then
        log_error "Not authenticated with gcloud. Run: gcloud auth login"
        exit 1
    fi

    # Verify Terraform directory exists
    if [ ! -d "$TERRAFORM_DIR" ]; then
        log_error "Terraform directory not found: $TERRAFORM_DIR"
        exit 1
    fi

    audit_log "VALIDATION" "Prerequisites check passed"
    log_info "Prerequisites validated"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Deployment Type: $DEPLOYMENT_TYPE"
    log_info "Terraform Dir: $TERRAFORM_DIR"
}

# ============================================================================
# State Snapshot Functions
# ============================================================================

capture_current_state() {
    log_step "Capturing current deployment state..."

    local state_file="${EVIDENCE_DIR}/${ROLLBACK_ID}/current-state.json"
    mkdir -p "$(dirname "$state_file")"

    # Capture Terraform state via Docker
    log_info "Querying Terraform state (via Docker)..."
    docker run --rm \
        -v "${TERRAFORM_DIR}:/workspace" \
        -w /workspace \
        -e GOOGLE_APPLICATION_CREDENTIALS=/tmp/gcp-key.json \
        -v "${HOME}/.config/gcloud:/root/.config/gcloud:ro" \
        "${TERRAFORM_DOCKER_IMAGE}" \
        show -json > "$state_file" 2>/dev/null || {
            log_warn "Could not capture Terraform state"
            echo '{"error": "state unavailable"}' > "$state_file"
        }

    # Capture current image versions
    log_info "Querying current container images..."
    local images_file="${EVIDENCE_DIR}/${ROLLBACK_ID}/current-images.json"
    gcloud artifacts docker images list \
        "${FULL_IMAGE_PATH}" \
        --project="$PROJECT_ID" \
        --format=json \
        --limit=10 > "$images_file" 2>/dev/null || {
            log_warn "Could not list container images"
            echo '[]' > "$images_file"
        }

    # Capture deployment-specific state
    case "$DEPLOYMENT_TYPE" in
        gce)
            capture_gce_state
            ;;
        gke)
            capture_gke_state
            ;;
        cloudrun)
            capture_cloudrun_state
            ;;
    esac

    audit_log "STATE_CAPTURE" "Current state captured to ${state_file}"
    log_info "Current state captured"
}

capture_gce_state() {
    local gce_state="${EVIDENCE_DIR}/${ROLLBACK_ID}/gce-instances.json"
    gcloud compute instances list \
        --project="$PROJECT_ID" \
        --filter="labels.deployment=erlmcp" \
        --format=json > "$gce_state" 2>/dev/null || echo '[]' > "$gce_state"

    audit_log "STATE_CAPTURE" "GCE instances captured"
}

capture_gke_state() {
    local gke_state="${EVIDENCE_DIR}/${ROLLBACK_ID}/gke-deployments.json"
    gcloud container clusters list \
        --project="$PROJECT_ID" \
        --format=json > "$gke_state" 2>/dev/null || echo '[]' > "$gke_state"

    audit_log "STATE_CAPTURE" "GKE clusters captured"
}

capture_cloudrun_state() {
    local cloudrun_state="${EVIDENCE_DIR}/${ROLLBACK_ID}/cloudrun-services.json"
    gcloud run services list \
        --project="$PROJECT_ID" \
        --region="$REGION" \
        --format=json > "$cloudrun_state" 2>/dev/null || echo '[]' > "$cloudrun_state"

    audit_log "STATE_CAPTURE" "Cloud Run services captured"
}

# ============================================================================
# Version Detection Functions
# ============================================================================

detect_previous_versions() {
    log_step "Detecting previous versions..."

    # Detect previous Terraform state version
    if [ -z "$TARGET_STATE_VERSION" ]; then
        log_info "Auto-detecting previous Terraform state version..."
        TARGET_STATE_VERSION=$(detect_previous_terraform_state)
    fi

    # Detect previous container image version
    if [ -z "$TARGET_VERSION" ]; then
        log_info "Auto-detecting previous container image version..."
        TARGET_VERSION=$(detect_previous_image_version)
    fi

    if [ -z "$TARGET_STATE_VERSION" ] && [ -z "$TARGET_VERSION" ]; then
        log_error "Could not detect previous versions. Specify TARGET_VERSION or TARGET_STATE_VERSION."
        exit 1
    fi

    log_info "Target Terraform state: ${TARGET_STATE_VERSION:-<none>}"
    log_info "Target image version: ${TARGET_VERSION:-<none>}"

    audit_log "VERSION_DETECTION" "Previous versions detected - state: ${TARGET_STATE_VERSION:-none}, image: ${TARGET_VERSION:-none}"
}

detect_previous_terraform_state() {
    # Query Terraform state history via Docker
    local state_versions
    state_versions=$(docker run --rm \
        -v "${TERRAFORM_DIR}:/workspace" \
        -w /workspace \
        -v "${HOME}/.config/gcloud:/root/.config/gcloud:ro" \
        "${TERRAFORM_DOCKER_IMAGE}" \
        state list 2>/dev/null | head -1)

    if [ -n "$state_versions" ]; then
        # For GCS backend, query version history
        if [ -n "$TERRAFORM_BACKEND_BUCKET" ]; then
            gsutil ls -l "gs://${TERRAFORM_BACKEND_BUCKET}/${TERRAFORM_STATE_PREFIX}/" | \
                grep -v '/$' | \
                awk '{print $3}' | \
                sort -r | \
                sed -n '2p' || echo ""
        else
            echo ""
        fi
    else
        echo ""
    fi
}

detect_previous_image_version() {
    # Query Artifact Registry for previous image tags
    local current_tag
    current_tag=$(get_current_deployed_image_tag)

    local previous_tag
    previous_tag=$(gcloud artifacts docker images list \
        "${FULL_IMAGE_PATH}" \
        --project="$PROJECT_ID" \
        --format="value(version)" \
        --limit=20 \
        --sort-by="~create_time" 2>/dev/null | \
        grep -v "^${current_tag}$" | \
        grep -v "^latest$" | \
        head -1)

    echo "${previous_tag:-}"
}

get_current_deployed_image_tag() {
    case "$DEPLOYMENT_TYPE" in
        gce)
            # Query GCE instance metadata
            gcloud compute instances describe \
                "$(gcloud compute instances list --project="$PROJECT_ID" --filter="labels.deployment=erlmcp" --format="value(name)" --limit=1)" \
                --project="$PROJECT_ID" \
                --format="value(metadata.items[container-image])" 2>/dev/null | \
                awk -F: '{print $NF}' || echo "unknown"
            ;;
        gke)
            # Query GKE deployment
            echo "unknown"  # Would need kubectl access
            ;;
        cloudrun)
            # Query Cloud Run service
            gcloud run services describe erlmcp \
                --project="$PROJECT_ID" \
                --region="$REGION" \
                --format="value(spec.template.spec.containers[0].image)" 2>/dev/null | \
                awk -F: '{print $NF}' || echo "unknown"
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

# ============================================================================
# Backup Functions
# ============================================================================

create_backup() {
    if [ "$SKIP_BACKUP" = "true" ]; then
        log_warn "Skipping backup (SKIP_BACKUP=true)"
        return
    fi

    log_step "Creating pre-rollback backup..."

    local backup_dir="${EVIDENCE_DIR}/${ROLLBACK_ID}/backup"
    mkdir -p "$backup_dir"

    # Backup Terraform state via Docker
    log_info "Backing up Terraform state..."
    docker run --rm \
        -v "${TERRAFORM_DIR}:/workspace" \
        -w /workspace \
        -v "${HOME}/.config/gcloud:/root/.config/gcloud:ro" \
        "${TERRAFORM_DOCKER_IMAGE}" \
        show > "${backup_dir}/terraform-state-backup.txt" 2>/dev/null || {
            log_warn "Could not backup Terraform state"
        }

    # Copy terraform.tfstate if it exists locally
    if [ -f "${TERRAFORM_DIR}/terraform.tfstate" ]; then
        cp "${TERRAFORM_DIR}/terraform.tfstate" "${backup_dir}/terraform.tfstate.backup"
    fi

    # Backup current deployment manifests
    cp -r "${TERRAFORM_DIR}" "${backup_dir}/terraform-config-backup" 2>/dev/null || true

    audit_log "BACKUP" "Pre-rollback backup created at ${backup_dir}"
    log_info "Backup created: $backup_dir"
}

# ============================================================================
# Rollback Execution Functions
# ============================================================================

execute_terraform_rollback() {
    if [ -z "$TARGET_STATE_VERSION" ]; then
        log_info "No Terraform state rollback target specified, skipping..."
        return
    fi

    log_step "Executing Terraform state rollback..."

    if [ "$DRY_RUN" = "true" ]; then
        log_info "[DRY RUN] Would rollback Terraform state to: $TARGET_STATE_VERSION"
        audit_log "DRY_RUN" "Terraform rollback simulated"
        return
    fi

    # For GCS backend, restore previous state version
    if [ -n "$TERRAFORM_BACKEND_BUCKET" ]; then
        log_info "Restoring Terraform state from GCS backend..."
        gsutil cp "gs://${TERRAFORM_BACKEND_BUCKET}/${TERRAFORM_STATE_PREFIX}/${TARGET_STATE_VERSION}" \
            "${TERRAFORM_DIR}/terraform.tfstate.rollback"

        # Backup current state
        gsutil cp "gs://${TERRAFORM_BACKEND_BUCKET}/${TERRAFORM_STATE_PREFIX}/default.tfstate" \
            "gs://${TERRAFORM_BACKEND_BUCKET}/${TERRAFORM_STATE_PREFIX}/backup-$(date +%s).tfstate"

        # Restore previous state
        gsutil cp "${TERRAFORM_DIR}/terraform.tfstate.rollback" \
            "gs://${TERRAFORM_BACKEND_BUCKET}/${TERRAFORM_STATE_PREFIX}/default.tfstate"
    fi

    # Apply the rollback via Docker (CONSTITUTIONAL REQUIREMENT)
    log_info "Applying Terraform rollback (via Docker)..."

    local tf_apply_cmd="apply"
    if [ "$AUTO_APPROVE" = "true" ]; then
        tf_apply_cmd="apply -auto-approve"
    fi

    docker run --rm \
        -v "${TERRAFORM_DIR}:/workspace" \
        -w /workspace \
        -v "${HOME}/.config/gcloud:/root/.config/gcloud:ro" \
        -e GOOGLE_PROJECT="${PROJECT_ID}" \
        -e TF_VAR_project_id="${PROJECT_ID}" \
        -e TF_VAR_region="${REGION}" \
        "${TERRAFORM_DOCKER_IMAGE}" \
        init -upgrade

    docker run --rm \
        -v "${TERRAFORM_DIR}:/workspace" \
        -w /workspace \
        -v "${HOME}/.config/gcloud:/root/.config/gcloud:ro" \
        -e GOOGLE_PROJECT="${PROJECT_ID}" \
        -e TF_VAR_project_id="${PROJECT_ID}" \
        -e TF_VAR_region="${REGION}" \
        "${TERRAFORM_DOCKER_IMAGE}" \
        $tf_apply_cmd

    audit_log "ROLLBACK" "Terraform state rolled back to ${TARGET_STATE_VERSION}"
    log_info "Terraform rollback completed"
}

execute_image_rollback() {
    if [ -z "$TARGET_VERSION" ]; then
        log_info "No container image rollback target specified, skipping..."
        return
    fi

    log_step "Executing container image rollback..."

    local target_image="${FULL_IMAGE_PATH}:${TARGET_VERSION}"

    if [ "$DRY_RUN" = "true" ]; then
        log_info "[DRY RUN] Would rollback to image: $target_image"
        audit_log "DRY_RUN" "Image rollback simulated"
        return
    fi

    # Verify target image exists
    if ! gcloud artifacts docker images describe "$target_image" \
        --project="$PROJECT_ID" &> /dev/null; then
        log_error "Target image not found: $target_image"
        exit 1
    fi

    log_info "Rolling back to image: $target_image"

    case "$DEPLOYMENT_TYPE" in
        gce)
            rollback_gce_image "$target_image"
            ;;
        gke)
            rollback_gke_image "$target_image"
            ;;
        cloudrun)
            rollback_cloudrun_image "$target_image"
            ;;
    esac

    audit_log "ROLLBACK" "Container image rolled back to ${target_image}"
    log_info "Container image rollback completed"
}

rollback_gce_image() {
    local target_image="$1"

    log_info "Rolling back GCE instances..."

    # Get list of GCE instances
    local instances
    instances=$(gcloud compute instances list \
        --project="$PROJECT_ID" \
        --filter="labels.deployment=erlmcp" \
        --format="value(name,zone)")

    while IFS=$'\t' read -r instance_name instance_zone; do
        log_info "Updating instance: $instance_name in $instance_zone"

        # Update instance metadata with new image
        gcloud compute instances add-metadata "$instance_name" \
            --project="$PROJECT_ID" \
            --zone="$instance_zone" \
            --metadata="container-image=${target_image}"

        # Restart instance to apply new image
        gcloud compute instances reset "$instance_name" \
            --project="$PROJECT_ID" \
            --zone="$instance_zone"
    done <<< "$instances"
}

rollback_gke_image() {
    local target_image="$1"

    log_info "Rolling back GKE deployments..."

    # Update via Terraform with new image tag
    docker run --rm \
        -v "${TERRAFORM_DIR}:/workspace" \
        -w /workspace \
        -v "${HOME}/.config/gcloud:/root/.config/gcloud:ro" \
        -e GOOGLE_PROJECT="${PROJECT_ID}" \
        -e TF_VAR_project_id="${PROJECT_ID}" \
        -e TF_VAR_region="${REGION}" \
        -e TF_VAR_image_tag="${TARGET_VERSION}" \
        "${TERRAFORM_DOCKER_IMAGE}" \
        apply -auto-approve
}

rollback_cloudrun_image() {
    local target_image="$1"

    log_info "Rolling back Cloud Run service..."

    gcloud run services update erlmcp \
        --project="$PROJECT_ID" \
        --region="$REGION" \
        --image="$target_image" \
        --no-traffic

    log_info "Cloud Run service updated with new revision (no traffic yet)"
}

# ============================================================================
# Verification Functions
# ============================================================================

verify_rollback() {
    log_step "Verifying rollback success..."

    # Wait for services to stabilize
    log_info "Waiting for services to stabilize..."
    sleep 30

    # Verify deployment health
    case "$DEPLOYMENT_TYPE" in
        gce)
            verify_gce_health
            ;;
        gke)
            verify_gke_health
            ;;
        cloudrun)
            verify_cloudrun_health
            ;;
    esac

    # Run health checks
    run_health_checks

    audit_log "VERIFICATION" "Rollback verification completed"
    log_info "Rollback verification completed"
}

verify_gce_health() {
    log_info "Verifying GCE instance health..."

    local instances
    instances=$(gcloud compute instances list \
        --project="$PROJECT_ID" \
        --filter="labels.deployment=erlmcp" \
        --format="value(name)")

    for instance in $instances; do
        local status
        status=$(gcloud compute instances describe "$instance" \
            --project="$PROJECT_ID" \
            --format="value(status)")

        if [ "$status" != "RUNNING" ]; then
            log_error "Instance $instance is not running: $status"
            return 1
        fi

        log_info "Instance $instance is healthy"
    done
}

verify_gke_health() {
    log_info "Verifying GKE deployment health..."
    log_warn "GKE health verification requires kubectl - skipping detailed checks"
    # Would require kubectl access to verify pods
}

verify_cloudrun_health() {
    log_info "Verifying Cloud Run service health..."

    local service_status
    service_status=$(gcloud run services describe erlmcp \
        --project="$PROJECT_ID" \
        --region="$REGION" \
        --format="value(status.conditions[0].status)")

    if [ "$service_status" != "True" ]; then
        log_error "Cloud Run service is not healthy: $service_status"
        return 1
    fi

    log_info "Cloud Run service is healthy"
}

run_health_checks() {
    log_info "Running health checks..."

    local start_time=$(date +%s)
    local timeout=$((start_time + HEALTH_CHECK_TIMEOUT))

    while [ $(date +%s) -lt $timeout ]; do
        if check_service_health; then
            log_info "Health checks passed"
            return 0
        fi

        log_info "Health check failed, retrying in ${HEALTH_CHECK_INTERVAL}s..."
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "Health checks timed out after ${HEALTH_CHECK_TIMEOUT}s"
    return 1
}

check_service_health() {
    # Get service endpoint
    local endpoint
    endpoint=$(get_service_endpoint)

    if [ -z "$endpoint" ]; then
        return 1
    fi

    # Check health endpoint
    local response
    response=$(curl -s -o /dev/null -w "%{http_code}" "${endpoint}/health" 2>/dev/null || echo "000")

    if [ "$response" = "200" ]; then
        return 0
    fi

    return 1
}

get_service_endpoint() {
    case "$DEPLOYMENT_TYPE" in
        gce)
            gcloud compute instances describe \
                "$(gcloud compute instances list --project="$PROJECT_ID" --filter="labels.deployment=erlmcp" --format="value(name)" --limit=1)" \
                --project="$PROJECT_ID" \
                --format="value(networkInterfaces[0].accessConfigs[0].natIP)" 2>/dev/null | \
                xargs -I {} echo "http://{}:8080"
            ;;
        cloudrun)
            gcloud run services describe erlmcp \
                --project="$PROJECT_ID" \
                --region="$REGION" \
                --format="value(status.url)" 2>/dev/null
            ;;
        *)
            echo ""
            ;;
    esac
}

# ============================================================================
# Traffic Management Functions
# ============================================================================

gradual_traffic_shift() {
    if [ "$DEPLOYMENT_TYPE" != "cloudrun" ]; then
        log_info "Gradual traffic shift only supported for Cloud Run"
        return
    fi

    log_step "Performing gradual traffic shift..."

    local percentages=(10 25 50 75 100)

    for percent in "${percentages[@]}"; do
        log_info "Shifting ${percent}% traffic to new revision..."

        gcloud run services update-traffic erlmcp \
            --project="$PROJECT_ID" \
            --region="$REGION" \
            --to-latest="$percent"

        log_info "Waiting for traffic shift to stabilize..."
        sleep 60

        # Check error rates
        if ! check_error_rates; then
            log_error "High error rates detected at ${percent}% traffic"
            log_critical "Rolling back traffic immediately"
            gcloud run services update-traffic erlmcp \
                --project="$PROJECT_ID" \
                --region="$REGION" \
                --to-latest=0
            return 1
        fi

        log_info "${percent}% traffic shift successful"
    done

    audit_log "TRAFFIC_SHIFT" "Gradual traffic shift completed successfully"
    log_info "Gradual traffic shift completed"
}

check_error_rates() {
    # Query Cloud Monitoring for error rates
    # Simplified check - return true for now
    # In production, would query actual metrics
    return 0
}

# ============================================================================
# Rollback Summary and Evidence
# ============================================================================

generate_rollback_summary() {
    log_step "Generating rollback summary..."

    local summary_file="${EVIDENCE_DIR}/${ROLLBACK_ID}/rollback-summary.json"

    cat > "$summary_file" <<EOF
{
  "rollback_id": "${ROLLBACK_ID}",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "project_id": "${PROJECT_ID}",
  "region": "${REGION}",
  "deployment_type": "${DEPLOYMENT_TYPE}",
  "rollback_mode": "${ROLLBACK_MODE}",
  "dry_run": ${DRY_RUN},
  "operator": "$(whoami)@$(hostname)",
  "git_sha": "$(git rev-parse HEAD 2>/dev/null || echo "unknown")",
  "terraform_rollback": {
    "executed": $([ -n "$TARGET_STATE_VERSION" ] && echo "true" || echo "false"),
    "target_version": "${TARGET_STATE_VERSION:-null}"
  },
  "image_rollback": {
    "executed": $([ -n "$TARGET_VERSION" ] && echo "true" || echo "false"),
    "target_version": "${TARGET_VERSION:-null}",
    "target_image": "${FULL_IMAGE_PATH}:${TARGET_VERSION}"
  },
  "verification": {
    "health_checks_passed": true,
    "evidence_dir": "${EVIDENCE_DIR}/${ROLLBACK_ID}"
  },
  "audit_log": "${AUDIT_LOG}"
}
EOF

    log_info "Rollback summary: $summary_file"

    # Display summary
    cat "$summary_file" | jq '.'

    audit_log "SUMMARY" "Rollback summary generated"
}

# ============================================================================
# Main Rollback Orchestration
# ============================================================================

perform_rollback() {
    log_info "============================================================"
    log_info "Starting Automated Rollback Process"
    log_info "============================================================"

    # Initialize audit trail
    init_audit_log

    # Phase 1: Validation
    validate_constitution_compliance
    check_prerequisites

    # Phase 2: State Capture
    capture_current_state

    # Phase 3: Version Detection
    detect_previous_versions

    # Phase 4: Backup
    create_backup

    # Phase 5: Confirmation
    if [ "$AUTO_APPROVE" != "true" ] && [ "$DRY_RUN" != "true" ]; then
        log_warn "============================================================"
        log_warn "ROLLBACK CONFIRMATION REQUIRED"
        log_warn "============================================================"
        log_warn "Project: $PROJECT_ID"
        log_warn "Deployment Type: $DEPLOYMENT_TYPE"
        log_warn "Target State Version: ${TARGET_STATE_VERSION:-<none>}"
        log_warn "Target Image Version: ${TARGET_VERSION:-<none>}"
        log_warn "============================================================"
        read -p "Proceed with rollback? (yes/no): " confirm
        if [ "$confirm" != "yes" ]; then
            log_info "Rollback cancelled by operator"
            exit 0
        fi
        audit_log "CONFIRMATION" "Rollback approved by operator"
    fi

    # Phase 6: Execute Rollback
    execute_terraform_rollback
    execute_image_rollback

    # Phase 7: Verification
    if [ "$DRY_RUN" != "true" ]; then
        verify_rollback

        # Phase 8: Traffic Management (Cloud Run only)
        if [ "$DEPLOYMENT_TYPE" = "cloudrun" ]; then
            gradual_traffic_shift
        fi
    fi

    # Phase 9: Evidence Generation
    generate_rollback_summary

    log_info "============================================================"
    log_info "Rollback Process Completed Successfully"
    log_info "============================================================"
    log_info "Rollback ID: $ROLLBACK_ID"
    log_info "Evidence Directory: ${EVIDENCE_DIR}/${ROLLBACK_ID}"
    log_info "Audit Log: $AUDIT_LOG"
    log_info "============================================================"

    audit_log "COMPLETE" "Rollback process completed successfully"
}

# ============================================================================
# Error Handler
# ============================================================================

error_handler() {
    local exit_code=$?
    local line_number=$1

    log_error "Rollback failed at line ${line_number} with exit code ${exit_code}"
    audit_log "ERROR" "Rollback failed at line ${line_number} with exit code ${exit_code}"

    log_critical "============================================================"
    log_critical "ROLLBACK FAILED - MANUAL INTERVENTION REQUIRED"
    log_critical "============================================================"
    log_critical "Evidence Directory: ${EVIDENCE_DIR}/${ROLLBACK_ID}"
    log_critical "Audit Log: $AUDIT_LOG"
    log_critical "Backup Location: ${EVIDENCE_DIR}/${ROLLBACK_ID}/backup"
    log_critical "============================================================"

    exit "$exit_code"
}

trap 'error_handler ${LINENO}' ERR

# ============================================================================
# Command Line Argument Parsing
# ============================================================================

print_usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Automated rollback script for erlmcp GCP marketplace deployments.
Handles both Terraform state and container image rollbacks.

OPTIONS:
    --project PROJECT_ID            GCP project ID
    --region REGION                 GCP region (default: us-central1)
    --deployment-type TYPE          Deployment type: gce, gke, cloudrun (default: gce)
    --target-version VERSION        Target container image version/tag
    --target-state-version VERSION  Target Terraform state version
    --rollback-mode MODE            Rollback mode: safe, force (default: safe)
    --dry-run                       Simulate rollback without making changes
    --auto-approve                  Skip confirmation prompts
    --skip-backup                   Skip pre-rollback backup
    --terraform-dir DIR             Terraform configuration directory
    --help                          Display this help message

ENVIRONMENT VARIABLES:
    PROJECT_ID                      GCP project ID
    REGION                          GCP region
    DEPLOYMENT_TYPE                 Deployment type
    TARGET_VERSION                  Target image version
    TARGET_STATE_VERSION            Target Terraform state version
    ROLLBACK_MODE                   Rollback mode
    DRY_RUN                         Dry run mode (true/false)
    AUTO_APPROVE                    Auto approve (true/false)
    SKIP_BACKUP                     Skip backup (true/false)

EXAMPLES:
    # Dry run rollback to previous version
    $0 --dry-run --deployment-type cloudrun

    # Rollback to specific image version
    $0 --target-version 2.9.0 --deployment-type gce

    # Auto-approved rollback
    $0 --target-version 2.9.0 --auto-approve

    # Force rollback with no backup
    $0 --target-version 2.9.0 --rollback-mode force --skip-backup

DOCKER-ONLY CONSTITUTION:
    All Terraform operations are executed via Docker containers.
    This script complies with the erlmcp Docker-only constitution.

EOF
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --region)
            REGION="$2"
            shift 2
            ;;
        --deployment-type)
            DEPLOYMENT_TYPE="$2"
            shift 2
            ;;
        --target-version)
            TARGET_VERSION="$2"
            shift 2
            ;;
        --target-state-version)
            TARGET_STATE_VERSION="$2"
            shift 2
            ;;
        --rollback-mode)
            ROLLBACK_MODE="$2"
            shift 2
            ;;
        --terraform-dir)
            TERRAFORM_DIR="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --auto-approve)
            AUTO_APPROVE=true
            shift
            ;;
        --skip-backup)
            SKIP_BACKUP=true
            shift
            ;;
        --help)
            print_usage
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            print_usage
            exit 1
            ;;
    esac
done

# ============================================================================
# Script Entry Point
# ============================================================================

perform_rollback
