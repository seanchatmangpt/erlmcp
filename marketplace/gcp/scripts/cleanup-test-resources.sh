#!/bin/bash
# ============================================================================
# Test Resource Cleanup Script
# Removes all test resources created during deployment testing
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
ZONE="${ZONE:-us-central1-a}"
DRY_RUN="${DRY_RUN:-false}"

# Test resource prefix
TEST_PREFIX="test-"
TEST_ALT_PREFIX="erlmcp-test-"

# Counters
DELETED=0
SKIPPED=0
ERRORS=0

# ============================================================================
# Cleanup Functions
# =============================================================================

cleanup_cloud_run_services() {
    log_info "Cleaning up Cloud Run services..."

    local services=$(gcloud run services list \
        --project="$PROJECT_ID" \
        --region="$REGION" \
        --filter="name:$TEST_PREFIX OR name:$TEST_ALT_PREFIX" \
        --format="value(name)" 2>/dev/null || echo "")

    if [ -z "$services" ]; then
        log_info "  No test Cloud Run services found"
        return
    fi

    while read -r service; do
        if [ -n "$service" ]; then
            log_info "  Deleting Cloud Run service: $service"
            if [ "$DRY_RUN" = "false" ]; then
                if gcloud run services delete "$service" \
                    --project="$PROJECT_ID" \
                    --region="$REGION" \
                    --quiet 2>/dev/null; then
                    log_info "    ✓ Deleted"
                    DELETED=$((DELETED + 1))
                else
                    log_error "    ✗ Failed to delete"
                    ERRORS=$((ERRORS + 1))
                fi
            else
                log_info "    [DRY RUN] Would delete"
                DELETED=$((DELETED + 1))
            fi
        fi
    done <<< "$services"
}

cleanup_gke_clusters() {
    log_info "Cleaning up GKE clusters..."

    local clusters=$(gcloud container clusters list \
        --project="$PROJECT_ID" \
        --region="$REGION" \
        --filter="name:$TEST_PREFIX OR name:$TEST_ALT_PREFIX" \
        --format="value(name)" 2>/dev/null || echo "")

    if [ -z "$clusters" ]; then
        log_info "  No test GKE clusters found"
        return
    fi

    while read -r cluster; do
        if [ -n "$cluster" ]; then
            log_info "  Deleting GKE cluster: $cluster"

            # Get node pools to delete first
            local node_pools=$(gcloud container node-pools list \
                --cluster="$cluster" \
                --region="$REGION" \
                --project="$PROJECT_ID" \
                --format="value(name)" 2>/dev/null || echo "")

            if [ "$DRY_RUN" = "false" ]; then
                # Delete node pools first (faster)
                while read -r pool; do
                    if [ -n "$pool" ]; then
                        gcloud container node-pools delete "$pool" \
                            --cluster="$cluster" \
                            --region="$REGION" \
                            --project="$PROJECT_ID" \
                            --quiet 2>/dev/null || true
                    fi
                done <<< "$node_pools"

                # Delete cluster
                if gcloud container clusters delete "$cluster" \
                    --region="$REGION" \
                    --project="$PROJECT_ID" \
                    --quiet 2>/dev/null; then
                    log_info "    ✓ Deleted"
                    DELETED=$((DELETED + 1))
                else
                    log_error "    ✗ Failed to delete"
                    ERRORS=$((ERRORS + 1))
                fi
            else
                log_info "    [DRY RUN] Would delete"
                DELETED=$((DELETED + 1))
            fi
        fi
    done <<< "$clusters"
}

cleanup_compute_instances() {
    log_info "Cleaning up Compute Engine instances..."

    local instances=$(gcloud compute instances list \
        --project="$PROJECT_ID" \
        --filter="name:$TEST_PREFIX OR name:$TEST_ALT_PREFIX" \
        --format="value(name,zone)" 2>/dev/null || echo "")

    if [ -z "$instances" ]; then
        log_info "  No test Compute Engine instances found"
        return
    fi

    while read -r instance zone; do
        if [ -n "$instance" ]; then
            log_info "  Deleting Compute Engine instance: $instance ($zone)"
            if [ "$DRY_RUN" = "false" ]; then
                if gcloud compute instances delete "$instance" \
                    --zone="$zone" \
                    --project="$PROJECT_ID" \
                    --quiet 2>/dev/null; then
                    log_info "    ✓ Deleted"
                    DELETED=$((DELETED + 1))
                else
                    log_error "    ✗ Failed to delete"
                    ERRORS=$((ERRORS + 1))
                fi
            else
                log_info "    [DRY RUN] Would delete"
                DELETED=$((DELETED + 1))
            fi
        fi
    done <<< "$instances"
}

cleanup_firestore_databases() {
    log_info "Cleaning up Firestore databases..."

    local databases=$(gcloud firestore databases list \
        --project="$PROJECT_ID" \
        --format="value(name)" 2>/dev/null | grep -E "$TEST_PREFIX|$TEST_ALT_PREFIX" || echo "")

    if [ -z "$databases" ]; then
        log_info "  No test Firestore databases found"
        return
    fi

    while read -r db; do
        if [ -n "$db" ]; then
            log_info "  Deleting Firestore database: $db"
            if [ "$DRY_RUN" = "false" ]; then
                # Firestore deletion requires special handling
                log_warn "    Firestore deletion requires manual cleanup"
                SKIPPED=$((SKIPPED + 1))
            else
                log_info "    [DRY RUN] Would delete"
            fi
        fi
    done <<< "$databases"
}

cleanup_images() {
    log_info "Cleaning up test container images..."

    local images=$(gcloud artifacts images list \
        --project="$PROJECT_ID" \
        --repository=erlmcp-test \
        --location="$REGION" \
        --format="value(name)" 2>/dev/null || echo "")

    if [ -z "$images" ]; then
        log_info "  No test container images found"
        return
    fi

    while read -r image; do
        if [ -n "$image" ]; then
            log_info "  Deleting container image: $image"
            if [ "$DRY_RUN" = "false" ]; then
                if gcloud artifacts images delete "$image" \
                    --project="$PROJECT_ID" \
                    --quiet 2>/dev/null; then
                    log_info "    ✓ Deleted"
                    DELETED=$((DELETED + 1))
                else
                    log_error "    ✗ Failed to delete"
                    ERRORS=$((ERRORS + 1))
                fi
            else
                log_info "    [DRY RUN] Would delete"
                DELETED=$((DELETED + 1))
            fi
        fi
    done <<< "$images"
}

# ============================================================================
# Main
# ============================================================================

main() {
    echo "=========================================="
    echo "Test Resource Cleanup"
    echo "=========================================="
    echo "Project:  $PROJECT_ID"
    echo "Region:   $REGION"
    echo "Zone:     $ZONE"
    echo "Dry Run:  $DRY_RUN"
    echo "=========================================="
    echo ""

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    # Run cleanup functions
    cleanup_cloud_run_services
    cleanup_gke_clusters
    cleanup_compute_instances
    cleanup_firestore_databases
    cleanup_images

    # Summary
    echo ""
    echo "=========================================="
    echo "Cleanup Summary"
    echo "=========================================="
    log_info "Deleted:  $DELETED"
    log_info "Skipped:  $SKIPPED"
    log_info "Errors:   $ERRORS"
    echo "=========================================="

    if [ $ERRORS -gt 0 ]; then
        log_warn "Some resources failed to clean up. Check logs above."
    fi
}

# ============================================================================
# Script Entry Point
# ============================================================================

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
        --zone)
            ZONE="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT_ID] [--region REGION] [--zone ZONE] [--dry-run]"
            exit 1
            ;;
    esac
done

main "$@"
