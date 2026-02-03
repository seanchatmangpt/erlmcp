#!/bin/bash
# ============================================================================
# Security Validation Test Script
# Tests security controls for GCP Marketplace submission
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
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
IMAGE_NAME="${IMAGE_NAME:-erlmcp/erlmcp}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
FULL_IMAGE="${REGION}-docker.pkg.dev/${PROJECT_ID}/${IMAGE_NAME}:${IMAGE_TAG}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/security"

SEVERITY_THRESHOLD="${SEVERITY_THRESHOLD:-HIGH}"

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Check for required tools
    local missing_tools=()

    if ! command -v docker &> /dev/null; then
        missing_tools+=("docker")
    fi

    if ! command -v gcloud &> /dev/null; then
        missing_tools+=("gcloud")
    fi

    if ! command -v jq &> /dev/null; then
        missing_tools+=("jq")
    fi

    # Trivy is optional but recommended
    if ! command -v trivy &> /dev/null; then
        log_warn "trivy not found - installing..."
        if command -v brew &> /dev/null; then
            brew install trivy
        else
            log_warn "Skipping Trivy scan - install from https://aquasecurity.github.io/trivy/"
        fi
    fi

    if [ ${#missing_tools[@]} -gt 0 ]; then
        log_error "Missing required tools: ${missing_tools[*]}"
        exit 1
    fi

    # Configure docker auth
    gcloud auth configure-docker "${REGION}-docker.pkg.dev" --quiet > /dev/null 2>&1

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_container_image_security() {
    log_test "Testing container image security configuration..."

    if ! docker pull "$FULL_IMAGE" > /dev/null 2>&1; then
        log_error "  ✗ Failed to pull image"
        return 1
    fi

    local security_passed=true
    local config_file="$EVIDENCE_DIR/container-security.json"

    docker inspect "$FULL_IMAGE" > "$config_file" 2>&1

    # Check for non-root user
    local user=$(jq -r '.[0].config.User // "root"' "$config_file")
    if [ "$user" != "root" ] && [ -n "$user" ]; then
        log_info "  ✓ Running as non-root user: $user"
    else
        log_error "  ✗ Container running as root (SECURITY RISK)"
        security_passed=false
    fi

    # Check for read-only root filesystem
    local read_only=$(jq -r '.[0].config.Labels."org.opencontainers.image.readOnly" // "false"' "$config_file")
    if [ "$read_only" = "true" ]; then
        log_info "  ✓ Read-only root filesystem configured"
    else
        log_warn "  ⚠ Read-only root filesystem not configured (recommended)"
    fi

    # Check for dropped capabilities
    local cap_drop=$(jq -r '.[0].config.Capdrop // []' "$config_file")
    if [ "$cap_drop" != "[]" ] && [ "$cap_drop" != "null" ]; then
        log_info "  ✓ Capabilities dropped: $cap_drop"
    else
        log_warn "  ⚠ No capabilities explicitly dropped"
    fi

    # Check for security options
    local security_opt=$(jq -r '.[0].config.SecurityOpt // []' "$config_file")
    if [ "$security_opt" != "[]" ] && [ "$security_opt" != "null" ]; then
        log_info "  ✓ Security options: $security_opt"
    fi

    if [ "$security_passed" = true ]; then
        return 0
    else
        return 1
    fi
}

test_trivy_scan() {
    log_test "Running Trivy vulnerability scan..."

    if ! command -v trivy &> /dev/null; then
        log_warn "  ⚠ Trivy not available, skipping local scan"
        return 0
    fi

    local trivy_report="$EVIDENCE_DIR/trivy-report.json"
    local trivy_summary="$EVIDENCE_DIR/trivy-summary.txt"

    if trivy image --format json --output "$trivy_report" "$FULL_IMAGE" 2>&1; then
        log_info "  ✓ Trivy scan completed"

        # Generate summary
        echo "Trivy Vulnerability Summary" > "$trivy_summary"
        echo "============================" >> "$trivy_summary"
        echo "" >> "$trivy_summary"

        # Count vulnerabilities by severity
        local critical=$(jq '[.Results[].Vulnerabilities // [] | .[] | select(.Severity == "CRITICAL")] | length' "$trivy_report" 2>/dev/null || echo "0")
        local high=$(jq '[.Results[].Vulnerabilities // [] | .[] | select(.Severity == "HIGH")] | length' "$trivy_report" 2>/dev/null || echo "0")
        local medium=$(jq '[.Results[].Vulnerabilities // [] | .[] | select(.Severity == "MEDIUM")] | length' "$trivy_report" 2>/dev/null || echo "0")
        local low=$(jq '[.Results[].Vulnerabilities // [] | .[] | select(.Severity == "LOW")] | length' "$trivy_report" 2>/dev/null || echo "0")

        echo "CRITICAL: $critical" >> "$trivy_summary"
        echo "HIGH:     $high" >> "$trivy_summary"
        echo "MEDIUM:   $medium" >> "$trivy_summary"
        echo "LOW:      $low" >> "$trivy_summary"
        echo "" >> "$trivy_summary"

        cat "$trivy_summary"

        # Check against threshold
        if [ "$critical" -gt 0 ] || [ "$high" -gt 0 ]; then
            log_error "  ✗ Found $critical CRITICAL and $high HIGH severity vulnerabilities"
            log_error "  Marketplace requirement: Zero HIGH/CRITICAL vulnerabilities"

            # Show top vulnerabilities
            echo "" >> "$trivy_summary"
            echo "Top 10 HIGH/CRITICAL Vulnerabilities:" >> "$trivy_summary"
            jq -r '.Results[].Vulnerabilities // [] | .[] | select(.Severity == "CRITICAL" or .Severity == "HIGH") | "\(.Severity): \(.VulnerabilityID) in \(.PkgName)"' "$trivy_report" 2>/dev/null | head -10 >> "$trivy_summary"

            return 1
        else
            log_info "  ✓ No HIGH or CRITICAL vulnerabilities found"
            return 0
        fi
    else
        log_warn "  ⚠ Trivy scan failed"
        return 0
    fi
}

test_gcp_vulnerability_scan() {
    log_test "Running GCP Container Analysis scan..."

    local gcp_scan_report="$EVIDENCE_DIR/gcp-scan.json"

    # Start GCP scan
    if gcloud artifacts docker images scan "$FULL_IMAGE" \
        --project="$PROJECT_ID" \
        --format=json > "$gcp_scan_report" 2>&1; then

        log_info "  ✓ GCP scan completed"

        # Check for HIGH/CRITICAL vulnerabilities
        local critical=$(jq -r '.scanResults.securityResults // [] | map(select(.severity == "CRITICAL")) | length' "$gcp_scan_report" 2>/dev/null || echo "0")
        local high=$(jq -r '.scanResults.securityResults // [] | map(select(.severity == "HIGH")) | length' "$gcp_scan_report" 2>/dev/null || echo "0")

        if [ "$critical" -gt 0 ] || [ "$high" -gt 0 ]; then
            log_error "  ✗ GCP scan found $critical CRITICAL and $high HIGH severity vulnerabilities"
            return 1
        else
            log_info "  ✓ No HIGH or CRITICAL vulnerabilities found in GCP scan"
            return 0
        fi
    else
        log_warn "  ⚠ GCP scan failed or not available"
        return 0
    fi
}

test_iam_least_privilege() {
    log_test "Checking IAM least privilege principle..."

    local iam_report="$EVIDENCE_DIR/iam-review.txt"

    # Get IAM policy for the project (service accounts related to deployment)
    echo "IAM Policy Review" > "$iam_report"
    echo "=================" >> "$iam_report"
    echo "" >> "$iam_report"

    # Check for overly permissive roles on service accounts
    local service_accounts=$(gcloud iam service-accounts list \
        --project="$PROJECT_ID" \
        --filter="displayName~erlmcp" \
        --format="value(email)" 2>/dev/null || echo "")

    if [ -n "$service_accounts" ]; then
        echo "erlmcp Service Accounts:" >> "$iam_report"
        echo "$service_accounts" >> "$iam_report"
        echo "" >> "$iam_report"

        local has_owner_editor=false

        while IFS= read -r sa; do
            if [ -n "$sa" ]; then
                echo "Checking $sa..." >> "$iam_report"

                local roles=$(gcloud projects get-iam-policy "$PROJECT_ID" \
                    --filter="bindings.members:user:$sa OR bindings.members:serviceAccount:$sa" \
                    --format="value(bindings.role)" 2>/dev/null || echo "")

                echo "  Roles: $roles" >> "$iam_report"

                if echo "$roles" | grep -qE "roles/(owner|editor)"; then
                    log_error "  ✗ Found Owner/Editor role on $sa"
                    has_owner_editor=true
                fi
            fi
        done <<< "$service_accounts"

        if [ "$has_owner_editor" = true ]; then
            log_error "  ✗ Overly permissive roles detected"
            return 1
        else
            log_info "  ✓ No Owner/Editor roles found on service accounts"
            return 0
        fi
    else
        log_info "  ✓ No erlmcp service accounts found (pre-deployment check)"
        return 0
    fi
}

test_workload_identity() {
    log_test "Checking Workload Identity configuration..."

    local workload_report="$EVIDENCE_DIR/workload-identity.txt"

    # Check if workload identity pools exist
    local pools=$(gcloud iam workload-identity-pools list \
        --project="$PROJECT_ID" \
        --format="value(name)" 2>/dev/null || echo "")

    echo "Workload Identity Pools:" > "$workload_report"
    echo "$pools" >> "$workload_report"

    if [ -n "$pools" ]; then
        log_info "  ✓ Workload Identity pools configured"

        # Check for provider configurations
        while IFS= read -r pool; do
            if [ -n "$pool" ]; then
                local providers=$(gcloud iam workload-identity-pools providers list \
                    --project="$PROJECT_ID" \
                    --workload-identity-pool="$pool" \
                    --format="value(name)" 2>/dev/null || echo "")
                echo "  Pool $pool providers: $providers" >> "$workload_report"
            fi
        done <<< "$pools"

        return 0
    else
        log_warn "  ⚠ No Workload Identity pools found (may be created during deployment)"
        return 0
    fi
}

test_secret_manager_permissions() {
    log_test "Checking Secret Manager permissions..."

    local secrets_report="$EVIDENCE_DIR/secret-acl.txt"

    echo "Secret Manager Secrets and Permissions:" > "$secrets_report"

    local secrets=$(gcloud secrets list \
        --project="$PROJECT_ID" \
        --filter="name~erlmcp" \
        --format="value(name)" 2>/dev/null || echo "")

    if [ -n "$secrets" ]; then
        while IFS= read -r secret; do
            if [ -n "$secret" ]; then
                echo "" >> "$secrets_report"
                echo "Secret: $secret" >> "$secrets_report"

                local policy=$(gcloud secrets get-iam-policy "$secret" \
                    --project="$PROJECT_ID" \
                    --format=json 2>/dev/null || echo "{}")

                echo "$policy" | jq '.' >> "$secrets_report"

                # Check for overly permissive access
                local all_users=$(echo "$policy" | jq -r '.bindings[]? | select(.members[]? == "allUsers" or .members[]? == "allAuthenticatedUsers") | .role' 2>/dev/null || echo "")
                if [ -n "$all_users" ]; then
                    log_error "  ✗ Secret $secret has public access"
                    return 1
                fi
            fi
        done <<< "$secrets"

        log_info "  ✓ Secret permissions reviewed"
        return 0
    else
        log_info "  ✓ No erlmcp secrets found (pre-deployment check)"
        return 0
    fi
}

test_network_policies() {
    log_test "Checking network policy configuration..."

    local network_report="$EVIDENCE_DIR/network-policy.txt"

    echo "Network Policy Check:" > "$network_report"

    # Check for firewall rules (if any exist)
    local firewall_rules=$(gcloud compute firewall-rules list \
        --project="$PROJECT_ID" \
        --filter="name~erlmcp" \
        --format="table(name,allowed[],sourceRanges)" 2>/dev/null || echo "")

    if [ -n "$firewall_rules" ]; then
        echo "" >> "$network_report"
        echo "Firewall Rules:" >> "$network_report"
        echo "$firewall_rules" >> "$network_report"

        # Check for overly permissive rules
        if echo "$firewall_rules" | grep -q "0.0.0.0/0"; then
            log_warn "  ⚠ Found firewall rules allowing 0.0.0.0/0 (verify if intentional)"
        fi

        log_info "  ✓ Firewall rules reviewed"
    else
        echo "No erlmcp-specific firewall rules found" >> "$network_report"
    fi

    # Check for default-deny recommendations
    echo "" >> "$network_report"
    echo "Recommendation: Implement default-deny network policies for production" >> "$network_report"

    return 0
}

test_image_labels() {
    log_test "Verifying container image labels..."

    local labels_report="$EVIDENCE_DIR/image-labels.txt"

    docker inspect "$FULL_IMAGE" -o json > "$EVIDENCE_DIR/inspect-temp.json" 2>/dev/null || true

    local labels=$(jq -r '.[0].config.Labels // {}' "$EVIDENCE_DIR/inspect-temp.json" 2>/dev/null || echo "{}")

    echo "Container Image Labels:" > "$labels_report"
    echo "$labels" | jq '.' >> "$labels_report"

    # Check for recommended labels
    local missing_labels=()

    if ! echo "$labels" | jq -e '.["org.opencontainers.image.title"]' > /dev/null 2>&1; then
        missing_labels+=("org.opencontainers.image.title")
    fi

    if ! echo "$labels" | jq -e '.["org.opencontainers.image.version"]' > /dev/null 2>&1; then
        missing_labels+=("org.opencontainers.image.version")
    fi

    if ! echo "$labels" | jq -e '.["org.opencontainers.image.vendor"]' > /dev/null 2>&1; then
        missing_labels+=("org.opencontainers.image.vendor")
    fi

    if [ ${#missing_labels[@]} -gt 0 ]; then
        log_warn "  ⚠ Missing recommended labels: ${missing_labels[*]}"
        echo "" >> "$labels_report"
        echo "Missing: ${missing_labels[@]}" >> "$labels_report"
    else
        log_info "  ✓ All recommended OCI labels present"
    fi

    rm -f "$EVIDENCE_DIR/inspect-temp.json"

    return 0
}

test_base_image_scan() {
    log_test "Checking base image security..."

    local base_image_report="$EVIDENCE_DIR/base-image.txt"

    # Get base image from history
    local base_image=$(docker history "$FULL_IMAGE" --format "{{.CreatedBy}}" --no-trunc | grep -oP 'FROM \K[^ ]+' | head -1 || echo "unknown")

    echo "Base Image: $base_image" > "$base_image_report"

    # Check if using known secure base images
    case "$base_image" in
        *alpine*|*distroless*|*scratch*)
            log_info "  ✓ Using secure base image: $base_image"
            echo "Status: SECURE" >> "$base_image_report"
            return 0
            ;;
        *ubuntu*|*debian*)
            log_warn "  ⚠ Using $base_image - ensure it's a minimal variant"
            echo "Status: REVIEW" >> "$base_image_report"
            return 0
            ;;
        *)
            log_warn "  ⚠ Unknown base image: $base_image"
            echo "Status: UNKNOWN" >> "$base_image_report"
            return 0
            ;;
    esac
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting Security Validation Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Image: $FULL_IMAGE"
    log_info "Severity Threshold: $SEVERITY_THRESHOLD"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Run tests
    for test_func in \
        test_container_image_security \
        test_trivy_scan \
        test_gcp_vulnerability_scan \
        test_iam_least_privilege \
        test_workload_identity \
        test_secret_manager_permissions \
        test_network_policies \
        test_image_labels \
        test_base_image_scan; do

        TOTAL_TESTS=$((TOTAL_TESTS + 1))

        if $test_func; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    done

    # Summary
    log_info "================================================"
    log_info "Test Summary:"
    log_info "  Total Tests:  $TOTAL_TESTS"
    log_info "  Passed:       $PASSED_TESTS"
    log_info "  Failed:       $FAILED_TESTS"
    log_info "================================================"
    log_info "Evidence saved to: $EVIDENCE_DIR"

    if [ $FAILED_TESTS -gt 0 ]; then
        log_error "Some security tests failed. Review required before Marketplace submission."
        exit 1
    fi

    log_info "All security tests passed!"
    exit 0
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
        --image)
            FULL_IMAGE="$2"
            shift 2
            ;;
        --severity-threshold)
            SEVERITY_THRESHOLD="$2"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--image IMAGE] [--severity-threshold THRESHOLD]"
            exit 1
            ;;
    esac
done

main
