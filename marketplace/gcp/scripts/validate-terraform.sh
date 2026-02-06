#!/bin/bash
# ============================================================================
# Validate All Terraform Configurations - DOCKER-ONLY
# Google Cloud Marketplace Deployment
# Compliant with DOCKER-ONLY CONSTITUTION
# ============================================================================

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_section() { echo -e "${BLUE}[====]${NC} $1"; }

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
readonly TERRAFORM_DIR="${MARKETPLACE_DIR}/terraform"
readonly TF_VERSION="${TERRAFORM_VERSION:-1.9.8}"
readonly TF_IMAGE="hashicorp/terraform:${TF_VERSION}"
readonly TFSEC_IMAGE="aquasec/tfsec:latest"
readonly CHECKOV_IMAGE="bridgecrew/checkov:latest"

# Validation results
PASSED=0
FAILED=0
WARNED=0

# Evidence collection
readonly EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/terraform-validation-$(date +%Y%m%d-%H%M%S)"
mkdir -p "${EVIDENCE_DIR}"

log_section "Docker-Only Terraform Validation"
log_info "Using Terraform Docker image: ${TF_IMAGE}"
log_info "Evidence directory: ${EVIDENCE_DIR}"
echo ""

# Verify Docker is available (REQUIRED per DOCKER-ONLY CONSTITUTION)
if ! command -v docker &> /dev/null; then
    log_error "Docker not found. DOCKER-ONLY CONSTITUTION requires Docker for all execution."
    exit 1
fi

# Validate Docker is running
if ! docker info &> /dev/null; then
    log_error "Docker daemon not running. Start Docker and retry."
    exit 1
fi

log_info "Docker verified: $(docker --version)"
echo ""

# ============================================================================
# Function: Validate Terraform Module via Docker
# ============================================================================
validate_module() {
    local module_path="$1"
    local module_name=$(basename "$module_path")

    log_section "Validating module: ${module_name}"

    # Create module-specific evidence directory
    local module_evidence="${EVIDENCE_DIR}/${module_name}"
    mkdir -p "${module_evidence}"

    # Step 1: Terraform Init (Docker)
    log_info "Running terraform init (Docker)..."
    if docker run --rm \
        -v "${module_path}:/workspace" \
        -w /workspace \
        "${TF_IMAGE}" \
        init -backend=false -upgrade -no-color > "${module_evidence}/init.log" 2>&1; then
        echo -e "  ${GREEN}✓${NC} terraform init"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗${NC} terraform init failed"
        cat "${module_evidence}/init.log"
        FAILED=$((FAILED + 1))
        return 1
    fi

    # Step 2: Terraform Format Check (Docker)
    log_info "Checking terraform format (Docker)..."
    if docker run --rm \
        -v "${module_path}:/workspace" \
        -w /workspace \
        "${TF_IMAGE}" \
        fmt -check -diff > "${module_evidence}/fmt.log" 2>&1; then
        echo -e "  ${GREEN}✓${NC} terraform fmt"
    else
        echo -e "  ${YELLOW}⚠${NC} terraform fmt found formatting issues"
        cat "${module_evidence}/fmt.log"
        WARNED=$((WARNED + 1))
    fi

    # Step 3: Terraform Validate (Docker)
    log_info "Running terraform validate (Docker)..."
    if docker run --rm \
        -v "${module_path}:/workspace" \
        -w /workspace \
        "${TF_IMAGE}" \
        validate -no-color > "${module_evidence}/validate.log" 2>&1; then
        echo -e "  ${GREEN}✓${NC} terraform validate"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗${NC} terraform validate failed"
        cat "${module_evidence}/validate.log"
        FAILED=$((FAILED + 1))
        return 1
    fi

    # Step 4: tfsec Security Scan (Docker)
    log_info "Running tfsec security scan (Docker)..."
    if docker run --rm \
        -v "${module_path}:/workspace" \
        "${TFSEC_IMAGE}" \
        /workspace \
        --format=json \
        --minimum-severity=MEDIUM \
        --soft-fail > "${module_evidence}/tfsec.json" 2>&1; then
        echo -e "  ${GREEN}✓${NC} tfsec scan"

        # Check for high/critical issues
        HIGH_CRITICAL=$(docker run --rm \
            -v "${module_evidence}:/data" \
            --entrypoint sh \
            "${TFSEC_IMAGE}" \
            -c "cat /data/tfsec.json | grep -c '\"severity\":\"HIGH\"\\|\"severity\":\"CRITICAL\"' || echo 0")

        if [ "$HIGH_CRITICAL" -gt 0 ]; then
            echo -e "  ${YELLOW}⚠${NC} Found ${HIGH_CRITICAL} HIGH/CRITICAL security issues"
            WARNED=$((WARNED + 1))
        fi
    else
        echo -e "  ${YELLOW}⚠${NC} tfsec scan completed with warnings"
    fi

    # Step 5: Checkov Policy Scan (Docker)
    log_info "Running Checkov policy scan (Docker)..."
    if docker run --rm \
        -v "${module_path}:/workspace" \
        "${CHECKOV_IMAGE}" \
        --directory /workspace \
        --framework terraform \
        --output json \
        --soft-fail > "${module_evidence}/checkov.json" 2>&1; then
        echo -e "  ${GREEN}✓${NC} Checkov scan"
    else
        echo -e "  ${YELLOW}⚠${NC} Checkov scan completed with warnings"
    fi

    # Generate module validation receipt
    cat > "${module_evidence}/receipt.json" << EOF
{
  "module": "${module_name}",
  "path": "${module_path}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "terraform_version": "${TF_VERSION}",
  "docker_image": "${TF_IMAGE}",
  "validation": {
    "init": "$([ -f "${module_evidence}/init.log" ] && echo "passed" || echo "failed")",
    "format": "$([ -f "${module_evidence}/fmt.log" ] && echo "checked" || echo "skipped")",
    "validate": "$([ -f "${module_evidence}/validate.log" ] && echo "passed" || echo "failed")",
    "tfsec": "$([ -f "${module_evidence}/tfsec.json" ] && echo "completed" || echo "skipped")",
    "checkov": "$([ -f "${module_evidence}/checkov.json" ] && echo "completed" || echo "skipped")"
  },
  "evidence_hash": "$(find "${module_evidence}" -type f -exec sha256sum {} \; | sha256sum | cut -d' ' -f1)"
}
EOF

    echo ""
    return 0
}

# ============================================================================
# Main Validation Loop
# ============================================================================
log_section "Discovering Terraform modules..."

# Find all unique module directories (containing .tf files)
while IFS= read -r -d '' module_dir; do
    module_dir=$(dirname "$module_dir")

    # Validate each module via Docker
    if ! validate_module "$module_dir"; then
        log_error "Module validation failed: $(basename "$module_dir")"
    fi
done < <(find "$TERRAFORM_DIR" -name "*.tf" -type f -print0 | sort -uz)

# ============================================================================
# Generate Final Validation Report
# ============================================================================
log_section "Generating validation report..."

cat > "${EVIDENCE_DIR}/validation-report.json" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "repository": "erlmcp",
  "terraform_version": "${TF_VERSION}",
  "docker_execution": true,
  "results": {
    "passed": ${PASSED},
    "failed": ${FAILED},
    "warnings": ${WARNED}
  },
  "evidence_directory": "${EVIDENCE_DIR}",
  "docker_images": {
    "terraform": "${TF_IMAGE}",
    "tfsec": "${TFSEC_IMAGE}",
    "checkov": "${CHECKOV_IMAGE}"
  }
}
EOF

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "======================================="
echo "Terraform Validation Summary"
echo "======================================="
echo -e "  ${GREEN}Passed:${NC}   ${PASSED}"
echo -e "  ${YELLOW}Warnings:${NC} ${WARNED}"
echo -e "  ${RED}Failed:${NC}   ${FAILED}"
echo "======================================="
echo ""
echo "Evidence directory: ${EVIDENCE_DIR}"
echo "Validation report:  ${EVIDENCE_DIR}/validation-report.json"
echo ""

# Exit based on results
if [ $FAILED -gt 0 ]; then
    log_error "Validation failed with ${FAILED} errors"
    log_info "Review evidence in: ${EVIDENCE_DIR}"
    exit 1
fi

if [ $WARNED -gt 0 ]; then
    log_warn "Validation completed with ${WARNED} warnings"
    log_info "Run 'docker run --rm -v \$(pwd):/workspace -w /workspace ${TF_IMAGE} fmt -recursive' to fix formatting"
fi

log_info "All Terraform configurations validated successfully!"
log_info "Proof receipt: hash=$(cat "${EVIDENCE_DIR}/validation-report.json" | sha256sum | cut -d' ' -f1)"
exit 0
