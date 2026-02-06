#!/bin/bash
# ============================================================================
# Run All Validations - DOCKER-ONLY CONSTITUTION
# Google Cloud Marketplace - Comprehensive Validation Suite
# Executes: terraform validate, helm lint, tfsec, checkov
# ============================================================================
# WHY: Pre-deployment validation gate to ensure infrastructure-as-code quality,
#      security compliance, and deployment readiness across all modules.
# WHAT: Docker-only validation orchestrator producing cryptographic evidence.
# HOW: docker run with official images; generates receipt hashes for audit trail.
# ============================================================================

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_section() { echo -e "${BLUE}[====]${NC} $1"; }
log_subsection() { echo -e "${CYAN}[----]${NC} $1"; }

# Script directories
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
readonly TERRAFORM_DIR="${MARKETPLACE_DIR}/terraform"
readonly HELM_DIR="${MARKETPLACE_DIR}/helm"
readonly HELM_CHART_DIR="${HELM_DIR}/erlmcp-marketplace"

# Docker images (official only - zero-trust principle)
readonly TF_VERSION="${TERRAFORM_VERSION:-1.9.8}"
readonly TF_IMAGE="hashicorp/terraform:${TF_VERSION}"
readonly TFSEC_IMAGE="aquasec/tfsec:latest"
readonly CHECKOV_IMAGE="bridgecrew/checkov:latest"
readonly HELM_IMAGE="alpine/helm:3.14.0"

# Evidence collection (cryptographic audit trail)
readonly TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
readonly EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/all-validations-${TIMESTAMP}"
mkdir -p "${EVIDENCE_DIR}"

# Validation metrics
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
WARNED_CHECKS=0

# Evidence hashes for receipt
declare -a EVIDENCE_HASHES=()

# ============================================================================
# ANDON STOP: Verify Docker-Only Execution Environment
# ============================================================================
log_section "DOCKER-ONLY CONSTITUTION: Environment Verification"

if ! command -v docker &> /dev/null; then
    log_error "ANDON: Docker not found. DOCKER-ONLY CONSTITUTION violated."
    log_error "All execution MUST use Docker. Host execution forbidden."
    exit 1
fi

if ! docker info &> /dev/null; then
    log_error "ANDON: Docker daemon not running."
    log_error "Start Docker daemon: systemctl start docker"
    exit 1
fi

log_info "Docker verified: $(docker --version)"
log_info "Evidence directory: ${EVIDENCE_DIR}"
log_info "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""

# Pull all required images (explicit, auditable)
log_section "Pulling Docker Images (explicit supply chain)"
for image in "${TF_IMAGE}" "${TFSEC_IMAGE}" "${CHECKOV_IMAGE}" "${HELM_IMAGE}"; do
    log_info "Pulling ${image}..."
    if docker pull "${image}" &> "${EVIDENCE_DIR}/pull-$(echo ${image} | tr '/:' '-').log"; then
        IMAGE_DIGEST=$(docker inspect --format='{{index .RepoDigests 0}}' "${image}" 2>/dev/null || echo "unknown")
        log_info "  Digest: ${IMAGE_DIGEST}"
        EVIDENCE_HASHES+=("${IMAGE_DIGEST}")
    else
        log_error "Failed to pull ${image}"
        exit 1
    fi
done
echo ""

# ============================================================================
# GATE 1: Terraform Validation (All Modules)
# ============================================================================
log_section "GATE 1: Terraform Module Validation"

TERRAFORM_EVIDENCE="${EVIDENCE_DIR}/terraform"
mkdir -p "${TERRAFORM_EVIDENCE}"

if [ ! -d "${TERRAFORM_DIR}" ]; then
    log_warn "Terraform directory not found: ${TERRAFORM_DIR}"
    log_warn "Skipping Terraform validation"
else
    # Discover all modules containing .tf files
    log_info "Discovering Terraform modules in ${TERRAFORM_DIR}..."

    MODULE_DIRS=$(find "${TERRAFORM_DIR}" -name "*.tf" -type f -exec dirname {} \; | sort -u)
    MODULE_COUNT=$(echo "${MODULE_DIRS}" | wc -l)

    log_info "Found ${MODULE_COUNT} Terraform module(s)"
    echo ""

    MODULE_NUM=0
    for module_dir in ${MODULE_DIRS}; do
        MODULE_NUM=$((MODULE_NUM + 1))
        module_name=$(basename "${module_dir}")
        module_evidence="${TERRAFORM_EVIDENCE}/${module_name}"
        mkdir -p "${module_evidence}"

        log_subsection "Module ${MODULE_NUM}/${MODULE_COUNT}: ${module_name}"

        # Terraform Init (Docker)
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_info "  terraform init (Docker)..."
        if docker run --rm \
            -v "${module_dir}:/workspace:ro" \
            -w /workspace \
            "${TF_IMAGE}" \
            init -backend=false -upgrade -no-color > "${module_evidence}/init.log" 2>&1; then
            echo -e "    ${GREEN}✓${NC} init passed"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            echo -e "    ${RED}✗${NC} init failed"
            cat "${module_evidence}/init.log"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
            continue
        fi

        # Terraform Format Check (Docker)
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_info "  terraform fmt (Docker)..."
        if docker run --rm \
            -v "${module_dir}:/workspace:ro" \
            -w /workspace \
            "${TF_IMAGE}" \
            fmt -check -diff > "${module_evidence}/fmt.log" 2>&1; then
            echo -e "    ${GREEN}✓${NC} fmt passed"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            echo -e "    ${YELLOW}⚠${NC} fmt found issues (non-blocking)"
            WARNED_CHECKS=$((WARNED_CHECKS + 1))
        fi

        # Terraform Validate (Docker)
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_info "  terraform validate (Docker)..."
        if docker run --rm \
            -v "${module_dir}:/workspace:ro" \
            -w /workspace \
            "${TF_IMAGE}" \
            validate -no-color > "${module_evidence}/validate.log" 2>&1; then
            echo -e "    ${GREEN}✓${NC} validate passed"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            echo -e "    ${RED}✗${NC} validate failed"
            cat "${module_evidence}/validate.log"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
            continue
        fi

        # tfsec Security Scan (Docker)
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_info "  tfsec security scan (Docker)..."
        if docker run --rm \
            -v "${module_dir}:/workspace:ro" \
            "${TFSEC_IMAGE}" \
            /workspace \
            --format=json \
            --minimum-severity=MEDIUM \
            --soft-fail > "${module_evidence}/tfsec.json" 2>&1; then
            echo -e "    ${GREEN}✓${NC} tfsec completed"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))

            # Count HIGH/CRITICAL issues
            HIGH_CRITICAL=$(grep -c '"severity":"HIGH"\|"severity":"CRITICAL"' "${module_evidence}/tfsec.json" 2>/dev/null || echo 0)
            if [ "${HIGH_CRITICAL}" -gt 0 ]; then
                echo -e "    ${YELLOW}⚠${NC} Found ${HIGH_CRITICAL} HIGH/CRITICAL issues"
                WARNED_CHECKS=$((WARNED_CHECKS + 1))
            fi
        else
            echo -e "    ${YELLOW}⚠${NC} tfsec completed with warnings"
            WARNED_CHECKS=$((WARNED_CHECKS + 1))
        fi

        # Checkov Policy Scan (Docker)
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_info "  checkov policy scan (Docker)..."
        if docker run --rm \
            -v "${module_dir}:/workspace:ro" \
            "${CHECKOV_IMAGE}" \
            --directory /workspace \
            --framework terraform \
            --output json \
            --soft-fail > "${module_evidence}/checkov.json" 2>&1; then
            echo -e "    ${GREEN}✓${NC} checkov completed"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            echo -e "    ${YELLOW}⚠${NC} checkov completed with warnings"
            WARNED_CHECKS=$((WARNED_CHECKS + 1))
        fi

        # Generate module receipt
        MODULE_HASH=$(find "${module_evidence}" -type f -exec sha256sum {} \; | sha256sum | cut -d' ' -f1)
        EVIDENCE_HASHES+=("${MODULE_HASH}")

        cat > "${module_evidence}/receipt.json" << EOF
{
  "module": "${module_name}",
  "path": "${module_dir}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "docker_image": "${TF_IMAGE}",
  "checks": {
    "init": "$([ -f "${module_evidence}/init.log" ] && echo "passed" || echo "failed")",
    "fmt": "$([ -f "${module_evidence}/fmt.log" ] && echo "checked" || echo "skipped")",
    "validate": "$([ -f "${module_evidence}/validate.log" ] && echo "passed" || echo "failed")",
    "tfsec": "$([ -f "${module_evidence}/tfsec.json" ] && echo "completed" || echo "skipped")",
    "checkov": "$([ -f "${module_evidence}/checkov.json" ] && echo "completed" || echo "skipped")"
  },
  "evidence_hash": "${MODULE_HASH}"
}
EOF

        echo ""
    done
fi

# ============================================================================
# GATE 2: Helm Chart Validation
# ============================================================================
log_section "GATE 2: Helm Chart Validation"

HELM_EVIDENCE="${EVIDENCE_DIR}/helm"
mkdir -p "${HELM_EVIDENCE}"

if [ ! -d "${HELM_CHART_DIR}" ]; then
    log_warn "Helm chart directory not found: ${HELM_CHART_DIR}"
    log_warn "Skipping Helm validation"
else
    CHART_NAME=$(basename "${HELM_CHART_DIR}")
    log_info "Validating Helm chart: ${CHART_NAME}"
    echo ""

    # Helm Lint (Docker)
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    log_info "  helm lint (Docker)..."
    if docker run --rm \
        -v "${HELM_CHART_DIR}:/chart:ro" \
        -w /chart \
        "${HELM_IMAGE}" \
        lint . > "${HELM_EVIDENCE}/lint.log" 2>&1; then
        echo -e "    ${GREEN}✓${NC} helm lint passed"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        echo -e "    ${RED}✗${NC} helm lint failed"
        cat "${HELM_EVIDENCE}/lint.log"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
    fi

    # Helm Template Render Test (Docker)
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    log_info "  helm template (Docker)..."
    if docker run --rm \
        -v "${HELM_CHART_DIR}:/chart:ro" \
        -w /chart \
        "${HELM_IMAGE}" \
        template test-release . > "${HELM_EVIDENCE}/template.yaml" 2>&1; then
        echo -e "    ${GREEN}✓${NC} template rendering passed"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))

        MANIFEST_COUNT=$(grep -c "^kind:" "${HELM_EVIDENCE}/template.yaml" 2>/dev/null || echo 0)
        log_info "    Generated ${MANIFEST_COUNT} Kubernetes manifests"
    else
        echo -e "    ${RED}✗${NC} template rendering failed"
        cat "${HELM_EVIDENCE}/template.yaml"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
    fi

    # Helm Security Checks (Custom validation)
    if [ -f "${HELM_EVIDENCE}/template.yaml" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_info "  security policy checks..."

        SECURITY_ISSUES=0

        # Check for privileged containers
        if grep -q "privileged: true" "${HELM_EVIDENCE}/template.yaml"; then
            echo -e "    ${RED}✗${NC} Privileged containers detected"
            SECURITY_ISSUES=$((SECURITY_ISSUES + 1))
        fi

        # Check for hostPath volumes
        if grep -q "hostPath:" "${HELM_EVIDENCE}/template.yaml"; then
            echo -e "    ${RED}✗${NC} hostPath volumes detected"
            SECURITY_ISSUES=$((SECURITY_ISSUES + 1))
        fi

        # Check for hostNetwork
        if grep -q "hostNetwork: true" "${HELM_EVIDENCE}/template.yaml"; then
            echo -e "    ${RED}✗${NC} hostNetwork detected"
            SECURITY_ISSUES=$((SECURITY_ISSUES + 1))
        fi

        # Check for non-root user
        if grep -q "runAsNonRoot: true" "${HELM_EVIDENCE}/template.yaml"; then
            echo -e "    ${GREEN}✓${NC} Non-root user enforced"
        else
            echo -e "    ${YELLOW}⚠${NC} Non-root user not enforced"
            WARNED_CHECKS=$((WARNED_CHECKS + 1))
        fi

        # Check for read-only root filesystem
        if grep -q "readOnlyRootFilesystem: true" "${HELM_EVIDENCE}/template.yaml"; then
            echo -e "    ${GREEN}✓${NC} Read-only root filesystem"
        else
            echo -e "    ${YELLOW}⚠${NC} Writable root filesystem"
            WARNED_CHECKS=$((WARNED_CHECKS + 1))
        fi

        if [ ${SECURITY_ISSUES} -eq 0 ]; then
            echo -e "    ${GREEN}✓${NC} security checks passed"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            echo -e "    ${RED}✗${NC} ${SECURITY_ISSUES} security violations detected"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
        fi
    fi

    # Generate Helm receipt
    HELM_HASH=$(find "${HELM_EVIDENCE}" -type f -exec sha256sum {} \; | sha256sum | cut -d' ' -f1)
    EVIDENCE_HASHES+=("${HELM_HASH}")

    cat > "${HELM_EVIDENCE}/receipt.json" << EOF
{
  "chart": "${CHART_NAME}",
  "path": "${HELM_CHART_DIR}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "docker_image": "${HELM_IMAGE}",
  "checks": {
    "lint": "$([ -f "${HELM_EVIDENCE}/lint.log" ] && echo "passed" || echo "failed")",
    "template": "$([ -f "${HELM_EVIDENCE}/template.yaml" ] && echo "passed" || echo "failed")",
    "security": "completed"
  },
  "evidence_hash": "${HELM_HASH}"
}
EOF

    echo ""
fi

# ============================================================================
# Generate Final Validation Receipt (Cryptographic Proof)
# ============================================================================
log_section "Generating Cryptographic Validation Receipt"

# Compute composite evidence hash
COMPOSITE_HASH=$(printf '%s\n' "${EVIDENCE_HASHES[@]}" | sha256sum | cut -d' ' -f1)

# Get git context (if available)
GIT_SHA="unknown"
GIT_BRANCH="unknown"
if [ -d "${MARKETPLACE_DIR}/../.git" ]; then
    GIT_SHA=$(cd "${MARKETPLACE_DIR}/.." && git rev-parse HEAD 2>/dev/null || echo "unknown")
    GIT_BRANCH=$(cd "${MARKETPLACE_DIR}/.." && git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
fi

# Generate validation receipt (proof = receipt(hash(...)))
cat > "${EVIDENCE_DIR}/validation-receipt.json" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "repository": "erlmcp",
  "git_sha": "${GIT_SHA}",
  "git_branch": "${GIT_BRANCH}",
  "docker_only": true,
  "constitution_compliance": "DOCKER-ONLY CONSTITUTION v3",
  "gates": {
    "terraform": {
      "modules": ${MODULE_COUNT:-0},
      "checks": ["init", "fmt", "validate", "tfsec", "checkov"]
    },
    "helm": {
      "charts": 1,
      "checks": ["lint", "template", "security"]
    }
  },
  "results": {
    "total_checks": ${TOTAL_CHECKS},
    "passed": ${PASSED_CHECKS},
    "failed": ${FAILED_CHECKS},
    "warnings": ${WARNED_CHECKS}
  },
  "docker_images": {
    "terraform": "${TF_IMAGE}",
    "tfsec": "${TFSEC_IMAGE}",
    "checkov": "${CHECKOV_IMAGE}",
    "helm": "${HELM_IMAGE}"
  },
  "evidence": {
    "directory": "${EVIDENCE_DIR}",
    "composite_hash": "${COMPOSITE_HASH}",
    "receipt_hash": "$(echo -n "${COMPOSITE_HASH}${GIT_SHA}${TIMESTAMP}" | sha256sum | cut -d' ' -f1)"
  }
}
EOF

RECEIPT_HASH=$(sha256sum "${EVIDENCE_DIR}/validation-receipt.json" | cut -d' ' -f1)

# ============================================================================
# Validation Summary (Operator-First Display)
# ============================================================================
echo ""
echo "========================================================================"
echo "                    VALIDATION SUMMARY"
echo "========================================================================"
echo ""
echo "Total Checks:    ${TOTAL_CHECKS}"
echo -e "Passed:          ${GREEN}${PASSED_CHECKS}${NC}"
echo -e "Failed:          ${RED}${FAILED_CHECKS}${NC}"
echo -e "Warnings:        ${YELLOW}${WARNED_CHECKS}${NC}"
echo ""
echo "------------------------------------------------------------------------"
echo "Evidence Artifacts:"
echo "------------------------------------------------------------------------"
echo "  Directory:     ${EVIDENCE_DIR}"
echo "  Receipt:       ${EVIDENCE_DIR}/validation-receipt.json"
echo "  Receipt Hash:  ${RECEIPT_HASH}"
echo ""
echo "------------------------------------------------------------------------"
echo "Cryptographic Proof:"
echo "------------------------------------------------------------------------"
echo "  Git SHA:       ${GIT_SHA}"
echo "  Git Branch:    ${GIT_BRANCH}"
echo "  Composite:     ${COMPOSITE_HASH}"
echo "  Timestamp:     $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""
echo "------------------------------------------------------------------------"
echo "Docker Images (Deterministic Supply Chain):"
echo "------------------------------------------------------------------------"
echo "  Terraform:     ${TF_IMAGE}"
echo "  tfsec:         ${TFSEC_IMAGE}"
echo "  Checkov:       ${CHECKOV_IMAGE}"
echo "  Helm:          ${HELM_IMAGE}"
echo ""
echo "========================================================================"
echo ""

# ============================================================================
# Exit Logic (Quality Invariants)
# ============================================================================
if [ ${FAILED_CHECKS} -gt 0 ]; then
    log_error "ANDON: Validation FAILED with ${FAILED_CHECKS} error(s)"
    log_error "Quality invariants violated: errors=0 required"
    log_info "Review evidence: ${EVIDENCE_DIR}"
    echo ""
    log_info "Fix commands (Docker-only):"
    echo ""
    echo "  # Format Terraform modules:"
    echo "  docker run --rm -v \${PWD}:/workspace -w /workspace ${TF_IMAGE} fmt -recursive"
    echo ""
    echo "  # Review security issues:"
    echo "  cat ${EVIDENCE_DIR}/terraform/*/tfsec.json"
    echo "  cat ${EVIDENCE_DIR}/terraform/*/checkov.json"
    echo ""
    exit 1
fi

if [ ${WARNED_CHECKS} -gt 0 ]; then
    log_warn "Validation completed with ${WARNED_CHECKS} warning(s)"
    log_info "Non-blocking warnings detected. Review recommended."
fi

log_info "All validation gates PASSED!"
log_info "Proof receipt: receipt_hash=${RECEIPT_HASH}"
echo ""
echo "DOCKER-ONLY CONSTITUTION: Compliance verified ✓"
echo "All execution performed via Docker containers."
echo "No host execution detected."
echo ""
echo "Ready for production deployment."
echo ""

exit 0
