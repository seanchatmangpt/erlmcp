#!/usr/bin/env bash
# ============================================================================
# Google Marketplace Reviewer Simulation - Phase 1: Prerequisites Check
# ============================================================================
# This script validates all required tooling for deploying and testing
# the erlmcp v3 enterprise solution.
#
# Google Marketplace reviewers verify that:
# 1. All required CLI tools are installed
# 2. Tool versions meet minimum requirements
# 3. Authentication is properly configured
# 4. Required permissions are available
#
# Usage: ./01-prereq-check.sh [--verbose]
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
VERBOSE="${VERBOSE:-false}"

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
WARNED_CHECKS=0

# Log functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
    ((PASSED_CHECKS++)) || true
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $1"
    ((WARNED_CHECKS++)) || true
}

log_error() {
    echo -e "${RED}[✗]${NC} $1"
    ((FAILED_CHECKS++)) || true
}

log_check() {
    ((TOTAL_CHECKS++)) || true
    if [[ "${VERBOSE}" == "true" ]]; then
        log_info "Checking: $1"
    fi
}

# Version comparison helper
version_ge() {
    local installed="$1"
    local required="$2"
    if [[ "$(printf '%s\n' "${required}" "${installed}" | sort -V | head -n1)" == "${required}" ]]; then
        return 0
    else
        return 1
    fi
}

# ============================================================================
# SECTION 1: Core GCP Tools
# ============================================================================
check_gcloud() {
    log_check "Google Cloud CLI (gcloud)"
    local min_version="400.0.0"

    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud not installed"
        log_info "Install from: https://cloud.google.com/sdk/docs/install"
        return 1
    fi

    local version
    version=$(gcloud version --format='value(version)' 2>/dev/null || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_error "gcloud version ${version} is too old (required: >= ${min_version})"
        return 1
    fi

    log_success "gcloud ${version} (>= ${min_version})"
    return 0
}

check_gsutil() {
    log_check "Cloud Storage utility (gsutil)"

    if ! command -v gsutil &> /dev/null; then
        log_error "gsutil not installed (included with gcloud)"
        return 1
    fi

    local version
    version=$(gsutil version 2>&1 | head -n1 || echo "unknown")
    log_success "gsutil available (${version})"
    return 0
}

# ============================================================================
# SECTION 2: Container Tools
# ============================================================================
check_docker() {
    log_check "Docker container engine"
    local min_version="20.10.0"

    if ! command -v docker &> /dev/null; then
        log_error "docker not installed"
        log_info "Install from: https://docs.docker.com/engine/install/"
        return 1
    fi

    if ! docker info &> /dev/null; then
        log_error "docker daemon not running or no permission"
        log_info "Start docker: sudo systemctl start docker"
        log_info "Or add user to docker group: sudo usermod -aG docker \$USER"
        return 1
    fi

    local version
    version=$(docker version --format '{{.Server.Version}}' 2>/dev/null || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_warning "docker version ${version} may be too old (recommended: >= ${min_version})"
        return 1
    fi

    log_success "docker ${version}"
    return 0
}

check_helm() {
    log_check "Helm package manager"
    local min_version="3.0.0"

    if ! command -v helm &> /dev/null; then
        log_error "helm not installed"
        log_info "Install from: https://helm.sh/docs/intro/install/"
        return 1
    fi

    local version
    version=$(helm version --template '{{.Version}}' 2>/dev/null || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_error "helm version ${version} is too old (required: >= ${min_version})"
        return 1
    fi

    log_success "helm ${version}"
    return 0
}

check_kubectl() {
    log_check "Kubernetes command-line tool"
    local min_version="1.24.0"

    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl not installed"
        log_info "Install from: https://kubernetes.io/docs/tasks/tools/"
        return 1
    fi

    local version
    version=$(kubectl version --client --short 2>/dev/null | grep -oP 'Client Version: \K[^ ]+' || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_error "kubectl version ${version} is too old (required: >= ${min_version})"
        return 1
    fi

    log_success "kubectl ${version}"
    return 0
}

# ============================================================================
# SECTION 3: Infrastructure as Code Tools
# ============================================================================
check_terraform() {
    log_check "Terraform infrastructure tool"
    local min_version="1.3.0"

    if ! command -v terraform &> /dev/null; then
        log_error "terraform not installed"
        log_info "Install from: https://developer.hashicorp.com/terraform/install"
        return 1
    fi

    local version
    version=$(terraform version -json 2>/dev/null | jq -r '.terraform_version' || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_error "terraform version ${version} is too old (required: >= ${min_version})"
        return 1
    fi

    log_success "terraform ${version}"

    # Check terraform providers
    if [[ -d "${SCRIPT_DIR}/../../terraform" ]]; then
        log_info "  Checking terraform providers..."
        if ! terraform init -upgrade=false &> /dev/null; then
            log_warning "terraform providers not initialized"
        fi
    fi

    return 0
}

check_packer() {
    log_check "Packer image builder"
    local min_version="1.8.0"

    if ! command -v packer &> /dev/null; then
        log_warning "packer not installed (required for custom image builds)"
        log_info "Optional - Install from: https://developer.hashicorp.com/packer/install"
        return 1
    fi

    local version
    version=$(packer version 2>&1 | grep -oP 'Packer v\K[^ ]+' || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_warning "packer version ${version} may be too old (recommended: >= ${min_version})"
        return 1
    fi

    log_success "packer ${version}"
    return 0
}

# ============================================================================
# SECTION 4: Build Tools
# ============================================================================
check_make() {
    log_check "GNU Make build automation"

    if ! command -v make &> /dev/null; then
        log_error "make not installed"
        log_info "Install: apt-get install build-essential (Debian/Ubuntu)"
        log_info "Install: yum install make (RHEL/CentOS)"
        return 1
    fi

    local version
    version=$(make --version 2>&1 | head -n1 | grep -oP 'Make \K[^ ]+' || echo "unknown")
    log_success "make ${version}"
    return 0
}

check_jq() {
    log_check "jq JSON processor"

    if ! command -v jq &> /dev/null; then
        log_error "jq not installed"
        log_info "Install: apt-get install jq (Debian/Ubuntu)"
        log_info "Install: yum install jq (RHEL/CentOS)"
        log_info "Or: brew install jq (macOS)"
        return 1
    fi

    local version
    version=$(jq --version 2>/dev/null || echo "unknown")
    log_success "jq ${version}"
    return 0
}

# ============================================================================
# SECTION 5: Testing Tools
# ============================================================================
check_curl() {
    log_check "curl HTTP client"

    if ! command -v curl &> /dev/null; then
        log_error "curl not installed"
        return 1
    fi

    local version
    version=$(curl --version 2>&1 | head -n1 | grep -oP 'curl \K[^ ]+' || echo "unknown")
    log_success "curl ${version}"
    return 0
}

check_bash() {
    log_check "Bash shell"
    local min_version="4.0"

    if ! command -v bash &> /dev/null; then
        log_error "bash not found (unexpected)"
        return 1
    fi

    local version
    version=$(bash --version 2>&1 | grep -oP 'GNU bash, version \K[^ ]+' || echo "unknown")
    log_info "  Installed version: ${version}"

    if ! version_ge "${version}" "${min_version}"; then
        log_warning "bash version ${version} may be too old (recommended: >= ${min_version})"
        return 1
    fi

    log_success "bash ${version}"
    return 0
}

# ============================================================================
# SECTION 6: GCP Authentication & Permissions
# ============================================================================
check_gcp_auth() {
    log_check "GCP authentication"

    if ! gcloud auth list --filter="status:ACTIVE" --format="value(account)" 2>/dev/null | grep -q .; then
        log_error "No active gcloud authentication"
        log_info "Run: gcloud auth login"
        return 1
    fi

    local account
    account=$(gcloud auth list --filter="status:ACTIVE" --format="value(account)")
    log_success "Authenticated as: ${account}"
    return 0
}

check_gcp_project() {
    log_check "GCP project configuration"

    local project
    project=$(gcloud config get-value project 2>/dev/null || echo "")

    if [[ -z "${project}" ]]; then
        log_error "No default project set"
        log_info "Run: gcloud config set project PROJECT_ID"
        return 1
    fi

    log_success "Default project: ${project}"

    # Verify project exists
    if ! gcloud projects describe "${project}" &> /dev/null; then
        log_error "Project ${project} does not exist or no access"
        return 1
    fi

    return 0
}

check_gcp_permissions() {
    log_check "Required GCP permissions"

    local project
    project=$(gcloud config get-value project 2>/dev/null || echo "")

    log_info "  Checking key permissions..."
    local permissions_ok=true

    # Check compute permissions
    if gcloud compute instances list --project "${project}" --limit=0 &> /dev/null; then
        log_info "    ✓ compute.instances.list"
    else
        log_warning "    ✗ compute.instances.list (missing permission)"
        permissions_ok=false
    fi

    # Check storage permissions
    if gsutil ls -p "${project}" &> /dev/null; then
        log_info "    ✓ storage.buckets.list"
    else
        log_warning "    ✗ storage.buckets.list (missing permission)"
        permissions_ok=false
    fi

    if [[ "${permissions_ok}" == "true" ]]; then
        log_success "Required permissions available"
        return 0
    else
        log_warning "Some permissions missing (may limit testing)"
        return 1
    fi
}

# ============================================================================
# SECTION 7: Docker Hub Access
# ============================================================================
check_docker_hub() {
    log_check "Docker Hub access"

    if ! docker pull busybox:latest &> /dev/null; then
        log_warning "Cannot pull from Docker Hub (rate limited or network issue)"
        log_info "This may affect container deployment"
        return 1
    fi

    log_success "Docker Hub access verified"
    return 0
}

# ============================================================================
# SECTION 8: Network Connectivity
# ============================================================================
check_network() {
    log_check "Network connectivity to GCP APIs"

    if ! curl -s --connect-timeout 5 https://www.googleapis.com > /dev/null; then
        log_error "Cannot reach GCP APIs (network or firewall issue)"
        return 1
    fi

    log_success "GCP APIs reachable"
    return 0
}

# ============================================================================
# Generate Report
# ============================================================================
generate_report() {
    local exit_code=$1
    local report_file="${SCRIPT_DIR}/prereq-report.md"

    log_info "Generating prerequisite report..."

    cat > "${report_file}" <<EOF
# Prerequisites Check Report

**Generated**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Project**: $(gcloud config get-value project 2>/dev/null || echo "not set")

## Summary

- **Total Checks**: ${TOTAL_CHECKS}
- **Passed**: ${PASSED_CHECKS}
- **Warnings**: ${WARNED_CHECKS}
- **Failed**: ${FAILED_CHECKS}

## Check Results by Category

### 1. Core GCP Tools
$(check_gcloud >/dev/null 2>&1 && echo "✅ gcloud" || echo "❌ gcloud")
$(check_gsutil >/dev/null 2>&1 && echo "✅ gsutil" || echo "❌ gsutil")

### 2. Container Tools
$(check_docker >/dev/null 2>&1 && echo "✅ docker" || echo "❌ docker")
$(check_helm >/dev/null 2>&1 && echo "✅ helm" || echo "❌ helm")
$(check_kubectl >/dev/null 2>&1 && echo "✅ kubectl" || echo "❌ kubectl")

### 3. Infrastructure Tools
$(check_terraform >/dev/null 2>&1 && echo "✅ terraform" || echo "❌ terraform")
$(check_packer >/dev/null 2>&1 && echo "✅ packer" || echo "⚠️  packer (optional)")

### 4. Build Tools
$(check_make >/dev/null 2>&1 && echo "✅ make" || echo "❌ make")
$(check_jq >/dev/null 2>&1 && echo "✅ jq" || echo "❌ jq")

### 5. Testing Tools
$(check_curl >/dev/null 2>&1 && echo "✅ curl" || echo "❌ curl")
$(check_bash >/dev/null 2>&1 && echo "✅ bash" || echo "❌ bash")

### 6. Authentication & Permissions
$(check_gcp_auth >/dev/null 2>&1 && echo "✅ gcloud auth" || echo "❌ gcloud auth")
$(check_gcp_project >/dev/null 2>&1 && echo "✅ gcloud project" || echo "❌ gcloud project")
$(check_gcp_permissions >/dev/null 2>&1 && echo "✅ gcp permissions" || echo "⚠️  gcp permissions")

### 7. External Services
$(check_docker_hub >/dev/null 2>&1 && echo "✅ docker hub" || echo "⚠️  docker hub")
$(check_network >/dev/null 2>&1 && echo "✅ network" || echo "❌ network")

## Remediation Steps

### Required Installations
EOF

    # Add remediation steps based on failures
    if ! check_docker >/dev/null 2>&1; then
        echo "" >> "${report_file}"
        echo "#### Docker" >> "${report_file}"
        echo "```bash" >> "${report_file}"
        echo "# Ubuntu/Debian" >> "${report_file}"
        echo "curl -fsSL https://get.docker.com -o get-docker.sh" >> "${report_file}"
        echo "sudo sh get-docker.sh" >> "${report_file}"
        echo "sudo usermod -aG docker \$USER" >> "${report_file}"
        echo "```" >> "${report_file}"
    fi

    if ! check_helm >/dev/null 2>&1; then
        echo "" >> "${report_file}"
        echo "#### Helm" >> "${report_file}"
        echo "```bash" >> "${report_file}"
        echo "curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash" >> "${report_file}"
        echo "```" >> "${report_file}"
    fi

    if ! check_kubectl >/dev/null 2>&1; then
        echo "" >> "${report_file}"
        echo "#### kubectl" >> "${report_file}"
        echo "```bash" >> "${report_file}"
        echo "curl -LO \"https://dl.k8s.io/release/\$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl\"" >> "${report_file}"
        echo "sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl" >> "${report_file}"
        echo "```" >> "${report_file}"
    fi

    if ! check_terraform >/dev/null 2>&1; then
        echo "" >> "${report_file}"
        echo "#### Terraform" >> "${report_file}"
        echo "```bash" >> "${report_file}"
        echo "wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg" >> "${report_file}"
        echo "echo \"deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com \$(lsb_release -cs) main\" | sudo tee /etc/apt/sources.list.d/hashicorp.list" >> "${report_file}"
        echo "sudo apt update && sudo apt install terraform" >> "${report_file}"
        echo "```" >> "${report_file}"
    fi

    cat >> "${report_file}" <<EOF

## Next Steps

1. Fix all failed checks above
2. Re-run this script: \`./01-prereq-check.sh\`
3. Proceed to API enablement: \`./02-api-enable.sh\`

## Validation Criteria

- ✅ All required tools installed
- ✅ Tool versions meet minimum requirements
- ✅ GCP authentication configured
- ✅ Required permissions available
- ⚠️  Optional tools can be skipped for basic testing

---
**Exit Code**: ${exit_code}
**Status**: $([ ${exit_code} -eq 0 ] && echo "READY FOR DEPLOYMENT" || echo "REQUIRES REMEDIATION")
EOF

    log_success "Report saved to: ${report_file}"
    return 0
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main() {
    log_info "Starting Prerequisites Check"
    log_info "Verbose mode: ${VERBOSE}"
    echo ""

    # Run all checks
    check_gcloud
    check_gsutil
    check_docker
    check_helm
    check_kubectl
    check_terraform
    check_packer
    check_make
    check_jq
    check_curl
    check_bash
    check_gcp_auth
    check_gcp_project
    check_gcp_permissions
    check_docker_hub
    check_network

    echo ""
    log_info "================================"
    log_info "Check Summary:"
    log_info "  Total:   ${TOTAL_CHECKS}"
    log_success "  Passed:  ${PASSED_CHECKS}"
    log_warning "  Warnings: ${WARNED_CHECKS}"
    log_error "  Failed:  ${FAILED_CHECKS}"
    log_info "================================"
    echo ""

    # Generate report
    local exit_code=0
    if [[ ${FAILED_CHECKS} -gt 0 ]]; then
        exit_code=1
    fi

    generate_report ${exit_code}

    if [[ ${exit_code} -eq 0 ]]; then
        log_success "All prerequisites satisfied!"
        log_info "Next: ./02-api-enable.sh"
    else
        log_error "Some prerequisites failed - see report above"
    fi

    return ${exit_code}
}

# Run main
main "$@"
