#!/bin/bash
# ============================================================================
# Validate All Terraform Configurations
# Google Cloud Marketplace Deployment
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
TERRAFORM_DIR="${MARKETPLACE_DIR}/terraform"

# Counters
PASSED=0
FAILED=0
WARNED=0

# Check if terraform is installed
if ! command -v terraform &> /dev/null; then
    log_error "terraform not found. Install from: https://developer.hashicorp.com/terraform/downloads"
    exit 1
fi

TF_VERSION=$(terraform version -json | jq -r '.terraform_version')
log_info "Using Terraform version: $TF_VERSION"

# Validate all Terraform configurations
log_info "Validating Terraform configurations..."
echo ""

# Find all Terraform files and validate their directories
find "$TERRAFORM_DIR" -name "*.tf" -type f | while read -r tf_file; do
    module_dir=$(dirname "$tf_file")

    echo "Checking: $module_dir"

    # Change to module directory
    cd "$module_dir"

    # Run terraform init
    if terraform init -upgrade -backend=false -input=false &> /dev/null; then
        echo -e "  ${GREEN}✓${NC} terraform init"
    else
        echo -e "  ${RED}✗${NC} terraform init failed"
        FAILED=$((FAILED + 1))
        continue
    fi

    # Run terraform fmt -check
    if terraform fmt -check -diff &> /dev/null; then
        echo -e "  ${GREEN}✓${NC} terraform fmt"
    else
        echo -e "  ${YELLOW}⚠${NC} terraform fmt found formatting issues"
        terraform fmt -check -diff
        WARNED=$((WARNED + 1))
    fi

    # Run terraform validate
    if terraform validate &> /dev/null; then
        echo -e "  ${GREEN}✓${NC} terraform validate"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗${NC} terraform validate failed"
        terraform validate
        FAILED=$((FAILED + 1))
    fi

    echo ""
done

# Summary
echo "======================================="
echo "Validation Summary:"
echo "  Passed: $PASSED"
echo "  Warnings: $WARNED"
echo "  Failed: $FAILED"
echo "======================================="

if [ $FAILED -gt 0 ]; then
    log_error "Validation failed with $FAILED errors"
    exit 1
fi

if [ $WARNED -gt 0 ]; then
    log_warn "Validation completed with $WARNED warnings"
    log_info "Run 'terraform fmt' to fix formatting issues"
    exit 0
fi

log_info "All Terraform configurations validated successfully!"
exit 0
