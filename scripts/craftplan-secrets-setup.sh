#!/usr/bin/env bash
# ============================================================================
# Craftplan Secrets Generator - Docker Swarm
# ============================================================================
# Generates all required Docker secrets for Craftplan ERP deployment.
#
# Usage:
#   ./scripts/craftplan-secrets-setup.sh [namespace]
#
# Arguments:
#   namespace  - Optional namespace for secrets (default: craftplan)
#
# Example:
#   ./scripts/craftplan-secrets-setup.sh
#   ./scripts/craftplan-secrets-setup.sh prod
#
# Requirements:
#   - Docker Swarm initialized
#   - openssl command available
# ============================================================================
set -euo pipefail

# Configuration
NAMESPACE="${1:-craftplan}"
SECRET_DIR="${SECRET_DIR:-./secrets/${NAMESPACE}}"
DOCKER_CMD="${DOCKER_CMD:-docker}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# ============================================================================
# Helper Functions
# ============================================================================

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_swarm() {
    log_info "Checking Docker Swarm status..."
    if ! ${DOCKER_CMD} info --format '{{.Swarm.LocalNodeState}}' 2>/dev/null | grep -q 'active'; then
        log_error "Docker Swarm is not initialized."
        echo "Run: ${DOCKER_CMD} swarm init"
        exit 1
    fi
    log_info "Docker Swarm is active ✓"
}

generate_secret() {
    local secret_name="$1"
    local secret_file="${SECRET_DIR}/${secret_name}.txt"
    local bytes="${2:-64}"

    if [[ -f "${secret_file}" ]]; then
        log_warn "Secret ${secret_name} already exists at ${secret_file}"
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_info "Keeping existing secret"
            return 1
        fi
    fi

    mkdir -p "${SECRET_DIR}"
    openssl rand -base64 "${bytes}" | tr -d '\n' > "${secret_file}"
    chmod 600 "${secret_file}"
    log_info "Generated secret: ${secret_name} (${bytes} bytes)"
    return 0
}

create_docker_secret() {
    local secret_name="$1"
    local secret_file="${SECRET_DIR}/${secret_name}.txt"
    local docker_secret="${NAMESPACE}_${secret_name}"

    if ! ${DOCKER_CMD} secret ls --format '{{.Name}}' | grep -q "^${docker_secret}$"; then
        ${DOCKER_CMD} secret create "${docker_secret}" "${secret_file}"
        log_info "Created Docker secret: ${docker_secret}"
    else
        log_warn "Docker secret ${docker_secret} already exists"
        read -p "Update? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            ${DOCKER_CMD} secret rm "${docker_secret}"
            ${DOCKER_CMD} secret create "${docker_secret}" "${secret_file}"
            log_info "Updated Docker secret: ${docker_secret}"
        fi
    fi
}

# ============================================================================
# Main Setup
# ============================================================================

main() {
    log_info "Setting up Craftplan secrets for namespace: ${NAMESPACE}"
    log_info "Secret directory: ${SECRET_DIR}"
    echo

    check_swarm

    # Generate all required secrets
    log_info "Generating secrets..."

    # SECRET_KEY_BASE - Phoenix secret key base (min 64 bytes)
    generate_secret "secret_key_base" 48

    # TOKEN_SIGNING_SECRET - API token signing (48 bytes)
    generate_secret "token_signing_secret" 48

    # CLOAK_KEY - AES encryption key (32 bytes, must be base64)
    generate_secret "cloak_key" 32

    # POSTGRES_PASSWORD - Database password (32 bytes)
    generate_secret "db_password" 32

    echo
    log_info "All secrets generated in: ${SECRET_DIR}"
    echo

    # Create Docker secrets
    log_info "Creating Docker secrets..."

    create_docker_secret "secret_key_base"
    create_docker_secret "token_signing_secret"
    create_docker_secret "cloak_key"
    create_docker_secret "db_password"

    echo
    log_info "Secret setup complete! ✓"
    echo
    log_info "Next steps:"
    echo "  1. Review generated secrets in: ${SECRET_DIR}"
    echo "  2. Configure environment: cp .env.craftplan.example .env.craftplan"
    echo "  3. Deploy: docker stack deploy -c docker-compose.craftplan.yml ${NAMESPACE}"
    echo
    log_info "To view secrets:"
    echo "  ${DOCKER_CMD} secret ls --filter name=${NAMESPACE}_"
}

# Run main function
main "$@"
