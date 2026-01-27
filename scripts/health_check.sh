#!/bin/bash
# Health check script for erlmcp deployments
# Usage: ./scripts/health_check.sh [environment] [endpoint]

set -euo pipefail

# === CONFIGURATION ===
ENVIRONMENT="${1:-production}"
ENDPOINT="${2:-http://localhost:8080}"
MAX_RETRIES="${MAX_RETRIES:-30}"
RETRY_DELAY="${RETRY_DELAY:-2}"

# === COLORS ===
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# === LOGGING ===
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# === HEALTH CHECK ===
check_health() {
    local url="${ENDPOINT}/health"
    local retry=1

    log_info "Checking health at: $url"

    while [[ $retry -le $MAX_RETRIES ]]; do
        if response=$(curl -sf "$url" 2>/dev/null); then
            log_success "Health check passed!"
            echo "$response" | jq '.' 2>/dev/null || echo "$response"
            return 0
        fi

        log_warning "Attempt $retry/$MAX_RETRIES failed, retrying in ${RETRY_DELAY}s..."
        sleep "$RETRY_DELAY"
        ((retry++))
    done

    log_error "Health check failed after $MAX_RETRIES attempts"
    return 1
}

# === MAIN ===
main() {
    log_info "=== erlmcp Health Check ==="
    log_info "Environment: $ENVIRONMENT"
    log_info "Endpoint: $ENDPOINT"
    log_info "=========================="

    if ! command -v curl &> /dev/null; then
        log_error "curl not found. Please install curl"
        exit 1
    fi

    if check_health; then
        exit 0
    else
        exit 1
    fi
}

main "$@"
