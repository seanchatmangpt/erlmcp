#!/bin/bash
# Production deployment automation for erlmcp v0.6.0 + TCPS
# Usage: ./scripts/deploy.sh [environment] [options]
# Environments: dev, staging, production
# Options: --skip-tests, --force, --rollback, --dry-run

set -euo pipefail

# === CONFIGURATION ===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BACKUP_DIR="${BACKUP_DIR:-/opt/erlmcp/backups}"
DEPLOY_LOG="${DEPLOY_LOG:-/var/log/erlmcp/deploy.log}"
RELEASE_DIR="${PROJECT_ROOT}/_build"
VERSION="0.6.0"

# === COLORS ===
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# === LOGGING ===
log() {
    local level=$1
    shift
    local msg="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${timestamp} [${level}] ${msg}" | tee -a "${DEPLOY_LOG}" 2>/dev/null || echo "${timestamp} [${level}] ${msg}"
}

log_info() { log "INFO" "${BLUE}$*${NC}"; }
log_success() { log "SUCCESS" "${GREEN}$*${NC}"; }
log_warning() { log "WARNING" "${YELLOW}$*${NC}"; }
log_error() { log "ERROR" "${RED}$*${NC}"; }

# === HELP ===
show_help() {
    cat << EOF
Usage: $0 [environment] [options]

Environments:
    dev         Deploy to local development environment
    staging     Deploy to staging environment
    production  Deploy to production environment

Options:
    --skip-tests    Skip test execution (not recommended)
    --force         Force deployment even with warnings
    --rollback      Rollback to previous deployment
    --dry-run       Show what would be deployed without deploying
    --help          Show this help message

Examples:
    $0 dev                      # Deploy to dev
    $0 staging                  # Deploy to staging with tests
    $0 production --skip-tests  # Deploy to prod without tests (dangerous!)
    $0 production --rollback    # Rollback production deployment

EOF
    exit 0
}

# === ARGUMENT PARSING ===
ENVIRONMENT="${1:-}"
SKIP_TESTS=false
FORCE=false
ROLLBACK=false
DRY_RUN=false

shift || true
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-tests) SKIP_TESTS=true; shift ;;
        --force) FORCE=true; shift ;;
        --rollback) ROLLBACK=true; shift ;;
        --dry-run) DRY_RUN=true; shift ;;
        --help) show_help ;;
        *) log_error "Unknown option: $1"; show_help ;;
    esac
done

# Validate environment
if [[ ! "$ENVIRONMENT" =~ ^(dev|staging|production)$ ]]; then
    log_error "Invalid environment: $ENVIRONMENT"
    show_help
fi

# === PRE-FLIGHT CHECKS ===
preflight_checks() {
    log_info "Running pre-flight checks for $ENVIRONMENT..."

    # Check if git repo is clean
    if [[ "$ENVIRONMENT" == "production" ]]; then
        if ! git diff-index --quiet HEAD -- 2>/dev/null; then
            log_error "Git working directory is not clean"
            if [[ "$FORCE" != "true" ]]; then
                log_error "Use --force to deploy with uncommitted changes (not recommended)"
                exit 1
            fi
            log_warning "Deploying with uncommitted changes (--force enabled)"
        fi
    fi

    # Check Erlang/OTP version
    if ! command -v erl &> /dev/null; then
        log_error "Erlang/OTP not found. Please install Erlang 26+"
        exit 1
    fi

    local erl_version=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)
    log_info "Erlang/OTP version: $erl_version"

    # Check rebar3
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found. Please install rebar3"
        exit 1
    fi

    # Check required tools
    for tool in git make; do
        if ! command -v "$tool" &> /dev/null; then
            log_error "Required tool not found: $tool"
            exit 1
        fi
    done

    # Check version in src file
    local app_version=$(grep -E "^\s*\{vsn" "${PROJECT_ROOT}/src/erlmcp.app.src" | sed -E 's/.*"([^"]+)".*/\1/')
    log_info "Application version: $app_version"

    log_success "Pre-flight checks passed"
}

# === BUILD RELEASE ===
build_release() {
    log_info "Building release for $ENVIRONMENT..."

    cd "$PROJECT_ROOT"

    # Clean previous builds
    log_info "Cleaning previous builds..."
    rebar3 clean || true

    # Compile with appropriate profile
    local profile="prod"
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        profile="dev"
    fi

    log_info "Compiling with profile: $profile"
    if ! rebar3 as "$profile" compile; then
        log_error "Compilation failed"
        exit 1
    fi

    # Run tests unless skipped
    if [[ "$SKIP_TESTS" != "true" ]]; then
        log_info "Running test suite..."
        if ! rebar3 as test do eunit, ct; then
            log_error "Tests failed"
            if [[ "$FORCE" != "true" ]]; then
                exit 1
            fi
            log_warning "Continuing despite test failures (--force enabled)"
        fi
        log_success "Tests passed"
    else
        log_warning "Skipping tests (--skip-tests enabled)"
    fi

    # Build release
    log_info "Building release..."
    if ! rebar3 as "$profile" release; then
        log_error "Release build failed"
        exit 1
    fi

    local release_path="${RELEASE_DIR}/${profile}/rel/erlmcp"
    if [[ ! -d "$release_path" ]]; then
        log_error "Release directory not found: $release_path"
        exit 1
    fi

    log_success "Release built successfully: $release_path"
    echo "$release_path"
}

# === SMOKE TESTS ===
run_smoke_tests() {
    local release_path=$1
    log_info "Running smoke tests..."

    # Start application in background
    log_info "Starting application for smoke tests..."
    "${release_path}/bin/erlmcp" start || {
        log_error "Failed to start application"
        return 1
    }

    # Wait for startup
    sleep 5

    # Check if running
    if ! "${release_path}/bin/erlmcp" ping; then
        log_error "Application not responding to ping"
        "${release_path}/bin/erlmcp" stop || true
        return 1
    fi

    # Check health endpoint (if available)
    if command -v curl &> /dev/null; then
        local health_url="http://localhost:8080/health"
        if curl -sf "$health_url" > /dev/null 2>&1; then
            log_success "Health check passed: $health_url"
        else
            log_warning "Health check endpoint not available: $health_url"
        fi
    fi

    # Stop application
    log_info "Stopping application..."
    "${release_path}/bin/erlmcp" stop || true

    log_success "Smoke tests passed"
    return 0
}

# === BACKUP ===
backup_deployment() {
    local deploy_target=$1
    log_info "Backing up current deployment..."

    if [[ ! -d "$deploy_target" ]]; then
        log_info "No existing deployment to backup"
        return 0
    fi

    mkdir -p "$BACKUP_DIR"
    local backup_name="erlmcp-backup-$(date +%Y%m%d-%H%M%S)"
    local backup_path="${BACKUP_DIR}/${backup_name}"

    if cp -r "$deploy_target" "$backup_path"; then
        log_success "Backup created: $backup_path"

        # Keep only last 5 backups
        local backup_count=$(ls -1t "$BACKUP_DIR" | wc -l)
        if [[ $backup_count -gt 5 ]]; then
            log_info "Cleaning old backups (keeping 5 most recent)..."
            ls -1t "$BACKUP_DIR" | tail -n +6 | xargs -I {} rm -rf "${BACKUP_DIR}/{}"
        fi
    else
        log_warning "Backup failed, but continuing deployment"
    fi
}

# === DEPLOY ===
deploy() {
    local release_path=$1
    local deploy_target="/opt/erlmcp"

    if [[ "$ENVIRONMENT" == "dev" ]]; then
        deploy_target="${PROJECT_ROOT}/_build/dev/rel/erlmcp"
        log_info "Dev environment detected, skipping deployment to $deploy_target"
        return 0
    fi

    log_info "Deploying to $deploy_target..."

    # Backup current deployment
    backup_deployment "$deploy_target"

    # Stop current deployment if running
    if [[ -f "${deploy_target}/bin/erlmcp" ]]; then
        log_info "Stopping current deployment..."
        "${deploy_target}/bin/erlmcp" stop || true
        sleep 2
    fi

    # Deploy new release
    log_info "Copying release to $deploy_target..."
    sudo mkdir -p "$(dirname "$deploy_target")"
    sudo cp -r "$release_path" "$deploy_target" || {
        log_error "Failed to deploy release"
        exit 1
    }

    # Set permissions
    sudo chown -R "$(whoami):$(id -gn)" "$deploy_target"

    log_success "Deployment completed"
}

# === HEALTH CHECK ===
health_check() {
    local deploy_target=$1
    log_info "Running health check..."

    # Start application
    log_info "Starting application..."
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        "${deploy_target}/bin/erlmcp" start || {
            log_error "Failed to start application"
            return 1
        }
    else
        sudo "${deploy_target}/bin/erlmcp" start || {
            log_error "Failed to start application"
            return 1
        }
    fi

    # Wait for startup
    sleep 5

    # Check if running
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        if ! "${deploy_target}/bin/erlmcp" ping; then
            log_error "Application not responding"
            return 1
        fi
    else
        if ! sudo "${deploy_target}/bin/erlmcp" ping; then
            log_error "Application not responding"
            return 1
        fi
    fi

    log_success "Health check passed - application is running"
    return 0
}

# === ROLLBACK ===
rollback() {
    log_info "Rolling back deployment..."

    local latest_backup=$(ls -1t "$BACKUP_DIR" 2>/dev/null | head -1)
    if [[ -z "$latest_backup" ]]; then
        log_error "No backups found in $BACKUP_DIR"
        exit 1
    fi

    local backup_path="${BACKUP_DIR}/${latest_backup}"
    local deploy_target="/opt/erlmcp"

    log_info "Rolling back to: $latest_backup"

    # Stop current
    if [[ -f "${deploy_target}/bin/erlmcp" ]]; then
        log_info "Stopping current deployment..."
        sudo "${deploy_target}/bin/erlmcp" stop || true
        sleep 2
    fi

    # Restore backup
    log_info "Restoring backup..."
    sudo rm -rf "$deploy_target"
    sudo cp -r "$backup_path" "$deploy_target" || {
        log_error "Failed to restore backup"
        exit 1
    }

    # Start application
    log_info "Starting application..."
    sudo "${deploy_target}/bin/erlmcp" start || {
        log_error "Failed to start application after rollback"
        exit 1
    }

    # Health check
    sleep 5
    if sudo "${deploy_target}/bin/erlmcp" ping; then
        log_success "Rollback completed successfully"
    else
        log_error "Rollback completed but application not responding"
        exit 1
    fi
}

# === MAIN ===
main() {
    log_info "=== erlmcp Deployment Script ==="
    log_info "Environment: $ENVIRONMENT"
    log_info "Version: $VERSION"
    log_info "Skip Tests: $SKIP_TESTS"
    log_info "Force: $FORCE"
    log_info "Dry Run: $DRY_RUN"
    log_info "================================"

    # Handle rollback
    if [[ "$ROLLBACK" == "true" ]]; then
        rollback
        exit 0
    fi

    # Pre-flight checks
    preflight_checks

    # Build release
    local release_path
    release_path=$(build_release)

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "Dry run completed. Would deploy: $release_path"
        exit 0
    fi

    # Smoke tests
    if [[ "$SKIP_TESTS" != "true" ]]; then
        if ! run_smoke_tests "$release_path"; then
            log_error "Smoke tests failed"
            if [[ "$FORCE" != "true" ]]; then
                exit 1
            fi
            log_warning "Continuing despite smoke test failures (--force enabled)"
        fi
    fi

    # Deploy
    local deploy_target="/opt/erlmcp"
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        deploy_target="$release_path"
    else
        deploy "$release_path"
    fi

    # Health check
    if ! health_check "$deploy_target"; then
        log_error "Health check failed after deployment"
        if [[ "$ENVIRONMENT" != "dev" && "$FORCE" != "true" ]]; then
            log_error "Initiating automatic rollback..."
            rollback
            exit 1
        fi
    fi

    log_success "=== Deployment completed successfully ==="
    log_info "Environment: $ENVIRONMENT"
    log_info "Deploy target: $deploy_target"
    log_info "Version: $VERSION"
    log_info "Logs: $DEPLOY_LOG"
}

# Run main
main "$@"
