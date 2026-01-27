#!/bin/bash
# Rollback script for erlmcp deployments
# Usage: ./scripts/rollback.sh [environment] [backup-name]

set -euo pipefail

# === CONFIGURATION ===
BACKUP_DIR="${BACKUP_DIR:-/opt/erlmcp/backups}"
DEPLOY_DIR="/opt/erlmcp"

# === COLORS ===
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# === LOGGING ===
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# === HELP ===
show_help() {
    cat << EOF
Usage: $0 [environment] [backup-name]

Arguments:
    environment   Target environment (dev, staging, production)
    backup-name   Specific backup to restore (optional, defaults to latest)

Examples:
    $0 production                          # Rollback to latest backup
    $0 production erlmcp-backup-20260126   # Rollback to specific backup
    $0 staging                             # Rollback staging to latest

Available backups:
EOF
    if [[ -d "$BACKUP_DIR" ]]; then
        ls -1t "$BACKUP_DIR" | head -10
    else
        echo "    (No backups found)"
    fi
    exit 0
}

# === ARGUMENT PARSING ===
ENVIRONMENT="${1:-}"
BACKUP_NAME="${2:-}"

if [[ "$ENVIRONMENT" == "--help" || "$ENVIRONMENT" == "-h" ]]; then
    show_help
fi

if [[ -z "$ENVIRONMENT" ]]; then
    log_error "Environment is required"
    show_help
fi

if [[ ! "$ENVIRONMENT" =~ ^(dev|staging|production)$ ]]; then
    log_error "Invalid environment: $ENVIRONMENT"
    show_help
fi

# === FIND BACKUP ===
find_backup() {
    if [[ -n "$BACKUP_NAME" ]]; then
        # Use specified backup
        local backup_path="${BACKUP_DIR}/${BACKUP_NAME}"
        if [[ ! -d "$backup_path" ]]; then
            log_error "Backup not found: $backup_path"
            exit 1
        fi
        echo "$backup_path"
    else
        # Use latest backup
        local latest=$(ls -1t "$BACKUP_DIR" 2>/dev/null | head -1)
        if [[ -z "$latest" ]]; then
            log_error "No backups found in $BACKUP_DIR"
            exit 1
        fi
        echo "${BACKUP_DIR}/${latest}"
    fi
}

# === VERIFY BACKUP ===
verify_backup() {
    local backup_path=$1
    log_info "Verifying backup: $backup_path"

    # Check if backup contains required files
    if [[ ! -d "${backup_path}/bin" ]]; then
        log_error "Invalid backup: missing bin directory"
        exit 1
    fi

    if [[ ! -f "${backup_path}/bin/erlmcp" ]]; then
        log_error "Invalid backup: missing erlmcp binary"
        exit 1
    fi

    log_success "Backup verification passed"
}

# === STOP CURRENT ===
stop_current() {
    log_info "Stopping current deployment..."

    if [[ ! -f "${DEPLOY_DIR}/bin/erlmcp" ]]; then
        log_warning "No current deployment found"
        return 0
    fi

    if [[ "$ENVIRONMENT" == "dev" ]]; then
        "${DEPLOY_DIR}/bin/erlmcp" stop || true
    else
        sudo "${DEPLOY_DIR}/bin/erlmcp" stop || true
    fi

    # Wait for graceful shutdown
    sleep 3

    # Force kill if still running
    local pid
    if pid=$(pgrep -f "erlmcp.*foreground" 2>/dev/null); then
        log_warning "Force killing process: $pid"
        if [[ "$ENVIRONMENT" == "dev" ]]; then
            kill -9 "$pid" || true
        else
            sudo kill -9 "$pid" || true
        fi
    fi

    log_success "Current deployment stopped"
}

# === BACKUP CURRENT ===
backup_current() {
    log_info "Creating backup of current deployment before rollback..."

    if [[ ! -d "$DEPLOY_DIR" ]]; then
        log_warning "No current deployment to backup"
        return 0
    fi

    local backup_name="erlmcp-pre-rollback-$(date +%Y%m%d-%H%M%S)"
    local backup_path="${BACKUP_DIR}/${backup_name}"

    mkdir -p "$BACKUP_DIR"

    if [[ "$ENVIRONMENT" == "dev" ]]; then
        cp -r "$DEPLOY_DIR" "$backup_path"
    else
        sudo cp -r "$DEPLOY_DIR" "$backup_path"
        sudo chown -R "$(whoami):$(id -gn)" "$backup_path"
    fi

    log_success "Current deployment backed up to: $backup_path"
}

# === RESTORE BACKUP ===
restore_backup() {
    local backup_path=$1
    log_info "Restoring backup from: $backup_path"

    # Remove current deployment
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        rm -rf "$DEPLOY_DIR"
    else
        sudo rm -rf "$DEPLOY_DIR"
    fi

    # Restore backup
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        cp -r "$backup_path" "$DEPLOY_DIR"
    else
        sudo cp -r "$backup_path" "$DEPLOY_DIR"
        sudo chown -R "$(whoami):$(id -gn)" "$DEPLOY_DIR"
    fi

    log_success "Backup restored"
}

# === START APPLICATION ===
start_application() {
    log_info "Starting application..."

    if [[ "$ENVIRONMENT" == "dev" ]]; then
        "${DEPLOY_DIR}/bin/erlmcp" start || {
            log_error "Failed to start application"
            return 1
        }
    else
        sudo "${DEPLOY_DIR}/bin/erlmcp" start || {
            log_error "Failed to start application"
            return 1
        }
    fi

    # Wait for startup
    sleep 5

    log_success "Application started"
}

# === HEALTH CHECK ===
health_check() {
    log_info "Running health check..."

    # Ping check
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        if ! "${DEPLOY_DIR}/bin/erlmcp" ping; then
            log_error "Application not responding to ping"
            return 1
        fi
    else
        if ! sudo "${DEPLOY_DIR}/bin/erlmcp" ping; then
            log_error "Application not responding to ping"
            return 1
        fi
    fi

    # HTTP health check (if available)
    if command -v curl &> /dev/null; then
        local health_url="http://localhost:8080/health"
        local max_attempts=10
        local attempt=1

        while [[ $attempt -le $max_attempts ]]; do
            if curl -sf "$health_url" > /dev/null 2>&1; then
                log_success "Health check passed: $health_url"
                return 0
            fi
            log_info "Health check attempt $attempt/$max_attempts failed, retrying..."
            sleep 2
            ((attempt++))
        done

        log_warning "Health endpoint not responding after $max_attempts attempts"
    fi

    log_success "Basic health check passed (ping successful)"
    return 0
}

# === MAIN ===
main() {
    log_info "=== erlmcp Rollback Script ==="
    log_info "Environment: $ENVIRONMENT"
    log_info "Backup directory: $BACKUP_DIR"
    log_info "================================"

    # Find backup
    local backup_path
    backup_path=$(find_backup)
    log_info "Selected backup: $backup_path"

    # Verify backup
    verify_backup "$backup_path"

    # Confirm rollback for production
    if [[ "$ENVIRONMENT" == "production" ]]; then
        log_warning "=== PRODUCTION ROLLBACK WARNING ==="
        log_warning "You are about to rollback production to:"
        log_warning "  $backup_path"
        read -p "Are you sure? Type 'YES' to continue: " confirmation
        if [[ "$confirmation" != "YES" ]]; then
            log_info "Rollback cancelled"
            exit 0
        fi
    fi

    # Stop current
    stop_current

    # Backup current (just in case)
    backup_current

    # Restore backup
    restore_backup "$backup_path"

    # Start application
    if ! start_application; then
        log_error "Failed to start application after rollback"
        exit 1
    fi

    # Health check
    if ! health_check; then
        log_error "Health check failed after rollback"
        log_error "Application may not be functioning correctly"
        exit 1
    fi

    log_success "=== Rollback completed successfully ==="
    log_info "Restored backup: $backup_path"
    log_info "Environment: $ENVIRONMENT"
}

# Run main
main "$@"
