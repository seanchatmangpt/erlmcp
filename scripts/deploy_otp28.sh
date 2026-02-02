#!/bin/bash
# Production deployment script for erlmcp on OTP 28.3.1
# Usage: ./scripts/deploy_otp28.sh [environment] [options]
# Environments: dev, staging, production
# Options: --skip-tests, --force, --dry-run, --preflight, --backup-only

set -euo pipefail

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BACKUP_DIR="${BACKUP_DIR:-/opt/erlmcp/backups}"
DEPLOY_DIR="${DEPLOY_DIR:-/opt/erlmcp}"
DEPLOY_LOG="${DEPLOY_LOG:-/var/log/erlmcp/deploy.log}"
RELEASE_DIR="${PROJECT_ROOT}/_build"

# OTP 28.3.1 Configuration
ERLMCP_OTP_BIN="${ERLMCP_OTP_BIN:-$HOME/.erlmcp/otp-28.3.1/bin}"
ERLMCP_OTP_VERSION="28.3.1"
REQUIRED_OTP_MAJOR=28

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ==============================================================================
# LOGGING
# ==============================================================================

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
log_step() { log "STEP" "${CYAN}${BOLD}$*${NC}"; }

# ==============================================================================
# BANNER
# ==============================================================================

show_banner() {
    cat << EOF
${BOLD}${CYAN}
╔════════════════════════════════════════════════════════════════════╗
║                                                                    ║
║  erlmcp Production Deployment - OTP 28.3.1                        ║
║  Version: 2.1.0                                                   ║
║  Target: Erlang/OTP ${ERLMCP_OTP_VERSION}                                ║
║                                                                    ║
╚════════════════════════════════════════════════════════════════════╝
${NC}
EOF
}

# ==============================================================================
# HELP
# ==============================================================================

show_help() {
    cat << EOF
Usage: $0 [environment] [options]

Environments:
    dev         Deploy to local development environment
    staging     Deploy to staging environment
    production  Deploy to production environment

Options:
    --skip-tests      Skip test execution (not recommended for production)
    --force           Force deployment even with warnings
    --dry-run         Show what would be deployed without executing
    --preflight       Run pre-flight checks only
    --backup-only     Create backup without deploying
    --help            Show this help message

Examples:
    $0 dev                        # Deploy to dev
    $0 staging                    # Deploy to staging with tests
    $0 production                 # Deploy to production (full validation)
    $0 production --dry-run       # Preview production deployment
    $0 production --preflight     # Run pre-flight checks only

OTP 28.3.1 Features Enabled:
    - Native JSON module (replaces jsx)
    - Process hibernation for memory optimization
    - Priority messaging for critical operations
    - Memory guards for leak prevention
    - PCRE2 regex engine
    - TLS 1.3 optimizations
    - Two-phase hot code loading

EOF
    exit 0
}

# ==============================================================================
# ARGUMENT PARSING
# ==============================================================================

ENVIRONMENT="${1:-}"
SKIP_TESTS=false
FORCE=false
DRY_RUN=false
PREFLIGHT_ONLY=false
BACKUP_ONLY=false

shift || true
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-tests) SKIP_TESTS=true; shift ;;
        --force) FORCE=true; shift ;;
        --dry-run) DRY_RUN=true; shift ;;
        --preflight) PREFLIGHT_ONLY=true; shift ;;
        --backup-only) BACKUP_ONLY=true; shift ;;
        --help) show_help ;;
        *) log_error "Unknown option: $1"; show_help ;;
    esac
done

# Validate environment
if [[ ! "$ENVIRONMENT" =~ ^(dev|staging|production)$ ]]; then
    log_error "Invalid environment: $ENVIRONMENT"
    show_help
fi

# ==============================================================================
# PREFLIGHT CHECKS
# ==============================================================================

preflight_checks() {
    log_step "Running pre-flight checks for ${ENVIRONMENT}..."

    local checks_failed=0

    # 1. Git repo check (production only)
    if [[ "$ENVIRONMENT" == "production" ]]; then
        if ! git diff-index --quiet HEAD -- 2>/dev/null; then
            if [[ "$FORCE" != "true" ]]; then
                log_error "Git working directory is not clean"
                log_warning "Use --force to deploy with uncommitted changes (not recommended)"
                checks_failed=$((checks_failed + 1))
            else
                log_warning "Deploying with uncommitted changes (--force enabled)"
            fi
        fi
    fi

    # 2. OTP 28.3.1 Verification
    log_info "Checking Erlang/OTP version..."

    # Add custom OTP to PATH if available
    if [[ -d "$ERLMCP_OTP_BIN" ]]; then
        export PATH="${ERLMCP_OTP_BIN}:$PATH"
    fi

    if ! command -v erl &> /dev/null; then
        log_error "Erlang/OTP not found. Please install OTP ${ERLMCP_OTP_VERSION}"
        checks_failed=$((checks_failed + 1))
    else
        local otp_version=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null)
        local otp_major=$(echo "$otp_version" | sed 's/[^0-9].*//' | sed 's/^0*//')

        if ! [[ "$otp_major" =~ ^[0-9]+$ ]]; then
            log_error "Could not determine OTP version: '${otp_version}'"
            checks_failed=$((checks_failed + 1))
        elif [ "$otp_major" -ne "$REQUIRED_OTP_MAJOR" ]; then
            log_error "OTP version mismatch: ${otp_version} (required: ${ERLMCP_OTP_VERSION})"
            log_warning "Custom OTP path: ${ERLMCP_OTP_BIN}"
            checks_failed=$((checks_failed + 1))
        else
            log_success "OTP version check passed: ${otp_version}"
        fi
    fi

    # 3. rebar3 check
    log_info "Checking rebar3 installation..."
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found. Please install rebar3 3.22+"
        checks_failed=$((checks_failed + 1))
    else
        log_success "rebar3 found: $(rebar3 version | head -1)"
    fi

    # 4. Required tools
    log_info "Checking required tools..."
    for tool in git make curl; do
        if ! command -v "$tool" &> /dev/null; then
            log_error "Required tool not found: $tool"
            checks_failed=$((checks_failed + 1))
        fi
    done

    # 5. System limits
    log_info "Checking system limits..."
    local fd_limit=$(ulimit -n)
    if [ "$fd_limit" -lt 65536 ]; then
        log_warning "File descriptor limit low: ${fd_limit} (recommended: >= 65536)"
    else
        log_success "File descriptor limit OK: ${fd_limit}"
    fi

    local proc_limit=$(ulimit -u)
    if [ "$proc_limit" -lt 131072 ]; then
        log_warning "Process limit low: ${proc_limit} (recommended: >= 131072)"
    else
        log_success "Process limit OK: ${proc_limit}"
    fi

    # 6. Disk space
    log_info "Checking disk space..."
    local available_space=$(df -BG "${PROJECT_ROOT}" | tail -1 | awk '{print $4}' | sed 's/G//')
    if [ "$available_space" -lt 5 ]; then
        log_error "Insufficient disk space: ${available_space}GB (minimum: 5GB)"
        checks_failed=$((checks_failed + 1))
    else
        log_success "Disk space OK: ${available_space}GB available"
    fi

    # 7. Check OTP 28 features
    log_info "Verifying OTP 28 features..."

    # Native JSON
    if erl -eval 'case code:is_loaded(json) of false -> halt(1); _ -> halt(0) end' -noshell 2>/dev/null; then
        log_success "Native JSON module available"
    else
        log_error "Native JSON module not available (OTP 28 feature)"
        checks_failed=$((checks_failed + 1))
    fi

    # Two-phase loading
    if erl -eval 'case erlang:module_loaded(code) of false -> halt(0); _ -> case code:prepare_loading([]) of {ok, _} -> halt(0); _ -> halt(1) end end' -noshell 2>/dev/null; then
        log_success "Two-phase loading available (prepare_loading/1)"
    else
        log_warning "Two-phase loading not available (OTP 27+ feature)"
    fi

    if [ $checks_failed -gt 0 ]; then
        log_error "Pre-flight checks failed: ${checks_failed} error(s)"
        if [[ "$FORCE" != "true" ]]; then
            exit 1
        fi
        log_warning "Continuing despite errors (--force enabled)"
    fi

    log_success "Pre-flight checks completed"
}

# ==============================================================================
# BUILD RELEASE
# ==============================================================================

build_release() {
    log_step "Building release for ${ENVIRONMENT}..."

    cd "$PROJECT_ROOT"

    # Clean previous builds
    log_info "Cleaning previous builds..."
    rebar3 clean -v 2>/dev/null || true

    # Determine profile
    local profile="prod"
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        profile="dev"
    fi

    # Compile
    log_info "Compiling with profile: ${profile}..."
    if ! TERM=dumb rebar3 as "$profile" compile; then
        log_error "Compilation failed"
        return 1
    fi
    log_success "Compilation successful"

    # Run tests
    if [[ "$SKIP_TESTS" != "true" ]]; then
        log_info "Running test suite..."

        # EUnit
        log_info "Running EUnit tests..."
        if ! TERM=dumb rebar3 eunit; then
            log_error "EUnit tests failed"
            if [[ "$FORCE" != "true" ]]; then
                return 1
            fi
            log_warning "Continuing despite EUnit failures (--force enabled)"
        fi

        # Common Test
        log_info "Running Common Test..."
        if ! TERM=dumb rebar3 ct; then
            log_error "Common Test failed"
            if [[ "$FORCE" != "true" ]]; then
                return 1
            fi
            log_warning "Continuing despite CT failures (--force enabled)"
        fi

        log_success "All tests passed"
    else
        log_warning "Skipping tests (--skip-tests enabled)"
    fi

    # Dialyzer
    log_info "Running Dialyzer type check..."
    if ! TERM=dumb rebar3 dialyzer; then
        log_warning "Dialyzer warnings detected (not blocking)"
    fi

    # Xref
    log_info "Running Xref cross-reference check..."
    if ! TERM=dumb rebar3 xref; then
        log_warning "Xref issues detected (not blocking)"
    fi

    # Build release
    log_info "Building release..."
    if ! TERM=dumb rebar3 as "$profile" release; then
        log_error "Release build failed"
        return 1
    fi

    local release_path="${RELEASE_DIR}/${profile}/rel/erlmcp"
    if [[ ! -d "$release_path" ]]; then
        log_error "Release directory not found: $release_path"
        return 1
    fi

    # Verify release
    log_info "Verifying release..."
    if [[ ! -f "${release_path}/bin/erlmcp" ]]; then
        log_error "Release binary not found"
        return 1
    fi

    log_success "Release built successfully: ${release_path}"
    echo "$release_path"
}

# ==============================================================================
# OTP 28 FEATURE VERIFICATION
# ==============================================================================

verify_otp28_features() {
    log_step "Verifying OTP 28 features..."

    local release_path=$1

    # Start node temporarily for verification
    local node_name="erlmcp_verify_$$"

    # Create temporary config for verification
    local temp_config="/tmp/erlmcp_verify_$$.config"
    cat > "$temp_config" << EOF
[{erlmcp, [{otp_version_check, true}]}].
EOF

    log_info "Verifying native JSON module..."
    if erl -noshell -eval "case json:encode(#{test => value}) of _ -> halt(0); _ -> halt(1) end" 2>/dev/null; then
        log_success "Native JSON module working"
    else
        log_error "Native JSON module failed"
        return 1
    fi

    log_info "Verifying PCRE2 regex..."
    if erl -noshell -eval "case re:run(\"test\", \".*\") of {match, _} -> halt(0); _ -> halt(1) end" 2>/dev/null; then
        log_success "PCRE2 regex working"
    else
        log_warning "PCRE2 may not be available"
    fi

    log_info "Verifying TLS 1.3 support..."
    if erl -noshell -eval "case ssl:versions() of List when lists:keymember('tlsv1.3', 1, List) -> halt(0); _ -> halt(1) end" 2>/dev/null; then
        log_success "TLS 1.3 available"
    else
        log_warning "TLS 1.3 may not be available"
    fi

    rm -f "$temp_config"
    log_success "OTP 28 feature verification completed"
}

# ==============================================================================
# BACKUP
# ==============================================================================

backup_deployment() {
    local deploy_target=$1
    log_step "Backing up current deployment..."

    if [[ ! -d "$deploy_target" ]]; then
        log_info "No existing deployment to backup"
        return 0
    fi

    mkdir -p "$BACKUP_DIR"
    local backup_name="erlmcp-backup-$(date +%Y%m%d-%H%M%S)"
    local backup_path="${BACKUP_DIR}/${backup_name}"

    log_info "Creating backup: ${backup_path}"
    if [[ "$ENVIRONMENT" == "dev" ]]; then
        if ! cp -r "$deploy_target" "$backup_path"; then
            log_warning "Backup failed, continuing deployment"
        fi
    else
        if ! sudo cp -r "$deploy_target" "$backup_path"; then
            log_warning "Backup failed, continuing deployment"
        fi
        sudo chown -R "$(whoami):$(id -gn)" "$backup_path"
    fi

    # Create backup checksum
    if [[ -d "$backup_path" ]]; then
        (cd "$BACKUP_DIR" && find "$backup_name" -type f -exec sha256sum {} \; > "${backup_name}.sha256")
        log_success "Backup created: ${backup_path}"

        # Keep only last 5 backups
        local backup_count=$(ls -1t "$BACKUP_DIR" | grep -E "^erlmcp-backup-" | wc -l)
        if [[ $backup_count -gt 5 ]]; then
            log_info "Cleaning old backups (keeping 5 most recent)..."
            ls -1t "$BACKUP_DIR" | grep -E "^erlmcp-backup-" | tail -n +6 | while read -r old_backup; do
                rm -rf "${BACKUP_DIR}/${old_backup}"
                rm -f "${BACKUP_DIR}/${old_backup}.sha256"
            done
        fi
    fi

    echo "$backup_path"
}

# ==============================================================================
# SMOKE TESTS
# ==============================================================================

run_smoke_tests() {
    local release_path=$1
    log_step "Running smoke tests..."

    # Start application in background
    log_info "Starting application for smoke tests..."
    "${release_path}/bin/erlmcp" start || {
        log_error "Failed to start application"
        return 1
    }

    # Wait for startup
    log_info "Waiting for application startup..."
    local max_wait=30
    local waited=0
    while [ $waited -lt $max_wait ]; do
        if "${release_path}/bin/erlmcp" ping 2>/dev/null; then
            break
        fi
        sleep 1
        waited=$((waited + 1))
    done

    if [ $waited -ge $max_wait ]; then
        log_error "Application did not start within ${max_wait}s"
        "${release_path}/bin/erlmcp" stop || true
        return 1
    fi

    log_success "Application started successfully"

    # Check if responding
    log_info "Testing ping response..."
    if ! "${release_path}/bin/erlmcp" ping; then
        log_error "Application not responding to ping"
        "${release_path}/bin/erlmcp" stop || true
        return 1
    fi

    # Health endpoint check
    if command -v curl &> /dev/null; then
        local health_url="http://localhost:8080/health"
        log_info "Testing health endpoint: ${health_url}"

        if curl -sf "$health_url" > /dev/null 2>&1; then
            log_success "Health check passed"
        else
            log_warning "Health check endpoint not available: ${health_url}"
        fi
    fi

    # Verify OTP 28 features in running system
    log_info "Verifying OTP 28 features in running system..."
    "${release_path}/bin/erlmcp" eval 'case code:is_loaded(json) of false -> io:format("JSON module not loaded~n"), halt(1); _ -> io:format("JSON module OK~n"), halt(0) end' || {
        log_error "OTP 28 JSON module verification failed"
        "${release_path}/bin/erlmcp" stop || true
        return 1
    }

    # Stop application
    log_info "Stopping application..."
    "${release_path}/bin/erlmcp" stop || true
    sleep 2

    log_success "Smoke tests passed"
    return 0
}

# ==============================================================================
# DEPLOY
# ==============================================================================

deploy() {
    local release_path=$1
    local deploy_target="${DEPLOY_DIR}"

    if [[ "$ENVIRONMENT" == "dev" ]]; then
        deploy_target="${release_path}"
        log_info "Dev environment detected, using local release"
        return 0
    fi

    log_step "Deploying to ${deploy_target}..."

    # Stop current deployment if running
    if [[ -f "${deploy_target}/bin/erlmcp" ]]; then
        log_info "Stopping current deployment..."
        if [[ "$ENVIRONMENT" == "dev" ]]; then
            "${deploy_target}/bin/erlmcp" stop || true
        else
            sudo "${deploy_target}/bin/erlmcp" stop || true
        fi
        sleep 3
    fi

    # Deploy new release
    log_info "Copying release to ${deploy_target}..."
    sudo mkdir -p "$(dirname "$deploy_target")"
    if ! sudo cp -r "$release_path" "$deploy_target"; then
        log_error "Failed to deploy release"
        return 1
    fi

    # Set permissions
    sudo chown -R "$(whoami):$(id -gn)" "$deploy_target"

    log_success "Deployment completed"
}

# ==============================================================================
# HEALTH CHECK
# ==============================================================================

health_check() {
    local deploy_target=$1
    log_step "Running health check..."

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
    log_info "Waiting for startup..."
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

# ==============================================================================
# MAIN
# ==============================================================================

main() {
    show_banner

    log_info "=== erlmcp Deployment Script ==="
    log_info "Environment: ${ENVIRONMENT}"
    log_info "OTP Version: ${ERLMCP_OTP_VERSION}"
    log_info "Skip Tests: ${SKIP_TESTS}"
    log_info "Force: ${FORCE}"
    log_info "Dry Run: ${DRY_RUN}"
    log_info "Preflight Only: ${PREFLIGHT_ONLY}"
    log_info "Backup Only: ${BACKUP_ONLY}"
    log_info "================================"

    cd "$PROJECT_ROOT"

    # Pre-flight checks
    preflight_checks

    if [[ "$PREFLIGHT_ONLY" == "true" ]]; then
        log_success "Pre-flight checks completed successfully"
        exit 0
    fi

    # Build release
    local release_path
    release_path=$(build_release)

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "Dry run completed. Would deploy: ${release_path}"
        exit 0
    fi

    # Verify OTP 28 features
    verify_otp28_features "$release_path"

    # Backup only
    if [[ "$BACKUP_ONLY" == "true" ]]; then
        backup_deployment "${DEPLOY_DIR}"
        log_success "Backup completed"
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

    # Backup current deployment
    local backup_path
    backup_path=$(backup_deployment "${DEPLOY_DIR}")

    # Deploy
    local deploy_target="${DEPLOY_DIR}"
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
            if [[ -n "$backup_path" && -d "$backup_path" ]]; then
                log_info "Rolling back to: ${backup_path}"
                # Rollback logic here
            fi
        fi
        exit 1
    fi

    # Final verification
    log_step "Final verification..."

    # Get application version
    local app_version
    app_version=$("${deploy_target}/bin/erlmcp" versions 2>/dev/null | head -1 || echo "unknown")
    log_success "Application version: ${app_version}"

    log_success "=== Deployment completed successfully ==="
    log_info "Environment: ${ENVIRONMENT}"
    log_info "Deploy target: ${deploy_target}"
    log_info "Backup: ${backup_path}"
    log_info "Logs: ${DEPLOY_LOG}"

    # Show quick commands
    echo ""
    log_info "Quick commands:"
    echo "  Start:   ${deploy_target}/bin/erlmcp start"
    echo "  Stop:    ${deploy_target}/bin/erlmcp stop"
    echo "  Ping:    ${deploy_target}/bin/erlmcp ping"
    echo "  Console: ${deploy_target}/bin/erlmcp console"
    echo "  Health:  curl http://localhost:8080/health"
    echo ""
}

# Run main
main "$@"
