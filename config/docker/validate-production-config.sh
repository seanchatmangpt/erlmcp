#!/usr/bin/env bash
#===============================================================================
# Production Configuration Validation Script for erlmcp v3
#===============================================================================
# Validates vm.args.production and sys.config.production settings
#
# Usage:
#   ./validate-production-config.sh
#
# Exit codes:
#   0 - All validations passed
#   1 - Validation failed
#   2 - Missing dependencies
#
#===============================================================================

set -euo pipefail

#===============================================================================
# COLORS AND FORMATTING
#===============================================================================
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

info() { echo -e "${BLUE}[INFO]${NC} $*"; }
success() { echo -e "${GREEN}[PASS]${NC} $*"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[FAIL]${NC} $*"; }

#===============================================================================
# CONFIGURATION FILES
#===============================================================================
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
readonly VM_ARGS="${SCRIPT_DIR}/vm.args.production"
readonly SYS_CONFIG="${SCRIPT_DIR}/sys.config.production"

#===============================================================================
# VALIDATION FUNCTIONS
#===============================================================================

# Validate vm.args file exists and is readable
validate_vm_args_exists() {
    info "Validating vm.args.production exists..."

    if [[ ! -f "${VM_ARGS}" ]]; then
        error "vm.args.production not found at: ${VM_ARGS}"
        return 1
    fi

    if [[ ! -r "${VM_ARGS}" ]]; then
        error "vm.args.production is not readable"
        return 1
    fi

    success "vm.args.production exists and is readable"
    return 0
}

# Validate sys.config file exists and is readable
validate_sys_config_exists() {
    info "Validating sys.config.production exists..."

    if [[ ! -f "${SYS_CONFIG}" ]]; then
        error "sys.config.production not found at: ${SYS_CONFIG}"
        return 1
    fi

    if [[ ! -r "${SYS_CONFIG}" ]]; then
        error "sys.config.production is not readable"
        return 1
    fi

    success "sys.config.production exists and is readable"
    return 0
}

# Validate critical vm.args flags are present
validate_vm_args_flags() {
    info "Validating critical vm.args flags..."

    local flags=(
        "+smp"
        "+A"
        "+K"
        "+P"
        "+Q"
        "+MBacul"
        "+MBacgs"
        "+sbwt"
        "+zdbbl"
    )

    local all_passed=true

    for flag in "${flags[@]}"; do
        if grep -q "${flag}" "${VM_ARGS}"; then
            success "Found flag: ${flag}"
        else
            error "Missing flag: ${flag}"
            all_passed=false
        fi
    done

    if [[ "${all_passed}" == "true" ]]; then
        success "All critical vm.args flags present"
        return 0
    else
        error "Some vm.args flags are missing"
        return 1
    fi
}

# Validate process and port limits
validate_limits() {
    info "Validating process and port limits..."

    # Extract process limit
    local process_limit
    process_limit=$(grep "^\+P" "${VM_ARGS}" | awk '{print $2}')

    if [[ -n "${process_limit}" ]] && [[ "${process_limit}" -ge 262144 ]]; then
        success "Process limit ${process_limit} >= 262144"
    else
        warn "Process limit ${process_limit:-not set} < 262144 (recommended)"
    fi

    # Extract port limit
    local port_limit
    port_limit=$(grep "^+Q" "${VM_ARGS}" | awk '{print $2}')

    if [[ "${port_limit}" -ge 131072 ]]; then
        success "Port limit ${port_limit} >= 131072"
    else
        warn "Port limit ${port_limit} < 131072 (recommended)"
    fi

    # Extract ETS tables limit
    local ets_limit
    ets_limit=$(grep "ERL_MAX_ETS_TABLES" "${VM_ARGS}" | awk '{print $3}')

    if [[ "${ets_limit}" -ge 50000 ]]; then
        success "ETS tables limit ${ets_limit} >= 50000"
    else
        warn "ETS tables limit ${ets_limit} < 50000 (recommended)"
    fi

    return 0
}

# Validate async thread pool
validate_async_threads() {
    info "Validating async thread pool configuration..."

    local async_threads
    async_threads=$(grep "^+A" "${VM_ARGS}" | awk '{print $2}')

    if [[ "${async_threads}" -ge 64 ]]; then
        success "Async threads ${async_threads} >= 64"
    else
        warn "Async threads ${async_threads} < 64 (recommended for production)"
    fi

    return 0
}

# Validate SMP configuration
validate_smp() {
    info "Validating SMP configuration..."

    if grep -q "+smp auto" "${VM_ARGS}"; then
        success "SMP auto-detection enabled"
    else
        warn "SMP auto-detection not found"
    fi

    if grep -q "+subprocs" "${VM_ARGS}"; then
        success "SMP subprocess configuration present"
    else
        warn "SMP subprocess configuration not found"
    fi

    return 0
}

# Validate kernel poll
validate_kernel_poll() {
    info "Validating kernel poll configuration..."

    if grep -q "^+K true" "${VM_ARGS}"; then
        success "Kernel poll enabled (+K true)"
    else
        error "Kernel poll not enabled"
        return 1
    fi

    return 0
}

# Validate cgroups memory detection
validate_cgroups() {
    info "Validating cgroups memory detection..."

    local cgroups_flags=("+MBacul" "+Msbagf" "+MBacgs")
    local all_found=true

    for flag in "${cgroups_flags[@]}"; do
        if grep -q "${flag}" "${VM_ARGS}"; then
            success "Found cgroups flag: ${flag}"
        else
            error "Missing cgroups flag: ${flag}"
            all_found=false
        fi
    done

    if [[ "${all_found}" == "true" ]]; then
        success "All cgroups flags present"
    else
        error "Some cgroups flags are missing"
        return 1
    fi

    return 0
}

# Validate distribution settings
validate_distribution() {
    info "Validating distribution settings..."

    if grep -q "proto_dist inet_tls" "${VM_ARGS}"; then
        success "TLS distribution enabled"
    else
        warn "TLS distribution not enabled"
    fi

    if grep -q "inet_dist_listen_min" "${VM_ARGS}"; then
        success "Distribution port range configured"
    else
        warn "Distribution port range not configured"
    fi

    return 0
}

# Validate connection pooling in sys.config
validate_connection_pooling() {
    info "Validating connection pooling configuration..."

    if grep -q "connection_pooling" "${SYS_CONFIG}"; then
        success "Connection pooling configuration found"
    else
        error "Connection pooling configuration not found"
        return 1
    fi

    local pool_size
    pool_size=$(grep -A 10 "connection_pooling" "${SYS_CONFIG}" | grep "pool_size" | head -1 | sed 's/.*pool_size.*=>.*\([0-9]*\).*/\1/')

    if [[ "${pool_size}" -ge 50 ]]; then
        success "Connection pool size ${pool_size} >= 50"
    else
        warn "Connection pool size ${pool_size} < 50 (recommended)"
    fi

    return 0
}

# Validate rate limiting in sys.config
validate_rate_limiting() {
    info "Validating rate limiting configuration..."

    if grep -q "rate_limiting" "${SYS_CONFIG}"; then
        success "Rate limiting configuration found"
    else
        error "Rate limiting configuration not found"
        return 1
    fi

    local rate_limit
    rate_limit=$(grep -A 20 "rate_limiting" "${SYS_CONFIG}" | grep "max_messages_per_sec" | head -1 | sed 's/.*max_messages_per_sec.*=>.*\([0-9]*\).*/\1/')

    if [[ "${rate_limit}" -ge 100 ]]; then
        success "Rate limit ${rate_limit} msg/sec >= 100"
    else
        warn "Rate limit ${rate_limit} msg/sec < 100 (recommended)"
    fi

    return 0
}

# Validate caching configuration
validate_caching() {
    info "Validating caching configuration..."

    if grep -q "cache_enabled.*true" "${SYS_CONFIG}"; then
        success "Caching is enabled"
    else
        warn "Caching is not enabled"
    fi

    return 0
}

# Validate Erlang syntax of sys.config
validate_sys_config_syntax() {
    info "Validating sys.config Erlang syntax..."

    if command -v erl &> /dev/null; then
        if erl -noshell -eval "file:script(\"${SYS_CONFIG}\"), halt()." 2>&1 | grep -q "error"; then
            error "sys.config has syntax errors"
            return 1
        else
            success "sys.config syntax is valid"
        fi
    else
        warn "Erlang not found, skipping syntax validation"
    fi

    return 0
}

# Print configuration summary
print_summary() {
    info "Configuration Summary:"
    echo ""
    echo "vm.args.production:"
    grep -E "^\+[A-Za-z]" "${VM_ARGS}" | sort -u | while read -r flag; do
        echo "  ${flag}"
    done | head -20
    echo "  ..."
    echo ""

    echo "sys.config.production key settings:"
    grep -E "(pool_size|max_messages|cache_enabled)" "${SYS_CONFIG}" | grep -v "%%" | head -10
    echo ""
}

#===============================================================================
# MAIN VALIDATION FLOW
#===============================================================================
main() {
    echo "=============================================================================="
    echo "erlmcp v3 Production Configuration Validation"
    echo "=============================================================================="
    echo ""

    local exit_code=0

    # Run all validations
    validate_vm_args_exists || exit_code=1
    validate_sys_config_exists || exit_code=1
    validate_vm_args_flags || exit_code=1
    validate_limits || exit_code=1
    validate_async_threads || exit_code=1
    validate_smp || exit_code=1
    validate_kernel_poll || exit_code=1
    validate_cgroups || exit_code=1
    validate_distribution || exit_code=1
    validate_connection_pooling || exit_code=1
    validate_rate_limiting || exit_code=1
    validate_caching || exit_code=1
    validate_sys_config_syntax || exit_code=1

    echo ""
    echo "=============================================================================="

    if [[ ${exit_code} -eq 0 ]]; then
        success "All production configuration validations passed!"
        echo ""
        print_summary
    else
        error "Some validations failed. Please review the output above."
    fi

    echo "=============================================================================="

    return ${exit_code}
}

# Run main function
main "$@"
