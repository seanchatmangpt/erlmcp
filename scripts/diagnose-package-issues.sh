#!/usr/bin/env bash
# diagnose-package-issues.sh - Master diagnostic tool for package manager issues
# Purpose: Run all diagnostic tests and provide recommendations
# Usage: ./diagnose-package-issues.sh [--quick|--full|--fix]

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
LOG_DIR="${PROJECT_ROOT}/log/diagnostics"
REPORT_FILE="${LOG_DIR}/diagnostic-report-$(date +%Y%m%d_%H%M%S).txt"

mkdir -p "${LOG_DIR}"

log() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $*" | tee -a "${REPORT_FILE}"
}

log_success() {
    echo -e "${GREEN}✓${NC} $*" | tee -a "${REPORT_FILE}"
}

log_error() {
    echo -e "${RED}✗${NC} $*" | tee -a "${REPORT_FILE}"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $*" | tee -a "${REPORT_FILE}"
}

log_section() {
    echo "" | tee -a "${REPORT_FILE}"
    echo -e "${CYAN}${BOLD}=========================================${NC}" | tee -a "${REPORT_FILE}"
    echo -e "${CYAN}${BOLD}$*${NC}" | tee -a "${REPORT_FILE}"
    echo -e "${CYAN}${BOLD}=========================================${NC}" | tee -a "${REPORT_FILE}"
}

# Test current rebar3 functionality
test_baseline() {
    log_section "1. BASELINE TEST - Current Setup"
    
    log "Testing current rebar3 configuration"
    
    # Test 1: Basic compile
    cd "${PROJECT_ROOT}"
    log "Attempting basic compile..."
    
    if timeout 120 bash -c "TERM=dumb rebar3 compile" >/dev/null 2>&1; then
        log_success "Baseline compile: WORKING"
        return 0
    else
        log_error "Baseline compile: FAILED"
        
        # Capture error
        log "Error details:"
        TERM=dumb rebar3 compile 2>&1 | tail -20 | tee -a "${REPORT_FILE}"
        
        return 1
    fi
}

# Test network connectivity
test_network() {
    log_section "2. NETWORK CONNECTIVITY"
    
    local -a endpoints=(
        "https://hex.pm"
        "https://repo.hex.pm"
        "https://github.com"
        "https://mirrors.aliyun.com"
    )
    
    local failures=0
    
    for endpoint in "${endpoints[@]}"; do
        log "Testing ${endpoint}"
        
        if curl -s -I -m 5 "${endpoint}" >/dev/null 2>&1; then
            log_success "${endpoint}: OK"
        else
            log_error "${endpoint}: FAILED"
            ((failures++))
        fi
    done
    
    if [ ${failures} -gt 0 ]; then
        log_warning "Network issues detected (${failures} endpoints failed)"
        return 1
    else
        log_success "All network endpoints accessible"
        return 0
    fi
}

# Test hex cache
test_hex_cache() {
    log_section "3. HEX CACHE STATUS"
    
    local hex_cache="${HOME}/.cache/rebar3/hex/default"
    
    if [ -d "${hex_cache}" ]; then
        local tarball_count=$(find "${hex_cache}/tarballs" -name "*.tar" 2>/dev/null | wc -l)
        local cache_size=$(du -sh "${hex_cache}" 2>/dev/null | awk '{print $1}')
        
        log_success "Hex cache exists: ${hex_cache}"
        log "  Packages: ${tarball_count}"
        log "  Size: ${cache_size}"
        
        if [ ${tarball_count} -gt 0 ]; then
            return 0
        else
            log_warning "Cache empty"
            return 1
        fi
    else
        log_error "Hex cache not found"
        return 1
    fi
}

# Run diagnostic scripts
run_hex_mirror_test() {
    log_section "4. HEX MIRROR TEST"
    
    log "Running hex-mirror-alternatives.sh"
    
    if [ -f "${SCRIPT_DIR}/hex-mirror-alternatives.sh" ]; then
        if bash "${SCRIPT_DIR}/hex-mirror-alternatives.sh" 2>&1 | tee -a "${REPORT_FILE}"; then
            log_success "Mirror test completed"
            return 0
        else
            log_error "Mirror test failed"
            return 1
        fi
    else
        log_error "Script not found: hex-mirror-alternatives.sh"
        return 1
    fi
}

run_git_fallback_test() {
    log_section "5. GIT FALLBACK TEST"
    
    log "Running git-fallback-test.sh (dry-run)"
    
    if [ -f "${SCRIPT_DIR}/git-fallback-test.sh" ]; then
        if bash "${SCRIPT_DIR}/git-fallback-test.sh" --dry-run 2>&1 | tee -a "${REPORT_FILE}"; then
            log_success "Git fallback test completed"
            return 0
        else
            log_error "Git fallback test failed"
            return 1
        fi
    else
        log_error "Script not found: git-fallback-test.sh"
        return 1
    fi
}

# Generate diagnostic report
generate_report() {
    log_section "DIAGNOSTIC SUMMARY"
    
    # System info
    log "System Information:"
    log "  OS: $(uname -s) $(uname -r)"
    log "  Date: $(date)"
    log "  User: $(whoami)"
    
    # Erlang info
    if command -v erl >/dev/null 2>&1; then
        log "  Erlang: $(erl -version 2>&1 | head -1)"
    else
        log_warning "  Erlang: Not found"
    fi
    
    # Rebar3 info
    if command -v rebar3 >/dev/null 2>&1; then
        log "  Rebar3: $(rebar3 version 2>&1 | head -1)"
    else
        log_warning "  Rebar3: Not found"
    fi
    
    echo ""
    
    # Test results summary
    log "Test Results:"
    
    local total=0
    local passed=0
    local failed=0
    
    # Count results from report
    passed=$(grep -c "✓" "${REPORT_FILE}" || echo 0)
    failed=$(grep -c "✗" "${REPORT_FILE}" || echo 0)
    total=$((passed + failed))
    
    log "  Total checks: ${total}"
    log_success "  Passed: ${passed}"
    if [ ${failed} -gt 0 ]; then
        log_error "  Failed: ${failed}"
    fi
}

# Provide recommendations
provide_recommendations() {
    log_section "RECOMMENDATIONS"
    
    # Check for common issues
    local has_network_issues=false
    local has_cache_issues=false
    local has_compile_issues=false
    
    if grep -q "NETWORK.*FAILED" "${REPORT_FILE}"; then
        has_network_issues=true
    fi
    
    if grep -q "Hex cache.*not found" "${REPORT_FILE}"; then
        has_cache_issues=true
    fi
    
    if grep -q "Baseline compile: FAILED" "${REPORT_FILE}"; then
        has_compile_issues=true
    fi
    
    # Provide targeted recommendations
    if [ "${has_compile_issues}" = true ]; then
        log "Issue: Compilation failures detected"
        log ""
        log "Recommended actions:"
        log "  1. Try alternative mirrors:"
        log "     ./scripts/hex-mirror-alternatives.sh"
        log ""
        log "  2. Use git-based dependencies:"
        log "     ./scripts/git-fallback-test.sh"
        log "     cp rebar.config.git rebar.config"
        log ""
        log "  3. Manual package download:"
        log "     ./scripts/manual-download.sh"
        log ""
        log "  4. Offline mode:"
        log "     ./scripts/offline-package-test.sh --create-repo"
    fi
    
    if [ "${has_network_issues}" = true ]; then
        log "Issue: Network connectivity problems"
        log ""
        log "Recommended actions:"
        log "  1. Check proxy settings:"
        log "     echo \$HTTP_PROXY \$HTTPS_PROXY"
        log ""
        log "  2. Try offline installation:"
        log "     ./scripts/offline-package-test.sh --create-repo"
        log ""
        log "  3. Use pre-downloaded bundle:"
        log "     ./scripts/offline-package-test.sh --bundle"
    fi
    
    if [ "${has_cache_issues}" = true ]; then
        log "Issue: Hex cache empty or missing"
        log ""
        log "Recommended actions:"
        log "  1. Manual download all packages:"
        log "     ./scripts/manual-download.sh"
        log ""
        log "  2. Use git dependencies:"
        log "     ./scripts/git-fallback-test.sh"
    fi
    
    # OTP recommendations
    log ""
    log "Alternative OTP management:"
    log "  ./scripts/kerl-otp-manager.sh --install 28.3.1"
    log "  source ~/.kerl/installations/28.3.1/activate"
}

# Quick diagnostic
quick_diagnostic() {
    log_section "QUICK DIAGNOSTIC"
    
    test_baseline
    test_network
    test_hex_cache
    
    generate_report
    provide_recommendations
}

# Full diagnostic
full_diagnostic() {
    log_section "FULL DIAGNOSTIC"
    
    test_baseline
    test_network
    test_hex_cache
    run_hex_mirror_test
    run_git_fallback_test
    
    generate_report
    provide_recommendations
}

# Auto-fix mode
auto_fix() {
    log_section "AUTO-FIX MODE"
    
    log_warning "This will attempt to automatically fix package issues"
    log_warning "It may modify rebar.config and download packages"
    
    read -p "Continue? (y/n) " -n 1 -r
    echo
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        log "Cancelled"
        return 1
    fi
    
    # Step 1: Test baseline
    if ! test_baseline; then
        log "Baseline test failed, attempting fixes..."
        
        # Step 2: Try mirrors
        log "Trying alternative mirrors..."
        if bash "${SCRIPT_DIR}/hex-mirror-alternatives.sh"; then
            log_success "Mirror test completed"
            
            # Retry compile
            if test_baseline; then
                log_success "Fixed! Using alternative mirror"
                return 0
            fi
        fi
        
        # Step 3: Try git fallback
        log "Trying git-based dependencies..."
        if bash "${SCRIPT_DIR}/git-fallback-test.sh"; then
            # Backup and switch config
            cp rebar.config rebar.config.hex.bak
            cp rebar.config.git rebar.config
            
            # Retry compile
            if test_baseline; then
                log_success "Fixed! Using git dependencies"
                return 0
            else
                # Restore config
                mv rebar.config.hex.bak rebar.config
            fi
        fi
        
        # Step 4: Manual download
        log "Trying manual package download..."
        if bash "${SCRIPT_DIR}/manual-download.sh"; then
            if test_baseline; then
                log_success "Fixed! Using manually downloaded packages"
                return 0
            fi
        fi
        
        log_error "All fix attempts failed"
        log "Please review recommendations in the report"
        return 1
    else
        log_success "No issues detected, system working normally"
        return 0
    fi
}

# Main
main() {
    echo -e "${BOLD}${CYAN}"
    cat << 'BANNER'
╔═══════════════════════════════════════════════════════════╗
║   erlmcp Package Manager Diagnostic Tool                 ║
║   Debug rebar3/hex download issues                       ║
╚═══════════════════════════════════════════════════════════╝
BANNER
    echo -e "${NC}"
    
    log "Starting diagnostic at $(date)"
    log "Report: ${REPORT_FILE}"
    echo ""
    
    local mode="quick"
    
    while [ $# -gt 0 ]; do
        case "$1" in
            --quick)
                mode="quick"
                shift
                ;;
            --full)
                mode="full"
                shift
                ;;
            --fix)
                mode="fix"
                shift
                ;;
            *)
                echo "Usage: $0 [--quick|--full|--fix]"
                echo ""
                echo "Modes:"
                echo "  --quick  Quick diagnostic (default)"
                echo "  --full   Full diagnostic with all tests"
                echo "  --fix    Attempt automatic fixes"
                exit 1
                ;;
        esac
    done
    
    case "${mode}" in
        quick)
            quick_diagnostic
            ;;
        full)
            full_diagnostic
            ;;
        fix)
            auto_fix
            ;;
    esac
    
    echo ""
    log_section "COMPLETE"
    log "Diagnostic report saved to:"
    log "  ${REPORT_FILE}"
    log ""
    log "Next steps:"
    log "  1. Review the report above"
    log "  2. Follow the recommendations"
    log "  3. Run individual scripts as needed"
    log ""
    log "Available scripts:"
    log "  - hex-mirror-alternatives.sh  (Test alternative mirrors)"
    log "  - git-fallback-test.sh        (Git-based dependencies)"
    log "  - manual-download.sh          (Manual package download)"
    log "  - offline-package-test.sh     (Offline installation)"
    log "  - kerl-otp-manager.sh         (Alternative OTP management)"
}

main "$@"
