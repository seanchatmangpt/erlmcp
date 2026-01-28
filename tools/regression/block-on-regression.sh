#!/usr/bin/env bash
# block-on-regression.sh - Blocks merge on critical regressions
# Part of TCPS Quality Gate System (行灯 - Andon)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
METRICS_DIR="${PROJECT_ROOT}/.metrics"
REGRESSION_REPORT="${METRICS_DIR}/regression-report.json"
ALLOWLIST_FILE="${SCRIPT_DIR}/allowlist.json"

# Colors
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

# Check if regression is in allowlist
check_allowlist() {
    local metric=$1
    local severity=$2
    
    if [[ ! -f "${ALLOWLIST_FILE}" ]]; then
        return 1
    fi
    
    local allowed=$(jq -r --arg metric "$metric" --arg severity "$severity" \
        '.allowed_regressions[] | select(.metric == $metric and .severity == $severity) | .justification' \
        "${ALLOWLIST_FILE}" 2>/dev/null || echo "")
    
    if [[ -n "$allowed" ]]; then
        log_info "Regression allowed: ${allowed}"
        return 0
    fi
    
    return 1
}

# Enforce quality gates
enforce_quality_gates() {
    log_info "Enforcing quality gates..."
    
    if [[ ! -f "${REGRESSION_REPORT}" ]]; then
        log_error "No regression report found. Run detect-regression.sh first."
        return 1
    fi
    
    local severity=$(jq -r '.severity' "${REGRESSION_REPORT}")
    local regressions=$(jq -r '.regressions | length' "${REGRESSION_REPORT}")
    
    if [[ "$severity" == "none" ]] || [[ $regressions -eq 0 ]]; then
        log_success "✅ QUALITY GATE PASSED - No regressions detected"
        return 0
    fi
    
    log_info "Analyzing ${regressions} regression(s) with severity: ${severity}"
    echo ""
    
    local blocking_found=false
    local warnings_found=false
    
    # Check each regression
    for ((i=0; i<regressions; i++)); do
        local metric=$(jq -r ".regressions[$i].metric" "${REGRESSION_REPORT}")
        local reg_severity=$(jq -r ".regressions[$i].severity" "${REGRESSION_REPORT}")
        local message=$(jq -r ".regressions[$i].message" "${REGRESSION_REPORT}")
        local baseline=$(jq -r ".regressions[$i].baseline" "${REGRESSION_REPORT}")
        local current=$(jq -r ".regressions[$i].current" "${REGRESSION_REPORT}")
        
        case "$reg_severity" in
            critical)
                if check_allowlist "$metric" "critical"; then
                    log_warn "⚠️ CRITICAL REGRESSION (ALLOWED): ${message}"
                    log_info "   Baseline: ${baseline}, Current: ${current}"
                    warnings_found=true
                else
                    log_error "🚫 BLOCKING: ${message}"
                    log_error "   Baseline: ${baseline}, Current: ${current}"
                    log_error "   This critical regression blocks merge!"
                    blocking_found=true
                fi
                ;;
            high)
                if check_allowlist "$metric" "high"; then
                    log_warn "⚠️ HIGH REGRESSION (ALLOWED): ${message}"
                    log_info "   Baseline: ${baseline}, Current: ${current}"
                    warnings_found=true
                else
                    log_error "🚫 BLOCKING: ${message}"
                    log_error "   Baseline: ${baseline}, Current: ${current}"
                    log_error "   This high severity regression blocks merge!"
                    blocking_found=true
                fi
                ;;
            medium)
                log_warn "⚠️ WARNING: ${message}"
                log_info "   Baseline: ${baseline}, Current: ${current}"
                log_warn "   Requires justification but does not block merge"
                warnings_found=true
                ;;
            low)
                log_info "ℹ️ LOW: ${message}"
                log_info "   Baseline: ${baseline}, Current: ${current}"
                ;;
        esac
        echo ""
    done
    
    # Print summary and exit accordingly
    echo ""
    log_info "=========================================="
    log_info "  QUALITY GATE SUMMARY"
    log_info "=========================================="
    echo ""
    
    if [[ "$blocking_found" == "true" ]]; then
        log_error "❌ QUALITY GATE FAILED - MERGE BLOCKED"
        echo ""
        log_error "Critical or high severity regressions detected."
        log_error "To proceed, either:"
        log_error "  1. Fix the regressions"
        log_error "  2. Add to allowlist with justification (tools/regression/allowlist.json)"
        log_error "  3. Request override from tech lead"
        echo ""
        return 1
    elif [[ "$warnings_found" == "true" ]]; then
        log_warn "⚠️ QUALITY GATE PASSED WITH WARNINGS"
        echo ""
        log_warn "Medium severity regressions require justification."
        log_warn "Document the reason in your PR description."
        echo ""
        return 0
    else
        log_success "✅ QUALITY GATE PASSED"
        echo ""
        log_success "All quality checks passed!"
        echo ""
        return 0
    fi
}

# Generate human-readable report
generate_report() {
    local output_file="${1:-${METRICS_DIR}/regression-summary.txt}"
    
    if [[ ! -f "${REGRESSION_REPORT}" ]]; then
        echo "No regression report available" > "$output_file"
        return
    fi
    
    {
        echo "========================================"
        echo "  ERLMCP REGRESSION REPORT"
        echo "========================================"
        echo ""
        echo "Timestamp: $(jq -r '.timestamp' "${REGRESSION_REPORT}")"
        echo "Baseline:  $(jq -r '.baseline_commit' "${REGRESSION_REPORT}")"
        echo "Current:   $(jq -r '.current_commit' "${REGRESSION_REPORT}")"
        echo "Severity:  $(jq -r '.severity' "${REGRESSION_REPORT}")"
        echo ""
        echo "Regressions:"
        echo ""
        
        local count=$(jq -r '.regressions | length' "${REGRESSION_REPORT}")
        
        if [[ $count -eq 0 ]]; then
            echo "  ✅ No regressions detected!"
        else
            for ((i=0; i<count; i++)); do
                local metric=$(jq -r ".regressions[$i].metric" "${REGRESSION_REPORT}")
                local severity=$(jq -r ".regressions[$i].severity" "${REGRESSION_REPORT}")
                local message=$(jq -r ".regressions[$i].message" "${REGRESSION_REPORT}")
                local baseline=$(jq -r ".regressions[$i].baseline" "${REGRESSION_REPORT}")
                local current=$(jq -r ".regressions[$i].current" "${REGRESSION_REPORT}")
                
                echo "  [$severity] $metric"
                echo "    Message: $message"
                echo "    Baseline: $baseline"
                echo "    Current: $current"
                echo ""
            done
        fi
        
        echo "========================================"
    } > "$output_file"
    
    log_info "Report generated: $output_file"
}

# Main execution
main() {
    log_info "Starting quality gate enforcement for erlmcp..."
    
    cd "${PROJECT_ROOT}"
    
    # Run detection if report doesn't exist
    if [[ ! -f "${REGRESSION_REPORT}" ]]; then
        log_info "Running regression detection first..."
        "${SCRIPT_DIR}/detect-regression.sh" || true
    fi
    
    enforce_quality_gates
    local exit_code=$?
    
    generate_report
    
    return $exit_code
}

main "$@"
