#!/usr/bin/env bash
###############################################################################
# Dependency Vulnerability Scanner (FM-12)
#
# Automated CVE scanning for rebar.lock dependencies with CI/CD integration.
#
# Usage:
#   ./scripts/security/scan_dependencies.sh [OPTIONS]
#
# Options:
#   --lock-file PATH    Path to rebar.lock (default: ./rebar.lock)
#   --cache-dir PATH    NVD cache directory (default: /tmp/nvd_cache)
#   --output-dir PATH   Report output directory (default: ./reports)
#   --ci-mode           CI/CD mode (exit 1 on blocking vulnerabilities)
#   --help              Show this help message
#
# Exit Codes:
#   0 - No blocking vulnerabilities found
#   1 - Critical or High severity CVEs found (blocking)
#   2 - Script error
#
# Environment:
#   NVD_API_KEY         NVD API key (optional, increases rate limit)
#
###############################################################################

set -euo pipefail

# Default configuration
LOCK_FILE="${LOCK_FILE:-./rebar.lock}"
CACHE_DIR="${CACHE_DIR:-/tmp/nvd_cache}"
OUTPUT_DIR="${OUTPUT_DIR:-./reports}"
CI_MODE="${CI_MODE:-false}"

# Color output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
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

# Show usage
show_usage() {
    cat << EOF
Dependency Vulnerability Scanner (FM-12)

Usage: $0 [OPTIONS]

Options:
  --lock-file PATH    Path to rebar.lock (default: ./rebar.lock)
  --cache-dir PATH    NVD cache directory (default: /tmp/nvd_cache)
  --output-dir PATH   Report output directory (default: ./reports)
  --ci-mode           CI/CD mode (exit 1 on blocking vulnerabilities)
  --help              Show this help message

Environment Variables:
  NVD_API_KEY         NVD API key (optional, increases rate limit)

Exit Codes:
  0 - No blocking vulnerabilities found
  1 - Critical or High severity CVEs found (blocking)
  2 - Script error

EOF
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --lock-file)
                LOCK_FILE="$2"
                shift 2
                ;;
            --cache-dir)
                CACHE_DIR="$2"
                shift 2
                ;;
            --output-dir)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            --ci-mode)
                CI_MODE=true
                shift
                ;;
            --help)
                show_usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 2
                ;;
        esac
    done
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check if rebar.lock exists
    if [[ ! -f "$LOCK_FILE" ]]; then
        log_error "rebar.lock not found: $LOCK_FILE"
        exit 2
    fi

    # Check if Erlang is available
    if ! command -v erl &> /dev/null; then
        log_error "Erlang not found. Please install Erlang/OTP."
        exit 2
    fi

    # Check if rebar3 is available
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found. Please install rebar3."
        exit 2
    fi

    # Create output directory
    mkdir -p "$OUTPUT_DIR"

    # Create cache directory
    mkdir -p "$CACHE_DIR"

    log_success "Prerequisites checked"
}

# Download NVD data feeds (optional, for offline mode)
download_nvd_feeds() {
    log_info "Checking NVD data feeds..."

    # NVD feed is large and changes daily
    # For now, we query the API directly per dependency
    # Future: implement local feed mirroring for air-gapped environments

    log_info "Using NVD API (online mode)"
}

# Run Erlang vulnerability scanner
run_scanner() {
    log_info "Scanning dependencies in $LOCK_FILE..."

    # Generate timestamp for report
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    REPORT_FILE="$OUTPUT_DIR/cve_audit_${TIMESTAMP}.json"

    # Build Erlang command
    ERL_CMD="
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_validation),

    LockFile = \"$LOCK_FILE\",
    Opts = #{
        cache_dir => \"$CACHE_DIR\"
    },

    case erlmcp_vulnerability_scanner:scan_dependencies(LockFile, Opts) of
        {ok, Results} ->
            %% Save report
            ok = erlmcp_vulnerability_scanner:save_audit_trail(Results, \"$REPORT_FILE\"),

            %% Print summary
            Summary = erlmcp_vulnerability_scanner:generate_summary(Results),
            io:format(\"~s~n\", [Summary]),

            %% Exit with appropriate code
            ExitCode = maps:get(exit_code, Results),
            halt(ExitCode);
        {error, Reason} ->
            io:format(\"Scan failed: ~p~n\", [Reason]),
            halt(2)
    end.
    "

    # Run Erlang scanner
    if erl -noshell -pa _build/default/lib/*/ebin -eval "$ERL_CMD"; then
        SCANNER_EXIT_CODE=$?
    else
        SCANNER_EXIT_CODE=$?
    fi

    # Check if report was generated
    if [[ -f "$REPORT_FILE" ]]; then
        log_success "Report generated: $REPORT_FILE"

        # Parse results
        parse_results "$REPORT_FILE"
    else
        log_error "Report not generated"
        exit 2
    fi

    return $SCANNER_EXIT_CODE
}

# Parse scan results and display
parse_results() {
    local report_file="$1"

    # Check if jq is available for pretty printing
    if command -v jq &> /dev/null; then
        log_info "Scan Results:"
        echo "========================================"

        # Extract key metrics
        DEPS_SCANNED=$(jq -r '.dependencies_scanned' "$report_file")
        VULNS_FOUND=$(jq -r '.vulnerabilities_found' "$report_file")
        CRITICAL=$(jq -r '.critical // 0' "$report_file")
        HIGH=$(jq -r '.high // 0' "$report_file")
        MEDIUM=$(jq -r '.medium // 0' "$report_file")
        LOW=$(jq -r '.low // 0' "$report_file")

        echo "Dependencies scanned: $DEPS_SCANNED"
        echo "Vulnerabilities found: $VULNS_FOUND"
        echo ""

        if [[ "$CRITICAL" -gt 0 ]]; then
            echo -e "${RED}  Critical: $CRITICAL${NC}"
        else
            echo "  Critical: $CRITICAL"
        fi

        if [[ "$HIGH" -gt 0 ]]; then
            echo -e "${YELLOW}  High: $HIGH${NC}"
        else
            echo "  High: $HIGH"
        fi

        echo "  Medium: $MEDIUM"
        echo "  Low: $LOW"
        echo "========================================"

        # Show vulnerable dependencies
        if [[ "$VULNS_FOUND" -gt 0 ]]; then
            echo ""
            log_warn "Vulnerable dependencies:"
            jq -r '.vulnerable_dependencies[] | "  - \(.dependency):\(.version) (\(.cves | length) CVEs)"' "$report_file"
        fi
    else
        log_info "Install 'jq' for formatted output"
        cat "$report_file"
    fi
}

# Main execution
main() {
    parse_args "$@"

    log_info "=== Dependency Vulnerability Scanner (FM-12) ==="
    log_info "Lock file: $LOCK_FILE"
    log_info "Cache directory: $CACHE_DIR"
    log_info "Output directory: $OUTPUT_DIR"
    log_info "CI mode: $CI_MODE"
    echo ""

    check_prerequisites
    download_nvd_feeds

    # Run scanner
    if run_scanner; then
        SCANNER_EXIT=$?
    else
        SCANNER_EXIT=$?
    fi

    echo ""

    # Handle results based on mode
    if [[ "$CI_MODE" == "true" ]]; then
        if [[ $SCANNER_EXIT -eq 0 ]]; then
            log_success "No blocking vulnerabilities found"
            exit 0
        else
            log_error "Blocking vulnerabilities found (Critical or High severity)"
            log_error "Build BLOCKED - fix vulnerabilities before merging"
            exit 1
        fi
    else
        if [[ $SCANNER_EXIT -eq 0 ]]; then
            log_success "Scan completed successfully"
        else
            log_warn "Vulnerabilities found - review report"
        fi
        exit $SCANNER_EXIT
    fi
}

# Run main
main "$@"
