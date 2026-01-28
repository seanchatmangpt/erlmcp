#!/usr/bin/env bash
# ============================================================================
# ERLMCP QUALITY WATCH
# ============================================================================
# Continuous monitoring of quality gates
# Runs quality-dashboard.sh every 30 seconds
# Shows real-time updates with alerts on failures
# ============================================================================

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DASHBOARD_SCRIPT="${SCRIPT_DIR}/quality-dashboard.sh"
REFRESH_INTERVAL="${REFRESH_INTERVAL:-30}"  # seconds
ENABLE_SOUND="${ENABLE_SOUND:-true}"
ENABLE_NOTIFICATION="${ENABLE_NOTIFICATION:-true}"

# ============================================================================
# ANSI CODES
# ============================================================================
CLEAR_SCREEN='\033[2J\033[H'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
BOLD='\033[1m'
NC='\033[0m'

# ============================================================================
# STATE TRACKING
# ============================================================================
PREVIOUS_STATUS=""
FAILURE_COUNT=0
WATCH_START_TIME=$(date +%s)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

beep() {
    if [[ "$ENABLE_SOUND" == "true" ]]; then
        # ASCII bell character
        echo -ne '\007'
    fi
}

notify() {
    local title="$1"
    local message="$2"
    local urgency="${3:-normal}"  # low, normal, critical

    if [[ "$ENABLE_NOTIFICATION" != "true" ]]; then
        return
    fi

    # macOS notification
    if command -v osascript &> /dev/null; then
        osascript -e "display notification \"$message\" with title \"$title\"" 2>/dev/null || true
    fi

    # Linux notification (notify-send)
    if command -v notify-send &> /dev/null; then
        notify-send -u "$urgency" "$title" "$message" 2>/dev/null || true
    fi
}

print_watch_header() {
    local current_time
    current_time=$(date '+%Y-%m-%d %H:%M:%S')

    local uptime
    uptime=$(($(date +%s) - WATCH_START_TIME))

    local hours=$((uptime / 3600))
    local minutes=$(((uptime % 3600) / 60))
    local seconds=$((uptime % 60))

    echo -e "${BOLD}${CYAN}╔═══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${CYAN}║${WHITE}              ERLMCP QUALITY GATE MONITOR                  ${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}╠═══════════════════════════════════════════════════════════════╣${NC}"
    echo -e "${BOLD}${CYAN}║${NC} Current Time: ${current_time}                      ${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}║${NC} Watch Uptime: ${hours}h ${minutes}m ${seconds}s                               ${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}║${NC} Refresh:      Every ${REFRESH_INTERVAL} seconds                             ${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}║${NC} Failures:     ${FAILURE_COUNT}                                          ${CYAN}║${NC}"
    echo -e "${BOLD}${CYAN}╚═══════════════════════════════════════════════════════════════╝${NC}"
}

handle_status_change() {
    local new_status="$1"

    if [[ -z "$PREVIOUS_STATUS" ]]; then
        PREVIOUS_STATUS="$new_status"
        return
    fi

    if [[ "$PREVIOUS_STATUS" == "pass" ]] && [[ "$new_status" == "fail" ]]; then
        # Quality gates degraded
        FAILURE_COUNT=$((FAILURE_COUNT + 1))
        beep
        beep
        beep
        notify "Quality Gates FAILED" "Build is broken! Check dashboard for details." "critical"
        echo -e "${RED}${BOLD}⚠️  ALERT: Quality gates have FAILED!${NC}" >&2
    elif [[ "$PREVIOUS_STATUS" == "fail" ]] && [[ "$new_status" == "pass" ]]; then
        # Quality gates recovered
        beep
        notify "Quality Gates RECOVERED" "Build is now passing." "normal"
        echo -e "${GREEN}${BOLD}✅  Quality gates have RECOVERED!${NC}" >&2
    fi

    PREVIOUS_STATUS="$new_status"
}

run_dashboard() {
    local output
    local status

    # Run dashboard and capture both output and exit status
    if output=$("$DASHBOARD_SCRIPT" 2>&1); then
        status="pass"
    else
        status="fail"
    fi

    # Display output
    echo "$output"

    # Handle status change
    handle_status_change "$status"

    return 0
}

print_watch_footer() {
    echo ""
    echo -e "${BLUE}${BOLD}───────────────────────────────────────────────────────────────${NC}"
    echo -e "${CYAN}Next refresh in ${REFRESH_INTERVAL} seconds... ${NC}(Press Ctrl+C to stop)"
    echo -e "${BLUE}${BOLD}───────────────────────────────────────────────────────────────${NC}"
}

cleanup() {
    echo ""
    echo -e "${YELLOW}Stopping quality watch...${NC}"

    local total_time=$(($(date +%s) - WATCH_START_TIME))
    local hours=$((total_time / 3600))
    local minutes=$(((total_time % 3600) / 60))
    local seconds=$((total_time % 60))

    echo -e "${CYAN}Watch duration: ${hours}h ${minutes}m ${seconds}s${NC}"
    echo -e "${CYAN}Total failures detected: ${FAILURE_COUNT}${NC}"
    echo -e "${GREEN}Goodbye!${NC}"
    exit 0
}

# ============================================================================
# MAIN WATCH LOOP
# ============================================================================

main() {
    # Check if dashboard script exists
    if [[ ! -f "$DASHBOARD_SCRIPT" ]]; then
        echo -e "${RED}ERROR: Dashboard script not found at $DASHBOARD_SCRIPT${NC}" >&2
        exit 1
    fi

    # Make dashboard script executable
    chmod +x "$DASHBOARD_SCRIPT"

    # Set up cleanup on exit
    trap cleanup SIGINT SIGTERM

    # Initial notification
    notify "Quality Watch Started" "Monitoring quality gates every ${REFRESH_INTERVAL}s" "low"

    # Main loop
    while true; do
        # Clear screen
        echo -e "${CLEAR_SCREEN}"

        # Print header
        print_watch_header

        # Run dashboard
        run_dashboard

        # Print footer
        print_watch_footer

        # Sleep
        sleep "$REFRESH_INTERVAL"
    done
}

# ============================================================================
# USAGE
# ============================================================================

usage() {
    cat <<EOF
ERLMCP Quality Watch - Continuous quality gate monitoring

USAGE:
    $(basename "$0") [OPTIONS]

OPTIONS:
    -i, --interval SECONDS   Refresh interval (default: 30)
    -s, --no-sound          Disable sound alerts
    -n, --no-notify         Disable desktop notifications
    -h, --help              Show this help

EXAMPLES:
    # Watch with default settings (30s refresh)
    $(basename "$0")

    # Watch with 60s refresh
    $(basename "$0") --interval 60

    # Watch without sound/notifications
    $(basename "$0") --no-sound --no-notify

ENVIRONMENT VARIABLES:
    REFRESH_INTERVAL        Refresh interval in seconds (default: 30)
    ENABLE_SOUND           Enable sound alerts (default: true)
    ENABLE_NOTIFICATION    Enable desktop notifications (default: true)
EOF
}

# ============================================================================
# ARGUMENT PARSING
# ============================================================================

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -i|--interval)
                REFRESH_INTERVAL="$2"
                shift 2
                ;;
            -s|--no-sound)
                ENABLE_SOUND="false"
                shift
                ;;
            -n|--no-notify)
                ENABLE_NOTIFICATION="false"
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                echo -e "${RED}ERROR: Unknown option: $1${NC}" >&2
                usage
                exit 1
                ;;
        esac
    done
}

# ============================================================================
# SCRIPT EXECUTION
# ============================================================================

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    parse_args "$@"
    main
fi
