#!/usr/bin/env bash
# TCPS Andon Cord (行灯 - visible problem signaling)
# Purpose: Stop the line immediately when problems are detected
# Philosophy: Make problems visible, give workers authority to stop production

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Andon symbols
ANDON_RED="🔴"
ANDON_YELLOW="🟡"
ANDON_GREEN="🟢"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ANDON_LOG="${PROJECT_ROOT}/.tcps/andon_log.txt"
RECEIPT_DIR="${PROJECT_ROOT}/.tcps/receipts"

# ============================================================================
# Andon Status Display
# ============================================================================

show_andon_status() {
    echo ""
    echo -e "${CYAN}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║  TCPS Andon Board (行灯板)                                ║${NC}"
    echo -e "${CYAN}║  Stop-The-Line Status                                     ║${NC}"
    echo -e "${CYAN}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo ""

    if [[ ! -f "${ANDON_LOG}" ]]; then
        echo -e "${GREEN}${ANDON_GREEN} No issues detected - Line running normally${NC}"
        echo ""
        return 0
    fi

    # Parse recent Andon events
    local total_events=0
    local failure_events=0
    local recent_failures=()

    while IFS= read -r line; do
        if [[ "${line}" == "status: FAILURE" ]]; then
            ((failure_events++))
        fi
    done < "${ANDON_LOG}" || true

    total_events=$(grep -c "^---$" "${ANDON_LOG}" 2>/dev/null || echo "0")
    if [[ ${total_events} -gt 0 ]]; then
        total_events=$((total_events / 2))
    fi

    echo -e "${BLUE}Total Andon Events: ${total_events}${NC}"
    echo -e "${RED}Failure Events:     ${failure_events}${NC}"
    echo ""

    if [[ ${failure_events} -gt 0 ]]; then
        echo -e "${RED}╔═══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║  ${ANDON_RED} ACTIVE ISSUES - LINE STOPPED                     ║${NC}"
        echo -e "${RED}╚═══════════════════════════════════════════════════════════╝${NC}"
        echo ""

        # Show last 5 failures
        echo -e "${YELLOW}Recent Failures (last 5):${NC}"
        echo ""

        awk '
            /^---$/,/^---$/ {
                if (/^---$/) {
                    if (event_started) {
                        if (status == "FAILURE") {
                            print "  " timestamp
                            print "  Gate:   " gate
                            print "  Reason: " reason
                            print ""
                            count++
                            if (count >= 5) exit
                        }
                    }
                    event_started = !event_started
                    timestamp = gate = reason = status = ""
                } else {
                    if ($1 == "timestamp:") timestamp = substr($0, 12)
                    if ($1 == "gate:") gate = substr($0, 7)
                    if ($1 == "reason:") reason = substr($0, 9)
                    if ($1 == "status:") status = substr($0, 9)
                }
            }
        ' "${ANDON_LOG}" | tail -25

        echo -e "${YELLOW}Action Required:${NC}"
        echo "  1. Fix the root cause (do not bypass)"
        echo "  2. Run quality gates again"
        echo "  3. Clear Andon with: $0 --clear"
        echo ""
    else
        echo -e "${GREEN}${ANDON_GREEN} All gates passing - No active issues${NC}"
        echo ""
    fi

    # Show receipt chain status
    if [[ -d "${RECEIPT_DIR}" ]]; then
        local receipt_count=$(ls -1 "${RECEIPT_DIR}"/*.txt 2>/dev/null | wc -l)
        echo -e "${BLUE}Receipt Chain: ${receipt_count} receipts generated${NC}"

        local latest_receipt=$(ls -t "${RECEIPT_DIR}"/*.txt 2>/dev/null | head -1)
        if [[ -n "${latest_receipt}" ]]; then
            local receipt_hash=$(sha256sum "${latest_receipt}" | cut -d' ' -f1)
            echo -e "${CYAN}Latest Receipt: $(basename "${latest_receipt}")${NC}"
            echo -e "${CYAN}Hash: ${receipt_hash:0:16}...${NC}"
        fi
    fi

    echo ""
}

# ============================================================================
# Pull Andon Cord (Manual)
# ============================================================================

pull_cord() {
    local reason="${1:-Manual stop requested}"
    local gate="${2:-Manual}"

    echo ""
    echo -e "${RED}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║  ${ANDON_RED} ANDON CORD PULLED                                ║${NC}"
    echo -e "${RED}║  行灯 - Line Stopped                                      ║${NC}"
    echo -e "${RED}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${YELLOW}Gate:${NC}    ${gate}"
    echo -e "${YELLOW}Reason:${NC}  ${reason}"
    echo -e "${YELLOW}Time:${NC}    $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
    echo ""

    # Log event
    mkdir -p "$(dirname "${ANDON_LOG}")"
    cat >> "${ANDON_LOG}" << EOF
---
timestamp: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
work_order: ${WORK_ORDER_ID:-manual}
gate: ${gate}
status: FAILURE
reason: ${reason}
details: Manual Andon cord pull
---
EOF

    echo -e "${YELLOW}Production line stopped.${NC}"
    echo -e "${YELLOW}Fix the issue before resuming.${NC}"
    echo ""
}

# ============================================================================
# Clear Andon
# ============================================================================

clear_andon() {
    echo ""
    echo -e "${BLUE}Clearing Andon status...${NC}"

    if [[ -f "${ANDON_LOG}" ]]; then
        # Archive current log
        local archive_file="${ANDON_LOG}.$(date +%Y%m%d_%H%M%S)"
        mv "${ANDON_LOG}" "${archive_file}"
        echo -e "${GREEN}✓ Andon log archived: ${archive_file}${NC}"
    fi

    echo -e "${GREEN}✓ Andon cleared - Line ready to resume${NC}"
    echo ""
}

# ============================================================================
# Watch Mode (Real-time monitoring)
# ============================================================================

watch_andon() {
    echo ""
    echo -e "${CYAN}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║  TCPS Andon Monitor (行灯監視)                            ║${NC}"
    echo -e "${CYAN}║  Press Ctrl+C to exit                                     ║${NC}"
    echo -e "${CYAN}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo ""

    while true; do
        clear
        show_andon_status
        sleep 5
    done
}

# ============================================================================
# Main
# ============================================================================

usage() {
    cat << EOF
TCPS Andon Cord - Stop The Line Authority

Usage:
  $0 [command]

Commands:
  status             Show current Andon board status (default)
  pull [reason]      Pull Andon cord (stop the line)
  clear              Clear Andon status (resume production)
  watch              Real-time monitoring (refresh every 5s)
  help               Show this help

Examples:
  $0                           # Show status
  $0 pull "Test failure"       # Stop line with reason
  $0 clear                     # Clear and resume
  $0 watch                     # Live monitoring

Andon Philosophy:
  - Anyone can stop the line when problems occur
  - Problems must be made visible immediately
  - Stop and fix, don't work around
  - Build quality into the process

自働化 (Jidoka) - Automation with Human Touch
EOF
}

main() {
    local command="${1:-status}"

    case "${command}" in
        status)
            show_andon_status
            ;;
        pull)
            local reason="${2:-Manual stop requested}"
            pull_cord "${reason}"
            exit 1
            ;;
        clear)
            clear_andon
            ;;
        watch)
            watch_andon
            ;;
        help|--help|-h)
            usage
            ;;
        *)
            echo -e "${RED}Unknown command: ${command}${NC}"
            echo ""
            usage
            exit 1
            ;;
    esac
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
