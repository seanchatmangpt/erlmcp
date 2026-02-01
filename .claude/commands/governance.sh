#!/usr/bin/env bash
#
# Governance System CLI - erlmcp v3.0.0
#
# Usage:
#   governance hooks       - List active hooks
#   governance receipts    - Show recent session receipts
#   governance verify      - Run verification subagent manually
#   governance help        - Show this help message
#
# References:
#   - CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md
#   - AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md (WO-010)
#   - .claude/settings.json (governance configuration)

set -euo pipefail

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Colors for output
BLUE='\033[0;34m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Functions

show_help() {
    cat <<EOF
${BOLD}${BLUE}Governance System CLI${NC}

${BOLD}USAGE:${NC}
    governance <command> [options]

${BOLD}COMMANDS:${NC}
    hooks               List active hooks from .claude/settings.json
    receipts [N]        Show recent session receipts (default: 10 most recent)
    verify              Run verification subagent manually
    status              Show governance system status
    validate            Validate governance configuration
    help                Show this help message

${BOLD}EXAMPLES:${NC}
    governance hooks                 # List all active hooks
    governance receipts              # Show 10 most recent receipts
    governance receipts 20           # Show 20 most recent receipts
    governance verify                # Run manual verification
    governance status                # Show system status
    governance validate              # Validate settings.json

${BOLD}HOOK LIFECYCLE:${NC}
    SessionStart → PreToolUse* → PostToolUse* → Stop → SessionEnd

${BOLD}FILES:${NC}
    .claude/settings.json            Governance configuration
    .claude/hooks/*.sh               Hook implementations
    .erlmcp/receipts/*.json          Session receipts
    .erlmcp/transcripts/*.log        Session transcripts

${BOLD}REFERENCES:${NC}
    CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md    Governance specification
    AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md Work order details (WO-010)
    DEVELOPMENT.md                          Development guide

EOF
}

list_hooks() {
    echo -e "${BOLD}${CYAN}Active Hooks${NC}"
    echo ""

    if [ ! -f "$PROJECT_ROOT/.claude/settings.json" ]; then
        echo -e "${RED}Error: .claude/settings.json not found${NC}"
        exit 1
    fi

    # Parse hooks from settings.json using jq
    if ! command -v jq &> /dev/null; then
        echo -e "${YELLOW}Warning: jq not found. Showing raw hook configuration.${NC}"
        echo ""
        grep -A 5 '"hooks"' "$PROJECT_ROOT/.claude/settings.json" | head -20
        return
    fi

    echo -e "${BOLD}SessionStart Hooks:${NC}"
    jq -r '.hooks.SessionStart[]?.hooks[]? | "  - \(.type): \(.command // .prompt[0:80])"' \
        "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "  (none)"
    echo ""

    echo -e "${BOLD}PreToolUse Hooks:${NC}"
    jq -r '.hooks.PreToolUse[]? | "  - Matcher: \(.matcher)\n    Command: \(.hooks[]?.command // .hooks[]?.prompt[0:80])"' \
        "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "  (none)"
    echo ""

    echo -e "${BOLD}PostToolUse Hooks:${NC}"
    jq -r '.hooks.PostToolUse[]? | "  - Matcher: \(.matcher)\n    Command: \(.hooks[]?.command) (async: \(.hooks[]?.async // false))"' \
        "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "  (none)"
    echo ""

    echo -e "${BOLD}Stop Hooks:${NC}"
    jq -r '.hooks.Stop[]?.hooks[]? | "  - \(.type): \(.subagent // "inline")"' \
        "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "  (none)"
    echo ""

    echo -e "${BOLD}SessionEnd Hooks:${NC}"
    jq -r '.hooks.SessionEnd[]?.hooks[]? | "  - \(.type): \(.command)"' \
        "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "  (none)"
    echo ""
}

show_receipts() {
    local count="${1:-10}"

    echo -e "${BOLD}${CYAN}Recent Session Receipts${NC}"
    echo ""

    local receipts_dir="$PROJECT_ROOT/.erlmcp/receipts"

    if [ ! -d "$receipts_dir" ]; then
        echo -e "${YELLOW}No receipts directory found: $receipts_dir${NC}"
        return
    fi

    local receipt_count=$(find "$receipts_dir" -name "*.json" 2>/dev/null | wc -l)

    if [ "$receipt_count" -eq 0 ]; then
        echo -e "${YELLOW}No receipts found in $receipts_dir${NC}"
        return
    fi

    echo -e "${BOLD}Total receipts: $receipt_count${NC}"
    echo -e "${BOLD}Showing: $count most recent${NC}"
    echo ""

    # Table header
    printf "%-30s %-15s %-12s %-10s %-10s\n" "Timestamp" "Session ID" "OTP Version" "Cost" "Status"
    printf "%s\n" "$(printf '%.0s-' {1..100})"

    # List receipts (most recent first)
    find "$receipts_dir" -name "*.json" -type f -printf "%T@ %p\n" 2>/dev/null \
        | sort -rn \
        | head -n "$count" \
        | while read -r timestamp filepath; do
            if [ -f "$filepath" ] && command -v jq &> /dev/null; then
                local session_id=$(jq -r '.session_id // "unknown"' "$filepath" 2>/dev/null)
                local ts=$(jq -r '.timestamp // "unknown"' "$filepath" 2>/dev/null)
                local otp=$(jq -r '.otp_version // "unknown"' "$filepath" 2>/dev/null)
                local cost=$(jq -r '.cost_estimate // "$0.00"' "$filepath" 2>/dev/null)
                local compile_status=$(jq -r '.quality_gates.compile // "unknown"' "$filepath" 2>/dev/null)
                local eunit_status=$(jq -r '.quality_gates.eunit // "unknown"' "$filepath" 2>/dev/null)

                # Determine overall status
                local status="PASS"
                if [[ "$compile_status" == "fail" ]] || [[ "$eunit_status" == "fail" ]]; then
                    status="FAIL"
                elif [[ "$compile_status" == "unknown" ]]; then
                    status="UNKNOWN"
                fi

                # Color code status
                local status_color="$GREEN"
                if [ "$status" == "FAIL" ]; then
                    status_color="$RED"
                elif [ "$status" == "UNKNOWN" ]; then
                    status_color="$YELLOW"
                fi

                # Truncate session ID to first 15 chars
                local session_id_short="${session_id:0:15}"

                printf "%-30s %-15s %-12s %-10s ${status_color}%-10s${NC}\n" \
                    "$ts" "$session_id_short" "$otp" "$cost" "$status"
            else
                echo -e "${YELLOW}  Skipping $filepath (jq not available or invalid JSON)${NC}"
            fi
        done

    echo ""
}

run_verification() {
    echo -e "${BOLD}${CYAN}Running Manual Verification${NC}"
    echo ""

    echo -e "${BOLD}Verification Checklist:${NC}"
    echo "  1. OTP version >= 28.3.1"
    echo "  2. Compilation succeeds"
    echo "  3. Unit tests pass"
    echo ""

    # Check 1: OTP version
    echo -e "${BOLD}[1/3] Checking OTP version...${NC}"
    local otp_version
    otp_version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)])' -s init stop 2>/dev/null || echo "unknown")

    if [[ "$otp_version" =~ ^28 ]] || [[ "$otp_version" =~ ^2[89] ]]; then
        echo -e "  ${GREEN}✓ OTP version: $otp_version (>= 28.3.1)${NC}"
    else
        echo -e "  ${RED}✗ OTP version: $otp_version (need >= 28.3.1)${NC}"
        echo -e "${RED}Verification FAILED${NC}"
        return 1
    fi

    # Check 2: Compilation
    echo -e "${BOLD}[2/3] Checking compilation...${NC}"
    if TERM=dumb rebar3 compile > /tmp/governance_compile.log 2>&1; then
        echo -e "  ${GREEN}✓ Compilation: PASS${NC}"
    else
        echo -e "  ${RED}✗ Compilation: FAIL${NC}"
        echo -e "${RED}See /tmp/governance_compile.log for details${NC}"
        echo -e "${RED}Verification FAILED${NC}"
        return 1
    fi

    # Check 3: Unit tests
    echo -e "${BOLD}[3/3] Running unit tests...${NC}"
    if rebar3 eunit --application=erlmcp_core > /tmp/governance_eunit.log 2>&1; then
        echo -e "  ${GREEN}✓ Unit tests: PASS${NC}"
    else
        echo -e "  ${RED}✗ Unit tests: FAIL${NC}"
        echo -e "${RED}See /tmp/governance_eunit.log for details${NC}"
        echo -e "${RED}Verification FAILED${NC}"
        return 1
    fi

    echo ""
    echo -e "${BOLD}${GREEN}✓ All verification checks PASSED${NC}"
    echo ""

    return 0
}

show_status() {
    echo -e "${BOLD}${CYAN}Governance System Status${NC}"
    echo ""

    # Check settings.json
    echo -e "${BOLD}Configuration:${NC}"
    if [ -f "$PROJECT_ROOT/.claude/settings.json" ]; then
        echo -e "  ${GREEN}✓ .claude/settings.json exists${NC}"

        if command -v jq &> /dev/null; then
            local hook_count
            hook_count=$(jq '[.hooks | to_entries[] | .value] | length' "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "0")
            echo -e "    Hook categories: $hook_count"

            local subagent_count
            subagent_count=$(jq '.subagents | keys | length' "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null || echo "0")
            echo -e "    Subagents: $subagent_count"
        fi
    else
        echo -e "  ${RED}✗ .claude/settings.json missing${NC}"
    fi
    echo ""

    # Check hooks directory
    echo -e "${BOLD}Hook Files:${NC}"
    if [ -d "$PROJECT_ROOT/.claude/hooks" ]; then
        local hook_count
        hook_count=$(find "$PROJECT_ROOT/.claude/hooks" -name "*.sh" -executable 2>/dev/null | wc -l)
        echo -e "  ${GREEN}✓ .claude/hooks directory exists${NC}"
        echo -e "    Executable hooks: $hook_count"
    else
        echo -e "  ${RED}✗ .claude/hooks directory missing${NC}"
    fi
    echo ""

    # Check receipts
    echo -e "${BOLD}Receipts:${NC}"
    if [ -d "$PROJECT_ROOT/.erlmcp/receipts" ]; then
        local receipt_count
        receipt_count=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" 2>/dev/null | wc -l)
        echo -e "  ${GREEN}✓ .erlmcp/receipts directory exists${NC}"
        echo -e "    Total receipts: $receipt_count"

        if [ "$receipt_count" -gt 0 ]; then
            local latest
            latest=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f -printf "%T@ %p\n" 2>/dev/null \
                | sort -rn | head -1 | awk '{print $2}')

            if [ -n "$latest" ] && command -v jq &> /dev/null; then
                local latest_ts
                latest_ts=$(jq -r '.timestamp // "unknown"' "$latest" 2>/dev/null)
                echo -e "    Latest receipt: $latest_ts"
            fi
        fi
    else
        echo -e "  ${YELLOW}⚠ .erlmcp/receipts directory missing (will be created on first session)${NC}"
    fi
    echo ""

    # OTP version
    echo -e "${BOLD}Environment:${NC}"
    local otp_version
    otp_version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)])' -s init stop 2>/dev/null || echo "unknown")

    if [[ "$otp_version" =~ ^28 ]] || [[ "$otp_version" =~ ^2[89] ]]; then
        echo -e "  ${GREEN}✓ OTP version: $otp_version (>= 28.3.1)${NC}"
    else
        echo -e "  ${YELLOW}⚠ OTP version: $otp_version (recommended: >= 28.3.1)${NC}"
    fi
    echo ""
}

validate_configuration() {
    echo -e "${BOLD}${CYAN}Validating Governance Configuration${NC}"
    echo ""

    local errors=0

    # Validate settings.json exists
    echo -e "${BOLD}[1/4] Checking settings.json...${NC}"
    if [ ! -f "$PROJECT_ROOT/.claude/settings.json" ]; then
        echo -e "  ${RED}✗ .claude/settings.json not found${NC}"
        ((errors++))
    else
        echo -e "  ${GREEN}✓ .claude/settings.json exists${NC}"

        # Validate JSON syntax
        if command -v jq &> /dev/null; then
            if jq empty "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null; then
                echo -e "  ${GREEN}✓ Valid JSON syntax${NC}"
            else
                echo -e "  ${RED}✗ Invalid JSON syntax${NC}"
                ((errors++))
            fi
        else
            echo -e "  ${YELLOW}⚠ jq not available, skipping JSON validation${NC}"
        fi
    fi
    echo ""

    # Validate hook files
    echo -e "${BOLD}[2/4] Checking hook files...${NC}"
    if [ ! -d "$PROJECT_ROOT/.claude/hooks" ]; then
        echo -e "  ${RED}✗ .claude/hooks directory not found${NC}"
        ((errors++))
    else
        local hook_errors=0

        # Check each hook file referenced in settings.json
        if command -v jq &> /dev/null && [ -f "$PROJECT_ROOT/.claude/settings.json" ]; then
            while IFS= read -r hook_file; do
                local full_path="$PROJECT_ROOT/$hook_file"
                if [ ! -f "$full_path" ]; then
                    echo -e "  ${RED}✗ Hook file missing: $hook_file${NC}"
                    ((hook_errors++))
                elif [ ! -x "$full_path" ]; then
                    echo -e "  ${YELLOW}⚠ Hook not executable: $hook_file${NC}"
                    ((hook_errors++))
                else
                    echo -e "  ${GREEN}✓ $hook_file${NC}"
                fi
            done < <(jq -r '.. | .command? // empty | select(startswith("./.claude/hooks/"))' \
                "$PROJECT_ROOT/.claude/settings.json" 2>/dev/null | sort -u)
        fi

        if [ "$hook_errors" -gt 0 ]; then
            ((errors += hook_errors))
        fi
    fi
    echo ""

    # Validate receipts directory
    echo -e "${BOLD}[3/4] Checking receipts directory...${NC}"
    if [ ! -d "$PROJECT_ROOT/.erlmcp/receipts" ]; then
        echo -e "  ${YELLOW}⚠ .erlmcp/receipts directory missing (will be created on first session)${NC}"
        mkdir -p "$PROJECT_ROOT/.erlmcp/receipts" 2>/dev/null && \
            echo -e "  ${GREEN}✓ Created .erlmcp/receipts directory${NC}"
    else
        echo -e "  ${GREEN}✓ .erlmcp/receipts directory exists${NC}"
    fi
    echo ""

    # Validate schema (if schema file exists)
    echo -e "${BOLD}[4/4] Checking receipt schema...${NC}"
    if [ -f "$PROJECT_ROOT/.claude/hooks/receipt.schema.json" ]; then
        echo -e "  ${GREEN}✓ Receipt schema exists${NC}"
    else
        echo -e "  ${YELLOW}⚠ Receipt schema not found (optional)${NC}"
    fi
    echo ""

    # Summary
    if [ "$errors" -eq 0 ]; then
        echo -e "${BOLD}${GREEN}✓ Configuration validation PASSED${NC}"
        echo ""
        return 0
    else
        echo -e "${BOLD}${RED}✗ Configuration validation FAILED ($errors errors)${NC}"
        echo ""
        return 1
    fi
}

# Main command dispatcher

COMMAND="${1:-help}"

case "$COMMAND" in
    hooks)
        list_hooks
        ;;
    receipts)
        show_receipts "${2:-10}"
        ;;
    verify)
        run_verification
        ;;
    status)
        show_status
        ;;
    validate)
        validate_configuration
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo -e "${RED}Error: Unknown command '$COMMAND'${NC}"
        echo ""
        show_help
        exit 1
        ;;
esac
