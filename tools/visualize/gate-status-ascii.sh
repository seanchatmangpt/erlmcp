#!/usr/bin/env bash
###############################################################################
# gate-status-ascii.sh - ASCII art visualization of quality gates
#
# Displays all 8 TCPS quality gates with visual status indicators.
# Designed for terminal/CLI monitoring and CI/CD pipelines.
#
# Usage:
#   ./gate-status-ascii.sh [--watch] [--json-file=FILE]
#
# Options:
#   --watch         Refresh every 2 seconds
#   --json-file     Read from JSON file instead of running gates
#   --compact       Compact output (no box drawing)
#   --color         Force colored output
#   --no-color      Disable colored output
#
# Examples:
#   ./gate-status-ascii.sh
#   ./gate-status-ascii.sh --watch
#   ./gate-status-json.sh | jq . > status.json && ./gate-status-ascii.sh --json-file=status.json
#
# TCPS Integration:
#   - 自働化 (Jidoka): Stop-the-line visualization
#   - 看板 (Kanban): WIP limits displayed
#   - ポカヨケ (Poka-yoke): Error-proof status indicators
###############################################################################

set -euo pipefail

# Colors (ANSI escape codes)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
GRAY='\033[0;90m'
NC='\033[0m' # No Color

# Box drawing characters
BOX_TL="┌"
BOX_TR="┐"
BOX_BL="└"
BOX_BR="┘"
BOX_H="─"
BOX_V="│"
BOX_VR="├"
BOX_VL="┤"
BOX_HU="┴"
BOX_HD="┬"

# Status icons
ICON_PASS="✅"
ICON_FAIL="❌"
ICON_RUNNING="⏳"
ICON_WARNING="⚠️"
ICON_BLOCKED="🚫"
ICON_PENDING="⏸️"

# Configuration
USE_COLOR=true
COMPACT_MODE=false
WATCH_MODE=false
JSON_FILE=""

# Detect if output is a TTY
if [ ! -t 1 ]; then
    USE_COLOR=false
fi

# Parse arguments
for arg in "$@"; do
    case $arg in
        --watch)
            WATCH_MODE=true
            ;;
        --json-file=*)
            JSON_FILE="${arg#*=}"
            ;;
        --compact)
            COMPACT_MODE=true
            ;;
        --color)
            USE_COLOR=true
            ;;
        --no-color)
            USE_COLOR=false
            ;;
        --help)
            sed -n '2,/^$/p' "$0" | sed 's/^# //'
            exit 0
            ;;
    esac
done

# Color wrapper
color() {
    if [ "$USE_COLOR" = true ]; then
        echo -ne "${1}"
    fi
}

# Check for JSON parser (jq or python)
HAS_JQ=false
JSON_PARSER=""

if command -v jq &> /dev/null; then
    HAS_JQ=true
    JSON_PARSER="jq"
elif command -v python3 &> /dev/null; then
    HAS_JQ=false
    JSON_PARSER="python3"
else
    echo "Error: Neither jq nor python3 found. Please install one." >&2
    exit 1
fi

# JSON query function
json_query() {
    local json_data="$1"
    local query="$2"

    if [ "$JSON_PARSER" = "jq" ]; then
        echo "$json_data" | jq -r "$query" 2>/dev/null || echo ""
    elif [ "$JSON_PARSER" = "python3" ]; then
        echo "$json_data" | python3 -c "
import json, sys
data = json.load(sys.stdin)
try:
    $query
except: print('')
" 2>/dev/null || echo ""
    fi
}

# Get specific gate field
json_get_gate_field() {
    local json_data="$1"
    local gate_id="$2"
    local field="$3"

    if [ "$JSON_PARSER" = "jq" ]; then
        echo "$json_data" | jq -r ".gates[] | select(.id == $gate_id) | .$field" 2>/dev/null || echo ""
    elif [ "$JSON_PARSER" = "python3" ]; then
        # Use temp file to avoid stdin issues
        local tmp_json="/tmp/gate_json_$$_${gate_id}.json"
        echo "$json_data" > "$tmp_json"
        python3 <<PYEOF 2>/dev/null || echo ""
import json
with open('$tmp_json') as f:
    data = json.load(f)
gates = [g for g in data.get('gates', []) if g.get('id') == $gate_id]
if gates:
    print(gates[0].get('$field', ''))
PYEOF
        rm -f "$tmp_json"
    fi
}

# Get gate status from JSON
get_gate_status() {
    local json_data="$1"
    local gate_id="$2"

    local status=$(json_get_gate_field "$json_data" "$gate_id" "status")
    [ -z "$status" ] && status="unknown"
    echo "$status"
}

get_gate_details() {
    local json_data="$1"
    local gate_id="$2"

    json_get_gate_field "$json_data" "$gate_id" "details"
}

get_gate_name() {
    local json_data="$1"
    local gate_id="$2"

    local name=$(json_get_gate_field "$json_data" "$gate_id" "name")
    [ -z "$name" ] && name="Gate $gate_id"
    echo "$name"
}

# Get status icon
get_status_icon() {
    local status="$1"

    case "$status" in
        pass) echo "$ICON_PASS" ;;
        fail) echo "$ICON_FAIL" ;;
        running) echo "$ICON_RUNNING" ;;
        warning) echo "$ICON_WARNING" ;;
        blocked) echo "$ICON_BLOCKED" ;;
        pending) echo "$ICON_PENDING" ;;
        *) echo "❓" ;;
    esac
}

# Get status color
get_status_color() {
    local status="$1"

    case "$status" in
        pass) echo "$GREEN" ;;
        fail) echo "$RED" ;;
        running) echo "$YELLOW" ;;
        warning) echo "$YELLOW" ;;
        blocked) echo "$MAGENTA" ;;
        pending) echo "$GRAY" ;;
        *) echo "$WHITE" ;;
    esac
}

# Draw horizontal line
draw_line() {
    local width=60
    echo -n "$BOX_H"
    for ((i=1; i<width; i++)); do
        echo -n "$BOX_H"
    done
    echo ""
}

# Draw header
draw_header() {
    local width=60

    echo ""
    echo -n "$BOX_TL"
    draw_line
    echo -n "$BOX_TR"
    echo ""

    color "$CYAN"
    echo -n "$BOX_V"
    printf " %-58s " "🏭 erlmcp Quality Gates - TCPS Dashboard"
    echo "$BOX_V"
    color "$NC"

    echo -n "$BOX_VR"
    draw_line
    echo -n "$BOX_VL"
    echo ""
}

# Draw gate row
draw_gate() {
    local gate_id="$1"
    local json_data="$2"

    local status=$(get_gate_status "$json_data" "$gate_id")
    local name=$(get_gate_name "$json_data" "$gate_id")
    local details=$(get_gate_details "$json_data" "$gate_id")
    local icon=$(get_status_icon "$status")
    local status_color=$(get_status_color "$status")

    # Truncate details if too long
    if [ ${#details} -gt 35 ]; then
        details="${details:0:32}..."
    fi

    echo -n "$BOX_V "
    color "$status_color"
    printf "%-2s" "[$icon]"
    color "$NC"
    printf " %d. %-20s " "$gate_id" "$name"
    color "$GRAY"
    printf "%-20s" "$details"
    color "$NC"
    echo " $BOX_V"
}

# Draw footer with overall status
draw_footer() {
    local json_data="$1"

    if [ "$JSON_PARSER" = "jq" ]; then
        local overall=$(echo "$json_data" | jq -r '.overall' 2>/dev/null)
        local pass_rate=$(echo "$json_data" | jq -r '.pass_rate // 0' 2>/dev/null)
        local passed=$(echo "$json_data" | jq -r '.passed_count // 0' 2>/dev/null)
        local failed=$(echo "$json_data" | jq -r '.failed_count // 0' 2>/dev/null)
        local timestamp=$(echo "$json_data" | jq -r '.timestamp' 2>/dev/null)
    else
        local tmp_json="/tmp/gate_json_footer_$$.json"
        echo "$json_data" > "$tmp_json"
        local overall=$(python3 -c "import json; print(json.load(open('$tmp_json')).get('overall',''))" 2>/dev/null)
        local pass_rate=$(python3 -c "import json; print(json.load(open('$tmp_json')).get('pass_rate',0))" 2>/dev/null)
        local passed=$(python3 -c "import json; print(json.load(open('$tmp_json')).get('passed_count',0))" 2>/dev/null)
        local failed=$(python3 -c "import json; print(json.load(open('$tmp_json')).get('failed_count',0))" 2>/dev/null)
        local timestamp=$(python3 -c "import json; print(json.load(open('$tmp_json')).get('timestamp',''))" 2>/dev/null)
        rm -f "$tmp_json"
    fi

    # Set defaults if empty
    [ -z "$overall" ] && overall="unknown"
    [ -z "$pass_rate" ] && pass_rate="0"
    [ -z "$passed" ] && passed="0"
    [ -z "$failed" ] && failed="0"
    [ -z "$timestamp" ] && timestamp="Unknown"

    echo -n "$BOX_VR"
    draw_line
    echo -n "$BOX_VL"
    echo ""

    # Overall status
    local overall_icon=$(get_status_icon "$overall")
    local overall_color=$(get_status_color "$overall")

    echo -n "$BOX_V "
    color "$WHITE"
    echo -n "Overall: "
    color "$overall_color"
    printf "%-8s %s" "$overall_icon" "${overall^^}"
    color "$NC"
    printf "%38s" ""
    echo " $BOX_V"

    # Metrics
    echo -n "$BOX_V "
    color "$GRAY"
    printf "Pass Rate: "
    color "$NC"
    if [ "$pass_rate" -ge 90 ]; then
        color "$GREEN"
    elif [ "$pass_rate" -ge 70 ]; then
        color "$YELLOW"
    else
        color "$RED"
    fi
    printf "%3d%%" "$pass_rate"
    color "$GRAY"
    printf "  │  Passed: "
    color "$GREEN"
    printf "%d" "$passed"
    color "$GRAY"
    printf "  │  Failed: "
    color "$RED"
    printf "%d" "$failed"
    color "$NC"
    printf "%19s" ""
    echo " $BOX_V"

    # Timestamp
    echo -n "$BOX_V "
    color "$GRAY"
    printf "Last updated: %-43s" "$timestamp"
    color "$NC"
    echo " $BOX_V"

    echo -n "$BOX_BL"
    draw_line
    echo -n "$BOX_BR"
    echo ""
    echo ""
}

# Draw compact version
draw_compact() {
    local json_data="$1"

    echo ""
    color "$CYAN"
    echo "🏭 erlmcp Quality Gates"
    color "$NC"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    for gate_id in {1..8}; do
        local status=$(get_gate_status "$json_data" "$gate_id")
        local name=$(get_gate_name "$json_data" "$gate_id")
        local details=$(get_gate_details "$json_data" "$gate_id")
        local icon=$(get_status_icon "$status")
        local status_color=$(get_status_color "$status")

        color "$status_color"
        printf "%s %d. %-25s" "$icon" "$gate_id" "$name"
        color "$GRAY"
        echo "$details"
        color "$NC"
    done

    if [ "$JSON_PARSER" = "jq" ]; then
        local overall=$(echo "$json_data" | jq -r '.overall' 2>/dev/null)
    else
        local tmp_json="/tmp/gate_json_compact_$$.json"
        echo "$json_data" > "$tmp_json"
        local overall=$(python3 -c "import json; print(json.load(open('$tmp_json')).get('overall',''))" 2>/dev/null)
        rm -f "$tmp_json"
    fi
    [ -z "$overall" ] && overall="unknown"
    local overall_icon=$(get_status_icon "$overall")

    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    color "$(get_status_color "$overall")"
    echo "Overall: $overall_icon ${overall^^}"
    color "$NC"
    echo ""
}

# Main display function
display_status() {
    local json_data="$1"

    # Clear screen if in watch mode
    if [ "$WATCH_MODE" = true ]; then
        clear
    fi

    if [ "$COMPACT_MODE" = true ]; then
        draw_compact "$json_data"
    else
        draw_header

        for gate_id in {1..8}; do
            draw_gate "$gate_id" "$json_data"
        done

        draw_footer "$json_data"
    fi
}

# Main execution
main() {
    local json_data

    if [ -n "$JSON_FILE" ]; then
        # Read from file
        if [ ! -f "$JSON_FILE" ]; then
            echo "Error: JSON file not found: $JSON_FILE" >&2
            exit 1
        fi
        json_data=$(cat "$JSON_FILE")
    else
        # Run gate status script
        local script_dir=$(dirname "$0")
        json_data=$("$script_dir/gate-status-json.sh" 2>/dev/null)
    fi

    display_status "$json_data"

    # Watch mode
    if [ "$WATCH_MODE" = true ]; then
        while true; do
            sleep 2

            if [ -n "$JSON_FILE" ]; then
                json_data=$(cat "$JSON_FILE")
            else
                json_data=$("$script_dir/gate-status-json.sh" 2>/dev/null)
            fi

            display_status "$json_data"
        done
    fi
}

main "$@"
