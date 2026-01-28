#!/usr/bin/env bash
# Gate Failure Dispatcher - Monitors quality gates and dispatches fix agents
# Part of erlmcp Auto-Fix System (Jidoka 自働化)

set -euo pipefail

# Configuration
MAX_FIX_ATTEMPTS=3
LOG_DIR="logs/auto-fix"
STATE_FILE="$LOG_DIR/dispatcher-state.json"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Ensure log directory exists
mkdir -p "$LOG_DIR"

# Initialize state file if needed
init_state() {
    if [[ ! -f "$STATE_FILE" ]]; then
        cat > "$STATE_FILE" <<EOF
{
  "total_failures": 0,
  "total_fixes": 0,
  "total_escalations": 0,
  "active_attempts": {}
}
EOF
    fi
}

# Log with timestamp
log() {
    local level="$1"
    shift
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $*" | tee -a "$LOG_DIR/dispatcher.log"
}

# Update state
update_state() {
    local gate="$1"
    local action="$2"
    local count="${3:-1}"

    python3 <<EOF
import json
import sys

try:
    with open('$STATE_FILE', 'r') as f:
        state = json.load(f)

    if '$action' == 'attempt':
        if '$gate' not in state['active_attempts']:
            state['active_attempts']['$gate'] = 0
        state['active_attempts']['$gate'] += $count
    elif '$action' == 'fix':
        state['total_fixes'] += $count
        if '$gate' in state['active_attempts']:
            del state['active_attempts']['$gate']
    elif '$action' == 'escalate':
        state['total_escalations'] += $count
        if '$gate' in state['active_attempts']:
            del state['active_attempts']['$gate']
    elif '$action' == 'failure':
        state['total_failures'] += $count

    with open('$STATE_FILE', 'w') as f:
        json.dump(state, f, indent=2)
except Exception as e:
    print(f"Error updating state: {e}", file=sys.stderr)
    sys.exit(1)
EOF
}

# Get attempt count for a gate
get_attempt_count() {
    local gate="$1"
    python3 <<EOF
import json
import sys

try:
    with open('$STATE_FILE', 'r') as f:
        state = json.load(f)
    print(state['active_attempts'].get('$gate', 0))
except Exception as e:
    print("0")
EOF
}

# Dispatch appropriate fix agent based on failure type
dispatch_fix_agent() {
    local gate_type="$1"
    local error_file="$2"
    local attempt_num="$3"

    log "INFO" "${BLUE}Dispatching fix agent for $gate_type (attempt $attempt_num/$MAX_FIX_ATTEMPTS)${NC}"

    case "$gate_type" in
        compilation)
            "$SCRIPT_DIR/syntax-fix-agent.sh" "$error_file" "$attempt_num"
            ;;
        test)
            "$SCRIPT_DIR/test-fix-agent.sh" "$error_file" "$attempt_num"
            ;;
        coverage)
            "$SCRIPT_DIR/test-coverage-agent.sh" "$error_file" "$attempt_num"
            ;;
        dialyzer)
            "$SCRIPT_DIR/type-fix-agent.sh" "$error_file" "$attempt_num"
            ;;
        benchmark)
            "$SCRIPT_DIR/performance-agent.sh" "$error_file" "$attempt_num"
            ;;
        xref)
            "$SCRIPT_DIR/xref-fix-agent.sh" "$error_file" "$attempt_num"
            ;;
        *)
            log "ERROR" "${RED}Unknown gate type: $gate_type${NC}"
            return 1
            ;;
    esac
}

# Escalate to human intervention
escalate() {
    local gate_type="$1"
    local error_file="$2"
    local attempts="$3"

    log "ESCALATE" "${RED}Failed to auto-fix $gate_type after $attempts attempts${NC}"
    update_state "$gate_type" "escalate"

    # Create escalation report
    local escalation_file="$LOG_DIR/escalation-$(date +%s)-$gate_type.txt"
    cat > "$escalation_file" <<EOF
ESCALATION REPORT
=================
Gate: $gate_type
Timestamp: $(date)
Attempts: $attempts
Error File: $error_file

Error Details:
--------------
$(cat "$error_file" 2>/dev/null || echo "Error file not found")

Next Steps:
-----------
1. Review error details above
2. Apply manual fix
3. Run: rebar3 compile && rebar3 eunit && rebar3 dialyzer
4. Once fixed, run: rm -f $STATE_FILE (to reset attempt counter)

Escalation Log: $escalation_file
EOF

    log "ESCALATE" "Escalation report: $escalation_file"

    # Andon cord (行灯) - Stop the line
    echo -e "${RED}========================================${NC}"
    echo -e "${RED}   ANDON ALERT (行灯) - LINE STOPPED   ${NC}"
    echo -e "${RED}========================================${NC}"
    echo -e "${YELLOW}Gate:${NC} $gate_type"
    echo -e "${YELLOW}Escalation:${NC} $escalation_file"
    echo -e "${RED}========================================${NC}"
}

# Monitor and dispatch
monitor_gate() {
    local gate_type="$1"
    local check_command="$2"

    log "INFO" "Monitoring gate: $gate_type"

    # Create temporary error file
    local error_file="$LOG_DIR/$gate_type-errors-$(date +%s).txt"

    # Run check command and capture output
    if ! eval "$check_command" > "$error_file" 2>&1; then
        log "WARN" "${YELLOW}Quality gate failed: $gate_type${NC}"
        update_state "$gate_type" "failure"

        # Get current attempt count
        local attempt_count=$(get_attempt_count "$gate_type")
        ((attempt_count++))

        if [[ $attempt_count -le $MAX_FIX_ATTEMPTS ]]; then
            update_state "$gate_type" "attempt"

            if dispatch_fix_agent "$gate_type" "$error_file" "$attempt_count"; then
                log "INFO" "${GREEN}Fix agent succeeded for $gate_type${NC}"
                update_state "$gate_type" "fix"
                rm -f "$error_file"
                return 0
            else
                log "WARN" "${YELLOW}Fix agent failed for $gate_type (attempt $attempt_count)${NC}"
                return 1
            fi
        else
            escalate "$gate_type" "$error_file" "$attempt_count"
            return 1
        fi
    else
        log "INFO" "${GREEN}Quality gate passed: $gate_type${NC}"
        rm -f "$error_file"
        return 0
    fi
}

# Run all quality gates with auto-fix
run_all_gates() {
    local failed_gates=()

    log "INFO" "${BLUE}Starting quality gate monitoring${NC}"
    init_state

    # Gate 1: Compilation
    if ! monitor_gate "compilation" "TERM=dumb rebar3 compile"; then
        failed_gates+=("compilation")
    fi

    # Gate 2: Dialyzer (only if compilation passed)
    if [[ ${#failed_gates[@]} -eq 0 ]]; then
        if ! monitor_gate "dialyzer" "rebar3 dialyzer"; then
            failed_gates+=("dialyzer")
        fi
    fi

    # Gate 3: Xref (only if compilation passed)
    if [[ ${#failed_gates[@]} -eq 0 ]]; then
        if ! monitor_gate "xref" "rebar3 xref"; then
            failed_gates+=("xref")
        fi
    fi

    # Gate 4: Tests (only if compilation passed)
    if [[ ${#failed_gates[@]} -eq 0 ]]; then
        if ! monitor_gate "test" "rebar3 eunit"; then
            failed_gates+=("test")
        fi
    fi

    # Gate 5: Coverage (only if tests passed)
    if [[ ${#failed_gates[@]} -eq 0 ]]; then
        if ! monitor_gate "coverage" "rebar3 cover --verbose"; then
            failed_gates+=("coverage")
        fi
    fi

    # Report results
    echo ""
    echo "========================================="
    echo "Quality Gate Summary"
    echo "========================================="

    if [[ ${#failed_gates[@]} -eq 0 ]]; then
        echo -e "${GREEN}✅ All quality gates passed${NC}"
        return 0
    else
        echo -e "${RED}❌ Failed gates: ${failed_gates[*]}${NC}"
        echo -e "${YELLOW}See logs in: $LOG_DIR${NC}"
        return 1
    fi
}

# Main CLI
main() {
    case "${1:-help}" in
        monitor)
            if [[ $# -lt 3 ]]; then
                echo "Usage: $0 monitor <gate_type> <check_command>"
                exit 1
            fi
            monitor_gate "$2" "$3"
            ;;

        run-all)
            run_all_gates
            ;;

        status)
            if [[ -f "$STATE_FILE" ]]; then
                echo "Auto-Fix System Status:"
                echo "======================="
                cat "$STATE_FILE" | python3 -m json.tool
            else
                echo "No state file found. Run 'run-all' first."
            fi
            ;;

        reset)
            rm -f "$STATE_FILE"
            log "INFO" "State reset complete"
            ;;

        help|*)
            cat <<EOF
Gate Failure Dispatcher - Auto-Fix System (Jidoka 自働化)

Usage:
  $0 monitor <gate_type> <check_command>  Monitor single gate
  $0 run-all                              Run all quality gates
  $0 status                               Show current state
  $0 reset                                Reset attempt counters
  $0 help                                 Show this help

Gate Types:
  compilation  - Erlang compilation errors
  test         - EUnit test failures
  coverage     - Test coverage drops
  dialyzer     - Type specification errors
  benchmark    - Performance regressions
  xref         - Cross-reference errors

Examples:
  $0 run-all
  $0 monitor compilation "rebar3 compile"
  $0 status
  $0 reset

Logs: $LOG_DIR/
EOF
            ;;
    esac
}

main "$@"
