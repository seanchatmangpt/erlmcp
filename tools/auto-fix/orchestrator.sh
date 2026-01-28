#!/usr/bin/env bash
# Auto-Fix Orchestrator - Coordinates multiple fix agents
# Part of erlmcp Auto-Fix System (Jidoka 自働化)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="logs/auto-fix"
MAX_ITERATIONS=5
CURRENT_ITERATION=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

mkdir -p "$LOG_DIR"

log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [ORCHESTRATOR] $*" | tee -a "$LOG_DIR/orchestrator.log"
}

# Banner
print_banner() {
    cat <<'EOF'
╔══════════════════════════════════════════════════════════╗
║         Auto-Fix Orchestrator (Jidoka 自働化)            ║
║     Automated Quality Gate Enforcement & Repair          ║
╚══════════════════════════════════════════════════════════╝
EOF
}

# Print iteration status
print_iteration() {
    local iteration="$1"
    echo ""
    echo -e "${CYAN}╔════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║       ITERATION $iteration/$MAX_ITERATIONS                   ║${NC}"
    echo -e "${CYAN}╔════════════════════════════════════════════╗${NC}"
    echo ""
}

# Run single quality gate
run_quality_gate() {
    local gate_name="$1"
    local command="$2"

    log "${BLUE}Running quality gate: $gate_name${NC}"

    local error_file="$LOG_DIR/${gate_name}-errors-$(date +%s).txt"

    if eval "$command" > "$error_file" 2>&1; then
        log "${GREEN}✅ $gate_name: PASSED${NC}"
        rm -f "$error_file"
        return 0
    else
        log "${RED}❌ $gate_name: FAILED${NC}"
        echo "$error_file"
        return 1
    fi
}

# Orchestrate fix cycle
orchestrate_fix_cycle() {
    local max_iterations="${1:-$MAX_ITERATIONS}"
    local all_gates_passed=false

    print_banner

    for ((iteration=1; iteration<=max_iterations; iteration++)); do
        print_iteration "$iteration"
        CURRENT_ITERATION=$iteration

        local gates_passed=0
        local gates_failed=0
        local failed_gates=()

        # Gate 1: Compilation
        log "${BLUE}=== Gate 1: Compilation ===${NC}"
        if error_file=$(run_quality_gate "compilation" "TERM=dumb rebar3 compile"); then
            ((gates_passed++))
        else
            ((gates_failed++))
            failed_gates+=("compilation:$error_file")

            # Dispatch syntax fix agent
            if "$SCRIPT_DIR/syntax-fix-agent.sh" "$error_file" "$iteration"; then
                log "${GREEN}Syntax fix succeeded, continuing...${NC}"
            else
                log "${YELLOW}Syntax fix failed, will retry${NC}"
            fi
        fi

        # Only continue if compilation passed
        if [[ $gates_failed -eq 0 ]]; then

            # Gate 2: Dialyzer
            log "${BLUE}=== Gate 2: Dialyzer ===${NC}"
            if error_file=$(run_quality_gate "dialyzer" "rebar3 dialyzer"); then
                ((gates_passed++))
            else
                ((gates_failed++))
                failed_gates+=("dialyzer:$error_file")

                # Dispatch type fix agent
                if "$SCRIPT_DIR/type-fix-agent.sh" "$error_file" "$iteration"; then
                    log "${GREEN}Type fix succeeded, continuing...${NC}"
                else
                    log "${YELLOW}Type fix failed, will retry${NC}"
                fi
            fi

            # Gate 3: XRef
            log "${BLUE}=== Gate 3: XRef ===${NC}"
            if error_file=$(run_quality_gate "xref" "rebar3 xref"); then
                ((gates_passed++))
            else
                ((gates_failed++))
                failed_gates+=("xref:$error_file")
                log "${YELLOW}XRef failed, may need manual review${NC}"
            fi

            # Gate 4: Tests
            log "${BLUE}=== Gate 4: Tests ===${NC}"
            if error_file=$(run_quality_gate "tests" "rebar3 eunit"); then
                ((gates_passed++))
            else
                ((gates_failed++))
                failed_gates+=("tests:$error_file")

                # Dispatch test fix agent
                if "$SCRIPT_DIR/test-fix-agent.sh" "$error_file" "$iteration"; then
                    log "${GREEN}Test fix succeeded, continuing...${NC}"
                else
                    log "${YELLOW}Test fix failed, will retry${NC}"
                fi
            fi

            # Gate 5: Coverage
            log "${BLUE}=== Gate 5: Coverage ===${NC}"
            if error_file=$(run_quality_gate "coverage" "rebar3 cover --verbose"); then
                ((gates_passed++))
            else
                ((gates_failed++))
                failed_gates+=("coverage:$error_file")
                log "${YELLOW}Coverage below threshold${NC}"
            fi
        fi

        # Report iteration results
        echo ""
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo -e "Iteration $iteration Results:"
        echo -e "  ${GREEN}Passed: $gates_passed${NC}"
        echo -e "  ${RED}Failed: $gates_failed${NC}"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

        # Check if all gates passed
        if [[ $gates_failed -eq 0 ]]; then
            all_gates_passed=true
            break
        fi

        # If last iteration, escalate
        if [[ $iteration -eq $max_iterations ]]; then
            log "${RED}Maximum iterations reached, escalating...${NC}"
            escalate_failures "${failed_gates[@]}"
            return 1
        fi

        # Brief pause between iterations
        sleep 1
    done

    # Final report
    echo ""
    echo "╔══════════════════════════════════════════════════════════╗"
    if [[ "$all_gates_passed" == "true" ]]; then
        echo -e "║  ${GREEN}✅ ALL QUALITY GATES PASSED${NC}                          ║"
        echo "╚══════════════════════════════════════════════════════════╝"
        log "${GREEN}Auto-fix orchestration completed successfully${NC}"
        return 0
    else
        echo -e "║  ${RED}❌ QUALITY GATES FAILED AFTER $CURRENT_ITERATION ITERATIONS${NC}     ║"
        echo "╚══════════════════════════════════════════════════════════╝"
        log "${RED}Auto-fix orchestration failed${NC}"
        return 1
    fi
}

# Escalate failures to human
escalate_failures() {
    local failed_gates=("$@")

    log "${RED}Escalating failures to human intervention${NC}"

    local escalation_file="$LOG_DIR/escalation-$(date +%s).txt"

    cat > "$escalation_file" <<EOF
AUTO-FIX ESCALATION REPORT
==========================
Generated: $(date)
Iterations: $CURRENT_ITERATION/$MAX_ITERATIONS

Failed Gates:
-------------
EOF

    for gate_info in "${failed_gates[@]}"; do
        IFS=':' read -r gate_name error_file <<< "$gate_info"

        cat >> "$escalation_file" <<EOF

Gate: $gate_name
Error File: $error_file
Errors:
$(cat "$error_file" 2>/dev/null || echo "Error file not found")

EOF
    done

    cat >> "$escalation_file" <<EOF

Next Steps:
-----------
1. Review errors above
2. Apply manual fixes
3. Run: make check
4. Reset auto-fix state: rm -rf logs/auto-fix/*.json

Commands:
---------
make check           # Run all quality gates
make console         # Start Erlang shell for debugging
rebar3 dialyzer      # Type checking
rebar3 eunit         # Run tests

Escalation Report: $escalation_file
EOF

    log "Escalation report written to: $escalation_file"

    # Display Andon alert
    echo ""
    echo -e "${RED}╔════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║     ANDON ALERT (行灯) - LINE STOPPED      ║${NC}"
    echo -e "${RED}╔════════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}Failed after $CURRENT_ITERATION iterations${NC}"
    echo -e "${YELLOW}Report: $escalation_file${NC}"
    echo -e "${RED}╚════════════════════════════════════════════╝${NC}"

    cat "$escalation_file"
}

# Validate fixes were applied
validate_fixes() {
    log "${BLUE}Validating all fixes...${NC}"

    # Full clean build
    log "Running clean build..."
    rebar3 clean

    if TERM=dumb rebar3 compile && \
       rebar3 dialyzer && \
       rebar3 xref && \
       rebar3 eunit && \
       rebar3 cover --verbose; then
        log "${GREEN}✅ All validations passed${NC}"
        return 0
    else
        log "${RED}❌ Validation failed${NC}"
        return 1
    fi
}

# Interactive mode
interactive_mode() {
    log "${BLUE}Starting interactive auto-fix mode${NC}"

    while true; do
        echo ""
        echo "Auto-Fix Interactive Menu:"
        echo "  1) Run orchestration"
        echo "  2) Run single gate"
        echo "  3) View status"
        echo "  4) Reset state"
        echo "  5) Validate fixes"
        echo "  6) Exit"
        echo -n "Select option: "

        read -r choice

        case "$choice" in
            1)
                orchestrate_fix_cycle
                ;;
            2)
                echo -n "Enter gate name (compilation/dialyzer/xref/tests/coverage): "
                read -r gate
                "$SCRIPT_DIR/gate-failure-dispatcher.sh" monitor "$gate" "rebar3 $gate"
                ;;
            3)
                "$SCRIPT_DIR/gate-failure-dispatcher.sh" status
                ;;
            4)
                "$SCRIPT_DIR/gate-failure-dispatcher.sh" reset
                ;;
            5)
                validate_fixes
                ;;
            6)
                log "Exiting interactive mode"
                exit 0
                ;;
            *)
                echo "Invalid option"
                ;;
        esac
    done
}

# Main CLI
main() {
    case "${1:-orchestrate}" in
        orchestrate)
            local iterations="${2:-$MAX_ITERATIONS}"
            orchestrate_fix_cycle "$iterations"
            ;;

        validate)
            validate_fixes
            ;;

        interactive)
            interactive_mode
            ;;

        help|*)
            cat <<EOF
Auto-Fix Orchestrator - Jidoka (自働化) System

Usage:
  $0 orchestrate [iterations]    Run full orchestration (default: 5 iterations)
  $0 validate                    Validate all fixes
  $0 interactive                 Interactive mode
  $0 help                        Show this help

Examples:
  $0 orchestrate              # Run with default 5 iterations
  $0 orchestrate 10           # Run with 10 iterations
  $0 validate                 # Validate fixes with clean build
  $0 interactive              # Start interactive menu

Quality Gates (in order):
  1. Compilation (TERM=dumb rebar3 compile)
  2. Dialyzer (rebar3 dialyzer)
  3. XRef (rebar3 xref)
  4. Tests (rebar3 eunit)
  5. Coverage (rebar3 cover)

Fix Agents:
  - syntax-fix-agent.sh    (compilation errors)
  - type-fix-agent.sh      (dialyzer warnings)
  - test-fix-agent.sh      (test failures)

Logs: $LOG_DIR/
EOF
            ;;
    esac
}

main "$@"
