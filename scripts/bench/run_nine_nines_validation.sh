#!/usr/bin/env bash
# ==============================================================================
# NINE-NINES PERFORMANCE VALIDATION SCRIPT
# ==============================================================================
# Executes comprehensive performance validation for nine-nines posture:
# - Part 1: Baseline benchmarking
# - Part 2: Overload profiling (100K msg/sec)
# - Part 3: Full validation against SLOs
#
# Usage:
#   ./scripts/bench/run_nine_nines_validation.sh [baseline|overload|full]
#   make benchmark-nine-nines
#
# Exit codes:
#   0: All SLOs met (nine-nines achieved)
#   1: SLO violation detected
#   2: Benchmark execution error
# ==============================================================================

set -euo pipefail

# Colors
readonly BLUE='\033[0;34m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly RED='\033[0;31m'
readonly BOLD='\033[1m'
readonly NC='\033[0m'

# Configuration
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
readonly RESULTS_DIR="$PROJECT_ROOT/bench/results"

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# ==============================================================================
# Helper Functions
# ==============================================================================

log_header() {
    echo ""
    echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${BLUE}  $1${NC}"
    echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""
}

log_section() {
    echo ""
    echo -e "${BLUE}▶ $1${NC}"
}

log_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

log_error() {
    echo -e "${RED}✗ $1${NC}"
}

# ==============================================================================
# Benchmark Execution
# ==============================================================================

run_benchmark() {
    local target=${1:-full}
    
    log_header "NINE-NINES PERFORMANCE VALIDATION"
    
    echo "Target: $target"
    echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
    echo ""
    
    # Compile
    log_section "Compiling erlmcp with optimizations"
    cd "$PROJECT_ROOT"
    
    if TERM=dumb rebar3 compile 2>&1 | tail -5; then
        log_success "Compilation complete"
    else
        log_error "Compilation failed"
        exit 2
    fi
    
    # Run benchmark
    log_section "Executing benchmark: $target"
    
    local temp_script=$(mktemp)
    cat > "$temp_script" << 'ERLEOF'
case code:ensure_loaded(erlmcp_bench_nine_nines) of
    {module, _} ->
        try
            Target = list_to_atom(os:getenv("BENCH_TARGET", "full")),
            Result = erlmcp_bench_nine_nines:run(Target),
            io:format("~nBenchmark completed: ~p~n", [Result]),
            case Result of
                ok -> halt(0);
                {error, slo_violations} -> halt(1);
                _ -> halt(2)
            end
        catch
            Class:Error:Stacktrace ->
                io:format("ERROR: ~p:~p~n~p~n", [Class, Error, Stacktrace]),
                halt(2)
        end;
    {error, Reason} ->
        io:format("ERROR: Failed to load erlmcp_bench_nine_nines: ~p~n", [Reason]),
        halt(2)
end.
ERLEOF
    
    local exit_code=0
    BENCH_TARGET="$target" erl \
        -pa _build/default/lib/*/ebin \
        -pa apps/*/ebin \
        -noshell \
        -eval "$(cat $temp_script)" \
        2>&1 || exit_code=$?
    
    rm -f "$temp_script"
    
    # Summary
    echo ""
    log_header "VALIDATION SUMMARY"
    
    if [ $exit_code -eq 0 ]; then
        echo -e "${BOLD}${GREEN}╔═══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BOLD}${GREEN}║  ✓ NINE-NINES POSTURE ACHIEVED                           ║${NC}"
        echo -e "${BOLD}${GREEN}║  All SLOs met, no regressions detected                   ║${NC}"
        echo -e "${BOLD}${GREEN}╚═══════════════════════════════════════════════════════════╝${NC}"
        log_success "System validated for nine-nines operation"
    elif [ $exit_code -eq 1 ]; then
        echo -e "${BOLD}${RED}╔═══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BOLD}${RED}║  ✗ SLO VIOLATIONS DETECTED                               ║${NC}"
        echo -e "${BOLD}${RED}║  Review benchmark output for failed SLOs                 ║${NC}"
        echo -e "${BOLD}${RED}╚═══════════════════════════════════════════════════════════╝${NC}"
        log_error "Performance does not meet nine-nines requirements"
    else
        echo -e "${BOLD}${RED}╔═══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BOLD}${RED}║  ✗ BENCHMARK EXECUTION ERROR                             ║${NC}"
        echo -e "${BOLD}${RED}╚═══════════════════════════════════════════════════════════╝${NC}"
        log_error "Benchmark failed to execute - check logs above"
    fi
    
    echo ""
    echo "Results saved to: $RESULTS_DIR/nine_nines_*.json"
    echo ""
    
    exit $exit_code
}

# ==============================================================================
# Entry Point
# ==============================================================================

TARGET="${1:-full}"
run_benchmark "$TARGET"
