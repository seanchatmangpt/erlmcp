#!/usr/bin/env bash
# Performance Agent - Detects performance regressions and suggests fixes
# Part of erlmcp Auto-Fix System

set -euo pipefail

ERROR_FILE="${1:-}"
ATTEMPT="${2:-1}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="logs/auto-fix"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [PERF] $*" | tee -a "$LOG_DIR/performance-fix.log"
}

# Parse benchmark results
parse_benchmark_regression() {
    local error_file="$1"

    # Extract regression information
    grep -E "regression|slower|degradation|-[0-9]+%" "$error_file" || true
}

# Analyze performance issues
analyze_performance() {
    local error_file="$1"

    log "${BLUE}Analyzing performance regressions (attempt $ATTEMPT)${NC}"

    local regressions=$(parse_benchmark_regression "$error_file")

    if [[ -z "$regressions" ]]; then
        log "${YELLOW}No clear regressions found in error file${NC}"
        regressions=$(cat "$error_file")
    fi

    log "Performance Issues:"
    echo "$regressions"

    # Generate suggestions
    local suggestion_file="$LOG_DIR/performance-suggestions-$(date +%s).txt"

    cat > "$suggestion_file" <<EOF
PERFORMANCE FIX SUGGESTIONS
===========================
Generated: $(date)
Attempt: $ATTEMPT

Performance Issues:
-------------------
$regressions

Common Performance Anti-Patterns:
----------------------------------
1. List concatenation in loops
   BAD:  List ++ [Element]
   GOOD: [Element | List], then lists:reverse/1

2. Unnecessary list traversals
   BAD:  length(List) > 0
   GOOD: List =/= []

3. Large messages between processes
   BAD:  Pid ! {large, Data}
   GOOD: Use ETS or shared references

4. Missing tail recursion
   BAD:  func(N) -> 1 + func(N-1)
   GOOD: func(N, Acc) -> func(N-1, Acc+1)

5. Inefficient pattern matching
   BAD:  Multiple guards, complex patterns
   GOOD: Simple patterns, early returns

6. ETS without indexes
   BAD:  ets:match on unindexed fields
   GOOD: Use proper keys and indexes

7. Spawning too many processes
   BAD:  spawn for every request
   GOOD: Use poolboy or process pools

8. Synchronous calls in hot paths
   BAD:  gen_server:call in tight loops
   GOOD: Batch calls or use cast

Optimization Steps:
-------------------
1. Profile with fprof or eprof:
   fprof:apply(Module, Function, Args),
   fprof:profile(),
   fprof:analyse().

2. Check process info:
   observer:start().
   %% Look for process queue lengths, memory usage

3. Use recon for production debugging:
   recon:proc_count(memory, 10).
   recon:proc_count(message_queue_len, 10).

4. Benchmark specific functions:
   timer:tc(Module, Function, Args).

5. Check ETS usage:
   ets:info(TableName).

Erlang-Specific Optimizations:
-------------------------------
1. Use binary pattern matching:
   <<Header:4/binary, Rest/binary>> = Data

2. Leverage BEAM optimizations:
   - Tail call optimization
   - Process dictionary for hot data (use sparingly)
   - Selective receive with refs

3. Reduce copying:
   - Use binaries for large data
   - Pass references, not values
   - Use ETS for shared state

4. Optimize gen_server:
   - Batch handle_info messages
   - Use handle_cast for non-critical updates
   - Implement proper backpressure

Next Steps:
-----------
1. Profile the code to find hotspots
2. Review common anti-patterns above
3. Apply relevant optimizations
4. Re-run benchmarks: make benchmark-quick
5. Compare results

Tools:
------
make observer       # Visual process inspector
fprof:apply/3      # Function profiler
recon:proc_count/2 # Top processes by metric
timer:tc/3         # Time function execution

Suggestion file: $suggestion_file
EOF

    log "Performance suggestions written to: $suggestion_file"
    cat "$suggestion_file"
}

# Main
main() {
    if [[ -z "$ERROR_FILE" ]] || [[ ! -f "$ERROR_FILE" ]]; then
        echo "Usage: $0 <error_file> [attempt_number]"
        exit 1
    fi

    log "${BLUE}Performance Agent starting (attempt $ATTEMPT)${NC}"

    analyze_performance "$ERROR_FILE"

    log "${YELLOW}⚠ Performance issues require manual optimization${NC}"
    log "${YELLOW}⚠ Review suggestions and apply optimizations${NC}"

    # Performance fixes cannot be automated
    exit 1
}

main "$@"
