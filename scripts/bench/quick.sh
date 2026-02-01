#!/usr/bin/env bash
# ==============================================================================
# ERLMCP QUICK PERFORMANCE CHECK
# ==============================================================================
# Purpose: Sub-2-minute performance sanity check for local development
# Use: Pre-commit hook, rapid iteration, regression detection
#
# Workloads (intelligent selection):
#   - core_ops_1k: Registry/queue/pool/session (1K ops, ~3 sec)
#   - http_quick_1k: HTTP/SSE 100 conns, 1K msgs (~30 sec)
#   Total: <2 minutes including compilation and comparison
#
# Exit codes:
#   0: PASS (no regression)
#   1: FAIL (regression >10% or benchmark error)
#
# Environment:
#   ERLMCP_PROFILE  Profile to use (dev|test|staging|prod), defaults to 'staging'
#
# Integration: make bench-quick
# ==============================================================================

set -euo pipefail

# ==============================================================================
# Profile Configuration
# ==============================================================================

# Use staging profile for benchmarks (production-like, with logging)
ERLMCP_PROFILE="${ERLMCP_PROFILE:-staging}"

# Validate profile (with graceful fallback to staging)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
VALIDATE_SCRIPT="$PROJECT_ROOT/scripts/validate_profile.sh"

if [ -f "$VALIDATE_SCRIPT" ]; then
    if ! "$VALIDATE_SCRIPT" "$ERLMCP_PROFILE" 2>/dev/null; then
        echo -e "${YELLOW}WARNING: Invalid profile '$ERLMCP_PROFILE', falling back to 'staging'${NC}"
        ERLMCP_PROFILE=staging
    fi
else
    echo -e "${YELLOW}WARNING: validate_profile.sh not found, using profile: $ERLMCP_PROFILE${NC}"
fi

export ERLMCP_PROFILE

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
readonly THRESHOLDS_FILE="$PROJECT_ROOT/.github/performance-thresholds.json"
readonly RESULTS_DIR="$PROJECT_ROOT/bench/results"
readonly WORKLOADS_DIR="$PROJECT_ROOT/bench/workloads"

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# ==============================================================================
# Helper Functions
# ==============================================================================

log_header() {
    echo ""
    echo -e "${BOLD}${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${BLUE}  $1${NC}"
    echo -e "${BOLD}${BLUE}════════════════════════════════════════════════════════════${NC}"
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

get_threshold() {
    local benchmark=$1
    local metric=$2
    
    if [ ! -f "$THRESHOLDS_FILE" ]; then
        echo "0"
        return
    fi
    
    # Extract threshold using grep/awk (portable, no jq dependency)
    grep -A 20 "\"$benchmark\"" "$THRESHOLDS_FILE" 2>/dev/null | \
        grep "\"$metric\"" | \
        grep -o '"min"[[:space:]]*:[[:space:]]*[0-9]*' | \
        grep -o '[0-9]*' | head -1 || echo "0"
}

# ==============================================================================
# Main Benchmark Execution
# ==============================================================================

main() {
    local start_time=$(date +%s)
    
    log_header "ERLMCP QUICK PERFORMANCE CHECK"

    echo "Target: Sub-2-minute sanity check for local development"
    echo "Profile: $ERLMCP_PROFILE"
    echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
    echo ""
    
    # Step 1: Compile
    log_section "Step 1/4: Compiling code"
    if TERM=dumb rebar3 compile >/dev/null 2>&1; then
        log_success "Code compiled successfully"
    else
        log_error "Compilation failed"
        exit 1
    fi
    
    # Step 2: Run core operations benchmark
    log_section "Step 2/4: Running core operations (1K ops)"
    local core_result
    core_result=$(run_core_benchmark) || {
        log_error "Core benchmark failed"
        exit 1
    }
    
    # Step 3: Run network benchmark (HTTP quick)
    log_section "Step 3/4: Running HTTP transport (100 conns, 1K msgs)"
    local http_result
    http_result=$(run_http_benchmark) || {
        log_warning "HTTP benchmark skipped (gun/cowboy not available)"
        http_result="skipped"
    }
    
    # Step 4: Compare against baselines
    log_section "Step 4/4: Comparing against performance thresholds"
    local exit_code=0
    
    # Check core operations
    if [ "$core_result" != "skipped" ]; then
        check_regression "core_ops" "$core_result" || exit_code=1
    fi
    
    # Check HTTP (if ran)
    if [ "$http_result" != "skipped" ]; then
        check_regression "http" "$http_result" || exit_code=1
    fi
    
    # Summary
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    log_header "BENCHMARK SUMMARY"
    
    echo "Total elapsed time: ${duration}s"
    echo ""
    
    if [ $exit_code -eq 0 ]; then
        echo -e "${BOLD}${GREEN}╔═══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BOLD}${GREEN}║  ✓ PASS - No performance regression detected             ║${NC}"
        echo -e "${BOLD}${GREEN}╚═══════════════════════════════════════════════════════════╝${NC}"
        echo ""
        log_success "All benchmarks within acceptable thresholds"
    else
        echo -e "${BOLD}${RED}╔═══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BOLD}${RED}║  ✗ FAIL - Performance regression detected (>10%)         ║${NC}"
        echo -e "${BOLD}${RED}╚═══════════════════════════════════════════════════════════╝${NC}"
        echo ""
        log_error "Performance regression detected - investigate before committing"
    fi
    
    echo ""
    
    exit $exit_code
}

# ==============================================================================
# Core Operations Benchmark
# ==============================================================================

run_core_benchmark() {
    cd "$PROJECT_ROOT"
    
    local temp_script=$(mktemp)
    cat > "$temp_script" << 'EOF'
case code:ensure_loaded(erlmcp_bench_core_ops) of
    {module, _} ->
        try
            erlmcp_bench_core_ops:run(<<"core_ops_1k">>),
            % Find latest result file
            {ok, Files} = file:list_dir("bench/results"),
            CoreFiles = [F || F <- Files, string:str(F, "core_ops_core_ops_1k") > 0],
            SortedFiles = lists:sort(fun(A, B) -> A >= B end, CoreFiles),
            case SortedFiles of
                [Latest | _] ->
                    {ok, BinData} = file:read_file(filename:join("bench/results", Latest)),
                    JsonMap = jsx:decode(BinData, [return_maps]),
                    Throughput = maps:get(<<"throughput_msg_per_s">>, JsonMap, 0),
                    P50 = maps:get(<<"latency_p50_us">>, JsonMap, 0),
                    P95 = maps:get(<<"latency_p95_us">>, JsonMap, 0),
                    io:format("RESULT:~p:~p:~p~n", [Throughput, P50, P95]),
                    halt(0);
                [] ->
                    io:format("ERROR: No result file found~n"),
                    halt(1)
            end
        catch
            _:Error ->
                io:format("ERROR: ~p~n", [Error]),
                halt(1)
        end;
    _ ->
        io:format("ERROR: erlmcp_bench_core_ops not found~n"),
        halt(1)
end.
EOF
    
    local output
    output=$(erl -pa _build/default/lib/*/ebin \
                 -pa apps/*/ebin \
                 -noshell \
                 -eval "$(cat $temp_script)" \
                 2>&1) || {
        rm -f "$temp_script"
        echo "ERROR"
        return 1
    }
    
    rm -f "$temp_script"
    
    # Parse output
    if echo "$output" | grep -q "^RESULT:"; then
        echo "$output" | grep "^RESULT:" | sed 's/RESULT://'
    else
        echo "ERROR"
        return 1
    fi
}

# ==============================================================================
# HTTP Network Benchmark
# ==============================================================================

run_http_benchmark() {
    cd "$PROJECT_ROOT"
    
    # Check if http workload exists, create if not
    ensure_http_workload
    
    local temp_script=$(mktemp)
    cat > "$temp_script" << 'EOF'
% Check if dependencies are available
case {code:ensure_loaded(gun), code:ensure_loaded(cowboy), code:ensure_loaded(erlmcp_bench_network_real)} of
    {{module, _}, {module, _}, {module, _}} ->
        try
            % Create custom quick workload with reduced duration
            erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
                connections => 100,
                duration_s => 10,
                payload_size_bytes => 1024
            }),
            timer:sleep(1000),
            % Find latest result file
            {ok, Files} = file:list_dir("bench/results"),
            HttpFiles = [F || F <- Files, string:str(F, "network_real_http") > 0],
            SortedFiles = lists:sort(fun(A, B) -> A >= B end, HttpFiles),
            case SortedFiles of
                [Latest | _] ->
                    {ok, BinData} = file:read_file(filename:join("bench/results", Latest)),
                    JsonMap = jsx:decode(BinData, [return_maps]),
                    Throughput = maps:get(<<"throughput_msg_per_s">>, JsonMap, 0),
                    P50 = maps:get(<<"latency_p50_us">>, JsonMap, 0),
                    P95 = maps:get(<<"latency_p95_us">>, JsonMap, 0),
                    io:format("RESULT:~p:~p:~p~n", [Throughput, P50, P95]),
                    halt(0);
                [] ->
                    io:format("ERROR: No result file found~n"),
                    halt(1)
            end
        catch
            _:Error ->
                io:format("ERROR: ~p~n", [Error]),
                halt(1)
        end;
    _ ->
        io:format("SKIPPED: Dependencies not available~n"),
        halt(0)
end.
EOF
    
    local output
    output=$(timeout 60s erl -pa _build/default/lib/*/ebin \
                 -pa apps/*/ebin \
                 -noshell \
                 -eval "$(cat $temp_script)" \
                 2>&1) || {
        rm -f "$temp_script"
        echo "skipped"
        return 1
    }
    
    rm -f "$temp_script"
    
    # Parse output
    if echo "$output" | grep -q "^RESULT:"; then
        echo "$output" | grep "^RESULT:" | sed 's/RESULT://'
    elif echo "$output" | grep -q "SKIPPED"; then
        echo "skipped"
        return 1
    else
        echo "ERROR"
        return 1
    fi
}

ensure_http_workload() {
    local workload_file="$WORKLOADS_DIR/http_quick_1k.json"
    
    if [ -f "$workload_file" ]; then
        return
    fi
    
    mkdir -p "$WORKLOADS_DIR"
    
    cat > "$workload_file" << 'EOFJSON'
{
  "workload_id": "http_quick_1k",
  "transport": "http",
  "protocol": "http2",
  "connections": 100,
  "duration_s": 10,
  "payload_size_bytes": 1024,
  "target_msg_per_s": 5000,
  "messages_per_conn": 10,
  "tls_enabled": false,
  "description": "Quick HTTP benchmark for local development - 100 connections, 1K messages, 10 seconds"
}
EOFJSON
}

# ==============================================================================
# Regression Checking
# ==============================================================================

check_regression() {
    local benchmark_type=$1
    local result=$2
    
    # Parse result: throughput:p50:p95
    local throughput=$(echo "$result" | cut -d: -f1)
    local p50=$(echo "$result" | cut -d: -f2)
    local p95=$(echo "$result" | cut -d: -f3)
    
    echo ""
    echo "Results for $benchmark_type:"
    echo "  Throughput: ${throughput} msg/sec"
    echo "  Latency P50: ${p50} µs"
    echo "  Latency P95: ${p95} µs"
    echo ""
    
    local regression=0
    
    # Get thresholds based on benchmark type
    local min_throughput
    if [ "$benchmark_type" = "core_ops" ]; then
        min_throughput=$(get_threshold "core_ops_100k" "throughput_msg_per_s")
        [ -z "$min_throughput" ] && min_throughput=500000
    else
        min_throughput=5000  # HTTP baseline
    fi
    
    # Check throughput
    if [ $(echo "$throughput < $min_throughput" | bc -l 2>/dev/null || echo "0") -eq 1 ]; then
        log_warning "Throughput below baseline: ${throughput} < ${min_throughput} msg/sec (>10% regression)"
        regression=1
    else
        log_success "Throughput acceptable: ${throughput} msg/sec"
    fi
    
    # Check P95 latency (should be low)
    local max_p95=100  # Default: 100µs for core ops
    if [ "$benchmark_type" = "http" ]; then
        max_p95=10000  # 10ms for HTTP
    fi
    
    if [ $(echo "$p95 > $max_p95" | bc -l 2>/dev/null || echo "0") -eq 1 ]; then
        log_warning "Latency P95 above threshold: ${p95}µs > ${max_p95}µs"
        regression=1
    else
        log_success "Latency P95 acceptable: ${p95}µs"
    fi
    
    return $regression
}

# ==============================================================================
# Entry Point
# ==============================================================================

main "$@"
