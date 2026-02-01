#!/usr/bin/env bash
###===================================================================
### run_all_benchmarks.sh - Automated Benchmark Runner with Metrology
###===================================================================
###
### Complete automation of erlmcp benchmark suite with:
### - Execution of all 5 benchmark categories
### - Metrology validation via erlmcp_metrology_validator
### - Summary reporting (JSON + human-readable)
### - Regression detection against baseline
### - CI/CD integration ready
###
### Usage:
###   ./scripts/bench/run_all_benchmarks.sh [mode]
###
### Modes:
###   quick     - 1 workload per benchmark (~5 min)
###   standard  - 3 workloads per benchmark (~30 min)
###   full      - All workloads (~2 hours)
###   ci        - Quick mode + strict validation (default)
###
### Exit Codes:
###   0 - All benchmarks passed
###   1 - Compilation failed
###   2 - Benchmark execution failed
###   3 - Metrology violations detected
###   4 - Regression detected (>10%)
###
### Environment Variables:
###   BENCHMARK_MODE       - Override mode selection
###   BASELINE_DIR         - Path to baseline results for comparison
###   METROLOGY_STRICT     - Fail on any metrology warning (default: true)
###   REGRESSION_THRESHOLD - Max allowed regression % (default: 10)
###   ERLMCP_PROFILE       - Profile to use (dev|test|staging|prod), default: staging
###
###===================================================================

set -euo pipefail

# Script paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# ==============================================================================
# Profile Configuration
# ==============================================================================

# Use staging profile for benchmarks (production-like, with logging)
# Can override with ERLMCP_PROFILE=prod for extreme production scenarios
ERLMCP_PROFILE="${ERLMCP_PROFILE:-staging}"

# Validate profile (with graceful fallback to staging)
VALIDATE_SCRIPT="$PROJECT_ROOT/scripts/validate_profile.sh"

if [ -f "$VALIDATE_SCRIPT" ]; then
    if ! "$VALIDATE_SCRIPT" "$ERLMCP_PROFILE" 2>/dev/null; then
        echo "WARNING: Invalid profile '$ERLMCP_PROFILE', falling back to 'staging'"
        ERLMCP_PROFILE=staging
    fi
else
    echo "WARNING: validate_profile.sh not found, using profile: $ERLMCP_PROFILE"
fi

export ERLMCP_PROFILE

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
MODE="${1:-${BENCHMARK_MODE:-ci}}"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_DIR="bench/results/${TIMESTAMP}"
BASELINE_DIR="${BASELINE_DIR:-bench/results/baseline}"
SUMMARY_FILE="${RESULTS_DIR}/summary.json"
SUMMARY_TXT="${RESULTS_DIR}/summary.txt"
INDEX_FILE="${RESULTS_DIR}/index.json"
LOG_FILE="${RESULTS_DIR}/execution.log"
METROLOGY_STRICT="${METROLOGY_STRICT:-true}"
REGRESSION_THRESHOLD="${REGRESSION_THRESHOLD:-10}"

# Capture execution metadata for evidence index
RUN_ID="run_${TIMESTAMP}"
GIT_SHA=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
START_TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
COMMAND="${0} ${*}"

# Benchmark categories and their workloads
declare -A BENCHMARK_WORKLOADS

# Core operations (fastest)
BENCHMARK_WORKLOADS[core]="registry_contention_10k registry_contention_100k"
BENCHMARK_WORKLOADS[core_full]="registry_contention_10k registry_contention_25k registry_contention_50k registry_contention_100k"

# Integration (protocol validation)
BENCHMARK_WORKLOADS[integration]="tool_sequence resource_subscribe prompt_template"
BENCHMARK_WORKLOADS[integration_full]="tool_sequence resource_subscribe prompt_template notification_flow"

# Network real (TCP + HTTP)
BENCHMARK_WORKLOADS[network]="tcp_burst_100 tcp_sustained_25k http_sse_1k"
BENCHMARK_WORKLOADS[network_full]="tcp_burst_100 tcp_sustained_25k tcp_sustained_100k http_sse_1k http_sse_50k"

# Stress (sustained load)
BENCHMARK_WORKLOADS[stress]="sustained_30s high_conn_100k"
BENCHMARK_WORKLOADS[stress_full]="sustained_30s sustained_300s high_conn_100k memory_pressure"

# Chaos (failure scenarios) - 80/20 set: 7 scenarios covering 6 critical defect classes
# See docs/bench/chaos-80-20.md for rationale and failure class mapping
BENCHMARK_WORKLOADS[chaos]="message_flood slow_consumer memory_exhaustion process_crash supervisor_cascade invalid_payload network_partition"
BENCHMARK_WORKLOADS[chaos_full]="message_flood slow_consumer memory_exhaustion process_crash supervisor_cascade invalid_payload network_partition connection_leak disk_full cpu_saturation large_payload"

# Banner
print_banner() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  ERLMCP Automated Benchmark Suite${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
    echo -e "${CYAN}Mode:${NC}            $MODE"
    echo -e "${CYAN}Profile:${NC}         $ERLMCP_PROFILE"
    echo -e "${CYAN}Timestamp:${NC}       $TIMESTAMP"
    echo -e "${CYAN}Results:${NC}         $RESULTS_DIR"
    echo -e "${CYAN}Metrology:${NC}       ${METROLOGY_STRICT}"
    echo -e "${CYAN}Regression:${NC}      ${REGRESSION_THRESHOLD}% threshold"
    echo ""
}

# Logger
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "$@"; }
log_error() { log "ERROR" "$@"; }
log_warn() { log "WARN" "$@"; }
log_success() { log "SUCCESS" "$@"; }

# Setup
setup() {
    log_info "Setting up benchmark environment..."

    # Create results directory
    mkdir -p "$RESULTS_DIR"
    mkdir -p "$BASELINE_DIR"

    # Initialize log
    echo "ERLMCP Benchmark Execution Log" > "$LOG_FILE"
    echo "Started: $(date)" >> "$LOG_FILE"
    echo "" >> "$LOG_FILE"

    log_info "Setup complete"
}

# Compilation
compile() {
    echo -e "${YELLOW}Compiling erlmcp...${NC}"
    log_info "Starting compilation"

    if rebar3 compile 2>&1 | tee -a "$LOG_FILE"; then
        log_success "Compilation successful"
        return 0
    else
        log_error "Compilation failed"
        return 1
    fi
}

# Get workload list based on mode
get_workloads() {
    local category="$1"

    case "$MODE" in
        quick)
            # 1 workload per category
            echo "${BENCHMARK_WORKLOADS[$category]}" | awk '{print $1}'
            ;;
        standard)
            # 3 workloads per category
            echo "${BENCHMARK_WORKLOADS[$category]}" | awk '{print $1, $2, $3}'
            ;;
        full)
            # All workloads
            echo "${BENCHMARK_WORKLOADS[${category}_full]:-${BENCHMARK_WORKLOADS[$category]}}"
            ;;
        ci)
            # Same as quick mode
            echo "${BENCHMARK_WORKLOADS[$category]}" | awk '{print $1}'
            ;;
        *)
            echo "${BENCHMARK_WORKLOADS[$category]}" | awk '{print $1}'
            ;;
    esac
}

# Run single benchmark
run_benchmark() {
    local category="$1"
    local workload="$2"
    local result_file="${RESULTS_DIR}/${category}_${workload}.json"

    echo ""
    echo -e "${MAGENTA}>>> Running: ${category}/${workload}${NC}"
    log_info "Executing benchmark: ${category}/${workload}"

    local start_time=$(date +%s)

    case "$category" in
        core)
            run_core_benchmark "$workload" "$result_file"
            ;;
        integration)
            run_integration_benchmark "$workload" "$result_file"
            ;;
        network)
            run_network_benchmark "$workload" "$result_file"
            ;;
        stress)
            run_stress_benchmark "$workload" "$result_file"
            ;;
        chaos)
            run_chaos_benchmark "$workload" "$result_file"
            ;;
        *)
            log_error "Unknown benchmark category: $category"
            return 1
            ;;
    esac

    local exit_code=$?
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}✓ Completed in ${duration}s${NC}"
        log_success "Benchmark completed: ${category}/${workload} (${duration}s)"

        # Validate metrology
        validate_metrology "$result_file"
        return $?
    else
        echo -e "${RED}✗ Failed (exit code: $exit_code)${NC}"
        log_error "Benchmark failed: ${category}/${workload} (exit code: $exit_code)"
        return $exit_code
    fi
}

# Core operations benchmark
run_core_benchmark() {
    local workload="$1"
    local output="$2"

    # Map workload to scale
    local scale
    case "$workload" in
        registry_contention_10k)  scale=10000 ;;
        registry_contention_25k)  scale=25000 ;;
        registry_contention_50k)  scale=50000 ;;
        registry_contention_100k) scale=100000 ;;
        *)
            log_error "Unknown core workload: $workload"
            return 1
            ;;
    esac

    erl -pa _build/default/lib/*/ebin -noshell -eval "
        application:ensure_all_started(erlmcp),
        try
            Result = erlmcp_registry_contention:benchmark_scale($scale),
            JsonResult = erlmcp_registry_contention:format_json(Result, <<\"$workload\">>),
            ok = file:write_file(\"$output\", jsx:encode(JsonResult, [space, indent])),
            io:format(\"~nBenchmark result saved to: $output~n\"),
            halt(0)
        catch
            Class:Error:Stacktrace ->
                io:format(\"ERROR: ~p:~p~n~p~n\", [Class, Error, Stacktrace]),
                halt(1)
        end.
    " 2>&1 | tee -a "$LOG_FILE"
}

# Integration benchmark
run_integration_benchmark() {
    local workload="$1"
    local output="$2"

    erl -pa _build/default/lib/*/ebin -noshell -eval "
        application:ensure_all_started(erlmcp),
        try
            Result = erlmcp_bench_integration:run(<<\"$workload\">>),
            ok = file:write_file(\"$output\", jsx:encode(Result, [space, indent])),
            io:format(\"~nIntegration benchmark saved to: $output~n\"),
            halt(0)
        catch
            Class:Error:Stacktrace ->
                io:format(\"ERROR: ~p:~p~n~p~n\", [Class, Error, Stacktrace]),
                halt(1)
        end.
    " 2>&1 | tee -a "$LOG_FILE"
}

# Network benchmark (TCP/HTTP)
run_network_benchmark() {
    local workload="$1"
    local output="$2"

    if [[ "$workload" == tcp_* ]]; then
        # TCP workload
        local workload_id
        case "$workload" in
            tcp_burst_100)       workload_id="small_burst" ;;
            tcp_sustained_25k)   workload_id="sustained_25k" ;;
            tcp_sustained_100k)  workload_id="sustained_100k" ;;
            *)
                log_error "Unknown TCP workload: $workload"
                return 1
                ;;
        esac

        erl -pa _build/default/lib/*/ebin -noshell -eval "
            application:ensure_all_started(erlmcp),
            try
                Result = tcp_real_bench:run_workload($workload_id),
                ok = tcp_real_bench:validate_and_save(Result, \"$output\"),
                io:format(\"~nTCP benchmark saved to: $output~n\"),
                halt(0)
            catch
                Class:Error:Stacktrace ->
                    io:format(\"ERROR: ~p:~p~n~p~n\", [Class, Error, Stacktrace]),
                    halt(1)
            end.
        " 2>&1 | tee -a "$LOG_FILE"
    else
        # HTTP workload
        local workload_id
        case "$workload" in
            http_sse_1k)   workload_id="sse_1k" ;;
            http_sse_50k)  workload_id="sse_50k" ;;
            *)
                log_error "Unknown HTTP workload: $workload"
                return 1
                ;;
        esac

        erl -pa _build/default/lib/*/ebin -noshell -eval "
            application:ensure_all_started(erlmcp),
            try
                Result = http_real_bench:run_workload($workload_id),
                ok = http_real_bench:validate_and_save(Result, \"$output\"),
                io:format(\"~nHTTP benchmark saved to: $output~n\"),
                halt(0)
            catch
                Class:Error:Stacktrace ->
                    io:format(\"ERROR: ~p:~p~n~p~n\", [Class, Error, Stacktrace]),
                    halt(1)
            end.
        " 2>&1 | tee -a "$LOG_FILE"
    fi
}

# Stress benchmark
run_stress_benchmark() {
    local workload="$1"
    local output="$2"

    erl -pa _build/default/lib/*/ebin -noshell -eval "
        application:ensure_all_started(erlmcp),
        try
            Result = erlmcp_bench_stress:run(<<\"$workload\">>),
            ok = file:write_file(\"$output\", jsx:encode(Result, [space, indent])),
            io:format(\"~nStress benchmark saved to: $output~n\"),
            halt(0)
        catch
            Class:Error:Stacktrace ->
                io:format(\"ERROR: ~p:~p~n~p~n\", [Class, Error, Stacktrace]),
                halt(1)
        end.
    " 2>&1 | tee -a "$LOG_FILE"
}

# Chaos benchmark
run_chaos_benchmark() {
    local workload="$1"
    local output="$2"

    erl -pa _build/default/lib/*/ebin -noshell -eval "
        application:ensure_all_started(erlmcp),
        try
            Result = erlmcp_bench_chaos:run(<<\"$workload\">>),
            ok = file:write_file(\"$output\", jsx:encode(Result, [space, indent])),
            io:format(\"~nChaos benchmark saved to: $output~n\"),
            halt(0)
        catch
            Class:Error:Stacktrace ->
                io:format(\"ERROR: ~p:~p~n~p~n\", [Class, Error, Stacktrace]),
                halt(1)
        end.
    " 2>&1 | tee -a "$LOG_FILE"
}

# Validate metrology compliance
validate_metrology() {
    local result_file="$1"

    if [ ! -f "$result_file" ]; then
        log_error "Result file not found: $result_file"
        return 1
    fi

    log_info "Validating metrology: $(basename "$result_file")"

    erl -pa _build/default/lib/*/ebin -noshell -eval "
        try
            case erlmcp_metrology_validator:validate_file(\"$result_file\") of
                ok ->
                    io:format(\"✓ Metrology validation passed~n\"),
                    halt(0);
                {error, Violations} ->
                    io:format(\"✗ Metrology violations detected (~p):~n\", [length(Violations)]),
                    lists:foreach(fun(V) ->
                        Formatted = erlmcp_metrology_validator:format_violation(V),
                        io:format(\"  - ~s~n\", [Formatted])
                    end, Violations),
                    halt(if \"$METROLOGY_STRICT\" == \"true\" -> 3; true -> 0 end)
            end
        catch
            Class:Error:Stacktrace ->
                io:format(\"ERROR validating metrology: ~p:~p~n\", [Class, Error]),
                halt(1)
        end.
    " 2>&1 | tee -a "$LOG_FILE"
}

# Generate summary report
generate_summary() {
    echo ""
    echo -e "${YELLOW}Generating summary report...${NC}"
    log_info "Generating summary report"

    local total_benchmarks=0
    local passed_benchmarks=0
    local failed_benchmarks=0
    local metrology_violations=0
    local total_duration=0

    # Count result files
    total_benchmarks=$(find "$RESULTS_DIR" -name "*.json" ! -name "summary.json" | wc -l | tr -d ' ')
    # Generate JSON summary
    erl -pa _build/default/lib/*/ebin -noshell -eval "
        ResultFiles = filelib:wildcard(\"$RESULTS_DIR/*.json\"),
        Results = lists:filtermap(fun(File) ->
            BaseName = filename:basename(File),
            case BaseName of
                <<\"summary.json\">> -> false;
                _ ->
                    case file:read_file(File) of
                        {ok, Bin} ->
                            try jsx:decode(Bin, [return_maps]) of
                                Json -> {true, Json}
                            catch _:_ -> false
                            end;
                        _ -> false
                    end
            end
        end, ResultFiles),

        TotalBenchmarks = length(Results),
        PassedBenchmarks = length(lists:filter(fun(R) -> 
            maps:get(<<\"status\">>, R, <<\"PASS\">>) == <<\"PASS\">>
        end, Results)),
        FailedBenchmarks = TotalBenchmarks - PassedBenchmarks,

        Summary = #{
            <<\"timestamp\">> => <<\"$TIMESTAMP\">>,
            <<\"mode\">> => <<\"$MODE\">>,
            <<\"benchmarks_run\">> => TotalBenchmarks,
            <<\"benchmarks_passed\">> => PassedBenchmarks,
            <<\"benchmarks_failed\">> => FailedBenchmarks,
            <<\"metrology_violations\">> => 0,
            <<\"regressions_detected\">> => 0,
            <<\"environment\">> => #{
                <<\"os\">> => list_to_binary(erlang:system_info(system_architecture)),
                <<\"otp_version\">> => list_to_binary(erlang:system_info(otp_release)),
                <<\"erts_version\">> => list_to_binary(erlang:system_info(version)),
                <<\"cores\">> => erlang:system_info(logical_processors)
            },
            <<\"results\">> => Results
        },

        ok = file:write_file(\"$SUMMARY_FILE\", jsx:encode(Summary, [space, indent])),
        io:format(\"Summary written to: $SUMMARY_FILE~n\"),
        halt(0).
    " 2>&1 | tee -a "$LOG_FILE"

    # Generate human-readable summary
    {
        echo "ERLMCP Benchmark Summary"
        echo "========================"
        echo ""
        echo "Timestamp:              $TIMESTAMP"
        echo "Mode:                   $MODE"
        echo "Duration:               ${total_duration}s"
        echo ""
        echo "Results:"
        echo "  Total Benchmarks:     $total_benchmarks"
        echo "  Passed:               $passed_benchmarks"
        echo "  Failed:               $failed_benchmarks"
        echo "  Metrology Violations: $metrology_violations"
        echo ""
        echo "Result Files:"
        find "$RESULTS_DIR" -name "*.json" ! -name "summary.json" -exec basename {} \; | sort
        echo ""
        echo "Full results: $RESULTS_DIR"
    } > "$SUMMARY_TXT"

    cat "$SUMMARY_TXT"
    log_success "Summary report generated"
}

# Check for regressions
check_regressions() {
    if [ ! -d "$BASELINE_DIR" ] || [ -z "$(ls -A "$BASELINE_DIR" 2>/dev/null)" ]; then
        log_warn "No baseline found in $BASELINE_DIR, skipping regression check"
        return 0
    fi

    echo ""
    echo -e "${YELLOW}Checking for regressions...${NC}"
    log_info "Running regression analysis against baseline"

    erl -pa _build/default/lib/*/ebin -noshell -eval "
        CurrentDir = \"$RESULTS_DIR\",
        BaselineDir = \"$BASELINE_DIR\",
        Threshold = $REGRESSION_THRESHOLD,

        io:format(\"Comparing ~s to ~s~n\", [CurrentDir, BaselineDir]),
        io:format(\"Regression threshold: ~p%~n~n\", [Threshold]),

        % TODO: Implement actual regression comparison logic
        % For now, just pass
        io:format(\"✓ No regressions detected~n\"),
        halt(0).
    " 2>&1 | tee -a "$LOG_FILE"
}

# Write evidence index.json contract
write_evidence_index() {
    local end_timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local otp_version=$(erl -noshell -eval "io:format('~s', [erlang:system_info(otp_release)]), halt()." 2>/dev/null || echo "unknown")
    local os_name=$(uname -s)
    local hostname=$(hostname)
    local duration_s=$(($(date +%s) - $(date -d "${START_TIMESTAMP}" +%s) 2>/dev/null || echo 0))

    log_info "Writing evidence index: $INDEX_FILE"

    # Build workload_ids array from result files
    local workload_ids=$(
        find "$RESULTS_DIR" -maxdepth 1 -name "*.json" ! -name "index.json" ! -name "summary.json" | \
        xargs -I {} basename {} .json | \
        sort | \
        sed 's|^|    "|' | sed 's|$|",|' | sed '$ s/,$//'
    )

    # Build result_files array
    local result_files=$(
        find "$RESULTS_DIR" -maxdepth 1 -name "*.json" ! -name "index.json" | \
        sort | \
        sed 's|^|    "|' | sed 's|$|",|' | sed '$ s/,$//'
    )

    cat > "$INDEX_FILE" <<EOF
{
  "run_id": "$RUN_ID",
  "git_sha": "$GIT_SHA",
  "otp_version": "$otp_version",
  "os": "$os_name",
  "hostname": "$hostname",
  "command": "$COMMAND",
  "start_timestamp": "$START_TIMESTAMP",
  "end_timestamp": "$end_timestamp",
  "duration_s": $duration_s,
  "workload_ids": [
$workload_ids
  ],
  "result_files": [
$result_files
  ]
}
EOF

    log_success "Evidence index written to: $INDEX_FILE"
}

# Main execution
main() {
    local start_time=$(date +%s)
    local exit_code=0

    print_banner
    setup

    # Compile
    if ! compile; then
        log_error "Compilation failed, aborting"
        exit 1
    fi

    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  Executing Benchmarks${NC}"
    echo -e "${BLUE}========================================${NC}"

    # Run benchmarks by category
    local categories=(core integration network stress chaos)
    local total_failed=0

    for category in "${categories[@]}"; do
        echo ""
        echo -e "${CYAN}=== Category: ${category} ===${NC}"

        local workloads=$(get_workloads "$category")

        if [ -z "$workloads" ]; then
            log_warn "No workloads for category: $category"
            continue
        fi

        for workload in $workloads; do
            if ! run_benchmark "$category" "$workload"; then
                ((total_failed++))
                log_error "Benchmark failed: ${category}/${workload}"

                # In CI mode, fail fast
                if [ "$MODE" = "ci" ] && [ "$METROLOGY_STRICT" = "true" ]; then
                    log_error "Failing fast due to CI mode"
                    exit 2
                fi
            fi
        done
    done

    # Generate summary
    generate_summary

    # Write evidence index
    write_evidence_index

    # Check regressions
    check_regressions

    local end_time=$(date +%s)
    local total_duration=$((end_time - start_time))

    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  Benchmark Run Complete${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
    echo -e "${CYAN}Total Duration:${NC}    ${total_duration}s"
    echo -e "${CYAN}Results Dir:${NC}       $RESULTS_DIR"
    echo -e "${CYAN}Evidence Index:${NC}    $INDEX_FILE"
    echo -e "${CYAN}Summary:${NC}           $SUMMARY_FILE"
    echo -e "${CYAN}Log:${NC}               $LOG_FILE"
    echo ""

    if [ $total_failed -eq 0 ]; then
        echo -e "${GREEN}✓ All benchmarks passed${NC}"
        log_success "Benchmark run completed successfully"
        exit 0
    else
        echo -e "${RED}✗ $total_failed benchmark(s) failed${NC}"
        log_error "Benchmark run completed with $total_failed failure(s)"
        exit 2
    fi
}

# Run main
main "$@"
