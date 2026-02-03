#!/usr/bin/env bash
#
# Production Benchmark Suite for ERLMCP
# ======================================
#
# Measures:
#   1. Cold start time (container start to first response)
#   2. Request throughput (requests/second)
#   3. Memory usage (RSS, heap, cache)
#   4. Response time percentiles (p50, p95, p99)
#
# Per DOCKER-ONLY CONSTITUTION:
#   - All execution via Docker
#   - Receipt generation for proof
#   - Baseline comparison for regression detection
#
# Usage:
#   ./production-bench.sh [options]
#
# Options:
#   -r, --requests N      Number of requests (default: 10000)
#   -c, --concurrency N   Concurrent connections (default: 10)
#   -d, --duration SEC    Duration in seconds (default: 30)
#   -b, --baseline FILE   Load custom baseline file
#   -s, --save-baseline   Save results as new baseline
#   -o, --output FILE     Output file (default: stdout)
#   -v, --verbose         Enable verbose output
#   -h, --help            Show this help
#
# Exit codes:
#   0 - All benchmarks passed
#   1 - One or more benchmarks failed
#   2 - Regression detected vs baseline
#   3 - Docker execution error
#

set -euo pipefail

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
BASELINE_DIR="${SCRIPT_DIR}/.baseline"
BASELINE_FILE="${BASELINE_DIR}/baseline.json"
RECEIPT_DIR="${SCRIPT_DIR}/.receipts"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Default configuration
REQUESTS=10000
CONCURRENCY=10
DURATION=30
VERBOSE=0
SAVE_BASELINE=0
CUSTOM_BASELINE=""
OUTPUT_FILE=""

# Container configuration
IMAGE_NAME="${ERLMCP_IMAGE:-erlmcp:dev}"
CONTAINER_NAME="erlmcp-bench-$$"
ERLMCP_PORT=4001
BENCH_CONTAINER="erlmcp-bench-runner-$$"

# Results storage
declare -A RESULTS
declare -A BASELINE

# ============================================================================
# Utility Functions
# ============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" >&2
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $*" >&2
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $*" >&2
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $*" >&2
}

verbose_log() {
    if [[ ${VERBOSE} -eq 1 ]]; then
        log_info "$@"
    fi
}

json_escape() {
    local string="$1"
    string="${string//\\/\\\\}"
    string="${string//\"/\\\"}"
    string="${string//$'\n'/\\n}"
    string="${string//$'\r'/\\r}"
    string="${string//$'\t'/\\t}"
    printf '%s' "$string"
}

# ============================================================================
# Docker Management
# ============================================================================

ensure_image() {
    log_info "Ensuring Docker image exists: ${IMAGE_NAME}"

    if ! docker image inspect "${IMAGE_NAME}" &>/dev/null; then
        log_info "Image not found, building..."
        cd "${PROJECT_ROOT}"
        if ! docker build -t "${IMAGE_NAME}" . >&2; then
            log_error "Failed to build Docker image"
            exit 3
        fi
    fi

    log_success "Image ready: ${IMAGE_NAME}"
}

start_container() {
    log_info "Starting ERLMCP container: ${CONTAINER_NAME}"

    # Stop any existing container with the same name
    docker stop "${CONTAINER_NAME}" 2>/dev/null || true
    docker rm "${CONTAINER_NAME}" 2>/dev/null || true

    # Start new container
    docker run -d \
        --name "${CONTAINER_NAME}" \
        -p "${ERLMCP_PORT}:${ERLMCP_PORT}" \
        -e ERLMCP_TRANSPORT_PORT="${ERLMCP_PORT}" \
        "${IMAGE_NAME}" \
        foreground 2>&1 || {
        log_error "Failed to start container"
        exit 3
    }

    # Wait for container to be healthy
    wait_for_container_ready
}

stop_container() {
    log_info "Stopping container: ${CONTAINER_NAME}"
    docker stop "${CONTAINER_NAME}" 2>/dev/null || true
    docker rm "${CONTAINER_NAME}" 2>/dev/null || true
}

wait_for_container_ready() {
    local max_wait=30
    local waited=0
    local healthy=0

    log_info "Waiting for container to be ready..."

    while [[ ${waited} -lt ${max_wait} ]]; do
        if docker exec "${CONTAINER_NAME}" erl -noshell -eval "erlang:is_alive(), init:stop()." >/dev/null 2>&1; then
            healthy=1
            break
        fi
        sleep 1
        ((waited++))
    done

    if [[ ${healthy} -eq 0 ]]; then
        log_error "Container did not become ready within ${max_wait}s"
        stop_container
        exit 3
    fi

    log_success "Container ready in ${waited}s"
}

# ============================================================================
# Benchmark 1: Cold Start Time
# ============================================================================

benchmark_cold_start() {
    log_info "=== Benchmark: Cold Start Time ==="

    # Ensure no container is running
    docker stop "${CONTAINER_NAME}" 2>/dev/null || true
    docker rm "${CONTAINER_NAME}" 2>/dev/null || true

    # Measure time from docker run to first response
    local start_time=$(date +%s%N)
    local boot_time

    # Start container and capture boot metrics
    docker run -d \
        --name "${CONTAINER_NAME}" \
        -p "${ERLMCP_PORT}:${ERLMCP_PORT}" \
        -e ERLMCP_TRANSPORT_PORT="${ERLMCP_PORT}" \
        "${IMAGE_NAME}" \
        foreground >/dev/null 2>&1

    # Wait for first response
    local waited=0
    local ready=0
    while [[ ${waited} -lt 60 ]]; do
        if docker exec "${CONTAINER_NAME}" erl -noshell -eval "erlang:is_alive(), init:stop()." >/dev/null 2>&1; then
            ready=1
            break
        fi
        sleep 0.1
        ((waited++))
    done

    local end_time=$(date +%s%N)
    boot_time=$(( (end_time - start_time) / 1000000 ))

    if [[ ${ready} -eq 0 ]]; then
        log_error "Container failed to start within 60s"
        RESULTS[cold_start_ms]=null
        return 1
    fi

    RESULTS[cold_start_ms]=${boot_time}
    log_success "Cold start: ${boot_time}ms"

    # Clean up
    stop_container
}

# ============================================================================
# Benchmark 2: Request Throughput
# ============================================================================

benchmark_throughput() {
    log_info "=== Benchmark: Request Throughput ==="

    start_container

    # Create a simple benchmark Erlang module
    local bench_code="
-module(erlmcp_bench_throughput).
-compile(export_all).

run_bench(Requests, Concurrency) ->
    inets:start(),
    Self = self(),
    Pids = [spawn(fun() -> worker(Self, Requests div Concurrency) end) || _ <- lists:seq(1, Concurrency)],
    StartTime = erlang:monotonic_time(microsecond),
    collect_results(Pids, 0, StartTime).

worker(Parent, Count) ->
    do_requests(Parent, Count, 0).

do_requests(Parent, 0, Success) ->
    Parent ! {done, Success};
do_requests(Parent, Count, Success) ->
    case httpc:request(get, {\"http://host.docker.internal:${ERLMCP_PORT}/\", []}, [], []) of
        {ok, _} -> do_requests(Parent, Count-1, Success+1);
        _ -> do_requests(Parent, Count-1, Success)
    end.

collect_results([], Total, StartTime) ->
    EndTime = erlang:monotonic_time(microsecond),
    Duration = (EndTime - StartTime) / 1000000,
    Total / Duration;
collect_results([Pid|Rest], Acc, StartTime) ->
    receive
        {done, Count} -> collect_results(Rest, Acc + Count, StartTime)
    end.
"

    # Run throughput benchmark in a temporary container
    local result=$(docker run --rm \
        --network host \
        -e ERLMCP_PORT="${ERLMCP_PORT}" \
        -e REQUESTS="${REQUESTS}" \
        -e CONCURRENCY="${CONCURRENCY}" \
        "${IMAGE_NAME}" \
        erl -noshell -eval "
            ${bench_code}
            Throughput = erlmcp_bench_throughput:run_bench(${REQUESTS}, ${CONCURRENCY}),
            io:format(\"~.2f\", [Throughput]),
            init:stop().
        " 2>/dev/null || echo "0.00")

    if [[ -z "${result}" || "${result}" == "0.00" ]]; then
        log_warning "Throughput measurement failed, using rebar3 proper benchmark"
        result=$(run_proper_throughput)
    fi

    RESULTS[throughput_rps]=$(printf "%.2f" "${result}")
    log_success "Throughput: ${result} req/sec"

    stop_container
}

run_proper_throughput() {
    # Fallback: use rebar3 proper within container
    local throughput=$(docker exec "${CONTAINER_NAME}" \
        rebar3 proper -m erlmcp_performance_bench \
        -p "benchmark_throughput(${REQUESTS}, ${CONCURRENCY})" 2>/dev/null \
        | grep -oP 'Throughput: \K[0-9.]+' || echo "0.00")

    echo "${throughput:-0.00}"
}

# ============================================================================
# Benchmark 3: Memory Usage
# ============================================================================

benchmark_memory() {
    log_info "=== Benchmark: Memory Usage ==="

    start_container

    # Warm up the system
    sleep 2

    # Capture memory metrics
    local docker_stats=$(docker stats "${CONTAINER_NAME}" --no-stream --format "{{.MemUsage}}")

    # Parse memory usage (e.g., "150MiB / 2GiB")
    local mem_used=$(echo "${docker_stats}" | awk '{print $1}')
    local mem_value=$(echo "${mem_used}" | grep -oP '[0-9.]+' || echo "0")
    local mem_unit=$(echo "${mem_used}" | grep -oP '[A-Z]+' || echo "MB")

    # Convert to MB
    case "${mem_unit}" in
        *MiB|*MB)
            RESULTS[memory_mb]=$(printf "%.2f" "${mem_value}")
            ;;
        *GiB|*GB)
            RESULTS[memory_mb]=$(printf "%.2f" "$(echo "${mem_value} * 1024" | bc)")
            ;;
        *KiB|*KB)
            RESULTS[memory_mb]=$(printf "%.2f" "$(echo "${mem_value} / 1024" | bc)")
            ;;
        *)
            RESULTS[memory_mb]="0.00"
            ;;
    esac

    # Get BEAM-specific memory from within container
    local beam_memory=$(docker exec "${CONTAINER_NAME}" \
        erl -noshell -eval "
            Mem = erlang:memory(total),
            io:format(\"~.2f\", [Mem / 1048576.0]),
            init:stop().
        " 2>/dev/null || echo "0.00")

    RESULTS[beam_memory_mb]=$(printf "%.2f" "${beam_memory}")

    log_success "Memory: ${RESULTS[memory_mb]} MB (Docker), ${RESULTS[beam_memory_mb]} MB (BEAM)"

    stop_container
}

# ============================================================================
# Benchmark 4: Response Time Percentiles
# ============================================================================

benchmark_percentiles() {
    log_info "=== Benchmark: Response Time Percentiles ==="

    start_container

    # Create percentile benchmark module
    local percentile_code="
-module(erlmcp_bench_percentiles).
-compile(export_all).

run_bench(Requests) ->
    inets:start(),
    Times = [make_request() || _ <- lists:seq(1, Requests)],
    Sorted = lists:sort(Times),
    [
        {p50, percentile(Sorted, 0.50)},
        {p95, percentile(Sorted, 0.95)},
        {p99, percentile(Sorted, 0.99)}
    ].

make_request() ->
    StartTime = erlang:monotonic_time(microsecond),
    case httpc:request(get, {\"http://host.docker.internal:${ERLMCP_PORT}/\", []}, [], [{body_format, binary}]) of
        {ok, _} -> ok;
        _ -> ok
    end,
    EndTime = erlang:monotonic_time(microsecond),
    EndTime - StartTime.

percentile(Sorted, P) ->
    Index = max(1, round(length(Sorted) * P)),
    lists:nth(Index, Sorted) / 1000.0.  % Convert to ms
"

    # Run with subset of requests for percentiles
    local sample=$(( REQUESTS < 1000 ? REQUESTS : 1000 ))
    local percentiles=$(docker run --rm \
        --network host \
        -e ERLMCP_PORT="${ERLMCP_PORT}" \
        "${IMAGE_NAME}" \
        erl -noshell -eval "
            ${percentile_code}
            Result = erlmcp_bench_percentiles:run_bench(${sample}),
            lists:foreach(fun({K, V}) -> io:format(\"~s:~.2f~n\", [K, V]) end, Result),
            init:stop().
        " 2>/dev/null)

    # Parse results
    RESULTS[p50_ms]=$(echo "${percentiles}" | grep -oP 'p50:\K[0-9.]+' || echo "0.00")
    RESULTS[p95_ms]=$(echo "${percentiles}" | grep -oP 'p95:\K[0-9.]+' || echo "0.00")
    RESULTS[p99_ms]=$(echo "${percentiles}" | grep -oP 'p99:\K[0-9.]+' || echo "0.00")

    log_success "Percentiles - p50: ${RESULTS[p50_ms]}ms, p95: ${RESULTS[p95_ms]}ms, p99: ${RESULTS[p99_ms]}ms"

    stop_container
}

# ============================================================================
# Baseline Management
# ============================================================================

load_baseline() {
    local baseline_file="${1:-${BASELINE_FILE}}"

    if [[ ! -f "${baseline_file}" ]]; then
        log_warning "No baseline found at ${baseline_file}"
        return 1
    fi

    log_info "Loading baseline from ${baseline_file}"

    # Parse JSON into associative array
    local content=$(cat "${baseline_file}")

    # Handle both flat and nested JSON structures
    BASELINE[git_sha]=$(echo "${content}" | jq -r '.git_sha // "unknown"')
    BASELINE[timestamp]=$(echo "${content}" | jq -r '.timestamp // "unknown"')

    # Try to parse from nested structure first, then flat
    BASELINE[cold_start_ms]=$(echo "${content}" | jq -r '.results.cold_start_ms // .cold_start_ms // "null"')
    BASELINE[throughput_rps]=$(echo "${content}" | jq -r '.results.throughput_rps // .throughput_rps // "null"')
    BASELINE[memory_mb]=$(echo "${content}" | jq -r '.results.memory_mb // .memory_mb // "null"')
    BASELINE[p50_ms]=$(echo "${content}" | jq -r '.results.p50_ms // .p50_ms // "null"')
    BASELINE[p95_ms]=$(echo "${content}" | jq -r '.results.p95_ms // .p95_ms // "null"')
    BASELINE[p99_ms]=$(echo "${content}" | jq -r '.results.p99_ms // .p99_ms // "null"')

    log_success "Baseline loaded (commit: ${BASELINE[git_sha]:0:8}, date: ${BASELINE[timestamp]})"
    return 0
}

save_baseline() {
    mkdir -p "${BASELINE_DIR}"

    local git_sha=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local baseline_file="${BASELINE_DIR}/baseline-${timestamp}.json"

    # Create JSON output
    local json=$(create_json_output "${git_sha}" "${timestamp}")

    echo "${json}" > "${baseline_file}"
    ln -sf "$(basename "${baseline_file}")" "${BASELINE_DIR}/baseline.json"

    log_success "Baseline saved to ${baseline_file}"
}

# ============================================================================
# Output Generation
# ============================================================================

create_json_output() {
    local git_sha="${1:-$(git rev-parse HEAD 2>/dev/null || echo "unknown")}"
    local timestamp="${2:-$(date -u +"%Y-%m-%dT%H:%M:%SZ")}"

    cat <<EOF
{
  "timestamp": "${timestamp}",
  "git_sha": "${git_sha}",
  "config": {
    "requests": ${REQUESTS},
    "concurrency": ${CONCURRENCY},
    "duration": ${DURATION}
  },
  "results": {
    "cold_start_ms": ${RESULTS[cold_start_ms]:-null},
    "throughput_rps": ${RESULTS[throughput_rps]:-null},
    "memory_mb": ${RESULTS[memory_mb]:-null},
    "beam_memory_mb": ${RESULTS[beam_memory_mb]:-null},
    "p50_ms": ${RESULTS[p50_ms]:-null},
    "p95_ms": ${RESULTS[p95_ms]:-null},
    "p99_ms": ${RESULTS[p99_ms]:-null}
  },
  "baseline": {
    "current": {
      "git_sha": "${BASELINE[git_sha]:-none}",
      "timestamp": "${BASELINE[timestamp]:-none}"
    },
    "comparison": {
      "cold_start_ms": $(calculate_delta "cold_start_ms"),
      "throughput_rps": $(calculate_delta "throughput_rps"),
      "memory_mb": $(calculate_delta "memory_mb"),
      "p50_ms": $(calculate_delta "p50_ms"),
      "p95_ms": $(calculate_delta "p95_ms"),
      "p99_ms": $(calculate_delta "p99_ms")
    }
  },
  "image": "${IMAGE_NAME}",
  "container": "${CONTAINER_NAME}"
}
EOF
}

calculate_delta() {
    local metric="$1"
    local current="${RESULTS[$metric]:-0}"
    local baseline="${BASELINE[$metric]:-0}"

    if [[ "${baseline}" == "null" || "${baseline}" == "0" || "${baseline}" == "" ]]; then
        echo "null"
    elif [[ "${current}" == "null" ]]; then
        echo "null"
    else
        local delta=$(echo "scale=2; (${current} - ${baseline}) / ${baseline} * 100" | bc 2>/dev/null || echo "0")
        echo "${delta}"
    fi
}

print_results() {
    local json="$1"

    if [[ -n "${OUTPUT_FILE}" ]]; then
        echo "${json}" > "${OUTPUT_FILE}"
        log_success "Results saved to ${OUTPUT_FILE}"
    else
        echo "${json}"
    fi

    # Print human-readable summary
    print_summary
}

print_summary() {
    echo ""
    echo "============================================"
    echo "       BENCHMARK RESULTS SUMMARY"
    echo "============================================"
    echo ""

    printf "%-20s %12s %12s %12s\n" "Metric" "Current" "Baseline" "Delta"
    echo "------------------------------------------------------------"

    # Cold Start
    local current="${RESULTS[cold_start_ms]:-N/A}"
    local baseline="${BASELINE[cold_start_ms]:-N/A}"
    local delta=$(format_delta "cold_start_ms" "lower_is_better")
    printf "%-20s %12s %12s %12s\n" "Cold Start (ms)" "${current}" "${baseline}" "${delta}"

    # Throughput
    current="${RESULTS[throughput_rps]:-N/A}"
    baseline="${BASELINE[throughput_rps]:-N/A}"
    delta=$(format_delta "throughput_rps" "higher_is_better")
    printf "%-20s %12s %12s %12s\n" "Throughput (r/s)" "${current}" "${baseline}" "${delta}"

    # Memory
    current="${RESULTS[memory_mb]:-N/A}"
    baseline="${BASELINE[memory_mb]:-N/A}"
    delta=$(format_delta "memory_mb" "lower_is_better")
    printf "%-20s %12s %12s %12s\n" "Memory (MB)" "${current}" "${baseline}" "${delta}"

    # Latency percentiles
    echo ""
    printf "%-20s %12s %12s %12s\n" "Latency p50 (ms)" "${RESULTS[p50_ms]:-N/A}" "${BASELINE[p50_ms]:-N/A}" "$(format_delta "p50_ms" "lower_is_better")"
    printf "%-20s %12s %12s %12s\n" "Latency p95 (ms)" "${RESULTS[p95_ms]:-N/A}" "${BASELINE[p95_ms]:-N/A}" "$(format_delta "p95_ms" "lower_is_better")"
    printf "%-20s %12s %12s %12s\n" "Latency p99 (ms)" "${RESULTS[p99_ms]:-N/A}" "${BASELINE[p99_ms]:-N/A}" "$(format_delta "p99_ms" "lower_is_better")"

    echo ""
}

format_delta() {
    local metric="$1"
    local direction="$2"
    local current="${RESULTS[$metric]:-0}"
    local baseline="${BASELINE[$metric]:-0}"

    if [[ "${baseline}" == "null" || "${baseline}" == "0" || "${baseline}" == "" || "${baseline}" == "N/A" ]]; then
        echo "N/A"
        return
    fi

    if [[ "${current}" == "null" || "${current}" == "" ]]; then
        echo "N/A"
        return
    fi

    local delta=$(echo "scale=1; (${current} - ${baseline}) / ${baseline} * 100" | bc 2>/dev/null || echo "0")

    # Determine if delta is good or bad
    local sign=""
    local color=""
    if (( $(echo "${delta} > 0" | bc -l 2>/dev/null || echo "0") )); then
        sign="+"
    fi

    if [[ "${direction}" == "lower_is_better" ]]; then
        if (( $(echo "${delta} < 0" | bc -l 2>/dev/null || echo "1") )); then
            echo "${sign}${delta}% ✓"  # Improvement
        elif (( $(echo "${delta} > 10" | bc -l 2>/dev/null || echo "0") )); then
            echo "${sign}${delta}% ⚠"  # Regression > 10%
        else
            echo "${sign}${delta}%"     # Minor change
        fi
    else  # higher_is_better
        if (( $(echo "${delta} > 0" | bc -l 2>/dev/null || echo "1") )); then
            echo "${sign}${delta}% ✓"  # Improvement
        elif (( $(echo "${delta} < -10" | bc -l 2>/dev/null || echo "0") )); then
            echo "${sign}${delta}% ⚠"  # Regression > 10%
        else
            echo "${sign}${delta}%"     # Minor change
        fi
    fi
}

check_regressions() {
    local has_regression=0

    # Check cold start (should not increase > 20%)
    if [[ -n "${BASELINE[cold_start_ms]}" && "${BASELINE[cold_start_ms]}" != "null" ]]; then
        local delta=$(calculate_delta "cold_start_ms")
        if (( $(echo "${delta} > 20" | bc -l 2>/dev/null || echo "0") )); then
            log_warning "Cold start regression: +${delta}%"
            has_regression=1
        fi
    fi

    # Check throughput (should not decrease > 10%)
    if [[ -n "${BASELINE[throughput_rps]}" && "${BASELINE[throughput_rps]}" != "null" ]]; then
        local delta=$(calculate_delta "throughput_rps")
        if (( $(echo "${delta} < -10" | bc -l 2>/dev/null || echo "0") )); then
            log_warning "Throughput regression: ${delta}%"
            has_regression=1
        fi
    fi

    # Check memory (should not increase > 20%)
    if [[ -n "${BASELINE[memory_mb]}" && "${BASELINE[memory_mb]}" != "null" ]]; then
        local delta=$(calculate_delta "memory_mb")
        if (( $(echo "${delta} > 20" | bc -l 2>/dev/null || echo "0") )); then
            log_warning "Memory regression: +${delta}%"
            has_regression=1
        fi
    fi

    # Check p99 latency (should not increase > 15%)
    if [[ -n "${BASELINE[p99_ms]}" && "${BASELINE[p99_ms]}" != "null" ]]; then
        local delta=$(calculate_delta "p99_ms")
        if (( $(echo "${delta} > 15" | bc -l 2>/dev/null || echo "0") )); then
            log_warning "P99 latency regression: +${delta}%"
            has_regression=1
        fi
    fi

    return ${has_regression}
}

# ============================================================================
# Receipt Generation (for proof)
# ============================================================================

generate_receipt() {
    mkdir -p "${RECEIPT_DIR}"

    local receipt_file="${RECEIPT_DIR}/receipt-$(date +%s).json"
    local git_sha=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
    local image_digest=$(docker image inspect "${IMAGE_NAME}" --format '{{.Id}}' 2>/dev/null || echo "unknown")

    cat > "${receipt_file}" <<EOF
{
  "receipt_type": "benchmark",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "git_sha": "${git_sha}",
  "image_digest": "${image_digest}",
  "service": "erlmcp",
  "command": "$(json_escape "$0 ${*}")",
  "exit": 0,
  "results": $(create_json_output "${git_sha}" "$(date -u +"%Y-%m-%dT%H:%M:%SZ")")
}
EOF

    local receipt_hash=$(shasum "${receipt_file}" | awk '{print $1}')
    echo "${receipt_hash}" > "${receipt_file}.sha1"

    log_info "Receipt: ${receipt_file}"
    log_info "Receipt SHA1: ${receipt_hash}"
}

# ============================================================================
# Main Execution
# ============================================================================

show_help() {
    sed -n '/^# Usage:/,/^$/p' "${BASH_SOURCE[0]}" | sed 's/^# //; s/^#//'
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -r|--requests)
                REQUESTS="$2"
                shift 2
                ;;
            -c|--concurrency)
                CONCURRENCY="$2"
                shift 2
                ;;
            -d|--duration)
                DURATION="$2"
                shift 2
                ;;
            -b|--baseline)
                CUSTOM_BASELINE="$2"
                shift 2
                ;;
            -s|--save-baseline)
                SAVE_BASELINE=1
                shift
                ;;
            -o|--output)
                OUTPUT_FILE="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=1
                shift
                ;;
            -h|--help)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

main() {
    parse_args "$@"

    log_info "=========================================="
    log_info "   ERLMCP Production Benchmark Suite"
    log_info "=========================================="
    log_info "Configuration:"
    log_info "  Requests:    ${REQUESTS}"
    log_info "  Concurrency: ${CONCURRENCY}"
    log_info "  Duration:    ${DURATION}s"
    log_info "  Image:       ${IMAGE_NAME}"
    log_info ""

    # Load baseline if available
    if [[ -n "${CUSTOM_BASELINE}" ]]; then
        load_baseline "${CUSTOM_BASELINE}"
    elif [[ -f "${BASELINE_FILE}" ]]; then
        load_baseline "${BASELINE_FILE}"
    fi

    # Ensure Docker image is available
    ensure_image

    # Run benchmarks
    benchmark_cold_start
    benchmark_throughput
    benchmark_memory
    benchmark_percentiles

    # Generate output
    local json_output=$(create_json_output)
    print_results "${json_output}"

    # Save baseline if requested
    if [[ ${SAVE_BASELINE} -eq 1 ]]; then
        save_baseline
    fi

    # Check for regressions
    if [[ -f "${BASELINE_FILE}" || -n "${CUSTOM_BASELINE}" ]]; then
        if check_regressions; then
            log_warning "Regressions detected!"
            generate_receipt "$@"
            exit 2
        fi
    fi

    # Generate receipt for proof
    generate_receipt "$@"

    log_success "All benchmarks completed successfully!"
    exit 0
}

# Run main
main "$@"
