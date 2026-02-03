#!/usr/bin/env bash
#
# Benchmark Library for ERLMCP
# ==============================
#
# Shared functions for benchmark scripts.
# Per DOCKER-ONLY CONSTITUTION: All execution via Docker.
#

set -euo pipefail

# Version info
readonly BENCH_LIB_VERSION="1.0.0"

# Default configuration (can be overridden)
: "${ERLMCP_IMAGE:=erlmcp:dev}"
: "${ERLMCP_PORT:=4001}"
: "${BENCH_TIMEOUT:=300}"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# ============================================================================
# Logging Functions
# ============================================================================

bench_log() {
    local level="$1"
    shift
    local msg="$*"
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    echo "[${timestamp}] [${level}] ${msg}" >&2
}

bench_info() { bench_log "INFO" "$@"; }
bench_warn() { bench_log "WARN" "$@"; }
bench_error() { bench_log "ERROR" "$@"; }
bench_success() { bench_log "PASS" "$@"; }

# ============================================================================
# Docker Helpers
# ============================================================================

docker_image_exists() {
    local image="$1"
    docker image inspect "${image}" &>/dev/null
}

docker_container_running() {
    local container="$1"
    docker inspect -f '{{.State.Running}}' "${container}" 2>/dev/null | grep -q 'true'
}

docker_wait_for_log() {
    local container="$1"
    local pattern="$2"
    local max_wait="${3:-30}"
    local waited=0

    while [[ ${waited} -lt ${max_wait} ]]; do
        if docker logs "${container}" 2>&1 | grep -q "${pattern}"; then
            return 0
        fi
        sleep 1
        ((waited++))
    done

    return 1
}

docker_get_memory_mb() {
    local container="$1"
    docker stats "${container}" --no-stream --format '{{.MemUsage}}' | \
        awk '{print $1}' | \
        sed 's/MiB//; s/MB//; s/GiB/*1024/; s/GB/*1024/; s/KiB\/1024/; s/KB\/1024/' | \
        bc 2>/dev/null || echo "0"
}

docker_get_cpu_percent() {
    local container="$1"
    docker stats "${container}" --no-stream --format '{{.CPUPerc}}' | \
        sed 's/%//' || echo "0"
}

# ============================================================================
# Erlang/OTP Helpers
# ============================================================================

erl_beam_memory_mb() {
    local container="$1"
    docker exec "${container}" \
        erl -noshell -eval \
            "io:format(\"~.2f\", [erlang:memory(total) / 1048576.0]), init:stop()." \
        2>/dev/null || echo "0"
}

erl_process_count() {
    local container="$1"
    docker exec "${container}" \
        erl -noshell -eval \
            "io:format(\"~p\", [erlang:system_info(process_count)]), init:stop()." \
        2>/dev/null || echo "0"
}

erl_is_alive() {
    local container="$1"
    docker exec "${container}" \
        erl -noshell -eval "erlang:is_alive(), init:stop()." >/dev/null 2>&1
}

# ============================================================================
# Benchmark Functions
# ============================================================================

# Measure time to execute a command
bench_time() {
    local start=$(date +%s%N)
    "$@"
    local end=$(date +%s%N)
    echo $(( (end - start) / 1000000 ))  # Return milliseconds
}

# Run HTTP benchmark with configurable options
bench_http_load() {
    local url="$1"
    local requests="${2:-1000}"
    local concurrency="${3:-10}"
    local timeout="${4:-30}"

    # Use erlang-based HTTP load test
    cat <<'ERL'
-module(http_load).
-export([run/5]).

run(_Url, 0, _Concurrency, _Results, _StartTime) ->
    {ok, _Results};
run(Url, Requests, Concurrency, Results, StartTime) ->
    Self = self(),
    PerWorker = max(1, Requests div Concurrency),
    Pids = [spawn(fun() -> worker(Self, Url, PerWorker) end)
             || _ <- lists:seq(1, Concurrency)],
    collect_results(Pids, Results, StartTime).

worker(Parent, Url, Count) ->
    case do_request(Url, Count) of
        {ok, Times} -> Parent ! {done, Times};
        {error, _} -> Parent ! {done, []}
    end.

do_request(_Url, 0) -> {ok, []};
do_request(Url, Count) ->
    Start = erlang:monotonic_time(microsecond),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, _} ->
            End = erlang:monotonic_time(microsecond),
            {ok, [End - Start | Rest]} = do_request(Url, Count - 1),
            {ok, [End - Start | Rest]};
        {error, Reason} ->
            {error, Reason}
    end.

collect_results([], AllResults, _StartTime) ->
    {ok, lists:flatten(AllResults)};
collect_results([Pid|Rest], Results, StartTime) ->
    receive
        {done, Times} -> collect_results(Rest, [Times|Results], StartTime)
    end.
ERL
}

# Calculate percentiles from a list of numbers
bench_percentiles() {
    local data_file="$1"
    local p50="$2"
    local p95="$3"
    local p99="$4"

    # Sort the data
    local sorted=$(sort -n "${data_file}")
    local count=$(wc -l <<< "${sorted}")

    # Calculate indices
    local idx50=$(( count * 50 / 100 ))
    local idx95=$(( count * 95 / 100 ))
    local idx99=$(( count * 99 / 100 ))

    # Get values
    echo "p50=$(sed -n "${idx50}p" <<< "${sorted}")"
    echo "p95=$(sed -n "${idx95}p" <<< "${sorted}")"
    echo "p99=$(sed -n "${idx99}p" <<< "${sorted}")"
}

# Calculate statistics
bench_stats() {
    awk '
    BEGIN {
        sum = 0
        count = 0
        min = 999999999
        max = 0
    }
    {
        sum += $1
        if ($1 < min) min = $1
        if ($1 > max) max = $1
        count++
    }
    END {
        if (count > 0) {
            mean = sum / count
            printf "count:%d sum:%.2f mean:%.2f min:%.2f max:%.2f\n", count, sum, mean, min, max
        }
    }
    '
}

# ============================================================================
# Receipt Generation (Proof of Execution)
# ============================================================================

bench_generate_receipt() {
    local receipt_dir="$1"
    local exit_code="$2"
    shift 2
    local cmd="$*"

    mkdir -p "${receipt_dir}"

    local receipt_file="${receipt_dir}/receipt-$(date +%s).json"
    local git_sha=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
    local image_digest=$(docker image inspect "${ERLMCP_IMAGE}" --format '{{.Id}}' 2>/dev/null || echo "unknown")
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    cat > "${receipt_file}" <<EOF
{
  "receipt_type": "benchmark",
  "timestamp": "${timestamp}",
  "git_sha": "${git_sha}",
  "image_digest": "${image_digest}",
  "service": "erlmcp",
  "command": "$(echo "$@" | sed 's/"/\\"/g')",
  "exit": ${exit_code},
  "lib_version": "${BENCH_LIB_VERSION}"
}
EOF

    # Generate hash for proof
    local receipt_hash=$(shasum "${receipt_file}" | awk '{print $1}')
    echo "${receipt_hash}" > "${receipt_file}.sha1"

    bench_info "Receipt: ${receipt_file}"
    bench_info "Receipt SHA1: ${receipt_hash}"

    echo "${receipt_file}"
}

# ============================================================================
# Baseline Management
# ============================================================================

bench_save_baseline() {
    local baseline_file="$1"
    shift
    local results="$@"

    local git_sha=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    cat > "${baseline_file}" <<EOF
{
  "timestamp": "${timestamp}",
  "git_sha": "${git_sha}",
  "results": ${results}
}
EOF

    bench_info "Baseline saved: ${baseline_file}"
}

bench_load_baseline() {
    local baseline_file="$1"

    if [[ ! -f "${baseline_file}" ]]; then
        bench_warn "Baseline not found: ${baseline_file}"
        return 1
    fi

    cat "${baseline_file}"
}

bench_compare_baseline() {
    local current="$1"
    local baseline="$2"
    local metric="$3"
    local direction="${4:-lower_is_better}"  # or "higher_is_better"

    local curr_val=$(echo "${current}" | jq -r ".${metric} // 0")
    local base_val=$(echo "${baseline}" | jq -r ".${metric} // 0")

    if [[ "${base_val}" == "0" || "${base_val}" == "null" ]]; then
        echo "null"
        return
    fi

    local delta=$(echo "scale=2; (${curr_val} - ${base_val}) / ${base_val} * 100" | bc)

    # Determine if change is good or bad
    local status=""
    if [[ "${direction}" == "lower_is_better" ]]; then
        if (( $(echo "${delta} < 0" | bc -l 2>/dev/null || echo "0") )); then
            status="improved"
        elif (( $(echo "${delta} > 10" | bc -l 2>/dev/null || echo "0") )); then
            status="regressed"
        else
            status="stable"
        fi
    else  # higher_is_better
        if (( $(echo "${delta} > 0" | bc -l 2>/dev/null || echo "0") )); then
            status="improved"
        elif (( $(echo "${delta} < -10" | bc -l 2>/dev/null || echo "0") )); then
            status="regressed"
        else
            status="stable"
        fi
    fi

    echo "{\"delta\": ${delta}, \"status\": \"${status}\", \"current\": ${curr_val}, \"baseline\": ${base_val}}"
}

# ============================================================================
# Output Formatting
# ============================================================================

bench_print_table() {
    local title="$1"
    shift

    echo ""
    echo "=========================================="
    echo "  ${title}"
    echo "=========================================="
    printf "%-25s %15s %15s\n" "Metric" "Value" "Status"
    echo "--------------------------------------------------"

    while [[ $# -gt 0 ]]; do
        local metric="$1"
        local value="$2"
        local status="${3:-}"
        shift 3

        local color="${NC}"
        case "${status}" in
            improved|ok) color="${GREEN}" ;;
            regressed|fail) color="${RED}" ;;
            warning|warn) color="${YELLOW}" ;;
        esac

        printf "%-25s %15b %15b\n" "${metric}" "${value}" "${color}${status}${NC}"
    done

    echo ""
}

# Export functions for use in other scripts
export -f bench_log bench_info bench_warn bench_error bench_success
export -f bench_time bench_percentiles bench_stats
export -f docker_image_exists docker_container_running docker_wait_for_log
export -f docker_get_memory_mb docker_get_cpu_percent
export -f erl_beam_memory_mb erl_process_count erl_is_alive
export -f bench_generate_receipt bench_save_baseline bench_load_baseline bench_compare_baseline
export -f bench_print_table
