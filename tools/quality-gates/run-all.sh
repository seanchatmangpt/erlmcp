#!/usr/bin/env bash
# ============================================================================
# ERLMCP QUALITY GATES - PARALLEL EXECUTION SCRIPT
# ============================================================================
# Purpose: Execute all quality gates in parallel where possible
# Philosophy: DOCKER-ONLY CONSTITUTION - All execution via Docker containers
#
# Gates (7 total):
#   1. compile    - Compilation (errors=0)
#   2. eunit      - Unit tests (failures=0)
#   3. ct         - Integration tests (failures=0)
#   4. dialyzer   - Type checking (warnings=0)
#   5. xref       - Cross-reference analysis (undefined=0)
#   6. coverage   - Test coverage (>=80%)
#   7. security   - Security scan (secrets=0, vulnerabilities=0)
#
# Parallel Execution Strategy:
#   - Phase 1 (parallel): compile, dialyzer, xref, security
#   - Phase 2 (parallel): eunit, ct (after compile succeeds)
#   - Phase 3 (sequential): coverage (after tests complete)
#
# Receipt Generation:
#   Each gate generates a cryptographic receipt containing:
#   - git_sha, image_digest, service, cmd, exit, stdout, stderr
#   - Receipt hash for chain verification
#   - Aggregated master receipt for all gates
#
# Usage:
#   ./tools/quality-gates/run-all.sh                    # Run all gates
#   ./tools/quality-gates/run-all.sh --fast             # Fast mode (compile + tests only)
#   ./tools/quality-gates/run-all.sh --skip-dialyzer    # Skip slow gates
#   ./tools/quality-gates/run-all.sh --help             # Show help
# ============================================================================

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Quality gate thresholds
REQUIRED_TEST_COVERAGE=80
MAX_DIALYZER_WARNINGS=0
MAX_XREF_WARNINGS=0

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
RECEIPT_DIR="${PROJECT_ROOT}/.erlmcp/receipts/quality-gates"
LOG_DIR="${PROJECT_ROOT}/.erlmcp/logs/quality-gates"

# Create directories
mkdir -p "${RECEIPT_DIR}"
mkdir -p "${LOG_DIR}"

# Timestamps
TIMESTAMP_ISO=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
TIMESTAMP_UNIX=$(date +%s)
WORK_ORDER_ID="${WORK_ORDER_ID:-${TIMESTAMP_UNIX}}"

# Git metadata
GIT_SHA=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
GIT_SHORT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
GIT_COMMIT_MESSAGE=$(git log -1 --pretty=%s 2>/dev/null || echo "unknown")

# Docker configuration
DOCKER_IMAGE="${DOCKER_IMAGE:-erlmcp:3.0.0-build}"
DOCKER_COMPOSE_FILE="${SCRIPT_DIR}/docker-compose.quality-gates.yml"

# Gate execution tracking
declare -A GATE_STATUS
declare -A GATE_EXIT_CODE
declare -A GATE_DURATION
declare -A GATE_RECEIPT
declare -A GATE_STDOUT
declare -A GATE_STDERR

PARALLEL_JOBS=${PARALLEL_JOBS:-4}  # Max parallel gate executions

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $1"
}

log_error() {
    echo -e "${RED}[✗]${NC} $1"
}

log_gate() {
    local gate="$1"
    local status="$2"
    local message="$3"

    case "$status" in
        "PASS")
            echo -e "${GREEN}[✓]${NC} ${MAGENTA}[${gate}]${NC} ${message}"
            ;;
        "FAIL")
            echo -e "${RED}[✗]${NC} ${MAGENTA}[${gate}]${NC} ${message}"
            ;;
        "SKIP")
            echo -e "${YELLOW}[⊘]${NC} ${MAGENTA}[${gate}]${NC} ${message}"
            ;;
        "RUN")
            echo -e "${CYAN}[→]${NC} ${MAGENTA}[${gate}]${NC} ${message}"
            ;;
        *)
            echo -e "${BLUE}[?]${NC} ${MAGENTA}[${gate}]${NC} ${message}"
            ;;
    esac
}

print_section() {
    echo ""
    echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║  $1${NC}"
    echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
}

# ============================================================================
# RECEIPT GENERATION
# ============================================================================

generate_gate_receipt() {
    local gate="$1"
    local exit_code="$2"
    local stdout_file="$3"
    local stderr_file="$4"
    local duration_ms="$5"
    local docker_service="${6:-erlmcp-build}"

    local receipt_file="${RECEIPT_DIR}/${gate}_${TIMESTAMP_UNIX}.json"

    # Get Docker image digest
    local image_digest=$(docker images --format="{{.ID}}" "${DOCKER_IMAGE}" 2>/dev/null || echo "unknown")

    # Read stdout/stderr
    local stdout_content=$(cat "$stdout_file" 2>/dev/null | head -c 10000 | jq -Rs . 2>/dev/null || echo "\"\"")
    local stderr_content=$(cat "$stderr_file" 2>/dev/null | head -c 10000 | jq -Rs . 2>/dev/null || echo "\"\"")

    # Generate receipt JSON
    cat > "$receipt_file" << EOF
{
  "receipt_version": "1.0",
  "gate": "${gate}",
  "work_order_id": "${WORK_ORDER_ID}",
  "timestamp": "${TIMESTAMP_ISO}",
  "git": {
    "sha": "${GIT_SHA}",
    "short_sha": "${GIT_SHORT_SHA}",
    "branch": "${GIT_BRANCH}",
    "commit_message": $(echo "$GIT_COMMIT_MESSAGE" | jq -R .)
  },
  "docker": {
    "image": "${DOCKER_IMAGE}",
    "image_digest": "${image_digest}",
    "service": "${docker_service}"
  },
  "execution": {
    "cmd": "rebar3 ${gate}",
    "exit_code": ${exit_code},
    "duration_ms": ${duration_ms}
  },
  "output": {
    "stdout": ${stdout_content},
    "stderr": ${stderr_content}
  }
}
EOF

    # Compute receipt hash
    local receipt_hash=$(cat "$receipt_file" | sha256sum | cut -d' ' -f1)

    # Add hash to receipt
    local tmp_file=$(mktemp)
    jq --arg hash "$receipt_hash" '.receipt_hash = $hash' "$receipt_file" > "$tmp_file"
    mv "$tmp_file" "$receipt_file"

    GATE_RECEIPT[$gate]="$receipt_file"
    echo "$receipt_file"
}

generate_master_receipt() {
    local overall_status="$1"
    local master_receipt="${RECEIPT_DIR}/master_${TIMESTAMP_UNIX}.json"

    # Build gates array
    local gates_json="{"
    local first=true

    for gate in compile eunit ct dialyzer xref coverage security; do
        if [[ -n "${GATE_STATUS[$gate]:-}" ]]; then
            if [[ "$first" == "true" ]]; then
                first=false
            else
                gates_json+=","
            fi

            local status="${GATE_STATUS[$gate]}"
            local exit_code="${GATE_EXIT_CODE[$gate]:-0}"
            local duration="${GATE_DURATION[$gate]:-0}"
            local receipt="${GATE_RECEIPT[$gate]:-""}"

            gates_json+=$(cat << EOF
"${gate}": {
  "status": "${status}",
  "exit_code": ${exit_code},
  "duration_ms": ${duration},
  "receipt": "${receipt}"
}
EOF
            )
        fi
    done

    gates_json+="}"

    # Count failures
    local failed_count=0
    local passed_count=0
    local skipped_count=0

    for gate in compile eunit ct dialyzer xref coverage security; do
        case "${GATE_STATUS[$gate]:-SKIP}" in
            "PASS") passed_count=$((passed_count + 1)) ;;
            "FAIL") failed_count=$((failed_count + 1)) ;;
            "SKIP") skipped_count=$((skipped_count + 1)) ;;
        esac
    done

    # Generate master receipt
    cat > "$master_receipt" << EOF
{
  "receipt_version": "1.0",
  "type": "master",
  "work_order_id": "${WORK_ORDER_ID}",
  "timestamp": "${TIMESTAMP_ISO}",
  "git": {
    "sha": "${GIT_SHA}",
    "short_sha": "${GIT_SHORT_SHA}",
    "branch": "${GIT_BRANCH}",
    "commit_message": $(echo "$GIT_COMMIT_MESSAGE" | jq -R .)
  },
  "summary": {
    "overall_status": "${overall_status}",
    "total_gates": $((passed_count + failed_count + skipped_count)),
    "passed": ${passed_count},
    "failed": ${failed_count},
    "skipped": ${skipped_count}
  },
  "gates": ${gates_json}
}
EOF

    # Compute master receipt hash
    local master_hash=$(cat "$master_receipt" | sha256sum | cut -d' ' -f1)
    jq --arg hash "$master_hash" '.master_receipt_hash = $hash' "$master_receipt" > "${master_receipt}.tmp"
    mv "${master_receipt}.tmp" "$master_receipt"

    echo "$master_receipt"
}

# ============================================================================
# DOCKER EXECUTION FUNCTIONS
# ============================================================================

run_docker_gate() {
    local gate="$1"
    local docker_cmd="$2"
    local docker_service="${3:-erlmcp-build}"
    local timeout="${4:-600}"

    local stdout_file="${LOG_DIR}/${gate}_${TIMESTAMP_UNIX}.stdout"
    local stderr_file="${LOG_DIR}/${gate}_${TIMESTAMP_UNIX}.stderr"
    local start_time=$(date +%s%3N)  # Milliseconds

    log_gate "$gate" "RUN" "Executing via Docker (${docker_service})..."

    # Execute via Docker compose
    local exit_code=0

    # Check if docker-compose file exists
    if [[ ! -f "${DOCKER_COMPOSE_FILE}" ]]; then
        log_error "Docker compose file not found: ${DOCKER_COMPOSE_FILE}"
        GATE_STATUS[$gate]="FAIL"
        GATE_EXIT_CODE[$gate]=1
        return 1
    fi

    if timeout "$timeout" docker compose -f "${DOCKER_COMPOSE_FILE}" run \
        --rm \
        -e ERLMCP_QUALITY_GATE="$gate" \
        -e ERLMCP_WORK_ORDER_ID="$WORK_ORDER_ID" \
        "$docker_service" \
        sh -c "$docker_cmd" \
        > "$stdout_file" 2> "$stderr_file"; then
        exit_code=0
        GATE_STATUS[$gate]="PASS"
        log_gate "$gate" "PASS" "Completed successfully"
    else
        exit_code=$?
        GATE_STATUS[$gate]="FAIL"
        log_gate "$gate" "FAIL" "Failed with exit code $exit_code"
    fi

    local end_time=$(date +%s%3N)
    local duration=$((end_time - start_time))

    GATE_EXIT_CODE[$gate]=$exit_code
    GATE_DURATION[$gate]=$duration
    GATE_STDOUT[$gate]="$stdout_file"
    GATE_STDERR[$gate]="$stderr_file"

    # Generate receipt
    generate_gate_receipt "$gate" "$exit_code" "$stdout_file" "$stderr_file" "$duration" "$docker_service"

    return $exit_code
}

# ============================================================================
# QUALITY GATES
# ============================================================================

gate_compile() {
    log_info "Running compilation gate..."

    # Build Docker image first using docker-compose
    if ! docker compose -f "${DOCKER_COMPOSE_FILE}" build erlmcp-build > /dev/null 2>&1; then
        log_error "Failed to build Docker image"
        GATE_STATUS[compile]="FAIL"
        GATE_EXIT_CODE[compile]=1
        return 1
    fi

    run_docker_gate "compile" "rebar3 compile" "erlmcp-build" 300
}

gate_eunit() {
    log_info "Running EUnit gate..."
    run_docker_gate "eunit" "rebar3 eunit" "erlmcp-unit" 600
}

gate_ct() {
    log_info "Running Common Test gate..."
    run_docker_gate "ct" "rebar3 ct" "erlmcp-ct" 900
}

gate_dialyzer() {
    log_info "Running Dialyzer gate..."
    run_docker_gate "dialyzer" "rebar3 dialyzer" "erlmcp-build" 1800
}

gate_xref() {
    log_info "Running Xref gate..."
    run_docker_gate "xref" "rebar3 xref" "erlmcp-build" 300
}

gate_coverage() {
    log_info "Running coverage gate..."

    # Coverage requires tests to have run first
    if [[ "${GATE_STATUS[eunit]:-}" != "PASS" ]] && [[ "${GATE_STATUS[ct]:-}" != "PASS" ]]; then
        log_warning "Coverage requires at least one test gate to pass"
        GATE_STATUS[coverage]="SKIP"
        return 0
    fi

    run_docker_gate "coverage" "rebar3 cover" "erlmcp-check" 600

    # Parse coverage percentage
    if [[ "${GATE_STATUS[coverage]}" == "PASS" ]]; then
        local stdout_file="${GATE_STDOUT[coverage]}"
        local coverage_percent=$(grep -oE "total[^0-9]+[0-9]+%" "$stdout_file" 2>/dev/null | grep -oE "[0-9]+" | tail -1)

        if [[ -n "$coverage_percent" ]] && [[ "$coverage_percent" -lt "$REQUIRED_TEST_COVERAGE" ]]; then
            log_error "Coverage ${coverage_percent}% below required ${REQUIRED_TEST_COVERAGE}%"
            GATE_STATUS[coverage]="FAIL"
            GATE_EXIT_CODE[coverage]=1
        fi
    fi
}

gate_security() {
    log_info "Running security scan gate..."

    local stdout_file="${LOG_DIR}/security_${TIMESTAMP_UNIX}.stdout"
    local stderr_file="${LOG_DIR}/security_${TIMESTAMP_UNIX}.stderr"
    local start_time=$(date +%s%3N)

    log_gate "security" "RUN" "Scanning for secrets and vulnerabilities..."

    # Check for hardcoded secrets
    local secrets_found=0

    # Common secret patterns
    if grep -rE "(password|secret|api_key|token)\s*=\s*\"[^\"]+\"" \
        --include="*.erl" --include="*.hrl" \
        apps/*/src/ apps/*/include/ > "$stdout_file" 2>/dev/null; then
        secrets_found=$(wc -l < "$stdout_file" 2>/dev/null || echo "0")
    fi

    local exit_code=0

    if [[ "$secrets_found" -gt 0 ]]; then
        echo "Found ${secrets_found} potential hardcoded secrets" >> "$stderr_file"
        exit_code=1
        GATE_STATUS[security]="FAIL"
        log_gate "security" "FAIL" "Found ${secrets_found} potential secrets"
    else
        GATE_STATUS[security]="PASS"
        log_gate "security" "PASS" "No security issues found"
    fi

    local end_time=$(date +%s%3N)
    local duration=$((end_time - start_time))

    GATE_EXIT_CODE[security]=$exit_code
    GATE_DURATION[security]=$duration
    GATE_STDOUT[security]="$stdout_file"
    GATE_STDERR[security]="$stderr_file"

    generate_gate_receipt "security" "$exit_code" "$stdout_file" "$stderr_file" "$duration" "host"

    return $exit_code
}

# ============================================================================
# PARALLEL EXECUTION
# ============================================================================

run_phase1_gates() {
    # Phase 1: Compilation, Dialyzer, Xref, Security (can run in parallel)
    print_section "PHASE 1: PARALLEL GATES (compile, dialyzer, xref, security)"

    local pids=()

    # Run compilation first (required by other gates)
    gate_compile &
    pids[0]=$!

    # Wait for compilation to complete
    wait ${pids[0]}

    # If compilation failed, skip remaining gates
    if [[ "${GATE_STATUS[compile]}" != "PASS" ]]; then
        log_error "Compilation failed - skipping remaining Phase 1 gates"
        return 1
    fi

    # Run dialyzer, xref, security in parallel
    gate_dialyzer &
    pids[1]=$!

    gate_xref &
    pids[2]=$!

    gate_security &
    pids[3]=$!

    # Wait for all Phase 1 gates
    for pid in "${pids[@]:1}"; do
        wait $pid || true
    done

    return 0
}

run_phase2_gates() {
    # Phase 2: EUnit, CT (can run in parallel after compile)
    print_section "PHASE 2: PARALLEL TEST GATES (eunit, ct)"

    local pids=()

    gate_eunit &
    pids[0]=$!

    gate_ct &
    pids[1]=$!

    # Wait for all test gates
    for pid in "${pids[@]}"; do
        wait $pid || true
    done

    return 0
}

run_phase3_gates() {
    # Phase 3: Coverage (sequential, depends on tests)
    print_section "PHASE 3: COVERAGE GATE"

    gate_coverage

    return 0
}

# ============================================================================
# REPORTING
# ============================================================================

print_final_report() {
    local overall_status="$1"

    print_section "QUALITY GATES FINAL REPORT"

    echo -e "${CYAN}Work Order:${NC}    ${WORK_ORDER_ID}"
    echo -e "${CYAN}Timestamp:${NC}     ${TIMESTAMP_ISO}"
    echo -e "${CYAN}Git SHA:${NC}       ${GIT_SHORT_SHA} (${GIT_BRANCH})"
    echo ""
    echo -e "${CYAN}Gate Results:${NC}"
    echo ""

    # Print gate results table
    printf "${MAGENTA}%-15s${NC} %-10s %-10s %-15s\n" "Gate" "Status" "Exit" "Duration"
    printf "${CYAN}%-15s${NC} %-10s %-10s %-15s\n" "---------------" "----------" "----------" "---------------"

    for gate in compile eunit ct dialyzer xref coverage security; do
        if [[ -n "${GATE_STATUS[$gate]:-}" ]]; then
            local status="${GATE_STATUS[$gate]}"
            local exit_code="${GATE_EXIT_CODE[$gate]:-0}"
            local duration="${GATE_DURATION[$gate]:-0}"

            local status_color=""
            case "$status" in
                "PASS") status_color="${GREEN}" ;;
                "FAIL") status_color="${RED}" ;;
                "SKIP") status_color="${YELLOW}" ;;
            esac

            printf "%-15s ${status_color}%-10s${NC} %-10s %-15s\n" \
                "$gate" "$status" "$exit_code" "${duration}ms"
        fi
    done

    echo ""

    # Print overall status
    if [[ "$overall_status" == "PASS" ]]; then
        echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║                   ✓ ALL QUALITY GATES PASSED                 ║${NC}"
        echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
        echo ""
        echo -e "${GREEN}Production deployment ready!${NC}"
    else
        echo -e "${RED}╔══════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║                   ✗ QUALITY GATES FAILED                     ║${NC}"
        echo -e "${RED}╚══════════════════════════════════════════════════════════════╝${NC}"
        echo ""
        echo -e "${RED}Production deployment BLOCKED${NC}"
        echo ""
        echo -e "${YELLOW}Failed Gates:${NC}"

        for gate in compile eunit ct dialyzer xref coverage security; do
            if [[ "${GATE_STATUS[$gate]:-}" == "FAIL" ]]; then
                echo -e "  ${RED}✗${NC} ${gate}"
                echo -e "     Receipt: ${GATE_RECEIPT[$gate]}"
                echo -e "     Stderr:  ${GATE_STDERR[$gate]}"
            fi
        done
    fi

    echo ""
    echo -e "${CYAN}Master Receipt:${NC} $master_receipt"
    echo ""
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

main() {
    local start_time=$(date +%s)
    local skip_dialyzer=false
    local fast_mode=false

    # Parse command-line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --fast)
                fast_mode=true
                shift
                ;;
            --skip-dialyzer)
                skip_dialyzer=true
                shift
                ;;
            --help)
                echo "Usage: $0 [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --fast             Fast mode (compile + tests only)"
                echo "  --skip-dialyzer    Skip slow Dialyzer gate"
                echo "  --help             Show this help message"
                echo ""
                echo "Environment Variables:"
                echo "  PARALLEL_JOBS      Max parallel gate executions (default: 4)"
                echo "  WORK_ORDER_ID      Work order identifier (default: timestamp)"
                echo "  DOCKER_IMAGE       Docker image to use (default: erlmcp:3.0.0-build)"
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done

    # Print header
    echo ""
    echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║         ERLMCP QUALITY GATES - PARALLEL EXECUTION           ║${NC}"
    echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${CYAN}Work Order:${NC}    ${WORK_ORDER_ID}"
    echo -e "${CYAN}Timestamp:${NC}     ${TIMESTAMP_ISO}"
    echo -e "${CYAN}Git SHA:${NC}       ${GIT_SHORT_SHA} (${GIT_BRANCH})"
    echo -e "${CYAN}Parallel Jobs:${NC}  ${PARALLEL_JOBS}"
    echo ""

    # Check Docker availability
    if ! command -v docker &> /dev/null; then
        log_error "Docker not found - required for execution"
        exit 1
    fi

    if ! command -v docker compose &> /dev/null; then
        log_error "docker compose not found - required for execution"
        exit 1
    fi

    # Mark skipped gates
    if [[ "$skip_dialyzer" == "true" ]]; then
        GATE_STATUS[dialyzer]="SKIP"
        log_warning "Dialyzer gate skipped (--skip-dialyzer)"
    fi

    if [[ "$fast_mode" == "true" ]]; then
        GATE_STATUS[dialyzer]="SKIP"
        GATE_STATUS[xref]="SKIP"
        log_warning "Fast mode: Running compile + tests only"
    fi

    # Execute gates in phases
    run_phase1_gates || true
    run_phase2_gates || true
    run_phase3_gates || true

    # Determine overall status
    local failed_count=0

    for gate in compile eunit ct dialyzer xref coverage security; do
        if [[ "${GATE_STATUS[$gate]:-SKIP}" == "FAIL" ]]; then
            failed_count=$((failed_count + 1))
        fi
    done

    local overall_status="PASS"
    if [[ $failed_count -gt 0 ]]; then
        overall_status="FAIL"
    fi

    # Generate master receipt
    local master_receipt=$(generate_master_receipt "$overall_status")

    # Calculate total duration
    local end_time=$(date +%s)
    local total_duration=$((end_time - start_time))

    # Print final report
    print_final_report "$overall_status"

    echo -e "${CYAN}Total Duration:${NC}  ${total_duration}s"
    echo ""

    # Exit with appropriate code
    if [[ "$overall_status" == "PASS" ]]; then
        exit 0
    else
        exit 1
    fi
}

# ============================================================================
# SCRIPT ENTRY POINT
# ============================================================================

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
