#!/bin/bash
################################################################################
# docs_lint.sh - Documentation Linter for erlmcp
#
# Validates documentation for:
# 1. Legacy file references (benchmark_100k, throughput_SUITE, etc.)
# 2. Ambiguous metric terms (req/s, connections without context)
# 3. Hardcoded performance numbers without evidence
# 4. Valid workload_id references
#
# EXIT CODES:
#   0 - All checks pass
#   1 - Violations found (details printed)
#
# USAGE:
#   ./tools/docs_lint.sh                 # Run all checks
#   ./tools/docs_lint.sh --check-legacy  # Only check legacy files
#   ./tools/docs_lint.sh --check-terms   # Only check ambiguous terms
#   ./tools/docs_lint.sh --check-workloads # Only check workload IDs
#   ./tools/docs_lint.sh --verbose       # Verbose output
################################################################################

set -o pipefail

# Configuration
DOCS_DIR="docs"
BENCH_DIR="bench"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VERBOSE=false
FAILED=false

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Valid workload IDs (extracted from bench modules)
declare -a VALID_WORKLOADS=(
    "core_ops_1k"
    "core_ops_10k"
    "core_ops_100k"
    "core_ops_1m"
    "network_tcp_100_100k"
    "network_tcp_500_100k"
    "network_tcp_1k_100k"
    "network_tcp_5k_100k"
    "network_tcp_10k_100k"
    "network_http_100_5k"
    "network_http_500_5k"
    "network_http_1k_5k"
    "network_http_5k_5k"
    "stress_30s_100k_ops"
    "stress_5min_100k_ops"
    "stress_24h_100k_ops"
    "chaos_memory_exhaustion"
    "chaos_process_crash"
    "chaos_network_failure"
    "chaos_scheduler_load"
    "chaos_garbage_collection"
    "chaos_lock_contention"
    "chaos_timeout"
    "chaos_link_failure"
    "chaos_monitor_failure"
    "chaos_registry_failure"
    "integration_basic_initialize"
    "integration_tool_sequence"
    "integration_prompts_workflow"
    "integration_resources_workflow"
    "integration_complete_flow"
)

# Legacy files that should NOT be referenced
declare -a LEGACY_FILES=(
    "benchmark_100k"
    "benchmark_100k_registry"
    "benchmark_100k_SUITE"
    "throughput_SUITE"
    "latency_SUITE"
    "transport_real"
    "bench_stdio"
    "bench_tcp"
    "bench_http"
)

# Ambiguous terms (contexts that need disambiguation)
declare -a AMBIGUOUS_TERMS=(
    "req/s"           # Must be: jsonrpc_req_per_s, etc.
    "connections"     # Must be: sockets_open, concurrent_connections, etc.
)

# Logging functions
log_header() {
    echo -e "${BLUE}===================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}===================================================${NC}"
}

log_pass() {
    echo -e "${GREEN}✓ PASS${NC}: $1"
}

log_fail() {
    echo -e "${RED}✗ FAIL${NC}: $1"
    FAILED=true
}

log_warn() {
    echo -e "${YELLOW}⚠ WARN${NC}: $1"
}

log_info() {
    echo -e "${BLUE}ℹ INFO${NC}: $1"
}

log_verbose() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${BLUE}    └─ $1${NC}"
    fi
}

# Check for legacy file references
check_legacy_files() {
    log_header "Check 1: Legacy File References"

    local violations=0

    for legacy_file in "${LEGACY_FILES[@]}"; do
        # Search in docs for legacy files
        local matches=$(grep -r "$legacy_file" "$DOCS_DIR" 2>/dev/null || true)

        if [[ -n "$matches" ]]; then
            violations=$((violations + 1))
            log_fail "Found legacy reference to '$legacy_file' in docs"

            # Show matching lines
            while IFS= read -r line; do
                file=$(echo "$line" | cut -d: -f1)
                content=$(echo "$line" | cut -d: -f2-)
                log_verbose "$file: ${content:0:80}"
            done <<< "$matches"
        else
            log_verbose "No references to '$legacy_file' found (✓)"
        fi
    done

    if [[ $violations -eq 0 ]]; then
        log_pass "No legacy file references found"
        return 0
    else
        log_fail "Found $violations legacy file reference(s)"
        return 1
    fi
}

# Check for ambiguous metric terms
check_ambiguous_terms() {
    log_header "Check 2: Ambiguous Metric Terms"

    local violations=0

    # Check for "req/s" (must be qualified)
    local req_s_matches=$(grep -rn "req/s" "$DOCS_DIR" 2>/dev/null || true)
    if [[ -n "$req_s_matches" ]]; then
        # Filter out legitimate uses (jsonrpc_req_per_s, etc.)
        local invalid_matches=$(echo "$req_s_matches" | grep -v "jsonrpc_req_per_s" || true)

        if [[ -n "$invalid_matches" ]]; then
            violations=$((violations + 1))
            log_fail "Found ambiguous 'req/s' without proper qualification"

            while IFS= read -r line; do
                if [[ -n "$line" ]]; then
                    file=$(echo "$line" | cut -d: -f1)
                    linenum=$(echo "$line" | cut -d: -f2)
                    content=$(echo "$line" | cut -d: -f3-)
                    log_verbose "$file:$linenum - ${content:0:80}"
                fi
            done <<< "$invalid_matches"
        fi
    fi

    # Check for "connections" without scope qualification
    local conn_matches=$(grep -rn "\bconnections\b" "$DOCS_DIR" 2>/dev/null || true)
    if [[ -n "$conn_matches" ]]; then
        # Filter out legitimate uses (sockets_open, concurrent_connections, etc.)
        local invalid_matches=$(echo "$conn_matches" | grep -vE "sockets_open|concurrent_connections|tcp_connections|http_connections|100K connections|10K connections|5K connections|1K connections|500 connections|150 connections|at.*connections:|connections per|scale.*connections" || true)

        if [[ -n "$invalid_matches" ]]; then
            violations=$((violations + 1))
            log_fail "Found ambiguous 'connections' without proper context"

            while IFS= read -r line; do
                if [[ -n "$line" ]]; then
                    file=$(echo "$line" | cut -d: -f1)
                    linenum=$(echo "$line" | cut -d: -f2)
                    content=$(echo "$line" | cut -d: -f3-)
                    log_verbose "$file:$linenum - ${content:0:80}"
                fi
            done <<< "$invalid_matches"
        fi
    fi

    if [[ $violations -eq 0 ]]; then
        log_pass "No ambiguous metric terms found"
        return 0
    else
        log_fail "Found $violations ambiguous term(s)"
        return 1
    fi
}

# Check for hardcoded performance numbers without context
check_hardcoded_numbers() {
    log_header "Check 3: Hardcoded Performance Numbers"

    local violations=0

    # Look for patterns like "123 msg/s" or "456 ops/sec" without source attribution
    # This is a heuristic - some hardcoded numbers in examples are OK
    local number_patterns=$(grep -rn "\b[0-9]\+\s*\(msg/s\|ops/sec\|requests/s\|throughput\)" "$DOCS_DIR" 2>/dev/null || true)

    if [[ -n "$number_patterns" ]]; then
        # Filter out cases that have context (benchmark results, tables, examples marked as such)
        local potentially_bad=$(echo "$number_patterns" | grep -vE "Baseline|Example|Table|benchmark.*result|JSON|metrology" || true)

        # Only fail if we find suspicious hardcoded numbers in prose text
        local hardcoded_count=$(echo "$potentially_bad" | wc -l)

        if [[ $hardcoded_count -gt 0 && "$potentially_bad" != "" ]]; then
            log_warn "Found $hardcoded_count hardcoded performance number(s) - verify they have proper source attribution"

            while IFS= read -r line; do
                if [[ -n "$line" ]]; then
                    file=$(echo "$line" | cut -d: -f1)
                    linenum=$(echo "$line" | cut -d: -f2)
                    content=$(echo "$line" | cut -d: -f3-)
                    log_verbose "$file:$linenum - ${content:0:80}"
                fi
            done <<< "$potentially_bad" | head -5
        fi
    fi

    log_pass "Hardcoded number check complete (review warnings above)"
    return 0
}

# Check for valid workload_id references
check_workload_ids() {
    log_header "Check 4: Workload ID References"

    local violations=0

    # Find all workload_id references in docs
    local workload_refs=$(grep -rn "workload_id" "$DOCS_DIR" 2>/dev/null || true)

    if [[ -z "$workload_refs" ]]; then
        log_pass "No workload_id references in docs"
        return 0
    fi

    # Extract unique workload IDs referenced using a simpler approach (bash 3 compatibility)
    local referenced_workloads=""
    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi

        # Try to extract workload ID - look for patterns like "core_ops_100k", "stress_5min_100k_ops", etc.
        # Pattern: word characters with underscores
        workload=$(echo "$line" | grep -oE '(core_ops_[0-9kmK]+|network_(tcp|http)_[0-9k]+|stress_[0-9smhSMH]+|chaos_[a-z_]+|integration_[a-z_]+|test_[a-z_]+)' | head -1 || true)
        if [[ -n "$workload" ]]; then
            # Avoid duplicates by checking if already in list
            if ! echo "$referenced_workloads" | grep -q "$workload"; then
                referenced_workloads="$referenced_workloads $workload"
            fi
        fi
    done <<< "$workload_refs"

    # Check each referenced workload is valid
    for workload in $referenced_workloads; do
        local found=false
        for valid in "${VALID_WORKLOADS[@]}"; do
            if [[ "$workload" == "$valid" ]]; then
                found=true
                break
            fi
        done

        if [[ "$found" == "false" && "$workload" != "test" && "$workload" != "custom" ]]; then
            violations=$((violations + 1))
            log_fail "Invalid workload_id reference: '$workload' (not defined in benchmarks)"
        else
            log_verbose "Valid workload_id: '$workload' ✓"
        fi
    done

    if [[ $violations -eq 0 ]]; then
        log_pass "All workload_id references are valid"
        return 0
    else
        log_fail "Found $violations invalid workload_id(s)"
        return 1
    fi
}

# Summary and exit
print_summary() {
    log_header "LINTER SUMMARY"

    if [[ "$FAILED" == "false" ]]; then
        echo -e "${GREEN}✓ All documentation checks passed!${NC}"
        return 0
    else
        echo -e "${RED}✗ Documentation checks failed (see violations above)${NC}"
        return 1
    fi
}

# Print help
print_help() {
    cat << EOF
Documentation Linter for erlmcp

USAGE:
    $0 [OPTIONS]

OPTIONS:
    --check-legacy          Run only legacy file reference checks
    --check-terms           Run only ambiguous term checks
    --check-hardcoded       Run only hardcoded number checks
    --check-workloads       Run only workload ID checks
    --verbose              Print verbose output
    --help                 Show this help message

EXAMPLES:
    # Run all checks
    $0

    # Run with verbose output
    $0 --verbose

    # Check specific aspect
    $0 --check-legacy

EXIT CODES:
    0 - All checks passed
    1 - One or more checks failed

DOCUMENTATION:
    See docs/bench/README.md for benchmark documentation standards.
EOF
}

# Main execution
main() {
    local check_legacy=true
    local check_terms=true
    local check_hardcoded=true
    local check_workloads=true

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --check-legacy)
                check_legacy=true
                check_terms=false
                check_hardcoded=false
                check_workloads=false
                ;;
            --check-terms)
                check_legacy=false
                check_terms=true
                check_hardcoded=false
                check_workloads=false
                ;;
            --check-hardcoded)
                check_legacy=false
                check_terms=false
                check_hardcoded=true
                check_workloads=false
                ;;
            --check-workloads)
                check_legacy=false
                check_terms=false
                check_hardcoded=false
                check_workloads=true
                ;;
            --verbose)
                VERBOSE=true
                ;;
            --help)
                print_help
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                print_help
                exit 1
                ;;
        esac
        shift
    done

    # Change to project root
    cd "$PROJECT_ROOT"

    # Run selected checks
    [[ "$check_legacy" == "true" ]] && check_legacy_files
    [[ "$check_terms" == "true" ]] && check_ambiguous_terms
    [[ "$check_hardcoded" == "true" ]] && check_hardcoded_numbers
    [[ "$check_workloads" == "true" ]] && check_workload_ids

    # Print summary and exit
    print_summary
    exit $?
}

# Run main function
main "$@"
