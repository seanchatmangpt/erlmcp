#!/bin/bash
# ============================================================================
# Health Check Script for erlmcp GCP Marketplace Deployments
# Tests all endpoints across GKE, Cloud Run, and GCE deployments
# ============================================================================
#
# WHY: Production deadline requires comprehensive endpoint validation
# WHAT: Curl-based health checks for HTTP/JSON-RPC endpoints
# HOW: docker compose run erlmcp-build ./marketplace/gcp/scripts/health-check.sh
#
# Usage:
#   ./health-check.sh [OPTIONS]
#
# Options:
#   --gke URL         Test GKE endpoint
#   --cloudrun URL    Test Cloud Run endpoint
#   --gce URL         Test GCE endpoint
#   --all URL         Test all endpoints on URL
#   --timeout SECS    Request timeout (default: 10)
#   --evidence DIR    Evidence output directory
#   --json            Output JSON format
#   --verbose         Verbose output
#
# ============================================================================

set -euo pipefail

# ============================================================================
# Colors & Logging
# ============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }
log_pass() { echo -e "${GREEN}  ✓${NC} $1"; }
log_fail() { echo -e "${RED}  ✗${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

TIMEOUT="${TIMEOUT:-10}"
EVIDENCE_DIR="${EVIDENCE_DIR:-/tmp/health-check-evidence-$(date +%s)}"
VERBOSE="${VERBOSE:-false}"
JSON_OUTPUT="${JSON_OUTPUT:-false}"

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Results storage
declare -A TEST_RESULTS

# ============================================================================
# Helper Functions
# ============================================================================

# Execute curl with standard options
curl_get() {
    local url="$1"
    local output_file="$2"
    local timeout="${3:-$TIMEOUT}"

    curl -s -f -m "$timeout" \
         -w "\n%{http_code}\n%{time_total}\n" \
         -o "$output_file" \
         "$url" 2>&1 || echo "000\n0\n"
}

# Execute curl POST with JSON body
curl_post_json() {
    local url="$1"
    local data="$2"
    local output_file="$3"
    local timeout="${4:-$TIMEOUT}"

    curl -s -f -m "$timeout" \
         -X POST \
         -H "Content-Type: application/json" \
         -d "$data" \
         -w "\n%{http_code}\n%{time_total}\n" \
         -o "$output_file" \
         "$url" 2>&1 || echo "000\n0\n"
}

# Parse curl output
parse_response() {
    local file="$1"

    if [ ! -f "$file" ]; then
        echo "000|0|error"
        return 1
    fi

    # Read last three lines for status code and timing
    local lines=$(tail -n 2 "$file" 2>/dev/null || echo "000\n0")
    local http_code=$(echo "$lines" | head -n 1)
    local time_total=$(echo "$lines" | tail -n 1)

    # Remove last two lines from response body
    head -n -2 "$file" > "$file.body" 2>/dev/null || true

    echo "${http_code}|${time_total}|success"
}

# Validate JSON response
validate_json() {
    local file="$1"

    if [ ! -f "$file.body" ]; then
        return 1
    fi

    if jq empty "$file.body" 2>/dev/null; then
        return 0
    else
        return 1
    fi
}

# Record test result
record_test() {
    local name="$1"
    local status="$2"
    local message="$3"
    local response_time="${4:-0}"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if [ "$status" = "pass" ]; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
        log_pass "$name (${response_time}s)"
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
        log_fail "$name - $message"
    fi

    TEST_RESULTS["$name"]="$status|$message|$response_time"
}

# ============================================================================
# Test Functions
# ============================================================================

# Test /health endpoint
test_health_endpoint() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing /health endpoint on $deployment_type"

    local url="${base_url}/health"
    local output="$EVIDENCE_DIR/${deployment_type}-health.txt"

    curl_get "$url" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        if validate_json "$output"; then
            local status=$(jq -r '.status // .Status // "unknown"' "$output.body" 2>/dev/null)
            if [ "$status" = "ok" ] || [ "$status" = "healthy" ] || [ "$status" = "up" ]; then
                record_test "${deployment_type}:/health" "pass" "Status: $status" "$time_total"
            else
                record_test "${deployment_type}:/health" "fail" "Unexpected status: $status" "$time_total"
            fi
        else
            record_test "${deployment_type}:/health" "fail" "Invalid JSON response" "$time_total"
        fi
    else
        record_test "${deployment_type}:/health" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && cat "$output.body" 2>/dev/null || true
}

# Test /ready endpoint
test_ready_endpoint() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing /ready endpoint on $deployment_type"

    local url="${base_url}/ready"
    local output="$EVIDENCE_DIR/${deployment_type}-ready.txt"

    curl_get "$url" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        record_test "${deployment_type}:/ready" "pass" "Ready" "$time_total"
    else
        record_test "${deployment_type}:/ready" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && cat "$output.body" 2>/dev/null || true
}

# Test /metrics endpoint
test_metrics_endpoint() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing /metrics endpoint on $deployment_type"

    local url="${base_url}/metrics"
    local output="$EVIDENCE_DIR/${deployment_type}-metrics.txt"

    curl_get "$url" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        # Check for Prometheus format
        if grep -q "^# HELP\|^# TYPE" "$output.body" 2>/dev/null; then
            local metric_count=$(grep -c "^# TYPE" "$output.body" 2>/dev/null || echo "0")
            record_test "${deployment_type}:/metrics" "pass" "$metric_count metrics exposed" "$time_total"
        else
            record_test "${deployment_type}:/metrics" "fail" "Invalid Prometheus format" "$time_total"
        fi
    else
        record_test "${deployment_type}:/metrics" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && head -n 20 "$output.body" 2>/dev/null || true
}

# Test MCP initialize endpoint
test_mcp_initialize() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing MCP initialize on $deployment_type"

    local url="${base_url}/mcp"
    local output="$EVIDENCE_DIR/${deployment_type}-mcp-initialize.txt"

    local request_data='{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "health-check",
                "version": "1.0.0"
            }
        }
    }'

    curl_post_json "$url" "$request_data" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        if validate_json "$output"; then
            local protocol_version=$(jq -r '.result.protocolVersion // empty' "$output.body" 2>/dev/null)
            if [ -n "$protocol_version" ]; then
                record_test "${deployment_type}:MCP-initialize" "pass" "Protocol: $protocol_version" "$time_total"
            else
                record_test "${deployment_type}:MCP-initialize" "fail" "Missing protocolVersion" "$time_total"
            fi
        else
            record_test "${deployment_type}:MCP-initialize" "fail" "Invalid JSON response" "$time_total"
        fi
    else
        record_test "${deployment_type}:MCP-initialize" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && cat "$output.body" 2>/dev/null || true
}

# Test MCP tools/list endpoint
test_mcp_tools_list() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing MCP tools/list on $deployment_type"

    local url="${base_url}/mcp"
    local output="$EVIDENCE_DIR/${deployment_type}-mcp-tools-list.txt"

    local request_data='{
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list",
        "params": {}
    }'

    curl_post_json "$url" "$request_data" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        if validate_json "$output"; then
            local tools_count=$(jq -r '.result.tools | length // 0' "$output.body" 2>/dev/null)
            record_test "${deployment_type}:MCP-tools/list" "pass" "$tools_count tools" "$time_total"
        else
            record_test "${deployment_type}:MCP-tools/list" "fail" "Invalid JSON response" "$time_total"
        fi
    else
        record_test "${deployment_type}:MCP-tools/list" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && cat "$output.body" 2>/dev/null || true
}

# Test MCP resources/list endpoint
test_mcp_resources_list() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing MCP resources/list on $deployment_type"

    local url="${base_url}/mcp"
    local output="$EVIDENCE_DIR/${deployment_type}-mcp-resources-list.txt"

    local request_data='{
        "jsonrpc": "2.0",
        "id": 3,
        "method": "resources/list",
        "params": {}
    }'

    curl_post_json "$url" "$request_data" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        if validate_json "$output"; then
            local resources_count=$(jq -r '.result.resources | length // 0' "$output.body" 2>/dev/null)
            record_test "${deployment_type}:MCP-resources/list" "pass" "$resources_count resources" "$time_total"
        else
            record_test "${deployment_type}:MCP-resources/list" "fail" "Invalid JSON response" "$time_total"
        fi
    else
        record_test "${deployment_type}:MCP-resources/list" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && cat "$output.body" 2>/dev/null || true
}

# Test MCP prompts/list endpoint
test_mcp_prompts_list() {
    local base_url="$1"
    local deployment_type="$2"

    log_test "Testing MCP prompts/list on $deployment_type"

    local url="${base_url}/mcp"
    local output="$EVIDENCE_DIR/${deployment_type}-mcp-prompts-list.txt"

    local request_data='{
        "jsonrpc": "2.0",
        "id": 4,
        "method": "prompts/list",
        "params": {}
    }'

    curl_post_json "$url" "$request_data" "$output" "$TIMEOUT"
    local result=$(parse_response "$output")

    local http_code=$(echo "$result" | cut -d'|' -f1)
    local time_total=$(echo "$result" | cut -d'|' -f2)

    if [ "$http_code" = "200" ]; then
        if validate_json "$output"; then
            local prompts_count=$(jq -r '.result.prompts | length // 0' "$output.body" 2>/dev/null)
            record_test "${deployment_type}:MCP-prompts/list" "pass" "$prompts_count prompts" "$time_total"
        else
            record_test "${deployment_type}:MCP-prompts/list" "fail" "Invalid JSON response" "$time_total"
        fi
    else
        record_test "${deployment_type}:MCP-prompts/list" "fail" "HTTP $http_code" "$time_total"
    fi

    [ "$VERBOSE" = "true" ] && cat "$output.body" 2>/dev/null || true
}

# Test all endpoints for a deployment
test_all_endpoints() {
    local base_url="$1"
    local deployment_type="$2"

    echo ""
    log_info "Testing $deployment_type deployment: $base_url"
    echo "------------------------------------------------------------"

    # HTTP health endpoints
    test_health_endpoint "$base_url" "$deployment_type"
    test_ready_endpoint "$base_url" "$deployment_type"
    test_metrics_endpoint "$base_url" "$deployment_type"

    # MCP JSON-RPC endpoints
    test_mcp_initialize "$base_url" "$deployment_type"
    test_mcp_tools_list "$base_url" "$deployment_type"
    test_mcp_resources_list "$base_url" "$deployment_type"
    test_mcp_prompts_list "$base_url" "$deployment_type"

    echo ""
}

# ============================================================================
# Reporting
# ============================================================================

generate_summary() {
    echo ""
    echo "============================================================"
    log_info "Health Check Summary"
    echo "============================================================"
    echo ""
    echo "Total Tests:  $TOTAL_TESTS"
    echo "Passed:       $PASSED_TESTS"
    echo "Failed:       $FAILED_TESTS"
    echo ""

    local success_rate=0
    if [ "$TOTAL_TESTS" -gt 0 ]; then
        success_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    fi

    echo "Success Rate: ${success_rate}%"
    echo ""

    if [ "$FAILED_TESTS" -gt 0 ]; then
        echo "Failed Tests:"
        for test_name in "${!TEST_RESULTS[@]}"; do
            local result="${TEST_RESULTS[$test_name]}"
            local status=$(echo "$result" | cut -d'|' -f1)
            local message=$(echo "$result" | cut -d'|' -f2)

            if [ "$status" = "fail" ]; then
                log_fail "$test_name: $message"
            fi
        done
        echo ""
    fi

    echo "Evidence saved to: $EVIDENCE_DIR"
    echo "============================================================"
    echo ""

    # Exit with error if any tests failed
    [ "$FAILED_TESTS" -eq 0 ] && return 0 || return 1
}

generate_json_output() {
    local json_file="$EVIDENCE_DIR/health-check-results.json"

    cat > "$json_file" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "summary": {
    "total": $TOTAL_TESTS,
    "passed": $PASSED_TESTS,
    "failed": $FAILED_TESTS,
    "success_rate": $((PASSED_TESTS * 100 / TOTAL_TESTS))
  },
  "tests": {
EOF

    local first=true
    for test_name in "${!TEST_RESULTS[@]}"; do
        local result="${TEST_RESULTS[$test_name]}"
        local status=$(echo "$result" | cut -d'|' -f1)
        local message=$(echo "$result" | cut -d'|' -f2)
        local response_time=$(echo "$result" | cut -d'|' -f3)

        [ "$first" = false ] && echo "," >> "$json_file"
        first=false

        cat >> "$json_file" <<EOF
    "$test_name": {
      "status": "$status",
      "message": "$message",
      "response_time": $response_time
    }
EOF
    done

    cat >> "$json_file" <<EOF

  }
}
EOF

    log_info "JSON output saved to: $json_file"
}

# ============================================================================
# Main
# ============================================================================

usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --gke URL         Test GKE deployment endpoint
  --cloudrun URL    Test Cloud Run deployment endpoint
  --gce URL         Test GCE deployment endpoint
  --all URL         Test all endpoints on single URL
  --timeout SECS    Request timeout (default: 10)
  --evidence DIR    Evidence output directory
  --json            Output JSON format
  --verbose         Verbose output
  -h, --help        Show this help message

Examples:
  $0 --gke https://erlmcp.example.com
  $0 --cloudrun https://erlmcp-xyz.run.app
  $0 --gce http://35.123.45.67
  $0 --all https://erlmcp.example.com --timeout 30
  $0 --gke URL1 --cloudrun URL2 --gce URL3 --json

EOF
    exit 1
}

main() {
    local gke_url=""
    local cloudrun_url=""
    local gce_url=""
    local all_url=""

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --gke)
                gke_url="$2"
                shift 2
                ;;
            --cloudrun)
                cloudrun_url="$2"
                shift 2
                ;;
            --gce)
                gce_url="$2"
                shift 2
                ;;
            --all)
                all_url="$2"
                shift 2
                ;;
            --timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            --evidence)
                EVIDENCE_DIR="$2"
                shift 2
                ;;
            --json)
                JSON_OUTPUT=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                usage
                ;;
            *)
                log_error "Unknown option: $1"
                usage
                ;;
        esac
    done

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Check prerequisites
    if ! command -v curl &> /dev/null; then
        log_error "curl not found - required for health checks"
        exit 1
    fi

    if ! command -v jq &> /dev/null; then
        log_warn "jq not found - JSON validation will be limited"
    fi

    # Validate at least one URL provided
    if [ -z "$gke_url" ] && [ -z "$cloudrun_url" ] && [ -z "$gce_url" ] && [ -z "$all_url" ]; then
        log_error "No endpoint URL provided"
        usage
    fi

    log_info "Starting health checks..."
    log_info "Timeout: ${TIMEOUT}s"
    log_info "Evidence: $EVIDENCE_DIR"
    echo ""

    # Run tests
    if [ -n "$all_url" ]; then
        test_all_endpoints "$all_url" "ALL"
    else
        [ -n "$gke_url" ] && test_all_endpoints "$gke_url" "GKE"
        [ -n "$cloudrun_url" ] && test_all_endpoints "$cloudrun_url" "CloudRun"
        [ -n "$gce_url" ] && test_all_endpoints "$gce_url" "GCE"
    fi

    # Generate reports
    if [ "$JSON_OUTPUT" = "true" ]; then
        generate_json_output
    fi

    generate_summary
}

# Run main
main "$@"
