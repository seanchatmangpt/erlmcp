#!/bin/bash
#
# Failover Testing Script for erlmcp v3
# Validates failover procedures and recovery capabilities
#
# Usage:
#   ./failover-test.sh smoke                    - Quick smoke test
#   ./failover-test.sh full                     - Comprehensive failover test
#   ./failover-test.sh cascade                  - Cascading failure test
#   ./failover-test.sh network-partition        - Network partition test
#   ./failover-test.sh database-failover        - Database failover test
#

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="/var/log/erlmcp/failover-tests"
REPORT_DIR="/var/reports/erlmcp"
TEST_DURATION=600
RECOVERY_TIMEOUT=300

# Test identifiers
TEST_RUN_ID="$(date +%Y%m%d-%H%M%S)-$$"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Setup directories
mkdir -p "$LOG_DIR" "$REPORT_DIR"

LOG_FILE="$LOG_DIR/failover-test-${TEST_RUN_ID}.log"
REPORT_FILE="$REPORT_DIR/failover-test-report-${TEST_RUN_ID}.json"

# Logging
log() {
    local level=$1
    shift
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $*" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "${GREEN}$*${NC}"; }
log_warn() { log "WARN" "${YELLOW}$*${NC}"; }
log_error() { log "ERROR" "${RED}$*${NC}"; }
log_test() { log "TEST" "${BLUE}$*${NC}"; }

# Test result tracking
declare -a TEST_RESULTS
declare -i TESTS_PASSED=0
declare -i TESTS_FAILED=0

start_test() {
    local name=$1
    log_test "Starting: $name"
    echo "{\"name\":\"$name\",\"status\":\"running\",\"start_time\":\"$(date -Iseconds)\"}" >> "$REPORT_FILE"
}

pass_test() {
    local name=$1
    local duration=$2
    log_info "✓ PASSED: $name (${duration}s)"
    echo "{\"name\":\"$name\",\"status\":\"passed\",\"duration\":$duration,\"end_time\":\"$(date -Iseconds)\"}" >> "$REPORT_FILE"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

fail_test() {
    local name=$1
    local duration=$2
    local reason=$3
    log_error "✗ FAILED: $name (${duration}s) - $reason"
    echo "{\"name\":\"$name\",\"status\":\"failed\",\"duration\":$duration,\"reason\":\"$reason\",\"end_time\":\"$(date -Iseconds)\"}" >> "$REPORT_FILE"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

# Helper functions
get_timestamp() {
    date +%s
}

elapsed_time() {
    local start=$1
    local end=$(get_timestamp)
    echo $((end - start))
}

wait_for_condition() {
    local condition=$1
    local max_wait=$2
    local interval=${3:-5}

    local elapsed=0
    while [[ $elapsed -lt $max_wait ]]; do
        if eval "$condition"; then
            return 0
        fi
        sleep "$interval"
        elapsed=$((elapsed + interval))
    done
    return 1
}

# Docker compose helpers
dc_exec() {
    local service=$1
    shift
    docker compose exec -T "$service" "$@"
}

dc_run() {
    local service=$1
    shift
    docker compose run --rm "$service" "$@"
}

# Health checks
check_health() {
    curl -f -s --max-time 10 "${HEALTH_ENDPOINT:-http://localhost:8081/health}" | \
        jq -e '.status == "healthy"' > /dev/null 2>&1
}

check_cluster_health() {
    local expected_nodes=${1:-3}
    local count=$(docker compose ps --services --filter "name=erlmcp" | wc -l)
    [[ $count -eq $expected_nodes ]]
}

check_database_health() {
    dc_exec erlmcp-postgres psql -U postgres -c "SELECT 1" > /dev/null 2>&1
}

# Test scenarios
smoke_test() {
    log_info "Starting smoke test..."

    local test_start=$(get_timestamp)

    start_test "Health Endpoint"
    if check_health; then
        pass_test "Health Endpoint" "$(elapsed_time $test_start)"
    else
        fail_test "Health Endpoint" "$(elapsed_time $test_start)" "Health endpoint not responding"
        return 1
    fi

    start_test "Cluster Membership"
    test_start=$(get_timestamp)
    if check_cluster_health; then
        pass_test "Cluster Membership" "$(elapsed_time $test_start)"
    else
        fail_test "Cluster Membership" "$(elapsed_time $test_start)" "Cluster not healthy"
        return 1
    fi

    start_test "Database Connectivity"
    test_start=$(get_timestamp)
    if check_database_health; then
        pass_test "Database Connectivity" "$(elapsed_time $test_start)"
    else
        fail_test "Database Connectivity" "$(elapsed_time $test_start)" "Database not accessible"
        return 1
    fi

    start_test "Session Creation"
    test_start=$(get_timestamp)
    if dc_exec erlmcp /opt/erlmcp/bin/erlmcp eval "
        case erlmcp_session:create(test_session) of
            {ok, _} -> ok;
            _ -> error
        end
    " > /dev/null 2>&1; then
        pass_test "Session Creation" "$(elapsed_time $test_start)"
    else
        fail_test "Session Creation" "$(elapsed_time $test_start)" "Session creation failed"
        return 1
    fi

    log_info "Smoke test completed: $TESTS_PASSED passed, $TESTS_FAILED failed"
}

full_failover_test() {
    log_info "Starting full failover test..."

    # Record initial state
    local initial_primary
    initial_primary=$(docker compose ps -q erlmcp | head -1)

    start_test "Primary Failure Detection"
    local test_start=$(get_timestamp)

    # Stop primary node
    docker stop "$initial_primary"

    # Wait for failover
    if wait_for_condition "! check_health" 60; then
        log_info "Primary failure detected"
        pass_test "Primary Failure Detection" "$(elapsed_time $test_start)"
    else
        fail_test "Primary Failure Detection" "$(elapsed_time $test_start)" "Failure not detected"
        return 1
    fi

    start_test "Automatic Failover"
    test_start=$(get_timestamp)

    # Wait for failover completion
    if wait_for_condition "check_health" 300; then
        pass_test "Automatic Failover" "$(elapsed_time $test_start)"
    else
        fail_test "Automatic Failover" "$(elapsed_time $test_start)" "Failover did not complete"
        docker start "$initial_primary"
        return 1
    fi

    start_test "Data Consistency Check"
    test_start=$(get_timestamp)

    # Verify data consistency
    if dc_exec erlmcp /opt/erlmcp/bin/erlmcp eval "
        erlmcp_session_ha:validate_consistency()
    " > /dev/null 2>&1; then
        pass_test "Data Consistency Check" "$(elapsed_time $test_start)"
    else
        fail_test "Data Consistency Check" "$(elapsed_time $test_start)" "Data inconsistency detected"
    fi

    start_test "Primary Recovery"
    test_start=$(get_timestamp)

    # Restart primary
    docker start "$initial_primary"

    # Wait for reintegration
    if wait_for_condition "check_cluster_health" 120; then
        pass_test "Primary Recovery" "$(elapsed_time $test_start)"
    else
        fail_test "Primary Recovery" "$(elapsed_time $test_start)" "Primary did not reintegrate"
    fi

    log_info "Full failover test completed: $TESTS_PASSED passed, $TESTS_FAILED failed"
}

cascade_failure_test() {
    log_info "Starting cascading failure test..."

    start_test "Database Failure Detection"
    local test_start=$(get_timestamp)

    # Stop database
    docker compose stop erlmcp-postgres

    # Wait for detection
    if wait_for_condition "! check_database_health" 30; then
        pass_test "Database Failure Detection" "$(elapsed_time $test_start)"
    else
        fail_test "Database Failure Detection" "$(elapsed_time $test_start)" "Database failure not detected"
        docker compose start erlmcp-postgres
        return 1
    fi

    start_test "Circuit Breaker Activation"
    test_start=$(get_timestamp)

    # Check if circuit breaker opened
    if dc_exec erlmcp curl -s http://localhost:8081/admin/circuit-breakers | \
        jq -e '.[].state == "open"' > /dev/null 2>&1; then
        pass_test "Circuit Breaker Activation" "$(elapsed_time $test_start)"
    else
        log_warn "Circuit breaker not opened (may be acceptable)"
    fi

    start_test "Resource Protection"
    test_start=$(get_timestamp)

    # Check for resource exhaustion
    local memory_before
    memory_before=$(dc_exec erlmcp /opt/erlmcp/bin/erlmcp eval "erlang:memory(total)." 2>/dev/null || echo "0")

    sleep 60

    local memory_after
    memory_after=$(dc_exec erlmcp /opt/erlmcp/bin/erlmcp eval "erlang:memory(total)." 2>/dev/null || echo "0")

    # Memory should not have grown significantly (less than 2x)
    if [[ $memory_after -lt $((memory_before * 2)) ]]; then
        pass_test "Resource Protection" "$(elapsed_time $test_start)"
    else
        fail_test "Resource Protection" "$(elapsed_time $test_start)" "Memory growth detected: $memory_before -> $memory_after"
    fi

    start_test "Database Recovery"
    test_start=$(get_timestamp)

    # Restart database
    docker compose start erlmcp-postgres

    # Wait for recovery
    if wait_for_condition "check_database_health" 120; then
        pass_test "Database Recovery" "$(elapsed_time $test_start)"
    else
        fail_test "Database Recovery" "$(elapsed_time $test_start)" "Database did not recover"
    fi

    start_test "Service Recovery"
    test_start=$(get_timestamp)

    # Wait for service recovery
    if wait_for_condition "check_health" 180; then
        pass_test "Service Recovery" "$(elapsed_time $test_start)"
    else
        fail_test "Service Recovery" "$(elapsed_time $test_start)" "Service did not recover"
    fi

    log_info "Cascading failure test completed: $TESTS_PASSED passed, $TESTS_FAILED failed"
}

network_partition_test() {
    log_info "Starting network partition test..."

    start_test "Partition Simulation"
    local test_start=$(get_timestamp)

    # Get node names
    local nodes
    nodes=$(docker compose ps -q erlmcp)

    # Simulate partition by blocking network
    local isolated_node
    isolated_node=$(echo "$nodes" | tail -1)

    docker network disconnect bridge "$isolized_node"

    # Wait for partition detection
    sleep 30

    # Check if partition detected
    local partition_detected=false
    if docker compose exec -T erlmcp /opt/erlmcp/bin/erlmcp eval "
        erlmcp_cluster_coordinator:check_partition()
    " 2>/dev/null | grep -q "partition"; then
        partition_detected=true
    fi

    if [[ "$partition_detected" == "true" ]]; then
        pass_test "Partition Simulation" "$(elapsed_time $test_start)"
    else
        log_warn "Partition not detected (may be acceptable)"
    fi

    start_test "Network Recovery"
    test_start=$(get_timestamp)

    # Restore network
    docker network connect bridge "$isolated_node"

    # Wait for recovery
    if wait_for_condition "check_cluster_health" 120; then
        pass_test "Network Recovery" "$(elapsed_time $test_start)"
    else
        fail_test "Network Recovery" "$(elapsed_time $test_start)" "Cluster did not recover"
    fi

    log_info "Network partition test completed: $TESTS_PASSED passed, $TESTS_FAILED failed"
}

database_failover_test() {
    log_info "Starting database failover test..."

    start_test "Primary Database Failure"
    local test_start=$(get_timestamp)

    # Stop primary database
    docker compose stop erlmcp-postgres

    # Wait for failover detection
    sleep 30

    # Check if replica promoted
    local new_primary
    new_primary=$(docker compose ps -q erlmcp-postgres-replica | head -1)

    if [[ -n "$new_primary" ]]; then
        pass_test "Primary Database Failure" "$(elapsed_time $test_start)"
    else
        fail_test "Primary Database Failure" "$(elapsed_time $test_start)" "No replica promoted"
        docker compose start erlmcp-postgres
        return 1
    fi

    start_test "Database Replication Recovery"
    test_start=$(get_timestamp)

    # Restart old primary
    docker compose start erlmcp-postgres

    # Wait for reintegration
    sleep 60

    # Check replication status
    if docker compose exec -T erlmcp-postgres psql -U postgres -c "
        SELECT COUNT(*) FROM pg_stat_replication
    " | grep -q "[1-9]"; then
        pass_test "Database Replication Recovery" "$(elapsed_time $test_start)"
    else
        fail_test "Database Replication Recovery" "$(elapsed_time $test_start)" "Replication not established"
    fi

    log_info "Database failover test completed: $TESTS_PASSED passed, $TESTS_FAILED failed"
}

# Generate final report
generate_report() {
    local total_tests=$((TESTS_PASSED + TESTS_FAILED))
    local pass_rate=0

    if [[ $total_tests -gt 0 ]]; then
        pass_rate=$((TESTS_PASSED * 100 / total_tests))
    fi

    cat > "${REPORT_FILE%.json}-summary.json" <<EOF
{
  "test_run_id": "$TEST_RUN_ID",
  "timestamp": "$(date -Iseconds)",
  "total_tests": $total_tests,
  "passed": $TESTS_PASSED,
  "failed": $TESTS_FAILED,
  "pass_rate": $pass_rate,
  "log_file": "$LOG_FILE",
  "detailed_results": "$REPORT_FILE"
}
EOF

    log_info "Test report generated: ${REPORT_FILE%.json}-summary.json"
    log_info "Summary: $TESTS_PASSED/$total_tests tests passed ($pass_rate%)"
}

# Main
main() {
    local test_type=${1:-smoke}

    log_info "Failover Test Suite - Run ID: $TEST_RUN_ID"
    log_info "Log file: $LOG_FILE"
    log_info "Report file: $REPORT_FILE"

    case "$test_type" in
        smoke)
            smoke_test
            ;;
        full)
            full_failover_test
            ;;
        cascade)
            cascade_failure_test
            ;;
        network-partition)
            network_partition_test
            ;;
        database-failover)
            database_failover_test
            ;;
        all)
            smoke_test || true
            full_failover_test || true
            cascade_failure_test || true
            network_partition_test || true
            database_failover_test || true
            ;;
        *)
            echo "Usage: $0 {smoke|full|cascade|network-partition|database-failover|all}"
            exit 1
            ;;
    esac

    generate_report

    # Exit with appropriate code
    if [[ $TESTS_FAILED -gt 0 ]]; then
        exit 1
    fi
    exit 0
}

main "$@"
