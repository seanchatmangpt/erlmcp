#!/bin/bash
#
# High Availability Load Testing Script for erlmcp v3
# Simulates various failure scenarios and validates HA mechanisms
#

set -e

# Configuration
TEST_LOG_FILE="/var/log/erlmcp/ha-load-test.log"
RESULTS_DIR="/var/log/erlmcp/ha-benchmarks"
METRICS_API="https://erlmcp.company.com/api/metrics"
LOAD_DURATION=300  # 5 minutes per test
CONCURRENT_USERS=10000
RAMP_UP_TIME=60
TEST_SCENARIOS=(
    "baseline"
    "node_failure"
    "region_failure"
    "network_partition"
    "database_failover"
    "session_replication"
    "load_balancer_failure"
)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    local level=$1
    shift
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $*" | tee -a $TEST_LOG_FILE
}

# Status function
status() {
    log "INFO" "$*"
    echo -e "${BLUE}ℹ️ $*${NC}"
}

# Success function
success() {
    log "INFO" "$*"
    echo -e "${GREEN}✅ $*${NC}"
}

# Warning function
warning() {
    log "WARN" "$*"
    echo -e "${YELLOW}⚠️ $*${NC}"
}

# Error function
error() {
    log "ERROR" "$*"
    echo -e "${RED}❌ $*${NC}"
    exit 1
}

# Setup test environment
setup_test_environment() {
    status "Setting up test environment..."

    # Create results directory
    mkdir -p $RESULTS_DIR
    chmod 755 $RESULTS_DIR

    # Generate test configuration
    cat > /tmp/ha-test-config.json << EOF
{
    "duration": $LOAD_DURATION,
    "concurrent_users": $CONCURRENT_USERS,
    "ramp_up": $RAMP_UP_TIME,
    "scenarios": ${TEST_SCENARIOS[@]},
    "endpoints": {
        "health": "https://erlmcp.company.com/health",
        "metrics": "https://erlmcp.company.com/api/metrics",
        "session": "https://erlmcp.company.com/api/session",
        "tool": "https://erlmcp.company.com/api/tool"
    }
}
EOF

    # Initialize test data
    curl -s $METRICS_API > $RESULTS_DIR/metrics-baseline.json

    status "Test environment setup completed"
}

# Run baseline load test
run_baseline_test() {
    status "Running baseline load test..."

    # Start load test
    locust -f /tmp/load-test.py \
        --users $CONCURRENT_USERS \
        --spawn-rate=$((CONCURRENT_USERS / RAMP_UP_TIME)) \
        --run-time ${LOAD_DURATION}s \
        --html=$RESULTS_DIR/baseline-report.html \
        --json=$RESULTS_DIR/baseline-results.json \
        --logfile=$RESULTS_DIR/baseline.log \
        --only-summary

    # Collect metrics during test
    start_time=$(date +%s)
    while [ $(( $(date +%s) - start_time )) -lt $LOAD_DURATION ]; do
        curl -s $METRICS_API >> $RESULTS_DIR/baseline-metrics.json
        sleep 5
    done

    # Calculate baseline metrics
    calculate_metrics "baseline"

    success "Baseline test completed"
}

# Simulate node failure
simulate_node_failure() {
    local node_name=$1
    status "Simulating node failure: $node_name"

    # Get node IP
    node_ip=$(kubectl get pods -l app=erlmcp,env=blue -n erlmcp -o jsonpath='{.items[0].status.podIP}')

    # Terminate node container
    kubectl delete pod $node_name -n erlmcp --wait=false

    # Wait for termination
    sleep 10

    # Start recovery process
    kubectl delete pod $node_name -n erlmcp --wait=true

    success "Node failure simulation completed"
}

# Simulate region failure
simulate_region_failure() {
    local region=$1
    status "Simulating region failure: $region"

    case $region in
        us-east-1)
            # Terminate all nodes in us-east-1
            kubectl scale deployment erlmcp-blue --replicas=0 -n erlmcp
            ;;
        eu-west-1)
            # Terminate all nodes in eu-west-1
            kubectl scale deployment erlmcp-green --replicas=0 -n erlmcp
            ;;
    esac

    # Wait for failover
    sleep 30

    success "Region failure simulation completed"
}

# Run HA-specific load test
run_ha_test() {
    local scenario=$1
    status "Running HA load test: $scenario"

    # Start background monitoring
    monitor_metrics $scenario &

    # Run scenario-specific test
    case $scenario in
        node_failure)
            simulate_node_failure "erlmcp-blue-0"
            ;;
        region_failure)
            simulate_region_failure "us-east-1"
            ;;
        network_partition)
            simulate_network_partition
            ;;
        database_failover)
            simulate_database_failover
            ;;
        session_replication)
            test_session_replication
            ;;
        load_balancer_failure)
            simulate_lb_failure
            ;;
    esac

    # Run load test during scenario
    locust -f /tmp/load-test-ha.py \
        --users $((CONCURRENT_USERS / 2)) \
        --spawn-rate=$((CONCURRENT_USERS / RAMP_UP_TIME / 2)) \
        --run-time ${LOAD_DURATION}s \
        --html=$RESULTS_DIR/${scenario}-report.html \
        --json=$RESULTS_DIR/${scenario}-results.json \
        --logfile=$RESULTS_DIR/${scenario}.log

    # Stop monitoring
    kill $MONITOR_PID 2>/dev/null || true

    # Calculate metrics for scenario
    calculate_metrics "$scenario"

    success "HA test '$scenario' completed"
}

# Monitor metrics during test
monitor_metrics() {
    local scenario=$1
    status "Starting metrics monitoring for $scenario"

    $MONITOR_PID=$$

    while true; do
        timestamp=$(date +%s)
        curl -s $METRICS_API > $RESULTS_DIR/${scenario}-metrics-${timestamp}.json
        sleep 5
    done
}

# Calculate performance metrics
calculate_metrics() {
    local scenario=$1
    status "Calculating metrics for: $scenario"

    # Extract key metrics
    local metrics_file=$RESULTS_DIR/${scenario}-metrics.json
    local results_file=$RESULTS_DIR/${scenario}-results.json

    if [ -f "$results_file" ]; then
        # Calculate success rate
        local total_requests=$(jq '.stats.total_rps' $results_file 2>/dev/null || echo "0")
        local failed_requests=$(jq '.stats.total_failed_rps' $results_file 2>/dev/null || echo "0")
        local success_rate=$(echo "scale=2; ($total_requests - $failed_requests) * 100 / $total_requests" | bc 2>/dev/null || echo "0")

        # Calculate average response time
        local avg_response=$(jq '.stats.total_avg_response_time' $results_file 2>/dev/null | cut -d'.' -f1 || echo "0")

        # Save metrics
        cat > $RESULTS_DIR/${scenario}-metrics-summary.json << EOF
{
    "scenario": "$scenario",
    "timestamp": "$(date -Iseconds)",
    "metrics": {
        "total_requests": $total_requests,
        "failed_requests": $failed_requests,
        "success_rate": $success_rate,
        "avg_response_time_ms": $avg_response,
        "test_duration_seconds": $LOAD_DURATION
    }
}
EOF

        status "Metrics calculated for $scenario"
        status "   Success rate: ${success_rate}%"
        status "   Average response time: ${avg_response}ms"
    fi
}

# Simulate network partition
simulate_network_partition() {
    status "Simulating network partition..."

    # Block traffic between regions
    kubectl exec -n erlmcp $(kubectl get pods -l app=haproxy -n erlmcp -o jsonpath='{.items[0].metadata.name}') \
        -- iptables -A FORWARD -d 10.0.2.0/24 -j DROP

    # Wait for partition to take effect
    sleep 10

    # Restore network
    kubectl exec -n erlmcp $(kubectl get pods -l app=haproxy -n erlmcp -o jsonpath='{.items[0].metadata.name}') \
        -- iptables -D FORWARD -d 10.0.2.0/24 -j DROP

    success "Network partition simulation completed"
}

# Simulate database failover
simulate_database_failover() {
    status "Simulating database failover..."

    # Get primary pod
    primary_pod=$(kubectl get pods -l app=postgres,role=primary -n erlmcp -o jsonpath='{.items[0].metadata.name}')

    # Terminate primary
    kubectl delete pod $primary_pod -n erlmcp --wait=false

    # Wait for failover
    sleep 30

    # Verify new primary
    new_primary=$(kubectl get pods -l app=postgres,role=primary -n erlmcp -o jsonpath='{.items[0].metadata.name}')

    if [ -n "$new_primary" ]; then
        success "Database failover successful: $new_primary"
    else
        error "Database failover failed"
    fi
}

# Test session replication
test_session_replication() {
    status "Testing session replication..."

    # Create test sessions
    local session_count=1000
    local created_sessions=()

    for i in $(seq 1 $session_count); do
        local session_data='{"user_id":"test_user_'$i'","data":{"test":true}}'
        local session_id=$(curl -s -X POST \
            -H "Content-Type: application/json" \
            -d "$session_data" \
            $METRICS_API/session | jq -r '.session_id')

        if [ "$session_id" != "null" ]; then
            created_sessions+=("$session_id")
        fi

        sleep 0.01
    done

    # Verify sessions are replicated
    local replicated_count=0
    for session_id in "${created_sessions[@]}"; do
        if curl -s $METRICS_API/session/$session_id | jq -e '.data.test' >/dev/null 2>&1; then
            ((replicated_count++))
        fi
    done

    local replication_rate=$(echo "scale=2; $replicated_count * 100 / $session_count" | bc)

    cat > $RESULTS_DIR/session-replication-metrics.json << EOF
{
    "test_type": "session_replication",
    "created_sessions": $session_count,
    "replicated_sessions": $replicated_count,
    "replication_rate_percent": $replication_rate
}
EOF

    success "Session replication test completed: ${replication_rate}% replication rate"
}

# Simulate load balancer failure
simulate_lb_failure() {
    status "Simulating load balancer failure..."

    # Get haproxy pod
    lb_pod=$(kubectl get pods -l app=haproxy -n erlmcp -o jsonpath='{.items[0].metadata.name}')

    # Terminate haproxy
    kubectl delete pod $lb_pod -n erlmcp --wait=false

    # Wait for termination
    sleep 10

    # Verify failover
    if curl -f -s "https://erlmcp.company.com/health" > /dev/null 2>&1; then
        success "Load balancer failover successful"
    else
        error "Load balancer failover failed"
    fi
}

# Generate comprehensive report
generate_benchmark_report() {
    status "Generating comprehensive benchmark report..."

    # Create summary
    cat > $RESULTS_DIR/benchmark-summary.json << EOF
{
    "generated_at": "$(date -Iseconds)",
    "test_config": {
        "duration_seconds": $LOAD_DURATION,
        "concurrent_users": $CONCURRENT_USERS,
        "ramp_up_seconds": $RAMP_UP_TIME
    },
    "scenarios_completed": $(echo ${TEST_SCENARIOS[@]} | wc -w),
    "baseline_metrics": {
        "success_rate": "$(jq -r '.metrics.success_rate' $RESULTS_DIR/baseline-metrics-summary.json 2>/dev/null || echo 'N/A')",
        "avg_response_time": "$(jq -r '.metrics.avg_response_time_ms' $RESULTS_DIR/baseline-metrics-summary.json 2>/dev/null || echo 'N/A')"
    },
    "ha_test_results": {}
}
EOF

    # Collect all scenario metrics
    for scenario in "${TEST_SCENARIOS[@]}"; do
        if [ -f "$RESULTS_DIR/${scenario}-metrics-summary.json" ]; then
            jq -r ".metrics" $RESULTS_DIR/${scenario}-metrics-summary.json >> $RESULTS_DIR/benchmark-summary.json
        fi
    done

    # Generate HTML report
    cat > $RESULTS_DIR/benchmark-report.html << EOF
<!DOCTYPE html>
<html>
<head>
    <title>erlmcp v3 HA Benchmark Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background: #f0f0f0; padding: 20px; border-radius: 5px; }
        .scenario { margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }
        .metrics { margin-left: 20px; }
        .success { color: green; }
        .warning { color: orange; }
        .error { color: red; }
    </style>
</head>
<body>
    <div class="header">
        <h1>erlmcp v3 High Availability Benchmark Report</h1>
        <p>Generated at: $(date -Iseconds)</p>
        <p>Test Configuration: ${LOAD_DURATION}s duration, ${CONCURRENT_USERS} concurrent users</p>
    </div>

    <div class="scenario">
        <h2>Baseline Performance</h2>
        <div class="metrics">
            <p><strong>Success Rate:</strong> <span class="success">$(jq -r '.metrics.success_rate' $RESULTS_DIR/baseline-metrics-summary.json 2>/dev/null || echo 'N/A')%</span></p>
            <p><strong>Average Response Time:</strong> $(jq -r '.metrics.avg_response_time_ms' $RESULTS_DIR/baseline-metrics-summary.json 2>/dev/null || echo 'N/A')ms</p>
        </div>
    </div>

    <h2>HA Scenario Results</h2>
    EOF

    for scenario in "${TEST_SCENARIOS[@]:1}"; do
        if [ -f "$RESULTS_DIR/${scenario}-metrics-summary.json" ]; then
            cat >> $RESULTS_DIR/benchmark-report.html << EOF
    <div class="scenario">
        <h2>$scenario</h2>
        <div class="metrics">
            <p><strong>Success Rate:</strong> <span class="success">$(jq -r '.metrics.success_rate' $RESULTS_DIR/${scenario}-metrics-summary.json 2>/dev/null || echo 'N/A')%</span></p>
            <p><strong>Average Response Time:</strong> $(jq -r '.metrics.avg_response_time_ms' $RESULTS_DIR/${scenario}-metrics-summary.json 2>/dev/null || echo 'N/A')ms</p>
        </div>
    </div>
EOF
        fi
    done

    cat >> $RESULTS_DIR/benchmark-report.html << EOF
</body>
</html>
EOF

    success "Benchmark report generated: $RESULTS_DIR/benchmark-report.html"
}

# Cleanup test artifacts
cleanup() {
    status "Cleaning up test artifacts..."

    # Remove temporary files
    rm -f /tmp/load-test.py
    rm -f /tmp/load-test-ha.py
    rm -f /tmp/ha-test-config.json

    success "Cleanup completed"
}

# Main execution
main() {
    local test_scenario=${1:-all}

    # Setup test environment
    setup_test_environment

    case $test_scenario in
        all)
            # Run baseline test
            run_baseline_test

            # Run HA tests
            for scenario in "${TEST_SCENARIOS[@]:1}"; do
                run_ha_test "$scenario"
            done
            ;;
        baseline)
            run_baseline_test
            ;;
        *)
            if [[ " ${TEST_SCENARIOS[*]} " =~ " $test_scenario " ]]; then
                run_ha_test "$test_scenario"
            else
                error "Unknown test scenario: $test_scenario"
            fi
            ;;
    esac

    # Generate report
    generate_benchmark_report

    # Cleanup
    cleanup

    success "HA load testing completed successfully"
}

# Create load test scripts
create_load_test_scripts() {
    cat > /tmp/load-test.py << 'EOF'
from locust import HttpUser, task, between
import random

class ErLMCPUser(HttpUser):
    wait_time = between(1, 3)

    @task
    def health_check(self):
        self.client.get("/health")

    @task(3)
    def get_metrics(self):
        self.client.get("/api/metrics")

    @task(5)
    def create_session(self):
        user_id = f"user_{random.randint(1000, 9999)}"
        self.client.post("/api/session", json={"user_id": user_id})

    @task(10)
    def call_tool(self):
        tools = ["analysis", "computation", "generation"]
        tool_name = random.choice(tools)
        self.client.post("/api/tool", json={
            "name": tool_name,
            "parameters": {"input": "test data"}
        })
EOF

    cat > /tmp/load-test-ha.py << 'EOF'
from locust import HttpUser, task, between
import random
import time

class ErLMCPHAUser(HttpUser):
    wait_time = between(1, 5)

    @task
    def health_check(self):
        self.client.get("/health", timeout=30)

    @task(2)
    def test_resilience(self):
        # Test with higher timeout for HA scenarios
        try:
            response = self.client.get("/api/session", timeout=60)
        except:
            # Session may be unavailable during failover
            pass
EOF
}

# Initialize test scripts
create_load_test_scripts

# Run main function
main "$@"