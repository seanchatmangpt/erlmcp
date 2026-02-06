#!/usr/bin/env bash
# ============================================================================
# erlmcp Load Test Examples - Docker-Only Constitution
# ============================================================================
# Quick reference for common load testing scenarios
# All examples use Docker-based execution
# ============================================================================

set -euo pipefail

# ============================================================================
# Example 1: Quick baseline test (local development)
# ============================================================================
baseline_test_local() {
    echo "=== Example 1: Baseline Test (Local) ==="
    docker compose run --rm erlmcp-loadtest \
        --profile baseline \
        --url http://erlmcp:8080
}

# ============================================================================
# Example 2: Stress test with custom parameters
# ============================================================================
stress_test_custom() {
    echo "=== Example 2: Custom Stress Test ==="
    docker compose run --rm erlmcp-loadtest \
        --profile stress \
        --duration 180 \
        --concurrency 150 \
        --url http://erlmcp:8080
}

# ============================================================================
# Example 3: Test GCP Marketplace deployment
# ============================================================================
test_gcp_deployment() {
    echo "=== Example 3: GCP Marketplace Deployment Test ==="

    # Set GCP environment variables
    export GCP_PROJECT_ID="${GCP_PROJECT_ID:-my-project}"
    export GCP_INSTANCE_NAME="${GCP_INSTANCE_NAME:-erlmcp-marketplace-test}"
    export GCP_ZONE="${GCP_ZONE:-us-central1-a}"

    docker compose run --rm \
        -e GCP_PROJECT_ID="${GCP_PROJECT_ID}" \
        -e GCP_INSTANCE_NAME="${GCP_INSTANCE_NAME}" \
        -e GCP_ZONE="${GCP_ZONE}" \
        erlmcp-loadtest --profile stress
}

# ============================================================================
# Example 4: Spike test (sudden traffic surge)
# ============================================================================
spike_test() {
    echo "=== Example 4: Spike Test ==="
    docker compose run --rm erlmcp-loadtest \
        --profile spike \
        --url http://erlmcp:8080
}

# ============================================================================
# Example 5: Soak test (long-running stability)
# ============================================================================
soak_test() {
    echo "=== Example 5: Soak Test (1 hour) ==="
    docker compose run --rm erlmcp-loadtest \
        --profile soak \
        --url http://erlmcp:8080
}

# ============================================================================
# Example 6: Capacity test (maximum throughput)
# ============================================================================
capacity_test() {
    echo "=== Example 6: Capacity Test ==="
    docker compose run --rm erlmcp-loadtest \
        --profile capacity \
        --url http://erlmcp:8080
}

# ============================================================================
# Example 7: Build and run standalone container
# ============================================================================
standalone_container() {
    echo "=== Example 7: Standalone Container ==="

    # Build
    docker build \
        -f marketplace/gcp/scripts/Dockerfile.loadtest \
        -t erlmcp-loadtest:latest \
        .

    # Run
    docker run --rm --network host \
        -v /tmp/loadtest-results:/results \
        erlmcp-loadtest:latest \
        --profile baseline \
        --url http://localhost:8080
}

# ============================================================================
# Example 8: CI/CD integration (GitHub Actions style)
# ============================================================================
ci_cd_test() {
    echo "=== Example 8: CI/CD Integration ==="

    # Start services
    docker compose --profile runtime up -d

    # Wait for health check
    sleep 30

    # Run load test
    docker compose run --rm erlmcp-loadtest \
        --profile baseline \
        --url http://erlmcp:8080

    # Capture exit code
    local exit_code=$?

    # Cleanup
    docker compose --profile runtime down

    # Return exit code (0 = pass, 1 = fail)
    return ${exit_code}
}

# ============================================================================
# Example 9: Progressive load testing
# ============================================================================
progressive_test() {
    echo "=== Example 9: Progressive Load Test ==="

    local profiles=(baseline stress spike)

    for profile in "${profiles[@]}"; do
        echo "Running profile: ${profile}"
        docker compose run --rm erlmcp-loadtest \
            --profile "${profile}" \
            --url http://erlmcp:8080

        # Cool down between tests
        sleep 10
    done
}

# ============================================================================
# Example 10: Custom JSON-RPC endpoint testing
# ============================================================================
custom_endpoint_test() {
    echo "=== Example 10: Custom Endpoint Test ==="

    docker compose run --rm erlmcp-loadtest \
        --profile baseline \
        --url http://erlmcp:8080/api/v1/custom
}

# ============================================================================
# Main menu
# ============================================================================
show_menu() {
    cat <<EOF

=================================================================
erlmcp Load Test Examples - Docker-Only Constitution
=================================================================

Select an example to run:

 1. Baseline Test (Local Development)
 2. Custom Stress Test
 3. GCP Marketplace Deployment Test
 4. Spike Test (Sudden Traffic Surge)
 5. Soak Test (1-hour Stability)
 6. Capacity Test (Maximum Throughput)
 7. Standalone Container
 8. CI/CD Integration Test
 9. Progressive Load Testing
10. Custom Endpoint Test
 0. Exit

=================================================================
EOF

    read -p "Enter choice [0-10]: " choice

    case ${choice} in
        1) baseline_test_local ;;
        2) stress_test_custom ;;
        3) test_gcp_deployment ;;
        4) spike_test ;;
        5) soak_test ;;
        6) capacity_test ;;
        7) standalone_container ;;
        8) ci_cd_test ;;
        9) progressive_test ;;
        10) custom_endpoint_test ;;
        0) exit 0 ;;
        *) echo "Invalid choice" ;;
    esac
}

# ============================================================================
# Entry point
# ============================================================================
main() {
    if [ $# -eq 0 ]; then
        # Interactive mode
        while true; do
            show_menu
            echo ""
            read -p "Press Enter to continue..."
        done
    else
        # Direct command execution
        case "$1" in
            baseline) baseline_test_local ;;
            stress) stress_test_custom ;;
            gcp) test_gcp_deployment ;;
            spike) spike_test ;;
            soak) soak_test ;;
            capacity) capacity_test ;;
            standalone) standalone_container ;;
            ci) ci_cd_test ;;
            progressive) progressive_test ;;
            custom) custom_endpoint_test ;;
            *)
                echo "Usage: $0 [baseline|stress|gcp|spike|soak|capacity|standalone|ci|progressive|custom]"
                exit 1
                ;;
        esac
    fi
}

main "$@"
