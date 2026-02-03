#!/bin/bash

# Comprehensive smoke tests for erlmcp deployment
set -e

ENVIRONMENT=$1
BASE_URL=$2
CANARY=$3

if [ -z "$ENVIRONMENT" ] || [ -z "$BASE_URL" ]; then
    echo "Usage: $0 <environment> <base_url> [canary_percentage]"
    exit 1
fi

# Configuration
TIMEOUT=${TIMEOUT:-30}
MAX_RETRIES=${MAX_RETRIES:-3}
DELAY=${DELAY:-2}

# Test endpoints and expected responses
declare -A ENDPOINTS=(
    ["$BASE_URL/health"]="200"
    ["$BASE_URL/mcp/health"]="200"
    ["$BASE_URL/mcp/capabilities"]="200"
    ["$BASE_URL/mcp/list_tools"]="200"
    ["$BASE_URL/mcp/list_resources"]="200"
    ["$BASE_URL/health/metrics"]="200"
    ["$BASE_URL/health/readiness"]="200"
)

# Test expected responses
declare -A EXPECTED_RESPONSE=(
    ["$BASE_URL/mcp/capabilities"]="server_capabilities"
    ["$BASE_URL/mcp/list_tools"]="tools"
    ["$BASE_URL/mcp/list_resources"]="resources"
)

# Function to test endpoint
test_endpoint() {
    local endpoint=$1
    local expected_code=$2
    local expected_content=$3
    local retry=0

    echo "Testing $endpoint..."

    while [ $retry -lt $MAX_RETRIES ]; do
        response_code=$(curl -s -o /dev/null -w "%{http_code}" --max-time $TIMEOUT -f "$endpoint" 2>/dev/null || echo "000")

        if [ "$response_code" = "$expected_code" ]; then
            if [ -n "$expected_content" ]; then
                response_body=$(curl -s "$endpoint" 2>/dev/null)
                if echo "$response_body" | grep -q "$expected_content"; then
                    echo "  ✓ PASSED (code: $response_code, content: $expected_content)"
                    return 0
                else
                    echo "  ✗ FAILED (code: $response_code, content missing: $expected_content)"
                fi
            else
                echo "  ✓ PASSED (code: $response_code)"
                return 0
            fi
        else
            echo "  ⚠ RETRY ($retry/$MAX_RETRIES) - Expected: $expected_code, Got: $response_code"
        fi

        retry=$((retry + 1))
        sleep $DELAY
    done

    echo "  ✗ FAILED after $MAX_RETRIES attempts (code: $response_code)"
    return 1
}

# Performance test
performance_test() {
    local endpoint=$1
    local iterations=${2:-100}

    echo "Performance test: $endpoint ($iterations requests)"

    start_time=$(date +%s.%N)

    for i in $(seq 1 $iterations); do
        curl -s "$endpoint" >/dev/null 2>&1 &
    done

    wait

    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc)

    rps=$(echo "scale=2; $iterations / $duration" | bc)
    avg_duration=$(echo "scale=3; $duration / $iterations" | bc)

    echo "  Requests/sec: $rps"
    echo "  Avg duration: $avg_durations"

    # Check performance thresholds
    if [ "$(echo "$rps < 50" | bc)" -eq 1 ]; then
        echo "  ⚠ Performance below threshold (<50 RPS)"
        return 1
    fi

    return 0
}

# Load test
load_test() {
    local endpoint=$1
    local concurrent=${2:-50}
    local duration=${3:-30}

    echo "Load test: $endpoint ($concurrent concurrent users for $duration seconds)"

    # Create load test script
    cat > /tmp/load_test.sh <<EOF
#!/bin/bash
for i in $(seq 1 $concurrent); do
    curl -s "$endpoint" >/dev/null &
done
wait
EOF

    chmod +x /tmp/load_test.sh
    start_time=$(date +%s.%N)

    for i in $(seq 1 10); do  # Run 10 batches
        /tmp/load_test.sh
        sleep 1
    done

    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc)

    total_requests=$(echo "10 * $concurrent" | bc)
    rps=$(echo "scale=2; $total_requests / $duration" | bc)

    echo "  Total requests: $total_requests"
    echo "  Duration: $duration seconds"
    echo "  RPS: $rps"

    # Check error rate
    error_count=$(curl -s -w "%{http_code}" "$endpoint" | grep -E "^(4|5)[0-9]{2}" | wc -l)
    error_rate=$(echo "scale=2; $error_count * 100 / $total_requests" | bc)

    echo "  Error rate: $error_rate%"

    if [ "$(echo "$error_rate > 5" | bc)" -eq 1 ]; then
        echo "  ⚠ High error rate (>5%)"
        return 1
    fi

    return 0
}

# Run tests
PASSED=0
FAILED=0

echo "Starting smoke tests for $ENVIRONMENT environment..."
echo "Base URL: $BASE_URL"
if [ -n "$CANARY" ]; then
    echo "Canary percentage: $CANARY%"
fi

# Test all endpoints
echo "=== Testing Endpoints ==="
for endpoint in "${!ENDPOINTS[@]}"; do
    expected_code=${ENDPOINTS[$endpoint]}
    test_endpoint "$endpoint" "$expected_code" "" && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))
done

# Test specific content
echo "=== Testing Content ==="
for endpoint in "${!EXPECTED_RESPONSE[@]}"; do
    expected_code="200"
    expected_content=${EXPECTED_RESPONSE[$endpoint]}
    test_endpoint "$endpoint" "$expected_code" "$expected_content" && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))
done

# Performance tests
echo "=== Performance Tests ==="
performance_test "$BASE_URL/mcp/health" 100 && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))

# Load tests
if [ "$ENVIRONMENT" = "production" ]; then
    echo "=== Load Tests ==="
    load_test "$BASE_URL/mcp/capabilities" 50 30 && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))
fi

# Canary-specific tests
if [ -n "$CANARY" ]; then
    echo "=== Canary Tests ==="

    # Test canary header
    canary_response=$(curl -s -H "X-Canary: true" "$BASE_URL/mcp/health" -w "%{http_code}" -o /dev/null)
    if [ "$canary_response" = "200" ]; then
        echo "✓ Canary header test passed"
        PASSED=$((PASSED + 1))
    else
        echo "✗ Canary header test failed"
        FAILED=$((FAILED + 1))
    fi
fi

# Summary
echo ""
echo "=== Test Summary ==="
echo "Total tests: $((PASSED + FAILED))"
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [ $FAILED -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ $FAILED tests failed!"
    exit 1
fi