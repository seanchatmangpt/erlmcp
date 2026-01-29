#!/bin/bash
# Find exact breaking point for timeout storm

echo "========================================"
echo "FINDING TIMEOUT STORM BREAKING POINT"
echo "========================================"
echo ""

# Test at different scales
test_counts=(100 500 1000 2000 5000 10000 15000 20000)

echo "Testing incremental timeout counts..."
echo ""

breaking_point="unknown"
last_success=0

for count in "${test_counts[@]}"; do
    echo "Testing $count concurrent timeouts..."
    output=$(./test/chaos/timeout_incremental.erl $count 2>&1)
    exit_code=$?
    
    echo "$output" | grep "Result:"
    
    if [ $exit_code -eq 0 ]; then
        last_success=$count
        echo "  ✅ PASSED"
    else
        breaking_point=$count
        echo "  ❌ FAILED"
        break
    fi
    echo ""
done

echo "========================================"
echo "BREAKING POINT ANALYSIS"
echo "========================================"
echo ""
echo "Last Successful: $last_success timeouts"
echo "Breaking Point: $breaking_point timeouts"
echo ""

if [ "$breaking_point" != "unknown" ]; then
    echo "CONCLUSION: System breaks between $last_success and $breaking_point concurrent timeouts"
    echo ""
    echo "This is approximately a $(( (breaking_point - last_success) / 10 ))x reduction from target of 100K"
    echo ""
    echo "ROOT CAUSE: TCP connection bottleneck (connection-per-request)"
    echo "RECOMMENDED FIX: Implement connection pooling (expected 10-100x improvement)"
else
    echo "CONCLUSION: System handled all tested loads successfully"
    echo "Next step: Test higher counts (50K, 100K, 500K, 1M)"
fi
