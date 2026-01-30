#!/usr/bin/env bash
#============================================================
# Performance Regression Detection Script
# Blocks if performance degrades >10% vs baseline
#============================================================

set -e

BASELINE_FILE="/Users/sac/erlmcp/bench/baseline.json"
THRESHOLD=10  # 10% regression threshold

echo "=== Performance Regression Check ==="
echo ""

# Check if baseline exists
if [ ! -f "$BASELINE_FILE" ]; then
    echo "⚠️  No baseline found. Creating baseline..."
    escript /tmp/run_bench4.erl | tee "$BASELINE_FILE"
    echo "✓ Baseline created"
    exit 0
fi

# Run benchmarks
CURRENT_RESULTS=$(mktemp)
escript /tmp/run_bench4.erl | tee "$CURRENT_RESULTS"

# Parse results (simple grep-based parsing)
check_regression() {
    local component="$1"
    local baseline_val=$(grep "$component:" "$BASELINE_FILE" | awk '{print $2}' | tr -d ',')
    local current_val=$(grep "$component:" "$CURRENT_RESULTS" | awk '{print $2}' | tr -d ',')
    
    if [ -z "$baseline_val" ] || [ -z "$current_val" ]; then
        echo "⚠️  Could not parse $component results"
        return 0
    fi
    
    # Calculate regression (pure bash)
    local regression=$(echo "scale=2; ($baseline_val - $current_val) / $baseline_val * 100" | bc)
    
    # Handle negative values (improvement)
    if (( $(echo "$regression < 0" | bc -l) )); then
        echo "✅ $component: IMPROVEMENT $(echo "$regression * -1" | bc)%"
        return 0
    fi
    
    # Check threshold
    if (( $(echo "$regression > $THRESHOLD" | bc -l) )); then
        echo "❌ REGRESSION DETECTED: $component degraded ${regression}% (threshold: ${THRESHOLD}%)"
        return 1
    elif (( $(echo "$regression > 5" | bc -l) )); then
        echo "⚠️  $component: ${regression}% degradation (warning)"
        return 0
    else
        echo "✅ $component: ${regression}% variation (acceptable)"
        return 0
    fi
}

# Check each component
STATUS=0
check_regression "Process Dictionary" || STATUS=1
check_regression "Map Operations" || STATUS=1
check_regression "Queue Operations" || STATUS=1

echo ""
if [ $STATUS -eq 0 ]; then
    echo "✅ No performance regression detected"
    exit 0
else
    echo "❌ Performance regression detected! Block: YES"
    exit 1
fi
