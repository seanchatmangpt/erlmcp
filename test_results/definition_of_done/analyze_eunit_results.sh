#!/bin/bash
LOG_FILE="$1"

echo "=== EUnit Test Summary ==="
echo ""

# Count total tests
TOTAL=$(grep -c "Test passed" "$LOG_FILE" 2>/dev/null || echo "0")
echo "Total tests passed: $TOTAL"

# Count failed tests
FAILED=$(grep -c "*failed*" "$LOG_FILE" 2>/dev/null || echo "0")
echo "Total tests failed: $FAILED"

# Extract unique failure modules
echo ""
echo "=== Failure Categories ==="
grep "*failed*" "$LOG_FILE" | sed 's/.*module //' | sed 's/:.*//g' | sort -u | while read module; do
    count=$(grep "$module:" "$LOG_FILE" | grep -c "*failed*")
    echo "$module: $count failures"
done

echo ""
echo "=== Specific Failures ==="
grep "*failed*" "$LOG_FILE" | head -20
