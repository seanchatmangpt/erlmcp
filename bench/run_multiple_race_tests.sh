#!/bin/bash
echo "=== Multiple Race Condition Test Iterations ==="
echo ""

for i in 1 2 3 4 5; do
    echo "Iteration $i:"
    erl -noshell -pa . -s race_test_report run -s init stop 2>&1 | grep -E "(Expected Final Value|Actual Final Value|Lost Updates|CORRUPTION DETECTED|Analysis)"
    echo ""
done
