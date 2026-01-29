#!/bin/bash
set -e

echo "========================================================================"
echo "RACE CONDITION BOMBARDMENT CRASH TEST"
echo "========================================================================"
echo ""
echo "DESTRUCTIVE TEST #12: Extreme concurrent modification testing"
echo ""

cd "$(dirname "$0")"

echo "Step 1: Compiling race condition test module..."
erlc race_condition_bombardment.erl

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed"
    exit 1
fi

echo "✓ Compilation successful"
echo ""

echo "Step 2: Running race condition bombardment..."
echo ""

erl -noshell -s race_condition_bombardment main -s init stop

echo ""
echo "========================================================================"
echo "Race condition bombardment test complete!"
echo "========================================================================"
echo ""
