#!/bin/bash

# Registry Contention Benchmark Runner for erlmcp v1.3.0
# Executes both contention and correctness suites
# Generates CSV output and detailed reports

set -e

BENCHMARK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$BENCHMARK_DIR")"
RESULTS_DIR="$PROJECT_ROOT/bench/results"

# Create results directory
mkdir -p "$RESULTS_DIR"

echo "========================================="
echo "Registry v1.3.0 Benchmark Suite"
echo "========================================="
echo ""

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Ensure erlmcp is compiled
echo -e "${BLUE}[1/3]${NC} Compiling erlmcp..."
cd "$PROJECT_ROOT"
make compile > /dev/null 2>&1 || echo "Warning: Build had issues, proceeding..."

echo ""
echo -e "${BLUE}[2/3]${NC} Running contention benchmarks (all scales)..."
echo "Scales: 10K, 25K, 50K, 100K"
echo "Iterations: 5 per scale"
echo "Duration: ~5-10 minutes"
echo ""

# Run contention benchmark and capture output
BENCHMARK_OUTPUT=$(mktemp)
cd "$PROJECT_ROOT"

erl -noshell \
    -pa _build/default/lib/*/ebin \
    -sa 4096 \
    -run erlmcp_registry_contention benchmark_all \
    -s init stop 2>&1 | tee "$BENCHMARK_OUTPUT"

# Extract results
grep -A 100 "=== Contention Benchmark Results ===" "$BENCHMARK_OUTPUT" > "$RESULTS_DIR/contention_results.txt" || true

echo ""
echo -e "${BLUE}[3/3]${NC} Running correctness tests..."
echo "Tests: 10 comprehensive suites"
echo "Coverage: Message delivery, routing, concurrency, failure scenarios, memory safety"
echo ""

# Run correctness tests
CORRECTNESS_OUTPUT=$(mktemp)
cd "$PROJECT_ROOT"

rebar3 ct --suite=erlmcp_registry_correctness_SUITE 2>&1 | tee "$CORRECTNESS_OUTPUT" || true

# Extract test results
grep -E "(PASSED|FAILED|ERROR)" "$CORRECTNESS_OUTPUT" > "$RESULTS_DIR/correctness_results.txt" || true

echo ""
echo "========================================="
echo -e "${GREEN}Benchmark Complete${NC}"
echo "========================================="
echo ""
echo "Results saved to: $RESULTS_DIR/"
echo ""
echo "Output files:"
echo "  - contention_results.txt (latency table + recommendations)"
echo "  - correctness_results.txt (test pass/fail results)"
echo ""

# Summary
echo "Summary Statistics:"
echo ""

echo "Contention Results:"
grep "Scale.*|.*Shards" "$RESULTS_DIR/contention_results.txt" | tail -6 || echo "  (Run benchmark to populate)"
echo ""

echo "Correctness Tests:"
if [ -f "$RESULTS_DIR/correctness_results.txt" ]; then
    PASSED=$(grep -c "PASSED" "$RESULTS_DIR/correctness_results.txt" || echo "0")
    FAILED=$(grep -c "FAILED" "$RESULTS_DIR/correctness_results.txt" || echo "0")
    echo "  Passed: $PASSED"
    echo "  Failed: $FAILED"
else
    echo "  (Run benchmark to populate)"
fi
echo ""

# Cleanup temp files
rm -f "$BENCHMARK_OUTPUT" "$CORRECTNESS_OUTPUT"

echo "To analyze results in detail:"
echo "  cat $RESULTS_DIR/contention_results.txt"
echo "  cat $RESULTS_DIR/correctness_results.txt"
echo ""
