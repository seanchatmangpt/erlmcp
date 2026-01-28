#!/bin/bash
# Run erlmcp integration benchmark

set -e

echo "=== ERLMCP Integration Benchmark Runner ==="
echo ""

# Ensure we're in the right directory
cd "$(dirname "$0")/.."

# Compile
echo "Compiling erlmcp..."
rebar3 compile

# Run benchmark
echo ""
echo "Running integration benchmarks..."
erl -noshell \
  -pa _build/default/lib/*/ebin \
  -eval 'erlmcp_bench_integration:benchmark_all()' \
  -s init stop

echo ""
echo "Benchmark complete!"
echo "Results saved to bench/results/integration_*.json"
