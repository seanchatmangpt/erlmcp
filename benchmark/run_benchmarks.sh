#!/bin/bash
set -e

echo "========================================="
echo "erlmcp v3 Performance Benchmarking Suite"
echo "========================================="
echo ""

# Ensure results directory exists
mkdir -p benchmark/results

# Start Erlang node and run benchmarks
erl -pa _build/default/lib/*/ebin \
    -eval "application:ensure_all_started(erlmcp_core)" \
    -s erlmcp_performance_benchmark start_link \
    -s erlmcp_performance_benchmark run_all_benchmarks \
    -s init stop \
    -noshell

echo ""
echo "Benchmark results saved to: benchmark/results/"
