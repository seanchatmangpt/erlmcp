#!/usr/bin/env bash
# CLI Performance Benchmark Runner
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=========================================="
echo "ERLMCP CLI PERFORMANCE BENCHMARK"
echo "=========================================="
echo ""

# Ensure compiled
echo "Ensuring project is compiled..."
cd "$ROOT_DIR"
TERM=dumb rebar3 compile > /dev/null 2>&1

# Create results directory
mkdir -p bench/results

# Run benchmarks
echo ""
echo "1. Startup Performance Benchmark"
echo "-----------------------------------"
rebar3 shell --config config/sys.config --eval "
    erlmcp_cli_startup_bench:run(#{iterations => 100, profile => false}),
    init:stop().
" --sname cli_bench_startup

echo ""
echo "2. Command Execution Benchmark"
echo "-----------------------------------"
rebar3 shell --config config/sys.config --eval "
    erlmcp_cli_command_bench:run(#{iterations => 10}),
    init:stop().
" --sname cli_bench_commands

echo ""
echo "=========================================="
echo "Benchmark complete!"
echo "Results in: bench/results/"
echo "=========================================="
