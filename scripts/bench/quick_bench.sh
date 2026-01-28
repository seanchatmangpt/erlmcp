#!/usr/bin/env bash
###===================================================================
### quick_bench.sh - Quick Benchmark Run (1-2 minutes)
###===================================================================
###
### Runs minimal benchmark suite for rapid iteration during development.
### Executes 1 workload from each category, skips heavy stress/chaos tests.
###
### Usage:
###   ./scripts/bench/quick_bench.sh
###
### Perfect for:
###   - Local development verification
###   - Pre-commit smoke testing
###   - Quick regression checks
###
###===================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Run in quick mode with relaxed metrology (warnings only)
export BENCHMARK_MODE="quick"
export METROLOGY_STRICT="false"
export REGRESSION_THRESHOLD="20"  # More lenient for dev

echo "Running quick benchmark suite..."
echo ""

"$SCRIPT_DIR/run_all_benchmarks.sh" quick
