#!/usr/bin/env bash
###===================================================================
### run_transport_real.sh - Run Real Transport Benchmarks
###===================================================================
###
### Usage:
###   ./scripts/bench/run_transport_real.sh [tcp|http|all]
###
### Runs real transport benchmarks with gun/ranch integration
### Outputs JSON results to bench/results/

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
RESULTS_DIR="bench/results"
TRANSPORT="${1:-all}"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}ERLMCP Real Transport Benchmarks${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# Compile benchmark modules
echo -e "${YELLOW}Compiling benchmark modules...${NC}"
rebar3 compile || {
    echo -e "${RED}Compilation failed${NC}"
    exit 1
}

# Function to run TCP benchmark
run_tcp_benchmark() {
    echo ""
    echo -e "${GREEN}=== Running TCP Transport Benchmark ===${NC}"
    echo ""

    if [ -f "bench/transport_real/tcp_real_bench.erl" ]; then
        erl -pa _build/default/lib/*/ebin \
            -noshell \
            -s tcp_real_bench run \
            -s init stop

        echo -e "${GREEN}TCP benchmark completed${NC}"
    else
        echo -e "${YELLOW}TCP benchmark not found, skipping${NC}"
    fi
}

# Function to run HTTP benchmark
run_http_benchmark() {
    echo ""
    echo -e "${GREEN}=== Running HTTP Transport Benchmark ===${NC}"
    echo ""

    if [ -f "bench/transport_real/http_real_bench.erl" ]; then
        # Compile handler module first
        erlc -o _build/default/lib/erlmcp/ebin \
            -I include \
            -pa _build/default/lib/*/ebin \
            bench/transport_real/http_bench_handler.erl || {
            echo -e "${RED}Handler compilation failed${NC}"
            exit 1
        }

        # Compile benchmark module
        erlc -o _build/default/lib/erlmcp/ebin \
            -I include \
            -pa _build/default/lib/*/ebin \
            bench/transport_real/http_real_bench.erl || {
            echo -e "${RED}Benchmark compilation failed${NC}"
            exit 1
        }

        # Run benchmark
        erl -pa _build/default/lib/*/ebin \
            -noshell \
            -s http_real_bench run \
            -s init stop

        echo -e "${GREEN}HTTP benchmark completed${NC}"
    else
        echo -e "${YELLOW}HTTP benchmark not found, skipping${NC}"
    fi
}

# Run benchmarks based on argument
case "$TRANSPORT" in
    tcp)
        run_tcp_benchmark
        ;;
    http)
        run_http_benchmark
        ;;
    all)
        run_tcp_benchmark
        run_http_benchmark
        ;;
    *)
        echo -e "${RED}Unknown transport: $TRANSPORT${NC}"
        echo "Usage: $0 [tcp|http|all]"
        exit 1
        ;;
esac

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Benchmark Results${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# List generated result files
if [ -d "$RESULTS_DIR" ]; then
    echo "Results saved to:"
    ls -lh "$RESULTS_DIR"/transport_real_*.json 2>/dev/null || echo "No results found"
else
    echo -e "${YELLOW}No results directory found${NC}"
fi

echo ""
echo -e "${GREEN}Benchmarks complete!${NC}"
