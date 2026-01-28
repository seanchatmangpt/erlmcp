#!/usr/bin/env bash
###====================================================================
### baseline-capture.sh - Capture Performance Baseline
###====================================================================
### Runs all erlmcp benchmarks and saves consolidated baseline.
###
### Usage:
###   ./tools/baseline-capture.sh [--no-commit] [--version VERSION]
###
### Options:
###   --no-commit    Don't commit baseline to git
###   --version VER  Override version (default: read from erlmcp.app.src)
###
### Output:
###   bench/baselines/YYYY-MM-DD_vX.Y.Z.json
###
### Example:
###   ./tools/baseline-capture.sh
###   # Creates: bench/baselines/2026-01-28_v0.7.0.json
###====================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Defaults
COMMIT=true
VERSION=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-commit)
            COMMIT=false
            shift
            ;;
        --version)
            VERSION="$2"
            shift 2
            ;;
        -h|--help)
            head -n 20 "$0" | grep "^###" | sed 's/^### //'
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Extract version from erlmcp.app.src if not provided
if [ -z "$VERSION" ]; then
    VERSION=$(grep -o '{vsn, "[^"]*"}' src/erlmcp.app.src | sed 's/{vsn, "\(.*\)"}/\1/')
fi

if [ -z "$VERSION" ]; then
    echo -e "${RED}ERROR: Could not determine version${NC}"
    exit 1
fi

# Generate filename
TIMESTAMP=$(date +%Y-%m-%d)
BASELINE_FILE="bench/baselines/${TIMESTAMP}_v${VERSION}.json"

echo "========================================"
echo "erlmcp Baseline Capture"
echo "========================================"
echo "Version: $VERSION"
echo "Output: $BASELINE_FILE"
echo ""

# Ensure directories exist
mkdir -p bench/baselines
mkdir -p bench/results

# Compile project
echo -e "${YELLOW}[1/4] Compiling project...${NC}"
if ! TERM=dumb rebar3 compile 2>&1 | grep -v "^===>"; then
    echo -e "${RED}✗ Compilation failed${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Compiled${NC}"
echo ""

# Run benchmarks
echo -e "${YELLOW}[2/4] Running benchmarks...${NC}"
echo "This may take 5-10 minutes..."
echo ""

# Core ops benchmark
echo "  Running core_ops (100K operations)..."
if ! erl -pa _build/default/lib/*/ebin -noshell \
    -eval "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), halt()." \
    2>&1 | grep -v "^Erlang/OTP" | tail -5; then
    echo -e "${RED}✗ core_ops benchmark failed${NC}"
    exit 1
fi

# Network benchmark (quick version)
echo "  Running network_real (TCP 1K connections)..."
if ! erl -pa _build/default/lib/*/ebin -noshell \
    -eval "erlmcp_bench_network_real:run(<<\"tcp_quick_1k\">>), halt()." \
    2>&1 | grep -v "^Erlang/OTP" | tail -5; then
    echo -e "${RED}✗ network_real benchmark failed${NC}"
    exit 1
fi

# Stress benchmark (30 seconds)
echo "  Running stress (30 seconds sustained)..."
if ! erl -pa _build/default/lib/*/ebin -noshell \
    -eval "erlmcp_bench_stress:run(<<\"stress_30s_100k_ops\">>), halt()." \
    2>&1 | grep -v "^Erlang/OTP" | tail -5; then
    echo -e "${RED}✗ stress benchmark failed${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Benchmarks complete${NC}"
echo ""

# Consolidate results
echo -e "${YELLOW}[3/4] Consolidating results...${NC}"

# Create consolidated baseline JSON
cat > "$BASELINE_FILE" << JSON_START
{
  "version": "$VERSION",
  "timestamp": $(date +%s),
  "date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "environment": {
    "os": "$(uname -s | tr '[:upper:]' '[:lower:]')",
    "os_version": "$(uname -r)",
    "arch": "$(uname -m)",
    "hostname": "$(hostname)",
    "otp_version": "$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "unknown")",
    "erts_version": "$(erl -noshell -eval 'io:format("~s", [erlang:system_info(version)]), halt().' 2>/dev/null || echo "unknown")",
    "cores": $(sysctl -n hw.logicalcpu 2>/dev/null || nproc 2>/dev/null || echo "0")
  },
  "benchmarks": {
JSON_START

# Find most recent result files
CORE_OPS_FILE=$(ls -t bench/results/core_ops_core_ops_100k_*.json 2>/dev/null | head -1)
NETWORK_FILE=$(ls -t bench/results/network_real_tcp_quick_1k_*.json 2>/dev/null | head -1)
STRESS_FILE=$(ls -t bench/results/stress_stress_30s_100k_ops_*.json 2>/dev/null | head -1)

# Extract metrics using simple JSON parsing
extract_metrics() {
    local file=$1
    local workload=$2
    
    if [ ! -f "$file" ]; then
        echo "    \"$workload\": null"
        return
    fi
    
    # Extract key metrics (simple grep-based parsing)
    local throughput=$(grep -o '"throughput_msg_per_s":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local latency_p50=$(grep -o '"latency_p50_us":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local latency_p95=$(grep -o '"latency_p95_us":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local latency_p99=$(grep -o '"latency_p99_us":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local memory_delta=$(grep -o '"memory_delta_mib":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    
    cat << METRICS
    "$workload": {
      "throughput_msg_per_s": ${throughput:-0},
      "latency_p50_us": ${latency_p50:-0},
      "latency_p95_us": ${latency_p95:-0},
      "latency_p99_us": ${latency_p99:-0},
      "memory_delta_mib": ${memory_delta:-0}
    }
METRICS
}

# Add core_ops
if [ -n "$CORE_OPS_FILE" ]; then
    extract_metrics "$CORE_OPS_FILE" "core_ops_100k" >> "$BASELINE_FILE"
    echo "," >> "$BASELINE_FILE"
fi

# Add network_real
if [ -n "$NETWORK_FILE" ]; then
    extract_metrics "$NETWORK_FILE" "tcp_quick_1k" >> "$BASELINE_FILE"
    echo "," >> "$BASELINE_FILE"
fi

# Add stress (no trailing comma)
if [ -n "$STRESS_FILE" ]; then
    extract_metrics "$STRESS_FILE" "stress_30s_100k_ops" >> "$BASELINE_FILE"
fi

# Close JSON
cat >> "$BASELINE_FILE" << JSON_END
  }
}
JSON_END

echo -e "${GREEN}✓ Baseline saved: $BASELINE_FILE${NC}"
echo ""

# Validate JSON
if command -v jq &> /dev/null; then
    if ! jq empty "$BASELINE_FILE" 2>/dev/null; then
        echo -e "${RED}✗ Invalid JSON in baseline file${NC}"
        exit 1
    fi
    echo -e "${GREEN}✓ Baseline JSON valid${NC}"
fi

# Display summary
echo ""
echo "========================================"
echo "Baseline Summary"
echo "========================================"
if command -v jq &> /dev/null; then
    jq -r '.benchmarks | to_entries[] | "\(.key):\n  Throughput: \(.value.throughput_msg_per_s) msg/s\n  Latency p95: \(.value.latency_p95_us) µs\n  Memory: \(.value.memory_delta_mib) MiB"' "$BASELINE_FILE"
else
    cat "$BASELINE_FILE"
fi
echo ""

# Commit to git
if [ "$COMMIT" = true ]; then
    echo -e "${YELLOW}[4/4] Committing baseline to git...${NC}"
    
    git add "$BASELINE_FILE"
    git commit -m "Performance baseline for v${VERSION}

Captured on: $(date -u +%Y-%m-%d)
Environment: $(uname -s) $(uname -r)

Baselines:
- core_ops_100k
- tcp_quick_1k
- stress_30s_100k_ops

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
    
    COMMIT_SHA=$(git rev-parse --short HEAD)
    echo -e "${GREEN}✓ Git commit: $COMMIT_SHA${NC}"
else
    echo -e "${YELLOW}⊘ Skipped git commit (--no-commit)${NC}"
fi

echo ""
echo -e "${GREEN}✓ Baseline capture complete${NC}"
echo ""
echo "Next steps:"
echo "  - Review: cat $BASELINE_FILE"
echo "  - Compare: ./tools/baseline-compare.sh"
echo "  - Update: ./tools/baseline-update.sh"
