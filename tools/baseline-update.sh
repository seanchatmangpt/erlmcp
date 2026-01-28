#!/usr/bin/env bash
###====================================================================
### baseline-update.sh - Update Performance Baseline
###====================================================================
### Interactive tool to update baseline after intentional changes.
### Shows diff, requires justification, creates git commit.
###
### Usage:
###   ./tools/baseline-update.sh [--auto-yes] [--reason "TEXT"]
###
### Options:
###   --auto-yes     Skip confirmation prompts
###   --reason TEXT  Justification for baseline update
###
### Example:
###   ./tools/baseline-update.sh
###   # Interactive prompts guide you through update
###
###   ./tools/baseline-update.sh --reason "gproc migration improved registry perf"
###   # Non-interactive update with reason
###====================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Defaults
AUTO_YES=false
REASON=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --auto-yes)
            AUTO_YES=true
            shift
            ;;
        --reason)
            REASON="$2"
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

# Find latest baseline
OLD_BASELINE=$(ls -t bench/baselines/*.json 2>/dev/null | head -1)

if [ -z "$OLD_BASELINE" ] || [ ! -f "$OLD_BASELINE" ]; then
    echo -e "${RED}ERROR: No existing baseline found${NC}"
    echo "Run: ./tools/baseline-capture.sh"
    exit 1
fi

OLD_BASELINE_NAME=$(basename "$OLD_BASELINE")

# Find latest results
CORE_OPS_FILE=$(ls -t bench/results/core_ops_core_ops_100k_*.json 2>/dev/null | head -1)
TCP_FILE=$(ls -t bench/results/network_real_tcp_quick_1k_*.json 2>/dev/null | head -1)
STRESS_FILE=$(ls -t bench/results/stress_stress_30s_100k_ops_*.json 2>/dev/null | head -1)

if [ -z "$CORE_OPS_FILE" ] || [ -z "$TCP_FILE" ] || [ -z "$STRESS_FILE" ]; then
    echo -e "${RED}ERROR: No current results found${NC}"
    echo "Run benchmarks first:"
    echo "  ./tools/baseline-compare.sh"
    exit 1
fi

echo "========================================"
echo "erlmcp Baseline Update"
echo "========================================"
echo "Old baseline: $OLD_BASELINE_NAME"
echo ""

# Extract version
VERSION=$(grep -o '{vsn, "[^"]*"}' src/erlmcp.app.src | sed 's/{vsn, "\(.*\)"}/\1/')
TIMESTAMP=$(date +%Y-%m-%d)
NEW_BASELINE="bench/baselines/${TIMESTAMP}_v${VERSION}.json"
NEW_BASELINE_NAME=$(basename "$NEW_BASELINE")

echo "New baseline: $NEW_BASELINE_NAME"
echo ""

# Show comparison
echo -e "${YELLOW}[1/4] Comparing old vs new...${NC}"
echo ""

extract_metric() {
    local file=$1
    local path=$2
    
    if [ ! -f "$file" ]; then
        echo "0"
        return
    fi
    
    if command -v jq &> /dev/null; then
        jq -r "$path // 0" "$file"
    else
        grep -o "\"$(echo $path | sed 's/.*\.//')\":[[:space:]]*[0-9.]*" "$file" | head -1 | awk -F: '{print $2}' | xargs || echo "0"
    fi
}

# Extract old baseline metrics
OLD_CORE_OPS_THROUGHPUT=$(extract_metric "$OLD_BASELINE" '.benchmarks.core_ops_100k.throughput_msg_per_s')
OLD_TCP_THROUGHPUT=$(extract_metric "$OLD_BASELINE" '.benchmarks.tcp_quick_1k.throughput_msg_per_s')
OLD_STRESS_THROUGHPUT=$(extract_metric "$OLD_BASELINE" '.benchmarks.stress_30s_100k_ops.throughput_msg_per_s')

# Extract new metrics from results
NEW_CORE_OPS_THROUGHPUT=$(extract_metric "$CORE_OPS_FILE" '.throughput_msg_per_s')
NEW_TCP_THROUGHPUT=$(extract_metric "$TCP_FILE" '.throughput_msg_per_s')
NEW_STRESS_THROUGHPUT=$(extract_metric "$STRESS_FILE" '.throughput_msg_per_s')

# Calculate changes
calc_change() {
    local old=$1
    local new=$2
    
    if [ "$old" = "0" ] || [ -z "$old" ]; then
        echo "N/A"
        return
    fi
    
    echo "scale=1; (($new - $old) / $old) * 100" | bc
}

CORE_OPS_CHANGE=$(calc_change "$OLD_CORE_OPS_THROUGHPUT" "$NEW_CORE_OPS_THROUGHPUT")
TCP_CHANGE=$(calc_change "$OLD_TCP_THROUGHPUT" "$NEW_TCP_THROUGHPUT")
STRESS_CHANGE=$(calc_change "$OLD_STRESS_THROUGHPUT" "$NEW_STRESS_THROUGHPUT")

# Display comparison table
printf "%-25s %-15s %-15s %-10s\n" "Benchmark" "Old" "New" "Change"
printf "%-25s %-15s %-15s %-10s\n" "-------------------------" "---------------" "---------------" "----------"
printf "%-25s %-15.0f %-15.0f %+.1f%%\n" "core_ops_100k (msg/s)" "$OLD_CORE_OPS_THROUGHPUT" "$NEW_CORE_OPS_THROUGHPUT" "$CORE_OPS_CHANGE"
printf "%-25s %-15.0f %-15.0f %+.1f%%\n" "tcp_quick_1k (msg/s)" "$OLD_TCP_THROUGHPUT" "$NEW_TCP_THROUGHPUT" "$TCP_CHANGE"
printf "%-25s %-15.0f %-15.0f %+.1f%%\n" "stress_30s (msg/s)" "$OLD_STRESS_THROUGHPUT" "$NEW_STRESS_THROUGHPUT" "$STRESS_CHANGE"
echo ""

# Check if significant changes
SIGNIFICANT=false
for change in "$CORE_OPS_CHANGE" "$TCP_CHANGE" "$STRESS_CHANGE"; do
    if [ "$change" != "N/A" ]; then
        abs_change=$(echo "$change" | tr -d '-')
        if echo "$abs_change > 5" | bc -l | grep -q 1; then
            SIGNIFICANT=true
            break
        fi
    fi
done

if [ "$SIGNIFICANT" = false ]; then
    echo -e "${YELLOW}⚠ Warning: Changes are not significant (<5%)${NC}"
    if [ "$AUTO_YES" = false ]; then
        read -p "Continue anyway? (y/n): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Aborted."
            exit 0
        fi
    fi
fi

# Get justification
echo -e "${YELLOW}[2/4] Justification required${NC}"
echo ""

if [ -z "$REASON" ]; then
    if [ "$AUTO_YES" = true ]; then
        echo -e "${RED}ERROR: --reason required with --auto-yes${NC}"
        exit 1
    fi
    
    echo "Why is this baseline update necessary?"
    echo "Examples:"
    echo "  - gproc migration improved registry performance"
    echo "  - OTP 28 upgrade - expected changes"
    echo "  - Optimization: reduced memory allocations"
    echo ""
    read -p "Reason: " REASON
fi

if [ -z "$REASON" ]; then
    echo -e "${RED}ERROR: Justification required${NC}"
    exit 1
fi

echo "Reason: $REASON"
echo ""

# Confirm
if [ "$AUTO_YES" = false ]; then
    echo -e "${YELLOW}[3/4] Confirmation${NC}"
    echo ""
    echo "This will:"
    echo "  1. Create new baseline: $NEW_BASELINE_NAME"
    echo "  2. Commit to git with justification"
    echo "  3. Old baseline remains in git history"
    echo ""
    read -p "Proceed? (y/n): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
fi

# Capture new baseline
echo -e "${YELLOW}[3/4] Capturing new baseline...${NC}"

# Create consolidated baseline
cat > "$NEW_BASELINE" << JSON_START
{
  "version": "$VERSION",
  "timestamp": $(date +%s),
  "date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "previous_baseline": "$OLD_BASELINE_NAME",
  "update_reason": "$REASON",
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

# Extract and add metrics
extract_full_metrics() {
    local file=$1
    local name=$2
    
    if [ ! -f "$file" ]; then
        echo "    \"$name\": null"
        return
    fi
    
    local throughput=$(grep -o '"throughput_msg_per_s":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local latency_p50=$(grep -o '"latency_p50_us":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local latency_p95=$(grep -o '"latency_p95_us":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local latency_p99=$(grep -o '"latency_p99_us":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    local memory_delta=$(grep -o '"memory_delta_mib":[[:space:]]*[0-9.]*' "$file" | head -1 | awk -F: '{print $2}' | xargs)
    
    cat << METRICS
    "$name": {
      "throughput_msg_per_s": ${throughput:-0},
      "latency_p50_us": ${latency_p50:-0},
      "latency_p95_us": ${latency_p95:-0},
      "latency_p99_us": ${latency_p99:-0},
      "memory_delta_mib": ${memory_delta:-0}
    }
METRICS
}

extract_full_metrics "$CORE_OPS_FILE" "core_ops_100k" >> "$NEW_BASELINE"
echo "," >> "$NEW_BASELINE"
extract_full_metrics "$TCP_FILE" "tcp_quick_1k" >> "$NEW_BASELINE"
echo "," >> "$NEW_BASELINE"
extract_full_metrics "$STRESS_FILE" "stress_30s_100k_ops" >> "$NEW_BASELINE"

cat >> "$NEW_BASELINE" << JSON_END
  }
}
JSON_END

echo -e "${GREEN}✓ New baseline created: $NEW_BASELINE${NC}"
echo ""

# Validate JSON
if command -v jq &> /dev/null; then
    if ! jq empty "$NEW_BASELINE" 2>/dev/null; then
        echo -e "${RED}✗ Invalid JSON${NC}"
        exit 1
    fi
fi

# Commit to git
echo -e "${YELLOW}[4/4] Committing to git...${NC}"

git add "$NEW_BASELINE"
git commit -m "Update baseline: $REASON

Previous: $OLD_BASELINE_NAME
Current:  $NEW_BASELINE_NAME

Changes:
- core_ops_100k: ${CORE_OPS_CHANGE}% throughput
- tcp_quick_1k: ${TCP_CHANGE}% throughput
- stress_30s_100k_ops: ${STRESS_CHANGE}% throughput

Justification: $REASON

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

COMMIT_SHA=$(git rev-parse --short HEAD)
echo -e "${GREEN}✓ Git commit: $COMMIT_SHA${NC}"
echo ""

echo -e "${GREEN}✓ Baseline update complete${NC}"
echo ""
echo "New baseline: $NEW_BASELINE"
echo "Git commit: $COMMIT_SHA"
echo ""
echo "Baseline history:"
git log --oneline --decorate -- bench/baselines/ | head -5
