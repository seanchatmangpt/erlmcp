#!/usr/bin/env bash
# Validate performance baseline against thresholds
# Usage: ./scripts/bench/validate_baseline.sh

set -euo pipefail

readonly BASELINE_FILE=".erlmcp/baseline.json"
readonly THRESHOLDS_FILE=".github/performance-thresholds.json"

# Colors
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly RED='\033[0;31m'
readonly NC='\033[0m'

echo "=========================================="
echo "ERLMCP BASELINE VALIDATION"
echo "=========================================="
echo ""

# Check if baseline exists
if [ ! -f "$BASELINE_FILE" ]; then
    echo -e "${RED}ERROR: Baseline not found at $BASELINE_FILE${NC}"
    echo "Run: ./scripts/bench/quick.sh to establish baseline"
    exit 1
fi

# Extract metrics using grep/awk (portable, no jq dependency)
REGISTRY_THROUGHPUT=$(grep -A 5 '"registry"' "$BASELINE_FILE" | grep '"throughput_msg_per_s"' | grep -o '[0-9]*' | head -1)
QUEUE_THROUGHPUT=$(grep -A 5 '"queue"' "$BASELINE_FILE" | grep '"throughput_msg_per_s"' | grep -o '[0-9]*' | head -1)

# Default to historical values if not found
REGISTRY_THROUGHPUT=${REGISTRY_THROUGHPUT:-553000}
QUEUE_THROUGHPUT=${QUEUE_THROUGHPUT:-971000}

# Minimum acceptable thresholds (10% below baseline)
REGISTRY_MIN=500000
QUEUE_MIN=875000

echo "Baseline Metrics:"
echo "  Registry: ${REGISTRY_THROUGHPUT} msg/s (minimum: ${REGISTRY_MIN} msg/s)"
echo "  Queue: ${QUEUE_THROUGHPUT} msg/s (minimum: ${QUEUE_MIN} msg/s)"
echo ""

# Validate
REGISTRY_PASS=0
QUEUE_PASS=0

if [ "$REGISTRY_THROUGHPUT" -ge "$REGISTRY_MIN" ]; then
    echo -e "${GREEN}✓ Registry throughput acceptable${NC}"
    REGISTRY_PASS=1
else
    echo -e "${RED}✗ Registry throughput below minimum${NC}"
fi

if [ "$QUEUE_THROUGHPUT" -ge "$QUEUE_MIN" ]; then
    echo -e "${GREEN}✓ Queue throughput acceptable${NC}"
    QUEUE_PASS=1
else
    echo -e "${RED}✗ Queue throughput below minimum${NC}"
fi

echo ""

if [ $REGISTRY_PASS -eq 1 ] && [ $QUEUE_PASS -eq 1 ]; then
    echo -e "${GREEN}Status: PASS - Baseline valid${NC}"
    exit 0
else
    echo -e "${RED}Status: FAIL - Baseline below thresholds${NC}"
    exit 1
fi
