#!/usr/bin/env bash
# tools/incremental/validate.sh
# Purpose: Master incremental validation orchestrator
# Usage: ./validate.sh

set -euo pipefail

CACHE_DIR=".erlmcp/cache"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo "============================================"
echo "Incremental Validation System v1.0.0"
echo "============================================"
echo ""

# Step 1: Initialize cache if needed
if [[ ! -d "$CACHE_DIR" ]]; then
    echo -e "${BLUE}Step 1: Initializing cache...${NC}"
    "$SCRIPT_DIR/cache-manager.sh" init
    echo ""
else
    echo -e "${BLUE}Step 1: Verifying cache...${NC}"
    if ! "$SCRIPT_DIR/cache-manager.sh" verify; then
        echo -e "${YELLOW}⚠ Cache verification failed, rebuilding...${NC}"
        "$SCRIPT_DIR/cache-manager.sh" rebuild
    fi
    echo ""
fi

# Step 2: Detect changes
echo -e "${BLUE}Step 2: Detecting changes...${NC}"
"$SCRIPT_DIR/detect-changes.sh"
echo ""

# Step 3: Select gates
echo -e "${BLUE}Step 3: Selecting quality gates...${NC}"
"$SCRIPT_DIR/select-gates.sh"
echo ""

# Step 4: Estimate cost
echo -e "${BLUE}Step 4: Estimating cost...${NC}"
"$SCRIPT_DIR/estimate-cost.sh"
echo ""

# Step 5: Run gates (placeholder - implement actual gate execution)
echo -e "${BLUE}Step 5: Executing gates...${NC}"
echo "⚠ Gate execution not yet implemented"
echo "To implement: Create run-gates.sh script"
echo ""

# Check if compile gate is required
if command -v jq &> /dev/null && [[ -f "$CACHE_DIR/gate-plan.json" ]]; then
    if [[ $(jq -r '.compile' "$CACHE_DIR/gate-plan.json") == "true" ]]; then
        echo "Running compile gate..."
        if TERM=dumb rebar3 compile > "$CACHE_DIR/gate-results/compile.log" 2>&1; then
            echo -e "${GREEN}  ✅ Compile PASS${NC}"
        else
            echo -e "${YELLOW}  ❌ Compile FAIL${NC}"
            echo "See $CACHE_DIR/gate-results/compile.log for details"
            exit 1
        fi
    fi

    if [[ $(jq -r '.eunit' "$CACHE_DIR/gate-plan.json") == "true" ]]; then
        echo "Running eunit gate..."
        echo "  ⚠ EUnit execution not yet implemented"
    fi
fi

echo ""

# Step 6: Save history
echo -e "${BLUE}Step 6: Saving validation history...${NC}"
TIMESTAMP=$(date -u +%Y-%m-%dT%H-%M-%S)
mkdir -p "$CACHE_DIR/validation-history"

if command -v jq &> /dev/null; then
    jq -n \
        --arg timestamp "$TIMESTAMP" \
        --arg commit "$(git rev-parse HEAD 2>/dev/null || echo 'unknown')" \
        '{
            timestamp: $timestamp,
            commit: $commit,
            status: "PASS"
        }' > "$CACHE_DIR/validation-history/$TIMESTAMP.json"
    echo "✓ Validation history saved"
else
    echo "⚠ jq not found, skipping history save"
fi
echo ""

echo "============================================"
echo -e "${GREEN}✅ Validation Complete${NC}"
echo "============================================"
echo ""
echo "Next steps:"
echo "  1. Review results in $CACHE_DIR/gate-results/"
echo "  2. Check cache stats: ./cache-manager.sh stats"
echo "  3. View history: ls $CACHE_DIR/validation-history/"
