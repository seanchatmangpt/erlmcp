#!/usr/bin/env bash
###===================================================================
### set_baseline.sh - Set Benchmark Baseline for Regression Detection
###===================================================================
###
### Sets the baseline benchmark results used for regression detection.
### Copies validated results to bench/results/baseline/ directory.
###
### Usage:
###   ./scripts/bench/set_baseline.sh <results_dir>
###
### Example:
###   ./scripts/bench/set_baseline.sh bench/results/20260127_183000
###
### Validates:
###   - All result files have passed metrology validation
###   - Summary report exists
###   - No failed benchmarks in the set
###
###===================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

RESULTS_DIR="${1:-}"
BASELINE_DIR="bench/results/baseline"

# Validate input
if [ -z "$RESULTS_DIR" ]; then
    echo -e "${RED}Error: No results directory specified${NC}"
    echo "Usage: $0 <results_dir>"
    echo ""
    echo "Example: $0 bench/results/20260127_183000"
    exit 1
fi

if [ ! -d "$RESULTS_DIR" ]; then
    echo -e "${RED}Error: Results directory not found: $RESULTS_DIR${NC}"
    exit 1
fi

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Set Benchmark Baseline${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${YELLOW}Source:${NC}      $RESULTS_DIR"
echo -e "${YELLOW}Baseline:${NC}    $BASELINE_DIR"
echo ""

# Count result files
RESULT_FILES=$(find "$RESULTS_DIR" -name "*.json" ! -name "summary.json" | wc -l | tr -d ' ')

if [ "$RESULT_FILES" -eq 0 ]; then
    echo -e "${RED}Error: No benchmark results found in $RESULTS_DIR${NC}"
    exit 1
fi

echo "Found $RESULT_FILES benchmark result(s)"
echo ""

# Check for summary
if [ ! -f "$RESULTS_DIR/summary.json" ]; then
    echo -e "${YELLOW}Warning: No summary.json found${NC}"
    echo "Continuing anyway..."
else
    echo -e "${GREEN}✓ Summary report found${NC}"
fi

# Validate all results with metrology
echo ""
echo -e "${YELLOW}Validating metrology compliance...${NC}"

VALIDATION_FAILED=0

for result_file in "$RESULTS_DIR"/*.json; do
    if [ "$(basename "$result_file")" = "summary.json" ]; then
        continue
    fi

    echo -n "  Validating $(basename "$result_file")... "

    if erl -pa _build/default/lib/*/ebin -noshell -eval "
        case erlmcp_metrology_validator:validate_file(\"$result_file\") of
            ok -> halt(0);
            {error, _} -> halt(1)
        end.
    " > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗ FAILED${NC}"
        VALIDATION_FAILED=1
    fi
done

if [ $VALIDATION_FAILED -eq 1 ]; then
    echo ""
    echo -e "${RED}Error: Some results failed metrology validation${NC}"
    echo "Cannot set baseline with invalid results"
    exit 1
fi

echo ""
echo -e "${GREEN}✓ All results passed metrology validation${NC}"

# Backup existing baseline if present
if [ -d "$BASELINE_DIR" ] && [ -n "$(ls -A "$BASELINE_DIR" 2>/dev/null)" ]; then
    BACKUP_DIR="${BASELINE_DIR}_backup_$(date +%Y%m%d_%H%M%S)"
    echo ""
    echo -e "${YELLOW}Backing up existing baseline to: $BACKUP_DIR${NC}"
    mv "$BASELINE_DIR" "$BACKUP_DIR"
fi

# Create baseline directory
mkdir -p "$BASELINE_DIR"

# Copy results
echo ""
echo -e "${YELLOW}Copying results to baseline...${NC}"
cp -v "$RESULTS_DIR"/*.json "$BASELINE_DIR/" | head -20

# Create baseline metadata
cat > "$BASELINE_DIR/BASELINE_INFO.txt" <<EOF
Baseline Set: $(date)
Source: $RESULTS_DIR
Result Files: $RESULT_FILES
Set By: $(whoami)
Host: $(hostname)

This baseline is used for regression detection.
To update, run: ./scripts/bench/set_baseline.sh <new_results_dir>
EOF

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Baseline Set Successfully${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Location: $BASELINE_DIR"
echo "Files:    $RESULT_FILES benchmark results"
echo ""
echo "Use this baseline for regression detection:"
echo "  ./scripts/bench/compare_to_baseline.sh <new_results_dir>"
echo ""
