#!/usr/bin/env bash
# Automated Coverage Checker - Blocks on low coverage
# Usage: ./tools/coverage-checker.sh
# Exit codes: 0 = success, 1 = below threshold

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
COVERAGE_THRESHOLD=80
RESULTS_DIR="_build/test/coverage"
COVERAGE_JSON="${RESULTS_DIR}/coverage_results.json"
COVERAGE_REPORT="${RESULTS_DIR}/coverage_report.txt"

echo "========================================"
echo "ErlMCP Coverage Checker"
echo "========================================"
echo ""

# Create results directory
mkdir -p "${RESULTS_DIR}"

# Run tests with coverage
echo "Running tests with coverage..."
echo "----------------------------------------"
TERM=dumb rebar3 do eunit, ct --cover || {
    echo -e "${RED}❌ Tests failed - cannot generate coverage${NC}"
    exit 1
}

# Generate coverage report
echo ""
echo "Generating coverage report..."
echo "----------------------------------------"
COVERAGE_OUTPUT="${RESULTS_DIR}/cover_output.txt"
rebar3 cover --verbose 2>&1 | tee "${COVERAGE_OUTPUT}"

# Parse coverage results
echo ""
echo "Parsing coverage results..."
echo "----------------------------------------"

# Initialize JSON
cat > "${COVERAGE_JSON}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "threshold": ${COVERAGE_THRESHOLD},
  "modules": []
}
EOF

# Track statistics
TOTAL_MODULES=0
MODULES_BELOW_THRESHOLD=0
declare -a LOW_COVERAGE_MODULES

# Parse HTML index
COVER_HTML="_build/test/cover/index.html"
if [ -f "${COVER_HTML}" ]; then
    echo "Analyzing module coverage..."
    echo ""

    # Extract module coverage from HTML
    # Format: <td>module_name</td><td>XX%</td>
    while IFS= read -r line; do
        if echo "${line}" | grep -q "<td>.*_.*</td>"; then
            # Extract module name
            MODULE=$(echo "${line}" | sed -n 's/.*<td>\(.*\)<\/td>.*/\1/p')

            # Get next line for coverage percentage
            read -r next_line
            COVERAGE=$(echo "${next_line}" | sed -n 's/.*<td>\([0-9]*\)%<\/td>.*/\1/p')

            if [ -n "${MODULE}" ] && [ -n "${COVERAGE}" ]; then
                TOTAL_MODULES=$((TOTAL_MODULES + 1))

                printf "  %-40s " "${MODULE}"

                if [ "${COVERAGE}" -lt "${COVERAGE_THRESHOLD}" ]; then
                    echo -e "${RED}${COVERAGE}% ❌${NC}"
                    MODULES_BELOW_THRESHOLD=$((MODULES_BELOW_THRESHOLD + 1))
                    LOW_COVERAGE_MODULES+=("${MODULE}: ${COVERAGE}%")
                elif [ "${COVERAGE}" -lt 90 ]; then
                    echo -e "${YELLOW}${COVERAGE}% ⚠️${NC}"
                else
                    echo -e "${GREEN}${COVERAGE}% ✅${NC}"
                fi
            fi
        fi
    done < "${COVER_HTML}"
else
    echo -e "${RED}❌ Coverage HTML report not found${NC}"
    exit 1
fi

# Calculate overall coverage
OVERALL_COVERAGE=$(grep -oP 'Total.*?(\d+)%' "${COVERAGE_OUTPUT}" | \
    grep -oE '[0-9]+' | tail -1 || echo "0")

echo ""
echo "========================================"
echo "COVERAGE SUMMARY"
echo "========================================"
echo "Overall Coverage: ${OVERALL_COVERAGE}%"
echo "Threshold:        ${COVERAGE_THRESHOLD}%"
echo "Total Modules:    ${TOTAL_MODULES}"
echo "Below Threshold:  ${MODULES_BELOW_THRESHOLD}"
echo ""

# Generate detailed report
{
    echo "Coverage Report"
    echo "Generated: $(date)"
    echo ""
    echo "Overall Coverage: ${OVERALL_COVERAGE}%"
    echo "Threshold: ${COVERAGE_THRESHOLD}%"
    echo "Total Modules: ${TOTAL_MODULES}"
    echo "Modules Below Threshold: ${MODULES_BELOW_THRESHOLD}"
    echo ""

    if [ "${MODULES_BELOW_THRESHOLD}" -gt 0 ]; then
        echo "Modules with low coverage:"
        for module in "${LOW_COVERAGE_MODULES[@]}"; do
            echo "  - ${module}"
        done
        echo ""
    fi

    echo "Detailed coverage: file://${PWD}/${COVER_HTML}"
} > "${COVERAGE_REPORT}"

# Check if below threshold
if [ "${MODULES_BELOW_THRESHOLD}" -gt 0 ]; then
    echo -e "${RED}========================================"
    echo "LOW COVERAGE MODULES"
    echo "========================================${NC}"
    echo ""

    for module in "${LOW_COVERAGE_MODULES[@]}"; do
        echo -e "${RED}  ❌ ${module} (threshold: ${COVERAGE_THRESHOLD}%)${NC}"
    done
    echo ""

    echo -e "${RED}❌ FAILURE: ${MODULES_BELOW_THRESHOLD} module(s) below ${COVERAGE_THRESHOLD}% coverage${NC}"
    echo ""
    echo "Report saved to: ${COVERAGE_REPORT}"
    echo "Detailed HTML: file://${PWD}/${COVER_HTML}"
    exit 1
fi

if [ "${OVERALL_COVERAGE}" -lt "${COVERAGE_THRESHOLD}" ]; then
    echo -e "${RED}❌ FAILURE: Overall coverage ${OVERALL_COVERAGE}% is below threshold ${COVERAGE_THRESHOLD}%${NC}"
    echo ""
    echo "Report saved to: ${COVERAGE_REPORT}"
    echo "Detailed HTML: file://${PWD}/${COVER_HTML}"
    exit 1
fi

echo -e "${GREEN}✅ SUCCESS: All modules meet ${COVERAGE_THRESHOLD}% coverage threshold${NC}"
echo ""
echo "Report saved to: ${COVERAGE_REPORT}"
echo "Detailed HTML: file://${PWD}/${COVER_HTML}"
exit 0
