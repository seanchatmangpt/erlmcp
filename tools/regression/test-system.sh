#!/usr/bin/env bash
# test-system.sh - Test the regression prevention system
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "Testing Regression Prevention System"
echo "====================================="
echo ""

# Test 1: Script syntax
echo -n "Test 1: Script syntax validation... "
if bash -n "${SCRIPT_DIR}/detect-regression.sh" && \
   bash -n "${SCRIPT_DIR}/block-on-regression.sh" && \
   bash -n "${SCRIPT_DIR}/generate-html-report.sh"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 2: Scripts are executable
echo -n "Test 2: Scripts are executable... "
if [[ -x "${SCRIPT_DIR}/detect-regression.sh" ]] && \
   [[ -x "${SCRIPT_DIR}/block-on-regression.sh" ]] && \
   [[ -x "${SCRIPT_DIR}/generate-html-report.sh" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 3: Allowlist is valid JSON
echo -n "Test 3: Allowlist is valid JSON... "
if python3 -m json.tool "${SCRIPT_DIR}/allowlist.json" >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 4: Required dependencies available
echo -n "Test 4: Check dependencies (bc, python3)... "
if command -v bc >/dev/null 2>&1 && command -v python3 >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${YELLOW}WARN${NC} (bc and python3 recommended)"
fi

# Test 5: Can create metrics directory
echo -n "Test 5: Can create metrics directory... "
mkdir -p "${PROJECT_ROOT}/.metrics"
if [[ -d "${PROJECT_ROOT}/.metrics" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 6: GitHub workflow exists
echo -n "Test 6: GitHub workflow exists... "
if [[ -f "${PROJECT_ROOT}/.github/workflows/regression-guard.yml" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 7: Documentation exists
echo -n "Test 7: Documentation exists... "
if [[ -f "${PROJECT_ROOT}/docs/regression/PREVENTION_SYSTEM.md" ]] && \
   [[ -f "${PROJECT_ROOT}/docs/regression/QUICK_START.md" ]] && \
   [[ -f "${SCRIPT_DIR}/README.md" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 8: Allowlist schema is correct
echo -n "Test 8: Allowlist schema validation... "
if grep -q '"allowed_regressions"' "${SCRIPT_DIR}/allowlist.json" && \
   grep -q '"review_policy"' "${SCRIPT_DIR}/allowlist.json"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 9: All scripts have proper headers
echo -n "Test 9: Script headers present... "
if grep -q '#!/usr/bin/env bash' "${SCRIPT_DIR}/detect-regression.sh" && \
   grep -q '#!/usr/bin/env bash' "${SCRIPT_DIR}/block-on-regression.sh" && \
   grep -q '#!/usr/bin/env bash' "${SCRIPT_DIR}/generate-html-report.sh"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 10: File structure is correct
echo -n "Test 10: File structure validation... "
expected_files=(
    "detect-regression.sh"
    "block-on-regression.sh"
    "generate-html-report.sh"
    "allowlist.json"
    "README.md"
    "test-system.sh"
)
all_present=true
for file in "${expected_files[@]}"; do
    if [[ ! -f "${SCRIPT_DIR}/${file}" ]]; then
        all_present=false
        break
    fi
done

if $all_present; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

echo ""
echo "====================================="
echo -e "${GREEN}All tests passed!${NC}"
echo ""
echo "System is ready to use:"
echo "  ./tools/regression/detect-regression.sh"
echo "  ./tools/regression/block-on-regression.sh"
echo "  ./tools/regression/generate-html-report.sh"
echo ""
echo "Documentation:"
echo "  docs/regression/QUICK_START.md"
echo "  docs/regression/PREVENTION_SYSTEM.md"
echo "  tools/regression/README.md"
echo ""

# Optional: Run actual detection if in CI or requested
if [[ "${RUN_FULL_TEST:-false}" == "true" ]]; then
    echo "Running full regression detection..."
    "${SCRIPT_DIR}/detect-regression.sh" || true
    echo ""
fi
