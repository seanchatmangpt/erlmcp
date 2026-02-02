#!/bin/bash
# Chicago School TDD Anti-Pattern Scanner
# Version: 1.0.0
# Purpose: Detect TDD anti-patterns in erlmcp test code

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
VIOLATIONS=0
WARNINGS=0

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Chicago School TDD Anti-Pattern Scanner${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

# ============================================================================
# VIOLATION 1: Dummy Process Pattern
# ============================================================================
echo -e "${BLUE}[1/5] Scanning for dummy process patterns...${NC}"

DUMMY_PATTERN='spawn\(.*fun.*receive.*stop.*ok\|spawn_link.*fun.*receive.*stop.*ok'
DUMMY_FILES=$(find apps/*/test/ test/ -name "*.erl" -exec grep -l -E "$DUMMY_PATTERN" {} \; 2>/dev/null || true)

if [ -n "$DUMMY_FILES" ]; then
    echo -e "${RED}❌ VIOLATION: Dummy process pattern detected${NC}"
    echo ""
    echo "Rule: NO dummy processes (spawn/fun/receive/stop pattern)"
    echo "Rationale: Tests must use REAL erlmcp gen_server processes"
    echo ""
    echo "Violating files:"
    for file in $DUMMY_FILES; do
        LINE_NUMBERS=$(grep -n -E "$DUMMY_PATTERN" "$file" | cut -d: -f1 | tr '\n' ',' | sed 's/,$//')
        echo "  - $file (lines: $LINE_NUMBERS)"
    done
    echo ""
    VIOLATIONS=$((VIOLATIONS + 1))
else
    echo -e "${GREEN}✅ PASSED - No dummy process patterns found${NC}"
fi
echo ""

# ============================================================================
# VIOLATION 2: State Inspection via sys:get_status
# ============================================================================
echo -e "${BLUE}[2/5] Scanning for state inspection...${NC}"

STATE_PATTERN='sys:get_status\|sys:get_state'
STATE_FILES=$(find apps/*/test/ test/ -name "*.erl" -exec grep -l -E "$STATE_PATTERN" {} \; 2>/dev/null || true)

if [ -n "$STATE_FILES" ]; then
    echo -e "${RED}❌ VIOLATION: State inspection detected${NC}"
    echo ""
    echo "Rule: NO state inspection via sys:get_status or sys:get_state"
    echo "Rationale: Tests must verify OBSERVABLE behavior, not internal state"
    echo ""
    echo "Violating files:"
    for file in $STATE_FILES; do
        LINE_NUMBERS=$(grep -n -E "$STATE_PATTERN" "$file" | cut -d: -f1 | tr '\n' ',' | sed 's/,$//')
        echo "  - $file (lines: $LINE_NUMBERS)"
    done
    echo ""
    VIOLATIONS=$((VIOLATIONS + 1))
else
    echo -e "${GREEN}✅ PASSED - No state inspection found${NC}"
fi
echo ""

# ============================================================================
# VIOLATION 3: Record Duplication (test-specific state records)
# ============================================================================
echo -e "${BLUE}[3/5] Scanning for record duplication...${NC}"

RECORD_PATTERN='^-record\(state,'
RECORD_FILES=$(find apps/*/test/ test/ -name "*.erl" -exec grep -l "$RECORD_PATTERN" {} \; 2>/dev/null || true)

if [ -n "$RECORD_FILES" ]; then
    echo -e "${RED}❌ VIOLATION: Record duplication detected${NC}"
    echo ""
    echo "Rule: NO test-specific state records"
    echo "Rationale: Tests must use PRODUCTION records, not define test-specific state"
    echo ""
    echo "Violating files:"
    for file in $RECORD_FILES; do
        echo "  - $file"
    done
    echo ""
    VIOLATIONS=$((VIOLATIONS + 1))
else
    echo -e "${GREEN}✅ PASSED - No record duplication found${NC}"
fi
echo ""

# ============================================================================
# VIOLATION 4: Mock Framework Usage (meck)
# ============================================================================
echo -e "${BLUE}[4/5] Scanning for mock framework usage...${NC}"

MOCK_PATTERN='meck:\(new\|expect\|unload\|passthrough\)'
MOCK_FILES=$(find apps/*/test/ test/ -name "*.erl" -exec grep -l -E "$MOCK_PATTERN" {} \; 2>/dev/null || true)

if [ -n "$MOCK_FILES" ]; then
    echo -e "${RED}❌ VIOLATION: Mock framework usage detected${NC}"
    echo ""
    echo "Rule: NO mock framework usage (meck, mock, stub, fake)"
    echo "Rationale: Tests must use REAL erlmcp processes, not mocks/stubs/fakes"
    echo ""
    echo "Violating files:"
    for file in $MOCK_FILES; do
        LINE_NUMBERS=$(grep -n -E "$MOCK_PATTERN" "$file" | cut -d: -f1 | tr '\n' ',' | sed 's/,$//')
        echo "  - $file (lines: $LINE_NUMBERS)"
    done
    echo ""
    VIOLATIONS=$((VIOLATIONS + 1))
else
    echo -e "${GREEN}✅ PASSED - No mock framework usage found${NC}"
fi
echo ""

# ============================================================================
# VIOLATION 5: Stub/Fake Detection
# ============================================================================
echo -e "${BLUE}[5/5] Scanning for stub/fake patterns...${NC}"

STUB_PATTERNS='fake|stub|dummy.*server|mock.*server'
STUB_FILES=$(find apps/*/test/ test/ -name "*.erl" -exec grep -i -l -E "(module|behavior).*${STUB_PATTERNS}" {} \; 2>/dev/null || true)

if [ -n "$STUB_FILES" ]; then
    echo -e "${YELLOW}⚠️  WARNING: Possible stub/fake pattern detected${NC}"
    echo ""
    echo "Rule: NO stub/fake modules"
    echo "Rationale: Tests must use REAL erlmcp processes"
    echo ""
    echo "Potentially violating files:"
    for file in $STUB_FILES; do
        echo "  - $file"
    done
    echo ""
    WARNINGS=$((WARNINGS + 1))
else
    echo -e "${GREEN}✅ PASSED - No stub/fake patterns found${NC}"
fi
echo ""

# ============================================================================
# SUMMARY
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Scan Summary${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo "Total violations: $VIOLATIONS"
echo "Total warnings: $WARNINGS"
echo ""

if [ $VIOLATIONS -gt 0 ]; then
    echo -e "${RED}❌ CHICAGO SCHOOL TDD VIOLATIONS DETECTED${NC}"
    echo ""
    echo "Action Required: Fix all violations before merging."
    echo ""
    echo "Chicago School TDD Rules:"
    echo "  1. NO mocks, fakes, or placeholder implementations"
    echo "  2. NO dummy processes (spawn/fun/receive/stop)"
    echo "  3. NO state inspection (sys:get_status)"
    echo "  4. NO record duplication (test-specific state records)"
    echo "  5. Tests MUST use real erlmcp processes"
    echo ""
    echo "Documentation: See docs/ERLMCP_FLOW_QUALITY_STANDARDS.md"
    echo ""
    exit 1
else
    echo -e "${GREEN}✅ ALL CHECKS PASSED${NC}"
    echo ""
    echo "All tests comply with Chicago School TDD principles:"
    echo "  ✅ No mocks, fakes, or placeholder implementations"
    echo "  ✅ No dummy processes"
    echo "  ✅ No state inspection"
    echo "  ✅ No record duplication"
    echo ""
    if [ $WARNINGS -gt 0 ]; then
        echo -e "${YELLOW}⚠️  $WARNINGS warning(s) detected (non-blocking)${NC}"
        echo ""
    fi
    exit 0
fi
