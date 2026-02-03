#!/bin/bash
###############################################################################
# Poka-Yoke (ポカヨケ) Pre-Commit Validation
# Agent: agent-17
# Purpose: Mistake-proofing validation before git commit
#
# Validates:
# 1. Behavior callback definitions (compile-time OTP compliance)
# 2. Explicit timeouts on all gen_server:call
# 3. Safe process registration (gproc over register/2)
# 4. Dialyzer warnings
# 5. Named ETS tables only
#
# Exit codes:
# 0 = All checks passed
# 1 = One or more checks failed
###############################################################################

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
VIOLATIONS=0
WARNINGS=0

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🛡️  POKA-YOKE (ポカヨケ) - Mistake-Proofing Validation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

###############################################################################
# Check 1: Behavior Callback Definitions
###############################################################################

# Get the project root directory (works on macOS and Linux)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
APPS_DIR="${PROJECT_ROOT}/apps"

# Validate we're in the right directory
if [ ! -d "$APPS_DIR" ]; then
    echo -e "${RED}Error: apps directory not found at $APPS_DIR${NC}"
    exit 1
fi

echo "📋 Check 1: Behavior Callback Definitions"

BEHAVIOR_COUNT=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f -exec grep -h "-callback" {} \; 2>/dev/null | wc -l | xargs)

if [ "$BEHAVIOR_COUNT" -lt 50 ]; then
    echo -e "${RED}❌ BEHAVIOR: Only $BEHAVIOR_COUNT callback definitions found (expected 50+)${NC}"
    echo -e "${YELLOW}   Action: Add -callback directives to behavior modules${NC}"
    ((VIOLATIONS++))
else
    echo -e "${GREEN}✅ BEHAVIOR: $BEHAVIOR_COUNT callback definitions found${NC}"
fi
echo ""

###############################################################################
# Check 2: Missing Timeouts on gen_server:call
###############################################################################

echo "⏱️  Check 2: Explicit Timeouts on gen_server:call"

MISSING_TIMEOUTS=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f -exec grep -h "gen_server:call" {} \; 2>/dev/null | grep -vE ",\s*[0-9]+|,\s*infinity|,\s*[_a-zA-Z_]+_TIMEOUT" | wc -l | xargs)

if [ "$MISSING_TIMEOUTS" -gt 0 ]; then
    echo -e "${RED}❌ TIMEOUT: $MISSING_TIMEOUTS gen_server:call operations without explicit timeout${NC}"
    echo -e "${YELLOW}   Action: Add timeout parameter (e.g., ?CALL_TIMEOUT)${NC}"
    echo -e "${YELLOW}   Example: gen_server:call(?MODULE, Request, ?CALL_TIMEOUT)${NC}"
    ((VIOLATIONS++))
else
    echo -e "${GREEN}✅ TIMEOUT: All gen_server:call operations have explicit timeouts${NC}"
fi
echo ""

###############################################################################
# Check 3: Unsafe register/2 Usage
###############################################################################

echo "🔒 Check 3: Safe Process Registration"

UNSAFE_REGISTER=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f -exec grep -h "register(" {} \; 2>/dev/null | grep -v "gproc:register\|gproc:reg\|%%" | wc -l | xargs)

if [ "$UNSAFE_REGISTER" -gt 0 ]; then
    echo -e "${RED}❌ REGISTER: $UNSAFE_REGISTER unsafe register/2 calls found${NC}"
    echo -e "${YELLOW}   Action: Use gproc:reg/1 for scoped registration${NC}"
    echo -e "${YELLOW}   Example: gproc:reg({n, l, my_process_name})${NC}"
    ((VIOLATIONS++))
else
    echo -e "${GREEN}✅ REGISTER: Using gproc for safe process registration${NC}"
fi
echo ""

###############################################################################
# Check 4: Raw spawn/proc_lib Usage (unsupervised processes)
###############################################################################

echo "🏭 Check 4: Supervised Processes Only"

RAW_SPAWN=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f -exec grep -hE "erlang:spawn\(|proc_lib:spawn\(|spawn\(fun|spawn\(Module" {} \; 2>/dev/null | grep -v "%%\|supervisor:start_child\|proc_lib:start_link" | wc -l | xargs)

if [ "$RAW_SPAWN" -gt 0 ]; then
    echo -e "${YELLOW}⚠️  SPAWN: $RAW_SPAWN raw spawn operations found (may be supervised)${NC}"
    echo -e "${YELLOW}   Action: Verify all spawn operations are supervised${NC}"
    ((WARNINGS++))
else
    echo -e "${GREEN}✅ SPAWN: All processes supervised${NC}"
fi
echo ""

###############################################################################
# Check 5: Named ETS Tables Only
###############################################################################

echo "📊 Check 5: Named ETS Tables"

UNNAMED_ETS=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f -exec grep -hE "ets:new\s*\(" {} \; 2>/dev/null | grep -v "named_table" | wc -l | xargs)

if [ "$UNNAMED_ETS" -gt 0 ]; then
    echo -e "${YELLOW}⚠️  ETS: $UNNAMED_ETS unnamed ETS tables found (memory leak risk)${NC}"
    echo -e "${YELLOW}   Action: Add 'named_table' option to ets:new/2${NC}"
    ((WARNINGS++))
else
    echo -e "${GREEN}✅ ETS: All tables named (auto-cleanup safe)${NC}"
fi
echo ""

###############################################################################
# Check 6: Type Specifications Coverage
###############################################################################

echo "📝 Check 6: Type Specifications Coverage"

TOTAL_FILES=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f | wc -l | xargs)
SPECS_COUNT=$(find "$APPS_DIR" -name "*.erl" -path "*/src/*" -type f -exec grep -l "-spec " {} \; 2>/dev/null | wc -l | xargs)

if [ "$SPECS_COUNT" -lt "$TOTAL_FILES" ]; then
    MISSING=$((TOTAL_FILES - SPECS_COUNT))
    echo -e "${YELLOW}⚠️  SPECS: $MISSING files missing type specifications ($SPECS_COUNT/$TOTAL_FILES)${NC}"
    echo -e "${YELLOW}   Action: Add -spec to all public functions${NC}"
    ((WARNINGS++))
else
    echo -e "${GREEN}✅ SPECS: All files have type specifications${NC}"
fi
echo ""

###############################################################################
# Check 7: Dialyzer Warnings
###############################################################################

echo "🔍 Check 7: Dialyzer Analysis"

echo "   Running Dialyzer (this may take a minute)..."

if rebar3 dialyzer > /tmp/dialyzer_output.txt 2>&1; then
    DIALYZER_WARNINGS=$(grep -i "warning:" /tmp/dialyzer_output.txt 2>/dev/null | wc -l | xargs)
    if [ "$DIALYZER_WARNINGS" -eq 0 ]; then
        echo -e "${GREEN}✅ DIALYZER: No warnings${NC}"
    else
        echo -e "${YELLOW}⚠️  DIALYZER: $DIALYZER_WARNINGS warnings found${NC}"
        echo -e "${YELLOW}   Check: /tmp/dialyzer_output.txt${NC}"
        ((WARNINGS++))
    fi
else
    echo -e "${RED}❌ DIALYZER: Analysis failed${NC}"
    echo -e "${YELLOW}   Check: /tmp/dialyzer_output.txt${NC}"
    ((VIOLATIONS++))
fi
echo ""

###############################################################################
# Summary
###############################################################################

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 POKA-YOKE Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

if [ $VIOLATIONS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ POKA-YOKE: All checks passed - mistake-proofing active${NC}"
    echo ""
    echo "   ✓ Behavior callbacks: $BEHAVIOR_COUNT"
    echo "   ✓ Explicit timeouts: 0 missing"
    echo "   ✓ Safe registration: Using gproc"
    echo "   ✓ Dialyzer: Clean"
    echo ""
    echo -e "${GREEN}Commit allowed. Mistake-proofing: ACTIVE 🛡️${NC}"
    exit 0
elif [ $VIOLATIONS -eq 0 ] && [ $WARNINGS -gt 0 ]; then
    echo -e "${YELLOW}⚠️  POKA-YOKE: $WARNINGS warnings (non-blocking)${NC}"
    echo ""
    echo "   ✓ No critical violations"
    echo "   ⚠ $WARNINGS warnings (review recommended)"
    echo ""
    echo -e "${YELLOW}Commit allowed with warnings${NC}"
    exit 0
else
    echo -e "${RED}❌ POKA-YOKE: $VIOLATIONS critical violations - commit blocked${NC}"
    echo ""
    echo "   ❌ Behavior callbacks: $BEHAVIOR_COUNT (expected 50+)"
    echo "   ❌ Missing timeouts: $MISSING_TIMEOUTS"
    echo "   ❌ Unsafe registration: $UNSAFE_REGISTER"
    echo "   ❌ Dialyzer: Failed"
    echo ""
    echo -e "${RED}Fix violations before committing${NC}"
    echo ""
    echo "Quick fixes:"
    echo "  1. Add -callback to behavior modules"
    echo "  2. Add timeout to gen_server:call (use ?CALL_TIMEOUT)"
    echo "  3. Replace register/2 with gproc:reg/1"
    echo "  4. Fix Dialyzer warnings"
    echo ""
    echo "See docs/POKA_YOKE_ANALYSIS.md for detailed guidance"
    exit 1
fi
