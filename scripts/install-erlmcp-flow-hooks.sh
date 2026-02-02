#!/bin/bash
# Install erlmcp-flow Pre-Commit Quality Hooks
# Version: 1.0.0

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Installing erlmcp-flow Pre-Commit Quality Hooks${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

# Check if we're in a git repository
if [ ! -d ".git" ]; then
    echo -e "${RED}❌ Error: Not a git repository${NC}"
    echo "Run this script from the repository root."
    exit 1
fi

# Create hooks directory if it doesn't exist
mkdir -p .git/hooks

# ============================================================================
# PRE-COMMIT HOOK for erlmcp-flow
# ============================================================================
echo -e "${BLUE}[1/2] Installing erlmcp-flow pre-commit hook...${NC}"

cat > .git/hooks/pre-commit-erlmcp-flow << 'HOOK_EOF'
#!/bin/bash
# erlmcp-flow pre-commit quality gate hook
# Auto-generated - DO NOT EDIT MANUALLY

set -euo pipefail

# Only run if erlmcp-flow files changed
FLOW_CHANGED=$(git diff --cached --name-only | grep "^apps/erlmcp_flow/" || true)

if [ -z "$FLOW_CHANGED" ]; then
    exit 0
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo -e "${BLUE}🚦 Running erlmcp-flow pre-commit gates...${NC}"
echo ""

FAILURES=0

# GATE 1: Compilation (FAST)
echo -e "${BLUE}[1/5] Compilation...${NC}"
if ! cd apps/erlmcp_flow && TERM=dumb rebar3 compile > /tmp/flow-pre-commit-compile.log 2>&1; then
    echo -e "${RED}  ❌ Compilation failed${NC}"
    tail -20 /tmp/flow-pre-commit-compile.log
    FAILURES=$((FAILURES + 1))
else
    echo -e "${GREEN}  ✅ Compilation passed${NC}"
fi
cd ../..

# GATE 2: Format (AUTO-FIX)
echo -e "${BLUE}[2/5] Format check...${NC}"
cd apps/erlmcp_flow
if ! rebar3 format --verify > /tmp/flow-pre-commit-format.log 2>&1; then
    echo -e "${YELLOW}  ⚠️  Auto-formatting...${NC}"
    rebar3 format
    git add -u src/*.erl test/*.erl 2>/dev/null || true
    echo -e "${GREEN}  ✅ Formatted and staged${NC}"
else
    echo -e "${GREEN}  ✅ Format check passed${NC}"
fi
cd ../..

# GATE 3: Chicago TDD Compliance (FAST)
echo -e "${BLUE}[3/5] Chicago TDD compliance...${NC}"
VIOLATIONS=0

if grep -r "meck:new\|meck:expect" apps/erlmcp_flow/test/ > /dev/null 2>&1; then
    echo -e "${RED}  ❌ Mock usage detected (meck framework)${NC}"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

if grep -r "sys:get_status\|sys:get_state" apps/erlmcp_flow/test/ > /dev/null 2>&1; then
    echo -e "${RED}  ❌ State inspection detected${NC}"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

if grep -r -E "spawn\(fun\(\) -> receive" apps/erlmcp_flow/test/ > /dev/null 2>&1; then
    echo -e "${RED}  ❌ Dummy process pattern detected${NC}"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

if [ $VIOLATIONS -eq 0 ]; then
    echo -e "${GREEN}  ✅ Chicago TDD compliance${NC}"
else
    echo -e "${RED}  ❌ Chicago TDD violations: $VIOLATIONS${NC}"
    FAILURES=$((FAILURES + 1))
fi

# GATE 4: OTP Compliance (FAST)
echo -e "${BLUE}[4/5] OTP compliance...${NC}"
OTP_VIOLATIONS=0

if grep -r "spawn(fun()" apps/erlmcp_flow/src/ 2>/dev/null | grep -v "test" | grep -q "."; then
    echo -e "${RED}  ❌ Unsupervised spawn detected${NC}"
    OTP_VIOLATIONS=$((OTP_VIOLATIONS + 1))
fi

if [ $OTP_VIOLATIONS -eq 0 ]; then
    echo -e "${GREEN}  ✅ OTP compliance${NC}"
else
    echo -e "${RED}  ❌ OTP violations: $OTP_VIOLATIONS${NC}"
    FAILURES=$((FAILURES + 1))
fi

# GATE 5: Smoke Tests (FAST - Registry only)
echo -e "${BLUE}[5/5] Smoke tests...${NC}"
if [ -f "apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl" ]; then
    if ! cd apps/erlmcp_flow && rebar3 eunit --module=erlmcp_flow_registry_tests > /tmp/flow-pre-commit-smoke.log 2>&1; then
        echo -e "${RED}  ❌ Smoke tests failed${NC}"
        tail -20 /tmp/flow-pre-commit-smoke.log
        FAILURES=$((FAILURES + 1))
    else
        echo -e "${GREEN}  ✅ Smoke tests passed${NC}"
    fi
    cd ../..
else
    echo -e "${YELLOW}  ⚠️  No smoke tests found (skipped)${NC}"
fi

# SUMMARY
echo ""
if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ All erlmcp-flow pre-commit gates passed${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ $FAILURES gate(s) failed - commit blocked${NC}"
    echo ""
    echo "Action Required:"
    echo "  1. Fix the failing gates listed above"
    echo "  2. Run './scripts/erlmcp-flow-quality-check.sh --fast' for details"
    echo "  3. Run './scripts/erlmcp-flow-quality-check.sh --full' for comprehensive check"
    echo ""
    echo "To bypass (NOT RECOMMENDED):"
    echo "  git commit --no-verify"
    echo ""
    exit 1
fi
HOOK_EOF

chmod +x .git/hooks/pre-commit-erlmcp-flow

# Integrate with main pre-commit hook
if [ -f ".git/hooks/pre-commit" ]; then
    if ! grep -q "pre-commit-erlmcp-flow" .git/hooks/pre-commit; then
        echo "" >> .git/hooks/pre-commit
        echo "# erlmcp-flow quality gates" >> .git/hooks/pre-commit
        echo ".git/hooks/pre-commit-erlmcp-flow || exit 1" >> .git/hooks/pre-commit
        echo -e "${GREEN}  ✅ Integrated with existing pre-commit hook${NC}"
    else
        echo -e "${YELLOW}  ⚠️  Already integrated with pre-commit hook${NC}"
    fi
else
    # Create main pre-commit hook
    cat > .git/hooks/pre-commit << 'MAIN_HOOK_EOF'
#!/bin/bash
# Main pre-commit hook
set -euo pipefail

# Run erlmcp-flow gates
.git/hooks/pre-commit-erlmcp-flow || exit 1

echo "✅ All pre-commit checks passed"
MAIN_HOOK_EOF
    chmod +x .git/hooks/pre-commit
    echo -e "${GREEN}  ✅ Created main pre-commit hook${NC}"
fi

echo ""

# ============================================================================
# PRE-PUSH HOOK for erlmcp-flow
# ============================================================================
echo -e "${BLUE}[2/2] Installing erlmcp-flow pre-push hook...${NC}"

cat > .git/hooks/pre-push-erlmcp-flow << 'HOOK_EOF'
#!/bin/bash
# erlmcp-flow pre-push quality gate hook
# Auto-generated - DO NOT EDIT MANUALLY

set -euo pipefail

# Only run if erlmcp-flow files changed
FLOW_CHANGED=$(git diff --name-only origin/main...HEAD | grep "^apps/erlmcp_flow/" || true)

if [ -z "$FLOW_CHANGED" ]; then
    exit 0
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo -e "${BLUE}🚀 Running erlmcp-flow pre-push gates...${NC}"
echo ""

FAILURES=0

# Full Quality Check
echo -e "${BLUE}Running comprehensive quality gates...${NC}"
if ! ./scripts/erlmcp-flow-quality-check.sh --full; then
    echo -e "${RED}❌ Quality gates failed${NC}"
    FAILURES=$((FAILURES + 1))
fi

# Summary
echo ""
if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ All erlmcp-flow pre-push gates passed${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ Pre-push gates failed - push blocked${NC}"
    echo ""
    echo "Action Required:"
    echo "  1. Fix the failing gates"
    echo "  2. Run './scripts/erlmcp-flow-quality-check.sh --full'"
    echo "  3. Ensure all 12 quality gates pass"
    echo ""
    echo "To bypass (NOT RECOMMENDED for main/release branches):"
    echo "  git push --no-verify"
    echo ""
    exit 1
fi
HOOK_EOF

chmod +x .git/hooks/pre-push-erlmcp-flow

# Integrate with main pre-push hook
if [ -f ".git/hooks/pre-push" ]; then
    if ! grep -q "pre-push-erlmcp-flow" .git/hooks/pre-push; then
        echo "" >> .git/hooks/pre-push
        echo "# erlmcp-flow quality gates" >> .git/hooks/pre-push
        echo ".git/hooks/pre-push-erlmcp-flow || exit 1" >> .git/hooks/pre-push
        echo -e "${GREEN}  ✅ Integrated with existing pre-push hook${NC}"
    else
        echo -e "${YELLOW}  ⚠️  Already integrated with pre-push hook${NC}"
    fi
else
    # Create main pre-push hook
    cat > .git/hooks/pre-push << 'MAIN_HOOK_EOF'
#!/bin/bash
# Main pre-push hook
set -euo pipefail

# Run erlmcp-flow gates
.git/hooks/pre-push-erlmcp-flow || exit 1

echo "✅ All pre-push checks passed"
MAIN_HOOK_EOF
    chmod +x .git/hooks/pre-push
    echo -e "${GREEN}  ✅ Created main pre-push hook${NC}"
fi

echo ""

# ============================================================================
# SUMMARY
# ============================================================================
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✅ erlmcp-flow quality hooks installed successfully${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo "Installed hooks:"
echo "  ✅ pre-commit-erlmcp-flow  - Fast checks (compilation, format, Chicago TDD, smoke tests)"
echo "  ✅ pre-push-erlmcp-flow    - Full quality gates (all 12 gates)"
echo ""
echo "Hook behavior:"
echo "  Pre-commit: Runs on every commit (fast, ~30-60s)"
echo "  Pre-push:   Runs on every push (comprehensive, ~5-10min)"
echo ""
echo "Testing the hooks:"
echo "  ./scripts/erlmcp-flow-quality-check.sh --fast    # Fast checks"
echo "  ./scripts/erlmcp-flow-quality-check.sh --full    # All 12 gates"
echo ""
echo "Bypassing hooks (NOT RECOMMENDED):"
echo "  git commit --no-verify"
echo "  git push --no-verify"
echo ""
echo -e "${YELLOW}⚠️  Warning: Bypassing hooks on main/release branches is prohibited${NC}"
echo ""
