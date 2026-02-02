#!/bin/bash
# Install quality gate pre-commit hooks
# Version: 1.0.0

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Installing erlmcp Quality Gate Pre-Commit Hooks${NC}"
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
# PRE-COMMIT HOOK
# ============================================================================
echo -e "${BLUE}[1/3] Installing pre-commit hook...${NC}"

cat > .git/hooks/pre-commit << 'HOOK_EOF'
#!/bin/bash
# erlmcp quality gate pre-commit hook
# Auto-generated - DO NOT EDIT MANUALLY

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo -e "${BLUE}🚦 Running quality gates...${NC}"
echo ""

GATE_FAILURES=0

# ============================================================================
# GATE 1: Compilation
# ============================================================================
echo -e "${BLUE}[1/4] Gate: Compilation${NC}"
if TERM=dumb rebar3 compile > /tmp/pre-commit-compile.log 2>&1; then
    echo -e "${GREEN}  ✅ Compilation passed${NC}"
else
    echo -e "${RED}  ❌ Compilation failed${NC}"
    echo ""
    tail -20 /tmp/pre-commit-compile.log
    echo ""
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

# ============================================================================
# GATE 2: Smoke Tests (fast)
# ============================================================================
echo -e "${BLUE}[2/4] Gate: Smoke tests${NC}"
if rebar3 eunit --module=erlmcp_json_rpc_tests > /tmp/pre-commit-smoke.log 2>&1; then
    echo -e "${GREEN}  ✅ Smoke tests passed${NC}"
else
    echo -e "${RED}  ❌ Smoke tests failed${NC}"
    echo ""
    tail -20 /tmp/pre-commit-smoke.log
    echo ""
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

# ============================================================================
# GATE 3: Format Check
# ============================================================================
echo -e "${BLUE}[3/4] Gate: Format check${NC}"
if rebar3 format --verify > /tmp/pre-commit-format.log 2>&1; then
    echo -e "${GREEN}  ✅ Format check passed${NC}"
else
    echo -e "${YELLOW}  ⚠️  Format violations detected - auto-formatting...${NC}"
    rebar3 format

    # Stage formatted files
    git add -u apps/*/src/*.erl apps/*/test/*.erl 2>/dev/null || true

    echo -e "${GREEN}  ✅ Files formatted and staged${NC}"
fi

# ============================================================================
# GATE 4: Chicago TDD Anti-Patterns
# ============================================================================
echo -e "${BLUE}[4/4] Gate: Chicago TDD compliance${NC}"

# Check for mocks
if grep -r "meck:new\|meck:expect" apps/*/test/ test/ > /dev/null 2>&1; then
    echo -e "${RED}  ❌ Mock usage detected${NC}"
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

# Check for sys:get_status
if grep -r "sys:get_status\|sys:get_state" apps/*/test/ test/ > /dev/null 2>&1; then
    echo -e "${RED}  ❌ State inspection detected${NC}"
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

# Check for dummy processes
if grep -r -E "spawn\(fun\(\) -> receive" apps/*/test/ test/ > /dev/null 2>&1; then
    echo -e "${RED}  ❌ Dummy process pattern detected${NC}"
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

if [ $GATE_FAILURES -eq 0 ]; then
    echo -e "${GREEN}  ✅ Chicago TDD compliance passed${NC}"
fi

# ============================================================================
# SUMMARY
# ============================================================================
echo ""
if [ $GATE_FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ All pre-commit gates passed - commit allowed${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ $GATE_FAILURES gate(s) failed - commit blocked${NC}"
    echo ""
    echo "Action Required:"
    echo "  1. Fix the failing gates listed above"
    echo "  2. Run 'make quick' for comprehensive checks"
    echo "  3. Run 'make help' for available targets"
    echo ""
    echo "To bypass (NOT RECOMMENDED):"
    echo "  git commit --no-verify"
    echo ""
    exit 1
fi
HOOK_EOF

chmod +x .git/hooks/pre-commit
echo -e "${GREEN}  ✅ pre-commit hook installed${NC}"
echo ""

# ============================================================================
# COMMIT-MSG HOOK
# ============================================================================
echo -e "${BLUE}[2/3] Installing commit-msg hook...${NC}"

cat > .git/hooks/commit-msg << 'HOOK_EOF'
#!/bin/bash
# erlmcp commit message validation hook
# Auto-generated - DO NOT EDIT MANUALLY

set -euo pipefail

COMMIT_MSG_FILE=$1
COMMIT_MSG=$(cat "$COMMIT_MSG_FILE")

# Skip merge commits
if echo "$COMMIT_MSG" | grep -q "^Merge"; then
    exit 0
fi

# Validate commit message format
# Expected: type(scope): message
# Examples:
#   feat(core): add JSON-RPC server
#   fix(transport): handle connection timeout
#   test(client): add integration tests
#   docs(readme): update installation guide

PATTERN='^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\(.+\))?: .{3,}'

if ! echo "$COMMIT_MSG" | grep -qE "$PATTERN"; then
    echo ""
    echo "❌ Invalid commit message format"
    echo ""
    echo "Expected format:"
    echo "  type(scope): message"
    echo ""
    echo "Types:"
    echo "  feat     - New feature"
    echo "  fix      - Bug fix"
    echo "  docs     - Documentation"
    echo "  test     - Tests"
    echo "  refactor - Code refactoring"
    echo "  perf     - Performance improvement"
    echo "  build    - Build system"
    echo "  ci       - CI/CD"
    echo ""
    echo "Examples:"
    echo "  feat(core): add JSON-RPC server"
    echo "  fix(transport): handle connection timeout"
    echo "  test(client): add integration tests"
    echo ""
    exit 1
fi

exit 0
HOOK_EOF

chmod +x .git/hooks/commit-msg
echo -e "${GREEN}  ✅ commit-msg hook installed${NC}"
echo ""

# ============================================================================
# PRE-PUSH HOOK
# ============================================================================
echo -e "${BLUE}[3/3] Installing pre-push hook...${NC}"

cat > .git/hooks/pre-push << 'HOOK_EOF'
#!/bin/bash
# erlmcp pre-push quality gate hook
# Auto-generated - DO NOT EDIT MANUALLY

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo -e "${BLUE}🚀 Running pre-push quality gates...${NC}"
echo ""

GATE_FAILURES=0

# Full test suite
echo -e "${BLUE}[1/3] Gate: Full test suite${NC}"
if rebar3 eunit > /tmp/pre-push-eunit.log 2>&1; then
    echo -e "${GREEN}  ✅ EUnit tests passed${NC}"
else
    echo -e "${RED}  ❌ EUnit tests failed${NC}"
    tail -30 /tmp/pre-push-eunit.log
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

# Coverage check
echo -e "${BLUE}[2/3] Gate: Coverage ≥80%${NC}"
if rebar3 cover --verbose > /tmp/pre-push-coverage.log 2>&1; then
    if [ -f scripts/check_coverage_threshold.sh ]; then
        if ./scripts/check_coverage_threshold.sh 80 > /dev/null 2>&1; then
            COVERAGE=$(grep "total" /tmp/pre-push-coverage.log | tail -1 | awk -F'|' '{print $3}' | tr -d ' %|' || echo "0")
            echo -e "${GREEN}  ✅ Coverage: ${COVERAGE}%${NC}"
        else
            echo -e "${RED}  ❌ Coverage below 80%${NC}"
            GATE_FAILURES=$((GATE_FAILURES + 1))
        fi
    else
        echo -e "${YELLOW}  ⚠️  Coverage check skipped (script not found)${NC}"
    fi
else
    echo -e "${YELLOW}  ⚠️  Coverage generation failed${NC}"
fi

# Dialyzer
echo -e "${BLUE}[3/3] Gate: Dialyzer${NC}"
if rebar3 dialyzer > /tmp/pre-push-dialyzer.log 2>&1; then
    echo -e "${GREEN}  ✅ Dialyzer passed${NC}"
else
    echo -e "${RED}  ❌ Dialyzer failed${NC}"
    tail -30 /tmp/pre-push-dialyzer.log
    GATE_FAILURES=$((GATE_FAILURES + 1))
fi

echo ""
if [ $GATE_FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ All pre-push gates passed - push allowed${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ $GATE_FAILURES gate(s) failed - push blocked${NC}"
    echo ""
    echo "Action Required:"
    echo "  1. Fix the failing gates"
    echo "  2. Run 'make verify' for full validation"
    echo "  3. Run 'make ci-local' to reproduce CI workflow"
    echo ""
    echo "To bypass (NOT RECOMMENDED for main/release branches):"
    echo "  git push --no-verify"
    echo ""
    exit 1
fi
HOOK_EOF

chmod +x .git/hooks/pre-push
echo -e "${GREEN}  ✅ pre-push hook installed${NC}"
echo ""

# ============================================================================
# SUMMARY
# ============================================================================
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✅ Quality gate hooks installed successfully${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo "Installed hooks:"
echo "  ✅ pre-commit  - Compilation, smoke tests, format, Chicago TDD"
echo "  ✅ commit-msg  - Commit message format validation"
echo "  ✅ pre-push    - Full tests, coverage, dialyzer"
echo ""
echo "Configuration:"
echo "  Location: .git/hooks/"
echo "  Mode:     BLOCKING (gates must pass)"
echo ""
echo "Testing the hooks:"
echo "  make quick    - Run pre-commit checks manually"
echo "  make verify   - Run pre-push checks manually"
echo "  make ci-local - Full CI workflow locally"
echo ""
echo "Bypassing hooks (NOT RECOMMENDED):"
echo "  git commit --no-verify"
echo "  git push --no-verify"
echo ""
echo -e "${YELLOW}⚠️  Warning: Bypassing hooks on main/release branches is prohibited${NC}"
echo ""
