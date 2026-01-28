#!/usr/bin/env bash
# setup-quality-gates.sh - One-command quality gate installation
# Usage: ./tools/setup-quality-gates.sh
#
# Installs:
# - Git hooks (pre-commit, commit-msg, pre-push)
# - Quality gate configuration
# - Validation scripts
# - Baseline metrics
#
# Exit codes:
# 0 - Success
# 1 - Missing prerequisites
# 2 - Installation failed
# 3 - Validation failed

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Project root (assumes script is in tools/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo -e "${BLUE}==> Installing erlmcp Quality Gates...${NC}"
echo

# Step 1: Check prerequisites
check_prerequisites() {
    echo -e "${BLUE}[1/6] Checking prerequisites...${NC}"

    local all_ok=true

    # Check Erlang
    if ! command -v erl &> /dev/null; then
        echo -e "${RED}✗ Erlang not found${NC}"
        echo "  Install from: https://www.erlang.org/downloads"
        all_ok=false
    else
        local erl_version=$(erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell 2>/dev/null || echo "unknown")
        echo -e "${GREEN}✓ Erlang/OTP ${erl_version}${NC}"
    fi

    # Check rebar3
    if ! command -v rebar3 &> /dev/null; then
        echo -e "${RED}✗ rebar3 not found${NC}"
        echo "  Install from: https://rebar3.org/docs/getting-started/"
        all_ok=false
    else
        local rebar_version=$(rebar3 version 2>/dev/null | head -1 || echo "unknown")
        echo -e "${GREEN}✓ ${rebar_version}${NC}"
    fi

    # Check Git
    if ! command -v git &> /dev/null; then
        echo -e "${RED}✗ Git not found${NC}"
        echo "  Install from: https://git-scm.com/downloads"
        all_ok=false
    else
        local git_version=$(git --version)
        echo -e "${GREEN}✓ ${git_version}${NC}"
    fi

    # Check if in Git repository
    if ! git rev-parse --git-dir &> /dev/null; then
        echo -e "${RED}✗ Not a Git repository${NC}"
        all_ok=false
    fi

    if [ "$all_ok" = false ]; then
        echo
        echo -e "${RED}Prerequisites check failed. Please install missing tools.${NC}"
        exit 1
    fi

    echo
}

# Step 2: Create validation scripts
create_validation_scripts() {
    echo -e "${BLUE}[2/6] Creating validation scripts...${NC}"

    mkdir -p tools/quality

    # validate-compilation.sh
    cat > tools/quality/validate-compilation.sh <<'EOF'
#!/usr/bin/env bash
# Validate compilation with zero errors
set -euo pipefail

echo "==> Compiling erlmcp..."

# Compile and capture output
if ! TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_compile.log; then
    echo "❌ COMPILATION FAILED"
    echo
    grep -A5 "Error:" /tmp/erlmcp_compile.log || true
    exit 1
fi

# Count warnings
WARNINGS=$(grep -c "Warning:" /tmp/erlmcp_compile.log || true)
if [ "$WARNINGS" -gt 0 ]; then
    echo "⚠️  $WARNINGS compilation warning(s) detected"
    grep "Warning:" /tmp/erlmcp_compile.log
    echo
fi

echo "✅ Compilation successful"
exit 0
EOF
    chmod +x tools/quality/validate-compilation.sh
    echo -e "${GREEN}✓ validate-compilation.sh${NC}"

    # validate-tests.sh
    cat > tools/quality/validate-tests.sh <<'EOF'
#!/usr/bin/env bash
# Validate tests pass with minimum coverage
set -euo pipefail

COVERAGE_THRESHOLD=${COVERAGE_THRESHOLD:-80}

echo "==> Running EUnit tests..."

# Run tests with coverage
if ! rebar3 eunit --cover 2>&1 | tee /tmp/erlmcp_test.log; then
    echo "❌ TESTS FAILED"
    echo
    grep -A10 "Failed:" /tmp/erlmcp_test.log || true
    exit 1
fi

# Check coverage
echo
echo "==> Checking test coverage..."
COVERAGE=$(rebar3 cover --verbose 2>&1 | grep "total" | awk '{print $3}' | sed 's/%//' || echo "0")

if [ -z "$COVERAGE" ]; then
    COVERAGE=0
fi

if (( $(echo "$COVERAGE < $COVERAGE_THRESHOLD" | bc -l) )); then
    echo "❌ Coverage $COVERAGE% below threshold $COVERAGE_THRESHOLD%"
    exit 1
fi

echo "✅ All tests passed"
echo "✅ Coverage: $COVERAGE% (threshold: $COVERAGE_THRESHOLD%)"
exit 0
EOF
    chmod +x tools/quality/validate-tests.sh
    echo -e "${GREEN}✓ validate-tests.sh${NC}"

    # validate-types.sh
    cat > tools/quality/validate-types.sh <<'EOF'
#!/usr/bin/env bash
# Validate Dialyzer type checking
set -euo pipefail

echo "==> Running Dialyzer type checker..."

# Build PLT if missing (first run only)
if [ ! -f "_build/default/rebar3_*_plt" ]; then
    echo "Building PLT (first run, may take 5-10 minutes)..."
fi

# Run Dialyzer
if ! rebar3 dialyzer 2>&1 | tee /tmp/erlmcp_dialyzer.log; then
    echo "❌ DIALYZER FOUND TYPE ERRORS"
    echo
    grep -A3 "Warning:" /tmp/erlmcp_dialyzer.log || true
    exit 1
fi

# Count warnings
WARNINGS=$(grep -c "Warning:" /tmp/erlmcp_dialyzer.log || true)
if [ "$WARNINGS" -gt 0 ]; then
    echo "⚠️  $WARNINGS Dialyzer warning(s) found"
    grep "Warning:" /tmp/erlmcp_dialyzer.log
    exit 1
fi

echo "✅ No type errors found"
exit 0
EOF
    chmod +x tools/quality/validate-types.sh
    echo -e "${GREEN}✓ validate-types.sh${NC}"

    # validate-format.sh
    cat > tools/quality/validate-format.sh <<'EOF'
#!/usr/bin/env bash
# Validate code formatting
set -euo pipefail

echo "==> Checking code format..."

# Check if any files need formatting
if ! rebar3 format --verify 2>&1 | tee /tmp/erlmcp_format.log; then
    echo "❌ CODE FORMAT VIOLATIONS"
    echo
    echo "Files needing formatting:"
    grep "would reformat" /tmp/erlmcp_format.log || true
    echo
    echo "Fix with: rebar3 format"
    exit 1
fi

echo "✅ Code properly formatted"
exit 0
EOF
    chmod +x tools/quality/validate-format.sh
    echo -e "${GREEN}✓ validate-format.sh${NC}"

    # validate-xref.sh
    cat > tools/quality/validate-xref.sh <<'EOF'
#!/usr/bin/env bash
# Validate cross-references (no undefined functions)
set -euo pipefail

echo "==> Running xref analysis..."

if ! rebar3 xref 2>&1 | tee /tmp/erlmcp_xref.log; then
    echo "❌ XREF FOUND ISSUES"
    echo
    grep "undefined" /tmp/erlmcp_xref.log || true
    exit 1
fi

echo "✅ No undefined functions"
exit 0
EOF
    chmod +x tools/quality/validate-xref.sh
    echo -e "${GREEN}✓ validate-xref.sh${NC}"

    echo
}

# Step 3: Install Git hooks
install_git_hooks() {
    echo -e "${BLUE}[3/6] Installing Git hooks...${NC}"

    # pre-commit hook
    cat > .git/hooks/pre-commit <<'EOF'
#!/usr/bin/env bash
# pre-commit hook - Run quality gates before allowing commit
set -euo pipefail

echo "==> Running pre-commit quality gates..."
echo

# Allow skipping for emergencies
if [ "${SKIP_QUALITY_GATES:-0}" = "1" ]; then
    echo "⚠️  WARNING: Quality gates skipped (SKIP_QUALITY_GATES=1)"
    echo "   Document reason in commit message!"
    exit 0
fi

# Track failures
FAILED=0

# Gate 1: Compilation
echo "[1/5] Compiling..."
if ./tools/quality/validate-compilation.sh; then
    echo
else
    FAILED=1
fi

# Gate 2: Dialyzer (skip if disabled)
if [ "${SKIP_DIALYZER:-0}" != "1" ]; then
    echo "[2/5] Type checking (Dialyzer)..."
    if ./tools/quality/validate-types.sh; then
        echo
    else
        FAILED=1
    fi
else
    echo "[2/5] Type checking (Dialyzer)... SKIPPED"
    echo
fi

# Gate 3: XRef
echo "[3/5] Cross-reference check..."
if ./tools/quality/validate-xref.sh; then
    echo
else
    FAILED=1
fi

# Gate 4: Format
echo "[4/5] Format validation..."
if ./tools/quality/validate-format.sh; then
    echo
else
    FAILED=1
fi

# Gate 5: Tests (skip if disabled)
if [ "${SKIP_TESTS:-0}" != "1" ]; then
    echo "[5/5] Running tests..."
    if ./tools/quality/validate-tests.sh; then
        echo
    else
        FAILED=1
    fi
else
    echo "[5/5] Running tests... SKIPPED"
    echo
fi

# Report results
if [ $FAILED -eq 1 ]; then
    echo "❌ Quality gates FAILED"
    echo
    echo "To fix:"
    echo "  1. Review errors above"
    echo "  2. Fix issues"
    echo "  3. Try commit again"
    echo
    echo "Emergency skip (NOT RECOMMENDED):"
    echo "  git commit --no-verify"
    echo "  Must document reason in commit message!"
    exit 1
else
    echo "🎉 All quality gates passed!"
    echo
fi

exit 0
EOF
    chmod +x .git/hooks/pre-commit
    echo -e "${GREEN}✓ pre-commit hook${NC}"

    # commit-msg hook
    cat > .git/hooks/commit-msg <<'EOF'
#!/usr/bin/env bash
# commit-msg hook - Validate commit message format
set -euo pipefail

COMMIT_MSG_FILE=$1
COMMIT_MSG=$(cat "$COMMIT_MSG_FILE")

# Check for conventional commit format: type: subject
if ! echo "$COMMIT_MSG" | grep -qE '^(feat|fix|docs|style|refactor|test|chore|perf|ci|build|revert)(\(.+\))?: .{1,}'; then
    echo "❌ Invalid commit message format"
    echo
    echo "Expected format:"
    echo "  type: subject"
    echo
    echo "Valid types:"
    echo "  feat, fix, docs, style, refactor, test, chore, perf, ci, build, revert"
    echo
    echo "Examples:"
    echo "  feat: Add new quality gate for coverage"
    echo "  fix: Correct Dialyzer warning in client"
    echo "  docs: Update quality enforcement guide"
    echo
    echo "Your message:"
    echo "  $COMMIT_MSG"
    exit 1
fi

exit 0
EOF
    chmod +x .git/hooks/commit-msg
    echo -e "${GREEN}✓ commit-msg hook${NC}"

    # pre-push hook
    cat > .git/hooks/pre-push <<'EOF'
#!/usr/bin/env bash
# pre-push hook - Final validation before push
set -euo pipefail

echo "==> Running pre-push validation..."
echo

# Run full test suite including CT
echo "[1/2] Running full test suite..."
if rebar3 do eunit, ct --cover; then
    echo "✅ All tests passed"
    echo
else
    echo "❌ Tests failed"
    exit 1
fi

# Check coverage
echo "[2/2] Validating coverage..."
if ./tools/quality/validate-tests.sh; then
    echo
else
    exit 1
fi

echo "🎉 Ready to push!"
exit 0
EOF
    chmod +x .git/hooks/pre-push
    echo -e "${GREEN}✓ pre-push hook${NC}"

    echo
}

# Step 4: Configure quality thresholds
configure_thresholds() {
    echo -e "${BLUE}[4/6] Configuring quality thresholds...${NC}"

    mkdir -p config

    cat > config/quality-gates.config <<'EOF'
%% quality-gates.config - Centralized quality thresholds
[
    {quality_gates, [
        {compilation, [
            {max_warnings, 0},
            {warnings_as_errors, true}
        ]},
        {dialyzer, [
            {warnings, [
                error_handling,
                race_conditions,
                unmatched_returns,
                unknown
            ]},
            {plt_apps, [kernel, stdlib, crypto, ssl]}
        ]},
        {coverage, [
            {minimum_percentage, 80},
            {exclude_modules, []}
        ]},
        {tests, [
            {timeout_ms, 300000},  % 5 minutes
            {fail_fast, false}     % Run all tests
        ]},
        {format, [
            {line_length, 100},
            {verify_on_commit, true}
        ]}
    ]}
].
EOF
    echo -e "${GREEN}✓ quality-gates.config${NC}"
    echo
}

# Step 5: Record baseline metrics
record_baseline() {
    echo -e "${BLUE}[5/6] Recording baseline metrics...${NC}"

    mkdir -p metrics/baseline

    # Compile to get baseline time
    echo -n "  Measuring compilation time... "
    COMPILE_START=$(date +%s%3N)
    TERM=dumb rebar3 compile > /dev/null 2>&1 || true
    COMPILE_END=$(date +%s%3N)
    COMPILE_TIME=$((COMPILE_END - COMPILE_START))
    echo "${COMPILE_TIME}ms"

    # Count modules
    MODULE_COUNT=$(find src -name "*.erl" | wc -l | tr -d ' ')
    echo "  Module count: $MODULE_COUNT"

    # Record baseline
    cat > metrics/baseline/baseline.json <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "compilation_time_ms": $COMPILE_TIME,
  "module_count": $MODULE_COUNT,
  "git_commit": "$(git rev-parse HEAD 2>/dev/null || echo 'unknown')"
}
EOF
    echo -e "${GREEN}✓ Baseline recorded${NC}"
    echo
}

# Step 6: Validate installation
validate_installation() {
    echo -e "${BLUE}[6/6] Validating installation...${NC}"

    local all_ok=true

    # Check hook permissions
    if [ -x .git/hooks/pre-commit ]; then
        echo -e "${GREEN}✓ pre-commit hook executable${NC}"
    else
        echo -e "${RED}✗ pre-commit hook not executable${NC}"
        all_ok=false
    fi

    if [ -x .git/hooks/commit-msg ]; then
        echo -e "${GREEN}✓ commit-msg hook executable${NC}"
    else
        echo -e "${RED}✗ commit-msg hook not executable${NC}"
        all_ok=false
    fi

    if [ -x .git/hooks/pre-push ]; then
        echo -e "${GREEN}✓ pre-push hook executable${NC}"
    else
        echo -e "${RED}✗ pre-push hook not executable${NC}"
        all_ok=false
    fi

    # Check validation scripts
    if [ -x tools/quality/validate-compilation.sh ]; then
        echo -e "${GREEN}✓ Validation scripts executable${NC}"
    else
        echo -e "${RED}✗ Validation scripts not executable${NC}"
        all_ok=false
    fi

    # Check config
    if [ -f config/quality-gates.config ]; then
        echo -e "${GREEN}✓ Configuration file present${NC}"
    else
        echo -e "${RED}✗ Configuration file missing${NC}"
        all_ok=false
    fi

    if [ "$all_ok" = false ]; then
        echo
        echo -e "${RED}Installation validation failed${NC}"
        exit 3
    fi

    echo
}

# Step 7: Run first quality check
run_first_check() {
    echo -e "${BLUE}==> Running first quality check...${NC}"
    echo

    # Quick validation (skip Dialyzer for speed)
    export SKIP_DIALYZER=1

    if .git/hooks/pre-commit; then
        echo
        echo -e "${GREEN}🎉 Quality gates installed successfully!${NC}"
    else
        echo
        echo -e "${YELLOW}⚠️  Installation complete but quality check failed${NC}"
        echo -e "${YELLOW}   This is normal if you have uncommitted work-in-progress${NC}"
        echo -e "${YELLOW}   Fix issues and try: git commit${NC}"
    fi
}

# Main installation flow
main() {
    check_prerequisites
    create_validation_scripts
    install_git_hooks
    configure_thresholds
    record_baseline
    validate_installation
    run_first_check

    # Print next steps
    echo
    echo -e "${BLUE}Next steps:${NC}"
    echo "  1. Make a small change to any .erl file"
    echo "  2. Try to commit it: git commit -m 'test: Validate quality gates'"
    echo "  3. Watch quality gates in action"
    echo
    echo "Documentation:"
    echo "  Quick Start: docs/quality-enforcement/QUICK_START.md"
    echo "  Full Guide:  docs/quality-enforcement/IMPLEMENTATION_PLAN.md"
    echo
    echo "Emergency skip (use sparingly):"
    echo "  git commit --no-verify -m 'hotfix: Critical fix'"
    echo
}

# Run installation
main
