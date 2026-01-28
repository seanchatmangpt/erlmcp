#!/usr/bin/env bash
# Example usage scenarios for Auto-Fix System
# Demonstrates common workflows

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "Auto-Fix System - Example Usage Scenarios"
echo "=========================================="
echo ""

# Function to show example
show_example() {
    local title="$1"
    local description="$2"
    local command="$3"

    echo -e "${BLUE}Example: $title${NC}"
    echo "Description: $description"
    echo -e "${GREEN}Command:${NC}"
    echo "  $command"
    echo ""
}

# Example 1: Basic usage
show_example \
    "Basic Auto-Fix" \
    "Run full orchestration with default 5 iterations" \
    "$SCRIPT_DIR/orchestrator.sh orchestrate"

# Example 2: Quick check
show_example \
    "Quick Quality Check" \
    "Run all gates once with auto-fix" \
    "$SCRIPT_DIR/gate-failure-dispatcher.sh run-all"

# Example 3: Custom iterations
show_example \
    "Custom Iterations" \
    "Run orchestration with 10 iterations" \
    "$SCRIPT_DIR/orchestrator.sh orchestrate 10"

# Example 4: Single gate
show_example \
    "Single Gate Check" \
    "Monitor only compilation with auto-fix" \
    "$SCRIPT_DIR/gate-failure-dispatcher.sh monitor compilation 'rebar3 compile'"

# Example 5: Interactive mode
show_example \
    "Interactive Mode" \
    "Start interactive menu for manual control" \
    "$SCRIPT_DIR/orchestrator.sh interactive"

# Example 6: Status check
show_example \
    "Check Status" \
    "View current auto-fix state and attempt counts" \
    "$SCRIPT_DIR/gate-failure-dispatcher.sh status"

# Example 7: Validate fixes
show_example \
    "Validate All Fixes" \
    "Run clean build to verify all fixes applied correctly" \
    "$SCRIPT_DIR/orchestrator.sh validate"

# Example 8: Reset state
show_example \
    "Reset State" \
    "Clear all attempt counters and state" \
    "$SCRIPT_DIR/gate-failure-dispatcher.sh reset"

# Example 9: Development workflow
echo -e "${BLUE}Example: Development Workflow${NC}"
echo "Description: Typical development cycle with auto-fix"
echo -e "${GREEN}Commands:${NC}"
cat <<'EOF'
  # 1. Make changes
  vim src/my_module.erl

  # 2. Run auto-fix
  ./tools/auto-fix/orchestrator.sh orchestrate

  # 3. If successful, commit
  git add src/my_module.erl
  git commit -m "Add new feature"

  # 4. If escalated, review and fix manually
  cat logs/auto-fix/escalation-*.txt
  vim src/my_module.erl
  ./tools/auto-fix/gate-failure-dispatcher.sh reset
  make check
EOF
echo ""

# Example 10: CI/CD integration
echo -e "${BLUE}Example: CI/CD Integration${NC}"
echo "Description: Integrate into GitHub Actions workflow"
echo -e "${GREEN}YAML:${NC}"
cat <<'EOF'
  # .github/workflows/ci.yml
  - name: Run Auto-Fix Quality Gates
    run: |
      ./tools/auto-fix/orchestrator.sh orchestrate 3 || {
        echo "Quality gates failed after auto-fix"
        cat logs/auto-fix/escalation-*.txt
        exit 1
      }
EOF
echo ""

# Example 11: Pre-commit hook
echo -e "${BLUE}Example: Pre-Commit Hook${NC}"
echo "Description: Prevent commits with quality issues"
echo -e "${GREEN}Script (.git/hooks/pre-commit):${NC}"
cat <<'EOF'
  #!/bin/bash
  ./tools/auto-fix/gate-failure-dispatcher.sh run-all || {
    echo "❌ Quality gates failed"
    echo "Fix issues or use: git commit --no-verify"
    exit 1
  }
  echo "✅ All quality gates passed"
EOF
echo ""

# Example 12: Debugging failed auto-fix
echo -e "${BLUE}Example: Debug Failed Auto-Fix${NC}"
echo "Description: Investigate why auto-fix failed"
echo -e "${GREEN}Commands:${NC}"
cat <<'EOF'
  # 1. Check status
  ./tools/auto-fix/gate-failure-dispatcher.sh status

  # 2. View recent logs
  tail -50 logs/auto-fix/orchestrator.log

  # 3. View latest escalation
  cat $(ls -t logs/auto-fix/escalation-*.txt | head -1)

  # 4. View suggestions
  cat $(ls -t logs/auto-fix/*-suggestions-*.txt | head -1)

  # 5. Run gate manually to see full output
  rebar3 eunit --verbose
EOF
echo ""

# Example 13: Viewing all logs
show_example \
    "View All Logs" \
    "Check all auto-fix logs for debugging" \
    "tail -n 20 logs/auto-fix/*.log"

# Example 14: Clean slate
echo -e "${BLUE}Example: Clean Slate${NC}"
echo "Description: Reset everything and start fresh"
echo -e "${GREEN}Commands:${NC}"
cat <<'EOF'
  # Remove all logs and state
  rm -rf logs/auto-fix/*.json
  rm -rf logs/auto-fix/*-errors-*.txt

  # Reset state
  ./tools/auto-fix/gate-failure-dispatcher.sh reset

  # Run clean build
  rebar3 clean
  rebar3 compile
EOF
echo ""

# Example 15: Monitoring during development
echo -e "${BLUE}Example: Continuous Monitoring${NC}"
echo "Description: Run auto-fix on every file save (with entr/watchexec)"
echo -e "${GREEN}Commands:${NC}"
cat <<'EOF'
  # Using entr (install: brew install entr)
  ls src/*.erl test/*.erl | entr -c ./tools/auto-fix/gate-failure-dispatcher.sh run-all

  # Using watchexec (install: brew install watchexec)
  watchexec -w src -w test -- ./tools/auto-fix/gate-failure-dispatcher.sh run-all
EOF
echo ""

echo "=========================================="
echo "For more information, see:"
echo "  - tools/auto-fix/README.md"
echo "  - docs/auto-fix/AUTO_FIX_SYSTEM.md"
echo ""

# Interactive demo option
if [[ "${1:-}" == "--demo" ]]; then
    echo -e "${YELLOW}Running interactive demo...${NC}"
    echo ""

    read -p "Run status check? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        "$SCRIPT_DIR/gate-failure-dispatcher.sh" status
    fi

    echo ""
    read -p "Run quick quality check? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        "$SCRIPT_DIR/gate-failure-dispatcher.sh" run-all
    fi

    echo ""
    echo "Demo complete. Use './tools/auto-fix/orchestrator.sh interactive' for full interactive mode."
fi
