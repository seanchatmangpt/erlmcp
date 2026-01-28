#!/usr/bin/env bash
###############################################################################
# example-usage.sh - Quality Gates Visualization Examples
#
# Demonstrates all usage patterns for the quality gates system.
# Run this script to see the system in action.
#
# Usage:
#   ./example-usage.sh [demo-mode]
#
# Modes:
#   demo-mode    Simulate gate status changes with delays
###############################################################################

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)

echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}  erlmcp Quality Gates Visualization - Examples${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo ""

# Example 1: Run gates and show ASCII output
example_1() {
    echo -e "${BLUE}Example 1: Run Quality Gates + ASCII Visualization${NC}"
    echo "-----------------------------------------------------------"
    echo "Command: ./gate-status-json.sh 2>/dev/null | ./gate-status-ascii.sh --json-file=/dev/stdin"
    echo ""

    cd "$SCRIPT_DIR"
    ./gate-status-json.sh 2>/dev/null | tee /tmp/gates.json | ./gate-status-ascii.sh --json-file=/dev/stdin

    echo ""
    echo -e "${GREEN}✓ Gates executed and visualized${NC}"
    echo ""
}

# Example 2: JSON output
example_2() {
    echo -e "${BLUE}Example 2: JSON Output (for API integration)${NC}"
    echo "-----------------------------------------------------------"
    echo "Command: ./gate-status-json.sh 2>/dev/null | jq ."
    echo ""

    cd "$SCRIPT_DIR"
    ./gate-status-json.sh 2>/dev/null | jq '.' | head -30
    echo "..."

    echo ""
    echo -e "${GREEN}✓ JSON output ready for integration${NC}"
    echo ""
}

# Example 3: Compact ASCII mode
example_3() {
    echo -e "${BLUE}Example 3: Compact ASCII Mode (for CI/CD)${NC}"
    echo "-----------------------------------------------------------"
    echo "Command: ./gate-status-ascii.sh --compact --json-file=/tmp/gates.json"
    echo ""

    cd "$SCRIPT_DIR"
    ./gate-status-ascii.sh --compact --json-file=/tmp/gates.json

    echo ""
    echo -e "${GREEN}✓ Compact mode suitable for CI logs${NC}"
    echo ""
}

# Example 4: Specific gate
example_4() {
    echo -e "${BLUE}Example 4: Run Specific Gate${NC}"
    echo "-----------------------------------------------------------"
    echo "Command: ./gate-status-json.sh --gate=1"
    echo ""

    cd "$SCRIPT_DIR"
    ./gate-status-json.sh --gate=1 2>/dev/null | jq '.gates[0]'

    echo ""
    echo -e "${GREEN}✓ Single gate executed${NC}"
    echo ""
}

# Example 5: Watch mode
example_5() {
    echo -e "${BLUE}Example 5: Watch Mode (press Ctrl+C after 5 seconds)${NC}"
    echo "-----------------------------------------------------------"
    echo "Command: ./gate-status-ascii.sh --watch"
    echo ""
    echo -e "${YELLOW}Starting watch mode for 5 seconds...${NC}"

    cd "$SCRIPT_DIR"
    timeout 5 ./gate-status-ascii.sh --watch || true

    echo ""
    echo -e "${GREEN}✓ Watch mode demonstrated${NC}"
    echo ""
}

# Example 6: Server + Dashboard
example_6() {
    echo -e "${BLUE}Example 6: Start Dashboard Server${NC}"
    echo "-----------------------------------------------------------"
    echo "Command: ./start-dashboard.sh --no-browser"
    echo ""
    echo -e "${YELLOW}ℹ️  This would start the Cowboy server on port 9091${NC}"
    echo "   Then open: http://localhost:9091/"
    echo ""
    echo "   To actually start:"
    echo "   $ cd $SCRIPT_DIR"
    echo "   $ ./start-dashboard.sh"
    echo ""
    echo -e "${GREEN}✓ Server startup command shown${NC}"
    echo ""
}

# Example 7: CI/CD Integration
example_7() {
    echo -e "${BLUE}Example 7: CI/CD Integration Pattern${NC}"
    echo "-----------------------------------------------------------"
    echo ""

    cat <<'EOF'
# GitHub Actions Example
- name: Quality Gates
  run: |
    ./tools/visualize/gate-status-json.sh > gates.json
    ./tools/visualize/gate-status-ascii.sh --json-file=gates.json

- name: Upload Results
  uses: actions/upload-artifact@v3
  with:
    name: quality-gates
    path: gates.json

# Jenkins Pipeline Example
stage('Quality Gates') {
    steps {
        sh './tools/visualize/gate-status-json.sh > gates.json'
        sh './tools/visualize/gate-status-ascii.sh --json-file=gates.json'
        archiveArtifacts 'gates.json'
    }
}

# GitLab CI Example
quality-gates:
  script:
    - ./tools/visualize/gate-status-json.sh | tee gates.json
    - ./tools/visualize/gate-status-ascii.sh --json-file=gates.json
  artifacts:
    paths:
      - gates.json
    reports:
      junit: gates.json
EOF

    echo ""
    echo -e "${GREEN}✓ CI/CD patterns documented${NC}"
    echo ""
}

# Example 8: Pre-commit Hook
example_8() {
    echo -e "${BLUE}Example 8: Pre-commit Hook${NC}"
    echo "-----------------------------------------------------------"
    echo ""

    cat <<'EOF'
# .git/hooks/pre-commit
#!/bin/bash
echo "🔍 Running quality gates..."

./tools/visualize/gate-status-ascii.sh --compact

if [ $? -ne 0 ]; then
    echo "❌ Quality gates failed. Commit blocked."
    echo "   Fix issues and try again."
    exit 1
fi

echo "✅ Quality gates passed. Proceeding with commit."
EOF

    echo ""
    echo -e "${GREEN}✓ Pre-commit hook example shown${NC}"
    echo ""
}

# Demo mode - simulate gate updates
demo_mode() {
    echo -e "${MAGENTA}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${MAGENTA}  Demo Mode: Simulating Gate Status Changes${NC}"
    echo -e "${MAGENTA}═══════════════════════════════════════════════════════════${NC}"
    echo ""

    # Create demo status files
    for i in {1..5}; do
        echo -e "${CYAN}Demo Step $i/5${NC}"

        # Generate status with different patterns
        case $i in
            1)
                STATUS='{"gates":[{"id":1,"name":"Compilation","status":"running","timestamp":"2026-01-28T10:00:00Z","duration":"0s","details":"Compiling..."}],"overall":"running","pass_rate":0,"passed_count":0,"failed_count":0,"timestamp":"2026-01-28T10:00:00Z"}'
                ;;
            2)
                STATUS='{"gates":[{"id":1,"name":"Compilation","status":"pass","timestamp":"2026-01-28T10:00:02Z","duration":"2.1s","details":"0 errors"},{"id":2,"name":"Unit Tests","status":"running","timestamp":"2026-01-28T10:00:02Z","duration":"0s","details":"Running tests..."}],"overall":"running","pass_rate":12,"passed_count":1,"failed_count":0,"timestamp":"2026-01-28T10:00:02Z"}'
                ;;
            3)
                STATUS='{"gates":[{"id":1,"name":"Compilation","status":"pass","timestamp":"2026-01-28T10:00:02Z","duration":"2.1s","details":"0 errors"},{"id":2,"name":"Unit Tests","status":"fail","timestamp":"2026-01-28T10:00:05Z","duration":"3.2s","details":"3 tests failed"}],"overall":"fail","pass_rate":12,"passed_count":1,"failed_count":1,"timestamp":"2026-01-28T10:00:05Z"}'
                ;;
            4)
                STATUS='{"gates":[{"id":1,"name":"Compilation","status":"pass","timestamp":"2026-01-28T10:00:02Z","duration":"2.1s","details":"0 errors"},{"id":2,"name":"Unit Tests","status":"pass","timestamp":"2026-01-28T10:00:08Z","duration":"3.2s","details":"45 tests passed"},{"id":3,"name":"Integration Tests","status":"running","timestamp":"2026-01-28T10:00:08Z","duration":"0s","details":"Running suites..."}],"overall":"running","pass_rate":25,"passed_count":2,"failed_count":0,"timestamp":"2026-01-28T10:00:08Z"}'
                ;;
            5)
                STATUS='{"gates":[{"id":1,"name":"Compilation","status":"pass","timestamp":"2026-01-28T10:00:02Z","duration":"2.1s","details":"0 errors"},{"id":2,"name":"Unit Tests","status":"pass","timestamp":"2026-01-28T10:00:08Z","duration":"3.2s","details":"45 tests passed"},{"id":3,"name":"Integration Tests","status":"pass","timestamp":"2026-01-28T10:00:12Z","duration":"4.1s","details":"All suites passed"}],"overall":"pass","pass_rate":37,"passed_count":3,"failed_count":0,"timestamp":"2026-01-28T10:00:12Z"}'
                ;;
        esac

        echo "$STATUS" > /tmp/demo_gates_$i.json
        cd "$SCRIPT_DIR"
        ./gate-status-ascii.sh --json-file=/tmp/demo_gates_$i.json

        if [ $i -lt 5 ]; then
            sleep 2
        fi
    done

    echo ""
    echo -e "${GREEN}✓ Demo completed${NC}"
    echo ""
}

# Main menu
if [ "${1:-}" = "demo-mode" ]; then
    demo_mode
else
    echo "Running all examples..."
    echo ""

    example_1
    sleep 1

    example_2
    sleep 1

    example_3
    sleep 1

    example_4
    sleep 1

    example_5

    example_6

    example_7

    example_8

    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}✅ All examples completed!${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Try these commands:"
    echo ""
    echo "  ${BLUE}1.${NC} Run gates:        ./gate-status-json.sh"
    echo "  ${BLUE}2.${NC} ASCII view:       ./gate-status-ascii.sh"
    echo "  ${BLUE}3.${NC} Watch mode:       ./gate-status-ascii.sh --watch"
    echo "  ${BLUE}4.${NC} Start dashboard:  ./start-dashboard.sh"
    echo "  ${BLUE}5.${NC} Demo mode:        ./example-usage.sh demo-mode"
    echo ""
    echo "Documentation: ../../docs/visualization/QUALITY_GATES.md"
    echo ""
fi
