#!/bin/bash
# ==============================================================================
# Dev Container Post-Start Script
# ==============================================================================
# Runs every time the container starts (including after rebuilds).
# Performs quick environment checks and displays status.
# ==============================================================================
set -euo pipefail

echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║              erlmcp Development Container                      ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# Display environment info
echo "Environment:"
echo "  OTP Version: $(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell)"
echo "  Rebar3:      $(rebar3 --version 2>/dev/null | head -1 || echo 'not found')"
echo "  Profile:     ${ERLMCP_PROFILE:-dev}"
echo ""

# Check if dependencies are up to date
if [ -f "/workspace/rebar.lock" ]; then
    echo "Dependencies: Locked (rebar.lock present)"
else
    echo "Dependencies: Run 'rebar3 get-deps' to fetch"
fi
echo ""

# Check Docker availability (for running quality gates)
if command -v docker &> /dev/null; then
    if docker info &> /dev/null; then
        echo "Docker: Available (Docker-in-Docker)"
        echo ""
        echo "DOCKER-ONLY CONSTITUTION reminder:"
        echo "  All quality gates MUST run inside Docker containers."
        echo "  Use: docker compose run --rm erlmcp-check make validate"
    else
        echo "Docker: Available but daemon not running"
    fi
else
    echo "Docker: Not available"
fi
echo ""

# Check services if available
echo "Services:"
if pg_isready -h postgres -U erlmcp &> /dev/null 2>&1; then
    echo "  PostgreSQL: Ready"
else
    echo "  PostgreSQL: Not ready (optional)"
fi

if redis-cli -h redis ping &> /dev/null 2>&1; then
    echo "  Redis: Ready"
else
    echo "  Redis: Not ready (optional)"
fi
echo ""

echo "Quick Start:"
echo "  make help      - Show all available targets"
echo "  make quick     - Fast compile + smoke tests"
echo "  make doctor    - Environment health check"
echo ""
