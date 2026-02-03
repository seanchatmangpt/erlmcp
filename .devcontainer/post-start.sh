#!/usr/bin/env bash
# ============================================================================
# post-start.sh - Dev Container Post-Start Script
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# This script runs EVERY TIME the dev container starts.
# It performs quick checks and displays status information.
#
# Tasks:
#   1. Verify Docker-Only Constitution
#   2. Check service connectivity
#   3. Display environment status
#   4. Start background services if needed
# ============================================================================

set -euo pipefail

echo ""
echo "┌──────────────────────────────────────────────┐"
echo "│       ERLMCP DEVELOPMENT ENVIRONMENT         │"
echo "└──────────────────────────────────────────────┘"
echo ""

cd /workspace

# ============================================================================
# Quick Constitution Check
# ============================================================================
if [[ -f "/.dockerenv" ]] || grep -Eq "(docker|kubepods|containerd)" /proc/1/cgroup 2>/dev/null; then
    echo "✓ Docker-Only Constitution: UPHELD"
else
    echo "✗ ANDON: NOT IN DOCKER CONTAINER"
    exit 2
fi

# ============================================================================
# Environment Information
# ============================================================================
echo ""
echo "Environment:"
OTP_VERSION=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "unknown")
ERTS_VERSION=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(version)]), halt(0).' 2>/dev/null || echo "unknown")
echo "  OTP:      $OTP_VERSION"
echo "  ERTS:     $ERTS_VERSION"
echo "  Rebar3:   $(rebar3 version 2>/dev/null | head -1 || echo 'unknown')"
echo "  Node:     $(hostname)"

# ============================================================================
# Service Connectivity
# ============================================================================
echo ""
echo "Services:"

# Check PostgreSQL
if command -v pg_isready >/dev/null 2>&1; then
    if pg_isready -h postgres -U erlmcp -d erlmcp_dev -q 2>/dev/null; then
        echo "  PostgreSQL: ✓ connected"
    else
        echo "  PostgreSQL: ⚠ not ready (may be starting)"
    fi
elif nc -z postgres 5432 2>/dev/null; then
    echo "  PostgreSQL: ✓ port open"
else
    echo "  PostgreSQL: ○ not configured"
fi

# Check Redis
if command -v redis-cli >/dev/null 2>&1; then
    if redis-cli -h redis ping >/dev/null 2>&1; then
        echo "  Redis:      ✓ connected"
    else
        echo "  Redis:      ⚠ not ready (may be starting)"
    fi
elif nc -z redis 6379 2>/dev/null; then
    echo "  Redis:      ✓ port open"
else
    echo "  Redis:      ○ not configured"
fi

# Check Docker socket
if docker info >/dev/null 2>&1; then
    echo "  Docker:     ✓ socket available"
else
    echo "  Docker:     ○ socket not mounted"
fi

# ============================================================================
# Project Status
# ============================================================================
echo ""
echo "Project:"

# Check if compiled
if [[ -d "_build/default/lib" ]]; then
    LIB_COUNT=$(find _build/default/lib -maxdepth 1 -type d 2>/dev/null | wc -l)
    echo "  Compiled:   ✓ ($((LIB_COUNT - 1)) apps)"
else
    echo "  Compiled:   ○ (run: rebar3 compile)"
fi

# Check dialyzer PLT
if [[ -f "_build/default/rebar3_*_plt" ]] || [[ -d "_build/default/*_plt" ]]; then
    echo "  Dialyzer:   ✓ PLT available"
else
    echo "  Dialyzer:   ○ PLT not built (run: rebar3 dialyzer)"
fi

# Git status
if [[ -d ".git" ]]; then
    BRANCH=$(git branch --show-current 2>/dev/null || echo "unknown")
    CHANGES=$(git status --porcelain 2>/dev/null | wc -l)
    if [[ $CHANGES -eq 0 ]]; then
        echo "  Git:        ✓ clean ($BRANCH)"
    else
        echo "  Git:        ⚠ $CHANGES changes ($BRANCH)"
    fi
fi

# ============================================================================
# Receipt System Status
# ============================================================================
echo ""
echo "Receipt System:"

if [[ -d "receipts" ]]; then
    RECEIPT_COUNT=$(find receipts -maxdepth 1 -type d 2>/dev/null | wc -l)
    if [[ $RECEIPT_COUNT -gt 1 ]]; then
        LATEST=$(ls -td receipts/*/ 2>/dev/null | head -1 | xargs basename 2>/dev/null || echo "none")
        echo "  Receipts:   ✓ $((RECEIPT_COUNT - 1)) bundles"
        echo "  Latest:     $LATEST"
    else
        echo "  Receipts:   ○ none generated"
    fi
else
    echo "  Receipts:   ○ directory not created"
fi

# ============================================================================
# Quick Help
# ============================================================================
echo ""
echo "┌──────────────────────────────────────────────┐"
echo "│  make doctor     - Run health checks         │"
echo "│  make quick      - Quick validation          │"
echo "│  make test       - Run all tests             │"
echo "│  make help       - Show all targets          │"
echo "└──────────────────────────────────────────────┘"
echo ""
