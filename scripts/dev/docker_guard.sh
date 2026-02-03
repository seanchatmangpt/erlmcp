#!/usr/bin/env bash
# ==============================================================================
# DOCKER-ONLY CONSTITUTION: Script Guard
# ==============================================================================
# Source this file at the top of any script that must run inside Docker.
# If running on host, it will print ANDON message and exit 2.
#
# Usage:
#   #!/bin/bash
#   source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../dev/docker_guard.sh"
#   # ... rest of script only runs inside Docker
#
# Environment Variables:
#   ERLMCP_ALLOW_HOST=1  - Override to allow host execution (dangerous!)
# ==============================================================================

# Allow override for special cases (e.g., Docker build context)
if [[ "${ERLMCP_ALLOW_HOST:-}" == "1" ]]; then
    return 0 2>/dev/null || exit 0
fi

# Get the directory where this guard script lives
GUARD_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Detect environment using is_docker.sh
if [[ -x "${GUARD_SCRIPT_DIR}/is_docker.sh" ]]; then
    ENV_TYPE=$("${GUARD_SCRIPT_DIR}/is_docker.sh" 2>/dev/null || echo "host")
else
    # Fallback inline detection if is_docker.sh is missing
    if [[ -f "/.dockerenv" ]]; then
        ENV_TYPE="docker"
    elif [[ -r "/proc/1/cgroup" ]] && grep -Eq "(docker|kubepods|containerd|lxc)" /proc/1/cgroup 2>/dev/null; then
        ENV_TYPE="docker"
    elif [[ -f "/run/.containerenv" ]]; then
        ENV_TYPE="docker"
    else
        ENV_TYPE="host"
    fi
fi

# If running on host, refuse with ANDON message
if [[ "${ENV_TYPE}" == "host" ]]; then
    # Get the name of the script that sourced us
    CALLING_SCRIPT="${BASH_SOURCE[1]:-unknown}"
    CALLING_SCRIPT_NAME="$(basename "${CALLING_SCRIPT}")"

    # Print ANDON refusal message
    echo ""
    echo "========================================================================"
    echo ""
    echo "   ANDON: FORBIDDEN_HOST_EXECUTION"
    echo ""
    echo "========================================================================"
    echo ""
    echo "Refusal Code: FORBIDDEN_HOST_EXECUTION"
    echo "Constitution: DOCKER-ONLY CONSTITUTION"
    echo ""
    echo "Script: ${CALLING_SCRIPT}"
    echo "Detected: HOST (forbidden)"
    echo ""
    echo "CORRECT EXECUTION:"
    echo "   docker compose run --rm erlmcp-check ${CALLING_SCRIPT_NAME}"
    echo ""
    echo "Alternative services:"
    echo "   erlmcp-build  - for compile, dialyzer, xref"
    echo "   erlmcp-unit   - for eunit tests"
    echo "   erlmcp-ct     - for common test"
    echo "   erlmcp-check  - for validation, quality gates"
    echo "   erlmcp-bench  - for benchmarks"
    echo ""
    exit 2
fi

# If we get here, we're in Docker - allow script to continue
