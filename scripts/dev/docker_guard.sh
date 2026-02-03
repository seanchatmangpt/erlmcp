#!/usr/bin/env bash
set -euo pipefail

# docker_guard.sh - ANDON: FORBIDDEN_HOST_EXECUTION guard
#
# This script MUST be sourced at the top of any script that should ONLY
# run inside Docker. If executed on host, it exits with code 2 (ANDON stop).
#
# Usage:
#   source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/docker_guard.sh"
#
# Refusal codes:
#   2 - FORBIDDEN_HOST_EXECUTION (Andon stop)

# Determine location of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IS_DOCKER_SCRIPT="${SCRIPT_DIR}/is_docker.sh"

# Check if we're inside Docker
if [[ ! -x "${IS_DOCKER_SCRIPT}" ]]; then
    chmod +x "${IS_DOCKER_SCRIPT}" 2>/dev/null || true
fi

if "${IS_DOCKER_SCRIPT}" >/dev/null 2>&1; then
    # Running inside Docker - allow execution
    return 0 2>/dev/null || exit 0
fi

# === ANDON: FORBIDDEN_HOST_EXECUTION ===
cat >&2 <<'EOF'
╔═══════════════════════════════════════════════════════════════════╗
║                                                                   ║
║   🚨 ANDON: FORBIDDEN_HOST_EXECUTION 🚨                          ║
║                                                                   ║
║   This script MUST run inside Docker. Host execution is FORBIDDEN.║
║                                                                   ║
╚═══════════════════════════════════════════════════════════════════╝

Refusal Code: FORBIDDEN_HOST_EXECUTION
Constitution: DOCKER-ONLY CONSTITUTION

REMEDIATION:
  docker compose run --rm erlmcp-check <script>

Or use the Makefile wrapper (which auto-routes to Docker):
  make <target>  # Automatically runs via Docker
EOF

exit 2
