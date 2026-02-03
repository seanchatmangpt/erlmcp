#!/usr/bin/env bash
# ============================================================================
# GCP Simulation Stack - Logs Script
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Shows logs from GCP simulation services
#
# Usage:
#   ./logs.sh                    # All services
#   ./logs.sh postgres           # Specific service
#   ./logs.sh -f postgres redis  # Follow multiple services
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_DIR="$(dirname "$SCRIPT_DIR")"

cd "$COMPOSE_DIR"

# Default to follow mode if no arguments
if [[ $# -eq 0 ]]; then
    docker compose logs --tail=100 -f
else
    docker compose logs "$@"
fi
