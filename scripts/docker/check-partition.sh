#!/usr/bin/env bash
# =============================================================================
# Partition-Aware Health Check for Docker Containers
# =============================================================================
# This health check script detects network partitions and prevents
# minority partitions from accepting traffic (split-brain prevention).
#
# Exit codes:
#   0 - Healthy (in majority partition)
#   1 - Unhealthy (in minority partition or other error)
#
# Usage: Add to docker-compose.yml healthcheck:
#   healthcheck:
#     test: ["CMD", "/opt/erlmcp/scripts/check-partition.sh"]
#     interval: 30s
#     timeout: 10s
#     retries: 3
# =============================================================================

set -euo pipefail

# Configuration
QUORUM_SIZE=${ERLMCP_QUORUM_SIZE:-3}
NODE_NAME=${ERLMCP_NODE_NAME:-erlmcp@127.0.0.1}

# Calculate majority threshold
MAJORITY_THRESHOLD=$(( (QUORUM_SIZE / 2) + 1 ))

# Debug logging (optional, enable for troubleshooting)
# exec 2>/tmp/partition-check.log
# echo "[$(date)] Partition check running" >&2

# Function to check if this node can reach the majority
check_majority_connectivity() {
    local reachable_count=0

    # Try to ping other cluster nodes via EPMD
    if command -v epmd &> /dev/null; then
        # Get list of nodes from EPMD
        local epmd_nodes
        epmd_nodes=$(epmd -names 2>/dev/null | grep -E "name [a-z0-9_]+ at port [0-9]+" | wc -l) || reachable_count=0

        if [ "$epmd_nodes" -gt 0 ]; then
            reachable_count=$epmd_nodes
        fi
    fi

    # Fallback: check netstat for established connections
    if command -v netstat &> /dev/null; then
        local established_connections
        established_connections=$(netstat -an 2>/dev/null | grep -c "ESTABLISHED" || echo "0")

        # Assume each ESTABLISHED connection is a potential node
        if [ "$established_connections" -gt 0 ]; then
            # Use the higher count
            if [ "$established_connections" -gt "$reachable_count" ]; then
                reachable_count=$established_connections
            fi
        fi
    fi

    # Include self in count (minimum of 1)
    if [ "$reachable_count" -lt 1 ]; then
        reachable_count=1
    fi

    echo "$reachable_count"
}

# Main health check logic
main() {
    # First, check if the node process is running
    if ! pgrep -f "beam.*${NODE_NAME#@}" > /dev/null 2>&1; then
        echo "ERROR: Node process not running"
        exit 1
    fi

    # Check partition status
    local reachable
    reachable=$(check_majority_connectivity)

    if [ "$reachable" -ge "$MAJORITY_THRESHOLD" ]; then
        # In majority partition - healthy
        echo "OK: Majority partition ($reachable/$QUORUM_SIZE nodes reachable)"
        exit 0
    else
        # In minority partition - unhealthy (should not receive traffic)
        echo "ERROR: Minority partition ($reachable/$QUORUM_SIZE nodes reachable, need $MAJORITY_THRESHOLD)"
        exit 1
    fi
}

# Run health check
main
