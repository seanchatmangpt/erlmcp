#!/usr/bin/env bash
# =============================================================================
# RB-008: Minority Partition Detection and Auto-Shutdown
# =============================================================================
# This script implements automatic split-brain recovery by detecting
# minority partitions and shutting them down to prevent data divergence.
#
# This is part of the P0-007 split-brain recovery runbooks.
#
# Exit codes:
#   0 - Cluster healthy or in majority (continue operations)
#   1 - Minority partition detected (should shutdown)
#   2 - Error or unknown state
#
# Usage: ./scripts/runbooks/detect-and-shutdown-minority.sh
# Environment variables:
#   ERLMCP_QUORUM_SIZE - Total nodes in cluster (default: 3)
#   SHUTDOWN_DELAY - Seconds to wait before shutdown (default: 10)
#   FORCE_SHUTDOWN - Set to 'true' to actually shutdown (default: dry-run)
# =============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
QUORUM_SIZE=${ERLMCP_QUORUM_SIZE:-3}
SHUTDOWN_DELAY=${SHUTDOWN_DELAY:-10}
FORCE_SHUTDOWN=${FORCE_SHUTDOWN:-false}
CONTAINER_NAME=${ERLMCP_CONTAINER:-erlmcp}

echo "=== RB-008: Minority Partition Detection ==="
echo "Quorum size: $QUORUM_SIZE"
echo "Shutdown delay: ${SHUTDOWN_DELAY}s"
echo "Force shutdown: $FORCE_SHUTDOWN"
echo

# Calculate majority threshold (more than half)
MAJORITY_THRESHOLD=$(( (QUORUM_SIZE / 2) + 1 ))
echo "Majority threshold: $MAJORITY_THRESHOLD nodes"
echo

# Function to check cluster health via Docker
check_cluster_health() {
    local container="$1"

    # Try to get cluster health from running container
    if docker ps | grep -q "$container"; then
        # Check if net_admin is available for cluster operations
        if docker exec "$container" command -v netstat &> /dev/null; then
            # Count connected nodes via EPMD
            local reachable_nodes
            reachable_nodes=$(docker exec "$container" netstat -an 2>/dev/null | grep -c "4369.*ESTABLISHED" || echo "0")

            # Try Erlang distribution port connections
            local dist_ports
            dist_ports=$(docker exec "$container" netstat -an 2>/dev/null | grep -c ".*ESTABLISHED" | awk '{sum+=$1} END {print sum+0}' || echo "0")

            echo "$((reachable_nodes + dist_ports))"
        else
            echo "0"
        fi
    else
        echo "0"
    fi
}

# Function to get node count from Docker network
get_network_node_count() {
    local network="$1"

    if docker network inspect "$network" &> /dev/null 2>&1; then
        docker network inspect "$network" --format='{{len .Containers}}' 2>/dev/null || echo "1"
    else
        echo "1"
    fi
}

# Function to shutdown minority partition
shutdown_minority() {
    local reason="$1"

    echo -e "${RED}⚠️  MINORITY PARTITION DETECTED${NC}"
    echo "Reason: $reason"
    echo "Reachable nodes: $REACHABLE_NODES (need $MAJORITY_THRESHOLD for majority)"

    if [ "$FORCE_SHUTDOWN" = "true" ]; then
        echo
        echo "Initiating shutdown in ${SHUTDOWN_DELAY} seconds..."
        echo "Press Ctrl+C to abort"

        sleep "$SHUTDOWN_DELAY"

        echo "Shutting down minority partition..."
        if [ -f "docker-compose.yml" ]; then
            docker compose stop "$CONTAINER_NAME" 2>/dev/null || docker stop "$CONTAINER_NAME"
        else
            docker stop "$CONTAINER_NAME"
        fi

        echo -e "${GREEN}✅ Minority partition shut down successfully${NC}"
        return 1
    else
        echo
        echo -e "${YELLOW}[DRY RUN] Would shutdown after ${SHUTDOWN_DELAY}s delay${NC}"
        echo "Set FORCE_SHUTDOWN=true to execute actual shutdown"
        return 1
    fi
}

# Main detection logic
main() {
    # Method 1: Check container connectivity
    echo "Checking cluster connectivity..."
    REACHABLE_NODES=$(check_cluster_health "$CONTAINER_NAME")
    echo "Reachable nodes detected: $REACHABLE_NODES"

    # Method 2: Check Docker network
    echo
    echo "Checking Docker network..."
    local network_name
    network_name=$(docker inspect "$CONTAINER_NAME" --format='{{range $k, $v := .NetworkSettings.Networks}}{{$k}}{{end}}' 2>/dev/null || echo "")
    if [ -n "$network_name" ]; then
        local network_nodes
        network_nodes=$(get_network_node_count "$network_name")
        echo "Nodes on Docker network: $network_nodes"

        # Use the higher count
        if [ "$network_nodes" -gt "$REACHABLE_NODES" ]; then
            REACHABLE_NODES=$network_nodes
        fi
    fi

    echo
    echo "Final reachable node count: $REACHABLE_NODES"

    # Decision logic
    if [ "$REACHABLE_NODES" -ge "$MAJORITY_THRESHOLD" ]; then
        echo -e "${GREEN}✅ MAJORITY PARTITION: Continue operations${NC}"
        echo "This node is part of the majority partition ($REACHABLE_NODES >= $MAJORITY_THRESHOLD)"
        exit 0
    else
        echo
        shutdown_minority "Only $REACHABLE_NODES nodes reachable (need $MAJORITY_THRESHOLD)"
        exit 1
    fi
}

# Run main logic
main
