#!/bin/bash
set -euo pipefail

# erlmcp Partition Recovery Script
# Handles split-brain detection and recovery
#
# Usage:
#   ./scripts/recovery.sh detect              - Check for split-brain
#   ./scripts/recovery.sh recover [NODE]      - Recover with specified primary
#   ./scripts/recovery.sh force-restart       - Force cluster restart
#
# Environment:
#   PARTITION_THRESHOLD_SECONDS - Detection threshold (default: 30)
#   RECOVERY_TIMEOUT_SECONDS    - Recovery timeout (default: 120)

# Configuration
PARTITION_THRESHOLD_SECONDS="${PARTITION_THRESHOLD_SECONDS:-30}"
RECOVERY_TIMEOUT_SECONDS="${RECOVERY_TIMEOUT_SECONDS:-120}"
COMPOSE_FILE="${COMPOSE_FILE:-docker-compose.yml}"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Check docker compose availability
check_docker_compose() {
    if ! command -v docker &> /dev/null; then
        log_error "docker not found in PATH"
        exit 1
    fi

    if ! docker compose version &> /dev/null && ! docker-compose version &> /dev/null; then
        log_error "docker compose or docker-compose not available"
        exit 1
    fi

    if [[ ! -f "$COMPOSE_FILE" ]]; then
        log_error "Compose file not found: $COMPOSE_FILE"
        exit 1
    fi
}

# Get docker compose command
get_compose_cmd() {
    if docker compose version &> /dev/null; then
        echo "docker compose -f $COMPOSE_FILE"
    else
        echo "docker-compose -f $COMPOSE_FILE"
    fi
}

# Detect split-brain via docker exec
detect_split_brain() {
    log_info "Checking for split-brain conditions..."

    local compose_cmd
    compose_cmd=$(get_compose_cmd)

    # Get list of erlmcp services
    local nodes
    nodes=$($compose_cmd ps --services --filter "name=erlmcp" | grep -v node || true)

    if [[ -z "$nodes" ]]; then
        log_error "No erlmcp nodes found running"
        return 1
    fi

    log_info "Found erlmcp nodes:"
    echo "$nodes"

    # Check each node for cluster membership
    log_info "Querying cluster membership from each node..."

    local member_sets=()
    local node_name

    while IFS= read -r node; do
        [[ -z "$node" ]] && continue

        log_info "Querying node: $node"

        local members
        members=$($compose_cmd exec -T "$node" \
            /opt/erlmcp/bin/erlmcp eval \
            "erlmcp_cluster_membership:get_members()." 2>/dev/null || echo "error")

        if [[ "$members" == "error" ]]; then
            log_warn "Could not query node: $node"
        else
            log_info "Node $node sees members: $members"
            member_sets+=("$members")
        fi
    done <<< "$nodes"

    # Check if member sets differ (indicating split-brain)
    if [[ ${#member_sets[@]} -gt 1 ]]; then
        local first_set="${member_sets[0]}"
        local split_brain=false

        for member_set in "${member_sets[@]:1}"; do
            if [[ "$first_set" != "$member_set" ]]; then
                split_brain=true
                break
            fi
        done

        if [[ "$split_brain" == "true" ]]; then
            log_error "SPLIT-BRAIN DETECTED: Nodes have different membership views"
            log_error "Member sets differ across nodes"
            return 2
        fi
    fi

    # Check for partition via last seen timestamps
    log_info "Checking partition timestamps..."

    local partition_detected=false
    while IFS= read -r node; do
        [[ -z "$node" ]] && continue

        local last_seen
        last_seen=$($compose_cmd exec -T "$node" \
            /opt/erlmcp/bin/erlmcp eval \
            "erlmcp_cluster_heartbeat:get_last_seen_timestamp()." 2>/dev/null || echo "0")

        local current_time
        current_time=$(date +%s)

        local time_diff
        time_diff=$((current_time - ${last_seen%\.*}))

        if [[ $time_diff -gt $PARTITION_THRESHOLD_SECONDS ]]; then
            log_error "PARTITION DETECTED on $node: Last seen ${time_diff}s ago"
            partition_detected=true
        fi
    done <<< "$nodes"

    if [[ "$partition_detected" == "true" ]]; then
        return 3
    fi

    log_info "No split-brain detected. Cluster appears healthy."
    return 0
}

# Validate a node can be used as recovery primary
validate_primary_node() {
    local primary_node="$1"

    log_info "Validating primary node: $primary_node"

    local compose_cmd
    compose_cmd=$(get_compose_cmd)

    # Check if node is running
    if ! $compose_cmd ps "$primary_node" | grep -q "Up"; then
        log_error "Primary node not running: $primary_node"
        return 1
    fi

    # Check if node responds to Erlang ping
    local ping_result
    ping_result=$($compose_cmd exec -T "$primary_node" \
        /opt/erlmcp/bin/erlmcp eval \
        "net_adm:ping(list_to_atom(\"$primary_node\"))." 2>/dev/null || echo "error")

    if [[ "$ping_result" == "error" ]] || [[ "$ping_result" == "pang" ]]; then
        log_error "Primary node not responding: $primary_node"
        return 1
    fi

    log_info "Primary node validated: $primary_node"
    return 0
}

# Recovery procedure
recover_from_split_brain() {
    local primary_node="${1:-erlmcp@erlmcp}"
    local service_name="${2:-erlmcp}"

    log_warn "Initiating split-brain recovery..."
    log_warn "Primary node: $primary_node"
    log_warn "Service: $service_name"

    local compose_cmd
    compose_cmd=$(get_compose_cmd)

    # Step 1: Scale down to single node
    log_info "Step 1: Scaling down to single node..."
    $compose_cmd scale "$service_name=1" || {
        log_error "Failed to scale down"
        return 1
    }

    # Wait for single node to stabilize
    log_info "Waiting for single node to stabilize..."
    sleep 5

    # Step 2: Force reset cluster state on primary
    log_info "Step 2: Resetting cluster state..."
    $compose_cmd exec -T "$service_name" \
        /opt/erlmcp/bin/erlmcp eval \
        "erlmcp_cluster_coordinator:reset_cluster_state()." 2>/dev/null || {
        log_warn "Cluster reset failed or module not available (non-fatal)"
    }

    # Clear mnesia tables if needed
    $compose_cmd exec -T "$service_name" \
        /opt/erlmcp/bin/erlmcp eval \
        "mnesia:clear_table(cluster_membership)." 2>/dev/null || true

    # Step 3: Set primary as cluster leader
    log_info "Step 3: Setting primary as cluster leader..."
    $compose_cmd exec -T "$service_name" \
        /opt/erlmcp/bin/erlmcp eval \
        "erlmcp_cluster_coordinator:become_leader('$primary_node')." 2>/dev/null || {
        log_warn "Leader election failed or module not available (non-fatal)"
    }

    # Step 4: Restart other nodes
    log_info "Step 4: Restarting cluster nodes..."
    local original_scale
    original_scale=$(grep -A1 "$service_name:" "$COMPOSE_FILE" | grep "replicate:" | awk '{print $2}' || echo "3")

    if [[ -z "$original_scale" ]] || [[ "$original_scale" == "1" ]]; then
        original_scale=3
    fi

    $compose_cmd up -d --scale "$service_name=$original_scale" || {
        log_error "Failed to scale up"
        return 1
    }

    # Step 5: Wait for cluster convergence
    log_info "Step 5: Waiting for cluster convergence..."
    local max_wait=$RECOVERY_TIMEOUT_SECONDS
    local elapsed=0

    while [[ $elapsed -lt $max_wait ]]; do
        if detect_split_brain >/dev/null 2>&1; then
            log_info "Cluster converged successfully"
            return 0
        fi

        sleep 5
        elapsed=$((elapsed + 5))
        echo -n "."
    done

    echo ""
    log_error "Cluster did not converge within ${max_wait}s"
    return 1
}

# Force restart entire cluster
force_restart_cluster() {
    log_warn "Force restarting entire cluster..."

    local compose_cmd
    compose_cmd=$(get_compose_cmd)

    # Graceful restart with timeout
    log_info "Stopping cluster..."
    $compose_cmd down || {
        log_error "Failed to stop cluster"
        return 1
    }

    sleep 5

    log_info "Starting cluster..."
    $compose_cmd up -d || {
        log_error "Failed to start cluster"
        return 1
    }

    log_info "Waiting for cluster to be ready..."
    sleep 10

    # Verify health
    if detect_split_brain >/dev/null 2>&1; then
        log_info "Cluster restarted successfully"
        return 0
    else
        log_error "Cluster health check failed after restart"
        return 1
    fi
}

# Display cluster status
display_status() {
    log_info "Cluster Status:"
    echo ""

    local compose_cmd
    compose_cmd=$(get_compose_cmd)

    # Show running containers
    echo "=== Containers ==="
    $compose_cmd ps
    echo ""

    # Show node membership
    echo "=== Cluster Membership ==="
    local nodes
    nodes=$($compose_cmd ps --services --filter "name=erlmcp" | grep -v node || true)

    while IFS= read -r node; do
        [[ -z "$node" ]] && continue

        local members
        members=$($compose_cmd exec -T "$node" \
            /opt/erlmcp/bin/erlmcp eval \
            "erlmcp_cluster_membership:get_members()." 2>/dev/null || echo "unavailable")

        echo "  $node: $members"
    done <<< "$nodes"
    echo ""
}

# Main
main() {
    local command="${1:-detect}"

    check_docker_compose

    echo "=== erlmcp Partition Recovery ==="
    echo "Compose file: $COMPOSE_FILE"
    echo "Partition threshold: ${PARTITION_THRESHOLD_SECONDS}s"
    echo "Recovery timeout: ${RECOVERY_TIMEOUT_SECONDS}s"
    echo ""

    case "$command" in
        detect)
            detect_split_brain
            exit $?
            ;;
        recover)
            if [[ $# -lt 1 ]]; then
                log_error "Usage: $0 recover [PRIMARY_NODE] [SERVICE_NAME]"
                exit 1
            fi
            recover_from_split_brain "${2:-erlmcp@erlmcp}" "${3:-erlmcp}"
            exit $?
            ;;
        force-restart)
            force_restart_cluster
            exit $?
            ;;
        status)
            display_status
            exit 0
            ;;
        health)
            # Continuous health monitoring
            log_info "Starting health monitoring (Ctrl-C to stop)..."
            while true; do
                clear
                display_status
                detect_split_brain || true
                sleep 5
            done
            ;;
        *)
            echo "Usage: $0 {detect|recover|force-restart|status|health}"
            echo ""
            echo "Commands:"
            echo "  detect          - Check for split-brain conditions"
            echo "  recover [NODE]  - Recover from split-brain using specified primary node"
            echo "  force-restart   - Force restart entire cluster"
            echo "  status          - Display cluster status and membership"
            echo "  health          - Continuous health monitoring"
            echo ""
            echo "Environment:"
            echo "  PARTITION_THRESHOLD_SECONDS - Detection threshold (default: 30)"
            echo "  RECOVERY_TIMEOUT_SECONDS    - Recovery timeout (default: 120)"
            echo "  COMPOSE_FILE               - Docker Compose file (default: docker-compose.yml)"
            exit 1
            ;;
    esac
}

main "$@"
