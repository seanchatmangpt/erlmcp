#!/bin/bash
#
# Automated Failover Script for erlmcp v3
# Automates detection and execution of failover procedures
#
# Usage:
#   ./automated-failover.sh monitor              - Continuous monitoring mode
#   ./automated-failover.sh check                - Single health check
#   ./automated-failover.sh failover [region]    - Execute failover
#   ./automated-failover.sh validate             - Validate failover state
#   ./automated-failover.sh rollback             - Rollback to primary
#

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="/var/log/erlmcp/automated-failover.log"
STATE_FILE="/var/lib/erlmcp/failover-state.json"
CHECK_INTERVAL=30
FAIL_THRESHOLD=3
RECOVERY_TIMEOUT=600

# Regions
PRIMARY_REGION="${PRIMARY_REGION:-us-east-1}"
SECONDARY_REGION="${SECONDARY_REGION:-eu-central-1}"
BACKUP_REGION="${BACKUP_REGION:-ap-southeast-1}"

# Health check endpoints
HEALTH_ENDPOINT="${HEALTH_ENDPOINT:-https://erlmcp.example.com/health}"
METRICS_ENDPOINT="${METRICS_ENDPOINT:-https://erlmcp.example.com/metrics}"

# Notifications
SLACK_WEBHOOK="${SLACK_WEBHOOK:-}"
PAGERDUTY_KEY="${PAGERDUTY_KEY:-}"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Logging
log() {
    local level=$1
    shift
    local msg="[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $*"
    echo -e "$msg" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "${GREEN}$*${NC}"; }
log_warn() { log "WARN" "${YELLOW}$*${NC}"; }
log_error() { log "ERROR" "${RED}$*${NC}"; }
log_debug() { log "DEBUG" "${BLUE}$*${NC}"; }

# Notification functions
notify_slack() {
    local message=$1
    local color=${2:-good}

    if [[ -n "$SLACK_WEBHOOK" ]]; then
        curl -s -X POST "$SLACK_WEBHOOK" \
            -H 'Content-Type: application/json' \
            -d "{
                \"attachments\": [{
                    \"color\": \"$color\",
                    \"text\": \"$message\",
                    \"ts\": $(date +%s)
                }]
            }" > /dev/null 2>&1 || true
    fi
}

notify_pagerduty() {
    local event_action=$1  # trigger, acknowledge, resolve
    local dedup_key=$2
    local description=$3

    if [[ -n "$PAGERDUTY_KEY" ]]; then
        curl -s -X POST "https://events.pagerduty.com/v2/enqueue" \
            -H 'Content-Type: application/json' \
            -H "Accept: application/vnd.pagerduty+json;version=2" \
            -d "{
                \"routing_key\": \"$PAGERDUTY_KEY\",
                \"event_action\": \"$event_action\",
                \"dedup_key\": \"$dedup_key\",
                \"payload\": {
                    \"summary\": \"$description\",
                    \"source\": \"erlmcp-failover\",
                    \"severity\": \"critical\",
                    \"timestamp\": \"$(date -Iseconds)\"
                }
            }" > /dev/null 2>&1 || true
    fi
}

# State management
load_state() {
    if [[ -f "$STATE_FILE" ]]; then
        cat "$STATE_FILE"
    else
        echo '{"active_region":"'"$PRIMARY_REGION"'","failures":0,"last_check":"null"}'
    fi
}

save_state() {
    local state=$1
    mkdir -p "$(dirname "$STATE_FILE")"
    echo "$state" > "$STATE_FILE"
}

update_state() {
    local active_region=$1
    local failures=$2

    local state=$(load_state)
    local updated=$(echo "$state" | jq --arg r "$active_region" --arg f "$failures" \
        '.active_region = $r | .failures = ($f | tonumber) | .last_check = "'"$(date -Iseconds)"'"')

    save_state "$updated"
    echo "$updated"
}

# Health check functions
check_primary_health() {
    log_debug "Checking primary region health..."

    # Check HTTP endpoint
    if ! curl -f -s --max-time 10 "$HEALTH_ENDPOINT" > /dev/null 2>&1; then
        log_warn "Primary health check failed: HTTP endpoint unreachable"
        return 1
    fi

    # Check metrics endpoint
    if ! curl -f -s --max-time 10 "$METRICS_ENDPOINT" > /dev/null 2>&1; then
        log_warn "Primary health check failed: Metrics endpoint unreachable"
        return 1
    fi

    # Check for specific health indicators
    local health
    health=$(curl -s --max-time 10 "$HEALTH_ENDPOINT")

    if ! echo "$health" | jq -e '.status == "healthy"' > /dev/null 2>&1; then
        log_warn "Primary health check failed: Status not healthy"
        return 1
    fi

    # Check database connectivity
    if ! echo "$health" | jq -e '.components.database.status == "up"' > /dev/null 2>&1; then
        log_warn "Primary health check failed: Database down"
        return 1
    fi

    # Check session replication
    if ! echo "$health" | jq -e '.components.sessions.replication_lag < 5' > /dev/null 2>&1; then
        log_warn "Primary health check failed: Session replication lag too high"
        return 1
    fi

    log_debug "Primary region healthy"
    return 0
}

check_secondary_health() {
    local region=$1

    log_debug "Checking $region health..."

    # Build region-specific endpoint
    local region_endpoint="${HEALTH_ENDPOINT/erlmcp.example.com/erlmcp-${region}.example.com}"

    # Check HTTP endpoint
    if ! curl -f -s --max-time 10 "$region_endpoint" > /dev/null 2>&1; then
        log_warn "$region health check failed: Endpoint unreachable"
        return 1
    fi

    log_debug "$region healthy"
    return 0
}

# Failover execution
execute_failover() {
    local target_region=$1

    log_warn "Starting failover to $target_region..."
    notify_slack "🚨 Starting failover to $target_region" "warning"
    notify_pagerduty "trigger" "erlmcp-failover" "Failover to $target_region initiated"

    # Update state
    update_state "$target_region" 0

    # Step 1: Update DNS
    log_info "Step 1: Updating DNS records..."
    update_dns "$target_region"

    # Step 2: Activate target region
    log_info "Step 2: Activating $target_region..."
    activate_region "$target_region"

    # Step 3: Drain primary region
    log_info "Step 3: Draining primary region..."
    drain_region "$PRIMARY_REGION"

    # Step 4: Verify failover
    log_info "Step 4: Verifying failover..."
    if ! verify_failover "$target_region"; then
        log_error "Failover verification failed"
        notify_slack "❌ Failover to $target_region FAILED" "danger"
        notify_pagerduty "trigger" "erlmcp-failover-failed" "Failover verification failed"
        return 1
    fi

    log_info "Failover to $target_region completed successfully"
    notify_slack "✅ Failover to $target_region completed successfully" "good"
    notify_pagerduty "resolve" "erlmcp-failover" "Failover to $target_region completed"

    return 0
}

update_dns() {
    local target_region=$1

    # Get target IPs
    local target_ips
    target_ips=$(get_region_ips "$target_region")

    # Update Route53
    local zone_id="${HOSTED_ZONE_ID:-Z1D633PEXAMPLE}"

    aws route53 change-resource-record-sets \
        --hosted-zone-id "$zone_id" \
        --change-batch "{
            \"Comment\": \"Automated failover to $target_region\",
            \"Changes\": [{
                \"Action\": \"UPSERT\",
                \"ResourceRecordSet\": {
                    \"Name\": \"${DNS_RECORD_NAME:-erlmcp.example.com}\",
                    \"Type\": \"A\",
                    \"SetIdentifier\": \"primary\",
                    \"Region\": \"$target_region\",
                    \"TTL\": 60,
                    \"ResourceRecords\": $([ "$(echo "$target_ips" | wc -l)" -eq 1 ] && echo "[{\"Value\": \"$target_ips\"}]" || echo "$(echo "$target_ips" | jq -r '.[] | {Value: .}' | jq -s '.')")
                }
            }]
        }" > /dev/null 2>&1 || log_warn "DNS update failed (non-fatal)"
}

activate_region() {
    local region=$1

    # Scale up services in target region
    if [[ "$region" == "$SECONDARY_REGION" ]]; then
        aws ecs update-service \
            --cluster "erlmcp-${region}" \
            --service erlmcp \
            --desired-count 3 \
            --region "$region" > /dev/null 2>&1 || log_warn "ECS scale up failed (non-fatal)"
    fi

    # Wait for services to be ready
    local max_wait=60
    local elapsed=0

    while [[ $elapsed -lt $max_wait ]]; do
        if check_secondary_health "$region"; then
            log_info "$region services are ready"
            return 0
        fi
        sleep 10
        elapsed=$((elapsed + 10))
    done

    log_warn "$region services not ready after ${max_wait}s (continuing anyway)"
}

drain_region() {
    local region=$1

    # Graceful shutdown of primary region
    log_info "Initiating graceful shutdown of $region..."

    # Scale down to zero
    aws ecs update-service \
        --cluster "erlmcp-${region}" \
        --service erlmcp \
        --desired-count 0 \
        --region "$region" > /dev/null 2>&1 || log_warn "ECS scale down failed (non-fatal)"
}

verify_failover() {
    local target_region=$1
    local max_wait=$RECOVERY_TIMEOUT
    local elapsed=0

    while [[ $elapsed -lt $max_wait ]]; do
        if check_primary_health; then
            log_info "Failover verified: System healthy"
            return 0
        fi

        log_debug "Waiting for failover verification... (${elapsed}/${max_wait}s)"
        sleep 10
        elapsed=$((elapsed + 10))
    done

    return 1
}

rollback_to_primary() {
    log_warn "Starting rollback to primary region..."
    notify_slack "🔄 Starting rollback to primary region" "warning"

    # Execute failover to primary
    if execute_failover "$PRIMARY_REGION"; then
        log_info "Rollback completed successfully"
        return 0
    else
        log_error "Rollback failed"
        return 1
    fi
}

get_region_ips() {
    local region=$1

    # Get IPs from ECS tasks or EC2 instances
    aws ecs describe-tasks \
        --cluster "erlmcp-${region}" \
        --tasks $(aws ecs list-tasks --cluster "erlmcp-${region}" --service-name erlmcp --region "$region" --query 'taskArns' --output text) \
        --region "$region" \
        --query 'tasks[].attachments[].details[?name==`privateIPv4Address`].value' \
        --output text 2>/dev/null || echo ""
}

# Monitoring mode
monitor_mode() {
    log_info "Starting continuous monitoring (interval: ${CHECK_INTERVAL}s)"

    local consecutive_failures=0

    while true; do
        # Get current state
        local state
        state=$(load_state)

        local active_region
        active_region=$(echo "$state" | jq -r '.active_region')

        local failures
        failures=$(echo "$state" | jq -r '.failures')

        # Perform health check
        if check_primary_health; then
            consecutive_failures=0

            # If we're in failover state, consider rollback
            if [[ "$active_region" != "$PRIMARY_REGION" ]]; then
                log_info "Primary region recovered, considering rollback..."

                # Only auto-rollback if configured
                if [[ "${AUTO_ROLLBACK:-false}" == "true" ]]; then
                    rollback_to_primary
                else
                    log_info "Auto-rollback disabled, manual intervention required"
                    notify_slack "ℹ️ Primary region recovered, manual rollback required" "warning"
                fi
            fi
        else
            consecutive_failures=$((consecutive_failures + 1))
            log_error "Health check failed ($consecutive_failures/$FAIL_THRESHOLD)"

            # Update state with failures
            state=$(update_state "$active_region" "$consecutive_failures")

            # Check if we should failover
            if [[ $consecutive_failures -ge $FAIL_THRESHOLD ]]; then
                log_error "Failure threshold reached, initiating failover..."

                # Determine target region
                local target_region
                if [[ "$active_region" == "$PRIMARY_REGION" ]]; then
                    target_region="$SECONDARY_REGION"
                else
                    target_region="$BACKUP_REGION"
                fi

                # Execute failover
                execute_failover "$target_region"

                # Reset counter
                consecutive_failures=0
            fi
        fi

        # Wait before next check
        sleep "$CHECK_INTERVAL"
    done
}

# Main
main() {
    local command=${1:-monitor}

    mkdir -p "$(dirname "$LOG_FILE")"

    case "$command" in
        monitor)
            monitor_mode
            ;;
        check)
            if check_primary_health; then
                log_info "Health check passed"
                exit 0
            else
                log_error "Health check failed"
                exit 1
            fi
            ;;
        failover)
            if [[ $# -lt 2 ]]; then
                log_error "Usage: $0 failover [region]"
                exit 1
            fi
            execute_failover "$2"
            ;;
        validate)
            local state
            state=$(load_state)
            log_info "Current state: $state"
            echo "$state" | jq .
            ;;
        rollback)
            rollback_to_primary
            ;;
        *)
            echo "Usage: $0 {monitor|check|failover|validate|rollback}"
            echo ""
            echo "Commands:"
            echo "  monitor              - Continuous monitoring with auto-failover"
            echo "  check                - Single health check"
            echo "  failover [region]    - Execute failover to specified region"
            echo "  validate             - Display current failover state"
            echo "  rollback             - Rollback to primary region"
            exit 1
            ;;
    esac
}

main "$@"
