#!/bin/bash

# ErlMCP Production Feature Flag Update Script
# Update production feature flags with proper validation and rollout strategy

set -euo pipefail

# Configuration
ENVIRONMENT="production"
CONFIG_FILE="${CONFIG_FILE:-./config/feature-flags.json}"
KUBE_NAMESPACE="erlmcp-prod"
GRAFANA_DASHBOARDS="https://grafana.erlmcp.io"
HEALTH_CHECK_TIMEOUT=300
ROLLOUT_PERCENTAGE=10
NOTIFICATION_WEBHOOK="${NOTIFICATION_WEBHOOK:-$SLACK_WEBHOOK}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Feature flag states
STATE_ENABLED="enabled"
STATE_DISABLED="disabled"
STATE_ROLLOUT="rollout"

# Logging functions
log() {
    echo -e "${BLUE}[INFO] $1${NC}" | tee -a feature-flag-update.log
}

success() {
    echo -e "${GREEN}[SUCCESS] $1${NC}" | tee -a feature-flag-update.log
}

warning() {
    echo -e "${YELLOW}[WARNING] $1${NC}" | tee -a feature-flag-update.log
}

error() {
    echo -e "${RED}[ERROR] $1${NC}" | tee -a feature-flag-update.log >&2
    exit 1
}

# Send notification
send_notification() {
    local message=$1
    local severity=${2:-"info"}

    if [ -n "$NOTIFICATION_WEBHOOK" ]; then
        curl -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"[$severity] Feature Flag Update: $message\"}" \
            "$NOTIFICATION_WEBHOOK" >/dev/null 2>&1
    fi
}

# Load current feature flags
load_current_flags() {
    if [ -f "$CONFIG_FILE" ]; then
        cat "$CONFIG_FILE"
    else
        log "No existing feature flags configuration, starting fresh"
        echo "{}"
    fi
}

# Validate feature flag configuration
validate_feature_flags() {
    local flags=$1

    # Check if JSON is valid
    if ! jq empty <<< "$flags"; then
        error "Invalid JSON in feature flags configuration"
    fi

    # Validate each flag
    echo "$flags" | jq -c '.flags[]?' | while read -r flag; do
        local name=$(echo "$flag" | jq -r '.name')
        local state=$(echo "$flag" | jq -r '.state')
        local rollout_percentage=$(echo "$flag" | jq -r '.rollout_percentage // 0')

        # Validate name
        if [ -z "$name" ]; then
            error "Feature flag name is required"
        fi

        # Validate state
        if ! [[ "$state" =~ ^(enabled|disabled|rollout)$ ]]; then
            error "Invalid state for feature flag $name: $state"
        fi

        # Validate rollout percentage
        if [ "$state" = "rollout" ] && ! [[ "$rollout_percentage" =~ ^[0-9]+$ ]] || [ "$rollout_percentage" -gt 100 ]; then
            error "Invalid rollout percentage for feature flag $name: $rollout_percentage"
        fi

        log "Validated feature flag: $name ($state, ${rollout_percentage}%)"
    done
}

# Check health before enabling feature flags
check_environment_health() {
    log "Checking environment health before updating feature flags"

    # Check service health
    if ! kubectl get pods -n "$KUBE_NAMESPACE" -l app=erlmcp-server | grep -q "Running"; then
        error "ErlMCP services are not running"
    fi

    # Check database connectivity
    if ! kubectl exec -n "$KUBE_NAMESPACE" -c erlmcp-server deployment/erlmcp-server -- erlmcp eval "erlang:is_alive()." >/dev/null 2>&1; then
        error "Cannot connect to Erlang cluster"
    fi

    # Check resource usage
    local cpu_usage=$(kubectl top pods -n "$KUBE_NAMESPACE" --no-headers | awk '{sum += $3} END {print sum}')
    local memory_usage=$(kubectl top pods -n "$KUBE_NAMESPACE" --no-headers | awk '{sum += $2} END {print sum}')

    if [ "$cpu_usage" -gt 80 ]; then
        warning "High CPU usage detected: ${cpu_usage}%"
    fi

    if [ "$memory_usage" -gt 80 ]; then
        warning "High memory usage detected: ${memory_usage}%"
    fi

    log "Environment health check passed"
}

# Update feature flags in Redis
update_feature_flags() {
    local flags=$1

    log "Updating feature flags in Redis"

    # Generate Redis commands
    echo "$flags" | jq -c '.flags[]?' | while read -r flag; do
        local name=$(echo "$flag" | jq -r '.name')
        local state=$(echo "$flag" | jq -r '.state')
        local rollout_percentage=$(echo "$flag" | jq -r '.rollout_percentage // 0')
        local description=$(echo "$flag" | jq -r '.description // ""')
        local conditions=$(echo "$flag" | jq -c '.conditions // []' | sed 's/"/\\"/g')

        # Set feature flag
        redis-cli -h redis-master -p 6379 HSET "feature_flags:$name" \
            "state" "$state" \
            "rollout_percentage" "$rollout_percentage" \
            "description" "$description" \
            "conditions" "$conditions" \
            "updated_at" "$(date -Iseconds)"

        # Add to active set
        if [ "$state" = "enabled" ] || [ "$state" = "rollout" ]; then
            redis-cli -h redis-master -p 6379 SADD "active_feature_flags" "$name"
        else
            redis-cli -h redis-master -p 6379 SREM "active_feature_flags" "$name"
        fi

        # Log the update
        log "Updated feature flag: $name -> $state (${rollout_percentage}%)"
    done

    # Set feature flag version
    redis-cli -h redis-master -p 6379 SET "feature_flags_version" "$(date +%s)"

    success "Feature flags updated in Redis"
}

# Apply feature flag changes
apply_feature_flags() {
    local flags=$1

    log "Applying feature flag changes to ErlMCP instances"

    # Get all running ErlMCP pods
    local pods=$(kubectl get pods -n "$KUBE_NAMESPACE" -l app=erlmcp-server -o jsonpath='{.items[*].metadata.name}')

    # Update each pod
    for pod in $pods; do
        # Reload feature flags
        kubectl exec -n "$KUBE_NAMESPACE" "$pod" -- erl -pa deps/*/ebin -pa ebin -s erlmcp_feature_flags reload -s init stop >/dev/null 2>&1

        # Verify reload
        if [ $? -eq 0 ]; then
            log "Feature flags reloaded in $pod"
        else
            warning "Failed to reload feature flags in $pod"
        fi
    done

    success "Feature flags applied to all instances"
}

# Monitor feature flag rollout
monitor_rollout() {
    local flags=$1

    log "Monitoring feature flag rollout"

    # Check flags in rollout state
    rollout_flags=$(echo "$flags" | jq -c '.flags[]? | select(.state == "rollout")')

    if [ -n "$rollout_flags" ]; then
        echo "$rollout_flags" | while read -r flag; do
            local name=$(echo "$flag" | jq -r '.name')
            local expected_percentage=$(echo "$flag" | jq -r '.rollout_percentage')

            log "Monitoring rollout for $name ($expected_percentage%)"

            # Check actual rollout percentage
            local actual_percentage=$(kubectl exec -n "$KUBE_NAMESPACE" -c erlmcp-server deployment/erlmcp-server -- erl -pa deps/*/ebin -pa ebin -s erlmcp_feature_flags get_percentage "$name" -s init stop)

            if [ "$actual_percentage" -lt "$expected_percentage" ]; then
                warning "Rollout percentage for $name is lower than expected: $actual_percentage% vs $expected_percentage%"
            else
                success "Rollout percentage for $name is on track: $actual_percentage%"
            fi

            # Check error rates
            local error_rate=$(kubectl exec -n "$KUBE_NAMESPACE" -c erlmcp-server deployment/erlmcp-server -- erl -pa deps/*/ebin -pa ebin -s erlmcp_feature_flags get_error_rate "$name" -s init stop)

            if [ "$error_rate" -gt 5 ]; then
                warning "High error rate detected for feature flag $name: $error_rate%"
                # Consider disabling the feature flag
            fi
        done
    fi
}

# Generate feature flag dashboard
generate_dashboard() {
    local flags=$1

    log "Generating feature flag monitoring dashboard"

    # Create Grafana dashboard JSON
    cat > feature-flag-dashboard.json << EOF
{
  "dashboard": {
    "title": "ErlMCP Feature Flags",
    "panels": [
EOF

    # Add panel for each feature flag
    echo "$flags" | jq -c '.flags[]?' | while read -r flag; do
        local name=$(echo "$flag" | jq -r '.name')
        local state=$(echo "$flag" | jq -r '.state')
        local rollout_percentage=$(echo "$flag" | jq -r '.rollout_percentage // 0')

        cat << EOF
      {
        "title": "$name",
        "type": "singlestat",
        "targets": [
          {
            "expr": "erlmcp_feature_flag_percentage{name=\"$name\"}",
            "legendFormat": "$name"
          }
        ],
        "fieldConfig": {
          "defaults": {
            "thresholds": {
              "steps": [
                {"color": "red", "value": 0},
                {"color": "yellow", "value": 50},
                {"color": "green", "value": 100}
              ]
            }
          }
        }
      },
EOF
    done

    cat << EOF
    ]
  }
}
EOF

    success "Dashboard generated: feature-flag-dashboard.json"
}

# Main update function
main() {
    log "Starting feature flag update for $ENVIRONMENT environment"

    # Load current feature flags
    local current_flags=$(load_current_flags)

    # Validate new configuration if provided
    if [ -n "${1:-}" ]; then
        log "Validating new feature flag configuration"
        validate_feature_flags "$1"
        local new_flags="$1"
    else
        log "Using existing feature flag configuration"
        local new_flags="$current_flags"
    fi

    # Check environment health
    check_environment_health

    # Backup current configuration
    cp "$CONFIG_FILE" "$CONFIG_FILE.backup.$(date +%Y%m%d-%H%M%S)"

    # Update configuration
    echo "$new_flags" > "$CONFIG_FILE"

    # Update Redis
    update_feature_flags "$new_flags"

    # Apply to ErlMCP instances
    apply_feature_flags "$new_flags"

    # Monitor rollout
    monitor_rollout "$new_flags"

    # Generate monitoring dashboard
    generate_dashboard "$new_flags"

    # Send notification
    send_notification "Feature flags updated successfully for $ENVIRONMENT" "success"

    success "Feature flag update completed successfully"
}

# Parse command line arguments
case "${1:-}" in
    --validate)
        validate_feature_flags "$(cat)"
        ;;
    --health-check)
        check_environment_health
        ;;
    --monitor)
        if [ -z "${2:-}" ]; then
            error "Feature flag name required for monitoring"
        fi
        monitor_rollout "$(cat)"
        ;;
    *)
        main "$@"
        ;;
esac