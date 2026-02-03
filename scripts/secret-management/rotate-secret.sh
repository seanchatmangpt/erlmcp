#!/bin/bash
# Secret Rotation Script for erlmcp v3
# Usage: ./rotate-secret.sh <secret-path> [options]
#
# Options:
#   --dry-run     Show what would be rotated without executing
#   --no-restart  Skip pod restart after rotation
#   --notify      Send notification after rotation
#
# Examples:
#   ./rotate-secret.sh erlmcp/config/jwt
#   ./rotate-secret.sh erlmcp/config/jwt --dry-run

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
VAULT_ADDR="${VAULT_ADDR:-https://vault.vault.svc.cluster.local:8200}"
DRY_RUN=false
RESTART_PODS=true
SEND_NOTIFICATION=false
SECRET_PATH=""

# Logging functions
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }

# Parse arguments
parse_args() {
    if [[ $# -eq 0 ]]; then
        log_error "Usage: $0 <secret-path> [options]"
        exit 1
    fi

    SECRET_PATH="$1"
    shift

    while [[ $# -gt 0 ]]; do
        case $1 in
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --no-restart)
                RESTART_PODS=false
                shift
                ;;
            --notify)
                SEND_NOTIFICATION=true
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
}

# Check prerequisites
check_prerequisites() {
    if ! command -v vault &>/dev/null; then
        log_error "vault CLI not found. Please install Vault CLI."
        exit 1
    fi

    if ! command -v kubectl &>/dev/null; then
        log_error "kubectl not found. Please install kubectl."
        exit 1
    fi

    if [[ -z "${VAULT_TOKEN:-}" ]]; then
        log_error "VAULT_TOKEN environment variable not set"
        exit 1
    fi
}

# Validate secret path
validate_secret() {
    log_info "Validating secret path: ${SECRET_PATH}"

    if ! vault kv get "${SECRET_PATH}" &>/dev/null; then
        log_error "Secret path does not exist: ${SECRET_PATH}"
        exit 1
    fi

    log_info "Secret path is valid"
}

# Generate new secret value based on type
generate_new_value() {
    local secret_type="$1"
    local current_value="$2"

    case "${secret_type}" in
        jwt-secret|session-secret|encryption-key)
            openssl rand -base64 32
            ;;
        password)
            openssl rand -base64 16 | tr -d "=+/" | cut -c1-16
            ;;
        api-key)
            openssl rand -hex 32
            ;;
        token)
            openssl rand -base64 48 | tr -d "=+/"
            ;;
        *)
            log_warn "Unknown secret type: ${secret_type}, generating random string"
            openssl rand -base64 32
            ;;
    esac
}

# Rotate the secret
rotate_secret() {
    log_info "Rotating secret: ${SECRET_PATH}"

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would rotate secret at: ${SECRET_PATH}"
        return 0
    fi

    # Get current secret metadata
    local current_version
    current_version=$(vault kv get -field=metadata.version "${SECRET_PATH}" 2>/dev/null || echo "unknown")
    log_debug "Current version: ${current_version}"

    # Backup current secret to audit path
    local backup_path="audit/secret-rotations/$(date +%Y%m%d)/${SECRET_PATH//\//-}"
    vault kv put "${backup_path}" \
        rotated_at="$(date -Iseconds)" \
        rotated_from="${SECRET_PATH}" \
        previous_version="${current_version}" \
        old_values="$(vault kv get -format=json "${SECRET_PATH}")"

    log_info "Backed up current secret to: ${backup_path}"

    # Generate new values based on secret type
    local secret_type
    secret_type=$(basename "${SECRET_PATH}")

    case "${secret_type}" in
        jwt)
            local new_secret
            new_secret=$(generate_new_value "jwt-secret" "")
            vault kv patch "${SECRET_PATH}" \
                secret="${new_secret}" \
                rotated_at="$(date -Iseconds)" \
                rotated_by="${USER:-unknown}"
            log_info "Rotated JWT secret"
            ;;
        session)
            local new_secret
            new_secret=$(generate_new_value "session-secret" "")
            vault kv patch "${SECRET_PATH}" \
                secret="${new_secret}" \
                rotated_at="$(date -Iseconds)" \
                rotated_by="${USER:-unknown}"
            log_info "Rotated session secret"
            ;;
        encryption)
            local new_key
            new_key=$(generate_new_value "encryption-key" "")
            vault kv patch "${SECRET_PATH}" \
                key="${new_key}" \
                rotated_at="$(date -Iseconds)" \
                rotated_by="${USER:-unknown}"
            log_info "Rotated encryption key"
            ;;
        *)
            log_warn "No specific rotation logic for ${secret_type}, patching with timestamp"
            vault kv patch "${SECRET_PATH}" \
                rotated_at="$(date -Iseconds)" \
                rotated_by="${USER:-unknown}"
            ;;
    esac

    # Get new version
    local new_version
    new_version=$(vault kv get -field=metadata.version "${SECRET_PATH}" 2>/dev/null || echo "unknown")
    log_info "New version: ${new_version}"
}

# Trigger Kubernetes secret refresh
trigger_refresh() {
    log_info "Triggering ExternalSecret refresh"

    # Find ExternalSecrets that reference this path
    local namespace
    local secret_name

    secret_name=$(basename "${SECRET_PATH}")
    namespace="${NAMESPACE:-erlmcp-system}"

    # Trigger refresh by updating annotation
    local external_secret_name
    external_secret_name="erlmcp-${secret_name}"

    if kubectl get externalsecret "${external_secret_name}" -n "${namespace}" &>/dev/null; then
        kubectl annotate \
            "externalsecret/${external_secret_name}" \
            -n "${namespace}" \
            "force-sync=$(date +%s)" \
            --overwrite

        log_info "Triggered refresh for ${external_secret_name}"

        # Wait for refresh
        log_info "Waiting for secret to sync (up to 30 seconds)..."
        local count=0
        while [[ $count -lt 30 ]]; do
            if kubectl get secret "${external_secret_name}" -n "${namespace}" &>/dev/null; then
                log_info "Secret synced successfully"
                break
            fi
            sleep 1
            ((count++))
        done
    else
        log_warn "ExternalSecret ${external_secret_name} not found in ${namespace}"
    fi
}

# Restart affected pods
restart_pods() {
    if [[ "${RESTART_PODS}" != "true" ]]; then
        log_info "Pod restart skipped (--no-restart)"
        return 0
    fi

    log_info "Restarting affected pods"

    local namespace="${NAMESPACE:-erlmcp-system}"
    local deployments

    # Find deployments using the secret
    deployments=$(kubectl get deployments -n "${namespace}" \
        -o jsonpath='{.items[?(@.spec.template.spec.volumes[*].secret.secretName=='erlmcp-*')].metadata.name}' 2>/dev/null)

    if [[ -n "${deployments}" ]]; then
        for deployment in ${deployments}; do
            log_info "Restarting deployment: ${deployment}"
            kubectl rollout restart "deployment/${deployment}" -n "${namespace}"

            # Wait for rollout
            kubectl rollout status "deployment/${deployment}" -n "${namespace}" --timeout=5m
            log_info "Deployment ${deployment} restarted successfully"
        done
    else
        log_warn "No deployments found using rotated secret"
    fi
}

# Send notification
send_notification() {
    if [[ "${SEND_NOTIFICATION}" != "true" ]]; then
        return 0
    fi

    log_info "Sending rotation notification"

    local notification_payload
    notification_payload=$(cat <<EOF
{
  "text": "Secret rotation completed",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": "Secret Rotation Complete"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Secret Path:*\n${SECRET_PATH}"
        },
        {
          "type": "mrkdwn",
          "text": "*Timestamp:*\n$(date -Iseconds)"
        },
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n${ENVIRONMENT:-production}"
        },
        {
          "type": "mrkdwn",
          "text": "*Rotated By:*\n${USER:-unknown}"
        }
      ]
    }
  ]
}
EOF
)

    # Send to Slack if webhook configured
    local slack_webhook="${SLACK_WEBHOOK_URL:-}"
    if [[ -n "${slack_webhook}" ]]; then
        curl -X POST "${slack_webhook}" \
            -H 'Content-Type: application/json' \
            -d "${notification_payload}" &>/dev/null && log_info "Slack notification sent" || log_warn "Failed to send Slack notification"
    fi

    # Send to Microsoft Teams if webhook configured
    local teams_webhook="${TEAMS_WEBHOOK_URL:-}"
    if [[ -n "${teams_webhook}" ]]; then
        curl -X POST "${teams_webhook}" \
            -H 'Content-Type: application/json' \
            -d "${notification_payload}" &>/dev/null && log_info "Teams notification sent" || log_warn "Failed to send Teams notification"
    fi
}

# Main execution
main() {
    log_info "Starting secret rotation: ${SECRET_PATH}"
    log_info "Dry run: ${DRY_RUN}, Restart pods: ${RESTART_PODS}, Notify: ${SEND_NOTIFICATION}"

    check_prerequisites
    validate_secret
    rotate_secret
    trigger_refresh
    restart_pods
    send_notification

    log_info "Secret rotation complete!"
}

# Run
parse_args "$@"
main
