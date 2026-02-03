#!/bin/bash
# ============================================================================
# Pipeline Notification Script
# Sends notifications for pipeline completion (Slack, Email, Pub/Sub)
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

STATUS="${STATUS:-SUCCESS}"
BUILD_ID="${BUILD_ID:-unknown}"
PROJECT_ID="${PROJECT_ID:-}"
BUILD_LOG_URL="${BUILD_LOG_URL:-}"
EVIDENCE_URL="${EVIDENCE_URL:-}"
IMAGE_DIGEST="${IMAGE_DIGEST:-}"
BRANCH_NAME="${BRANCH_NAME:-unknown}"
SHORT_SHA="${SHORT_SHA:-unknown}"

# Determine notification channel from environment
NOTIFICATION_CHANNEL="${NOTIFICATION_CHANNEL:-slack}"  # slack | email | both | pubsub

# ============================================================================
# Notification Functions
# ============================================================================

send_slack_notification() {
    local status="$1"

    log_info "Sending Slack notification..."

    # Get Slack webhook from Secret Manager
    local webhook_url=""
    if [ -n "$PROJECT_ID" ]; then
        webhook_url=$(gcloud secrets versions access latest \
          --secret=slack-webhook-url \
          --project "$PROJECT_ID" 2>/dev/null || echo "")
    fi

    if [ -z "$webhook_url" ]; then
        log_warn "Slack webhook URL not found in Secret Manager"
        log_warn "To enable Slack notifications, create the secret:"
        log_warn "  echo 'https://hooks.slack.com/services/YOUR/WEBHOOK/URL' | \\"
        log_warn "    gcloud secrets create slack-webhook-url --data-file=-"
        return 1
    fi

    # Prepare message
    local emoji="🟢"
    local color="good"
    if [ "$status" = "FAILURE" ]; then
        emoji="🔴"
        color="danger"
    elif [ "$status" = "WARNING" ]; then
        emoji="🟡"
        color="warning"
    fi

    # Build attachment
    local attachment="{
      \"color\": \"$color\",
      \"blocks\": [
        {
          \"type\": \"header\",
          \"text\": {
            \"type\": \"plain_text\",
            \"text\": \"$emoji GCP Marketplace Build $status\"
          }
        },
        {
          \"type\": \"section\",
          \"fields\": [
            {
              \"type\": \"mrkdwn\",
              \"text\": \"*Build ID:*\n#$BUILD_ID\"
            },
            {
              \"type\": \"mrkdwn\",
              \"text\": \"*Project:*\n$PROJECT_ID\"
            },
            {
              \"type\": \"mrkdwn\",
              \"text\": \"*Branch:*\n$BRANCH_NAME\"
            },
            {
              \"type\": \"mrkdwn\",
              \"text\": \"*Commit:*\n\`$SHORT_SHA\`\"
            }
          ]
        }
      ]
    }"

    # Add optional fields
    if [ -n "$BUILD_LOG_URL" ]; then
        attachment=$(echo "$attachment" | jq --arg url "$BUILD_LOG_URL" '.blocks += [
          {
            "type": "section",
            "fields": [
              {
                "type": "mrkdwn",
                "text": "*Logs:*\n<$url|View Build Logs>"
              }
            ]
          }
        ]')
    fi

    if [ -n "$EVIDENCE_URL" ]; then
        attachment=$(echo "$attachment" | jq --arg url "$EVIDENCE_URL" '.blocks += [
          {
            "type": "section",
            "fields": [
              {
                "type": "mrkdwn",
                "text": "*Evidence:*\n<$url|View Artifacts>"
              }
            ]
          }
        ]')
    fi

    # Add image info if available
    if [ -n "$IMAGE_DIGEST" ]; then
        attachment=$(echo "$attachment" | jq --arg digest "$IMAGE_DIGEST" '.blocks += [
          {
            "type": "context",
            "elements": [
              {
                "type": "mrkdwn",
                "text": "Image: \`$digest\`"
              }
            ]
          }
        ]')
    fi

    # Send notification
    local payload="{\"attachments\": [$attachment]}"

    if curl -s -X POST "$webhook_url" \
      -H 'Content-Type: application/json' \
      -d "$payload" > /dev/null; then
        log_info "Slack notification sent successfully"
        return 0
    else
        log_error "Failed to send Slack notification"
        return 1
    fi
}

send_email_notification() {
    local status="$1"

    log_info "Sending email notification..."

    # Email notifications require an email service (SendGrid, Mailgun, etc.)
    # This is a placeholder for email integration

    local to="${NOTIFICATION_EMAIL:-marketplace-team@example.com}"
    local from="noreply@${PROJECT_ID:-gcp-marketplace}.appspotmail.com"
    local subject="GCP Marketplace Build $status - #$BUILD_ID"

    log_warn "Email notifications not configured"
    log_warn "To enable email notifications, configure an email service"
    log_warn "  Recipient: $to"
    log_warn "  Subject: $subject"

    return 0
}

send_pubsub_notification() {
    local status="$1"

    log_info "Sending Pub/Sub notification..."

    local topic="projects/$PROJECT_ID/topics/build-notifications"

    # Check if topic exists
    if ! gcloud pubsub topics describe "$topic" --project "$PROJECT_ID" &>/dev/null; then
        log_warn "Pub/Sub topic not found: $topic"
        return 1
    fi

    # Prepare message
    local message=$(cat <<EOF
{
  "build_id": "$BUILD_ID",
  "project_id": "$PROJECT_ID",
  "status": "$status",
  "branch": "$BRANCH_NAME",
  "commit": "$SHORT_SHA",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "image_digest": "$IMAGE_DIGEST",
  "log_url": "$BUILD_LOG_URL",
  "evidence_url": "$EVIDENCE_URL"
}
EOF
)

    # Publish message
    if echo "$message" | gcloud pubsub topics publish "$topic" --attribute="source=cloudbuild,pipeline=marketplace,status=$status"; then
        log_info "Pub/Sub notification sent successfully"
        return 0
    else
        log_error "Failed to send Pub/Sub notification"
        return 1
    fi
}

# ============================================================================
# Main
# ============================================================================

main() {
    local status="${STATUS:-SUCCESS}"

    log_info "Sending pipeline notifications..."
    log_info "  Status: $status"
    log_info "  Build ID: $BUILD_ID"
    log_info "  Channel: $NOTIFICATION_CHANNEL"

    case "$NOTIFICATION_CHANNEL" in
        slack)
            send_slack_notification "$status"
            ;;
        email)
            send_email_notification "$status"
            ;;
        both)
            send_slack_notification "$status"
            send_email_notification "$status"
            ;;
        pubsub)
            send_pubsub_notification "$status"
            ;;
        all)
            send_slack_notification "$status"
            send_email_notification "$status"
            send_pubsub_notification "$status"
            ;;
        *)
            log_warn "Unknown notification channel: $NOTIFICATION_CHANNEL"
            ;;
    esac

    log_info "Notifications completed"
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --status)
            STATUS="$2"
            shift 2
            ;;
        --build-id)
            BUILD_ID="$2"
            shift 2
            ;;
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --channel)
            NOTIFICATION_CHANNEL="$2"
            shift 2
            ;;
        --log-url)
            BUILD_LOG_URL="$2"
            shift 2
            ;;
        --evidence-url)
            EVIDENCE_URL="$2"
            shift 2
            ;;
        --image-digest)
            IMAGE_DIGEST="$2"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

main "$@"
