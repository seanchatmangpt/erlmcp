#!/bin/bash
#
# TCPS Notification Integration
# Send Andon alerts and pipeline status to Slack, Email, and other channels
#

set -e

# ============================================================================
# Configuration (Set via environment variables or CI/CD secrets)
# ============================================================================

SLACK_WEBHOOK_URL="${SLACK_WEBHOOK_URL:-}"
EMAIL_SMTP_SERVER="${EMAIL_SMTP_SERVER:-smtp.gmail.com}"
EMAIL_SMTP_PORT="${EMAIL_SMTP_PORT:-587}"
EMAIL_FROM="${EMAIL_FROM:-}"
EMAIL_TO="${EMAIL_TO:-}"
EMAIL_USERNAME="${EMAIL_USERNAME:-}"
EMAIL_PASSWORD="${EMAIL_PASSWORD:-}"

# Dashboard SSE endpoint
TCPS_DASHBOARD_URL="${TCPS_DASHBOARD_URL:-}"

# ============================================================================
# Helper Functions
# ============================================================================

send_slack_notification() {
    local event_type=$1
    local severity=$2
    local message=$3
    local work_order=$4

    if [ -z "$SLACK_WEBHOOK_URL" ]; then
        echo "⚠️  SLACK_WEBHOOK_URL not set, skipping Slack notification"
        return
    fi

    # Color coding
    local color
    case "$severity" in
        critical)
            color="#FF0000"  # Red
            ;;
        high)
            color="#FF9900"  # Orange
            ;;
        medium)
            color="#FFFF00"  # Yellow
            ;;
        *)
            color="#00FF00"  # Green
            ;;
    esac

    # Icon
    local icon
    case "$event_type" in
        andon)
            icon=":rotating_light:"
            ;;
        success)
            icon=":white_check_mark:"
            ;;
        failure)
            icon=":x:"
            ;;
        *)
            icon=":information_source:"
            ;;
    esac

    # Construct JSON payload
    local payload=$(cat <<EOF
{
  "attachments": [
    {
      "fallback": "${message}",
      "color": "${color}",
      "title": "${icon} TCPS ${event_type^^} Alert",
      "text": "${message}",
      "fields": [
        {
          "title": "Work Order",
          "value": "${work_order}",
          "short": true
        },
        {
          "title": "Severity",
          "value": "${severity}",
          "short": true
        },
        {
          "title": "Timestamp",
          "value": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
          "short": false
        }
      ],
      "footer": "TCPS Production Pipeline",
      "footer_icon": "https://platform.slack-edge.com/img/default_application_icon.png"
    }
  ]
}
EOF
)

    # Send to Slack
    if curl -X POST \
        -H 'Content-Type: application/json' \
        -d "$payload" \
        "$SLACK_WEBHOOK_URL" 2>&1; then
        echo "✅ Slack notification sent"
    else
        echo "❌ Failed to send Slack notification"
    fi
}

send_email_notification() {
    local event_type=$1
    local severity=$2
    local message=$3
    local work_order=$4

    if [ -z "$EMAIL_FROM" ] || [ -z "$EMAIL_TO" ]; then
        echo "⚠️  Email credentials not set, skipping email notification"
        return
    fi

    # Construct email
    local subject="[TCPS ${severity^^}] ${event_type^^}: ${work_order}"
    local body=$(cat <<EOF
TCPS Production Pipeline Alert

Event Type: ${event_type}
Severity: ${severity}
Work Order: ${work_order}
Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)

Message:
${message}

---
This is an automated notification from the TCPS Production Pipeline.
EOF
)

    # Send via Python (requires smtplib)
    if command -v python3 &> /dev/null; then
        python3 <<PYTHON
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

msg = MIMEMultipart()
msg['From'] = "${EMAIL_FROM}"
msg['To'] = "${EMAIL_TO}"
msg['Subject'] = "${subject}"

msg.attach(MIMEText("""${body}""", 'plain'))

try:
    server = smtplib.SMTP("${EMAIL_SMTP_SERVER}", ${EMAIL_SMTP_PORT})
    server.starttls()
    server.login("${EMAIL_USERNAME}", "${EMAIL_PASSWORD}")
    server.send_message(msg)
    server.quit()
    print("✅ Email notification sent")
except Exception as e:
    print(f"❌ Failed to send email: {e}")
PYTHON
    else
        echo "⚠️  Python not found, cannot send email"
    fi
}

send_dashboard_update() {
    local event_type=$1
    local severity=$2
    local message=$3
    local work_order=$4

    if [ -z "$TCPS_DASHBOARD_URL" ]; then
        echo "⚠️  TCPS_DASHBOARD_URL not set, skipping dashboard update"
        return
    fi

    # Construct JSON payload
    local payload=$(cat <<EOF
{
  "event": "${event_type}",
  "severity": "${severity}",
  "message": "${message}",
  "work_order": "${work_order}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
)

    # Send to dashboard via SSE/WebSocket
    if curl -X POST \
        -H 'Content-Type: application/json' \
        -d "$payload" \
        "${TCPS_DASHBOARD_URL}/events" 2>&1; then
        echo "✅ Dashboard update sent"
    else
        echo "❌ Failed to send dashboard update"
    fi
}

# ============================================================================
# Main Notification Function
# ============================================================================

send_tcps_notification() {
    local event_type=$1    # andon, success, failure, info
    local severity=$2      # critical, high, medium, low
    local message=$3       # Notification message
    local work_order=$4    # Work order ID

    echo "📢 Sending TCPS notification:"
    echo "   Event: ${event_type}"
    echo "   Severity: ${severity}"
    echo "   Message: ${message}"
    echo "   Work Order: ${work_order}"

    # Send to all configured channels
    send_slack_notification "$event_type" "$severity" "$message" "$work_order"
    send_email_notification "$event_type" "$severity" "$message" "$work_order"
    send_dashboard_update "$event_type" "$severity" "$message" "$work_order"
}

# ============================================================================
# CLI Interface
# ============================================================================

if [ $# -lt 4 ]; then
    echo "Usage: $0 <event_type> <severity> <message> <work_order>"
    echo ""
    echo "Event Types: andon, success, failure, info"
    echo "Severity: critical, high, medium, low"
    echo ""
    echo "Example:"
    echo "  $0 andon critical 'SHACL validation failed' WO-20260126-123456"
    exit 1
fi

send_tcps_notification "$1" "$2" "$3" "$4"
