#!/bin/bash
# ============================================================================
# Pipeline Secrets Setup Script
# Configures required secrets for the GCP Marketplace CI/CD Pipeline
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

PROJECT_ID="${PROJECT_ID:-}"
BUILD_SA_NAME="cloudbuild-marketplace"
BUILD_SA_EMAIL="${BUILD_SA_NAME}@${PROJECT_ID}.iam.gserviceaccount.com"

# ============================================================================
# Functions
# =============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found"
        exit 1
    fi

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    # Enable Secret Manager API
    gcloud services enable secretmanager.googleapis.com --project="$PROJECT_ID" 2>/dev/null || true

    log_info "✓ Prerequisites checked"
}

create_slack_webhook_secret() {
    log_info "Creating Slack webhook secret..."

    local secret_name="slack-webhook-url"

    # Check if secret already exists
    if gcloud secrets describe "$secret_name" --project="$PROJECT_ID" &>/dev/null; then
        log_warn "Secret already exists: $secret_name"
        grant_secret_access "$secret_name"
        return 0
    fi

    echo ""
    echo "Enter your Slack webhook URL (or press Enter to skip):"
    echo "Get it from: https://api.slack.com/messaging/webhooks"
    read -p "Webhook URL: " webhook_url

    if [ -n "$webhook_url" ]; then
        echo "$webhook_url" | gcloud secrets create "$secret_name" \
            --data-file=- \
            --project="$PROJECT_ID"

        grant_secret_access "$secret_name"
        log_info "✓ Created secret: $secret_name"
    else
        log_warn "Skipped Slack webhook secret"
    fi
}

create_pagerduty_secret() {
    log_info "Creating PagerDuty secret..."

    local secret_name="pagerduty-api-key"

    # Check if secret already exists
    if gcloud secrets describe "$secret_name" --project="$PROJECT_ID" &>/dev/null; then
        log_warn "Secret already exists: $secret_name"
        grant_secret_access "$secret_name"
        return 0
    fi

    echo ""
    echo "Enter your PagerDuty API key (or press Enter to skip):"
    read -sp "API Key: " api_key
    echo ""

    if [ -n "$api_key" ]; then
        echo "$api_key" | gcloud secrets create "$secret_name" \
            --data-file=- \
            --project="$PROJECT_ID"

        grant_secret_access "$secret_name"
        log_info "✓ Created secret: $secret_name"
    else
        log_warn "Skipped PagerDuty secret"
    fi
}

grant_secret_access() {
    local secret_name="$1"

    log_info "Granting access to $secret_name..."

    gcloud secrets add-iam-policy-binding "$secret_name" \
        --project="$PROJECT_ID" \
        --member="serviceAccount:$BUILD_SA_EMAIL" \
        --role="roles/secretmanager.secretAccessor" 2>/dev/null || {
        log_warn "    Failed to grant (may already have access)"
    }
}

create_notification_email_secret() {
    log_info "Creating notification email secret..."

    local secret_name="notification-email"

    # Check if secret already exists
    if gcloud secrets describe "$secret_name" --project="$PROJECT_ID" &>/dev/null; then
        log_warn "Secret already exists: $secret_name"
        grant_secret_access "$secret_name"
        return 0
    fi

    echo ""
    echo "Enter notification email (or press Enter to skip):"
    read -p "Email: " email

    if [ -n "$email" ]; then
        echo "$email" | gcloud secrets create "$secret_name" \
            --data-file=- \
            --project="$PROJECT_ID"

        grant_secret_access "$secret_name"
        log_info "✓ Created secret: $secret_name"
    else
        log_warn "Skipped notification email secret"
    fi
}

create_custom_secrets() {
    log_info "Creating custom secrets..."

    echo ""
    echo "You can create additional secrets now."
    echo "Enter secret name and value (or press Enter to finish):"

    while true; do
        read -p "Secret name: " secret_name
        if [ -z "$secret_name" ]; then
            break
        fi

        read -sp "Secret value: " secret_value
        echo ""

        if [ -n "$secret_value" ]; then
            echo "$secret_value" | gcloud secrets create "$secret_name" \
                --data-file=- \
                --project="$PROJECT_ID"

            grant_secret_access "$secret_name"
            log_info "✓ Created secret: $secret_name"
        fi
    done
}

list_secrets() {
    echo ""
    log_info "Current secrets in project:"

    gcloud secrets list \
        --project="$PROJECT_ID" \
        --filter="name:*slack* OR name:*pagerduty* OR name:*notification*" \
        --format="table(name,created)" 2>/dev/null || log_info "No secrets found"
}

print_summary() {
    echo ""
    echo "=========================================="
    echo "Secrets Setup Summary"
    echo "=========================================="
    log_info "Project ID:        $PROJECT_ID"
    log_info "Service Account:   $BUILD_SA_EMAIL"
    echo ""
    log_info "Secrets created:"
    gcloud secrets list \
        --project="$PROJECT_ID" \
        --format="value(name)" 2>/dev/null | while read -r secret; do
            if [ -n "$secret" ]; then
                log_info "  - $secret"
            fi
        done
    echo ""
    log_info "To use secrets in your pipeline:"
    echo "  gcloud secrets versions access latest \\"
    echo "    --secret=SECRET_NAME \\"
    echo "    --project=$PROJECT_ID"
    echo "=========================================="
}

# ============================================================================
# Main
# ============================================================================

main() {
    echo "=========================================="
    echo "GCP Marketplace Pipeline Secrets Setup"
    echo "=========================================="

    check_prerequisites

    # Check if SA exists
    if ! gcloud iam service-accounts describe "$BUILD_SA_EMAIL" \
        --project="$PROJECT_ID" &>/dev/null; then
        log_error "Service account not found: $BUILD_SA_EMAIL"
        log_info "Run setup-pipeline-iam.sh first"
        exit 1
    fi

    # Create secrets
    create_slack_webhook_secret
    create_pagerduty_secret
    create_notification_email_secret

    # Optional: create custom secrets
    echo ""
    read -p "Create additional custom secrets? (y/N): " create_custom
    if [ "$create_custom" = "y" ] || [ "$create_custom" = "Y" ]; then
        create_custom_secrets
    fi

    list_secrets
    print_summary

    log_info "Secrets setup completed!"
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --sa-name)
            BUILD_SA_NAME="$2"
            BUILD_SA_EMAIL="${BUILD_SA_NAME}@${PROJECT_ID}.iam.gserviceaccount.com"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT_ID] [--sa-name SA_NAME]"
            exit 1
            ;;
    esac
done

main "$@"
